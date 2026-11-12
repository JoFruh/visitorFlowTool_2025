#### Area descriptors for the area choice, with a forest-type attractivity score ####
#
# The area choice reads one attractivity value per area: the DULN layer, aggregated.
# That layer's element weights are fitted to where people WALK (cells), which is another
# decision than which area they GO to. This describes each area directly, for the area
# choice's likelihood (data-raw/calibrate_aoi_rum_desc.R), with shares of the area's
# 10 m cells taken from the clean element layers (data-raw/tailor_duln_elements.R,
# layers2_<box>.tif) and a published forest attractivity model:
#
#   lakeShare    in the band of a lake of >= 10 ha, by the rule the lake element already
#                follows (data-raw/tailor_duln.R lakeBand(): the path loop round the lake,
#                else a buffer growing with the root of the lake's area, 150-300 m; the
#                water itself included). Large rivers and ponds do not count.
#   protShare    in or near protected nature: forestReserve, birdSanctuary or wetlands
#   viewShare    near a viewpoint or summit (summitsViews)
#   forestShare  forest, by the national forest inventory's forest mask (the cells the
#                Waldmischungsgrad LFI raster covers)
#   forestScore  the mean recreational score (1-10) of the area's forest cells, NA
#                without forest
#   forestAttr   forestShare x forestScore / 10: forest weighted by its score
#
# THE FOREST SCORE: Edwards et al. 2012, "Public preferences across Europe for different
# forest stand types as sites for recreation", Ecology and Society 17(1): 27, Table 1,
# the Central Europe panel's median scores (a Delphi survey; 1-10) per stand type =
# tree species type x phase of development x forest management alternative (FMA).
# Nothing is fitted here; the operational mapping onto wall-to-wall data is:
#   species type  the broadleaved share (Waldmischungsgrad LFI, 10 m, swisstopo STAC
#                 ch.bafu.landesforstinventar-waldmischungsgrad; 0 = pure conifer, 100 =
#                 pure broadleaved, checked on Davos 4 and Laegern 86) averaged over a
#                 50 m stand window (5 x 5 forest cells), then the inventory's own limits
#                 for a pure stand: < 10 % conifer, > 90 % broadleaved, else mixed
#   phase         the canopy height (vegetation height model, 10 m) averaged over the
#                 same window: < 2 m establishment (the paper's own limit), 2-8 m young,
#                 8-18 m medium, >= 18 m adult. The paper defines the phases by age and
#                 "when most height growth has been reached"; 8 and 18 m of MEAN canopy
#                 height stand for ~10 and ~22 m of top height (thicket / pole / timber).
#   FMA           1 (forest nature reserve) inside forestReserve, 2 (close-to-nature
#                 forestry, what Swiss forest law asks for) elsewhere
#
#   VFT_AD_AOI=<validation folder> Rscript data-raw/area_descriptors.R
#
# VFT_AD_AOI      a validation folder (reads aoi/<ctx>.rds)
# VFT_AD_CTX      comma list of contexts (default glatt,glattPark,wigger)
# VFT_AD_LAYERS   comma list of folders with layers2_<box>.tif (searched in order)
# VFT_AD_OUT      output folder (default Local Data/method_2611/area_desc): desc_<ctx>.rds,
#                 desc_<ctx>.csv, forest/<box>_{mix,height,score}.tif
# VFT_AD_FORCE    1 = rebuild the forest rasters

LOCAL  <- "C:/Users/frueh/Documents/Local Data"
AD_AOI <- Sys.getenv("VFT_AD_AOI", file.path(LOCAL, "pathUsageValidation_2026_best_autoCut_e2cRumT"))
AD_CTX <- strsplit(Sys.getenv("VFT_AD_CTX", "glatt,glattPark,wigger"), ",", fixed = TRUE)[[1]]
AD_LAYERS <- strsplit(Sys.getenv("VFT_AD_LAYERS", paste(
  file.path(LOCAL, "DULN_agg_tailored/timefix_rebuilt/duln_tailored/elements/layers"),
  file.path(LOCAL, "osm_traces/duln_tailored/elements/layers"), sep = ",")), ",", fixed = TRUE)[[1]]
AD_OUT <- Sys.getenv("VFT_AD_OUT", file.path(LOCAL, "method_2611", "area_desc"))
AD_FORCE <- identical(Sys.getenv("VFT_AD_FORCE"), "1")
AD_VEGH <- file.path("C:/Users/frueh/OneDrive - Eidg. Forschungsanstalt WSL/Dokumente/Local Large Datasets",
                     "Environmental datasets/VegHeight Map/vegH_10m_LV95.tif")
AD_MIX_URL <- paste0("/vsicurl/https://data.geo.admin.ch/ch.bafu.landesforstinventar-waldmischungsgrad/",
                     "landesforstinventar-waldmischungsgrad/landesforstinventar-waldmischungsgrad_2056.tif")
#the layer stack a context's areas lie in (GlattPark's are inside Glatt's box)
AD_BOX <- c(glattPark = "glatt")
dir.create(file.path(AD_OUT, "forest"), recursive = TRUE, showWarnings = FALSE)
logf <- function(...) cat(format(Sys.time(), "%H:%M:%S"), sprintf(...), "\n")

#Edwards et al. 2012, Table 1, Central Europe: rows = phase, columns = FMA 1, FMA 2
EDWARDS_CE <- list(
  conifer     = rbind(establishment = c(4, 4),   young = c(4.5, 4),   medium = c(6, 6), adult = c(8, 7.5)),
  broadleaved = rbind(establishment = c(4, 4),   young = c(5.5, 5),   medium = c(7, 7), adult = c(8.5, 9)),
  mixed       = rbind(establishment = c(5, 4),   young = c(5.5, 5.5), medium = c(7, 8), adult = c(10, 9)))
PHASE_BREAKS <- c(-Inf, 2, 8, 18, Inf)      #mean canopy height, metres
MIX_LIMITS   <- c(10, 90)                   #% broadleaved: below = conifer, above = broadleaved
STAND_WINDOW <- 5                           #cells of 10 m

layersFile <- function(box){
  f <- file.path(AD_LAYERS, paste0("layers2_", box, ".tif"))
  f <- f[file.exists(f)]
  if(!length(f)) stop("no layers2_", box, ".tif in VFT_AD_LAYERS")
  f[1]
}

#The forest rasters of a box, on its layer grid: broadleaved share (%), canopy height (m),
#stand type (1 conifer, 2 broadleaved, 3 mixed), phase (1-4) and the score.
forestRasters <- function(box){
  fS <- file.path(AD_OUT, "forest", paste0(box, "_score.tif"))
  fA <- file.path(AD_OUT, "forest", paste0(box, "_parts.tif"))
  if(file.exists(fS) && file.exists(fA) && !AD_FORCE) return(list(score = terra::rast(fS), parts = terra::rast(fA)))
  L <- terra::rast(layersFile(box))
  g <- L[[1]]
  #the mixture raster, cropped and written before anything reads it again (a lazy
  #/vsicurl source re-fetches every block)
  Sys.setenv(GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR", CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif")
  fM <- file.path(AD_OUT, "forest", paste0(box, "_mix.tif"))
  if(!file.exists(fM) || AD_FORCE){
    m <- terra::crop(terra::rast(AD_MIX_URL), terra::ext(g) + 100)
    terra::writeRaster(m, fM, overwrite = TRUE)
  }
  mix <- terra::rast(fM)
  fH <- file.path(AD_OUT, "forest", paste0(box, "_height.tif"))
  if(!file.exists(fH) || AD_FORCE)
    terra::writeRaster(terra::crop(terra::rast(AD_VEGH), terra::ext(g) + 100), fH, overwrite = TRUE)
  hgt <- terra::rast(fH)
  #both rasters are 10 m LV95 but on their own origins: to the layer grid
  mix <- terra::resample(mix, g, method = "near")
  hgt <- terra::resample(hgt, g, method = "bilinear")
  forest <- !is.na(mix)
  w <- matrix(1, STAND_WINDOW, STAND_WINDOW)
  #stand means over the forest cells of the window
  mixS <- terra::focal(mix, w, fun = "mean", na.rm = TRUE)
  hF   <- terra::mask(hgt, forest, maskvalues = FALSE)
  hS   <- terra::focal(hF, w, fun = "mean", na.rm = TRUE)
  type  <- terra::ifel(mixS < MIX_LIMITS[1], 1, terra::ifel(mixS > MIX_LIMITS[2], 2, 3))
  phase <- terra::classify(hS, cbind(PHASE_BREAKS[-5], PHASE_BREAKS[-1], 1:4), right = FALSE)
  fma   <- terra::ifel(L[["forestReserve"]] > 0, 1, 2)
  #the table, as one lookup: code = type * 100 + phase * 10 + fma
  code <- type * 100 + phase * 10 + fma
  lut <- do.call(rbind, lapply(1:3, function(t) do.call(rbind, lapply(1:4, function(p)
    data.frame(code = t * 100 + p * 10 + 1:2, score = EDWARDS_CE[[t]][p, ])))))
  score <- terra::classify(code, as.matrix(lut), others = NA)
  score <- terra::mask(score, forest, maskvalues = FALSE)
  names(score) <- "forestScore"
  parts <- c(terra::mask(mixS, forest, maskvalues = FALSE), hS, terra::mask(type, forest, maskvalues = FALSE),
             terra::mask(phase, forest, maskvalues = FALSE), terra::mask(fma, forest, maskvalues = FALSE))
  names(parts) <- c("broadleavedPct", "canopyM", "type", "phase", "fma")
  terra::writeRaster(score, fS, overwrite = TRUE)
  terra::writeRaster(parts, fA, overwrite = TRUE)
  fr <- terra::freq(score)
  logf("forest %s: %.0f ha of forest; score mean %.2f; by score: %s", box,
       sum(fr$count) / 100, sum(fr$value * fr$count) / sum(fr$count),
       paste(sprintf("%g=%.0f%%", fr$value, 100 * fr$count / sum(fr$count)), collapse = " "))
  list(score = terra::rast(fS), parts = terra::rast(fA))
}

describeAreas <- function(ctx){
  box <- if(ctx %in% names(AD_BOX)) AD_BOX[[ctx]] else ctx
  a <- readRDS(file.path(AD_AOI, "aoi", paste0(ctx, ".rds")))
  p <- sf::st_transform(a$polys[, "AOI"], 2056)
  L <- terra::rast(layersFile(box))
  bb <- sf::st_bbox(p); e <- terra::ext(L)
  if(bb[["xmin"]] < e[1] || bb[["xmax"]] > e[2] || bb[["ymin"]] < e[3] || bb[["ymax"]] > e[4])
    stop(ctx, ": the areas reach beyond layers2_", box, ".tif")
  fr <- forestRasters(box)
  score <- fr$score; parts <- fr$parts
  isF <- !is.na(score)
  #the band of the lakes of >= 10 ha: lakes_<box>.rds, beside the 37-element layers
  fl <- file.path(dirname(dirname(dirname(layersFile(box)))), "layers", paste0("lakes_", box, ".rds"))
  if(!file.exists(fl)) stop("no ", fl)
  lb <- readRDS(fl)$band
  lake <- if(is.null(lb)) L[[1]] * 0 else terra::rasterize(terra::vect(lb), L[[1]], field = 1, background = 0)
  S <- c(lake > 0,
         (L[["forestReserve"]] > 0) | (L[["birdSanctuary"]] > 0) | (L[["wetlands"]] > 0),
         L[["summitsViews"]] > 0, isF, terra::ifel(isF, score / 10, 0),
         parts[["type"]] == 1, parts[["type"]] == 2, parts[["type"]] == 3,
         parts[["phase"]] <= 2, parts[["phase"]] == 3, parts[["phase"]] == 4, parts[["fma"]] == 1)
  names(S) <- c("lakeShare", "protShare", "viewShare", "forestShare", "forestAttr",
                "f_conifer", "f_broadleaved", "f_mixed", "f_young", "f_medium", "f_adult", "f_reserve")
  v <- terra::extract(S, terra::vect(p), fun = mean, na.rm = TRUE, ID = FALSE)
  sc <- terra::extract(score, terra::vect(p), fun = mean, na.rm = TRUE, ID = FALSE)[[1]]
  d <- data.frame(AOI = as.character(p$AOI), ha = as.numeric(sf::st_area(p)) / 1e4, v, forestScore = sc,
                  stringsAsFactors = FALSE)
  d$forestScore[!is.finite(d$forestScore)] <- NA
  for(k in c("f_conifer", "f_broadleaved", "f_mixed", "f_young", "f_medium", "f_adult", "f_reserve")) d[[k]][!is.finite(d[[k]])] <- NA
  saveRDS(list(ctx = ctx, box = box, desc = d, aoiStamp = a$stamp, source = AD_AOI,
               forestModel = "Edwards et al. 2012 Ecology and Society 17(1):27, Table 1, Central Europe",
               phaseBreaks = PHASE_BREAKS, mixLimits = MIX_LIMITS, made = format(Sys.time())),
          file.path(AD_OUT, paste0("desc_", ctx, ".rds")))
  utils::write.csv(d, file.path(AD_OUT, paste0("desc_", ctx, ".csv")), row.names = FALSE)
  logf("%s (%s): %d areas; lake zone in %d, forest share median %.2f, forest score median %.2f (range %.1f-%.1f)",
       ctx, a$stamp, nrow(d), sum(d$lakeShare > 0.05), stats::median(d$forestShare),
       stats::median(d$forestScore, na.rm = TRUE), min(d$forestScore, na.rm = TRUE), max(d$forestScore, na.rm = TRUE))
  invisible(d)
}

if(sys.nframe() == 0L){
  for(ctx in AD_CTX) describeAreas(ctx)
  logf("area_descriptors: done, %s", AD_OUT)
}
