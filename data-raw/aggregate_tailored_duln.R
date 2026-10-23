#### A tailored DULN, aggregated: a replacement for DULN_nat_majMaxMeanAGGBlur.tif ####
#
# Steps 3-4 draw the areas on maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif ("AGG"), an
# aggregate of today's xlsx Nature-walker DULN: the COG band (0.000239 deg) averaged
# in 4 x 4 blocks to 0.000955 deg (~73 x 106 m) and blurred by a 3 x 3 mean
# reproduces it at r = .997 (measured 2026-10-22, Glatt and Wigger). The tailored
# DULN variants therefore still pick their areas with the xlsx weights. This builds
# the same kind of map from a TAILORED Nature-walker layer (the user's method,
# 2026-10-22):
#   layer      the tailored walkNat at 10 m LV95 per element box (glatt - GlattPark
#              inside - and wigger), by the variants' arithmetic (tailor_duln_types.R
#              stageVariantsTypes): the xlsx layer + the type's delta + b0, b0 keeping
#              the box's site's mean edge DULN. The weights are a Strava fit's
#              nature_walk layer at strength S_TY (default the signed clean fit,
#              <TD>/strava/clean/signed, the user's choice)
#   agg        onto the AGG grid: projected (bilinear) to the 4326 grid at the COG's
#              resolution, aligned to AGG (as to4326 makes the COG), then a 4 x 4
#              moving window (VFT_AG_FUN, default max: a block scores as its best
#              detail, the user's choice) and the 3 x 3 mean blur. 4 x 4 COG cells
#              are one AGG cell, ~7 x 11 cells of the 10 m layer. Per box, and one
#              file on the national AGG grid (NA outside the boxes)
#   check      the same pipeline on the xlsx layer with mean must reproduce AGG
#              (r >= .99 per box); coverage of every perimeter + 1 km
#   threshold  the step-3 threshold whose areas best overlap a reference folder's
#              (default _best_autoCut: threshold 3.1 + the cut on AGG): one threshold
#              for all three sites, the mean Jaccard of the area unions, coarse on the
#              raster, then exact with generateAoI2(split = FALSE) around the best;
#              at the chosen one the cut too (counts, ha, gpkg, a figure)
# Thresholding and the cut's relative dips are unchanged by a positive scale and a
# shift, so S_TY and b0 move the threshold, not the shapes.
#
# Mixed weights (VFT_AG_MIX=paths, the user's solution 2, 2026-10-23, after iter1's
# areas were worse for every model): the Strava fit cannot identify the land cover
# (every fitted cell is on a path), so only the path and route elements (MIX_PATHS:
# the six path classes, hiking trails, bike routes, stairs) take the fit's weights, at
# strength VFT_AG_S (default 1: the fit on the xlsx scale, as typeWeights scales it).
# Everything else - land cover, lakes, amenities, road noise, and the paths' presence
# (narrowPaths ... naturePaths rings, which the clean set's reference class drops) -
# stays today's xlsx layer: tailored = xlsx + (fit - xlsxNew) on MIX_PATHS only.
#
#   Rscript data-raw/aggregate_tailored_duln.R
#
# VFT_AG_STAGES  which stages (default layer, agg, check, threshold)
# VFT_AG_TD      the tailored folder holding the fit (required)
# VFT_AG_FIT     its subfolder with fit_types.rds (default strava/clean/signed)
# VFT_AG_FUN     the 4 x 4 window's function (default max)
# VFT_AG_REF     the validation folder whose areas the threshold matches (default
#                _best_autoCut)
# VFT_AG_TAG     the output's name (default iter1)
# VFT_AG_OUT     output (default Local Data/DULN_agg_tailored/<tag>)
# VFT_AG_STEP    the threshold grid's step (default 0.1)
# VFT_AG_MIX     "paths": the mixed weights above (default empty: the whole fitted layer)
# VFT_AG_S       the fit's strength (default S_TY, 1 with VFT_AG_MIX)

.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
AG_TD <- Sys.getenv("VFT_AG_TD", "")
if(!nzchar(AG_TD)) stop("set VFT_AG_TD (the tailored folder holding the fit)")
AG_STAGES <- strsplit(Sys.getenv("VFT_AG_STAGES", "layer,agg,check,threshold"), ",", fixed = TRUE)[[1]]
AG_FIT <- Sys.getenv("VFT_AG_FIT", "strava/clean/signed")
AG_FUN <- Sys.getenv("VFT_AG_FUN", "max")
AG_TAG <- Sys.getenv("VFT_AG_TAG", "iter1")
AG_LOCAL <- "C:/Users/frueh/Documents/Local Data"
AG_REF <- Sys.getenv("VFT_AG_REF", file.path(AG_LOCAL, "pathUsageValidation_2026_best_autoCut"))
AG_OUT <- Sys.getenv("VFT_AG_OUT", file.path(AG_LOCAL, "DULN_agg_tailored", AG_TAG))
AG_STEP <- as.numeric(Sys.getenv("VFT_AG_STEP", "0.1"))
#the definitions only (weights, layers, the validation's perimeters); the fit folder is
#the validation the tailored folder belongs to
if(!nzchar(Sys.getenv("VFT_VAL_OUT"))) Sys.setenv(VFT_VAL_OUT = dirname(AG_TD))
Sys.setenv(VFT_TD_OUT = AG_TD, VFT_ST_STAGES = "none", VFT_ST_ELEMENTS = "clean", VFT_TY_STAGES = "none",
           VFT_ACT_STAGES = "none", VFT_EL_STAGES = "none")
source(file.path(.repo, "data-raw", "tailor_duln_strava.R"))

AG_MIX <- Sys.getenv("VFT_AG_MIX", "")
if(nzchar(AG_MIX) && !identical(AG_MIX, "paths")) stop("VFT_AG_MIX is empty or paths")
AG_S <- as.numeric(Sys.getenv("VFT_AG_S", if(nzchar(AG_MIX)) "1" else format(S_TY)))
MIX_PATHS <- c(PATH_CLASSES, "hikingTrails", "bikeRoutes", "stairs")
dir.create(AG_OUT, recursive = TRUE, showWarnings = FALSE)
agPath <- function(...) file.path(AG_OUT, ...)
#each element box and the site whose edges set its b0 (GlattPark lies in Glatt's box)
BOX_SITE <- c(glatt = "glatt", wigger = "wigger")
SITES3 <- c("glattPark", "wigger", "glatt")
AGG_FILE <- ns$vftData("maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif")
layer10File <- function(box) agPath(paste0("walkNat_10m_", box, ".tif"))
aggBoxFile <- function(box) agPath(paste0("agg_", box, ".tif"))
NEW_FILE <- agPath(paste0("DULN_tailored_", AG_TAG, "_AGG.tif"))
GDAL_OPT <- c("COMPRESS=DEFLATE", "TILED=YES")

#### stage layer: the tailored walkNat at 10 m per box ####

stageLayer <- function(){
  ft <- readRDS(file.path(AG_TD, AG_FIT, "fit_types.rds"))$all
  tw <- typeWeights(ft, readRDS(extractFile("glatt")))
  #the nature_walk group's layer at strength AG_S (no hand-set weights for walkers)
  wFit <- AG_S * tw$perType$walkNat
  wo <- xlsxOld("walkNat")
  wx <- xlsxNew("walkNat")
  if(identical(AG_MIX, "paths")){
    miss <- setdiff(MIX_PATHS, intersect(names(wFit), names(wx)))
    if(length(miss)) stop("path elements missing from the fit or xlsxNew: ", paste(miss, collapse = ", "))
    wNew <- wx
    wNew[MIX_PATHS] <- wFit[MIX_PATHS]
    d <- wFit[MIX_PATHS] - wx[MIX_PATHS]
  } else {
    wNew <- wFit
    d <- typeDelta(wNew, "walkNat")
  }
  d <- d[abs(d) > 1e-12]
  rows <- list()
  for(box in names(BOX_SITE)){
    X <- terra::rast(layersFile(box))
    miss <- setdiff(c(names(d), names(wo)[wo != 0]), names(X))
    if(length(miss)) stop(box, ": elements missing from ", layersFile(box), ": ", paste(miss, collapse = ", "))
    ex <- readRDS(extractFile(BOX_SITE[[box]]))
    old <- drop(ex$E[, names(wo)] %*% wo)
    new <- old + drop(ex$E[, names(d)] %*% d)
    b0 <- mean(old) - mean(new)
    f <- layer10File(box)
    if(!file.exists(f)){
      o10 <- weighted(X, wo)
      t10 <- o10 + weighted(X, d) + b0
      terra::writeRaster(c(t10, o10), f, names = c("tailored", "xlsx"), overwrite = TRUE, gdal = GDAL_OPT)
    }
    rows[[box]] <- data.frame(box = box, site = BOX_SITE[[box]], b0 = b0, edges = length(old),
                              edgeMeanXlsx = mean(old), edgeMeanTailored = mean(new + b0),
                              edgeSdXlsx = stats::sd(old), edgeSdTailored = stats::sd(new), rEdge = stats::cor(old, new))
  }
  lt <- do.call(rbind, rows)
  utils::write.csv(lt, agPath("layer.csv"), row.names = FALSE)
  els <- union(names(wo), names(wNew))
  z <- function(w) ifelse(is.na(w), 0, w)
  wt <- data.frame(element = els, xlsx = z(unname(wo[els])), xlsxNew = z(unname(wx[els])), fit = z(unname(wFit[els])),
                   tailored = z(unname(wNew[els])), delta = z(unname(d[els])))
  utils::write.csv(wt[order(-abs(wt$delta), -abs(wt$tailored)), ], agPath("weights_walkNat.csv"), row.names = FALSE)
  logf("layer: the %s nature_walk layer at strength %.1f (%s)%s; per box (b0 keeps the site's mean edge DULN):",
       AG_FIT, AG_S, AG_TD, if(nzchar(AG_MIX)) paste0(", mixed: the fit on ", paste(MIX_PATHS, collapse = " "),
       " only, the xlsx elsewhere") else "")
  print(lt, digits = 4, row.names = FALSE)
}

#### stage agg: onto the AGG grid ####

#the AGG grid over a box, kept 400 m inside it (the LV95 box is not lon/lat-aligned)
aggTemplate <- function(box){
  e <- terra::ext(terra::rast(layersFile(box)))
  e <- terra::ext(e$xmin + 400, e$xmax - 400, e$ymin + 400, e$ymax - 400)
  p <- terra::as.polygons(e, crs = "EPSG:2056")
  terra::crop(terra::rast(AGG_FILE), terra::ext(terra::project(p, "EPSG:4326")), snap = "in")
}
#a 10 m LV95 layer onto the template's (AGG) grid: the COG's own step (project
#bilinear to half the COG cell, mean of 2 x 2) on a grid aligned to AGG, then the
#4 x 4 window and the 3 x 3 mean blur
aggOf <- function(r10, tmpl, fun){
  fine <- terra::disagg(terra::rast(tmpl), 8)
  p <- terra::project(r10, fine, method = "bilinear")
  cog <- terra::aggregate(p, 2, fun = "mean", na.rm = TRUE)
  a <- terra::aggregate(cog, 4, fun = fun, na.rm = TRUE)
  b <- terra::focal(a, 3, fun = "mean", na.rm = TRUE)
  if(!isTRUE(terra::compareGeom(b, tmpl, stopOnError = FALSE))) stop("the aggregate is off the AGG grid")
  b
}

stageAgg <- function(){
  parts <- list()
  for(box in names(BOX_SITE)){
    f <- aggBoxFile(box)
    if(!file.exists(f)){
      t0 <- proc.time()[["elapsed"]]
      tm <- aggTemplate(box)
      L <- terra::rast(layer10File(box))
      out <- c(aggOf(L[["tailored"]], tm, AG_FUN), aggOf(L[["xlsx"]], tm, "mean"),
               aggOf(L[["tailored"]], tm, "mean"), aggOf(L[["xlsx"]], tm, AG_FUN), tm)
      terra::writeRaster(out, f, names = c("tailored", "xlsx_mean", "tailored_mean", "xlsx_win", "AGG"),
                         overwrite = TRUE, gdal = GDAL_OPT)
      logf("agg %s: %d x %d AGG cells, %.0f s", box, terra::nrow(tm), terra::ncol(tm), proc.time()[["elapsed"]] - t0)
    }
    parts[[box]] <- terra::rast(f)[["tailored"]]
  }
  #one file on the national AGG grid, NA outside the boxes: a drop-in for VFT_VAL_DULN_ALL
  if(!file.exists(NEW_FILE)){
    full <- terra::rast(AGG_FILE)
    m <- terra::merge(terra::sprc(unname(parts)))
    m <- terra::extend(m, full)
    m <- terra::crop(m, full)
    if(!isTRUE(terra::compareGeom(m, full, stopOnError = FALSE))) stop("the merged map is off the AGG grid")
    names(m) <- "tailored"
    terra::writeRaster(m, NEW_FILE, overwrite = TRUE, gdal = GDAL_OPT)
    logf("agg: written %s (window %s 4 x 4 COG cells = one AGG cell, ~7 x 11 cells at 10 m; 3 x 3 mean blur)",
         NEW_FILE, AG_FUN)
  }
}

#### stage check ####

stageCheck <- function(){
  rows <- list()
  for(box in names(BOX_SITE)){
    R <- terra::rast(aggBoxFile(box))
    v <- terra::values(R)
    v <- v[stats::complete.cases(v), , drop = FALSE]
    r <- function(a, b) stats::cor(v[, a], v[, b])
    q <- function(a) stats::quantile(v[, a], c(.1, .5, .9))
    rows[[box]] <- data.frame(box = box, cells = nrow(v),
      r_xlsxMean_AGG = r("xlsx_mean", "AGG"), r_xlsxWin_AGG = r("xlsx_win", "AGG"), r_tailored_xlsxMean = r("tailored", "xlsx_mean"),
      r_tailored_AGG = r("tailored", "AGG"), r_tailoredMean_AGG = r("tailored_mean", "AGG"),
      AGG_q10 = q("AGG")[[1]], AGG_q50 = q("AGG")[[2]], AGG_q90 = q("AGG")[[3]],
      new_q10 = q("tailored")[[1]], new_q50 = q("tailored")[[2]], new_q90 = q("tailored")[[3]])
  }
  ck <- do.call(rbind, rows)
  utils::write.csv(ck, agPath("check.csv"), row.names = FALSE)
  logf("check: the pipeline against AGG (r over the box) and the value distributions:")
  print(ck, digits = 3, row.names = FALSE)
  if(any(ck$r_xlsxMean_AGG < 0.99)) stop("the xlsx layer through the mean pipeline does not reproduce AGG (r < .99)")
  #coverage: what generateAoI2 reads, the perimeter + 1 km
  N <- terra::rast(NEW_FILE)
  for(ctx in SITES3){
    bufV <- terra::buffer(terra::vect(readPerimeter(CTX[[ctx]]$perimeter)), 1000)
    cv <- terra::mask(terra::crop(N, bufV, snap = "out"), bufV)
    inB <- terra::mask(terra::init(cv, 1), bufV)
    nMiss <- sum(terra::values(is.na(cv) & !is.na(inB), mat = FALSE), na.rm = TRUE)
    logf("check %s: %d NA cells of the new map inside the perimeter + 1 km", ctx, nMiss)
    if(nMiss > 0) stop(ctx, ": the new map does not cover the perimeter + 1 km")
  }
}

#### stage threshold ####

#a site as the validation's aoi stage reads it (R/providers.R crops)
siteInputs <- function(ctx, file){
  shape <- readPerimeter(CTX[[ctx]]$perimeter)
  sl <- sf::st_union(sf::st_buffer(sf::st_transform(shape, "EPSG:3857"), dist = 1000))
  D <- terra::crop(terra::rast(file), sf::st_transform(sl, "epsg:4326"))
  W <- terra::crop(terra::rast(ns$vftData("maps/attr/allAttrs_COG_final.tif"))[[4]],
                   terra::project(terra::vect(sl), "EPSG:4326"))
  names(W) <- "walkNat"
  list(shape = shape, D = D, W = W)
}
refUnion <- function(ctx){
  p <- readRDS(file.path(AG_REF, "aoi", paste0(ctx, ".rds")))$polys
  sf::st_union(sf::st_make_valid(sf::st_transform(sf::st_geometry(p), 2056)))
}
jaccard <- function(a, b){
  i <- as.numeric(sf::st_area(sf::st_intersection(a, b)))
  u <- as.numeric(sf::st_area(sf::st_union(a, b)))
  if(!length(i)) i <- 0
  i / u
}
areasAt <- function(si, thr, split) suppressWarnings(ns$generateAoI2(minThresh = thr, perimeter = si$shape,
  walkNat = si$W, DULN_all = si$D, split = split))$polygons
unionOf <- function(p) sf::st_union(sf::st_make_valid(sf::st_transform(sf::st_geometry(p), 2056)))

stageThreshold <- function(){
  t0 <- proc.time()[["elapsed"]]
  si <- lapply(stats::setNames(SITES3, SITES3), siteInputs, file = NEW_FILE)
  ru <- lapply(stats::setNames(SITES3, SITES3), refUnion)
  #coarse: cells at or above the threshold inside the perimeter + 1 km against the
  #reference areas rasterised on the same grid
  rng <- range(unlist(lapply(si, function(s) stats::quantile(terra::values(s$D, mat = FALSE), c(.01, .99), na.rm = TRUE))))
  grid <- seq(floor(rng[1] / AG_STEP) * AG_STEP, ceiling(rng[2] / AG_STEP) * AG_STEP, by = AG_STEP)
  coarse <- sapply(SITES3, function(ctx){
    s <- si[[ctx]]
    bufV <- terra::buffer(terra::vect(s$shape), 1000)
    v <- terra::values(terra::mask(s$D, bufV), mat = FALSE)
    ref <- terra::values(terra::rasterize(terra::project(terra::vect(ru[[ctx]]), terra::crs(s$D)), s$D, field = 1),
                         mat = FALSE)
    ref <- !is.na(ref) & !is.na(v)
    vapply(grid, function(t){ a <- !is.na(v) & v >= t; sum(a & ref) / max(sum(a | ref), 1) }, 1)
  })
  cs <- data.frame(threshold = grid, coarse, mean = rowMeans(coarse))
  utils::write.csv(cs, agPath("threshold_coarse.csv"), row.names = FALSE)
  tc <- grid[which.max(cs$mean)]
  logf("threshold, coarse (raster): best %.2f, mean Jaccard %.3f (%s)", tc, max(cs$mean),
       paste(sprintf("%s %.3f", SITES3, coarse[which.max(cs$mean), ]), collapse = ", "))
  #exact: generateAoI2's own areas around it
  fine <- round(seq(tc - 4 * AG_STEP, tc + 4 * AG_STEP, by = AG_STEP), 6)
  ex <- do.call(rbind, lapply(fine, function(t){
    j <- vapply(SITES3, function(ctx){ p <- areasAt(si[[ctx]], t, FALSE); if(!nrow(p)) 0 else jaccard(unionOf(p), ru[[ctx]]) }, 1)
    data.frame(threshold = t, t(j), mean = mean(j))
  }))
  utils::write.csv(ex, agPath("threshold_exact.csv"), row.names = FALSE)
  print(ex, digits = 3, row.names = FALSE)
  tStar <- ex$threshold[which.max(ex$mean)]
  if(tStar %in% range(fine)) logf("WARNING threshold: the best exact threshold %.2f is at the edge of the refined grid", tStar)
  #the reference's own recipe on AGG at its threshold: how far union-matching can go
  thrRef <- readRDS(file.path(AG_REF, "aoi", "glatt.rds"))$thr
  siOld <- lapply(stats::setNames(SITES3, SITES3), siteInputs, file = AGG_FILE)
  jOld <- vapply(SITES3, function(ctx) jaccard(unionOf(areasAt(siOld[[ctx]], thrRef, FALSE)), ru[[ctx]]), 1)
  #at the chosen threshold, the cut too
  res <- do.call(rbind, lapply(SITES3, function(ctx){
    ref <- readRDS(file.path(AG_REF, "aoi", paste0(ctx, ".rds")))$polys
    un <- areasAt(si[[ctx]], tStar, FALSE)
    ct <- areasAt(si[[ctx]], tStar, TRUE)
    sf::st_write(ct, agPath(paste0("areas_", ctx, "_t", tStar, ".gpkg")), delete_dsn = TRUE, quiet = TRUE)
    data.frame(site = ctx, threshold = tStar, jaccard = jaccard(unionOf(un), ru[[ctx]]), jaccardRefOnAGG = jOld[[ctx]],
               nUnsplit = nrow(un), nCut = nrow(ct), ha = sum(as.numeric(sf::st_area(sf::st_transform(ct, 2056)))) / 1e4,
               refN = nrow(ref), refHa = sum(ref$area) / 1e4,
               jaccardCutUnion = jaccard(unionOf(ct), ru[[ctx]]))
  }))
  utils::write.csv(res, agPath("threshold.csv"), row.names = FALSE)
  writeLines(format(tStar), agPath("threshold.txt"))
  logf("threshold: T* = %.2f (mean Jaccard %.3f); the reference recipe on AGG at %.1f: %s. %.0f s",
       tStar, max(ex$mean), thrRef, paste(sprintf("%s %.3f", SITES3, jOld), collapse = ", "), proc.time()[["elapsed"]] - t0)
  print(res, digits = 3, row.names = FALSE)
  thresholdFigure(si, tStar)
}

#old areas (the reference) against the new ones at T*, over the new map
thresholdFigure <- function(si, tStar){
  ps <- lapply(SITES3, function(ctx){
    ref <- sf::st_transform(readRDS(file.path(AG_REF, "aoi", paste0(ctx, ".rds")))$polys, 2056)
    nw <- sf::st_transform(sf::st_read(agPath(paste0("areas_", ctx, "_t", tStar, ".gpkg")), quiet = TRUE), 2056)
    D <- terra::project(si[[ctx]]$D, "EPSG:2056")
    df <- as.data.frame(D, xy = TRUE); names(df)[3] <- "v"
    ggplot2::ggplot() +
      ggplot2::geom_raster(data = df, ggplot2::aes(x, y, fill = v)) +
      ggplot2::scale_fill_gradient(low = "grey97", high = "grey45", guide = "none") +
      ggplot2::geom_sf(data = sf::st_transform(si[[ctx]]$shape, 2056), fill = NA, colour = "black", linewidth = .3, linetype = 2) +
      ggplot2::geom_sf(data = ref, fill = NA, colour = "#d95f02", linewidth = .35) +
      ggplot2::geom_sf(data = nw, fill = NA, colour = "#1b9e77", linewidth = .35) +
      ggplot2::coord_sf(datum = NA) + ggplot2::labs(title = ctx, x = NULL, y = NULL,
        subtitle = sprintf("orange: reference (%d areas), green: new map at %.2f (%d areas)", nrow(ref), tStar, nrow(nw))) +
      ggplot2::theme_void(base_size = 8)
  })
  p <- ggpubr::ggarrange(plotlist = ps, nrow = 1)
  ggplot2::ggsave(agPath("fig_areas_threshold.png"), p, width = 30, height = 11, units = "cm", dpi = 250, bg = "white")
}

#### run ####

t00 <- proc.time()[["elapsed"]]
if("layer" %in% AG_STAGES) stageLayer()
if("agg" %in% AG_STAGES) stageAgg()
if("check" %in% AG_STAGES) stageCheck()
if("threshold" %in% AG_STAGES) stageThreshold()
logf("aggregate_tailored_duln: done in %.1f min, output in %s", (proc.time()[["elapsed"]] - t00) / 60, AG_OUT)
