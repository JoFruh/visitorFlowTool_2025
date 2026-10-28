#### Area DULN under other aggregations, per agent type, for the area-choice test ####
#
# The area choice (findClosestAOI_cpp) reads ONE value per area: the Nature walker
# band's (median of the top quarter + median of all cells) / 2 (generateAoI2.R,
# areaSummary() in tailor_duln.R), for every agent type. The user (2026-10-27) wants
# small outstanding features (a beautiful pond in a large, plain park) to count more,
# and asks whether each type should read its own band. This rebuilds a typed DULN
# variant's per-type rasters at Glatt exactly as stageVariantsTypes() does (band
# without lakes + the variant's delta raster), checks them by reproducing the
# variant's stored DULN_<type> of every area, and writes per area and type:
#   stored  the variant's DULN_<type> (what the engine reads; walkNat = polys$DULN)
#   cur     the current rule on the rebuilt raster, unanchored
#   med     the median of all cells
#   top10 / top5   the median of the top 10 % / 5 % of cells
#   p95 / max      the 95th percentile / the maximum
#   excess  the area of high-value land: sum over cells of max(0, x - Q) in ha, Q the
#           type's 75th percentile over all Glatt area cells
# Read by data-raw/test_area_duln_fit.R. Builds nothing, writes only VFT_AD_OUT.
#
#   VFT_AD_BASE=<folder the variant was applied to> VFT_AD_VARIANT=<v> VFT_AD_TD=<tailored>
#   VFT_AD_SCRIPT=<script defining the variant> VFT_AD_OUT=<rds> Rscript data-raw/test_area_duln_aggs.R

BASE   <- Sys.getenv("VFT_AD_BASE")
VNAME  <- Sys.getenv("VFT_AD_VARIANT")
TDIR   <- Sys.getenv("VFT_AD_TD")
SCRIPT <- Sys.getenv("VFT_AD_SCRIPT", "tailor_duln_types.R")
AD_OUT    <- Sys.getenv("VFT_AD_OUT")
if(!nzchar(BASE) || !nzchar(VNAME) || !nzchar(TDIR) || !nzchar(AD_OUT)) stop("set VFT_AD_BASE, _VARIANT, _TD, _OUT")
AD_CTX <- "glatt"

#the variant's definitions as apply_duln_variant.R loads them, no stage run
Sys.setenv(VFT_VAL_OUT = file.path(BASE, "base_todayDULN"), VFT_TD_OUT = TDIR, VFT_TY_OUT = file.path(BASE, "duln_variant"),
           VFT_TY_STAGES = "none", VFT_ACT_STAGES = "none", VFT_EL_STAGES = "none", VFT_ST_STAGES = "none",
           VFT_OSM_STAGES = "none", VFT_VAL_CONTEXTS = AD_CTX)
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", SCRIPT))
stopifnot(VNAME %in% names(TY_VARIANTS))
sp <- TY_VARIANTS[[VNAME]]

#### the per-type rasters, as stageVariantsTypes() builds them ####

ft <- readRDS(tyPath("fit_types.rds"))
ex <- readRDS(extractFile(AD_CTX))
aoi <- readRDS(aoiFile(AD_CTX))
tw <- typeWeights(ft$all, readRDS(extractFile("glatt")))
tm <- cogTemplate(AD_CTX)
lakes <- ns$.vftReadLakesWithin(sf::st_transform(aoi$shapeLarger, 2056))
lakeV <- if(nrow(lakes)) terra::vect(sf::st_transform(lakes, 4326)) else NULL
cog <- terra::crop(terra::rast(COG), tm)
bandNoLakes <- function(k){ b <- cog[[BAND_OF[[k]]]]; if(!is.null(lakeV)) b[lakeV] <- NA; b }
polys <- aoi$polys
today <- lapply(stats::setNames(TYPES7, TYPES7), function(k) areaSummary(bandNoLakes(k), polys))
todayType <- lapply(stats::setNames(TYPES7, TYPES7), function(k) polys$DULN + today[[k]] - today$walkNat)

R <- list()
for(k in TYPES7){
  if(identical(sp$kind, "today")){ R[[k]] <- bandNoLakes(k); next }
  wNew <- if(identical(sp$kind, "shared")) xlsxNew(k) + S_TY * tw$shared - xlsxNew("walkNat") else S_TY * tw$perType[[k]]
  m <- sp$manual[[k]]
  if(length(m)) wNew[names(m)] <- m
  d <- typeDelta(wNew, k)
  old <- drop(ex$E[, names(xlsxOld(k))] %*% xlsxOld(k))
  b0 <- mean(old) - mean(old + drop(ex$E[, names(d)] %*% d))
  R[[k]] <- bandNoLakes(k) + to4326(siteRaster(AD_CTX, d, b0), tm)
}

#the check: the variant's stored per-type area DULN, reproduced
stored <- readRDS(file.path(BASE, "aoi", paste0(AD_CTX, ".rds")))$polys
stopifnot(identical(stored$AOI, polys$AOI))
for(k in TYPES7){
  a1 <- areaSummary(R[[k]], polys) - today[[k]]; a1[!is.finite(a1)] <- 0
  rec <- if(identical(sp$kind, "today")) todayType[[k]] else todayType[[k]] + a1 - mean(a1)
  col <- paste0("DULN_", k)
  ref <- if(col %in% names(stored)) stored[[col]] else stored$DULN
  dev <- max(abs(rec - ref))
  logf("%s %s: stored area DULN reproduced to %.2g", VNAME, k, dev)
  if(dev > 1e-6) stop(VNAME, " ", k, ": the rebuilt raster does not reproduce the stored area DULN")
}

#### the aggregations ####

topMed <- function(x, f) stats::median(x[seq_len(max(1L, floor(length(x) * f)))])
cellHa <- mean(terra::values(terra::cellSize(tm, unit = "ha")), na.rm = TRUE)
out <- do.call(rbind, lapply(TYPES7, function(k){
  v <- terra::extract(R[[k]], terra::vect(polys))
  x <- split(v[[2]], v$ID)
  Q <- stats::quantile(v[[2]], .75, na.rm = TRUE)[[1]]
  agg <- t(vapply(as.character(seq_len(nrow(polys))), function(i){
    y <- sort(x[[i]], decreasing = TRUE)
    if(!length(y)) return(rep(NA_real_, 7))
    c(cur = (topMed(y, .25) + stats::median(y)) / 2, med = stats::median(y), top10 = topMed(y, .10),
      top5 = topMed(y, .05), p95 = stats::quantile(y, .95)[[1]], max = y[1], excess = sum(pmax(0, y - Q)) * cellHa)
  }, numeric(7)))
  st <- if(paste0("DULN_", k) %in% names(stored)) stored[[paste0("DULN_", k)]] else stored$DULN
  data.frame(AOI = polys$AOI, type = k, stored = st, agg, row.names = NULL)
}))
saveRDS(list(variant = VNAME, base = BASE, aggs = out, cellHa = cellHa, made = format(Sys.time())), AD_OUT)
logf("area aggregations %s: %d areas x %d types written to %s", VNAME, nrow(polys), length(TYPES7), AD_OUT)
