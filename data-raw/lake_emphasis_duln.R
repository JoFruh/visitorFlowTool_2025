#### Lake emphasis on the Glatt-calibrated DULN ####
#
# Greifensee (Glatt area AP) and Katzensee (GlattPark area X) stay under-simulated by
# every model: survey 18.9 % / 7.8 % of the in-area usage, Full at alpha 5 5.7 % /
# 3.0 %. This asks whether a much higher DULN round the lakes draws the agents there
# and outlines the shores as the survey does. It multiplies the DULN layer inside the
# lakes' buffer by a factor f, on top of the Glatt-calibrated variant T_shared_s0.5
# (data-raw/tailor_duln_types.R), and writes a variant folder (aoi/ + network/) for
# data-raw/prepare_variant_validation.R. Diagnostic only: nothing in the package changes.
#
# The buffer: the lakes of at least 10 ha (VFT_AOI_LAKE_MIN_M2), from the water out to
# tailor_duln.R's lake band - the path loop round the lake (Greifensee, ~310 m from the
# water) or the 150 m ring (Katzensee: no loop) - joined with the 150 m ring of the same
# lakes, as the band layer is. Rivers and smaller lakes are left out.
# What is multiplied: every DULN value the engine reads inside the buffer.
#   edges  the stored edge DULN itself, every type column: x (1 + (f - 1) s), s the
#          share of the line inside the buffer. NOT the layer under it: the edge DULN
#          is the layer's mean + 34.2423, so doubling the layer alone raised the
#          Katzensee paths by 9 % (37.3 -> 40.5), no emphasis at all.
#   nodes  the node DULN is the layer itself (no offset): the variant's Nature walker
#          layer at 10 m, N = X w' + b0, becomes f N in the buffer where N > 0
#          (negative cells stay), dN = (f - 1) max(N, 0), through the 4326 step.
#   areas  generateAoI2's summary (lakes NA) of that same doubled layer, every
#          DULN_<type> column (the delta rule).
# The site's mean edge DULN (per column) and mean area DULN are kept (a shift b1 over
# the whole site, the areas re-centred): it moves where DULN is high, not its level.
#
#   VFT_LE_FACTOR=2 Rscript data-raw/lake_emphasis_duln.R
#
# VFT_LE_FACTOR  the factor (default 2)
# VFT_LE_OUT     the variant folder (default <types>/variants/T_shared_s0.5_lakes<f>)

Sys.setenv(VFT_TY_STAGES = "none")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
#tailor_duln_types.R's definitions (it sources tailor_duln.R and validate_usage_surveys.R)
source(file.path(.repo, "data-raw", "tailor_duln_types.R"))

LE_F   <- envNum("VFT_LE_FACTOR", 2)
SRC_V  <- tyVariantDir("T_shared_s0.5")
LE_OUT <- Sys.getenv("VFT_LE_OUT", tyVariantDir(paste0("T_shared_s0.5_lakes", LE_F)))
stopifnot(LE_F > 1, dir.exists(SRC_V))

#the buffer on the box's 10 m grid: 1 inside, 0 elsewhere
lakeMask <- function(box, grid){
  lb <- readRDS(lakesFile(box))
  if(is.null(lb$band)) return(NULL)
  zone <- sf::st_union(c(sf::st_geometry(lb$band), sf::st_buffer(lb$lakes, LAKE_RING)))
  terra::rasterize(terra::vect(sf::st_sf(geometry = zone)), grid, field = 1, background = 0)
}

log <- list()
for(ctx in CONTEXTS){
  t0 <- proc.time()[["elapsed"]]
  ex <- readRDS(extractFile(ctx))
  aoi0 <- readRDS(aoiFile(ctx)); base <- readRDS(netFile(ctx))       #today's (_final_timefix)
  aoiV <- readRDS(file.path(SRC_V, "aoi", paste0(ctx, ".rds")))
  netV <- readRDS(file.path(SRC_V, "network", paste0(ctx, "_net.rds")))$net
  stopifnot(identical(aoiV$stamp, aoi0$stamp))

  #the variant's Nature walker weights and shift, as stageVariantsTypes() made them
  ft <- readRDS(tyPath("fit_types.rds"))
  tw <- typeWeights(if(ctx == "glattPark") ft$noGP else ft$all, readRDS(extractFile("glatt")))
  wNew <- xlsxNew("walkNat") + S_TY * tw$shared - xlsxNew("walkNat")
  d <- typeDelta(wNew, "walkNat")
  old <- drop(ex$E[, names(xlsxOld("walkNat"))] %*% xlsxOld("walkNat"))
  b0 <- mean(old) - mean(old + drop(ex$E[, names(d)] %*% d))
  #check: those reproduce the variant's edge DULN
  eB <- igraph::edge_attr(base$net, "DULN_WALK_"); eV <- igraph::edge_attr(netV, "DULN_WALK_")
  dE0 <- drop(ex$E[, names(d)] %*% d) + b0
  err <- max(abs(eB + dE0 - eV))
  logf("lakes %s: the variant's walkNat delta rebuilt, max |edge error| %.2e", ctx, err)
  if(err > 1e-6) stop(ctx, ": the variant's weights are not reproduced")

  #the variant's 10 m layer and the lake change
  X <- terra::rast(layersFile(BOX_OF[[ctx]]))
  dS <- siteRaster(ctx, d, b0)
  N <- terra::crop(weighted(X, xlsxOld("walkNat")), dS) + dS
  M <- lakeMask(BOX_OF[[ctx]], N)
  aoiOut <- aoiV; netOut <- netV
  if(is.null(M) || terra::global(M, "max", na.rm = TRUE)[[1]] == 0){
    logf("lakes %s: no lake of 10 ha or more near the site - the variant's inputs, unchanged", ctx)
  }else{
    dL <- (LE_F - 1) * terra::clamp(N, lower = 0, values = TRUE) * M
    names(dL) <- "dL"
    #edges: the share of the line in the buffer (tailor_duln.R's extraction)
    g <- edgeGeom2056(netV)
    sh <- exactextractr::exact_extract(M, sf::st_buffer(g, 0.5), "mean", progress = FALSE)
    sh[!is.finite(sh)] <- 0
    eL <- (LE_F - 1) * sh * eV
    b1 <- -mean(eL)
    #nodes: through the 4326 step
    tm <- cogTemplate(ctx)
    d4L <- to4326(dL, tm)
    va <- igraph::vertex_attr(netV)
    dV <- terra::extract(d4L, cbind(va$X, va$Y)); dV <- dV[[ncol(dV)]]; dV[!is.finite(dV)] <- 0
    #areas: the variant's walkNat in 4326 with the lakes NA, before and after
    lakes <- ns$.vftReadLakesWithin(sf::st_transform(aoiV$shapeLarger, 2056))
    wn <- terra::crop(terra::rast(COG), tm)[[BAND_OF[["walkNat"]]]]
    if(nrow(lakes)) wn[terra::vect(sf::st_transform(lakes, 4326))] <- NA
    wnV <- wn + to4326(dS, tm)
    dA <- areaSummary(wnV + d4L, aoiV$polys) - areaSummary(wnV, aoiV$polys)
    dA[!is.finite(dA)] <- 0
    dAraw <- mean(dA); dA <- dA - dAraw
    #write them in
    cls <- class(netV)
    ea <- igraph::edge_attr(netV); vv <- igraph::vertex_attr(netV)
    for(cn in ns$VFT_DULN_COLS){
      e1 <- (LE_F - 1) * sh * ea[[cn]]
      ea[[cn]] <- ea[[cn]] + e1 - mean(e1)
      vv[[cn]] <- vv[[cn]] + dV
    }
    polys <- aoiV$polys
    for(k in TYPES7) polys[[paste0("DULN_", k)]] <- polys[[paste0("DULN_", k)]] + dA
    stopifnot(isTRUE(all.equal(polys$DULN + dA, polys$DULN_walkNat)))
    polys$DULN <- polys$DULN_walkNat
    #car parks carry their area's DULN (clamped at 0, as the variant does)
    pk <- which(va$parkingAttr != 0 & is.finite(va$parkingAttr))
    pkPoly <- match(va$parkingAttr[pk], aoiV$polys$DULN)
    if(anyNA(pkPoly)) stop(ctx, ": car-park nodes without their area")
    vv$parkingAttr[pk] <- pmax(polys$DULN[pkPoly], 0)
    igraph::edge_attr(netOut) <- ea; igraph::vertex_attr(netOut) <- vv
    class(netOut) <- cls
    netOut <- ns$vftRefreshAttrWeights(netOut, aoiV$thr)
    aoiOut$polys <- polys
    inBuf <- eL > 0
    log[[ctx]] <- data.frame(ctx = ctx, factor = LE_F, maskHa = terra::global(M, "sum")[[1]] * 100 / 1e4,
      edgesInBuffer = sum(inBuf), edgeKmInBuffer = sum(ea$SHAPE_Leng[inBuf]) / 1000, b1 = b1,
      edgeWalk_mean_before = mean(eV), edgeWalk_mean_after = mean(ea$DULN_WALK_),
      edgeWalk_inBuf_before = mean(eV[inBuf]), edgeWalk_inBuf_after = mean(ea$DULN_WALK_[inBuf]),
      edgeWalk_out_before = mean(eV[!inBuf]), edgeWalk_out_after = mean(ea$DULN_WALK_[!inBuf]),
      layer_inBuf_mean = terra::global(terra::mask(N, M, maskvalues = 0), "mean", na.rm = TRUE)[[1]],
      nodesChanged = sum(abs(dV) > 1e-9), dArea_level = dAraw, dArea_sd = stats::sd(dA))
    top <- order(dA, decreasing = TRUE)[1:min(5, length(dA))]
    logf("lakes %s: buffer %.0f ha, %d edges (%.1f km) inside, edge DULN there %.1f -> %.1f, elsewhere %.2f -> %.2f; areas up most: %s",
         ctx, log[[ctx]]$maskHa, sum(inBuf), log[[ctx]]$edgeKmInBuffer, log[[ctx]]$edgeWalk_inBuf_before,
         log[[ctx]]$edgeWalk_inBuf_after, log[[ctx]]$edgeWalk_out_before, log[[ctx]]$edgeWalk_out_after,
         paste(sprintf("%s %+.1f", polys$AOI[top], dA[top]), collapse = ", "))
    dir.create(file.path(LE_OUT, "rasters"), recursive = TRUE, showWarnings = FALSE)
    terra::writeRaster(c(N, dL, M), file.path(LE_OUT, "rasters", paste0(ctx, "_walkNat_10m.tif")), overwrite = TRUE, names = c("variant", "delta", "buffer"), gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
  }
  dir.create(file.path(LE_OUT, "aoi"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(LE_OUT, "network"), recursive = TRUE, showWarnings = FALSE)
  saveAtomic(aoiOut, file.path(LE_OUT, "aoi", paste0(ctx, ".rds")))
  for(s in c("_edges.rds", "_meta.rds"))
    file.copy(file.path(SRC_V, "network", paste0(ctx, s)), file.path(LE_OUT, "network", paste0(ctx, s)), overwrite = TRUE)
  saveAtomic(list(net = netOut, parking = readRDS(file.path(SRC_V, "network", paste0(ctx, "_net.rds")))$parking),
             file.path(LE_OUT, "network", paste0(ctx, "_net.rds")))
  logf("lakes %s: written to %s, %.0f s", ctx, LE_OUT, proc.time()[["elapsed"]] - t0)
}
if(length(log)){
  lg <- do.call(rbind, log)
  utils::write.csv(lg, file.path(LE_OUT, "lake_emphasis.csv"), row.names = FALSE)
  options(width = 220); print(lg, digits = 4, row.names = FALSE)
}
