#### DULN per STRAVA group (5), fitted over the OSM-trace areas instead of Glatt ####
#
# data-raw/tailor_duln_strava.R fitted the element weights to Strava's heatmap at Glatt,
# so Glatt's evaluation was not independent of the fit. This applies the same method to
# the 12 Swiss areas of data-raw/tailor_duln_osm.R (the user's choice, 2026-10-27): their
# networks (shapeLarger) keep >= 2 km from Glatt, GlattPark and Wigger, so none of the
# three surveys' sites enters the fit. Unchanged from the Glatt fit:
#   - the 5 groups and their layers (Hike, Walk, Ride + GravelRide, EBikeRide, Run), the
#     dog types taking their walker group's whole layer;
#   - z14 grayscale tiles, decoded and checked per area (georeferencing, seams: ranks over
#     the area or within each z14 tile, the same rule, decided per area);
#   - per cell the length-weighted mean of its edges' heat, only on the cells inside the
#     areas (no lakes), which drops most commuting;
#   - fitTypes(target = "ranks") on the clean elements, per group.
# Pooled: a cell's target is its normal score within its own area (or z14 tile), and
# log Nearby-Random per group - from the OSM fit's runs, scaled to mean 1 per area - is
# the free covariate. The element extracts, Nearby-Random runs and areas are the OSM
# fit's (tailor_duln_osm.R's extract stage must have run for every area).
#
# TWO HEAT MEASURES per edge (the user, 2026-10-27: tracks spread over a wide road, or
# scatter under trees, give a wider, dimmer band than the same use on a narrow path):
#   peak    as at Glatt: the 2 x 2 max mosaic, the 3 x 3 max round the line (~+-5 m), its
#           mean along the edge - the brightest heat next to the path
#   width   the 2 x 2 mean mosaic; every pixel with heat within SA_WIDTH_M of the network
#           goes to its nearest edge (parallel paths do not share pixels); the edge's heat
#           is the sum over its pixels x pixel area / edge length: heat x metres of width.
#           The grey level is a brightness scale, not counts: the sum approximates use.
# Each is fitted on its own: <SA>/clean (peak, variant SA5E_s0.5) and <SA>/width/clean
# (width, SA5W_s0.5). Descriptive: each measure per edge against the OSM trace counts.
#
# THE TILES: as tailor_duln_strava.R's header, but the session file goes to
# <VFT_SA_RAW>/session.txt. Delete it after the download; the tiles stay local.
#
# Stages (VFT_SA_STAGES):
#   tiles, decode, check   tailor_duln_strava.R's, per area (decode: both mosaics)
#   extract   both heat measures per edge and cell, each cell's z14 tile, Nearby-Random
#             per group; both measures against the OSM traces per edge
#   fit       per measure: the pooled fit; weights against S5E (Strava at Glatt), E2 (the
#             survey), the OSM fit and the xlsx; CV; leave one area out; Glatt's Strava
#             heat (peak) predicted out of sample
# The variants (stairs x3 for bikers) are built by data-raw/apply_duln_variant.R with
# VFT_AV_SCRIPT=tailor_duln_strava_areas.R, VFT_AV_TD=<the OSM tailored folder> and
#   SA5E_s0.5   VFT_AV_TYSUB=strava_areas/clean        (peak)
#   SA5W_s0.5   VFT_AV_TYSUB=strava_areas/width/clean  (width)
#
#   Rscript data-raw/tailor_duln_strava_areas.R
#
# VFT_SA_STAGES   which stages (default tiles, decode, check, extract, fit)
# VFT_SA_AREAS    a comma list: only these areas (default all of tailor_duln_osm.R's)
# VFT_SA_OUT      output (default <the OSM tailored folder>/strava_areas)
# VFT_SA_RAW      tiles, mosaics and the session file (default Local Data/strava_areas)
# VFT_SA_GLATT    Glatt's Strava extract for the out-of-sample check (default _wideT1_fit's)

IS_SA_WORKER <- identical(Sys.getenv("VFT_TY_WORKER"), "1")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
SA_SCRIPT <- file.path(.repo, "data-raw", "tailor_duln_strava_areas.R")
if(!IS_SA_WORKER){
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if(length(f) && grepl("tailor_duln_strava_areas", f[1])) SA_SCRIPT <- normalizePath(f[1], winslash = "/")
}
.saStages <- Sys.getenv("VFT_SA_STAGES", "tiles,decode,check,extract,fit")
#the areas, the OSM folders and the clean-element chain
Sys.setenv(VFT_OSM_STAGES = "none")
source(file.path(.repo, "data-raw", "tailor_duln_osm.R"))
#the Strava definitions, on that chain
SA <- Sys.getenv("VFT_SA_OUT", file.path(TD, "strava_areas"))
Sys.setenv(VFT_ST_STAGES = "none", VFT_ST_CHAIN = "none", VFT_ST_ELEMENTS = "clean", VFT_ST_OUT = SA,
           VFT_ST_RAW = Sys.getenv("VFT_SA_RAW", file.path(OSM_LOCAL, "strava_areas")))
source(file.path(.repo, "data-raw", "tailor_duln_strava.R"))
TY_SCRIPT <- SA_SCRIPT
#the heat in SA (basePath); the fits per measure
SA_MEASURES <- c("peak", "width")
SA_FIT_DIR <- c(peak = ST, width = file.path(SA, "width", "clean"))
ACT <- ST
SA_STAGES <- strsplit(.saStages, ",", fixed = TRUE)[[1]]
SA_AREAS <- envList("VFT_SA_AREAS", OSM_CTXS)
if(!all(SA_AREAS %in% OSM_CTXS)) stop("VFT_SA_AREAS: unknown area(s) ", paste(setdiff(SA_AREAS, OSM_CTXS), collapse = ", "))
SA_GLATT <- Sys.getenv("VFT_SA_GLATT", file.path(OSM_LOCAL, "pathUsageValidation_2026_wideT1_fit", "duln_tailored", "strava", "extract_glatt.rds"))
SA_WIDTH_M <- 15
saExtractFile <- function(ctx) basePath(paste0("extract_", ctx, ".rds"))

#### stage extract ####

#the width measure (edges x sports; header): heat x metres of width per metre of edge
edgeHeatWidth <- function(ea, ctx){
  R <- terra::rast(lapply(SPORTS, function(sp) terra::rast(mosaicFile(sp, ctx, "mean"))))
  names(R) <- SPORTS
  g <- sf::st_transform(sf::st_zm(ea$SHAPE), 3857)
  #3857 units per ground metre at the area's latitude
  bb <- sf::st_bbox(sf::st_transform(g, 4326))
  k <- 1 / cos(mean(bb[c("ymin", "ymax")]) * pi / 180)
  D <- SA_WIDTH_M * k; px <- terra::res(R)[1]
  #the pixels with heat near the network (distance to the rasterised edges), then each
  #one's nearest edge
  dist <- terra::distance(terra::rasterize(terra::vect(g), R[[1]], touches = TRUE))
  i <- which(terra::values(sum(R), mat = FALSE) > 0 & terra::values(dist, mat = FALSE) <= D + px)
  H <- matrix(0, length(g), length(SPORTS), dimnames = list(NULL, SPORTS))
  if(!length(i)) return(H)
  xy <- terra::xyFromCell(R, i)
  pts <- sf::st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = 3857)
  nn <- sf::st_nearest_feature(pts, g)
  d <- as.numeric(sf::st_distance(pts, g[nn], by_element = TRUE))
  ok <- d <= D
  v <- as.matrix(terra::extract(R, i[ok]))
  S <- rowsum(v, nn[ok])
  H[as.integer(rownames(S)), ] <- S * (px / k)^2
  H / pmax(ea$SHAPE_Leng, 1)
}

stageExtractAreas <- function(){
  for(ctx in SA_AREAS){
    f <- saExtractFile(ctx)
    if(file.exists(f)) next
    t0 <- proc.time()[["elapsed"]]
    if(!file.exists(nrFile(ctx))) stop(ctx, ": no Nearby-Random runs - run tailor_duln_osm.R's extract stage first")
    scope <- inVal(stageCheck(ctx))$scope
    ea <- igraph::edge_attr(inVal(readRDS(netFile(ctx)))$net)
    ex <- readRDS(extractFile(ctx))
    stopifnot(identical(ea$edgeID, ex$edgeID))
    H <- list(peak = edgeHeat(ea, ctx), width = edgeHeatWidth(ea, ctx))
    #the cells: the element extract's (in the areas, no lakes), length-weighted
    ag <- cellAgg(ctx, ea)
    if(!identical(ag$cells, ex$cells)) stop(ctx, ": the cells differ from the element extract's")
    Hc <- lapply(H, function(h){ m <- cellTargets(ag, h); lapply(stats::setNames(SPORTS, SPORTS), function(sp) m[, sp]) })
    ll <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(as.data.frame(terra::xyFromCell(ag$G$base, ex$cells)),
                                                           coords = c("x", "y"), crs = terra::crs(ag$G$base)), 4326))
    #Nearby-Random per Strava group
    nr <- readRDS(nrFile(ctx))
    ok <- vapply(nr$res, function(r) identical(r$status, "done"), TRUE)
    if(!all(ok)) stop(ctx, ": ", sum(!ok), " Nearby-Random runs failed")
    nrT <- lapply(stats::setNames(TYPES7, TYPES7), function(k) typeBase(nr$res, ag$G, k)[ex$cells])
    nr5 <- sapply(STRAVA5, function(k) Reduce(`+`, nrT[names(ST_OF)[ST_OF == k]]))
    #descriptive: each measure against the OSM trace counts on the edges in the areas
    m <- readMatch(ctx)
    stopifnot(identical(m$edgeID, ea$edgeID))
    oc <- edgeCounts(m)
    inA <- ea$edgeID %in% inVal(readRDS(edgesFile(ctx)))$edgeAoi$edgeID
    pairs <- list(foot = c("Hike", "Walk"), run = "Run", bike = c("Ride", "GravelRide", "EBikeRide"))
    osm <- do.call(rbind, lapply(names(pairs), function(gr) do.call(rbind, lapply(pairs[[gr]], function(sp)
      data.frame(ctx = ctx, osm = gr, sport = sp, edges = sum(inA), osmEdges = sum(oc[inA, gr] > 0),
                 peak = suppressWarnings(stats::cor(H$peak[inA, sp], oc[inA, gr], method = "spearman")),
                 width = suppressWarnings(stats::cor(H$width[inA, sp], oc[inA, gr], method = "spearman")))))))
    se <- list(ctx = ctx, cells = ex$cells, edgeID = ea$edgeID, H = H, Hc = Hc,
               tile = paste(lon2x(ll[, 1], ST_Z), lat2y(ll[, 2], ST_Z)), scope = scope, nr = nr5, osm = osm)
    saveRDS(se, f)
    logf("extract %s: %d edges, %d cells, ranks %s; peak vs width per cell (Spearman) %s; %.0f s", ctx, nrow(H$peak), length(ex$cells),
         if(scope == "global") "over the area" else "within each z14 tile",
         paste(sprintf("%s %.2f", SPORTS, vapply(SPORTS, function(sp) stats::cor(Hc$peak[[sp]], Hc$width[[sp]], method = "spearman"), 1)),
               collapse = ", "), proc.time()[["elapsed"]] - t0)
  }
  #both measures against the OSM traces, all areas
  osm <- do.call(rbind, lapply(SA_AREAS, function(ctx) readRDS(saExtractFile(ctx))$osm))
  utils::write.csv(osm, basePath("heat_vs_osm_edges.csv"), row.names = FALSE)
  ms <- stats::aggregate(cbind(peak, width) ~ osm + sport, osm, mean)
  logf("heat per edge against the OSM trace counts (Spearman over the areas' edges, mean over %d areas; descriptive):", length(SA_AREAS))
  print(ms, digits = 3, row.names = FALSE)
}

#### stage fit ####

#the areas stacked: virtual cells 1..N; per group, the normal scores of one heat measure
#within each area and Nearby-Random at mean 1 per area
saPool <- function(measure){
  parts <- lapply(SA_AREAS, function(ctx){
    se <- readRDS(saExtractFile(ctx)); ex <- readRDS(extractFile(ctx))
    stopifnot(identical(se$cells, ex$cells))
    se$Hc <- se$Hc[[measure]]
    nr <- apply(se$nr, 2, function(v) if(mean(v) > 0) v / mean(v) else rep(1, length(v)))
    list(ex = ex, tg = groupTarget(se, seq_along(se$cells)), nr = nr, scope = se$scope)
  })
  n <- vapply(parts, function(p) length(p$ex$cells), 1L)
  ex <- list(cells = seq_len(sum(n)), X = do.call(rbind, lapply(parts, function(p) p$ex$X)),
             logHa = unlist(lapply(parts, function(p) p$ex$logHa)), xy = do.call(rbind, lapply(parts, function(p) p$ex$xy)),
             area = rep(SA_AREAS, n))
  col <- function(what, k) unlist(lapply(parts, function(p) p[[what]][, k]))
  list(ex = ex, sty = list(base = lapply(stats::setNames(STRAVA5, STRAVA5), function(k) col("tg", k))),
       nr = lapply(stats::setNames(STRAVA5, STRAVA5), function(k) col("nr", k)),
       scope = stats::setNames(vapply(parts, `[[`, "", "scope"), SA_AREAS), n = stats::setNames(n, SA_AREAS))
}

#the reference weight sets (DULN units, scaled on Glatt's edges as every variant)
saRefs <- function(exG){
  refF <- c(S5E = file.path(TD, "strava", "clean", "fit_types.rds"), E2 = file.path(TD, "elements", "fit_types.rds"),
            OSM = file.path(OSM_FIT, "fit_types.rds"))
  refF <- refF[file.exists(refF)]
  lapply(refF, function(f) typeWeights(readRDS(f)$all, exG))
}

fitMeasure <- function(measure){
  d <- SA_FIT_DIR[[measure]]
  fp <- function(...) file.path(d, ...)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  pl <- saPool(measure)
  logf("fit %s: %d areas, %d cells (%s); ranks within each z14 tile at %s", measure, length(SA_AREAS), length(pl$ex$cells),
       paste(sprintf("%s %d", SA_AREAS, pl$n), collapse = ", "),
       if(any(pl$scope == "tile")) paste(SA_AREAS[pl$scope == "tile"], collapse = ", ") else "none")
  fd <- fp("fit_types.rds")
  if(file.exists(fd)) res <- readRDS(fd) else {
    all <- fitTypes(pl$ex, pl$sty, pl$nr, sprintf("Strava groups (%s), %d OSM areas", measure, length(SA_AREAS)),
                    types = STRAVA5, target = "ranks")
    all$actOf <- ST_OF; all$sameAs <- ST_SAME
    #no evaluation site is in the fit: noGP is the same
    res <- list(all = all, noGP = all, nrMaps = pl$nr, areas = SA_AREAS, scope = pl$scope, measure = measure)
    saveRDS(res, fd)
    #apply_duln_variant.R copies this one beside fit_types.rds
    file.copy(file.path(TD, "types", "survey_types.csv"), fp("survey_types.csv"), overwrite = TRUE)
  }
  free <- res$all
  #the weights against S5E (the Strava fit at Glatt), E2 (the survey's clean fit), the
  #OSM-trace fit and the xlsx
  exG <- readRDS(extractFile("glatt"))
  els <- ELEMENTS_NEW()
  tw <- typeWeights(free, exG)
  refW <- saRefs(exG)
  vs <- data.frame(element = els, xlsxEq_walkNat = unname(xlsxNew("walkNat")[els]), sa_shared = unname(tw$shared[els]),
                   identified = els %in% free$ident)
  for(r in names(refW)) vs[[paste0(r, "_shared")]] <- unname(refW[[r]]$shared[els])
  for(k in STRAVA5){
    vs[[paste0("sa_", k)]] <- unname(tw$perType[[ACT_COL[[k]]]][els])
    if(!is.null(refW$S5E)) vs[[paste0("S5E_", k)]] <- unname(refW$S5E$perType[[ACT_COL[[k]]]][els])
  }
  utils::write.csv(vs, fp("strava_areas_weights.csv"), row.names = FALSE)
  id <- vs$identified
  agree <- function(a, b) c(r = stats::cor(a[id], b[id]), sign = mean(sign(a[id]) == sign(b[id])))
  sm <- rbind(`areas vs xlsx` = agree(vs$sa_shared, vs$xlsxEq_walkNat),
              do.call(rbind, lapply(stats::setNames(names(refW), paste("areas vs", names(refW))), function(r) agree(vs$sa_shared, vs[[paste0(r, "_shared")]]))))
  if(!is.null(refW$S5E)) sm <- rbind(sm, do.call(rbind, lapply(stats::setNames(STRAVA5, paste("areas vs S5E,", STRAVA5)), function(k)
    agree(vs[[paste0("sa_", k)]], vs[[paste0("S5E_", k)]]))))
  logf("fit %s: weights over the %d elements the fit identifies (Pearson r, share of equal signs; DULN units at strength 1):", measure, sum(id))
  print(sm, digits = 3)
  cols <- intersect(c("element", "xlsxEq_walkNat", "E2_shared", "OSM_shared", "S5E_shared", "sa_shared", paste0("sa_", STRAVA5)), names(vs))
  print(vs[order(-abs(vs$sa_shared)), cols], digits = 2, row.names = FALSE)
  #CV: shared with the elements vs none; each group's own difference vs the shared weights
  cvt <- rbind(data.frame(group = "shared", lambda = free$lambdaShared, cvOwn = min(free$cvShared), cvShared = free$cvNone, gammaNR = NA_real_),
               do.call(rbind, lapply(STRAVA5, function(k){ dk <- free$dev[[k]]
                 data.frame(group = k, lambda = dk$lambda, cvOwn = dk$cv, cvShared = dk$cv0, gammaNR = unname(free$gammaNR[[paste0("nr_", k)]])) })))
  cvt$gain <- 1 - cvt$cvOwn / cvt$cvShared
  utils::write.csv(cvt, fp("strava_areas_cv.csv"), row.names = FALSE)
  logf("fit %s: cross-validated MSE (2 km blocks; shared: with the elements vs none; groups: own difference vs the shared weights):", measure)
  print(cvt, digits = 4, row.names = FALSE)
  #leave one area out: on the held-out area, per group, target ~ log Nearby-Random + s,
  #s = X w standardised (one scale per weight set, the same freedom for each); w the
  #pooled fit without that area (shared; per group: + the group's difference), S5E
  #(shared; per group), E2, OSM and the xlsx. The R2 each adds beyond Nearby-Random alone.
  W0 <- c(lapply(refW, function(x) x$shared[els]), list(xlsx = xlsxNew("walkNat")[els]))
  zs <- function(X, w){ w[is.na(w)] <- 0; s <- drop(X[, names(w), drop = FALSE] %*% w); if(stats::sd(s) > 0) (s - mean(s)) / stats::sd(s) else NULL }
  lo <- do.call(rbind, lapply(SA_AREAS, function(a){
    tr <- pl$ex$area != a; te <- which(!tr)
    sub <- list(cells = seq_len(sum(tr)), X = pl$ex$X[tr, , drop = FALSE], logHa = pl$ex$logHa[tr], xy = pl$ex$xy[tr, , drop = FALSE])
    ft <- fitTypes(sub, list(base = lapply(pl$sty$base, function(v) v[tr])), lapply(pl$nr, function(v) v[tr]),
                   paste(measure, "without", a), types = STRAVA5, target = "ranks")
    wO <- stats::setNames(numeric(length(els)), els); wO[ft$ident] <- ft$betaShared[ft$ident]
    Xt <- pl$ex$X[te, , drop = FALSE]
    do.call(rbind, lapply(STRAVA5, function(k){
      y <- pl$sty$base[[k]][te]; v <- pl$nr[[k]][te]; lnr <- log(pmax(v, 0.5 * min(v[v > 0])))
      m0 <- stats::lm(y ~ lnr); rss0 <- sum(stats::residuals(m0)^2)
      wk <- wO; wk[ft$ident] <- wk[ft$ident] + ft$dev[[k]]$delta[ft$ident]
      W <- c(list(areas = wO, areas_group = wk), W0, if(!is.null(refW$S5E)) list(S5E_group = refW$S5E$perType[[ACT_COL[[k]]]][els]))
      row <- data.frame(heldOut = a, group = k, cells = length(te), NR_spearman = stats::cor(y, stats::fitted(m0), method = "spearman"))
      for(nm in names(W)){
        s <- zs(Xt, W[[nm]])
        m <- if(is.null(s)) m0 else stats::lm(y ~ lnr + s)
        row[[paste0(nm, "_r2gain")]] <- 1 - sum(stats::residuals(m)^2) / rss0
        row[[paste0(nm, "_spearman")]] <- stats::cor(y, stats::fitted(m), method = "spearman")
      }
      row
    }))
  }))
  utils::write.csv(lo, fp("strava_areas_leave_one_area_out.csv"), row.names = FALSE)
  num <- names(lo)[grepl("_r2gain$|_spearman$", names(lo))]
  ms <- stats::aggregate(lo[, num], list(group = lo$group), mean)
  ms <- rbind(ms, data.frame(group = "all", t(colMeans(lo[, num]))))
  utils::write.csv(ms, fp("strava_areas_leave_one_area_out_mean.csv"), row.names = FALSE)
  logf("fit %s: leave one area out, R2 added beyond Nearby-Random on the held-out area, mean over the %d areas (per area: %s):",
       measure, length(SA_AREAS), fp("strava_areas_leave_one_area_out.csv"))
  print(ms[, c("group", grep("_r2gain$", names(ms), value = TRUE))], digits = 3, row.names = FALSE)
  #Glatt out of sample (descriptive): Glatt's Strava heat (the peak measure of S5E's fit)
  #as normal scores per group against X w (no Nearby-Random: Glatt's runs are not in
  #this folder); S5E in sample
  if(file.exists(SA_GLATT)){
    seG <- readRDS(SA_GLATT)
    if(length(seG$Hc[[1]]) < max(exG$cells) || anyNA(seG$Hc[[1]][exG$cells])){
      logf("Glatt check skipped: %s is not on the grid of %s", SA_GLATT, extractFile("glatt"))
    } else {
      tgG <- groupTarget(seG, exG$cells)
      gs <- do.call(rbind, lapply(STRAVA5, function(k){
        W <- c(list(areas = tw$shared[els], areas_group = tw$perType[[ACT_COL[[k]]]][els]), W0,
               if(!is.null(refW$S5E)) list(S5E_group = refW$S5E$perType[[ACT_COL[[k]]]][els]))
        data.frame(group = k, t(vapply(W, function(w){ s <- zs(exG$X, w); if(is.null(s)) NA_real_ else
          stats::cor(s, tgG[, k], method = "spearman") }, 1)))
      }))
      utils::write.csv(gs, fp("strava_areas_glatt_out_of_sample.csv"), row.names = FALSE)
      logf("fit %s: Glatt's Strava heat (%d cells, not in this fit; S5E was fitted on it): Spearman of X w per group and weight set:",
           measure, length(exG$cells))
      print(gs, digits = 3, row.names = FALSE)
    }
  } else logf("Glatt check skipped: no %s", SA_GLATT)
  invisible(res)
}
stageFitAreas <- function() invisible(lapply(stats::setNames(SA_MEASURES, SA_MEASURES), fitMeasure))

#### the variants (built by data-raw/apply_duln_variant.R; `heat` only tells them apart) ####

TY_VARIANTS <- list(
  SA5E_s0.5 = list(kind = "types", typeArea = FALSE, manual = BIKE_STAIRS, postNet = stairsPostNet, heat = "peak"),
  SA5W_s0.5 = list(kind = "types", typeArea = FALSE, manual = BIKE_STAIRS, postNet = stairsPostNet, heat = "width"))

#### run ####

if(!IS_SA_WORKER && !identical(SA_STAGES, "none")){
  t00 <- proc.time()[["elapsed"]]
  logf("Strava over the OSM areas: %d areas (%s); stages %s", length(SA_AREAS), paste(SA_AREAS, collapse = ", "), paste(SA_STAGES, collapse = ","))
  if("tiles" %in% SA_STAGES) for(ctx in SA_AREAS) inVal(stageTiles(ctx))
  if("decode" %in% SA_STAGES) for(ctx in SA_AREAS) inVal({ stageDecode(ctx, "max"); stageDecode(ctx, "mean") })
  if("check" %in% SA_STAGES) for(ctx in SA_AREAS) inVal(stageCheck(ctx))
  if("extract" %in% SA_STAGES) stageExtractAreas()
  if("fit" %in% SA_STAGES) stageFitAreas()
  logf("tailor_duln_strava_areas: done in %.1f min, output in %s", (proc.time()[["elapsed"]] - t00) / 60, SA)
}
