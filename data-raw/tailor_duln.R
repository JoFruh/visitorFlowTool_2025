#### A survey-tailored DULN: is the map the limit, or the dynamics? ####
#
# Full (DULN everywhere) does barely better than Nearby in the survey validation,
# whatever the engine's parameters (<calibration>/report.md, junction_test). This
# asks whether the attractiveness LAYER is the limit. It rebuilds DULN from the same
# landscape elements by the same method, GenerateUpdateAttractivityLayer.R in
# Documents/Local Data/AttractivityLayer (the Nov-2024 run behind
# allAttrs_COG_final.tif). Only the weights change, fitted to what the survey
# respondents did, plus the lake band below. Diagnostic only: a DULN fitted to the
# three sites cannot then validate the simulation, and nothing in the package or the
# app changes.
#
# How the layer is made (that script):
#   - 37 element rasters, 10 m LV95, each the sum of its 0/1 buffer rings, in
#     Local Large Datasets/AttractivityIntermediateRasters;
#   - one weight per element and agent type, AttractivityData_2.xlsx columns 9-15;
#   - the weighted sum per type (finalAttractivity/<Type>_attractivity.tif), then
#     project to 4326 (bilinear) and aggregate by 2: the 7 bands of
#     allAttrs_COG_final.tif;
#   - the path DULN: the length-weighted mean of the 10 m type raster along each
#     line, + 34.2423.
#
# Every one of those steps is linear, so a re-weighted DULN is the stored one plus a
# DELTA raster dN = sum(w' X') - sum(w X) + b0, per site:
#   edge DULN_* += mean of dN along the edge (all 7 columns: the Nature walker change
#                  is applied to every type as a delta, the user's choice)
#   node DULN_* += dN through the 4326 pipeline, sampled at the node
#   area DULN   += f(walkNat + dN) - f(walkNat), f generateAoI2's
#                  (median + median of the top quarter) / 2 with the lakes NA
# Every variant keeps each site's mean edge DULN (the shift b0) and mean area DULN
# (re-centred): it moves where DULN is high, not its level against distance.
# With dN = 0 the inputs are the stored ones exactly, so the stored runs are the
# check variant's reference.
#
# THE LAKE BAND (the user's rule). lakesAndBigRivers is three rings, 50/100/150 m.
# For a lake of at least 10 ha, the outer ring becomes a band that follows the path
# loop round the lake, as step 4 does: the tightest loop (vftLakeLoopAoI(), unchanged)
# inside a zone of C(A) = 50 m x sqrt(A / 52 000 m2) (vftLakeLoopBuffer()'s scaling),
# floored at today's 150 m and capped at VFT_TD_LAKE_MAX_M (300). Where no loop is
# found, the band is C(A) from the water. It never shrinks below the 150 m ring.
# Rivers and smaller lakes keep the ring. In the fit the lake is two elements:
# lakeBand (0/1, the main lake weight) and lakeNear (the 50 + 100 m rings, 0-2),
# nested inside it.
#
# THE FIT. Per comparison cell inside the areas, survey usage against the stored
# Nearby-Random runs (proximity and the network, no DULN) as offset, on the elements'
# length-weighted means along the cell's edges. Quasi-Poisson, ridge on the
# elements' weights in their own units, lambda by 2 km block CV, every site weighted equally, log
# area size of the cell's area as an unpenalised nuisance. Pooled over the three
# sites, and per site. The coefficients are scaled so the tailored pattern has the
# spread of today's edge DULN_WALK_, and shifted to its mean per site.
#
#   Rscript data-raw/tailor_duln.R
#
# VFT_TD_STAGES     layers, reproduce, extract, fit, loso, single, table, variants, rasters
#                   (default all)
# VFT_TD_OUT        output (default <VFT_VAL_OUT>/duln_tailored)
# VFT_TD_LAKE_MAX_M the lake band's ceiling (default 300)
# VFT_TD_XLSX       the weights table (default the H: original; cached as CSV)
# VFT_VAL_OUT       the validation whose inputs and runs are used (default _final_timefix)
#
# Then data-raw/test_tailored_duln.R simulates the variants.

Sys.setenv(VFT_VAL_WORKER = "1")   #definitions only: no main(), no compile
LOCAL <- "C:/Users/frueh/Documents/Local Data"
if(!nzchar(Sys.getenv("VFT_VAL_OUT")))
  Sys.setenv(VFT_VAL_OUT = file.path(LOCAL, "pathUsageValidation_2026_final_timefix"))
Sys.setenv(VFT_VAL_ENGINE = "corrected", VFT_VAL_STRAIGHT = "1", VFT_VAL_ALPHA = "2", VFT_VAL_REACH = "0.33")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "validate_usage_surveys.R"))

TD        <- Sys.getenv("VFT_TD_OUT", file.path(OUT, "duln_tailored"))
TD_STAGES <- envList("VFT_TD_STAGES", c("layers", "reproduce", "extract", "fit", "loso", "single",
                                        "table", "variants", "rasters"))
LAKE_MAX  <- envNum("VFT_TD_LAKE_MAX_M", 300)
LAKE_RING <- 150                      #today's outer ring: the band's floor
LAKE_PAD  <- ns$VFT_LAKE_LOOP_PAD_M   #15 m: the loop's path cells inside the band
EDGE_SHIFT <- 34.2423                 #GenerateUpdateAttractivityLayer.R:876
dir.create(TD, recursive = TRUE, showWarnings = FALSE)
tdPath <- function(...){ p <- file.path(TD, ...); dir.create(dirname(p[1]), recursive = TRUE, showWarnings = FALSE); p }

LLD       <- file.path(ONEDRIVE, "Local Large Datasets")
LAYER_DIR <- file.path(LLD, "AttractivityIntermediateRasters")
#the Nov-2024 originals; the two files one level up were rewritten in May 2026
TYPE_DIR  <- file.path(LLD, "finalAttractivity", "finalAttractivity")
XLSX      <- Sys.getenv("VFT_TD_XLSX", "H:/Jo/Project Visitor Flow Tool/Tables/AttractivityData_2.xlsx")
COG       <- ns$vftData("maps/attr/allAttrs_COG_final.tif")

#agent type (the engine's) -> the type raster's file stem, the xlsx column, the COG band
TYPES_TD <- data.frame(
  type = c("jogger", "dogNat", "ebikeNat", "walkNat", "dogProx", "walkSoc", "bikeSport"),
  file = c("Joggers", "Nature_dog_walkers", "Nature_ebike", "Nature_walkers",
           "Proximity_dog_walkers", "Social_walkers", "Sports_bike"),
  xlsx = c("Joggers", "Nat. dog walk.", "Nature e-bike", "Nature walkers",
           "Pr. dog walk.", "Social walkers", "Sports bike"),
  band = 1:7, stringsAsFactors = FALSE)
#which box (layer crop) each site's network lies in: GlattPark is inside Glatt's
BOX_OF <- c(glattPark = "glatt", glatt = "glatt", wigger = "wigger")
LAKE_EL <- c("lakeNear", "lakeBand")

#### weights ####

#The 37 elements and their weights, from the xlsx (rows 1-37: the sheet now carries a
#second copy of the table further down). Cached as CSV, so a later run does not need
#the H: share.
xlsxWeights <- function(){
  f <- tdPath("weights_xlsx.csv")
  if(file.exists(f)) return(utils::read.csv(f, check.names = FALSE, stringsAsFactors = FALSE))
  x <- readxl::read_xlsx(XLSX, range = readxl::cell_rows(c(2, NA)))
  x <- as.data.frame(x[seq_len(which(is.na(x$Variables))[1] - 1), ], check.names = FALSE)
  w <- data.frame(element = x$Variables, buffers = x[["Buffer sizes (0, 50, 100, 200)"]],
                  check.names = FALSE, stringsAsFactors = FALSE)
  for(i in seq_len(nrow(TYPES_TD))) w[[TYPES_TD$type[i]]] <- as.numeric(x[[TYPES_TD$xlsx[i]]])
  stopifnot(nrow(w) == 37, !anyNA(w[TYPES_TD$type]))
  utils::write.csv(w, f, row.names = FALSE)
  w
}

#### stage layers: the 37 elements + the lake band, cropped per box ####

#the box: every site's network extent (shapeLarger, 3857) in 2056, plus 1.5 km
boxExtent <- function(box){
  ctxs <- names(BOX_OF)[BOX_OF == box]
  bb <- do.call(rbind, lapply(ctxs, function(ctx)
    as.numeric(sf::st_bbox(sf::st_transform(readRDS(aoiFile(ctx))$shapeLarger, 2056)))))
  terra::ext(min(bb[, 1]) - 1500, max(bb[, 3]) + 1500, min(bb[, 2]) - 1500, max(bb[, 4]) + 1500)
}
layersFile <- function(box) tdPath("layers", paste0("layers_", box, ".tif"))
lakesFile  <- function(box) tdPath("layers", paste0("lakes_", box, ".rds"))

#The lake band (see the header). Returns the band as polygons (2056) and a table
#per lake.
lakeBand <- function(boxPoly){
  lk <- ns$.vftReadLakesWithin(boxPoly)
  lk <- lk[lk$SHAPE_Area >= ns$VFT_AOI_LAKE_MIN_M2, ]
  if(!nrow(lk)) return(list(band = NULL, table = NULL))
  g <- sf::st_make_valid(sf::st_zm(sf::st_geometry(sf::st_transform(lk, 2056))))
  A <- as.numeric(sf::st_area(g))
  #vftLakeLoopBuffer()'s scaling (50 m at 5.2 ha, with the root of the area),
  #floored at today's ring and capped at the user's ceiling
  C <- pmin(LAKE_MAX, pmax(LAKE_RING, ns$vftLakeLoopBuffer(A, maxM = Inf)))
  #the zones; touching ones are one (the two Katzensee basins), as the cut's anchors
  z <- sf::st_cast(sf::st_union(sf::st_buffer(g, C)), "POLYGON")
  t0 <- proc.time()[["elapsed"]]
  #the package's own lake-loop pass, unchanged: the zone is the lake's "holder", and
  #the loop may run LAKE_PAD past it
  dbg <- withDbgCapture(ns$vftLakeLoopAoI(z, sf::st_sf(geometry = g),
                                          buffer = function(a) LAKE_PAD))
  for(m in dbg$msgs) logf("lake loop: %s", trimws(m))
  loops <- dbg$value$geom[dbg$value$lakeLoop]
  logf("lake loop: %d lake(s) >= 10 ha, %d loop(s), %.0f s", length(g), length(loops),
       proc.time()[["elapsed"]] - t0)
  #per lake: the loop that holds most of it, else the distance band
  pick <- vapply(seq_along(g), function(k){
    if(!length(loops)) return(NA_integer_)
    hit <- vapply(seq_along(loops), function(j)
      sum(as.numeric(sf::st_area(sf::st_intersection(loops[j], g[k])))), 1)
    if(max(hit) / A[k] > 0.5) which.max(hit) else NA_integer_
  }, 1L)
  bands <- lapply(seq_along(g), function(k)
    if(is.na(pick[k])) sf::st_buffer(g[k], C[k]) else loops[pick[k]])
  tab <- do.call(rbind, lapply(seq_along(g), function(k){
    #how far the band reaches from the water: points every 20 m along its outline
    out <- sf::st_boundary(ns$.vftFillHoles(sf::st_union(bands[[k]])))
    pts <- sf::st_cast(sf::st_segmentize(out, 20), "POINT")
    xy <- sf::st_coordinates(sf::st_centroid(g[k]))
    data.frame(lake = k, ha = A[k] / 1e4, ceiling_m = C[k], loop = !is.na(pick[k]),
               outline_km = as.numeric(sum(sf::st_length(out))) / 1000,
               reach_max_m = max(as.numeric(sf::st_distance(pts, g[k]))),
               band_ha = as.numeric(sf::st_area(sf::st_union(bands[[k]]))) / 1e4,
               x = xy[1], y = xy[2])
  }))
  list(band = sf::st_union(do.call(c, bands)), table = tab, lakes = g)
}

stageLayers <- function(box){
  f <- layersFile(box)
  if(file.exists(f)) return(invisible(NULL))
  t0 <- proc.time()[["elapsed"]]
  e <- boxExtent(box)
  w <- xlsxWeights()
  fs <- file.path(LAYER_DIR, paste0(w$element, ".tif"))
  stopifnot(all(file.exists(fs)))
  X <- terra::rast(lapply(fs, function(p) terra::crop(terra::rast(p), e)))
  names(X) <- w$element
  #the lake split: lk is 3 within 50 m (and on the water), 2 to 100 m, 1 to 150 m
  lk <- X[["lakesAndBigRivers"]]
  lakeNear <- terra::clamp(lk - 1, lower = 0, values = TRUE)
  ev <- as.vector(e)
  boxPoly <- sf::st_as_sfc(sf::st_bbox(c(xmin = ev[["xmin"]], ymin = ev[["ymin"]], xmax = ev[["xmax"]],
                                         ymax = ev[["ymax"]]), crs = sf::st_crs(2056)))
  lb <- lakeBand(boxPoly)
  saveRDS(lb, lakesFile(box))
  ring <- lk >= 1
  band <- if(!is.null(lb$band)) {
    r <- terra::rasterize(terra::vect(lb$band), lk, field = 1, background = 0)
    (ring | r > 0) * 1
  } else ring * 1
  names(lakeNear) <- "lakeNear"; names(band) <- "lakeBand"
  X <- c(X, lakeNear, band)
  terra::writeRaster(X, f, overwrite = TRUE, gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES"),
                     datatype = "FLT4S")
  if(!is.null(lb$table)) utils::write.csv(lb$table, tdPath("layers", paste0("lakes_", box, ".csv")), row.names = FALSE)
  logf("layers %s: %d x %d cells, %d layers, %.0f s", box, terra::nrow(X), terra::ncol(X),
       terra::nlyr(X), proc.time()[["elapsed"]] - t0)
  if(!is.null(lb$table)) print(lb$table, digits = 4, row.names = FALSE)
}

#### stage reproduce: the old pipeline, rebuilt from the elements ####

#the COG grid around a site (template for node and area values), in 4326
cogTemplate <- function(ctx){
  sl <- sf::st_transform(readRDS(aoiFile(ctx))$shapeLarger, 4326)
  bb <- sf::st_bbox(sl)
  terra::crop(terra::rast(COG)[[4]], terra::ext(bb[["xmin"]] - 0.005, bb[["xmax"]] + 0.005,
                                               bb[["ymin"]] - 0.005, bb[["ymax"]] + 0.005))
}
#a 10 m LV95 raster through the script's 4326 step (project bilinear, aggregate by 2),
#onto the template's grid
to4326 <- function(r10, template){
  fine <- terra::disagg(terra::rast(template), 2)
  p <- terra::project(r10, fine, method = "bilinear")
  a <- terra::aggregate(p, 2, fun = "mean")
  if(!isTRUE(terra::compareGeom(a, template, stopOnError = FALSE)))
    a <- terra::resample(a, template, method = "near")
  a
}
#the weighted sum of the element layers
weighted <- function(X, w){
  w <- w[w != 0 & names(w) %in% names(X)]
  out <- X[[names(w)[1]]] * w[[1]]
  for(k in names(w)[-1]) out <- out + X[[k]] * w[[k]]
  out
}

stageReproduce <- function(){
  f <- tdPath("reproduce.rds")
  if(file.exists(f)) return(invisible(readRDS(f)))
  w <- xlsxWeights()
  rows <- list(); grid <- list()
  for(box in unique(BOX_OF)){
    X <- terra::rast(layersFile(box))[[w$element]]
    #(a) every type raster is the xlsx-weighted sum of the elements
    for(i in seq_len(nrow(TYPES_TD))){
      Y <- terra::crop(terra::rast(file.path(TYPE_DIR, paste0(TYPES_TD$file[i], "_attractivity.tif"))), X)
      set.seed(i)
      s <- terra::spatSample(c(Y, X), 80000, method = "random", na.rm = TRUE)
      names(s)[1] <- "Y"
      m <- stats::lm(Y ~ . + 0, data = s)
      cf <- stats::coef(m)
      rows[[length(rows) + 1]] <- data.frame(box = box, type = TYPES_TD$type[i], element = w$element,
                                             xlsx = w[[TYPES_TD$type[i]]], recovered = unname(cf[w$element]),
                                             r2 = summary(m)$r.squared, maxResid = max(abs(stats::resid(m))))
    }
    #(b) the Nature walker raster through project + aggregate = the stored COG band
    for(ctx in names(BOX_OF)[BOX_OF == box]){
      tm <- cogTemplate(ctx)
      N10 <- weighted(X, stats::setNames(w$walkNat, w$element))
      a <- to4326(N10, tm)
      #the interior only: the 10 m crop must cover each fine cell's neighbours
      inner <- terra::ext(tm) - 0.01
      v1 <- terra::values(terra::crop(a, inner), mat = FALSE)
      v0 <- terra::values(terra::crop(tm, inner), mat = FALSE)
      ok <- is.finite(v1) & is.finite(v0)
      grid[[ctx]] <- data.frame(ctx = ctx, n = sum(ok), r = stats::cor(v1[ok], v0[ok]),
                                maxAbs = max(abs(v1[ok] - v0[ok])), q99 = unname(stats::quantile(abs(v1[ok] - v0[ok]), .99)),
                                exact1e4 = mean(abs(v1[ok] - v0[ok]) < 1e-4))
    }
  }
  wt <- do.call(rbind, rows)
  wt$absent <- is.na(wt$recovered)
  wt$diff <- wt$recovered - wt$xlsx
  utils::write.csv(wt, tdPath("reproduce_weights.csv"), row.names = FALSE)
  gt <- do.call(rbind, grid)
  utils::write.csv(gt, tdPath("reproduce_4326.csv"), row.names = FALSE)
  agg <- stats::aggregate(cbind(r2, maxResid) ~ box + type, wt, function(x) x[1])
  agg$maxWeightDiff <- stats::aggregate(diff ~ box + type, wt[!wt$absent, ], function(x) max(abs(x)))$diff
  options(width = 200)
  print(agg, digits = 6, row.names = FALSE)
  print(gt, digits = 6, row.names = FALSE)
  nw <- wt[wt$type == "walkNat" & !wt$absent, ]
  if(any(nw$r2 < 0.999999) || any(abs(nw$diff) > 1e-3))
    stop("the Nature walker raster is NOT the xlsx-weighted sum of the elements - stop here")
  if(any(gt$r < 0.9999))
    stop("the 4326 step does not reproduce the stored COG (r < 0.9999) - stop here")
  res <- list(weights = wt, grid = gt)
  saveRDS(res, f)
  invisible(res)
}

#### stage extract: elements per edge, per comparison cell ####

ELEMENTS_ORIG <- function() xlsxWeights()$element
ELEMENTS_NEW  <- function() c(setdiff(ELEMENTS_ORIG(), "lakesAndBigRivers"), LAKE_EL)
extractFile <- function(ctx) tdPath("extract", paste0(ctx, ".rds"))

edgeGeom2056 <- function(net){
  ea <- igraph::edge_attr(net)
  sf::st_transform(sf::st_zm(ea$SHAPE), 2056)
}

stageExtract <- function(ctx){
  f <- extractFile(ctx)
  if(file.exists(f)) return(invisible(NULL))
  t0 <- proc.time()[["elapsed"]]
  X <- terra::rast(layersFile(BOX_OF[[ctx]]))
  net <- readRDS(netFile(ctx))$net
  ea <- igraph::edge_attr(net)
  g <- edgeGeom2056(net)
  #the line's length-weighted mean (the stored DULN_* match it at r = 1.0000)
  E <- as.matrix(exactextractr::exact_extract(X, sf::st_buffer(g, 0.5), "mean", progress = FALSE))
  colnames(E) <- sub("^mean\\.", "", colnames(E))
  E[!is.finite(E)] <- 0
  #(c) the stored edge DULN is the Nature walker weighting of these, + 34.2423
  w <- xlsxWeights()
  nw <- drop(E[, w$element] %*% w$walkNat) + EDGE_SHIFT
  ok <- is.finite(ea$DULN_WALK_)
  chk <- data.frame(ctx = ctx, nEdges = length(nw), r = stats::cor(nw[ok], ea$DULN_WALK_[ok]),
                    medAbs = stats::median(abs(nw[ok] - ea$DULN_WALK_[ok])),
                    q95 = unname(stats::quantile(abs(nw[ok] - ea$DULN_WALK_[ok]), .95)))
  logf("extract %s: %d edges x %d elements; vs stored DULN_WALK_ r %.5f, median |d| %.3f, q95 %.3f",
       ctx, nrow(E), ncol(E), chk$r, chk$medAbs, chk$q95)
  if(chk$r < 0.95) stop(ctx, ": the edge extraction does not reproduce the stored DULN (r < 0.95)")

  #the comparison cells: the base grid inside the areas and the perimeter, no lakes
  aoi <- readRDS(aoiFile(ctx)); sv <- readRDS(surveyFile(ctx)); edg <- readRDS(edgesFile(ctx))
  G <- buildGrid(ctx, aoi, sv, edg)
  t100 <- G$t100; base <- G$base
  ec <- edg$edgeCells
  er <- match(ec$edgeID, ea$edgeID)
  baseCell <- if(terra::ncell(base) == terra::ncell(t100)) ec$cell else
    terra::cellFromXY(base, terra::xyFromCell(t100, ec$cell))
  len <- ea$SHAPE_Leng[er]
  keep <- !is.na(baseCell) & !is.na(er)
  #a cell's elements: the length-weighted mean over the edges it holds
  S <- rowsum(E[er[keep], , drop = FALSE] * len[keep], baseCell[keep])
  W <- rowsum(len[keep], baseCell[keep])
  Xc <- S / as.vector(W)
  cells <- as.integer(rownames(Xc))
  inMask <- G$mBase[cells] & !(cells %in% sv$lakeCells)
  Xc <- Xc[inMask, , drop = FALSE]; cells <- cells[inMask]
  #the stored runs on the same grid
  simBase <- function(type){
    fs <- runFile(ctx, aoi$stamp, type, seq_len(1000))
    fs <- fs[file.exists(fs)]
    x <- numeric(terra::ncell(t100))
    for(p in fs){ r <- readRDS(p); x[r$cells] <- x[r$cells] + r$counts }
    list(map = as.vector(G$P %*% x), n = length(fs))
  }
  sims <- lapply(c(nearbyRandom = "nearbyRandom", nearby = "nearby", full = "full"), simBase)
  #the cell's area: log ha (a nuisance; the engine already uses area size)
  aoiOf <- rep(NA_character_, terra::ncell(base))
  for(a in names(G$aoiCells)) aoiOf[G$aoiCells[[a]]] <- a
  ha <- stats::setNames(edg$aoiHa, edg$aoiLetters)
  xy <- terra::xyFromCell(base, cells)
  out <- list(ctx = ctx, edgeID = ea$edgeID, E = E, check = chk,
              cells = cells, X = Xc, y = sv$surveyBase[cells],
              sim = lapply(sims, function(s) s$map[cells]), nReps = vapply(sims, `[[`, 1L, "n"),
              simFull = lapply(sims, `[[`, "map"),
              aoi = aoiOf[cells], logHa = log(ha[aoiOf[cells]]), xy = xy)
  saveRDS(out, f)
  logf("extract %s: %d comparison cells (%d with NR usage), %.0f s", ctx, length(cells),
       sum(out$sim$nearbyRandom > 0), proc.time()[["elapsed"]] - t0)
}

#### stage fit: survey -> weights ####

#ridge Poisson by Newton: minimise sum(W (mu - y eta)) + lambda/2 sum(pen b^2), with
#W the observation weights normalised to 1 (the quasi-Poisson point estimate)
ridgePois <- function(X, y, off, W, lambda, pen, b = NULL, maxit = 100){
  p <- ncol(X)
  if(is.null(b)) b <- numeric(p)
  W <- W / sum(W)
  obj <- function(bb){ e <- off + drop(X %*% bb); sum(W * (exp(e) - y * e)) + lambda / 2 * sum(pen * bb^2) }
  o <- obj(b)
  for(it in seq_len(maxit)){
    mu <- exp(off + drop(X %*% b))
    g  <- drop(crossprod(X, W * (y - mu))) - lambda * pen * b
    H  <- crossprod(X, X * (W * mu)) + diag(lambda * pen + 1e-10, p)
    st <- drop(solve(H, g))
    t <- 1
    repeat{
      bn <- b + t * st; on <- obj(bn)
      if(is.finite(on) && on <= o + 1e-14) break
      t <- t / 2
      if(t < 1e-6){ bn <- b; on <- o; break }
    }
    done <- max(abs(bn - b)) < 1e-7
    b <- bn; o <- on
    if(done) break
  }
  b
}
poisDev <- function(y, mu, W) 2 * sum(W * (ifelse(y > 0, y * log(y / mu), 0) - (y - mu))) / sum(W)

#the design of one fit over the given sites: the elements identified there
#(nonzero in >= 1 % of the cells), standardised; site intercepts; log ha
fitDesign <- function(ex, els){
  site <- unlist(lapply(ex, function(e) rep(e$ctx, length(e$cells))))
  Xr <- do.call(rbind, lapply(ex, function(e) e$X[, els, drop = FALSE]))
  y  <- unlist(lapply(ex, function(e) e$y / mean(e$y)))
  nr <- unlist(lapply(ex, function(e){ s <- e$sim$nearbyRandom
    s <- s / mean(s); pmax(s, 0.5 * min(s[s > 0])) }))
  logHa <- unlist(lapply(ex, `[[`, "logHa"))
  logHa[!is.finite(logHa)] <- mean(logHa[is.finite(logHa)])
  ident <- colMeans(Xr != 0) >= 0.01 & apply(Xr, 2, stats::sd) > 0
  mu0 <- colMeans(Xr[, ident, drop = FALSE]); sd0 <- apply(Xr[, ident, drop = FALSE], 2, stats::sd)
  Z <- sweep(sweep(Xr[, ident, drop = FALSE], 2, mu0), 2, sd0, "/")
  sites <- unique(site)
  D <- sapply(sites, function(s) as.numeric(site == s))
  D <- matrix(D, ncol = length(sites), dimnames = list(NULL, paste0("site_", sites)))
  lh <- if(length(sites) > 1 || stats::sd(logHa) > 0) cbind(logHa = logHa - mean(logHa)) else NULL
  M <- cbind(D, lh, Z)
  #the penalty is on the weights in their own units (rings, classes), not on the
  #standardised ones: a rare element (stairs, 3 % of the cells) then cannot take a
  #weight ten times any other's off a handful of cells. Block CV scores the two
  #alike (pooled 1.597 vs 1.585, against 1.688 without the elements).
  pz <- 1 / sd0^2
  pen <- c(rep(0, ncol(D) + NCOL(lh)), pz / mean(pz))
  #every site weighs the same
  W <- 1 / as.numeric(table(site)[site])
  blk <- unlist(lapply(ex, function(e) paste(e$ctx, floor(e$xy[, 1] / 2000), floor(e$xy[, 2] / 2000))))
  list(M = M, y = y, off = log(nr), W = W, pen = pen, site = site, ident = names(which(ident)),
       mu0 = mu0, sd0 = sd0, blk = blk, nr = nr)
}

LAMBDAS <- 10^seq(-4, 3, by = 0.25)
fitRidge <- function(ex, els, label){
  d <- fitDesign(ex, els)
  set.seed(11)
  ub <- unique(d$blk); fold <- stats::setNames(sample(rep_len(1:5, length(ub))), ub)[d$blk]
  cv <- vapply(LAMBDAS, function(l){
    mean(vapply(1:5, function(k){
      tr <- fold != k
      b <- ridgePois(d$M[tr, , drop = FALSE], d$y[tr], d$off[tr], d$W[tr], l, d$pen)
      mu <- exp(d$off[!tr] + drop(d$M[!tr, , drop = FALSE] %*% b))
      poisDev(d$y[!tr], mu, d$W[!tr])
    }, 1))
  }, 1)
  lam <- LAMBDAS[which.min(cv)]
  b <- ridgePois(d$M, d$y, d$off, d$W, lam, d$pen)
  names(b) <- colnames(d$M)
  mu <- exp(d$off + drop(d$M %*% b))
  #what the elements alone add (no site constant, no area term): for the static bound
  zcols <- d$ident
  etaL <- drop(d$M[, zcols, drop = FALSE] %*% b[zcols])
  beta <- stats::setNames(numeric(length(els)), els)
  beta[zcols] <- b[zcols] / d$sd0[zcols]
  dev0 <- poisDev(d$y, exp(d$off + drop(d$M[, d$pen == 0, drop = FALSE] %*%
                                          ridgePois(d$M[, d$pen == 0, drop = FALSE], d$y, d$off, d$W, 0, d$pen[d$pen == 0]))), d$W)
  keep <- d$pen == 0
  cv0 <- mean(vapply(1:5, function(k){
    tr <- fold != k
    b <- ridgePois(d$M[tr, keep, drop = FALSE], d$y[tr], d$off[tr], d$W[tr], 0, d$pen[keep])
    poisDev(d$y[!tr], exp(d$off[!tr] + drop(d$M[!tr, keep, drop = FALSE] %*% b)), d$W[!tr])
  }, 1))
  logf("fit %s: %d cells, %d elements identified, lambda %.2g; CV deviance %.4f with the elements, %.4f without; in sample %.4f vs %.4f",
       label, nrow(d$M), length(zcols), lam, min(cv), cv0, poisDev(d$y, mu, d$W), dev0)
  list(label = label, beta = beta, ident = zcols, lambda = lam, cv = data.frame(lambda = LAMBDAS, dev = cv),
       cvNone = cv0,
       coef = b, mu = mu, etaL = etaL, site = d$site, nr = d$nr, y = d$y)
}

#today's DULN as the only element: does it explain the survey beyond NR at all?
fitCurrent <- function(e){
  w <- xlsxWeights()
  dul <- drop(e$X[, w$element] %*% w$walkNat)
  y <- e$y / mean(e$y); s <- e$sim$nearbyRandom; s <- s / mean(s); off <- log(pmax(s, 0.5 * min(s[s > 0])))
  z <- (dul - mean(dul)) / stats::sd(dul)
  m <- suppressWarnings(stats::glm(y ~ z, offset = off, family = stats::quasipoisson()))
  cf <- summary(m)$coefficients
  data.frame(ctx = e$ctx, gamma_perSD = cf["z", 1], se = cf["z", 2], p = cf["z", 4])
}

#Pearson at the site's head views of a base-grid map, the compare stage's way
.gridCache <- new.env()
headScores <- function(ctx, baseMap){
  cfg <- CTX[[ctx]]
  sv <- readRDS(surveyFile(ctx))
  if(is.null(.gridCache[[ctx]]))
    .gridCache[[ctx]] <- buildGrid(ctx, readRDS(aoiFile(ctx)), sv, readRDS(edgesFile(ctx)))
  G <- .gridCache[[ctx]]
  stats::setNames(vapply(HEAD_VIEWS(ctx), function(v){
    V <- G$views[[v]]
    m <- as.numeric(G$mBase)
    s <- as.vector(V$A %*% (baseMap * m)) / pmax(as.vector(V$A %*% m), 1e-12)
    stats::cor(trans(s, cfg$simTransform), surveyView(G, V, sv$surveyBase))
  }, 1), paste0("r", vapply(HEAD_VIEWS(ctx), function(v) G$views[[v]]$scale_m, 1)))
}

#A fit's coefficients (log usage per unit) in DULN units: scaled so the tailored edge
#pattern over the sites `ctxs` (centred per site) has today's spread there. The
#elements the fit did not identify keep their xlsx weight.
scaleFit <- function(ft, ctxs, ex, wOld, wOldNew, els){
  cen <- function(v, s) v - ave(v, s)
  s  <- unlist(lapply(ctxs, function(cx) rep(cx, nrow(ex[[cx]]$E))))
  Eo <- do.call(rbind, lapply(ctxs, function(cx) ex[[cx]]$E[, names(wOld)]))
  En <- do.call(rbind, lapply(ctxs, function(cx) ex[[cx]]$E[, els]))
  old <- cen(drop(Eo %*% wOld), s)
  id <- ft$ident
  P <- cen(drop(En[, id, drop = FALSE] %*% ft$beta[id]), s)
  keepEl <- setdiff(els, id)
  Q <- cen(drop(En[, keepEl, drop = FALSE] %*% wOldNew[keepEl]), s)
  #Var(aP + Q) = Var(old): the positive root
  vp <- stats::var(P); cpq <- stats::cov(P, Q); vq <- stats::var(Q); vt <- stats::var(old)
  a <- (-cpq + sqrt(max(cpq^2 - vp * (vq - vt), 0))) / vp
  wn <- wOldNew; wn[id] <- a * ft$beta[id]
  list(w = wn, a = a, ctxs = ctxs)
}

stageFit <- function(){
  f <- tdPath("fit.rds")
  if(file.exists(f)) return(invisible(readRDS(f)))
  ex <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx) readRDS(extractFile(ctx)))
  els <- ELEMENTS_NEW()
  pooled <- fitRidge(ex, els, "pooled")
  site <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx) fitRidge(ex[ctx], els, ctx))
  cur <- do.call(rbind, lapply(ex, fitCurrent))
  logf("today's DULN, alone, beyond NR (log usage per SD):")
  print(cur, digits = 3, row.names = FALSE)

  #the static bound: NR x exp(elements), on the site's head views; checked first on NR
  #itself against the scorer
  sb <- list()
  for(ctx in CONTEXTS){
    e <- ex[[ctx]]
    sc <- scorerFor(ctx)(runsFrom(ctx, "nearbyRandom", seq_len(e$nReps[["nearbyRandom"]])))
    mine <- headScores(ctx, e$simFull$nearbyRandom)
    if(max(abs(sc - mine)) > 1e-6) logf("WARNING static bound %s: NR head scores %s vs the scorer's %s",
                                        ctx, paste(round(mine, 4), collapse = "/"), paste(round(sc, 4), collapse = "/"))
    nrMap <- e$simFull$nearbyRandom
    #NR re-weighted in the fitted cells, mass kept (the other cells keep NR's value)
    reweight <- function(mult){
      m <- nrMap; i <- e$cells
      m[i] <- m[i] * mult * sum(m[i]) / sum(m[i] * mult)
      m
    }
    row <- function(lbl, map) data.frame(ctx = ctx, map = lbl, t(headScores(ctx, map)))
    ip <- pooled$site == ctx
    rows <- list(row("NR (stored)", nrMap), row("Nearby (stored)", e$simFull$nearby),
                 row("Full (stored)", e$simFull$full),
                 row("NR x exp(pooled elements)", reweight(exp(pooled$etaL[ip]))),
                 row("NR x exp(pooled elements + area)", reweight(pooled$mu[ip] / pooled$nr[ip])),
                 row("NR x exp(site elements)", reweight(exp(site[[ctx]]$etaL))),
                 row("NR x exp(site elements + area)", reweight(site[[ctx]]$mu / site[[ctx]]$nr)))
    sb[[ctx]] <- rbindFill(rows)
  }
  sb <- rbindFill(sb)
  utils::write.csv(sb, tdPath("static_bound.csv"), row.names = FALSE)
  options(width = 200)
  print(sb, digits = 3, row.names = FALSE)

  #the weights, in DULN units: scaled so the tailored edge pattern has today's spread
  #(per-site centred), shifted to today's mean per site
  w <- xlsxWeights()
  wOld <- stats::setNames(w$walkNat, w$element)
  wOldNew <- c(wOld[setdiff(names(wOld), "lakesAndBigRivers")],
               lakeNear = wOld[["lakesAndBigRivers"]], lakeBand = wOld[["lakesAndBigRivers"]])
  sp <- scaleFit(pooled, CONTEXTS, ex, wOld, wOldNew, els)
  ss <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(cx)
    scaleFit(site[[cx]], cx, ex, wOld, wOldNew, els))
  wt <- data.frame(element = els, xlsx = unname(wOldNew[els]),
                   identified_pooled = els %in% pooled$ident,
                   beta_pooled = unname(pooled$beta[els]), w_pooled = unname(sp$w[els]),
                   check.names = FALSE)
  for(cx in CONTEXTS){
    wt[[paste0("beta_", cx)]] <- unname(site[[cx]]$beta[els])
    wt[[paste0("w_", cx)]] <- unname(ss[[cx]]$w[els])
  }
  #how common each element is in the comparison cells
  for(cx in CONTEXTS) wt[[paste0("share_", cx)]] <- colMeans(ex[[cx]]$X[, els] != 0)
  utils::write.csv(wt, tdPath("weights.csv"), row.names = FALSE)
  print(wt[order(-abs(wt$w_pooled - wt$xlsx)), c("element", "xlsx", "w_pooled",
                                                  paste0("w_", CONTEXTS), paste0("share_", CONTEXTS))],
        digits = 3, row.names = FALSE)
  res <- list(pooled = pooled, site = site, current = cur, static = sb, weights = wt,
              scale = list(pooled = sp, site = ss), wOld = wOld, wOldNew = wOldNew)
  saveRDS(res, f)
  invisible(res)
}

#### stage loso: leave one site out ####

#Per held-out site: the fit on the OTHER two sites (pooled, the same recipe), scaled
#on their edges, and its static bound on the held-out site. Nothing of the held-out
#site's survey enters; its own DULN only sets the shift b0 (stageVariants()).
#GlattPark lies mostly inside Glatt (87 % of its areas), so a training site's cells
#on the held-out site's ground are dropped too: else "without GlattPark" would learn
#GlattPark's ground from Glatt's survey of it.
subsetEx <- function(e, keep){
  e$X <- e$X[keep, , drop = FALSE]; e$y <- e$y[keep]; e$cells <- e$cells[keep]
  e$sim <- lapply(e$sim, function(v) v[keep]); e$logHa <- e$logHa[keep]
  e$aoi <- e$aoi[keep]; e$xy <- e$xy[keep, , drop = FALSE]
  e
}
#a site's extract without its cells on another site's areas
dropGround <- function(e, other){
  held <- sf::st_union(sf::st_make_valid(sf::st_transform(readRDS(aoiFile(other))$polys, 2056)))
  pts <- sf::st_as_sf(data.frame(x = e$xy[, 1], y = e$xy[, 2]), coords = c("x", "y"), crs = 2056)
  inHeld <- lengths(sf::st_intersects(pts, held)) > 0
  if(any(inHeld)) logf("%s: %d of its %d cells lie on %s's areas and are left out",
                       e$ctx, sum(inHeld), length(inHeld), other)
  subsetEx(e, !inHeld)
}
#the held-out site's NR re-weighted by a fit made elsewhere, on its head views
staticElsewhere <- function(ctx, e, ft, label){
  eta <- drop(e$X[, ft$ident, drop = FALSE] %*% ft$beta[ft$ident])
  m <- e$simFull$nearbyRandom; i <- e$cells
  mult <- exp(eta - mean(eta))
  m[i] <- m[i] * mult * sum(m[i]) / sum(m[i] * mult)
  data.frame(ctx = ctx, map = label, t(headScores(ctx, m)))
}

stageLoso <- function(){
  f <- tdPath("fit_loso.rds")
  if(file.exists(f)) return(invisible(readRDS(f)))
  fit <- readRDS(tdPath("fit.rds"))
  ex <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx) readRDS(extractFile(ctx)))
  els <- ELEMENTS_NEW()
  #the share of the held-out site's areas that a training site's areas also cover
  #(GlattPark lies inside Glatt's extent)
  ov <- function(a, b){
    ga <- sf::st_union(sf::st_make_valid(sf::st_transform(readRDS(aoiFile(a))$polys, 2056)))
    gb <- sf::st_union(sf::st_make_valid(sf::st_transform(readRDS(aoiFile(b))$polys, 2056)))
    i <- sf::st_intersection(ga, gb)
    if(!length(i)) return(0)
    as.numeric(sum(sf::st_area(i))) / as.numeric(sf::st_area(ga))
  }
  out <- list(); sb <- list(); ovl <- list()
  for(ctx in CONTEXTS){
    train <- setdiff(CONTEXTS, ctx)
    for(t in train){
      o <- ov(ctx, t)
      ovl[[length(ovl) + 1]] <- data.frame(heldOut = ctx, training = t, areaShared = o)
      logf("loso %s: %.1f %% of its areas also lie in %s's", ctx, 100 * o, t)
    }
    exT <- lapply(stats::setNames(train, train), function(t) dropGround(ex[[t]], ctx))
    ft <- fitRidge(exT, els, paste0("without ", ctx))
    sc <- scaleFit(ft, train, ex, fit$wOld, fit$wOldNew, els)
    #the static bound, out of sample: the held-out site's NR re-weighted by the
    #training fit (its level does not matter: the mass is kept)
    sb[[ctx]] <- staticElsewhere(ctx, ex[[ctx]], ft, "NR x exp(elements fitted WITHOUT this site)")
    out[[ctx]] <- list(fit = ft, scale = sc, train = train)
  }
  sb <- rbindFill(sb)
  print(sb, digits = 3, row.names = FALSE)
  res <- list(loso = out, static = sb, overlap = do.call(rbind, ovl))
  saveRDS(res, f)
  invisible(res)
}

#### stage single: fit on ONE site, use it on the other two ####

#Per training site t and test site u: the per-site recipe on t's cells (without
#those on u's areas: GlattPark lies mostly inside Glatt), scaled on t's edges. u's
#survey never enters; u's own DULN only sets the shift b0.
stageSingle <- function(){
  f <- tdPath("fit_single.rds")
  if(file.exists(f)) return(invisible(readRDS(f)))
  fit <- readRDS(tdPath("fit.rds"))
  ex <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx) readRDS(extractFile(ctx)))
  els <- ELEMENTS_NEW()
  out <- list(); sb <- list()
  for(t in CONTEXTS) for(u in setdiff(CONTEXTS, t)){
    k <- paste0(t, ">", u)
    ft <- fitRidge(list(dropGround(ex[[t]], u)), els, paste0("on ", t, " for ", u))
    sc <- scaleFit(ft, t, ex, fit$wOld, fit$wOldNew, els)
    out[[k]] <- list(fit = ft, scale = sc, train = t, test = u)
    sb[[k]] <- staticElsewhere(u, ex[[u]], ft, paste0("NR x exp(elements fitted on ", t, " only)"))
  }
  sb <- rbindFill(sb)
  print(sb, digits = 3, row.names = FALSE)
  res <- list(single = out, static = sb)
  saveRDS(res, f)
  invisible(res)
}

#### stage table: every element and its weights, one CSV ####

#What each element is, from GenerateUpdateAttractivityLayer.R (the xlsx's own
#descriptions are terse). The layer value is the number of buffer rings round the
#feature that cover the cell (0 = none), unless said otherwise.
ELEMENT_INFO <- c(
  altitude = "DEM 10 m in 44 equal classes (~100 m each); value = class",
  slope = "slope classes: 0 (<6 deg), 1 (6-9), 2 (9-20), 4 (20-45), 10 (>45)",
  UNESCO_natural = "UNESCO World Natural Heritage sites",
  UNESCO_cultural = "UNESCO cultural sites (BAK)",
  Biosphere = "biosphere reserves",
  forestProNatura = "Pro Natura nature reserves",
  forestReserves = "cantonal forest reserves",
  birdSanctuary = "water and migratory bird reserves (WZVV 2023)",
  recreationalAreas = "TLM3D leisure areas (Freizeitareal 1, 6)",
  culturalElements = "OSM castles, artworks, towers, ruins, archaeological sites, mills, lighthouses, forts; TLM monuments",
  naturalElements_tlm = "TLM3D waterfalls and caves",
  peaks = "TLM3D named peaks and hills",
  viewpoints = "OSM viewpoints",
  benches = "OSM benches",
  publicParks = "OSM parks (areas and points)",
  naturalParks = "Swiss parks of national importance (status P)",
  playgrounds = "OSM playgrounds",
  food = "OSM cafes, restaurants, pubs, fast food, bakeries, beer gardens",
  toilet = "OSM toilets",
  picnic_sites = "OSM picnic sites",
  wetlands = "TLM3D land cover: wetland (11)",
  forests = "TLM3D land cover: forest, open forest (12, 13)",
  trees_r = "kernel density of single trees outside forests, classes 0-3",
  hikingTrails = "TLM3D roads and paths marked as hiking trails (WANDERWEGE 0-2)",
  bikeRoutes = "Veloland Schweiz bike routes",
  highways = "TLM3D motorways (OBJEKTART 0-2), not in tunnels",
  largeRoads = "TLM3D 6-10 m roads (OBJEKTART 8, 9, 20), not in tunnels",
  medRoads = "TLM3D 3-4 m roads (OBJEKTART 10, 11)",
  lakesAndBigRivers = "TLM3D land cover: standing and flowing water areas (5, 10); rings 50/100/150 m (on the water: 3)",
  smallRivers = "TLM3D small rivers above ground (FLIESSGEWAESSER 4), lake cells removed",
  narrowPaths = "TLM3D 1 m paths (OBJEKTART 16, 17)",
  medPaths = "TLM3D 2 m paths (OBJEKTART 15, 18)",
  naturePaths = "TLM3D paths with natural surface (BELAGSART 200)",
  hardPaths = "TLM3D paths with sealed surface (BELAGSART 100)",
  fields_r = "land cover 2018: farmland (classes 11-18), within 50 m (resampled: 0-1)",
  stairs = "TLM3D stairs (KUNSTBAUTE 900)",
  urban = "land cover 2018: settlement (1, 2, 3, 5) plus rings 100/200 m (resampled: 0-3)",
  lakeNear = "TAILORED: the 50 and 100 m rings of lakesAndBigRivers (0-2)",
  lakeBand = "TAILORED: the 150 m ring, widened for lakes >= 10 ha to the path loop round the lake (ceiling 300 m), 0/1")

stageTable <- function(){
  w <- xlsxWeights()
  fit <- readRDS(tdPath("fit.rds"))
  lo <- if(file.exists(tdPath("fit_loso.rds"))) readRDS(tdPath("fit_loso.rds")) else NULL
  ex <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx) readRDS(extractFile(ctx)))
  meta <- tryCatch({
    x <- readxl::read_xlsx(XLSX, range = readxl::cell_rows(c(2, NA)))
    x <- as.data.frame(x[seq_len(37), ], check.names = FALSE)
    data.frame(element = x$Variables, label = x$Data, source = x$Source)
  }, error = function(e) data.frame(element = w$element, label = NA, source = NA))
  els <- c(w$element, LAKE_EL)
  tb <- data.frame(element = els, stringsAsFactors = FALSE)
  tb$label <- meta$label[match(els, meta$element)]
  tb$label[els == "lakeNear"] <- "lake and large rivers, near rings"
  tb$label[els == "lakeBand"] <- "lake and large rivers, outer band / lake loop"
  tb$source <- meta$source[match(els, meta$element)]
  tb$source[els %in% LAKE_EL] <- "TLM; maps/lakes.gdb; paths"
  tb$what <- unname(ELEMENT_INFO[els])
  tb$buffers_m <- c(w$buffers, "50, 100", "150 / lake loop")
  #the layer's value range over the two site boxes
  rng <- lapply(unique(BOX_OF), function(b) terra::minmax(terra::rast(layersFile(b))[[els]], compute = TRUE))
  tb$value_min <- apply(do.call(rbind, lapply(rng, function(r) r[1, ])), 2, min)[els]
  tb$value_max <- apply(do.call(rbind, lapply(rng, function(r) r[2, ])), 2, max)[els]
  for(cx in CONTEXTS) tb[[paste0("share_cells_", cx)]] <- unname(colMeans(ex[[cx]]$X[, els] != 0))
  #today's weights, every agent type (lakeNear and lakeBand did not exist)
  lab <- c(walkNat = "Nature walkers", walkSoc = "Social walkers", bikeSport = "Sports bike",
           ebikeNat = "Nature e-bike", jogger = "Joggers", dogNat = "Nature dog walkers",
           dogProx = "Proximity dog walkers")
  for(t in names(lab))
    tb[[paste0("w_", lab[[t]])]] <- unname(c(stats::setNames(w[[t]], w$element), lakeNear = NA, lakeBand = NA)[els])
  #the tailored Nature walker weights; every other type gets the same change (delta)
  pick <- function(wn) unname(c(wn, lakesAndBigRivers = NA)[els])
  tb[["tailored_NW_pooled_s1"]]   <- pick(fit$scale$pooled$w)
  tb[["tailored_NW_pooled_s0.5"]] <- pick(0.5 * fit$scale$pooled$w)
  base <- unname(c(stats::setNames(w$walkNat, w$element), fit$wOldNew[LAKE_EL])[els])
  tb[["delta_NW_pooled_s0.5"]] <- tb[["tailored_NW_pooled_s0.5"]] - base
  tb$delta_NW_pooled_s0.5[els == "lakesAndBigRivers"] <- -w$walkNat[w$element == "lakesAndBigRivers"]
  for(cx in CONTEXTS) tb[[paste0("tailored_NW_site_", cx, "_s1")]] <- pick(fit$scale$site[[cx]]$w)
  if(!is.null(lo)) for(cx in CONTEXTS)
    tb[[paste0("tailored_NW_without_", cx, "_s1")]] <- pick(lo$loso[[cx]]$scale$w)
  if(file.exists(tdPath("fit_single.rds"))){
    sg <- readRDS(tdPath("fit_single.rds"))$single
    for(k in names(sg))
      tb[[paste0("tailored_NW_from_", sg[[k]]$train, "_for_", sg[[k]]$test, "_s1")]] <- pick(sg[[k]]$scale$w)
  }
  tb$identified_pooled <- els %in% fit$pooled$ident
  tb$fitted_beta_pooled <- unname(fit$pooled$beta[els])
  f <- tdPath("DULN_elements_weights.csv")
  utils::write.csv(tb, f, row.names = FALSE, na = "")
  logf("table: %d elements written to %s", nrow(tb), f)
}

#### stage variants: the networks and areas with the tailored DULN ####

#The variant's weights on the NEW element set (lakeNear, lakeBand instead of
#lakesAndBigRivers), per site, before the per-site shift b0 and the strength s.
#NULL = the check (dN = 0).
VARIANTS_TD <- list(
  check          = list(kind = "check"),
  lakeBand       = list(kind = "hand", lake = 3),
  lakeBand_x3    = list(kind = "hand", lake = 9),
  fitPooled_s0.5 = list(kind = "pooled", s = 0.5),
  fitPooled_s1   = list(kind = "pooled", s = 1),
  fitPooled_s2   = list(kind = "pooled", s = 2),
  fitSite_s1     = list(kind = "site", s = 1),
  #leave one site out: each site gets the weights fitted on the other two
  loso_s0.5      = list(kind = "loso", s = 0.5),
  loso_s1        = list(kind = "loso", s = 1),
  #fitted on ONE site, used on the other two (none on the training site itself)
  from_glattPark_s0.5 = list(kind = "single", train = "glattPark", s = 0.5),
  from_glattPark_s1   = list(kind = "single", train = "glattPark", s = 1),
  from_wigger_s0.5    = list(kind = "single", train = "wigger", s = 0.5),
  from_wigger_s1      = list(kind = "single", train = "wigger", s = 1),
  from_glatt_s0.5     = list(kind = "single", train = "glatt", s = 0.5),
  from_glatt_s1       = list(kind = "single", train = "glatt", s = 1))
variantDir <- function(v) tdPath("variants", v)

#per site: the new weights (NEW set) and b0, so that dN = sum(w' X') + b0 - sum(w X)
variantWeights <- function(v, ctx, fit, ex){
  sp <- VARIANTS_TD[[v]]
  if(identical(sp$kind, "check")) return(NULL)
  if(identical(sp$kind, "hand")){
    w <- fit$wOldNew; w[LAKE_EL] <- sp$lake
  }else{
    w1 <- switch(sp$kind, pooled = fit$scale$pooled$w, site = fit$scale$site[[ctx]]$w,
                 loso = readRDS(tdPath("fit_loso.rds"))$loso[[ctx]]$scale$w,
                 single = readRDS(tdPath("fit_single.rds"))$single[[paste0(sp$train, ">", ctx)]]$scale$w)
    w <- sp$s * w1
  }
  els <- names(w)
  #a variant moves WHERE DULN is high, not its level: the site's mean edge DULN stays
  #(the area DULN is re-centred in stageVariants())
  E <- ex$E
  b0 <- mean(drop(E[, names(fit$wOld)] %*% fit$wOld)) - mean(drop(E[, els] %*% w))
  list(w = w, b0 = b0)
}

#the change per element: the new weights (lakeNear, lakeBand) minus the old
#(lakesAndBigRivers), so dN = sum(d X) + b0 touches only the elements that changed
deltaWeights <- function(vw, fit){
  els <- union(names(fit$wOld), names(vw$w))
  d <- stats::setNames(numeric(length(els)), els)
  d[names(vw$w)] <- vw$w
  d[names(fit$wOld)] <- d[names(fit$wOld)] - fit$wOld
  d
}
#dN at 10 m over the site, as a raster
deltaRaster <- function(vw, ctx, fit){
  X <- terra::rast(layersFile(BOX_OF[[ctx]]))
  sl <- sf::st_bbox(sf::st_transform(readRDS(aoiFile(ctx))$shapeLarger, 2056))
  X <- terra::crop(X, terra::ext(sl[["xmin"]] - 1500, sl[["xmax"]] + 1500, sl[["ymin"]] - 1500, sl[["ymax"]] + 1500))
  d <- deltaWeights(vw, fit)
  d <- d[abs(d) > 1e-12]
  if(!length(d)) return(X[[1]] * 0 + vw$b0)
  weighted(X, d) + vw$b0
}

#generateAoI2's area summary on a walkNat crop (lakes NA)
areaSummary <- function(walkNat, polys){
  vals <- terra::extract(walkNat, terra::vect(polys))
  nm <- names(vals)[2]
  d <- vapply(split(vals[[nm]], vals$ID), function(x){
    x <- sort(x, decreasing = TRUE)
    if(!length(x)) return(NA_real_)
    (stats::median(x[seq_len(max(1L, length(x) %/% 4L))]) + stats::median(x)) / 2
  }, numeric(1))
  unname(d[as.character(seq_len(nrow(polys)))])
}

stageVariants <- function(){
  fit <- readRDS(tdPath("fit.rds"))
  log <- list()
  for(ctx in CONTEXTS){
    ex <- readRDS(extractFile(ctx))
    aoi <- readRDS(aoiFile(ctx))
    base <- readRDS(netFile(ctx))
    tm <- cogTemplate(ctx)
    #the stored walkNat with the lakes NA, as generateAoI2 reads it
    lakes <- ns$.vftReadLakesWithin(sf::st_transform(aoi$shapeLarger, 2056))
    wn0 <- tm
    if(nrow(lakes)) wn0[terra::vect(sf::st_transform(lakes, 4326))] <- NA
    a0 <- areaSummary(wn0, aoi$polys)
    logf("variants %s: generateAoI2's summary on the stored walkNat vs the save's area DULN: r %.4f, median |d| %.3f",
         ctx, stats::cor(a0, aoi$polys$DULN, use = "complete"), stats::median(abs(a0 - aoi$polys$DULN), na.rm = TRUE))
    va <- igraph::vertex_attr(base$net)
    #the car parks carry the area DULN of their car park's nearest area
    #(R/prepare_network.R:332): found back by value, else the node's nearest area
    pk <- which(va$parkingAttr != 0 & is.finite(va$parkingAttr))
    pkPoly <- match(va$parkingAttr[pk], aoi$polys$DULN)
    if(anyNA(pkPoly)){
      miss <- which(is.na(pkPoly))
      pts <- sf::st_transform(sf::st_as_sf(data.frame(x = va$X[pk[miss]], y = va$Y[pk[miss]]),
                                           coords = c("x", "y"), crs = 4326), 2056)
      pkPoly[miss] <- sf::st_nearest_feature(pts, sf::st_transform(aoi$polys, 2056))
      logf("variants %s: %d of %d car-park nodes matched by their nearest area, not by value",
           ctx, length(miss), length(pk))
    }
    for(v in names(VARIANTS_TD)){
      #a single-site fit is not used on its own site
      if(identical(VARIANTS_TD[[v]]$kind, "single") && identical(VARIANTS_TD[[v]]$train, ctx)) next
      vd <- variantDir(v)
      fNet <- file.path(vd, "network", paste0(ctx, "_net.rds"))
      if(file.exists(fNet)) next
      t0 <- proc.time()[["elapsed"]]
      vw <- variantWeights(v, ctx, fit, ex)
      net <- base$net
      polys <- aoi$polys
      if(!is.null(vw)){
        dw <- deltaWeights(vw, fit)
        dE <- drop(ex$E[, names(dw)] %*% dw) + vw$b0
        dN <- deltaRaster(vw, ctx, fit)
        d4 <- to4326(dN, tm)
        dV <- terra::extract(d4, cbind(va$X, va$Y))
        dV <- dV[[ncol(dV)]]
        dV[!is.finite(dV)] <- 0
        wn1 <- wn0 + d4
        dA <- areaSummary(wn1, polys) - a0
        dA[!is.finite(dA)] <- 0
        #the site's mean area DULN stays too: the area summary is over every cell of
        #an area, not its paths, so the edge shift b0 does not hold it (forests, most
        #of an area's ground, carry other weights than its paths)
        dAraw <- mean(dA)
        dA <- dA - dAraw
        cls <- class(net)
        ea <- igraph::edge_attr(net)
        stopifnot(identical(ea$edgeID, ex$edgeID))
        for(cn in ns$VFT_DULN_COLS) ea[[cn]] <- ea[[cn]] + dE
        igraph::edge_attr(net) <- ea
        vv <- igraph::vertex_attr(net)
        for(cn in ns$VFT_DULN_COLS) vv[[cn]] <- vv[[cn]] + dV
        polys$DULN <- polys$DULN + dA
        #a sampling weight: clamped at 0 where the tailored area DULN goes negative,
        #as vftHeatwaveParkingAttr() does (else sample() stops: "negative probability")
        vv$parkingAttr[pk] <- pmax(polys$DULN[pkPoly], 0)
        igraph::vertex_attr(net) <- vv
        class(net) <- cls
        net <- ns$vftRefreshAttrWeights(net, aoi$thr)
        log[[length(log) + 1]] <- data.frame(variant = v, ctx = ctx, b0 = vw$b0,
          dEdge_mean = mean(dE), dEdge_sd = stats::sd(dE), dNode_sd = stats::sd(dV, na.rm = TRUE),
          dArea_level = dAraw, dArea_sd = stats::sd(dA), dArea_min = min(dA), dArea_max = max(dA),
          edgeWalk_min = min(ea$DULN_WALK_), edgeWalk_med = stats::median(ea$DULN_WALK_),
          edgeWalk_max = max(ea$DULN_WALK_), r_edge_old = stats::cor(dE + igraph::edge_attr(base$net, "DULN_WALK_"),
                                                                    igraph::edge_attr(base$net, "DULN_WALK_")))
      }else{
        #the check runs the same refresh, and must change nothing
        net <- ns$vftRefreshAttrWeights(net, aoi$thr)
        same <- identical(igraph::edge_attr(net), igraph::edge_attr(base$net)) &&
          identical(igraph::vertex_attr(net), igraph::vertex_attr(base$net))
        logf("variants %s check: network after the refresh %s", ctx, if(same) "IDENTICAL" else "DIFFERENT")
        if(!same) stop(ctx, ": the check network differs from the stored one")
      }
      a2 <- aoi; a2$polys <- polys
      dir.create(file.path(vd, "aoi"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(vd, "network"), recursive = TRUE, showWarnings = FALSE)
      saveAtomic(a2, file.path(vd, "aoi", paste0(ctx, ".rds")))
      file.copy(edgesFile(ctx), file.path(vd, "network", basename(edgesFile(ctx))), overwrite = TRUE)
      file.copy(metaFile(ctx), file.path(vd, "network", basename(metaFile(ctx))), overwrite = TRUE)
      saveAtomic(list(net = net, parking = base$parking), fNet)
      logf("variants %s %s: written, %.0f s", ctx, v, proc.time()[["elapsed"]] - t0)
    }
  }
  if(length(log)){
    lg <- do.call(rbind, log)
    f <- tdPath("variants.csv")
    if(file.exists(f)){
      old <- utils::read.csv(f)
      lg <- rbind(old[!paste(old$variant, old$ctx) %in% paste(lg$variant, lg$ctx), ], lg)
    }
    utils::write.csv(lg, f, row.names = FALSE)
    options(width = 220)
    print(lg, digits = 3, row.names = FALSE)
  }
}

#### stage rasters: the tailored Nature walk layers, for a look in GIS ####

stageRasters <- function(){
  fit <- readRDS(tdPath("fit.rds"))
  for(ctx in CONTEXTS) for(v in c("lakeBand_x3", "fitPooled_s1", "fitSite_s1")){
    f <- tdPath("rasters", paste0(ctx, "_", v, "_walkNat_10m.tif"))
    if(file.exists(f)) next
    ex <- readRDS(extractFile(ctx))
    vw <- variantWeights(v, ctx, fit, ex)
    dN <- deltaRaster(vw, ctx, fit)
    X <- terra::crop(terra::rast(layersFile(BOX_OF[[ctx]])), dN)
    old <- weighted(X, fit$wOld)
    terra::writeRaster(c(old + dN, dN, old), f, overwrite = TRUE, names = c("tailored", "delta", "current"),
                       gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    tm <- cogTemplate(ctx)
    terra::writeRaster(c(tm + to4326(dN, tm), tm), tdPath("rasters", paste0(ctx, "_", v, "_walkNat_4326.tif")),
                       overwrite = TRUE, names = c("tailored", "current"), gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
    logf("rasters %s %s written", ctx, v)
  }
}

#### run ####

t00 <- proc.time()[["elapsed"]]
if("layers" %in% TD_STAGES) for(box in unique(BOX_OF)) stageLayers(box)
if("reproduce" %in% TD_STAGES) stageReproduce()
if("extract" %in% TD_STAGES) for(ctx in CONTEXTS) stageExtract(ctx)
if("fit" %in% TD_STAGES) stageFit()
if("loso" %in% TD_STAGES) stageLoso()
if("single" %in% TD_STAGES) stageSingle()
if("table" %in% TD_STAGES) stageTable()
if("variants" %in% TD_STAGES) stageVariants()
if("rasters" %in% TD_STAGES) stageRasters()
logf("tailor_duln: done in %.1f min, output in %s", (proc.time()[["elapsed"]] - t00) / 60, TD)
