## Verification for the corrected route cost and the heatwave simulation
## (vftAttrWeights() in R/prepare_network.R, R/heatwave_helpers.R, and the
## heatwave arguments of vftSimulateScenario() / launchMultiSim()).
##
##   1. the route cost: a more attractive edge counts as shorter, the documented
##      anchor holds, and the C++ route finder takes the attractive detour
##   2. which prepared graphs get their weights recomputed, and which are left alone
##   3. the exposure: dPET per edge, node and area, on a NON-square raster (this
##      repository has shipped a transposed raster before), gaps, the car-park clamp
##   4. the ABM: kappa = 0 / attrWeight = 1 / legFactor = 8 is a normal run; heat
##      moves visitors the right way in each of the three decisions; attrWeight and
##      legFactor strengthen attractivity against distance; nothing heated leaks
##      into what goes back to the app
##   5. --daemon: the packed raster survives a real mirai daemon (needs the package
##      INSTALLED from this tree: R CMD INSTALL --no-docs .)
##
## Run:  Rscript data-raw/verify_heatwave.R [--daemon] [--quick]
##   --quick   fewer seeds in group 4 (directions are then less certain)
## Inputs: a step-5 save with a real network, as in verify_abm_speed.R
## (VFT_ABM_SAVES overrides).
##
## NEVER a global named `dayPop` in here: launchMultiSim() tests exists("dayPop"),
## which searches up to the global environment, and would skip the agents' goals.

args   <- commandArgs(trailingOnly = TRUE)
daemon <- "--daemon" %in% args
quick  <- "--quick"  %in% args

suppressPackageStartupMessages({
  pkgload::load_all(".", export_all = TRUE, quiet = TRUE)
})
ns <- asNamespace("visitorFlowTool")

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

#### 1. the route cost ####
cat("=== 1. the route cost: more attractive = shorter ===\n")
mt <- 5; cc <- 34.2423 + mt
types <- names(VFT_DULN_TYPE_COLS)
dulnList <- function(d) stats::setNames(rep(list(d), 7), VFT_DULN_TYPE_COLS)
w <- vftAttrWeights(rep(100, 5), dulnList(c(10, 20, 30, 40, 60)), mt)
ok("21 columns, named SHAPE_Leng_<type><suffix>",
   length(w) == 21 && all(c("SHAPE_Leng_walkNat", "SHAPE_Leng_jogger_attr",
                            "SHAPE_Leng_dogProx_ATTR") %in% names(w)))
ok("monotone: a more attractive edge costs less (all 21)",
   all(vapply(w, function(v) all(diff(v) < 0), logical(1))))
a <- vftAttrWeights(100, dulnList(cc), mt)
ok("anchor: at DULN = c the reduction is 1/8, 1/4, 1/2",
   isTRUE(all.equal(c(a$SHAPE_Leng_walkNat, a$SHAPE_Leng_walkNat_attr,
                      a$SHAPE_Leng_walkNat_ATTR), c(87.5, 75, 50))))
z <- vftAttrWeights(c(100, 100), dulnList(c(0, -5)), mt)
ok("DULN <= 0 costs the full length, never more", all(unlist(z) == 100))
f <- vftAttrWeights(c(100, 3), dulnList(c(500, 20)), mt)
ok("never below the 10 m floor", all(unlist(f) >= 10))
ok("NULL minThresh reads as 0",
   identical(vftAttrWeights(100, dulnList(30), NULL), vftAttrWeights(100, dulnList(30), 0)))

#the user's rule, through the real route finder: 1-2-4 is short and plain
#(2 x 50 m at DULN 20), 1-3-4 the attractive detour (DULN 40)
toyRoute <- function(detourLen, method){
  len  <- c(50, 50, detourLen / 2, detourLen / 2)
  duln <- c(20, 20, 40, 40)
  et <- data.frame(from = c(1L, 2L, 1L, 3L), to = c(2L, 4L, 3L, 4L))
  ww <- vftAttrWeights(len, dulnList(duln), mt)
  for(nm in names(ww)) et[[nm]] <- ww[[nm]]
  vt <- data.frame(nodeID = 1:4, lat = c(0, 1, -1, 0), lon = c(0, 1, 1, 2))
  lp <- generateAdjListAndDistTbl_cpp(edgeTable = et, vertexTable = vt)
  ptr <- adjListsToPtr_cpp(lp)
  findShortestRoute_cpp(ptr, method, 1L, 4L, "walkNat")$path[[1]][1]
}
ok("legs (f = 8): a 5 % longer attractive detour wins",   toyRoute(105, "distance") == 3)
ok("legs (f = 8): a 10 % longer one does not (~7 % limit)", toyRoute(110, "distance") == 2)
ok("_attr (f = 4): the 10 % longer attractive detour wins", toyRoute(110, "little_attr") == 3)
ok("_ATTR (f = 2): a 30 % longer one wins",                 toyRoute(130, "ATTR") == 3)

#### a real scenario ####
saves <- Sys.getenv("VFT_ABM_SAVES", "")
saves <- if(nzchar(saves)) strsplit(saves, ";", fixed = TRUE)[[1]] else
  file.path(Sys.getenv("USERPROFILE", path.expand("~")), "Downloads",
            "visitorFlowSave_step5_simulation_2026_08_31_10_30_42.148.RData")
saves <- saves[file.exists(saves)]
if(!length(saves)) stop("no save file found - set VFT_ABM_SAVES")
e <- new.env(); load(saves[[1]], envir = e)
raw <- e$envBase_network
if(is.null(raw)) raw <- e$envBase_networkList[[1]]$network
polys <- e$envBase_finalPolygons
thr   <- e$envBase_minThresh
cat(sprintf("\n(scenario %s: %d nodes, %d edges, %d areas)\n", basename(saves[[1]]),
            igraph::vcount(raw), igraph::ecount(raw), nrow(polys)))
prep <- suppressMessages(vftPrepareNetwork(raw, polys, thr, parking = e$envBase_parking))
net  <- prep$network

#### 2. which graphs are recomputed ####
cat("\n=== 2. stale weights are recomputed, deliberate ones are not ===\n")
ok("a freshly prepared graph is current", vftWeightsCurrent(net))
old <- igraph::delete_graph_attr(net, "vftWeights")
ok("an unstamped graph (prepared before the fix) is not", !vftWeightsCurrent(old))
ref <- vftRefreshAttrWeights(old, thr)
ok("refreshing restamps it, with the same weights as preparing",
   vftWeightsCurrent(ref) &&
     identical(igraph::edge_attr(ref)[grep("^SHAPE_Leng_.", igraph::edge_attr_names(ref))],
               igraph::edge_attr(net)[grep("^SHAPE_Leng_.", igraph::edge_attr_names(net))]))
ok("refreshing keeps a tbl_graph a tbl_graph", inherits(ref, "tbl_graph") || !inherits(net, "tbl_graph"))
na <- igraph::set_edge_attr(net, "SHAPE_Leng_walkNat", index = 1, value = NA_real_)
ok("an NA weight (an edge added after preparation) is not current", !vftWeightsCurrent(na))
sw <- net
for(nm in grep("^SHAPE_Leng_.", igraph::edge_attr_names(sw), value = TRUE))
  sw <- igraph::set_edge_attr(sw, nm, value = pmax(igraph::edge_attr(sw, "SHAPE_Leng"), 10))
ok("a stamped graph with deliberately swapped weights is left alone", vftWeightsCurrent(sw))
eid <- igraph::edge_attr(net, "edgeID_2")[1]
if(!is.null(eid)){
  sh <- vftShiftAttractivity(net, eid, stats::setNames(rep(1, 7), VFT_DULN_COLS))
  ok("an attractivity edit drops the stamp", !vftWeightsCurrent(sh))
}else{
  ok("an attractivity edit drops the stamp (no edgeID_2 here: skipped)", TRUE)
}

#### 3. the exposure ####
cat("\n=== 3. dPET per edge, node and area ===\n")
et <- vftGraphTibble(net, "edges")
vt <- vftGraphTibble(net, "nodes")
e2056 <- sf::st_transform(et$SHAPE, 2056)
bb <- sf::st_bbox(e2056)
#NON-square on purpose, and the extra margin is on one axis only
tpl <- terra::rast(xmin = bb[["xmin"]] - 400, xmax = bb[["xmax"]] + 400,
                   ymin = bb[["ymin"]] - 100, ymax = bb[["ymax"]] + 100,
                   resolution = 5, crs = "EPSG:2056")
cat(sprintf("(raster %d x %d cells)\n", terra::ncol(tpl), terra::nrow(tpl)))
hp <- VFT_HEATWAVE

r35 <- terra::init(tpl, 3.5)
t0  <- proc.time()[["elapsed"]]
ex  <- vftHeatwaveExposure(et, vt, polys, r35, hp)
cat(sprintf("(exposure of %d edges, %d nodes, %d areas: %.2f s)\n",
            nrow(et), nrow(vt), nrow(polys), proc.time()[["elapsed"]] - t0))
ok("a constant raster gives exactly that everywhere",
   all(ex$edge == 3.5) && all(ex$node == 3.5) && all(ex$area == 3.5))
ok("one value per edge row, node row and area",
   length(ex$edge) == nrow(et) && length(ex$node) == nrow(vt) && length(ex$area) == nrow(polys))
ok("areas named by their letter", identical(names(ex$area), as.character(polys$AOI)))
ok("full coverage reported", all(ex$coverage == 1))

#one row per edge, whatever its shape: the mean of its vertices
crd  <- sf::st_coordinates(e2056)
mid  <- cbind(X = as.numeric(tapply(crd[, "X"], crd[, "L1"], mean)),
              Y = as.numeric(tapply(crd[, "Y"], crd[, "L1"], mean)))
stopifnot(nrow(mid) == nrow(et))
nodeXY <- sf::st_coordinates(sf::st_transform(vt$geometry, 2056))
areaXY <- sf::st_coordinates(sf::st_centroid(sf::st_transform(sf::st_geometry(polys), 2056)))
exX <- vftHeatwaveExposure(et, vt, polys, terra::init(tpl, "x"), hp)
exY <- vftHeatwaveExposure(et, vt, polys, terra::init(tpl, "y"), hp)
ok("edge dPET tracks the edge's x (raster = x)", stats::cor(exX$edge, mid[, "X"]) > 0.99,
   sprintf("r = %.4f", stats::cor(exX$edge, mid[, "X"])))
ok("edge dPET tracks the edge's y (raster = y): not transposed",
   stats::cor(exY$edge, mid[, "Y"]) > 0.99, sprintf("r = %.4f", stats::cor(exY$edge, mid[, "Y"])))
ok("node dPET is within 5 m of the node's own x", max(abs(exX$node - nodeXY[, "X"])) < 5)
ok("node dPET is within 5 m of the node's own y", max(abs(exY$node - nodeXY[, "Y"])) < 5)
if(nrow(polys) > 2){
  ok("area dPET tracks the area's x", stats::cor(exX$area, areaXY[, "X"]) > 0.95,
     sprintf("r = %.3f", stats::cor(exX$area, areaXY[, "X"])))
}

xcut <- stats::median(mid[, "X"])
half <- terra::ifel(terra::init(tpl, "x") > xcut, NA, terra::init(tpl, "x"))
exH  <- vftHeatwaveExposure(et, vt, polys, half, hp)
ok("half coverage is reported as about half", abs(exH$coverage[["edge"]] - 0.5) < 0.1,
   sprintf("%.2f", exH$coverage[["edge"]]))
#entirely beyond the cut: uncovered. Reaching 5 m before it: covered. (Edges
#inside the 10 m strip either way shift the median by metres at most, on a
#raster whose values are x coordinates.)
minX <- as.numeric(tapply(crd[, "X"], crd[, "L1"], min))
unc  <- minX > xcut + 5
cov  <- minX < xcut - 5
vUnc <- unique(exH$edge[unc])
ok("uncovered edges take the median of the covered ones",
   any(unc) && length(vUnc) == 1 && abs(vUnc - stats::median(exH$edge[cov])) < 50,
   sprintf("%.1f vs %.1f", vUnc[1], stats::median(exH$edge[cov])))
ok("covered edges keep their own value",
   all(exH$edge[cov & tapply(crd[, "X"], crd[, "L1"], max) < xcut - 5] < xcut))
ok("no gaps left", !anyNA(exH$edge) && !anyNA(exH$node) && !anyNA(exH$area))

pk <- vt$parking
if(!is.null(pk) && any(pk > 0, na.rm = TRUE)){
  big <- vftHeatwaveParkingAttr(vt, polys, stats::setNames(rep(10, nrow(polys)), polys$AOI), 1e6)
  ok("car-park weights are clamped at 0 under extreme heat",
     all(big[pk > 0 & is.finite(pk)] == 0) && all(big >= 0 | is.na(big)))
  same <- vftHeatwaveParkingAttr(vt, polys, ex$area, 0)
  ok("kappa = 0 leaves the car parks exactly as they are", identical(same, vt$parkingAttr))
  cool <- vftHeatwaveParkingAttr(vt, polys, stats::setNames(rep(-5, nrow(polys)), polys$AOI), 1)
  ok("a cool area makes its car parks more attractive",
     all(cool[pk > 0 & is.finite(pk)] > vt$parkingAttr[pk > 0 & is.finite(pk)]))
}

#### 4. the ABM ####
cat("\n=== 4. the ABM ===\n")
SEEDS <- if(quick) 1:3 else 1:8
#launchSim() needs a progress handle; this one goes nowhere
nullBar <- list(set = function(...) invisible(NULL), inc = function(...) invisible(NULL),
                close = function() invisible(NULL))
sim <- function(heat = NULL, params = VFT_HEATWAVE, seed = 1L){
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  utils::capture.output(out <- suppressMessages(suppressWarnings(
    vftSimulateScenario(net, polys, thr, parking = prep$parking, residentDivision = 50,
                        progPrep = nullBar, progSim = nullBar,
                        heat = heat, heatParams = params))))
  out
}
fp <- function(out){
  g <- out$results$pathUsage
  list(e = igraph::edge_attr(g)[grep("^passage", igraph::edge_attr_names(g))],
       v = igraph::vertex_attr(g)[grep("^passage", igraph::vertex_attr_names(g))],
       start = out$results$dayPop$startV, goal = out$results$dayPop$AOIGoal)
}
neutral <- utils::modifyList(VFT_HEATWAVE, list(kappa = 0, attrWeight = 1, legFactor = 8))

#a raster that is NOT uniform, so the identity is not an accident of a constant
grad <- terra::init(tpl, "x"); grad <- (grad - terra::global(grad, "mean")[[1]]) / 200
norm1 <- sim(seed = 1L)
neu1  <- sim(terra::wrap(grad), neutral, seed = 1L)
ok("kappa 0, attrWeight 1, legFactor 8: identical to a normal run",
   identical(fp(norm1), fp(neu1)))
ok("...and it is labelled: results$heatwave carries the parameters",
   identical(neu1$results$heatwave$kappa, 0) && is.null(norm1$results$heatwave))

hw1 <- sim(terra::wrap(grad), VFT_HEATWAVE, seed = 1L)
g0 <- igraph::edge_attr(net); g1 <- igraph::edge_attr(hw1$results$pathUsage)
cols <- c(VFT_DULN_TYPE_COLS, grep("^SHAPE_Leng_.", names(g0), value = TRUE))
ok("no leak: pathUsage keeps the plain edge DULN and weights",
   identical(g0[cols], g1[cols]))
ok("no leak: pathUsage keeps the plain node DULN",
   identical(igraph::vertex_attr(net)[VFT_DULN_TYPE_COLS],
             igraph::vertex_attr(hw1$results$pathUsage)[VFT_DULN_TYPE_COLS]))
ok("a prepared, current graph is not sent back", is.null(hw1$network))
ok("a heatwave run changes the outcome", !identical(fp(norm1), fp(hw1)))

#over several seeds, each effect against its own baseline
runs <- function(heat, params) lapply(SEEDS, function(s) sim(heat, params, s))
goals <- function(rs) unlist(lapply(rs, function(o) o$results$dayPop$AOIGoal))
edgeSum <- function(rs, col) Reduce(`+`, lapply(rs, function(o)
  as.numeric(igraph::edge_attr(o$results$pathUsage, col))))
base <- runs(NULL, VFT_HEATWAVE)
gb   <- goals(base); gb <- gb[gb != "NO_GOAL"]
tab  <- sort(table(gb), decreasing = TRUE)
cat(sprintf("(baseline: %d goals over %d seeds; areas: %s)\n", length(gb), length(SEEDS),
            paste(sprintf("%s %d", names(tab), tab), collapse = ", ")))

#which areas the edges and nodes lie in
nodeAOI <- igraph::vertex_attr(net, "AOI")
edgeAOI <- ifelse(nodeAOI[et$from] == nodeAOI[et$to], nodeAOI[et$from], "0")

#(B) destination: the busiest area made hot, the second made cool
if(length(tab) >= 2){
  hotA <- names(tab)[1]; coolA <- names(tab)[2]
  rB <- terra::rasterize(terra::vect(sf::st_transform(polys[polys$AOI %in% c(hotA, coolA), ], 2056)),
                         tpl, field = ifelse(polys$AOI[polys$AOI %in% c(hotA, coolA)] == hotA, 10, -5),
                         background = 0)
  onlyHeat <- utils::modifyList(VFT_HEATWAVE, list(attrWeight = 1, legFactor = 8))
  gB <- goals(runs(terra::wrap(rB), onlyHeat)); gB <- gB[gB != "NO_GOAL"]
  sh <- function(g, a) mean(g == a)
  ok(sprintf("destination: the hot area %s loses visitors", hotA),
     sh(gB, hotA) < sh(gb, hotA), sprintf("%.3f -> %.3f", sh(gb, hotA), sh(gB, hotA)))
  ok(sprintf("destination: the cool area %s gains visitors", coolA),
     sh(gB, coolA) > sh(gb, coolA), sprintf("%.3f -> %.3f", sh(gb, coolA), sh(gB, coolA)))
}

#(C) inside the busiest area: its western half hot, its eastern half cool
pAOI <- edgeSum(base, "passageAOI")
busiest <- names(sort(tapply(pAOI, edgeAOI, sum)[names(tapply(pAOI, edgeAOI, sum)) != "0"],
                      decreasing = TRUE))[1]
inZ  <- edgeAOI == busiest
xmid <- stats::median(mid[inZ, "X"])
zPoly <- terra::vect(sf::st_transform(polys[polys$AOI == busiest, ], 2056))
rC <- terra::ifel(terra::init(tpl, "x") < xmid, 10, -5)
rC <- terra::mask(rC, zPoly, updatevalue = 0)
onlyHeat <- utils::modifyList(VFT_HEATWAVE, list(attrWeight = 1, legFactor = 8))
pC <- edgeSum(runs(terra::wrap(rC), onlyHeat), "passageAOI")
hotShare <- function(p) sum(p[inZ & mid[, "X"] < xmid]) / max(sum(p[inZ]), 1)
ok(sprintf("movement: inside area %s, passage moves to its cool half", busiest),
   hotShare(pC) < hotShare(pAOI), sprintf("hot-half share %.3f -> %.3f", hotShare(pAOI), hotShare(pC)))

#(A) the legs, deterministically: the baseline's own start/goal pairs routed on
#the adjacency a heatwave run builds, with and without a hot band across the
#network outside the areas. Both at the heatwave's legFactor, so only the heat
#differs. (The ABM's passage would show the same, buried in its noise.)
pass  <- function(rs) edgeSum(rs, "passage") - edgeSum(rs, "passageAOI")
ymid  <- stats::median(mid[, "Y"]); band <- abs(mid[, "Y"] - ymid) < 150 & edgeAOI == "0"
aoiV  <- terra::vect(sf::st_transform(polys, 2056))
rA <- terra::ifel(abs(terra::init(tpl, "y") - ymid) < 150, 10, 0)
rA <- terra::mask(rA, aoiV, inverse = TRUE, updatevalue = 0)
legAdj <- function(kappa){
  ex <- vftHeatwaveExposure(et, vt, polys, rA, hp)
  duln <- vftHeatwaveDULN(unclass(et)[VFT_DULN_TYPE_COLS], ex$edge, kappa)
  w  <- vftAttrWeights(et$SHAPE_Leng, duln, thr, factors = c(hp$legFactor, VFT_ATTR_FACTORS[-1]))
  t2 <- unclass(et); t2[names(w)] <- w; class(t2) <- class(et)
  adjListsToPtr_cpp(generateAdjListAndDistTbl_cpp(edgeTable = t2, vertexTable = vt))
}
#homes to area nodes. Not the baseline's dayPop: by the end of a run every agent
#that went home has goalV == startV.
set.seed(7L)
homes <- vt$nodeID[is.finite(vt$Residents) & vt$Residents > 0]
inAoi <- vt$nodeID[nodeAOI != "0"]
od <- data.frame(startV = as.integer(sample(homes, 400, replace = TRUE)),
                 goalV  = as.integer(sample(inAoi, 400, replace = TRUE)),
                 agentTyp = sample(names(VFT_DULN_TYPE_COLS), 400, replace = TRUE))
od <- od[od$startV != od$goalV, ]
edgeMap <- buildEdgeMap_cpp(as.data.frame(et[, c("from", "to", "edgeID")]))
routeRows <- function(ptr){
  pth <- findShortestRoute_cpp(ptr, "distance", od$startV, od$goalV, od$agentTyp)$path
  nodes <- Map(function(s, p) c(s, p), od$startV, pth)
  a <- unlist(lapply(nodes, function(v) utils::head(v, -1)))
  b <- unlist(lapply(nodes, function(v) v[-1]))
  utils::capture.output(ids <- getEdges_cpp(as.integer(a), as.integer(b), edgeMap, FALSE,
                                             list(NULL), list(NULL))$currentE[[1]])
  match(ids, et$edgeID)
}
bandLen <- function(rows){ rows <- rows[!is.na(rows)]
  sum(et$SHAPE_Leng[rows][band[rows]]) / sum(et$SHAPE_Leng[rows]) }
r0 <- routeRows(legAdj(0)); r1 <- routeRows(legAdj(hp$kappa))
ok("legs: the way to the areas avoids a hot band",
   bandLen(r1) < bandLen(r0), sprintf("band share of route length %.3f -> %.3f", bandLen(r0), bandLen(r1)))
legOnly0 <- utils::modifyList(VFT_HEATWAVE, list(kappa = 0, attrWeight = 1))
legOnly1 <- utils::modifyList(VFT_HEATWAVE, list(attrWeight = 1))
bandShare <- function(p) sum(p[band]) / max(sum(p), 1)
pA0 <- pass(runs(terra::wrap(rA), legOnly0)); pA1 <- pass(runs(terra::wrap(rA), legOnly1))
cat(sprintf("(for information - the ABM's passage outside the areas on the band: %.3f -> %.3f)\n",
            bandShare(pA0), bandShare(pA1)))

#attractivity against distance, with no heat at all. The utility's attractivity
#term is DULN * min(sqrt(area), 7000) / duration, so that is what must rise.
noHeat <- utils::modifyList(VFT_HEATWAVE, list(kappa = 0, legFactor = 8))
xOf <- stats::setNames(polys$DULN * pmin(sqrt(as.numeric(polys$area)), 7000), polys$AOI)
gW <- goals(runs(terra::wrap(grad), utils::modifyList(noHeat, list(attrWeight = 3))))
gW <- gW[gW != "NO_GOAL"]
ok("attrWeight 3: chosen areas offer more attractivity x size",
   mean(xOf[gW]) > mean(xOf[gb]), sprintf("mean DULN*sqrt(area) %.0f -> %.0f", mean(xOf[gb]), mean(xOf[gW])))
legDULN <- function(p) sum(p * et$DULN_WALK_, na.rm = TRUE) / max(sum(p), 1)
pL8 <- pass(base)
pL2 <- pass(runs(terra::wrap(grad), utils::modifyList(noHeat, list(attrWeight = 1, legFactor = 2))))
ok("legFactor 2: the legs run over more attractive edges",
   legDULN(pL2) > legDULN(pL8), sprintf("passage-weighted DULN %.2f -> %.2f", legDULN(pL8), legDULN(pL2)))

#### 5. a real daemon ####
if(daemon){
  cat("\n=== 5. a real mirai daemon (INSTALLED package) ===\n")
  mirai::daemons(1)
  m <- mirai::mirai({
    .libPaths(libs)
    suppressPackageStartupMessages(library(visitorFlowTool))
    ns <- asNamespace("visitorFlowTool")
    set.seed(1L, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    h <- list(set = function(...) NULL, inc = function(...) NULL, close = function() NULL)
    utils::capture.output(out <- ns$vftSimulateScenario(net, polys, thr, parking = parking,
                                                        residentDivision = 50,
                                                        progPrep = h, progSim = h,
                                                        heat = heat, heatParams = params))
    out
  }, net = net, polys = polys, thr = thr, parking = prep$parking, heat = terra::wrap(grad),
     params = VFT_HEATWAVE, libs = .libPaths(), .timeout = 600e3)
  res <- mirai::call_mirai(m)$data
  mirai::daemons(0)
  ok("the daemon run completes", !inherits(res, "miraiError") && !inherits(res, "errorValue"),
     if(inherits(res, "miraiError")) format(res) else "")
  if(is.list(res) && !is.null(res$results))
    ok("...identical to the in-process run", identical(fp(res), fp(hw1)))
}

cat(sprintf("\n%s\n", if(fails == 0) "ALL PASS" else paste(fails, "FAILED")))
quit(status = if (fails == 0) 0 else 1)
