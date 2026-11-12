## Verification for the heat mitigation search (R/heat_need_helpers.R): where
## usage falls most from the normal run to the heatwave run(s), the record it is
## stored with, and what a run still needs - plus the conflict search's
## clustering, which it now shares (vftCellCircles(), R/conflict_helpers.R).
##
## Run:  Rscript data-raw/verify_heat_need.R
## Pure: no app, no data.

suppressPackageStartupMessages({
  pkgload::load_all(".", export_all = TRUE, quiet = TRUE)
})

fails <- 0
ok <- function(what, cond, extra = ""){
  cat(sprintf("%-70s %s %s\n", what, if(isTRUE(cond)) "PASS" else "FAIL", extra))
  if(!isTRUE(cond)) fails <<- fails + 1
}

#### a synthetic network: 50 m segments on a 2 x 2 km lattice, EPSG:2056 ####
x0 <- 2600000; y0 <- 1200000; step <- 50; n <- 40
segs <- list(); mids <- NULL
for(i in 0:n) for(j in 0:(n - 1)){
  segs[[length(segs) + 1]] <- sf::st_linestring(rbind(c(x0 + i * step, y0 + j * step),
                                                     c(x0 + i * step, y0 + (j + 1) * step)))
  mids <- rbind(mids, c(i * step, (j + .5) * step))
  segs[[length(segs) + 1]] <- sf::st_linestring(rbind(c(x0 + j * step, y0 + i * step),
                                                     c(x0 + (j + 1) * step, y0 + i * step)))
  mids <- rbind(mids, c((j + .5) * step, i * step))
}
set.seed(1)
edges <- sf::st_sf(edgeID = seq_along(segs), passage = round(100 + stats::runif(length(segs), 0, 50)),
                   geometry = sf::st_sfc(segs, crs = 2056))
near <- function(cx, cy, r = 120) sqrt((mids[, 1] - cx)^2 + (mids[, 2] - cy)^2) < r
A <- near(500, 500); B <- near(1500, 500); C <- near(1000, 1500)
noisy <- function(p) round(p * (1 + stats::rnorm(length(p), 0, 0.05)))

mor <- edges; mor$passage <- noisy(edges$passage)
aft <- edges; aft$passage <- noisy(edges$passage)
mor$passage[A | C] <- round(edges$passage[A | C] * 0.4)
aft$passage[B | C] <- round(edges$passage[B | C] * 0.45)

#### 1. the search ####
dist_to <- function(h, cx, cy){
  p <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(h, coords = c("lng", "lat"), crs = 4326), 2056))
  sqrt((p[, 1] - x0 - cx)^2 + (p[, 2] - y0 - cy)^2)
}
which_at <- function(h, cx, cy){ d <- dist_to(h, cx, cy); if(min(d) < 100) h$when[which.min(d)] else NA }

h <- vftHeatNeedHotspots(edges, list(morning = mor, afternoon = aft))
ok("both times of day: three circles", nrow(h) == 3, nrow(h))
ok("...A is morning-only", identical(which_at(h, 500, 500), "morning"))
ok("...B is afternoon-only", identical(which_at(h, 1500, 500), "afternoon"))
ok("...C is both", identical(which_at(h, 1000, 1500), "both"))
ok("...losses are about 55-60 %", all(h$lossPct > 45 & h$lossPct < 70), paste(round(h$lossPct), collapse = ","))
ok("...radii cover the ~120 m drop areas", all(h$radius_m > 80 & h$radius_m < 250), paste(round(h$radius_m), collapse = ","))
ok("...strongest first", !is.unsorted(rev(h$score)))
ok("...per-bin loss columns", all(c("morning", "afternoon") %in% names(h)))

h1 <- vftHeatNeedHotspots(edges, list(afternoon = aft))
ok("afternoon only: B and C", nrow(h1) == 2 && !is.na(which_at(h1, 1500, 500)) &&
     !is.na(which_at(h1, 1000, 1500)) && is.na(which_at(h1, 500, 500)))
ok("...all labelled afternoon", all(h1$when == "afternoon"))
ok("...no morning column", !"morning" %in% names(h1))

#a heatwave that costs usage EVERYWHERE: places, not one circle over the area
wide <- edges; wide$passage <- round(edges$passage * 0.5)
hw <- vftHeatNeedHotspots(edges, list(afternoon = wide))
ok("a loss everywhere gives compact circles, not one over the area",
   nrow(hw) == 5 && all(hw$radius_m < 200), paste(round(hw$radius_m), collapse = ","))
ok("...that do not overlap much", {
  p <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(hw, coords = c("lng", "lat"), crs = 4326), 2056))
  min(stats::dist(p)) > 150 })

#noise only: nothing to circle
calm <- edges; calm$passage <- noisy(edges$passage)
h0 <- vftHeatNeedHotspots(edges, list(afternoon = calm))
ok("noise alone finds nothing (the floors hold)", nrow(h0) == 0)
ok("...and the empty result has the columns", all(c("lng", "when", "afternoon") %in% names(h0)))
#more usage in the heat (shade) is not a need
shade <- edges; shade$passage[A] <- edges$passage[A] * 2
ok("a RISE under heat is not circled", nrow(vftHeatNeedHotspots(edges, list(morning = shade))) == 0)

#edge order does not matter: matched by edgeID
shuf <- aft[sample(nrow(aft)), ]
h2 <- vftHeatNeedHotspots(edges, list(afternoon = shuf))
ok("shuffled heat edges give the same circles", isTRUE(all.equal(h1, h2)))
ok("in 4326 too (the same circles, to rounding)",
   isTRUE(all.equal(h1, vftHeatNeedHotspots(sf::st_transform(edges, 4326), list(afternoon = aft)),
                    tolerance = 0.01)))
spot <- edges; spot$passage <- edges$passage; spot$passage[which(A)[1]] <- 0
ok("one short edge's drop is a spot, not an area", nrow(vftHeatNeedHotspots(edges, list(morning = spot))) == 0)
ok("no heat runs: zero rows", nrow(vftHeatNeedHotspots(edges, list())) == 0)

#### 2. the shared clustering is the conflict search's, unchanged ####
oldSrc <- system2("git", c("show", "HEAD:R/conflict_helpers.R"), stdout = TRUE)
oldEnv <- new.env(parent = asNamespace("visitorFlowTool"))
eval(parse(text = oldSrc), envir = oldEnv)
sm <- terra::rast(xmin = x0, xmax = x0 + 2000, ymin = y0, ymax = y0 + 2000, resolution = 10, crs = "EPSG:2056")
set.seed(2)
terra::values(sm) <- stats::runif(terra::ncell(sm))^6
ok("vftConflictHotspots() identical to HEAD's",
   isTRUE(all.equal(vftConflictHotspots(sm, edges), oldEnv$vftConflictHotspots(sm, edges))))
sm4326 <- terra::project(sm, "EPSG:4326")
ok("...in 4326 too",
   isTRUE(all.equal(vftConflictHotspots(sm4326, edges), oldEnv$vftConflictHotspots(sm4326, edges))))

#### 3. the record, freshness and the plan ####
graph <- function(p) tidygraph::tbl_graph(nodes = data.frame(id = 1:3),
                                          edges = data.frame(from = c(1, 2), to = c(2, 3), passage = p))
rec <- function(bin, rev) list(bin = bin, rev = rev, base = TRUE, card = "versionBtn1",
                               params = VFT_HEATWAVE, summary = list())
sc <- list(network = "net")
sc <- vftScenarioSetSim(sc, "normal", list(pathUsage = graph(c(10, 20)), dayPop = NULL))
sc <- vftScenarioSetSim(sc, "afternoon", list(pathUsage = graph(c(5, 20)), dayPop = NULL,
                                              heatwave = rec("afternoon", 3L)))
ok("fingerprint: edge count and sum", identical(vftSimFingerprint(graph(c(10, 20))), c(2, 30)))
ok("nothing stored: fresh is NA", is.na(vftHeatNeedFresh(sc, 3L)))
sc$heatNeed <- list(hotspots = h1, bins = "afternoon", key = vftHeatNeedKey(sc, "afternoon"))
ok("just stored: fresh", isTRUE(vftHeatNeedFresh(sc, 3L)))
ok("painted since (rev bumped): stale", isFALSE(vftHeatNeedFresh(sc, 4L)))
sc2 <- vftScenarioSetSim(sc, "normal", list(pathUsage = graph(c(11, 20)), dayPop = NULL))
ok("normal re-run: stale", isFALSE(vftHeatNeedFresh(sc2, 3L)))
sc3 <- vftScenarioSetSim(sc, "afternoon", NULL)
ok("heat run dropped: stale", isFALSE(vftHeatNeedFresh(sc3, 3L)))
sc4 <- sc; sc4$pathUsage <- NULL; sc4$heatSims <- NULL
ok("network edited (sims gone): stale, but still stored", isFALSE(vftHeatNeedFresh(sc4, 3L)) &&
     !is.null(sc4$heatNeed$hotspots))

has <- function(...) function(b) b %in% c(...)
p <- vftHeatNeedPlan(sc, "afternoon", has("afternoon"), 3L)
ok("plan: everything present -> nothing to run", vftHeatNeedPlanEmpty(p))
p <- vftHeatNeedPlan(sc, c("morning", "afternoon"), has("afternoon"), 3L)
ok("plan both: morning map + morning sim", identical(p$heatMaps, "morning") &&
     identical(p$heatSims, "morning") && !p$normal)
p <- vftHeatNeedPlan(sc, "afternoon", has("afternoon"), 4L)
ok("plan: stale heat run is re-run", identical(p$heatSims, "afternoon") && !length(p$heatMaps))
p <- vftHeatNeedPlan(sc, "afternoon", has(), 3L)
ok("plan: a missing map means a new map AND a new run",
   identical(p$heatMaps, "afternoon") && identical(p$heatSims, "afternoon"))
p <- vftHeatNeedPlan(list(network = "net"), "morning", has("morning"), 1L)
ok("plan: empty scenario needs the normal run and the heat run", p$normal && identical(p$heatSims, "morning"))
ok("choices -> bins", identical(vftHeatNeedBins("both"), c("morning", "afternoon")) &&
     identical(vftHeatNeedBins("morning"), "morning") && !length(vftHeatNeedBins("midday")))

#### 4. the UI pieces build ####
ui <- as.character(vftHeatNeedPlanUI(vftHeatNeedPlan(sc, c("morning", "afternoon"), has("afternoon"), 3L),
                                     c("morning", "afternoon"), shiny::NS("step5"), NULL, here = FALSE))
ok("step 5 checklist: a missing map sends to Hitzeminderung",
   grepl("Zur Hitzeminderung und starten", ui) && grepl("wird auf der Seite Hitzeminderung berechnet", ui))
ui <- as.character(vftHeatNeedPlanUI(vftHeatNeedPlan(sc, "afternoon", has("afternoon"), 3L),
                                     "afternoon", shiny::NS("x"), NULL, here = TRUE))
ok("nothing to run: the button says Erkennen", grepl(">Erkennen<", ui) && grepl("x-heatNeedGo", ui))
pA <- vftHeatNeedPlan(sc, "afternoon", has("afternoon"), 3L); pA$aoi <- TRUE
ui <- as.character(vftHeatNeedPlanUI(pA, "afternoon", shiny::NS("x"), NULL, here = TRUE))
ok("no Zielgebiete: listed first, and the button says Starten",
   !vftHeatNeedPlanEmpty(pA) && grepl("werden automatisch bestimmt", ui) && grepl(">Starten<", ui) &&
     regexpr("Zielgebiete", ui) < regexpr("Normale Simulation", ui))
m <- leaflet::leaflet() |> vftHeatNeedDraw(h, NULL, stale = TRUE)
ok("draw: circles, labels and a legend", length(m$x$calls) == 3)
ok("...dashed when stale", grepl("8 6", jsonlite::toJSON(m$x$calls[[1]]$args, auto_unbox = TRUE)))

cat(sprintf("\n%d failure(s)\n", fails))
if(fails) quit(status = 1)
