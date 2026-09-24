## Verification for vftConflictHotspots() in R/conflict_helpers.R - the step 5
## "find biodiversity-recreation conflict" button.
##
## Synthetic landscape, EPSG:2056, 10 m cells over 2 x 2 km:
##   - sensitivity 0 almost everywhere, a band of low values (1-5) that sets the
##     top-10% quantile, and two high blobs A (10) and B (8), 100 m radius
##   - paths as horizontal lines in EPSG:4326, the way step 5 hands them over
##
## Checked (6 is a real mirai daemon):
##   1. a heavy path over A and a light one over B circles A only, and every
##      qualifying cell lies inside the circle
##   2. one heavy path over both blobs gives two circles, A (stronger) first
##   3. a heavy path that meets no sensitive cell gives zero rows, no error
##   4. a PackedSpatRaster and a lon/lat matrix give the same answer
##   5. everything runs with terra unattached (the app's condition)
##   6. the wrapped matrix and trimmed edges survive a mirai daemon unchanged
##   7. when a stored search may be shown again (vftScenarioConflicts)
##
## Run:  Rscript data-raw/verify_conflict.R
suppressPackageStartupMessages({library(shiny); library(terra); library(sf)})
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
env <- new.env(parent = globalenv())
for (f in sort(list.files(R, pattern = "[.][Rr]$", full.names = TRUE))) {
  suppressWarnings(try(sys.source(f, envir = env), silent = TRUE))
}
attach(env, warn.conflicts = FALSE)

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

X0 <- 2600000; Y0 <- 1200000
A <- c(X0 + 500,  Y0 + 505)
B <- c(X0 + 1500, Y0 + 505)

makeSM <- function(){
  sm <- terra::rast(xmin = X0, xmax = X0 + 2000, ymin = Y0, ymax = Y0 + 2000,
                    resolution = 10, crs = "EPSG:2056")
  xy <- terra::xyFromCell(sm, seq_len(terra::ncell(sm)))
  v  <- numeric(nrow(xy))
  set.seed(1)
  band <- xy[, 2] > Y0 + 1000 & xy[, 2] < Y0 + 1600
  v[band] <- stats::runif(sum(band), 1, 5)
  v[sqrt((xy[, 1] - A[1])^2 + (xy[, 2] - A[2])^2) <= 100] <- 10
  v[sqrt((xy[, 1] - B[1])^2 + (xy[, 2] - B[2])^2) <= 100] <- 8
  terra::values(sm) <- v
  sm
}

#horizontal line at y, from x1 to x2, in 2056 -> handed over in 4326
path <- function(y, x1, x2, passage){
  g <- sf::st_sfc(sf::st_linestring(rbind(c(x1, y), c(x2, y))), crs = 2056)
  sf::st_transform(sf::st_sf(passage = passage, geometry = g), 4326)
}

#the circle centre back in metres, and its distance to a point
centre2056 <- function(h, i = 1){
  sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(h$lng[i], h$lat[i])), crs = 4326), 2056))[1, ]
}
distTo <- function(h, p, i = 1) sqrt(sum((centre2056(h, i) - p)^2))

sm <- makeSM()

cat("=== 1. heavy path over A, light over B ===\n")
e1 <- rbind(path(Y0 + 505, X0, X0 + 1000, 100),
            path(Y0 + 1505, X0, X0 + 2000, 1),
            path(Y0 + 1805, X0 + 1000, X0 + 2000, 1))
h1 <- vftConflictHotspots(sm, e1)
ok("exactly one circle", nrow(h1) == 1, sprintf("(%d)", nrow(h1)))
ok("centred on A (< 20 m)", nrow(h1) == 1 && distTo(h1, A) < 20,
   if(nrow(h1)) sprintf("(%.1f m)", distTo(h1, A)) else "")
ok("found at the first level (top 10%)", isTRUE(all.equal(h1$level[1], 0.1)))
## every qualifying cell = the heavy path's cells inside blob A
hotX <- seq(A[1] - 95, A[1] + 95, by = 10)
inside <- all(sqrt((hotX - centre2056(h1)[1])^2 + (A[2] - centre2056(h1)[2])^2) <= h1$radius_m[1])
ok("every hot cell centre lies inside the circle", inside)
ok("radius is tight (< 150 m for a 200 m wide crossing)", h1$radius_m[1] < 150,
   sprintf("(%.1f m)", h1$radius_m[1]))

cat("\n=== 2. one heavy path over both blobs ===\n")
e2 <- rbind(path(Y0 + 505, X0, X0 + 2000, 100),
            path(Y0 + 1805, X0, X0 + 2000, 1))
h2 <- vftConflictHotspots(sm, e2)
ok("two circles", nrow(h2) == 2, sprintf("(%d)", nrow(h2)))
ok("A first (stronger sensitivity)", nrow(h2) == 2 && distTo(h2, A, 1) < 20)
ok("B second", nrow(h2) == 2 && distTo(h2, B, 2) < 20)
ok("scores descending", nrow(h2) == 2 && h2$score[1] > h2$score[2])
ok("maxCircles = 1 keeps only A",
   nrow(vftConflictHotspots(sm, e2, maxCircles = 1)) == 1)

cat("\n=== 3. no overlap anywhere ===\n")
e3 <- rbind(path(Y0 + 1905, X0, X0 + 2000, 100),
            path(Y0 + 1805, X0, X0 + 2000, 1))
h3 <- try(vftConflictHotspots(sm, e3), silent = TRUE)
ok("no error", !inherits(h3, "try-error"))
ok("zero rows, same columns", !inherits(h3, "try-error") && nrow(h3) == 0 &&
     identical(names(h3), names(h1)))
ok("NULL matrix: zero rows", nrow(vftConflictHotspots(NULL, e1)) == 0)
ok("empty edges: zero rows", nrow(vftConflictHotspots(sm, e1[0, ])) == 0)

cat("\n=== 4. other raster shapes ===\n")
h4 <- vftConflictHotspots(terra::wrap(sm), e1)
ok("PackedSpatRaster gives the same circle", isTRUE(all.equal(h4, h1)))
smLL <- terra::project(sm, "EPSG:4326", method = "near")
h5 <- vftConflictHotspots(smLL, e1)
ok("lon/lat matrix: one circle", nrow(h5) == 1, sprintf("(%d)", nrow(h5)))
ok("...on A (< 30 m)", nrow(h5) == 1 && distTo(h5, A) < 30,
   if(nrow(h5)) sprintf("(%.1f m)", distTo(h5, A)) else "")

cat("\n=== 5. terra unattached ===\n")
detach("package:terra")
h6 <- try(vftConflictHotspots(sm, e2), silent = TRUE)
suppressPackageStartupMessages(library(terra))
ok("runs with terra unattached (the app's condition)", !inherits(h6, "try-error"),
   if(inherits(h6, "try-error")) conditionMessage(attr(h6, "condition")) else "")
ok("...and gives the same answer", !inherits(h6, "try-error") && isTRUE(all.equal(h6, h2)))

cat("\n=== 6. across the process boundary (step 5 runs it in a daemon) ===\n")
## what obsConflict sends: the wrap()ped matrix and only the used edges, with
## only their usage column. A bare SpatRaster would arrive as a dead pointer.
edgesSent <- e2[is.finite(e2$passage) & e2$passage > 0, "passage"]
ok("trimming to used edges changes nothing",
   isTRUE(all.equal(vftConflictHotspots(sm, edgesSent), h2)))
mirai::daemons(1)
w <- mirai::everywhere({
  suppressPackageStartupMessages({library(terra); library(sf)})
  source(file.path(RD, "conflict_helpers.R"))
}, RD = R)
mirai::call_mirai(w)
m  <- mirai::mirai(vftConflictHotspots(sm, edges, "passage"),
                   sm = terra::wrap(sm), edges = edgesSent)
h7 <- mirai::call_mirai(m)$data
mirai::daemons(0)
ok("the daemon returns a data.frame, not an error",
   is.data.frame(h7), if(!is.data.frame(h7)) paste(format(h7), collapse = " ") else "")
ok("...with the same circles as the main thread",
   is.data.frame(h7) && isTRUE(all.equal(h7, h2)))

cat("\n=== 7. stored conflicts: when the newVersions page may show them ===\n")
k <- vftConflictKey(sm)
ok("the matrix key survives wrap()/unwrap()",
   isTRUE(all.equal(vftConflictKey(terra::wrap(sm)), k)))
ok("...and saveRDS()/readRDS() of the packed matrix (the save file)", {
  f <- tempfile(fileext = ".rds"); saveRDS(terra::wrap(sm), f)
  isTRUE(all.equal(vftConflictKey(readRDS(f)), k))
})
sm2 <- sm; sm2[1] <- 99
ok("a changed matrix has a different key", !isTRUE(all.equal(vftConflictKey(sm2), k)))
ok("no matrix, no key", is.null(vftConflictKey(NULL)))
sc <- list(pathUsage = "a simulation", conflicts = list(hotspots = h2, smKey = k))
ok("a searched, simulated scenario shows its circles",
   identical(vftScenarioConflicts(sc, k), h2))
ok("...not once an edit has dropped its simulation",
   is.null(vftScenarioConflicts(modifyList(sc, list(pathUsage = NULL)), k)))
ok("...not against a changed matrix",
   is.null(vftScenarioConflicts(sc, vftConflictKey(sm2))))
ok("...not with no matrix at all", is.null(vftScenarioConflicts(sc, NULL)))
ok("a search that found nothing shows nothing",
   is.null(vftScenarioConflicts(list(pathUsage = 1, conflicts = list(hotspots = h3, smKey = k)), k)))
ok("a scenario never searched shows nothing",
   is.null(vftScenarioConflicts(list(pathUsage = 1), k)))
ok("a NULL scenario shows nothing", is.null(vftScenarioConflicts(NULL, k)))

cat(sprintf("\n%s: %d failure(s)\n", if(fails) "FAILED" else "ALL PASS", fails))
quit(status = if(fails) 1 else 0)
