## Timing for the Hitzeminderung load path: the land cover baseline the map
## shows, a cold heat run, and the main-thread cost of drawing the surface.
## Run:  Rscript data-raw/profile_heat_load.R
##
## VFT_R points it at another tree's R/ (a `git worktree` of an older commit) so
## old and new can be run INTERLEAVED - timings on the dev box drift 2x over a
## day, so only back-to-back runs are comparable. VFT_DLL overrides the compiled
## code when that tree has none built.
##
## Measured 2026-09-24, interleaved, HEAD 7800cec -> this change, Sion:
##
##   area          baseline_s    heat_cold_s   project_s (main thread)
##   1.8 x 1.3 km  1.41-1.47 -> 0.58   ~1.8 -> ~1.8   0.23-0.35 -> 0 (daemon)
##   3.6 x 2.6 km  3.08-3.24 -> 1.26   3.83-3.96 -> 3.69-3.75   0.36 -> 0 (daemon)
##
## The baseline also left the main thread on a cache miss. The cold heat run
## hardly moved; what changed for heat is that the daemon's cache now survives
## between jobs (see heatCacheFor()), so a change of time of day is ~0.85 s
## instead of a cold build.
suppressPackageStartupMessages({library(terra); library(sf)})
terraOptions(progress = 0)
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
for (f in c("perf_helpers.R", "data_paths.R", "paintbrush_helpers.R",
            "heat_helpers.R", "shadow_helpers.R", "svf_helpers.R"))
  suppressWarnings(try(source(file.path(R, f)), silent = TRUE))
dll <- Sys.getenv("VFT_DLL", file.path(dirname(R), "src",
                                       paste0("visitorFlowTool", .Platform$dynlib.ext)))
dyn.load(dll)
source(file.path(dirname(dll), "..", "R", "RcppExports.R"))

CX <- 2593956; CY <- 1119554
mk <- function(w, h) st_sf(geometry = st_transform(st_sfc(st_polygon(list(rbind(
  c(CX-w/2, CY-h/2), c(CX+w/2, CY-h/2), c(CX+w/2, CY+h/2), c(CX-w/2, CY+h/2),
  c(CX-w/2, CY-h/2)))), crs = 2056), 4326))
el <- function(expr) { t0 <- proc.time()[["elapsed"]]; force(expr)
  proc.time()[["elapsed"]] - t0 }

out <- NULL
for (sz in list(c(1800, 1300), c(3600, 2600))) {
  aoi <- mk(sz[1], sz[2])
  base <- el(paintLandcoverBaselinePNG(aoi, cache = FALSE))
  cold <- el(h <- heatRaster(aoi, bin = "midday"))
  ## what drawHeat() does on the main thread: before, project + encode; after,
  ## the projection arrives from the daemon and only the encode is left
  proj <- el(p <- leaflet::projectRasterForLeaflet(h, "bilinear"))
  enc  <- el(leaflet::addRasterImage(leaflet::leaflet(), p, colors = heatPalette(h),
                                     project = FALSE))
  out <- rbind(out, data.frame(area = sprintf("%g x %g", sz[1], sz[2]),
                               baseline_s = base, heat_cold_s = cold,
                               project_s = proj, encode_s = enc))
}
cat("tree:", R, "\n")
print(out, row.names = FALSE, digits = 3)
