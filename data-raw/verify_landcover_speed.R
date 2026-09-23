## Verification for the land cover / heat load speed-up (2026-09-24).
## Run:  Rscript data-raw/verify_landcover_speed.R
##
## Every change it covers is meant to be EXACT - faster, same answer. So each
## check pits the new path against the one it replaced, on real Sion data:
##
##   1. paintLandcoverSeed() masks through one rasterised outline instead of
##      handing the polygon to mask() per layer
##   2. paintLandcoverBaselinePNG() encodes through GDAL instead of png::writePNG
##   3. a heat cache restored from disk answers like the one that was saved
##
## Exits non-zero on any failure.
suppressPackageStartupMessages({library(terra); library(sf)})
terraOptions(progress = 0)
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
for (f in c("perf_helpers.R", "data_paths.R", "paintbrush_helpers.R",
            "heat_helpers.R", "shadow_helpers.R", "svf_helpers.R")) {
  suppressWarnings(try(source(file.path(R, f)), silent = TRUE))
}
## the working tree's compiled code, not the installed package - see
## verify_heat_model.R for why
{
  .dll <- file.path(dirname(R), "src", paste0("visitorFlowTool", .Platform$dynlib.ext))
  if(!file.exists(.dll))
    stop("compiled code missing: ", .dll,
         " -- build it with:  Rscript -e 'pkgbuild::compile_dll(\".\")'")
  dyn.load(.dll)
  source(file.path(R, "RcppExports.R"))
}

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-64s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}
if(!file.exists(file.path(paintLandcoverDir(), "ground_CH_1m.tif")))
  stop("national land cover not found in ", paintLandcoverDir())

CX <- 2593956; CY <- 1119554
rect <- function(w, h, dx = 0, dy = 0)
  st_polygon(list(rbind(c(CX+dx-w/2, CY+dy-h/2), c(CX+dx+w/2, CY+dy-h/2),
                        c(CX+dx+w/2, CY+dy+h/2), c(CX+dx-w/2, CY+dy+h/2),
                        c(CX+dx-w/2, CY+dy-h/2))))
## a lobed outline, so the mask edge crosses cells at every angle
ang <- seq(0, 2*pi, length.out = 40)[-40]; rr <- 900 + 300*sin(5*ang)
lobed <- st_polygon(list(rbind(cbind(CX + rr*cos(ang), CY + rr*sin(ang)),
                               c(CX + rr[1], CY))))
as_aoi <- function(p) st_sf(geometry = st_transform(st_sfc(p, crs = 2056), 4326))
same <- function(a, b) {
  va <- values(a, mat = FALSE); vb <- values(b, mat = FALSE)
  length(va) == length(vb) && identical(is.na(va), is.na(vb)) &&
    all(va[!is.na(va)] == vb[!is.na(vb)])
}

cat("=== 1. one rasterised outline masks like mask(vect) ===\n")
for (nm in c("rectangle", "lobed")) {
  p   <- if (nm == "rectangle") rect(1800, 1300) else lobed
  aoi <- as_aoi(p)
  s   <- paintLandcoverSeed(aoi)
  raw <- paintLandcoverSeed(aoi, mask = FALSE)
  shp <- st_buffer(st_union(st_transform(st_geometry(aoi), 2056)), 250)
  mv  <- vect(shp)
  ok(sprintf("%-9s ground identical to mask(r, vect)", nm), same(s$ground, mask(raw$ground, mv)))
  ok(sprintf("%-9s canopy identical to mask(r, vect)", nm), same(s$canopy, mask(raw$canopy, mv)))
  ok(sprintf("%-9s mask actually removed cells", nm), anyNA(values(s$ground)))
}

cat("\n=== 2. the GDAL-encoded baseline decodes to the same ids ===\n")
old_encode <- function(r) {
  valid <- c(0L, PAINT_CATEGORIES$id)
  v <- terra::values(r); v[is.na(v)] <- 0; v[!v %in% valid] <- 0
  matrix(as.numeric(v), nrow = terra::nrow(r), byrow = TRUE)
}
decode <- function(uri) {
  img <- png::readPNG(jsonlite::base64_dec(sub("^data:image/png;base64,", "", uri)))
  round((if (length(dim(img)) == 3) img[, , 1] else img) * 255)
}
aoi <- as_aoi(lobed)
seed <- paintLandcoverSeed(aoi)
msg <- paintLandcoverBaselinePNG(aoi, cache = FALSE)
ok("ground decodes to the old encoder's ids", identical(decode(msg$ground), old_encode(seed$ground)))
ok("canopy decodes to the old encoder's ids", identical(decode(msg$canopy), old_encode(seed$canopy)))
ok("w/h match the seed", msg$w == ncol(seed$ground) && msg$h == nrow(seed$ground))
## the bit-flip squash: a 128 and a 67 must come out as 0, not as themselves.
## Exercised through the encoder on a hand-made raster, since the national file
## is not ours to edit.
g <- seed$ground; g[3, 3] <- 128; g[4, 4] <- 67; g[5, 5] <- 2
local({
  e <- environment(paintLandcoverBaselinePNG)
  f <- get("paintLandcoverSeed", envir = e)
  assign("paintLandcoverSeed", function(...) list(ground = g, canopy = seed$canopy), envir = e)
  on.exit(assign("paintLandcoverSeed", f, envir = e))
  m <- paintLandcoverBaselinePNG(aoi, cache = FALSE)
  d <- decode(m$ground)
  ok("stray ids 128 and 67 are squashed to 0", d[3, 3] == 0 && d[4, 4] == 0)
  ok("a valid id next to them survives", d[5, 5] == 2)
})
ok("no .aux.xml sidecar left in tempdir",
   !length(list.files(tempdir(), pattern = "\\.png\\.aux\\.xml$")))

cat("\n=== 3. a heat cache restored from disk answers like the saved one ===\n")
aoi <- as_aoi(rect(1400, 1000))
dir <- file.path(tempdir(), "vft_heat_verify"); unlink(dir, recursive = TRUE)
key <- "verify-session"
cold <- heatRaster(aoi, bin = "midday")
## first job: builds and saves
h1 <- heatRasterPacked(aoi, bin = "midday", key = key, cacheDir = dir)
ok("the job saved its cache", file.exists(heatCacheFile(dir, key)))
## a second process would have no in-memory entry: drop ours to simulate it
rm(list = key, envir = get(".vft_heatCaches", envir = .GlobalEnv))
t0 <- proc.time()[[3]]
h2 <- heatRasterPacked(aoi, bin = "afternoon", key = key, cacheDir = dir)
t_restored <- proc.time()[[3]] - t0
t0 <- proc.time()[[3]]
c2 <- heatRaster(aoi, bin = "afternoon")
t_cold <- proc.time()[[3]] - t0
ok("restored cache: afternoon identical to a cold run",
   max(abs(values(unwrap(h2)) - values(c2)), na.rm = TRUE) < 1e-9)
## warm, not merely right: a change of time of day off the restored cache
## rebuilds shade and wall only, so it must beat a cold run by a wide margin
ok("restored cache is warm (under 60 % of a cold run)", t_restored < 0.6 * t_cold,
   sprintf("(%.2f s restored vs %.2f s cold)", t_restored, t_cold))
ok("the templates came back as geometry",
   inherits(get(key, envir = get(".vft_heatCaches", envir = .GlobalEnv))$tpl, "SpatRaster"))
ok("restored cache: midday unchanged", max(abs(values(unwrap(h1)) - values(cold)), na.rm = TRUE) < 1e-9)
## a corrupt file is a cold cache, never an error
rm(list = key, envir = get(".vft_heatCaches", envir = .GlobalEnv))
writeLines("not an rds", heatCacheFile(dir, key))
h3 <- try(heatRasterPacked(aoi, bin = "midday", key = key, cacheDir = dir), silent = TRUE)
ok("a corrupt cache file falls back to a cold build",
   !inherits(h3, "try-error") && max(abs(values(unwrap(h3)) - values(cold)), na.rm = TRUE) < 1e-9)
unlink(dir, recursive = TRUE)

cat(sprintf("\n%d failure(s)\n", fails))
quit(status = if (fails) 1 else 0)
