## Fingerprint every stage of the "cut" AoI split (R/aoiCut.R) on filBleu, so
## two machines can be compared line by line: the first line that differs is
## the stage - or the data, or the library - that makes their cuts differ.
##
## Runs against the INSTALLED package (asNamespace), as the app's daemons do.
##
## Run:  Rscript data-raw/fingerprint_aoi_cut.R > fp_<machine>.txt
##       VFT_FP_THRESH=8 sets the threshold (default 8, filBleu's suggestion)
suppressPackageStartupMessages({library(sf)})
terra::terraOptions(progress = 0)
ns  <- asNamespace("visitorFlowTool")
thr <- as.numeric(Sys.getenv("VFT_FP_THRESH", "8"))
p   <- function(label, ...) cat(sprintf("%-26s", label), ..., "\n")
f6  <- function(x) format(signif(x, 8), scientific = FALSE)

cat("=== versions ===\n")
p("R", R.version.string)
for(pk in c("visitorFlowTool", "sf", "terra", "Rcpp", "s2"))
  p(pk, as.character(utils::packageVersion(pk)))
p("sf libs", paste(names(sf::sf_extSoftVersion()), sf::sf_extSoftVersion(), collapse = " "))
p("terra GDAL/GEOS/PROJ", terra::gdal(lib = "gdal"), terra::gdal(lib = "geos"), terra::gdal(lib = "proj"))
p("s2 on", sf::sf_use_s2())

cat("\n=== data ===\n")
fShape <- ns$vftData("maps/filBleu_area_final.shp")
fDULN  <- ns$vftData("maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif")
fLakes <- ns$vftData("maps/lakes.gdb")
p("DULN file bytes", file.size(fDULN))
lf <- list.files(fLakes, full.names = TRUE)
tf <- tempfile(); writeLines(unname(tools::md5sum(lf)), tf)
p("lakes.gdb bytes / md5", sum(file.size(lf)), substr(tools::md5sum(tf), 1, 12))
for(f in lf) p(paste0("lakes.gdb/", basename(f)), file.size(f), substr(tools::md5sum(f), 1, 12))
p("shape md5", substr(tools::md5sum(fShape), 1, 12))

shape <- sf::st_transform(sf::st_read(fShape, quiet = TRUE), 4326)
D     <- terra::crop(terra::rast(fDULN), sf::st_buffer(shape, 1000))
names(D) <- "walkNat"
v <- terra::values(D, mat = FALSE)
p("D dim", terra::nrow(D), terra::ncol(D))
p("D res", f6(terra::res(D)))
p("D ext", f6(as.vector(terra::ext(D))))
p("D crs", terra::crs(D, describe = TRUE)$code)
p("D NA / sum", sum(is.na(v)), f6(sum(v, na.rm = TRUE)))
p("D quantiles 10..90", f6(stats::quantile(v, c(.1, .3, .5, .7, .9), na.rm = TRUE)))
p("suggested threshold", tryCatch(if(exists("vftSuggestAoIThreshold", ns, inherits = FALSE))
  ns$vftSuggestAoIThreshold(D, shape) else "not in installed pkg", error = function(e) paste("ERROR", conditionMessage(e))))
p("threshold used", thr)

try({
cat("\n=== cut internals (vftCutAoI, step by step) ===\n")
buf   <- sf::st_union(sf::st_buffer(sf::st_geometry(sf::st_transform(shape, 2056)), 1000))
lakes <- ns$.vftReadLakesWithin(buf)
p("lakes read", nrow(lakes), "crs", sf::st_crs(lakes)$Name)
p("lake areas >= 10 ha (ha)", f6(sort(round(as.numeric(sf::st_area(sf::st_zm(lakes))) / 1e4, 1), decreasing = TRUE)[1:5]))
nr <- terra::nrow(D); nc <- terra::ncol(D)
ca <- terra::values(terra::cellSize(D, mask = FALSE, unit = "m"), mat = FALSE)
yM <- terra::yres(D) * 111132
xM <- mean(ca, na.rm = TRUE) / yM
p("cell m2 mean/min/max", f6(c(mean(ca), min(ca), max(ca))))
p("xM yM", f6(c(xM, yM)))
p("lake buffer 823 ha / 10 ha", f6(c(ns$vftLakeLoopBuffer(8.23e6), ns$vftLakeLoopBuffer(1e5))))
inside <- !is.na(v) & v >= thr
p("cells above threshold", sum(inside))
anchor <- ns$.vftLakeAnchors(D, thr, lakes, xM, yM, "EPSG:4326")
p("anchor cells per anchor", if(any(anchor > 0)) paste(table(anchor[anchor > 0]), collapse = " ") else "none")
if(any(anchor > 0)) inside <- inside | anchor > 0
dist <- ns$aoi_edt(inside, nr, nc, xM, yM)
p("distance sum / max", f6(c(sum(dist, na.rm = TRUE), max(dist, na.rm = TRUE))))
if(any(anchor > 0)) dist[anchor > 0] <- max(dist, na.rm = TRUE) + 1
b  <- ns$aoi_basins(dist, nr, nc, 0)
nb <- length(b$peak)
p("basins / borders", nb, length(b$a))
p("saddle sum / peak sum", f6(c(sum(b$saddle), sum(b$peak))))
lab <- terra::values(ns$vftCutAoI(D, thr, lakes = lakes), mat = FALSE)
sz  <- sort(table(lab), decreasing = TRUE)
p("label areas", length(sz))
p("largest labels (cells)", paste(head(as.integer(sz), 8), collapse = " "))

}, silent = FALSE)
try({
cat("\n=== generateAoI2(split = TRUE) ===\n")
out <- suppressWarnings(ns$generateAoI2(thr, shape, walkNat = D, DULN_all = D, split = TRUE))$polygons
a   <- sort(out$area / 1e4, decreasing = TRUE)
p("areas", nrow(out), "of which lake areas", sum(out$lakeLoop))
p("largest (ha)", paste(round(head(a, 8)), collapse = " "))
p("lake areas (ha)", paste(round(out$area[out$lakeLoop] / 1e4), collapse = " "))
p("total (ha)", round(sum(a)))
}, silent = FALSE)
