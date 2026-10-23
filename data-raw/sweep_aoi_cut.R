## Parameter sweep of step 4's "Automatic Cuts" (the "cut" split, R/aoiCut.R)
## over one perimeter and threshold, one map image per parameter combination,
## so the combination can be picked by eye.
##
## Every run is the real generateAoI2(split = TRUE) - lake anchoring included,
## the polygons exactly what step 4 draws - with the cut's settings overridden:
##   VFT_AOI_CUT_DEPTH         bridge narrower than (1 - this) x body is cut
##   VFT_AOI_CUT_MAX_BRIDGE_M  bridge at least this wide is never cut
##   VFT_AOI_CUT_CONTACT       shared border above this share of an outline joins
##   VFT_AOI_CUT_DULN_DEPTH    attractiveness valley across a border vetoes the
##                             two rules above
## Every value in the grid below the current one cuts more (bridge: above).
##
## Runs against the working tree (R/ sourced, src DLL loaded), like
## data-raw/compare_aoi_split.R. Build the DLL first if src/ changed:
##   Rscript -e "Rcpp::compileAttributes(); pkgbuild::compile_dll('.')"
##
## Run:  Rscript data-raw/sweep_aoi_cut.R
##       VFT_SWEEP_DIR=<dir>     output (images, sweep.csv, polygons.rds);
##                               default tempdir()/aoi_sweep
##       VFT_SWEEP_THRESH=3.1    the step-3 threshold
##       VFT_SWEEP_SHAPE=maps/filBleu_area_final.shp   the step-1 perimeter: below
##                               the data dir or absolute; a closed LINE (the Wigger
##                               KML) is closed into a polygon
##       VFT_SWEEP_DEPTH=0.5,0.35,0.2  VFT_SWEEP_BRIDGE=700,1000,1500,Inf
##       VFT_SWEEP_CONTACT=0.2,0.35,0.5  VFT_SWEEP_DULN=Inf,0.5,0.35
##                               the grid (Inf = no floor / veto off). The app's
##                               current setting is always run first, as the
##                               reference the checks compare with
##       VFT_SWEEP_N=1           only the first (default) run, to check the look
suppressPackageStartupMessages({library(sf)})
terra::terraOptions(progress = 0)
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
env <- new.env(parent = globalenv())
for (f in sort(list.files(R, pattern = "[.][Rr]$", full.names = TRUE))) {
  suppressWarnings(try(sys.source(f, envir = env), silent = TRUE))
}
attach(env, warn.conflicts = FALSE)
{
  .dll <- file.path(dirname(R), "src", paste0("visitorFlowTool", .Platform$dynlib.ext))
  if(!file.exists(.dll)) stop("compiled code missing: ", .dll)
  dyn.load(.dll)
}

outDir <- Sys.getenv("VFT_SWEEP_DIR", file.path(tempdir(), "aoi_sweep"))
dir.create(file.path(outDir, "img"), recursive = TRUE, showWarnings = FALSE)
thr    <- as.numeric(Sys.getenv("VFT_SWEEP_THRESH", "3.1"))
secs   <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))

#### the data, as step 3/4 get it ####
#step 1 on an uploaded file - as data-raw/validate_usage_surveys.R reads it
readPerimeter <- function(file){
  if(!file.exists(file)) file <- vftData(file)
  g <- sf::st_geometry(sf::st_zm(sf::st_read(file, quiet = TRUE)))
  if(all(sf::st_geometry_type(g) %in% c("LINESTRING", "MULTILINESTRING"))){
    g <- sf::st_sfc(lapply(sf::st_cast(g, "LINESTRING"), function(l){
      m <- unname(sf::st_coordinates(l)[, c("X", "Y"), drop = FALSE])
      if(any(m[1, ] != m[nrow(m), ])) m <- rbind(m, m[1, ])
      sf::st_polygon(list(m))
    }), crs = sf::st_crs(g))
  }
  g <- sf::st_make_valid(sf::st_union(sf::st_transform(g, 4326)))
  if(any(sf::st_geometry_type(g) == "GEOMETRYCOLLECTION")) g <- sf::st_collection_extract(g, "POLYGON")
  sf::st_sf(geometry = sf::st_cast(g, "POLYGON"))
}
shape <- readPerimeter(Sys.getenv("VFT_SWEEP_SHAPE", "maps/filBleu_area_final.shp"))
full  <- terra::rast(vftData("maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif"))
D     <- terra::crop(full, sf::st_transform(sf::st_as_sfc(sf::st_buffer(
  sf::st_transform(shape, 2056), 1500)), 4326))
names(D) <- "walkNat"

#the lakes read is the same in every run: read once
buf2056 <- sf::st_union(sf::st_buffer(sf::st_geometry(sf::st_transform(shape, 2056)), 1000))
lakes0  <- .vftReadLakesWithin(buf2056)
assign(".vftReadLakesWithin", function(area2056) lakes0, envir = env)

#### the grid ####
envVals <- function(name, default) as.numeric(strsplit(Sys.getenv(name, default), ",")[[1]])
grid <- expand.grid(depth   = envVals("VFT_SWEEP_DEPTH",   "0.5,0.35,0.2"),
                    bridge  = envVals("VFT_SWEEP_BRIDGE",  "700,1000,1500,Inf"),
                    contact = envVals("VFT_SWEEP_CONTACT", "0.2,0.35,0.5"),
                    duln    = envVals("VFT_SWEEP_DULN",    "Inf,0.5,0.35"),
                    KEEP.OUT.ATTRS = FALSE)
#the app's current setting first, in the grid or not: it times the rest and is
#the reference the checks compare with
def <- data.frame(depth = VFT_AOI_CUT_DEPTH, bridge = VFT_AOI_CUT_MAX_BRIDGE_M,
                  contact = VFT_AOI_CUT_CONTACT, duln = VFT_AOI_CUT_DULN_DEPTH)
isDef <- grid$depth == def$depth & grid$bridge == def$bridge &
  grid$contact == def$contact & grid$duln == def$duln
grid <- rbind(def, grid[!isDef, ])
grid$default <- c(TRUE, rep(FALSE, nrow(grid) - 1))
grid$inGrid  <- c(any(isDef), rep(TRUE, nrow(grid) - 1))
grid$id <- sprintf("d%03.0f_b%s_c%03.0f_v%s", 100 * grid$depth,
                   ifelse(is.finite(grid$bridge), grid$bridge, "Inf"), 100 * grid$contact,
                   ifelse(is.finite(grid$duln), sprintf("%03.0f", 100 * grid$duln), "off"))
#VFT_SWEEP_N=1 draws the default alone, to check the look
grid <- grid[seq_len(min(nrow(grid), as.integer(Sys.getenv("VFT_SWEEP_N", nrow(grid))))), ]

setCut <- function(depth, bridge, contact, duln){
  assign("VFT_AOI_CUT_DEPTH",        depth,   envir = env)
  assign("VFT_AOI_CUT_MAX_BRIDGE_M", bridge,  envir = env)
  assign("VFT_AOI_CUT_CONTACT",      contact, envir = env)
  assign("VFT_AOI_CUT_DULN_DEPTH",   duln,    envir = env)
}
run <- function(split = TRUE) suppressWarnings(
  generateAoI2(thr, shape, walkNat = D, DULN_all = D, split = split))$polygons

#### the drawing ####
ext2056 <- sf::st_as_sfc(sf::st_bbox(sf::st_buffer(sf::st_transform(shape, 2056), 1000)))
tileF   <- file.path(outDir, "basemap.tif")
base <- tryCatch({
  #swisstopo's grey national map (public WMTS, no key; Carto now wants one)
  if(!file.exists(tileF))
    terra::writeRaster(maptiles::get_tiles(ext2056, crop = TRUE, zoom = 14,
      provider = maptiles::create_provider(name = "swisstopo_grey", citation = "swisstopo",
        url = "https://wmts.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-grau/default/current/3857/{z}/{x}/{y}.jpeg")),
      tileF, overwrite = TRUE)
  terra::rast(tileF)
}, error = function(e){ cat("basemap failed, grey DULN instead:", conditionMessage(e), "\n"); NULL })
shape2056 <- sf::st_geometry(sf::st_transform(shape, 2056))
bb <- sf::st_bbox(ext2056)
W  <- 1100; H <- round(W * (bb["ymax"] - bb["ymin"]) / (bb["xmax"] - bb["xmin"])) + 60
D2056 <- terra::project(terra::mask(terra::crop(D, terra::vect(sf::st_transform(buf2056, 4326))),
                                    terra::vect(sf::st_transform(buf2056, 4326))), "EPSG:2056")

#Touching (or nearly touching) areas never share a colour, so every cut shows. The first choice is
#hashed from the area's position, so an area keeps its colour from panel to
#panel where nothing about it changed. No blue: that is the lake outline.
PAL <- c("#e6194b", "#3cb44b", "#ffe119", "#f58231", "#911eb4", "#f032e6",
         "#bfef45", "#469990", "#9a6324", "#808000", "#fabed4", "#aaffc3")
colourAreas <- function(g){
  n <- length(g); if(!n) return(character(0))
  #near counts as touching: two areas 100 m apart in one colour read as one
  nb  <- sf::st_intersects(sf::st_buffer(g, 75))
  xy  <- sf::st_coordinates(sf::st_point_on_surface(g))
  pref <- (floor(xy[, 1] / 300) * 7 + floor(xy[, 2] / 300) * 13) %% length(PAL) + 1
  ord <- order(-as.numeric(sf::st_area(g)))
  col <- integer(n)
  for(i in ord){
    used <- col[setdiff(nb[[i]], i)]
    cand <- ((pref[i] - 1 + 0:(length(PAL) - 1)) %% length(PAL)) + 1
    free <- cand[!cand %in% used]
    col[i] <- if(length(free)) free[1] else pref[i]
  }
  PAL[col]
}
background <- function(){
  plot(ext2056, border = NA, xaxs = "i", yaxs = "i", reset = FALSE)
  if(!is.null(base)){
    terra::plotRGB(base, add = TRUE)
    #washed out, so the areas carry the picture
    graphics::rect(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"], col = grDevices::adjustcolor("white", 0.45), border = NA)
  } else terra::plot(D2056, col = grDevices::grey.colors(50, 0.95, 0.4), legend = FALSE, add = TRUE, axes = FALSE)
}
drawPanel <- function(file, title, sub, polys = NULL, raster = NULL){
  grDevices::jpeg(file, width = W, height = H, quality = 88)
  graphics::par(mar = c(0, 0, 3.2, 0), bg = "white")
  background()
  if(!is.null(raster)){
    terra::plot(raster, col = grDevices::hcl.colors(60, "YlGn", rev = TRUE), alpha = 0.75,
                add = TRUE, legend = FALSE, axes = FALSE)
    terra::contour(raster, levels = thr, add = TRUE, drawlabels = FALSE, col = "black", lwd = 1.2)
  }
  if(!is.null(polys) && nrow(polys)){
    g <- sf::st_transform(sf::st_geometry(polys), 2056)
    plot(g, col = grDevices::adjustcolor(colourAreas(g), 0.5), border = "black", lwd = 1.1, add = TRUE)
    if(any(polys$lakeLoop %in% TRUE))
      plot(g[polys$lakeLoop %in% TRUE], col = NA, border = "#0050ff", lwd = 3.5, add = TRUE)
  }
  plot(shape2056, col = NA, border = "grey25", lty = 2, lwd = 1.5, add = TRUE)
  graphics::mtext(title, side = 3, line = 1.6, cex = 1.45, font = 2, adj = 0.02)
  graphics::mtext(sub,   side = 3, line = 0.25, cex = 1.2, adj = 0.02)
  grDevices::dev.off()
}
stat <- function(p){
  a <- p$area / 1e4
  list(n = length(a), largest = if(length(a)) max(a) else 0, median = if(length(a)) stats::median(a) else 0,
       over500 = sum(a > 500), over1000 = sum(a > 1000), lakeAreas = sum(p$lakeLoop %in% TRUE), total = sum(a))
}
subOf <- function(s) sprintf("%d areas  |  largest %.0f ha  |  %d over 500 ha  |  median %.0f ha",
                             s$n, s$largest, s$over500, s$median)
overlapArea <- function(g){
  pairs <- sf::st_intersects(g); tot <- 0
  for(i in seq_along(g)) for(j in pairs[[i]]) if(j > i){
    x <- sf::st_intersection(g[i], g[j]); if(length(x)) tot <- tot + sum(as.numeric(sf::st_area(x)))
  }
  tot
}

#### references ####
t0 <- Sys.time(); unsplit <- run(split = FALSE)
s0 <- stat(unsplit)
drawPanel(file.path(outDir, "img", "ref_unsplit.jpg"), sprintf("Unsplit (threshold %.1f)", thr), subOf(s0), unsplit)
drawPanel(file.path(outDir, "img", "ref_duln.jpg"), sprintf("Attractiveness (DULN_all), %.1f contour", thr),
          "the raster the threshold and the DULN veto read", raster = D2056)
cat(sprintf("unsplit: %d areas, largest %.0f ha (%.1f s)\n", s0$n, s0$largest, secs(t0)))

#### the sweep ####
rows <- list(); polys <- list(); fails <- 0
tStart <- Sys.time()
for(r in seq_len(nrow(grid))){
  q <- grid[r, ]
  setCut(q$depth, q$bridge, q$contact, q$duln)
  t0 <- Sys.time(); p <- run(); tRun <- secs(t0)
  s  <- stat(p)
  g  <- sf::st_transform(sf::st_geometry(p), 2056)
  ov <- overlapArea(g)
  #pieces under the 25 ha join size left next to a lake area
  lakeG <- g[p$lakeLoop %in% TRUE]
  scraps <- if(length(lakeG)) sum(p$area < VFT_AOI_SPLIT_JOIN_M2 & !(p$lakeLoop %in% TRUE) &
                                  lengths(sf::st_intersects(sf::st_buffer(g, 5), lakeG)) > 0) else 0
  if(r == 1){ ref <- s; cat(sprintf("default run %.1f s -> ETA ~%.0f min for %d runs\n", tRun,
                                    (tRun + 2) * nrow(grid) / 60, nrow(grid))) }
  okCover <- abs(s$total - ref$total) / ref$total < 0.01
  okLakes <- s$lakeAreas == ref$lakeAreas
  okOver  <- ov < 100
  if(!(okCover && okLakes && okOver)) fails <- fails + 1
  ttl <- sprintf("depth %.2f · bridge %s · contact %.2f · DULN %s%s", q$depth,
                 if(is.finite(q$bridge)) paste(q$bridge, "m") else "no floor", q$contact,
                 if(is.finite(q$duln)) sprintf("%.2f", q$duln) else "off", if(q$default) "  (current)" else "")
  drawPanel(file.path(outDir, "img", paste0(q$id, ".jpg")), ttl, subOf(s), p)
  rows[[r]] <- data.frame(q, n = s$n, largest_ha = round(s$largest), median_ha = round(s$median),
                          over500 = s$over500, over1000 = s$over1000, lake_areas = s$lakeAreas,
                          lake_scraps = scraps, total_ha = round(s$total), overlap_m2 = round(ov, 1),
                          ok_cover = okCover, ok_lakes = okLakes, ok_overlap = okOver,
                          seconds = round(tRun, 1), row.names = NULL)
  polys[[q$id]] <- p
  cat(sprintf("%2d/%d %-24s %3d areas, largest %5.0f ha, >500 %2d, lakes %d, scraps %d, %.1f s%s\n",
              r, nrow(grid), q$id, s$n, s$largest, s$over500, s$lakeAreas, scraps, tRun,
              if(okCover && okLakes && okOver) "" else "  CHECK FAILED"))
}
out <- do.call(rbind, rows)
utils::write.csv(out, file.path(outDir, "sweep.csv"), row.names = FALSE)
saveRDS(list(unsplit = unsplit, runs = polys, threshold = thr), file.path(outDir, "polygons.rds"))
cat(sprintf("\n%d runs in %.1f min, %d with a failed check. Output in %s\n",
            nrow(out), secs(tStart) / 60, fails, outDir))
