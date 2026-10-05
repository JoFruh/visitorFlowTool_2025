## Verification for the lake-loop pass (R/lakeLoopAoI.R) inside generateAoI2().
##
## Part 1 is synthetic and needs no data: a square lake inside a square area,
## with the path lines handed in through `readPaths`, so every rule is checked
## against geometry whose answer is known.
## Part 2 runs the real generateAoI2() on perimeters drawn around real lakes,
## reading lakes.gdb, the paths GDB and the DULN raster from the data folder,
## and checks the invariants on whatever comes out.
##
## Run:  Rscript data-raw/verify_lake_loop_aoi.R            (both parts)
##       VFT_LAKE_N=0 Rscript data-raw/verify_lake_loop_aoi.R   (part 1 only)
suppressPackageStartupMessages({library(sf); library(shiny)})
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
env <- new.env(parent = globalenv())
for (f in sort(list.files(R, pattern = "[.][Rr]$", full.names = TRUE))) {
  suppressWarnings(try(sys.source(f, envir = env), silent = TRUE))
}
attach(env, warn.conflicts = FALSE)
## generateAoI2() splits areas with C++ now (src/aoi_segment.cpp). Without the
## working tree's dll the split fails over to the unsplit areas and parts 2-3
## would test the fallback, not the app.
{
  .dll <- file.path(dirname(R), "src", paste0("visitorFlowTool", .Platform$dynlib.ext))
  if(!file.exists(.dll)) stop("compiled code missing: ", .dll,
                              " -- build it with:  Rscript -e 'pkgbuild::compile_dll(\".\")'")
  dyn.load(.dll)
}

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

#total area shared by any two geometries - 0 when nothing overlaps
overlapArea <- function(g){
  if(length(g) < 2) return(0)
  pairs <- sf::st_intersects(g)
  tot <- 0
  for(i in seq_along(g)) for(j in pairs[[i]]) if(j > i){
    x <- sf::st_intersection(g[i], g[j])
    if(length(x)) tot <- tot + sum(as.numeric(sf::st_area(x)))
  }
  tot
}

cat("=== 1. synthetic ===\n")
crs <- sf::st_crs(2056)
sq <- function(x0, y0, x1, y1) sf::st_polygon(list(rbind(c(x0,y0), c(x1,y0), c(x1,y1), c(x0,y1), c(x0,y0))))
ln <- function(...) sf::st_linestring(rbind(...))
o <- c(2600000, 1200000)

#an area 3 km square; a 600 m square lake in its middle (36 ha); a path ring
#50 m off the shore
area <- sf::st_sfc(sq(o[1], o[2], o[1] + 3000, o[2] + 3000), crs = crs)
lake <- sf::st_sfc(sq(o[1] + 1200, o[2] + 1200, o[1] + 1800, o[2] + 1800), crs = crs)
ring <- sf::st_sfc(ln(c(o[1]+1150, o[2]+1150), c(o[1]+1850, o[2]+1150), c(o[1]+1850, o[2]+1850),
                      c(o[1]+1150, o[2]+1850), c(o[1]+1150, o[2]+1150)), crs = crs)
paths <- function(lines) function(area2056) lines

res <- vftLakeLoopAoI(area, lake, readPaths = paths(ring))
ok("a ring 50 m off the shore makes a lake area", sum(res$lakeLoop) == 1)
la <- res$geom[res$lakeLoop]
ok("...which contains the whole lake",
   isTRUE(as.numeric(sf::st_area(sf::st_intersection(la, lake))) >= 0.999 * as.numeric(sf::st_area(lake))))
ok("...and the path ring (padded)", isTRUE(sf::st_covers(la, ring, sparse = FALSE)[1, 1]))
ok("...and the remnant is the area minus it", sum(!res$lakeLoop) == 1 &&
     abs(sum(as.numeric(sf::st_area(res$geom))) - as.numeric(sf::st_area(area))) < 1)
ok("...with no overlap", overlapArea(res$geom) < 1, sprintf("(%.2f m2)", overlapArea(res$geom)))

#The band is measured from the area HOLDING the lake, not from the water.
#square ring `d` m off the 600 m lake
sqRing <- function(d) sf::st_sfc(ln(c(o[1]+1200-d, o[2]+1200-d), c(o[1]+1800+d, o[2]+1200-d),
                                    c(o[1]+1800+d, o[2]+1800+d), c(o[1]+1200-d, o[2]+1800+d),
                                    c(o[1]+1200-d, o[2]+1200-d)), crs = crs)
#a holder barely bigger than the lake (20 m round it): the band (132 m for
#36 ha) counts from there. 100 m off the shore is 80 m outside the holder
#(121 m at the corners); 200 m off is 180 m outside it.
snug <- sf::st_buffer(lake, 20)
res <- vftLakeLoopAoI(snug, lake, readPaths = paths(sqRing(100)))
ok("a snug holder: a ring 80 m outside it is a loop", sum(res$lakeLoop) == 1)
res <- vftLakeLoopAoI(snug, lake, readPaths = paths(sqRing(200)))
ok("...a ring 180 m outside it (band 132 m) is not",
   !any(res$lakeLoop) && identical(res$geom, snug))

#the Greifensee case: the path swings 400 m off the water round a reed belt,
#but the attractive area holding the lake reaches past it - no detour
res <- vftLakeLoopAoI(area, lake, readPaths = paths(sqRing(400)))
ok("a ring 400 m off the water but inside the holder is a loop", sum(res$lakeLoop) == 1)

#with a zone that large, the lake area is the TIGHTEST loop, not the outermost
res <- vftLakeLoopAoI(area, lake, readPaths = paths(c(sqRing(50), sqRing(400), sqRing(800))))
la <- res$geom[res$lakeLoop]
ok("...and with rings at 50, 400 and 800 m it follows the 50 m one",
   sum(res$lakeLoop) == 1 && as.numeric(sf::st_area(la)) < 1.1 * 700^2,
   sprintf("(%.0f ha; the 50 m ring holds %.0f ha)", as.numeric(sf::st_area(la)) / 1e4, 700^2 / 1e4))

#three sides only: no cycle
open <- sf::st_sfc(ln(c(o[1]+1150, o[2]+1150), c(o[1]+1850, o[2]+1150), c(o[1]+1850, o[2]+1850),
                      c(o[1]+1150, o[2]+1850)), crs = crs)
res <- vftLakeLoopAoI(area, lake, readPaths = paths(open))
ok("an open U makes none", !any(res$lakeLoop))

#a ring that cuts a corner off the lake: a 300 m right triangle (4.5 ha) of
#the 36 ha lake is outside it, so it encloses 87.5%
cut <- sf::st_sfc(ln(c(o[1]+1150, o[2]+1150), c(o[1]+1850, o[2]+1150), c(o[1]+1850, o[2]+1450),
                     c(o[1]+1450, o[2]+1850), c(o[1]+1150, o[2]+1850), c(o[1]+1150, o[2]+1150)), crs = crs)
res <- vftLakeLoopAoI(area, lake, readPaths = paths(cut))
ok("a loop enclosing 87.5% of the lake is rejected (< 95%)", !any(res$lakeLoop))

#the ring drawn as four separate segments, plus a jetty into the lake: still a loop
segs <- sf::st_sfc(ln(c(o[1]+1150, o[2]+1150), c(o[1]+1850, o[2]+1150)),
                   ln(c(o[1]+1850, o[2]+1150), c(o[1]+1850, o[2]+1850)),
                   ln(c(o[1]+1850, o[2]+1850), c(o[1]+1150, o[2]+1850)),
                   ln(c(o[1]+1150, o[2]+1850), c(o[1]+1150, o[2]+1150)),
                   ln(c(o[1]+1500, o[2]+1150), c(o[1]+1500, o[2]+1300)), crs = crs)
res <- vftLakeLoopAoI(area, lake, readPaths = paths(segs))
ok("a ring in four pieces plus a jetty is a loop", sum(res$lakeLoop) == 1)

#the lake is only 25% inside the area: not a candidate, and paths are never read
edgeArea <- sf::st_sfc(sq(o[1], o[2], o[1] + 1500, o[2] + 1500), crs = crs)
readCalled <- FALSE
res <- vftLakeLoopAoI(edgeArea, lake, readPaths = function(a){ readCalled <<- TRUE; ring })
ok("a lake 25% covered is no candidate", !any(res$lakeLoop))
ok("...and the paths are not read at all", !readCalled)

#an area that RINGS the lake (lake below threshold = a hole) still counts
holed <- sf::st_sfc(sf::st_polygon(list(sf::st_coordinates(area)[, 1:2],
                                        sf::st_coordinates(lake)[5:1, 1:2])), crs = crs)
res <- vftLakeLoopAoI(holed, lake, readPaths = paths(ring))
ok("an area with the lake as a hole still makes a lake area", sum(res$lakeLoop) == 1)
ok("...with no overlap", overlapArea(res$geom) < 1)

#a lake area under 10 ha: a 100 m pond with a ring 30 m off it
pond <- sf::st_sfc(sq(o[1] + 1450, o[2] + 1450, o[1] + 1550, o[2] + 1550), crs = crs)
pring <- sf::st_sfc(ln(c(o[1]+1420, o[2]+1420), c(o[1]+1580, o[2]+1420), c(o[1]+1580, o[2]+1580),
                       c(o[1]+1420, o[2]+1580), c(o[1]+1420, o[2]+1420)), crs = crs)
res <- vftLakeLoopAoI(area, pond, readPaths = paths(pring))
ok("a pond whose loop is under 10 ha stays inside its area",
   !any(res$lakeLoop) && identical(res$geom, area))

#the lake area cuts the area in two (lake spans its width): two remnants
band  <- sf::st_sfc(sq(o[1], o[2] + 1000, o[1] + 3000, o[2] + 2000), crs = crs)
wide  <- sf::st_sfc(sq(o[1] + 1300, o[2] + 900, o[1] + 1700, o[2] + 2100), crs = crs)
wring <- sf::st_sfc(ln(c(o[1]+1250, o[2]+850), c(o[1]+1750, o[2]+850), c(o[1]+1750, o[2]+2150),
                       c(o[1]+1250, o[2]+2150), c(o[1]+1250, o[2]+850)), crs = crs)
res <- vftLakeLoopAoI(band, wide, readPaths = paths(wring))
ok("an area the lake area splits becomes two areas + the lake",
   sum(res$lakeLoop) == 1 && sum(!res$lakeLoop) == 2)
ok("...every one a single POLYGON",
   all(sf::st_geometry_type(res$geom) == "POLYGON"))

#the band grows with sqrt(lake area): 50 m at 5.2 ha, 200 m from ~83 ha on
b <- vftLakeLoopBuffer(c(1e4, 52000, 4 * 52000, 360000, 16 * 52000, 5e6))
ok("band: 50 m below and at 5.2 ha, x2 at 4x the area, 200 m from 83 ha",
   isTRUE(all.equal(b, c(50, 50, 100, 50 * sqrt(360000 / 52000), 200, 200))),
   sprintf("(%s)", paste(round(b), collapse = " ")))
disc <- sf::st_buffer(sf::st_sfc(sf::st_point(c(0, 0)), crs = crs), sqrt(52000 / pi), nQuadSegs = 90)
ok("...a 5.2 ha disc plus its 50 m band clears the 10 ha minimum",
   as.numeric(sf::st_area(sf::st_buffer(disc, 50, nQuadSegs = 90))) > VFT_AOI_MIN_AREA_M2)

#a 100 ha lake in a snug holder (band capped at 200 m): a ring 150 m outside
#the holder counts - it would not for the 36 ha lake above (band 132 m) - and
#250 m outside does not. Round, because a square ring d off a square holder is
#d*sqrt(2) from its corners.
ctr  <- sf::st_sfc(sf::st_point(o + 1500), crs = crs)
bigR <- sqrt(1e6 / pi)
big  <- sf::st_buffer(ctr, bigR, nQuadSegs = 90)
bigHolder <- sf::st_buffer(ctr, bigR + 20, nQuadSegs = 90)
bigRing <- function(d) sf::st_cast(sf::st_boundary(sf::st_buffer(ctr, bigR + 20 + d, nQuadSegs = 90)), "LINESTRING")
res <- vftLakeLoopAoI(bigHolder, big, readPaths = paths(bigRing(150)))
ok("a 100 ha lake: a ring 150 m outside its holder is a loop", sum(res$lakeLoop) == 1)
res <- vftLakeLoopAoI(bigHolder, big, readPaths = paths(bigRing(250)))
ok("...but 250 m outside is past the 200 m cap", !any(res$lakeLoop))

#a failure inside the pass leaves the areas exactly as generated
res <- vftLakeLoopAoI(area, lake, readPaths = function(a) stop("GDB gone"))
ok("an error in the pass returns the input unchanged",
   identical(res$geom, area) && identical(res$lakeLoop, FALSE))

cat("\n=== 1b. prepare_network keeps one AOI per node ===\n")
#the defensive fix: two overlapping areas, a point inside both
pa <- sf::st_sf(AOI = c("A", "B"),
                geometry = sf::st_sfc(sq(0, 0, 2, 2), sq(1, 0, 3, 2), crs = 4326))
pts <- terra::vect(rbind(c(0.5, 1), c(1.5, 1), c(2.5, 1), c(5, 5)), type = "points", crs = "EPSG:4326")
x <- terra::extract(terra::vect(pa["AOI"]), pts)
ok("terra::extract does return an extra row for the shared point", nrow(x) == 5)
x <- x[!duplicated(x[[1]]), , drop = FALSE]
ok("...the dedup leaves one row per point, first match kept",
   nrow(x) == 4 && identical(x$AOI, c("A", "A", "B", NA)))

cat("\n=== 1c. step 4 with a lake area in the working set ===\n")
#the same fixture verify_polydraw.R uses: skip = TRUE, walkNat 1 west / 0 east
i18n <- shiny.i18n::Translator$new(translation_csvs_path = vftData("tables"), separator_csv = ";")
i18n$set_translation_language("de")
shape <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = 8.58, ymin = 47.35, xmax = 8.62, ymax = 47.37),
                                                          crs = sf::st_crs(4326))))
rast <- terra::rast(xmin = 8.57, xmax = 8.63, ymin = 47.34, ymax = 47.38, res = 0.0005, crs = "EPSG:4326")
terra::values(rast) <- ifelse(terra::xFromCell(rast, seq_len(terra::ncell(rast))) < 8.60, 1, 0)
names(rast) <- "walkNat"
box4326 <- function(x0, y0, x1, y1) sf::st_as_sfc(sf::st_bbox(c(xmin = x0, ymin = y0, xmax = x1, ymax = y1), crs = sf::st_crs(4326)))

shiny::testServer(step4_server, args = list(
  minThresh = shiny::reactive(1), i18n = function() i18n, currentLang = shiny::reactive("de"),
  skip = shiny::reactive(TRUE), DULN = shiny::reactive(rast), DULN_all = shiny::reactive(rast),
  shape = shiny::reactive(shape)), {
  session$flushReact()
  r$polygonsList <- sf::st_sf(DULN = c(1, 0), area = c(1e6, 1e6), lakeLoop = c(FALSE, TRUE),
                              id = 1:2, polygons = c(box4326(8.585, 47.355, 8.595, 47.365),
                                                     box4326(8.605, 47.355, 8.615, 47.365)))
  ok("drawing a set with one lake area does not error",
     !inherits(try(.vftDrawAOI(), silent = TRUE), "try-error"))
  session$setInputs(polyCut = list(lng = c(8.603, 8.617), lat = c(47.360, 47.360)))
  a <- shiny::isolate(r$polygonsList)
  ok("a cut across the lake area splits it", nrow(a) == 3)
  ok("...its pieces are ordinary areas again", identical(a$lakeLoop, c(FALSE, FALSE, FALSE)))
  r$polygonsList <- sf::st_sf(DULN = c(1, 0), area = c(1e6, 1e6), lakeLoop = c(FALSE, TRUE),
                              id = 1:2, polygons = c(box4326(8.585, 47.355, 8.595, 47.365),
                                                     box4326(8.605, 47.355, 8.615, 47.365)))
  session$setInputs(polyDrawn = list(lng = c(8.610, 8.618, 8.618, 8.610), lat = c(47.360, 47.360, 47.368, 47.368)))
  a <- shiny::isolate(r$polygonsList)
  ok("a ring merged into the lake area makes it an ordinary area",
     nrow(a) == 2 && !isTRUE(a$lakeLoop[2]) && isFALSE(a$lakeLoop[1]))
  ok("...and the set still draws", !inherits(try(.vftDrawAOI(), silent = TRUE), "try-error"))
})

n <- as.integer(Sys.getenv("VFT_LAKE_N", "6"))
if(n > 0){
  cat("\n=== 2. real lakes, generateAoI2() end to end ===\n")
  lakesAll <- sf::st_read(vftData("maps/lakes.gdb"), quiet = TRUE)
  lakesAll <- lakesAll[lakesAll$SHAPE_Area > 2e5 & lakesAll$SHAPE_Area < 3e6, ]
  set.seed(as.integer(Sys.getenv("VFT_LAKE_SEED", "1")))
  pick <- lakesAll[sample(nrow(lakesAll), n), ]
  full <- terra::rast(vftData("maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif"))
  nLake <- 0

  for(i in seq_len(nrow(pick))){
    #a perimeter 1.5 km around the lake, the threshold at the crop's 60th percentile
    shape <- sf::st_sf(geometry = sf::st_transform(
      sf::st_buffer(sf::st_zm(sf::st_geometry(pick[i, ])), 1500), 4326))
    crop  <- terra::crop(full, terra::ext(terra::vect(sf::st_buffer(shape, 3000))))
    names(crop) <- "walkNat"
    thr   <- as.numeric(stats::quantile(terra::values(crop), 0.6, na.rm = TRUE))

    t0 <- Sys.time()
    with    <- generateAoI2(thr, shape, walkNat = crop, DULN_all = crop)$polygons
    t1 <- Sys.time()
    without <- generateAoI2(thr, shape, walkNat = crop, DULN_all = crop, lakeLoop = FALSE)$polygons
    t2 <- Sys.time()
    g <- sf::st_transform(sf::st_geometry(with), 2056)
    lab <- sprintf("lake %d (%.0f ha)", i, pick$SHAPE_Area[i] / 1e4)
    cat(sprintf("%s: %d areas (%d lake) vs %d without; %.1f s vs %.1f s\n", lab,
                nrow(with), sum(with$lakeLoop), nrow(without),
                as.numeric(t1 - t0, units = "secs"), as.numeric(t2 - t1, units = "secs")))
    nLake <- nLake + sum(with$lakeLoop)

    ok(paste(lab, "no overlap"), overlapArea(g) < 100, sprintf("(%.1f m2)", overlapArea(g)))
    ok(paste(lab, "every area > 10 ha"), all(as.numeric(sf::st_area(g)) > VFT_AOI_MIN_AREA_M2 * 0.99))
    ok(paste(lab, "DULN scored for every area"), !anyNA(with$DULN))
    ok(paste(lab, "lakeLoop = FALSE has no lake areas"), !any(without$lakeLoop))
    if(any(with$lakeLoop)){
      lk <- sf::st_zm(sf::st_geometry(pick[i, ])); lk <- sf::st_set_crs(sf::st_set_crs(lk, NA), 2056)
      inLake <- sum(as.numeric(sf::st_area(sf::st_intersection(sf::st_union(g[with$lakeLoop]), lk))))
      cat(sprintf("   lake area holds %.1f%% of the sampled lake\n", 100 * inLake / as.numeric(sf::st_area(lk))))
    }
  }
  cat(sprintf("\n%d lake area(s) made across %d real lakes\n", nLake, n))
}

if(n > 0){
  cat("\n=== 3. Greifensee in the filBleu perimeter (the reported case) ===\n")
  #Its 18.2 km lake path swings up to 298 m off the water round the Riedikon
  #reed belt, which the first version (band measured from the water, 200 m
  #cap) rejected. Measured from the holding area, it is a loop.
  shape <- sf::st_transform(sf::st_read(vftData("maps/filBleu_area_final.shp"), quiet = TRUE), 4326)
  crop  <- terra::crop(full, sf::st_transform(sf::st_as_sfc(sf::st_buffer(sf::st_transform(shape, 3857), 1000)), 4326))
  names(crop) <- "walkNat"
  thr <- as.numeric(stats::quantile(terra::values(crop), 0.6, na.rm = TRUE))
  t0  <- Sys.time()
  out <- suppressWarnings(generateAoI2(thr, shape, walkNat = crop, DULN_all = crop))$polygons
  cat(sprintf("%d areas, %d lake, in %.1f s\n", nrow(out), sum(out$lakeLoop),
              as.numeric(Sys.time() - t0, units = "secs")))
  gsl <- sf::st_read(vftData("maps/lakes.gdb"), wkt_filter = "POINT (2694500 1244000)", quiet = TRUE)
  gsl <- sf::st_set_crs(sf::st_set_crs(sf::st_zm(sf::st_geometry(gsl[which.max(gsl$SHAPE_Area), ])), NA), 2056)
  g   <- sf::st_set_crs(sf::st_set_crs(sf::st_transform(sf::st_geometry(out), 2056), NA), 2056)
  la  <- g[out$lakeLoop]
  inLake <- if(length(la)) sum(as.numeric(sf::st_area(sf::st_intersection(sf::st_union(la), gsl)))) else 0
  ok("Greifensee lies in a lake area", inLake / as.numeric(sf::st_area(gsl)) > 0.99,
     sprintf("(%.0f%%)", 100 * inLake / as.numeric(sf::st_area(gsl))))
  ok("...outlined by its ~18 km lake path, not a longer round",
     length(la) > 0 && max(as.numeric(sf::st_length(sf::st_boundary(la)))) < 20000,
     sprintf("(%.1f km)", max(c(0, as.numeric(sf::st_length(sf::st_boundary(la))))) / 1000))
  ok("...and nothing overlaps", overlapArea(g) < 100)
}

cat(sprintf("\n%s\n", if (fails == 0) "ALL PASS" else paste(fails, "FAIL(S)")))
quit(status = if (fails == 0) 0 else 1)
