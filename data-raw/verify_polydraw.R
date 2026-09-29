## Verification for the client-side polygon drawer (inst/app/www/polydraw.js)
## as the step 1 and step 4 modules receive it.
##
## The browser half - vertices, rubber band, closing on the first/last vertex
## or a double-click, the scissors on a two-vertex line - cannot run here. What
## can is everything R does with what the browser sends: input$polyDrawn (a
## closed ring) and input$polyCut (a two-point line), fed to the real module
## servers through shiny::testServer.
##
## Run:  Rscript data-raw/verify_polydraw.R
suppressPackageStartupMessages({library(shiny)})
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
env <- new.env(parent = globalenv())
for (f in sort(list.files(R, pattern = "[.][Rr]$", full.names = TRUE))) {
  suppressWarnings(try(sys.source(f, envir = env), silent = TRUE))
}
attach(env, warn.conflicts = FALSE)
options(shiny.i18n.warnings = FALSE)

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

i18n <- shiny.i18n::Translator$new(translation_csvs_path = vftData("tables"), separator_csv = ";")
i18n$set_translation_language("de")
i18nFn <- function() i18n

#a ring the way polydraw.js sends it: first vertex not repeated
ring <- function(xmin, ymin, xmax, ymax) list(lng = c(xmin, xmax, xmax, xmin), lat = c(ymin, ymin, ymax, ymax))
line <- function(x1, y1, x2, y2) list(lng = c(x1, x2), lat = c(y1, y2))

cat("=== 0. helpers ===\n")
p <- vftPolyDrawSf(ring(8.50, 47.36, 8.51, 47.37))
ok("a ring becomes one sf row", inherits(p, "sf") && nrow(p) == 1)
ok("...geometry column is `polygons`", identical(attr(p, "sf_column"), "polygons"))
ok("...in EPSG:4326", identical(sf::st_crs(p)$epsg, 4326L))
ok("...and closed", sf::st_is_valid(p))
ok("two vertices is no ring", is.null(vftPolyDrawSf(list(lng = c(1, 2), lat = c(1, 2)))))
ok("NA coordinates are no ring", is.null(vftPolyDrawSf(list(lng = c(1, NA, 3), lat = c(1, 2, 3)))))
ok("a cut line is a two-point LINESTRING",
   inherits(vftPolyDrawLine(line(8.5, 47.3, 8.6, 47.4)), "sfc_LINESTRING"))
ok("three points is no cut line", is.null(vftPolyDrawLine(list(lng = 1:3, lat = 1:3))))
w <- vftPolyDraw(leaflet::leaflet(), list(ns = function(x) paste0("step4-", x)), cut = TRUE)
js <- w$jsHooks$render[[1]]$code
ok("vftPolyDraw adds one onRender hook", length(w$jsHooks$render) == 1)
ok("...carrying the namespace and the cut flag",
   grepl('"ns":"step4-"', js, fixed = TRUE) && grepl('"cut":true', js, fixed = TRUE))

cat("\n=== 1. step 4 ===\n")
#a perimeter around Zurich-Witikon, well away from the lake, and a walkNat band
#of known values over it: 1 in the west half, 0 in the east half
shape <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = 8.58, ymin = 47.35, xmax = 8.62, ymax = 47.37),
                                                          crs = sf::st_crs(4326))))
rast <- terra::rast(xmin = 8.57, xmax = 8.63, ymin = 47.34, ymax = 47.38, res = 0.0005, crs = "EPSG:4326")
terra::values(rast) <- ifelse(terra::xFromCell(rast, seq_len(terra::ncell(rast))) < 8.60, 1, 0)
names(rast) <- "walkNat"
DULN <- c(rast); names(DULN) <- "walkNat"

shiny::testServer(step4_server, args = list(
  minThresh = shiny::reactive(1), i18n = i18nFn, currentLang = shiny::reactive("de"),
  skip = shiny::reactive(TRUE), DULN = shiny::reactive(DULN), DULN_all = shiny::reactive(rast),
  shape = shiny::reactive(shape)), {

  #the flush a real session runs at start-up: without it every observer's
  #first run is still its ignoreInit run, and the first ring would be dropped
  session$flushReact()
  areas <- function() shiny::isolate(r$polygonsList)
  n     <- function() { a <- areas(); if (inherits(a, "sf")) nrow(a) else 0L }
  area  <- function(i = NULL) { a <- areas(); sum(if (is.null(i)) a$area else a$area[i]) }

  ok("starts empty (skip)", n() == 0)

  session$setInputs(polyDrawn = ring(8.585, 47.355, 8.595, 47.365))
  ok("a ring on an empty map is one area", n() == 1)
  ok("...scored from walkNat (west half = 1)", isTRUE(all.equal(areas()$DULN, 1)))
  ok("...with an area in m2", area() > 500000 && area() < 1000000, sprintf("(%.0f)", area()))
  a1 <- area()

  session$setInputs(polyDrawn = ring(8.590, 47.360, 8.598, 47.368))
  ok("a ring over an area is merged into it", n() == 1)
  ok("...which grows", area() > a1)
  merged <- area()

  session$setInputs(polyDrawn = ring(8.605, 47.355, 8.615, 47.365))
  ok("a ring elsewhere is a second area", n() == 2)
  ok("...scored from ITS ground (east half = 0)", isTRUE(all.equal(areas()$DULN[2], 0)))
  ok("...with a fresh id", !anyDuplicated(areas()$id))

  session$setInputs(polyCut = line(8.603, 47.360, 8.617, 47.360))
  ok("a cut clean across the second area splits it", n() == 3)
  ok("...ids stay unique", !anyDuplicated(areas()$id))
  ok("...the first area is untouched", isTRUE(all.equal(area(1), merged)))

  before <- area(1)
  session$setInputs(polyCut = line(8.580, 47.358, 8.588, 47.358))
  ok("a cut ending inside an area does not split it", n() == 3)
  notched <- areas()[areas()$DULN == 1, ]
  ok("...it takes a slit from the edge in (still one piece, smaller)",
     nrow(notched) == 1 && notched$area < before, sprintf("(%.0f -> %.0f)", before, notched$area))

  before <- sum(areas()$area); nb <- n()
  session$setInputs(polyCut = line(8.588, 47.361, 8.593, 47.361))
  ok("a cut wholly inside an area leaves the count alone", n() == nb)
  ok("...and makes a hole", sum(areas()$area) < before)
  inner <- sf::st_geometry(areas()[areas()$DULN == 1, ])[[1]]
  ok("...an interior ring", length(unclass(inner)) >= 2 || inherits(inner, "MULTIPOLYGON"))

  before <- areas()
  session$setInputs(polyCut = line(8.620, 47.340, 8.625, 47.345))
  ok("a cut that touches nothing changes nothing", identical(areas(), before))

  session$setInputs(polyDrawn = list(lng = c(8.582, 8.584, 8.582, 8.584), lat = c(47.370, 47.372, 47.372, 47.370)))
  ok("a bow-tie ring is repaired, not refused", n() == nb + 1)
  ok("...into a valid area", all(sf::st_is_valid(areas())))

  id <- areas()$id[n()]
  session$setInputs(finalAOIMap_geojson_click = list(group = "eraseable", properties = list(id = id)))
  ok("clicking an area still deletes it", n() == nb && !(id %in% areas()$id))

  session$setInputs(resetButton = 1)
  ok("reset goes back to the starting areas", n() == 0)

  #the confirm path reads what the handlers wrote
  session$setInputs(polyDrawn = ring(8.585, 47.355, 8.595, 47.365))
  session$setInputs(confirmButton4 = 1)
  fp <- shiny::isolate(r$finalPolygons)
  ok("confirm hands the drawn area on, with an AOI letter", inherits(fp, "sf") && identical(fp$AOI, "A"))

  #a return after the confirm: .vftStep4Launch() seeds the working set from
  #the confirmed areas, which carry an AOI column a fresh ring does not - the
  #old handler's rbind died on that
  r$polygonsList <- fp
  session$setInputs(polyDrawn = ring(8.605, 47.355, 8.615, 47.365))
  ok("drawing after a confirm binds despite the AOI column", n() == 2)
})

cat("\n=== 2. step 1 ===\n")
shiny::testServer(step1_server, args = list(i18n = i18nFn), {
  #the flush a real session runs at start-up: without it every observer's
  #first run is still its ignoreInit run, and the first ring would be dropped
  session$flushReact()
  outline <- function() shiny::isolate(r$polygonsList)

  session$setInputs(polyDrawn = ring(8.585, 47.355, 8.595, 47.365))
  ok("a ring in Switzerland is the outline", inherits(outline(), "sf") && nrow(outline()) == 1)
  ok("...a drawing, and a new one", identical(shiny::isolate(r1$shapeType), "drawing") &&
       isTRUE(shiny::isolate(r1$isNewShape)))
  ok("...and the confirm button shows", isTRUE(shiny::isolate(button2Visible())))

  session$setInputs(polyDrawn = ring(8.60, 47.36, 8.61, 47.37))
  ok("a second ring REPLACES it (one outline in step 1)",
     nrow(outline()) == 1 && isTRUE(all.equal(sf::st_bbox(outline())[["xmin"]], 8.60)))

  #the old handler flagged the SECOND polygon through a TRUE->TRUE write that
  #never invalidated anything; this one sets the buttons directly
  shiny::isolate(button2Visible(FALSE))
  session$setInputs(polyDrawn = ring(8.61, 47.36, 8.62, 47.37))
  ok("...and every ring shows the confirm button again", isTRUE(shiny::isolate(button2Visible())))

  kept <- outline()
  session$setInputs(polyDrawn = ring(2.30, 48.85, 2.35, 48.87))   #Paris
  ok("a ring outside Switzerland is refused", identical(outline(), kept))

  session$setInputs(polyDrawn = list(lng = c(8.58, 8.59, 8.58, 8.59), lat = c(47.36, 47.37, 47.37, 47.36)))
  ok("a bow-tie is refused (step 1 asks for a simpler shape)", identical(outline(), kept))

  session$setInputs(areaSelectMap_geojson_click = list(group = "eraseable", properties = list(id = 1)))
  ok("clicking the outline removes it", is.null(outline()) && is.null(shiny::isolate(r1$finalShape)))
  ok("...and hides the confirm button", !isTRUE(shiny::isolate(button2Visible())))
})

cat("\n=== 3. the live area check agrees with paintAreaTooLarge() ===\n")
#polydraw.js restates paintAreaTooLarge() with swisstopo's approximate
#LV95 formulas, so the browser can warn on every pointer move. It is run here
#in V8, against the R function, on rectangles straddling the ceiling.
lim <- vftPolyDrawAreaLimit()
ok("the limit is paintAreaTooLarge()'s own defaults",
   identical(lim$maxCells, 40e6) && identical(lim$buffer, 250) && identical(lim$res, PAINT_RES))
w1 <- vftPolyDraw(leaflet::leaflet(), list(ns = function(x) paste0("step1-", x)), areaWarn = "areaWarn")
js1 <- w1$jsHooks$render[[1]]$code
ok("areaWarn hands the browser the ceiling and the namespaced element",
   grepl('"maxCells":4e+07|"maxCells":40000000', js1) && grepl('"warn":"step1-areaWarn"', js1, fixed = TRUE))
ok("...and without it there is no live check", !grepl('"area"', js, fixed = TRUE))
ok("the server's ceiling is AOI_SERVER_FACTOR (3) times the heat one",
   identical(lim$hardCells, 3 * lim$maxCells) && identical(AOI_SERVER_FACTOR, 3))
w2 <- vftPolyDraw(leaflet::leaflet(), list(ns = function(x) paste0("step1-", x)), hardWarn = "areaHardWarn")
js2 <- w2$jsHooks$render[[1]]$code
ok("hardWarn alone: the server's ceiling and element, no heat ceiling",
   grepl('"hardWarn":"step1-areaHardWarn"', js2, fixed = TRUE) && grepl('"hardCells"', js2, fixed = TRUE) &&
     !grepl('"maxCells"', js2, fixed = TRUE))
ok("areaWarn alone carries no server ceiling", !grepl('"hardCells"', js1, fixed = TRUE))

if (requireNamespace("V8", quietly = TRUE)) {
  ctx <- V8::v8()
  #just enough DOM for the script's load-time CSS injection and key listener
  ctx$eval("var window = {}; var document = { createElement: function(){ return { appendChild: function(){} }; },
            createTextNode: function(){ return {}; }, head: { appendChild: function(){} },
            addEventListener: function(){} };")
  #the working copy, never the installed package's: that is whatever was
  #installed last, and would test yesterday's script against today's R
  pd <- file.path(dirname(R), "inst/app/www/polydraw.js")
  ctx$source(pd)
  ctx$assign("lim", lim)

  #lv95() against PROJ, across the country
  pts <- expand.grid(lng = seq(6.0, 10.4, by = 0.55), lat = seq(45.9, 47.7, by = 0.3))
  proj <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(pts, coords = c("lng", "lat"), crs = 4326), 2056))
  ctx$assign("pts", pts)
  js_xy <- ctx$get("pts.map(function(p){ var q = window.vftPolyDraw.lv95(p); return [q.e, q.n]; })")
  err <- sqrt(rowSums((js_xy - proj)^2))
  ok("lv95() is within 2 m of PROJ across Switzerland", max(err) < 2, sprintf("(max %.2f m)", max(err)))

  #square outlines whose buffered, snapped box sits a known distance from the
  #ceiling: side s gives (s + 500)^2 cells, the ceiling is 40e6 = 6324.6^2
  crossing <- sqrt(lim$maxCells) - 2 * lim$buffer
  jsOver <- function(p) {
    ctx$assign("ring", data.frame(lng = p$lng, lat = p$lat))
    ctx$get("window.vftPolyDraw.areaOver(lim, ring)")
  }
  rOver <- function(p) paintAreaTooLarge(vftPolyDrawSf(p))
  square <- function(e0, n0, side) {       #an LV95 square, as a WGS84 ring
    sq <- sf::st_sfc(sf::st_polygon(list(rbind(c(e0, n0), c(e0 + side, n0), c(e0 + side, n0 + side),
                                              c(e0, n0 + side), c(e0, n0)))), crs = 2056)
    xy <- sf::st_coordinates(sf::st_transform(sf::st_segmentize(sq, 50), 4326))[, 1:2]
    xy <- xy[-nrow(xy), ]
    list(lng = xy[, 1], lat = xy[, 2])
  }
  agree <- 0; total <- 0; wrongFar <- 0
  for (origin in list(c(2600000, 1200000), c(2500000, 1110000), c(2750000, 1180000), c(2700000, 1270000))) {
    for (d in c(-200, -20, -3, 3, 20, 200)) {
      p <- square(origin[1], origin[2], crossing + d)
      a <- jsOver(p); b <- rOver(p)
      total <- total + 1; agree <- agree + (a == b)
      if (abs(d) >= 20 && a != b) wrongFar <- wrongFar + 1
    }
  }
  ok("browser and R agree on every outline 20 m or more from the ceiling", wrongFar == 0)
  ok("...and on most within 3 m of it", agree >= total - 4, sprintf("(%d of %d)", agree, total))

  #the same at the server's ceiling: areaLevel() == 2 against aoiTooLargeForServer()
  jsLevel <- function(p, a = lim) {
    ctx$assign("ring", data.frame(lng = p$lng, lat = p$lat)); ctx$assign("a", a)
    ctx$get("window.vftPolyDraw.areaLevel(a, ring)")
  }
  hardCrossing <- sqrt(lim$hardCells) - 2 * lim$buffer
  wrongFar <- 0
  for (origin in list(c(2600000, 1200000), c(2700000, 1180000))) {
    for (d in c(-200, -20, 20, 200)) {
      p <- square(origin[1], origin[2], hardCrossing + d)
      if ((jsLevel(p) == 2) != aoiTooLargeForServer(vftPolyDrawSf(p))) wrongFar <- wrongFar + 1
    }
  }
  ok("...and at the server's ceiling, 20 m or more from it", wrongFar == 0)
  mid <- square(2600000, 1200000, (crossing + hardCrossing) / 2)
  ok("between the two ceilings is level 1", jsLevel(mid) == 1)
  ok("...past the second, level 2", jsLevel(square(2600000, 1200000, hardCrossing + 200)) == 2)
  ok("...and with no heat ceiling (switched off) level 0 between them",
     jsLevel(mid, lim[c("hardCells", "buffer", "res")]) == 0)
  ok("the pointer counts: a small ring does not warn", !jsOver(ring(8.585, 47.355, 8.595, 47.365)))
  ok("...a ring pulled out 10 km does", jsOver(list(lng = c(8.585, 8.595, 8.72), lat = c(47.355, 47.355, 47.45))))
  ok("no limit, no warning", !ctx$get("window.vftPolyDraw.areaOver(null, [{lng: 6, lat: 46}, {lng: 10, lat: 47.5}])"))
} else {
  cat("(V8 not installed - browser/R agreement not checked)\n")
}

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
