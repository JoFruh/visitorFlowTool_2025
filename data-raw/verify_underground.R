## Verification for the underground warning in Hitzeminderung:
## R/underground_helpers.R and data-raw/generate_underground_CH.r.
##
## Checked:
##   1. hit arithmetic on a synthetic seed - exact m2 for a tree, a block
##      counted once although it arrives on both levels, water warns, nothing
##      for grass or artificial canopy, nothing for an ignored element, the
##      depth range of the hit cells only, and a flush sent twice counting once;
##      the brush's mask runs decode back to exactly the seed's cells
##   2. ug_pieces() on a synthetic 3D line over a flat terrain: piece length,
##      depth, width
##   3. labels: garage with EGID, building with its GWR class in French, a
##      named tunnel, unknown and measured depth; the warning box names what is
##      done over which element (bold), with the ignore button or "ignored"
##   4. the national file (skipped when absent): central Zurich has >= 352 AV
##      underground buildings, the Sion window >= 32 plus measured TLM3D
##      pieces, one kind per element, Lausanne falls back to OSM and OSM rows
##      stay inside the fallback cantons
##   5. undergroundSeed() on a real area: grid on the paint grid, every
##      element's own cells carry its id, a tree stroke over one element hits
##      exactly it, deep tunnel pieces never warn
##   everything with terra NOT attached - the app's condition
##
## Run:  Rscript data-raw/verify_underground.R
suppressPackageStartupMessages({library(shiny); library(sf)})
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
env <- new.env(parent = globalenv())
for (f in sort(list.files(R, pattern = "[.][Rr]$", full.names = TRUE))) {
  suppressWarnings(try(sys.source(f, envir = env), silent = TRUE))
}
attach(env, warn.conflicts = FALSE)
stopifnot(!"package:terra" %in% search())

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

# ------------------------------------------------------------------ 1 ------
cat("=== 1. hit arithmetic on a synthetic seed ===\n")
X0 <- 2600000; YT <- 1200100
idm <- matrix(NA_integer_, 100, 100)          #[ri, ci]
idm[11:20, 11:30] <- 5L
idm[51:60, 51:60] <- 9L
dpm <- matrix(NA_real_, 100, 100)
dpm[11:12, 11:30] <- 3.2
dpm[13, 11:30]    <- 4.8
seed <- list(grid = list(xmin = X0, ymax = YT, ncol = 100L, nrow = 100L, res = 1),
             id = as.vector(t(idm)), depth = as.vector(t(dpm)), noData = FALSE)
#a run over raster rows ri, from column ci0, n cells long - in global indices
run <- function(ri, ci0, n) as.vector(rbind(YT - ri, X0 + ci0 - 1, n))
runs <- function(id, ri, ci0, n) list(list(id = id, runs = unlist(lapply(ri, run, ci0 = ci0, n = n))))

tree  <- list(ground = list(), canopy = runs(10L, 11:15, 1, 50))          #3 m tree
h <- undergroundHits(tree, seed)
ok("a tree over element 5 gives exactly one row", nrow(h) == 1 && h$ug_id == 5 && h$base == 7)
ok("...of 5 x 20 = 100 m2", isTRUE(h$m2 == 100), sprintf("(%s)", h$m2))
ok("...with the depth range of the hit cells only (3.2-4.8)",
   isTRUE(h$dmin == 3.2 && h$dmax == 4.8), sprintf("(%s-%s)", h$dmin, h$dmax))

blk <- runs(16L, 16:20, 21, 20)                                           #5 m block
hb <- undergroundHits(list(ground = blk, canopy = blk), seed)
ok("a block on both levels is counted once (5 x 10 = 50 m2)",
   nrow(hb) == 1 && hb$base == 8 && hb$m2 == 50, sprintf("(%s)", paste(hb$m2, collapse = ",")))
ok("...and its depth is unknown", is.na(hb$dmin))

ok("water over the element warns",
   nrow(undergroundHits(list(ground = runs(5L, 11:12, 11, 5), canopy = list()), seed)) == 1)
ok("grass over the element warns nothing",
   nrow(undergroundHits(list(ground = runs(1L, 11:20, 1, 100), canopy = list()), seed)) == 0)
ok("artificial canopy over it warns nothing",
   nrow(undergroundHits(list(ground = list(), canopy = runs(14L, 11:20, 1, 100)), seed)) == 0)
ok("the eraser (id 0) warns nothing",
   nrow(undergroundHits(list(ground = list(), canopy = runs(0L, 11:20, 1, 100)), seed)) == 0)
ok("an ignored element warns nothing",
   nrow(undergroundHits(tree, seed, ignore = 5L)) == 0)
both <- list(ground = list(), canopy = c(runs(7L, 11, 11, 1), runs(7L, 55, 55, 1)))
ok("two elements hit give two rows", setequal(undergroundHits(both, seed)$ug_id, c(5, 9)))
ok("cells outside the grid are simply not hit",
   nrow(undergroundHits(list(ground = list(), canopy = runs(7L, 200, -50, 10)), seed)) == 0)

c1 <- undergroundHitCells(tree, seed)
acc <- undergroundMergeCells(undergroundMergeCells(NULL, c1), c1)
ok("the same flush twice still covers 100 m2", undergroundSummarise(acc, seed)$m2 == 100)
acc <- undergroundMergeCells(acc, undergroundHitCells(list(ground = list(),
                                                            canopy = runs(10L, 16:17, 11, 20)), seed))
ok("a second flush over new cells adds them (140 m2)", undergroundSummarise(acc, seed)$m2 == 140)

th <- undergroundTopHit(undergroundHitCells(both, seed))
ok("the top hit of a plan names one element and its material",
   !is.null(th) && th$ug_id %in% c(5, 9) && th$base == 7)
ok("no cells, no top hit", is.null(undergroundTopHit(undergroundHitCells(list(), seed))))

#the mask the brush stops at: runs (row, colStart, count, id) decode back to
#exactly the seed's element cells, in the same global indices as the paint
mr <- matrix(undergroundMaskRuns(seed), nrow = 4)
ok("mask: one run per element row (10 + 10)", ncol(mr) == 20, sprintf("(%d)", ncol(mr)))
dec <- do.call(rbind, lapply(seq_len(ncol(mr)), function(j)
  data.frame(row = mr[1, j], col = mr[2, j] + seq_len(mr[3, j]) - 1L, id = mr[4, j])))
ref <- which(!is.na(seed$id))
refdf <- data.frame(row = YT - ((ref - 1L) %/% 100L + 1L), col = X0 + (ref - 1L) %% 100L,
                    id = seed$id[ref])
ok("mask: decoded cells equal the seed's cells",
   nrow(dec) == nrow(refdf) &&
     identical(sort(paste(dec$row, dec$col, dec$id)), sort(paste(refdf$row, refdf$col, refdf$id))))
ok("mask: a stroke at a decoded cell hits that element",
   undergroundHits(list(ground = list(), canopy = list(list(id = 7L,
     runs = c(dec$row[1], dec$col[1], 1)))), seed)$ug_id == dec$id[1])

# ------------------------------------------------------------------ 2 ------
cat("=== 2. ug_pieces() on a synthetic line ===\n")
local({
  old <- getwd(); on.exit(setwd(old))
  if(!file.exists("data-raw/generate_underground_CH.r"))
    setwd("C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
  source("data-raw/generate_underground_CH.r", local = TRUE)
  dtm <- terra::rast(xmin = X0 - 100, xmax = X0 + 200, ymin = YT - 100, ymax = YT + 100,
                     resolution = 5, crs = "EPSG:2056", vals = 500)
  #a 100 m line along x, floor from 500 m (at the surface) down to 490 m
  ln <- sf::st_sf(width = 8, geometry = sf::st_sfc(sf::st_linestring(
    rbind(c(X0, YT, 500), c(X0 + 100, YT, 490))), crs = 2056))
  p <- ug_pieces(ln, dtm, step = 20)
  ok("100 m at 20 m gives 5 pieces", nrow(p) == 5, sprintf("(%d)", nrow(p)))
  ok("depths are 1, 3, 5, 7, 9 m", isTRUE(all.equal(p$depth_m, c(1, 3, 5, 7, 9))),
     paste(p$depth_m, collapse = ","))
  a <- as.numeric(sf::st_area(p))
  ok("each piece is 8 m wide and 20 + 8 m long (ends overlap)",
     isTRUE(all.equal(a, rep(8 * 28, 5))), sprintf("(%s)", a[1]))
  #a line above the terrain is a surface, depth clamped to 0
  up <- sf::st_sf(width = 3, geometry = sf::st_sfc(sf::st_linestring(
    rbind(c(X0, YT, 501), c(X0 + 10, YT, 501))), crs = 2056))
  ok("a line 1 m above the terrain has depth 0", all(ug_pieces(up, dtm)$depth_m == 0))
})

# ------------------------------------------------------------------ 3 ------
cat("=== 3. labels ===\n")
el <- function(kind, name = NA, egid = NA, gklas = NA)
  data.frame(ug_id = 1L, kind = kind, name = name, egid = egid, gklas = gklas)
ok("garage with EGID", undergroundLabel(el("garage", egid = 140889)) == "Tiefgarage (EGID 140889)",
   undergroundLabel(el("garage", egid = 140889)))
lb <- undergroundLabel(el("building", egid = 302005846, gklas = 1220L), lang = "fr")
ok("building with its GWR class, French class name",
   lb == "Unterirdisches Bauwerk (Immeuble de bureaux, EGID 302005846)", lb)
lt <- undergroundLabel(el("road_tunnel", name = "Milchbucktunnel"))
ok("named road tunnel", lt == "Strassentunnel \u00abMilchbucktunnel\u00bb", lt)
ok("unknown depth", undergroundDepthText(NA, NA) == "Tiefe unbekannt")
ok("measured depth range", undergroundDepthText(2.6, 6.2) == "Sohle ca. 3\u20136 m unter Terrain",
   undergroundDepthText(2.6, 6.2))
ok("single depth", undergroundDepthText(4, 4) == "Sohle ca. 4 m unter Terrain")
gar <- el("garage", egid = 1) |> transform(ug_id = 5L)
html <- as.character(undergroundWarningUI(gar, 5L, 7L, ignored = FALSE,
                                          ignoreInput = "newVersions-ugIgnore"))
ok("the box says what is done over which element, the element in bold",
   grepl("Sie pflanzen Baeume ueber einem unterirdischen Bauwerk: <b>Tiefgarage (EGID 1)</b>.",
         html, fixed = TRUE), html)
ok("...that it is not recommended", grepl("Dies wird nicht empfohlen.", html, fixed = TRUE))
ok("...with a dark grey ignore button for it",
   grepl(UG_COLOR, html, fixed = TRUE) && grepl("Dieses Element ignorieren", html, fixed = TRUE) &&
     #the attribute is HTML-escaped; the browser hands the handler plain quotes
     grepl("Shiny.setInputValue(&#39;newVersions-ugIgnore&#39;, 5,", html, fixed = TRUE))
hi <- as.character(undergroundWarningUI(gar, 5L, 8L, ignored = TRUE,
                                        ignoreInput = "newVersions-ugIgnore"))
ok("ignored: a light grey 'Element ignoriert' replaces the button",
   grepl("Element ignoriert", hi, fixed = TRUE) && grepl(UG_IGNORED_COLOR, hi, fixed = TRUE) &&
     !grepl("setInputValue", hi, fixed = TRUE))
ok("a block reads as building, water as placing water",
   grepl("Sie bauen", hi, fixed = TRUE) &&
     grepl("Sie platzieren Wasser", as.character(undergroundWarningUI(gar, 5L, 5L, FALSE, "x")), fixed = TRUE))

# ------------------------------------------------------------------ 4 ------
path <- undergroundPath()
if(!file.exists(path)){
  cat("=== 4-5 skipped: ", path, " not built ===\n")
}else{
  cat("=== 4. the national file ===\n")
  win <- function(xmin, ymin, xmax, ymax, layer = "underground")
    sf::st_read(path, layer = layer, quiet = TRUE,
                wkt_filter = sf::st_as_text(sf::st_as_sfc(sf::st_bbox(
                  c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = 2056)))) |>
    (\(x) x[lengths(sf::st_intersects(x, sf::st_as_sfc(sf::st_bbox(
      c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = 2056)))) > 0, ])()
  zh <- win(2682500, 1247500, 2683500, 1248500)
  nzh <- sum(zh$source == "AV" & zh$kind %in% c("garage", "building"))
  #the raw WFS has 352 here: 343 "gueltig" and 9 "projektiert" (planned, not in
  #the ground), which ug_read_av() drops on purpose
  ok("central Zurich: >= 343 existing AV underground buildings", nzh >= 343, sprintf("(%d)", nzh))
  ok("...most of them with an EGID", mean(!is.na(zh$egid[zh$source == "AV" &
                                                          zh$kind %in% c("garage", "building")])) > 0.5)
  si <- win(2592000, 1118500, 2596000, 1121500)
  nsi <- sum(si$source == "AV" & si$kind %in% c("garage", "building"))
  ok("Sion window: >= 32 AV underground buildings", nsi >= 32, sprintf("(%d)", nsi))
  ok("...and measured TLM3D pieces", sum(si$rank == 1 & !is.na(si$depth_m)) > 0,
     sprintf("(%d)", sum(si$rank == 1)))
  both <- rbind(sf::st_drop_geometry(zh[, c("ug_id", "kind")]),
                sf::st_drop_geometry(si[, c("ug_id", "kind")]))
  ok("every element has one kind", all(tapply(both$kind, both$ug_id,
                                              function(k) length(unique(k))) == 1))
  lau <- win(2537500, 1152000, 2538500, 1153000, layer = "coverage")
  ok("Lausanne (VD, unreleased) falls back to OSM", identical(unique(lau$status), "osm"),
     paste(lau$kanton, lau$status))
  #central Lausanne, 2 x 2 km: OSM has underground car parks there
  lz <- win(2537000, 1151500, 2539000, 1153500)
  ok("...and OSM underground car parks are in the file there",
     sum(lz$source == "OSM" & lz$kind == "garage") >= 3,
     sprintf("(%d OSM garages, %d AV rows)", sum(lz$source == "OSM" & lz$kind == "garage"),
             sum(lz$source == "AV")))
  cov <- sf::st_read(path, layer = "coverage", quiet = TRUE)
  osm <- sf::st_read(path, quiet = TRUE,
                     query = "SELECT * FROM underground WHERE source = 'OSM'")
  inK <- cov$kanton[unlist(sf::st_intersects(sf::st_point_on_surface(sf::st_geometry(osm)),
                                             cov))]
  ok("OSM rows lie only in cantons marked osm", all(inK %in% cov$kanton[cov$status == "osm"]),
     sprintf("(%d rows: %s)", nrow(osm), paste(names(table(inK)), table(inK), collapse = ", ")))

  cat("=== 5. undergroundSeed() on a real area ===\n")
  aoi <- sf::st_transform(sf::st_as_sfc(sf::st_bbox(
    c(xmin = 2682700, ymin = 1247700, xmax = 2683300, ymax = 1248300), crs = 2056)), 4326)
  t0 <- Sys.time()
  s <- undergroundSeed(aoi)
  dt <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  ok("seed built", !is.null(s) && nrow(s$elements) > 50,
     sprintf("(%d elements, %.1f s)", if(is.null(s)) 0L else nrow(s$elements), dt))
  g <- s$grid
  ok("grid sits on the paint grid", g$xmin %% PAINT_RES == 0 && g$ymax %% PAINT_RES == 0 &&
       length(s$id) == g$ncol * g$nrow)
  ok("the area is released, so no no-data flag", isFALSE(s$noData))
  #a 3 x 3 tree at an interior point of the largest AV element
  e2 <- sf::st_transform(s$elements, 2056)
  big <- e2[e2$kind %in% c("garage", "building"), ]
  big <- big[which.max(as.numeric(sf::st_area(big))), ]
  pt <- sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(big)))
  row <- floor(pt[2]); col <- floor(pt[1])
  hr <- undergroundHits(list(ground = list(), canopy = list(list(id = 7L,
          runs = as.vector(rbind(row + (-1:1), col - 1, 3))))), s)
  ok("a 3 x 3 tree inside the largest underground building hits it, 9 m2",
     nrow(hr) == 1 && hr$ug_id == big$ug_id && hr$m2 == 9,
     sprintf("(%s)", paste(hr$ug_id, hr$m2, collapse = ";")))
  lbl <- undergroundLabel(sf::st_drop_geometry(big))
  cat("   e.g.", lbl, "\n")

  #deep pieces: the Milchbucktunnel region has a deep stretch? take any tunnel
  #element in a wider window and check no cell deeper than the limit warns
  aoi2 <- sf::st_transform(sf::st_as_sfc(sf::st_bbox(
    c(xmin = 2682000, ymin = 1248000, xmax = 2685000, ymax = 1250500), crs = 2056)), 4326)
  s2 <- undergroundSeed(aoi2)
  d <- s2$depth[!is.na(s2$id)]
  ok("no warning cell has a measured depth past UG_MAX_DEPTH_M",
     all(is.na(d) | d <= UG_MAX_DEPTH_M), sprintf("(max %.1f)", max(d, na.rm = TRUE)))
  measured <- s2$elements$ug_id[!is.na(s2$elements$dmin)]
  cellsM <- !is.na(s2$id) & s2$id %in% measured
  ok("every cell of a tunnel/culvert with a measured depth carries one",
     all(!is.na(s2$depth[cellsM])), sprintf("(%d of %d)", sum(!is.na(s2$depth[cellsM])), sum(cellsM)))
  ok("the overlay shows exactly the elements that can warn",
     setequal(s2$elements$ug_id, unique(s2$id[!is.na(s2$id)])))
  ok("depth is only reported on line elements' own cells",
     all(is.na(s2$depth[!is.na(s2$id) & !(s2$id %in% s2$elements$ug_id[s2$elements$kind %in%
            c("road_tunnel", "rail_tunnel", "underpass", "culvert", "tunnel")])])))
}

cat(sprintf("\n%d check(s) failed\n", fails))
