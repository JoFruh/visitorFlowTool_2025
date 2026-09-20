## Verification for Phase 4: the assembled heat model (R/heat_helpers.R).
## Run:  Rscript data-raw/verify_heat_model.R
##
## Phase 1 checked the tables, Phases 2-3 checked the geometry. This checks that
## the thing built out of them says what the tables say it should - in kelvin of
## PET against unshaded grass at midday - and that the five structural defects
## the rewrite existed to fix are actually fixed.
suppressPackageStartupMessages({library(terra); library(sf)})
terraOptions(progress = 0)
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
for (f in c("perf_helpers.R", "data_paths.R", "paintbrush_helpers.R",
            "heat_helpers.R", "shadow_helpers.R", "svf_helpers.R")) {
  suppressWarnings(try(source(file.path(R, f)), silent = TRUE))
}

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-64s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

cat("=== 1. tables load and cover the classes ===\n")
mat <- heatMaterials(); dec <- heatDecay(); geo <- heatGeometry()
ok("heat_materials.csv loads", !is.null(mat) && nrow(mat) == 45)
ok("heat_decay.csv loads", !is.null(dec) && nrow(dec) == 9)
ok("heat_geometry.csv loads", !is.null(geo))
ok("pet_0m_K parsed as numeric, not character", is.numeric(mat$pet_0m_K))
ok("amp_edge_K parsed as numeric", is.numeric(dec$amp_edge_K))
ok("the reference surface is exactly zero",
   mat$pet_0m_K[mat$class_id == 1 & mat$shaded == 0 & mat$time_bin == "midday"] == 0)

cat("\n=== 2. the decay kernel and its half-plane response ===\n")
for (i in which(!is.na(dec$half_dist_m))) {
  a <- dec$amp_edge_K[i]; h <- dec$half_dist_m[i]; m <- dec$max_extent_m[i]
  x <- seq(0, m, length.out = 400)
  w <- heat_decay_weight(x, h, m)
  ok(sprintf("%-17s kernel falls to zero without a step (%.4f at max)",
             dec$class_name[i], w[length(w)]), abs(w[length(w)]) < 1e-9)
  ok(sprintf("%-17s kernel is monotonic and starts at 1", dec$class_name[i]),
     all(diff(w) <= 1e-12) && abs(w[1] - 1) < 1e-12)
  ## the normaliser: the half-plane a straight edge presents must return amp
  ok(sprintf("%-17s straight edge returns amp_edge_K (%+.3f vs %+.3f)",
             dec$class_name[i], heat_decay_halfplane(0, a, h, m), a),
     abs(heat_decay_halfplane(0, a, h, m) - a) < 0.002)
}

cat("\n=== 2b. a big forest must beat a small copse ===\n")
## The defect this replaced: with distance-to-nearest, a 0.25 ha copse and a
## 64 ha forest produced bit-identical fields. Size now enters through geometry -
## a bigger wood has more of itself inside the kernel - so the response must rise
## with area and then saturate once the patch outgrows max_extent_m.
tree <- dec[dec$class_id == 7, ]
## Ground 0 as the background, deliberately. Grass looks like the natural choice
## and is a trap: class 1 has its own advective term, so a field of it adds a
## uniform -0.6 K to every probe and the wood's own contribution can no longer be
## read off. 0 is absent from heat_decay.csv, so it contributes nothing and the
## measurement is of the wood alone.
sq <- function(side_m, res = HEAT_RES, pad_m = 320) {
  ns <- round(side_m / res); np <- round(pad_m / res); n <- ns + 2 * np
  g <- rast(nrows = n, ncols = n, xmin = 0, xmax = n * res, ymin = 0, ymax = n * res,
            crs = "EPSG:2056")
  values(g) <- 0L; k <- rast(g); values(k) <- 0L
  k[(np + 1):(np + ns), (np + 1):(np + ns)] <- 7L
  list(g = g, k = k, i1 = np + ns, mid = np + ns %/% 2)
}
resp <- sapply(c(50, 100, 200, 400, 800), function(sd) {
  s <- sq(sd)
  a <- heatAdvectiveTerm(s$g, s$k, dec, res = HEAT_RES, conv_res = HEAT_RES)
  as.numeric(a[s$mid, s$i1 + round(25 / HEAT_RES)][1])
})
cat("   response 25 m from the edge, by wood size (0.25 / 1 / 4 / 16 / 64 ha):\n   ",
    paste(sprintf("%+.3f", resp), collapse = "  "), "K\n")
ok("a larger wood cools more, up to saturation", all(diff(resp[1:4]) < -1e-4))
ok(sprintf("0.25 ha is far weaker than a large forest (%.0f%% of it)",
           100 * resp[1] / resp[5]), abs(resp[1]) < 0.5 * abs(resp[5]))
ok("the response saturates once the wood outgrows max_extent_m",
   abs(resp[5] - resp[4]) < 0.01)

cat("\n=== 2c. the raster model agrees with the table ===\n")
## The table carries the analytic continuous half-plane response; the model does
## a discrete convolution on a finite grid. They cannot agree exactly, and the
## gap is a discretisation artefact rather than a disagreement about physics -
## but it has to be bounded and visible rather than assumed small.
s <- sq(1600, pad_m = 320)
a <- heatAdvectiveTerm(s$g, s$k, dec, res = HEAT_RES, conv_res = HEAT_RES)
worst <- 0
for (d in c(25, 75, 150)) {
  got  <- as.numeric(a[s$mid, s$i1 + round(d / HEAT_RES)][1])
  want <- heat_decay_halfplane(d, tree$amp_edge_K, tree$half_dist_m, tree$max_extent_m)
  worst <- max(worst, abs(got - want))
  cat(sprintf("   %3d m  raster %+.3f  table %+.3f  diff %+.3f K\n", d, got, want, got - want))
}
ok(sprintf("raster and table agree within 0.10 K (worst %.3f K)", worst), worst < 0.10)

## --- the real rasters -------------------------------------------------------
LC <- Sys.getenv("VFT_LANDCOVER_DIR", "C:/Users/frueh/Documents/Local Data/landcover")
if (!file.exists(file.path(LC, "ground_CH_1m.tif"))) {
  cat("\nnational land cover not present - skipping the on-raster checks\n")
  cat(sprintf("\n%d check(s) failed\n", fails)); quit(status = if (fails == 0) 0 else 1)
}
CX <- 2593956; CY <- 1119554
e <- ext(CX - 700, CX + 700, CY - 500, CY + 500)     # non-square on purpose
gr <- aggregate(crop(rast(file.path(LC, "ground_CH_1m.tif")), e), HEAT_RES, fun = "modal")
cn <- aggregate(crop(rast(file.path(LC, "canopy_CH_1m.tif")), e), HEAT_RES, fun = "modal")

cat("\n=== 3. the terms, separately ===\n")
sh <- heatShadeRaster(gr, cn, "midday", geo)
lo <- heatLocalTerm(gr, cn, sh, "midday", mat)
ok("local term returned", !is.null(lo))
ok("local term is in the range the table spans",
   min(values(lo), na.rm = TRUE) >= -6 && max(values(lo), na.rm = TRUE) <= 12,
   sprintf("[%.1f, %.1f]", min(values(lo), na.rm = TRUE), max(values(lo), na.rm = TRUE)))
## DEFECT 1: the pixel's own material must reach the output directly
as_sun <- values(lo)[values(gr) == 3 & values(sh) == 0]
gr_sun <- values(lo)[values(gr) == 1 & values(sh) == 0]
ok(sprintf("sunlit asphalt reads hotter than sunlit grass (%.1f vs %.1f K)",
           mean(as_sun, na.rm = TRUE), mean(gr_sun, na.rm = TRUE)),
   mean(as_sun, na.rm = TRUE) > mean(gr_sun, na.rm = TRUE) + 5)
## DEFECT 3: shade must collapse the spread between materials, not shift it
spread <- function(msk) {
  v <- sapply(c(1, 3, 4), function(k) mean(values(lo)[values(gr) == k & msk], na.rm = TRUE))
  diff(range(v, na.rm = TRUE))
}
s_sun <- spread(values(sh) == 0); s_shd <- spread(values(sh) == 1)
ok(sprintf("shade collapses the material spread (%.1f K sunlit -> %.1f K shaded)",
           s_sun, s_shd), s_shd < s_sun)

adv <- heatAdvectiveTerm(gr, cn, dec, res = HEAT_RES)
ok("advective term returned and is small next to the local one",
   !is.null(adv) && max(abs(values(adv)), na.rm = TRUE) < 3,
   sprintf("max |adv| = %.2f K", max(abs(values(adv)), na.rm = TRUE)))
## DEFECT 5: water must be a cool source at distance, not neutral
ok("water contributes cooling to its surroundings",
   any(values(gr) == 5, na.rm = TRUE) && min(values(adv), na.rm = TRUE) < 0)

cat("\n=== 4. the assembled model ===\n")
h <- list()
for (b in HEAT_BINS) {
  t0 <- Sys.time()
  sh_b <- heatShadeRaster(gr, cn, b, geo)
  lo_b <- heatLocalTerm(gr, cn, sh_b, b, mat)
  sv_b <- heatSvfRaster(gr, cn, geom = geo)
  wl_b <- heatWallRaster(gr, cn, b, geo, sh_b)
  ge_b <- heatGeometryTerm(sh_b, sv_b, wl_b, geo)
  h[[b]] <- lo_b + ge_b + adv
  cat(sprintf("   %-9s median %+5.1f K  range [%+.1f, %+.1f]  %.2f s\n", b,
              median(values(h[[b]]), na.rm = TRUE),
              min(values(h[[b]]), na.rm = TRUE), max(values(h[[b]]), na.rm = TRUE),
              as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
ok("output is finite where the ground is classified",
   !any(is.infinite(values(h$midday)), na.rm = TRUE))
## DEFECT 2: the result must carry kelvin, not an arbitrary +-0.09
ok("the scale spans kelvin, not the old unitless +-0.09",
   diff(range(values(h$midday), na.rm = TRUE)) > 5,
   sprintf("span %.1f K", diff(range(values(h$midday), na.rm = TRUE))))
## DEFECT 6: thermal inertia must reorder the day.
##
## Tested on the MEAN, not the median, and that is not a softer test - it is the
## correct one. The share of the map that is shaded changes with sun elevation
## (50 % morning, 40 % midday, 50 % afternoon), so the median lands on a
## different material-and-state combination in each bin and is not monotonic:
## at midday the median cell is sunlit grass, which is the reference surface and
## therefore exactly 0.00 by construction. The mean is monotonic because every
## individual material really is hotter later in the day, which group 6 below
## checks cell by cell.
mn <- sapply(HEAT_BINS, function(b) mean(values(h[[b]]), na.rm = TRUE))
ok(sprintf("the day warms monotonically in the mean (%.2f -> %.2f -> %.2f K)",
           mn[1], mn[2], mn[3]), all(diff(mn) > 0))

cat("\n=== 6. the model reproduces the tables ===\n")
## The strongest check available: on cells where nothing but the material acts -
## sunlit, fully open sky, no wall nearby, no advective source in range - the
## output must BE the table value. Anything else means a term is leaking.
sv <- heatSvfRaster(gr, cn, geom = geo)
for (b in HEAT_BINS) {
  sh_b <- heatShadeRaster(gr, cn, b, geo)
  wl_b <- heatWallRaster(gr, cn, b, geo, sh_b)
  tot  <- heatLocalTerm(gr, cn, sh_b, b, mat) + heatGeometryTerm(sh_b, sv, wl_b, geo) + adv
  clean <- values(sh_b) == 0 & values(sv) > 0.995 & values(wl_b) == 0 & abs(values(adv)) < 0.02
  for (k in c(1, 3, 4, 5)) {
    m <- clean & values(gr) == k
    if (sum(m, na.rm = TRUE) < 5) next
    got  <- median(values(tot)[m], na.rm = TRUE)
    want <- mat$pet_0m_K[mat$class_id == k & mat$shaded == 0 & mat$time_bin == b]
    ok(sprintf("%-9s class %d reproduces the table (%+.2f vs %+.2f)", b, k, got, want),
       abs(got - want) < 0.05)
  }
}

cat("\n=== 7. unclassified ground ===\n")
na_share <- 100 * mean(is.na(values(h$midday)))
ok(sprintf("almost nothing drops out as unclassified now (%.2f%%)", na_share),
   na_share < 1)

cat("\n=== 8. the cache never changes an answer ===\n")
## A cache is only worth having if it is invisible in the output. This walks a
## realistic session - paint, switch bin, paint something that moves the
## geometry, switch back, erase - and demands the cached frame be IDENTICAL to a
## cold one, not close to it. Every term here is either reused whole or rebuilt
## whole, so exact equality is the right bar.
##
## Step 5 is the one that matters. Geometry layers are held per bin, and
## `touched` is measured against the previous call, so planting a tree while the
## afternoon is on screen leaves a stale midday shade behind that no later call
## would ever see as dirty. Before heat_cache_state() learned to purge every
## bin's geometry rather than the current one's, that step was wrong by 5 K.
poly <- st_sfc(st_polygon(list(rbind(c(CX-700, CY-500), c(CX+700, CY-500),
                                     c(CX+700, CY+500), c(CX-700, CY+500),
                                     c(CX-700, CY-500)))), crs = 2056)
aoi <- st_sf(geometry = st_transform(poly, 4326))
ed <- function(cls, side) {
  r <- rast(ext(CX - side/2, CX + side/2, CY - side/2, CY + side/2),
            resolution = 1, crs = "EPSG:2056")
  values(r) <- as.integer(cls); r
}
G <- ed(1, 120); T7 <- ed(7, 80)
walk <- list(list("cold midday",           "midday",    NULL, NULL),
             list("ground repaint",        "midday",    G,    NULL),
             list("switch bin",            "afternoon", G,    NULL),
             list("plant trees",           "afternoon", G,    T7),
             list("switch back",           "midday",    G,    T7),
             list("erase the trees",       "midday",    G,    NULL),
             list("repaint another class", "midday",    ed(5, 120), NULL))
ca <- heatCacheNew()
for (s in walk) {
  cold <- heatRaster(aoi, s[[3]], s[[4]], bin = s[[2]])
  warm <- heatRaster(aoi, s[[3]], s[[4]], bin = s[[2]], cache = ca)
  d <- abs(values(cold) - values(warm)); d <- d[is.finite(d)]
  ok(sprintf("cached frame is identical after: %s", s[[1]]),
     length(d) > 0 && max(d) == 0, sprintf("[max |diff| %.3g]", max(d)))
}
## The incremental baseline: a stroke INSIDE an already-painted area, so the
## edits keep their extent and only a handful of 1 m cells differ. That is the
## path heat_landcover() re-reads a window for, and the one a live repaint would
## take on every flush. The whole stroke must still land, and nothing outside it
## may move - a window snapped in rather than out would corrupt the blocks on the
## boundary, where a 5 m cell would take its mode from only part of its cells.
big <- ed(1, 400)
stroke <- function(cls){
  r <- big
  ix <- cells(r, ext(CX - 30, CX + 30, CY - 30, CY + 30))
  r[ix] <- as.integer(cls); r
}
ca2 <- heatCacheNew()
invisible(heatRaster(aoi, big, NULL, bin = "midday", cache = ca2))
for (cls in c(3, 5, 2)) {
  cold <- heatRaster(aoi, stroke(cls), NULL, bin = "midday")
  warm <- heatRaster(aoi, stroke(cls), NULL, bin = "midday", cache = ca2)
  d <- abs(values(cold) - values(warm)); d <- d[is.finite(d)]
  ok(sprintf("a stroke inside a painted area is exact (class %d)", cls),
     length(d) > 0 && max(d) == 0, sprintf("[max |diff| %.3g]", max(d)))
}
## and the stroke really did reach the model, rather than being lost in a window
## that never got re-read - which would also give max |diff| of 0 against a cold
## call only if the cold call were wrong too, so compare the two class fields
w3 <- heatRaster(aoi, stroke(3), NULL, bin = "midday", cache = ca2)
w5 <- heatRaster(aoi, stroke(5), NULL, bin = "midday", cache = ca2)
ok("and repainting that stroke changes the surface",
   max(abs(values(w3) - values(w5)), na.rm = TRUE) > 1)

## THE CASE THAT BREAKS A NAIVE DIRTY RECTANGLE.
##
## min_patch_ha is a property of a whole connected patch, so one cell can change
## the eligibility of cells far outside any halo drawn around the edit. A 300 m
## tree avenue at 0.3 ha clears the 0.2 ha floor; erase one cell pair at its
## midpoint and it becomes two 0.15 ha halves, both below the floor, and the
## entire line stops being a source - changing cells 256 m from the edit.
##
## The incremental path survives this because it diffs the mask AFTER the patch
## test, not the class raster before it: every cell that stopped qualifying
## shows up in that diff, so the window covers the whole avenue rather than the
## erased cell. A diff taken on the raw paint would miss it entirely.
avenue <- function(gap) {
  r <- rast(ext(CX - 150, CX + 150, CY - 5, CY + 5), resolution = 1, crs = "EPSG:2056")
  values(r) <- 7L
  if (gap) r[cells(r, ext(CX - 3, CX + 3, CY - 5, CY + 5))] <- NA
  r
}
ca3 <- heatCacheNew()
a_full <- heatRaster(aoi, NULL, avenue(FALSE), bin = "midday", cache = ca3)
inc    <- heatRaster(aoi, NULL, avenue(TRUE),  bin = "midday", cache = ca3)
cold   <- heatRaster(aoi, NULL, avenue(TRUE),  bin = "midday")
d <- abs(values(inc) - values(cold)); d <- d[is.finite(d)]
ok("cutting a tree avenue below min_patch_ha is exact incrementally",
   length(d) > 0 && max(d) == 0, sprintf("[max |diff| %.3g]", max(d)))
ok("...and it really did change the surface far from the cut",
   max(abs(values(a_full) - values(cold)), na.rm = TRUE) > 0.05,
   sprintf("[%.2f K]", max(abs(values(a_full) - values(cold)), na.rm = TRUE)))

## the windowed convolution must equal the full one wherever it is defined
dtr <- dec[dec$class_id == 7, , drop = FALSE]
mk  <- heat_source_mask(gr, cn, dtr, HEAT_RES)
if (!is.null(mk)) {
  fullf <- heat_adv_field(mk, dtr, HEAT_RES, HEAT_ADV_RES)
  w     <- ext(CX - 200, CX + 200, CY - 150, CY + 150)
  partf <- heat_adv_field(mk, dtr, HEAT_RES, HEAT_ADV_RES, win = w)
  cmp   <- abs(values(crop(fullf, ext(partf))) - values(partf))
  ok(sprintf("windowed advective field matches the full one (max %.3g K)",
             max(cmp, na.rm = TRUE)), max(cmp, na.rm = TRUE) < 1e-9)
}

## and it must still be a cache - a no-op call should reuse, not rebuild
t0 <- Sys.time(); invisible(heatRaster(aoi, ed(5,120), NULL, bin="midday", cache=ca))
twarm <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
t0 <- Sys.time(); invisible(heatRaster(aoi, ed(5,120), NULL, bin="midday"))
tcold <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
ok(sprintf("an unchanged repeat is faster than a cold one (%.2f vs %.2f s)",
           twarm, tcold), twarm < tcold)
## a different area must not be answered out of the old one's cache
poly2 <- st_sfc(st_polygon(list(rbind(c(CX+1200, CY-400), c(CX+2000, CY-400),
                                      c(CX+2000, CY+400), c(CX+1200, CY+400),
                                      c(CX+1200, CY-400)))), crs = 2056)
aoi2 <- st_sf(geometry = st_transform(poly2, 4326))
h2c <- heatRaster(aoi2, bin = "midday", cache = ca)
h2  <- heatRaster(aoi2, bin = "midday")
ok("a change of area rebuilds instead of reusing the old grid",
   !is.null(h2c) && ext(h2c) == ext(h2) &&
     max(abs(values(h2c) - values(h2)), na.rm = TRUE) == 0)

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
