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
## The heat model's inner loops are C++ now (src/heat_cpp.cpp), so sourcing R/
## alone no longer gives a runnable model. Load the WORKING TREE's compiled code,
## not the installed package: the installed one is whatever was last built, and a
## suite that silently tests an older binary than the source beside it is worse
## than no suite. Build it with pkgbuild::compile_dll(".") if this fails.
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
## Coarsened the way heat_landcover() coarsens it for the app - the two-stage
## modal, not a plain one. A plain modal over height ids hands a 5 m cell that is
## entirely forest to whatever single id happens to be commonest, which can be
## open sky; heat_modal_class() votes on the material first and the height only
## if the material agrees. The app never does it the other way.
gr <- heat_modal_class(crop(rast(file.path(LC, "ground_CH_1m.tif")), e), HEAT_RES)
cn <- heat_modal_class(crop(rast(file.path(LC, "canopy_CH_1m.tif")), e), HEAT_RES)

## RAW ids carry the height and are what the geometry terms march over. BASE ids
## are the nine materials heat_materials.csv and heat_decay.csv are keyed on, and
## are what heatRaster() hands to heatLocalTerm() and the advective term - see
## the comment there. Feeding raw ids to those two fails SILENTLY rather than
## loudly, which is why this fixture has to make the same distinction the model
## makes: heatLocalTerm() resolves an unlisted ground class with `others = NA`,
## so every block variant drops out of the finished map (14.31 % of this window),
## and heat_source_mask() matches on the id itself, so a forest split across five
## tree classes falls below min_patch_ha and comes back NULL. Both were invisible
## while every tree was class 7 and every building class 8.
mg <- paintBaseRaster(gr)
mc <- paintBaseRaster(cn)

cat("\n=== 3. the terms, separately ===\n")
sh <- heatShadeRaster(gr, cn, "midday", geo)
lo <- heatLocalTerm(mg, mc, sh, "midday", mat)
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

adv <- heatAdvectiveTerm(mg, mc, dec, res = HEAT_RES)
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
  lo_b <- heatLocalTerm(mg, mc, sh_b, b, mat)
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
## sunlit, open sky, no wall nearby - the assembled output must BE the table
## value once the advective term is taken back off. Anything else means a term
## is leaking.
##
## WHY adv IS SUBTRACTED RATHER THAN REQUIRED TO VANISH. This group used to ask
## for `abs(values(adv)) < 0.02` as well, and in this window that condition is
## met by 1366 cells of 56 000 - essentially none of which are also sunlit, open
## and wall-free. Every check below is conditional on finding five such cells, so
## the group passed for some time while asserting NOTHING AT ALL: measured on the
## flat rasters it found 0 clean cells for all four classes, and on the height
## rasters 195 spread over three of them. That is a silent check, which is worse
## than a failing one, so `n_tab` now counts what was actually tested and fails
## the group if the answer is nothing. Sion is a town centre and no cell in it is
## out of reach of every advective source; the term is verified exactly, against
## terra::focal() and against a windowed rebuild, in group 10.
##
## The SVF floor is 0.98 rather than 0.995 for the same reason - at 0.995 grass
## is the only one of the four materials that clears five cells (12; asphalt gets
## 3, soil and water none). At 0.98 all four do, with 362, 587, 31 and 91 cells,
## and the geometry term there is at most 0.04 K, comfortably inside the 0.05 K
## the comparison allows. That bound is the point: what is left after subtracting
## adv is the local term PLUS a geometry term small enough that the table value
## still has to come through it.
n_tab <- 0
sv <- heatSvfRaster(gr, cn, geom = geo)
for (b in HEAT_BINS) {
  sh_b <- heatShadeRaster(gr, cn, b, geo)
  wl_b <- heatWallRaster(gr, cn, b, geo, sh_b)
  tot  <- heatLocalTerm(mg, mc, sh_b, b, mat) + heatGeometryTerm(sh_b, sv, wl_b, geo) + adv
  clean <- values(sh_b) == 0 & values(sv) > 0.98 & values(wl_b) == 0
  for (k in c(1, 3, 4, 5)) {
    m <- clean & values(mg) == k
    if (sum(m, na.rm = TRUE) < 5) next
    n_tab <- n_tab + 1
    got  <- median((values(tot) - values(adv))[m], na.rm = TRUE)
    want <- mat$pet_0m_K[mat$class_id == k & mat$shaded == 0 & mat$time_bin == b]
    ok(sprintf("%-9s class %d reproduces the table (%+.2f vs %+.2f)", b, k, got, want),
       abs(got - want) < 0.05)
  }
}

ok(sprintf("...and there were clean cells to check it against at all (%d of %d)",
           n_tab, 4 * length(HEAT_BINS)), n_tab == 4 * length(HEAT_BINS))

cat("\n=== 7. unclassified ground ===\n")
na_share <- 100 * mean(is.na(values(h$midday)))
ok(sprintf("almost nothing drops out as unclassified now (%.2f%%)", na_share),
   na_share < 1)

cat("\n=== 8. the cache never changes an answer ===\n")
## WHY THIS IS NO LONGER "== 0". The cache is still exact in the sense that
## matters - a layer is either reused untouched or rebuilt over the whole area,
## and no cell is ever left stale - but the convolution behind it is an FFT now
## (heat_conv(), R/heat_helpers.R), and an FFT's rounding depends on the size of
## the padded grid it runs on. A windowed rebuild pads to a different size than
## a full one, so the two agree to floating point rather than to the bit.
##
## HEAT_EXACT_K is three orders of magnitude above that noise (measured at
## 9.4e-13 K) and seven below the smallest error any of these checks exists to
## catch: the avenue case below asserts that a real staleness shows up as more
## than 0.05 K. So nothing is being waved through - a tolerance this tight
## cannot hide a cell that failed to rebuild.
HEAT_EXACT_K <- 1e-9

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
## The same trees at another height. Since the height bar a class id carries the
## metres, so tree@25 (13) and tree@10 (11) are DIFFERENT IDS over the SAME
## CELLS - which is the one edit that changes the shadow, the horizon and the
## wall band while leaving every material in the design exactly where it was.
##
## Nothing else in this walk can catch a stale HEAT_OBSTRUCTION_IDS. Every other
## step also moves a ground class, so the cache is dirtied by the local term
## whatever the geometry list says; here the ONLY thing that moved is a height.
## If that list is ever written out as c(6L, 7L, 8L) again, `touched` comes back
## {11, 13}, geom_dirty is FALSE, the cached shade/svf/wall are reused with the
## old trees still standing in them, and this is the check that says so.
T13 <- ed(13, 80); T11 <- ed(11, 80)
walk <- list(list("cold midday",           "midday",    NULL, NULL),
             list("ground repaint",        "midday",    G,    NULL),
             list("switch bin",            "afternoon", G,    NULL),
             list("plant trees",           "afternoon", G,    T7),
             list("switch back",           "midday",    G,    T7),
             list("raise them to 25 m",    "midday",    G,    T13),
             list("drop them to 10 m",     "midday",    G,    T11),
             list("erase the trees",       "midday",    G,    NULL),
             list("repaint another class", "midday",    ed(5, 120), NULL))
ca <- heatCacheNew()
for (s in walk) {
  cold <- heatRaster(aoi, s[[3]], s[[4]], bin = s[[2]])
  warm <- heatRaster(aoi, s[[3]], s[[4]], bin = s[[2]], cache = ca)
  d <- abs(values(cold) - values(warm)); d <- d[is.finite(d)]
  ok(sprintf("cached frame is identical after: %s", s[[1]]),
     length(d) > 0 && max(d) < HEAT_EXACT_K, sprintf("[max |diff| %.3g]", max(d)))
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
     length(d) > 0 && max(d) < HEAT_EXACT_K, sprintf("[max |diff| %.3g]", max(d)))
}
## and the stroke really did reach the model, rather than being lost in a window
## that never got re-read - which would also give max |diff| of 0 against a cold
## call only if the cold call were wrong too, so compare the two class fields
w3 <- heatRaster(aoi, stroke(3), NULL, bin = "midday", cache = ca2)
w5 <- heatRaster(aoi, stroke(5), NULL, bin = "midday", cache = ca2)
ok("and repainting that stroke changes the surface",
   max(abs(values(w3) - values(w5)), na.rm = TRUE) > 1)

## A HEIGHT IS NOT A MATERIAL, AND BOTH HALVES OF THAT HAVE TO HOLD.
##
## The exactness walk above proves the cache agrees with itself; it cannot tell
## a working height from a height that reaches nothing at all, because a model
## that ignored the metres entirely would also be perfectly self-consistent. So:
## the same trees at 3 m and at 25 m must give DIFFERENT surfaces (the geometry
## terms read the height), while their local term must be IDENTICAL (the thermal
## tables are keyed on the base material - a crown is a crown).
cat("
")
lo <- heatRaster(aoi, G, ed(10, 80), bin = "morning")   # tree @ 3 m
hi <- heatRaster(aoi, G, ed(13, 80), bin = "morning")   # tree @ 25 m
ok(sprintf("a 25 m tree is not a 3 m tree (max %.2f K)",
           max(abs(values(lo) - values(hi)), na.rm = TRUE)),
   max(abs(values(lo) - values(hi)), na.rm = TRUE) > 0.5)
## ...and taller must mean a LONGER CAST SHADOW, not merely a different surface.
##
## Measured off the canopy, deliberately. A crown shades its own footprint at any
## height, and that footprint is the same 80 m square in both runs, so counting
## every shaded cell drowns the effect being tested: 22.1% against 22.1%, which
## would pass just as happily if the height reached nothing at all. What height
## buys is the shadow OUTSIDE the crown, and that is the only place to look.
.lcLo <- heat_landcover(aoi, G, ed(10, 80), HEAT_RES, NULL)
.lcHi <- heat_landcover(aoi, G, ed(13, 80), HEAT_RES, NULL)
shLo <- heatShadeRaster(.lcLo$ground, .lcLo$canopy, "morning")
shHi <- heatShadeRaster(.lcHi$ground, .lcHi$canopy, "morning")
## Stated as CONTAINMENT rather than as a count, because a count over real town
## land cover is mostly other people's buildings: 12748 against 12697 is the
## right sign but it is 51 cells of signal in 12700 of noise, and it would stay
## green if the height were doing almost nothing. On an open cell the ray only
## has to clear 0, so raising an obstruction can add shade and can never take it
## away - an exact invariant, and one a broken height field breaks immediately.
##
## Open cells only. ON the crown the invariant genuinely does not hold: a cell
## that is itself 25 m of canopy has to clear 25 m to count as shaded where a
## 3 m one had to clear 3, which is the roof-in-the-sun rule and is correct.
.open  <- !is.na(values(.lcHi$canopy)) & values(.lcHi$canopy) == 0
.loSh  <- values(shLo)[.open] == 1
.hiSh  <- values(shHi)[.open] == 1
ok(sprintf("raising a tree never un-shades open ground (%d cells lost)",
           sum(.loSh & !.hiSh)), sum(.loSh & !.hiSh) == 0)
ok(sprintf("...and it shades more of it (%d cells gained)",
           sum(.hiSh & !.loSh)), sum(.hiSh & !.loSh) > 0)

## The thermal half: every step of a ramp must read the same row of
## heat_materials.csv, which is what paintBaseRaster() in heatRaster() is for.
## Without it a variant id misses the table and goes quiet - canopy classes
## resolve with others = 0 ("open sky" under a 25 m crown) and ground classes
## with others = NA, which drops those cells out of the finished map altogether.
lcLo <- heat_landcover(aoi, G, ed(10, 80), HEAT_RES, NULL)
lcHi <- heat_landcover(aoi, G, ed(13, 80), HEAT_RES, NULL)
flat <- setValues(rast(lcLo$ground), 0L)
locLo <- heatLocalTerm(paintBaseRaster(lcLo$ground), paintBaseRaster(lcLo$canopy), flat, "morning")
locHi <- heatLocalTerm(paintBaseRaster(lcHi$ground), paintBaseRaster(lcHi$canopy), flat, "morning")
ok("...but it is made of the same thing (local term identical)",
   max(abs(values(locLo) - values(locHi)), na.rm = TRUE) < HEAT_EXACT_K)
ok("...and no cell of it fell out of the table",
   sum(is.na(values(locHi))) == sum(is.na(values(locLo))) &&
   mean(is.na(values(locHi))) < 0.5,
   sprintf("[%.2f%% NA]", 100 * mean(is.na(values(locHi)))))

## THE INVARIANTS THE PALETTE AND THE MODEL HAVE TO SHARE.
cat("
")
ok("every class maps to a base material in 1:9",
   all(paintBaseId(PAINT_CATEGORIES$id) %in% 1:9))
ok("HEAT_OBSTRUCTION_IDS is exactly the ids that carry a height",
   identical(sort(as.integer(HEAT_OBSTRUCTION_IDS)), sort(as.integer(PAINT_HEIGHT_IDS))))
.pcH <- PAINT_CATEGORIES[!is.na(PAINT_CATEGORIES$height), ]
.csvH <- unname(heatHeights()[as.character(.pcH$id)])
ok("the palette's heights agree with heat_geometry.csv",
   isTRUE(all.equal(.csvH, .pcH$height)))
ok("every material button still has a colour of its own",
   !any(duplicated(PAINT_CATEGORIES$hex[PAINT_CATEGORIES$hex != "transparent"])))

## The two-stage modal. A 5 m cell that is 60% tree but whose commonest single
## id is grass must coarsen to a TREE - the material wins the vote, and the
## height follows only if it agrees. A plain modal over the raw ids gives grass
## here, which is a painted avenue with no crown, no shade and no sky blocking.
.mv <- c(rep(11, 8), rep(13, 7), rep(1, 10))
.mr <- rast(ext(0, 5, 0, 5), resolution = 1, crs = "EPSG:2056")
values(.mr) <- .mv
ok(sprintf("a split-height patch keeps its material (plain modal says %d)",
           as.integer(values(aggregate(.mr, 5, "modal")))),
   paintBaseId(as.integer(values(heat_modal_class(.mr, 5)))) == 7L)
ok("...and an unsplit one keeps its height too",
   { values(.mr) <- rep(13, 25); as.integer(values(heat_modal_class(.mr, 5))) == 13L })

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
   length(d) > 0 && max(d) < HEAT_EXACT_K, sprintf("[max |diff| %.3g]", max(d)))
ok("...and it really did change the surface far from the cut",
   max(abs(values(a_full) - values(cold)), na.rm = TRUE) > 0.05,
   sprintf("[%.2f K]", max(abs(values(a_full) - values(cold)), na.rm = TRUE)))

## the windowed convolution must equal the full one wherever it is defined
dtr <- dec[dec$class_id == 7, , drop = FALSE]
mk  <- heat_source_mask(mg, mc, dtr, HEAT_RES)
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
     max(abs(values(h2c) - values(h2)), na.rm = TRUE) < HEAT_EXACT_K)

cat("\n=== 9. run it the way the app runs it: terra NOT on the search path ===\n")
## The defect this exists for shipped twice and passed every check above:
##   terra::ifel(canopy %in% c(6L, 7L), ...)   and   terra::ifel(pch %in% big, ...)
## terra defines `%in%` as an S4 method on SpatRaster, but the package reaches
## terra only through `terra::` and imports nothing from it - so inside the
## package namespace `%in%` is *base's*, which calls match() on a SpatRaster and
## dies with "'match' requires vector arguments". Every script in data-raw opens
## with library(terra), which puts terra's generic on the search path and hides
## the bug completely. So take it off and run the real functions.
detach("package:terra")
r9 <- try({
  s9 <- heatShadeRaster(gr, cn, "midday")
  m9 <- heat_source_mask(mg, mc, dtr, HEAT_RES)
  h9 <- heatRaster(aoi, bin = "midday")
  list(shade = s9, mask = m9, heat = h9)
}, silent = TRUE)
suppressPackageStartupMessages(library(terra))
ok("the whole heat path runs with terra unattached (the app's condition)",
   !inherits(r9, "try-error"),
   if (inherits(r9, "try-error")) conditionMessage(attr(r9, "condition")) else "")
ok("...and returns a surface, not an empty one",
   !inherits(r9, "try-error") && !is.null(r9$heat) &&
     any(is.finite(values(r9$heat))))
ok("...and the shade raster is still 0/1",
   !inherits(r9, "try-error") && !is.null(r9$shade) &&
     all(stats::na.omit(unique(values(r9$shade))) %in% c(0, 1)))


cat("\n=== 10. the fast paths mean exactly what the slow ones meant ===\n")
## Three inner loops were replaced for speed, taking a cold heatRaster() over
## 3.6 x 2.6 km from 12.6 s to under 3 s. Speed is not the risk; a silent change
## of meaning is. So each one is checked here against the implementation it
## replaced, on the REAL land cover rather than a synthetic grid - the shapes
## that break a connected-component labeller (diagonal touches, patches that
## meet only at a corner, a patch that wraps a row boundary) are exactly the
## shapes a hand-built test raster does not happen to contain.
##
## The originals are written out in full below rather than kept in R/. They are
## not a fallback and must never be called by the model: a fallback that silently
## catches a broken fast path is how a suite comes to pass while the app is
## wrong, which is the failure mode group 9 exists for.

## --- 10a. ccl_big_patches() vs terra::patches() + freq() + subst() ----------
old_source_mask <- function(ground, canopy, row, res) {
  half <- row$half_dist_m[1]; mx <- row$max_extent_m[1]
  amp  <- row$amp_edge_K[1];  mp <- row$min_patch_ha[1]
  if (is.na(half) || is.na(mx) || is.na(amp) || amp == 0) return(NULL)
  cid <- row$class_id[1]
  src <- terra::ifel((ground == cid) | (canopy == cid), 1, NA)
  if (all(is.na(terra::values(src)))) return(NULL)
  pch <- terra::patches(src, directions = 8, zeroAsNA = TRUE)
  fr  <- terra::freq(pch)
  big <- fr$value[fr$count * res^2 >= (if (is.na(mp)) 0 else mp) * 10000]
  if (!length(big)) return(NULL)
  msk <- terra::subst(pch, from = big, to = rep(1, length(big)), others = 0)
  terra::ifel(is.na(msk), 0, msk)
}
n_mask <- 0; bad_mask <- character(0)
for (i in seq_len(nrow(dec))) {
  rw <- dec[i, , drop = FALSE]
  a <- old_source_mask(mg, mc, rw, HEAT_RES)
  b <- heat_source_mask(mg, mc, rw, HEAT_RES)
  agree <- if (is.null(a) && is.null(b)) TRUE
           else if (is.null(a) || is.null(b)) FALSE
           else ext(a) == ext(b) && all(values(a) == values(b))
  n_mask <- n_mask + 1
  if (!isTRUE(agree)) bad_mask <- c(bad_mask, dec$class_name[i])
}
ok(sprintf("patch masks are bit-identical to terra::patches() (%d classes)", n_mask),
   length(bad_mask) == 0, if (length(bad_mask)) paste("differ:", paste(bad_mask, collapse = ", ")) else "")

## the floor itself must still bite, or the check above passes on two masks that
## are identically wrong because nothing was ever excluded
tiny <- dec[dec$class_id == 7, , drop = FALSE]; tiny$min_patch_ha <- 1e6
ok("...and an impossible min_patch_ha still excludes everything",
   is.null(heat_source_mask(mg, mc, tiny, HEAT_RES)))
huge <- dec[dec$class_id == 7, , drop = FALSE]; huge$min_patch_ha <- 0
ok("...and a zero floor keeps strictly more cells than the real one",
   sum(values(heat_source_mask(mg, mc, huge, HEAT_RES))) >
     sum(values(heat_source_mask(mg, mc, dec[dec$class_id == 7, , drop = FALSE], HEAT_RES))))

## a corner-only join is the case 4-connectivity gets wrong and 8-connectivity
## gets right, and it is what `directions = 8` in the original was for
diag_r <- rast(ext(0, 40, 0, 40), resolution = 5, crs = "EPSG:2056")
values(diag_r) <- 0L
diag_r[cells(diag_r, ext(0, 20, 20, 40))]  <- 7L
diag_r[cells(diag_r, ext(20, 40, 0, 20))] <- 7L   # touches the first only at a corner
d8 <- ccl_big_patches(as.integer(values(diag_r, mat = FALSE)),
                      nrow(diag_r), ncol(diag_r), 0)
d_split <- ccl_big_patches(as.integer(values(diag_r, mat = FALSE)),
                           nrow(diag_r), ncol(diag_r),
                           sum(values(diag_r) == 7L) * 0.75)
ok("corner-touching blocks are ONE patch under 8-connectivity",
   sum(d8) == sum(values(diag_r) == 7L) && sum(d_split) == sum(d8))

## --- 10b. svf_horizon() vs the R reference march ---------------------------
Hm <- as.matrix(heatObstructionHeight(gr, cn, geo), wide = TRUE)
Hm[is.na(Hm)] <- 0
sv_cpp <- heat_svf_matrix(Hm, HEAT_RES)
sv_r   <- heat_svf_matrix_r(Hm, HEAT_RES)
ok(sprintf("SVF matches the R reference march (max |diff| %.3g)",
           max(abs(sv_cpp - sv_r))), max(abs(sv_cpp - sv_r)) < 1e-12)
## and the raster path, which skips the matrix and its transpose entirely, must
## land on the same grid the same way round - a transposed SVF over a nearly
## square window still looks like a plausible map and is wrong everywhere
sv_rast <- heatSvfRaster(gr, cn, geom = geo)
ok("...and heatSvfRaster() agrees with it cell for cell, untransposed",
   max(abs(values(sv_rast, mat = FALSE) - as.vector(t(sv_r)))) < 1e-12)

## --- 10c. heat_conv() vs terra::focal() ------------------------------------
n_conv <- 0; worst <- 0
for (i in which(!is.na(dec$half_dist_m) & dec$amp_edge_K != 0)) {
  rw <- dec[i, , drop = FALSE]
  mk0 <- heat_source_mask(mg, mc, rw, HEAT_RES)
  if (is.null(mk0)) next
  fct <- max(1L, as.integer(round(HEAT_ADV_RES / HEAT_RES)))
  mk1 <- if (fct > 1) aggregate(mk0, fct, fun = "mean", na.rm = TRUE) else mk0
  kk  <- heat_decay_kernel(rw$half_dist_m[1], rw$max_extent_m[1],
                           if (fct > 1) HEAT_ADV_RES else HEAT_RES)
  fo <- focal(mk1, w = kk, fun = "sum", na.rm = TRUE, fillvalue = 0)
  ff <- heat_conv(mk1, kk)
  d  <- abs(values(fo, mat = FALSE) - values(ff, mat = FALSE))
  d  <- d[is.finite(d)]
  worst <- max(worst, if (length(d)) max(d) else 0)
  n_conv <- n_conv + 1
}
ok(sprintf("FFT convolution matches terra::focal() (%d classes, max %.3g K)",
           n_conv, worst), n_conv > 0 && worst < 1e-9)

## The kernel being radially symmetric is what makes correlation and convolution
## the same operation here. If that ever stops being true, heat_conv() silently
## returns a field flipped through the origin - so assert the property the proof
## rests on rather than trusting the comment.
ksym <- heat_decay_kernel(60, 200, HEAT_ADV_RES)
ok("the decay kernel is symmetric under a 180 degree flip",
   max(abs(ksym - ksym[nrow(ksym):1, ncol(ksym):1])) == 0)

## --- 10d. the whole surface, end to end ------------------------------------
## Everything above compares a part against the implementation it replaced.
## This compares the whole answer against a stored surface, which is the only
## check here that still bites once the old implementations are gone - and they
## will be, because nobody keeps a reference copy of terra::patches() forever.
##
## The stored file was first written from the version measured equal to the
## pre-C++ model over both AOI sizes and all three bins (max |diff| 9.4e-13 K),
## so it does carry the old model's values - but it is a REGRESSION baseline,
## not a proof of equivalence, and it is only as good as the run that wrote it.
## Regenerate with VFT_HEAT_BASELINE=1 when a table or a coefficient
## legitimately moves, and say so in the commit that does it.
bl <- file.path(dirname(R), "data-raw", "heat_baseline_sion_midday.rds")
h10 <- heatRaster(aoi, bin = "midday")
if (nzchar(Sys.getenv("VFT_HEAT_BASELINE"))) {
  saveRDS(list(ext = as.vector(ext(h10)), dim = dim(h10)[1:2],
               v = values(h10, mat = FALSE)), bl)
  cat("  baseline written to", bl, "\n")
}
if (file.exists(bl)) {
  b10 <- readRDS(bl)
  same_grid <- identical(as.integer(b10$dim), as.integer(dim(h10)[1:2])) &&
    isTRUE(all.equal(b10$ext, as.vector(ext(h10))))
  dd <- if (same_grid) abs(values(h10, mat = FALSE) - b10$v) else NA_real_
  dd <- dd[is.finite(dd)]
  ok(sprintf("the assembled surface still matches the stored baseline (max %.3g K)",
             if (length(dd)) max(dd) else NA_real_),
     same_grid && length(dd) > 0 && max(dd) < 1e-6)
} else {
  cat("  no stored baseline yet - run once with VFT_HEAT_BASELINE=1 to create it\n")
}

cat("\n=== 11. the progress bar says where the model is, and changes nothing ===\n")
## The bar is driven from inside heatRaster() (heat_ticker(), one mark per term),
## which puts a display feature in the middle of the model. Two things have to
## hold: the marks are the pipeline in order, and the model cannot tell whether
## anyone is watching. The second is the one worth a check - a $set() that throws
## is not hypothetical, it is what a closed session leaves behind while the
## daemon is still working.
##
## The marks themselves are spaced by the measured cost of each term; the table
## behind them is in the comment at the top of heatRaster(). To re-measure, run
## this file's aoi through heatRaster() with a handle that records Sys.time()
## instead of a value, and read the gaps.
anchors <- c(0.02, 0.42, 0.50, 0.53, 0.60, 0.64, 0.66, 0.99)
rec <- function() {
  e <- new.env(); e$v <- numeric(0)
  list(handle = list(set = function(value = NULL, message = NULL, detail = NULL) {
         e$v <- c(e$v, value); invisible(NULL) }),
       seen = function() e$v)
}
r11 <- rec()
h11 <- heatRaster(aoi, bin = "midday", progress = r11$handle)
ok(sprintf("a cold run reports every mark, in order (%d of %d)",
           length(r11$seen()), length(anchors)),
   isTRUE(all.equal(r11$seen(), anchors)))
ok("no mark is outside (0, 1]",
   length(r11$seen()) > 0 && all(r11$seen() > 0) && all(r11$seen() <= 1))

## A warm run skips terms but not marks: the ticks sit outside the cache
## lookups, so the bar still walks the whole pipeline - it just gets there
## faster. If this ever fails, a tick has been moved inside a heat_cached().
ca11 <- heatCacheNew()
invisible(heatRaster(aoi, bin = "midday", cache = ca11))
r11b <- rec()
invisible(heatRaster(aoi, bin = "midday", cache = ca11, progress = r11b$handle))
ok("a warm run reports the same marks as a cold one",
   isTRUE(all.equal(r11b$seen(), anchors)))

## and the model is the model whether or not a bar is attached
h11p <- heatRaster(aoi, bin = "midday",
                   progress = list(set = function(...) stop("session has gone")))
d11 <- abs(values(h11p, mat = FALSE) - values(h11, mat = FALSE))
d11 <- d11[is.finite(d11)]
ok("a progress handle that throws costs the bar and not the surface",
   length(d11) > 0 && max(d11) == 0)
h11n <- heatRaster(aoi, bin = "midday", progress = NULL)
d11n <- abs(values(h11n, mat = FALSE) - values(h11, mat = FALSE))
d11n <- d11n[is.finite(d11n)]
ok("the surface is identical with and without a bar",
   length(d11n) > 0 && max(d11n) == 0)

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
