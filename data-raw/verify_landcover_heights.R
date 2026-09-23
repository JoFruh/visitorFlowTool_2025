## Verification for the height classes in the national land cover
## (data-raw/generate_ground_canopy_CH.r). Run:
##   Rscript data-raw/verify_landcover_heights.R
##
## Everything here is synthetic and runs in seconds. That is the point: the
## functions it checks decide what 82.8 G cells of Switzerland say, and the
## cheapest place to find out that a cut is on the wrong side of a boundary is
## before the 4500-tile run rather than after it.
##
## What is NOT here, because it needs the real sources: that a retrofitted tile
## equals a rebuilt one. That lives in qa_height_retrofit_vs_rebuild() in the
## generator's driver block, and it is the check to run before a national pass.
suppressPackageStartupMessages({library(terra); library(sf)})
terraOptions(progress = 0)

R  <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
GEN <- file.path(dirname(R), "data-raw", "generate_ground_canopy_CH.r")
source(file.path(R, "paintbrush_helpers.R"))
suppressMessages(source(GEN))

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-64s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

## --- 1. the ramps agree with the palette ------------------------------------
cat("=== 1. the step tables restate PAINT_CATEGORIES ===\n")
ok("lc_check_steps() accepts the current palette",
   isTRUE(try(lc_check_steps(file.path(R, "paintbrush_helpers.R")), silent = TRUE)))

for(nm in c("LC_TREE_STEPS", "LC_BLOCK_STEPS")){
  st <- get(nm)
  i  <- match(st$id, PAINT_CATEGORIES$id)
  ok(sprintf("%s heights equal the palette's", nm),
     !anyNA(i) && identical(as.numeric(PAINT_CATEGORIES$height[i]), as.numeric(st$height)))
  ok(sprintf("%s is sorted by height", nm), !is.unsorted(st$height))
}
## every id the two tables write must be a real class, and every obstruction
## class the palette carries for these two materials must be writable
wrote <- c(LC_TREE_STEPS$id, LC_BLOCK_STEPS$id)
want  <- PAINT_CATEGORIES$id[PAINT_CATEGORIES$base %in% c(7L, 8L) &
                             !is.na(PAINT_CATEGORIES$height)]
ok("the tables write exactly the tree and block ramps",
   setequal(wrote, want), sprintf("[%s]", paste(sort(wrote), collapse = " ")))
ok("every written id is a class the paint palette knows",
   all(wrote %in% PAINT_CATEGORIES$id))
ok("15 m block (19) is in the block ramp", 19L %in% LC_BLOCK_STEPS$id)

## --- 2. nearest step, and which side of a cut ------------------------------
cat("\n=== 2. lc_nearest_step ===\n")
cuts <- lc_step_cuts(LC_TREE_STEPS)
ok("tree cuts are the midpoints", identical(cuts, c(6.5, 12.5, 17.5, 22.5)),
   sprintf("[%s]", paste(cuts, collapse = " ")))
ok("block cuts are the midpoints",
   identical(lc_step_cuts(LC_BLOCK_STEPS), c(7.5, 12.5, 20, 37.5)))

h   <- c(3, 6.49, 6.5, 6.51, 12.5, 15, 17.4, 22.5, 25, 60)
got <- lc_nearest_step(h, LC_TREE_STEPS)
want <- c(10L, 10L, 11L, 11L, 7L, 7L, 7L, 13L, 13L, 13L)
ok("tree heights land on the nearest step", identical(got, want),
   sprintf("[%s]", paste(got, collapse = " ")))
ok("a height exactly on a cut rounds UP", lc_nearest_step(6.5, LC_TREE_STEPS) == 11L)
ok("NA becomes na_id and nothing else",
   identical(lc_nearest_step(c(NA, 20), LC_TREE_STEPS, na_id = 8L), c(8L, 12L)))
ok("an empty vector gives an empty answer",
   length(lc_nearest_step(numeric(0), LC_TREE_STEPS)) == 0)

## The property that matters more than any single case: the error is bounded by
## half the local gap, and it is unbiased rather than one-sided. Half the gap is
## 2.5 m over most of the tree ramp but 3.5 m across the wide 3-10 m band, and
## the block ramp is coarser still at the top on purpose - so these numbers are
## the contract, not a target to tighten.
step_h <- function(h, st) st$height[match(lc_nearest_step(h, st), st$id)]
hh  <- seq(3, 25, by = 0.01)
sg  <- step_h(hh, LC_TREE_STEPS) - hh
ok(sprintf("no tree between 3 and 25 m is misrepresented by more than 3.5 m (%.2f m)",
           max(abs(sg))), max(abs(sg)) <= 3.5 + 1e-9)
ok(sprintf("and by no more than 2.5 m once above the wide first gap (%.2f m)",
           max(abs(sg[hh >= 10]))), max(abs(sg[hh >= 10])) <= 2.5 + 1e-9)
ok(sprintf("the rounding is unbiased, not one-sided (mean %+.2f m)", mean(sg)),
   abs(mean(sg)) < 0.25)
bb <- seq(2, 50, by = 0.01)
sb <- step_h(bb, LC_BLOCK_STEPS) - bb
ok(sprintf("no building up to 50 m is misrepresented by more than 12.5 m (%.2f m)",
           max(abs(sb))), max(abs(sb)) <= 12.5 + 1e-9)
## 3 m and not 2.5 m at the bottom, and the extra half-metre is the ramp's floor
## rather than a cut: the lowest block step is 5 m, so a 2 m shed is the worst
## case in this band and it is rounded UP to 5 m rather than left out.
ok(sprintf("and by no more than 3 m below 12.5 m, where most of them are (%.2f m)",
           max(abs(sb[bb <= 12.5]))), max(abs(sb[bb <= 12.5])) <= 3 + 1e-9)
## lower-edge binning is what this is NOT; assert the difference is real
lower <- LC_TREE_STEPS$height[findInterval(hh, LC_TREE_STEPS$height)]
ok(sprintf("lower-edge binning would have under-shaded by %.2f m on average",
           mean(hh - lower)), mean(hh - lower) > 2 && mean(abs(sg)) < mean(hh - lower))

## --- 3. the canopy raster ---------------------------------------------------
cat("\n=== 3. lc_canopy_class ===\n")
v <- rast(ext(0, 10, 0, 1), resolution = 1, crs = "EPSG:2056")
values(v) <- c(-1, 0, 2.99, 3, 6.5, 12.4, 15, 21, 24, 48)
cl <- as.integer(values(lc_canopy_class(v))[, 1])
ok("below LC_CANOPY_MIN is open sky, including the -1 sentinel",
   identical(cl[1:3], c(0L, 0L, 0L)), sprintf("[%s]", paste(cl[1:3], collapse = " ")))
## 12.4 is BELOW the 12.5 cut, so it is a 10 m tree and not a 15 m one - the
## kind of off-by-one-side that is invisible in a raster and obvious here
ok("crowns take their nearest step",
   identical(cl[4:10], c(10L, 11L, 11L, 7L, 12L, 13L, 13L)),
   sprintf("[%s]", paste(cl[4:10], collapse = " ")))
ok("every value produced is a tree class or 0",
   all(cl %in% c(0L, LC_TREE_STEPS$id)))

values(v) <- rep(NA_real_, 10)
ok("an all-NA height field produces no canopy at all",
   all(as.integer(values(lc_canopy_class(v))[, 1]) %in% c(0L, NA_integer_)))

## the quiet failure this replaced: the old seed was ifel(vhm >= 3, 7, 0), so a
## build that still used it would put every crown on the ramp's 15 m default
values(v) <- c(-1, 0, 2.99, 3, 6.5, 12.4, 15, 21, 24, 48)
ok("a 3 m crown and a 25 m crown are no longer the same class",
   lc_nearest_step(3, LC_TREE_STEPS) != lc_nearest_step(25, LC_TREE_STEPS))

## --- 4. building heights, per cell ---------------------------------------
cat("\n=== 4. lc_block_class_tile and lc_apply_block_heights ===\n")

## a two-cell-wide tile is enough; the grid only has to be one lc_template()
tile <- data.frame(tile_id = "test_h", xmin = 2600000, xmax = 2600010,
                   ymin = 1200000, ymax = 1200010, stringsAsFactors = FALSE)
tmpl <- lc_template(tile, NA)

sq <- function(x0, y0, w = 4) st_polygon(list(cbind(
  c(x0, x0 + w, x0 + w, x0, x0), c(y0, y0, y0 + w, y0 + w, y0))))
b3d <- st_sf(GESAMTHOEHE = c(4.0, 13.6, 31.5, 125.9),
             geometry = st_sfc(sq(2600000, 1200000), sq(2600005, 1200000),
                               sq(2600000, 1200005), sq(2600005, 1200005),
                               crs = 2056))
gpkg <- file.path(tempdir(), "vlh_b3d.gpkg")
unlink(gpkg)
suppressWarnings(st_write(b3d, gpkg, layer = "buildings3d", quiet = TRUE))

bc <- lc_block_class_tile(tile, tmpl, gpkg)
ok("a tile with 3D buildings returns a class raster", !is.null(bc))
vals <- sort(unique(stats::na.omit(as.integer(values(bc)[, 1]))))
ok("a 4 m shed, a 13.6 m house, a 31.5 m Hochhaus and a 125.9 m tower",
   identical(vals, sort(c(16L, 19L, 17L, 18L))),
   sprintf("[%s]", paste(vals, collapse = " ")))
ok("the tallest building in the country still lands on the top step, not NA",
   18L %in% vals)
ok("cells no 3D building covers are NA, not a class",
   any(is.na(values(bc)[, 1])))
ok("no gpkg at all returns NULL rather than failing",
   is.null(lc_block_class_tile(tile, tmpl, file.path(tempdir(), "nope.gpkg"))))

## the cuts, read off the ramp rather than restated
bb2 <- rast(ext(0, 8, 0, 1), resolution = 1, crs = "EPSG:2056")
values(bb2) <- c(2, 7.49, 7.5, 12.49, 12.5, 19.9, 20, 400)
bcl <- as.integer(values(lc_step_raster(bb2, LC_BLOCK_STEPS))[, 1])
ok("block heights land on the nearest step, cuts at 7.5/12.5/20/37.5",
   identical(bcl, c(16L, 16L, 8L, 8L, 19L, 19L, 17L, 18L)),
   sprintf("[%s]", paste(bcl, collapse = " ")))
values(bb2) <- rep(NA_real_, 8)
ok("an unmeasured building stays NA and so keeps its default",
   all(is.na(values(lc_step_raster(bb2, LC_BLOCK_STEPS))[, 1])))

## and the application: only class 8 may move, and only where a height exists
gr <- lc_template(tile, 0); cn <- lc_template(tile, 0)
values(gr) <- rep(c(8L, 1L), each = 50)
values(cn) <- rep(c(8L, 7L), each = 50)
res <- lc_apply_block_heights(gr, cn, tile, gpkg)
vg <- as.integer(values(res$ground)[, 1]); vc <- as.integer(values(res$canopy)[, 1])
og <- as.integer(values(gr)[, 1]);          oc <- as.integer(values(cn)[, 1])
ok("nothing that was not a block moved, in either raster",
   all(vg[og != 8L] == og[og != 8L]) && all(vc[oc != 8L] == oc[oc != 8L]))
ok("block cells took a block class", all(vg[og == 8L] %in% LC_BLOCK_STEPS$id))
ok("...and the canopy took the same one as the ground",
   identical(vg[og == 8L & oc == 8L], vc[og == 8L & oc == 8L]))
ok("a block cell the 3D model does not cover keeps the default 8",
   all(vg[og == 8L & is.na(values(bc)[, 1])] == 8L))
ok("no gpkg leaves both rasters untouched",
   { z <- lc_apply_block_heights(gr, cn, tile, file.path(tempdir(), "nope.gpkg"))
     identical(values(z$ground), values(gr)) && identical(values(z$canopy), values(cn)) })

## --- 4b. the legacy era: uuid join + terrain subtraction -------------------
cat("\n=== 4b. lc_footprint_z and lc_block_class_legacy ===\n")

sq3 <- function(x0, y0, z, w = 4) st_polygon(list(cbind(
  c(x0, x0 + w, x0 + w, x0, x0), c(y0, y0, y0 + w, y0 + w, y0),
  c(z, z + 0.4, z, z + 0.2, z))))
fp <- st_sf(uuid = c("{A}", "{B}", "{C}", "{D}", "{E}"),
            objektart = "Gebaeude",
            geometry = st_sfc(sq3(2600000, 1200000, 400), sq3(2600005, 1200000, 400),
                              sq3(2600000, 1200005, 400), sq3(2600005, 1200005, 400),
                              sq3(2600010, 1200000, 900), crs = 2056))
dm <- data.frame(uuid = c("{A}", "{B}", "{C}", "{E}"),
                 dach_max = c(400 + 13.6,   # ordinary house  -> 15 m
                              400 + 31.5,   # Hochhaus        -> 25 m
                              400 +  4.0,   # Flugdach        ->  5 m
                              452.0),       # 900 m terrain   -> -448 m, absurd
                 stringsAsFactors = FALSE)

z <- lc_footprint_z(fp)
ok("footprint Z is the median of the outline", isTRUE(all.equal(z[1], 400)),
   sprintf("[%s]", paste(round(z, 1), collapse = " ")))
ok("one Z per footprint, in order", length(z) == nrow(fp))

cls <- lc_block_class_legacy(fp, dm)
ok("a 13.6 m house takes the new 15 m step", identical(cls[1], 19L), sprintf("[%s]", cls[1]))
ok("a 31.5 m Hochhaus takes the 25 m step",  identical(cls[2], 17L), sprintf("[%s]", cls[2]))
ok("a 4 m Flugdach takes the 5 m step",      identical(cls[3], 16L), sprintf("[%s]", cls[3]))
ok("a footprint the 3D model does not carry gets no class", is.na(cls[4]))
ok("an absurd height gets no class rather than a wrong one", is.na(cls[5]),
   sprintf("[%s]", cls[5]))
ok("no roof elevations at all yields no classes",
   all(is.na(lc_block_class_legacy(fp, NULL))) ||
     length(lc_block_class_legacy(fp, NULL)) == 0)
ok("no footprints yields an empty answer",
   length(lc_block_class_legacy(fp[0, ], dm)) == 0)

## THE REGRESSION. lc_read_tile() flattens geometries by default, and a caller
## that forgets drop_z = FALSE reaches here with a valid uuid join and no
## terrain: every height comes out NA and the Valais silently gets no building
## heights at all. It must warn rather than return quietly.
flat <- st_zm(fp, drop = TRUE, what = "ZM")
w <- NULL
res <- withCallingHandlers(lc_block_class_legacy(flat, dm),
                           warning = function(e) { w <<- conditionMessage(e)
                                                   invokeRestart("muffleWarning") })
ok("flattened footprints warn instead of quietly yielding nothing",
   !is.null(w) && grepl("drop_z", w))
ok("...and still return an answer of the right length, all NA",
   length(res) == nrow(flat) && all(is.na(res)))

## both ends of the guard
d2 <- data.frame(uuid = "{A}", dach_max = 400 + LC_BUILDING_H_RANGE[1] - 0.01,
                 stringsAsFactors = FALSE)
ok("just under the low guard is refused", is.na(lc_block_class_legacy(fp[1, ], d2)))
d2$dach_max <- 400 + LC_BUILDING_H_RANGE[2] + 0.01
ok("just over the high guard is refused", is.na(lc_block_class_legacy(fp[1, ], d2)))
d2$dach_max <- 400 + LC_BUILDING_H_RANGE[2]
ok("exactly on the high guard is accepted",
   identical(lc_block_class_legacy(fp[1, ], d2), 18L))

## and the precedence: a re-surveyed cell must not be overwritten by the legacy
## route, because the whole point of the split is that the newer survey wins
gr <- lc_template(tile, 0); cn2 <- lc_template(tile, 0)
values(gr) <- 8L; values(cn2) <- 8L
fp2 <- st_sf(uuid = "{A}", objektart = "Gebaeude",
             geometry = st_sfc(sq3(2600000, 1200000, 400, w = 10), crs = 2056))
dm2 <- data.frame(uuid = "{A}", dach_max = 400 + 45, stringsAsFactors = FALSE)
both <- lc_apply_block_heights(gr, cn2, tile, gpkg, dm = dm2, bld = fp2)
vb <- as.integer(values(both$ground)[, 1])
covered <- !is.na(values(bc)[, 1])
ok("where both eras answer, the re-surveyed one wins",
   all(vb[covered] == as.integer(values(bc)[, 1])[covered]))
ok("...and where only the legacy one does, it is used",
   all(vb[!covered] == 18L), sprintf("[%s]", paste(unique(vb[!covered]), collapse = " ")))

## --- 5. coarsening must not lose the material ------------------------------
cat("\n=== 5. lc_base_raster and the two-stage modal ===\n")
r <- rast(ext(0, 5, 0, 5), resolution = 1, crs = "EPSG:2056")
## 8 x tree@10 m (11), 7 x tree@25 m (13), 10 x grass (1): 60 % tree, but the
## commonest single id is grass
values(r) <- c(rep(11L, 8), rep(13L, 7), rep(1L, 10))
one <- as.integer(values(aggregate(r, 5, fun = "modal"))[, 1])
two <- as.integer(values(lc_modal_class(r, 5))[, 1])
ok("a one-stage modal loses a split-height crown to the grass", one == 1L,
   sprintf("[%d]", one))
ok("the two-stage modal keeps it a tree", two %in% LC_TREE_STEPS$id, sprintf("[%d]", two))
ok("and falls back to the ramp default when the heights disagree", two == 7L)

values(r) <- c(rep(13L, 15), rep(1L, 10))
ok("an unsplit patch keeps its HEIGHT, not just its material",
   as.integer(values(lc_modal_class(r, 5))[, 1]) == 13L)

values(r) <- c(rep(19L, 8), rep(17L, 7), rep(1L, 10))
ok("the same holds for buildings", as.integer(values(lc_modal_class(r, 5))[, 1]) == 8L)

values(r) <- rep(c(1L, 2L, 3L, 4L, 5L), 5)
ok("a raster with no height variants is unchanged by lc_base_raster()",
   identical(as.integer(values(lc_base_raster(r))[, 1]), as.integer(values(r)[, 1])))
ok("fact = 1 is the identity", identical(values(lc_modal_class(r, 1)), values(r)))

## lc_base_raster() restates paintBaseRaster() for a file that cannot load the
## package. Two implementations of one mapping drift; this is what notices.
all_ids <- c(0L, PAINT_CATEGORIES$id)
r2 <- rast(ext(0, length(all_ids), 0, 1), resolution = 1, crs = "EPSG:2056")
values(r2) <- all_ids
a <- as.integer(values(lc_base_raster(r2))[, 1])
b <- as.integer(values(paintBaseRaster(r2))[, 1])
## the generator only ever writes tree and block classes, so it maps only those;
## everywhere the package maps something, the two must agree
same <- a == b
ok("lc_base_raster() agrees with paintBaseRaster() on every id it maps",
   all(same[all_ids %in% c(0L, LC_TREE_STEPS$id, LC_BLOCK_STEPS$id,
                           PAINT_CATEGORIES$id[PAINT_CATEGORIES$id ==
                                               PAINT_CATEGORIES$base])]),
   sprintf("[%d of %d ids identical]", sum(same), length(all_ids)))
ok("...and maps every id the national rasters can contain",
   all(a[all_ids %in% c(LC_TREE_STEPS$id, LC_BLOCK_STEPS$id)] %in% c(7L, 8L)))

## --- 6. the ids the country may contain -------------------------------------
cat("\n=== 6. legal ids ===\n")
ground_ids <- c(0L, 1L, 2L, 3L, 4L, 5L, LC_BLOCK_STEPS$id)
canopy_ids <- c(0L, 6L, LC_TREE_STEPS$id, LC_BLOCK_STEPS$id)
ok("every id the ground build can write is in the palette",
   all(setdiff(ground_ids, 0L) %in% PAINT_CATEGORIES$id))
ok("every id the canopy build can write is in the palette",
   all(setdiff(canopy_ids, 0L) %in% PAINT_CATEGORIES$id))
ok("every canopy class that carries a height is an obstruction to the heat model",
   all(intersect(canopy_ids, PAINT_HEIGHT_IDS) %in% HEAT_OBSTRUCTION_IDS))
ok("ids fit in the INT1U the tiles are written as", max(c(ground_ids, canopy_ids)) <= 255)
ok("paintBaseId() maps every written id onto one of the frozen nine",
   all(paintBaseId(unique(c(ground_ids, canopy_ids))) %in% 0:9))

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
