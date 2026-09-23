## Verification for the Phase 2 cast shadows (R/shadow_helpers.R).
## The caster is only trustworthy if it reproduces the shadow lengths
## heat_geometry.csv already states, and puts them on the correct side of the
## obstruction. Run:  Rscript data-raw/verify_shadows.R
##
## Checks 1-4 are self-contained. Check 5 needs the national land cover and is
## skipped when it is not on this machine.
suppressPackageStartupMessages({library(terra)})
terraOptions(progress = 0)
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
source(file.path(R, "data_paths.R"))
source(file.path(R, "paintbrush_helpers.R"))   ## PAINT_CATEGORIES: heatHeights() is keyed off it
source(file.path(R, "shadow_helpers.R"))
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

## heat_helpers.R is here for ONE function: heat_modal_class(), the two-stage
## modal the app coarsens the land cover with. Nothing below runs the heat model
## - this file stays what it was - but a fixture built with a plain modal is not
## the raster the app marches over, and over height ids the two genuinely
## differ: a 5 m cell that is entirely forest can come out as open sky simply
## because the crowns in it are two different heights.
source(file.path(R, "heat_helpers.R"))

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-58s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

g <- heatGeometry()
ok("geometry table loads", !is.null(g) && !is.na(g[["sun_elevation_midday"]]))
cat("\n")

## --- 1. shadow length vs the table's own derived figures --------------------
## One 15 m obstruction alone on a fine grid; measure how far the shade reaches.
RES <- 1
mk <- function(h, n = 201) {
  r <- rast(ext(0, n * RES, 0, n * RES), resolution = RES, crs = "EPSG:2056")
  values(r) <- 0
  r[n %/% 2 + 1, n %/% 2 + 1] <- h
  r
}
## Every step of every ramp, not just the three defaults. This is the direct
## test that the height bar does what it says: a 25 m tree has to reach 24.3 m in
## the morning and a 3 m one 2.9 m, and both numbers come from the same table the
## march is supposed to reproduce.
##
## The grid has to be big enough for the longest shadow in the table - a 50 m
## block reaches 48.6 m - or the reach is clipped by the window and every tall
## class fails for a reason that has nothing to do with the caster.
HCLASSES <- PAINT_CATEGORIES[!is.na(PAINT_CATEGORIES$height), c("id", "name", "height")]
NGRID <- 2 * ceiling(max(HCLASSES$height) / tan(heatSunPosition("morning")$elevation * pi / 180) / RES) + 21
for (bin in HEAT_BINS) {
  s <- heatSunPosition(bin)
  for (k in seq_len(nrow(HCLASSES))) {
    cls <- HCLASSES$id[k]
    h <- unname(heatHeights()[[as.character(cls)]])
    H <- mk(h, n = NGRID)
    m <- as.matrix(H, wide = TRUE)
    sh <- heat_shadow_march(m, RES, s$elevation, s$azimuth)
    idx <- which(sh, arr.ind = TRUE)
    ctr <- nrow(m) %/% 2 + 1
    reach <- if (nrow(idx)) max(sqrt((idx[, 1] - ctr)^2 + (idx[, 2] - ctr)^2)) * RES else 0
    want <- unname(g[[sprintf("shadow_length_%s_%s", HCLASSES$name[k], bin)]])
    ok(sprintf("%s / %s (%g m) reach %.1f m vs table %.1f m",
               bin, HCLASSES$name[k], h, reach, want),
       abs(reach - want) <= 1.5)
  }
}

## --- 2. direction: the shadow falls away from the sun -----------------------
cat("\n")
H <- mk(15)
m <- as.matrix(H, wide = TRUE); ctr <- nrow(m) %/% 2 + 1
for (bin in HEAT_BINS) {
  s <- heatSunPosition(bin)
  sh <- heat_shadow_march(m, RES, s$elevation, s$azimuth)
  idx <- which(sh, arr.ind = TRUE)
  ## mean offset of the shaded cells, in map coordinates
  dE <- mean(idx[, 2] - ctr) * RES
  dN <- mean(ctr - idx[, 1]) * RES
  ## the direction the shadow should point: opposite the sun's azimuth
  wantE <- -sin(s$azimuth * pi / 180); wantN <- -cos(s$azimuth * pi / 180)
  cosang <- (dE * wantE + dN * wantN) / sqrt(dE^2 + dN^2)
  ok(sprintf("%s shadow points away from the sun (cos %.3f)", bin, cosang),
     cosang > 0.97, sprintf("[dE %+.1f dN %+.1f]", dE, dN))
}

## --- 3. midday shadow is shorter than morning and afternoon -----------------
cat("\n")
reach <- sapply(HEAT_BINS, function(b) {
  s <- heatSunPosition(b)
  sh <- heat_shadow_march(m, RES, s$elevation, s$azimuth)
  idx <- which(sh, arr.ind = TRUE)
  max(sqrt((idx[, 1] - ctr)^2 + (idx[, 2] - ctr)^2)) * RES
})
ok(sprintf("midday shadow shortest (%.1f vs %.1f / %.1f m)",
           reach[["midday"]], reach[["morning"]], reach[["afternoon"]]),
   reach[["midday"]] < reach[["morning"]] && reach[["midday"]] < reach[["afternoon"]])
ok(sprintf("morning and afternoon reach match (%.1f / %.1f m)",
           reach[["morning"]], reach[["afternoon"]]),
   abs(reach[["morning"]] - reach[["afternoon"]]) <= 1.01)

## --- 4. degenerate inputs ---------------------------------------------------
cat("\n")
flat <- matrix(0, 50, 50)
ok("flat ground casts no shadow", !any(heat_shadow_march(flat, 5, 45, 180)))
ok("sun below the horizon shades everything", all(heat_shadow_march(mk(15) |>
     as.matrix(wide = TRUE), 5, -5, 180)))

## --- 5. on the real rasters, at 5 m ----------------------------------------
cat("\n")
LC <- Sys.getenv("VFT_LANDCOVER_DIR", "C:/Users/frueh/Documents/Local Data/landcover")
if(!file.exists(file.path(LC, "ground_CH_1m.tif"))){
  cat("\nnational land cover not present - skipping the on-raster checks\n")
  cat(sprintf("\n%d check(s) failed\n", fails))
  quit(status = if (fails == 0) 0 else 1)
}
CX <- 2593956; CY <- 1119554
e <- ext(CX - 600, CX + 600, CY - 600, CY + 600)
## heat_modal_class(), not a plain modal: the app coarsens this way, and over
## height ids a plain modal can turn a 5 m cell that is entirely forest into open
## sky merely because the crowns in it are two different heights.
gr <- heat_modal_class(crop(rast(file.path(LC, "ground_CH_1m.tif")), e), 5)
cn <- heat_modal_class(crop(rast(file.path(LC, "canopy_CH_1m.tif")), e), 5)
Hh <- heatObstructionHeight(gr, cn)
ok("height field has no NA", !any(is.na(values(Hh))))
cat("   height values present:", paste(sort(unique(values(Hh))), collapse = ", "), "\n")
for (bin in HEAT_BINS) {
  t0 <- Sys.time()
  sd <- heatShadeRaster(gr, cn, bin)
  el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  own <- mean(values(cn) %in% c(6, 7))
  cat(sprintf("   %-9s shaded %.1f%% (own canopy alone %.1f%%)  %.2fs\n",
              bin, 100 * mean(values(sd)), 100 * own, el))
}
sd_mid <- heatShadeRaster(gr, cn, "midday")
own <- ifel(cn %in% c(6, 7), 1, 0)
ok("cast shade is a superset of own-canopy shade",
   all(values(sd_mid)[values(own) == 1] == 1))
ok("cast shade exceeds own-canopy shade", mean(values(sd_mid)) > mean(values(own)))

## --- 6. orientation ---------------------------------------------------------
## The two checks above are both satisfied by a TRANSPOSED shade raster, because
## heatShadeRaster() ORs the march's output with the own-canopy mask - which is
## built from the raster and is therefore always the right way round. That is
## exactly the bug this group exists to catch: terra fills row-major, as.vector()
## on a matrix is column-major, and on a square window the difference looks like
## a perfectly plausible map of shadows.
##
## The window is deliberately NON-square, so a transpose cannot even be assembled,
## and the test is that shade must TOUCH the thing that casts it.
cat("\n")
e2 <- ext(CX - 500, CX + 500, CY - 300, CY + 300)     # 1000 x 600 m
g2 <- heat_modal_class(crop(rast(file.path(LC, "ground_CH_1m.tif")), e2), 5)
c2 <- heat_modal_class(crop(rast(file.path(LC, "canopy_CH_1m.tif")), e2), 5)
ok("non-square window really is non-square", nrow(g2) != ncol(g2),
   sprintf("[%d x %d]", nrow(g2), ncol(g2)))
s2 <- heatShadeRaster(g2, c2, "midday")
ok("shade raster returned on a non-square window", !is.null(s2))

if (!is.null(s2)) {
  H2 <- heatObstructionHeight(g2, c2)
  ## every shaded cell that is not under a canopy of its own must lie within one
  ## maximum shadow length of some obstruction. A transposed raster scatters
  ## shade into open country and fails this badly.
  obstruction <- ifel(H2 > 0, 1, NA)
  d <- distance(obstruction)
  maxlen <- max(values(H2), na.rm = TRUE) / tan(heatSunPosition("midday")$elevation * pi/180)
  own2 <- ifel(c2 %in% c(6, 7), 1, 0)
  strayed <- values(s2) == 1 & values(own2) == 0 & values(d) > (maxlen + 2 * res(s2)[1])
  ok(sprintf("no shade further than %.1f m from any obstruction (%d stray cells)",
             maxlen, sum(strayed, na.rm = TRUE)),
     sum(strayed, na.rm = TRUE) == 0)

  ## and the converse: the mean distance-to-obstruction of shaded cells must be
  ## much smaller than that of sunlit ones. Under a transpose the two converge.
  dv <- values(d); sv <- values(s2)
  m_sh <- mean(dv[sv == 1], na.rm = TRUE); m_su <- mean(dv[sv == 0], na.rm = TRUE)
  ok(sprintf("shaded cells sit nearer obstructions than sunlit ones (%.1f vs %.1f m)",
             m_sh, m_su), m_sh < m_su)
}

## --- 7. an obstruction's own roof is in the sun -----------------------------
## The march must clear the cell's OWN height, not 0. Compared against 0, a cell
## that is itself a building is shaded by any neighbour of equal height, so a
## district of uniform blocks reads as almost entirely shaded and every roof
## picks the shaded row of the material table - a 5 K error over a fifth of a
## Swiss town centre, with no symptom anywhere in groups 1-6 because every one
## of those measures shade on GROUND cells, where H is 0 and the two forms agree.
##
## Note this is the same convention heat_svf_matrix() already uses (horizon
## angles are measured from the cell's own height, so a roof sees open sky). The
## two modules disagreeing is what made the Sion map show cool buildings.
cat("\n")
flat <- rast(ext(0, 300, 0, 300), resolution = 5, crs = "EPSG:2056")
values(flat) <- 8L                                   # nothing but built block
noc <- rast(flat); values(noc) <- 0L
for (b in HEAT_BINS) {
  sf_ <- heatShadeRaster(flat, noc, b)
  frac <- mean(values(sf_) == 1)
  ok(sprintf("uniform roof plain is sunlit at %s (%.1f%% shaded)", b, 100 * frac),
     frac < 0.02)
}
## an isolated building cannot shade itself either
iso <- rast(flat); values(iso) <- 3L; iso[28:32, 28:32] <- 8L
si <- heatShadeRaster(iso, noc, "midday")
roof <- values(iso) == 8
ok(sprintf("an isolated building does not shade its own roof (%.1f%%)",
           100 * mean(values(si)[roof] == 1)),
   mean(values(si)[roof] == 1) < 0.02)
## but it must still shade the ground beside it, or the fix has gone too far.
##
## AT MORNING, NOT MIDDAY, and the difference is the grid rather than the model.
## The default block is 10 m since the height ramps were fixed, and at the midday
## elevation (64.5 deg) that casts 10/tan = 4.8 m - less than one 5 m cell, so
## the march's very first step drops the ray 10.4 m and clears the building.
## Zero shaded cells there is the right answer for a 10 m block on a 5 m grid,
## not a regression; the case this check exists for is "a building casts at all",
## and morning (45.8 deg, 9.7 m, two cells) is where that question has an answer.
sim <- heatShadeRaster(iso, noc, "morning")
ok("the same building still shades the ground beside it",
   sum(values(sim) == 1 & !roof) > 0,
   sprintf("[%d cells]", sum(values(sim) == 1 & !roof)))
## A genuinely taller neighbour must still cast onto a lower roof - otherwise the
## fix has replaced one error with the opposite one. This goes at the march
## directly with heights of its own: the per-class heights cannot express the
## case at all, since tree (15 m) beats building (12 m) by 3 m, which at 45 deg
## reaches under 3 m horizontally and the march's first step is a whole cell.
## Sun due east at 45 deg, so shade falls west and each step drops exactly 5 m.
Hm <- matrix(0, 9, 25)
Hm[, 20] <- 30                       # a 30 m wall
Hm[, 15:19] <- 5                     # 5 m roofs to its west
mm <- heat_shadow_march(Hm, res = 5, elev = 45, azim = 90)
ok("a 30 m wall shades the 5 m roof beside it", all(mm[, 19]))
ok("... and the ground further along, out to where the ray lands",
   all(mm[, 14]))
## The discriminating case: at col 15 the ray has fallen to 5 m, so a roof
## standing 8 m there is above it and lit, while bare ground at that same spot
## is below it and shaded. Only a march that compares against the cell's own
## height can return both. 8 rather than 5 deliberately - level-with-the-ray is
## a floating-point tie (tan(pi/4) is not exactly 1) and would test luck.
Hr <- Hm; Hr[, 15] <- 8
ok("a roof standing above the ray is lit",
   !any(heat_shadow_march(Hr, 5, 45, 90)[, 15]))
Hg <- Hm; Hg[, 15] <- 0
ok("... while ground at that same distance is shaded",
   all(heat_shadow_march(Hg, 5, 45, 90)[, 15]))

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
