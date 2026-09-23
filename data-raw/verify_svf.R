## Verification for Phase 3: sky view factor and the sunlit-wall term
## (R/svf_helpers.R). Run:  Rscript data-raw/verify_svf.R
##
## Group 1 is analytic - a circular wall of known height and radius has a sky
## view factor we can write down - so it tests the integration itself and not
## merely that the output is plausible. Groups 2-4 need the national land cover
## and are skipped when it is absent.
suppressPackageStartupMessages({library(terra)})
terraOptions(progress = 0)
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
source(file.path(R, "data_paths.R"))
source(file.path(R, "paintbrush_helpers.R"))   ## PAINT_CATEGORIES: heatHeights() is keyed off it
source(file.path(R, "shadow_helpers.R"))
source(file.path(R, "svf_helpers.R"))
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
  cat(sprintf("%-62s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

## --- 1. analytic: a circular wall -------------------------------------------
## A cell ringed by a wall of height h at radius d sees the same horizon angle
## atan(h/d) in every direction, so 1 - mean(sin^2) collapses to
##   SVF = 1 - sin^2(atan(h/d)) = 1 / (1 + (h/d)^2).
cat("=== 1. analytic, circular wall ===\n")
RES <- 1; N <- 201; ctr <- (N %/% 2) + 1
ring <- function(d, h, thick = 2) {
  m <- matrix(0, N, N)
  ij <- expand.grid(i = seq_len(N), j = seq_len(N))
  rr <- sqrt((ij$i - ctr)^2 + (ij$j - ctr)^2)
  m[cbind(ij$i, ij$j)[rr >= d & rr < d + thick, , drop = FALSE]] <- h
  m
}
for (p in list(c(d = 20, h = 10), c(d = 20, h = 20), c(d = 40, h = 10), c(d = 10, h = 30))) {
  d <- p[["d"]]; h <- p[["h"]]
  s <- heat_svf_matrix(ring(d, h), RES, n_dir = 32, max_dist_m = N)[ctr, ctr]
  want <- 1 / (1 + (h / d)^2)
  ok(sprintf("wall h=%g at d=%g: SVF %.3f vs analytic %.3f", h, d, s, want),
     abs(s - want) < 0.03)
}

cat("\n=== 2. bounds and monotonicity ===\n")
flat <- matrix(0, 60, 60)
ok("open flat ground has SVF 1", all(abs(heat_svf_matrix(flat, 5) - 1) < 1e-9))
s10 <- heat_svf_matrix(ring(20, 10), RES, n_dir = 32, max_dist_m = N)[ctr, ctr]
s30 <- heat_svf_matrix(ring(20, 30), RES, n_dir = 32, max_dist_m = N)[ctr, ctr]
ok(sprintf("a taller wall lowers SVF (%.3f -> %.3f)", s10, s30), s30 < s10)
sv <- heat_svf_matrix(ring(20, 20), RES, n_dir = 32, max_dist_m = N)
ok("SVF stays within [0, 1]", all(sv >= 0 & sv <= 1))
## not exactly 1: from the far corner the ring is ~118 cells away and 20 tall,
## which still subtends ~10 deg in the one or two directions that hit it
ok(sprintf("a cell far outside the ring is nearly unobstructed (%.4f)", sv[3, 3]),
   sv[3, 3] > 0.99)

## --- 3. on the real rasters -------------------------------------------------
LC <- Sys.getenv("VFT_LANDCOVER_DIR", "C:/Users/frueh/Documents/Local Data/landcover")
if (!file.exists(file.path(LC, "ground_CH_1m.tif"))) {
  cat("\nnational land cover not present - skipping the on-raster checks\n")
  cat(sprintf("\n%d check(s) failed\n", fails)); quit(status = if (fails == 0) 0 else 1)
}
cat("\n=== 3. real land cover ===\n")
CX <- 2593956; CY <- 1119554
## non-square on purpose - see the orientation group in verify_shadows.R
e <- ext(CX - 500, CX + 500, CY - 300, CY + 300)
## the app's two-stage modal - see the note in verify_shadows.R
gr <- heat_modal_class(crop(rast(file.path(LC, "ground_CH_1m.tif")), e), 5)
cn <- heat_modal_class(crop(rast(file.path(LC, "canopy_CH_1m.tif")), e), 5)
ok("window is non-square", nrow(gr) != ncol(gr), sprintf("[%d x %d]", nrow(gr), ncol(gr)))

t0 <- Sys.time()
svf <- heatSvfRaster(gr, cn)
cat(sprintf("   SVF over %d x %d in %.2f s\n", nrow(gr), ncol(gr),
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
ok("SVF raster returned", !is.null(svf))
ok("SVF within [0, 1]", all(values(svf) >= 0 & values(svf) <= 1, na.rm = TRUE))
H <- heatObstructionHeight(gr, cn)
d <- distance(ifel(H > 0, 1, NA))
hv <- values(H)[, 1]; sv <- values(svf)[, 1]; dv <- values(d)[, 1]

## A roof genuinely sees the whole sky, and heat_svf_matrix() measures horizon
## angles RELATIVE to the cell's own height, so obstruction cells come out near 1
## by design. Here that is 35 % of the window, so every ground-level statement
## below has to exclude them or it measures the rooftops instead - which is what
## made the first draft of this file report SVF as *higher* among the buildings.
##
## "near 1" held ABSOLUTELY only while every building was 10 m and every crown
## 15 m, because then every roof was the skyline. With real heights it is
## relative: measured over this window a 3 m hedge reads 0.708 because
## everything around it overtops it, while a 50 m block reads 1.000. The
## invariant that survives is the one the design actually implies - the taller
## the cell's own obstruction, the more sky it sees - together with the tallest
## class present still seeing essentially all of it.
oh  <- sort(unique(hv[hv > 0]))
rho <- if (length(oh) > 1) cor(hv[hv > 0], sv[hv > 0]) else NA_real_
ok(sprintf("a roof's SVF rises with its own height (r = %+.2f over %d heights)",
           rho, length(oh)), length(oh) > 1 && rho > 0.4)
ok(sprintf("the tallest obstruction present sees open sky (%.0f m -> %.3f)",
           max(oh), mean(sv[hv == max(oh)])), mean(sv[hv == max(oh)]) > 0.95)
g <- hv == 0
ok(sprintf("ground-level SVF is well below 1 in a town centre (%.3f)", mean(sv[g])),
   mean(sv[g]) < 0.85)

## ORIENTATION: if the matrix came back transposed, SVF would not track distance
## to the nearest obstruction. This is the check that caught the same bug in
## Phase 2 - see verify_shadows.R group 6.
brk <- c(-0.1, 5, 10, 20, 40, 1e9); lab <- c("0-5", "5-10", "10-20", "20-40", ">40")
band <- tapply(sv[g], cut(dv[g], brk, labels = lab), mean)
cat("   ground SVF by distance to obstruction (m):",
    paste(sprintf("%s=%.3f", lab, band), collapse = "  "), "\n")
## Monotonic out to 40 m, and deliberately NOT beyond it.
##
## This file used to assert the last band was "essentially 1", and that held only
## because nothing in the window was taller than 15 m. These same 130 cells now
## sit in full view of a 50 m block: the largest horizon angle they see went from
## 13.6 to 35.0 degrees when real heights arrived, and their mean SVF from 0.985
## to 0.926. Distance to the NEAREST obstruction stops predicting sky once
## heights vary, because a tower 200 m off beats a hedge at 45 m. Out to 40 m the
## nearest thing still dominates, which is what makes this an orientation test.
ok("ground SVF rises monotonically with distance out to 40 m",
   all(diff(as.numeric(band[1:4])) > 0))
ok(sprintf("...and cells far from anything see far more sky than cells beside it (%.3f vs %.3f)",
           band[[length(band)]], band[[1]]),
   band[[length(band)]] - band[[1]] > 0.25)
cr <- cor(sv[g], dv[g], use = "complete.obs")
ok(sprintf("SVF correlates with distance to obstruction (r = %.2f)", cr), cr > 0.5)

cat("\n=== 4. sunlit wall term ===\n")
w <- lapply(setNames(HEAT_BINS, HEAT_BINS), function(b) heatWallRaster(gr, cn, b))
for (b in HEAT_BINS) {
  ok(sprintf("%s wall raster returned and is 0/1", b),
     !is.null(w[[b]]) && all(values(w[[b]]) %in% c(0L, 1L)))
  cat(sprintf("   %-9s %.2f%% of cells in front of a sunlit facade\n", b,
              100 * mean(values(w[[b]]))))
}
sh <- heatShadeRaster(gr, cn, "midday")
ok("no wall bonus on a shaded cell", !any(values(w$midday) == 1 & values(sh) == 1))
ok("no wall bonus on a cell that is itself built", !any(values(w$midday) == 1 & values(H) > 0))
ok("wall cells sit near obstructions",
   mean(values(d)[values(w$midday) == 1], na.rm = TRUE) < 10)

## The geometric signature: the bonus is on the sun-facing side, so it must swap
## sides of the buildings between morning and afternoon. Morning sun is ESE
## (az 109) and afternoon WSW (az 251), so the morning band sits further east.
##
## THIS CHECK IS SENSITIVE TO THE DEFAULT BLOCK HEIGHT, and it is worth knowing
## why before reading a failure as a broken wall term. A wall cell is the cell
## DOWNSUN of a taller neighbour, which is also where that neighbour's shadow
## falls - and the term then discards every shaded cell, because a shaded cell
## gets no facade bonus. So a building only contributes wall cells at all when it
## is too SHORT to shade its own neighbour, i.e. when its height is under one
## march step, res * tan(elevation). At HEAT_RES = 5 m that threshold is 5.1 m
## at morning and afternoon and 10.4 m at midday.
##
## This check FAILED for as long as the land cover had one height per material.
## Every building was the 10 m default - just under the midday threshold, well
## over the morning one - so blocks contributed wall cells at midday and none at
## morning or afternoon, and the whole east-west signature rested on the 5 m
## artificial canopies. Real heights fixed it by giving the window obstructions
## on BOTH sides of both thresholds: 3 m crowns and 5 m blocks are under the
## morning step, 15 m and taller are over the midday one. The numbers printed
## below say which classes are actually in play, so a failure here points at the
## grid and the height table rather than at this file.
.rise <- sapply(HEAT_BINS, function(b) res(H)[1] * tan(heatSunPosition(b)$elevation * pi / 180))
cat(sprintf("   one march step drops the ray: %s\n",
            paste(sprintf("%s %.1f m", HEAT_BINS, .rise), collapse = "  ")))
cat(sprintf("   obstruction heights in play : %s\n",
            paste(sprintf("%s=%g", names(heatHeights()), unname(heatHeights())), collapse = " ")))
xy <- crds(w$morning, na.rm = FALSE)
ex_m <- mean(xy[values(w$morning) == 1, 1]); ex_a <- mean(xy[values(w$afternoon) == 1, 1])
ok(sprintf("wall band shifts east in the morning vs afternoon (%.1f m)", ex_m - ex_a),
   ex_m > ex_a)

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
