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
source(file.path(R, "shadow_helpers.R"))
source(file.path(R, "svf_helpers.R"))

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
gr <- aggregate(crop(rast(file.path(LC, "ground_CH_1m.tif")), e), 5, fun = "modal")
cn <- aggregate(crop(rast(file.path(LC, "canopy_CH_1m.tif")), e), 5, fun = "modal")
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
ok(sprintf("obstruction cells see open sky, as intended (%.3f)", mean(sv[hv > 0])),
   mean(sv[hv > 0]) > 0.95)
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
ok("ground SVF rises monotonically with distance to the nearest obstruction",
   all(diff(as.numeric(band)) > 0))
ok(sprintf("far from anything, SVF is essentially 1 (%.3f)", band[[length(band)]]),
   band[[length(band)]] > 0.95)
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
xy <- crds(w$morning, na.rm = FALSE)
ex_m <- mean(xy[values(w$morning) == 1, 1]); ex_a <- mean(xy[values(w$afternoon) == 1, 1])
ok(sprintf("wall band shifts east in the morning vs afternoon (%.1f m)", ex_m - ex_a),
   ex_m > ex_a)

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
