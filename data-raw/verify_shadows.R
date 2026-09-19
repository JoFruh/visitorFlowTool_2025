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
source(file.path(R, "shadow_helpers.R"))

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
for (bin in HEAT_BINS) {
  s <- heatSunPosition(bin)
  for (cls in c(canopy_tree = 7, artificial_block = 8, canopy_artificial = 6)) {
    h <- unname(heatHeights()[[as.character(cls)]])
    H <- mk(h)
    m <- as.matrix(H, wide = TRUE)
    sh <- heat_shadow_march(m, RES, s$elevation, s$azimuth)
    idx <- which(sh, arr.ind = TRUE)
    ctr <- nrow(m) %/% 2 + 1
    reach <- if (nrow(idx)) max(sqrt((idx[, 1] - ctr)^2 + (idx[, 2] - ctr)^2)) * RES else 0
    want <- unname(g[[sprintf("shadow_length_%s_%s",
                              names(which(c(canopy_tree = 7, artificial_block = 8,
                                            canopy_artificial = 6) == cls)), bin)]])
    ok(sprintf("%s / %s reach %.1f m vs table %.1f m", bin, cls, reach, want),
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
gr <- aggregate(crop(rast(file.path(LC, "ground_CH_1m.tif")), e), 5, fun = "modal")
cn <- aggregate(crop(rast(file.path(LC, "canopy_CH_1m.tif")), e), 5, fun = "modal")
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
g2 <- aggregate(crop(rast(file.path(LC, "ground_CH_1m.tif")), e2), 5, fun = "modal")
c2 <- aggregate(crop(rast(file.path(LC, "canopy_CH_1m.tif")), e2), 5, fun = "modal")
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

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
