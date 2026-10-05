## Comparison of the two destination splits inside generateAoI2():
##   "watershed" (R/aoiSegment.R) - high-value cores of DULN_all grown out to the
##                                  threshold, merged back by dip and paths;
##   "cut"       (R/aoiCut.R)     - the inverse: the finished threshold areas cut
##                                  at narrow bridges, by shape alone, no paths.
##
## Part 0 checks the distance transform the cut runs on against brute force.
## Part 1 is synthetic: shapes whose answer is known, and the watershed's own
## fixtures (data-raw/verify_aoi_split.R) run through both.
## Part 2 runs generateAoI2() on real perimeters three ways - unsplit,
## watershed, cut - and reports speed, sizes and how far the two splits agree,
## with a CSV and a PNG per case.
##
## Run:  Rscript data-raw/compare_aoi_split.R                (all parts)
##       VFT_COMPARE_REAL=0 Rscript data-raw/compare_aoi_split.R   (parts 0-1)
##       VFT_SPLIT_PNG=<dir>  where the PNGs and the CSV go (default: tempdir())
suppressPackageStartupMessages({library(sf); library(shiny)})
terra::terraOptions(progress = 0)
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
env <- new.env(parent = globalenv())
for (f in sort(list.files(R, pattern = "[.][Rr]$", full.names = TRUE))) {
  suppressWarnings(try(sys.source(f, envir = env), silent = TRUE))
}
attach(env, warn.conflicts = FALSE)
## The working tree's compiled code, not the installed package. Build it with
## Rscript -e "Rcpp::compileAttributes(); pkgbuild::compile_dll('.')"
{
  .dll <- file.path(dirname(R), "src", paste0("visitorFlowTool", .Platform$dynlib.ext))
  if(!file.exists(.dll)) stop("compiled code missing: ", .dll)
  dyn.load(.dll)
}

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}
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
nLab <- function(l) length(unique(stats::na.omit(terra::values(l, mat = FALSE))))

## How far two labellings of the SAME cells agree.
##   ari: adjusted Rand index - 1 identical, ~0 no better than chance.
##   iouAB: for each area of A, the best IoU with any area of B, weighted by
##          A's area - "how well does B reproduce A's areas". iouBA the reverse.
agreement <- function(la, lb){
  a <- terra::values(la, mat = FALSE); b <- terra::values(lb, mat = FALSE)
  k <- !is.na(a) & !is.na(b)
  tab <- table(a[k], b[k])
  c2  <- function(x) x * (x - 1) / 2
  sIJ <- sum(c2(as.numeric(tab)))
  sA  <- sum(c2(as.numeric(rowSums(tab)))); sB <- sum(c2(as.numeric(colSums(tab))))
  e   <- sA * sB / c2(sum(k))
  ari <- if((sA + sB) / 2 - e == 0) 1 else (sIJ - e) / ((sA + sB) / 2 - e)
  inter <- unclass(tab)
  uni   <- outer(rowSums(tab), colSums(tab), "+") - inter
  iou   <- inter / uni
  list(ari   = ari,
       iouAB = sum(apply(iou, 1, max) * rowSums(tab)) / sum(k),
       iouBA = sum(apply(iou, 2, max) * colSums(tab)) / sum(k))
}

cat("=== 0. the distance transform ===\n")
m <- matrix(FALSE, 7, 9); m[2:6, 2:8] <- TRUE
d <- matrix(aoi_edt(as.vector(t(m)), 7L, 9L, 10, 20), 7, 9, byrow = TRUE)
ok("rectangle, 10 x 20 m cells: the centre is 2 rows (40 m) in", d[4, 5] == 40)
ok("...the corner cell is one cell (10 m, the short way) in", d[2, 2] == 10)
ok("...outside is NA", all(is.na(d[!m])))
ok("the grid's edge counts as outside",
   all(aoi_edt(rep(TRUE, 12), 3L, 4L, 1, 1) == c(1,1,1,1, 1,2,2,1, 1,1,1,1)))
set.seed(1); nr <- 40; nc <- 55; ins <- stats::runif(nr * nc) > 0.2
d  <- aoi_edt(ins, nr, nc, 73, 106)
rr <- (seq_len(nr * nc) - 1) %/% nc; cc <- (seq_len(nr * nc) - 1) %% nc
oR <- c(rr[!ins], rep(-1, nc + 2), rep(nr, nc + 2), 0:(nr - 1), 0:(nr - 1))
oC <- c(cc[!ins], -1:nc, -1:nc, rep(-1, nr), rep(nc, nr))
bf <- vapply(which(ins), function(i) sqrt(min(((rr[i] - oR) * 106)^2 + ((cc[i] - oC) * 73)^2)), 1)
ok("random mask, 73 x 106 m cells: exact against brute force", max(abs(bf - d[ins])) < 1e-6)

cat("\n=== 1. synthetic ===\n")
crs  <- "EPSG:2056"
o    <- c(2600000, 1200000)
grid <- terra::rast(xmin = o[1], xmax = o[1] + 6000, ymin = o[2], ymax = o[2] + 3000,
                    res = 50, crs = crs)
xy   <- terra::xyFromCell(grid, seq_len(terra::ncell(grid)))
X <- xy[, 1] - o[1]; Y <- xy[, 2] - o[2]
T <- 0.3
disc  <- function(x, y, r) (X - x)^2 + (Y - y)^2 <= r^2
strip <- function(x0, x1, w) X >= x0 & X <= x1 & abs(Y - 1500) <= w / 2
asD   <- function(m) terra::setValues(grid, ifelse(m, 1, 0))
cut   <- function(D, ...) vftCutAoI(D, T, crs = crs, ...)

dumbbell <- asD(disc(1500, 1500, 800) | disc(4500, 1500, 800) | strip(1500, 4500, 150))
ok("dumbbell, two 1.6 km discs on a 150 m bridge: two", nLab(cut(dumbbell)) == 2)
lv <- terra::values(cut(dumbbell), mat = FALSE)
ok("...THE INVARIANT: exactly the mask's cells are labelled",
   identical(!is.na(lv), terra::values(dumbbell, mat = FALSE) >= T))
ok("...the cut falls on the bridge (each disc whole in one area)",
   length(unique(lv[disc(1500, 1500, 800)])) == 1 && length(unique(lv[disc(4500, 1500, 800)])) == 1)
ok("the same discs on a 1.2 km wide bridge: one",
   nLab(cut(asD(disc(1500, 1500, 800) | disc(4500, 1500, 800) | strip(1500, 4500, 1200)))) == 1)
ok("...the 150 m bridge merges at depth 0.95 (floor off)",
   nLab(cut(dumbbell, depth = 0.95, bridge = Inf)) == 1)
th <- atan2(Y - 1500, X - 3000)
wavy <- asD(sqrt((X - 3000)^2 + (Y - 1500)^2) <= 1000 + 150 * sin(8 * th))
bw   <- aoi_basins(aoi_edt(terra::values(wavy, mat = FALSE) >= T, terra::nrow(grid),
                           terra::ncol(grid), 50, 50), terra::nrow(grid), terra::ncol(grid), 0)
ok("a disc with a wavy outline: one area", nLab(cut(wavy)) == 1,
   sprintf("(%d raw bodies)", length(bw$peak)))
app <- function(r) asD(disc(2000, 1500, 1000) | strip(2900, 3400, 100) | disc(3400 + r - 50, 1500, r))
ok("a 20 ha appendix on a 100 m neck joins: one", nLab(cut(app(250))) == 1)
ok("...a 50 ha one stays apart: two",                        nLab(cut(app(400))) == 2)
ok("nothing above the threshold: no areas", nLab(cut(asD(X < 0))) == 0)

## The bridge-width floor (VFT_AOI_CUT_MAX_BRIDGE_M): two 2.4 km discs on a
## 900 m waist - dip 1 - 450/1200 = 0.63, so the dip alone cuts it.
waist <- function(w) asD(disc(1700, 1500, 1200) | disc(4300, 1500, 1200) | strip(1700, 4300, w))
ok("a 900 m waist between two 2.4 km discs: one (the floor)",  nLab(cut(waist(900))) == 1)
ok("...two with the floor off",                               nLab(cut(waist(900), bridge = Inf)) == 2)
ok("...a 400 m bridge between them is still cut: two",         nLab(cut(waist(400))) == 2)
ok("the 150 m dumbbell bridge is under any sane floor: two",   nLab(cut(dumbbell, bridge = 300)) == 2)
## join in .vftMergeBasins(): outright, and kept when two borders collapse.
## 1|3 merge (shallow, crossed); 2 has a deep dip to both, and only its border
## with 3 is flagged - once 1 and 3 are one, that flag carries over.
jm <- function(join) .vftMergeBasins(3L, c(1, 1, 1), c(1e6, 1e6, 1e6), a = c(1L, 1L, 2L), b = c(3L, 2L, 3L),
        saddle = c(0.9, 0.4, 0.4), crossed = c(TRUE, FALSE, FALSE), len = c(100, 100, 100),
        minThresh = 0.3, depth = 0.5, neck = 300, minArea = 1e5, join = join)
ok("join: a flagged border merges despite a deep dip",        length(unique(jm(c(FALSE, FALSE, TRUE)))) == 1)
ok("...without the flag: two",                                length(unique(jm(c(FALSE, FALSE, FALSE)))) == 2)

## The watershed's fixtures (verify_aoi_split.R), through both. The cut sees
## shapes only: the ridge is a 100-200 m neck between two ~1.9 km forests
## whatever its attractiveness, so it cuts all three - and a path does not
## change that. The watershed lets the shallow ridge merge where a path crosses.
surface <- function(h){
  fA <- exp(-((X - 1500)^2 + (Y - 1500)^2) / (2 * 600^2))
  fB <- exp(-((X - 4500)^2 + (Y - 1500)^2) / (2 * 600^2))
  along <- 1 - (1 - h) * sin(pi * (X - 1500) / 3000)
  ridge <- ifelse(X >= 1500 & X <= 4500, along * exp(-(Y - 1500)^2 / (2 * 60^2)), 0)
  terra::setValues(grid, pmax(fA, fB, ridge))
}
through <- sf::st_sfc(sf::st_linestring(rbind(o + c(500, 1500), o + c(5500, 1500))), crs = 2056)
cat(sprintf("\n  %-34s %10s %10s %6s\n", "fixture", "watershed", "+path", "cut"))
for(h in c(deep = 0.4, shallow = 0.85, level = 0.97)){
  s <- surface(h)
  w0 <- nLab(vftSegmentAoI(s, T, sf::st_sfc(crs = 2056), crs = crs))
  w1 <- nLab(vftSegmentAoI(s, T, through, crs = crs))
  cc <- nLab(cut(s))
  cat(sprintf("  ridge h = %-24.2f %10d %10d %6d\n", h, w0, w1, cc))
  ok(sprintf("  cut: ridge h = %.2f is a narrow neck, two", h), cc == 2)
}
close <- terra::setValues(grid, pmax(exp(-((X - 1500)^2 + (Y - 1500)^2) / (2 * 600^2)),
                                     exp(-((X - 2500)^2 + (Y - 1500)^2) / (2 * 600^2))))
ok("two forests 1 km apart, one wide blob: cut gives one", nLab(cut(close)) == 1)

n <- as.integer(Sys.getenv("VFT_COMPARE_REAL", "1"))
if(n > 0){
  cat("\n=== 2. real perimeters, generateAoI2() unsplit / watershed / cut ===\n")
  pngDir <- Sys.getenv("VFT_SPLIT_PNG", tempdir())
  full <- terra::rast(vftData("maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif"))
  discLL <- function(x, y, r) sf::st_sf(geometry = sf::st_transform(
    sf::st_buffer(sf::st_sfc(sf::st_point(c(x, y)), crs = 2056), r), 4326))
  perims <- list(
    filBleu      = sf::st_transform(sf::st_read(vftData("maps/filBleu_area_final.shp"), quiet = TRUE), 4326),
    glatt        = sf::st_transform(sf::st_read(vftData("maps/glattPerimeter_LV95_AREA.shp"), quiet = TRUE), 4326),
    zuerichberg  = discLL(2686500, 1249000, 3000),
    sihlwald     = discLL(2684500, 1234500, 3500))
  secs <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))
  stats <- function(g){
    a <- as.numeric(sf::st_area(sf::st_transform(sf::st_geometry(g), 2056))) / 1e4
    c(n = length(a), largest = max(c(0, a)), median = if(length(a)) stats::median(a) else 0,
      over500 = sum(a > 500), total = sum(a))
  }
  rows <- list()

  for(nm in names(perims)){
    shape <- perims[[nm]]
    crop  <- terra::crop(full, sf::st_transform(sf::st_as_sfc(sf::st_buffer(
      sf::st_transform(shape, 2056), 1500)), 4326))
    names(crop) <- "walkNat"
    #generateAoI2()'s block 1, for the cores alone
    buf  <- terra::buffer(terra::vect(shape), 1000)
    sel  <- terra::mask(terra::crop(crop, buf), buf)
    buf2056 <- sf::st_union(sf::st_buffer(sf::st_geometry(sf::st_transform(shape, 2056)), 1000))

    for(q in c(0.5, 0.7)){
      thr <- as.numeric(stats::quantile(terra::values(crop), q, na.rm = TRUE))
      lab <- sprintf("%s q%.0f", nm, 100 * q)

      t0 <- Sys.time()
      off <- suppressWarnings(generateAoI2(thr, shape, walkNat = crop, DULN_all = crop, split = FALSE))$polygons
      tOff <- secs(t0); t0 <- Sys.time()
      ws  <- suppressWarnings(generateAoI2(thr, shape, walkNat = crop, DULN_all = crop, method = "watershed"))$polygons
      tWs <- secs(t0); t0 <- Sys.time()
      ct  <- suppressWarnings(generateAoI2(thr, shape, walkNat = crop, DULN_all = crop, method = "cut"))$polygons
      tCt <- secs(t0)

      #the cores alone: paths read + watershed split vs the cut
      t0 <- Sys.time(); paths <- .vftReadPathsWithin(buf2056); tRead <- secs(t0)
      t0 <- Sys.time(); lWs <- vftSegmentAoI(sel, thr, paths); tSeg <- secs(t0)
      t0 <- Sys.time(); lCt <- vftCutAoI(sel, thr); tCut <- secs(t0)
      ag <- agreement(lWs, lCt)

      sO <- stats(off); sW <- stats(ws); sC <- stats(ct)
      cat(sprintf(paste0("%s\n  areas   unsplit %3d (largest %5.0f ha) | watershed %3d (largest %5.0f, median %4.0f, >500 ha %d)",
                         " | cut %3d (largest %5.0f, median %4.0f, >500 ha %d)\n",
                         "  time    generateAoI2 %.1f / %.1f / %.1f s;  core: paths read %.1f s + watershed %.2f s  vs  cut %.2f s\n",
                         "  agree   ARI %.2f; watershed areas found in cut IoU %.2f, cut areas found in watershed IoU %.2f\n"),
                  lab, sO["n"], sO["largest"], sW["n"], sW["largest"], sW["median"], sW["over500"],
                  sC["n"], sC["largest"], sC["median"], sC["over500"],
                  tOff, tWs, tCt, tRead, tSeg, tCut, ag$ari, ag$iouAB, ag$iouBA))

      g  <- sf::st_transform(sf::st_geometry(ct), 2056)
      ok(paste(lab, "cut: no overlap"), overlapArea(g) < 100, sprintf("(%.1f m2)", overlapArea(g)))
      ok(paste(lab, "cut: every area > 10 ha"), all(as.numeric(sf::st_area(g)) > VFT_AOI_MIN_AREA_M2 * 0.99))
      ok(paste(lab, "cut: covers the same ground as unsplit (within 1 %)"),
         abs(sC["total"] - sO["total"]) / max(1, sO["total"]) < 0.01,
         sprintf("(%.0f vs %.0f ha)", sC["total"], sO["total"]))
      ok(paste(lab, "cut: valid in lon/lat and in 2056"),
         all(sf::st_is_valid(ct)) && all(sf::st_is_valid(g)))
      ok(paste(lab, "cut: DULN scored for every area"), !anyNA(ct$DULN))
      ok(paste(lab, "cut: the split ran (more areas than unsplit)"), sC["n"] > sO["n"])

      rows[[lab]] <- data.frame(perimeter = nm, quantile = q, threshold = thr,
        n_unsplit = sO["n"], n_watershed = sW["n"], n_cut = sC["n"],
        largest_unsplit_ha = sO["largest"], largest_watershed_ha = sW["largest"], largest_cut_ha = sC["largest"],
        median_watershed_ha = sW["median"], median_cut_ha = sC["median"],
        over500_watershed = sW["over500"], over500_cut = sC["over500"],
        t_unsplit_s = tOff, t_watershed_s = tWs, t_cut_s = tCt,
        t_paths_read_s = tRead, t_watershed_core_s = tSeg, t_cut_core_s = tCut,
        ari = ag$ari, iou_watershed_in_cut = ag$iouAB, iou_cut_in_watershed = ag$iouBA,
        row.names = NULL)

      f <- file.path(pngDir, sprintf("aoi_compare_%s_q%.0f.png", nm, 100 * q))
      grDevices::png(f, width = 2400, height = 800)
      graphics::par(mfrow = c(1, 3), mar = c(1, 1, 3, 1))
      cols <- function(k) grDevices::hcl.colors(max(k, 1), "Dark 3")[sample.int(max(k, 1))]
      for(p in list(list(off, "unsplit"), list(ws, "watershed"), list(ct, "cut"))){
        set.seed(1)
        gg <- sf::st_transform(sf::st_geometry(p[[1]]), 2056)
        plot(sf::st_geometry(sf::st_transform(shape, 2056)), border = "grey40",
             main = sprintf("%s: %s, %d areas", lab, p[[2]], length(gg)), cex.main = 2)
        plot(gg, col = cols(length(gg)), border = "black", add = TRUE)
      }
      grDevices::dev.off()
    }
  }
  csv <- file.path(pngDir, "aoi_split_compare.csv")
  utils::write.csv(do.call(rbind, rows), csv, row.names = FALSE)
  cat(sprintf("\nPNGs and %s in %s\n", basename(csv), pngDir))
}

cat(sprintf("\n%s\n", if (fails == 0) "ALL PASS" else paste(fails, "FAIL(S)")))
quit(status = if (fails == 0) 0 else 1)
