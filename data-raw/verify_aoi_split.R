## Verification for the destination split (R/aoiSegment.R, src/aoi_segment.cpp)
## inside generateAoI2().
##
## Part 1 is synthetic and needs no data: two round "forests" on a 50 m grid in
## EPSG:2056, joined by a ridge whose height sets how deep the dip between them
## is, with the path lines handed in - so every rule is checked against a
## surface whose answer is known.
## Part 2 runs the real generateAoI2() on real perimeters, against the committed
## (pre-split) version and against split = FALSE, and draws before/after PNGs.
##
## Run:  Rscript data-raw/verify_aoi_split.R              (both parts)
##       VFT_SPLIT_REAL=0 Rscript data-raw/verify_aoi_split.R   (part 1 only)
##       VFT_SPLIT_PNG=<dir>  where the PNGs go (default: tempdir())
suppressPackageStartupMessages({library(sf); library(shiny)})
terra::terraOptions(progress = 0)
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
env <- new.env(parent = globalenv())
for (f in sort(list.files(R, pattern = "[.][Rr]$", full.names = TRUE))) {
  suppressWarnings(try(sys.source(f, envir = env), silent = TRUE))
}
attach(env, warn.conflicts = FALSE)
## The basin sweep is C++. Load the WORKING TREE's compiled code, not the
## installed package (see verify_heat_model.R). Build it with
## Rscript -e "pkgbuild::compile_dll('.')" if this fails.
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

cat("=== 0. the basin sweep ===\n")
#   row-major 3 x 5:   9 1 0 1 8      two peaks, a pass of 1 between them
#                      9 5 2 5 8      through the middle row
#                      9 1 0 1 8
v <- c(9,1,0,1,8, 9,5,2,5,8, 9,1,0,1,8)
b <- aoi_basins(v, 3L, 5L, 0.5)
ok("two peaks make two basins", length(b$peak) == 2 && identical(sort(b$peak), c(8, 9)))
ok("...one border, saddle = the pass (2)", length(b$a) == 1 && b$saddle == 2)
ok("...cells below the threshold stay 0", all(b$label[v < 0.5] == 0))
ok("...every other cell is labelled", all(b$label[v >= 0.5] > 0))
ok("...the peaks' columns go to their own basin",
   length(unique(b$label[c(1, 6, 11)])) == 1 && length(unique(b$label[c(5, 10, 15)])) == 1 &&
   b$label[1] != b$label[5])
b <- aoi_basins(rep(3, 12), 3L, 4L, 1)
ok("a plateau is one basin, not one per cell", length(b$peak) == 1 && all(b$label == 1))
b <- aoi_basins(c(NA, 5, 5, NA), 2L, 2L, 1)
ok("NA cells are skipped (two diagonal cells = two basins, no border)",
   length(b$peak) == 2 && !length(b$a))

cat("\n=== 1. synthetic ===\n")
crs  <- "EPSG:2056"
o    <- c(2600000, 1200000)
grid <- terra::rast(xmin = o[1], xmax = o[1] + 6000, ymin = o[2], ymax = o[2] + 3000,
                    res = 50, crs = crs)
xy   <- terra::xyFromCell(grid, seq_len(terra::ncell(grid)))
X <- xy[, 1] - o[1]; Y <- xy[, 2] - o[2]
T <- 0.3
#two forests (peak 1) at x = 1500 and 4500, and a ridge of height h between
#them. max() so the dip is exactly 1 - h: relative dip (1-h)/(1-T). The ridge
#is a NECK: sigma 60 m keeps its above-threshold band 2-4 cells (100-200 m)
#wide, under VFT_AOI_SPLIT_NECK_M.
surface <- function(h, bump = FALSE){
  fA <- exp(-((X - 1500)^2 + (Y - 1500)^2) / (2 * 600^2))
  fB <- exp(-((X - 4500)^2 + (Y - 1500)^2) / (2 * 600^2))
  #along its line the ridge falls smoothly from 1 at each forest's centre to h
  #at the midpoint, so each half climbs to its own forest and the two meet only
  #ACROSS the band at x = 3000. A flat band at h ran on into forest B's disc,
  #and A's plateau cells then touched B's disc along the band's edges for
  #~500 m - a wide border, not a neck.
  along <- 1 - (1 - h) * sin(pi * (X - 1500) / 3000)
  ridge <- ifelse(X >= 1500 & X <= 4500, along * exp(-(Y - 1500)^2 / (2 * 60^2)), 0)
  v <- pmax(fA, fB, ridge)
  #a small hill on forest A's south flank: its own core, but under 10 ha
  #(sigma 60 made it 10.5 ha - just too big to be a fringe)
  if(bump) v <- v + 0.25 * exp(-((X - 1500)^2 + (Y - 800)^2) / (2 * 40^2))
  terra::setValues(grid, v)
}
ln <- function(...) sf::st_linestring(rbind(...))
P  <- function(...) sf::st_sfc(..., crs = 2056)
through <- P(ln(o + c(500, 1500), o + c(5500, 1500)))                    # along the ridge
around  <- P(ln(o + c(1500, 1500), o + c(1500, 2950), o + c(4500, 2950),
                o + c(4500, 1500)))                                     # out of the mask and back
nAoI <- function(D, paths, ...){
  l <- vftSegmentAoI(D, T, paths, crs = crs, ...)
  length(unique(stats::na.omit(terra::values(l, mat = FALSE))))
}

#the cell centres sit 25 m off the ridge line, so the pass is 0.917 h, not h
deep    <- surface(0.4)    # dip 0.90
shallow <- surface(0.85)   # dip 0.32
level   <- surface(0.97)   # dip 0.16
ok("a neck with no path: two areas",                nAoI(shallow, P()) == 2)
ok("a path across a DEEP neck: still two",          nAoI(deep, through) == 2)
ok("a path across a SHALLOW neck: one",             nAoI(shallow, through) == 1)
ok("a path that leaves the mask and comes back: two", nAoI(shallow, around) == 2)
ok("a near-level NECK with no path: still two",    nAoI(level, P()) == 2)
ok("depth = 0.95 lets the deep neck merge",         nAoI(deep, through, depth = 0.95) == 1)
ok("neck = 100 m makes the 200 m ridge wide: one without a path",
   nAoI(shallow, P(), neck = 100) == 1)

#No neck: two forests 1 km apart touch along a border well over a kilometre
#long. Dip 0.42.
close <- terra::setValues(grid, pmax(exp(-((X - 1500)^2 + (Y - 1500)^2) / (2 * 600^2)),
                                     exp(-((X - 2500)^2 + (Y - 1500)^2) / (2 * 600^2))))
b <- aoi_basins(terra::values(close, mat = FALSE), terra::nrow(close), terra::ncol(close), T)
ok("two close forests: two cores, a border over 1 km long",
   length(b$peak) == 2 && (b$nh + b$nv) * 50 > 1000, sprintf("(%d m)", (b$nh + b$nv) * 50))
ok("...a WIDE border needs no path: one",          nAoI(close, P()) == 1)
ok("...but the dip still counts (depth 0.3, contact rule off): two",
   nAoI(close, P(), depth = 0.3, contact = Inf) == 2)
shareClose <- (b$nh + b$nv) / min(b$ph + b$pv)
ok("...unless the border is over 20 % of an outline (contact rule): one",
   shareClose > 0.2 && nAoI(close, P(), depth = 0.3) == 1, sprintf("(%.2f)", shareClose))
ok("the ridge neck is a tiny share of either forest's outline: still two",
   nAoI(level, P()) == 2)

#Widths add up as areas merge: 1 and 3 share a wide border, 2 touches each of
#them along 200 m, no path anywhere. Once 1 and 3 are one area, its border
#with 2 is 400 m - no neck.
m <- .vftMergeBasins(3L, c(1, 1, 1), c(1e6, 1e6, 1e6), a = c(1L, 1L, 2L), b = c(3L, 2L, 3L),
                     saddle = c(0.9, 0.9, 0.9), crossed = c(FALSE, FALSE, FALSE),
                     len = c(1000, 200, 200), minThresh = 0.3, depth = 0.5, neck = 300,
                     minArea = 1e5)
ok("two short borders to one merged area add up: one area", length(unique(m)) == 1)
m <- .vftMergeBasins(3L, c(1, 1, 1), c(1e6, 1e6, 1e6), a = c(1L, 1L, 2L), b = c(3L, 2L, 3L),
                     saddle = c(0.9, 0.9, 0.9), crossed = c(FALSE, FALSE, FALSE),
                     len = c(1000, 200, 50), minThresh = 0.3, depth = 0.5, neck = 300,
                     minArea = 1e5)
ok("...but 200 + 50 m is still a neck: two",       length(unique(m)) == 2)

#The contact rule, on its own: a deep dip (0.86), no path, a 200 m border.
cm <- function(per, len = 200) .vftMergeBasins(2L, c(1, 1), c(1e6, 1e6), a = 1L, b = 2L,
        saddle = 0.4, crossed = FALSE, len = len, minThresh = 0.3, depth = 0.5,
        neck = 300, minArea = 1e5, per = per, contact = 0.2)
ok("contact: 200 m of a 800 m outline (25 %) connects despite the dip",
   length(unique(cm(c(800, 10000)))) == 1)
ok("...200 m of two 5 km outlines (4 %) does not",  length(unique(cm(c(5000, 5000)))) == 2)
ok("...exactly 20 % does not (more than, not at least)", length(unique(cm(c(1000, 5000)))) == 2)

#Outlines and borders add up. 1 and 3 merge (shallow, wide); 2 has a deep dip
#to both and 200 m with each, against its own 1500 m outline: 13 % each,
#27 % with the merged area.
m <- .vftMergeBasins(3L, c(1, 1, 1), c(1e6, 1e6, 1e6), a = c(1L, 1L, 2L), b = c(3L, 2L, 3L),
                     saddle = c(0.9, 0.4, 0.4), crossed = c(FALSE, FALSE, FALSE),
                     len = c(1000, 200, 200), minThresh = 0.3, depth = 0.5, neck = 300,
                     minArea = 1e5, per = c(4000, 1500, 4000), contact = 0.2)
ok("two 13 % contacts with a merged pair make one of 27 %: one area", length(unique(m)) == 1)

#The size rule: a 15 ha appendix with a deep dip, no path, a short border and
#a tiny share of either outline - nothing else would join it.
jm <- function(minArea, areas = c(1.5e5, 3e6)) .vftMergeBasins(2L, c(1, 1), areas, a = 1L, b = 2L,
        saddle = 0.4, crossed = FALSE, len = 100, minThresh = 0.3, depth = 0.5, neck = 300,
        minArea = minArea, per = c(5000, 20000), contact = 0.2)
ok("a 15 ha appendix joins its neighbour under the 25 ha rule", length(unique(jm(VFT_AOI_SPLIT_JOIN_M2))) == 1)
ok("...but not under a 10 ha one",                    length(unique(jm(1e5))) == 2)
ok("two 30 ha woods: neither joins under 25 ha",       length(unique(jm(VFT_AOI_SPLIT_JOIN_M2, c(3e5, 3e5)))) == 2)
ok("the default join size is VFT_AOI_SPLIT_JOIN_M2 (25 ha)",
   identical(formals(vftSegmentAoI)$minArea, quote(VFT_AOI_SPLIT_JOIN_M2)) && VFT_AOI_SPLIT_JOIN_M2 == 2.5e5)

bumpy <- surface(0.4, bump = TRUE)
b <- aoi_basins(terra::values(bumpy, mat = FALSE), terra::nrow(bumpy), terra::ncol(bumpy), T)
ok("the hill on the flank is a core of its own", length(b$peak) == 3)
l <- vftSegmentAoI(bumpy, T, P(), crs = crs)
lv <- terra::values(l, mat = FALSE)
ok("...under 10 ha, so it joins forest A rather than standing alone",
   length(unique(stats::na.omit(lv))) == 2)
ok("THE INVARIANT: every cell above the threshold is in an area",
   sum(!is.na(lv)) == sum(terra::values(bumpy, mat = FALSE) >= T))
ok("...and none below it", all(terra::values(bumpy, mat = FALSE)[!is.na(lv)] >= T))

#Crossings on a hand-made label grid: basin 1 west of x = 3000, 2 east of it.
#(On the surfaces above a flat ridge goes wholly to whichever forest the sweep
#reaches first, so the border is not where the eye puts it.)
halves <- ifelse(X < 3000, 1L, 2L)
cross <- function(paths) .vftPathCrossings(grid, halves, 1L, 2L, paths, 20, crs)
ok("a line across the border crosses it",            cross(through))
ok("a line ending short of it does not",
   !cross(P(ln(o + c(500, 1500), o + c(2950, 1500)))))
gap <- sf::st_sfc(sf::st_multilinestring(list(rbind(o + c(500, 1500), o + c(2960, 1500)),
                                              rbind(o + c(3040, 1500), o + c(5500, 1500)))), crs = 2056)
ok("a MULTILINESTRING broken at the border does not cross it", !cross(gap))
gapJoined <- sf::st_sfc(sf::st_multilinestring(list(rbind(o + c(500, 1500), o + c(5500, 1500)),
                                                    rbind(o + c(200, 200), o + c(300, 300)))), crs = 2056)
ok("...one whose first part spans it does",          cross(gapJoined))
ok("two separate lines meeting at the border do not",
   !cross(P(ln(o + c(500, 1500), o + c(2990, 1500)), ln(o + c(3010, 1500), o + c(5500, 1500)))))

#Simplifying the split areas as ONE coverage: neighbours share their
#simplified border exactly. The two forests with the deep neck, plus a hole of
#below-threshold land punched into forest A.
holed <- surface(0.4)
holed[terra::cellFromXY(holed, cbind(o[1] + c(1500, 1550, 1500, 1550), o[2] + c(1500, 1500, 1550, 1550)))] <- 0
lab <- vftSegmentAoI(holed, T, P(), crs = crs)
cov <- .vftSimplifyCoverage(lab, VFT_AOI_TOLERANCE_M, crs = crs)
rawA <- sum(!is.na(terra::values(lab, mat = FALSE))) * 50^2
ok("coverage: two areas", length(cov) == 2)
ok("...sharing a border, not overlapping", overlapArea(cov) < 1 &&
     any(lengths(sf::st_touches(cov)) > 0), sprintf("(%.2f m2)", overlapArea(cov)))
ok("...valid, and still valid after a lon/lat round trip",
   all(sf::st_is_valid(cov)) && all(sf::st_is_valid(sf::st_transform(sf::st_transform(cov, 4326), 2056))))
ok("...within 1 % of the raw cells' area",
   abs(sum(as.numeric(sf::st_area(cov))) - rawA) / rawA < 0.01,
   sprintf("(%.1f vs %.1f ha)", sum(as.numeric(sf::st_area(cov))) / 1e4, rawA / 1e4))
ok("...the below-threshold hole stays a hole",
   !any(sf::st_intersects(sf::st_sfc(sf::st_point(o + c(1525, 1525)), crs = 2056), cov, sparse = FALSE)))
ok("...simplified: fewer vertices than the staircase",
   nrow(sf::st_coordinates(cov)) < nrow(sf::st_coordinates(sf::st_as_sf(terra::as.polygons(lab)))))
ok("tolerance 0 keeps the cell edges exactly",
   abs(sum(as.numeric(sf::st_area(.vftSimplifyCoverage(lab, 0, crs = crs)))) - rawA) < 1)

#a fragment the simplification pinches off joins its neighbour
frag <- sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(1000, 0), c(1000, 1000), c(0, 1000), c(0, 0)))),
                   sf::st_polygon(list(rbind(c(1000, 0), c(1100, 0), c(1100, 300), c(1000, 300), c(1000, 0)))),
                   sf::st_polygon(list(rbind(c(5000, 0), c(5100, 0), c(5100, 100), c(5000, 100), c(5000, 0)))),
                   crs = 2056)
af <- .vftAbsorbFragments(frag, VFT_AOI_MIN_AREA_M2)
ok("a 3 ha fragment joins the area it borders",
   length(af) == 2 && max(as.numeric(sf::st_area(af))) == 1e6 + 3e4)
ok("...an isolated 1 ha one is left for the area filter", min(as.numeric(sf::st_area(af))) == 1e4)

#nothing above the threshold, and one core alone
ok("nothing above the threshold: no areas", nAoI(surface(0.4) * 0, through) == 0)
one <- terra::setValues(grid, exp(-((X - 1500)^2 + (Y - 1500)^2) / (2 * 600^2)))
ok("one core: one area", nAoI(one, through) == 1)

n <- as.integer(Sys.getenv("VFT_SPLIT_REAL", "1"))
if(n > 0){
  cat("\n=== 2. real perimeters, generateAoI2() end to end ===\n")
  pngDir <- Sys.getenv("VFT_SPLIT_PNG", tempdir())
  full <- terra::rast(vftData("maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif"))
  #the committed generateAoI2(), before the split, for the A/B
  old <- new.env(parent = env)
  eval(parse(text = system2("git", c("-C", shQuote(dirname(R)), "show", "HEAD:R/generateAoI2.R"),
                            stdout = TRUE)), envir = old)

  disc <- function(x, y, r) sf::st_sf(geometry = sf::st_transform(
    sf::st_buffer(sf::st_sfc(sf::st_point(c(x, y)), crs = 2056), r), 4326))
  perims <- list(
    filBleu      = sf::st_transform(sf::st_read(vftData("maps/filBleu_area_final.shp"), quiet = TRUE), 4326),
    glatt        = sf::st_transform(sf::st_read(vftData("maps/glattPerimeter_LV95_AREA.shp"), quiet = TRUE), 4326),
    zuerichberg  = disc(2686500, 1249000, 3000),
    sihlwald     = disc(2684500, 1234500, 3500))

  for(nm in names(perims)){
    shape <- perims[[nm]]
    crop  <- terra::crop(full, sf::st_transform(sf::st_as_sfc(sf::st_buffer(
      sf::st_transform(shape, 2056), 1500)), 4326))
    names(crop) <- "walkNat"
    for(q in c(0.5, 0.7)){
      thr <- as.numeric(stats::quantile(terra::values(crop), q, na.rm = TRUE))
      lab <- sprintf("%s q%.0f", nm, 100 * q)
      t0 <- Sys.time()
      new <- suppressWarnings(generateAoI2(thr, shape, walkNat = crop, DULN_all = crop))$polygons
      t1 <- Sys.time()
      off <- suppressWarnings(generateAoI2(thr, shape, walkNat = crop, DULN_all = crop,
                                           split = FALSE))$polygons
      t2 <- Sys.time()
      #the lake loop is off now (VFT_LAKE_LOOP); the committed default had it on
      ref <- suppressWarnings(old$generateAoI2(thr, shape, walkNat = crop, DULN_all = crop,
                                               lakeLoop = FALSE))$polygons

      g   <- sf::st_transform(sf::st_geometry(new), 2056)
      gO  <- sf::st_transform(sf::st_geometry(off), 2056)
      aN  <- as.numeric(sf::st_area(g)); aO <- as.numeric(sf::st_area(gO))
      cat(sprintf("%s: %d areas (largest %.0f ha) vs %d unsplit (largest %.0f ha); %.1f s vs %.1f s\n",
                  lab, length(g), max(c(0, aN)) / 1e4, length(gO), max(c(0, aO)) / 1e4,
                  as.numeric(t1 - t0, units = "secs"), as.numeric(t2 - t1, units = "secs")))

      ok(paste(lab, "split = FALSE is the committed version"),
         isTRUE(all.equal(sf::st_geometry(off), sf::st_geometry(ref))) &&
           isTRUE(all.equal(off$DULN, ref$DULN)))
      ok(paste(lab, "no overlap"), overlapArea(g) < 100, sprintf("(%.1f m2)", overlapArea(g)))
      ok(paste(lab, "every area > 10 ha"), all(aN > VFT_AOI_MIN_AREA_M2 * 0.99))
      #within 1 %, not exactly: the split's outer edge is simplified edge by
      #edge between junctions, the unsplit one ring by ring (see
      #.vftSimplifyCoverage()). At glatt q70 that is +56/-85 ha over ~100 small
      #parts each - corner-cutting noise, not a loss. The exact invariant is on
      #the cells, checked in part 1.
      ok(paste(lab, "covers the same ground (within 1 %)"),
         abs(sum(aN) - sum(aO)) / max(1, sum(aO)) < 0.01,
         sprintf("(%.0f vs %.0f ha)", sum(aN) / 1e4, sum(aO) / 1e4))
      #every perimeter here has at least one run that should split; equal counts
      #would mean a silent fallback to the unsplit areas
      ok(paste(lab, "the split ran (more areas than unsplit)"), length(g) > length(gO))
      ok(paste(lab, "valid in lon/lat and after the round trip"),
         all(sf::st_is_valid(new)) && all(sf::st_is_valid(g)))
      ok(paste(lab, "DULN scored for every area"), !anyNA(new$DULN))

      f <- file.path(pngDir, sprintf("aoi_split_%s_q%.0f.png", nm, 100 * q))
      grDevices::png(f, width = 1600, height = 800)
      graphics::par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))
      cols <- function(k) grDevices::hcl.colors(max(k, 1), "Dark 3")[sample.int(max(k, 1))]
      set.seed(1)
      plot(sf::st_geometry(sf::st_transform(shape, 2056)), border = "grey40",
           main = sprintf("%s: unsplit, %d areas", lab, length(gO)))
      plot(gO, col = cols(length(gO)), border = "black", add = TRUE)
      plot(sf::st_geometry(sf::st_transform(shape, 2056)), border = "grey40",
           main = sprintf("%s: split, %d areas", lab, length(g)))
      plot(g, col = cols(length(g)), border = "black", add = TRUE)
      grDevices::dev.off()
    }
  }
  cat(sprintf("\nPNGs in %s\n", pngDir))
}

cat(sprintf("\n%s\n", if (fails == 0) "ALL PASS" else paste(fails, "FAIL(S)")))
quit(status = if (fails == 0) 0 else 1)
