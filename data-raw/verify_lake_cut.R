## Verification for the lake anchoring of the "cut" split (R/aoiCut.R,
## VFT_AOI_CUT_LAKES): a qualifying lake plus a band round its water is the core
## of an area of its own, which takes the shore land draining to it; the land
## that strays from it is cut off.
##
## Part 1 is synthetic (EPSG:2056, 50 m cells): lakes and land whose answer is
## known, and the anchor rules of .vftMergeBasins().
## Part 2 runs generateAoI2() on filBleu (Greifensee) three ways - the cut
## without lakes, the cut with the old lake-loop pass, the cut with anchored
## lakes - and draws a PNG zoomed on Greifensee.
##
## Run:  Rscript data-raw/verify_lake_cut.R                (both parts)
##       VFT_LAKE_REAL=0 Rscript data-raw/verify_lake_cut.R   (part 1 only)
##       VFT_SPLIT_PNG=<dir>  where the PNG goes (default: tempdir())
suppressPackageStartupMessages({library(sf); library(shiny)})
terra::terraOptions(progress = 0)
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
env <- new.env(parent = globalenv())
for (f in sort(list.files(R, pattern = "[.][Rr]$", full.names = TRUE))) {
  suppressWarnings(try(sys.source(f, envir = env), silent = TRUE))
}
attach(env, warn.conflicts = FALSE)
## the working tree's compiled code, not the installed package
{
  .dll <- file.path(dirname(R), "src", paste0("visitorFlowTool", .Platform$dynlib.ext))
  if(!file.exists(.dll)) stop("compiled code missing: ", .dll)
  dyn.load(.dll)
}

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-70s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
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

cat("=== 1. synthetic ===\n")
crs  <- "EPSG:2056"
o    <- c(2600000, 1200000)
grid <- terra::rast(xmin = o[1], xmax = o[1] + 8000, ymin = o[2], ymax = o[2] + 5000,
                    res = 50, crs = crs)
xy   <- terra::xyFromCell(grid, seq_len(terra::ncell(grid)))
X <- xy[, 1] - o[1]; Y <- xy[, 2] - o[2]
T <- 0.5
disc  <- function(x, y, r) (X - x)^2 + (Y - y)^2 <= r^2
strip <- function(x0, x1, w, y = 2500) X >= x0 & X <= x1 & abs(Y - y) <= w / 2
asD   <- function(m) terra::setValues(grid, ifelse(m, 1, 0))
lakeP <- function(x, y, r) sf::st_sfc(sf::st_buffer(sf::st_point(o + c(x, y)), r), crs = 2056)
labs  <- function(l) terra::values(l, mat = FALSE)
nLab  <- function(l) length(unique(stats::na.omit(labs(l))))
cut   <- function(D, lakes = NULL, ...) vftCutAoI(D, T, lakes = lakes, crs = crs, ...)
anch  <- function(D, lakes) .vftLakeAnchors(D, T, lakes, 50, 50, crs)

#A 2 km lake inside a 200 m attractive shore, and a 1.8 km forest joined to the
#shore by an 800 m bridge - over the 700 m floor, so without the lake anchor
#it all stays one area.
L1    <- lakeP(3000, 2500, 800)
shore <- disc(3000, 2500, 1000)
land  <- shore | strip(3900, 4400, 800) | disc(5200, 2500, 900)
Dl    <- asD(land)
ok("lake + shore + forest on an 800 m bridge, no lakes: one area",  nLab(cut(Dl)) == 1)
lab <- labs(cut(Dl, L1))
ok("...with the lake anchored: two",                                length(unique(stats::na.omit(lab))) == 2)
ok("...the lake and its shore are one area",                        length(unique(lab[shore])) == 1)
#The bridge is FLAT - 800 m wide all along - so it has no narrowest point, and
#the cut falls where it meets the forest's rising flank: a few edge cells of
#the forest go with the bridge. On real land a bridge has a waist.
fl <- lab[disc(5200, 2500, 900)]
ok("...the forest is the other (>= 98 % of it)",
   max(table(fl)) / length(fl) >= 0.98 && lab[disc(5200, 2500, 50)][1] != lab[shore][1],
   sprintf("(%.1f %%)", 100 * max(table(fl)) / length(fl)))

#the band is in the lake's area even where the land is below the threshold
a <- anch(asD(disc(3000, 2500, 800)), L1)
lab <- labs(cut(asD(disc(3000, 2500, 800)), L1))
B <- vftLakeLoopBuffer(as.numeric(sf::st_area(L1)))
ok(sprintf("a lake in unattractive land: its %.0f m band is covered", B),
   all(!is.na(lab[disc(3000, 2500, 800 + B - 1)])) && all(is.na(lab[!disc(3000, 2500, 800 + B + 50)])))
ok("...THE INVARIANT: labelled cells = mask + anchors",
   identical(!is.na(lab), terra::values(asD(disc(3000, 2500, 800)), mat = FALSE) >= T | a > 0))
ok("...one area", length(unique(stats::na.omit(lab))) == 1)

#two lakes whose bands touch are one anchor; far apart, two
two <- function(gap) c(lakeP(2000, 2500, 300), lakeP(2000 + 600 + gap, 2500, 300))
lakesD <- function(gap) asD(disc(2000, 2500, 300) | disc(2000 + 600 + gap, 2500, 300))
ok("two 28 ha lakes 150 m apart: one area",  nLab(cut(lakesD(150), two(150))) == 1)
ok("...800 m apart: two",                    nLab(cut(lakesD(800), two(800))) == 2)

#lakes that do not qualify get no anchor
ok("an 8 ha lake: no anchor",                !any(anch(asD(disc(3000, 2500, 160)), lakeP(3000, 2500, 160)) > 0))
half <- asD(disc(3000, 2500, 800) & X < 2800)          # ~40 % of the lake attractive
ok("a lake 40 % above the threshold: no anchor", !any(anch(half, L1) > 0))
ok("a lake 100 % above the threshold: anchored",  any(anch(asD(disc(3000, 2500, 800)), L1) > 0))
ok("no lakes: no anchor",                     !any(anch(Dl, NULL) > 0))

#an appendix under 25 ha joins the lake; a 60 ha wood on the same neck does not
app <- function(r) asD(shore | strip(3950, 4150, 100) | disc(4150 + r - 30, 2500, r))
ok("a 13 ha appendix on a 100 m neck joins the lake area: one",   nLab(cut(app(200), L1)) == 1)
ok("...a 60 ha wood on the same neck stays apart: two",           nLab(cut(app(440), L1)) == 2)

#.vftMergeBasins() anchors
mb <- function(anchor, join = c(FALSE, TRUE), area = c(1e6, 1e6, 1e6), per = rep(Inf, 3),
               contact = Inf, len = c(100, 900))
  .vftMergeBasins(3L, c(1, 1, 0.9), area, a = c(1L, 1L), b = c(2L, 3L),
                  saddle = c(1, 0.89), crossed = c(TRUE, TRUE), len = len, minThresh = 0,
                  depth = 0.5, neck = Inf, minArea = 2.5e5, per = per, contact = contact,
                  join = join, anchor = anchor)
ok("anchor: the same anchor merges",                      length(unique(mb(c(1L, 1L, 0L)))) == 2)
ok("...an anchored|unanchored border ignores dip and join: stays", mb(c(1L, 1L, 0L))[3] == 3)
ok("...without anchors the same borders merge: one",      length(unique(mb(c(0L, 0L, 0L)))) == 1)
ok("...but the contact rule still joins it",
   length(unique(mb(c(1L, 1L, 0L), per = c(1e4, 1e4, 2000), contact = 0.2))) == 1)
ok("...two different anchors never merge",
   length(unique(mb(c(1L, 1L, 2L), per = c(1e4, 1e4, 2000), contact = 0.2))) == 2)
ok("...an anchored piece under 25 ha is not absorbed",
   length(unique(mb(c(1L, 1L, 2L), area = c(1e6, 1e6, 1e4)))) == 2)
ok("...an unanchored one is",
   length(unique(mb(c(1L, 1L, 0L), area = c(1e6, 1e6, 1e4)))) == 1)

n <- as.integer(Sys.getenv("VFT_LAKE_REAL", "1"))
if(n > 0){
  cat("\n=== 2. filBleu: Greifensee ===\n")
  pngDir <- Sys.getenv("VFT_SPLIT_PNG", tempdir())
  shape  <- sf::st_transform(sf::st_read(vftData("maps/filBleu_area_final.shp"), quiet = TRUE), 4326)
  larger <- sf::st_as_sfc(sf::st_buffer(shape, dist = 1000))
  D <- terra::crop(terra::rast(vftData("maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif")), larger)
  names(D) <- "walkNat"
  buf2056 <- sf::st_union(sf::st_buffer(sf::st_geometry(sf::st_transform(shape, 2056)), 1000))
  lakes <- .vftReadLakesWithin(buf2056)
  gs    <- sf::st_zm(sf::st_geometry(sf::st_transform(lakes[which.max(lakes$SHAPE_Area), ], 2056)))
  gsA   <- as.numeric(sf::st_area(gs))
  secs  <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))
  g2056 <- function(p) sf::st_transform(sf::st_geometry(p), 2056)
  holds <- function(g) { s <- vapply(seq_along(g), function(i)
    sum(as.numeric(sf::st_area(sf::st_intersection(g[i], gs)))), 1) / gsA; c(i = which.max(s), share = max(s)) }

  for(thr in c(5.8, as.numeric(stats::quantile(terra::values(D), c(0.5, 0.7), na.rm = TRUE)))){
    lab <- sprintf("filBleu threshold %.2f", thr)
    t0 <- Sys.time(); off <- suppressWarnings(generateAoI2(thr, shape, walkNat = D, DULN_all = D, method = "cut", cutLakes = FALSE))$polygons; tOff <- secs(t0)
    t0 <- Sys.time(); old <- suppressWarnings(generateAoI2(thr, shape, walkNat = D, DULN_all = D, method = "cut", cutLakes = FALSE, lakeLoop = TRUE))$polygons; tOld <- secs(t0)
    t0 <- Sys.time(); new <- suppressWarnings(generateAoI2(thr, shape, walkNat = D, DULN_all = D, method = "cut"))$polygons; tNew <- secs(t0)
    gO <- g2056(off); gL <- g2056(old); gN <- g2056(new)
    aO <- as.numeric(sf::st_area(gO)) / 1e4; aL <- as.numeric(sf::st_area(gL)) / 1e4; aN <- as.numeric(sf::st_area(gN)) / 1e4
    hO <- holds(gO); hN <- holds(gN)
    cat(sprintf("%s\n  cut, no lakes : %3d areas, %2d under 25 ha, Greifensee in a %4.0f ha area; %.1f s\n",
                lab, length(gO), sum(aO < 25), aO[hO["i"]], tOff))
    cat(sprintf("  + old loop    : %3d areas, %2d under 25 ha, %d lake-loop area(s) %s; %.1f s\n",
                length(gL), sum(aL < 25), sum(old$lakeLoop),
                paste(sprintf("%.0f ha", aL[old$lakeLoop]), collapse = ", "), tOld))
    cat(sprintf("  + anchored    : %3d areas, %2d under 25 ha, Greifensee in a %4.0f ha area (%.0f ha land); %.1f s\n",
                length(gN), sum(aN < 25), aN[hN["i"]], aN[hN["i"]] - gsA / 1e4, tNew))

    ok(paste(lab, "Greifensee is in ONE area holding >= 95 % of it"), hN["share"] >= 0.95,
       sprintf("(%.3f)", hN["share"]))
    ok(paste(lab, "...flagged lakeLoop"), isTRUE(new$lakeLoop[hN["i"]]))
    #Beyond 1 km: under 2 % of the area. Not none - at the south end the
    #attractive Riedikon reed belt / Aabach run on past it (9-14 ha).
    beyond <- function(g, d) sum(as.numeric(sf::st_area(sf::st_difference(g, sf::st_buffer(gs, d))))) / 1e4
    far <- beyond(gN[hN["i"]], 1000)
    ok(paste(lab, "...its land stays near the water (< 2 % beyond 1 km)"),
       far < 0.02 * aN[hN["i"]], sprintf("(%.1f ha beyond)", far))
    #What strays is the land past the shore band - NOT the total: the band adds
    #unattractive shore all round, so at a high threshold, where the cut's
    #holder is already tight, the lake area is larger in total.
    sN <- beyond(gN[hN["i"]], 300); sO <- beyond(gO[hO["i"]], 300)
    ok(paste(lab, "...no more land past 300 m from the water than the cut's holder"), sN <= sO + 5,   # 5 ha: simplification noise, ~6 cells
       sprintf("(%.1f vs %.1f ha)", sN, sO))
    ok(paste(lab, "no more areas under 25 ha than without lakes"), sum(aN < 25) <= sum(aO < 25))
    ok(paste(lab, "no overlap"), overlapArea(gN) < 100, sprintf("(%.1f m2)", overlapArea(gN)))
    ok(paste(lab, "covers at least the ground without lakes (within 1 %)"), sum(aN) >= 0.99 * sum(aO),
       sprintf("(%.0f vs %.0f ha)", sum(aN), sum(aO)))
    ok(paste(lab, "valid in lon/lat and in 2056"), all(sf::st_is_valid(new)) && all(sf::st_is_valid(gN)))
    ok(paste(lab, "DULN scored for every area"), !anyNA(new$DULN))

    #zoomed on Greifensee
    bb  <- sf::st_bbox(sf::st_buffer(gs, 2500))
    ext <- sf::st_as_sfc(bb)
    base <- tryCatch(maptiles::get_tiles(sf::st_transform(ext, 3857), provider = "OpenStreetMap",
                                         zoom = 14, crop = TRUE), error = function(e) NULL)
    pal <- c("#1b9e77","#d95f02","#7570b3","#e7298a","#66a61e","#e6ab02","#a6761d","#1f78b4",
             "#b2df8a","#fb9a99","#fdbf6f","#cab2d6","#6a3d9a","#b15928","#33a02c","#ff7f00")
    f <- file.path(pngDir, sprintf("lake_cut_greifensee_t%.2f.png", thr))
    grDevices::png(f, width = 2400, height = 900, res = 110)
    graphics::par(mfrow = c(1, 3), mar = c(0.3, 0.3, 3, 0.3))
    for(p in list(list(off, "cut, no lakes"), list(old, "cut + old lake-loop pass"),
                  list(new, "cut + anchored lakes"))){
      g <- sf::st_transform(sf::st_geometry(p[[1]]), 3857)
      E <- sf::st_transform(ext, 3857)
      plot(E, border = NA)
      if(!is.null(base)){ terra::plotRGB(base, add = TRUE)
        bbE <- sf::st_bbox(E); graphics::rect(bbE["xmin"], bbE["ymin"], bbE["xmax"], bbE["ymax"],
                                              col = grDevices::adjustcolor("white", 0.45), border = NA) }
      set.seed(2); cols <- pal[sample(rep_len(seq_along(pal), length(g)))]
      plot(g, col = grDevices::adjustcolor(cols, 0.6), border = "grey10", lwd = 1, add = TRUE)
      if(any(p[[1]]$lakeLoop)) plot(g[p[[1]]$lakeLoop], col = NA, border = "#0050c8", lwd = 3, add = TRUE)
      plot(sf::st_transform(gs, 3857), col = NA, border = "#003a8c", lty = 3, lwd = 1, add = TRUE)
      inView <- lengths(sf::st_intersects(g, E)) > 0
      graphics::title(sprintf("%s\n%s: %d areas in view (blue: lake area)", lab, p[[2]], sum(inView)),
                      cex.main = 1.1)
    }
    grDevices::dev.off()
    cat("  PNG:", f, "\n")
  }
}

cat(sprintf("\n%s\n", if (fails == 0) "ALL PASS" else paste(fails, "FAIL(S)")))
quit(status = if (fails == 0) 0 else 1)
