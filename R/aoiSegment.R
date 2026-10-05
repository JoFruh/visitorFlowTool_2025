#Split the thresholded attractiveness raster into destinations.
#
#generateAoI2() used to make one area of interest out of every connected run of
#cells above the threshold. Two forests joined by a thin strip of attractive
#land came out as ONE area - and an area of interest is one destination: the
#agents walk to it, stay in it and go home. This pass splits each run into the
#places a visitor would name separately.
#
#THE RULE, as the user set it:
#  - every high-value core grows out to the threshold (aoi_basins() in
#    src/aoi_segment.cpp - a watershed), so the border between two cores falls
#    on the least attractive line between them;
#  - two neighbouring cores become one area only if the dip in attractiveness
#    between them is shallow (VFT_AOI_SPLIT_DEPTH) AND they are connected: a
#    path CROSSES their border, or the border is WIDE (VFT_AOI_SPLIT_NECK_M).
#    A narrow neck no path crosses separates two places however shallow the
#    dip - there is no getting from one to the other. Paths are a veto, never
#    a reason to merge on their own: the Swiss path network is dense enough
#    that "any path merges" would rebuild the giant area;
#  - the width rule is the user's second round. With the path rule alone, a
#    large block fell apart into many pieces whose long internal borders no
#    path happened to cross. Those are not necks. Widths add up as areas
#    merge, so a short border between two pieces that both border a third
#    becomes a wide one once either joins it;
#  - the contact rule is the user's third round: two pieces whose shared border
#    is more than VFT_AOI_SPLIT_CONTACT of EITHER one's circumference connect,
#    whatever the dip and whatever the paths. Large forests still came out in
#    chunks, kept apart by the dip rule along borders that were a big part of
#    a chunk's outline - a chunk largely wrapped by its neighbour is part of
#    it. Two big forests touching through a neck share a border that is small
#    against both outlines, so they stay apart;
#  - the size rule is the fourth round: a split piece under 25 ha
#    (VFT_AOI_SPLIT_JOIN_M2) joins the neighbour with the shallowest dip, path
#    or not - the appendices left hanging off large areas were 10-50 ha, so the
#    10 ha floor that drops isolated areas was not enough. A piece with no
#    neighbour is untouched by it;
#  - there is no size cap.
#
#Why a watershed finds the necks by itself: DULN_all is BLURRED
#(DULN_nat_majMaxMeanAGGBlur.tif), so a narrow strip between two forests already
#reads lower than either forest's interior. The strip is a saddle.
#
#THE INVARIANT: this changes how the attractive land is divided, never which
#land is covered. Pieces too small to be a destination on their own are joined
#to a neighbour rather than dropped, so the union of the areas is what the
#threshold alone made.

#' Label each cell above the threshold with its area of interest.
#'
#' @param D the attractiveness raster (one layer), already cropped and masked.
#' @param minThresh the step-3 threshold.
#' @param paths2056 path lines (sfc, EPSG:2056) over the same ground.
#' @param depth relative dip at or above which two cores stay apart, path or
#'   not - VFT_AOI_SPLIT_DEPTH.
#' @param neck m: a border at least this long is no neck, and needs no path -
#'   VFT_AOI_SPLIT_NECK_M.
#' @param contact share of either piece's circumference above which their
#'   border connects them outright - VFT_AOI_SPLIT_CONTACT.
#' @param minArea m2 below which a piece is joined to a neighbour -
#'   VFT_AOI_SPLIT_JOIN_M2 (25 ha), not the 10 ha VFT_AOI_MIN_AREA_M2 that drops
#'   isolated areas: a split piece of 10-25 ha hanging off a large area is an
#'   appendix of it, not a destination.
#' @param pathStep spacing, m, of the points the paths are sampled at.
#' @param crs the CRS of `D`, spelled out: terra's own srs can come back empty
#'   on a machine where PROJ_LIB is shadowed.
#' @return a SpatRaster on D's grid, integer area ids 1..k, NA elsewhere.
vftSegmentAoI <- function(D, minThresh, paths2056,
                          depth    = VFT_AOI_SPLIT_DEPTH,
                          neck     = VFT_AOI_SPLIT_NECK_M,
                          contact  = VFT_AOI_SPLIT_CONTACT,
                          minArea  = VFT_AOI_SPLIT_JOIN_M2,
                          pathStep = VFT_AOI_SPLIT_PATH_STEP_M,
                          crs      = "EPSG:4326"){

  #### 1. the basins ####
  b  <- aoi_basins(terra::values(D, mat = FALSE), terra::nrow(D), terra::ncol(D),
                   minThresh)
  nb <- length(b$peak)
  if(nb < 2) return(.vftLabelRaster(D, b$label))

  #cell areas: lon/lat cells shrink northwards, so per cell, not one constant
  ca   <- terra::values(terra::cellSize(D, mask = FALSE, unit = "m"), mat = FALSE)
  inB  <- b$label > 0
  area <- as.numeric(tapply(ca[inB], factor(b$label[inB], levels = seq_len(nb)), sum))
  area[is.na(area)] <- 0

  #border lengths, m: nh shared edges across left/right steps are a cell HIGH,
  #nv across up/down steps a cell WIDE. On lon/lat the height is near constant
  #and the width follows from the mean cell area.
  ll  <- isTRUE(sf::st_is_longlat(sf::st_crs(crs)))
  yM  <- if(ll) terra::yres(D) * 111132 else terra::yres(D)
  xM  <- if(ll) mean(ca, na.rm = TRUE) / yM else terra::xres(D)
  len <- b$nh * yM + b$nv * xM
  #circumference per basin, the same way - holes and the grid's edge included
  per <- b$ph * yM + b$pv * xM

  #### 2. which borders a path crosses ####
  crossed <- .vftPathCrossings(D, b$label, b$a, b$b, paths2056, pathStep, crs)

  #### 3. merge ####
  root <- .vftMergeBasins(nb, b$peak, area, b$a, b$b, b$saddle, crossed, len,
                          minThresh, depth, neck, minArea, per, contact)

  vftDbgCat(sprintf("aoi split: %d basin(s), %d border(s), %d crossed by a path, %d wide -> %d area(s)\n",
                    nb, length(b$a), sum(crossed), sum(len >= neck), length(unique(root))))

  lab <- b$label
  lab[inB] <- match(root, unique(root))[lab[inB]]
  .vftLabelRaster(D, lab)
}

#' The 0-for-nothing label vector back onto D's grid, NA for nothing.
.vftLabelRaster <- function(D, lab){
  lab[lab == 0] <- NA
  out <- terra::setValues(terra::rast(D, nlyrs = 1), lab)
  names(out) <- "aoi"
  out
}

#' TRUE per border (a[i], b[i]) that a path crosses INSIDE the attractive land.
#'
#' Each path is sampled every `pathStep` metres - well under the 73 x 106 m cell
#' - and the basin under each sample read off the label grid. A path crosses the
#' a|b border where one sample is in a and the NEXT is in b. A path that leaves
#' the attractive land and comes back elsewhere goes a -> nothing -> b, which is
#' not a crossing: two forests linked only through the fields between them are
#' two places, which is what the threshold already says.
.vftPathCrossings <- function(D, label, a, b, paths2056, pathStep, crs){
  crossed <- logical(length(a))
  if(!length(a) || is.null(paths2056) || !length(paths2056)) return(crossed)

  #The vertices, and extra points only on the segments longer than `pathStep`.
  #st_segmentize() + st_coordinates() + interaction() did the same in ~14 s at
  #filBleu (55k lines); wk_coords() is C, and the GDB's lines are dense already
  #(390k vertices; segmentising to 20 m only adds 16 %).
  co   <- wk::wk_coords(sf::st_geometry(paths2056))
  n0   <- nrow(co)
  #one run per line part: a segment joins vertex i to i + 1 only inside a run
  same <- c(co$feature_id[-1] == co$feature_id[-n0] & co$part_id[-1] == co$part_id[-n0], FALSE)
  dx   <- c(diff(co$x), 0); dy <- c(diff(co$y), 0)
  k    <- ifelse(same, pmax(1L, as.integer(ceiling(sqrt(dx^2 + dy^2) / pathStep))), 1L)
  idx  <- rep.int(seq_len(n0), k)
  frac <- (sequence(k) - 1) / k[idx]
  xy   <- cbind(co$x[idx] + frac * dx[idx], co$y[idx] + frac * dy[idx])
  run  <- cumsum(c(TRUE, !same[-n0]))[idx]
  ll   <- sf::sf_project(sf::st_crs(2056), sf::st_crs(crs), xy, warn = FALSE)
  cl  <- terra::cellFromXY(D, ll)
  l   <- label[cl]
  l[is.na(l)] <- 0L

  n  <- length(l)
  if(n < 2) return(crossed)
  l1 <- l[-n]; l2 <- l[-1]
  hit <- run[-n] == run[-1] & l1 > 0 & l2 > 0 & l1 != l2
  if(!any(hit)) return(crossed)
  key <- unique(paste(pmin(l1[hit], l2[hit]), pmax(l1[hit], l2[hit])))
  #a diagonal step between basins that only touch at a corner is no border
  paste(a, b) %in% key
}

#' Merge the basins: a root basin per basin.
#'
#' Relative dip of a border = how far the land comes down from the lower of the
#' two cores to the pass between them, as a share of the way from that core down
#' to the threshold. 0 is no dip at all, 1 is all the way down. A share rather
#' than raw DULN units, so one setting means the same at every threshold.
#'
#' A border merges when it is more than `contact` of either area's
#' circumference (`per`), or when the dip is under `depth` and the two are
#' connected: a path crosses it (`crossed`) or it is at least `neck` metres
#' long (`len`).
#'
#' Shallowest first, one merge at a time, because every merge can change what
#' comes next: the merged area's core is the higher of the two, the pass to a
#' third area is the highest of the two areas' passes to it, and the border
#' with it is the two borders added together - so a short contact that only
#' looked like a neck becomes a wide one once its neighbours have merged. The
#' merged area's circumference is the two added, less twice the border between
#' them.
#'
#' A border flagged in `join` merges outright, whatever the rest says. The
#' watershed split passes none; the inverse split (R/aoiCut.R) flags the
#' bridges too wide to cut. When two borders collapse into one, it is joined if
#' either was.
#'
#' `anchor` ties basins to a lake (R/aoiCut.R): 0 for none, k for lake area k.
#' Basins of the same anchor always merge; basins of two different anchors
#' never do; an anchored and an unanchored basin merge only by the contact
#' rule - no dip, no `join`, so the land that strays from a lake is cut off at
#' its border with the lake region however wide that is. The merged basin
#' keeps the anchor. The watershed split and the cut without lakes pass none.
#'
#' Then the pieces under `minArea` - fringes, a bump in a neck - are joined to
#' the neighbour with the shallowest dip, path or not. They cannot be a
#' destination on their own, and dropping them would bite holes in the areas.
#' An anchored piece is never one of them: a lake area stands whatever its size.
.vftMergeBasins <- function(nb, peak, area, a, b, saddle, crossed, len,
                            minThresh, depth, neck, minArea,
                            per = rep(Inf, nb), contact = Inf,
                            join = logical(length(a)),
                            anchor = integer(nb)){
  root <- seq_len(nb)
  find <- function(i){ while(root[i] != i) i <- root[i]; i }

  #The borders between CURRENT areas, one entry per pair, as plain vectors: a
  #data.frame copied whole on every merge cost ~5 s over filBleu's 600 merges.
  #(Not `j` for the joins: merge() below uses i and j for basin ids.)
  s <- saddle; x <- crossed; w <- len; jn <- join; anc <- anchor
  relDip <- function(k){
    lo <- pmin(peak[a[k]], peak[b[k]])
    (lo - s[k]) / pmax(lo - minThresh, 1e-9)
  }
  #share of the SMALLER circumference the border is - the larger of the two
  #shares
  share <- function(k) w[k] / pmax(pmin(per[a[k]], per[b[k]]), 1e-9)

  #fold border k's two areas into one (the lower id) and re-collapse the
  #borders the absorbed one had: a pair that now appears twice keeps the
  #higher pass, either path, and the two lengths added
  merge <- function(k){
    i <- a[k]; j <- b[k]
    root[j] <<- i
    peak[i] <<- max(peak[i], peak[j])
    area[i] <<- area[i] + area[j]
    per[i]  <<- per[i] + per[j] - 2 * w[k]
    anc[i]  <<- max(anc[i], anc[j])
    a[a == j] <<- i
    b[b == j] <<- i
    sw <- a > b
    if(any(sw)){ t <- a[sw]; a[sw] <<- b[sw]; b[sw] <<- t }
    keep <- a != b
    t <- which(keep & (a == i | b == i))
    other <- ifelse(a[t] == i, b[t], a[t])
    if(anyDuplicated(other)){
      g     <- match(other, other)          # the first row of each pair
      first <- t[g == seq_along(t)]
      s[first] <<- as.numeric(tapply(s[t], g, max))
      x[first] <<- as.logical(tapply(x[t], g, any))
      w[first] <<- as.numeric(tapply(w[t], g, sum))
      jn[first] <<- as.logical(tapply(jn[t], g, any))
      keep[t[g != seq_along(t)]] <- FALSE
    }
    a <<- a[keep]; b <<- b[keep]; s <<- s[keep]; x <<- x[keep]; w <<- w[keep]
    jn <<- jn[keep]
  }

  #the rule
  repeat{
    if(!length(a)) break
    k  <- seq_along(a)
    d  <- relDip(k)
    ok <- jn | share(k) > contact | (d < depth & (x | w >= neck))
    if(any(anc > 0)){
      aA <- anc[a]; aB <- anc[b]
      same <- aA > 0 & aA == aB
      ok <- ifelse(aA > 0 | aB > 0,
                   same | ((aA == 0 | aB == 0) & share(k) > contact), ok)
    }
    if(!any(ok)) break
    merge(which(ok)[which.min(d[ok])])
  }

  #the pieces too small to stand alone, smallest first
  repeat{
    if(!length(a)) break
    alive <- unique(c(a, b))
    small <- alive[area[alive] < minArea & anc[alive] == 0]
    if(!length(small)) break
    i   <- small[which.min(area[small])]
    nbr <- which(a == i | b == i)
    merge(nbr[which.min(relDip(nbr))])
  }

  vapply(seq_len(nb), find, integer(1))
}

#' Polygons for the split areas, simplified as ONE coverage.
#'
#' Split areas share borders. st_simplify() on each area alone simplifies a
#' shared border twice, once per side, and the two versions cross: overlaps on
#' one side, gaps on the other. (Patching that afterwards - clipping, overlaps to
#' one side, gaps to the other - cost 8 s at filBleu, still lost ~0.2 % of the
#' ground, and left near-touches that the lon/lat transform turned into
#' self-intersections GEOS measured as 262 ha of "overlap".)
#'
#' So the BORDERS are simplified, not the areas:
#'   1. every area boundary, unioned: each shared border once, noded where three
#'      areas (or an area and the outside) meet;
#'   2. merged into edges between those junctions and simplified as ONE
#'      geometry with preserveTopology - GEOS keeps every edge's endpoints and
#'      lets no two edges cross;
#'   3. polygonised back into faces, each face labelled by the area under it
#'      (faces in a hole of the mask find no label and go);
#'   4. faces dissolved per area; parts under `minArea` that simplification
#'      pinched off join the neighbour they share the most border with.
#' Neighbours share their simplified border exactly, so there is nothing to
#' patch. The outer edge is simplified edge by edge rather than ring by ring,
#' so it is not bit-identical to the unsplit areas', but within the same
#' tolerance (0.1-0.3 % of the area on the perimeters checked).
#'
#' @param labels the label raster from vftSegmentAoI().
#' @param tolerance metres, VFT_AOI_TOLERANCE_M.
#' @param minArea m2, VFT_AOI_MIN_AREA_M2.
#' @param crs the CRS of `labels`, spelled out (see vftSegmentAoI()).
#' @return sfc of POLYGONs, EPSG:2056, disjoint.
.vftSimplifyCoverage <- function(labels, tolerance = VFT_AOI_TOLERANCE_M,
                                 minArea = VFT_AOI_MIN_AREA_M2,
                                 crs = "EPSG:4326"){
  P <- terra::as.polygons(labels, aggregate = TRUE, na.rm = TRUE)
  P <- sf::st_set_crs(sf::st_set_crs(sf::st_geometry(sf::st_as_sf(P)), NA), sf::st_crs(crs))
  P <- sf::st_transform(P, 2056)
  if(!length(P)) return(P)

  #1. + 2.
  edges <- sf::st_line_merge(sf::st_union(sf::st_cast(sf::st_boundary(P), "MULTILINESTRING")))
  if(is.finite(tolerance) && tolerance > 0)
    edges <- sf::st_simplify(edges, dTolerance = tolerance, preserveTopology = TRUE)

  #3.
  faces <- suppressWarnings(sf::st_collection_extract(sf::st_polygonize(edges), "POLYGON"))
  if(!length(faces)) return(faces)
  pts <- sf::st_transform(sf::st_point_on_surface(faces), sf::st_crs(crs))
  lab <- terra::extract(labels, terra::vect(pts))[, 2]
  keep <- !is.na(lab)

  #4.
  areas <- do.call(c, lapply(split(which(keep), lab[keep]), function(ix)
    sf::st_union(faces[ix])))
  areas <- .vftPolygonParts(areas)
  .vftAbsorbFragments(areas, minArea)
}

#' Parts under `minArea` join the neighbour they share the most border with,
#' smallest first. One touching nobody, or only at a corner, is left for
#' generateAoI2()'s area filter - the unsplit areas lose it the same way.
.vftAbsorbFragments <- function(g, minArea){
  repeat{
    if(length(g) < 2) break
    a     <- as.numeric(sf::st_area(g))
    small <- which(a < minArea)
    if(!length(small)) break
    touch <- sf::st_intersects(g[small], g)
    merged <- FALSE
    for(s in order(a[small])){
      i    <- small[s]
      cand <- setdiff(touch[[s]], i)
      if(!length(cand)) next
      edge <- sf::st_boundary(g[i])
      len  <- vapply(cand, function(j)
        sum(as.numeric(sf::st_length(sf::st_intersection(edge, g[j])))), numeric(1))
      if(max(len) <= 0) next
      j <- cand[which.max(len)]
      g[j] <- sf::st_union(c(g[j], g[i]))
      g <- g[-i]
      merged <- TRUE
      break
    }
    if(!merged) break
  }
  .vftPolygonParts(g)
}

#' Repair, on the sphere, the areas the lon/lat transform made invalid.
#'
#' Valid in EPSG:2056 is not valid in lon/lat under s2: an edge of a few
#' hundred metres becomes a geodesic, and where the coverage's simplified
#' borders pass within centimetres of each other - at a junction, or where a
#' neck pinches - two of them cross ("Edge 2 crosses edge 4", two areas at
#' filBleu q70). The app runs with s2 on, so every later st_intersects() on
#' such an area could fail. Only the invalid ones are touched; a repair that
#' splits an area keeps its largest part, the rest being slivers, so there is
#' still one POLYGON per area and the caller's per-area vectors stay aligned.
.vftS2Valid <- function(g){
  bad <- which(!sf::st_is_valid(g))
  for(i in bad){
    v <- sf::st_make_valid(g[i])
    if(any(sf::st_geometry_type(v) == "GEOMETRYCOLLECTION"))
      v <- suppressWarnings(sf::st_collection_extract(v, "POLYGON"))
    v <- suppressWarnings(sf::st_cast(sf::st_cast(v, "MULTIPOLYGON"), "POLYGON"))
    if(!length(v)) next
    g[i] <- v[which.max(as.numeric(sf::st_area(v)))]
  }
  g
}
