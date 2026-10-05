#Split the thresholded attractiveness raster into destinations - the INVERSE way.
#
#R/aoiSegment.R builds destinations bottom-up: high-value cores of DULN_all
#grow out to the threshold and merge back by dip and paths. This file is the
#candidate it is being compared against (data-raw/compare_aoi_split.R), chosen
#by VFT_AOI_SPLIT_METHOD = "cut": start from the FINISHED threshold areas and
#cut them where a narrow bridge joins two larger bodies. It looks at the shape
#of the areas only - never at the attractiveness inside them, never at paths,
#so it skips the ~8 s read of the paths GDB.
#
#HOW: a distance transform of the threshold mask. Every cell inside gets its
#distance, in metres, to the nearest cell outside (aoi_edt() in
#src/aoi_segment.cpp). A body's interior peaks at the radius of the largest
#circle that fits in it; a bridge is a saddle at half its width. The SAME
#watershed the other split runs on DULN_all (aoi_basins()) then runs on that
#surface, so each body is a basin and the border between two falls across the
#narrowest cells of the bridge. The SAME merge (.vftMergeBasins()) then
#rejoins them, with:
#  - minThresh = 0, so the relative dip of a border is 1 - saddle / lower peak:
#    1 - the bridge's width over the narrower body's. A bridge more than
#    (1 - VFT_AOI_CUT_DEPTH) as wide as the narrower body is no bridge;
#  - every border "crossed": no path veto - the dip alone decides;
#  - a bridge at least VFT_AOI_CUT_MAX_BRIDGE_M wide is never cut, whatever the
#    dip. The dip is relative, so two very wide bodies were cut apart at a
#    waist ~1 km wide (Zuerichberg q50) - narrow against the bodies, but no
#    bridge a visitor would notice. The width is twice the saddle: twice the
#    distance from the bridge's middle to its edge, exact to within a cell;
#  - the contact rule (VFT_AOI_SPLIT_CONTACT): a distance transform peaks in
#    every bulge of a wavy outline, and those pieces share a border long
#    against their own outline, so they rejoin;
#  - the size rule (VFT_AOI_SPLIT_JOIN_M2): a piece under 25 ha joins its
#    neighbour.
#
#LAKES (VFT_AOI_CUT_LAKES). DULN_all is high over water, so a lake sits inside
#whatever area surrounds it - at Greifensee (filBleu, threshold 5.8) one
#1685 ha area: the 823 ha lake and land up to 1.7 km from it. A lake that can
#be walked around is a destination of its own. The old lake-loop pass
#(R/lakeLoopAoI.R) cut the path loop round the lake out of that area, and
#left the rest in scraps along the loop. Here the lake is ANCHORED instead:
#  - a qualifying lake (VFT_AOI_LAKE_MIN_M2, and more than VFT_LAKE_LOOP_SHARE
#    of its cells above the threshold) and a band round its water,
#    vftLakeLoopBuffer() wide, are always in its area - the band even where the
#    land is below the threshold, so the shore walk is never cut off. Lakes
#    whose bands touch (the two Katzensee basins) are one anchor;
#  - the anchor is one plateau above every distance in the transform, so the
#    watershed grows the lake area out from the shore over the land that
#    drains to it, and stops where a body of land has a core of its own. At
#    Greifensee that land reaches 550-680 m from the water at most;
#  - the border between the lake area and such a body is never merged by dip
#    or bridge width: the bodies round a lake meet it on bridges nearly as wide
#    as themselves, so a shape rule cannot tell the hinterland from the shore.
#    Only the contact rule (a piece mostly wrapped by the lakeside) and the
#    size rule (under 25 ha, so no scraps) still join a piece to the lake.
#
#THE INVARIANT is the other split's: the cells covered are exactly the cells
#above the threshold - plus, with lakes, the lakes' bands; only how they are
#divided changes.

#' Label each cell above the threshold with its area of interest, cutting the
#' threshold areas at narrow bridges.
#'
#' @param D the attractiveness raster (one layer), already cropped and masked.
#' @param minThresh the step-3 threshold.
#' @param depth a border whose relative dip (1 - bridge width / narrower body
#'   width) is at or above this stays cut - VFT_AOI_CUT_DEPTH.
#' @param bridge m: a bridge at least this wide is never cut -
#'   VFT_AOI_CUT_MAX_BRIDGE_M. Inf turns the floor off.
#' @param contact see vftSegmentAoI() - VFT_AOI_SPLIT_CONTACT.
#' @param minArea m2 below which a piece joins a neighbour -
#'   VFT_AOI_SPLIT_JOIN_M2.
#' @param lakes the lakes (sf or sfc, any CRS) to anchor, or NULL for none. Only
#'   the qualifying ones are used - see .vftLakeAnchors().
#' @param crs the CRS of `D`, spelled out (see vftSegmentAoI()).
#' @return a SpatRaster on D's grid, integer area ids 1..k, NA elsewhere.
vftCutAoI <- function(D, minThresh,
                      depth   = VFT_AOI_CUT_DEPTH,
                      bridge  = VFT_AOI_CUT_MAX_BRIDGE_M,
                      contact = VFT_AOI_SPLIT_CONTACT,
                      minArea = VFT_AOI_SPLIT_JOIN_M2,
                      lakes   = NULL,
                      crs     = "EPSG:4326"){

  v  <- terra::values(D, mat = FALSE)
  nr <- terra::nrow(D); nc <- terra::ncol(D)

  #cell sizes in metres, as vftSegmentAoI() takes them
  ca <- terra::values(terra::cellSize(D, mask = FALSE, unit = "m"), mat = FALSE)
  ll <- isTRUE(sf::st_is_longlat(sf::st_crs(crs)))
  yM <- if(ll) terra::yres(D) * 111132 else terra::yres(D)
  xM <- if(ll) mean(ca, na.rm = TRUE) / yM else terra::xres(D)

  #### 1. the distance surface, and its basins ####
  inside <- !is.na(v) & v >= minThresh
  anchor <- .vftLakeAnchors(D, minThresh, lakes, xM, yM, crs)
  if(any(anchor > 0)) inside <- inside | anchor > 0
  dist   <- aoi_edt(inside, nr, nc, xM, yM)
  #the anchors: one plateau above everything, so the lake areas grow out from
  #their shores
  if(any(anchor > 0)) dist[anchor > 0] <- max(dist, na.rm = TRUE) + 1
  b  <- aoi_basins(dist, nr, nc, 0)
  nb <- length(b$peak)
  if(nb < 2) return(.vftLabelRaster(D, b$label))

  #a basin holding anchor cells carries that anchor; no basin holds two (two
  #anchors never touch, and the plateau is swept before any other cell)
  ancB <- integer(nb)
  if(any(anchor > 0)) ancB[b$label[anchor > 0]] <- anchor[anchor > 0]

  inB  <- b$label > 0
  area <- as.numeric(tapply(ca[inB], factor(b$label[inB], levels = seq_len(nb)), sum))
  area[is.na(area)] <- 0
  len <- b$nh * yM + b$nv * xM
  per <- b$ph * yM + b$pv * xM

  #### 2. merge: the dip alone, no path veto, wide bridges never cut ####
  wide <- 2 * b$saddle >= bridge
  root <- .vftMergeBasins(nb, b$peak, area, b$a, b$b, b$saddle,
                          crossed = rep(TRUE, length(b$a)), len = len,
                          minThresh = 0, depth = depth, neck = Inf,
                          minArea = minArea, per = per, contact = contact,
                          join = wide, anchor = ancB)

  vftDbgCat(sprintf("aoi cut: %d body/bodies, %d border(s), %d too wide to cut, %d lake anchor(s) -> %d area(s)\n",
                    nb, length(b$a), sum(wide), length(unique(anchor[anchor > 0])),
                    length(unique(root))))

  lab <- b$label
  lab[inB] <- match(root, unique(root))[lab[inB]]
  .vftLabelRaster(D, lab)
}

#' The lake anchors on D's grid: per cell, 0 or the id of the anchor it is in.
#'
#' A lake qualifies when it is at least `minLake` m2 and more than `share` of
#' its cells are above the threshold - attractive enough to be a destination.
#' Its anchor is its cells (centre in the water) plus the band round it: the
#' cells whose centre is within vftLakeLoopBuffer() of a lake cell's centre,
#' plus half a cell so the band is measured from the water's edge rather than
#' from the middle of the outermost lake cell (else a lake under ~30 ha, whose
#' band is under a cell wide, would get no band at all). Cells outside the
#' perimeter (NA in D) are never part of one. Anchors are the 4-connected runs
#' of those cells, so lakes whose bands touch share one.
#'
#' @param lakes sf or sfc, any CRS; NULL or empty for none.
#' @param xM,yM cell width and height in metres.
.vftLakeAnchors <- function(D, minThresh, lakes, xM, yM, crs = "EPSG:4326",
                            minLake = VFT_AOI_LAKE_MIN_M2,
                            share   = VFT_LAKE_LOOP_SHARE,
                            buffer  = vftLakeLoopBuffer){
  nr <- terra::nrow(D); nc <- terra::ncol(D)
  out <- integer(nr * nc)
  if(is.null(lakes) || !length(sf::st_geometry(lakes))) return(out)

  g <- sf::st_zm(sf::st_geometry(lakes))
  g <- g[as.numeric(sf::st_area(g)) >= minLake]
  if(!length(g)) return(out)
  g <- sf::st_set_crs(sf::st_set_crs(sf::st_transform(g, sf::st_crs(crs)), NA), sf::st_crs(crs))

  v    <- terra::values(D, mat = FALSE)
  open <- !is.na(v)
  id   <- terra::values(terra::rasterize(terra::vect(sf::st_sf(id = seq_along(g), geometry = g)),
                                         D, field = "id"), mat = FALSE)
  #the grid's edge must not read as water: pad by more than the widest band
  maxB <- buffer(max(as.numeric(sf::st_area(g)))) + max(xM, yM)
  p    <- ceiling(maxB / min(xM, yM)) + 1
  padded <- function(x){
    m <- matrix(TRUE, nr + 2 * p, nc + 2 * p)
    m[p + seq_len(nr), p + seq_len(nc)] <- matrix(x, nr, nc, byrow = TRUE)
    as.vector(t(m))
  }
  inner <- as.vector(t(matrix(seq_len((nr + 2 * p) * (nc + 2 * p)), nr + 2 * p, nc + 2 * p,
                              byrow = TRUE)[p + seq_len(nr), p + seq_len(nc)]))

  cells <- logical(nr * nc)
  for(k in seq_along(g)){
    lk <- !is.na(id) & id == k & open
    if(!any(lk)) next
    if(mean(v[lk] >= minThresh, na.rm = TRUE) <= share) next
    d <- aoi_edt(padded(!lk), nr + 2 * p, nc + 2 * p, xM, yM)[inner]
    B <- buffer(as.numeric(sf::st_area(g[k]))) + max(xM, yM) / 2
    cells <- cells | lk | (open & !is.na(d) & d <= B)
  }
  if(!any(cells)) return(out)
  r <- terra::patches(terra::setValues(terra::rast(D, nlyrs = 1), ifelse(cells, 1, NA)),
                      directions = 4)
  pv <- terra::values(r, mat = FALSE)
  out[!is.na(pv)] <- as.integer(factor(pv[!is.na(pv)]))
  out
}
