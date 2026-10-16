#### Heatwave simulations: the heat maps reshape attractivity ####

# Step 5's "Hitzewellen-Simulation" switch runs the ABM on a scenario's heat map
# from the Hitzeminderung page (heatRaster() in R/heat_helpers.R: kelvin of PET
# against unshaded grass at solar midday, EPSG:2056, 5 m). The model does not get
# a second decision rule for heat. Heat and attractivity become ONE quantity, the
# heat-weighted attractivity
#
#   DULN_hw = DULN - kappa * dPET
#
# for each of the seven per-type DULN columns, and DULN_hw is then used exactly
# where and how DULN is used in a normal run: cooler raises attractivity, hotter
# lowers it.
#
#   edge   dPET = mean under points every half cell  -> the 21 attractivity-
#          along the line
#          weighted distances (vftAttrWeights()): the way to and from an area,
#          and the distances behind the area choice (reach rule, travel time)
#   node   dPET = mean of the cells within nodeRadius -> the junction choice
#          inside an area (chooseBestRoutes_cpp() reads node DULN)
#   area   dPET = mean of the cells in the polygon    -> finalPolygons$DULN, the
#          destination utility's attractivity term
#   car park  its nearest area's DULN_hw              -> parkingAttr, the weight
#          of a car park as a starting point
#
# and, in a heatwave run only, attractivity weighs more against proximity:
#
#   attrWeight  multiplies the destination utility's attractivity coefficient,
#               U = attrWeight * 3.23695 * STD(X) - 6.65703 * STD(Y)
#               (findClosestAOI_cpp(); 1 is the calibrated model)
#   legFactor   the strength f of the "" weights, the way to and from an area
#               (8 normally; 4 is the existing "_attr" strength)
#
# The heated values exist only inside the job. The adjacency lists, the agents'
# DULN table and the areas are built from COPIES; the graph and the pathUsage
# that go back to the app carry the ordinary DULN and weights.
#
# THE NUMBERS ARE UNCALIBRATED ASSUMPTIONS, chosen so that heat matters about as
# much as attractivity does:
#   - kappa = 1 point per kelvin. In the junction choice one DULN point is a
#     factor e^(1/0.5) = 7.4 in the odds, so a path 3 K hotter is avoided like a
#     path 3 points less attractive. Afternoon asphalt sits ~10 K above sunny
#     grass, deep shade ~5 K below it (HEAT_COEFFICIENTS.md), against an edge
#     DULN range of roughly 18-41.
#   - attrWeight = 1.5, legFactor = 4.
# kappa = 0, attrWeight = 1 and legFactor = 8 give a run identical to a normal
# one; data-raw/verify_heatwave.R checks exactly that.
#
# Uncovered elements (the map ends 250 m beyond the perimeter) take the MEDIAN
# dPET of the covered ones of their kind: 0 would mean "sunny grass at noon", not
# "unknown".

#' The time-of-day bins step 5 offers. A LITERAL, not a subset of HEAT_BINS:
#' shadow_helpers.R, which defines that, is collated after this file.
VFT_HEATWAVE_BINS <- c("morning", "afternoon")

#' The heatwave model's parameters. See the header of this file.
VFT_HEATWAVE <- list(kappa = 1, attrWeight = 1.5, legFactor = 4, nodeRadius = 10)

#' The normal-run strengths of the three weightings ("", "_attr", "_ATTR").
VFT_ATTR_FACTORS <- c(8, 4, 2)

#' Mean dPET of every edge, node and area under a heat map.
#'
#' Pure; runs in the daemon. Rasters never leave it: the result is plain vectors.
#'
#' @param edgeTable,vertexTable the graph as tables (vftGraphTibble()), carrying
#'   their sf geometry. Edge rows and node rows come back in the same order.
#' @param finalPolygons the areas of interest, an sf with an `AOI` column.
#' @param heat a SpatRaster (unwrapped), the scenario's heat map.
#' @param params VFT_HEATWAVE or a variant of it.
#' @return list(edge, node, area, coverage, range) - `area` named by AOI letter,
#'   `coverage` the covered share of each kind, `range` the dPET range seen.
vftHeatwaveExposure <- function(edgeTable, vertexTable, finalPolygons, heat,
                                params = VFT_HEATWAVE){
  crs <- sf::st_crs(terra::crs(heat))
  res <- terra::res(heat)[1]

  geomOf <- function(tbl){
    g <- tbl[[which(vapply(tbl, inherits, logical(1), what = "sfc"))[1]]]
    if(is.null(g)) stop("vftHeatwaveExposure: no geometry column")
    if(is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
    sf::st_transform(sf::st_zm(g), crs)
  }
  #POINT LOOKUPS, NOT terra::extract() ON GEOMETRIES. extract() of lines and of
  #node buffers took 44 s on a 4.5k-edge network; the cells under points are a
  #vectorised index into the grid's values, read out once.
  hv <- terra::values(heat, mat = FALSE)
  at <- function(xy) hv[terra::cellFromXY(heat, xy)]
  #mean of `v` per `id` in 1..n, NA where an id has no value
  meanBy <- function(v, id, n){
    ok <- !is.na(v)
    s  <- tabulate(id[ok], nbins = n)
    m  <- rep(NA_real_, n)
    m[s > 0] <- rowsum(v[ok], id[ok], reorder = TRUE)[, 1] / s[s > 0]
    m
  }
  fill <- function(x){
    x[!is.finite(x)] <- NA_real_
    cov <- if(length(x)) mean(!is.na(x)) else 0
    if(cov > 0) x[is.na(x)] <- stats::median(x, na.rm = TRUE)
    list(x = x, cov = cov)
  }

  #edges: the cells under points every half cell along the line, endpoints
  #included - a length-weighted mean of what the walker crosses
  eg  <- sf::st_segmentize(geomOf(edgeTable), dfMaxLength = res / 2)
  ec  <- sf::st_coordinates(eg)
  eid <- as.integer(ec[, ncol(ec)])          #the feature index, LINESTRING or MULTI
  e <- fill(meanBy(at(ec[, c("X", "Y")]), eid, length(eg)))
  if(e$cov == 0) stop("the heat map does not cover the path network")

  #nodes: the cells within nodeRadius, as a stencil of offsets round the node
  nc  <- sf::st_coordinates(geomOf(vertexTable))[, c("X", "Y"), drop = FALSE]
  k   <- seq(-ceiling(params$nodeRadius / res), ceiling(params$nodeRadius / res)) * res
  off <- as.matrix(expand.grid(dx = k, dy = k))
  off <- off[off[, 1]^2 + off[, 2]^2 <= params$nodeRadius^2, , drop = FALSE]
  nn  <- nrow(nc)
  nxy <- cbind(rep(nc[, 1], each = nrow(off)) + off[, 1],
               rep(nc[, 2], each = nrow(off)) + off[, 2])
  n <- fill(meanBy(at(nxy), rep(seq_len(nn), each = nrow(off)), nn))

  #areas: the cells whose centre lies in the polygon (rasterize, as extract()
  #would choose them), averaged per zone
  pv <- terra::vect(sf::st_sf(AOI = seq_len(nrow(finalPolygons)),
                              geometry = sf::st_transform(sf::st_zm(sf::st_geometry(finalPolygons)), crs)))
  zr <- terra::rasterize(pv, heat, field = "AOI")
  zv <- terra::values(zr, mat = FALSE)
  okz <- !is.na(zv)
  a <- fill(meanBy(hv[okz], as.integer(zv[okz]), nrow(finalPolygons)))
  #an area the map misses entirely takes the edges' median: there is no other
  #area value to fall back on when it is the only one
  if(a$cov == 0) a$x[] <- stats::median(e$x)
  names(a$x) <- as.character(finalPolygons$AOI)

  list(edge = e$x, node = n$x, area = a$x,
       coverage = c(edge = e$cov, node = n$cov, area = a$cov),
       range = range(c(e$x, n$x, a$x)))
}

#' `tbl` with its seven DULN columns heat-weighted: DULN - kappa * dPET.
#' Columns it does not carry are left out rather than created.
vftHeatwaveDULN <- function(tbl, dPET, kappa){
  for(col in intersect(VFT_DULN_TYPE_COLS, names(tbl)))
    tbl[[col]] <- tbl[[col]] - kappa * dPET
  tbl
}

#' A car park's weight as a starting point, heat-weighted.
#'
#' parkingAttr is the DULN of the area nearest the car park (vftPrepareNetwork());
#' here it loses kappa * that area's dPET, the area being the one nearest the
#' car-park NODE. Clamped at 0: it is a sampling weight.
vftHeatwaveParkingAttr <- function(vertexTable, finalPolygons, areaDPET, kappa){
  pa <- vertexTable$parkingAttr
  if(is.null(pa) || isTRUE(kappa == 0)) return(pa)
  pk <- vertexTable$parking
  at <- which(pk > 0 & is.finite(pk))
  if(!length(at) || !nrow(finalPolygons)) return(pa)
  g    <- vertexTable[[which(vapply(vertexTable, inherits, logical(1), what = "sfc"))[1]]]
  if(is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
  polys <- sf::st_transform(sf::st_zm(sf::st_geometry(finalPolygons)), 2056)
  near  <- sf::st_nearest_feature(sf::st_transform(g[at], 2056), polys)
  pa[at] <- pmax(pa[at] - kappa * unname(areaDPET[near]), 0)
  pa
}

#' What a stored heatwave result says about the run that made it: plain numbers,
#' so it survives the save file.
vftHeatwaveSummary <- function(exposure, params){
  list(kappa = params$kappa, attrWeight = params$attrWeight,
       legFactor = params$legFactor, nodeRadius = params$nodeRadius,
       coverage = exposure$coverage, range = exposure$range,
       edgeMean = mean(exposure$edge))
}

#' The heatwave record of a scenario's CURRENT result, or NULL.
#'
#' The one rule every reader uses: a record only describes a pathUsage that is
#' still there. The sites that drop a scenario's result do not all know about this
#' field, and need not.
vftScenarioHeatwave <- function(sc){
  if(is.null(sc) || is.null(sc$pathUsage)) return(NULL)
  hw <- sc$heatwave
  if(is.null(hw) || is.null(hw$bin)) NULL else hw
}

#' "Hitzewelle (Nachmittag)", in the Translator's language. Plain text.
vftHeatwaveTag <- function(i18n, bin){
  sprintf(vftTrText(i18n, "Hitzewelle (%s)"), vftTrText(i18n, HEAT_BIN_KEYS[[bin]]))
}
