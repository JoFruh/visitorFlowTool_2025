#### biodiversity-recreation conflict hotspots (step 5) ####
#
# Where do the most sensitive places and the most used paths coincide? Step 5's
# "find conflict" button asks this of the selected scenario's simulation and the
# sensitivity matrix, and draws one circle per cluster of answers.
#
# Pure function, no Shiny: data-raw/verify_conflict.R drives it headless.
#
# NO `%in%` ON A SpatRaster ANYWHERE HERE. The package imports nothing from
# terra, so terra's `%in%` generic is invisible inside the namespace and base's
# match() refuses a raster - while every data-raw script, which attaches terra,
# passes. Everything below works on plain vectors once values() has been taken.

#### stored conflicts: which matrix, which simulation ####
#
# Step 5 keeps each search on its scenario, as r$networkList[[i]]$conflicts =
# list(hotspots = <the data.frame below>, smKey = vftConflictKey(<matrix>)),
# and the newVersions page shows it again. A stored search is only true while
# both of its inputs are the ones it was computed from:
#
#   * the SIMULATION. Every edit on the newVersions page NULLs the scenario's
#     pathUsage, and a re-simulation in step 5 clears `conflicts` in the same
#     write that stores the new pathUsage. So "pathUsage is present" is exactly
#     "this is the simulation the search saw", with no need to clear
#     `conflicts` at each of the page's many edit handlers.
#   * the MATRIX. Going back to step 2 does NOT discard simulations (see the
#     note on pathUsage in VFT_DERIVED_FROM, R/providers.R), so a changed
#     species selection leaves the simulation standing and the search stale.
#     Hence the fingerprint.
#
# Plain numbers in the fingerprint, not the SpatRaster: networkList goes into
# the save file, and a SpatRaster there would be a dead pointer on restore.

#' A fingerprint of a sensitivity matrix: its grid and two sums over its values.
#' Cheap (terra's C++ global()), and stable across wrap()/unwrap() and a save,
#' which a pointer comparison is not. NULL for no matrix.
vftConflictKey <- function(sm){
  if(is.null(sm)) return(NULL)
  if(inherits(sm, "PackedSpatRaster")) sm <- terra::unwrap(sm)
  if(terra::nlyr(sm) > 1) sm <- sm[[1]]
  g <- terra::global(sm, c("sum", "sd"), na.rm = TRUE)
  c(terra::nrow(sm), terra::ncol(sm), as.vector(terra::ext(sm)),
    as.numeric(g[1, 1]), as.numeric(g[1, 2]))
}

#' The conflicts stored on a scenario, if they still describe it - else NULL.
#' `smKey` is vftConflictKey() of the matrix in force now. A search that found
#' nothing is stored too (zero rows) and also comes back NULL: there is nothing
#' to show.
vftScenarioConflicts <- function(scenario, smKey){
  h <- vftStoredConflicts(scenario, smKey)
  if(is.null(h) || !nrow(h)) return(NULL)
  h
}

#' Like vftScenarioConflicts(), but a valid search that found nothing comes
#' back as its zero-row data.frame rather than NULL - so NULL means "never
#' searched, or searched on something else", and a caller that would run the
#' search itself (the newVersions page's Original button) can tell that apart
#' from "searched, none".
vftStoredConflicts <- function(scenario, smKey){
  if(!is.list(scenario) || is.null(scenario$pathUsage)) return(NULL)
  cf <- scenario$conflicts
  if(!is.list(cf) || is.null(cf$hotspots) || is.null(smKey)) return(NULL)
  if(!isTRUE(all.equal(cf$smKey, smKey))) return(NULL)
  cf$hotspots
}

#' Find the clusters of cells where sensitivity AND path usage are both highest.
#'
#' A cell qualifies when its sensitivity is in the top (1 - p) of the matrix's
#' non-zero cells AND the heaviest path crossing it is in the top (1 - p) of the
#' non-zero path cells. `probs` is tried in order - 10%, then 20%, then 30% - and
#' the first level that finds anything is used, so a landscape whose extremes
#' never meet still gets its nearest misses rather than nothing.
#'
#' Qualifying cells closer than `clusterDist` metres to each other form one
#' cluster, and each cluster becomes one circle: the minimum bounding circle of
#' its cell centres, widened by half a cell diagonal so the cells themselves sit
#' inside it. Clusters are ranked by the sum over their cells of
#' (sm / max sm) * (usage / max usage), and the `maxCircles` strongest are kept.
#'
#' @param sm the sensitivity matrix, a SpatRaster or PackedSpatRaster.
#' @param edges the simulation's edge table, sf LINESTRINGs in any CRS.
#' @param usageCol the edge column holding path usage.
#' @return a data.frame, one row per circle, strongest first: `lng`, `lat`
#'   (EPSG:4326), `radius_m`, `score`, `nCells`, and `level` (the top share
#'   used, 0.1 for top 10%). Zero rows when nothing qualifies at any level.
vftConflictHotspots <- function(sm, edges, usageCol = "passage",
                                probs = c(0.9, 0.8, 0.7),
                                clusterDist = 250, maxCircles = 5){
  none <- data.frame(lng = numeric(0), lat = numeric(0), radius_m = numeric(0),
                     score = numeric(0), nCells = integer(0), level = numeric(0))

  if(is.null(sm) || is.null(edges) || !nrow(edges)) return(none)
  if(inherits(sm, "PackedSpatRaster")) sm <- terra::unwrap(sm)
  if(terra::nlyr(sm) > 1) sm <- sm[[1]]

  #the usage on the SM's own grid: the heaviest path through each cell, in one
  #C++ pass rather than an extract per edge
  #step 5's passage table is already flat; st_zm on 50k lines is 0.6 s of nothing
  if(!is.null(sf::st_z_range(edges)) || !is.null(sf::st_m_range(edges)))
    edges <- sf::st_zm(edges, drop = TRUE, what = "ZM")
  edges <- sf::st_transform(edges, terra::crs(sm))
  edges <- edges[!is.na(edges[[usageCol]]), ]
  if(!nrow(edges)) return(none)
  useR <- terra::rasterize(terra::vect(edges), sm, field = usageCol, fun = "max")

  smV  <- terra::values(sm,   mat = FALSE)
  useV <- terra::values(useR, mat = FALSE)

  smPos  <- smV[is.finite(smV) & smV > 0]
  usePos <- useV[is.finite(useV) & useV > 0]
  if(!length(smPos) || !length(usePos)) return(none)

  ok <- is.finite(smV) & smV > 0 & is.finite(useV) & useV > 0
  hot <- integer(0)
  level <- NA_real_
  for(p in probs){
    qS <- stats::quantile(smPos,  p, names = FALSE)
    qU <- stats::quantile(usePos, p, names = FALSE)
    hot <- which(ok & smV >= qS & useV >= qU)
    if(length(hot)){
      level <- 1 - p
      break
    }
  }
  if(!length(hot)) return(none)

  #metres for the clustering, whatever the matrix is stored in
  lonlat <- isTRUE(terra::is.lonlat(sm))
  xy  <- terra::xyFromCell(sm, hot)
  pts <- sf::st_as_sf(data.frame(x = xy[, 1], y = xy[, 2],
                                 s = (smV[hot] / max(smPos)) * (useV[hot] / max(usePos))),
                      coords = c("x", "y"), crs = terra::crs(sm))
  if(lonlat) pts <- sf::st_transform(pts, 2056)

  res <- terra::res(sm)
  halfDiag <- if(lonlat){
    lat <- mean(xy[, 2])
    sqrt((res[1] * 111320 * cos(lat * pi / 180))^2 + (res[2] * 110574)^2) / 2
  }else{
    sqrt(sum(res^2)) / 2
  }

  #clusters: cells whose buffers touch belong together
  blobs <- sf::st_cast(sf::st_union(sf::st_buffer(pts, clusterDist / 2)), "POLYGON")
  member <- vapply(sf::st_intersects(pts, blobs), `[`, integer(1), 1L)

  #rank first and build circles for the kept clusters only. A noisy matrix can
  #produce hundreds of clusters, and building every circle before discarding
  #most of them was two thirds of the cost (sf's per-call CRS lookups).
  score  <- tapply(pts$s, member, sum)
  keep   <- as.integer(names(sort(score, decreasing = TRUE)))[seq_len(min(maxCircles, length(score)))]
  pxy    <- sf::st_coordinates(pts)

  centres <- matrix(NA_real_, length(keep), 2)
  radius  <- numeric(length(keep))
  for(i in seq_along(keep)){
    m <- member == keep[i]
    if(sum(m) == 1L){
      centres[i, ] <- pxy[m, 1:2]
    }else{
      mbc <- lwgeom::st_minimum_bounding_circle(sf::st_union(sf::st_geometry(pts)[m]))
      centres[i, ] <- sf::st_coordinates(sf::st_centroid(mbc))[1, 1:2]
    }
    radius[i] <- max(sqrt((pxy[m, 1] - centres[i, 1])^2 + (pxy[m, 2] - centres[i, 2])^2))
  }

  ll <- sf::st_coordinates(sf::st_transform(
    sf::st_as_sf(data.frame(x = centres[, 1], y = centres[, 2]),
                 coords = c("x", "y"), crs = sf::st_crs(pts)), 4326))
  data.frame(lng = ll[, "X"], lat = ll[, "Y"],
             radius_m = radius + halfDiag,
             score = as.numeric(score[as.character(keep)]),
             nCells = as.integer(table(member)[as.character(keep)]),
             level = level)
}
