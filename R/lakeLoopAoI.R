#Split the walk around a lake off into an area of interest of its own.
#
#generateAoI2() cuts its areas out of a thresholded attractiveness raster, and
#that raster has values over water - mostly high ones - so a lake ends up inside
#whatever area happens to surround it, lumped in with unrelated land. Visitors do
#not think of it that way: a lake that can be walked around is a destination in
#itself, "the loop around the lake". This pass finds those lakes and gives each
#one an area of its own: the lake plus the tightest path loop around it, where
#that loop stays within the attractive area holding the lake (or within a band
#around it - see step 1 below). The ground it
#takes is subtracted from the other areas, so the result never overlaps - which
#vftPrepareNetwork() needs, since it labels each node with ONE area.
#
#It runs inside generateAoI2(), so in the step-4 worker, never on the main
#thread. It does NOT use the path network: that is the ~30 s job that stays lazy
#until a simulation is launched (see vftPrepareThen()). It reads the path lines
#near the candidate lakes straight from the GDB instead, in ONE read - a filtered
#read of that file costs a fixed ~5-6 s whatever the size of the window, so one
#read per lake would multiply that, and no candidate means no read at all.
#
#Everything here is in EPSG:2056, so buffers and areas are in metres.

#' Lake-loop areas of interest
#'
#' @param geom2056 sfc of the generated areas, EPSG:2056.
#' @param lakes2056 the lakes near them (sf or sfc), EPSG:2056.
#' @param buffer function(lakeArea) giving, per lake, how far its loop may run
#'   outside the area holding it - vftLakeLoopBuffer(), 50 m to 200 m with the
#'   lake's size.
#' @param readPaths function(area2056) returning the path lines (sfc, 2056)
#'   inside `area2056`. An argument so the checks can hand in their own lines.
#' @return list(geom = sfc in 2056, lakeLoop = logical, one per geometry). On any
#'   failure, the input unchanged with every flag FALSE: a failed lake pass must
#'   never cost the user their areas.
vftLakeLoopAoI <- function(geom2056, lakes2056,
                           share    = VFT_LAKE_LOOP_SHARE,
                           buffer   = vftLakeLoopBuffer,
                           enclosed = VFT_LAKE_LOOP_ENCLOSED,
                           pad      = VFT_LAKE_LOOP_PAD_M,
                           minArea  = VFT_AOI_MIN_AREA_M2,
                           readPaths = .vftReadPathsWithin){

  unchanged <- list(geom = geom2056, lakeLoop = rep(FALSE, length(geom2056)))
  if(!length(geom2056) || is.null(lakes2056) || !length(sf::st_geometry(lakes2056)))
    return(unchanged)

  tryCatch(
    .vftLakeLoopAoI(geom2056, lakes2056, share, buffer, enclosed, pad, minArea,
                    readPaths),
    error = function(e){
      vftDbgCat("WARNING lake loop pass failed, areas left as generated: ",
                conditionMessage(e), "\n")
      unchanged
    })
}

.vftLakeLoopAoI <- function(geom2056, lakes2056, share, buffer, enclosed, pad,
                            minArea, readPaths){
  crs <- sf::st_crs(geom2056)

  #the lakes carry LN02 heights; nothing below wants a Z
  lakes <- sf::st_make_valid(sf::st_zm(sf::st_geometry(lakes2056)))
  lakes <- sf::st_set_crs(sf::st_set_crs(lakes, NA), crs)
  lakeArea <- as.numeric(sf::st_area(lakes))

  #### 1. candidates: a lake ONE area mostly covers ####
  #Holes filled first. The raster usually puts the lake inside the area, but an
  #area that merely RINGS the lake - water below threshold - covers it too, as
  #far as a visitor is concerned. That area is the lake's `holder`.
  #
  #One intersection per (lake, area) pair rather than one call over all of
  #them: st_intersection() drops empty results, which would misalign the areas
  #with `h` and make the wrong area the holder.
  filled  <- .vftFillHoles(geom2056)
  hits    <- sf::st_intersects(lakes, filled)
  covered <- numeric(length(lakes))
  holder  <- integer(length(lakes))
  for(i in seq_along(lakes)){
    h <- hits[[i]]
    if(!length(h)) next
    a <- vapply(h, function(j)
      sum(as.numeric(sf::st_area(sf::st_intersection(filled[j], lakes[i])))), numeric(1))
    covered[i] <- max(a) / lakeArea[i]
    holder[i]  <- h[which.max(a)]
  }
  cand <- which(covered > share)
  if(!length(cand)) return(list(geom = geom2056, lakeLoop = rep(FALSE, length(geom2056))))

  #largest first: where two lake areas meet, the larger lake keeps the ground
  cand <- cand[order(lakeArea[cand], decreasing = TRUE)]

  #The loop's zone is measured from the HOLDER (plus the lake, for the share of
  #it the holder leaves out), not from the water. A path that swings away from
  #the shore through ground that is still attractive - round a reed belt or a
  #reserve, as the Greifensee path does at Riedikon, up to 298 m off the water
  #- is no detour as far as the visitor is concerned. Only leaving the
  #attractive ground by more than the band is. The band still grows with the
  #lake (vftLakeLoopBuffer()).
  zones <- do.call(c, lapply(cand, function(i)
    sf::st_buffer(sf::st_union(c(lakes[i], filled[holder[i]])), buffer(lakeArea[i]))))

  #### 2. the paths near them, ONE read ####
  t0 <- Sys.time()
  paths <- readPaths(sf::st_union(zones))
  vftDbgCat(sprintf("lake loop: %d candidate lake(s), %d path line(s) read in %.1f s\n",
                    length(cand), length(paths),
                    as.numeric(difftime(Sys.time(), t0, units = "secs"))))
  if(!length(paths)) return(list(geom = geom2056, lakeLoop = rep(FALSE, length(geom2056))))
  paths <- sf::st_set_crs(sf::st_set_crs(paths, NA), crs)

  #### 3. + 4. per lake: is there a loop inside its zone? ####
  lakeAoI <- list()
  for(k in seq_along(cand)){
    lake <- lakes[cand[k]]
    zone <- zones[k]

    #The zone FIRST in both predicates: sf prepares the first argument, and
    #the zone is one polygon of thousands of vertices against ~10k lines.
    near <- paths[sf::st_intersects(zone, paths)[[1]]]
    if(!length(near)) next

    #Clip to the zone, then node: st_union() splits every line where it
    #crosses another, which is what st_polygonize() needs to see the cycles.
    #What polygonize returns is every FACE - the smallest regions the path
    #cycles inside the zone cut the ground into. A dangling path (a jetty, a
    #path leaving the zone) bounds nothing and drops out.
    #
    #Only the lines that cross the zone's edge are clipped. The rest lie wholly
    #inside it, and running them through st_intersection() and
    #st_collection_extract() one by one was half the cost of this loop.
    inside  <- seq_along(near) %in% sf::st_contains_properly(zone, near)[[1]]
    crossed <- near[!inside]
    if(length(crossed))
      crossed <- suppressWarnings(sf::st_collection_extract(
        sf::st_intersection(crossed, zone), "LINESTRING"))
    clipped <- c(near[inside], crossed)
    if(!length(clipped)) next
    faces <- suppressWarnings(sf::st_collection_extract(
      sf::st_polygonize(sf::st_union(clipped)), "POLYGON"))
    if(!length(faces)) next

    #THE TIGHTEST LOOP: the faces whose interior overlaps the water. The one
    #holding the lake is bounded by the innermost path cycle around it (a
    #bridge or dam splits it into several, which together still are), so
    #their outline is the walk round the lake - not the outermost cycle in
    #the zone, which with a zone the size of the holder could be any long
    #round in the hinterland. Islands' paths leave holes; filled.
    tight <- sf::st_relate(faces, lake, pattern = "T********", sparse = FALSE)[, 1]
    if(!any(tight)) next
    region <- .vftFillHoles(sf::st_union(faces[tight]))

    inLake <- sum(as.numeric(sf::st_area(sf::st_intersection(region, lake))))
    if(inLake / lakeArea[cand[k]] < enclosed) next

    #The loop plus the lake itself (a loop may cut across the odd corner of
    #the water), holes removed, and grown by `pad` so the path's nodes fall
    #inside rather than on the edge.
    aoi <- .vftFillHoles(sf::st_union(c(region, lake)))
    aoi <- sf::st_union(sf::st_buffer(aoi, pad))

    #the ground an earlier (larger) lake already took
    if(length(lakeAoI)){
      aoi <- sf::st_difference(aoi, sf::st_union(do.call(c, lakeAoI)))
      if(!length(aoi)) next
    }
    lakeAoI[[length(lakeAoI) + 1]] <- aoi
  }
  if(!length(lakeAoI)) return(list(geom = geom2056, lakeLoop = rep(FALSE, length(geom2056))))

  lakeAoI   <- .vftPolygonParts(do.call(c, lakeAoI))
  lakeAoI   <- lakeAoI[as.numeric(sf::st_area(lakeAoI)) > minArea]
  if(!length(lakeAoI)) return(list(geom = geom2056, lakeLoop = rep(FALSE, length(geom2056))))
  lakeUnion <- sf::st_union(lakeAoI)

  #### 5. no overlaps: the other areas give the lake areas up ####
  #Only the areas a lake area actually reaches are touched, so the rest come
  #through exactly as generated. A remnant can fall apart into pieces - each
  #becomes an area of its own, as disagg() would have made it - and pieces under
  #the minimum go, the same rule generateAoI2() applies to its own slivers.
  touched <- lengths(sf::st_intersects(geom2056, lakeUnion)) > 0
  rest <- lapply(seq_along(geom2056), function(i){
    g <- geom2056[i]
    if(!touched[i]) return(g)
    g <- .vftPolygonParts(sf::st_difference(g, lakeUnion))
    g[as.numeric(sf::st_area(g)) > minArea]
  })
  rest <- do.call(c, rest)
  if(is.null(rest)) rest <- sf::st_sfc(crs = crs)

  vftDbgCat(sprintf("lake loop: %d lake area(s), %d other area(s) (were %d)\n",
                    length(lakeAoI), length(rest), length(geom2056)))

  list(geom     = sf::st_set_crs(sf::st_set_crs(c(rest, lakeAoI), NA), crs),
       lakeLoop = c(rep(FALSE, length(rest)), rep(TRUE, length(lakeAoI))))
}

#' Path lines inside `area2056`, read straight from the paths GDB.
#'
#' The layer is EPSG:4326, so the filter has to be too - a 2056 WKT is not an
#' error, it just matches nothing. Same file and query as the network provider
#' (R/providers.R), and like it, no attribute filter: every line the network
#' has, this sees.
.vftReadPathsWithin <- function(area2056){
  wkt <- sf::st_as_text(sf::st_union(sf::st_transform(area2056, "epsg:4326")))
  p <- sf::st_read(vftData("maps/paths/paths_11_24_final_4.gdb"),
                   query = 'SELECT * FROM "paths_11_24_final_4"',
                   wkt_filter = wkt, quiet = TRUE)
  sf::st_transform(sf::st_zm(sf::st_geometry(p)), "epsg:2056")
}

#' Drop every interior ring, keep the outer ones.
.vftFillHoles <- function(g){
  out <- lapply(g, function(p){
    if(inherits(p, "POLYGON")) sf::st_polygon(p[1])
    else if(inherits(p, "MULTIPOLYGON")) sf::st_multipolygon(lapply(p, `[`, 1))
    else p
  })
  sf::st_sfc(out, crs = sf::st_crs(g))
}

#' Valid, single POLYGONs, whatever an overlay handed back.
.vftPolygonParts <- function(g){
  if(!length(g)) return(g)
  g <- sf::st_make_valid(g)
  if(any(sf::st_geometry_type(g) == "GEOMETRYCOLLECTION"))
    g <- suppressWarnings(sf::st_collection_extract(g, "POLYGON"))
  g <- g[!sf::st_is_empty(g)]
  if(!length(g)) return(g)
  sf::st_cast(sf::st_cast(g, "MULTIPOLYGON"), "POLYGON")
}
