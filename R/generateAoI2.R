#Automatically generate Area of Interest (AoI or Zielgebiete)

#inputs: the attractiveness rasters (walkNat, DULN_all), a threshold and a perimeter

#outputs: a list of the generated polygons (sf) and the lake-masked walkNat band

#The `network` argument is gone. It was the first parameter for years and the
#body never read it - the two lines that would have are commented out below - so
#every caller had to have a path network in hand to ask for polygons cut out of a
#raster. That is what made merely opening step 4 dispatch the ~30s network job.
#The areas of interest are a function of DULN_all, the threshold and the
#perimeter, and that is now what the signature says.
#
#`DULN` is gone too, replaced by `walkNat`. This function read exactly one band
#out of the seven - terra::subset(DULN, "walkNat") - so taking the whole raster
#meant step 4 wrapped, sent, and unwrapped six bands nobody here looks at.
#
#WHAT THIS FUNCTION RETURNS IS NOW A LIST, not the polygons alone. The second
#element is the walkNat band with the lakes cut out of it, which this function
#has to build anyway and which step 4's manual draw/cut/hole handlers need. It
#used to be built TWICE per perimeter - once here (and then thrown away
#unused; see below) and once more on the main thread in step 4's enter(), each
#time paying a read of lakes.gdb and a rasterize. It is built once, here, in the
#worker, and handed back.
#
#THE ORDER OF OPERATIONS BELOW IS LOAD-BEARING. Every expensive step is done on
#the smallest set of cells or polygons that can still produce the right answer:
#crop before threshold, simplify before area, area-filter before extraction.
#The old body did the opposite at each of those three points.
generateAoI2 <- function(minThresh, perimeter = NULL,
                         walkNat = NULL, DULN_all = NULL,
                         tolerance = VFT_AOI_TOLERANCE_M){

  sf::sf_use_s2(TRUE)

  # networkEdgs <- terra::vect(network |> tidygraph::activate(edges) |> dplyr::as_tibble() |> sf::st_as_sf())
  #get vertices above selected threshold (Areas of Interest)

  #nodes <- igraph::V(network)$nodeID[igraph::V(network)$DULN > minThresh]
  #instead, get vertices within selected raster

  #### 1. cut down to the perimeter BEFORE doing anything per-cell ####
  #
  #The mask used to happen after the threshold, so both threshold passes ran over
  #every cell of the crop including the ones about to be discarded. crop() first
  #shrinks the extent; mask() then NAs the corners outside the buffered
  #perimeter. Buffered by 1000 m, as before, to avoid involving many unseen AoIs.
  rasterSel <- DULN_all
  if(!is.null(perimeter)){
    buf       <- terra::buffer(terra::vect(perimeter), 1000)
    rasterSel <- terra::mask(terra::crop(rasterSel, buf), buf)
  }

  #### 2. one threshold pass, not two ####
  #
  #This was `rasterSel[rasterSel < minThresh] <- NA` followed by
  #`rasterSel[rasterSel >= minThresh] <- 1`: two full-raster comparisons and two
  #allocations to compute one binary mask. ifel() does it in one, and treats an
  #already-NA cell the same way the pair did.
  rasterSel <- terra::ifel(rasterSel >= minThresh, 1, NA)

  # rasterSel <- terra::buffer(rasterSel, 10)
  rasterSel <- terra::as.polygons(rasterSel, aggregate = TRUE, na.rm = TRUE) #TRUE
  rasterSel <- terra::disagg(rasterSel)

  #geometry only. The value column as.polygons() carries is the constant 1 from
  #the threshold above; the old body dropped it at the very end with
  #`polygons[,1] <- NULL`.
  geom <- sf::st_geometry(sf::st_as_sf(rasterSel))

  #### 3. simplify, measure, filter - in that order, before any extraction ####
  #
  #as.polygons() traces CELL BOUNDARIES, so every one of these rings is a
  #staircase carrying far more vertices than its shape needs. That is the
  #few-features/many-vertices case st_simplify() is for - the same case as the
  #protected areas layer (VFT_PA_TOLERANCE_M in R/data_paths.R), and the exact
  #opposite of the path network, where vertices were not the cost at all (see
  #vftAddNetworkLines()). The vertex count drives three separate things
  #downstream: geojsonsf::sf_geojson() and the browser draw in step 4, the
  #terra::extract() below, and every st_intersects()/st_nearest_feature() the
  #simulation and the newVersions page run against these same polygons.
  #
  #In EPSG:2056 so the tolerance is in metres rather than degrees. The area is
  #then measured here too, planar and cheap, instead of spherically on lon/lat
  #staircase geometry.
  geom2056 <- sf::st_transform(geom, 2056)
  if(is.finite(tolerance) && tolerance > 0){
    geom2056 <- sf::st_simplify(geom2056, dTolerance = tolerance,
                                preserveTopology = TRUE)
    geom2056 <- sf::st_make_valid(geom2056)
    #st_make_valid() can hand back a GEOMETRYCOLLECTION when a staircase ring
    #self-touched. Only pay for the extraction when one actually appears.
    if(any(sf::st_geometry_type(geom2056) == "GEOMETRYCOLLECTION"))
      geom2056 <- sf::st_collection_extract(geom2056, "POLYGON")
  }

  area <- as.numeric(sf::st_area(geom2056))

  #the area filter used to sit AFTER the per-polygon extraction loop, so every
  #sliver disagg() produced was extracted at full cost and then thrown away.
  keep     <- !is.na(area) & area > 100000 & !sf::st_is_empty(geom2056)
  geom2056 <- geom2056[keep]
  area     <- area[keep]

  #back to lon/lat for the extraction below, and for the caller. Spelled as
  #"epsg:4326" rather than read off DULN_all: both providers crop in 4326
  #(R/providers.R), every consumer of these polygons assumes it, and terra's own
  #srs can come back empty on a machine where PROJ_LIB is shadowed.
  geom <- sf::st_transform(geom2056, "epsg:4326")

  #### 4. the lakes, read ONCE and actually used ####
  #
  #This block used to build `dulnRaster` and never reference it again - the one
  #line that would have (a median of the lake-masked band) was commented out, and
  #the loop below extracted from the UNMASKED band instead. So the read and the
  #rasterize were pure cost here, and step 4's enter() paid for the identical
  #pair a second time on the main thread to get the raster its manual editing
  #handlers use. One read now, used for both: the automatically generated areas
  #and the hand-drawn ones are scored against the same cells, which they were
  #not before.
  walkNatNoLakes <- walkNat
  if(!is.null(perimeter)){
    #before extracting values, remove lakes (make them NA)
    wkt <- sf::st_as_text( sf::st_as_sfc(sf::st_transform(perimeter, "epsg:2056") ) )

    lakes <- sf::st_read( vftData("maps/lakes.gdb"),
                          query = 'SELECT * FROM "lakes"',
                          wkt_filter = wkt)
    lakes <- sf::st_transform(lakes[lakes$SHAPE_Area > 10000, ], "epsg:4326")

    if(nrow(lakes) > 0) walkNatNoLakes[terra::vect(lakes)] <- NA
  }
  #the name every consumer indexes by, kept whatever terra did to it above
  names(walkNatNoLakes) <- "walkNat"

  #### 5. one extraction for every polygon, not one per polygon ####
  #
  #try with an intermediate between mean and max.
  #while the most attractive area is important. The general attractivity of the
  #area is as well. This also helps reduce an issue with AoIs: if a very
  #beautiful and pretty large core happens to be surrounded by mediocre, but
  #above threshold land, it becomes a large, mediocre area.
  #
  #The arithmetic is unchanged. What is gone is the loop around it: it called
  #terra::extract() once per polygon (per-call setup dominates for small ones),
  #re-ran the loop-invariant terra::subset() inside the body, and - the real
  #cost - assigned through `polygons[polyNb,]$DULN <-`, which copies the whole
  #sf object, geometry included, on every iteration. Same shape as the
  #vectorised extraction step 4's own cut handler already uses.
  if(length(geom) > 0){
    vals <- terra::extract(walkNatNoLakes, terra::vect(geom))
    duln <- vapply(split(vals$walkNat, vals$ID), function(x){
      #sort() drops NA by default, which is how the lake and out-of-raster cells
      #left the calculation before this too
      x <- sort(x, decreasing = TRUE)
      if(!length(x)) return(NA_real_)
      (stats::median(x[seq_len(max(1L, length(x) %/% 4L))]) +
         stats::median(x)) / 2
    }, numeric(1))
    #by name, not position: split() omits any ID whose polygon caught no cells,
    #and a positional read would then silently shift every value after it.
    duln <- unname(duln[as.character(seq_along(geom))])
  }else{
    duln <- numeric(0)
  }

  polygons <- sf::st_sf(DULN = duln, area = area, polygons = geom)

  return(list(polygons = polygons, walkNatNoLakes = terra::wrap(walkNatNoLakes)))
}
