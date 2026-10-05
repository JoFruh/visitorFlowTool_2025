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
#
#`lakeLoop` switches the lake-loop pass (R/lakeLoopAoI.R) on: a lake one area
#mostly covers, and that a path circles near the shore, becomes an area of its
#own - the lake plus the loop - subtracted from the others. The output carries a
#`lakeLoop` column flagging those areas either way.
#
#`split` switches the destination split (R/aoiSegment.R) on: each connected run
#above the threshold is divided into its high-value cores, which merge back only
#where a path crosses between them and the dip is shallow. FALSE is the old
#one-area-per-run behaviour exactly. The split needs the path lines, read ONCE
#here and handed to the lake-loop pass too.
#
#`method` picks the split: "watershed" is the one above; "cut" (R/aoiCut.R) is
#the inverse candidate under comparison - the threshold areas cut at narrow
#bridges, by shape alone, with no paths read. With VFT_AOI_CUT_LAKES it also
#anchors the lakes - a qualifying lake and its shore band are the core of an
#area of its own - and that replaces the lake-loop pass.
#
#Step 4 generates with split = FALSE (plain threshold areas) and runs the split
#only when the user presses "Automatic Cuts".
#
#`within` (sf polygons, any crs) replaces the threshold areas with these
#polygons: only cells inside them count, and every cell inside them counts,
#raised to the threshold where it is below it. That is how "Automatic Cuts"
#cuts the areas the user has already edited - erased ones stay gone, drawn ones
#are cut too - rather than starting over from the threshold. At raster
#resolution, and the VFT_AOI_MIN_AREA_M2 filter applies as always.
generateAoI2 <- function(minThresh, perimeter = NULL,
                         walkNat = NULL, DULN_all = NULL,
                         tolerance = VFT_AOI_TOLERANCE_M,
                         lakeLoop = VFT_LAKE_LOOP,
                         split = VFT_AOI_SPLIT,
                         method = VFT_AOI_SPLIT_METHOD,
                         cutLakes = VFT_AOI_CUT_LAKES,
                         within = NULL){

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

  #the user's working set in place of the threshold areas (see `within` above).
  #Its crs is set to the raster's rather than compared: both are lon/lat, and
  #terra's srs can come back empty where PROJ_LIB is shadowed.
  if(!is.null(within) && length(sf::st_geometry(within)) > 0){
    v <- terra::vect(sf::st_transform(sf::st_geometry(within), "epsg:4326"))
    terra::crs(v) <- terra::crs(rasterSel)
    inside    <- terra::rasterize(v, rasterSel, field = 1)
    rasterSel <- terra::ifel(is.na(inside), NA,
                             terra::ifel(rasterSel < minThresh, minThresh, rasterSel))
  }

  #the buffered perimeter in metres: the lakes (block 4) and, with `split`, the
  #paths are read inside it
  buf2056 <- NULL
  if(!is.null(perimeter))
    buf2056 <- sf::st_union(sf::st_buffer(
      sf::st_geometry(sf::st_transform(perimeter, "epsg:2056")), 1000))

  #### 2. one threshold pass, not two - or the destination split ####
  #
  #This was `rasterSel[rasterSel < minThresh] <- NA` followed by
  #`rasterSel[rasterSel >= minThresh] <- 1`: two full-raster comparisons and two
  #allocations to compute one binary mask. ifel() does it in one, and treats an
  #already-NA cell the same way the pair did.
  #
  #With `split`, the mask is a LABEL raster instead - one id per destination -
  #and the dissolve below makes one polygon per id. Any failure falls back to
  #the plain mask: a failed split must never cost the user their areas. So does
  #a read that finds no paths at all, which would otherwise leave every core
  #apart.
  #The cut anchors the lakes, so it needs them BEFORE the split; block 4 then
  #uses the same read.
  lakes     <- NULL
  lakeCut   <- isTRUE(split) && !is.null(buf2056) && identical(method, "cut") &&
               isTRUE(cutLakes)
  if(lakeCut) lakes <- .vftReadLakesWithin(buf2056)

  paths2056 <- NULL
  labels    <- NULL
  if(isTRUE(split) && !is.null(buf2056)){
    labels <- tryCatch(if(identical(method, "cut")) vftCutAoI(rasterSel, minThresh, lakes = lakes) else {
      t0 <- Sys.time()
      paths2056 <- .vftReadPathsWithin(buf2056)
      vftDbgCat(sprintf("aoi split: %d path line(s) read in %.1f s\n", length(paths2056),
                        as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      if(!length(paths2056)) stop("no path lines inside the perimeter")
      vftSegmentAoI(rasterSel, minThresh, paths2056)
    }, error = function(e){
      vftDbgCat("WARNING aoi split failed, one area per run: ",
                conditionMessage(e), "\n")
      NULL
    })
  }
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
  #
  #Split areas SHARE borders, and st_simplify() on each area alone simplifies a
  #shared border twice, differently: overlaps one side, gaps the other. They are
  #simplified as one coverage instead - see .vftSimplifyCoverage(). A geometry
  #failure there leaves the unsplit areas, as a failed segmentation does.
  geom2056 <- NULL
  if(!is.null(labels)){
    geom2056 <- tryCatch(.vftSimplifyCoverage(labels, tolerance), error = function(e){
      vftDbgCat("WARNING aoi split simplification failed, one area per run: ",
                conditionMessage(e), "\n")
      NULL
    })
    rasterSel <- terra::ifel(is.na(labels), NA, 1)
  }else{
    rasterSel <- terra::ifel(rasterSel >= minThresh, 1, NA)
  }

  #the lakes were anchored only if the cut AND its polygons came through
  anchored <- lakeCut && !is.null(geom2056)

  if(is.null(geom2056)){
    # rasterSel <- terra::buffer(rasterSel, 10)
    rasterSel <- terra::as.polygons(rasterSel, aggregate = TRUE, na.rm = TRUE) #TRUE
    rasterSel <- terra::disagg(rasterSel)

    #geometry only. The value column as.polygons() carries is the constant 1
    #from the threshold above; the old body dropped it at the very end with
    #`polygons[,1] <- NULL`.
    geom2056 <- sf::st_transform(sf::st_geometry(sf::st_as_sf(rasterSel)), 2056)
    if(is.finite(tolerance) && tolerance > 0){
      geom2056 <- sf::st_simplify(geom2056, dTolerance = tolerance,
                                  preserveTopology = TRUE)
      geom2056 <- sf::st_make_valid(geom2056)
      #st_make_valid() can hand back a GEOMETRYCOLLECTION when a staircase ring
      #self-touched. Only pay for the extraction when one actually appears.
      if(any(sf::st_geometry_type(geom2056) == "GEOMETRYCOLLECTION"))
        geom2056 <- sf::st_collection_extract(geom2056, "POLYGON")
    }
  }

  area <- as.numeric(sf::st_area(geom2056))

  #the area filter used to sit AFTER the per-polygon extraction loop, so every
  #sliver disagg() produced was extracted at full cost and then thrown away.
  keep     <- !is.na(area) & area > VFT_AOI_MIN_AREA_M2 & !sf::st_is_empty(geom2056)
  geom2056 <- geom2056[keep]
  area     <- area[keep]

  #### 4. the lakes, read ONCE and used twice ####
  #
  #This block used to build `dulnRaster` and never reference it again - the one
  #line that would have (a median of the lake-masked band) was commented out, and
  #the loop below extracted from the UNMASKED band instead. So the read and the
  #rasterize were pure cost here, and step 4's enter() paid for the identical
  #pair a second time on the main thread to get the raster its manual editing
  #handlers use. One read now, used for both: the automatically generated areas
  #and the hand-drawn ones are scored against the same cells, which they were
  #not before.
  #
  #Filtered by the BUFFERED perimeter, not the perimeter itself: the areas reach
  #1000 m past it (block 1), and the lake-loop pass below has to see every lake
  #they cover. wkt_filter returns each matching lake whole, so a lake straddling
  #the edge comes in entire. Inside the perimeter the mask is what it was.
  walkNatNoLakes <- walkNat
  lakeLoopFlag   <- rep(FALSE, length(geom2056))
  if(!is.null(perimeter)){
    if(is.null(lakes)) lakes <- .vftReadLakesWithin(buf2056)

    #### 4a. the anchored lakes' areas (method "cut") ####
    #The cut has already given each qualifying lake an area of its own; flag
    #it: the area holding more than half of a lake of at least
    #VFT_AOI_LAKE_MIN_M2. The lake-loop pass below is not run on top.
    if(anchored && nrow(lakes) > 0 && length(geom2056) > 0){
      lk <- sf::st_zm(sf::st_geometry(sf::st_transform(lakes, "epsg:2056")))
      lk <- sf::st_set_crs(sf::st_set_crs(lk, NA), sf::st_crs(geom2056))
      lk <- lk[as.numeric(sf::st_area(lk)) >= VFT_AOI_LAKE_MIN_M2]
      hits <- sf::st_intersects(lk, geom2056)
      for(i in seq_along(lk)){
        h <- hits[[i]]
        if(!length(h)) next
        a <- vapply(h, function(j)
          sum(as.numeric(sf::st_area(sf::st_intersection(geom2056[j], lk[i])))), numeric(1))
        if(max(a) / as.numeric(sf::st_area(lk[i])) > VFT_LAKE_LOOP_SHARE)
          lakeLoopFlag[h[which.max(a)]] <- TRUE
      }
    }

    #### 4b. the walk around a lake is an area of its own ####
    #See R/lakeLoopAoI.R. Before the extraction below, so block 5 scores the
    #final set in its one pass, lake areas included - and their lake cells are
    #NA by then, so a lake area's DULN is its shore and its loop.
    #The split already read every path inside the buffered perimeter, and the
    #lake zones lie inside it, so the pass takes its lines from that read
    #rather than paying the ~8 s scan a second time.
    if(isTRUE(lakeLoop) && !anchored && nrow(lakes) > 0 && length(geom2056) > 0){
      readPaths <- if(length(paths2056)) function(area2056){
        #"epsg:2056" here and 2056 there are not always the same crs to sf
        a <- sf::st_set_crs(sf::st_set_crs(sf::st_union(area2056), NA),
                            sf::st_crs(paths2056))
        paths2056[sf::st_intersects(a, paths2056)[[1]]]
      } else .vftReadPathsWithin
      ll <- vftLakeLoopAoI(geom2056, sf::st_transform(lakes, "epsg:2056"),
                           readPaths = readPaths)
      geom2056     <- ll$geom
      lakeLoopFlag <- ll$lakeLoop
      area         <- as.numeric(sf::st_area(geom2056))
    }

    #remove lakes from walkNat (make them NA) before extracting values
    lakes <- sf::st_transform(lakes, "epsg:4326")
    if(nrow(lakes) > 0) walkNatNoLakes[terra::vect(lakes)] <- NA
  }
  #the name every consumer indexes by, kept whatever terra did to it above
  names(walkNatNoLakes) <- "walkNat"

  #back to lon/lat for the extraction below, and for the caller. Spelled as
  #"epsg:4326" rather than read off DULN_all: both providers crop in 4326
  #(R/providers.R), every consumer of these polygons assumes it, and terra's own
  #srs can come back empty on a machine where PROJ_LIB is shadowed.
  geom <- sf::st_transform(geom2056, "epsg:4326")
  #the split's shared borders can come out of the transform crossing on the
  #sphere; the unsplit areas never have, and are left alone
  if(!is.null(labels)) geom <- .vftS2Valid(geom)

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

  polygons <- sf::st_sf(DULN = duln, area = area, lakeLoop = lakeLoopFlag,
                        polygons = geom)

  return(list(polygons = polygons, walkNatNoLakes = terra::wrap(walkNatNoLakes)))
}
