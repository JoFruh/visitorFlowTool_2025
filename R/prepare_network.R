#### Preparing a scenario's network for simulation ####

# This was step 4's confirm handler. Confirming the areas of interest dispatched
# one future that stamped AOI letters onto every node, read and clipped the
# parking shapefile, distributed lot capacity across nodes, computed seventy
# attractivity-weighted edge-distance columns and rebuilt the tbl_graph - about
# thirty seconds, paid by every user who drew their polygons and pressed Confirm.
#
# None of it is READ until a simulation runs. Step 5 before a simulation shows a
# static placeholder image and a row of scenario cards, and every display
# checkbox that would need this data is disabled until a pathUsage exists. The
# consumers are generatePopulation() (the node `parking` / `parkingAttr`
# columns), determineShortestPath() / launchSim_v2() (the SHAPE_Leng_* weights)
# and determineAgentCharacteristics() (finalPolygons$AOI) - all of them reached
# only through step 5's simulation button.
#
# So step 4 confirms instantly now, and this runs at the two places that
# genuinely need it: the simulation launch, and the newVersions page when its
# context is "Wegen/Strassen" or "Parken/Wohnen". "Hitzeminderung" deliberately
# does NOT trigger it - a direct route to that context should be able to skip the
# path and parking load entirely.
#
# NOT a provider. VFT_PROVIDERS fills whole `r$` keys, and what this produces
# belongs to one SCENARIO - r$networkList[[pos]]$network - which a provider has
# no way to address. The once-per-scenario guard below is what a provider's
# in-flight marker would have done.

#' Has this network already been prepared?
#'
#' The same question VFT_KEY_READY$networkNodes asks about `r$network`, and asked
#' the same way: the columns are the honest test, not a flag beside them, because
#' a scenario can arrive from a save file, from step 4, or from the newVersions
#' page having its edges rewritten, and only one of those three could carry a
#' flag.
#'
#' tryCatch for the same reason as in R/steps.R: igraph::vertex_attr_names()
#' aborts outright on anything that is not a graph, and an abort here would kill
#' the observer that asked rather than answer "not prepared".
vftNetworkPrepared <- function(network){
  if(is.null(network)) return(FALSE)
  isTRUE(tryCatch({
    all(c("AOI", "parking", "parkingAttr") %in% igraph::vertex_attr_names(network)) &&
      "SHAPE_Leng_walkNat" %in% igraph::edge_attr_names(network)
  }, error = function(e) FALSE))
}

#' The node geometry in EPSG:4326, transformed only if it is not there already.
#'
#' IT ALWAYS IS. sf_to_tidygraph3() samples the residential and DULN rasters -
#' both EPSG:4326 - with the raw node X/Y (R/sf_to_tidygraph3.R#249-268), and the
#' network provider hands the paths GDB a 4326 wkt_filter
#' (R/providers.R#212-218), so the graph has been in lon/lat since the moment it
#' was built. vftPrepareNetwork() used to re-project both the nodes AND all ~50k
#' edge geometries into the CRS they were already in, which is a full geometry
#' rebuild for no change in a single coordinate.
#'
#' Guarded rather than assumed: a differently-projected source still comes out
#' right, it just pays for it. The EDGE geometry is not transformed anywhere any
#' more, because nothing in preparation reads it.
.vftNodes4326 <- function(geom){
  if(is.null(geom)) return(NULL)
  crs <- tryCatch(sf::st_crs(geom), error = function(e) NA)
  if(is.na(crs) || isTRUE(crs == sf::st_crs(4326))) return(geom)
  sf::st_transform(geom, 4326)
}

#' Everything a network needs before it can be simulated on.
#'
#' Pure: no reactives, no session, no `r`. Meant to be called from inside
#' vftFuture(), which is where it used to live.
#'
#' NOTHING HERE BUILDS A TABLE ANY MORE. It used to materialise the nodes and the
#' edges as `sf` data frames - st_as_sf(as_tibble(activate(...))), the shape the
#' 2026-08-25 line profile put at the top of the pure-R cost in the whole app -
#' re-project both, assign seventy weight columns INTO the edge data frame by
#' logical index, and then rebuild the entire tbl_graph from the two tables. The
#' graph that comes in is now the graph that goes out, with attributes set on it:
#' the geometry is never copied, the edge list is never re-derived, and the
#' weights are computed on plain numeric vectors.
#'
#' Column ORDER is therefore not what it was, and nothing cares: every consumer
#' indexes by name. generateAdjListAndDistTbl_cpp() reads
#' edgeTable["SHAPE_Leng_walkNat"] / vertexTable["nodeID"] / edgeTable["from"]
#' (src/CPP_FUNCTIONS.cpp), and vftNetworkPrepared() tests attribute NAMES.
#'
#' @param network a tbl_graph - the SCENARIO's network, not `r$network`, so that
#'   any edges the newVersions page has already added or edited survive.
#' @param finalPolygons the confirmed areas of interest, carrying the `AOI`
#'   letters step 4 stamped on them (see the note there for why that one line
#'   stayed behind).
#' @param minThresh the step-3 attractiveness threshold.
#' @param parking an already-loaded parking table, to skip the read.
#' @param progress optional vftProgress handle.
#' @param base,span where this call's share of the bar starts and how much of it
#'   it owns. The newVersions page owns the whole bar, which is the default; step
#'   5 gives preparation the first 60% of a bar it shares with the population,
#'   the adjacency lists and the agent goals.
#'
#' @return list(network = , parking = )
vftPrepareNetwork <- function(network, finalPolygons, minThresh,
                              parking = NULL, progress = NULL,
                              base = 0, span = 1){

  #Step 3's skip button commits `isSkip` and nothing else, so minThresh is NULL
  #on that path - and `8/(34.2423 + NULL)` is numeric(0), which makes every
  #weight assignment below fail with "replacement has 0 rows". That was already
  #true in step 4; skip step 3, draw a polygon by hand, confirm, and it fired.
  #Zero is what the slider's own floor means, and a working simulation beats an
  #error carried from one step into another.
  if(is.null(minThresh) || !length(minThresh)) minThresh <- 0

  #a tbl_graph must come back a tbl_graph: the newVersions page runs
  #dplyr::mutate() on it. igraph's attribute setters preserve the class of what
  #they are given, but this is cheap and it is not worth finding out otherwise
  #three modules away.
  cls <- class(network)

  p <- function(f, detail = NULL){
    if(!is.null(progress))
      try(progress$set(base + span * f, detail = detail), silent = TRUE)
  }

  vftDbgCat("PREPARE NETWORK: AOI nodes\n")
  p(0.05, "Zielgebiete werden auf das Wegnetz übertragen...")

  nodeGeom <- .vftNodes4326(igraph::vertex_attr(network, "geometry"))
  nodeID   <- igraph::vertex_attr(network, "nodeID")

  #The points go to terra as a bare coordinate matrix. Building an sf data frame
  #to hand over - which is what st_as_sf(as_tibble(activate(nodes))) did - copies
  #every node attribute in the graph for the sake of two columns.
  vertices_vect <- terra::vect(sf::st_coordinates(nodeGeom)[, c("X", "Y"), drop = FALSE],
                               type = "points", crs = "EPSG:4326")

  vertices_AOI_data <- terra::extract(terra::vect(finalPolygons["AOI"]), vertices_vect)

  #add node_DULN data to original nodes
  AOI <- vertices_AOI_data$AOI
  AOI[is.na(AOI)] <- 0

  #for every polygon-extracted set of nodes, check whether they form a single
  #component; otherwise keep the largest one
  for(letter in finalPolygons$AOI){
    #get nodes with this letter
    letternode <- nodeID[AOI == letter]
    #determine components
    subnetwork <- igraph::subgraph(network, letternode)
    comp <- igraph::components(subnetwork, "strong")

    #restored but potential ERROR, keep an eye out
    # as large AOIs would have only one part active
    #keep largest components as is
    #smaller components: replace letter with "0"
    smllerCompIDs <- igraph::V(subnetwork)$nodeID[comp$membership %in% which(comp$csize != max(comp$csize))]
    #apply a "0" to all vertices in smaller components
    AOI[nodeID %in% smllerCompIDs] <- "0"

    #get largest comp and add letter to neighbouring nodes (buffer)
    largestCompID <- igraph::V(subnetwork)$nodeID[comp$membership %in% which(comp$csize == max(comp$csize))]
    #assign current letter to all neighbhood nodes around current AOI (buffer zone).
    #NOTE this one indexes by POSITION - neighborhood() returns vertex indices -
    #while the line above indexes by nodeID. Left exactly as found.
    AOI[unique(unlist(igraph::neighborhood(network, 1, nodes = largestCompID)))] <- letter
  }

  #The READ is what an already-loaded table skips. The node attribution below is
  #not: it used to sit inside this branch, so handing the function a parking
  #table produced a network with no `parking` / `parkingAttr` / `newResidential`
  #columns at all - which generatePopulation() reads, and which
  #vftNetworkPrepared() tests, so the scenario would come back "not prepared"
  #and be prepared again on the next launch, forever. (The same shape was in
  #step 4's confirm handler, where a second confirm in one visit hit it.)
  if(is.null(parking)){
    p(0.30, "Parkplätze werden geladen...")

    #LOAD / FILTER PARKING ####
    #filter out all parkings that are not in proximity to an AOI (further than 100m)
    wkt <- sf::st_as_text(sf::st_transform(sf::st_union( sf::st_buffer(sf::st_transform(finalPolygons, 2056), 100) ), "epsg:4326"  ))
    #retrieve parking areas and crop
    parking <- sf::st_read(vftData("maps/parking/parkingShapes.shp"),
                           query = 'SELECT * FROM "parkingShapes"',
                           wkt_filter = wkt
    )
    if(!is.null(parking )){
      parking <-  parking |>
        dplyr::rename(polygons = .data$`_ogr_geometry_`) |>
        dplyr::select(.data$polygons)
      parking$id <- 1:nrow(parking)
      parking$isNew <- 0
    }
  }

  #Deliberately NOT gated on nrow(parking) > 0. A perimeter with no parking at
  #all still has to come out of here with the three columns present and zero -
  #every spatial call below is a no-op on an empty table - or the network would
  #never test as prepared and the whole job would run again on every launch.
  parkingCap  <- NULL
  parkingAttrV <- NULL
  newResidential <- NULL
  if(!is.null(parking)){

      #PARKING DATA ####
      #similar to residential, but sampling parkingPolygons
      #nodes start at 0 and are filled in by position - the two `%in%` scans over
      #every node in the graph that used to carry the numbers back from a
      #filtered sf table are gone with the table.
      nNodes         <- length(nodeID)
      parkingCap     <- numeric(nNodes)
      parkingAttrV   <- numeric(nNodes)
      newResidential <- numeric(nNodes)

      #THE SPATIAL WORK IS DONE IN EPSG:2056, ON THE GEOMETRY ALONE. A 50 m
      #st_buffer() in lon/lat routes through s2's spherical geometry - the same
      #trap that made the protected-areas clip the largest main-thread blocker in
      #the app (see step5_server.R). st_area() deliberately stays on the ORIGINAL
      #table so the areas remain geodesic and the agent counts do not move.
      nodes2056   <- sf::st_transform(nodeGeom, 2056)
      parking2056 <- sf::st_transform(sf::st_geometry(parking), 2056)
      polys2056   <- sf::st_transform(sf::st_geometry(finalPolygons), 2056)

      #problematic to use filtered nodes, use all nodes instead? or buffered areas around parking
      near <- lengths(sf::st_intersects(nodes2056, sf::st_buffer(parking2056, 50))) > 0
      idx  <- which(near)

      # Pre-compute all spatial relationships ONCE before the loop instead of once per iteration:
      # 1. Area / agentNb for every parking polygon (vectorised)
      polyAreas    <- as.numeric(sf::st_area(parking)) / 30
      # 2. Nearest AOI for every parking polygon (vectorised, single call)
      nearestAOIs  <- sf::st_nearest_feature(parking2056, polys2056)
      parkingAttrs <- finalPolygons$DULN[nearestAOIs]
      # 3. Which of the nearby nodes fall within each parking polygon (one spatial op for all polygons)
      nodesInParkings <- sf::st_contains(parking2056, nodes2056[idx], sparse = TRUE)

      p(0.55, "Parkplatzpotenzial wird bestimmt...")

      #cycle through parking polygons
      for(polyNb in seq_along(polyAreas)){

        nodeIndices <- idx[nodesInParkings[[polyNb]]]
        nodeCount   <- length(nodeIndices)
        agentNb     <- polyAreas[[polyNb]]
        nearestAttr <- parkingAttrs[[polyNb]]

        if(nodeCount > 0){
          #add number of agents a parking can hold (per node within parking)
          parkingCap[nodeIndices]   <- parkingCap[nodeIndices] + (agentNb / nodeCount)
          parkingAttrV[nodeIndices] <- nearestAttr

        }else{
          #CAPTURE EXCEPTION : no nodes in polygon
          #in this case, find a single closest node outside polygon
          #(guarded on length() too: with no node anywhere near any parking this
          #returns integer(0), and the old `if(!is.na(x))` raised on that rather
          #than skipping the polygon.)
          nearestNodeIdx <- sf::st_nearest_feature(parking2056[polyNb], nodes2056[idx])

          if(length(nearestNodeIdx) == 1L && !is.na(nearestNodeIdx)){
            j <- idx[[nearestNodeIdx]]
            parkingCap[j]   <- parkingCap[j] + agentNb
            parkingAttrV[j] <- nearestAttr
          }
        }
      }
  }

  # DETERMINE ATTR WEIGHTED DISTANCES ####
  # determine new distances that are weighted by attractivity
  # ex: walkNat, walkNat_attr, walkNat_ATTR => slightly, moderately, heavily weighted by attractivity
  # bigger the attractivity, shorter the "distance".
  # 0 and negatives create problems, thus add min()+1

  #Removed min()+1, as these change weight of various DULNs.
  #Instead, added single value (min(all DULN)) to all DULNs at creation, to bring all values to positive

  #divide distance in a way that, attr values close to the minimum threshold (for AoIs), are halved for ATTR, reduced by 0.25 for attr, and reduced by 0.125 for distance
  #(minThresh has 24.11 added to avoid 0s and negatives in attractivity maps)
  #
  #TWENTY-ONE COLUMNS IN ONE PASS EACH. This was 21 assignments, then 21
  #"max distance is not increased" clamps by logical index, then 21 "min is not
  #below 10" clamps by logical index - 63 passes over the edge table, each one
  #materialising a logical vector and subassigning into an sf data frame.
  #pmin/pmax is the same arithmetic in one pass on a plain numeric vector, and
  #it propagates an NA in a DULN column instead of raising "NAs are not allowed
  #in subscripted assignments", which is what the old form did.
  vftDbgCat("PREPARE NETWORK: edges\n")
  p(0.75, "Weggewichte werden berechnet...")

  lenv <- igraph::edge_attr(network, "SHAPE_Leng")

  #DULN_WALK_ serves as DULN_ALL
  dulnCols <- c(walkNat   = "DULN_WALK_", walkSoc   = "DULN_WALK1",
                dogNat    = "DULN_DOG_N", dogProx   = "DULN_DOG_P",
                ebikeNat  = "DULN_EBIKE", bikeSport = "DULN_BIKER",
                jogger    = "DULN_JOGGE")
  #plain, _attr, _ATTR => slightly, moderately, heavily weighted by attractivity
  weightFactors <- c(8, 4, 2)
  weightSuffix  <- c("", "_attr", "_ATTR")

  weights <- vector("list", length(dulnCols) * length(weightFactors))
  names(weights) <- as.vector(outer(names(dulnCols), weightSuffix,
                                    function(a, b) paste0("SHAPE_Leng_", a, b)))
  for(k in seq_along(dulnCols)){
    duln <- igraph::edge_attr(network, dulnCols[[k]])
    for(j in seq_along(weightFactors)){
      x <- weightFactors[[j]] / (34.2423 + minThresh)
      #max distance is not increased, min distance is not 0 or negative
      weights[[paste0("SHAPE_Leng_", names(dulnCols)[[k]], weightSuffix[[j]])]] <-
        pmax(pmin(lenv * (1 - (1/(x * duln))), lenv), 10)
    }
  }

  #### write it all back onto the graph that came in ####
  p(0.95, "Wegnetz wird zusammengesetzt...")

  for(nm in names(weights))
    network <- igraph::set_edge_attr(network, nm, value = weights[[nm]])

  network <- igraph::set_vertex_attr(network, "AOI", value = AOI)
  if(!is.null(parkingCap)){
    network <- igraph::set_vertex_attr(network, "parking",        value = parkingCap)
    network <- igraph::set_vertex_attr(network, "parkingAttr",    value = parkingAttrV)
    network <- igraph::set_vertex_attr(network, "newResidential", value = newResidential)
  }
  class(network) <- cls

  p(1)

  list(network = network, parking = parking)
}

#' Make sure a scenario HAS a network, then do something with it.
#'
#' Stage 1 of what vftPrepareThen() used to do in one piece, split out because
#' step 5 needs this half on its own: its launch prepares the network and runs
#' the simulation in a SINGLE dispatch (see R/simulate_scenario.R), so it wants
#' the scenario in hand rather than a preparation already under way.
#'
#' THIS IS WHERE THE PATH NETWORK IS LOADED. No step's `needs` names `network`
#' any more (see R/steps.R), so steps 1-4 never touch the paths GDB at all: a
#' user who draws a perimeter, picks species, moves the attractiveness slider
#' three times and redraws their areas of interest pays for two raster crops and
#' nothing else. The two callers are the two places that genuinely read paths -
#' the simulation launch in step 5, and the newVersions page's "Wegen/Strassen"
#' and "Parken/Wohnen" contexts - and both arrive through here.
#'
#' Loaded ONCE. The raw graph lands in the app-level `r$network`, which is a
#' provider key and therefore survives until the PERIMETER changes; every
#' scenario built afterwards is seeded from it (app_server's step-4 confirm) and
#' every new version copies the prepared graph off scenario 1 (newVersions). So
#' the GDB read happens once per perimeter no matter how many simulations, new
#' versions, threshold changes or redrawn polygons follow.
#'
#' `then` is called with the SCENARIO, and in the same tick whenever nothing had
#' to be loaded - which matters for the newVersions page, whose caller bumps a
#' render trigger the map must not wait a flush for.
#'
#' @param r the CALLING MODULE's own reactiveValues; `pos` the index into its
#'   `r$networkList`.
#' @param enable input ids to put back if this cannot get to a network, so a
#'   failure does not leave the user looking at a dead button.
#' @param session the module's session; its userData is shared with the root's,
#'   which is how the provider layer is reached.
vftScenarioNetworkThen <- function(r, pos, then, enable = NULL,
                                   session = shiny::getDefaultReactiveDomain()){

  #Guarded rather than a bare [[ ]]: this runs inside an observer, and a
  #subscript error here would take that observer down with it rather than
  #producing a message the user can see. The list is briefly empty while an
  #invalidation rebuilds it, and briefly shorter than `pos` while the
  #newVersions page deletes a scenario.
  scenario <- shiny::isolate({
    nl <- r$networkList
    if(is.null(nl) || is.null(pos) || pos > length(nl)) NULL else nl[[pos]]
  })

  if(is.null(scenario)){
    #`then` is deliberately NOT called - there is nothing to run it against -
    #so put back whatever the caller disabled before asking, or the user is left
    #looking at a dead button.
    vftDbg(paste0("PREPARE: no scenario at position ", pos, " - nothing to do"))
    for(el in enable) try(shinyjs::enable(el), silent = TRUE)
    return(invisible(FALSE))
  }

  if(!is.null(scenario$network)){
    then(scenario)
    return(invisible(FALSE))
  }

  #The scenario was seeded from `r$network` when step 4 confirmed, and that was
  #NULL unless some earlier simulation had already paid for the load. Ask the
  #provider layer for it and come back to this same function when it lands, so
  #there is one path rather than two.
  #
  #The scenario is re-read on re-entry rather than captured here on purpose: the
  #load takes ~30s, and the user can delete a version or confirm new areas of
  #interest in that time. Every guard above therefore runs again against what is
  #actually in the list.
  appR <- vftAppReactives(session)
  base <- if(is.null(appR)) NULL else shiny::isolate(appR$network)

  if(!is.null(base)){
    #already loaded earlier in this session: free, and in this same tick.
    vftDbg(paste0("PREPARE: seeding scenario ", pos, " from the cached network"))
    shiny::isolate(r$networkList[[pos]]$network <- base)
    scenario$network <- base
    then(scenario)
    return(invisible(FALSE))
  }

  vftDbg(paste0("PREPARE: scenario ", pos, " has no network - loading it"))
  ok <- vftEnsureThen(
    "network",
    session = session,
    then = function(){
      vftScenarioNetworkThen(r, pos, then, enable = enable, session = session)
    },
    #the provider layer opens and closes its own progress bar and shows its
    #own error notification, so there is nothing to report here - only the
    #caller's buttons to put back.
    onFail = function(){
      for(el in enable) try(shinyjs::enable(el), silent = TRUE)
    })

  if(!isTRUE(ok)){
    #no provider server on this session (tests, or a build without one).
    #Nothing can produce the network, so behave like the missing-scenario
    #case above rather than dispatching a preparation against a NULL graph.
    vftDbg("PREPARE: no provider layer to load the network from")
    for(el in enable) try(shinyjs::enable(el), silent = TRUE)
  }
  invisible(ok)
}

#' Make sure a scenario's network is prepared, then do something with it.
#'
#' The newVersions page's door. Step 5's launch does NOT come through here any
#' more - it needs the preparation and the simulation to be one job, so it calls
#' vftScenarioNetworkThen() above and then vftSimulateScenario() in a single
#' dispatch. What is left is stage 1 plus the preparation dispatch, exactly as
#' before.
#'
#' On success the prepared network and the parking table are written back into
#' the scenario, so a second call is free.
#'
#' `then` is a callback rather than a return value for the reason the whole app
#' is written this way: nothing is awaited. When the network is already prepared
#' `then()` runs in the SAME tick.
#'
#' @param label progress bar message.
#' @param enable input ids for vftAsyncError() to re-enable if the job fails.
#' @param session the module's session.
vftPrepareThen <- function(r, pos, finalPolygons, minThresh, then,
                           label = "Wegnetz wird vorbereitet...",
                           enable = NULL,
                           session = shiny::getDefaultReactiveDomain()){

  vftScenarioNetworkThen(r, pos, enable = enable, session = session,
                         then = function(scenario){

    if(vftNetworkPrepared(scenario$network)){
      vftDbg(paste0("PREPARE: scenario ", pos, " already prepared"))
      then()
      return(invisible(FALSE))
    }

    vftDbg(paste0("PREPARE: scenario ", pos))

    network <- scenario$network
    parking <- scenario$parking

    #vftProgress, not ipc::AsyncProgress: the latter drags the whole session into
    #the worker. This dispatch was measured at 120 MB of session state when it
    #lived in step 4's confirm handler. See R/async_helpers.R.
    progress <- vftProgress(message = label,
                            detail  = "Dies sollte weniger als 30 Sekunden dauern",
                            queue   = ipc::shinyQueue(),
                            millis  = 1000)

    vftFuture({
      out <- vftPrepareNetwork(network, finalPolygons, minThresh,
                               parking = parking, progress = progress)
      progress$close()
      out
    }, seed = TRUE, progress = progress) %...>% (function(out){

      r$networkList[[pos]]$network <- out$network
      r$networkList[[pos]]$parking <- out$parking

      then()

    }) %...!% (vftAsyncError(progress, label, enable))

    invisible(TRUE)
  })
}
