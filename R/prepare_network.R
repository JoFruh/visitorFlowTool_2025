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

#' Everything a network needs before it can be simulated on.
#'
#' Pure: no reactives, no session, no `r`. Meant to be called from inside
#' vftFuture(), which is where it used to live.
#'
#' @param network a tbl_graph - the SCENARIO's network, not `r$network`, so that
#'   any edges the newVersions page has already added or edited survive.
#' @param finalPolygons the confirmed areas of interest, carrying the `AOI`
#'   letters step 4 stamped on them (see the note there for why that one line
#'   stayed behind).
#' @param minThresh the step-3 attractiveness threshold.
#' @param parking an already-loaded parking table, to skip the read.
#' @param progress optional vftProgress handle.
#'
#' @return list(network = , parking = )
vftPrepareNetwork <- function(network, finalPolygons, minThresh,
                              parking = NULL, progress = NULL){

  #Step 3's skip button commits `isSkip` and nothing else, so minThresh is NULL
  #on that path - and `8/(34.2423 + NULL)` is numeric(0), which makes every
  #weight assignment below fail with "replacement has 0 rows". That was already
  #true in step 4; skip step 3, draw a polygon by hand, confirm, and it fired.
  #Zero is what the slider's own floor means, and a working simulation beats an
  #error carried from one step into another.
  if(is.null(minThresh) || !length(minThresh)) minThresh <- 0

  vftDbgCat("PREPARE NETWORK: AOI nodes\n")

  vertices <- sf::st_as_sf(dplyr::as_tibble(tidygraph::activate(network, nodes)))
  vertices <- sf::st_transform(vertices, 4326)
  vertices_vect <- terra::vect(vertices)

  vertices_AOI_data <- terra::extract(terra::vect(finalPolygons["AOI"]), vertices_vect)

  #add node_DULN data to original nodes
  newvertices <- vertices
  newvertices$AOI <- vertices_AOI_data$AOI
  newvertices$AOI[is.na(newvertices$AOI)] <- 0

  #for every polygon-extracted set of nodes, check whether they form a single
  #component; otherwise keep the largest one
  for(letter in finalPolygons$AOI){
    #get nodes with this letter
    letternode <- newvertices$nodeID[newvertices$AOI == letter]
    #determine components
    subnetwork <- igraph::subgraph(network, letternode)
    comp <- igraph::components(subnetwork, "strong")

    #restored but potential ERROR, keep an eye out
    # as large AOIs would have only one part active
    #keep largest components as is
    #smaller components: replace letter with "0"
    smllerCompIDs <- igraph::V(subnetwork)$nodeID[comp$membership %in% which(comp$csize != max(comp$csize))]
    #apply a "0" to all vertices in smaller components
    newvertices$AOI[newvertices$nodeID %in% smllerCompIDs] <- "0"

    #get largest comp and add letter to neighbouring nodes (buffer)
    largestCompID <- igraph::V(subnetwork)$nodeID[comp$membership %in% which(comp$csize == max(comp$csize))]
    #assign current letter to all neighbhood nodes around current AOI (buffer zone)
    newvertices$AOI[unique(unlist(igraph::neighborhood(network, 1, nodes = largestCompID)))] <- letter
  }

  vftDbgCat("PREPARE NETWORK: edges\n")

  #add edge AOICol column for Debugging agent movement
  newedges <- sf::st_as_sf(dplyr::as_tibble(tidygraph::activate(network, edges)))
  newedges <- sf::st_transform(newedges, 4326)

  #DULN_WALK_ serves as DULN_ALL
  # Not using AOICol, removed
  # newedges$AOICol <- ifelse(newedges$DULN_WALK_ > minThresh, 1, 0)

  #The READ is what an already-loaded table skips. The node attribution below is
  #not: it used to sit inside this branch, so handing the function a parking
  #table produced a network with no `parking` / `parkingAttr` / `newResidential`
  #columns at all - which generatePopulation() reads, and which
  #vftNetworkPrepared() tests, so the scenario would come back "not prepared"
  #and be prepared again on the next launch, forever. (The same shape was in
  #step 4's confirm handler, where a second confirm in one visit hit it.)
  if(is.null(parking)){
    if(!is.null(progress)) try(progress$inc(1/2, detail = "Loading parking info..."), silent = TRUE)

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
  if(!is.null(parking)){

      #PARKING DATA ####
      #similar to residential, but sampling parkingPolygons
      #populate nodes with 0s in $parking
      newvertices$parking <- 0
      newvertices$parkingAttr <- 0
      newvertices$newResidential <- 0

      #problematic to use filtered nodes, use all nodes instead? or buffered areas around parking
      filteredNodes <- sf::st_filter(newvertices, sf::st_buffer(parking, 50) )

      # Pre-compute all spatial relationships ONCE before the loop instead of once per iteration:
      # 1. Area / agentNb for every parking polygon (vectorised)
      polyAreas    <- as.numeric(sf::st_area(parking)) / 30
      # 2. Nearest AOI for every parking polygon (vectorised, single call)
      nearestAOIs  <- sf::st_nearest_feature(parking, finalPolygons)
      parkingAttrs <- finalPolygons$DULN[nearestAOIs]
      # 3. Which filteredNodes fall within each parking polygon (one spatial op for all polygons)
      nodesInParkings <- sf::st_contains(parking, filteredNodes, sparse = TRUE)

      if(!is.null(progress)) try(progress$inc(1/2, detail = "Determining parking potential..."), silent = TRUE)

      #cycle through parking polygons
      for(polyNb in seq_len(nrow(parking))){

        nodeIndices <- nodesInParkings[[polyNb]]
        nodeCount   <- length(nodeIndices)
        agentNb     <- polyAreas[[polyNb]]
        nearestAttr <- parkingAttrs[[polyNb]]

        if(nodeCount > 0){
          #add number of agents a parking can hold (per node within parking)
          filteredNodes$parking[nodeIndices]    <- filteredNodes$parking[nodeIndices] + (agentNb / nodeCount)
          filteredNodes$parkingAttr[nodeIndices] <- nearestAttr

        }else{
          #CAPTURE EXCEPTION : no nodes in polygon
          #in this case, find a single closest node outside polygon
          nearestNodeIdx <- sf::st_nearest_feature(parking[polyNb,], sf::st_as_sf(filteredNodes))

          if(!is.na(nearestNodeIdx)){
            filteredNodes$parking[[nearestNodeIdx]]    <- filteredNodes$parking[[nearestNodeIdx]] + agentNb
            filteredNodes$parkingAttr[[nearestNodeIdx]] <- nearestAttr
          }
        }
      }
      #
      #transfer parking info back to network
      newvertices$parking[newvertices$nodeID %in% filteredNodes$nodeID] <- filteredNodes$parking
      newvertices$parkingAttr[newvertices$nodeID %in% filteredNodes$nodeID] <- filteredNodes$parkingAttr

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

  xDist <- 8/(34.2423 + minThresh)
  xattr <- 4/(34.2423 + minThresh)
  xATTR <- 2/(34.2423 + minThresh)

  newedges$SHAPE_Leng_walkNat_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_WALK_ ))) #+ abs(min(newedges$DULN_WALK_)) + 1
  newedges$SHAPE_Leng_walkSoc_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_WALK1 )))
  newedges$SHAPE_Leng_dogNat_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_DOG_N )))
  newedges$SHAPE_Leng_dogProx_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_DOG_P )))
  newedges$SHAPE_Leng_ebikeNat_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_EBIKE)))
  newedges$SHAPE_Leng_bikeSport_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_BIKER )))
  newedges$SHAPE_Leng_jogger_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_JOGGE )))

  newedges$SHAPE_Leng_walkNat_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_WALK_ )))
  newedges$SHAPE_Leng_walkSoc_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_WALK1 )))
  newedges$SHAPE_Leng_dogNat_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_DOG_N )))
  newedges$SHAPE_Leng_dogProx_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_DOG_P )))
  newedges$SHAPE_Leng_ebikeNat_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_EBIKE )))
  newedges$SHAPE_Leng_bikeSport_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_BIKER )))
  newedges$SHAPE_Leng_jogger_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_JOGGE )))

  newedges$SHAPE_Leng_walkNat <- newedges$SHAPE_Leng* (1-(1/(xDist*newedges$DULN_WALK_ )))
  newedges$SHAPE_Leng_walkSoc <- newedges$SHAPE_Leng* (1-(1/(xDist*newedges$DULN_WALK1 )))
  newedges$SHAPE_Leng_dogNat <- newedges$SHAPE_Leng* (1-(1/(xDist*newedges$DULN_DOG_N )))
  newedges$SHAPE_Leng_dogProx <- newedges$SHAPE_Leng* (1-(1/(xDist*newedges$DULN_DOG_P )))
  newedges$SHAPE_Leng_ebikeNat <- newedges$SHAPE_Leng* (1-(1/(xDist*newedges$DULN_EBIKE )))
  newedges$SHAPE_Leng_bikeSport <- newedges$SHAPE_Leng* (1-(1/(xDist*newedges$DULN_BIKER )))
  newedges$SHAPE_Leng_jogger <- newedges$SHAPE_Leng * (1-(1/(xDist*newedges$DULN_JOGGE )))

  #make sure max distance is not increased
  newedges$SHAPE_Leng_walkNat[newedges$SHAPE_Leng_walkNat > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_walkNat > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_walkSoc[newedges$SHAPE_Leng_walkSoc > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_walkSoc > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_dogNat[newedges$SHAPE_Leng_dogNat > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_dogNat > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_dogProx[newedges$SHAPE_Leng_dogProx > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_dogProx > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_ebikeNat[newedges$SHAPE_Leng_ebikeNat > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_ebikeNat > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_bikeSport[newedges$SHAPE_Leng_bikeSport > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_bikeSport > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_jogger[newedges$SHAPE_Leng_jogger > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_jogger > newedges$SHAPE_Leng]

  newedges$SHAPE_Leng_walkNat_attr[newedges$SHAPE_Leng_walkNat_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_walkNat_attr > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_walkSoc_attr[newedges$SHAPE_Leng_walkSoc_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_walkSoc_attr > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_dogNat_attr[newedges$SHAPE_Leng_dogNat_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_dogNat_attr > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_dogProx_attr[newedges$SHAPE_Leng_dogProx_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_dogProx_attr > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_ebikeNat_attr[newedges$SHAPE_Leng_ebikeNat_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_ebikeNat_attr > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_bikeSport_attr[newedges$SHAPE_Leng_bikeSport_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_bikeSport_attr > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_jogger_attr[newedges$SHAPE_Leng_jogger_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_jogger_attr > newedges$SHAPE_Leng]

  newedges$SHAPE_Leng_walkNat_ATTR[newedges$SHAPE_Leng_walkNat_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_walkNat_ATTR > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_walkSoc_ATTR[newedges$SHAPE_Leng_walkSoc_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_walkSoc_ATTR > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_dogNat_ATTR[newedges$SHAPE_Leng_dogNat_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_dogNat_ATTR > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_dogProx_ATTR[newedges$SHAPE_Leng_dogProx_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_dogProx_ATTR > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_ebikeNat_ATTR[newedges$SHAPE_Leng_ebikeNat_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_ebikeNat_ATTR > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_bikeSport_ATTR[newedges$SHAPE_Leng_bikeSport_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_bikeSport_ATTR > newedges$SHAPE_Leng]
  newedges$SHAPE_Leng_jogger_ATTR[newedges$SHAPE_Leng_jogger_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_jogger_ATTR > newedges$SHAPE_Leng]

  #make sure min distance is not 0 or negative
  newedges$SHAPE_Leng_walkNat[newedges$SHAPE_Leng_walkNat < 10] <- 10
  newedges$SHAPE_Leng_walkSoc[newedges$SHAPE_Leng_walkSoc < 10] <- 10
  newedges$SHAPE_Leng_dogNat[newedges$SHAPE_Leng_dogNat < 10] <- 10
  newedges$SHAPE_Leng_dogProx[newedges$SHAPE_Leng_dogProx < 10] <- 10
  newedges$SHAPE_Leng_ebikeNat[newedges$SHAPE_Leng_ebikeNat < 10] <-10
  newedges$SHAPE_Leng_bikeSport[newedges$SHAPE_Leng_bikeSport < 10] <- 10
  newedges$SHAPE_Leng_jogger[newedges$SHAPE_Leng_jogger < 10] <- 10

  newedges$SHAPE_Leng_walkNat_attr[newedges$SHAPE_Leng_walkNat_attr < 10] <- 10
  newedges$SHAPE_Leng_walkSoc_attr[newedges$SHAPE_Leng_walkSoc_attr < 10] <- 10
  newedges$SHAPE_Leng_dogNat_attr[newedges$SHAPE_Leng_dogNat_attr < 10] <- 10
  newedges$SHAPE_Leng_dogProx_attr[newedges$SHAPE_Leng_dogProx_attr < 10] <- 10
  newedges$SHAPE_Leng_ebikeNat_attr[newedges$SHAPE_Leng_ebikeNat_attr < 10] <- 10
  newedges$SHAPE_Leng_bikeSport_attr[newedges$SHAPE_Leng_bikeSport_attr < 10] <- 10
  newedges$SHAPE_Leng_jogger_attr[newedges$SHAPE_Leng_jogger_attr < 10] <- 10

  newedges$SHAPE_Leng_walkNat_ATTR[newedges$SHAPE_Leng_walkNat_ATTR < 10] <- 10
  newedges$SHAPE_Leng_walkSoc_ATTR[newedges$SHAPE_Leng_walkSoc_ATTR < 10] <-10
  newedges$SHAPE_Leng_dogNat_ATTR[newedges$SHAPE_Leng_dogNat_ATTR < 10] <-10
  newedges$SHAPE_Leng_dogProx_ATTR[newedges$SHAPE_Leng_dogProx_ATTR < 10] <- 10
  newedges$SHAPE_Leng_ebikeNat_ATTR[newedges$SHAPE_Leng_ebikeNat_ATTR < 10] <- 10
  newedges$SHAPE_Leng_bikeSport_ATTR[newedges$SHAPE_Leng_bikeSport_ATTR < 10] <-10
  newedges$SHAPE_Leng_jogger_ATTR[newedges$SHAPE_Leng_jogger_ATTR < 10] <-10

  #create tbl_graph
  newNetwork <- tidygraph::tbl_graph( nodes = dplyr::as_tibble(newvertices), edges = dplyr::as_tibble(newedges), directed = FALSE )

  list(network = newNetwork, parking = parking)
}

#' Make sure a scenario's network is prepared, then do something with it.
#'
#' The one door both call sites use, so that "has this scenario been prepared"
#' is answered in one place rather than two.
#'
#' `r` is the CALLING MODULE's own reactiveValues - step 5 and the newVersions
#' page each shadow the app-level one - and `pos` the index into its
#' `r$networkList`. On success the prepared network and the parking table are
#' written back into that scenario, so a second call is free.
#'
#' `then` is a callback rather than a return value for the reason the whole app
#' is written this way: nothing is awaited. When the network is already prepared
#' `then()` runs in the SAME tick, which matters for the newVersions page - its
#' caller bumps a render trigger and the map must not wait a flush for it.
#'
#' @param label progress bar message.
#' @param enable input ids for vftAsyncError() to re-enable if the job fails.
vftPrepareThen <- function(r, pos, finalPolygons, minThresh, then,
                           label = "Wegnetz wird vorbereitet...",
                           enable = NULL){

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
}
