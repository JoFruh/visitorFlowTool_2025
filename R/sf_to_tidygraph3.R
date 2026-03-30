#function to transform a road map into a network of nodes/edges

#inputs: x, a sf object of lines (paths)

#outputs: a tbl_graph for edges and nodes
#' @importFrom plyr .

sf_to_tidygraph3 = function(x, shape, directed = FALSE, parkingPolygons = NULL, progress = NULL) {


  #-----------------------------
  # 1. Build spatial network
  #-----------------------------

  net <- sfnetworks::as_sfnetwork(x, directed = directed)

  #-----------------------------
  # 2. Keep largest component
  #-----------------------------

  net <- net |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(comp = tidygraph::group_components()) |>
    dplyr::filter(comp == which.max(table(comp))) |>
    dplyr::select(-comp)

  #-----------------------------
  # 3. Extract nodes
  #-----------------------------

  nodes_sf <- net |>
    tidygraph::activate("nodes") |>
    sf::st_as_sf()

  coords <- sf::st_coordinates(nodes_sf)

  nodes <- nodes_sf |>
    dplyr::mutate(
      nodeID = dplyr::row_number(),
      X = coords[,1],
      Y = coords[,2]
    )
  # |>
  #   sf::st_drop_geometry()

  #-----------------------------
  # 4. Extract edges
  #-----------------------------

  edgs <- net |>
    tidygraph::activate("edges") |>
    sf::st_as_sf() |>
    dplyr::mutate(
      edgeID = dplyr::row_number(),
      from = as.integer(from),
      to   = as.integer(to)
    )

  #-----------------------------
  # 5. Convert attributes
  #-----------------------------

  edgs$walkBike  <- as.integer(edgs$walkBike) + 1
  edgs$roadWidth <- as.double(edgs$roadWidth)
  edgs$hardNatur <- as.integer(edgs$hardNatur)


#   edgs <- x |>
#     dplyr::mutate(edgeID = c(1:dplyr::n()))
#
#   nodes <- edgs |>
#     sf::st_coordinates() |>
#     dplyr::as_tibble() |>
#     dplyr::rename(edgeID = .data$L1) |>
#     dplyr::group_by(.data$edgeID) |>
#     dplyr::slice(c(1, dplyr::n())) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(start_end = rep(c('start', 'end'), times = dplyr::n()/2))
#
# #DEPRECATED
#   # nodes <- nodes |>
#   #   dplyr::mutate(xy = paste(.data$X, .data$Y)) |>
#   #   dplyr::mutate(nodeID = dplyr::group_indices(., factor(.data$xy, levels = unique(.data$xy)))) |>
#   #   dplyr::select(-.data$xy)
#
#   # #corrected
#   # nodes <- nodes |>
#   #   dplyr::mutate(xy = paste(.data$X, .data$Y)) |>
#   #   dplyr::group_by(xy) |>                           # group by xy
#   #   dplyr::mutate(nodeID = dplyr::cur_group_id()) |>        # assign group IDs
#   #   dplyr::ungroup() |>                               # ungroup to remove grouping
#   #   dplyr::select(-xy)                                # drop the temporary xy column
#
#
#   #updated
#   nodes <- nodes |>
#     dplyr::mutate(
#       X = round(X, 6),
#       Y = round(Y, 6),
#       xy = paste(X, Y)
#     ) |>
#     dplyr::group_by(xy) |>
#     dplyr::mutate(nodeID = dplyr::cur_group_id()) |>
#     dplyr::ungroup() |>
#     dplyr::select(-xy)
#
#
#   #add node ids into edge ids as start and end
#   source_nodes <- nodes |>
#     dplyr::filter(.data$start_end == 'start') |>
#     dplyr::pull(.data$nodeID)
#
#
#   target_nodes <- nodes |>
#     dplyr::filter(.data$start_end == 'end') |>
#     dplyr::pull(.data$nodeID)
#
#   edgs <- edgs |>
#     dplyr::mutate(from = source_nodes, to = target_nodes)
#
#   nodes <- nodes |>
#     dplyr::distinct(.data$nodeID, .keep_all = TRUE) |>
#     tidygraph::select(-c(.data$edgeID, .data$start_end)) |>
#     sf::st_as_sf(coords = c('X', 'Y')) |>
#     sf::st_set_crs(sf::st_crs(edgs))
#
#   #create tbl_graph
#   graph <- tidygraph::tbl_graph(nodes = dplyr::as_tibble(nodes), edges = dplyr::as_tibble(edgs), directed = directed)
#
#
#
#   #### FILTER OUT NON CONNECTED NODES####
#   #get the components of general graph
#   cmpnts <- igraph::components(graph)
#   largestID <- which(cmpnts$csize == max(cmpnts$csize))
#   #keep component with most nodes
#   largestNodes <-  dplyr::as_tibble(nodes)[cmpnts$membership %in% largestID, ]
#
#   #keep edges connected to retained nodes at both ends (from AND to)
#   largestEdges <- edgs[edgs$from %in% largestNodes$nodeID & edgs$to %in% largestNodes$nodeID, ]
#
#   rm(nodes)
#   rm(edgs)
#
#   #REPEAT PROCESS BUT IGNORING UNCONNECTED EDGES
#   edgs <- largestEdges |> #[, c(-8:-11)]
#     dplyr::mutate(edgeID = c(1:dplyr::n()))
#
#   #make char variables into integer
#   edgs$walkBike <- as.integer(edgs$walkBike)
#   edgs$roadWidth <- as.double(edgs$roadWidth)
#   edgs$hardNatur <- as.integer(edgs$hardNatur)
#   #make walkBike from 1 to 4, rather than 0 to 3
#   edgs$walkBike <- edgs$walkBike + 1
#
#   nodes <- edgs |>
#     sf::st_coordinates() |>
#     dplyr::as_tibble() |>
#     dplyr::rename(edgeID = .data$L1) |>
#     dplyr::group_by(.data$edgeID) |>
#     dplyr::slice(c(1, dplyr::n())) |>
#     dplyr::ungroup() |>
#     dplyr::mutate(start_end = rep(c('start', 'end'), times = dplyr::n()/2))
#
#
#   #DEPRECATED
#   # nodes <- nodes |>
#   #   dplyr::mutate(xy = paste(.data$X, .data$Y)) |>
#   #   dplyr::mutate(nodeID = dplyr::group_indices(., factor(.data$xy, levels = unique(.data$xy)))) |>
#   #   dplyr::select(-.data$xy)
#
#   #corrected
#   nodes <- nodes |>
#     dplyr::mutate(xy = paste(.data$X, .data$Y)) |>
#     dplyr::group_by(xy) |>                           # group by xy
#     dplyr::mutate(nodeID = dplyr::cur_group_id()) |>        # assign group IDs
#     dplyr::ungroup() |>                               # ungroup to remove grouping
#     dplyr::select(-xy)
#
#
#
#
#   # REMOVED DUE TO IMPLICIT ORDERING
#
#   # #add node ids into edge ids as start and end
#   # source_nodes <- nodes |>
#   #   dplyr::filter(.data$start_end == 'start') |>
#   #   dplyr::pull(.data$nodeID)
#   #
#   #
#   # target_nodes <- nodes |>
#   #   dplyr::filter(.data$start_end == 'end') |>
#   #   dplyr::pull(.data$nodeID)
#   #
#   # edgs = edgs |>
#   #   dplyr::mutate(from = source_nodes, to = target_nodes)
#
#   #Fix
#   edge_nodes <- nodes |>
#     dplyr::select(edgeID, nodeID, start_end) |>
#     tidyr::pivot_wider(
#       names_from = start_end,
#       values_from = nodeID
#     )
#
#   edgs <- edgs |>
#     dplyr::left_join(edge_nodes, by = "edgeID") |>
#     dplyr::rename(
#       from = start,
#       to   = end
#     )
#
#
#
#
#   nodes <- nodes |>
#     dplyr::distinct(.data$nodeID, .keep_all = TRUE) |>
#     tidygraph::select(-c(.data$edgeID, .data$start_end)) |>
#     sf::st_as_sf(coords = c('X', 'Y')) |>
#     sf::st_set_crs(sf::st_crs(edgs))


  ### ADD DULN DATA TO NODES

  #get raster
  # DULN <- terra::rast("www/data/maps/DULN/DULN_raster_web.tif" )

  #TODO: GET LATEST COG ATTR
  # attrMaps <- terra::rast("www/data/maps/attr/allAttrs_COG.tif")
  DULN <- terra::rast("www/data/maps/attr/allAttrs_COG_final.tif" )

  #grow shape slightly
  # shape <- terra::vect(sf::st_buffer(shape, dist = 1000))
  #crop
  DULN <- terra::crop(DULN, terra::project(terra::vect(shape), "EPSG:4326"))#attrMaps

  names(DULN) <- c("jog", "dogNat", "ebikeNat", "walkNat","dogProx","walkSoc","bikerSport")


  #convert nodes
  # node_points <- as_tibble( network_Tbl_allCH|>activate("nodes") )
  node_vect <- terra::vect(nodes)

  # DULN_all <- DULN$all
  # DULN_all <- terra::aggregate(DULN$all, fact = 2)
  # terra::plot(terra::focal(DULN_all, w=matrix(1, 3, 3), mean)) #1, 5, 5
  # #Get a single layer of DULN
  #
  # #Blur it to make it a smoother selection
  # DULN_all <- terra::focal(DULN$walkNat, w=matrix(1, 3, 3), mean)
  # DULN_all <- terra::aggregate(DULN_all, fact = 2)

  DULN_all <- terra::rast("www/data/maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif")
  DULN_all <- terra::crop(DULN_all, sf::st_transform(shape, "epsg:4326"))


  #sample raster with nodes
  node_DULN_data <- terra::extract(DULN_all, nodes[,c("X", "Y")])

  #for all agent rasters
  node_DULN_allAgents <- terra::extract(DULN[[1:7]], nodes[,c("X", "Y")])

  #remove IDs

  node_DULN_data <- node_DULN_data[,-1, drop = FALSE]
  node_DULN_allAgents <- node_DULN_allAgents[,-1]

# progress$inc(1/4, detail = "extracting relevant spatial data..")
  #RESIDENTIAL DATA ####
  residential_tif <- terra::rast("www/data/maps/residential/residentialData_raster_final.tif")

  residential_local <- terra::crop(residential_tif, terra::project(terra::vect(shape), "epsg:4326") )

  #extract residential data into nodes
  extrResults <- terra::extract(residential_local, nodes[,c("X", "Y")], cells = TRUE)
  extrResults <- extrResults[,-1]
  extrResults_new <- extrResults

  #cycle through all cells of residential raster to determine value of each path node within each cell (cell value spread across nodes within)
  for(i in min(extrResults_new$cell, na.rm = TRUE):max(extrResults_new$cell, na.rm = TRUE) ){
    #get nodes within specific cell
    cellResults <- extrResults_new$cell == i
    #convert NA to FALSE
    cellResults[is.na(cellResults)] <- FALSE
    #divide results of nodes within cell by number of nodes within cell
    extrResults_new$Band_1[cellResults] <- extrResults$Band_1[cellResults] / sum(cellResults) #divide residents nb in cell by nodes within cell
  }

  #add node_DULN data to original nodes
  colnames(extrResults) <- c("Residents", "cell")
  newnodes <- nodes |> dplyr::bind_cols(extrResults[, "Residents", drop = FALSE]) #remove cell column




  #PARKING DATA #### (moved to step 5)
  #similar to residential, but sampling parkingPolygons
  #populate nodes with 0s in $parking
#   newnodes$parking <- 0
#   newnodes$newResidential <- 0
#
# filteredNodes <- sf::st_filter(newnodes, parkingPolygons)
#
#   #cycle through parking polygons
#   for(polyNb in 1:nrow(parkingPolygons)){
#
#
#     # progress$inc((1/nrow(parkingPolygons))  / 4, detail = "Determining parking potential...")
#     #get polygon size (should be in meters if proj = 4326)
#     polyArea <- sf::st_area(parkingPolygons[polyNb,])
#     #convert to nb of agents (1agent / 30m^2)
#     agentNb <- polyArea/30
#     #get nodes within polygon and distribute number of agents equally among nodes (decimals allowed)
#     # ADD the values to nodes (pre-populated with 0s). This is to avoid conflicts with nodes already filled due to exception below.
#     nodeCount <- nrow(filteredNodes[parkingPolygons[polyNb,], op = sf::st_within])
#
#     if(nodeCount > 0){
#       #use sf polygon to select terra vect nodes (may have to convert to sf first)
#       isWithin <- sf::st_within(filteredNodes, parkingPolygons[polyNb,])
#       isWithin <- lengths(isWithin)
#       filteredNodes[isWithin > 0,]$parking <- filteredNodes[isWithin > 0,]$parking + ( as.numeric(agentNb)/nodeCount )
#     }else{
#       #CAPTURE EXCEPTION : no nodes in polygon
#       #in this case, find a single closest node outside polygon
#       nearestIndex <- sf::st_nearest_feature(parkingPolygons[polyNb,], sf::st_as_sf(filteredNodes))
#       filteredNodes$parking[[nearestIndex]] <- agentNb
#     }
#
#
#
#
#   }
#     #
#
# newnodes$parking[newnodes$nodeID %in% filteredNodes$nodeID] <- filteredNodes$parking
#


  #ADD DULN DATA TO NODES ####
  colnames(node_DULN_data) <- c("DULN")
  #rename to fit tool's existing name convention
  colnames(node_DULN_allAgents) <- c("DULN_JOGGE", "DULN_DOG_N", "DULN_EBIKE", "DULN_WALK_","DULN_DOG_P","DULN_WALK1","DULN_BIKER")
  newnodes <- newnodes |> dplyr::bind_cols(node_DULN_data) |> dplyr::bind_cols(node_DULN_allAgents)

  #add agentLoc to nodes (for Debug plotting agent locations)
  newnodes <- newnodes |> dplyr::bind_cols(data.frame(agentLoc = rep(0, nrow(nodes))))

  #rename geometry to stay in line with previous versions
  newnodes <- newnodes |> dplyr::rename(geometry = SHAPE)


  # print(edgs)
  #
  # #place edgeID as first column
  # edgs <- edgs |> select(edgeID, everything())
  #
  # print(edgs)
# progress$inc(1/4)
  #create tbl_graph
  graph <- tidygraph::tbl_graph(nodes = newnodes, edges = edgs, directed = directed)

  #Diagnostic
  cmp <- igraph::components(graph)

  print(cmp$no)
  print(sort(cmp$csize, decreasing = TRUE)[1:10])


  return(list(graph = list(graph),
              DULN = list(terra::wrap(DULN)),
              DULN_all = list(terra::wrap(DULN_all))) )

  # return(graph)
}

