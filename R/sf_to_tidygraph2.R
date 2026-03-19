#function to transform a road map into a network of nodes/edges

#inputs: x, a sf object of lines (paths)

#outputs: a tbl_graph for edges and nodes
#' @importFrom plyr .

sf_to_tidygraph2 = function(x, shape, directed = TRUE) {
  edgs <- x %>%
    dplyr::mutate(edgeID = c(1:dplyr::n()))

  nodes <- edgs %>%
    sf::st_coordinates() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(edgeID = .data$L1) %>%
    dplyr::group_by(.data$edgeID) %>%
    dplyr::slice(c(1, dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(start_end = rep(c('start', 'end'), times = dplyr::n()/2))


  nodes <- nodes %>%
    dplyr::mutate(xy = paste(.data$X, .data$Y)) %>%
    dplyr::mutate(nodeID = dplyr::group_indices(., factor(.data$xy, levels = unique(.data$xy)))) %>%
    dplyr::select(-.data$xy)

  #add node ids into edge ids as start and end
  source_nodes <- nodes %>%
    dplyr::filter(.data$start_end == 'start') %>%
    dplyr::pull(.data$nodeID)


  target_nodes <- nodes %>%
    dplyr::filter(.data$start_end == 'end') %>%
    dplyr::pull(.data$nodeID)

  edgs <- edgs %>%
    dplyr::mutate(from = source_nodes, to = target_nodes)

  nodes <- nodes %>%
    dplyr::distinct(.data$nodeID, .keep_all = TRUE) %>%
    tidygraph::select(-c(.data$edgeID, .data$start_end)) %>%
    sf::st_as_sf(coords = c('X', 'Y')) %>%
    sf::st_set_crs(sf::st_crs(edgs))

  #create tbl_graph
  graph <- tidygraph::tbl_graph(nodes = dplyr::as_tibble(nodes), edges = dplyr::as_tibble(edgs), directed = directed)



  #### FILTER OUT NON CONNECTED NODES####
  #get the components of general graph
  cmpnts <- igraph::components(graph)
  largestID <- which(cmpnts$csize == max(cmpnts$csize))
  #keep component with most nodes
  largestNodes <-  dplyr::as_tibble(nodes)[cmpnts$membership %in% largestID, ]

  #keep edges connected to retained nodes at both ends (from AND to)
  largestEdges <- edgs[edgs$from %in% largestNodes$nodeID & edgs$to %in% largestNodes$nodeID, ]

  rm(nodes)
  rm(edgs)

  #REPEAT PROCESS BUT IGNORING UNCONNECTED EDGES
  edgs <- largestEdges[, c(-8:-11)] %>%
    dplyr::mutate(edgeID = c(1:dplyr::n()))

  nodes <- edgs %>%
    sf::st_coordinates() %>%
    dplyr::as_tibble() %>%
    dplyr::rename(edgeID = .data$L1) %>%
    dplyr::group_by(.data$edgeID) %>%
    dplyr::slice(c(1, dplyr::n())) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(start_end = rep(c('start', 'end'), times = dplyr::n()/2))


  nodes <- nodes %>%
    dplyr::mutate(xy = paste(.data$X, .data$Y)) %>%
    dplyr::mutate(nodeID = dplyr::group_indices(., factor(.data$xy, levels = unique(.data$xy)))) %>%
    dplyr::select(-.data$xy)

  #add node ids into edge ids as start and end
  source_nodes <- nodes %>%
    dplyr::filter(.data$start_end == 'start') %>%
    dplyr::pull(.data$nodeID)


  target_nodes <- nodes %>%
    dplyr::filter(.data$start_end == 'end') %>%
    dplyr::pull(.data$nodeID)

  edgs = edgs %>%
    dplyr::mutate(from = source_nodes, to = target_nodes)


  nodes <- nodes %>%
    dplyr::distinct(.data$nodeID, .keep_all = TRUE) %>%
    tidygraph::select(-c(.data$edgeID, .data$start_end)) %>%
    sf::st_as_sf(coords = c('X', 'Y')) %>%
    sf::st_set_crs(sf::st_crs(edgs))


  ### ADD DULN DATA TO NODES

  #get raster
  DULN <- envBase$DULN$all

  #grow shape slightly
  # shape <- terra::vect(sf::st_buffer(shape, dist = 1000))
  # #crop
  # DULN <- terra::crop(DULN, shape)

  #convert nodes
  # node_points <- as_tibble( network_Tbl_allCH%>%activate("nodes") )
  node_vect <- terra::vect(nodes)

  #sample raster with nodes
  node_DULN_data <- terra::extract(DULN, nodes)

  #RESIDENTIAL DATA ####
  residential_tif <- terra::rast("www/data/maps/residential/residentialData_raster_final.tif")
  residential_local <- terra::crop(terra::project(residential_tif, "epsg:3857"), terra::project(terra::vect(shape), "epsg:3857") )

  #extract residential data into nodes
  extrResults <- terra::extract(residential_local, nodes, cells = TRUE)
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
  colnames(extrResults) <- c("ID", "Residents", "cell")
  newnodes <- nodes %>% dplyr::bind_cols(extrResults[, 1:2]) #remove cell column


  #PARKING DATA ####
  #crop parking data
  parking_vect <- terra::vect("www/data/maps/parking/parkingData.shp")
  #similar to residential, but sampling polygons
  terra::extract()
  #cycle through parking polygons

    #

  #add node_DULN data to original nodes
  colnames(node_DULN_data) <- c("ID", "DULN")
  newnodes <- newnodes %>% dplyr::bind_cols(node_DULN_data)

  #add agentLoc to nodes (for Debug plotting agent locations)
  newnodes <- newnodes %>% dplyr::bind_cols(data.frame(agentLoc = rep(0, nrow(nodes))))

  # print(edgs)
  #
  # #place edgeID as first column
  # edgs <- edgs %>% select(edgeID, everything())
  #
  # print(edgs)

  #create tbl_graph
  graph <- tidygraph::tbl_graph(nodes = dplyr::as_tibble(newnodes), edges = dplyr::as_tibble(edgs), directed = directed)


  return(graph)
}

