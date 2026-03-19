#function to determine shortest overall path (vertex sequence) from a start position and a selection of goal positions.
  #inputs:
          #network = igraph network
          #startV = initial position
          #goalVertices = final positions

determineShortestPath <- function(network, startV, goalVertices){

  #TODO: create weights that account for attractiveness of path (ex: multiply length of path by attractiveness*0.025)
  #FOR NOW_ weights represent only euclidean distance
  shortPaths <- igraph::shortest_paths(network, startV, goalVertices, weights = igraph::E(network)$Shape_Leng, output = "both")

  #TODO: avoid using algorithm twice (shortest_paths AND distances). Perhaps using "epaths" list from shortPaths

  #dist <- distances(network, startV, to = goalVertices, weights = E(network)$Shape_Leng)
  translateEdgestoDist <- function(paths){
    #get distances for edges returned in shortest_paths
    distances <- NULL
    for(pth in paths){
      dist <- sum(igraph::E(network)$SHAPE_Leng[pth])
      distances <- c(distances, dist)

      }

    return(distances)
}

  #determine distances
  dist <- translateEdgestoDist(shortPaths$epath)
  #get shortest
  shortest <-  which(dist == min(dist))

  shortestPath <- list(vpath = shortPaths$vpath[shortest][[1]], epath = shortPaths$epath[shortest][[1]], totalDistance = min(dist))
  #remove first node (starting node, as agent will already be there)
  shortestPath$vpath <- list(shortestPath$vpath[2:length(shortestPath$vpath)])

  return(shortestPath)
}
