# function to manage multiple simulation launches (days and iterations)

#input:
  # pop = dataframe of general population (all agents and their charactersitics and starting positions)
  # network = the igraph network to use
  # days = the day structure to follow
  #AOIList = list of Areas of Interest determined for the study
  # iter = how many times each day is repeated to generate mean results (to account for stochasticity factors)

#output:
  # igraph network with path usage as an edge/vertex layer

launchMultiSim <- function(pop, network, days, finalPolygons, iter = 1, progress = NULL){
  progress$set(0, message = "Preparing the ABM...")
  areasOfInterest <- NULL

  vftDbg("list of Pointers")
  #prepare adjacency lists for pathfinding in C++ (get pointers to C++ objects: avoids converting large tables back to R)
  #vftGraphTibble() for the same reason as in launchSim_v2.R: this runs inside
  #the worker too, and as_tibble() on edges carrying an sf geometry column is
  #what killed the ABM on tibble < 3.3. See R/graph_helpers.R.
  listOfPointers <- generateAdjListAndDistTbl_cpp(edgeTable = vftGraphTibble(network, "edges"),
                                                  vertexTable = vftGraphTibble(network, "nodes"))
# print(days)
progress$inc(1/2)

  #determine population subset (currently whole pop)
  if(!exists("dayPop")){

    vftDbg("Subset Population")
    dayPop <- subsetPopulation(pop, currentDay, singleAgent = FALSE)

    vftDbg("Determine Agent Characteristics")
    dayPop <- determineAgentCharacteristics(dayPop, currentDay, network, finalPolygons = finalPolygons, listOfPointers = listOfPointers)
  }
  #launch a specific simulation

  progress$inc(1/2)

  vftDbg("Launch Simulation")

  simData <- launchSim(dayPop, network, iter, trackable = FALSE, areasOfInterest = areasOfInterest, listOfPointers = listOfPointers, debug = FALSE, progress = progress)

  #delete listOfPointers
  for(i in length(listOfPointers):1){
    if(length(listOfPointers[[i]]) > 1){
      for(j in length(listOfPointers[[i]]):1){
        if(!is.null(listOfPointers[[i]][[j]])){
          listOfPointers[[i]][[j]] <- NULL
      }
      }
    }else{
      listOfPointers[[i]] <- NULL
    }
  }

  # rm(listOfPointers)
  return(simData)
}
