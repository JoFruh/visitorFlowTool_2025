# function to manage multiple simulation launches (days and iterations)

#input:
  # pop = dataframe of general population (all agents and their charactersitics and starting positions)
  # network = the igraph network to use
  # days = the day structure to follow
  #AOIList = list of Areas of Interest determined for the study
  # iter = how many times each day is repeated to generate mean results (to account for stochasticity factors)

#output:
  # igraph network with path usage as an edge/vertex layer

#' @param progress the DATA PREPARATION bar: adjacency lists, agents, goals.
#' @param progressSim the SIMULATION bar, handed straight to launchSim(). Defaults
#'   to `progress`, so a caller with one bar behaves exactly as before.
#' @param base,span where this function's share of `progress` starts and how much
#'   of it it owns. Step 5 gives it the last 30%; a caller that owns the whole bar
#'   gets the default.
#' @param onPrepDone called once, after the agents have their goals and before the
#'   ABM starts. Step 5 closes the preparation bar here, so that the two bars
#'   appear in sequence rather than stacked.
launchMultiSim <- function(pop, network, days, finalPolygons, iter = 1,
                           progress = NULL, progressSim = progress,
                           base = 0, span = 1, onPrepDone = NULL){

  p <- function(f, detail = NULL){
    if(!is.null(progress))
      try(progress$set(base + span * f, detail = detail), silent = TRUE)
  }

  areasOfInterest <- NULL

  #THE TWO TABLES ARE BUILT ONCE AND PASSED DOWN. launchSim() used to build them
  #again for itself, so every run materialised the whole edge table - tens of
  #thousands of rows, geometry included - twice.
  #
  #vftGraphTibble() for the same reason as in launchSim_v2.R: this runs inside
  #the worker too, and as_tibble() on edges carrying an sf geometry column is
  #what killed the ABM on tibble < 3.3. See R/graph_helpers.R.
  p(0, "Wegnetz wird für die Simulation aufbereitet...")
  edgeTable   <- vftGraphTibble(network, "edges")
  vertexTable <- vftGraphTibble(network, "nodes")

  vftDbg("list of Pointers")
  #prepare adjacency lists for pathfinding in C++ (get pointers to C++ objects: avoids converting large tables back to R)
  listOfPointers <- generateAdjListAndDistTbl_cpp(edgeTable = edgeTable,
                                                  vertexTable = vertexTable)
# print(days)
  p(0.5, "Agenten werden erzeugt...")

  #determine population subset (currently whole pop)
  if(!exists("dayPop")){

    vftDbg("Subset Population")
    dayPop <- subsetPopulation(pop, currentDay, singleAgent = FALSE)

    vftDbg("Determine Agent Characteristics")
    dayPop <- determineAgentCharacteristics(dayPop, currentDay, network, finalPolygons = finalPolygons, listOfPointers = listOfPointers)
  }
  #launch a specific simulation

  p(1)
  if(is.function(onPrepDone)) try(onPrepDone(), silent = TRUE)

  vftDbg("Launch Simulation")

  #ARGUMENTS BY NAME. This used to read
  #`launchSim(dayPop, network, iter, trackable = FALSE, ...)`, where the third
  #POSITIONAL argument lands in `AOIList` rather than in `iter`. It is harmless -
  #launchSim() never reads AOIList and `iter` is 1 either way - and naming them
  #is what keeps it harmless.
  simData <- launchSim(dayPop = dayPop, network = network, AOIList = NULL,
                       listOfPointers = listOfPointers, iter = iter,
                       trackable = FALSE, areasOfInterest = areasOfInterest,
                       debug = FALSE, progress = progressSim,
                       edgeTable = edgeTable, vertexTable = vertexTable)

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
