#function to determine Agents' AOI goal, timer, habit etc..


determineAgentCharacteristics <- function(dayPop, currentDay, network, finalPolygons, listOfPointers){
  # Make sure it closes when we exit this reactive, even if there's an error
  # ABMprogress <- shiny::Progress$new()
  # on.exit(ABMprogress$close())
  # ABMprogress$set(message = "Erzeugen von Agenten", value = 0)
  #determine total timesteps of ABM (maximum timeExt of all agents)
  total <- length(dayPop$id)
  #insert relevant fields to base generatePopulation dataframes
  dayPop$active <- TRUE
  dayPop$AOIGoal <- NA
  dayPop$goalV <- NA
  dayPop$pathToGoal <- NA
  dayPop$moving <- 1
  dayPop$currentV <- NA
  dayPop$nextV <- NA
  dayPop$lastV <- NA
  dayPop$priorV <- list(NULL)
  dayPop$priorE <- list(NULL)
  dayPop$priorEAOI <- list(NULL)
  dayPop$shortestPriorV <- list(NULL)
  dayPop$nicestPriorV <- list(NULL)
  dayPop$nicestShortestPriorV <- list(NULL)
  dayPop$retracedV <- list(NULL)
  dayPop$timer <- 0
  dayPop$timeNotAccounted <- 0
  dayPop$timeExt <- NA
  dayPop$behaviour <- "shortest"
  dayPop$speed <- NA
  dayPop$mobility <- NA
  dayPop$totalDistance <- NA
  dayPop$currentE <- NA

  #instead, rapidly determine speed and durations based on agentTyp
  #durations per agent type

  #walkNat
  dayPop$timeExt[dayPop$agentTyp == "walkNat"] <- sample(c(30, 60, 120, 180, 240), nrow(dayPop[dayPop$agentTyp == "walkNat",] ),
                                                         prob = c(7.50, 12.50, 51.25, 23.75,  5.00), replace = TRUE)
  #walkProx
  dayPop$timeExt[dayPop$agentTyp == "walkSoc"] <- sample(c(30, 60, 120, 180, 240), nrow(dayPop[dayPop$agentTyp == "walkSoc",] ),
                                                         prob = c(6, 34, 58, 0,  2), replace = TRUE)
  #dogNat
  dayPop$timeExt[dayPop$agentTyp == "dogNat"] <- sample(c(30, 60, 120, 180, 240), nrow(dayPop[dayPop$agentTyp == "dogNat",] ),
                                                         prob = c(0, 34.28, 40, 22.857,  2.87), replace = TRUE)
  #dogProx
  dayPop$timeExt[dayPop$agentTyp == "dogProx"] <- sample(c(30, 60, 120, 180, 240), nrow(dayPop[dayPop$agentTyp == "dogProx",] ),
                                                         prob = c(0, 55.55, 40.7, 3.7,  0), replace = TRUE)
  #ebikeNat
  dayPop$timeExt[dayPop$agentTyp == "ebikeNat"] <- sample(c(30, 60, 120, 180, 240), nrow(dayPop[dayPop$agentTyp == "ebikeNat",] ),
                                                         prob = c(0, 36.84, 42.1, 21.05,  0), replace = TRUE)
  #bikeSport
  dayPop$timeExt[dayPop$agentTyp == "bikeSport"] <- sample(c(30, 60, 120, 180, 240), nrow(dayPop[dayPop$agentTyp == "bikeSport",] ),
                                                         prob = c(0, 18.75, 56.25, 25,  0), replace = TRUE)
  #jogger
  dayPop$timeExt[dayPop$agentTyp == "jogger"] <- sample(c(30, 60, 120, 180, 240), nrow(dayPop[dayPop$agentTyp == "jogger",] ),
                                                         prob = c(5.15, 31.818, 44.848, 13.939,  4.24), replace = TRUE)



  #speeds
  #walkNat
  dayPop$speed[dayPop$agentTyp == "walkNat"] <- 5
  #walkProx
  dayPop$speed[dayPop$agentTyp == "walkSoc"] <- 3
  #dogNat
  dayPop$speed[dayPop$agentTyp == "dogNat"] <- 3
  #dogProx
  dayPop$speed[dayPop$agentTyp == "dogProx"] <- 2
  #ebikeNat
  dayPop$speed[dayPop$agentTyp == "ebikeNat"] <- 20
  #bikeSport
  dayPop$speed[dayPop$agentTyp == "bikeSport"] <- 20
  #jogger
  dayPop$speed[dayPop$agentTyp == "jogger"] <- 7





  #old method of cycling over agents DEPRECATED
#   for(agentID in dayPop$id){
#
#     ABMprogress$set(which(dayPop$id == agentID)/total )
#
#     #assign row as agent
#     agent <- dayPop[dayPop$id == agentID, ]
#
#     ##TIME EXTENT (time available for recreation; dependant of currentDay and agentTyp)
#     #TODO: develop finer means of selecting time extent
#     #FOR NOW: randomly choose within predetermined possiblities, depending on day of the week and retirement age
#     if(agent$age < 65 & currentDay %in% 1:5){
#       agent$timeExt <- stats::runif(1, 40, 120)
#     }else{
#       agent$timeExt <- stats::runif(1,420, 640)
#     }
#
#     ##HABIT (from 1-5, least habit based to most habit based; dependant on timeExt)
#     #TODO: develop finer means of determining habit
#     #FOR NOW: link it to time available and base characteristic of agent (Barbara et al. 2012, less time leads to higher habits)
#     if(agent$timeExt <= 30){
#       agent$habit <- agent$habit + 2
#       if(agent$habit > 5){agent$habit <- 5}
#     }else if (agent$timeExt > 30 & agent$timeExt < 60){
#       agent$habit <- agent$habit
#       if(agent$habit > 5){agent$habit <- 5}
#     }else{
#       agent$habit <- agent$habit - 2
#       if(agent$habit < 1){agent$habit <- 1}
#     }
#
#     ##MOBILITY (walk, jog, bike etc.)
#     #TODO: choose mobility following general trends (walk often, bike less often etc...)
#     #FOR NOW: sample equivalently among possibilities
#     #choose one from what is available for this agent
#     choices <- agent[c("walk", "run", "bike")] %in% c("X")
#     mobilityChoices <- c("walking", "jogging", "biking")
#     agent$mobility <- sample(mobilityChoices[choices], 1)
#
#     #SPEED
#     #TODO: determine speed more precisely (stochasticity, age etc..)
#     if(agent$mobility == "walking"){
#       agent$speed <- 2
#     }else if(agent$mobility == "jogging"){
#       agent$speed <- 4
#     }else if(agent$mobility == "biking"){
#       agent$speed <- 12
#     }
#
#     dayPop[dayPop$id == agentID,] <- agent
#
#     #Determine if an agent will follow a habit or not, based on timeExtent, age?
#     #TODO: flesh out habit process. Select from a series of habits? Determine habits by simulating time prior to simulations.
#     #Find the most attractive/short/close route with differeing weights.
#     #FORNOW: ignore habit process
#
#
#
#     #determine closest AOI
#     # aoiDist <- NULL
#
#
#     #DETERMINE CLOSEST NODE OF ALL NON-"0" NODES (TOO SLOW)
#
#     #get shortest dist to AOI
#     # aoiShortestPath <- determineShortestPath(network, agent$startV, V(network)$nodeID[V(network)$AOI != "0"] )
#
#     # aoiShortestDist <- sum(E(network)$Shape_Leng[aoiNewPath$epath])
#     #get AOI of destination node
#     # vertices <- ends(network, aoiShortestPath$epath)
#     # lastVerts <- vertices[length(vertices)]
#     # lastVertexID <- lastVerts[!lastVerts %in% vertices[length(vertices)-1]]
#     # agent$AOIGoal <- V(network)$AOI[V(network)$nodeID == lastVertexID]
#     #moved to main Sim to maintain link
# #
#
#
#
#
# #     #TRY BFS (Breadth-First Search)
# #     #use vector of nodeIDs that are not "0" rather than V(network) due to bug
# #     df_network <- as_data_frame(network)
# #     AOIv <-df_network[df_network$AOI != "0", ]$nodeID
# #
# #     #make callback to stop BFS when appropriate vertex visited
# #     callback_f <- function(graph, data, extra) {
# #           #visited vertex is in the AOIv (is part of an AOI) and not same as startV
# #           data["vid"] %in% AOIv & data["vid"] != agent$startV
# #     }
# #     bfs_data <- bfs(network, root=agent$startV, callback=callback_f)
# #     #get visited vertices
# #     bfs_vertices <- bfs_data$order[!is.na(bfs_data$order)]
# #     #get AOI of last visited vertex
# #     agent$AOIGoal <-  bfs_vertices[length(bfs_vertices)]$AOI
# #     agent$goalV <- bfs_vertices[length(bfs_vertices)]$nodeID
# #     #place agents on starting vertex
# #     #TODO (later): allow for different starting times for evaluating crowding
# #     agent$nextV <- agent$startV
# #     #alter orginal agent
# #     dayPop[dayPop$id == agentID,] <- agent
#   }



  #Use CPP function in finding shortest route to the different AOIs (A, B, C etc..)

  #for each agent, generate a list of AOIs from closest to furthest, a list of their distances, a list of their size and a list of their mean attractiveness
  #these can be used to decide which AOI to visit.

  #ListofPointers:

  #TODO: (use other factors to choose AOI), but currently, just choose the closest one
  # 1) use modified version of pathfinder to return distances of all paths for all agents (single source no targets)
    # 1a) take note of of distance, node and AOI type for the first of every AOI type encountered

  AOIList <- unique(igraph::V(network)$AOI)#get AOI types from network
  AOIList <- AOIList[AOIList != "0"]#remove "0" from AOIList


  #insert DULN value into cpp function to weight against distance (finalPolygons)
  # AOIList_DULN <- finalPolygons[, c("DULN", "AOI")]
  # ABMprogress$inc(2/4, message = "Erzeugen von Agenten")
  #listOfPointers [[1]] and [[2]] = vertices and IDs
  #listOfPointers [[3]]..., [[4]]... and [[5]]... = distances normal, attr and ATTR
  #listOfPointers ...[[1]] to ...[[7]] = walkNat, walkSoc, dogNat, dogProx ,ebikeNat, bikeSport, jogger
   AOIInfo <- findClosestAOI_cpp(AOIList,
                                igraph::V(network)$AOI,
                                listOfPointers[[1]],
                                listOfPointers[[2]],
                                listOfPointers[[4]][[1]],
                                listOfPointers[[4]][[2]],
                                listOfPointers[[4]][[3]],
                                listOfPointers[[4]][[4]],
                                listOfPointers[[4]][[5]],
                                listOfPointers[[4]][[6]],
                                listOfPointers[[4]][[7]],
                                dayPop$startV,
                                dayPop$speed,
                                dayPop$timeExt,
                                finalPolygons$AOI,
                                finalPolygons$DULN,
                                dayPop$agentTyp,
                                finalPolygons$area)

# ABMprogress$set(3/4, message = "Erzeugen von Agenten")
#use AOIList_DULN to translate polygon names to DULN values

#weight distances (times) with

for(agentNo in 1:length(AOIInfo$nodes)){
  #capture error
  if(!is.finite(sum(AOIInfo$prob[agentNo][[1]]) == 0)){browser()}
  # ABMprogress$inc((1/length(AOIInfo$nodes))/2, message = "Erzeugen von Agenten")
  if(sum(AOIInfo$prob[agentNo][[1]]) == 0){
    #if no areas are in proximity, label agent accordingly
    dayPop$goalV[[agentNo]] <- -1
    dayPop$AOIGoal[[agentNo]] <- "NO_GOAL"

  }else{
  #sample a series of indices
  whichAOI <- sample(1:length(AOIInfo$nodes[1][[1]]), 1, prob= AOIInfo$prob[agentNo][[1]])
  #use same index for goalV and AOIGoal
  dayPop$goalV[[agentNo]] <- AOIInfo$nodes[agentNo][[1]][[whichAOI]]
  dayPop$AOIGoal[[agentNo]] <- AOIInfo$aoi[agentNo][[1]][[whichAOI]]

  }

}
#
#   # 2) distribute the AOI info to all agents
#   dayPop$goalV <- mapply(function(x) x[[1]],
#                          AOIInfo$nodes)
#
#   dayPop$AOIGoal <- mapply(function(x) x[[1]],
#                            AOIInfo$aoi)
  #initialize start
  dayPop$nextV <- dayPop$startV



  print("DONE!")
  return(dayPop)
}
