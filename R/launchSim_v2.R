#function of the MAIN simulation.
#this will determine and track the individual movement of all agents.

#inputs:
  #dayPop = the dataframe of agents active this day
  #network = the path network to use
  #iter = number of iterations (default 1)

#output:
  #pathUsage = network with edge layer revealing intensity of path usage
  #dayPop = modified agent dataframe with updated visited Areas of Interest.



edges <- NULL
nodes <- NULL

#' @param edgeTable,vertexTable the graph as tables, when the caller already has
#'   them. launchMultiSim() builds both for the adjacency lists one call up, and
#'   this function used to build them again for itself - two full materialisations
#'   of a tens-of-thousands-of-rows edge table, geometry included, per run.
#'   Computed here when NULL, so any other caller is unaffected.
launchSim <- function(dayPop, network, AOIList, listOfPointers, iter = 1, trackable = FALSE, areasOfInterest = NULL, debug = FALSE, progress = NULL,
                      edgeTable = NULL, vertexTable = NULL){

  # save(dayPop, network, AOIList,iter, trackable, areasOfInterest, debug, file = "dataForAssessement_2000agents.RData")
  #### sub - functions ####
  # calculate_timeTimestepsRatio <- function(network, dayPop){
  #   #converts time to timesteps by defining the time it takes to walk at a moderate pace 10 meters as 1 timestep.
  #   #time unit = minutes
  #
  #   #30 timesteps in 1 minute (2 seconds per timestep == too long?)
  #   # timeTimestepsRatio <- 30
  #
  #   #3 timesteps in 1 minute (20 seconds per timestep)
  #   # timeTimestepsRatio <- 3
  #   #10 second per timestep
  #   timeTimestepsRatio <- 6
  #   return(timeTimestepsRatio)
  # }
  #   #20 second per timestep ( 3 timesteps in 1 minute)
  #this may overly slow down agents at complex intersections. But doubles speed of simulation
  timeTimestepsRatio <- 3
  convertToTimesteps <- function(timeInput, unit = "h"){

    if(unit == "h"){
      timesteps <- timeInput * timeTimestepsRatio * 60
    }else if(unit == "m"){
      timesteps <- timeInput * timeTimestepsRatio
    }

    return(ceiling(timesteps))
  }
  #### variables ####
  # #intialise usage accounting for agent passage
  # V(network)$passage <- 0
  # E(network)$passage <- 0

  #prepare easy access edge table
  #
  #vftGraphTibble(), not as_tibble(activate(...)): these two lines are where the
  #ABM died on the server with "All columns in a tibble must be vectors". The
  #edges carry SHAPE (an sf geometry) and the nodes carry `geometry`, and an sfc
  #is a classed list, which tibble < 3.3 / vctrs < 0.7 refuse as a column. The
  #helper produces the identical table without that validation - verified
  #identical() on 946 node/edge tables from 368 real save files. See
  #R/graph_helpers.R; upgrading tibble/vctrs on the server is still the real fix.
  if(is.null(edgeTable))   edgeTable   <- vftGraphTibble(network, "edges")
  if(is.null(vertexTable)) vertexTable <- vftGraphTibble(network, "nodes")

  #node coordinates are constant for the whole simulation; compute the coordinate matrix
  #once here instead of recomputing sf::st_coordinates() on every timestep in the loop below.
  vertexCoords <- sf::st_coordinates(vertexTable$geometry)

  #TODO: implement more detailed behaviour based on surveys, questionnaires, cluster analysis etc.
  #FOR NOW:agents walk towards closest AOI vertex using shortest route. Within AOI they maintain "nicest route" tactic.

  #determine agents that are active (become inactive when they returned home)
  activeAgents <- dayPop$id

  # constants
  MAX_DURATION <- 10*60 #minutes
  MAX_TIMESTEPS <-  convertToTimesteps(MAX_DURATION)

  #intialise simulation
  timestep <- 0
  time <- 0
  timer <- 0

  initialTime <- Sys.time()

  #initialise all agents to start at the same time
  #TODO : allow possiblity to start at different times
  dayPop$startTime <- 0

  #START DEBUG PLOTTING
  #edges on which agent locations will be plotted
  AOIvetrices <- igraph::V(network)$geometry[igraph::V(network)$AOI %in% dayPop$AOIGoal[1]]
  bbox <- sf::st_bbox(AOIvetrices)

  # spatVect <- terra::vect(sf::st_as_sf(dplyr::as_tibble(network %>% tidygraph::activate(edges))))

  #DETERMINE SHORTEST PATHS TO AOI####

  ##Create Progress Bar (Initialzation)####
  if(!debug){
    # ABMprogress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    # on.exit(ABMprogress$close())
    progress$set(message = "ABM initialisieren", detail = "Bestimmung von Zielgebieten", value = 0)
    #determine total timesteps of ABM (maximum timeExt of all agents)
    total <- nrow(dayPop)
  }

#ignore agents with goals at -1 (these are people too far to any recreation point)
cannotRecreate <- dayPop$goalV == -1

  shortestPaths <- findShortestRoute_cpp(V_ptr = listOfPointers[[1]],
                                         adjList_IDs_ptr = listOfPointers[[2]],
                                         adjList_dist_ptr_walkNat = listOfPointers[[3]][[1]],
                                         adjList_dist_ptr_walkNat_attr = listOfPointers[[4]][[1]],
                                         adjList_dist_ptr_walkNat_ATTR = listOfPointers[[5]][[1]],
                                         adjList_dist_ptr_walkSoc = listOfPointers[[3]][[2]],
                                         adjList_dist_ptr_walkSoc_attr = listOfPointers[[4]][[2]],
                                         adjList_dist_ptr_walkSoc_ATTR = listOfPointers[[5]][[2]],
                                         adjList_dist_ptr_dogNat = listOfPointers[[3]][[3]],
                                         adjList_dist_ptr_dogNat_attr = listOfPointers[[4]][[3]],
                                         adjList_dist_ptr_dogNat_ATTR = listOfPointers[[5]][[3]],
                                         adjList_dist_ptr_dogProx = listOfPointers[[3]][[4]],
                                         adjList_dist_ptr_dogProx_attr = listOfPointers[[4]][[4]],
                                         adjList_dist_ptr_dogProx_ATTR = listOfPointers[[5]][[4]],
                                         adjList_dist_ptr_ebikeNat = listOfPointers[[3]][[5]],
                                         adjList_dist_ptr_ebikeNat_attr = listOfPointers[[4]][[5]],
                                         adjList_dist_ptr_ebikeNat_ATTR = listOfPointers[[5]][[5]],
                                         adjList_dist_ptr_bikeSport = listOfPointers[[3]][[6]],
                                         adjList_dist_ptr_bikeSport_attr = listOfPointers[[4]][[6]],
                                         adjList_dist_ptr_bikeSport_ATTR = listOfPointers[[5]][[6]],
                                         adjList_dist_ptr_jogger = listOfPointers[[3]][[7]],
                                         adjList_dist_ptr_jogger_attr = listOfPointers[[4]][[7]],
                                         adjList_dist_ptr_jogger_ATTR = listOfPointers[[5]][[7]],
                                         weighingMethod = "distance",
                                         src_v = dayPop$startV[!cannotRecreate],
                                         goal_v = dayPop$goalV[!cannotRecreate],
                                         agentTyps = dayPop$agentTyp[!cannotRecreate]) #allDistTbl_ptr = listOfPointers[[4]],

  progress$set(1/2, message = "ABM initialisieren")
  #pass every path back to agents
  dayPop$pathToGoal[!cannotRecreate] <- shortestPaths$path
  #pass distances back to agents
  dayPop$totalDistance[!cannotRecreate] <- shortestPaths$distance
  #get currend edge and record history of edges
  edgesResultsStart <- getEdges_cpp(dayPop$startV[!cannotRecreate],
                                    dayPop$goalV[!cannotRecreate],
                                    edgeTable[, c("from", "to", "edgeID")],
                                    usingPathToGoal = TRUE,
                                    pathToGoal = dayPop$pathToGoal[!cannotRecreate],
                                    oldPriorEs = dayPop$priorE[!cannotRecreate])

  dayPop$currentE[!cannotRecreate] <- edgesResultsStart$currentE[[1]]
  dayPop$priorE[!cannotRecreate] <- edgesResultsStart$priorE[[1]]

  #mark all agents with -1 goal as inactive
  dayPop$active[cannotRecreate] <- FALSE
  # for(agentNo in 1:nrow(dayPop)){
  #   # PROGRESS BAR (INTIALIZATION) ####
  #   if(!debug){
  #     if(!exists("debugStartTime") ){
  #       debugStartTime <- Sys.time()
  #     }else{
  #       debugTime <- Sys.time()
  #       timeDiff = difftime( debugTime, debugStartTime, units = "min" )
  #       #calculate time left: ex: at 25% of time (100/25)-1 = 3. The time already spent has to pass 3 more times.
  #       #If it took 1min, then there's 3 more mins to go.
  #       timeLeft = round( ((total/agentNo) - 1) * timeDiff )
          # progress$set(agentNo/total, message = "Initializing ABM", detail = paste0("Approximately ", timeLeft, " minutes left.") )
  #     }
  #   }
  #
  #   agent <- dayPop[agentNo, ]
  #   ##Determine shortest path to AOIGoal
  #   #get all vertices of AOIGoal
  #   # AOIVertices <- V(network)$nodeID[V(network)$AOI == agent$AOIGoal]
  #   # AOIVertices <- AOIVertices[!is.na(AOIVertices)]
  #
  #   #determine shortest path to goalV
  #   # print("start")
  #   # print(agent$startV)
  #   # print("goal")
  #   # print(agent$goalV)
  #
  #   shortestPath <- determineShortestPath(network, agent$startV, agent$goalV)
  #   agent$pathToGoal <- list(as_ids( shortestPath$vpath[[1]] ))
  #   agent$totalDistance <-  shortestPath$totalDistance
  #
  #   if(!length(shortestPath$vpath[[1]]) > 0){browser()}
  #   #replace row with modified agent
  #   dayPop[dayPop$id == agent$id, ] <- agent
  # }
  if(trackable == TRUE){

    #get a random agent to track
    chosenAgentID <- sample(dayPop$id[dayPop$active == TRUE], 1)

    #plot full map
    plot(spatVect, y = 11 , col = c("grey", "green"))
    #or plot close-up
    #plot(spatVect, y = 11 , col = c("grey", "green"), xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]))

    #highlight the AOI area aimed by an agent
    plot(igraph::V(network)$geometry[igraph::V(network)$AOI == dayPop$AOIGoal[dayPop$id == chosenAgentID]], col = "blue", add = TRUE)
    #plot polygon of AOI
    if(!is.null(areasOfInterest)){
      plot(terra::vect(areasOfInterest), add = TRUE, border = "black")
    }
  }

  progress$set(1, message = "ABM initialisieren")

  if(!debug){
    #close previous progress bar
    # progress$close()
    #start a new one
    # progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    # on.exit(ABMprogress$close())
    progress$set(message = "ABM l\u00E4uft (oder f\u00E4hrt Rad...)", value = 0)
    #determine total timesteps of ABM (maximum timeExt of all agents)
    total <- convertToTimesteps( max(dayPop$timeExt), unit = "m" )
  }
  # MAIN LOOP ####
  debugRetracing <- rep(0, max(dayPop$id)+1)
  #run simulation as long as agents are active, or max duration is not reached
  vftDbg(MAX_TIMESTEPS)



  while(sum(dayPop$active) > 0 & timestep <= MAX_TIMESTEPS){#& timestep <= MAX_TIMESTEPS

# if(timestep == 900){browser()}
    # if(time > MAX_DURATION){browser()}
    #keep track of time (both real time and timesteps)
    timestep <- timestep + 1
    time <- time + (1*(1/timeTimestepsRatio) )

    #increment 1: timesteps / ~total timesteps
    if(!debug){
      if(!exists("debugStartTime") ){
        debugStartTime <- Sys.time()
      }else{
        debugTime <- Sys.time()
        timeDiff = difftime( debugTime, debugStartTime, units = "min" )
        #calculate time left: ex: at 25% of time (100/25)-1 = 3. The time already spent has to pass 3 more times.
        #If it took 1min, then there's 3 more mins to go.
        timeLeft = round( ((total/timestep) - 1) * timeDiff )
        progress$set(timestep/(total/1.75), message = "ABM l\u00E4uft (oder f\u00E4hrt Rad...)", detail = paste0("Ungef\u00E4hr ", round(timeLeft/3), " Minuten \u00E4brig.") )
      }
    }

    #VECTORISED:
    #remove as much as possible from loop

    #determine if next location reached (and not starting state where currentV = NA)
    allActive <- dayPop$active == TRUE

    if(length(dayPop$moving) != length(allActive)){browser()}

    agentsReachedNextV <- (dayPop$moving <= timestep) & allActive
    #ignore this part on the first step when all agents have NA currentV
    if(timestep != 1){#sum(is.na(dayPop$currentV)) == 0
      #determine behaviours for agents that reached the next location
      behaviourIsNicest <- dayPop$behaviour == "nicest" & agentsReachedNextV
      behaviourIsShortest <- dayPop$behaviour == "shortest" & agentsReachedNextV
      behaviourIsNicestShortest <-dayPop$behaviour == "nicestShortest" & agentsReachedNextV
      behaviourIsRetracing <- dayPop$behaviour == "retracing" & agentsReachedNextV

      #### MAKE HISTORY ####
      #of vertices
      #only apply if some agents require it
      if(sum(agentsReachedNextV) > 0){
        #general priorV
        #ignore this for cases of retracing
        agentsNotRetracing <- dayPop$behaviour != "retracing"

        if(sum(agentsReachedNextV & agentsNotRetracing) > 0){
          # #append currentV to end of priorVs (x is the index of each agent to alter in the context of agents that reached nextV)
          # dayPop$priorV[agentsReachedNextV & agentsNotRetracing] <-mapply( function(x)
          #   list( c(
          #     dayPop$priorV[agentsReachedNextV & agentsNotRetracing][[x]],
          #     dayPop$currentV[agentsReachedNextV & agentsNotRetracing][[x]])
          #   ),
          #   1:length(dayPop$priorV[agentsReachedNextV & agentsNotRetracing])
          # )
          #CPP FUNCTION:
          #appendCurrentToPrior:
          #takes List of priorV and vector of currentV, appends currentV to priorV and returns a List
          #also takes pathToGoal and behaviour to append all vertices of shortPaths up to currentV
          dayPop$priorV[agentsReachedNextV & agentsNotRetracing] <- appendCurrentToPrior_cpp(priorVs = dayPop$priorV[agentsReachedNextV & agentsNotRetracing],
                                                                                             pathToGoal = dayPop$pathToGoal[agentsReachedNextV & agentsNotRetracing],
                                                                                             currentVs = dayPop$currentV[agentsReachedNextV & agentsNotRetracing],
                                                                                             behaviour = dayPop$behaviour[agentsReachedNextV & agentsNotRetracing])
        }


        #for shortest behaviour
        if(sum(behaviourIsShortest)> 0){
          dayPop$shortestPriorV[behaviourIsShortest] <-mapply( function(x) list( c(dayPop$shortestPriorV[behaviourIsShortest][[x]], dayPop$currentV[behaviourIsShortest][[x]]) ),

                                                               1:length(dayPop$shortestPriorV[behaviourIsShortest])

          )
        }

        #for nicest behaviour
        if(sum(behaviourIsNicest)> 0){
          # dayPop$nicestPriorV[behaviourIsNicest] <-mapply( function(x) list( c(dayPop$nicestPriorV[behaviourIsNicest][[x]], dayPop$currentV[behaviourIsNicest][[x]]) ),
          #
          #                               dayPop$nicestPriorV[behaviourIsNicest])
          #
          # )
          #CPP FUNCTION: updateNicestHistory
          dayPop$nicestPriorV[behaviourIsNicest] <- updateNicestHistory_cpp(nicestPriorVs = dayPop$nicestPriorV[behaviourIsNicest], nicestCurrentVs = dayPop$currentV[behaviourIsNicest] )
        }
        #for nicestShortest behaviour
        if(sum(behaviourIsNicestShortest)> 0){
          dayPop$nicestShortestPriorV[behaviourIsNicestShortest] <-mapply( function(x) list( c(dayPop$nicestShortestPriorV[behaviourIsNicestShortest][[x]], dayPop$currentV[behaviourIsNicestShortest][[x]]) ),

                                                                           1:length(dayPop$nicestShortestPriorV[behaviourIsNicestShortest])

          )
        }

        #Now that history is recorded, revert retracing to nicest
        dayPop$behaviour[agentsReachedNextV & !agentsNotRetracing] <- "nicest"
        #explanation:
        #At every retracing step, an agent evaluates possible paths other than retraced ones (nicest behaviour)
        #However, it needs to be recorded as "retracing", even if it uses "nicest" behaviour.





        #REMOVE ?
        #EASIER TO SAVE CURRENT EDGES INTO PRIOR EDGES AT END OF TIMESTEP?
      #   #history of edges
      #   #utility function
      #   lastPriorVs <- function(prrV){
      #     mapply(function(x)x[length(x)],
      #            prrV )
      #   }
      #
      #
      #   #TODO: make sure retracing and shortest routes are taken into account adequately
      #   #(three functions? all for agentsReachedNextV & .... either behaviour != retracing, nicest or (shortest and nicestShortest)
      #   #CPP FUNCTION: getVisitedEdges_cpp
      #   visitedEdges <- getVisitedEdges_cpp(dayPop$nextV[agentsReachedNextV],
      #                                       lastPriorVs(dayPop$priorV[agentsReachedNextV]),
      #                                       edgeTable[, c("to", "from", "edgeID")])
      #
      #   # visitedEdges_retracing <- getVisitedEdgesRetracing_cpp(dayPop$nextV[agentsReachedNextV],
      #   #                                     lastPriorVs(dayPop$priorV[agentsReachedNextV]),
      #   #                                     edgeTable[, c("to", "from", "edgeID")])
      #
      #   # visitedEdges_shortest <- getVisitedEdgesShortest_cpp(dayPop$nextV[agentsReachedNextV],
      #   #                                     lastPriorVs(dayPop$priorV[agentsReachedNextV]),
      #   #                                     edgeTable[, c("to", "from", "edgeID")])
      #
      #   #determine where there are multiple visited paths (multiple paths between the two vertices: priorV and nextV)
      #   edgesAreMultiple <- mapply(function(x) length(x) > 1,
      #                              visitedEdges
      #   )
      #   #resolve multiple paths between two nodes
      #   #TODO: resolve dependant on behaviour (shortest/nicest)
      #   #FOR NOW_: take random
      #   visitedEdges[edgesAreMultiple] <- mapply(function(x) sample(x, 1),
      #                                            visitedEdges[edgesAreMultiple]
      #   )
      #
      #   #add it to each agents' edge history
      #   dayPop$priorE[agentsReachedNextV] <- mapply(function(vstd, hstry) list(c(hstry, vstd)),
      #                                               visitedEdges,
      #                                               dayPop$priorE[agentsReachedNextV]
      #   )
      }

    }

    #### RESOLVE MOVEMENT ####

    if(NA %in% agentsReachedNextV){browser()}
    dayPop$lastV[agentsReachedNextV] <- dayPop$currentV[agentsReachedNextV]
    dayPop$currentV[agentsReachedNextV] <- dayPop$nextV[agentsReachedNextV]

    if(sum(0 %in% dayPop$nextV[agentsReachedNextV]) > 0){browser()}#catch bug

    dayPop$nextV[agentsReachedNextV] <- NA
    #determine if agents are still active (did they return home?)
    #make inactive if they were returning home and reached home
    dayPop$active[( (timer > dayPop$timeExt) | (dayPop$behaviour %in% c("shortest", "nicestShortest"))) &
                    agentsReachedNextV & dayPop$startV == dayPop$goalV &
                    dayPop$currentV == dayPop$startV] <- FALSE

    #update active agents to reflect change
    agentsReachedNextV <- dayPop$active == TRUE & agentsReachedNextV

    #### DETERMINE BEHAVIOUR CHANGES ####
    #determine current behaviours of active agents
    activeShortest <- dayPop$behaviour == "shortest" & agentsReachedNextV
    activeNicest <- dayPop$behaviour == "nicest" & agentsReachedNextV
    activeNicestShortest <- dayPop$behaviour == "nicestShortest" & agentsReachedNextV
    activeRetracing <- dayPop$behaviour == "retracing" & agentsReachedNextV

    # #contexts of behaviour change:
    # #when reaching AOI, change from "shortest" to "nicest"
    # dayPop$behaviour[activeShortest & dayPop$AOIGoal == V(network)$AOI[dayPop$currentV]] <- "nicest"
    # #remove pathToGoal
    # dayPop$pathToGoal[activeShortest & dayPop$AOIGoal == V(network)$AOI[dayPop$currentV]] <- list(NULL)

    #when reaching goalV instead, to avoid problems with AOIs on nodes
    dayPop$behaviour[activeShortest & dayPop$goalV == dayPop$currentV] <- "nicest"
    #remove pathToGoal
    dayPop$pathToGoal[activeShortest & dayPop$goalV == dayPop$currentV] <- list(NULL)


    #when time runs out (timeExt), start returning home  TODO: Follow "nicestShortest" while in a AOI
    #if not already returning home

    #determine if time ran out (consider time not accounted ie. while reaching aoi)
    isTimeOver <- dayPop$timeExt + dayPop$timeNotAccounted <= timer

    #if(isTimeOver[dayPop$id == 159] == TRUE){browser()}
    dayPop$behaviour[agentsReachedNextV & isTimeOver & (dayPop$goalV != dayPop$startV)] <- "shortest"
    dayPop$goalV[agentsReachedNextV & isTimeOver] <- dayPop$startV[agentsReachedNextV & isTimeOver]
    #remove pathToGoal
    dayPop$pathToGoal[agentsReachedNextV & isTimeOver] <- list(NULL)

    #TODO:
    #when following "nicestShortest", switch to "shortest" if outside a AOI
    #dayPop$behaviour[activeNicestShortest]
    #dayPop$behaviour[activeShortest & dayPop$currentV ... ]

    # DECISION MAKING ####
    #all agents that haven't decided yet
    toDecide <- is.na(dayPop$nextV) & agentsReachedNextV



    # * Nicest routes ####
    # Use current Vs to determine nextVs

    #first revert retracing agents to "nicest" behaviour

    #active agents with a "nicest" behaviour
    toDecideNicest <- toDecide & dayPop$behaviour == "nicest"

    if(sum(toDecideNicest) > 0){
      #subset relevant agents
      #TO DO: avoid subsets, only use full dayPop logical vectors
      # nicestAgents <- dayPop[toDecideNicest, ]

      #return es if using mapply, ids if using cpp
      igraph::igraph_options(return.vs.es = FALSE)
      #determine neighbourhood nodes
      #### NEED SPEED IMPROVEMENT (Pr 2)

      #if vertex not in network, interrupt
      if(sum(dayPop$currentV[toDecideNicest] %in% igraph::V(network)$nodeID ) < length(dayPop$currentV[toDecideNicest])){browser()}

      neighborhoodVs <- igraph::ego(network, 1, dayPop$currentV[toDecideNicest])
      igraph::igraph_options(return.vs.es = TRUE)
      #filter out non-viable nodes (currentVs, priorVs, retraced, outside AOI)

      #vector of agents where  not in current

      # CPP function: filterRouteChoices_cpp
      #for every agent, keeps nodes from potential routes, that are not current, prior or retraced nodes
      routeChoices <- filterRouteChoices_cpp(neighbours = neighborhoodVs,
                                             currentVs = dayPop$currentV[toDecideNicest],
                                             lastVs = dayPop$lastV[toDecideNicest],
                                             retracedVs = dayPop$retracedV[toDecideNicest],
                                             V_outside_aoi = vertexTable$nodeID[vertexTable$AOI == "0"])
      #, priorVs = dayPop$priorV[toDecideNicest],

      #which ones have viable options (at least 1 solution)
      areViableChoices <- mapply(function(x) length(x) > 0,
                                 routeChoices)

      #if there are viable choices
      if(sum(areViableChoices) > 0){

        #CPP function: chooseBestRoutes_cpp
        #choose most attractive routes (and if possible not already visited)
        # print(paste0("areViableChoices: ", areViableChoices))
        routeChoicesProbs <- routeChoices
        routeChoicesProbs[areViableChoices] <- chooseBestRoutes_cpp(viableRoutes = routeChoices[areViableChoices],
                                                                    DULN_df =  vertexTable[,c("DULN_DOG_P","DULN_BIKER","DULN_DOG_N","DULN_EBIKE",
                                                                                              "DULN_JOGGE","DULN_WALK_","DULN_WALK1" , "nodeID")],
                                                                    priorVs = dayPop$priorV[areViableChoices],
                                                                    agentTyps = dayPop$agentTyp[areViableChoices],
                                                                    V_coords_df = vertexCoords,
                                                                    currentVs = dayPop$currentV[areViableChoices])

        #TODO: sample using routeChoicesProbs as probabilities
        #for every viable route
        #sample with uneven probabilities
        # print("(routeChoicesProbs[areViableChoices]::")
        # print(routeChoicesProbs[areViableChoices])
        # print("---")
        # print("(routeChoices[areViableChoices]::")
        # print(routeChoices[areViableChoices])
        # print("********")
        # print("********")

        for(routeNo in 1:length(routeChoicesProbs[areViableChoices]) ){
          if(length(routeChoices[areViableChoices][[routeNo]]) == 1){
            #select first element
            routeChoices[areViableChoices][[routeNo]] <- routeChoices[areViableChoices][[routeNo]][1]
            #this is needed as it seems possible there are multiple elements with a single probability of 1
          }else{
            if(length(routeChoicesProbs[areViableChoices][[routeNo]]) == 1){
              #might be multiple routeChoices, but only one probability of 1
              routeChoices[areViableChoices][[routeNo]] <- routeChoices[areViableChoices][[routeNo]][1]

            }else{
              #make a choice based on probabilities
              # print("routeChoices[areViableChoices][[routeNo]]: ")
              # print(routeChoices[areViableChoices][[routeNo]])
              # cat(file = stderr(), (paste0("length: ", length(routeChoices[areViableChoices][[routeNo]])) ))
              # cat(file = stderr(), (paste0("lengthProbs: ", length(routeChoicesProbs[areViableChoices][[routeNo]])) ))

              if(sum(is.na(routeChoices[areViableChoices][[routeNo]])) > 0){browser()}
              if(sum(is.na(routeChoicesProbs[areViableChoices][[routeNo]])) > 0){browser()}



              routeChoices[areViableChoices][[routeNo]] <- sample(routeChoices[areViableChoices][[routeNo]], 1, replace = FALSE, prob = routeChoicesProbs[areViableChoices][[routeNo]])

              #PROXIMITY ONLY
              # routeProbs <- routeChoicesProbs[areViableChoices][[routeNo]]
              # routeProbs[routeProbs > 0.00001] <- 1
              # routeChoices[areViableChoices][[routeNo]] <- sample(routeChoices[areViableChoices][[routeNo]], 1, replace = FALSE, prob = routeProbs)
              # ##

              # print("SAMPLE DONE")
            }
          }
        }



        #resolve nextV for retracing agents
        # startRetracing <- function(notViableAgentID){
        #   notViableAgent <- dayPop[dayPop$id == notViableAgentID,]
        #   #go back to last visited vertex (RETRACING)
        #   bestChoice <- notViableAgent$priorV[[1]][[length(notViableAgent$priorV[[1]])]]
        #   #remove last priorV (allows to keep track of retracing steps)
        #   notViableAgent$priorV <- list(notViableAgent$priorV[[1]][1:length(notViableAgent$priorV[[1]])-1])
        #
        #   bestChoice
        # }
        # if(sum(toDecideNicest) != length(!areViableChoices)){browser()}
        #
        # #TODO: move to next retracing area, here we should just change behaviour.
        # routeChoices[!areViableChoices] <- mapply( startRetracing,
        #                                           nicestAgents$id[!areViableChoices])





        #resolve decision
        #TO DO NEXT:
        #replace agents in dayPop by using their ids
        #check that there is 1 routeChoice per routeChoice
        routeChoices_v <- unlist(routeChoices[areViableChoices])

        if(length(routeChoices[areViableChoices]) != length(routeChoices_v)){browser()}

        dayPop$nextV[dayPop$id %in% dayPop$id[toDecideNicest][areViableChoices]] <- routeChoices_v

        if(sum(0 %in% dayPop$nextV[dayPop$id %in% dayPop$id[toDecideNicest][areViableChoices]]) > 0){browser()}#catch bug

      }
      # #CPP function: getEdges
      # #gets edges based on nextVs and currentVs, using edgeTable
      # finalEdges <- getEdges_cpp(dayPop$currentV[dayPop$id %in% dayPop$id[toDecideNicest][areViableChoices]],
      #                            dayPop$nextV[dayPop$id %in% dayPop$id[toDecideNicest][areViableChoices]],
      #                            edgeTable[, c("from", "to", "edgeID")])
      #
      # dayPop$currentE[dayPop$id %in% dayPop$id[toDecideNicest][areViableChoices]] <- finalEdges

      if(sum(!areViableChoices) > 0){
        #start retracing behaviour
        dayPop$behaviour[toDecideNicest][!areViableChoices] <- "retracing"

        #remove toDecideNicest
        toDecideNicest[toDecideNicest][!areViableChoices] <- FALSE

      }

    }

    # * retracing ####
    #active agents with "retracing" behaviour
    toDecideRetracing <- toDecide & dayPop$behaviour == "retracing"

    if(sum(toDecideRetracing) > 0){

      #to insert somehow?
      # #get results from retracing function (contains $route and $priorV results)
      # retracingResults <- startRetracing_cpp(agentID = dayPop$id[toDecideNicest & !areViableChoices],
      # )
      # #get route choice results
      # routeChoices[toDecideNicest & !areViableChoices] <- retracingResults$routes
      # #update priorV info in dayPop
      # dayPop$priorV <- retracingResults$priorV
      # #add currentV to retracedV list
      # dayPop$retracedV[toDecideNicest & !areViableChoices] <- c(dayPop$retracedV[toDecideNicest & !areViableChoices][[1]], dayPop$currentV[toDecideNicest & !areViableChoices])
      #
      #
      #             retraceSteps <- function(agentID){
      #               agent <- dayPop[dayPop$id == agentID, ]
      #
      #               #check if there are other unexplored possibilities (only remove current and retraced vertices)
      #               routeChoices <- neighborhood(network, 1, agent$currentV)[[1]]
      #
      #               #not current position
      #               routeChoices <- routeChoices[routeChoices$nodeID != agent$currentV]
      #               #not retraced
      #               routeChoices <- routeChoices[!routeChoices$nodeID %in% agent$retracedV[[1]]]
      #               #not visited prior
      #               routeChoices <- routeChoices[!routeChoices$nodeID %in% agent$priorV[[1]]]
      #
      #               if(length(routeChoices) > 0){
      #                 #if there are choices
      #                 #choose most attractive route,
      #                 bestChoice <- routeChoices$nodeID[routeChoices$DULN == max(routeChoices$DULN)]
      #                 #if tied randomise choice
      #                 if(length(bestChoice) > 1){bestChoice <- sample(bestChoice, 1)}
      #                 #return to "nicest" behaviour
      #                 agent$behaviour <- "nicest"
      #
      #
      #               }else{
      #                 debugRetracing[agentID] <<- debugRetracing[agentID] + 1
      #
      #                 if(debugRetracing[agentID] > 100){browser()}
      #                 #if there still no choices
      #                 #back up further
      #                 if(is.null(agent$priorV[[1]])){browser()}
      #                 #make the earlier prior the nextV,
      #                 bestChoice <- agent$priorV[[1]][[length(agent$priorV[[1]])]]
      #
      #                 #remove the last priorV (put NULL if no priorVs left)
      #                 if( ( length(agent$priorV[[1]])-1 ) < 1){
      #                   agent$priorV <- list(NULL)
      #                 }else{
      #                   agent$priorV <- list(agent$priorV[[1]][1:(length(agent$priorV[[1]])-1)])
      #                 }
      #                 #and place currentV in retracedV
      #                 agent$retracedV <- list(c(agent$retracedV[[1]], agent$currentV))
      #
      #
      #               }
      #
      #               return(agent)
      #
      #             }
      # browser()
      #             #apply retracing behaviour to all retracing agents
      #             dayPop[toDecideRetracing, ] <- sapply(dayPop$id[toDecideRetracing], retraceSteps)

      # #not worth resources to recheck this
      # neighborhoodVs <- ego(network, 1, dayPop$currentV[toDecideRetracing])
      #
      # #filter out current, prior, retraced and outside AOI
      # if(2108 %in% dayPop$currentV[toDecideRetracing]){browser()}
      # remainingRoutes <- filterRouteChoices_cpp(neighbours =  neighborhoodVs,
      #                                           currentVs =  dayPop$currentV[toDecideRetracing],
      #                                           priorVs = dayPop$priorV[toDecideRetracing],
      #                                           retracedVs =  dayPop$retracedV[toDecideRetracing],
      #                                           V_outside_aoi = vertexTable$nodeID[vertexTable$AOI == "0"])

      #Double check there are no remaining possiblities (routes)
      # if(sum(unlist(remainingRoutes)) == 0 ){

      #CPP FUNCTION: retraceSteps

      if(NA %in% dayPop$currentV[toDecideRetracing]){browser()}
      if(NA %in% dayPop$priorV[toDecideRetracing]){browser()}
      if(NA %in% dayPop$retracedV[toDecideRetracing]){browser()}
      # if(NA %in% dayPop$nextV[toDecideRetracing]){browser()}
      # print(dayPop$currentV[toDecideRetracing])
      # print(dayPop$priorV[toDecideRetracing])
      # print(dayPop$retracedV[toDecideRetracing])


      #check if priorVs contains NULL
      for(x in dayPop$priorV[toDecideRetracing]){if(is.null(x)){browser()}}


      retracingResults <- retraceSteps_cpp(  currentVs = dayPop$currentV[toDecideRetracing],
                                             priorVs = dayPop$priorV[toDecideRetracing],
                                             retracedVs = dayPop$retracedV[toDecideRetracing] ,
                                             nextVs =  dayPop$nextV[toDecideRetracing]
      )
      if(length(retracingResults$nextV) !=  length(dayPop$currentV[toDecideRetracing]) ){ browser()}
      #CATCH EXCEPTION
      #agents can get themselves into a deadend by having retraced all other options
      #being obliged to retrace back into current V
      sameCV_NV <- dayPop$currentV[toDecideRetracing] == retracingResults$nextV

      if(sum(sameCV_NV) > 0){
        #if this happens, change behaviour to returning home (AoI has been thoroughly explored)

        #this creates too many problems, abort for now
        # dayPop$active[sameCV_NV] <- FALSE
        #remove agents with problem from retracing


        #update retracingResults (removing those results with problems)
        retracingResults$priorV[sameCV_NV] <- NULL
        retracingResults$retracedV[sameCV_NV] <- NULL
        retracingResults$nextV <- retracingResults$nextV[!sameCV_NV]

        #
        # dayPop$goalV[sameCV_NV] <- dayPop$startV[sameCV_NV]
        # #remove pathToGoal
        # dayPop$pathToGoal[sameCV_NV] <- list(NULL)
        #
        # toDecideRetracing[sameCV_NV] <- FALSE #remove it from raytracing decision making

        #make the agent go home by fastest route
        dayPop$behaviour[toDecideRetracing][sameCV_NV] <- "nicestShortest"
        dayPop$goalV[toDecideRetracing][sameCV_NV] <- dayPop$startV[toDecideRetracing][sameCV_NV]

        toDecideRetracing[toDecideRetracing][sameCV_NV] <- FALSE

        # dayPop$pathToGoal[is.na(dayPop$nextV[agentsReachedNextV])] <- list()


        #update toDecide

        #shift timestep to reach next V one timestep back
        # dayPop$moving[agentsReachedNextV][is.na(dayPop$nextV[agentsReachedNextV])] <- dayPop$moving[agentsReachedNextV][is.na(dayPop$nextV[agentsReachedNextV])] + 1
        #update agentsReachedNextV for agents with problem
        # agentsReachedNextV[is.na(dayPop$nextV[agentsReachedNextV])] <- FALSE
        #update toDecideRetracing
        #already updated beforehand
        # toDecideRetracing[is.na(dayPop$nextV[toDecideRetracing])] <- FALSE

        #this should allow agents with a problem to start heading home next timestep


      }

      if(is.na(sum(retracingResults$currentVs))){browser}
      #place results into relevant parts of dayPop

      #currentV and nextV cannot be the same
      # dayPop$currentV[toDecideRetracing] <- NA
      dayPop$priorV[toDecideRetracing] <- retracingResults$priorV
      dayPop$retracedV[toDecideRetracing] <- retracingResults$retracedV

      # behaviour remains "retracing" so it can be properly recorded in history
      # dayPop$behaviour[toDecideRetracing] <- "nicest"

      dayPop$nextV[toDecideRetracing] <- retracingResults$nextV

      if(sum(0 %in% dayPop$nextV[toDecideRetracing]) > 0){browser()}#catch bug

      # }else{
      #   browser()
      #   print("ERROR: route options remain when retracing")
      # }

    }

    # * Shortest routes ####
    #active agents with "shortest" behaviour
    toDecideShortest <- toDecide & dayPop$behaviour == "shortest"



    #agents with a shortest route
    tmp <- sapply(dayPop$pathToGoal[toDecideShortest], is.null)
    haveShortestRoutes <- !tmp
    rm(tmp)


    #when agents of "shortest" behaviour have no shortest route,
    if(sum(!haveShortestRoutes) > 0){

      #determine a shortest route
      shortestPaths <- findShortestRoute_cpp(V_ptr = listOfPointers[[1]],
                                             adjList_IDs_ptr = listOfPointers[[2]],
                                             adjList_dist_ptr_walkNat = listOfPointers[[3]][[1]],
                                             adjList_dist_ptr_walkNat_attr = listOfPointers[[4]][[1]],
                                             adjList_dist_ptr_walkNat_ATTR = listOfPointers[[5]][[1]],
                                             adjList_dist_ptr_walkSoc = listOfPointers[[3]][[2]],
                                             adjList_dist_ptr_walkSoc_attr = listOfPointers[[4]][[2]],
                                             adjList_dist_ptr_walkSoc_ATTR = listOfPointers[[5]][[2]],
                                             adjList_dist_ptr_dogNat = listOfPointers[[3]][[3]],
                                             adjList_dist_ptr_dogNat_attr = listOfPointers[[4]][[3]],
                                             adjList_dist_ptr_dogNat_ATTR = listOfPointers[[5]][[3]],
                                             adjList_dist_ptr_dogProx = listOfPointers[[3]][[4]],
                                             adjList_dist_ptr_dogProx_attr = listOfPointers[[4]][[4]],
                                             adjList_dist_ptr_dogProx_ATTR = listOfPointers[[5]][[4]],
                                             adjList_dist_ptr_ebikeNat = listOfPointers[[3]][[5]],
                                             adjList_dist_ptr_ebikeNat_attr = listOfPointers[[4]][[5]],
                                             adjList_dist_ptr_ebikeNat_ATTR = listOfPointers[[5]][[5]],
                                             adjList_dist_ptr_bikeSport = listOfPointers[[3]][[6]],
                                             adjList_dist_ptr_bikeSport_attr = listOfPointers[[4]][[6]],
                                             adjList_dist_ptr_bikeSport_ATTR = listOfPointers[[5]][[6]],
                                             adjList_dist_ptr_jogger = listOfPointers[[3]][[7]],
                                             adjList_dist_ptr_jogger_attr = listOfPointers[[4]][[7]],
                                             adjList_dist_ptr_jogger_ATTR = listOfPointers[[5]][[7]],
                                             weighingMethod = "distance",
                                             src_v = dayPop$currentV[toDecideShortest][!haveShortestRoutes],
                                             goal_v = dayPop$goalV[toDecideShortest][!haveShortestRoutes],
                                             agentTyps = dayPop$agentTyp[toDecideShortest][!haveShortestRoutes]) #allDistTbl_ptr = listOfPointers[[4]],

      #pass every path back to agents
      dayPop$pathToGoal[toDecideShortest][!haveShortestRoutes] <- shortestPaths$path
      #pass distances back to agents
      dayPop$totalDistance[toDecideShortest][!haveShortestRoutes] <- shortestPaths$distance


      # shortestPathInfo <- mapply(function(crrnt, gl) determineShortestPath(network, crrnt, gl),
      #                            dayPop$currentV[toDecideShortest][!haveShortestRoutes],
      #                            dayPop$goalV[toDecideShortest][!haveShortestRoutes])
      #
      # dayPop$pathToGoal[toDecideShortest][!haveShortestRoutes] <- mapply(function(x) list(as_ids(x$vpath[[1]])),
      #                                                                    shortestPathInfo)
      #
      # dayPop$totalDistance[toDecideShortest][!haveShortestRoutes] <- mapply(function(x) x$totalDistance,
      #                                                                       shortestPathInfo)

      #catch error
      if(is.null(dayPop$pathToGoal[toDecideShortest][!haveShortestRoutes][[1]] ) ){browser()}
    }

    #when shortest paths exist, use it to move to next node
    #assume all agents with no shortest routes now have one
    if(sum(toDecideShortest) > 0){

      #faster version(don't get next node, but last node of shortest route)
      #calculate length of entire shortest route -> agent$moving

      # #extract first nodeID from pathToGoal
      # dayPop$nextV[toDecideShortest] <- mapply(function(ptg) ptg$nodeID[[1]],
      #                                                             dayPop$pathToGoal[toDecideShortest]
      #                                                      )
      # #remove first node of paths with 2 nodes at least
      # has2NodesOrMore <- mapply(function(x) length(x) >= 1,
      #                           dayPop$pathToGoal
      #                           )
      # dayPop$pathToGoal[ toDecideShortest & has2NodesOrMore] <- mapply(function(ptg) list(ptg[-1]),
      #                             dayPop$pathToGoal[toDecideShortest & has2NodesOrMore]
      #                             )
      # #put an empty list in paths with 1 node left (TODO: double check this, seems this should be done with 1 node left, not 2...)
      # dayPop$pathToGoal[toDecideShortest & !has2NodesOrMore] <- list(NULL)

      # #extract last node from pathToGoal and make it next V
      dayPop$nextV[toDecideShortest] <- mapply(function(ptg) ptg[length(ptg)],
                                               dayPop$pathToGoal[toDecideShortest])

      if(sum(0 %in% dayPop$nextV[toDecideShortest]) > 0){browser()}#catch bug

      # #extract last nodeID from pathToGoal and make it next V
      dayPop$goalV[toDecideShortest] <- dayPop$nextV[toDecideShortest]


      #translate total distance to timesteps and put in agent$moving (totalDistance[m] / speed [km/h] * (60/1000)[m/min] * 3)
      addedTime <- ceiling((dayPop$totalDistance[toDecideShortest]) /(dayPop$speed[toDecideShortest]*1000/60))
      dayPop$moving[toDecideShortest] <- timestep + convertToTimesteps(time = addedTime, unit = "m")#+ ceiling( ((dayPop$totalDistance[toDecideShortest]) / (dayPop$speed[toDecideShortest])/60 ) * 3 )

      #determine time not accounted for (the moving time during shortest)
      dayPop$timeNotAccounted[toDecideShortest] <- dayPop$timeNotAccounted[toDecideShortest] + dayPop$moving[toDecideShortest]

    }

    # * nicestShortest Route ####
    #active agents with "nicestShortest" behaviour
    toDecideNicestShortest <- toDecide & dayPop$behaviour == "nicestShortest"

    if(sum(toDecideNicestShortest) > 0){
      #agents with a shortest route
      tmp <- sapply(dayPop$pathToGoal[toDecideNicestShortest], is.null)
      haveShortestRoutes <- !tmp
      rm(tmp)


      #when agents of "shortest" behaviour have no shortest route,
      if(sum(!haveShortestRoutes) > 0){
        # * * * INCREASE SPEED (Pr 2)

        #determine a shortest route
        shortestPaths <- findShortestRoute_cpp(V_ptr = listOfPointers[[1]],
                                               adjList_IDs_ptr = listOfPointers[[2]],
                                               adjList_dist_ptr_walkNat = listOfPointers[[3]][[1]],
                                               adjList_dist_ptr_walkNat_attr = listOfPointers[[4]][[1]],
                                               adjList_dist_ptr_walkNat_ATTR = listOfPointers[[5]][[1]],
                                               adjList_dist_ptr_walkSoc = listOfPointers[[3]][[2]],
                                               adjList_dist_ptr_walkSoc_attr = listOfPointers[[4]][[2]],
                                               adjList_dist_ptr_walkSoc_ATTR = listOfPointers[[5]][[2]],
                                               adjList_dist_ptr_dogNat = listOfPointers[[3]][[3]],
                                               adjList_dist_ptr_dogNat_attr = listOfPointers[[4]][[3]],
                                               adjList_dist_ptr_dogNat_ATTR = listOfPointers[[5]][[3]],
                                               adjList_dist_ptr_dogProx = listOfPointers[[3]][[4]],
                                               adjList_dist_ptr_dogProx_attr = listOfPointers[[4]][[4]],
                                               adjList_dist_ptr_dogProx_ATTR = listOfPointers[[5]][[4]],
                                               adjList_dist_ptr_ebikeNat = listOfPointers[[3]][[5]],
                                               adjList_dist_ptr_ebikeNat_attr = listOfPointers[[4]][[5]],
                                               adjList_dist_ptr_ebikeNat_ATTR = listOfPointers[[5]][[5]],
                                               adjList_dist_ptr_bikeSport = listOfPointers[[3]][[6]],
                                               adjList_dist_ptr_bikeSport_attr = listOfPointers[[4]][[6]],
                                               adjList_dist_ptr_bikeSport_ATTR = listOfPointers[[5]][[6]],
                                               adjList_dist_ptr_jogger = listOfPointers[[3]][[7]],
                                               adjList_dist_ptr_jogger_attr = listOfPointers[[4]][[7]],
                                               adjList_dist_ptr_jogger_ATTR = listOfPointers[[5]][[7]],
                                               weighingMethod = "little_attr",
                                               src_v = dayPop$currentV[toDecideNicestShortest][!haveShortestRoutes],
                                               goal_v =dayPop$goalV[toDecideNicestShortest][!haveShortestRoutes],
                                               agentTyps = dayPop$agentTyp[toDecideNicestShortest][!haveShortestRoutes]) #allDistTbl_ptr = listOfPointers[[4]],

        #pass every path back to agents
        dayPop$pathToGoal[toDecideNicestShortest][!haveShortestRoutes] <- shortestPaths$path

        if(is.null(dayPop$pathToGoal[toDecideNicestShortest][!haveShortestRoutes][[1]])){browser()}
        #pass distances back to agents
        dayPop$totalDistance[toDecideNicestShortest][!haveShortestRoutes] <- shortestPaths$distance

        #
        # #determine a shortest route
        # dayPop$pathToGoal[toDecideNicestShortest][!haveShortestRoutes] <- mapply(function(crrnt, gl) list(as_ids(determineShortestPath(network, crrnt, gl)$vpath[[1]])),
        #                                                                          dayPop$currentV[toDecideNicestShortest][!haveShortestRoutes],
        #                                                                          dayPop$goalV[toDecideNicestShortest][!haveShortestRoutes])
        #catch error
        if(is.null(dayPop$pathToGoal[toDecideNicestShortest][!haveShortestRoutes][[1]] ) ){browser()}
      }

      #insert shortest routes in dayPop

      #when shortest paths exist, use it to move to last node of path
      #assume all agents with no shortest routes now have one
      if(sum(toDecideNicestShortest) > 0){
        # #extract first nodeID from pathToGoal
        # dayPop$nextV[toDecideNicestShortest] <- mapply(function(ptg) ptg$nodeID[[1]],
        #                                          dayPop$pathToGoal[toDecideNicestShortest]
        # )
        # #remove first node of paths with 2 nodes at least
        # has2NodesOrMore <- mapply(function(x) length(x) >= 1,
        #                           dayPop$pathToGoal
        # )
        # dayPop$pathToGoal[ toDecideNicestShortest & has2NodesOrMore] <- mapply(function(ptg) list(ptg[-1]),
        #                                                                  dayPop$pathToGoal[toDecideNicestShortest & has2NodesOrMore]
        # )
        # #put an empty list in paths with 1 node left (TODO: double check this, seems this should be done with 1 node left, not 2...)
        # dayPop$pathToGoal[toDecideNicestShortest & !has2NodesOrMore] <- list(NULL)

        # #extract last nodeID from pathToGoal and make it next V
        dayPop$nextV[toDecideNicestShortest] <- mapply(function(ptg) ptg[length(ptg)],
                                                       dayPop$pathToGoal[toDecideNicestShortest])

        if(sum(0 %in% dayPop$nextV[toDecideNicestShortest]) > 0){browser()}#catch bug


        #translate total distance to timesteps and put in agent$moving (totalDistance[m] / speed [km/h] * (60/1000)[m/min] * 3)
        addedTime <- ceiling((dayPop$totalDistance[toDecideNicestShortest]) /(dayPop$speed[toDecideNicestShortest]*1000/60))
        dayPop$moving[toDecideNicestShortest] <- timestep + convertToTimesteps(time = addedTime, unit = "m")#+ ceiling( ((dayPop$totalDistance[toDecideNicestShortest]) / (dayPop$speed[toDecideNicestShortest])/60 ) * 3 )

        # #make nextV the goalV too
        dayPop$goalV[toDecideNicestShortest] <- dayPop$nextV[toDecideNicestShortest]

        #TODO:
        #add passage to all relevant edges and vertices

      }
    }




    ## GET CURRENT EDGES ####
    ### For Nicest and Retracing ####
    #CPP function: getEdges
    #for nicest and retracing : gets edges based on nextVs and currentVs, using edgeTable
    #need to precise if usingPathToGoal, and if not, use list(NULL) for pathToGoal. This is due to complications in using Rcpp and C++
    #### NEED SPEED IMPROVEMENT (Pr 1) ####
    if(sum(toDecideRetracing | toDecideNicest) > 0){
      edgesResultsNice <- getEdges_cpp(dayPop$currentV[toDecideRetracing | toDecideNicest],
                                                                          dayPop$nextV[toDecideRetracing | toDecideNicest],
                                                                          edgeTable[, c("from", "to", "edgeID")],
                                                                          usingPathToGoal = FALSE,
                                                                          pathToGoal = list(NULL),
                                                                          oldPriorEs = list(NULL))

      dayPop$currentE[toDecideRetracing | toDecideNicest] <- edgesResultsNice$currentE[[1]]
      dayPop$priorE[toDecideRetracing | toDecideNicest] <- edgesResultsNice$priorE[[1]]

    }

    ### For Shortest and NicestShortest ####
    #CPP function: getEdges
    #for shortest routes: gets last edge based on nextV and last node of pathToGoal
    #need to precise if usingPathToGoal, and if not, use list(NULL) for pathToGoal. This is due to complications in using Rcpp and C++
    if( sum(toDecideNicestShortest | toDecideShortest) > 0){
      edgesResultsShort <- getEdges_cpp(dayPop$currentV[toDecideNicestShortest | toDecideShortest],
                                                                                 dayPop$nextV[toDecideNicestShortest | toDecideShortest],
                                                                                 edgeTable[, c("from", "to", "edgeID")],
                                                                                 usingPathToGoal = TRUE,
                                                                                 pathToGoal = dayPop$pathToGoal[toDecideNicestShortest | toDecideShortest],
                                                                                 oldPriorEs = dayPop$priorE[toDecideNicestShortest | toDecideShortest])

      dayPop$currentE[toDecideNicestShortest | toDecideShortest] <- edgesResultsShort$currentE[[1]]
      dayPop$priorE[toDecideNicestShortest | toDecideShortest] <- edgesResultsShort$priorE[[1]]

    }


    if(sum(is.na(dayPop$nextV[agentsReachedNextV])) > 0){

      browser()
      #something went wrong

      }



    # if(trackable == TRUE){
    #   #cycle through every active agent
    #
    #   # for(agentID in dayPop$id[dayPop$active == TRUE]){
    #   #   #determine dataframe row as "agent" entity
    #   #   ##AGENT REACHED NEXT LOCATION (noted timestep is reached)
    #   #   if(agentID == chosenAgentID & dayPop$moving[dayPop$id == chosenAgentID] == timestep){
    #   #
    #   #     agent <- dayPop[dayPop$id == agentID, ]
    #   #
    #   #
    #   #     #### DEBUG, AGENT PLOTTING ####
    #   #     #TODO:
    #   #     #Plot each agent as a different color on grey paths
    #   #     #Do this every time an agent reaches destination
    #   #     #Update agent locations for plotting on network
    #   #     # if(!is.na(agent$priorV)){V(network)$agentLoc[[agent$priorV[[1]][[1]]]] <- 0}
    #   #     # V(network)$agentLoc[[agent$id]] <- 1
    #   #
    #   #     #FOLLOW A SPECIFIC AGENT (first one on the list)
    #   #     if(trackable == TRUE){
    #   #       if( agent$behaviour == "nicest" | agent$behaviour == "nicestShortest" | agent$behaviour == "retracing" | timestep%%10 == 0 | timestep == 1   ){ #
    #   #         #plot
    #   #         if(!is.na(agent$priorV) & agent$behaviour == "shortest"  ){ # & (agent$behaviour == "shortest" | agent$behaviour == "nicest")
    #   #
    #   #           #old locations for "shortest" behaviour
    #   #           terra::plot(igraph::V(network)$geometry[igraph::V(network)$nodeID %in% agent$priorV[[1]]], col = "grey", cex = 1, pch = 16, add = TRUE)
    #   #
    #   #         }
    #   #         else if(!is.na(agent$priorV) & agent$behaviour == "nicest" | agent$behaviour == "nicestShortest"  ){ # & (agent$behaviour == "shortest" | agent$behaviour == "nicest")
    #   #
    #   #           #old locations for other behaviours
    #   #           terra::plot(igraph::V(network)$geometry[igraph::V(network)$nodeID == agent$priorV[[1]][[length(agent$priorV[[1]])]]], col = "grey", cex = 2, pch = 16, add = TRUE)
    #   #
    #   #         }
    #   #         else if(!is.na(agent$retracedV) & agent$behaviour == "retracing"){
    #   #
    #   #           #retracing
    #   #           terra::plot(igraph::V(network)$geometry[igraph::V(network)$nodeID == agent$retracedV[[1]][[length(agent$retracedV[[1]])]]], col = "black", cex = 2, pch = 16, add = TRUE)
    #   #
    #   #         }
    #   #         #agent position
    #   #         agentColor <- switch(agent$behaviour,
    #   #                              nicest = "dark green",
    #   #                              shortest = "red",
    #   #                              nicestShortest = "dark grey",
    #   #                              retracing = "black")
    #   #
    #   #         terra::plot(igraph::V(network)$geometry[igraph::V(network)$nodeID == agent$currentV], col = agentColor, cex = 2, pch = 16, add = TRUE)
    #   #
    #   #
    #   #         # print(paste0("BEHAVIOUR: ", agent$behaviour) )
    #   #         # print(paste0("agent timer: ", agent$timer, " minutes"))
    #   #         # print(paste0("time extent before going home: ", agent$timeExt/2, " minutes"))
    #   #         # print(paste0("currentV: ", dayPop$currentV[1]))
    #   #         # print(paste0("nextV: ", dayPop$nextV[1]))
    #   #         # print("priorV: ")
    #   #         # print(dayPop$priorV[1])
    #   #         # print("---------------------")
    #   #
    #   #         #Wait sufficient time to be able to follow plotting between steps
    #   #         timeElapsed <- Sys.time()
    #   #         timeDiff <- as.numeric(timeElapsed - initialTime)
    #   #         if(timeDiff < 0.15){
    #   #           # wait enough time to reach 0.3 pause
    #   #           while(as.numeric(as.numeric(timeElapsed - initialTime )) < 0.15){
    #   #             timeElapsed <- Sys.time()
    #   #           }
    #   #           # Sys.sleep(0.2-timeDiff)
    #   #         }
    #   #       }
    #   #       initialTime <- Sys.time()
    #   #
    #   #     }
    #   #     # terra::plot(vect(st_as_sf(as_tibble(network %>% activate(nodes)))), "agentLoc",col = c("grey", "red"), alpha = 0.5, add = TRUE )
    #   #
    #   #
    #   #
    #   #     #update agent's timer with minutes passed (timesteps since last calculation)
    #   #     #do this only when agents reach a vertex to avoid slowing down model
    #   #     #TODO:  AOI choice has to be close enough to avoid returning home without visiting AOI
    #   #     # agent$timer <- time - agent$startTime
    #   #
    #   #
    #   #     #update dayPop with agent info
    #   #     dayPop[dayPop$id == agent$id, ] <- agent
    #   #
    #   #   }
    #   #
    #   # }
    # }


    #### DETERMINE TIME TO REACH NEXT POSITION ####
    #(only for vertex by vertex decisions, shortest path methods were immediately resolved)
    #if decisions were required (ignoring methods using shortest route)
    if(sum(toDecide & !toDecideShortest & ! toDecideNicestShortest) > 0){
      #determine time it takes for each agent

      #TODO: remove loop now that its possible

      toDecideNicest <- toDecide & !toDecideShortest & !toDecideNicestShortest
      #use that id directly in a vectorised fashion
      dist <- edgeTable$SHAPE_Leng[ match( dayPop$currentE[toDecideNicest], edgeTable$edgeID)  ]
      pth <-  edgeTable[ dayPop$currentE[toDecideNicest], , drop = TRUE]

      if(length(dayPop$moving[toDecideNicest]) != length(dist) ){
        #something went wrong
        browser()
      }

      if(sum(NA %in% dist) > 0){
        # browser()
        toDecideNicest[dist[toDecideNicest] == 0]
        }

      #determine timestep at which agent will reach vertex (current timestep + timesteps required)
      #adapt time to timestep
      addedTime <- ceiling(dist/(dayPop$speed[toDecideNicest]*1000/60))
      dayPop$moving[toDecideNicest] <- timestep + convertToTimesteps(time = addedTime, unit = "m")


      # for(agentNo in 1:nrow(dayPop[toDecide & !toDecideShortest & ! toDecideNicestShortest, ])){
      #
      #   agent <- dayPop[toDecide & !toDecideShortest & ! toDecideNicestShortest,][agentNo,]
      #
      #   #determine distance
      #   # using boolean operations with "to" and "from" columns for efficiency
      #   #TODO get current edge id when determining movements earlier
      #   #use that id directly in a vectorised fashion
      #   dist <- edgeTable$Shape_Leng[  agent$currentE ]
      #   pth <-  edgeTable[ agent$currentE, , drop = TRUE]
      #   #calculate timesteps
      #   #if multiple paths join two vertices
      #TODO: determine which path to choose when there are two earlier when currentE is chosen
      #   if(length(dist) > 1){
      #     #choose the most attractive
      #     dist <- pth$Shape_Leng[pth$DULN_final == max(pth$DULN_final)]
      #     if(length(dist) > 1){
      #       #if there is still a tie, choose shortest path
      #       dist <- min(dist)
      #       if(length(dist) > 1){
      #         #still a tie? choose randomly
      #         dist <- sample(dist, 1)
      #       }
      #     }
      #   }
      #   if(length(dist) == 0){browser()}
      #   if(!is.finite(dist)){ browser()}
      #
      #   #determine timestep at which agent will reach vertex (current timestep + timesteps required)
      #   agent$moving <- timestep + convertToTimesteps( (dist/1000)/agent$speed, unit = "h" )
      #
      #   #update dayPop with agent info
      #   dayPop[dayPop$id == agent$id, ] <- agent
      #
      # }
    }

    ## SAVE CURRENT EDGE AS PRIORE ####
    #save currentE into priorE for all agents that took decisions
    dayPop$priorE[toDecideRetracing | toDecideNicest] <- mapply(function(hstry, vstd) list(c(hstry, vstd)),
                                                                dayPop$priorE[toDecideRetracing | toDecideNicest],
                                                                dayPop$currentE[toDecideRetracing | toDecideNicest]
                                                                )

    dayPop$priorEAOI[toDecideRetracing | toDecideNicest] <- mapply(function(hstry, vstd) list(c(hstry, vstd)),
                                                                dayPop$priorEAOI[toDecideRetracing | toDecideNicest],
                                                                dayPop$currentE[toDecideRetracing | toDecideNicest]
    )



    dayPop$priorE[toDecideNicestShortest | toDecideShortest] <- mapply(function(hstry, vstd) list(c(hstry, vstd)),
                                                                dayPop$priorE[toDecideNicestShortest | toDecideShortest],
                                                                dayPop$currentE[toDecideNicestShortest | toDecideShortest]
                                                                )



    #update agent's timer with minutes passed (timesteps since last calculation)
    #do this only when agents reach a vertex to avoid slowing down model
    #TODO:  AOI choice has to be close enough to avoid returning home without visiting AOI
    timer <- time - dayPop$startTime




    # #PRINT UPDATE OF AGENTS
    # print("****************************************************")
    # print("----------------------------------------------------")
    # print(paste0("TIMESTEP:", timestep) )
    # print("----------------------------------------------------")
    # print(dayPop)
    # if( dayPop$moving[dayPop$id == 159] == timestep+1){
    #
    #   print(dayPop[dayPop$id == 159, c("behaviour", "currentV", "nextV", "goalV", "active") ])
    #
    #   }


    if(timestep%%100 == 0){
      vftDbg(paste0("TIME: ", time ))
      vftDbg(paste0("TIMESTEPS: ", timestep))
      vftDbg("--------------------------------")
    }
  }

  # TRANSFORM AGENT HISTORY INTO PATH USAGE ####
  # V(network)$passage <- 0
  # E(network)$passage <- 0
  #add passage column initialised with 0
  edgeTable$passage <- 0
  vertexTable$passage <- 0
  edgeTable$passageAOI <- 0
  vertexTable$passageAOI <- 0

  edgeTable$passageWalk <- 0
  edgeTable$passageWalkAOI <- 0
  edgeTable$passageDog <- 0
  edgeTable$passageDogAOI <- 0
  edgeTable$passageBike <- 0
  edgeTable$passageBikeAOI <- 0
  edgeTable$passageJog <- 0
  edgeTable$passageJogAOI <- 0

  #CPP FUNCTION:
  #historyToPassage: use each agent's history to create passage information in vertex and edge databases
  #returns a list of two vectors: edgePassage and vertexPassage
  #takes dataframe arguments but only returns vectors to reduce pressure on conversion between R and C++

  edgeAndVertexPassage <- historyToPassage_cpp( agentTable = dayPop,  edgeTable = edgeTable,  vertexTable = vertexTable)

  #place passages back into tables
  edgeTable$passage <- edgeAndVertexPassage$edge
  vertexTable$passage <- edgeAndVertexPassage$vertex

  #as well for areas NOT including shortest and nicestShortest behaviour (AOIs)
  edgeTable$passageAOI <- edgeAndVertexPassage$edgeAOI
  vertexTable$passageAOI <- edgeAndVertexPassage$vertexAOI

  #WALK agents
  #place passages back into tables
  edgeTable$passageWalk <- edgeAndVertexPassage$edgeWalk

  #as well for areas NOT including shortest and nicestShortest behaviour (AOIs)
  edgeTable$passageWalkAOI <- edgeAndVertexPassage$edgeWalkAOI

  #DOG agents
  #place passages back into tables
  edgeTable$passageDog <- edgeAndVertexPassage$edgeDog

  #as well for areas NOT including shortest and nicestShortest behaviour (AOIs)
  edgeTable$passageDogAOI <- edgeAndVertexPassage$edgeDogAOI

  #BIKE agents
  #place passages back into tables
  edgeTable$passageBike <- edgeAndVertexPassage$edgeBike

  #as well for areas NOT including shortest and nicestShortest behaviour (AOIs)
  edgeTable$passageBikeAOI <- edgeAndVertexPassage$edgeBikeAOI

  #JOG agents
  #place passages back into tables
  edgeTable$passageJog <- edgeAndVertexPassage$edgeJog

  #as well for areas NOT including shortest and nicestShortest behaviour (AOIs)
  edgeTable$passageJogAOI <- edgeAndVertexPassage$edgeJogAOI


  # Try counting passages within finalPolygons

  #get points within AOI
  pointsWithin <- igraph::V(network)$nodeID[igraph::V(network)$AOI != "0"]
  #determine adacent edges
  incidentEdges <- igraph::incident_edges(network, pointsWithin)
  edgesWithin <- igraph::E(network)$edgeID[igraph::E(network)$edgeID %in% unlist(incidentEdges)]

  edgeTable$passageAOI2 <- 0
  edgeTable$passageAOI2[igraph::E(network)$edgeID %in% edgesWithin] <- edgeTable$passage[igraph::E(network)$edgeID %in% edgesWithin]


  network <- tidygraph::tbl_graph(nodes = vertexTable, edges = edgeTable, directed = FALSE)

    #
  # for(agentNo in 1:nrow(dayPop) ){
  #   #for vertices
  #   history <- factor( dayPop$priorV[agentNo][[1]] )
  #   nodeIDs <- factor(V(network)$nodeID)
  #
  #   passageTable <- table(nodeIDs[match(history, nodeIDs)])
  #
  #
  #   V(network)$passage <- V(network)$passage + as.vector(passageTable)
  #
  #   #for edges
  #   history <- factor( dayPop$priorE[agentNo][[1]] )
  #   edgeIDs <- factor(E(network)$edgeID)
  #
  #   passageTable <- table(edgeIDs[match(history, edgeIDs)])
  #
  #
  #   E(network)$passage <- E(network)$passage + as.vector(passageTable)
  #
  # }

  vftDbg(paste0(nrow(dayPop), " AGENTS!"))
  vftDbg(paste0(sum(dayPop$active == TRUE), " AGENTS STILL ACTIVE!"))

  #GENERATE LINE STRINGS FOR ANALYSIS
  ## convert edge history into line strings for each agent's path
  #make list of lists to contain all agents' paths
  #each agent element has two elements: going, returning
  agentNicestPathsList <- sf::st_sfc(crs = 4326)
  agentPathsList <- sf::st_sfc(crs = 4326)

  #cycle through agents
  for(agentNo in 1:nrow(dayPop)){
    #get priorVs while recreating (all but nicestShortest)
    #append nicest after shortest
    # priorVGoing_points <- igraph::V(network)$geometry[igraph::V(network)$nodeID %in% dayPop$priorV[[agentNo]]]

    #for nicest areas (aoi)
    #get vertex points agents went through
    aoiPriorVMatch <- na.exclude( match(dayPop$nicestPriorV[[agentNo]], igraph::V(network)$nodeID))
    aoiPriorVGoing_points <- igraph::V(network)$geometry[ aoiPriorVMatch ]

    #use vertex points to create a linestring

    #for within aoi
    #ignore if there are no points or just 1
    if(length(aoiPriorVGoing_points) > 1){
      #verify that linestring is valid
      agentNicestPathGoing <- sf::st_cast(sf::st_combine( aoiPriorVGoing_points), "MULTILINESTRING")

      #add path to collection
      agentNicestPathsList <- c(agentNicestPathsList, agentNicestPathGoing)
    }

    #for all areas
    #get vertex points agents went through
    priorVMatch <- na.exclude( match(dayPop$priorV[[agentNo]], igraph::V(network)$nodeID))
    priorVGoing_points <- igraph::V(network)$geometry[ priorVMatch ]

    #use vertex points to create a linestring
    agentPathGoing <- sf::st_cast(sf::st_combine( priorVGoing_points), "MULTILINESTRING")

    #add path to collection
    agentPathsList <- c(agentPathsList, agentPathGoing)

  }


  ## convert vertex passage to edge passage

  return(list(pathUsage = network, dayPop = dayPop, pathNicestGeo = agentNicestPathsList,  pathGeo = agentPathsList))

}
