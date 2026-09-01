#### One scenario, from a raw network to a finished simulation, in one job ####

# Clicking "Simulation" at step 5 used to cost up to THREE async jobs: the path
# network load, then the network preparation, then the ABM. Each one took its own
# place in the FIFO worker queue - so a user on a busy host waited behind other
# people's work three separate times - each round-tripped the graph through
# serialisation in both directions, and each raised and dropped a progress bar of
# its own, so the display went blank and came back with a different caption twice
# during a single click.
#
# The load has to stay separate: it is a PROVIDER key, cached in the app-level
# `r$network` for the rest of the session and shared with the newVersions page
# and the nav bar, and it happens at most once per session anyway. See
# vftScenarioNetworkThen() in R/prepare_network.R.
#
# The other two are one job, and this is it. The preparation result still comes
# back so that a second simulation on the same card is free - but only when this
# call actually prepared anything, so the common case sends nothing back.
#
# TWO BARS, NOT THREE AND NOT ONE. Everything up to and including the agents'
# goals is data preparation and drives the first; the ABM proper drives the
# second, which is not created until the ABM starts. See vftProgressPair() in
# R/async_helpers.R for how one worker drives two bars over one queue.

#' Prepare a scenario if it needs it, then simulate on it.
#'
#' Pure: no reactives, no session, no `r`. Meant to be called from inside
#' vftFuture(), the same rule vftPrepareNetwork() follows.
#'
#' The progress split, as fractions of the FIRST bar:
#'
#'   0.00 - 0.60  vftPrepareNetwork()  - skipped when the scenario is prepared
#'   0.60 - 0.70  generatePopulation()
#'   0.70 - 1.00  adjacency lists, then the agents and their goals
#'   close
#'
#' and then the second bar, created by its own first message: the shortest-path
#' pass and the timestep loop, with the ETA launchSim() already computes.
#'
#' @param network the SCENARIO's network, prepared or not.
#' @param finalPolygons the confirmed areas of interest.
#' @param minThresh the step-3 attractiveness threshold.
#' @param parking an already-loaded parking table, to skip the read.
#' @param residentDivision residents per simulated agent.
#' @param progPrep,progSim the two handles from vftProgressPair().
#'
#' @return list(network = , parking = , results = ). `network` and `parking` are
#'   NULL when the scenario was already prepared and there is nothing to write
#'   back - which also means the prepared graph does not cross the process
#'   boundary a second time for every re-run of the same card.
vftSimulateScenario <- function(network, finalPolygons, minThresh, parking = NULL,
                                residentDivision = 50,
                                progPrep = NULL, progSim = NULL){

  p <- function(f, detail = NULL){
    if(!is.null(progPrep))
      try(progPrep$set(f, detail = detail), silent = TRUE)
  }

  outNetwork <- NULL
  outParking <- NULL

  #### the network itself ####
  #Once per SCENARIO, not once per launch: vftNetworkPrepared() tests the graph's
  #own attribute names, so a scenario that arrived from a save file, from step 4
  #or from the newVersions page is judged on what it actually carries.
  if(!vftNetworkPrepared(network)){
    vftDbg("SIMULATE: preparing the network")
    out <- vftPrepareNetwork(network, finalPolygons, minThresh,
                             parking = parking, progress = progPrep,
                             base = 0, span = 0.6)
    network    <- out$network
    outNetwork <- out$network
    outParking <- out$parking
  }else{
    vftDbg("SIMULATE: network already prepared")
  }

  #### the population ####
  p(0.6, "Bevölkerung wird erzeugt...")

  #determine sum of residents in area of focus
  nbResidents <- sum(igraph::V(network)$Residents, na.rm = TRUE)
  #determine number of agents
  # do not divide by CONST for glatt/wigger subset
  nbAgents <- nbResidents / residentDivision

  vftDbg("GENERATE POPULATION")
  #get dataframe of all agents, their characteristics and their starting positions
  pop <- generatePopulation(network, nAgents = nbAgents, parkingIntensity = 0.1)

  #### the agents, then the ABM ####
  vftDbg("LAUNCH MULTISIM")
  results <- launchMultiSim(pop, network, days = "1wk", finalPolygons = finalPolygons,
                            progress = progPrep, progressSim = progSim,
                            base = 0.7, span = 0.3,
                            #the preparation bar goes away BEFORE the ABM bar
                            #appears, so the user sees one bar at a time rather
                            #than two stacked ones.
                            onPrepDone = function(){
                              if(!is.null(progPrep)) try(progPrep$close(), silent = TRUE)
                            })

  if(!is.null(progSim)) try(progSim$close(), silent = TRUE)

  list(network = outNetwork, parking = outParking, results = results)
}
