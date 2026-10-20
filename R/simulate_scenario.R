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
#
# The load, when there is one, shows on that same first bar: step 5 makes the
# bar before asking for the network and lends it to the provider for its first
# VFT_SIM_NETWORK_SHARE (vftProviderLendBar() in R/providers.R), and this job
# fills the rest. One bar for the data, one for the ABM.

#' How much of step 5's data bar the path network's load fills, when the launch
#' has to load it first.
VFT_SIM_NETWORK_SHARE <- 0.4

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
#' @param heat a heatwave run's heat map: a SpatRaster, or the PackedSpatRaster
#'   it crossed the process boundary as (terra::wrap()). NULL is a normal run.
#' @param heatParams the heatwave model's parameters, VFT_HEATWAVE by default.
#'   See R/heatwave_helpers.R.
#' @param twoStage NULL for a normal run; TRUE, or a list overriding entries of
#'   VFT_TWO_STAGE, for a two-stage run: the simulation explores, and a second
#'   population chooses among the walks it recorded. See R/two_stage.R.
#'
#' @return list(network = , parking = , results = ). `network` and `parking` are
#'   NULL when the scenario was already prepared and there is nothing to write
#'   back - which also means the prepared graph does not cross the process
#'   boundary a second time for every re-run of the same card. A heatwave run's
#'   `results$heatwave` summarises the parameters and the exposure; the graph
#'   and the pathUsage carry no heat. A two-stage run's `results$twoStage`
#'   summarises the path library and the choice.
vftSimulateScenario <- function(network, finalPolygons, minThresh, parking = NULL,
                                residentDivision = 50,
                                progPrep = NULL, progSim = NULL,
                                heat = NULL, heatParams = VFT_HEATWAVE,
                                twoStage = NULL){

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
  }else if(!vftWeightsCurrent(network)){
    #prepared before the weight formula was corrected, or edited since: the
    #weights are recomputed from the DULN the graph carries now, and the graph
    #goes back so the card does not pay for this again. See vftAttrWeights().
    vftDbg("SIMULATE: recomputing the attractivity-weighted distances")
    network    <- vftRefreshAttrWeights(network, minThresh)
    outNetwork <- network
  }else{
    vftDbg("SIMULATE: network already prepared")
  }

  #### a heatwave run: how hot every edge, node and area is ####
  #The tables are built here because the exposure needs their geometry, and
  #handed on so launchMultiSim() does not build them a second time. No random
  #numbers are drawn, so the stream the population draws from is unchanged.
  heatwave    <- NULL
  edgeTable   <- NULL
  vertexTable <- NULL
  if(!is.null(heat)){
    p(0.6, "Hitzebelastung der Wege wird bestimmt...")
    if(inherits(heat, "PackedSpatRaster")) heat <- terra::unwrap(heat)
    edgeTable   <- vftGraphTibble(network, "edges")
    vertexTable <- vftGraphTibble(network, "nodes")
    heatwave <- list(exposure = vftHeatwaveExposure(edgeTable, vertexTable,
                                                    finalPolygons, heat, heatParams),
                     params   = heatParams)
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
  #(a car park weighs as much as its nearest area is attractive - heat-weighted
  #in a heatwave run)
  parkingAttr <- if(!is.null(heatwave))
    vftHeatwaveParkingAttr(vertexTable, finalPolygons, heatwave$exposure$area,
                           heatParams$kappa)
  drawPop <- function(n) generatePopulation(network, nAgents = n, parkingIntensity = 0.1,
                                            parkingAttr = parkingAttr)
  #a two-stage run: this population explores (`explorers` times as many), and
  #phase 2 draws its own from the same call once the exploration is done
  ts  <- vftTwoStageParams(twoStage)
  pop <- drawPop(if(is.null(ts)) nbAgents else nbAgents * ts$explorers)

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
                            },
                            minThresh = minThresh,
                            edgeTable = edgeTable, vertexTable = vertexTable,
                            heatwave = heatwave,
                            twoStage = if(!is.null(ts))
                              list(params = ts, makePop = function() drawPop(nbAgents)))

  if(!is.null(progSim)) try(progSim$close(), silent = TRUE)

  list(network = outNetwork, parking = outParking, results = results)
}
