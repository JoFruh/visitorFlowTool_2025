#### Two-stage simulation: explore the areas, then choose among the walks ####

# A normal run lets every agent choose an area of interest by proximity and
# attractivity (findClosestAOI_cpp()) and then explore it one junction at a time
# (chooseBestRoutes_cpp()) inside launchSim()'s timestep loop. A two-stage run
# splits that in two:
#
#   PHASE 1  the normal simulation, unchanged, run as an exploration
#            (launchSim(explore = TRUE)). It keeps every agent's walk inside the
#            areas: where it entered (its goalV), the edges and nodes it walked in
#            order, where it turned home. That is the path library
#            (vftPathLibrary()).
#
#   PHASE 2  a new population - the same generatePopulation() call, drawn after
#            phase 1 so that phase 1 is exactly a normal run on the same seed -
#            in which every agent CHOOSES ONE RECORDED WALK, by the proximity of
#            its entrance and its mean attractivity (vftChoosePaths()), walks to
#            the entrance, follows the walk whole and goes home from where it
#            ended (vftFollowPaths()). No timestep loop runs: the only routing is
#            the two legs, the engine's own shortest routes.
#
# An agent chooses only among walks of its OWN TYPE and its OWN OUTING DURATION
# (timeExt), so a jogger with an hour takes an hour-long jogger's walk whole. If
# no explorer of that type had that duration, the nearest duration of the type
# stands in (`pathFallback`). An agent with no walk within reach stays home, as
# one with no area within reach does in a normal run.
#
# THE CHOICE is the normal area choice (findClosestAOI_cpp()) with a walk in place
# of an area - the same utility, the same standardisation, the same calibrated
# pair - and the walk's mean DULN in place of the area's DULN x size. For agent i
# over the walks j of its type and duration (vftPathUtility()):
#
#   d_ij     network distance from i's start to j's entrance, on the type's
#            "_attr" weights - the distances behind the normal area choice
#   t_ij     = max(d_ij / (1000 * speed_i), 0.01) hours
#            reachable only if t_ij <= reach * T_i / 60 (the engine's 10 %)
#   D_j      the walk's mean edge DULN of its type, weighted by the length walked
#            (an edge walked twice counts twice), minus the paths GDB's offset
#            (34.2423) to put it on the raster scale the areas' DULN is on
#   dSTD     = 1 + 9 * (t_ij/T_i - lo) / (hi - lo), with the area choice's bounds:
#            hi = (max_j d_ij / 2 km/h) / shortest outing,
#            lo = (min_j d_ij / 20 km/h) / longest outing
#   aSTD     = 1 + 9 * (D_j/T_i - lo) / (hi - lo), hi = max D / shortest outing,
#            lo = min D / longest outing (D over the whole library)
#   U_ij     = attrWeight * 3.23695 * aSTD - 6.65703 * dSTD
#   P_ij     = exp(U_ij / alpha) / sum over the reachable walks; alpha (default 1)
#            sharpens the choice below 1, as the route choice's alpha does
#
# Those wide, theoretical bounds keep the choice soft, as it is among the areas.
# (A first version standardised over the library and the reach limit instead: the
# terms then spanned ~30 and ~60 utility units, and agents took the nearest
# entrance almost deterministically - 98 % of GlattPark's into one area.) Walks
# that share an entrance share its distance, so an entrance that many explorers
# used draws more choices.
#
# Heat: in a heatwave run phase 1 is a heatwave run, and phase 2 reads the
# heat-weighted edge DULN for D_j and the heatwave's attrWeight - heat is used
# exactly where DULN is (R/heatwave_helpers.R).
#
# Passage: phase 2's agents carry the same history columns a normal run's do -
# priorE the way there and back, priorEAOI the walk inside - and go through the
# same vftHistoryToPathUsage(). historyToPassage_cpp() is called once per run,
# by phase 2 only.

#' The two-stage model's parameters. See the header of this file.
#'
#'   explorers    phase 1's population as a multiple of phase 2's
#'   reach        the reach rule: travel time to an entrance <= reach * duration
#'   bAttr,bDist  the utility's coefficients (the area choice's)
#'   alpha        the walk choice's logit scale, P ~ exp(U / alpha): 1 is the area
#'                choice's, below 1 is more selective
#'   weighting    the weights the entrance distance is measured on
#'   dulnOffset   the paths GDB's edge DULN over the raster's (vftAttrWeights())
#'   keepLibrary  TRUE also returns the path library and the legs (for testing)
VFT_TWO_STAGE <- list(explorers = 1, reach = 0.1, bAttr = 3.23695, bDist = -6.65703, alpha = 1,
                      weighting = "little_attr", dulnOffset = 34.2423, keepLibrary = FALSE)

#' A run's two-stage parameters: NULL for a normal run, VFT_TWO_STAGE with the
#' caller's entries in place otherwise.
#'
#' @param twoStage NULL or FALSE (normal run), TRUE (the defaults) or a list
#'   overriding entries of VFT_TWO_STAGE.
vftTwoStageParams <- function(twoStage){
  if(is.null(twoStage) || isFALSE(twoStage)) return(NULL)
  p <- VFT_TWO_STAGE
  if(is.list(twoStage)){
    unknown <- setdiff(names(twoStage), names(p))
    if(length(unknown)) stop("unknown two-stage parameter(s): ", paste(unknown, collapse = ", "))
    p[names(twoStage)] <- twoStage
  }
  p
}

#' Phase 1's walks inside the areas, one row per explorer that walked any.
#'
#' Called by launchSim(explore = TRUE) before the histories are dropped.
#'
#' @param dayPop the explorers, with entryV, priorEAOI and priorVAOI (the edges
#'   and the nodes of the walk inside the areas, in order).
#' @param edgeLength,rowOfEdge the edge lengths and launchSim()'s edge id ->
#'   row lookup (vftEdgeRowLookup()).
#' @return a data.frame: explorer, agentTyp, timeExt, speed, AOI, entryV, exitV,
#'   length_m, and the list columns `edges` (edge ids, repeats kept) and `nodes`
#'   (entryV first, exitV last, one more than `edges`).
vftPathLibrary <- function(dayPop, edgeLength, rowOfEdge){
  walked <- which(lengths(dayPop$priorEAOI) > 0 & !(dayPop$entryV %in% -1))
  edges  <- lapply(dayPop$priorEAOI[walked], as.integer)
  nodes  <- Map(function(v, w) as.integer(c(v, w)), dayPop$entryV[walked], dayPop$priorVAOI[walked])
  lib <- data.frame(explorer = dayPop$id[walked],
                    agentTyp = as.character(dayPop$agentTyp[walked]),
                    timeExt  = dayPop$timeExt[walked],
                    speed    = dayPop$speed[walked],
                    AOI      = as.character(dayPop$AOIGoal[walked]),
                    entryV   = as.integer(dayPop$entryV[walked]),
                    exitV    = vapply(nodes, function(n) n[length(n)], 1L),
                    length_m = vapply(edges, function(e) sum(edgeLength[rowOfEdge(e)], na.rm = TRUE), 1),
                    stringsAsFactors = FALSE)
  lib$edges <- edges
  lib$nodes <- nodes
  lib
}

#' Each walk's mean attractivity: its type's edge DULN, weighted by the length
#' walked (an edge walked twice counts twice).
#'
#' @param lib the path library (vftPathLibrary()).
#' @param edgeLength,rowOfEdge as for vftPathLibrary().
#' @param edgeDULN the seven DULN columns (VFT_DULN_TYPE_COLS), one value per
#'   edge row - a heatwave run's heat-weighted copy.
#' @return one value per walk; NA for a walk with no edge DULN at all.
vftPathDULN <- function(lib, edgeLength, rowOfEdge, edgeDULN){
  out <- rep(NA_real_, nrow(lib))
  if(!nrow(lib)) return(out)
  k    <- rep.int(seq_len(nrow(lib)), lengths(lib$edges))
  rows <- rowOfEdge(unlist(lib$edges, use.names = FALSE))
  col  <- unname(VFT_DULN_TYPE_COLS[lib$agentTyp])[k]
  d    <- rep(NA_real_, length(rows))
  for(cn in unique(col[!is.na(col)])){
    sel <- which(col == cn & !is.na(rows))
    d[sel] <- edgeDULN[[cn]][rows[sel]]
  }
  w  <- edgeLength[rows]
  ok <- is.finite(d) & is.finite(w) & w > 0
  if(!any(ok)) return(out)
  num <- rowsum(d[ok] * w[ok], k[ok])
  den <- rowsum(w[ok], k[ok])
  out[as.integer(rownames(num))] <- num[, 1] / den[, 1]
  out
}

#' The walk utility: the area choice's (findClosestAOI_cpp()), with a walk's mean
#' DULN in place of the area's DULN x size. See the header of this file.
#'
#' @param D the candidate walks' DULN, raster scale.
#' @param d the agent's distances to their entrances ("_attr" weights, metres) -
#'   ALL its candidates, as the area choice takes all areas it finds: the
#'   farthest and the nearest set the distance term's bounds.
#' @param speed,dur the agent's speed (km/h) and outing duration (minutes).
#' @param durRange the shortest and longest outing in the population (minutes).
#' @param Drange the lowest and highest walk DULN in the library.
#' @return one utility per candidate.
vftPathUtility <- function(D, d, speed, dur, durRange, Drange, attrWeight = 1,
                           bAttr = VFT_TWO_STAGE$bAttr, bDist = VFT_TWO_STAGE$bDist){
  #distance per duration, against the farthest candidate at the slowest speed in
  #the shortest outing and the nearest at the fastest in the longest
  td    <- pmax(d / (speed * 1000), 0.01)
  dpdHi <- (max(d) / (2 * 1000)) / durRange[1]
  dpdLo <- (min(d) / (20 * 1000)) / durRange[2]
  dSTD  <- if(dpdHi > dpdLo) 9 * (td / dur - dpdLo) / (dpdHi - dpdLo) + 1 else rep(1, length(d))
  #attractivity per duration, against the library's extremes in the extreme outings
  b     <- c(Drange[1] / durRange, Drange[2] / durRange)
  aSTD  <- if(max(b) > min(b)) 9 * (D / dur - min(b)) / (max(b) - min(b)) + 1 else rep(1, length(D))
  attrWeight * bAttr * aSTD + bDist * dSTD
}

#' Every agent's walk: one draw from the walks of its type and duration.
#'
#' See the header of this file for the utility.
#'
#' @param agents agentTyp, timeExt (minutes), speed (km/h), startV.
#' @param lib the path library with its `DULN` (vftPathDULN()).
#' @param adjPtr the adjacency lists (adjListsToPtr_cpp()).
#' @param params vftTwoStageParams().
#' @param attrWeight a heatwave run's weight on attractivity; 1 otherwise.
#' @return list(pick = library row or NA, fallback = TRUE where another
#'   duration stood in, distance = d to the chosen entrance, metres).
vftChoosePaths <- function(agents, lib, adjPtr, params, attrWeight = 1){
  n        <- nrow(agents)
  pick     <- rep(NA_integer_, n)
  fallback <- rep(FALSE, n)
  distance <- rep(NA_real_, n)
  Dall     <- lib$DULN - params$dulnOffset
  Drange   <- range(Dall[is.finite(Dall)])
  durRange <- range(agents$timeExt)

  for(typ in unique(agents$agentTyp)){
    ai <- which(agents$agentTyp == typ)
    li <- which(lib$agentTyp == typ & is.finite(lib$DULN))
    if(!length(li)) next

    #entrance -> start distances to EVERY entrance (the utility's bounds need the
    #farthest one too); the reach rule filters below
    entries <- unique(lib$entryV[li])
    starts  <- unique(as.integer(agents$startV[ai]))
    D    <- nodeDistances_cpp(adjPtr, params$weighting, typ, entries, starts, Inf)
    eRow <- match(lib$entryV[li], entries)

    durs <- sort(unique(lib$timeExt[li]))
    for(i in ai){
      dur <- agents$timeExt[i]
      #the nearest duration on offer when the agent's own is not (ties: shorter)
      cls <- if(dur %in% durs) dur else durs[which.min(abs(durs - dur))]
      fallback[i] <- cls != dur
      j   <- which(lib$timeExt[li] == cls)
      d   <- D[cbind(eRow[j], match(as.integer(agents$startV[i]), starts))]
      fin <- is.finite(d)
      if(!any(fin)) next
      j <- j[fin]; d <- d[fin]
      ok <- pmax(d / (1000 * agents$speed[i]), 0.01) <= params$reach * dur / 60
      if(!any(ok)) next
      U  <- vftPathUtility(Dall[li][j], d, agents$speed[i], dur, durRange, Drange,
                           attrWeight, params$bAttr, params$bDist)[ok]
      jj <- which(ok)
      k  <- if(length(jj) == 1L) 1L else sample.int(length(jj), 1L, prob = exp((U - max(U)) / params$alpha))
      pick[i]     <- li[j[jj[k]]]
      distance[i] <- d[jj[k]]
    }
  }
  list(pick = pick, fallback = fallback, distance = distance)
}

#' The legs of phase 2: the engine's shortest route from each source to its goal
#' ("distance" weights, as the walk to an area and the walk home), as nodes and
#' edges.
#'
#' @return list(nodes = , edges = ), one element per leg: nodes from source to
#'   goal, both included; a leg that starts at its goal is that node alone and
#'   no edge.
vftLegs <- function(adjPtr, edgeMapPtr, src, goal, typ){
  src   <- as.integer(src); goal <- as.integer(goal)
  nodes <- as.list(src)
  edges <- rep(list(integer(0)), length(src))
  need  <- which(src != goal)
  if(length(need)){
    sp <- findShortestRoute_cpp(adj_ptr = adjPtr, weighingMethod = "distance",
                                src_v = src[need], goal_v = goal[need],
                                agentTyps = typ[need])
    ge <- getEdges_cpp(src[need], goal[need], edgeMapPtr,
                       usingPathToGoal = TRUE, pathToGoal = sp$path,
                       oldPriorEs = rep(list(NULL), length(need)))
    for(k in seq_along(need)){
      i <- need[k]
      nodes[[i]] <- c(src[i], as.integer(sp$path[[k]]))
      edges[[i]] <- c(as.integer(ge$priorE[[1]][[k]]), as.integer(ge$currentE[[1]][[k]]))
    }
  }
  list(nodes = nodes, edges = edges)
}

#' Phase 2 of a two-stage run: a new population chooses among phase 1's walks.
#'
#' @param pop the new population (generatePopulation()): agentTyp, startV, id.
#' @param explored launchSim(explore = TRUE)'s result: list(paths, dayPop).
#' @param network,edgeTable,vertexTable the graph and its tables.
#' @param listOfPointers launchMultiSim()'s, with `$adj`.
#' @param edgeDULN the seven edge DULN columns the walks are judged on (NULL:
#'   the edge table's own; a heatwave run passes its heat-weighted copy).
#' @param attrWeight the heatwave run's attractivity weight, 1 otherwise.
#' @param params vftTwoStageParams().
#' @param progress the ABM bar, or NULL.
#' @return list(pathUsage = , dayPop = , twoStage = <summary>), as launchSim()
#'   returns, plus `paths` and `legs` when params$keepLibrary.
vftFollowPaths <- function(pop, explored, network, listOfPointers, edgeTable, vertexTable,
                           edgeDULN = NULL, attrWeight = 1, params = VFT_TWO_STAGE,
                           progress = NULL){
  t0 <- proc.time()[["elapsed"]]
  if(!is.null(progress))
    try(progress$set(1, message = "ABM läuft (oder fährt Rad...)"), silent = TRUE)

  lib <- explored$paths
  if(is.null(edgeDULN)) edgeDULN <- unclass(edgeTable)[VFT_DULN_TYPE_COLS]
  rowOfEdge  <- vftEdgeRowLookup(edgeTable$edgeID)
  edgeMapPtr <- buildEdgeMap_cpp(as.data.frame(edgeTable[, c("from", "to", "edgeID")]))
  adjPtr     <- listOfPointers$adj
  if(is.null(adjPtr)) adjPtr <- adjListsToPtr_cpp(listOfPointers)

  #### the choice ####
  dp <- vftAgentTimeAndSpeed(pop)
  lib$DULN <- vftPathDULN(lib, edgeTable$SHAPE_Leng, rowOfEdge, edgeDULN)
  ch   <- vftChoosePaths(dp, lib, adjPtr, params, attrWeight)
  has  <- which(!is.na(ch$pick))
  pick <- ch$pick[has]

  #### the walk to the entrance and home from the exit ####
  typ  <- as.character(dp$agentTyp[has])
  go   <- vftLegs(adjPtr, edgeMapPtr, dp$startV[has], lib$entryV[pick], typ)
  back <- vftLegs(adjPtr, edgeMapPtr, lib$exitV[pick], dp$startV[has], typ)

  #### the agents, with the histories a normal run's carry ####
  dp$AOIGoal       <- "NO_GOAL"
  dp$goalV         <- -1
  dp$pathId        <- NA_integer_
  dp$pathDULN      <- NA_real_
  dp$pathFallback  <- ch$fallback
  dp$entryDistance <- ch$distance
  dp$AOIGoal[has]  <- lib$AOI[pick]
  dp$goalV[has]    <- lib$entryV[pick]
  dp$pathId[has]   <- pick
  dp$pathDULN[has] <- lib$DULN[pick]

  dp$priorE    <- list(NULL)
  dp$priorEAOI <- list(NULL)
  dp$priorV    <- list(NULL)
  dp$retracedV <- list(NULL)
  if(length(has)){
    dp$priorE[has]    <- Map(c, go$edges, back$edges)
    dp$priorEAOI[has] <- lib$edges[pick]
    #the entrance and the exit end one list and start the next: once each
    dp$priorV[has]    <- Map(function(a, b, c) c(a, b[-1], c[-1]), go$nodes, lib$nodes[pick], back$nodes)
  }

  pathUsage <- vftHistoryToPathUsage(dp, network, edgeTable, vertexTable)

  ex <- explored$dayPop
  summary <- list(params      = params,
                  nExplorers  = nrow(ex),
                  nExplorerNoArea = sum(ex$goalV %in% -1 | ex$AOIGoal %in% "NO_GOAL"),
                  nPaths      = nrow(lib),
                  pathsByType = table(type = lib$agentTyp, duration = lib$timeExt),
                  nAgents     = nrow(dp),
                  nNoPath     = length(dp$id) - length(has),
                  nFallback   = sum(ch$fallback[has]),
                  secs        = proc.time()[["elapsed"]] - t0)

  dp  <- dp[, setdiff(names(dp), VFT_HISTORY_COLS), drop = FALSE]
  out <- list(pathUsage = pathUsage, dayPop = dp, twoStage = summary)
  if(isTRUE(params$keepLibrary)){
    out$paths <- lib
    out$legs  <- list(agent = has, go = go, back = back)
  }
  out
}
