#### Engine corrections, as hooks: the area choice and the route choice in R ####
#
# Shared by data-raw/validate_usage_surveys.R (VFT_VAL_ENGINE=corrected) and
# data-raw/sweep_straight_reach.R. Definitions only; nothing runs here.
#
#   aoiProbsR()     findClosestAOI_cpp()'s multinomial logit (src/CPP_FUNCTIONS.cpp
#                   ~2021-2396) redone from the engine's own distances, with:
#                   the reach limit, optionally on the REAL path length
#                   (trueAreaDistances()) instead of the attractivity-weighted one;
#                   a floor on that limit; and a fallback for an agent with nothing
#                   in reach - a uniform pick among the areas within FALLBACK_H of
#                   its closest one, if that one is within `fallbackCap` of its
#                   outing.
#   chooseRoutesR() chooseBestRoutes_cpp() (~135-366) with the turn term (+1 within
#                   45 degrees, -2 past 90) times s. Single-precision lon/lat is
#                   emulated, as the C++ does it.
# At the engine's values both reproduce the C++ exactly (sweep verify stage:
# area 1e-16, route choices 0 of 48,150 off).
#
# The corrected engine, as decided on 2026-10-13 from the sweeps:
#   no straight-on preference (s = 0); the 10 % limit on the real distance, never
#   below 5 minutes; the closest-area fallback, capped at 33 % of the outing.
CORR <- list(straight = local({ v <- Sys.getenv("VFT_VAL_STRAIGHT", ""); if(nzchar(v)) as.numeric(v) else 0 }),
             #(VFT_VAL_STRAIGHT overrides the turn-term scale: 1 = the engine's)
             #(VFT_VAL_REACH overrides the reach: the share of the outing an area may
             #be away, on the real distance)
             reach = local({ v <- Sys.getenv("VFT_VAL_REACH", ""); if(nzchar(v)) as.numeric(v) else 0.10 }),
             floorH = 5 / 60, fallbackH = 1 / 60, fallbackCap = 0.33,
             #the weight of a node the agent has already passed (the engine: 0.00001).
             #NA = no penalty: the walk may come back over its own track. VFT_VAL_REVISIT
             #overrides it ("none" or a number) for diagnostic runs.
             revisit = local({ v <- Sys.getenv("VFT_VAL_REVISIT", "")
               if(!nzchar(v)) 0.00001 else if(identical(v, "none")) NA_real_ else as.numeric(v) }),
             #the scale of the route choice's random utility, exp(value / alpha): the
             #engine's 0.5 makes the most attractive neighbour ~10^4 times likelier
             #than the least at a median junction. VFT_VAL_ALPHA overrides it.
             alpha = local({ v <- Sys.getenv("VFT_VAL_ALPHA", ""); if(nzchar(v)) as.numeric(v) else 0.5 }),
             #the junction DULN weight wQ in exp(wQ * DULN + (s / alpha) * turn), set on
             #its own; NA = DULN / alpha as the engine has it. VFT_VAL_WQ overrides it.
             wQ = local({ v <- Sys.getenv("VFT_VAL_WQ", ""); if(nzchar(v)) as.numeric(v) else NA_real_ }),
             #which DULN the junction choice reads: "node" (the engine: the candidate
             #node's raster value) or "edge" (the paths GDB value of the edge to it, times
             #EDGE_K). VFT_VAL_JUNCTION overrides it.
             junction = local({ v <- Sys.getenv("VFT_VAL_JUNCTION", ""); if(nzchar(v)) v else "node" }))
FLOOR_H    <- 5 / 60                #the floor, hours
FALLBACK_H <- 1 / 60                #the fallback band above the closest area, hours
#edge DULN differences between the edges of one junction are ~1.4x smaller than the
#node DULN differences of its neighbours (within-junction SD, junctions of degree >= 3:
#walkNat 2.81/2.00 GlattPark, 2.73/1.97 Wigger, 3.08/2.14 Glatt; cor .64-.81), so the
#edge values are scaled by this to keep the same weight on the same spread
EDGE_K     <- 1.4
.corrStats <- new.env()

#### the area choice, with another reach ####

#findClosestAOI_cpp()'s probabilities, from its own distances (~2021-2396)
#`trueDist`, if given: per agent, the REAL path length to each area in res$aoi
#order; the reach test then uses it instead of the engine's weighted distance.
#`bAttr`, `bDist`: the utility's two coefficients (the engine's GlattPark pair by
#default), one value or one per agent. `durRange`: the shortest and longest outing
#the standardisation is bounded by; NULL = the population's, as the engine does
#(30 and 240 minutes whenever the durations are the engine's classes).
aoiProbsR <- function(res, agentSpeeds, agentDurations, AOI_aois, AOI_dulns, AOI_areas, reach,
                      trueDist = NULL, floorH = 0, fallback = FALSE, fallbackCap = Inf,
                      bAttr = 3.23695, bDist = -6.65703, durRange = NULL){
  dulnMax <- max(AOI_dulns); dulnMin <- min(AOI_dulns)
  if(is.null(durRange)) durRange <- range(agentDurations)
  durMax  <- durRange[2]; durMin <- durRange[1]
  bAttr <- rep_len(bAttr, length(res$prob)); bDist <- rep_len(bDist, length(res$prob))
  lastMatch <- function(x, table){ k <- match(x, rev(table)); ifelse(is.na(k), NA_integer_, length(table) + 1L - k) }
  lapply(seq_along(res$prob), function(i){
    d <- res$distances[[i]]
    #an agent that cannot reach every area gets no distances back (~2011): its
    #probabilities cannot be redone, so they stay the engine's
    if(!length(d)) return(res$prob[[i]])
    speed <- agentSpeeds[i]; dur <- agentDurations[i]
    tdMax <- max(d) / (2 * 1000); tdMin <- min(d) / (20 * 1000)
    td <- d / (speed * 1000)
    td[td < 0.01] <- 0.01
    if(length(AOI_aois) > 1){
      k <- lastMatch(res$aoi[[i]], AOI_aois)
      duln <- AOI_dulns[k]; area <- sqrt(AOI_areas[k]); area[area > 7000] <- 7000
      duln[is.na(k)] <- 0; area[is.na(k)] <- 0
      dpd   <- td / dur
      dbapd <- duln * area / dur
      dpdMax <- tdMax / durMin; dpdMin <- tdMin / durMax
      dbMax  <- (dulnMax * 15000) / durMin; dbMin <- (dulnMin * 316) / durMax
      dbSTD <- ((10 - 1) * (dbapd - dbMin) / (dbMax - dbMin)) + 1
      dSTD  <- ((10 - 1) * (dpd - dpdMin) / (dpdMax - dpdMin)) + 1
      u <- (bAttr[i] * dbSTD) + (bDist[i] * dSTD)
    }else u <- rep(1, length(d))
    tReach <- if(is.null(trueDist)) td else trueDist[[i]] / (speed * 1000)
    u[tReach > max(reach * (dur / 60), floorH)] <- 0
    nz <- u != 0
    s  <- sum(exp(u[nz]))
    p  <- ifelse(nz, exp(u), 0)
    if(s != 0) return(p / s)
    #nothing in reach: a uniform pick among the closest areas
    if(fallback && !is.null(trueDist)){
      ok <- is.finite(tReach)
      if(any(ok) && min(tReach[ok]) <= fallbackCap * (dur / 60)){
        pick <- ok & tReach <= min(tReach[ok]) + FALLBACK_H
        .corrStats$fallback <- .corrStats$fallback + 1L
        return(pick / sum(pick))
      }
    }
    p * 0
  })
}

#The real path length (SHAPE_Leng) from every node to the nearest node of every
#area: one search per area, from a helper node joined to all its nodes at length
#0. Nodes are indexed by nodeID (= the vertex index on these networks).
.trueCache <- new.env()
trueAreaDistances <- function(ctx, net){
  if(identical(.trueCache$ctx, ctx)) return(.trueCache$D)
  ids <- igraph::vertex_attr(net, "nodeID")
  stopifnot(identical(as.numeric(ids), as.numeric(seq_along(ids))))
  aoiV <- igraph::vertex_attr(net, "AOI")
  areas <- setdiff(unique(aoiV), "0")
  #a bare graph: the network's sf geometry attributes refuse add_vertices()
  n <- igraph::vcount(net)
  g <- igraph::make_graph(as.vector(t(igraph::as_edgelist(net, names = FALSE))), n = n, directed = FALSE)
  w <- igraph::edge_attr(net, "SHAPE_Leng")
  D <- matrix(Inf, n, length(areas), dimnames = list(NULL, areas))
  for(a in areas){
    nodes <- which(aoiV == a)
    g2 <- igraph::add_vertices(g, 1)
    g2 <- igraph::add_edges(g2, as.vector(rbind(n + 1L, nodes)))
    D[, a] <- igraph::distances(g2, v = n + 1L, to = seq_len(n), weights = c(w, rep(0, length(nodes))))[1, ]
  }
  .trueCache$ctx <- ctx; .trueCache$D <- D
  D
}

#### the two-stage walk choice, with the corrected reach (type twoStage) ####

#The engine's adjacency lists with every weight the REAL edge length (SHAPE_Leng, no
#10 m floor), for ns$nodeDistances_cpp(): the real path length between two nodes.
lengthAdjPtr <- function(net){
  el  <- igraph::as_edgelist(net, names = FALSE)
  len <- igraph::edge_attr(net, "SHAPE_Leng")
  et  <- data.frame(from = as.integer(el[, 1]), to = as.integer(el[, 2]))
  for(t in names(ns$VFT_DULN_TYPE_COLS)) for(s in c("", "_attr", "_ATTR"))
    et[[paste0("SHAPE_Leng_", t, s)]] <- len
  vt <- data.frame(nodeID = as.integer(igraph::vertex_attr(net, "nodeID")))
  ns$adjListsToPtr_cpp(ns$generateAdjListAndDistTbl_cpp(edgeTable = et, vertexTable = vt))
}

#vftChoosePaths() (R/two_stage.R) with aoiProbsR()'s corrections for the areas: the
#same utility (ns$vftPathUtility(), on the engine's "_attr" distance to a walk's
#entrance, over every walk of the class), but the reach test on the REAL path length,
#never below floorH; with nothing in reach, a uniform pick among the walks whose
#entrance is within fallbackH of the closest one, if that one is within fallbackCap of
#the outing (counted in .corrStats$pathFallback). params$alpha scales the logit as in
#vftChoosePaths(); the fallback stays uniform.
choosePathsCorr <- function(agents, lib, adjPtr, params, attrWeight = 1, lenPtr,
                            reach = CORR$reach, floorH = CORR$floorH,
                            fallbackCap = CORR$fallbackCap, fallbackH = FALLBACK_H){
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
    entries <- unique(lib$entryV[li])
    starts  <- unique(as.integer(agents$startV[ai]))
    #the real length only as far as any of these agents may go, by the reach or
    #the fallback; the utility's distances to every entrance
    far <- max(pmax(max(reach, fallbackCap) * agents$timeExt[ai] / 60, floorH + fallbackH) *
                 agents$speed[ai] * 1000)
    Dr   <- ns$nodeDistances_cpp(lenPtr, "distance", typ, entries, starts, far)
    Dw   <- ns$nodeDistances_cpp(adjPtr, params$weighting, typ, entries, starts, Inf)
    eRow <- match(lib$entryV[li], entries)
    durs <- sort(unique(lib$timeExt[li]))
    for(i in ai){
      dur <- agents$timeExt[i]
      cls <- if(dur %in% durs) dur else durs[which.min(abs(durs - dur))]
      fallback[i] <- cls != dur
      j   <- which(lib$timeExt[li] == cls)
      col <- match(as.integer(agents$startV[i]), starts)
      sp  <- agents$speed[i]
      dw  <- Dw[cbind(eRow[j], col)]
      fin <- is.finite(dw)
      if(!any(fin)) next
      j <- j[fin]; dw <- dw[fin]
      hR  <- Dr[cbind(eRow[j], col)] / (1000 * sp)
      ok  <- is.finite(hR) & hR <= max(reach * dur / 60, floorH)
      if(any(ok)){
        U  <- ns$vftPathUtility(Dall[li][j], dw, sp, dur, durRange, Drange, attrWeight,
                                params$bAttr, params$bDist)[ok]
        jj <- which(ok)
        k  <- if(length(jj) == 1L) 1L else sample.int(length(jj), 1L, prob = exp((U - max(U)) / params$alpha))
        pick[i]     <- li[j[jj[k]]]
        distance[i] <- dw[jj[k]]
      }else{
        okF <- is.finite(hR)
        if(any(okF) && min(hR[okF]) <= fallbackCap * dur / 60){
          cand <- which(okF & hR <= min(hR[okF]) + fallbackH)
          k    <- if(length(cand) == 1L) 1L else sample.int(length(cand), 1L)
          pick[i]     <- li[j[cand[k]]]
          distance[i] <- dw[cand[k]]
          .corrStats$pathFallback <- .corrStats$pathFallback + 1L
        }
      }
    }
  }
  list(pick = pick, fallback = fallback, distance = distance)
}

#### the calibrated model (type fullCal of validate_usage_surveys.R) ####
#
# One parameter list, CAL_DEFAULT's names:
#   area choice  a multinomial logit over the areas the engine found, on the REAL path
#                length t (hours) and the outing duration T (hours):
#                  U = bT * t/T + bD * t + bQ * DULN + bA * log(area in ha)
#                DULN and area per letter (area-weighted, summed). An area is in reach
#                when t <= max(reach * T, floorH); with none in reach, the closest-area
#                fallback (FALLBACK_H band) if the closest is within fallbackCap * T.
#                No standardisation: the coefficients are on the raw scales, so they
#                transfer between sites.
#   movement     chooseRoutesR() with the node DULN times wQ and the turn term (+1 / 0
#                / -2) times wS, at alpha 1: weight exp(wQ * DULN + wS * turn). The
#                engine is wQ = 2, wS = 2 (alpha 0.5, s = 1). wQ = 0 and wS = 0 walk
#                uniformly (the revisit penalty stays).
#   legs         f, the strength of the "" weights (vftAttrWeights(), 8 normally)
CAL_DEFAULT <- list(bT = 0, bD = -30, bQ = 0, bA = 0, reach = 0.10, floorH = 5 / 60,
                    fallbackCap = 0.33, wQ = 2, wS = 2, revisit = 0.00001, f = 8)

aoiProbsCal <- function(res, src_v, agentSpeeds, agentDurations, AOI_aois, AOI_dulns, AOI_areas,
                        trueD, par){
  duln <- tapply(AOI_dulns * AOI_areas, AOI_aois, sum) / tapply(AOI_areas, AOI_aois, sum)
  area <- tapply(AOI_areas, AOI_aois, sum)
  lapply(seq_along(res$prob), function(i){
    a <- as.character(res$aoi[[i]])
    if(!length(a)) return(res$prob[[i]])
    d <- trueD[src_v[i], match(a, colnames(trueD))]
    t <- d / (agentSpeeds[i] * 1000); T <- agentDurations[i] / 60
    t[!is.finite(t)] <- Inf
    u <- par$bT * t / T + par$bD * t + par$bQ * unname(duln[a]) + par$bA * log(unname(area[a]) / 1e4)
    ok <- is.finite(t) & is.finite(u) & t <= max(par$reach * T, par$floorH)
    if(any(ok)){
      e <- numeric(length(a))
      e[ok] <- exp(u[ok] - max(u[ok]))
      return(e / sum(e))
    }
    ok <- is.finite(t)
    if(any(ok) && min(t[ok]) <= par$fallbackCap * T){
      pick <- ok & t <= min(t[ok]) + FALLBACK_H
      .corrStats$fallback <- .corrStats$fallback + 1L
      return(pick / sum(pick))
    }
    numeric(length(a))
  })
}

chooseRoutesCal <- function(viableRoutes, DULN_df, priorVs, agentTyps, V_coords_df, currentVs, par){
  for(cl in DULN_BY_TYPE) DULN_df[[cl]] <- DULN_df[[cl]] * par$wQ
  chooseRoutesR(viableRoutes, DULN_df, priorVs, agentTyps, V_coords_df, currentVs, par$wS,
                revisit = par$revisit, alpha = 1)
}

#### the route choice, with the turn term scaled ####

f32 <- function(x) readBin(writeBin(as.double(x), raw(), size = 4), "double", size = 4, n = length(x))
DULN_BY_TYPE <- c(walkNat = "DULN_WALK_", walkSoc = "DULN_WALK1", dogNat = "DULN_DOG_N",
                  dogProx = "DULN_DOG_P", ebikeNat = "DULN_EBIKE", bikeSport = "DULN_BIKER",
                  jogger = "DULN_JOGGE")

#The edge DULN per (junction, neighbour) for the "edge" junction choice: both
#directions of every edge, parallel edges averaged. Keys are from * N + to on the
#vertex index, which is the nodeID in the validation networks (checked).
edgeDulnTable <- function(net){
  if(!identical(as.numeric(igraph::vertex_attr(net, "nodeID")), as.numeric(seq_len(igraph::vcount(net)))))
    stop("edgeDulnTable: nodeID is not the vertex index")
  el <- igraph::ends(net, igraph::E(net), names = FALSE)
  N  <- igraph::vcount(net) + 1
  M  <- vapply(DULN_BY_TYPE, function(cl) as.numeric(igraph::edge_attr(net, cl)), numeric(nrow(el)))
  #named by column, as chooseRoutesR() looks them up (vapply names them by agent type)
  colnames(M) <- unname(DULN_BY_TYPE)
  key <- c(el[, 1] * N + el[, 2], el[, 2] * N + el[, 1])
  M  <- rbind(M, M)
  #parallel edges: the mean
  u  <- unique(key)
  g  <- match(key, u)
  M  <- rowsum(M, g, reorder = FALSE, na.rm = TRUE) / rowsum((!is.na(M)) + 0, g, reorder = FALSE)
  list(key = u, M = M, N = N)
}

#chooseBestRoutes_cpp() (~135-366) in R. s scales the +1 / 0 / -2 of the turn.
#wQ (NA = the engine's DULN / alpha) sets the DULN weight on its own; edgeTab (from
#edgeDulnTable()) swaps the candidate node's DULN for the edge's, times EDGE_K.
chooseRoutesR <- function(viableRoutes, DULN_df, priorVs, agentTyps, V_coords_df, currentVs, s,
                          revisit = 0.00001, alpha = 0.5, wQ = NA_real_, edgeTab = NULL){
  n  <- lengths(viableRoutes)
  ag <- rep.int(seq_along(viableRoutes), n)
  rt <- as.numeric(unlist(viableRoutes, use.names = FALSE))
  ids <- DULN_df$nodeID
  row <- match(rt, ids)
  M   <- as.matrix(DULN_df[, DULN_BY_TYPE])
  colI <- match(DULN_BY_TYPE[as.character(agentTyps)], colnames(M))
  val <- M[cbind(row, colI[ag])]
  if(!is.null(edgeTab)){
    er  <- match(as.numeric(currentVs)[ag] * edgeTab$N + rt, edgeTab$key)
    val <- EDGE_K * edgeTab$M[cbind(er, match(DULN_BY_TYPE[as.character(agentTyps)], colnames(edgeTab$M))[ag])]
    if(length(val) && all(is.na(val))) stop("chooseRoutesR: no edge DULN found for any candidate")
  }
  val[is.na(val)] <- 0
  if(!is.na(wQ)) val <- val * (wQ * alpha)
  X <- f32(V_coords_df$X); Y <- f32(V_coords_df$Y)
  hasP <- !vapply(priorVs, is.null, logical(1))
  if(s != 0 && any(hasP)){
    lastP <- vapply(priorVs, function(p) if(length(p)) as.numeric(p[length(p)]) else NA_real_, 1)
    pr <- match(lastP, ids); cr <- match(as.numeric(currentVs), ids)
    sel <- hasP[ag] & !is.na(row) & !is.na(pr[ag]) & !is.na(cr[ag])
    if(any(sel)){
      a <- ag[sel]; nr <- row[sel]
      cvx <- X[cr[a]]; cvy <- Y[cr[a]]; pvx <- X[pr[a]]; pvy <- Y[pr[a]]
      h1 <- f32(atan2(f32(Y[nr] - cvy), f32(X[nr] - cvx)))
      h0 <- f32(atan2(f32(cvy - pvy), f32(cvx - pvx)))
      ang <- f32(abs(f32(h1 - h0)) * (180 / 3.14159265359))
      ang <- ifelse(ang > 180, f32(180 - f32(ang - 180)), ang)
      term <- ifelse(ang <= 45, 1, ifelse(ang <= 90, 0, -2))
      #the C++ skips the turn when pv is (0, 0); a lon/lat node never is
      val[sel] <- val[sel] + s * term
    }
  }
  e <- exp(val / alpha)
  p <- e / rowsum(e, ag, reorder = FALSE)[ag]
  #nodes already passed: 0.00001, where there is more than one choice (~356)
  inPrior <- unlist(lapply(seq_along(viableRoutes), function(i)
    if(hasP[i] && n[i] > 1) as.numeric(viableRoutes[[i]]) %in% as.numeric(priorVs[[i]]) else rep(FALSE, n[i])),
    use.names = FALSE)
  if(!is.na(revisit)) p[inPrior] <- revisit
  unname(split(p, factor(ag, levels = seq_along(viableRoutes))))
}
