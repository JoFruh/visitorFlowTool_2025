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
             reach = 0.10, floorH = 5 / 60, fallbackH = 1 / 60, fallbackCap = 0.33,
             #the weight of a node the agent has already passed (the engine: 0.00001).
             #NA = no penalty: the walk may come back over its own track. VFT_VAL_REVISIT
             #overrides it ("none" or a number) for diagnostic runs.
             revisit = local({ v <- Sys.getenv("VFT_VAL_REVISIT", "")
               if(!nzchar(v)) 0.00001 else if(identical(v, "none")) NA_real_ else as.numeric(v) }),
             #the scale of the route choice's random utility, exp(value / alpha): the
             #engine's 0.5 makes the most attractive neighbour ~10^4 times likelier
             #than the least at a median junction. VFT_VAL_ALPHA overrides it.
             alpha = local({ v <- Sys.getenv("VFT_VAL_ALPHA", ""); if(nzchar(v)) as.numeric(v) else 0.5 }))
FLOOR_H    <- 5 / 60                #the floor, hours
FALLBACK_H <- 1 / 60                #the fallback band above the closest area, hours
.corrStats <- new.env()

#### the area choice, with another reach ####

#findClosestAOI_cpp()'s probabilities, from its own distances (~2021-2396)
#`trueDist`, if given: per agent, the REAL path length to each area in res$aoi
#order; the reach test then uses it instead of the engine's weighted distance.
aoiProbsR <- function(res, agentSpeeds, agentDurations, AOI_aois, AOI_dulns, AOI_areas, reach,
                      trueDist = NULL, floorH = 0, fallback = FALSE, fallbackCap = Inf){
  dulnMax <- max(AOI_dulns); dulnMin <- min(AOI_dulns)
  durMax  <- max(agentDurations); durMin <- min(agentDurations)
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
      u <- (3.23695 * dbSTD) + (-6.65703 * dSTD)
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

#### the route choice, with the turn term scaled ####

f32 <- function(x) readBin(writeBin(as.double(x), raw(), size = 4), "double", size = 4, n = length(x))
DULN_BY_TYPE <- c(walkNat = "DULN_WALK_", walkSoc = "DULN_WALK1", dogNat = "DULN_DOG_N",
                  dogProx = "DULN_DOG_P", ebikeNat = "DULN_EBIKE", bikeSport = "DULN_BIKER",
                  jogger = "DULN_JOGGE")

#chooseBestRoutes_cpp() (~135-366) in R. s scales the +1 / 0 / -2 of the turn.
chooseRoutesR <- function(viableRoutes, DULN_df, priorVs, agentTyps, V_coords_df, currentVs, s,
                          revisit = 0.00001, alpha = 0.5){
  n  <- lengths(viableRoutes)
  ag <- rep.int(seq_along(viableRoutes), n)
  rt <- as.numeric(unlist(viableRoutes, use.names = FALSE))
  ids <- DULN_df$nodeID
  row <- match(rt, ids)
  M   <- as.matrix(DULN_df[, DULN_BY_TYPE])
  colI <- match(DULN_BY_TYPE[as.character(agentTyps)], colnames(M))
  val <- M[cbind(row, colI[ag])]
  val[is.na(val)] <- 0
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
