#### Per-type DULN from the Glatt survey ####
#
# The seven agent types' DULN layers differ by ASSUMED weights (the xlsx columns of
# GenerateUpdateAttractivityLayer.R). The Glatt survey (FilBleu, the full export) has
# each route's activity and motives, so the differences can be MEASURED there:
#   walking       -> Social walkers if social contact is rated at least as important
#                    as nature experience, else Nature walkers
#   dog walking   -> Nature dog walkers if nature experience is "wichtig", else
#                    Proximity dog walkers
#   jogging       -> Joggers;  biking -> Sports bikers;  e-biking -> Nature e-bikers
# (the user's rules, 2026-10-18).
#
# Stages (VFT_TY_STAGES):
#   survey    the routes typed; a survey map per type on Glatt's grid
#   capture   Nearby-Random (40 reps) and today's Full (20 reps) at Glatt with the
#             track of every agent TYPE kept (the stored runs keep only the total);
#             their totals must equal the stored runs
#   fit       the shared fit (all types, one weight set) and, per type, its
#             DIFFERENCE from it - ridge again, lambda by 2 km block CV, so a type
#             with few routes stays at the shared weights unless its routes say
#             otherwise. Each type's offset is its own Nearby-Random map. A second
#             fit without Glatt's cells on GlattPark's ground serves GlattPark.
#   variants  the networks, as tailor_duln.R builds them (edge, node, area deltas),
#             now with a delta per type, and the areas' DULN per type
#   sim       Full at the 3 sites, 20 paired reps, with per-type tracks
#   score     totals (Glatt on the corrected survey) and, at Glatt, each type's map
#             against its own routes
#
# Variants (all at s = 0.5, the strength that did best before):
#   T_today_typeArea       today's layers; each type chooses areas by its OWN band
#   T_shared_s0.5          the Glatt fit, one weight set, the delta rule (as before)
#   T_types_s0.5           the per-type fit; areas still chosen by the Nature walker band
#   T_types_typeArea_s0.5  the per-type fit, and each type chooses areas by its own
# The per-type area choice is a hook (data-raw only): the corrected engine's logit
# (aoiProbsR) with the agent's own type's area DULN.
#
#   Rscript data-raw/tailor_duln_types.R
#
# VFT_TY_STAGES   which stages (default all)
# VFT_TY_OUT      output (default <duln_tailored>/types)
# VFT_TY_REPS     reps per variant (default 20)
# VFT_TY_WORKERS  daemons (default 13)

IS_TY_WORKER <- identical(Sys.getenv("VFT_TY_WORKER"), "1")
Sys.setenv(VFT_TD_STAGES = "none", VFT_VAL_FILBLEU = "full")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
#tailor_duln.R's definitions (it sources validate_usage_surveys.R; no stage runs)
source(file.path(.repo, "data-raw", "tailor_duln.R"))
TY_SCRIPT <- file.path(.repo, "data-raw", "tailor_duln_types.R")
if(!IS_TY_WORKER){
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if(length(f)) TY_SCRIPT <- normalizePath(f[1], winslash = "/")
}

REF_OUT <- OUT
TY  <- Sys.getenv("VFT_TY_OUT", file.path(TD, "types"))
RS  <- file.path(REF_OUT, "glatt_survey_full")   #data-raw/rescore_glatt_survey.R
TY_STAGES <- envList("VFT_TY_STAGES", c("survey", "capture", "fit", "variants", "sim", "score"))
TY_REPS <- as.integer(envNum("VFT_TY_REPS", 20))
TY_NR   <- 40
TY_NW   <- as.integer(envNum("VFT_TY_WORKERS", 13))
TYPES7  <- c("walkNat", "walkSoc", "dogNat", "dogProx", "ebikeNat", "bikeSport", "jogger")
TYPE_LABEL <- c(walkNat = "Nature walkers", walkSoc = "Social walkers", dogNat = "Nature dog walkers",
                dogProx = "Proximity dog walkers", ebikeNat = "Nature e-bikers", bikeSport = "Sports bikers",
                jogger = "Joggers")
COL_OF  <- ns$VFT_DULN_TYPE_COLS                       #type -> edge/node column
BAND_OF <- stats::setNames(TYPES_TD$band, TYPES_TD$type) #type -> COG band
S_TY    <- 0.5
tyPath <- function(...){ p <- file.path(TY, ...); dir.create(dirname(p[1]), recursive = TRUE, showWarnings = FALSE); p }
#agent type mixes for data-raw/test_type_mix.R (simTyped(mixId =)): the engine's own
#(R/generatePopulation.R) and the Glatt survey's share of routes per type
ENGINE_MIX <- c(walkNat = 0.232, walkSoc = 0.1667, dogNat = 0.1535, dogProx = 0.1124,
                ebikeNat = 0.089, bikeSport = 0.05937, jogger = 0.2328)
TY_MIXES <- local({
  f <- file.path(TY, "survey_types.csv")
  m <- list(engine = ENGINE_MIX)
  if(file.exists(f)){ x <- utils::read.csv(f); m$survey <- stats::setNames(x$routeShare, x$type)[TYPES7] }
  m
})
withOut <- function(dir, expr){ old <- OUT; OUT <<- dir; on.exit(OUT <<- old); force(expr) }

#### the routes, typed ####

IMPORTANCE <- c("unwichtig" = 1, "eher unwichtig" = 2, "weder / noch" = 3, "eher wichtig" = 4, "wichtig" = 5)
typedLines <- function(){
  x <- readSurveyLines("filBleu")
  n <- unname(IMPORTANCE[x$nature]); s <- unname(IMPORTANCE[x$social])
  t <- rep(NA_character_, nrow(x))
  w <- x$layer == "walking"
  t[w] <- ifelse(!is.na(n[w]) & !is.na(s[w]) & s[w] >= n[w], "walkSoc", "walkNat")
  d <- x$layer == "dogWalking"
  t[d] <- ifelse(x$nature[d] %in% "wichtig", "dogNat", "dogProx")
  t[x$layer == "jogging_nordicWalking"] <- "jogger"
  t[x$layer == "bike"] <- "bikeSport"
  t[x$layer == "E_bike"] <- "ebikeNat"
  x$agentType <- t
  x
}

stageSurveyTypes <- function(){
  f <- tyPath("survey_types.rds")
  if(file.exists(f)) return(invisible(readRDS(f)))
  ln <- typedLines()
  sv <- withOut(RS, readRDS(surveyFile("glatt")))
  if(nrow(sv$L) != nrow(ln) || !identical(sv$resp, ln$resp))
    stop("the corrected survey's lines are not the typed lines - rerun data-raw/rescore_glatt_survey.R")
  base <- lapply(stats::setNames(TYPES7, TYPES7), function(k)
    as.vector(Matrix::colSums(sv$L[ln$agentType %in% k, , drop = FALSE])))
  stopifnot(isTRUE(all.equal(Reduce(`+`, base), sv$surveyBase)))
  tab <- data.frame(type = TYPES7, label = TYPE_LABEL[TYPES7],
                    routes = vapply(TYPES7, function(k) sum(ln$agentType %in% k), 1L),
                    respondents = vapply(TYPES7, function(k) length(unique(ln$resp[ln$agentType %in% k])), 1L),
                    km = vapply(TYPES7, function(k) sum(as.numeric(sf::st_length(ln[ln$agentType %in% k, ]))) / 1000, 1),
                    cellVisits = vapply(base, sum, 1))
  tab$routeShare <- tab$routes / sum(tab$routes)
  logf("the Glatt routes by agent type:")
  print(tab, digits = 3, row.names = FALSE)
  res <- list(table = tab, base = base, lakeCells = sv$lakeCells)
  saveRDS(res, f)
  utils::write.csv(tab, tyPath("survey_types.csv"), row.names = FALSE)
  invisible(res)
}

#### the per-type simulation: tracks by agent type, and the per-type area choice ####

.typeCtx <- new.env()
aoiProbsR_orig <- aoiProbsR
#aoiProbsR() (data-raw/engine_corrections.R) with the agent's own type's area DULN:
#the same arithmetic, the DULN range standardised within the type. With every type
#given the same DULN it is aoiProbsR() exactly.
aoiProbsTypeR <- function(res, agentSpeeds, agentDurations, AOI_aois, AOI_dulns, AOI_areas, reach,
                          trueDist = NULL, floorH = 0, fallback = FALSE, fallbackCap = Inf){
  D <- .typeCtx$duln; typ <- .typeCtx$typ
  if(is.null(D) || is.null(typ))
    return(aoiProbsR_orig(res, agentSpeeds, agentDurations, AOI_aois, AOI_dulns, AOI_areas, reach,
                          trueDist, floorH, fallback, fallbackCap))
  D <- D[match(AOI_aois, rownames(D)), , drop = FALSE]
  if(anyNA(D)) stop("the per-type area DULN misses areas the engine has")
  rngMax <- apply(D, 2, max); rngMin <- apply(D, 2, min)
  durMax <- max(agentDurations); durMin <- min(agentDurations)
  lastMatch <- function(x, table){ k <- match(x, rev(table)); ifelse(is.na(k), NA_integer_, length(table) + 1L - k) }
  lapply(seq_along(res$prob), function(i){
    d <- res$distances[[i]]
    if(!length(d)) return(res$prob[[i]])
    ty <- typ[i]
    dulnMax <- rngMax[[ty]]; dulnMin <- rngMin[[ty]]
    speed <- agentSpeeds[i]; dur <- agentDurations[i]
    tdMax <- max(d) / (2 * 1000); tdMin <- min(d) / (20 * 1000)
    td <- d / (speed * 1000)
    td[td < 0.01] <- 0.01
    if(length(AOI_aois) > 1){
      k <- lastMatch(res$aoi[[i]], AOI_aois)
      duln <- D[k, ty]; area <- sqrt(AOI_areas[k]); area[area > 7000] <- 7000
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

#One replicate with the track of every agent type. simCore() runs it (its hooks, its
#seed); this adds a capture that keeps the agents' types, and - typeArea - hands the
#corrected area choice the per-type area DULN (polys DULN_<type>). The area hook's
#agent types come from a recorder around the engine's own findClosestAOI_cpp().
simTyped <- function(ctx, type, rep, vdir, typeArea = FALSE, mixId = NA_character_){
  if(!identical(OUT, vdir)){ OUT <<- vdir; rm(list = ls(.valCache), envir = .valCache) }
  sc <- scenarioFor(ctx, type)
  cap <- new.env(); cap$n <- 0L
  nsSet("historyToPassage_cpp", function(agentTable, edgeTable, vertexTable){
    cap$n <- cap$n + 1L
    cap$priorE <- Map(function(a, b) c(a, b), agentTable$priorE, agentTable$priorEAOI)
    cap$typ <- as.character(agentTable$agentTyp)
    ORIG$historyToPassage_cpp(agentTable = agentTable, edgeTable = edgeTable, vertexTable = vertexTable)
  })
  on.exit(nsSet("historyToPassage_cpp", ORIG$historyToPassage_cpp), add = TRUE)
  if(isTRUE(typeArea)){
    cols <- paste0("DULN_", TYPES7)
    p <- sf::st_drop_geometry(sc$polys)
    if(!all(cols %in% names(p))) stop("variant ", vdir, " has no per-type area DULN")
    D <- as.matrix(p[, cols]); colnames(D) <- TYPES7; rownames(D) <- p$AOI
    .typeCtx$duln <- D
    origFC <- ORIG$findClosestAOI_cpp
    ORIG$findClosestAOI_cpp <<- function(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds, agentDurations,
                                         AOI_aois, AOI_dulns, agentTyps, AOI_areas){
      .typeCtx$typ <- as.character(agentTyps)
      origFC(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds, agentDurations, AOI_aois, AOI_dulns,
             agentTyps, AOI_areas)
    }
    aoiProbsR <<- aoiProbsTypeR
    on.exit({ ORIG$findClosestAOI_cpp <<- origFC; aoiProbsR <<- aoiProbsR_orig
              .typeCtx$duln <- NULL; .typeCtx$typ <- NULL }, add = TRUE)
  }
  #another agent type mix: the engine draws its population as always, then every
  #agent's type is drawn again from the mix (start nodes do not depend on the type)
  if(!is.na(mixId)){
    mix <- TY_MIXES[[mixId]]
    if(is.null(mix)) stop("no agent type mix ", mixId)
    origGP <- ns$generatePopulation
    nsSet("generatePopulation", function(network, parkingIntensity = 1, nAgents = 2000, parkingAttr = NULL){
      pop <- origGP(network, parkingIntensity = parkingIntensity, nAgents = nAgents, parkingAttr = parkingAttr)
      pop$agentTyp <- sample(names(mix), nrow(pop), replace = TRUE, prob = unname(mix))
      pop
    })
    on.exit(nsSet("generatePopulation", origGP), add = TRUE)
  }
  r <- simCore(sc, BASE_SEED + rep, capture = FALSE)
  if(cap$n != 1L) stop("the track capture fired ", cap$n, " times, expected once")
  dp <- r$out$results$dayPop
  tot <- summariseTracks(cap$priorE, sc$ix)
  byType <- lapply(stats::setNames(TYPES7, TYPES7), function(k){
    i <- which(cap$typ == k)
    if(!length(i)) return(list(cells = integer(0), counts = integer(0)))
    t <- summariseTracks(cap$priorE[i], sc$ix); t$visits <- NULL; t
  })
  list(status = "done", ctx = ctx, type = type, rep = rep, secs = r$secs, nAgents = nrow(dp),
       nNoGoal = sum(dp$AOIGoal %in% "NO_GOAL" | dp$goalV %in% -1), typeArea = isTRUE(typeArea), mixId = mixId,
       agentTypes = table(factor(cap$typ, levels = TYPES7)),
       cells = tot$cells, counts = tot$counts, visits = tot$visits, byType = byType)
}

#runs a grid (ctx, type, rep, vdir, typeArea) on daemons; results in grid order
runGrid <- function(g, label){
  mirai::daemons(min(TY_NW, nrow(g)))
  on.exit(mirai::daemons(0), add = TRUE)
  w <- mirai::everywhere({
    .libPaths(libs); Sys.setenv(VFT_VAL_WORKER = "1", VFT_TY_WORKER = "1"); source(script, local = globalenv()); Sys.getpid()
  }, libs = .libPaths(), script = TY_SCRIPT)
  mirai::call_mirai(w)
  t0 <- proc.time()[["elapsed"]]
  ms <- lapply(seq_len(nrow(g)), function(i) mirai::mirai(
    tryCatch(simTyped(ctx, type, rep, vdir, typeArea, mixId),
             error = function(e) list(status = "failed", msg = conditionMessage(e))),
    ctx = g$ctx[i], type = g$type[i], rep = g$rep[i], vdir = g$vdir[i], typeArea = g$typeArea[i],
    mixId = if(is.null(g$mixId)) NA_character_ else g$mixId[i]))
  res <- vector("list", nrow(g))
  for(i in seq_along(ms)){
    r <- mirai::call_mirai(ms[[i]])$data
    if(mirai::is_error_value(r)) r <- list(status = "failed", msg = format(r))
    res[[i]] <- r
    if(i %% 50 == 0) logf("%s: %d / %d done, %.0f min", label, i, nrow(g), (proc.time()[["elapsed"]] - t0) / 60)
  }
  bad <- which(!vapply(res, function(r) identical(r$status, "done"), logical(1)))
  if(length(bad)) logf("%s: %d FAILED, first: %s", label, length(bad), res[[bad[1]]]$msg)
  res
}

#### stage capture: today's Full and Nearby-Random at Glatt, by type ####

stageCapture <- function(){
  f <- tyPath("capture_runs.rds")
  if(file.exists(f)) return(invisible(readRDS(f)))
  #the per-type area hook's check: every type given the Nature walker area DULN
  chk <- tyPath("variants", "T_checkTypeArea")
  for(f0 in c(file.path("network", paste0("glatt", c("_net.rds", "_edges.rds", "_meta.rds"))))){
    dir.create(dirname(file.path(chk, f0)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(REF_OUT, f0), file.path(chk, f0), overwrite = TRUE)
  }
  a <- readRDS(aoiFile("glatt"))
  for(k in TYPES7) a$polys[[paste0("DULN_", k)]] <- a$polys$DULN
  dir.create(file.path(chk, "aoi"), recursive = TRUE, showWarnings = FALSE)
  saveAtomic(a, file.path(chk, "aoi", "glatt.rds"))
  g <- rbind(data.frame(ctx = "glatt", type = "nearbyRandom", rep = seq_len(TY_NR), vdir = REF_OUT, typeArea = FALSE),
             data.frame(ctx = "glatt", type = "full", rep = seq_len(TY_REPS), vdir = REF_OUT, typeArea = FALSE),
             data.frame(ctx = "glatt", type = "full", rep = 1:3, vdir = chk, typeArea = TRUE))
  res <- runGrid(g, "capture")
  saveRDS(list(grid = g, res = res), f)
  #their totals are the stored runs': the capture and the hook change nothing
  same <- vapply(seq_len(nrow(g)), function(i){
    if(!identical(res[[i]]$status, "done")) return(NA)
    s <- runsFrom(g$ctx[i], g$type[i], g$rep[i])[[1]]
    identical(as.integer(s$cells), as.integer(res[[i]]$cells)) && identical(as.integer(s$counts), as.integer(res[[i]]$counts))
  }, logical(1))
  logf("capture: the per-type runs reproduce the stored totals: %d of %d identical", sum(same, na.rm = TRUE), length(same))
  if(!isTRUE(all(same))) stop("the per-type capture changed the simulation")
  invisible(list(grid = g, res = res))
}

#### stage fit: the shared Glatt fit and each type's difference ####

#the per-type maps of a set of runs on the base grid
typeBase <- function(runs, G, k){
  x <- numeric(terra::ncell(G$t100))
  for(r in runs){ b <- r$byType[[k]]; x[b$cells] <- x[b$cells] + b$counts }
  as.vector(G$P %*% x)
}

fitTypes <- function(ex, sty, nrMaps, label){
  els <- ELEMENTS_NEW()
  cells <- ex$cells
  ident <- names(which(colMeans(ex$X[, els] != 0) >= 0.01 & apply(ex$X[, els], 2, stats::sd) > 0))
  Xr <- ex$X[, ident, drop = FALSE]
  mu0 <- colMeans(Xr); sd0 <- apply(Xr, 2, stats::sd)
  Z <- sweep(sweep(Xr, 2, mu0), 2, sd0, "/")
  pz <- 1 / sd0^2; pz <- pz / mean(pz)
  lh <- ex$logHa; lh[!is.finite(lh)] <- mean(lh[is.finite(lh)]); lh <- lh - mean(lh)
  blk <- paste(floor(ex$xy[, 1] / 2000), floor(ex$xy[, 2] / 2000))
  set.seed(11)
  ub <- unique(blk); fold <- stats::setNames(sample(rep_len(1:5, length(ub))), ub)[blk]
  ys <- lapply(TYPES7, function(k) sty$base[[k]][cells])
  offs <- lapply(TYPES7, function(k){ s <- nrMaps[[k]][cells]
    s <- s / mean(s) * mean(sty$base[[k]][cells]); log(pmax(s, 0.5 * min(s[s > 0]))) })
  names(ys) <- names(offs) <- TYPES7
  #the shared fit: every type's cells stacked, an intercept per type, one weight set
  nT <- length(TYPES7); n <- length(cells)
  Dm <- kronecker(diag(nT), matrix(1, n, 1)); colnames(Dm) <- paste0("type_", TYPES7)
  M  <- cbind(Dm, logHa = rep(lh, nT), Z[rep(seq_len(n), nT), , drop = FALSE])
  y  <- unlist(ys); off <- unlist(offs); W <- rep(1, length(y))
  pen <- c(rep(0, nT + 1), pz)
  foldS <- rep(fold, nT)
  cvS <- vapply(LAMBDAS, function(l) mean(vapply(1:5, function(f){
    tr <- foldS != f
    b <- ridgePois(M[tr, ], y[tr], off[tr], W[tr], l, pen)
    poisDev(y[!tr], exp(off[!tr] + drop(M[!tr, ] %*% b)), W[!tr])
  }, 1)), 1)
  lamS <- LAMBDAS[which.min(cvS)]
  bS <- ridgePois(M, y, off, W, lamS, pen); names(bS) <- colnames(M)
  betaS <- bS[ident] / sd0
  logf("types fit %s: shared, %d cells x %d types, %d elements, lambda %.2g, CV deviance %.4f (largest lambda %.4f)",
       label, n, nT, length(ident), lamS, min(cvS), cvS[length(cvS)])
  #per type: its difference from the shared weights, offset by the shared fit
  LAM2 <- 10^seq(-2, 4, by = 0.5)
  dev <- lapply(TYPES7, function(k){
    j <- which(TYPES7 == k)
    o <- offs[[k]] + bS[[paste0("type_", k)]] + bS[["logHa"]] * lh + drop(Z %*% bS[ident])
    Mk <- cbind(int = 1, Z); pk <- c(0, pz)
    yk <- ys[[k]]; Wk <- rep(1, n)
    cv <- vapply(LAM2, function(l) mean(vapply(1:5, function(f){
      tr <- fold != f
      b <- ridgePois(Mk[tr, ], yk[tr], o[tr], Wk[tr], l, pk)
      poisDev(yk[!tr], exp(o[!tr] + drop(Mk[!tr, ] %*% b)), Wk[!tr])
    }, 1)), 1)
    cv0 <- mean(vapply(1:5, function(f){
      tr <- fold != f
      b <- ridgePois(Mk[tr, 1, drop = FALSE], yk[tr], o[tr], Wk[tr], 0, 0)
      poisDev(yk[!tr], exp(o[!tr] + drop(Mk[!tr, 1, drop = FALSE] %*% b)), Wk[!tr])
    }, 1))
    l <- LAM2[which.min(cv)]
    b <- ridgePois(Mk, yk, o, Wk, l, pk); names(b) <- colnames(Mk)
    logf("types fit %s: %-9s lambda %.2g, CV deviance %.4f with its own difference, %.4f at the shared weights",
         label, k, l, min(cv), cv0)
    list(delta = b[ident] / sd0, lambda = l, cv = min(cv), cv0 = cv0, cvCurve = cv)
  })
  names(dev) <- TYPES7
  list(ident = ident, betaShared = betaS, lambdaShared = lamS, cvShared = cvS, dev = dev, label = label)
}

stageFitTypes <- function(){
  f <- tyPath("fit_types.rds")
  if(file.exists(f)) return(invisible(readRDS(f)))
  sty <- stageSurveyTypes()
  cr <- readRDS(tyPath("capture_runs.rds"))
  ex <- readRDS(extractFile("glatt"))
  G <- withOut(RS, buildGrid("glatt", readRDS(aoiFile("glatt")), readRDS(surveyFile("glatt")), readRDS(edgesFile("glatt"))))
  nrRuns <- cr$res[cr$grid$type == "nearbyRandom"]
  nrMaps <- lapply(stats::setNames(TYPES7, TYPES7), function(k) typeBase(nrRuns, G, k))
  #the cells: tailor_duln.R's Glatt cells (no lakes); the corrected survey per type
  all <- fitTypes(ex, sty, nrMaps, "all Glatt cells")
  noGP <- fitTypes(dropGround(ex, "glattPark"), sty, nrMaps, "without GlattPark's ground")
  res <- list(all = all, noGP = noGP, nrMaps = nrMaps)
  saveRDS(res, f)
  invisible(res)
}

#### weights: shared and per type, in DULN units ####

xlsxNew <- function(k){
  w <- xlsxWeights(); v <- stats::setNames(w[[k]], w$element)
  c(v[setdiff(names(v), "lakesAndBigRivers")], lakeNear = v[["lakesAndBigRivers"]], lakeBand = v[["lakesAndBigRivers"]])
}
xlsxOld <- function(k){ w <- xlsxWeights(); stats::setNames(w[[k]], w$element) }

#The weights per type (NEW element set) for one fit, at strength 1. The shared
#pattern is scaled to today's Nature walker spread on Glatt's edges (as tailor_duln.R
#scales a fit); every type's difference rides on the same scale, so the measured
#differences keep their size. Unidentified elements keep the type's xlsx weight.
typeWeights <- function(ft, ex){
  fit <- readRDS(tdPath("fit.rds"))
  els <- ELEMENTS_NEW()
  sc <- scaleFit(list(ident = ft$ident, beta = ft$betaShared), "glatt", list(glatt = ex),
                 fit$wOld, fit$wOldNew, els)
  a <- sc$a
  shared <- sc$w
  perType <- lapply(stats::setNames(TYPES7, TYPES7), function(k){
    w <- xlsxNew(k)
    w[ft$ident] <- a * (ft$betaShared[ft$ident] + ft$dev[[k]]$delta[ft$ident])
    w
  })
  list(a = a, shared = shared, perType = perType)
}

#### stage variants ####

TY_VARIANTS <- list(
  T_today_typeArea      = list(kind = "today", typeArea = TRUE),
  T_shared_s0.5         = list(kind = "shared", typeArea = FALSE),
  T_types_s0.5          = list(kind = "types", typeArea = FALSE),
  T_types_typeArea_s0.5 = list(kind = "types", typeArea = TRUE))
tyVariantDir <- function(v) tyPath("variants", v)

#the change of one type's layer: new weights (NEW set) minus its xlsx weights (ORIG set)
typeDelta <- function(wNew, k){
  wo <- xlsxOld(k)
  els <- union(names(wo), names(wNew))
  d <- stats::setNames(numeric(length(els)), els)
  d[names(wNew)] <- wNew
  d[names(wo)] <- d[names(wo)] - wo
  d
}
siteRaster <- function(ctx, d, b0){
  X <- terra::rast(layersFile(BOX_OF[[ctx]]))
  sl <- sf::st_bbox(sf::st_transform(readRDS(aoiFile(ctx))$shapeLarger, 2056))
  X <- terra::crop(X, terra::ext(sl[["xmin"]] - 1500, sl[["xmax"]] + 1500, sl[["ymin"]] - 1500, sl[["ymax"]] + 1500))
  d <- d[abs(d) > 1e-12]
  if(!length(d)) return(X[[1]] * 0 + b0)
  weighted(X, d) + b0
}

stageVariantsTypes <- function(){
  ft <- readRDS(tyPath("fit_types.rds"))
  log <- list()
  for(ctx in CONTEXTS){
    ex <- readRDS(extractFile(ctx))
    aoi <- readRDS(aoiFile(ctx)); base <- readRDS(netFile(ctx))
    fitUse <- if(ctx == "glattPark") ft$noGP else ft$all
    #weights from Glatt's cells (GlattPark: without its own ground); scaled on Glatt's edges
    tw <- typeWeights(fitUse, readRDS(extractFile("glatt")))
    tm <- cogTemplate(ctx)
    lakes <- ns$.vftReadLakesWithin(sf::st_transform(aoi$shapeLarger, 2056))
    lakeV <- if(nrow(lakes)) terra::vect(sf::st_transform(lakes, 4326)) else NULL
    cog <- terra::crop(terra::rast(COG), tm)
    bandNoLakes <- function(k){ b <- cog[[BAND_OF[[k]]]]; if(!is.null(lakeV)) b[lakeV] <- NA; b }
    today <- lapply(stats::setNames(TYPES7, TYPES7), function(k) areaSummary(bandNoLakes(k), aoi$polys))
    #today's per-type area DULN, anchored on the saved (Nature walker) value
    todayType <- lapply(TYPES7, function(k) aoi$polys$DULN + today[[k]] - today$walkNat)
    names(todayType) <- TYPES7
    va <- igraph::vertex_attr(base$net)
    pk <- which(va$parkingAttr != 0 & is.finite(va$parkingAttr))
    pkPoly <- match(va$parkingAttr[pk], aoi$polys$DULN)
    if(anyNA(pkPoly)) stop(ctx, ": car-park nodes without their area")
    for(v in names(TY_VARIANTS)){
      vd <- tyVariantDir(v)
      fNet <- file.path(vd, "network", paste0(ctx, "_net.rds"))
      if(file.exists(fNet)) next
      t0 <- proc.time()[["elapsed"]]
      sp <- TY_VARIANTS[[v]]
      net <- base$net; polys <- aoi$polys
      for(k in TYPES7) polys[[paste0("DULN_", k)]] <- todayType[[k]]
      #the two per-type variants share one network: built once, then copied
      same <- Filter(function(u) u != v && identical(TY_VARIANTS[[u]]$kind, sp$kind) &&
                       file.exists(file.path(tyVariantDir(u), "network", paste0(ctx, "_net.rds"))), names(TY_VARIANTS))
      if(length(same) && !identical(sp$kind, "today")){
        for(f in c(file.path("aoi", paste0(ctx, ".rds")), file.path("network", paste0(ctx, c("_net.rds", "_edges.rds", "_meta.rds"))))){
          dir.create(dirname(file.path(vd, f)), recursive = TRUE, showWarnings = FALSE)
          file.copy(file.path(tyVariantDir(same[1]), f), file.path(vd, f), overwrite = TRUE)
        }
        logf("variants %s %s: copied from %s", ctx, v, same[1])
        next
      }
      if(!identical(sp$kind, "today")){
        ea <- igraph::edge_attr(net); vv <- igraph::vertex_attr(net)
        stopifnot(identical(ea$edgeID, ex$edgeID))
        dA <- list()
        for(k in TYPES7){
          #shared: the Nature walker layer becomes half the shared fit, and every type
          #takes that same change (the delta rule, as the earlier s0.5 variants);
          #types: each type's own fitted layer, halved
          wNew <- if(identical(sp$kind, "shared")) xlsxNew(k) + S_TY * tw$shared - xlsxNew("walkNat") else S_TY * tw$perType[[k]]
          d <- typeDelta(wNew, k)
          old <- drop(ex$E[, names(xlsxOld(k))] %*% xlsxOld(k))
          b0 <- mean(old) - mean(old + drop(ex$E[, names(d)] %*% d))
          dE <- drop(ex$E[, names(d)] %*% d) + b0
          col <- COL_OF[[k]]
          ea[[col]] <- ea[[col]] + dE
          dN <- siteRaster(ctx, d, b0)
          d4 <- to4326(dN, tm)
          dV <- terra::extract(d4, cbind(va$X, va$Y)); dV <- dV[[ncol(dV)]]; dV[!is.finite(dV)] <- 0
          vv[[col]] <- vv[[col]] + dV
          bk <- bandNoLakes(k)
          a1 <- areaSummary(bk + d4, polys) - today[[k]]
          a1[!is.finite(a1)] <- 0
          dA[[k]] <- a1 - mean(a1)
          log[[length(log) + 1]] <- data.frame(variant = v, ctx = ctx, type = k, b0 = b0, dEdge_sd = stats::sd(dE),
                                               dArea_sd = stats::sd(dA[[k]]), r_edge_today = stats::cor(ea[[col]], ea[[col]] - dE))
        }
        for(k in TYPES7) polys[[paste0("DULN_", k)]] <- todayType[[k]] + dA[[k]]
        polys$DULN <- polys$DULN_walkNat
        vv$parkingAttr[pk] <- pmax(polys$DULN[pkPoly], 0)
        cls <- class(net)
        igraph::edge_attr(net) <- ea; igraph::vertex_attr(net) <- vv
        class(net) <- cls
        net <- ns$vftRefreshAttrWeights(net, aoi$thr)
      }
      a2 <- aoi; a2$polys <- polys
      dir.create(file.path(vd, "aoi"), recursive = TRUE, showWarnings = FALSE)
      dir.create(file.path(vd, "network"), recursive = TRUE, showWarnings = FALSE)
      saveAtomic(a2, file.path(vd, "aoi", paste0(ctx, ".rds")))
      file.copy(edgesFile(ctx), file.path(vd, "network", basename(edgesFile(ctx))), overwrite = TRUE)
      file.copy(metaFile(ctx), file.path(vd, "network", basename(metaFile(ctx))), overwrite = TRUE)
      if(identical(sp$kind, "today")) file.copy(netFile(ctx), fNet, overwrite = TRUE) else
        saveAtomic(list(net = net, parking = base$parking), fNet)
      logf("variants %s %s: written, %.0f s", ctx, v, proc.time()[["elapsed"]] - t0)
    }
  }
  if(length(log)) utils::write.csv(do.call(rbind, log), tyPath("variants.csv"), row.names = FALSE)
  #the weights, every type, as a table
  wt <- do.call(rbind, lapply(c("all", "noGP"), function(fu){
    tw <- typeWeights(ft[[fu]], readRDS(extractFile("glatt")))
    do.call(rbind, lapply(TYPES7, function(k) data.frame(fit = fu, type = k, label = TYPE_LABEL[[k]],
      element = names(xlsxNew(k)), xlsx = unname(xlsxNew(k)), fitted_s1 = unname(tw$perType[[k]][names(xlsxNew(k))]),
      shared_s1 = unname(tw$shared[names(xlsxNew(k))]), identified = names(xlsxNew(k)) %in% ft[[fu]]$ident)))
  }))
  utils::write.csv(wt, tyPath("type_weights.csv"), row.names = FALSE)
}

#### stage sim ####

stageSimTypes <- function(){
  f <- tyPath("typed_runs.rds")
  g <- do.call(rbind, lapply(names(TY_VARIANTS), function(v)
    expand.grid(rep = seq_len(TY_REPS), ctx = CONTEXTS, variant = v, stringsAsFactors = FALSE)))
  g$type <- "full"; g$vdir <- tyVariantDir(g$variant); g$typeArea <- vapply(g$variant, function(v) TY_VARIANTS[[v]]$typeArea, TRUE)
  g <- g[order(match(g$ctx, c("glatt", "glattPark", "wigger")), g$variant, g$rep), ]
  res <- vector("list", nrow(g)); todo <- seq_len(nrow(g))
  if(file.exists(f)){
    old <- readRDS(f); m <- match(paste(g$ctx, g$variant, g$rep), paste(old$grid$ctx, old$grid$variant, old$grid$rep))
    res[!is.na(m)] <- old$res[m[!is.na(m)]]
    todo <- which(!vapply(res, function(r) identical(r$status, "done"), logical(1)))
  }
  logf("sim: %d runs, %d to do", nrow(g), length(todo))
  if(length(todo)) res[todo] <- runGrid(g[todo, ], "sim")
  saveRDS(list(grid = g, res = res), f)
  invisible(list(grid = g, res = res))
}

#### stage score ####

stageScoreTypes <- function(){
  sim <- readRDS(tyPath("typed_runs.rds")); cr <- readRDS(tyPath("capture_runs.rds"))
  sty <- stageSurveyTypes()
  g <- sim$grid; res <- sim$res
  B <- 1000; set.seed(1); idx <- matrix(sample.int(TY_REPS, TY_REPS * B, replace = TRUE), B)
  scorers <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx)
    if(ctx == "glatt") withOut(RS, scorerFor(ctx)) else scorerFor(ctx))
  score <- function(ctx, rr) mean(scorers[[ctx]](rr))
  boot <- function(ctx, rr) vapply(seq_len(B), function(b) score(ctx, rr[idx[b, ]]), 1)
  stored <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx)
    lapply(stats::setNames(c("full", "nearby", "nearbyRandom"), c("full", "nearby", "nearbyRandom")),
           function(t) runsFrom(ctx, t, seq_len(TY_REPS))))
  refB <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx)
    lapply(stored[[ctx]], function(rr) boot(ctx, rr)))
  tot <- list()
  for(ctx in CONTEXTS) for(v in names(TY_VARIANTS)){
    i <- which(g$ctx == ctx & g$variant == v)
    rr <- res[i][order(g$rep[i])]
    if(!length(i) || !all(vapply(rr, function(r) identical(r$status, "done"), logical(1)))) next
    bt <- boot(ctx, rr)
    best <- vapply(c("nearby", "nearbyRandom"), function(t) score(ctx, stored[[ctx]][[t]]), 1)
    bT <- names(best)[which.max(best)]
    dS <- bt - refB[[ctx]]$full; dB <- bt - refB[[ctx]][[bT]]
    tot[[length(tot) + 1]] <- data.frame(ctx = ctx, variant = v, score = score(ctx, rr),
      todaysFull = score(ctx, stored[[ctx]]$full), dSelf = score(ctx, rr) - score(ctx, stored[[ctx]]$full),
      dSelf_lo = unname(stats::quantile(dS, .025)), dSelf_hi = unname(stats::quantile(dS, .975)),
      vsBest = TYPES[[bT]]$label, best = best[[bT]], dBest = score(ctx, rr) - best[[bT]],
      dBest_lo = unname(stats::quantile(dB, .025)), dBest_hi = unname(stats::quantile(dB, .975)))
  }
  tot <- rbindFill(tot)
  utils::write.csv(tot, tyPath("totals.csv"), row.names = FALSE)
  options(width = 220)
  logf("totals (Glatt on the corrected survey):")
  print(tot, digits = 3, row.names = FALSE)

  #per type at Glatt: each simulated type's map against its own routes
  G <- withOut(RS, buildGrid("glatt", readRDS(aoiFile("glatt")), readRDS(surveyFile("glatt")), readRDS(edgesFile("glatt"))))
  cfg <- CTX$glatt
  vn <- HEAD_VIEWS("glatt")
  surT <- lapply(stats::setNames(TYPES7, TYPES7), function(k)
    lapply(stats::setNames(vn, vn), function(v) surveyView(G, G$views[[v]], sty$base[[k]])))
  scoreT <- function(rr, k){
    x <- numeric(terra::ncell(G$t100))
    for(r in rr){ b <- r$byType[[k]]; x[b$cells] <- x[b$cells] + b$counts }
    mean(vapply(vn, function(v) stats::cor(trans(as.vector(G$views[[v]]$Msim %*% x), cfg$simTransform), surT[[k]][[v]]), 1))
  }
  todayFull <- cr$res[cr$grid$type == "full" & !cr$grid$typeArea]
  nr20 <- cr$res[cr$grid$type == "nearbyRandom"][seq_len(TY_REPS)]
  pt <- list()
  for(k in TYPES7){
    bF <- vapply(seq_len(B), function(b) scoreT(todayFull[idx[b, ]], k), 1)
    pt[[length(pt) + 1]] <- data.frame(type = k, label = TYPE_LABEL[[k]], variant = "today (Full)", score = scoreT(todayFull, k),
                                       dToday = 0, lo = 0, hi = 0)
    pt[[length(pt) + 1]] <- data.frame(type = k, label = TYPE_LABEL[[k]], variant = "Nearby-Random", score = scoreT(nr20, k),
                                       dToday = scoreT(nr20, k) - scoreT(todayFull, k), lo = NA, hi = NA)
    for(v in names(TY_VARIANTS)){
      i <- which(g$ctx == "glatt" & g$variant == v)
      rr <- res[i][order(g$rep[i])]
      if(!length(i) || !all(vapply(rr, function(r) identical(r$status, "done"), logical(1)))) next
      bV <- vapply(seq_len(B), function(b) scoreT(rr[idx[b, ]], k), 1)
      d <- bV - bF
      pt[[length(pt) + 1]] <- data.frame(type = k, label = TYPE_LABEL[[k]], variant = v, score = scoreT(rr, k),
                                         dToday = scoreT(rr, k) - scoreT(todayFull, k),
                                         lo = unname(stats::quantile(d, .025)), hi = unname(stats::quantile(d, .975)))
    }
  }
  pt <- do.call(rbind, pt)
  utils::write.csv(pt, tyPath("per_type_glatt.csv"), row.names = FALSE)
  logf("per type at Glatt (mean Pearson 100 + 500 m of the type's map against its own routes):")
  print(stats::reshape(pt[, c("type", "variant", "score")], idvar = "type", timevar = "variant", direction = "wide"),
        digits = 3, row.names = FALSE)
  #the type mix: the engine's agents against the survey's routes
  mix <- data.frame(type = TYPES7, label = TYPE_LABEL[TYPES7],
                    surveyRoutes = sty$table$routeShare,
                    engineAgents = as.numeric(Reduce(`+`, lapply(todayFull, `[[`, "agentTypes"))) /
                      sum(Reduce(`+`, lapply(todayFull, `[[`, "agentTypes"))))
  utils::write.csv(mix, tyPath("type_mix.csv"), row.names = FALSE)
  print(mix, digits = 3, row.names = FALSE)
}

#### run ####

if(!IS_TY_WORKER){
  t00 <- proc.time()[["elapsed"]]
  if("survey" %in% TY_STAGES) stageSurveyTypes()
  if("capture" %in% TY_STAGES) stageCapture()
  if("fit" %in% TY_STAGES) stageFitTypes()
  if("variants" %in% TY_STAGES) stageVariantsTypes()
  if("sim" %in% TY_STAGES) stageSimTypes()
  if("score" %in% TY_STAGES) stageScoreTypes()
  logf("tailor_duln_types: done in %.1f min, output in %s", (proc.time()[["elapsed"]] - t00) / 60, TY)
}
