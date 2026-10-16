#### Sweep: the straight-on preference x the reach limit of the area choice ####
#
# The validation's Full type (respondents' homes, its agent count and car-park
# share) re-run with two engine constants changed:
#
#   straightness  chooseBestRoutes_cpp() adds +1 to a next node that keeps the
#                 heading within 45 degrees and -2 past 90 (src/CPP_FUNCTIONS.cpp
#                 ~303). Scaled by s: none (0), weak (0.5), strong (1 = the engine).
#   reach         findClosestAOI_cpp() gives an area probability 0 when getting
#                 there takes more than 10 % of the outing (~2096). Swept over
#                 10 % (the engine), 25 % and 33 %.
#   true reach    the engine measures that distance on the attractivity-weighted
#                 lengths (SHAPE_Leng_<type>_attr, ~0.72 of the real length on
#                 Glatt), so a third of the agents went to areas beyond 10 % in
#                 metres. The "p" combinations (r10p) apply the limit to the REAL
#                 path length to the area's nearest node instead; the choice among
#                 the areas that pass still uses the engine's weighted distances.
#   floor and     "r10pf": as r10p, but the limit is never below 5 minutes (10 % of a
#   fallback      30-minute outing is 3), and an agent with still no area in reach
#                 picks uniformly among its closest areas: those within 1 minute
#                 (at its speed, real distance) of the nearest one. Without this
#                 the agent stays home, as in the engine.
#   capped        "r10pfc": as r10pf, but the fallback only applies when the
#                 nearest area is within 33 % of the outing; beyond that the
#                 agent stays home.
#
# No package code changes. Each changed constant is a hook in the namespace, as in
# validate_usage_surveys.R, that recomputes the engine's own result in R with the
# one constant replaced:
#   - the area choice: findClosestAOI_cpp() still finds the areas and distances;
#     the hook redoes its multinomial logit (same standardisation, same 3.23695 /
#     -6.65703, same 0.01 h floor) with the new reach;
#   - the route choice: chooseBestRoutes_cpp() is redone in R with the turn term
#     times s. The C++ works on lon/lat in single precision; that is emulated.
# The unchanged value (s = 1, reach 10 %) is never hooked: those runs ARE the
# engine. The verify stage checks that both R versions, at the engine's values,
# give the engine's probabilities.
#
#   VFT_VAL_OUT=".../pathUsageValidation_2026_userAoi" VFT_VAL_CONTEXTS=glatt \
#   VFT_SWEEP_REPS=10 VFT_SWEEP_WORKERS=3 Rscript data-raw/sweep_straight_reach.R
#
#   alpha         the scale of the route choice's random utility, exp(value / alpha):
#                 a05 (0.5, the engine), a2, a5. Larger = less deterministic.
#
# A combination id is straight_reach_alpha, e.g. strong_r10_a05 (the engine) or
# none_r10pfc_a2. VFT_SWEEP_COMBOS picks them (default: straight none/strong x reach
# r10/r10pfc x alpha 0.5/2/5, 12 combinations).
#
# Stages (VFT_SWEEP_STAGES): verify, sim, compare. Runs are saved as they finish
# under VFT_VAL_OUT/<VFT_SWEEP_DIR, default sweep2>/runs, the same seeds in every
# combination (paired). sweep2 started 2026-10-13 with the full tracks (approach +
# in-area walking, TRACK_VERSION 2); the runs in sweep/ have the approach only.
# compare scores every combination with agreementScores() of the validation script
# (r, hit, AUC, Boyce, contradicted presence, concentration) against the survey's
# own half-vs-half level.

Sys.setenv(VFT_VAL_WORKER = "1")   #definitions only from the validation script
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "validate_usage_surveys.R"))

IS_SWEEP_WORKER <- identical(Sys.getenv("VFT_SWEEP_WORKER"), "1")
STRAIGHT <- c(none = 0, weak = 0.5, strong = 1)
REACH    <- c(r10 = 0.10, r25 = 0.25, r33 = 0.33, r10p = 0.10, r10pf = 0.10, r10pfc = 0.10)
REACH_TRUE <- c("r10p", "r10pf", "r10pfc")   #measured on the real path length
REACH_FLOOR_FALLBACK <- c("r10pf", "r10pfc") #5-minute floor, closest-area fallback
FALLBACK_CAP <- c(r10pfc = 0.33)             #no fallback beyond this share of the outing
ALPHA    <- c(a05 = 0.5, a2 = 2, a5 = 5)
ENGINE   <- list(straight = 1, reach = 0.10, alpha = 0.5)
SWEEP_DIR <- Sys.getenv("VFT_SWEEP_DIR", "sweep2")
SWEEP_REPS    <- as.integer(envNum("VFT_SWEEP_REPS", 10))
SWEEP_WORKERS <- as.integer(envNum("VFT_SWEEP_WORKERS", 1))
SWEEP_STAGES  <- envList("VFT_SWEEP_STAGES", c("verify", "sim", "compare"))
COMBOS <- expand.grid(straight = names(STRAIGHT), reach = names(REACH), alpha = names(ALPHA),
                      stringsAsFactors = FALSE)
COMBOS$id <- paste(COMBOS$straight, COMBOS$reach, COMBOS$alpha, sep = "_")
GRID_IDS <- COMBOS$id[COMBOS$straight %in% c("none", "strong") & COMBOS$reach %in% c("r10", "r10pfc")]
COMBOS <- COMBOS[COMBOS$id %in% envList("VFT_SWEEP_COMBOS", GRID_IDS), ]
if(!nrow(COMBOS)) stop("VFT_SWEEP_COMBOS matches no combination")
if(!IS_SWEEP_WORKER) SWEEP_SCRIPT <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1]),
                                                   winslash = "/")
sweepFile <- function(ctx, stamp, id, rep)
  outPath(SWEEP_DIR, "runs", paste0(ctx, "_", stamp), id, sprintf("rep_%03d.rds", rep))
runDone <- function(f){
  if(!file.exists(f)) return(FALSE)
  x <- readRDS(f)
  !is.null(x$realReachShare) && identical(x$trackVersion, TRACK_VERSION)
}

#### hooks ####

installHooks <- function(straight, reach, verify = NULL, trueD = NULL, floorH = 0, fallback = FALSE,
                         fallbackCap = Inf, alpha = ENGINE$alpha){
  restore <- character(0)
  if(!identical(reach, ENGINE$reach) || !is.null(verify) || !is.null(trueD)){
    nsSet("findClosestAOI_cpp", function(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds, agentDurations,
                                         AOI_aois, AOI_dulns, agentTyps, AOI_areas){
      res <- ORIG$findClosestAOI_cpp(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds, agentDurations,
                                     AOI_aois, AOI_dulns, agentTyps, AOI_areas)
      td <- if(is.null(trueD)) NULL else
        lapply(seq_along(res$prob), function(i) trueD[cbind(rep(src_v[i], length(res$aoi[[i]])),
                                                             match(res$aoi[[i]], colnames(trueD)))])
      p <- aoiProbsR(res, agentSpeeds, agentDurations, AOI_aois, AOI_dulns, AOI_areas, reach, td,
                     floorH = floorH, fallback = fallback, fallbackCap = fallbackCap)
      if(!is.null(verify)){
        dd <- mapply(function(a, b) if(length(a) == length(b)) max(c(0, abs(a - b))) else Inf, p, res$prob)
        verify$aoiMax <- max(verify$aoiMax, dd); verify$aoiAgents <- verify$aoiAgents + length(dd)
        return(res)
      }
      res$prob <- p
      res
    })
    restore <- c(restore, "findClosestAOI_cpp")
  }
  if(!identical(straight, ENGINE$straight) || !identical(alpha, ENGINE$alpha) || !is.null(verify)){
    nsSet("chooseBestRoutes_cpp", function(viableRoutes, DULN_df, priorVs, agentTyps, V_coords_df,
                                           currentVs, nodeRow_ptr){
      p <- chooseRoutesR(viableRoutes, DULN_df, priorVs, agentTyps, V_coords_df, currentVs, straight,
                         alpha = alpha)
      if(!is.null(verify)){
        o  <- ORIG$chooseBestRoutes_cpp(viableRoutes, DULN_df, priorVs, agentTyps, V_coords_df,
                                        currentVs, nodeRow_ptr)
        dd <- mapply(function(a, b) max(abs(a - b)), p, o)
        verify$routeMax <- max(verify$routeMax, dd); verify$routeCalls <- verify$routeCalls + 1L
        verify$routeAgents <- verify$routeAgents + length(dd); verify$routeOff <- verify$routeOff + sum(dd > 1e-6)
        return(o)
      }
      p
    })
    restore <- c(restore, "chooseBestRoutes_cpp")
  }
  function() for(nm in restore) nsSet(nm, ORIG[[nm]])
}

#### one run ####

sweepTask <- function(ctx, id, rep){
  cb <- COMBOS[COMBOS$id == id, ]
  sc <- scenarioFor(ctx, "full")
  f  <- sweepFile(ctx, sc$stamp, id, rep)
  if(runDone(f)) return(list(status = "skipped"))
  trueD <- trueAreaDistances(ctx, sc$net)
  ff <- cb$reach %in% REACH_FLOOR_FALLBACK
  .corrStats$fallback <- 0L
  undo <- installHooks(STRAIGHT[[cb$straight]], REACH[[cb$reach]],
                       trueD = if(cb$reach %in% REACH_TRUE) trueD else NULL,
                       floorH = if(ff) FLOOR_H else 0, fallback = ff,
                       fallbackCap = if(cb$reach %in% names(FALLBACK_CAP)) FALLBACK_CAP[[cb$reach]] else Inf,
                       alpha = ALPHA[[cb$alpha]])
  on.exit(undo(), add = TRUE)
  #the agents' start nodes as the walk to the area starts (the first route search);
  #dayPop's own columns are rewritten by the end of the run
  first <- NULL
  fsr <- ns$findShortestRoute_cpp
  nsSet("findShortestRoute_cpp", function(...){ a <- list(...); if(is.null(first)) first <<- a; fsr(...) })
  on.exit(nsSet("findShortestRoute_cpp", fsr), add = TRUE)
  r  <- simCore(sc, BASE_SEED + rep)
  nsSet("findShortestRoute_cpp", fsr)
  dp <- r$out$results$dayPop
  #how far, in real metres, each agent's chosen area is, against its outing
  hasG <- !(dp$AOIGoal %in% "NO_GOAL")
  if(length(first$src_v) != sum(hasG)) stop("start nodes and agents with an area do not line up")
  realD <- trueD[cbind(first$src_v, match(dp$AOIGoal[hasG], colnames(trueD)))]
  realShare <- (realD / (dp$speed[hasG] * 1000)) / (dp$timeExt[hasG] / 60)
  tr <- summariseTracks(r$cap$priorE, sc$ix)
  res <- list(ctx = ctx, id = id, straight = cb$straight, reach = cb$reach, alpha = cb$alpha, rep = rep,
              trackVersion = TRACK_VERSION,
              seed = BASE_SEED + rep, stamp = sc$stamp, git = GIT, secs = r$secs, nAgents = nrow(dp),
              nNoGoal = sum(dp$AOIGoal %in% "NO_GOAL" | dp$goalV %in% -1),
              meanDistance = mean(dp$totalDistance, na.rm = TRUE),
              trackEdges = mean(lengths(r$cap$priorE)),
              realReachShare = realShare, nFallback = .corrStats$fallback,
              goals = table(dp$AOIGoal), cells = tr$cells, counts = tr$counts, visits = tr$visits,
              passage = as.integer(round(igraph::edge_attr(r$out$results$pathUsage, "passage"))))
  saveAtomic(res, f)
  list(status = "done", secs = r$secs, nAgents = res$nAgents, nNoGoal = res$nNoGoal)
}

#### stages ####

stageVerify <- function(ctx){
  f <- outPath(SWEEP_DIR, paste0("verify_", ctx, ".rds"))
  if(file.exists(f)){ v <- readRDS(f); logf("verify %s: done before (aoi max %.2g, route max %.2g, %d of %d route choices off)",
                                            ctx, v$aoiMax, v$routeMax, v$routeOff, v$routeAgents); return(invisible(v)) }
  sc <- scenarioFor(ctx, "full")
  v <- new.env(); v$aoiMax <- 0; v$aoiAgents <- 0L; v$routeMax <- 0; v$routeCalls <- 0L
  v$routeAgents <- 0L; v$routeOff <- 0L
  undo <- installHooks(ENGINE$straight, ENGINE$reach, verify = v)
  on.exit(undo(), add = TRUE)
  r <- simCore(sc, BASE_SEED + 1L)
  out <- as.list(v)
  saveAtomic(out, f)
  logf("verify %s: area choice max |R - C++| = %.2g over %d agents; route choice max %.2g, %d of %d choices off by > 1e-6 (%d calls)",
       ctx, out$aoiMax, out$aoiAgents, out$routeMax, out$routeOff, out$routeAgents, out$routeCalls)
  if(out$aoiMax > 1e-9) stop("the area choice in R does not reproduce the engine")
  if(out$routeOff > 0.001 * out$routeAgents) stop("the route choice in R does not reproduce the engine")
  invisible(out)
}

stageSweepSim <- function(){
  g <- expand.grid(rep = seq_len(SWEEP_REPS), id = COMBOS$id, ctx = CONTEXTS, stringsAsFactors = FALSE)
  #every combination's replicate k before any k + 1: an early stop leaves a balanced set
  g <- g[order(match(g$ctx, CONTEXTS), g$rep, match(g$id, COMBOS$id)), ]
  stamps <- vapply(CONTEXTS, function(cx) readRDS(aoiFile(cx))$stamp, "")
  g <- g[!vapply(mapply(sweepFile, g$ctx, stamps[g$ctx], g$id, g$rep), runDone, logical(1)), ]
  if(!nrow(g)){ logf("sweep: nothing to do"); return(invisible(NULL)) }
  logf("sweep: %d run(s) on %d worker(s)", nrow(g), SWEEP_WORKERS)
  secs <- numeric(0)
  report <- function(k, res){
    lbl <- sprintf("[%d/%d] %s %s rep %d", k, nrow(g), g$ctx[k], g$id[k], g$rep[k])
    if(identical(res$status, "done")){
      secs <<- c(secs, res$secs)
      logf("%s: %.0f s, %d agents (%d without an area), ETA %s", lbl, res$secs, res$nAgents, res$nNoGoal,
           fmtDur(mean(utils::tail(secs, 20)) * (nrow(g) - k) / max(1, SWEEP_WORKERS)))
    }else logf("%s: %s", lbl, if(is.null(res$msg)) res$status else paste("FAILED -", res$msg))
  }
  if(SWEEP_WORKERS <= 1){
    for(k in seq_len(nrow(g)))
      report(k, tryCatch(sweepTask(g$ctx[k], g$id[k], g$rep[k]),
                         error = function(e) list(status = "failed", msg = conditionMessage(e))))
  }else{
    mirai::daemons(SWEEP_WORKERS)
    on.exit(mirai::daemons(0), add = TRUE)
    w <- mirai::everywhere({
      .libPaths(libs)
      Sys.setenv(VFT_VAL_WORKER = "1", VFT_SWEEP_WORKER = "1")
      source(script, local = globalenv())
      Sys.getpid()
    }, libs = .libPaths(), script = SWEEP_SCRIPT)
    mirai::call_mirai(w)
    ms <- lapply(seq_len(nrow(g)), function(k)
      mirai::mirai(tryCatch(sweepTask(ctx, id, rep),
                            error = function(e) list(status = "failed", msg = conditionMessage(e))),
                   ctx = g$ctx[k], id = g$id[k], rep = g$rep[k]))
    for(k in seq_along(ms)){
      res <- mirai::call_mirai(ms[[k]])$data
      if(mirai::is_error_value(res)) res <- list(status = "failed", msg = format(res))
      report(k, res)
    }
  }
}

LAB_S <- c(none = "no straight-on", weak = "weak straight-on", strong = "straight-on (engine)")
LAB_R <- c(r10 = "reach 10 % (engine)", r25 = "reach 25 %", r33 = "reach 33 %", r10p = "reach 10 % real",
           r10pf = "10 % real + fallback", r10pfc = "10 % real + capped fallback")
LAB_A <- c(a05 = "alpha 0.5 (engine)", a2 = "alpha 2", a5 = "alpha 5")
comboLabel <- function(id){
  cb <- COMBOS[match(id, COMBOS$id), ]
  paste(LAB_S[cb$straight], LAB_R[cb$reach], LAB_A[cb$alpha], sep = " | ")
}

stageSweepCompare <- function(ctx){
  suppressPackageStartupMessages(library(ggplot2))
  cfg <- CTX[[ctx]]
  aoi <- readRDS(aoiFile(ctx)); sv <- readRDS(surveyFile(ctx)); edg <- readRDS(edgesFile(ctx))
  G <- buildGrid(ctx, aoi, sv, edg)
  n100 <- terra::ncell(G$t100)
  reps <- Reduce(intersect, lapply(COMBOS$id, function(id){
    k <- seq_len(SWEEP_REPS); k[vapply(sweepFile(ctx, aoi$stamp, id, k), runDone, logical(1))]
  }))
  if(!length(reps)){ logf("compare %s: no complete replicate", ctx); return(NULL) }
  vPub  <- paste0(cfg$oldScale, "_all")
  views <- unique(c("1_all", vPub))
  surV  <- lapply(stats::setNames(views, views), function(vn) surveyView(G, G$views[[vn]], sv$surveyBase))
  hasL  <- !is.null(sv$L)
  nResp <- if(hasL) length(unique(sv$resp)) else NA_real_
  rows <- list(); maps <- list()
  for(i in seq_len(nrow(COMBOS))){
    id <- COMBOS$id[i]
    runs <- lapply(reps, function(k) readRDS(sweepFile(ctx, aoi$stamp, id, k)))
    tot <- numeric(n100)
    for(r in runs) tot[r$cells] <- tot[r$cells] + r$counts
    walks <- sum(vapply(runs, function(r) as.numeric(r$nAgents - r$nNoGoal), 1))
    for(vn in views){
      v   <- G$views[[vn]]
      sim <- trans(as.vector(v$Msim %*% tot), cfg$simTransform)
      pO  <- if(vn == "1_all" && hasL && identical(cfg$simTransform, "none")) as.vector(v$Msim %*% tot) / walks else NULL
      sc  <- agreementScores(sim, surV[[vn]], pO, nResp)
      rows[[length(rows) + 1]] <- data.frame(context = ctx, combo = id, straight = COMBOS$straight[i],
        reach = COMBOS$reach[i], alpha = COMBOS$alpha[i], view = vn, scale_m = v$scale_m, reps = length(reps),
        t(sc),
        share_no_area = mean(vapply(runs, function(r) r$nNoGoal / r$nAgents, 1)),
        share_fallback = mean(vapply(runs, function(r) if(is.null(r$nFallback)) 0 else r$nFallback / r$nAgents, 1)),
        over_10pc_real = mean(unlist(lapply(runs, `[[`, "realReachShare")) > 0.10),
        track_edges = mean(vapply(runs, `[[`, 1, "trackEdges")),
        secs = mean(vapply(runs, function(r) as.numeric(r$secs), 1)))
    }
    maps[[id]] <- trans(as.vector(G$views[["1_all"]]$Msim %*% tot), cfg$simTransform)
  }
  tab <- do.call(rbind, rows)
  #the survey against itself, half the respondents as the model
  if(hasL){
    set.seed(BASE_SEED + 6L)
    respIdx <- match(sv$resp, unique(sv$resp))
    for(vn in views){
      v <- G$views[[vn]]
      sc <- replicate(50, {
        A  <- sample(nResp, floor(nResp / 2)); wA <- as.numeric(respIdx %in% A)
        sA <- surveyView(G, v, as.vector(Matrix::crossprod(sv$L, wA)))
        sB <- surveyView(G, v, as.vector(Matrix::crossprod(sv$L, 1 - wA)))
        agreementScores(sA, sB, if(vn == "1_all") sA / length(A) else NULL, nResp - length(A))
      })
      tab <- rbindFill(list(tab, data.frame(context = ctx, combo = "survey_half", view = vn,
                                            scale_m = v$scale_m, t(rowMeans(sc, na.rm = TRUE)))))
    }
  }
  utils::write.csv(tab, outPath(SWEEP_DIR, paste0("sweep_", ctx, ".csv")), row.names = FALSE)

  #### figure 1: every score per combination, the survey's own level as a line ####
  scores <- c(pearson = "Pearson r", hit = "hit (survey use covered)", auc = "AUC (used vs unused)",
              boyce = "Boyce index", contra = "contradicted presence (lower = better)",
              top5 = "busiest 5 % of cells")
  long <- do.call(rbind, lapply(names(scores), function(m){
    d <- tab[tab$combo != "survey_half", c("combo", "view", "scale_m", m)]
    names(d)[4] <- "value"; d$score <- scores[[m]]; d
  }))
  long <- long[is.finite(long$value), ]
  long$label <- factor(comboLabel(long$combo), levels = rev(comboLabel(COMBOS$id)))
  long$scale <- paste(long$scale_m, "m")
  ref <- do.call(rbind, lapply(names(scores), function(m){
    d <- tab[tab$combo == "survey_half", c("view", "scale_m", m)]
    if(!nrow(d)) return(NULL)
    names(d)[3] <- "value"; d$score <- scores[[m]]; d
  }))
  #concentration: the survey's own value is the target, not the half-vs-half one
  tgt <- tab[tab$combo == COMBOS$id[1], c("view", "scale_m", "top5_survey")]
  tgt <- data.frame(view = tgt$view, scale_m = tgt$scale_m, value = tgt$top5_survey,
                    score = scores[["top5"]])
  ref <- rbind(if(!is.null(ref)) ref[ref$score != scores[["top5"]], ], tgt)
  ref <- ref[is.finite(ref$value), ]
  ref$scale <- paste(ref$scale_m, "m")
  eng <- long$combo == "strong_r10_a05"
  p1 <- ggplot(long, aes(value, label)) +
    geom_vline(data = ref, aes(xintercept = value), colour = "#9a9994", linetype = "22", linewidth = 0.35) +
    geom_point(aes(colour = eng), size = 1.7) +
    scale_colour_manual(values = c(`FALSE` = "#2a78d6", `TRUE` = "#eb6834"), guide = "none") +
    facet_grid(scale ~ score, scales = "free_x") +
    labs(x = NULL, y = NULL,
         title = sprintf("%s: Full, %d replicates per combination (orange: the engine; dashed: survey half vs half, or the survey's own concentration)",
                         cfg$label, length(reps))) +
    theme_bw(base_size = 7) + theme(strip.text = element_text(size = 6.5), plot.title = element_text(size = 8),
                                    panel.grid.minor = element_blank())
  ggsave(outPath(SWEEP_DIR, paste0("fig_sweep_scores_", ctx, ".png")), p1, width = 30,
         height = 4 + 0.45 * nrow(COMBOS) * length(views), units = "cm", dpi = 300, bg = "white")

  #### figure 2: the maps, survey first ####
  v1 <- G$views[["1_all"]]; grid <- gridFrom(v1$grid)
  panel <- function(vals, title){
    r <- terra::rast(grid); x <- rep(NA_real_, terra::ncell(r)); x[v1$idx] <- std01(vals)
    terra::values(r) <- x; names(r) <- "layer"
    ggplot() + ggtitle(title) + tidyterra::geom_spatraster(data = r, aes(fill = layer)) +
      scale_fill_gradientn(colours = c("white", "red", "purple4"), na.value = "lightgrey", limits = c(0, 1)) +
      theme_bw(base_size = 6) + theme(legend.position = "none", axis.text = element_blank(),
                                      axis.ticks = element_blank(), panel.grid = element_blank(),
                                      plot.title = element_text(size = 5.5))
  }
  ps <- c(list(panel(surV[["1_all"]], "Survey")),
          lapply(COMBOS$id, function(id){
            x <- tab[tab$combo == id & tab$view == vPub, ]
            panel(maps[[id]], sprintf("%s\nr %.2f, AUC %.2f, Boyce %.2f", gsub(" | ", "\n", comboLabel(id), fixed = TRUE),
                                      x$pearson, x$auc, x$boyce))
          }))
  g <- gridExtra::arrangeGrob(grobs = ps, ncol = 4)
  ggsave(outPath(SWEEP_DIR, paste0("fig_sweep_maps_", ctx, ".png")), ggpubr::as_ggplot(g), width = 24,
         height = 6.5 * ceiling(length(ps) / 4), units = "cm", dpi = 300, bg = "white")

  d <- tab[tab$view == vPub & tab$combo != "survey_half", ]
  d <- d[order(-d$pearson), ]
  logf("compare %s at %d m (%d reps), Pearson: %s", ctx, G$views[[vPub]]$scale_m, length(reps),
       paste(sprintf("%s %.3f", d$combo, d$pearson), collapse = " | "))
  tab
}

main <- function(){
  logf("sweep | %s | contexts %s | %d reps | %d worker(s) | stages %s | code %s", OUT,
       paste(CONTEXTS, collapse = ","), SWEEP_REPS, SWEEP_WORKERS, paste(SWEEP_STAGES, collapse = ","), GIT)
  if("verify" %in% SWEEP_STAGES) for(ctx in CONTEXTS) stageVerify(ctx)
  if("sim" %in% SWEEP_STAGES) stageSweepSim()
  if("compare" %in% SWEEP_STAGES) for(ctx in CONTEXTS) stageSweepCompare(ctx)
  logf("sweep done")
}

if(!IS_SWEEP_WORKER) main()
