#### Calibrating the Full model on one site, for scoring on the other two ####
#
# The fullCal type of data-raw/validate_usage_surveys.R (aoiProbsCal() and
# chooseRoutesCal() in data-raw/engine_corrections.R) has one parameter list for the
# area choice, the movement inside the areas and the legs to and from them. This
# searches it on ONE training site, against that site's survey, by running the
# simulation itself:
#
#   start     the area choice of data-raw/diagnose_duln.R's maximum-likelihood fit on
#             the training site (M1c_dulnArea: distance in hours, DULN, area, reach); movement
#             and legs at the engine's values (wQ 2, wS 2, f 8)
#   blocks    Nelder-Mead over three blocks in turn, on bounded (logistic) scales:
#               M  movement + legs   wQ (node DULN weight), wS (turn weight), log2 f
#               D  area choice       log lambda (one scale on bT, bD, bQ, bA), bQ, reach
#               M  again, shorter
#   score     mean Pearson r of the training site's maps, at 100 m and 500 m (GlattPark:
#             500 m only), computed exactly as the compare stage does (verified
#             below), over VFT_CAL_REPS replicates on the fixed seeds BASE_SEED + k -
#             the same seeds as every other type, so a parameter change is never
#             mistaken for seed noise
#
# Writes <CAL>/fold_<train>/search.csv (every evaluation) and best_par.rds, which
# validate_usage_surveys.R reads through VFT_VAL_CAL for the test runs.
#
#   VFT_CAL_TRAIN=wigger Rscript data-raw/calibrate_full.R
#
# VFT_CAL_TRAIN    the training site (glattPark, wigger or glatt)
# VFT_CAL_REPS     replicates per evaluation (default 10)
# VFT_CAL_WORKERS  mirai daemons (default VFT_CAL_REPS)
# VFT_CAL_BUDGET   evaluations per block, comma list (default 24,24,14)
# VFT_CAL_STAGES   verify,search (default both)
# VFT_CAL_OUT      root (default .../pathUsageValidation_2026_calibration)

Sys.setenv(VFT_VAL_WORKER = "1")   #definitions only
LOCAL <- "C:/Users/frueh/Documents/Local Data"
if(!nzchar(Sys.getenv("VFT_VAL_OUT")))
  Sys.setenv(VFT_VAL_OUT = file.path(LOCAL, "pathUsageValidation_2026_final_manual_fixed"))
Sys.setenv(VFT_VAL_ENGINE = "corrected")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
VAL_SCRIPT <- file.path(.repo, "data-raw", "validate_usage_surveys.R")
source(VAL_SCRIPT)

TRAIN <- Sys.getenv("VFT_CAL_TRAIN", "")
if(!TRAIN %in% names(CTX)) stop("VFT_CAL_TRAIN must be one of ", paste(names(CTX), collapse = ", "))
CAL_REPS <- as.integer(envNum("VFT_CAL_REPS", 10))
CAL_W    <- as.integer(envNum("VFT_CAL_WORKERS", CAL_REPS))
BUDGET   <- as.integer(envList("VFT_CAL_BUDGET", c("24", "24", "14")))
CSTAGES  <- envList("VFT_CAL_STAGES", c("verify", "search"))
CAL  <- Sys.getenv("VFT_CAL_OUT", file.path(LOCAL, "pathUsageValidation_2026_calibration"))
FOLD <- file.path(CAL, paste0("fold_", TRAIN))
dir.create(FOLD, recursive = TRUE, showWarnings = FALSE)

#### the score, as the compare stage computes it ####
#scorerFor(), HEAD_VIEWS() and runsFrom() live in validate_usage_surveys.R (shared with
#data-raw/sweep_reach_alpha.R)

#### the parameter blocks ####

#bounded scales: x unconstrained, value = lo + (hi - lo) * plogis(x)
BOUNDS <- list(wQ = c(0, 6), wS = c(0, 6), log2f = c(1, 12),
               logLambda = c(log(0.05), log(5)), bQ = c(-3, 3), reach = c(0.05, 1))
toX   <- function(v, nm){ b <- BOUNDS[[nm]]; stats::qlogis(min(max((v - b[1]) / (b[2] - b[1]), 0.02), 0.98)) }
fromX <- function(x, nm){ b <- BOUNDS[[nm]]; b[1] + (b[2] - b[1]) * stats::plogis(x) }
BLOCKS <- list(M = c("wQ", "wS", "log2f"), D = c("logLambda", "bQ", "reach"))

#the full parameter list from the search state (the destination's MLE scaled by lambda,
#with bQ set on its own)
parFrom <- function(st, dest0){
  p <- CAL_DEFAULT
  lam <- exp(st$logLambda)
  p$bT <- lam * dest0$bT; p$bD <- lam * dest0$bD; p$bA <- lam * dest0$bA
  p$bQ <- st$bQ
  p$reach <- st$reach
  p$wQ <- st$wQ; p$wS <- st$wS; p$f <- 2^st$log2f
  p
}

#### evaluations, on the daemons ####

startDaemons <- function(){
  mirai::daemons(CAL_W)
  w <- mirai::everywhere({
    .libPaths(libs)
    Sys.setenv(VFT_VAL_WORKER = "1")
    source(script, local = globalenv())
    Sys.getpid()
  }, libs = .libPaths(), script = VAL_SCRIPT)
  mirai::call_mirai(w)
}

simReps <- function(ctx, par, reps){
  ms <- lapply(reps, function(k) mirai::mirai(
    tryCatch(simMemory(ctx, "fullCal", rep, par),
             error = function(e) list(status = "failed", msg = conditionMessage(e))),
    ctx = ctx, rep = k, par = par))
  out <- lapply(ms, function(m){
    r <- mirai::call_mirai(m)$data
    if(mirai::is_error_value(r)) r <- list(status = "failed", msg = format(r))
    r
  })
  bad <- vapply(out, function(r) !identical(r$status, "done"), logical(1))
  if(any(bad)) stop("simulation failed: ", out[[which(bad)[1]]]$msg)
  out
}

LOGF <- file.path(FOLD, "search.csv")
evalLog <- if(file.exists(LOGF)) utils::read.csv(LOGF) else NULL
parKey <- function(p) paste(sprintf("%.6g", unlist(p[order(names(p))])), collapse = "|")

evaluate <- function(p, block, score){
  key <- parKey(p)
  if(!is.null(evalLog) && key %in% evalLog$key) return(evalLog[match(key, evalLog$key), ])
  t0 <- proc.time()[["elapsed"]]
  runs <- simReps(TRAIN, p, seq_len(CAL_REPS))
  r <- score(runs)
  row <- data.frame(eval = if(is.null(evalLog)) 1L else nrow(evalLog) + 1L, block = block, key = key,
                    score = mean(r), t(r), as.data.frame(p[c("bT", "bD", "bQ", "bA", "reach", "wQ", "wS", "f")]),
                    noGoal = mean(vapply(runs, function(x) x$nNoGoal / x$nAgents, 1)),
                    fallback = mean(vapply(runs, function(x) x$nFallback / x$nAgents, 1)),
                    secs = proc.time()[["elapsed"]] - t0)
  evalLog <<- rbindFill(list(evalLog, row))
  utils::write.csv(evalLog, LOGF, row.names = FALSE)
  logf("eval %3d [%s] score %.4f (%s) | bT %.2f bD %.2f bQ %.3f bA %.3f reach %.3f | wQ %.2f wS %.2f f %.1f | no area %.3f | %.0f s",
       row$eval, block, row$score, paste(sprintf("%s %.4f", names(r), r), collapse = ", "),
       p$bT, p$bD, p$bQ, p$bA, p$reach, p$wQ, p$wS, p$f, row$noGoal, row$secs)
  row
}

#a small Nelder-Mead with an explicit first step and a budget of evaluations (the
#objective is deterministic: fixed seeds)
nelderMead <- function(fn, x0, step = 1, budget = 20){
  n <- length(x0)
  X <- rbind(x0, t(vapply(seq_len(n), function(i){ x <- x0; x[i] <- x[i] + step; x }, numeric(n))))
  f <- apply(X, 1, fn); used <- n + 1
  while(used < budget){
    o <- order(f); X <- X[o, , drop = FALSE]; f <- f[o]
    c0 <- colMeans(X[-(n + 1), , drop = FALSE])
    xr <- c0 + (c0 - X[n + 1, ]); fr <- fn(xr); used <- used + 1
    if(fr < f[1] && used < budget){
      xe <- c0 + 2 * (c0 - X[n + 1, ]); fe <- fn(xe); used <- used + 1
      if(fe < fr){ X[n + 1, ] <- xe; f[n + 1] <- fe } else { X[n + 1, ] <- xr; f[n + 1] <- fr }
    }else if(fr < f[n]){
      X[n + 1, ] <- xr; f[n + 1] <- fr
    }else{
      xc <- c0 + 0.5 * (X[n + 1, ] - c0); fc <- fn(xc); used <- used + 1
      if(fc < f[n + 1]){ X[n + 1, ] <- xc; f[n + 1] <- fc }
      else{ #shrink towards the best
        for(i in 2:(n + 1)){ if(used >= budget) break
          X[i, ] <- X[1, ] + 0.5 * (X[i, ] - X[1, ]); f[i] <- fn(X[i, ]); used <- used + 1 }
      }
    }
  }
  list(x = X[which.min(f), ], f = min(f))
}

#### stages ####

verifyStage <- function(score){
  #1. the scorer against the compare stage's own numbers (Full, all its replicates)
  cmp <- readRDS(file.path(OUT, "results", "compare.rds"))[[TRAIN]]$metrics
  N   <- cmp$N[1]
  r   <- score(runsFrom(TRAIN, "full", seq_len(N)))
  ref <- vapply(HEAD_VIEWS(TRAIN), function(v)
    cmp$point[cmp$type == "Full" & cmp$test == "map" & cmp$view == v & cmp$method == "pearson"], 1)
  logf("verify: scorer %s vs compare %s", paste(sprintf("%.6f", r), collapse = " "),
       paste(sprintf("%.6f", ref), collapse = " "))
  if(max(abs(r - ref)) > 1e-6) stop("the scorer does not reproduce the compare stage")
  #2. the calibrated walk at the engine's values is the engine's walk (alpha 0.5, s 1)
  set.seed(1)
  vr <- replicate(200, sample(1:50, sample(1:4, 1)), simplify = FALSE)
  DF <- as.data.frame(matrix(stats::rnorm(50 * 7, 0, 5), 50, 7, dimnames = list(NULL, DULN_BY_TYPE)))
  DF$nodeID <- 1:50
  VC <- data.frame(X = 8.5 + stats::runif(50) / 100, Y = 47.4 + stats::runif(50) / 100)
  pv <- replicate(200, if(stats::runif(1) < 0.8) sample(1:50, 3) else NULL, simplify = FALSE)
  typ <- sample(names(DULN_BY_TYPE), 200, TRUE); cv <- sample(1:50, 200, TRUE)
  a <- chooseRoutesR(vr, DF, pv, typ, VC, cv, 1, revisit = 0.00001, alpha = 0.5)
  b <- chooseRoutesCal(vr, DF, pv, typ, VC, cv, utils::modifyList(CAL_DEFAULT, list(wQ = 2, wS = 2)))
  d <- max(abs(unlist(a) - unlist(b)))
  logf("verify: calibrated walk vs engine walk, max |dp| %.2g", d)
  if(d > 1e-12) stop("chooseRoutesCal() at wQ = wS = 2 is not the engine's walk")
  #3. the legs at f = 8 are the prepared network's weights
  base <- readRDS(netFile(TRAIN))$net
  CAL_PAR <<- CAL_DEFAULT
  v <- typeVariant(base, readRDS(aoiFile(TRAIN))$polys, "fullCal", thr = readRDS(aoiFile(TRAIN))$thr)
  nm <- grep("^SHAPE_Leng_", igraph::edge_attr_names(base), value = TRUE)
  same <- all(vapply(nm, function(k) identical(igraph::edge_attr(base, k), igraph::edge_attr(v$net, k)), TRUE))
  logf("verify: fullCal legs at f = 8 identical to the prepared weights: %s", same)
  if(!same) stop("typeVariant(fullCal) at f = 8 changes the weights")
}

searchStage <- function(score){
  fits <- readRDS(file.path(CAL, "diagnosis", "destination_fits.rds"))[[TRAIN]]
  d0 <- fits$M1c_dulnArea
  dest0 <- as.list(d0$b)
  logf("search %s: start from the area-choice MLE bT %.2f bD %.2f bQ %.3f bA %.3f reach %s",
       TRAIN, dest0$bT, dest0$bD, dest0$bQ, dest0$bA, format(d0$reach))
  st <- list(wQ = 2, wS = 2, log2f = 3, logLambda = 0, bQ = dest0$bQ,
             reach = min(max(if(is.finite(d0$reach)) d0$reach else 1, 0.05), 1))
  #the references at the same seeds: the existing types' first CAL_REPS replicates
  ref <- do.call(rbind, lapply(c("full", "nearby", "nearbyRandom", "random", "fullRandom"), function(t){
    r <- score(runsFrom(TRAIN, t, seq_len(CAL_REPS)))
    data.frame(type = TYPES[[t]]$label, score = mean(r), t(r))
  }))
  utils::write.csv(ref, file.path(FOLD, "search_reference.csv"), row.names = FALSE)
  logf("search %s: references at %d reps: %s", TRAIN, CAL_REPS,
       paste(sprintf("%s %.4f", ref$type, ref$score), collapse = ", "))
  startDaemons()
  on.exit(mirai::daemons(0), add = TRUE)
  evaluate(parFrom(st, dest0), "start", score)
  plan <- list(c("M", BUDGET[1]), c("D", BUDGET[2]), c("M", BUDGET[3]))
  for(b in plan){
    nm <- BLOCKS[[b[1]]]
    fn <- function(x){
      s <- st; for(i in seq_along(nm)) s[[nm[i]]] <- fromX(x[i], nm[i])
      -evaluate(parFrom(s, dest0), b[1], score)$score
    }
    x0 <- vapply(nm, function(k) toX(st[[k]], k), 1)
    res <- nelderMead(fn, x0, step = 1, budget = as.integer(b[2]))
    for(i in seq_along(nm)) st[[nm[i]]] <- fromX(res$x[i], nm[i])
    logf("search %s: block %s done, score %.4f", TRAIN, b[1], -res$f)
  }
  best <- evalLog[which.max(evalLog$score), ]
  bestPar <- utils::modifyList(CAL_DEFAULT, as.list(best[c("bT", "bD", "bQ", "bA", "reach", "wQ", "wS", "f")]))
  saveRDS(bestPar, file.path(FOLD, "best_par.rds"))
  logf("search %s: best eval %d, score %.4f (Full %.4f, Nearby %.4f, Nearby-Random %.4f)",
       TRAIN, best$eval, best$score, ref$score[ref$type == "Full"], ref$score[ref$type == "Nearby"],
       ref$score[ref$type == "Nearby-Random"])
}

score <- scorerFor(TRAIN)
if("verify" %in% CSTAGES) verifyStage(score)
if("search" %in% CSTAGES) searchStage(score)
logf("calibration %s done", TRAIN)
