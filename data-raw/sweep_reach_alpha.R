#### Quick sweep: a longer reach and a more random route choice ####
#
# The corrected engine lets an agent pick an area only when getting there takes at most
# CORR$reach (10 %) of its outing, floored at 5 minutes, on the real path length. The
# approach walk is not charged to the outing (launchSim's timeNotAccounted), so a longer
# reach means "the outing plus up to reach x outing of walking". This runs Full,
# Nearby and Nearby-Random over reach x alpha on the first VFT_SWEEP_REPS seeds of
# every site and scores them like the compare stage (scorerFor()).
#
#   Rscript data-raw/sweep_reach_alpha.R
#
# VFT_SWEEP_REACH   comma list (default 0.10,0.20,0.33,0.50)
# VFT_SWEEP_ALPHA   comma list (default 0.5,2)
# VFT_SWEEP_REPS    replicates (default 10)
# VFT_SWEEP_WORKERS daemons (default 16)
# VFT_SWEEP_OUT     output folder (default <calibration>/sweep_reach_alpha)

Sys.setenv(VFT_VAL_WORKER = "1")
LOCAL <- "C:/Users/frueh/Documents/Local Data"
if(!nzchar(Sys.getenv("VFT_VAL_OUT")))
  Sys.setenv(VFT_VAL_OUT = file.path(LOCAL, "pathUsageValidation_2026_final_manual_fixed"))
Sys.setenv(VFT_VAL_ENGINE = "corrected", VFT_VAL_STRAIGHT = "1")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
VAL_SCRIPT <- file.path(.repo, "data-raw", "validate_usage_surveys.R")
source(VAL_SCRIPT)

REACHES <- as.numeric(envList("VFT_SWEEP_REACH", c("0.10", "0.20", "0.33", "0.50")))
ALPHAS  <- as.numeric(envList("VFT_SWEEP_ALPHA", c("0.5", "2")))
NREP    <- as.integer(envNum("VFT_SWEEP_REPS", 10))
NW      <- as.integer(envNum("VFT_SWEEP_WORKERS", 16))
SOUT    <- Sys.getenv("VFT_SWEEP_OUT", file.path(LOCAL, "pathUsageValidation_2026_calibration", "sweep_reach_alpha"))
dir.create(SOUT, recursive = TRUE, showWarnings = FALSE)
SWEEP_TYPES <- c("full", "nearby", "nearbyRandom")

g <- expand.grid(rep = seq_len(NREP), type = SWEEP_TYPES, alpha = ALPHAS, reach = REACHES,
                 ctx = CONTEXTS, stringsAsFactors = FALSE)
#site by site, so a daemon rarely switches networks
g <- g[order(match(g$ctx, CONTEXTS), g$type, g$reach, g$alpha, g$rep), ]
logf("sweep: %d runs (%d reach x %d alpha x %d types x %d sites x %d reps) on %d daemons",
     nrow(g), length(REACHES), length(ALPHAS), length(SWEEP_TYPES), length(CONTEXTS), NREP, NW)

mirai::daemons(NW)
w <- mirai::everywhere({
  .libPaths(libs); Sys.setenv(VFT_VAL_WORKER = "1"); source(script, local = globalenv()); Sys.getpid()
}, libs = .libPaths(), script = VAL_SCRIPT)
mirai::call_mirai(w)
t0 <- proc.time()[["elapsed"]]
ms <- lapply(seq_len(nrow(g)), function(i) mirai::mirai(
  tryCatch(simMemory(ctx, type, rep, corr = list(reach = reach, alpha = alpha)),
           error = function(e) list(status = "failed", msg = conditionMessage(e))),
  ctx = g$ctx[i], type = g$type[i], rep = g$rep[i], reach = g$reach[i], alpha = g$alpha[i]))
res <- vector("list", nrow(g))
for(i in seq_along(ms)){
  r <- mirai::call_mirai(ms[[i]])$data
  if(mirai::is_error_value(r)) r <- list(status = "failed", msg = format(r))
  res[[i]] <- r
  if(i %% 50 == 0) logf("sweep: %d / %d done, %.0f min", i, nrow(g), (proc.time()[["elapsed"]] - t0) / 60)
}
mirai::daemons(0)
bad <- which(!vapply(res, function(r) identical(r$status, "done"), logical(1)))
if(length(bad)) logf("sweep: %d FAILED, first: %s", length(bad), res[[bad[1]]]$msg)

#the reference combination must reproduce the stored runs exactly
ok <- g$reach == 0.10 & g$alpha == 0.5
if(any(ok)){
  same <- vapply(which(ok), function(i){
    if(!identical(res[[i]]$status, "done")) return(NA)
    f <- runsFrom(g$ctx[i], g$type[i], g$rep[i])[[1]]
    identical(as.integer(f$cells), as.integer(res[[i]]$cells)) && identical(as.integer(f$counts), as.integer(res[[i]]$counts))
  }, logical(1))
  logf("sweep: reach 0.10 / alpha 0.5 reproduces the stored runs: %d of %d identical", sum(same, na.rm = TRUE), length(same))
}

#the raw replicates first: a scoring error must never cost the runs
saveRDS(list(grid = g, res = res), file.path(SOUT, "sweep_runs.rds"))
key <- unique(g[c("ctx", "type", "reach", "alpha")])
scorers <- lapply(stats::setNames(CONTEXTS, CONTEXTS), scorerFor)
#rbindFill: GlattPark has no 100 m score, so its rows have one column fewer
tab <- rbindFill(lapply(seq_len(nrow(key)), function(k){
  i <- which(g$ctx == key$ctx[k] & g$type == key$type[k] & g$reach == key$reach[k] & g$alpha == key$alpha[k])
  rr <- res[i]; rr <- rr[vapply(rr, function(r) identical(r$status, "done"), logical(1))]
  if(!length(rr)) return(NULL)
  sc <- scorers[[key$ctx[k]]](rr)
  data.frame(key[k, ], reps = length(rr), score = mean(sc), t(sc),
             noArea = mean(vapply(rr, function(r) r$nNoGoal / r$nAgents, 1)),
             fallback = mean(vapply(rr, function(r) r$nFallback / r$nAgents, 1)),
             secs = mean(vapply(rr, function(r) r$secs, 1)))
}))
tab$type <- vapply(tab$type, function(t) TYPES[[t]]$label, "")
utils::write.csv(tab, file.path(SOUT, "sweep.csv"), row.names = FALSE)
options(width = 200)
print(tab, digits = 3, row.names = FALSE)
logf("sweep written to %s", SOUT)
