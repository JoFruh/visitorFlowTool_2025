#### Sweep: route-choice randomness (alpha) x outing-duration factor ####
#
# The question (2026-10-24): Wigger's simulated walks spread much further than the
# survey's. Is the route choice too random (alpha 5), or are the outings too long?
# Runs one type over alpha x duration factor (VFT_VAL_DURATION, outing scope) on the
# first VFT_SWEEP_REPS seeds of each site and scores like the compare stage
# (scorerFor()). Also reports the outing (median km whole / inside, VFT_VAL_OUTLOG) and
# how concentrated the 100 m map is: the share of usage in its busiest 5 % of cells,
# beside the survey's.
#
#   Rscript data-raw/sweep_alpha_duration.R
#
# VFT_SWEEP_TYPE     one type (default fullRumTime)
# VFT_SWEEP_ALPHA    comma list (default 0.5,1,2,5)
# VFT_SWEEP_FACTOR   comma list of duration factors, 1 = the engine's (default 1,0.5,0.25)
# VFT_SWEEP_REPS     replicates (default 40)
# VFT_SWEEP_WORKERS  daemons (default 3)
# VFT_SWEEP_OUT      output folder (default <VFT_VAL_OUT>/sweep_alpha_duration)
# plus the run's VFT_VAL_* settings (VFT_VAL_OUT, VFT_VAL_CONTEXTS, VFT_VAL_RUM_TIME...)

Sys.setenv(VFT_VAL_WORKER = "1", VFT_VAL_OUTLOG = "1")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
VAL_SCRIPT <- file.path(.repo, "data-raw", "validate_usage_surveys.R")
source(VAL_SCRIPT)

STYPE   <- Sys.getenv("VFT_SWEEP_TYPE", "fullRumTime")
ALPHAS  <- as.numeric(envList("VFT_SWEEP_ALPHA", c("0.5", "1", "2", "5")))
FACTORS <- as.numeric(envList("VFT_SWEEP_FACTOR", c("1", "0.5", "0.25")))
NREP    <- as.integer(envNum("VFT_SWEEP_REPS", 40))
NW      <- as.integer(envNum("VFT_SWEEP_WORKERS", 3))
SOUT    <- Sys.getenv("VFT_SWEEP_OUT", file.path(OUT, "sweep_alpha_duration"))
dir.create(SOUT, recursive = TRUE, showWarnings = FALSE)

g <- expand.grid(rep = seq_len(NREP), alpha = ALPHAS, factor = FACTORS, ctx = CONTEXTS, stringsAsFactors = FALSE)
g <- g[order(match(g$ctx, CONTEXTS), g$factor, g$alpha, g$rep), ]
logf("sweep: %d runs of %s (%d alpha x %d factors x %d sites x %d reps) on %d daemons",
     nrow(g), STYPE, length(ALPHAS), length(FACTORS), length(CONTEXTS), NREP, NW)

mirai::daemons(NW)
w <- mirai::everywhere({
  .libPaths(libs); Sys.setenv(VFT_VAL_WORKER = "1", VFT_VAL_OUTLOG = "1"); source(script, local = globalenv()); Sys.getpid()
}, libs = .libPaths(), script = VAL_SCRIPT)
mirai::call_mirai(w)
t0 <- proc.time()[["elapsed"]]
ms <- lapply(seq_len(nrow(g)), function(i) mirai::mirai(
  tryCatch({
    r <- simMemory(ctx, type, rep, corr = list(alpha = alpha), duration = if(factor == 1) NA else factor)
    #the outing in a few numbers, not the agents' table
    lg <- r$outingLog[!r$outingLog$noGoal, ]
    r$outing <- c(totalKm = stats::median((lg$wayM + lg$inM) / 1000), inKm = stats::median(lg$inM / 1000),
                  inMin = stats::median(lg$inArea, na.rm = TRUE), T = stats::median(lg$timeExt))
    r$outingLog <- NULL
    r
  }, error = function(e) list(status = "failed", msg = conditionMessage(e))),
  ctx = g$ctx[i], type = STYPE, rep = g$rep[i], alpha = g$alpha[i], factor = g$factor[i]))
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
saveRDS(list(grid = g, res = res), file.path(SOUT, "sweep_runs.rds"))

#concentration: share of the (100 m) map's usage in its busiest 5 % of cells
conc <- function(v){ v <- sort(v[is.finite(v)], decreasing = TRUE); if(!sum(v)) NA else sum(v[seq_len(ceiling(.05 * length(v)))]) / sum(v) }
concOf <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx){
  aoi <- readRDS(aoiFile(ctx)); sv <- readRDS(surveyFile(ctx)); edg <- readRDS(edgesFile(ctx))
  G <- buildGrid(ctx, aoi, sv, edg); v <- G$views[[HEAD_VIEWS(ctx)[1]]]
  n100 <- terra::ncell(G$t100)
  list(survey = conc(surveyView(G, v, sv$surveyBase)),
       sim = function(runs){ x <- numeric(n100); for(r in runs) x[r$cells] <- x[r$cells] + r$counts
                             conc(as.vector(v$Msim %*% x)) })
})
scorers <- lapply(stats::setNames(CONTEXTS, CONTEXTS), scorerFor)
key <- unique(g[c("ctx", "factor", "alpha")])
tab <- rbindFill(lapply(seq_len(nrow(key)), function(k){
  i <- which(g$ctx == key$ctx[k] & g$factor == key$factor[k] & g$alpha == key$alpha[k])
  rr <- res[i]; rr <- rr[vapply(rr, function(r) identical(r$status, "done"), logical(1))]
  if(!length(rr)) return(NULL)
  sc <- scorers[[key$ctx[k]]](rr)
  oc <- Reduce(`+`, lapply(rr, `[[`, "outing")) / length(rr)
  data.frame(key[k, ], reps = length(rr), score = mean(sc), t(sc),
             top5 = concOf[[key$ctx[k]]]$sim(rr), top5_survey = concOf[[key$ctx[k]]]$survey,
             T_med = oc[["T"]], inMin_med = oc[["inMin"]], totalKm_med = oc[["totalKm"]], inKm_med = oc[["inKm"]],
             noArea = mean(vapply(rr, function(r) r$nNoGoal / r$nAgents, 1)),
             secs = mean(vapply(rr, function(r) r$secs, 1)))
}))
utils::write.csv(tab, file.path(SOUT, "sweep.csv"), row.names = FALSE)
options(width = 220)
print(tab, digits = 3, row.names = FALSE)
logf("sweep written to %s", SOUT)
