#### Two junction-choice tests on the fixed engine ####
#
# The leave-two-out calibration (<calibration>/report.md) left two ideas for the walk
# inside the areas:
#   1. a higher junction DULN weight wQ: every fold raised it (3.6-5.3 against the
#      engine's 2, at its alpha 0.5);
#   2. the edge DULN instead of the node DULN at a junction: in the movement GLM the edge
#      layer kept the GlattPark effect, the node layer lost it.
# This runs both on top of the settings of the last full validation (reach 0.33, alpha 2,
# straight 1: there wQ = 1/alpha = 0.5 and the turn weight wS = s/alpha = 0.5), for the
# two types whose walk reads DULN (Full, Nearby), and scores every variant against the
# stored runs of that validation on the same seeds (paired bootstrap over replicates).
#
#   Rscript data-raw/test_junction_duln.R
#
# VFT_JT_REPS     replicates (default 20)
# VFT_JT_WORKERS  daemons (default 13)
# VFT_JT_OUT      output folder (default <timefix OUT>/junction_test)
# VFT_VAL_OUT     the validation whose runs are the reference (default _final_timefix)

Sys.setenv(VFT_VAL_WORKER = "1")
LOCAL <- "C:/Users/frueh/Documents/Local Data"
if(!nzchar(Sys.getenv("VFT_VAL_OUT")))
  Sys.setenv(VFT_VAL_OUT = file.path(LOCAL, "pathUsageValidation_2026_final_timefix"))
Sys.setenv(VFT_VAL_ENGINE = "corrected", VFT_VAL_STRAIGHT = "1", VFT_VAL_ALPHA = "2", VFT_VAL_REACH = "0.33")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
VAL_SCRIPT <- file.path(.repo, "data-raw", "validate_usage_surveys.R")
source(VAL_SCRIPT)

NREP <- as.integer(envNum("VFT_JT_REPS", 20))
NW   <- as.integer(envNum("VFT_JT_WORKERS", 13))
JOUT <- Sys.getenv("VFT_JT_OUT", file.path(OUT, "junction_test"))
dir.create(JOUT, recursive = TRUE, showWarnings = FALSE)
JT_TYPES <- c("full", "nearby")

#every variant sets all three fields: simMemory() writes CORR for good on its daemon.
#straight = wS * alpha (alpha stays 2)
VARIANTS <- list(
  check      = list(junction = "node", wQ = 0.5, straight = 1),   #= the stored runs
  node_wQ1   = list(junction = "node", wQ = 1,   straight = 1),
  node_wQ2   = list(junction = "node", wQ = 2,   straight = 1),
  node_wQ4.5 = list(junction = "node", wQ = 4.5, straight = 1),
  node_wQ4.5_wS2 = list(junction = "node", wQ = 4.5, straight = 4),
  edge_wQ0.5 = list(junction = "edge", wQ = 0.5, straight = 1),
  edge_wQ1   = list(junction = "edge", wQ = 1,   straight = 1),
  edge_wQ2   = list(junction = "edge", wQ = 2,   straight = 1),
  edge_wQ4.5 = list(junction = "edge", wQ = 4.5, straight = 1))
NCHECK <- 3

g <- do.call(rbind, lapply(names(VARIANTS), function(v)
  expand.grid(rep = seq_len(if(v == "check") NCHECK else NREP), type = JT_TYPES, variant = v,
              ctx = CONTEXTS, stringsAsFactors = FALSE)))
#site by site, so a daemon rarely switches networks; the slow sites first
g <- g[order(match(g$ctx, c("glatt", "glattPark", "wigger")), g$type, g$variant, g$rep), ]
logf("junction test: %d runs (%d variants x %d types x %d sites) on %d daemons",
     nrow(g), length(VARIANTS), length(JT_TYPES), length(CONTEXTS), NW)

mirai::daemons(NW)
w <- mirai::everywhere({
  .libPaths(libs); Sys.setenv(VFT_VAL_WORKER = "1"); source(script, local = globalenv()); Sys.getpid()
}, libs = .libPaths(), script = VAL_SCRIPT)
mirai::call_mirai(w)
t0 <- proc.time()[["elapsed"]]
ms <- lapply(seq_len(nrow(g)), function(i) mirai::mirai(
  tryCatch(simMemory(ctx, type, rep, corr = corr),
           error = function(e) list(status = "failed", msg = conditionMessage(e))),
  ctx = g$ctx[i], type = g$type[i], rep = g$rep[i], corr = VARIANTS[[g$variant[i]]]))
res <- vector("list", nrow(g))
for(i in seq_along(ms)){
  r <- mirai::call_mirai(ms[[i]])$data
  if(mirai::is_error_value(r)) r <- list(status = "failed", msg = format(r))
  res[[i]] <- r
  if(i %% 50 == 0) logf("junction test: %d / %d done, %.0f min", i, nrow(g), (proc.time()[["elapsed"]] - t0) / 60)
}
mirai::daemons(0)
saveRDS(list(grid = g, res = res, variants = VARIANTS), file.path(JOUT, "junction_runs.rds"))
bad <- which(!vapply(res, function(r) identical(r$status, "done"), logical(1)))
if(length(bad)) logf("junction test: %d FAILED, first: %s", length(bad), res[[bad[1]]]$msg)

#the check variant must reproduce the stored runs exactly
ok <- which(g$variant == "check")
same <- vapply(ok, function(i){
  if(!identical(res[[i]]$status, "done")) return(NA)
  f <- runsFrom(g$ctx[i], g$type[i], g$rep[i])[[1]]
  identical(as.integer(f$cells), as.integer(res[[i]]$cells)) && identical(as.integer(f$counts), as.integer(res[[i]]$counts))
}, logical(1))
logf("junction test: the check variant reproduces the stored runs: %d of %d identical", sum(same, na.rm = TRUE), length(same))

#paired bootstrap over replicates: the same resampled seeds for the variant and the
#reference. Score = mean Pearson over the site's head views (100 + 500 m; GlattPark 500).
B <- 1000
set.seed(1)
idx <- matrix(sample.int(NREP, NREP * B, replace = TRUE), B)
scorers <- lapply(stats::setNames(CONTEXTS, CONTEXTS), scorerFor)
stored <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx)
  lapply(stats::setNames(c(JT_TYPES, "nearbyRandom"), c(JT_TYPES, "nearbyRandom")),
         function(t) runsFrom(ctx, t, seq_len(NREP))))
scoreSet <- function(ctx, rr){ sc <- scorers[[ctx]](rr); c(score = mean(sc), sc) }
boot <- function(ctx, rr) t(vapply(seq_len(B), function(b) scoreSet(ctx, rr[idx[b, ]]),
                                   numeric(length(scoreSet(ctx, rr[1:2])))))
refBoot <- list()
for(ctx in CONTEXTS) for(t in names(stored[[ctx]])) refBoot[[ctx]][[t]] <- boot(ctx, stored[[ctx]][[t]])

tab <- list()
for(ctx in CONTEXTS) for(t in JT_TYPES) for(v in setdiff(names(VARIANTS), "check")){
  i  <- which(g$ctx == ctx & g$type == t & g$variant == v)
  rr <- res[i][order(g$rep[i])]
  if(!all(vapply(rr, function(r) identical(r$status, "done"), logical(1)))) next
  pt <- scoreSet(ctx, rr); bt <- boot(ctx, rr)
  ref <- scoreSet(ctx, stored[[ctx]][[t]])
  best <- c(nearby = scoreSet(ctx, stored[[ctx]]$nearby)["score"],
            nearbyRandom = scoreSet(ctx, stored[[ctx]]$nearbyRandom)["score"])
  bestT <- names(stored[[ctx]])[-1][which.max(best)]
  dSelf <- bt[, "score"] - refBoot[[ctx]][[t]][, "score"]
  dBest <- bt[, "score"] - refBoot[[ctx]][[if(t == "nearby") "nearbyRandom" else bestT]][, "score"]
  tab[[length(tab) + 1]] <- data.frame(
    ctx = ctx, type = TYPES[[t]]$label, variant = v, t(pt), ref = ref[["score"]],
    dSelf = pt[["score"]] - ref[["score"]], dSelf_lo = unname(quantile(dSelf, .025)), dSelf_hi = unname(quantile(dSelf, .975)),
    vsBest = if(t == "nearby") "Nearby-Random" else TYPES[[bestT]]$label,
    dBest = unname(stats::median(dBest)), dBest_lo = unname(quantile(dBest, .025)), dBest_hi = unname(quantile(dBest, .975)),
    secs = mean(vapply(rr, function(r) r$secs, 1)))
}
tab <- rbindFill(tab)
utils::write.csv(tab, file.path(JOUT, "junction_test.csv"), row.names = FALSE)
#the mean over the three sites, per type and variant
agg <- aggregate(cbind(score, ref, dSelf, dBest) ~ type + variant, tab, mean)
agg$sitesWorse <- aggregate(dSelf_hi ~ type + variant, tab, function(x) sum(x < 0))$dSelf_hi
agg$sitesBetter <- aggregate(dSelf_lo ~ type + variant, tab, function(x) sum(x > 0))$dSelf_lo
utils::write.csv(agg, file.path(JOUT, "junction_test_mean.csv"), row.names = FALSE)
options(width = 220)
print(tab, digits = 3, row.names = FALSE)
print(agg[order(agg$type, -agg$dSelf), ], digits = 3, row.names = FALSE)
logf("junction test written to %s", JOUT)
