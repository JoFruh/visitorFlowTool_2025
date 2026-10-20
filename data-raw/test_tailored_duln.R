#### The survey-tailored DULN in the simulation ####
#
# Runs the variants data-raw/tailor_duln.R wrote (<duln_tailored>/variants/<id>/, each
# an aoi/ and network/ folder with the tailored edge, node and area DULN) for the two
# types whose inputs read DULN, Full and Nearby, on the settings of the last full
# validation (reach 0.33, alpha 2, straight 1, manual areas). Every variant is scored
# against that validation's stored runs on the same seeds (paired bootstrap over
# replicates), as data-raw/test_junction_duln.R does. The check variant (DULN
# unchanged) must reproduce the stored runs exactly.
#
# Decision rule, set before the runs:
#   - DULN creation is a limit if fitPooled Full beats the best baseline (stored
#     Nearby or Nearby-Random) by >= +.02 on the mean over the 3 sites, with no site
#     significantly worse;
#   - the preferences are site-specific if only fitSite passes;
#   - else the dynamics are the limit (recreationists choose proximity).
#
#   Rscript data-raw/test_tailored_duln.R
#
# VFT_TT_REPS      replicates (default 20)
# VFT_TT_WORKERS   daemons (default 13)
# VFT_TT_VARIANTS  which variants (default every folder in <duln_tailored>/variants)
# VFT_TD_OUT       the tailor_duln.R output (default <VFT_VAL_OUT>/duln_tailored)
# VFT_VAL_OUT      the validation whose runs are the reference (default _final_timefix)

Sys.setenv(VFT_VAL_WORKER = "1")
LOCAL <- "C:/Users/frueh/Documents/Local Data"
if(!nzchar(Sys.getenv("VFT_VAL_OUT")))
  Sys.setenv(VFT_VAL_OUT = file.path(LOCAL, "pathUsageValidation_2026_final_timefix"))
Sys.setenv(VFT_VAL_ENGINE = "corrected", VFT_VAL_STRAIGHT = "1", VFT_VAL_ALPHA = "2", VFT_VAL_REACH = "0.33")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
VAL_SCRIPT <- file.path(.repo, "data-raw", "validate_usage_surveys.R")
source(VAL_SCRIPT)

REF_OUT <- OUT
NREP <- as.integer(envNum("VFT_TT_REPS", 20))
NW   <- as.integer(envNum("VFT_TT_WORKERS", 13))
TD   <- Sys.getenv("VFT_TD_OUT", file.path(REF_OUT, "duln_tailored"))
VARS <- envList("VFT_TT_VARIANTS", list.dirs(file.path(TD, "variants"), recursive = FALSE, full.names = FALSE))
if(!"check" %in% VARS) VARS <- c("check", VARS)
TT_TYPES <- c("full", "nearby")
NCHECK <- 3
#a variant runs where it has a network (a single-site fit has none on its own site)
hasNet <- function(v, ctx) file.exists(file.path(TD, "variants", v, "network", paste0(ctx, "_net.rds")))
for(v in VARS) if(!any(vapply(CONTEXTS, function(ctx) hasNet(v, ctx), TRUE)))
  stop("variant ", v, " has no network at all - run data-raw/tailor_duln.R first")
g <- do.call(rbind, lapply(VARS, function(v)
  expand.grid(rep = seq_len(if(v == "check") NCHECK else NREP), type = TT_TYPES, variant = v,
              ctx = CONTEXTS[vapply(CONTEXTS, function(ctx) hasNet(v, ctx), TRUE)],
              stringsAsFactors = FALSE)))
#site by site, then variant: a daemon rarely switches networks; the slow sites first
g <- g[order(match(g$ctx, c("glatt", "glattPark", "wigger")), g$variant, g$type, g$rep), ]
logf("tailored DULN: %d runs (%d variants x %d types x %d sites) on %d daemons",
     nrow(g), length(VARS), length(TT_TYPES), length(CONTEXTS), NW)

runsF <- file.path(TD, "tailored_runs.rds")
todoI <- seq_len(nrow(g))
res <- vector("list", nrow(g))
if(file.exists(runsF) && !("sim" %in% FORCE)){
  #the runs already made (same replicate, type, variant and site) are kept; only the
  #new ones and the failed ones are run
  saved <- readRDS(runsF)
  key <- function(d) paste(d$rep, d$type, d$variant, d$ctx)
  m <- match(key(g), key(saved$grid))
  res[!is.na(m)] <- saved$res[m[!is.na(m)]]
  todoI <- which(!vapply(res, function(r) identical(r$status, "done"), logical(1)))
  logf("tailored DULN: %d runs read from %s, %d to run", nrow(g) - length(todoI), runsF, length(todoI))
}
if(length(todoI)){
  mirai::daemons(min(NW, length(todoI)))
  w <- mirai::everywhere({
    .libPaths(libs); Sys.setenv(VFT_VAL_WORKER = "1"); source(script, local = globalenv()); Sys.getpid()
  }, libs = .libPaths(), script = VAL_SCRIPT)
  mirai::call_mirai(w)
  t0 <- proc.time()[["elapsed"]]
  #a task points OUT at its variant (scenarioFor() reads the network from there) and
  #drops the cached network when the variant changes: DULN is not in its key
  ms <- lapply(todoI, function(i) mirai::mirai({
    if(!identical(OUT, vdir)){ OUT <<- vdir; rm(list = ls(.valCache), envir = .valCache) }
    tryCatch(simMemory(ctx, type, rep),
             error = function(e) list(status = "failed", msg = conditionMessage(e)))
  }, ctx = g$ctx[i], type = g$type[i], rep = g$rep[i],
     vdir = file.path(TD, "variants", g$variant[i])))
  for(k in seq_along(ms)){
    r <- mirai::call_mirai(ms[[k]])$data
    if(mirai::is_error_value(r)) r <- list(status = "failed", msg = format(r))
    res[[todoI[k]]] <- r
    if(k %% 50 == 0) logf("tailored DULN: %d / %d done, %.0f min", k, length(todoI), (proc.time()[["elapsed"]] - t0) / 60)
  }
  mirai::daemons(0)
  saveRDS(list(grid = g, res = res, variants = VARS), runsF)
}
bad <- which(!vapply(res, function(r) identical(r$status, "done"), logical(1)))
if(length(bad)) logf("tailored DULN: %d FAILED, first: %s", length(bad), res[[bad[1]]]$msg)

#the check variant must reproduce the stored runs exactly
ok <- which(g$variant == "check")
same <- vapply(ok, function(i){
  if(!identical(res[[i]]$status, "done")) return(NA)
  f <- runsFrom(g$ctx[i], g$type[i], g$rep[i])[[1]]
  identical(as.integer(f$cells), as.integer(res[[i]]$cells)) && identical(as.integer(f$counts), as.integer(res[[i]]$counts))
}, logical(1))
logf("tailored DULN: the check variant reproduces the stored runs: %d of %d identical", sum(same, na.rm = TRUE), length(same))
if(!isTRUE(all(same))) stop("the check variant does not reproduce the stored runs - the variants cannot be trusted")

#paired bootstrap over replicates: the same resampled seeds for the variant and the
#reference. Score = mean Pearson over the site's head views (100 + 500 m; GlattPark 500).
B <- 1000
set.seed(1)
idx <- matrix(sample.int(NREP, NREP * B, replace = TRUE), B)
scorers <- lapply(stats::setNames(CONTEXTS, CONTEXTS), scorerFor)
stored <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx)
  lapply(stats::setNames(c(TT_TYPES, "nearbyRandom"), c(TT_TYPES, "nearbyRandom")),
         function(t) runsFrom(ctx, t, seq_len(NREP))))
scoreSet <- function(ctx, rr){ sc <- scorers[[ctx]](rr); c(score = mean(sc), sc) }
boot <- function(ctx, rr) t(vapply(seq_len(B), function(b) scoreSet(ctx, rr[idx[b, ]]),
                                   numeric(length(scoreSet(ctx, rr[1:2])))))
refBoot <- list()
for(ctx in CONTEXTS) for(t in names(stored[[ctx]])) refBoot[[ctx]][[t]] <- boot(ctx, stored[[ctx]][[t]])

tab <- list()
for(ctx in CONTEXTS) for(t in TT_TYPES) for(v in setdiff(VARS, "check")){
  i  <- which(g$ctx == ctx & g$type == t & g$variant == v)
  if(!length(i)) next   #a single-site fit has no runs on its own site
  rr <- res[i][order(g$rep[i])]
  if(!all(vapply(rr, function(r) identical(r$status, "done"), logical(1)))) next
  pt <- scoreSet(ctx, rr); bt <- boot(ctx, rr)
  ref <- scoreSet(ctx, stored[[ctx]][[t]])
  best <- c(nearby = scoreSet(ctx, stored[[ctx]]$nearby)[["score"]],
            nearbyRandom = scoreSet(ctx, stored[[ctx]]$nearbyRandom)[["score"]])
  bestT <- if(t == "nearby") "nearbyRandom" else names(best)[which.max(best)]
  dSelf <- bt[, "score"] - refBoot[[ctx]][[t]][, "score"]
  dBest <- bt[, "score"] - refBoot[[ctx]][[bestT]][, "score"]
  tab[[length(tab) + 1]] <- data.frame(
    ctx = ctx, type = TYPES[[t]]$label, variant = v, t(pt), ref = ref[["score"]],
    dSelf = pt[["score"]] - ref[["score"]], dSelf_lo = unname(stats::quantile(dSelf, .025)),
    dSelf_hi = unname(stats::quantile(dSelf, .975)),
    vsBest = TYPES[[bestT]]$label, best = best[[bestT]],
    dBest = pt[["score"]] - best[[bestT]], dBest_lo = unname(stats::quantile(dBest, .025)),
    dBest_hi = unname(stats::quantile(dBest, .975)),
    noGoal = mean(vapply(rr, function(r) r$nNoGoal / r$nAgents, 1)),
    secs = mean(vapply(rr, function(r) r$secs, 1)))
}
tab <- rbindFill(tab)
utils::write.csv(tab, file.path(TD, "tailored_test.csv"), row.names = FALSE)
#the mean over the three sites, per type and variant
agg <- stats::aggregate(cbind(score, ref, dSelf, dBest) ~ type + variant, tab, mean)
agg$sitesWorseThanBest  <- stats::aggregate(dBest_hi ~ type + variant, tab, function(x) sum(x < 0))$dBest_hi
agg$sitesBetterThanBest <- stats::aggregate(dBest_lo ~ type + variant, tab, function(x) sum(x > 0))$dBest_lo
agg$sitesBetterThanSelf <- stats::aggregate(dSelf_lo ~ type + variant, tab, function(x) sum(x > 0))$dSelf_lo
agg$sitesWorseThanSelf  <- stats::aggregate(dSelf_hi ~ type + variant, tab, function(x) sum(x < 0))$dSelf_hi
agg$nSites <- stats::aggregate(score ~ type + variant, tab, length)$score
#the rule needs all three sites
agg$passes <- agg$type == "Full" & agg$nSites == length(CONTEXTS) & agg$dBest >= 0.02 &
  agg$sitesWorseThanBest == 0
utils::write.csv(agg, file.path(TD, "tailored_test_mean.csv"), row.names = FALSE)
options(width = 230)
print(tab, digits = 3, row.names = FALSE)
print(agg[order(agg$type, -agg$dBest), ], digits = 3, row.names = FALSE)

#### the verdict under the rule, and the report ####

RULE <- "by >= .02 on the mean over the 3 sites, no site significantly worse"
#which sites a variant is significantly better than the best baseline at
better <- function(v){
  s <- tab$ctx[tab$type == "Full" & tab$variant == v & tab$dBest_lo > 0]
  if(length(s)) paste(s, collapse = ", ") else "none"
}
#the best passing variant of a family, NA if none passes
bestOf <- function(pat){
  a <- agg[agg$type == "Full" & grepl(pat, agg$variant) & agg$passes, ]
  if(nrow(a)) a$variant[which.max(a$dBest)] else NA_character_
}
vP <- bestOf("^fitPooled"); vS <- bestOf("^fitSite"); vL <- bestOf("^loso")
verdict <- if(!is.na(vP)) sprintf("DULN creation is a limit: one survey-tailored weighting (%s) lifts Full past the best baseline %s (significantly better at: %s).", vP, RULE, better(vP)) else
  if(!is.na(vS)) sprintf("The preferences are site-specific: only a per-site weighting lifts Full past the best baseline %s.", RULE) else
  sprintf("The dynamics are the limit: no tailored DULN lifts Full past the best baseline %s.", RULE)
#out of sample: every site simulated with the weights fitted on the other two
verdictLoso <- if(!any(grepl("^loso", agg$variant))) NULL else
  if(!is.na(vL)) sprintf("OUT OF SAMPLE (leave one site out): weights fitted on two sites (%s) lift Full on the third past the best baseline %s; significantly better at: %s.", vL, RULE, better(vL)) else
  sprintf("OUT OF SAMPLE (leave one site out): no strength lifts Full on the held-out site past the best baseline %s.", RULE)
logf("verdict: %s", verdict)
if(!is.null(verdictLoso)) logf("verdict: %s", verdictLoso)

#fitted on ONE site, used on the other two: every (training, test) pair against
#today's Full and against the best baseline at the test site
single <- tab[tab$type == "Full" & grepl("^from_", tab$variant), ]
singleSum <- NULL
if(nrow(single)){
  single$train <- sub("^from_(.*)_s[0-9.]+$", "\\1", single$variant)
  single$s <- sub("^.*_s", "", single$variant)
  singleSum <- do.call(rbind, lapply(split(single, single$s), function(d) data.frame(
    strength = d$s[1], pairs = nrow(d), mean_dSelf = mean(d$dSelf), mean_dBest = mean(d$dBest),
    betterThanTodaysFull = sum(d$dSelf_lo > 0), worseThanTodaysFull = sum(d$dSelf_hi < 0),
    betterThanBest = sum(d$dBest_lo > 0), worseThanBest = sum(d$dBest_hi < 0))))
  utils::write.csv(single, file.path(TD, "single_site_transfer.csv"), row.names = FALSE)
  print(single[, c("train", "ctx", "s", "score", "ref", "dSelf", "dSelf_lo", "dSelf_hi", "best", "dBest", "dBest_lo", "dBest_hi")],
        digits = 3, row.names = FALSE)
  print(singleSum, digits = 3, row.names = FALSE)
}

fit <- readRDS(file.path(TD, "fit.rds"))
md <- function(d, digits = 3){
  d <- as.data.frame(d)
  for(cn in names(d)) if(is.numeric(d[[cn]])) d[[cn]] <- formatC(d[[cn]], digits = digits, format = "f")
  c(paste0("| ", paste(names(d), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(d)), collapse = "|"), "|"),
    apply(d, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |")))
}
wt <- fit$weights
wt <- wt[order(-abs(wt$w_pooled - wt$xlsx)), ]
lakes <- do.call(rbind, lapply(c("glatt", "wigger"), function(b){
  f <- file.path(TD, "layers", paste0("lakes_", b, ".csv"))
  if(file.exists(f)) cbind(box = b, utils::read.csv(f)) else NULL
}))
rep <- c(
  "# Survey-tailored DULN (data-raw/tailor_duln.R, data-raw/test_tailored_duln.R)", "",
  sprintf("Reference: `%s` (reach 0.33, alpha 2, straight 1, manual areas). %d paired reps per variant.", REF_OUT, NREP), "",
  "## Verdict", "", verdict, "", if(!is.null(verdictLoso)) c(verdictLoso, "") else NULL,
  paste("IN SAMPLE: the fit* variants were fitted to the same survey cells they are scored on, so",
        "they show what a better map COULD do, not what this one would do elsewhere. lakeBand_x3 was",
        "not fitted (the lake and large-river weight x 3, with the loop band)."), "",
  "## Full and Nearby with each variant (mean over the 3 sites)", "",
  md(agg[order(agg$type, -agg$dBest), ]), "",
  if(!is.null(singleSum)) c("## Fitted on ONE site, used on the other two (Full)", "",
    "Every (training site -> test site) pair, against today's Full (dSelf) and the best baseline at the test site (dBest).", "",
    md(single[, c("train", "ctx", "s", "score", "ref", "dSelf", "dSelf_lo", "dSelf_hi", "vsBest", "best", "dBest", "dBest_lo", "dBest_hi")]), "",
    md(singleSum), "") else NULL,
  "## Per site", "", md(tab[, c("ctx", "type", "variant", "score", "ref", "dSelf", "dSelf_lo", "dSelf_hi",
                               "vsBest", "best", "dBest", "dBest_lo", "dBest_hi", "noGoal")]), "",
  "## Static bound (no ABM): NR re-weighted by the fitted elements", "",
  md(rbindFill(c(list(fit$static),
                 lapply(c("fit_loso.rds", "fit_single.rds"), function(f)
                   if(file.exists(file.path(TD, f))) readRDS(file.path(TD, f))$static else NULL)))), "",
  "## Today's DULN alone, beyond NR (log usage per SD)", "", md(fit$current), "",
  "## Weights (Nature walkers, DULN units; largest pooled changes first)", "",
  md(wt[, c("element", "xlsx", "w_pooled", paste0("w_", CONTEXTS), paste0("share_", CONTEXTS))]), "",
  "## Lake bands", "", if(!is.null(lakes)) md(lakes, 1) else "none", "")
writeLines(rep, file.path(TD, "report.md"))
logf("tailored DULN written to %s", TD)
