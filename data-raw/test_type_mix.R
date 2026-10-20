#### The agent type mix, from the Glatt survey ####
#
# The engine draws every agent's type from one fixed mix (R/generatePopulation.R):
# Nature walkers 23 %, Social walkers 17 %, dog walkers 27 %, e-bikers 9 %, sports
# bikers 6 %, joggers 23 %. The Glatt survey's routes (the full FilBleu export,
# typed as in data-raw/tailor_duln_types.R) have another one: Nature walkers 45 %,
# Social walkers 11 %, dog walkers 6 %, e-bikers 2 %, sports bikers 19 %, joggers
# 17 %. Only Glatt's survey has activities; Wigger and GlattPark get Glatt's mix
# (a transfer, as a DULN fitted there would be).
#
# Runs (20 reps, the last validation's settings; Glatt scored on the corrected survey):
#   MIX_full    Full, today's DULN, the survey mix
#   MIX_nr      Nearby-Random, the survey mix (the mix moves the baseline too: speed
#               and outing length are the type's)
#   MIX_types   Full, the per-type Glatt weights (T_types_s0.5), the survey mix
# against the stored runs (the engine's mix) and against each other. A new mix
# re-draws the types, so the random stream after that differs from the stored run
# of the same seed: the comparison is unpaired in effect (the bootstrap still
# resamples the replicates).
#
#   Rscript data-raw/test_type_mix.R
#
# VFT_TY_REPS     reps (default 20)
# VFT_TY_WORKERS  daemons (default 13)

Sys.setenv(VFT_TY_WORKER = "1")   #tailor_duln_types.R's definitions only
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "tailor_duln_types.R"))
#the daemons source tailor_duln_types.R itself (runGrid())
TY_SCRIPT <- file.path(.repo, "data-raw", "tailor_duln_types.R")
if(is.null(TY_MIXES$survey)) stop("no survey mix: run data-raw/tailor_duln_types.R (stage survey) first")
MX <- tyPath("mix")
dir.create(MX, recursive = TRUE, showWarnings = FALSE)
logf("the survey mix: %s", paste(sprintf("%s %.1f %%", names(TY_MIXES$survey), 100 * TY_MIXES$survey), collapse = ", "))

#### the runs ####

RUNS <- list(MIX_full  = list(type = "full", vdir = REF_OUT),
             MIX_nr    = list(type = "nearbyRandom", vdir = REF_OUT),
             MIX_types = list(type = "full", vdir = tyVariantDir("T_types_s0.5")))
f <- file.path(MX, "mix_runs.rds")
g <- do.call(rbind, lapply(names(RUNS), function(v) data.frame(
  variant = v, ctx = rep(CONTEXTS, each = TY_REPS), rep = rep(seq_len(TY_REPS), length(CONTEXTS)),
  type = RUNS[[v]]$type, vdir = RUNS[[v]]$vdir, typeArea = FALSE, mixId = "survey", stringsAsFactors = FALSE)))
g <- g[order(match(g$ctx, c("glatt", "glattPark", "wigger")), g$variant, g$rep), ]
res <- vector("list", nrow(g)); todo <- seq_len(nrow(g))
if(file.exists(f)){
  old <- readRDS(f); m <- match(paste(g$variant, g$ctx, g$rep), paste(old$grid$variant, old$grid$ctx, old$grid$rep))
  res[!is.na(m)] <- old$res[m[!is.na(m)]]
  todo <- which(!vapply(res, function(r) identical(r$status, "done"), logical(1)))
}
logf("type mix: %d runs, %d to do", nrow(g), length(todo))
if(length(todo)) res[todo] <- runGrid(g[todo, ], "type mix")
saveRDS(list(grid = g, res = res), f)
bad <- which(!vapply(res, function(r) identical(r$status, "done"), logical(1)))
if(length(bad)) stop(length(bad), " runs failed, first: ", res[[bad[1]]]$msg)

#the mix the runs actually had
realised <- do.call(rbind, lapply(names(RUNS), function(v){
  i <- which(g$variant == v)
  a <- Reduce(`+`, lapply(res[i], `[[`, "agentTypes"))
  data.frame(variant = v, t(round(100 * a / sum(a), 1)))
}))
logf("the realised mix (%% of agents):")
print(realised, row.names = FALSE)

#### the scores ####

B <- 1000; set.seed(1)
idx <- matrix(sample.int(TY_REPS, TY_REPS * B, replace = TRUE), B)
scorers <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx)
  if(ctx == "glatt") withOut(RS, scorerFor(ctx)) else scorerFor(ctx))
score <- function(ctx, rr) mean(scorers[[ctx]](rr))
boot  <- function(ctx, rr) vapply(seq_len(B), function(b) score(ctx, rr[idx[b, ]]), 1)
runsOf <- function(v, ctx){ i <- which(g$variant == v & g$ctx == ctx); res[i][order(g$rep[i])] }
typed <- readRDS(tyPath("typed_runs.rds"))
typesDefault <- function(ctx){ i <- which(typed$grid$variant == "T_types_s0.5" & typed$grid$ctx == ctx)
  typed$res[i][order(typed$grid$rep[i])] }
sets <- function(ctx) list(
  `Full (engine mix)`          = runsFrom(ctx, "full", seq_len(TY_REPS)),
  `Nearby-Random (engine mix)` = runsFrom(ctx, "nearbyRandom", seq_len(TY_REPS)),
  `Nearby (engine mix)`        = runsFrom(ctx, "nearby", seq_len(TY_REPS)),
  `Full + per-type weights (engine mix)` = typesDefault(ctx),
  `Full (survey mix)`          = runsOf("MIX_full", ctx),
  `Nearby-Random (survey mix)` = runsOf("MIX_nr", ctx),
  `Full + per-type weights (survey mix)` = runsOf("MIX_types", ctx))
CMP <- list(c("Full (survey mix)", "Full (engine mix)"),
            c("Nearby-Random (survey mix)", "Nearby-Random (engine mix)"),
            c("Full (survey mix)", "Nearby-Random (survey mix)"),
            c("Full (engine mix)", "Nearby-Random (engine mix)"),
            c("Full + per-type weights (survey mix)", "Full (engine mix)"),
            c("Full + per-type weights (survey mix)", "Full + per-type weights (engine mix)"),
            c("Full + per-type weights (survey mix)", "Nearby-Random (survey mix)"))
sc <- list(); df <- list()
for(ctx in CONTEXTS){
  S <- sets(ctx)
  bt <- lapply(S, function(rr) boot(ctx, rr))
  sc[[ctx]] <- data.frame(ctx = ctx, run = names(S),
                          do.call(rbind, lapply(S, function(rr) scorers[[ctx]](rr))),
                          score = vapply(S, function(rr) score(ctx, rr), 1), row.names = NULL, check.names = FALSE)
  for(cm in CMP){
    d <- bt[[cm[1]]] - bt[[cm[2]]]
    df[[length(df) + 1]] <- data.frame(ctx = ctx, run = cm[1], vs = cm[2],
      diff = score(ctx, S[[cm[1]]]) - score(ctx, S[[cm[2]]]),
      lo = unname(stats::quantile(d, .025)), hi = unname(stats::quantile(d, .975)))
  }
}
sc <- rbindFill(sc); df <- do.call(rbind, df)
df$sig <- ifelse(df$lo > 0, "better", ifelse(df$hi < 0, "worse", ""))
utils::write.csv(sc, file.path(MX, "mix_scores.csv"), row.names = FALSE)
utils::write.csv(df, file.path(MX, "mix_differences.csv"), row.names = FALSE)
options(width = 220)
print(sc, digits = 3, row.names = FALSE)
print(df, digits = 3, row.names = FALSE)

#per type at Glatt: does each type's map change with the mix (it should not much:
#a type's map is its own agents')
sty <- stageSurveyTypes()
G <- withOut(RS, buildGrid("glatt", readRDS(aoiFile("glatt")), readRDS(surveyFile("glatt")), readRDS(edgesFile("glatt"))))
vn <- HEAD_VIEWS("glatt")
surT <- lapply(stats::setNames(TYPES7, TYPES7), function(k)
  lapply(stats::setNames(vn, vn), function(v) surveyView(G, G$views[[v]], sty$base[[k]])))
scoreT <- function(rr, k){
  x <- numeric(terra::ncell(G$t100))
  for(r in rr){ b <- r$byType[[k]]; x[b$cells] <- x[b$cells] + b$counts }
  if(!any(x > 0)) return(NA_real_)
  mean(vapply(vn, function(v) stats::cor(as.vector(G$views[[v]]$Msim %*% x), surT[[k]][[v]]), 1))
}
cr <- readRDS(tyPath("capture_runs.rds"))
todayFull <- cr$res[cr$grid$type == "full" & !cr$grid$typeArea]
pt <- do.call(rbind, lapply(TYPES7, function(k) data.frame(type = k, label = TYPE_LABEL[[k]],
  `Full (engine mix)` = scoreT(todayFull, k), `Full (survey mix)` = scoreT(runsOf("MIX_full", "glatt"), k),
  `Full + per-type weights (survey mix)` = scoreT(runsOf("MIX_types", "glatt"), k), check.names = FALSE)))
utils::write.csv(pt, file.path(MX, "mix_per_type_glatt.csv"), row.names = FALSE)
print(pt, digits = 3, row.names = FALSE)
logf("type mix written to %s", MX)
