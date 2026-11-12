#### The survey-based models against Strava's heat in 12 areas they never saw ####
#
# The Strava heatmap loses to the survey as a CALIBRATION target for the DULN
# (data-raw/tailor_duln_strava.R, tailor_duln_strava_areas.R). Here it is a TEST set
# instead: the 12 Swiss areas of data-raw/tailor_duln_osm.R keep >= 2 km from Glatt,
# GlattPark and Wigger, so nothing fitted on the surveys - the DULN weights (E2c), the
# area choice (RUM-time), its descriptors - has seen them. Each model is simulated there
# and its usage per Strava group compared with that group's heat, cell by cell:
#   the cells      the element extract's: inside the areas, no lakes (as the Strava fits)
#   the target     groupTarget(): the normal scores of the peak heat per group (Hike,
#                  Walk, Ride + Gravel, E-Bike, Run), within the area or its z14 tile
#   the score      Spearman between the simulated passages of the group's agent types
#                  (mean over the replicates) and the target; an area x group whose group
#                  has heat on < SV_MIN_HEAT of the cells is left out
#   the inference  the 12 areas are the sample: per group, the mean over the areas of a
#                  model's score minus another's, with a t interval over the areas
# The heat is a brightness scale of athletes who record, not counts of everyone: a rank
# check on where a group goes, not on how many go.
#
# The runs: simTyped() (data-raw/tailor_duln_types.R) on <VFT_SV_DIR>, a validation folder
# of the 12 areas carrying the DULN variant (data-raw/apply_duln_variant.R). No survey
# exists there, so every model gets the same stand-in population: the engine's 800
# agents from the census residents, GLATT's survey type mix and stated durations
# (VFT_VAL_TYPEMIX / VFT_VAL_DURSRC, as Wigger takes Glatt's mix).
#
#   VFT_SV_DIR=<folder> VFT_VAL_RUM_TIME=<coefs> ... Rscript data-raw/validate_strava_areas.R
#
# VFT_SV_STAGES  sim, score (default both), scales (the scores at 100-500 m and the map data, see below)
# VFT_SV_DIR     the areas' validation folder with the variant (required)
# VFT_SV_MODELS  the validation types to run (default nearbyRandom, nearby, fullRumTime)
# VFT_SV_REPS    replicates per area and model (default 10)
# VFT_SV_OUT     output (default <VFT_SV_DIR>/strava_validation)
# VFT_SV_REF     the models every other one is contrasted with (default nearbyRandom,fullRumTime)

IS_SV_WORKER <- identical(Sys.getenv("VFT_TY_WORKER"), "1")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
SV_SCRIPT <- file.path(.repo, "data-raw", "validate_strava_areas.R")
if(!IS_SV_WORKER){
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if(length(f) && grepl("validate_strava_areas", f[1])) SV_SCRIPT <- normalizePath(f[1], winslash = "/")
}
.svStages <- Sys.getenv("VFT_SV_STAGES", "sim,score")
SV_DIR <- Sys.getenv("VFT_SV_DIR", "")
if(!nzchar(SV_DIR)) stop("set VFT_SV_DIR")
#tailor_duln.R sets VFT_VAL_ALPHA=2 when it is sourced; tailor_duln_activities.R then sets
#CORR$alpha back to 5, so the runs ran at 5 but their saved settings said "2". VFT_SV_ALPHA
#(default 5) is set explicitly after the chain, env and CORR alike, in every worker too.
.svAlpha <- Sys.getenv("VFT_SV_ALPHA", "5")
Sys.setenv(VFT_SA_STAGES = "none", VFT_VAL_OUT = SV_DIR)
source(file.path(.repo, "data-raw", "tailor_duln_strava_areas.R"))
Sys.setenv(VFT_VAL_ALPHA = .svAlpha); CORR$alpha <- as.numeric(.svAlpha)
TY_SCRIPT <- SV_SCRIPT
SV_STAGES <- strsplit(.svStages, ",", fixed = TRUE)[[1]]
SV_MODELS <- envList("VFT_SV_MODELS", c("nearbyRandom", "nearby", "fullRumTime"))
SV_REPS   <- as.integer(envNum("VFT_SV_REPS", 10))
SV_OUT    <- Sys.getenv("VFT_SV_OUT", file.path(SV_DIR, "strava_validation"))
SV_REF    <- envList("VFT_SV_REF", c("nearbyRandom", "fullRumTime"))
SV_MIN_HEAT <- 0.05
svPath <- function(...){ p <- file.path(SV_OUT, ...); dir.create(dirname(p[1]), recursive = TRUE, showWarnings = FALSE); p }
runsFile <- function(ctx, type) svPath("runs", paste0(type, "_", ctx, ".rds"))

#### stage sim ####

stageSimAreas <- function(){
  if(!all(SV_MODELS %in% names(TYPES))) stop("unknown type(s) ", paste(setdiff(SV_MODELS, names(TYPES)), collapse = ", "))
  for(type in SV_MODELS){
    todo <- SA_AREAS[!file.exists(vapply(SA_AREAS, runsFile, "", type = type))]
    if(!length(todo)) next
    t0 <- proc.time()[["elapsed"]]
    g <- data.frame(ctx = rep(todo, each = SV_REPS), type = type, rep = rep(seq_len(SV_REPS), length(todo)),
                    vdir = SV_DIR, typeArea = FALSE, stringsAsFactors = FALSE)
    res <- runGrid(g, type)
    for(ctx in todo){
      r <- res[g$ctx == ctx]
      ok <- vapply(r, function(x) identical(x$status, "done"), TRUE)
      if(!all(ok)){ logf("sim %s %s: %d of %d runs FAILED (%s) - not saved", type, ctx, sum(!ok), length(ok), r[[which(!ok)[1]]]$msg); next }
      saveRDS(list(grid = g[g$ctx == ctx, ], res = r, settings = Sys.getenv(c("VFT_VAL_RUM_TIME", "VFT_VAL_DURSRC", "VFT_VAL_TYPEMIX",
                                                                           "VFT_VAL_OUTING", "VFT_VAL_REACH", "VFT_VAL_ALPHA"))),
              runsFile(ctx, type))
    }
    logf("sim %s: %d areas x %d reps in %.1f min (%.0f s a run; agents without an area %.0f %%)", type, length(todo), SV_REPS,
         (proc.time()[["elapsed"]] - t0) / 60, mean(vapply(res, function(x) if(is.null(x$secs)) NA_real_ else x$secs, 1), na.rm = TRUE),
         100 * mean(vapply(res, function(x) if(is.null(x$nNoGoal)) NA_real_ else x$nNoGoal / x$nAgents, 1), na.rm = TRUE))
  }
}

#### stage score ####

stageScoreAreas <- function(){
  rows <- list(); split <- list()
  for(ctx in SA_AREAS){
    se <- readRDS(saExtractFile(ctx))
    ex <- inVal(readRDS(extractFile(ctx)))
    stopifnot(identical(se$cells, ex$cells))
    ea <- igraph::edge_attr(inVal(readRDS(netFile(ctx)))$net)
    ag <- inVal(cellAgg(ctx, ea))
    se$Hc <- se$Hc$peak
    tg <- groupTarget(se, seq_along(se$cells))
    heat <- sapply(STRAVA5, function(k) mean(rowSums(sapply(ST_SPORTS[[k]], function(s) se$Hc[[s]])) > 0))
    for(type in SV_MODELS){
      f <- runsFile(ctx, type)
      if(!file.exists(f)) next
      runs <- readRDS(f)$res
      simOf <- function(rr, k) Reduce(`+`, lapply(names(ST_OF)[ST_OF == k], function(ty) typeBase(rr, ag$G, ty)[ex$cells]))
      for(k in STRAVA5){
        s <- simOf(runs, k)
        rows[[length(rows) + 1]] <- data.frame(area = ctx, group = k, model = type, cells = length(s), heatShare = heat[[k]],
          agentsPerRep = sum(vapply(runs, function(x) sum(x$agentTypes[names(ST_OF)[ST_OF == k]]), 1)) / length(runs),
          spearman = suppressWarnings(stats::cor(s, tg[, k], method = "spearman")),
          #the replicates' own noise: the two halves of the runs against each other
          simHalves = if(length(runs) >= 4){ h <- seq_along(runs) %% 2 == 0
            suppressWarnings(stats::cor(simOf(runs[h], k), simOf(runs[!h], k), method = "spearman")) } else NA_real_)
      }
    }
  }
  sc <- do.call(rbind, rows)
  sc$used <- sc$heatShare >= SV_MIN_HEAT & is.finite(sc$spearman)
  utils::write.csv(sc, svPath("scores_long.csv"), row.names = FALSE)
  u <- sc[sc$used, ]
  #per model and group: the mean over the areas
  ms <- stats::aggregate(spearman ~ model + group, u, mean)
  wide <- stats::reshape(ms, idvar = "model", timevar = "group", direction = "wide")
  names(wide) <- sub("^spearman\\.", "", names(wide))
  wide <- wide[match(intersect(SV_MODELS, wide$model), wide$model), c("model", intersect(STRAVA5, names(wide)))]
  wide$mean <- rowMeans(wide[, -1, drop = FALSE])
  utils::write.csv(wide, svPath("scores_by_group.csv"), row.names = FALSE)
  options(width = 200)
  logf("Spearman of the simulated usage with the Strava heat, mean over the areas (%d areas; an area x group counts with heat on >= %.0f %% of its cells):",
       length(unique(u$area)), 100 * SV_MIN_HEAT)
  print(format(wide, digits = 3), row.names = FALSE)
  nA <- stats::aggregate(area ~ group, unique(u[, c("area", "group")]), length)
  logf("areas per group: %s", paste(sprintf("%s %d", nA$group, nA$area), collapse = ", "))
  #the contrasts: model minus reference, per group and over the groups (an area's mean over
  #its groups), the areas as the sample
  ct <- list()
  for(ref in intersect(SV_REF, SV_MODELS)) for(m in setdiff(SV_MODELS, ref)){
    a <- u[u$model == m, c("area", "group", "spearman")]; b <- u[u$model == ref, c("area", "group", "spearman")]
    d <- merge(a, b, by = c("area", "group"), suffixes = c("", ".ref"))
    if(!nrow(d)) next
    d$diff <- d$spearman - d$spearman.ref
    one <- function(x, lab){
      n <- length(x); se <- stats::sd(x) / sqrt(n)
      data.frame(model = m, ref = ref, group = lab, areas = n, diff = mean(x), lo = mean(x) - stats::qt(.975, n - 1) * se,
                 hi = mean(x) + stats::qt(.975, n - 1) * se, areasBetter = sum(x > 0),
                 p_t = if(n > 2) stats::t.test(x)$p.value else NA_real_)
    }
    for(k in STRAVA5) if(sum(d$group == k) > 2) ct[[length(ct) + 1]] <- one(d$diff[d$group == k], k)
    ct[[length(ct) + 1]] <- one(tapply(d$diff, d$area, mean), "all groups")
  }
  ct <- do.call(rbind, ct)
  utils::write.csv(ct, svPath("contrasts.csv"), row.names = FALSE)
  logf("contrasts (model - reference; mean over the areas, 95 %% t interval, areas where the model is better):")
  print(format(ct, digits = 2), row.names = FALSE)
  #the per-area table over the groups
  pa <- stats::aggregate(spearman ~ area + model, u, mean)
  paw <- stats::reshape(pa, idvar = "area", timevar = "model", direction = "wide")
  names(paw) <- sub("^spearman\\.", "", names(paw))
  utils::write.csv(paw, svPath("scores_by_area.csv"), row.names = FALSE)
  logf("per area (mean over its groups):"); print(format(paw, digits = 3), row.names = FALSE)
  invisible(list(scores = sc, byGroup = wide, contrasts = ct))
}

#### stage scales: the scores at 100-500 m and the data for the maps page ####
#
# Per area, per Strava group and per cell size (blocks of f x f base cells of 100 m,
# aggregated from the top-left as terra::aggregate and the survey comparisons do): the
# mean heat of the block's cells against the mean simulated usage of the group's agent
# types (over the replicates), over the blocks holding at least one of the extract's cells.
#   spearman  rank agreement (the heat is a brightness scale, not counts)
#   auc       how well the simulated usage separates the busiest quarter of the blocks
#             (heat above its 75th percentile) from the rest
# An area x group counts when the group has heat on >= SV_MIN_HEAT of the cells (as score).
# VFT_SV_RUNDIRS: further strava_validation folders searched for a model's runs (the
# GlattPark-tailored models are simulated in a folder of their own, with that DULN).
# Writes <SV_OUT>/scales_long.csv, scales_contrasts.csv and scales_maps.rds.
SV_SCALES  <- as.integer(envList("VFT_SV_SCALES", c("1", "2", "3", "4", "5")))
SV_RUNDIRS <- envList("VFT_SV_RUNDIRS", character(0))
runsAny <- function(ctx, type){
  f <- c(runsFile(ctx, type), file.path(SV_RUNDIRS, "runs", paste0(type, "_", ctx, ".rds")))
  f <- f[file.exists(f)]
  if(length(f)) f[1] else NA_character_
}
aucOf <- function(score, pos){
  n1 <- sum(pos); n0 <- sum(!pos)
  if(n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(score)
  (sum(r[pos]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
stageScales <- function(){
  rows <- list(); maps <- list(); stamps <- list()
  for(ctx in SA_AREAS){
    se <- readRDS(saExtractFile(ctx))
    ex <- inVal(readRDS(extractFile(ctx)))
    stopifnot(identical(se$cells, ex$cells))
    ea <- igraph::edge_attr(inVal(readRDS(netFile(ctx)))$net)
    ag <- inVal(cellAgg(ctx, ea))
    B <- ag$G$base; nr <- terra::nrow(B); nc <- terra::ncol(B)
    stopifnot(abs(terra::res(B)[1] - 100) < 1e-6)
    #the groups' heat on the extract's cells: each sport's peak heat, Ride + GravelRide averaged
    H <- sapply(STRAVA5, function(k) rowMeans(cbind(sapply(ST_SPORTS[[k]], function(s) se$Hc$peak[[s]]))))
    heatShare <- colMeans(H > 0)
    used <- STRAVA5[heatShare >= SV_MIN_HEAT]
    sims <- list()
    for(type in SV_MODELS){
      f <- runsAny(ctx, type)
      if(is.na(f)){ logf("scales %s: no %s runs - left out", ctx, type); next }
      rr <- readRDS(f); runs <- rr$res
      if(!all(vapply(runs, function(x) identical(x$status, "done"), TRUE))) stop(ctx, " ", type, ": a run failed")
      stamps[[length(stamps) + 1]] <- data.frame(area = ctx, model = type, reps = length(runs), file = f,
        t(rr$settings[c("VFT_VAL_RUM_TIME", "VFT_VAL_DURSRC", "VFT_VAL_TYPEMIX", "VFT_VAL_OUTING", "VFT_VAL_REACH")]),
        agents = mean(vapply(runs, function(x) x$nAgents, 1)), stringsAsFactors = FALSE)
      sims[[type]] <- sapply(STRAVA5, function(k) Reduce(`+`, lapply(names(ST_OF)[ST_OF == k], function(ty)
        typeBase(runs, ag$G, ty)[ex$cells])) / length(runs))
    }
    for(f in SV_SCALES){
      A <- if(f == 1) Matrix::Diagonal(nr * nc) else aggMatrix(nr, nc, f)
      A <- A[, ex$cells, drop = FALSE]
      nIn <- Matrix::rowSums(A); blk <- which(nIn > 0)
      Mf <- Matrix::Diagonal(x = 1 / nIn[blk]) %*% A[blk, , drop = FALSE]
      Hb <- as.matrix(Mf %*% H)
      for(type in names(sims)){
        Sb <- as.matrix(Mf %*% sims[[type]])
        for(k in used){
          h <- Hb[, k]; s <- Sb[, k]
          rows[[length(rows) + 1]] <- data.frame(area = ctx, scale_m = 100 * f, model = type, group = k, blocks = length(blk),
            heatShare = heatShare[[k]], spearman = suppressWarnings(stats::cor(s, h, method = "spearman")), pearson = suppressWarnings(stats::cor(s, h)),
            auc = aucOf(s, h > stats::quantile(h, .75)))
        }
      }
    }
    #the map data: the 100 m values on the extract's cells (the page aggregates them)
    aoi <- inVal(readRDS(aoiFile(ctx)))
    maps[[ctx]] <- list(ctx = ctx, nrows = nr, ncols = nc, ext = as.vector(terra::ext(B)), crs = terra::crs(B, describe = TRUE)$code,
                        cells = ex$cells, heat = H, sims = sims, used = used, heatShare = heatShare,
                        polys = sf::st_transform(sf::st_geometry(aoi$polys), 2056), letters = aoi$polys$AOI)
    logf("scales %s: %d cells, groups %s, %d models", ctx, length(ex$cells), paste(used, collapse = " "), length(sims))
  }
  sc <- do.call(rbind, rows)
  utils::write.csv(sc, svPath("scales_long.csv"), row.names = FALSE)
  st <- do.call(rbind, stamps)
  utils::write.csv(st, svPath("scales_settings.csv"), row.names = FALSE)
  saveRDS(maps, svPath("scales_maps.rds"))
  #per area: the mean over its groups; then the areas as the sample
  pa <- stats::aggregate(cbind(spearman, auc, pearson) ~ area + scale_m + model, sc, mean, na.action = stats::na.pass)
  ct <- list()
  for(ref in SV_MODELS) for(m in setdiff(SV_MODELS, ref)) for(s in unique(pa$scale_m)) for(what in c("spearman", "auc", "pearson")){
    a <- pa[pa$model == m & pa$scale_m == s, c("area", what)]; b <- pa[pa$model == ref & pa$scale_m == s, c("area", what)]
    d <- merge(a, b, by = "area"); x <- d[[2]] - d[[3]]; x <- x[is.finite(x)]; n <- length(x)
    if(n < 3) next
    se <- stats::sd(x) / sqrt(n)
    ct[[length(ct) + 1]] <- data.frame(model = m, ref = ref, scale_m = s, score = what, areas = n, diff = mean(x),
                                       lo = mean(x) - stats::qt(.975, n - 1) * se, hi = mean(x) + stats::qt(.975, n - 1) * se,
                                       areasBetter = sum(x > 0), p_t = stats::t.test(x)$p.value)
  }
  ct <- do.call(rbind, ct)
  utils::write.csv(ct, svPath("scales_contrasts.csv"), row.names = FALSE)
  options(width = 200)
  mw <- stats::aggregate(cbind(spearman, auc) ~ scale_m + model, pa, mean)
  logf("mean over the areas (each area's mean over its groups):")
  print(format(stats::reshape(mw[, c("scale_m", "model", "spearman")], idvar = "model", timevar = "scale_m", direction = "wide"), digits = 3), row.names = FALSE)
  print(format(stats::reshape(mw[, c("scale_m", "model", "auc")], idvar = "model", timevar = "scale_m", direction = "wide"), digits = 3), row.names = FALSE)
  invisible(list(scores = sc, contrasts = ct))
}

if(!IS_SV_WORKER){
  if("sim" %in% SV_STAGES) stageSimAreas()
  if("score" %in% SV_STAGES) stageScoreAreas()
  if("scales" %in% SV_STAGES) stageScales()
  logf("validate_strava_areas: done, %s", SV_OUT)
}
