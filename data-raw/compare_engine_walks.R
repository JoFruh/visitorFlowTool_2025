#### Why do the simulated walks look like single lines, not hotspots? ####
#
# The same validation runs (Full, Nearby-Random, Full-Random; same areas, same
# respondents, same seeds) on three engines, side by side with the survey:
#
#   published-era  commit 9e24404, the engine before the two route-choice fixes of
#                  7800cec (memory: vft-abm-speed-pass-2026-09). Its speed pass was
#                  verified bit-identical to the engine the paper ran.
#   current        the engine as it is in the repository
#   corrected      current + data-raw/engine_corrections.R
#   no revisit     corrected, but a node the agent has passed keeps its normal
#                  weight (VFT_VAL_REVISIT=none) instead of 0.00001
#
# Each sits in its own VFT_VAL_OUT (pathUsageValidation_2026_diag_<engine>), made by
# validate_usage_surveys.R stage sim, the published-era one with VFT_VAL_REPO
# pointing at a worktree of 9e24404.
#
# Writes to VFT_DIAG_OUT (default pathUsageValidation_2026_diag): one figure per
# context in the style of Fig 5_6 (100 m, linear white-red-purple), and a table of
# Pearson r at the published scale, the share of the usage in the busiest 5 % of
# cells (how concentrated the map is), and the mean track length.

Sys.setenv(VFT_VAL_WORKER = "1")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "validate_usage_surveys.R"))
suppressPackageStartupMessages(library(ggplot2))

LOCAL   <- "C:/Users/frueh/Documents/Local Data"
ENGINES <- c(published = "pathUsageValidation_2026_diag2_old",
             corrected = "pathUsageValidation_2026_diag2_corrected",
             alpha2    = "pathUsageValidation_2026_diag2_alpha2",
             alpha5    = "pathUsageValidation_2026_diag2_alpha5")
ENGINE_LABEL <- c(published = "published-era engine (9e24404)", corrected = "corrected engine (alpha 0.5)",
                  alpha2 = "corrected, alpha 2", alpha5 = "corrected, alpha 5")
SHOW  <- c("full", "nearbyRandom", "fullRandom")
DIAG  <- Sys.getenv("VFT_DIAG_OUT", file.path(LOCAL, "pathUsageValidation_2026_diag2"))
dir.create(DIAG, showWarnings = FALSE, recursive = TRUE)

topShare <- function(x, p = 0.05){ x <- sort(x, decreasing = TRUE); sum(x[seq_len(ceiling(p * length(x)))]) / sum(x) }

rows <- list()
for(ctx in CONTEXTS){
  cfg <- CTX[[ctx]]
  OUT <- file.path(LOCAL, ENGINES[["corrected"]])
  aoi <- readRDS(aoiFile(ctx)); sv <- readRDS(surveyFile(ctx)); edg <- readRDS(edgesFile(ctx))
  G <- buildGrid(ctx, aoi, sv, edg)
  n100 <- terra::ncell(G$t100)
  vPub <- paste0(cfg$oldScale, "_all")
  v1 <- G$views[["1_all"]]
  sur1 <- surveyView(G, v1, sv$surveyBase); surP <- surveyView(G, G$views[[vPub]], sv$surveyBase)
  maps <- list(); mapsP <- list(); titles <- list()
  for(e in names(ENGINES)){
    OUT <- file.path(LOCAL, ENGINES[[e]])
    for(t in SHOW){
      k <- seq_len(N_REPS); k <- k[file.exists(runFile(ctx, aoi$stamp, t, k))]
      if(!length(k)) next
      runs <- lapply(k, function(i) readRDS(runFile(ctx, aoi$stamp, t, i)))
      x <- numeric(n100)
      for(r in runs) x[r$cells] <- x[r$cells] + r$counts
      m1 <- trans(as.vector(v1$Msim %*% x), cfg$simTransform)
      mP <- trans(as.vector(G$views[[vPub]]$Msim %*% x), cfg$simTransform)
      walk <- do.call(rbind, lapply(runs, `[[`, "walk"))
      rows[[length(rows) + 1]] <- data.frame(context = ctx, engine = e, type = TYPES[[t]]$label,
        reps = length(k), r_pub = stats::cor(mP, surP), r_100 = stats::cor(m1, sur1),
        top5_share = topShare(m1), survey_top5_share = topShare(sur1),
        track_edges = if(is.null(walk)) NA else mean(walk[, "trackEdges"]),
        engine_tag = unique(vapply(runs, function(r) paste(r$git, if(is.null(r$engine)) "engine" else r$engine), "")))
      maps[[paste(e, t)]] <- m1
      mapsP[[paste(e, t)]] <- mP
      titles[[paste(e, t)]] <- sprintf("%s\nr = %.2f, top 5 %% = %.0f %%", TYPES[[t]]$label,
                                       stats::cor(mP, surP), 100 * topShare(m1))
    }
  }
  if(!length(maps)) next
  #how alike the types are within an engine: Full against the random ones
  for(e in names(ENGINES)) for(t in setdiff(SHOW, "full")){
    a <- mapsP[[paste(e, "full")]]; b <- mapsP[[paste(e, t)]]
    if(is.null(a) || is.null(b)) next
    k <- which(vapply(rows, function(r) r$context == ctx && r$engine == e && r$type == TYPES[[t]]$label, TRUE))
    rows[[k]]$r_with_full <- stats::cor(a, b)
  }

  #### the figure: survey, then one row per engine ####
  grid <- gridFrom(v1$grid)
  panel <- function(vals, title){
    r <- terra::rast(grid); v <- rep(NA_real_, terra::ncell(r)); v[v1$idx] <- std01(vals)
    terra::values(r) <- v; names(r) <- "layer"
    ggplot() + ggtitle(title) + tidyterra::geom_spatraster(data = r, aes(fill = layer)) +
      scale_fill_gradientn(name = "Std.\nUsage", colours = c("white", "red", "purple4"),
                           na.value = "lightgrey", limits = c(0, 1)) +
      theme_bw(base_size = 8) +
      theme(legend.position = "none", axis.text = element_blank(), axis.ticks = element_blank(),
            plot.title = element_text(size = 7.5), panel.grid = element_blank())
  }
  blank <- ggplot() + theme_void()
  grobs <- list()
  for(e in names(ENGINES)){
    first <- if(e == names(ENGINES)[1]) panel(sur1, sprintf("Survey\ntop 5 %% = %.0f %%", 100 * topShare(sur1))) else blank
    row <- c(list(first), lapply(SHOW, function(t){
      key <- paste(e, t)
      if(is.null(maps[[key]])) blank else panel(maps[[key]], titles[[key]])
    }))
    grobs[[e]] <- gridExtra::arrangeGrob(grobs = row, nrow = 1,
      left = grid::textGrob(ENGINE_LABEL[[e]], rot = 90, gp = grid::gpar(fontsize = 9)))
  }
  g <- gridExtra::arrangeGrob(grobs = grobs, ncol = 1,
    top = grid::textGrob(sprintf("%s: the same runs on three engines (100 m maps; r at %d m)",
                                 cfg$label, G$views[[vPub]]$scale_m), gp = grid::gpar(fontsize = 11)))
  ggsave(file.path(DIAG, sprintf("fig_engines_%s.png", ctx)), ggpubr::as_ggplot(g),
         width = 22, height = 6.5 * length(ENGINES), units = "cm", dpi = 300, bg = "white")
}
tab <- rbindFill(rows)
utils::write.csv(tab, file.path(DIAG, "engines_walks.csv"), row.names = FALSE)
tab$r_pub <- round(tab$r_pub, 3); tab$r_100 <- round(tab$r_100, 3)
tab$top5_share <- round(100 * tab$top5_share, 1); tab$survey_top5_share <- round(100 * tab$survey_top5_share, 1)
tab$track_edges <- round(tab$track_edges, 1)
print(tab, row.names = FALSE)
