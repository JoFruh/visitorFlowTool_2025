#### Where the simulation and the survey disagree ####
#
# Maps and tables of the misalignment behind the survey validation, drawn from the
# runs of data-raw/validate_usage_surveys.R (same VFT_VAL_OUT, same switches).
# The values are exactly the ones its compare stage correlates: this file sources
# that script's definitions (no stage runs) and rebuilds the views from the runs.
#
#   VFT_VAL_OUT="C:/Users/frueh/Documents/Local Data/pathUsageValidation_2026_userAoi" \
#   Rscript data-raw/figures_misalignment.R
#
# The misalignment measure is the z-score difference d = z(sim) - z(survey) over
# the cells a correlation runs on. Pearson's r splits exactly into these:
#   sum(d^2) = 2 (n - 1) (1 - r)
# so d^2 / sum(d^2) is each cell's share of what keeps r below 1. Positive d: the
# simulation puts relatively more there than the respondents; negative: less.
#
# Writes to VFT_VAL_OUT/figures and /results:
#   misalign_<ctx>.png        survey, Full and their difference at the published
#                             scale; the 10 cells that cost r the most numbered
#   misalign_<ctx>_100m.png   the same at 100 m, for the paths (Wigger, Glatt)
#   misalign_types_<ctx>.png  the difference for every type: a gap that every type
#                             shares lies in the areas, network or survey, not in
#                             the behaviour
#   misalign_areas.png        each area's share of the usage, survey vs simulations
#   misalign_cells.csv, misalign_areas.csv

Sys.setenv(VFT_VAL_WORKER = "1")   #definitions only: no main(), no compile
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "validate_usage_surveys.R"))
suppressPackageStartupMessages(library(ggplot2))

TOP_N <- 10
COL <- list(ink = "#0b0b0b", ink2 = "#52514e", muted = "#9a9994", surface = "#fcfcfb",
            mid = "#f0efec", under = "#184f95", under2 = "#6da7ec",
            over = "#a3302f", over2 = "#e8807f",
            seq = c("#fcfcfb", "#cde2fb", "#86b6ef", "#2a78d6", "#184f95", "#0d366b"),
            survey = "#0b0b0b", full = "#2a78d6", nearbyRandom = "#eb6834")

#### the values the compare stage correlates ####

contextData <- function(ctx){
  cfg <- CTX[[ctx]]
  aoi <- readRDS(aoiFile(ctx)); sv <- readRDS(surveyFile(ctx)); edg <- readRDS(edgesFile(ctx))
  types <- intersect(names(TYPES), TYPES_RUN)
  reps <- Reduce(intersect, lapply(types, function(t){
    k <- seq_len(N_REPS); k[file.exists(runFile(ctx, aoi$stamp, t, k))]
  }))
  if(length(reps) < 1) stop(ctx, ": no complete replicate in ", OUT)
  G <- buildGrid(ctx, aoi, sv, edg)
  n100 <- terra::ncell(G$t100)
  #the 100 m counts summed over the replicates (the maps' scale does not matter to r)
  sim100 <- lapply(stats::setNames(types, types), function(t){
    x <- numeric(n100)
    for(i in reps){
      r <- readRDS(runFile(ctx, aoi$stamp, t, i))
      x[r$cells] <- x[r$cells] + r$counts
    }
    x
  })
  list(ctx = ctx, cfg = cfg, aoi = aoi, sv = sv, edg = edg, G = G, types = types,
       reps = reps, sim100 = sim100)
}

#one view: survey, every type, the z differences, with cell coordinates
viewValues <- function(D, vn){
  v  <- D$G$views[[vn]]
  sur <- surveyView(D$G, v, D$sv$surveyBase)
  grid <- gridFrom(v$grid)
  xy <- terra::xyFromCell(grid, v$idx)
  z  <- function(x) (x - mean(x)) / stats::sd(x)
  sims <- lapply(D$sim100, function(s) trans(as.vector(v$Msim %*% s), D$cfg$simTransform))
  list(vn = vn, scale_m = v$scale_m, x = xy[, 1], y = xy[, 2], cell = v$idx,
       res = terra::res(grid)[1], survey = sur, sim = sims,
       d = lapply(sims, function(s) z(s) - z(sur)),
       r = vapply(sims, function(s) stats::cor(s, sur), 1))
}

#the area each cell centre falls in (clipped areas), NA outside all
cellAoi <- function(V, ac){
  pts <- sf::st_as_sf(data.frame(x = V$x, y = V$y), coords = c("x", "y"), crs = 2056)
  hit <- sf::st_intersects(pts, ac)
  vapply(hit, function(h) if(length(h)) ac$AOI[h[1]] else NA_character_, "")
}

#### drawing ####

lakesOf <- function(aoi){
  perim <- perimeter2056(aoi)
  lk <- sf::st_read(ns$vftData("maps/lakes.gdb"), query = 'SELECT * FROM "lakes"',
                    wkt_filter = sf::st_as_text(perim), quiet = TRUE)
  if(!nrow(lk)) return(NULL)
  g <- sf::st_transform(sf::st_make_valid(sf::st_zm(sf::st_geometry(lk))), 2056)
  g <- suppressWarnings(sf::st_intersection(g, sf::st_buffer(perim, 500)))
  g[!sf::st_is_empty(g)]
}

basePanel <- function(df, ctxGeo, title, labels = TRUE){
  p <- ggplot() + geom_tile(data = df, aes(x, y, fill = v), width = df$res[1], height = df$res[1])
  if(!is.null(ctxGeo$lakes))
    p <- p + geom_sf(data = sf::st_sf(geometry = ctxGeo$lakes), fill = NA, colour = COL$muted,
                     linewidth = 0.25, linetype = "22")
  p <- p + geom_sf(data = ctxGeo$ac, fill = NA, colour = COL$ink2, linewidth = 0.2) +
    geom_sf(data = sf::st_sf(geometry = ctxGeo$perim), fill = NA, colour = COL$ink, linewidth = 0.35)
  if(labels)
    p <- p + geom_sf_text(data = ctxGeo$lab, aes(label = AOI), size = 1.7, colour = COL$ink2,
                          fontface = "bold")
  p + coord_sf(datum = NA, crs = 2056, expand = FALSE) + labs(title = title, x = NULL, y = NULL) +
    theme_void(base_size = 8) +
    theme(plot.title = element_text(size = 8.5, colour = COL$ink, margin = margin(b = 3)),
          legend.title = element_text(size = 7, colour = COL$ink2),
          legend.text = element_text(size = 6.5, colour = COL$ink2),
          plot.background = element_rect(fill = "white", colour = NA),
          legend.position = "bottom", legend.key.width = unit(1.1, "cm"),
          legend.key.height = unit(0.18, "cm")) +
    guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))
}
seqScale <- function() scale_fill_gradientn(
  colours = COL$seq, limits = c(0, 1), trans = "sqrt", breaks = c(0, 0.05, 0.25, 0.5, 1),
  name = "Std. usage\n(sqrt colour)")
divScale <- function(lim, name) scale_fill_gradientn(
  colours = c(COL$under, COL$under2, COL$mid, COL$over2, COL$over), limits = c(-lim, lim),
  oob = scales::squish, name = name,
  breaks = c(-lim, 0, lim), labels = c("more in survey", "0", "more in sim."))

#the published-style triple: survey, Full, difference with the worst cells numbered
triple <- function(V, ctxGeo, type, file, sub){
  cfg <- ctxGeo$cfg
  dfS <- data.frame(x = V$x, y = V$y, res = V$res, v = std01(V$survey))
  dfM <- data.frame(x = V$x, y = V$y, res = V$res, v = std01(V$sim[[type]]))
  d   <- V$d[[type]]
  lim <- max(stats::quantile(abs(d), 0.99), 1)
  dfD <- data.frame(x = V$x, y = V$y, res = V$res, v = d)
  loss <- d^2 / sum(d^2)
  top <- order(loss, decreasing = TRUE)[seq_len(min(TOP_N, length(d)))]
  topDf <- data.frame(x = V$x[top], y = V$y[top], k = seq_along(top))
  tl <- TYPES[[type]]$label
  simLbl <- if(identical(cfg$simTransform, "sqrt")) sprintf("%s sim. [sqrt]", tl) else sprintf("%s sim.", tl)
  gS <- basePanel(dfS, ctxGeo, "a) Survey") + seqScale()
  gM <- basePanel(dfM, ctxGeo, paste0("b) ", simLbl)) + seqScale()
  gD <- basePanel(dfD, ctxGeo, sprintf("c) %s minus survey (z), r = %.3f", tl, V$r[[type]]),
                  labels = FALSE) +
    divScale(round(lim, 1), "z difference")
  #numbered circles only where cells are big enough to tell apart (not at 100 m)
  marked <- V$res >= 500
  if(marked)
    gD <- gD + geom_point(data = topDf, aes(x, y), shape = 21, size = 3.2, stroke = 0.35, fill = NA,
                          colour = COL$ink) +
      geom_text(data = topDf, aes(x, y, label = k), size = 1.8, colour = COL$ink, fontface = "bold")
  cap <- sprintf("%s at %d m, %d cells, %d replicate(s) per type. %s %d cells with the largest share of 1 - r carry %.0f %% of it. Grey letters: areas; dashed: lakes.",
                 cfg$label, V$scale_m, length(d), ctxGeo$nReps, if(marked) "Circles: the" else "The",
                 length(top), 100 * sum(loss[top]))
  ab <- ggpubr::ggarrange(gS, gM, nrow = 1, common.legend = TRUE, legend = "bottom")
  p <- ggpubr::ggarrange(ab, gD, nrow = 1, widths = c(2, 1))
  p <- ggpubr::annotate_figure(p, bottom = ggpubr::text_grob(
    paste(c(sub, strwrap(cap, 160)), collapse = "\n"), size = 6.5, color = COL$ink2,
    x = 0.01, hjust = 0))
  ggsave(file, p, width = 24, height = 11, units = "cm", dpi = 300, bg = "white")
}

#the difference for every type, one shared scale
typesRow <- function(V, ctxGeo, file){
  lim <- max(unlist(lapply(V$d, function(d) stats::quantile(abs(d), 0.99))), 1)
  ps <- lapply(seq_along(V$d), function(i){
    t <- names(V$d)[i]
    df <- data.frame(x = V$x, y = V$y, res = V$res, v = V$d[[t]])
    basePanel(df, ctxGeo, sprintf("%s) %s, r = %.3f", letters[i], TYPES[[t]]$label, V$r[[t]]),
              labels = FALSE) + divScale(round(lim, 1), "z difference\n(sim - survey)")
  })
  p <- ggpubr::ggarrange(plotlist = ps, nrow = 1, common.legend = TRUE, legend = "bottom")
  p <- ggpubr::annotate_figure(p, top = ggpubr::text_grob(
    sprintf("%s at %d m: simulation minus survey (z-scores) per type", ctxGeo$cfg$label, V$scale_m),
    size = 9, color = COL$ink))
  ggsave(file, p, width = 30, height = 9.5, units = "cm", dpi = 300, bg = "white")
}

#### main ####

main <- function(){
  logf("misalignment figures | %s | contexts %s", OUT, paste(CONTEXTS, collapse = ","))
  cmp <- if(file.exists(outPath("results", "compare.rds"))) readRDS(outPath("results", "compare.rds")) else NULL
  cellRows <- list(); areaRows <- list()
  for(ctx in CONTEXTS){
    D <- contextData(ctx)
    cfg <- D$cfg
    ac <- D$G$ac
    lab <- sf::st_sf(AOI = ac$AOI, geometry = sf::st_point_on_surface(sf::st_geometry(ac)))
    ctxGeo <- list(cfg = cfg, ac = ac, lab = lab, perim = perimeter2056(D$aoi),
                   lakes = tryCatch(lakesOf(D$aoi), error = function(e) NULL), nReps = length(D$reps))
    vPub <- paste0(cfg$oldScale, "_all")
    Vp <- viewValues(D, vPub)

    #the same r as the compare stage, or these are not its numbers
    if(!is.null(cmp[[ctx]])){
      m <- cmp[[ctx]]$metrics
      for(t in names(Vp$r)){
        ref <- m$point[m$test == "map" & m$view == vPub & m$method == "pearson" & m$type == TYPES[[t]]$label]
        if(length(ref) && abs(ref - Vp$r[[t]]) > 1e-6)
          stop(sprintf("%s %s: r %.6f here vs %.6f in compare.rds", ctx, t, Vp$r[[t]], ref))
      }
      logf("%s: r at %s matches compare.rds for all %d types", ctx, vPub, length(Vp$r))
    }

    sub <- sprintf("Areas: %s", if(is.null(D$aoi$source)) D$aoi$stamp else basename(D$aoi$source))
    triple(Vp, ctxGeo, "full", outPath("figures", sprintf("misalign_%s.png", ctx)), sub)
    if(cfg$oldScale > 1){
      V1 <- viewValues(D, "1_all")
      triple(V1, ctxGeo, "full", outPath("figures", sprintf("misalign_%s_100m.png", ctx)), sub)
    }
    typesRow(Vp, ctxGeo, outPath("figures", sprintf("misalign_types_%s.png", ctx)))

    #the cells that cost r the most, per type
    aoiOf <- cellAoi(Vp, ac)
    for(t in names(Vp$d)){
      d <- Vp$d[[t]]; loss <- d^2 / sum(d^2)
      o <- order(loss, decreasing = TRUE)[seq_len(min(20, length(d)))]
      cellRows[[length(cellRows) + 1]] <- data.frame(
        context = ctx, type = TYPES[[t]]$label, scale_m = Vp$scale_m, rank = seq_along(o),
        x = Vp$x[o], y = Vp$y[o], aoi = aoiOf[o],
        survey_std = std01(Vp$survey)[o], sim_std = std01(Vp$sim[[t]])[o],
        z_diff = d[o], loss_share = loss[o], loss_cum = cumsum(loss[o]), r = Vp$r[[t]])
    }

    #each area's share of the usage, as the destination test counts it
    simBase <- lapply(D$sim100, function(s) as.vector(D$G$P %*% s))
    surMass <- vapply(D$G$aoiCells, function(cl) sum(D$sv$surveyBase[cl]), 1)
    for(t in names(simBase)){
      simMass <- vapply(D$G$aoiCells, function(cl) sum(simBase[[t]][cl]), 1)
      areaRows[[length(areaRows) + 1]] <- data.frame(
        context = ctx, type = TYPES[[t]]$label, aoi = names(D$G$aoiCells), area_ha = ac$areaHa,
        survey_share = surMass / sum(surMass), sim_share = simMass / sum(simMass))
    }
    #what to look at first, in the log
    d <- Vp$d[["full"]]; loss <- d^2 / sum(d^2); o <- order(loss, decreasing = TRUE)[1:min(TOP_N, length(d))]
    logf("%s Full at %d m: r = %.3f; the %d worst cells carry %.0f %% of 1 - r (%d over-, %d under-simulated); areas %s",
         ctx, Vp$scale_m, Vp$r[["full"]], length(o), 100 * sum(loss[o]), sum(d[o] > 0), sum(d[o] < 0),
         paste(unique(stats::na.omit(aoiOf[o])), collapse = " "))
  }

  cells <- do.call(rbind, cellRows); areas <- do.call(rbind, areaRows)
  utils::write.csv(cells, outPath("results", "misalign_cells.csv"), row.names = FALSE)
  utils::write.csv(areas, outPath("results", "misalign_areas.csv"), row.names = FALSE)

  #### the areas' shares: survey (black) against Full and Nearby-Random ####
  show <- intersect(c("Full", "Nearby-Random"), unique(areas$type))
  ps <- lapply(CONTEXTS, function(ctx){
    a <- areas[areas$context == ctx & areas$type %in% show, ]
    s <- unique(a[, c("aoi", "survey_share", "area_ha")])
    lv <- s$aoi[order(s$survey_share)]
    a$aoi <- factor(a$aoi, levels = lv); s$aoi <- factor(s$aoi, levels = lv)
    a$type <- factor(a$type, levels = show)
    ggplot() +
      geom_segment(data = a[a$type == "Full", ], aes(y = aoi, yend = aoi, x = survey_share, xend = sim_share),
                   colour = COL$muted, linewidth = 0.3) +
      geom_point(data = a, aes(y = aoi, x = sim_share, colour = type), size = 1.6) +
      geom_point(data = s, aes(y = aoi, x = survey_share), shape = 21, size = 1.9, stroke = 0.5,
                 colour = COL$survey, fill = "white") +
      scale_colour_manual(values = c(Full = COL$full, `Nearby-Random` = COL$nearbyRandom), name = NULL) +
      scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
      labs(x = "Share of the usage inside the areas", y = NULL, title = CTX[[ctx]]$label) +
      theme_minimal(base_size = 8) +
      theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line(colour = "#e6e5e1", linewidth = 0.25),
            axis.text = element_text(colour = COL$ink2, size = 6.5),
            plot.title = element_text(size = 9, colour = COL$ink), legend.position = "bottom")
  })
  p <- ggpubr::ggarrange(plotlist = ps, nrow = 1, common.legend = TRUE, legend = "bottom")
  p <- ggpubr::annotate_figure(p, bottom = ggpubr::text_grob(
    "Open circle: survey. Grey line: from the survey's share to Full's. Letters as on the maps.",
    size = 7, color = COL$ink2))
  ggsave(outPath("figures", "misalign_areas.png"), p, width = 24, height = 16, units = "cm",
         dpi = 300, bg = "white")
  logf("misalignment figures written to %s", file.path(OUT, "figures"))
}

main()
