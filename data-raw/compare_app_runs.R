#### Does the web app simulate what the validation simulates? ####
#
# An app save (step 5 done) carries the app's own result: the prepared network,
# the population (dayPop) and the path usage (pathUsage). This script re-runs that
# simulation locally, the way the app does it - the app's residents raster
# (maps/residential/residentialData_raster_final.tif), CONST_residentDivision = 50,
# the engine's own parkingIntensity 0.1, no hooks - and asks whether the app's one
# run looks like one more replicate of the local ones.
#
# Two local variants, so a difference can be placed:
#   local  the network built here as validate_usage_surveys.R builds it (paths gdb,
#          sf_to_tidygraph3, vftPrepareNetwork), but with the app's residents
#   saved  the app's own prepared network from the save, simulated here
# app vs saved isolates the simulation code; saved vs local isolates the network.
#
# Needs the aoi, survey and network stages of validate_usage_surveys.R in the same
# VFT_VAL_OUT, with VFT_VAL_AOI_SAVES pointing at the same saves:
#
#   VFT_VAL_OUT=".../pathUsageValidation_2026_userAoi" \
#   VFT_VAL_AOI_SAVES="glattPark=...,wigger=...,glatt=..." VFT_APP_REPS=6 \
#   Rscript data-raw/compare_app_runs.R
#
# Writes VFT_VAL_OUT/appcheck: network parity, run summaries, the comparison tables
# and one figure per context. Every run is saved as it finishes (resumable).

Sys.setenv(VFT_VAL_WORKER = "1")   #definitions only: no main(), no compile
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "validate_usage_surveys.R"))
suppressPackageStartupMessages(library(ggplot2))

APP_REPS  <- as.integer(envNum("VFT_APP_REPS", 6))
APP_DIV   <- 50                     #CONST_residentDivision, R/step5_server.R
APP_SEED  <- 20261013L
VARIANTS  <- c("local", "saved")
appPath   <- function(...) outPath("appcheck", ...)
PASS_COLS <- c("passage", "passageAOI", "passageWalk", "passageDog", "passageBike", "passageJog")

#### inputs ####

loadSave <- function(ctx){
  src <- AOI_SAVES[[ctx]]
  if(is.null(src) || !file.exists(src)) stop(ctx, ": no app save (VFT_VAL_AOI_SAVES)")
  e <- new.env(); load(src, envir = e)
  x <- e$envBase_networkList[[1]]
  if(is.null(x$pathUsage) || is.null(x$dayPop)) stop(ctx, ": the save holds no simulation result")
  list(src = src, network = x$network, parking = x$parking, pathUsage = x$pathUsage, dayPop = x$dayPop)
}

#the network as the validation builds it, with the app's residents raster
localNetwork <- function(ctx, aoi){
  f <- appPath(paste0(ctx, "_localnet.rds"))
  if(file.exists(f)) return(readRDS(f))
  t0 <- proc.time()[["elapsed"]]
  #sf_to_tidygraph3() reads this global if it exists; without it, it loads the
  #app's raster (R/sf_to_tidygraph3.R:261)
  if(exists(".vft_residential_full", envir = .GlobalEnv, inherits = FALSE))
    rm(".vft_residential_full", envir = .GlobalEnv)
  wkt <- sf::st_as_text(sf::st_transform(aoi$shapeLarger, "epsg:4326"))
  paths <- sf::st_read(ns$vftData("maps/paths/paths_11_24_final_4.gdb"),
                       query = 'SELECT * FROM "paths_11_24_final_4"',
                       wkt_filter = wkt, promote_to_multi = FALSE, type = 2, quiet = TRUE)
  net <- ns$sf_to_tidygraph3(paths, aoi$shapeLarger, directed = FALSE)[[1]][[1]]
  rm(paths); invisible(gc())
  prep <- ns$vftPrepareNetwork(net, aoi$polys, aoi$thr)
  out <- list(network = prep$network, parking = prep$parking)
  saveAtomic(out, f)
  logf("appcheck %s: local network built, %.0f s", ctx, proc.time()[["elapsed"]] - t0)
  out
}

#### network parity ####

attrDiff <- function(a, b){
  if(is.null(a) || is.null(b)) return(c(maxAbs = NA_real_, nDiff = NA_real_))
  if(is.character(a) || is.character(b) || is.factor(a)){
    a <- as.character(a); b <- as.character(b)
    return(c(maxAbs = NA_real_, nDiff = sum(!(a == b | (is.na(a) & is.na(b))))))
  }
  a <- as.numeric(a); b <- as.numeric(b)
  d <- abs(a - b); d[is.na(a) & is.na(b)] <- 0
  c(maxAbs = suppressWarnings(max(d, na.rm = TRUE)), nDiff = sum(d > 1e-9 | xor(is.na(a), is.na(b)), na.rm = TRUE))
}

networkParity <- function(ctx, loc, sav){
  rows <- list(data.frame(context = ctx, what = "counts", attr = "nodes / edges",
                          local = sprintf("%d / %d", igraph::vcount(loc), igraph::ecount(loc)),
                          saved = sprintf("%d / %d", igraph::vcount(sav), igraph::ecount(sav)),
                          maxAbs = NA_real_, nDiff = NA_real_))
  if(igraph::vcount(loc) == igraph::vcount(sav)){
    for(a in intersect(c("nodeID", "X", "Y", "Residents", "AOI", "parking", "parkingAttr", "newResidential",
                         "DULN", DULN_NODE_COLS), igraph::vertex_attr_names(loc))){
      va <- igraph::vertex_attr(loc, a); vb <- igraph::vertex_attr(sav, a)
      d <- attrDiff(va, vb)
      rows[[length(rows) + 1]] <- data.frame(context = ctx, what = "node", attr = a,
        local = if(is.numeric(va)) sprintf("sum %.6g", sum(va, na.rm = TRUE)) else "",
        saved = if(is.numeric(vb)) sprintf("sum %.6g", sum(vb, na.rm = TRUE)) else "",
        maxAbs = d[["maxAbs"]], nDiff = d[["nDiff"]])
    }
  }
  if(igraph::ecount(loc) == igraph::ecount(sav)){
    el <- igraph::as_edgelist(loc, names = FALSE); es <- igraph::as_edgelist(sav, names = FALSE)
    rows[[length(rows) + 1]] <- data.frame(context = ctx, what = "edge", attr = "from/to",
      local = "", saved = "", maxAbs = NA_real_, nDiff = sum(rowSums(el != es) > 0))
    for(a in c("edgeID", grep("^SHAPE_Leng", igraph::edge_attr_names(loc), value = TRUE))){
      d <- attrDiff(igraph::edge_attr(loc, a), igraph::edge_attr(sav, a))
      rows[[length(rows) + 1]] <- data.frame(context = ctx, what = "edge", attr = a, local = "", saved = "",
                                             maxAbs = d[["maxAbs"]], nDiff = d[["nDiff"]])
    }
  }
  do.call(rbind, rows)
}

#### one run, and the summary kept of it ####

runSummary <- function(pathUsage, dayPop, net){
  ea  <- igraph::edge_attr(pathUsage)
  res <- igraph::vertex_attr(net, "Residents")[match(dayPop$startV, igraph::vertex_attr(net, "nodeID"))]
  list(edgeID = as.integer(ea$edgeID),
       pass = lapply(stats::setNames(PASS_COLS, PASS_COLS), function(p) as.numeric(ea[[p]])),
       nAgents = nrow(dayPop),
       noGoal = sum(dayPop$AOIGoal %in% "NO_GOAL" | dayPop$goalV %in% -1),
       parkStart = sum(!(res > 0) | is.na(res)),
       typ = table(factor(dayPop$agentTyp, levels = c("walkNat", "walkSoc", "dogNat", "dogProx",
                                                      "ebikeNat", "bikeSport", "jogger"))),
       goals = table(dayPop$AOIGoal),
       distance = mean(dayPop$totalDistance, na.rm = TRUE))
}

simOnce <- function(ctx, variant, rep, nets, aoi){
  f <- appPath("runs", ctx, sprintf("%s_rep_%03d.rds", variant, rep))
  if(file.exists(f)) return(readRDS(f))
  nw <- nets[[variant]]
  pp <- fakeProgress()
  set.seed(APP_SEED + rep, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  t0 <- proc.time()[["elapsed"]]
  quiet <- file(nullfile(), open = "wt"); sink(quiet)
  out <- tryCatch(ns$vftSimulateScenario(nw$network, aoi$polys, aoi$thr, parking = nw$parking,
                                         residentDivision = APP_DIV, progPrep = pp, progSim = pp),
                  finally = { sink(); close(quiet) })
  net <- if(is.null(out$network)) nw$network else out$network
  s <- runSummary(out$results$pathUsage, out$results$dayPop, net)
  s$secs <- proc.time()[["elapsed"]] - t0; s$git <- GIT; s$seed <- APP_SEED + rep
  saveAtomic(s, f)
  logf("appcheck %s %s rep %d: %.0f s, %d agents, %d without an area", ctx, variant, rep, s$secs,
       s$nAgents, s$noGoal)
  s
}

#### the comparison ####

#passage indexed by edgeID
byEdge <- function(s, col, n){ x <- numeric(n); x[s$edgeID] <- s$pass[[col]]; x[is.na(x)] <- 0; x }

#the edges' passage on the survey's 100 m grid (each edge into the cells the
#validation counts it in), then the view the published score uses
passMap <- function(p, edg, G, view, cfg){
  ec <- edg$edgeCells
  v <- p[ec$edgeID]; v[is.na(v)] <- 0
  x <- numeric(terra::ncell(G$t100))
  a <- rowsum(v, ec$cell); x[as.integer(rownames(a))] <- a[, 1]
  trans(as.vector(G$views[[view]]$Msim %*% x), cfg$simTransform)
}

compareCtx <- function(ctx){
  cfg <- CTX[[ctx]]
  aoi <- readRDS(aoiFile(ctx)); sv <- readRDS(surveyFile(ctx)); edg <- readRDS(edgesFile(ctx))
  sav <- loadSave(ctx)
  loc <- localNetwork(ctx, aoi)
  nets <- list(local = loc, saved = list(network = sav$network, parking = sav$parking))

  par <- networkParity(ctx, loc$network, sav$network)
  bad <- par[!is.na(par$nDiff) & par$nDiff > 0, ]
  logf("appcheck %s: network parity local vs saved - %s", ctx,
       if(nrow(bad)) paste(sprintf("%s %s differs in %d", bad$what, bad$attr, bad$nDiff), collapse = "; ")
       else "identical in every attribute checked")

  runs <- lapply(stats::setNames(VARIANTS, VARIANTS), function(v)
    lapply(seq_len(APP_REPS), function(k) simOnce(ctx, v, k, nets, aoi)))
  app <- runSummary(sav$pathUsage, sav$dayPop, sav$network)

  nE <- max(c(app$edgeID, unlist(lapply(runs, function(rr) lapply(rr, `[[`, "edgeID")))), na.rm = TRUE)
  G  <- buildGrid(ctx, aoi, sv, edg)
  vPub <- paste0(cfg$oldScale, "_all")
  surPub <- surveyView(G, G$views[[vPub]], sv$surveyBase)

  #### population ####
  popRow <- function(s, who, rep) data.frame(context = ctx, source = who, rep = rep,
    agents = s$nAgents, share_no_area = s$noGoal / s$nAgents, share_parking_start = s$parkStart / s$nAgents,
    mean_distance_m = s$distance, passage_total = sum(s$pass$passage, na.rm = TRUE),
    passage_aoi_share = sum(s$pass$passageAOI, na.rm = TRUE) / sum(s$pass$passage, na.rm = TRUE),
    t(as.matrix(prop.table(s$typ))), check.names = FALSE)
  pop <- rbind(popRow(app, "app", NA),
               do.call(rbind, lapply(VARIANTS, function(v) do.call(rbind, lapply(seq_along(runs[[v]]),
                 function(k) popRow(runs[[v]][[k]], v, k))))))

  #### edges: is the app one more replicate? ####
  #every run against the mean of the local runs it is not part of. For the app
  #that is every set of APP_REPS - 1 local runs, averaged, so both sides compare
  #one run with a mean of the same size.
  edgeRows <- list(); mapRows <- list(); goalRows <- list()
  for(col in c("passage", "passageAOI")){
    for(v in VARIANTS){
      P <- sapply(runs[[v]], byEdge, col = col, n = nE)
      pa <- byEdge(app, col, nE)
      for(k in seq_len(ncol(P))){
        others <- rowMeans(P[, -k, drop = FALSE])
        rr <- corBoth(P[, k], others); ra <- corBoth(pa, others)
        edgeRows[[length(edgeRows) + 1]] <- data.frame(context = ctx, variant = v, column = col, k = k,
          r_rep = rr[["pearson"]], rho_rep = rr[["spearman"]],
          r_app = ra[["pearson"]], rho_app = ra[["spearman"]],
          total_rep = sum(P[, k]), total_app = sum(pa))
      }
    }
  }
  #### the survey, through the published view, from the edges' passage ####
  mapOf <- function(s) passMap(byEdge(s, "passage", nE), edg, G, vPub, cfg)
  mapRows[[1]] <- data.frame(context = ctx, source = "app", rep = NA, r_survey = stats::cor(mapOf(app), surPub))
  for(v in VARIANTS) for(k in seq_along(runs[[v]]))
    mapRows[[length(mapRows) + 1]] <- data.frame(context = ctx, source = v, rep = k,
                                                 r_survey = stats::cor(mapOf(runs[[v]][[k]]), surPub))
  #the validation's own Full runs (respondents' homes, its agent count and car-park
  #share), scored the same way: separates the residents from the metric
  for(k in seq_len(N_REPS)){
    f <- runFile(ctx, aoi$stamp, "full", k)
    if(!file.exists(f)) break
    vr <- readRDS(f)
    pv <- numeric(nE); pv[seq_along(vr$passage)] <- vr$passage
    mapRows[[length(mapRows) + 1]] <- data.frame(context = ctx, source = "validation", rep = k,
      r_survey = stats::cor(passMap(pv, edg, G, vPub, cfg), surPub))
  }
  #### destinations ####
  lv <- sort(unique(c(names(app$goals), unlist(lapply(runs, function(rr) lapply(rr, function(s) names(s$goals)))))))
  share <- function(s){ x <- setNames(numeric(length(lv)), lv); x[names(s$goals)] <- s$goals; x / sum(x) }
  tvd <- function(p, q) 0.5 * sum(abs(p - q))
  for(v in VARIANTS){
    Sh <- sapply(runs[[v]], share)
    for(k in seq_len(ncol(Sh))){
      others <- rowMeans(Sh[, -k, drop = FALSE])
      goalRows[[length(goalRows) + 1]] <- data.frame(context = ctx, variant = v, k = k,
        tvd_rep = tvd(Sh[, k], others), tvd_app = tvd(share(app), others))
    }
  }
  goalShares <- data.frame(context = ctx, goal = lv, app = share(app),
                           local = rowMeans(sapply(runs$local, share)),
                           saved = rowMeans(sapply(runs$saved, share)))

  list(ctx = ctx, parity = par, pop = pop, edges = do.call(rbind, edgeRows),
       maps = do.call(rbind, mapRows), goals = do.call(rbind, goalRows), goalShares = goalShares,
       draw = list(app = byEdge(app, "passage", nE),
                   local = sapply(runs$local, byEdge, col = "passage", n = nE),
                   saved = sapply(runs$saved, byEdge, col = "passage", n = nE),
                   appMap = mapOf(app), localMap = rowMeans(sapply(runs$local, mapOf)),
                   savedMap = rowMeans(sapply(runs$saved, mapOf)),
                   survey = surPub, view = G$views[[vPub]], G = G))
}

#### figures ####

COL <- list(ink = "#0b0b0b", ink2 = "#52514e", muted = "#9a9994", grid = "#e6e5e1",
            app = "#eb6834", local = "#2a78d6", saved = "#1baf7a",
            under = "#184f95", under2 = "#6da7ec", mid = "#f0efec", over2 = "#e8807f", over = "#a3302f")
thm <- theme_minimal(base_size = 8) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = COL$grid, linewidth = 0.25),
        axis.text = element_text(colour = COL$ink2, size = 6.5), axis.title = element_text(colour = COL$ink2, size = 7),
        plot.title = element_text(size = 8.5, colour = COL$ink), legend.position = "bottom",
        legend.text = element_text(size = 6.5, colour = COL$ink2), legend.title = element_blank())

figureCtx <- function(R){
  ctx <- R$ctx; cfg <- CTX[[ctx]]; d <- R$draw
  #a) app vs the local mean, edge by edge; local rep 1 vs the others behind it
  others1 <- rowMeans(d$local[, -1, drop = FALSE])
  dfA <- rbind(data.frame(x = rowMeans(d$local), y = d$app, who = "app run"),
               data.frame(x = others1, y = d$local[, 1], who = "local run 1"))
  dfA <- dfA[dfA$x > 0 | dfA$y > 0, ]
  e <- R$edges[R$edges$column == "passage" & R$edges$variant == "local", ]
  gA <- ggplot(dfA, aes(log1p(x), log1p(y), colour = who)) +
    geom_abline(slope = 1, intercept = 0, colour = COL$muted, linewidth = 0.3) +
    geom_point(size = 0.25, alpha = 0.35) +
    scale_colour_manual(values = c(`app run` = COL$app, `local run 1` = COL$local)) +
    guides(colour = guide_legend(override.aes = list(size = 1.8, alpha = 1))) +
    labs(x = "log(1 + passage), mean of local runs", y = "log(1 + passage), one run",
         title = sprintf("a) Edges: app r = %.3f, local runs r = %.3f-%.3f", mean(e$r_app),
                         min(e$r_rep), max(e$r_rep))) + thm
  #b) the survey score of every run
  m <- R$maps; m$source <- factor(m$source, levels = c("app", "saved", "local", "validation"),
                                  labels = c("app run", "saved network, here", "local network, here",
                                             "validation Full
(respondents)"))
  gB <- ggplot(m, aes(r_survey, source, colour = source)) +
    geom_point(size = 2, alpha = 0.85, position = position_jitter(height = 0.08, width = 0, seed = 1)) +
    scale_colour_manual(values = setNames(c(COL$app, COL$saved, COL$local, COL$ink2), levels(m$source))) +
    labs(x = sprintf("Pearson r with the survey (%d m, edge passage)", R$draw$view$scale_m), y = NULL,
         title = "b) Survey score per run") + thm + theme(legend.position = "none")
  #c) the population
  pp <- R$pop
  long <- do.call(rbind, lapply(c("share_no_area", "share_parking_start", "passage_aoi_share"), function(cn)
    data.frame(metric = cn, source = pp$source, value = pp[[cn]])))
  long$metric <- factor(long$metric, levels = c("share_no_area", "share_parking_start", "passage_aoi_share"),
                        labels = c("agents without an area", "agents starting at car parks", "passage inside areas"))
  long$source <- factor(long$source, levels = c("app", "saved", "local"),
                        labels = c("app run", "saved network, here", "local network, here"))
  gC <- ggplot(long, aes(value, metric, colour = source)) +
    geom_point(size = 1.8, alpha = 0.85, position = position_dodge(width = 0.5)) +
    scale_colour_manual(values = setNames(c(COL$app, COL$saved, COL$local), levels(long$source))) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(x = NULL, y = NULL, title = sprintf("c) Population (%d agents in the app)", pp$agents[pp$source == "app"])) + thm
  #d) where the app differs from the local mean, on the survey's view
  z <- function(x) (x - mean(x)) / stats::sd(x)
  v <- R$draw$view; grid <- gridFrom(v$grid); xy <- terra::xyFromCell(grid, v$idx)
  dd <- z(d$appMap) - z(d$localMap)
  lim <- max(stats::quantile(abs(dd), 0.99), 0.5)
  gD <- ggplot(data.frame(x = xy[, 1], y = xy[, 2], v = dd)) +
    geom_tile(aes(x, y, fill = v), width = terra::res(grid)[1], height = terra::res(grid)[1]) +
    geom_sf(data = R$draw$G$ac, fill = NA, colour = COL$ink2, linewidth = 0.2) +
    scale_fill_gradientn(colours = c(COL$under, COL$under2, COL$mid, COL$over2, COL$over),
                         limits = c(-lim, lim), oob = scales::squish, breaks = c(-lim, 0, lim),
                         labels = c("more in local", "0", "more in app")) +
    coord_sf(datum = NA, crs = 2056, expand = FALSE) +
    labs(title = sprintf("d) App minus local mean (z), r = %.3f", stats::cor(d$appMap, d$localMap))) +
    theme_void(base_size = 8) +
    theme(plot.title = element_text(size = 8.5, colour = COL$ink), legend.position = "bottom",
          legend.title = element_blank(), legend.text = element_text(size = 6.5, colour = COL$ink2),
          legend.key.width = unit(1, "cm"), legend.key.height = unit(0.18, "cm"))
  p <- ggpubr::ggarrange(gA, gB, gC, gD, nrow = 2, ncol = 2)
  p <- ggpubr::annotate_figure(p, top = ggpubr::text_grob(
    sprintf("%s: the app's run against %d local runs per network (residents raster, division %d)",
            cfg$label, APP_REPS, APP_DIV), size = 9, color = COL$ink))
  ggsave(appPath(sprintf("fig_appcheck_%s.png", ctx)), p, width = 22, height = 18, units = "cm",
         dpi = 300, bg = "white")
}

#### main ####

main <- function(){
  logf("appcheck | %s | contexts %s | %d runs per network | code %s", OUT,
       paste(CONTEXTS, collapse = ","), APP_REPS, GIT)
  res <- lapply(stats::setNames(CONTEXTS, CONTEXTS), compareCtx)
  wcsv <- function(x, f) utils::write.csv(x, appPath(f), row.names = FALSE)
  wcsv(do.call(rbind, lapply(res, `[[`, "parity")), "network_parity.csv")
  wcsv(do.call(rbind, lapply(res, `[[`, "pop")), "population.csv")
  wcsv(do.call(rbind, lapply(res, `[[`, "edges")), "edges.csv")
  wcsv(do.call(rbind, lapply(res, `[[`, "maps")), "survey_scores.csv")
  wcsv(do.call(rbind, lapply(res, `[[`, "goals")), "destinations.csv")
  wcsv(do.call(rbind, lapply(res, `[[`, "goalShares")), "destination_shares.csv")
  for(R in res) figureCtx(R)

  for(R in res){
    e <- R$edges[R$edges$column == "passage", ]; m <- R$maps; g <- R$goals; pp <- R$pop
    logf("== %s ==", R$ctx)
    for(v in VARIANTS){
      ev <- e[e$variant == v, ]; gv <- g[g$variant == v, ]; pv <- pp[pp$source == v, ]
      logf("  vs %-5s edges r: app %.3f | runs %.3f-%.3f ; destinations TVD: app %.3f | runs %.3f-%.3f",
           v, mean(ev$r_app), min(ev$r_rep), max(ev$r_rep), mean(gv$tvd_app), min(gv$tvd_rep), max(gv$tvd_rep))
      logf("  %-5s runs: agents %s, without an area %.1f-%.1f %%, car-park starts %.1f-%.1f %%, passage %.0f-%.0f",
           v, paste(unique(pv$agents), collapse = "/"), 100 * min(pv$share_no_area), 100 * max(pv$share_no_area),
           100 * min(pv$share_parking_start), 100 * max(pv$share_parking_start),
           min(pv$passage_total), max(pv$passage_total))
    }
    pa <- pp[pp$source == "app", ]
    logf("  app run: agents %d, without an area %.1f %%, car-park starts %.1f %%, passage %.0f",
         pa$agents, 100 * pa$share_no_area, 100 * pa$share_parking_start, pa$passage_total)
      rng <- function(s) if(any(m$source == s)) sprintf("%.3f-%.3f", min(m$r_survey[m$source == s]),
                                                       max(m$r_survey[m$source == s])) else "-"
    logf("  survey r (edge passage, %s m): app %.3f | saved %s | local %s | validation Full %s",
         R$draw$view$scale_m, m$r_survey[m$source == "app"], rng("saved"), rng("local"), rng("validation"))
  }
  logf("appcheck written to %s", file.path(OUT, "appcheck"))
}

main()
