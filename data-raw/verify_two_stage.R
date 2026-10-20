## Verification for the two-stage simulation (R/two_stage.R, the `explore` mode of
## launchSim() and the `twoStage` argument of vftSimulateScenario()).
##
##   A. phase 1 IS the normal simulation: same seed, same walks inside the areas
##   B. the path library: each walk is a chain of edges from its entrance, inside
##      the areas, with a mean DULN inside its type's range
##   C. the choice: own type, own duration (or the nearest one on offer), within
##      reach, at the distance the route finder gives
##   D. the trip: start -> entrance -> walk -> home chains up, and the passage
##      counts are exactly the agents' histories
##   E. the coefficients do what they say (bDist = 0: more attractive walks;
##      bAttr = 0: nearer entrances; the logit scale alpha = 0.5 = both x2)
##   F. the same seed gives the same run
##   G. a heatwave two-stage run runs, and keeps its label
##   and a report: timings, who stays home, area shares, passage normal vs
##   two-stage, explorers = 3.
##
## Run:  Rscript data-raw/verify_two_stage.R
## Inputs:
##   VFT_TS_VAL    a survey-validation output folder with aoi/ and network/
##                 (default: Documents/Local Data/pathUsageValidation_2026_final_timefix)
##   VFT_TS_CTX    its contexts, ";"-separated (default glattPark)
##   VFT_TS_AGENTS agents per run on those (default 2000)
##   VFT_ABM_SAVES step-5 save files, as in verify_abm_speed.R (default: the first
##                 Downloads save that exists)
##
## NEVER a global named `dayPop` in here: launchMultiSim() tests exists("dayPop").

suppressPackageStartupMessages({
  pkgload::load_all(".", export_all = TRUE, quiet = TRUE)
})
ns <- asNamespace("visitorFlowTool")

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("  %-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}
SEED <- 42L
seed <- function() set.seed(SEED, kind = "Mersenne-Twister", normal.kind = "Inversion",
                            sample.kind = "Rejection")
h <- list(set = function(...) invisible(NULL), inc = function(...) invisible(NULL),
          close = function() invisible(NULL))

#### the scenarios ####
scenarios <- list()
valDir <- Sys.getenv("VFT_TS_VAL", file.path(Sys.getenv("USERPROFILE", path.expand("~")),
                                             "Documents/Local Data/pathUsageValidation_2026_final_timefix"))
nAgentsVal <- as.numeric(Sys.getenv("VFT_TS_AGENTS", "2000"))
for(ctx in strsplit(Sys.getenv("VFT_TS_CTX", "glattPark"), ";", fixed = TRUE)[[1]]){
  fa <- file.path(valDir, "aoi", paste0(ctx, ".rds"))
  fn <- file.path(valDir, "network", paste0(ctx, "_net.rds"))
  if(!file.exists(fa) || !file.exists(fn)) next
  aoi <- readRDS(fa); nb <- readRDS(fn)
  res <- igraph::vertex_attr(nb$net, "Residents")
  scenarios[[ctx]] <- list(net = nb$net, polys = aoi$polys, thr = aoi$thr, parking = nb$parking,
                           div = sum(res, na.rm = TRUE) / nAgentsVal)
}
saves <- Sys.getenv("VFT_ABM_SAVES", "")
saves <- if(nzchar(saves)) strsplit(saves, ";", fixed = TRUE)[[1]] else
  file.path(Sys.getenv("USERPROFILE", path.expand("~")), "Downloads",
            c("visitorFlowSave_step5_simulation_2026_08_31_10_30_42.148.RData",
              "visitorFlowSave_step5_simulation_2026_08_31_18_56_03.708.RData"))
saves <- saves[file.exists(saves)]
if(length(saves)){
  e <- new.env(); load(saves[[1]], envir = e)
  net <- e$envBase_network
  if(is.null(net)) net <- e$envBase_networkList[[1]]$network
  if(!is.null(net))
    scenarios[[sub("[.]RData$", "", basename(saves[[1]]))]] <-
      list(net = net, polys = e$envBase_finalPolygons, thr = e$envBase_minThresh,
           parking = e$envBase_parking, div = 50)
}
if(!length(scenarios)) stop("no scenario found - set VFT_TS_VAL / VFT_ABM_SAVES")

#### one run, with the passage step watched ####
#The hook counts historyToPassage_cpp() calls and keeps what it was handed, the
#same capture the survey validation uses.
ORIG_H2P <- ns$historyToPassage_cpp
run <- function(sc, twoStage = NULL, heat = NULL){
  cap <- new.env(); cap$n <- 0L
  assignInNamespace("historyToPassage_cpp", function(agentTable, edgeTable, vertexTable){
    cap$n <- cap$n + 1L
    cap$agents <- agentTable[, intersect(c("id", "startV", "agentTyp", "priorE", "priorEAOI", "AOIGoal"),
                                         names(agentTable))]
    ORIG_H2P(agentTable = agentTable, edgeTable = edgeTable, vertexTable = vertexTable)
  }, "visitorFlowTool")
  on.exit(assignInNamespace("historyToPassage_cpp", ORIG_H2P, "visitorFlowTool"), add = TRUE)
  seed()
  t0 <- proc.time()[["elapsed"]]
  quiet <- file(nullfile(), open = "wt"); sink(quiet)
  out <- tryCatch(ns$vftSimulateScenario(sc$net, sc$polys, sc$thr, parking = sc$parking,
                                         residentDivision = sc$div, progPrep = h, progSim = h,
                                         heat = heat, twoStage = twoStage),
                  finally = { sink(); close(quiet) })
  list(out = out, res = out$results, cap = cap, secs = proc.time()[["elapsed"]] - t0)
}
edgeCol <- function(r, nm) igraph::edge_attr(r$res$pathUsage, nm)
`%|%`   <- function(a, b) ifelse(is.na(a), b, a)
top5   <- function(x){ x <- sort(x, decreasing = TRUE); sum(x[seq_len(ceiling(0.05 * length(x)))]) / max(sum(x), 1) }

for(scName in names(scenarios)){
  sc <- scenarios[[scName]]
  cat(sprintf("\n================ %s ================\n", scName))

  norm <- run(sc)
  ts   <- run(sc, twoStage = list(keepLibrary = TRUE))
  net  <- if(!is.null(norm$out$network)) norm$out$network else sc$net
  et   <- vftGraphTibble(net, "edges")
  vt   <- vftGraphTibble(net, "nodes")
  rowOf <- vftEdgeRowLookup(et$edgeID)
  lib  <- ts$res$paths
  dp   <- ts$res$dayPop
  lg   <- ts$res$legs
  has  <- lg$agent
  pick <- dp$pathId[has]
  S    <- ts$res$twoStage
  cat(sprintf("(%d agents; normal %.1f s, two-stage %.1f s of which phase 2 %.1f s; %d walks)\n",
              nrow(dp), norm$secs, ts$secs, S$secs, nrow(lib)))

  #### A ####
  cat("A. phase 1 is the normal simulation\n")
  ok("the normal run's passage step ran once", norm$cap$n == 1L)
  ok("the two-stage run's passage step ran once (phase 2 only)", ts$cap$n == 1L)
  na  <- norm$cap$agents
  wk  <- lengths(na$priorEAOI) > 0 & !(na$AOIGoal %in% "NO_GOAL")
  ok("the same explorers walked as the normal run's agents",
     identical(as.integer(na$id[wk]), as.integer(lib$explorer)))
  ok("with the same walks, edge for edge",
     identical(lapply(na$priorEAOI[wk], as.integer), unname(lib$edges)))
  ok("towards the same areas", identical(as.character(na$AOIGoal[wk]), lib$AOI))
  ok("phase 1 had as many agents as the normal run", S$nExplorers == nrow(norm$res$dayPop))

  #### B ####
  cat("B. the path library\n")
  ok("every walk has one node more than edges", all(lengths(lib$nodes) == lengths(lib$edges) + 1L))
  ok("starts at its entrance, ends at its exit",
     all(vapply(lib$nodes, `[`, 1L, 1L) == lib$entryV) &&
       all(vapply(lib$nodes, function(n) n[length(n)], 1L) == lib$exitV))
  chained <- function(nodes, edges){
    k <- rep.int(seq_along(edges), lengths(edges))
    e <- unlist(edges); rr <- rowOf(e)
    a <- unlist(lapply(nodes, function(n) n[-length(n)]))
    b <- unlist(lapply(nodes, function(n) n[-1]))
    !anyNA(rr) && all((et$from[rr] == a & et$to[rr] == b) | (et$from[rr] == b & et$to[rr] == a))
  }
  ok("each walk is a chain of edges along its nodes", chained(lib$nodes, lib$edges))
  aoiOf <- as.character(vt$AOI)
  ok("node ids are the vertex rows (what the engine assumes)",
     identical(as.integer(vt$nodeID), seq_len(nrow(vt))))
  ok("every node of every walk lies in an area", all(aoiOf[unlist(lib$nodes)] != "0"))
  ok("each entrance lies in the walk's area", all(aoiOf[lib$entryV] == lib$AOI))
  inRange <- vapply(seq_len(nrow(lib)), function(i){
    col <- VFT_DULN_TYPE_COLS[[lib$agentTyp[i]]]
    rng <- range(et[[col]][rowOf(lib$edges[[i]])], na.rm = TRUE)
    is.na(lib$DULN[i]) || (lib$DULN[i] >= rng[1] - 1e-9 && lib$DULN[i] <= rng[2] + 1e-9)
  }, logical(1))
  ok("each walk's mean DULN lies within its edges' DULN", all(inRange))
  ok("almost every walk has a mean DULN", mean(is.finite(lib$DULN)) > 0.99,
     sprintf("(%d NA)", sum(!is.finite(lib$DULN))))

  #### C ####
  cat("C. the choice\n")
  ok("own type", all(dp$agentTyp[has] == lib$agentTyp[pick]))
  same <- dp$timeExt[has] == lib$timeExt[pick]
  ok("own duration, or flagged as the fallback", all(same | dp$pathFallback[has]))
  if(any(!same)){
    nearest <- vapply(which(!same), function(k){
      i <- has[k]
      durs <- sort(unique(lib$timeExt[lib$agentTyp == dp$agentTyp[i] & is.finite(lib$DULN)]))
      lib$timeExt[pick[k]] == durs[which.min(abs(durs - dp$timeExt[i]))]
    }, logical(1))
    ok("a fallback is the nearest duration on offer", all(nearest))
  }
  reach <- ts$res$twoStage$params$reach
  hh <- pmax(dp$entryDistance[has] / (1000 * dp$speed[has]), 0.01)
  ok("every entrance within reach", all(hh <= reach * dp$timeExt[has] / 60 + 1e-12))
  lp  <- generateAdjListAndDistTbl_cpp(edgeTable = et, vertexTable = vt)
  adj <- adjListsToPtr_cpp(lp)
  smp <- has[unique(round(seq(1, length(has), length.out = min(300, length(has)))))]
  sp  <- findShortestRoute_cpp(adj, "little_attr", as.integer(dp$startV[smp]),
                               as.integer(dp$goalV[smp]), dp$agentTyp[smp])
  ok("the entrance distance is the route finder's (_attr, 300 agents)",
     isTRUE(all.equal(sp$distance, dp$entryDistance[smp], tolerance = 1e-9)))
  ok("nobody without a walk has an area", all(dp$AOIGoal[-has] == "NO_GOAL") && all(dp$goalV[-has] == -1))
  #the fallback, which a library drawn from the same population rarely needs:
  #take one type's 120-minute walks away
  tt   <- names(sort(table(lib$agentTyp[lib$timeExt == 120]), decreasing = TRUE))[1]
  lib2 <- lib[!(lib$agentTyp == tt & lib$timeExt == 120), ]
  ch2  <- vftChoosePaths(dp, lib2, adj, ts$res$twoStage$params)
  hit  <- which(dp$agentTyp == tt & dp$timeExt == 120)
  durs <- sort(unique(lib2$timeExt[lib2$agentTyp == tt]))
  want <- durs[which.min(abs(durs - 120))]
  got  <- lib2$timeExt[ch2$pick[hit]]
  ok(sprintf("no 120-min %s walks: those agents fall back to %g min", tt, want),
     all(ch2$fallback[hit]) && all(got[!is.na(got)] == want) && any(!is.na(got)),
     sprintf("(%d agents, %d with a walk)", length(hit), sum(!is.na(got))))
  ok("and nobody else's fallback changes", identical(ch2$fallback[-hit], dp$pathFallback[-hit]))

  #### D ####
  cat("D. the trip and its passage\n")
  ok("the way there runs start -> entrance",
     all(vapply(lg$go$nodes, `[`, 1L, 1L) == dp$startV[has]) &&
       all(vapply(lg$go$nodes, function(n) n[length(n)], 1L) == dp$goalV[has]))
  ok("the way home runs exit -> start",
     all(vapply(lg$back$nodes, `[`, 1L, 1L) == lib$exitV[pick]) &&
       all(vapply(lg$back$nodes, function(n) n[length(n)], 1L) == dp$startV[has]))
  ok("both are chains of edges, first edge included",
     chained(lg$go$nodes, lg$go$edges) && chained(lg$back$nodes, lg$back$edges))
  realE <- function(e) unique(e[e %in% et$edgeID])
  inside <- sum(vapply(lib$edges[pick], function(e) length(realE(e)), 1L))
  legs   <- sum(mapply(function(a, b) length(realE(c(a, b))), lg$go$edges, lg$back$edges))
  ok("passageAOI = the chosen walks, once per agent and edge",
     sum(edgeCol(ts, "passageAOI")) == inside, sprintf("(%d)", inside))
  ok("passage = the legs + the walks, once per agent and edge each",
     sum(edgeCol(ts, "passage")) == legs + inside, sprintf("(%d + %d)", legs, inside))
  ok("the per-type columns add up to the total",
     sum(edgeCol(ts, "passageWalk"), edgeCol(ts, "passageDog"), edgeCol(ts, "passageBike"),
         edgeCol(ts, "passageJog")) == sum(edgeCol(ts, "passage")))

  #### E ####
  cat("E. the coefficients\n")
  #the calibrated pair on the area choice's wide bounds is a soft choice, where one
  #term alone moves little; ten times either term alone shows the direction plainly
  attrOnly <- run(sc, twoStage = list(bAttr = 10 * VFT_TWO_STAGE$bAttr, bDist = 0))
  distOnly <- run(sc, twoStage = list(bAttr = 0, bDist = 10 * VFT_TWO_STAGE$bDist))
  ok("the reach rule does not depend on them",
     attrOnly$res$twoStage$nNoPath == S$nNoPath && distOnly$res$twoStage$nNoPath == S$nNoPath)
  mD <- function(r) mean(r$res$dayPop$pathDULN, na.rm = TRUE)
  mE <- function(r) mean(r$res$dayPop$entryDistance, na.rm = TRUE)
  ok("DULN alone (x10) chooses more attractive walks", mD(attrOnly) > mD(ts),
     sprintf("(mean DULN %.2f -> %.2f; library %.2f)", mD(ts), mD(attrOnly), mean(lib$DULN, na.rm = TRUE)))
  ok("distance alone (x10) chooses nearer entrances", mE(distOnly) < mE(ts),
     sprintf("(mean entrance %.0f m -> %.0f m)", mE(ts), mE(distOnly)))
  #the logit scale: exp(U / 0.5) = exp(2U), and doubling is exact in floating point,
  #so alpha = 0.5 must choose exactly what both coefficients doubled choose
  pr <- ts$res$twoStage$params
  pA <- pr; pA$alpha <- 0.5
  p2 <- pr; p2$bAttr <- 2 * pr$bAttr; p2$bDist <- 2 * pr$bDist
  seed(); chA <- vftChoosePaths(dp, lib, adj, pA)
  seed(); ch2x <- vftChoosePaths(dp, lib, adj, p2)
  ok("alpha = 0.5 chooses what both coefficients x2 choose", identical(chA, ch2x))
  seed(); ch1 <- vftChoosePaths(dp, lib, adj, pr)
  ok("and differs from alpha = 1", !identical(chA$pick, ch1$pick))
  cat(sprintf("  alpha 0.5 vs 1: mean walk DULN %.2f vs %.2f, mean entrance %.0f m vs %.0f m\n",
              mean(lib$DULN[chA$pick], na.rm = TRUE), mean(lib$DULN[ch1$pick], na.rm = TRUE),
              mean(chA$distance, na.rm = TRUE), mean(ch1$distance, na.rm = TRUE)))

  #### F ####
  cat("F. determinism\n")
  again <- run(sc, twoStage = list(keepLibrary = TRUE))
  ok("the same seed gives the same passage and agents",
     identical(igraph::edge_attr(again$res$pathUsage), igraph::edge_attr(ts$res$pathUsage)) &&
       identical(again$res$dayPop, ts$res$dayPop))

  #### G ####
  cat("G. a heatwave two-stage run\n")
  e2056 <- sf::st_transform(et$SHAPE, 2056)
  bb  <- sf::st_bbox(e2056)
  tpl <- terra::rast(xmin = bb[["xmin"]] - 300, xmax = bb[["xmax"]] + 300,
                     ymin = bb[["ymin"]] - 300, ymax = bb[["ymax"]] + 300,
                     resolution = 10, crs = "EPSG:2056")
  hot <- terra::init(tpl, "x")
  hot <- (hot - bb[["xmin"]]) / (bb[["xmax"]] - bb[["xmin"]]) * 10     #0 K west -> 10 K east
  hw  <- tryCatch(run(sc, twoStage = TRUE, heat = hot), error = function(err) err)
  ok("runs", !inherits(hw, "error"), if(inherits(hw, "error")) conditionMessage(hw) else "")
  if(!inherits(hw, "error")){
    ok("and is labelled a heatwave run and a two-stage run",
       !is.null(hw$res$heatwave) && !is.null(hw$res$twoStage))
    ok("passage ran once", hw$cap$n == 1L)
  }

  #### report ####
  cat("Report\n")
  ex <- norm$res$dayPop
  cat(sprintf("  phase 1: %d explorers, %d without an area (%.1f %%), %d walked (%.1f %%)\n",
              S$nExplorers, S$nExplorerNoArea, 100 * S$nExplorerNoArea / S$nExplorers,
              S$nPaths, 100 * S$nPaths / S$nExplorers))
  #launchSim()'s "back home" test retires an agent whose start node IS its area's
  #nearest node on the first timestep, before it walks
  noWalk  <- na$AOIGoal != "NO_GOAL" & lengths(na$priorEAOI) == 0
  startIn <- aoiOf[as.integer(na$startV)] == as.character(na$AOIGoal)
  cat(sprintf("           %d with an area but no walk; %d of them start on a node of that area\n",
              sum(noWalk), sum(noWalk & startIn)))
  cat(sprintf("  phase 2: %d agents, %d without a walk in reach (%.1f %%), %d on a fallback duration\n",
              S$nAgents, S$nNoPath, 100 * S$nNoPath / S$nAgents, S$nFallback))
  byDur <- tapply(is.na(dp$pathId), dp$timeExt, mean)
  cat("           no walk in reach, by duration:", paste(sprintf("%s min %.0f %%", names(byDur), 100 * byDur),
                                                         collapse = ", "), "\n")
  byTyp <- tapply(is.na(dp$pathId), dp$agentTyp, mean)
  cat("           by type:", paste(sprintf("%s %.0f %%", names(byTyp), 100 * byTyp), collapse = ", "), "\n")
  sh <- function(x){ t <- table(x[x != "NO_GOAL"]); t / sum(t) }
  a1 <- sh(ex$AOIGoal); a2 <- sh(dp$AOIGoal)
  ar <- union(names(a1), names(a2))
  cat(sprintf("  area shares normal vs two-stage: cor %.3f over %d areas; top area %s %.1f %% vs %.1f %%\n",
              stats::cor(as.numeric(a1[ar]) %|% 0, as.numeric(a2[ar]) %|% 0), length(ar),
              names(which.max(a1)), 100 * max(a1), 100 * (a2[names(which.max(a1))] %|% 0)))
  for(nm in c("passage", "passageAOI")){
    x <- edgeCol(norm, nm); y <- edgeCol(ts, nm)
    cat(sprintf("  %-10s normal %7d vs two-stage %7d; cor %.3f; edges used %d vs %d; top-5 %% share %.2f vs %.2f\n",
                nm, sum(x), sum(y), stats::cor(x, y), sum(x > 0), sum(y > 0), top5(x), top5(y)))
  }
  three <- run(sc, twoStage = list(explorers = 3))
  S3 <- three$res$twoStage
  cat(sprintf("  explorers = 3: %d walks, %d of %d without a walk in reach (%.1f %%); passage cor with normal %.3f\n",
              S3$nPaths, S3$nNoPath, S3$nAgents, 100 * S3$nNoPath / S3$nAgents,
              stats::cor(edgeCol(norm, "passage"), edgeCol(three, "passage"))))
}

cat(sprintf("\n%s - %d failure(s)\n", if(fails == 0) "ALL PASS" else "FAILURES", fails))
if(fails > 0) quit(status = 1)
