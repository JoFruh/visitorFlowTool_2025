#### The area choice re-fitted on the Glatt survey's routes ####
#
# The engine's area utility (src/CPP_FUNCTIONS.cpp ~2384; aoiProbsR() in
# data-raw/engine_corrections.R)
#   U = bAttr * dbSTD + bDist * dSTD,   P ~ exp(U) over the areas within reach
# carries a pair fitted on GlattPark (H:/Jo/Project Visitor Flow Tool/R/
# pathUsageComparisons/Calibration.R: logitr, every visited polygon one choice,
# distance from the park, the reported duration). This fits the same model to the
# FilBleu routes of the wider Glatt region, one choice per route:
#
#   choice     the area the route spends the most metres in (>= VISIT_MIN_M)
#   origin     the respondent's home (filbleu_addresses.csv), snapped to the nearest
#              node with residents - where the simulation starts its agents. Routes
#              reached by car or public transport are left out (home is not where
#              they start), as the runs that use the fit start nobody at car parks.
#   type       the route's agent type by activity and motives (typedLines() in
#              data-raw/tailor_duln_types.R); its speed is the engine's
#   duration   the WHOLE drawn route at that speed (the outing as VFT_VAL_OUTING=whole
#              simulates it: there, inside, and back), routes < 100 m dropped,
#              capped at DUR_CAP. The same durations, per type, are what the runs
#              draw (VFT_VAL_DURSRC = rum/durations.rds).
#   features   dbSTD and dSTD exactly as aoiProbsR() computes them: the engine's own
#              "_attr" distances (findClosestAOI_cpp() called for the respondents
#              inside a Full - or Nearby - scenario), the areas' DULN and size, the
#              standardisation bounded by DUR_RANGE (30, 240 minutes)
#   choice set the areas within reach as the corrected engine has it: real path time
#              <= max(reach * duration, 5 minutes). A route whose area lies beyond, or
#              that would get only the parameter-free closest-area fallback, is left
#              out and counted.
#
# Models: the engine's pair (scored only); distance only, on the Nearby scenario's
# distances (-> nearbyRumG); pooled (-> fullRumG); one pair per activity group -
# walking, dog walking, biking, jogging - shrunk to the pooled one by a ridge
# penalty on (b_g - b_pooled) / SE, lambda by respondent-blocked CV (-> fullRumGT).
# Standard errors are clustered by respondent. Sensitivities (reported only): no
# reach, reach .5, every visited area as a choice, the car and public-transport
# routes kept (from home).
#
#   Rscript data-raw/calibrate_aoi_rum.R
#
# VFT_VAL_OUT      a prepared validation folder (aoi, survey, network); writes <OUT>/rum
# VFT_RUM_REACH    the reach of the main fit (default 0.33, CORR's)

Sys.setenv(VFT_VAL_WORKER = "1", VFT_VAL_ENGINE = "corrected", VFT_VAL_FILBLEU = "full")
for(v in c("VFT_VAL_RUM", "VFT_VAL_DURSRC", "VFT_VAL_OUTING", "VFT_VAL_DURATION")) Sys.unsetenv(v)
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "validate_usage_surveys.R"))

RUM_DIR <- outPath("rum")
dir.create(RUM_DIR, showWarnings = FALSE)
rumPath <- function(f) file.path(RUM_DIR, f)
REACH   <- envNum("VFT_RUM_REACH", CORR$reach)
DUR_CAP <- 480            #minutes; launchSim() stops every agent at 600
SNAP_MAX <- 500           #metres from home to its node
MIN_ROUTE_M <- 100
ADDRESSES <- file.path(ONEDRIVE, "Local Data/filBleuResidents/filbleu_addresses.csv")
GROUP_OF <- c(walkNat = "walking", walkSoc = "walking", dogNat = "dog walking", dogProx = "dog walking",
              ebikeNat = "biking", bikeSport = "biking", jogger = "jogging")
ENGINE_B <- c(bAttr = 3.23695, bDist = -6.65703)
CV_FOLDS <- 5
LAMBDAS  <- c(0, 0.1, 0.3, 1, 3, 10, 30, 100, 1e6)
set.seed(20261020)

#### the routes ####

#the engine's speed per type
TYPE_SPEED <- local({
  d <- ORIG$vftAgentTimeAndSpeed(data.frame(id = seq_along(AGENT_TYPES), agentTyp = AGENT_TYPES))
  stats::setNames(d$speed, AGENT_TYPES)
})

#as data-raw/tailor_duln_types.R types them (the user's rules, 2026-10-18)
IMPORTANCE <- c("unwichtig" = 1, "eher unwichtig" = 2, "weder / noch" = 3, "eher wichtig" = 4, "wichtig" = 5)
typeRoutes <- function(x){
  n <- unname(IMPORTANCE[x$nature]); s <- unname(IMPORTANCE[x$social])
  t <- rep(NA_character_, nrow(x))
  w <- x$layer == "walking"
  t[w] <- ifelse(!is.na(n[w]) & !is.na(s[w]) & s[w] >= n[w], "walkSoc", "walkNat")
  d <- x$layer == "dogWalking"
  t[d] <- ifelse(x$nature[d] %in% "wichtig", "dogNat", "dogProx")
  t[x$layer == "jogging_nordicWalking"] <- "jogger"
  t[x$layer == "bike"] <- "bikeSport"
  t[x$layer == "E_bike"] <- "ebikeNat"
  t
}

stageRoutes <- function(){
  f <- rumPath("routes.rds")
  if(file.exists(f)) return(readRDS(f))
  x  <- readSurveyLines("filBleu")
  sv <- readRDS(surveyFile("glatt"))
  if(!identical(sv$resp, x$resp)) stop("the survey's lines are not the FilBleu routes - check VFT_VAL_FILBLEU")
  aoi <- readRDS(aoiFile("glatt"))
  ac  <- clipAois(aoi)
  r <- data.frame(line = seq_len(nrow(x)), id = x$id, resp = x$resp, layer = x$layer,
                  type = typeRoutes(x), stringsAsFactors = FALSE)
  r$group  <- unname(GROUP_OF[r$type])
  r$access <- ifelse(grepl("Auto", x$access), "car", ifelse(grepl("Verkehr", x$access), "public transport",
              ifelse(is.na(x$access), "no answer", "home, foot or bike")))
  r$len_m  <- as.numeric(sf::st_length(x))
  r$speed  <- unname(TYPE_SPEED[r$type])
  r$T_raw  <- r$len_m / (r$speed * 1000) * 60
  r$T      <- pmin(r$T_raw, DUR_CAP)
  #the areas each route visits, by metres inside (the survey stage's intersection)
  ints <- suppressWarnings(sf::st_intersection(sf::st_sf(line = r$line, geometry = sf::st_geometry(x)), ac[, "AOI"]))
  v <- stats::aggregate(len ~ line + AOI, data.frame(line = ints$line, AOI = ints$AOI,
                                                     len = as.numeric(sf::st_length(ints))), sum)
  v <- v[v$len >= VISIT_MIN_M, ]
  v <- v[order(v$line, -v$len), ]
  prim <- v[!duplicated(v$line), ]
  r$area   <- prim$AOI[match(r$line, prim$line)]
  r$nAreas <- as.integer(table(factor(v$line, levels = r$line)))
  #home: the respondent's address, to the nearest node with residents
  a  <- utils::read.csv(ADDRESSES)
  k  <- match(sub("^f", "", r$resp), as.character(a$respondent))
  hx <- a$GKODE[k]; hy <- a$GKODN[k]
  net <- readRDS(netFile("glatt"))$net
  va  <- igraph::vertex_attr(net)
  res <- va$Residents
  okN <- which(res > 0 & is.finite(res))
  xy  <- sf::sf_project("EPSG:4326", "EPSG:2056", cbind(va$X[okN], va$Y[okN]))
  r$home <- NA_integer_; r$snap_m <- NA_real_
  has <- which(is.finite(hx) & is.finite(hy))
  for(i in has){
    d2 <- (xy[, 1] - hx[i])^2 + (xy[, 2] - hy[i])^2
    j  <- which.min(d2)
    r$home[i] <- as.integer(va$nodeID[okN[j]]); r$snap_m[i] <- sqrt(d2[j])
  }
  out <- list(routes = r, visits = v)
  saveRDS(out, f)
  logf("routes: %d (%d without an activity type), %d with a main area, %d with a home within %d m",
       nrow(r), sum(is.na(r$type)), sum(!is.na(r$area)), sum(r$snap_m <= SNAP_MAX, na.rm = TRUE), SNAP_MAX)
  out
}

#The durations the runs draw: every route from home, on foot or by bike (or without an
#answer) of at least MIN_ROUTE_M, whatever area it visits, per type.
stageDurations <- function(rt){
  f <- rumPath("durations.rds")
  r <- rt$routes
  use <- !is.na(r$type) & !r$access %in% c("car", "public transport") & r$len_m >= MIN_ROUTE_M
  durs <- lapply(stats::setNames(AGENT_TYPES, AGENT_TYPES), function(k) r$T[use & r$type == k])
  if(any(lengths(durs) == 0)) stop("no routes for type(s) ", paste(names(durs)[lengths(durs) == 0], collapse = ", "))
  saveRDS(list(durs = durs, cap = DUR_CAP, minRoute = MIN_ROUTE_M, speeds = TYPE_SPEED,
               source = "FilBleu routes, whole drawn line at the type's engine speed"), f)
  #against the engine's own draw (vftAgentTimeAndSpeed()), 20000 agents per type
  eng <- local({
    set.seed(1)
    d <- ORIG$vftAgentTimeAndSpeed(data.frame(id = seq_len(7 * 20000), agentTyp = rep(AGENT_TYPES, each = 20000)))
    split(d$timeExt, d$agentTyp)
  })
  tab <- do.call(rbind, lapply(AGENT_TYPES, function(k){
    q <- stats::quantile(durs[[k]], c(.1, .5, .9))
    data.frame(type = k, routes = length(durs[[k]]), capped = sum(r$T_raw[use & r$type == k] > DUR_CAP),
               route_p10 = q[[1]], route_median = q[[2]], route_p90 = q[[3]], route_mean = mean(durs[[k]]),
               engine_median = stats::median(eng[[k]]), engine_mean = mean(eng[[k]]))
  }))
  utils::write.csv(tab, rumPath("durations.csv"), row.names = FALSE)
  logf("durations per type (minutes), the routes vs the engine:")
  print(format(tab, digits = 3), row.names = FALSE)
  invisible(tab)
}

#### the engine's features for the respondents ####

#One scenario of `type` at Glatt, stopped inside the area choice: findClosestAOI_cpp()
#is then called for the respondents (start nodes, speeds, types) on the scenario's own
#adjacency lists and areas - the distances its utility reads.
captureFeatures <- function(r, type){
  f <- rumPath(paste0("capture_", type, ".rds"))
  if(file.exists(f)){
    got <- readRDS(f)
    if(identical(got$ids, r$id)) return(got)
  }
  sc <- scenarioFor("glatt", type)
  got <- NULL
  nsSet("findClosestAOI_cpp", function(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds,
                                       agentDurations, AOI_aois, AOI_dulns, agentTyps, AOI_areas){
    res <- ORIG$findClosestAOI_cpp(AOIList_o, AOI_v, adj_ptr, as.integer(r$home), r$speed, r$T,
                                   AOI_aois, AOI_dulns, r$type, AOI_areas)
    got <<- list(res = res[c("distances", "aoi")], AOI_aois = AOI_aois, AOI_dulns = AOI_dulns,
                 AOI_areas = AOI_areas, popDur = range(agentDurations))
    stop(structure(class = c("vftCaptured", "error", "condition"), list(message = "captured", call = NULL)))
  })
  on.exit(nsSet("findClosestAOI_cpp", ORIG$findClosestAOI_cpp))
  set.seed(BASE_SEED + 1L, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  quiet <- file(nullfile(), open = "wt"); sink(quiet)
  tryCatch(ns$vftSimulateScenario(sc$net, sc$polys, sc$thr, parking = sc$parking,
                                  residentDivision = sc$residentDivision,
                                  progPrep = fakeProgress(), progSim = fakeProgress()),
           vftCaptured = function(e) NULL, finally = { sink(); close(quiet) })
  if(is.null(got)) stop("the area choice was never reached")
  got$trueD <- trueAreaDistances("glatt", sc$net)
  got$ids <- r$id
  saveRDS(got, f)
  got
}

#The long table: one row per (route, area the engine found), with aoiProbsR()'s
#features at the route's duration and the reach test at `reach`.
lastMatch <- function(x, table){ k <- match(x, rev(table)); ifelse(is.na(k), NA_integer_, length(table) + 1L - k) }
longTable <- function(r, cap, reach, chosenOf = NULL){
  dulnMax <- max(cap$AOI_dulns); dulnMin <- min(cap$AOI_dulns)
  durMin <- DUR_RANGE[1]; durMax <- DUR_RANGE[2]
  rows <- lapply(seq_len(nrow(r)), function(i){
    d <- cap$res$distances[[i]]
    if(!length(d)) return(NULL)
    a <- as.character(cap$res$aoi[[i]])
    speed <- r$speed[i]; dur <- r$T[i]
    tdMax <- max(d) / (2 * 1000); tdMin <- min(d) / (20 * 1000)
    td <- d / (speed * 1000); td[td < 0.01] <- 0.01
    k <- lastMatch(a, cap$AOI_aois)
    duln <- cap$AOI_dulns[k]; area <- sqrt(cap$AOI_areas[k]); area[area > 7000] <- 7000
    duln[is.na(k)] <- 0; area[is.na(k)] <- 0
    dpd <- td / dur; dbapd <- duln * area / dur
    dpdMax <- tdMax / durMin; dpdMin <- tdMin / durMax
    dbMax <- (dulnMax * 15000) / durMin; dbMin <- (dulnMin * 316) / durMax
    tR <- cap$trueD[r$home[i], match(a, colnames(cap$trueD))] / (speed * 1000)
    data.frame(obs = i, area = a,
               dbSTD = ((10 - 1) * (dbapd - dbMin) / (dbMax - dbMin)) + 1,
               dSTD  = ((10 - 1) * (dpd - dpdMin) / (dpdMax - dpdMin)) + 1,
               tReach = tR, inReach = is.finite(tR) & tR <= max(reach * dur / 60, CORR$floorH),
               chosen = a == r$area[i], stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

#Keep the routes the model can speak to: their area found and within reach.
choiceSet <- function(L, r, reach){
  byObs <- split(L, L$obs)
  why <- vapply(seq_len(nrow(r)), function(i){
    x <- byObs[[as.character(i)]]
    if(is.null(x)) return("network: not every area reached")
    if(!any(x$chosen)) return("main area not among the engine's")
    if(!any(x$inReach)) return("nothing in reach (fallback only)")
    if(!x$inReach[x$chosen]) return("main area beyond reach")
    "fitted"
  }, "")
  L <- L[L$obs %in% which(why == "fitted") & L$inReach, ]
  list(L = L, why = why)
}

#### the conditional logit ####

#LL, score per observation and information at b, over the rows of L (cols = X)
clEval <- function(b, L, cols){
  X <- as.matrix(L[, cols, drop = FALSE])
  g <- match(L$obs, unique(L$obs))
  u <- drop(X %*% b)
  m <- as.vector(tapply(u, g, max))[g]
  e <- exp(u - m)
  s <- as.vector(rowsum(e, g, reorder = FALSE))[g]
  p <- e / s
  ll <- sum((u - m - log(s))[L$chosen])
  S  <- rowsum((L$chosen - p) * X, g, reorder = FALSE)
  xb <- rowsum(p * X, g, reorder = FALSE)[g, , drop = FALSE]
  Z  <- (X - xb) * sqrt(p)
  list(ll = ll, S = S, I = crossprod(Z), p = p, g = g)
}

#Newton with step halving; `pen` = list(b0, w): the penalty sum(w * (b - b0)^2)
clFit <- function(L, cols, b = rep(0, length(cols)), pen = NULL, cluster = NULL){
  pll <- function(e, b) e$ll - if(is.null(pen)) 0 else sum(pen$w * (b - pen$b0)^2)
  e <- clEval(b, L, cols); f0 <- pll(e, b)
  for(it in 1:200){
    gr <- colSums(e$S) - if(is.null(pen)) 0 else 2 * pen$w * (b - pen$b0)
    H  <- e$I + if(is.null(pen)) 0 else diag(2 * pen$w, length(b))
    step <- solve(H, gr)
    t <- 1
    repeat{
      b1 <- b + t * step; e1 <- clEval(b1, L, cols); f1 <- pll(e1, b1)
      if(is.finite(f1) && f1 >= f0 - 1e-12) break
      t <- t / 2
      if(t < 1e-8) break
    }
    conv <- abs(f1 - f0) < 1e-10
    b <- b1; e <- e1; f0 <- f1
    if(conv) break
  }
  names(b) <- cols
  out <- list(b = b, ll = e$ll, n = length(unique(L$obs)), iter = it)
  if(!is.null(cluster)){
    #sandwich, clustered by respondent
    cl <- cluster[match(unique(L$obs), names(cluster))]
    Sc <- rowsum(e$S, cl)
    Hi <- solve(e$I + if(is.null(pen)) 0 else diag(2 * pen$w, length(b)))
    V  <- Hi %*% crossprod(Sc) %*% Hi
    out$se <- stats::setNames(sqrt(diag(V)), cols)
    out$seNaive <- stats::setNames(sqrt(diag(Hi)), cols)
  }
  out
}
llAt <- function(b, L, cols) if(nrow(L)) clEval(b, L, cols)$ll else 0

#per-group pairs, each shrunk to b0 by lambda * ((b_g - b0) / se)^2
groupFit <- function(L, grp, b0, se, lambda){
  lapply(stats::setNames(unique(grp), unique(grp)), function(g){
    Lg <- L[grp[L$obs] == g, ]
    if(!nrow(Lg)) return(b0)
    if(lambda >= 1e6) return(b0)
    pen <- if(lambda > 0) list(b0 = b0, w = lambda / se^2) else NULL
    clFit(Lg, names(b0), b = b0, pen = pen)$b
  })
}

#### stage fit ####

stageFit <- function(rt){
  r0 <- rt$routes
  keep <- !is.na(r0$type) & !is.na(r0$area) & r0$len_m >= MIN_ROUTE_M & !is.na(r0$home) & r0$snap_m <= SNAP_MAX
  smp <- data.frame(step = c("routes of the five activities", "with a main area (>= 100 m inside)",
                                "of at least 100 m", "with a home within 500 m of a node"),
                       routes = c(sum(!is.na(r0$type)), sum(!is.na(r0$type) & !is.na(r0$area)),
                                  sum(!is.na(r0$type) & !is.na(r0$area) & r0$len_m >= MIN_ROUTE_M), sum(keep)))
  rAll <- r0[keep, ]
  rownames(rAll) <- NULL
  capF <- captureFeatures(rAll, "full")
  capN <- captureFeatures(rAll, "nearby")
  noCar <- !rAll$access %in% c("car", "public transport")
  smp <- rbind(smp, data.frame(step = "not reached by car or public transport", routes = sum(noCar)))

  fitSet <- function(reach, use = noCar, cap = capF){
    L  <- longTable(rAll, cap, reach)
    L  <- L[L$obs %in% which(use), ]
    cs <- choiceSet(L, rAll, reach)
    cs$why[!use] <- "left out (car / public transport)"
    cs
  }
  csF <- fitSet(REACH); csN <- fitSet(REACH, cap = capN)
  why <- table(factor(csF$why))
  logf("routes by outcome (Full features, reach %.2f):", REACH); print(why)
  byType <- as.data.frame.matrix(table(rAll$type, csF$why))
  utils::write.csv(cbind(type = rownames(byType), byType), rumPath("sample_by_type.csv"), row.names = FALSE)
  smp <- rbind(smp, data.frame(step = paste("fitted (main area within reach", REACH, ")"),
                                     routes = sum(csF$why == "fitted")))
  utils::write.csv(smp, rumPath("sample.csv"), row.names = FALSE)

  #how far the chosen areas lie, as a share of the route's duration
  Lc <- longTable(rAll, capF, REACH); Lc <- Lc[Lc$chosen & Lc$obs %in% which(noCar), ]
  ratio <- Lc$tReach / (rAll$T[Lc$obs] / 60)
  beyond <- data.frame(group = c(rAll$group[Lc$obs], rep("all", nrow(Lc))), ratio = c(ratio, ratio))
  far <- do.call(rbind, lapply(split(beyond$ratio, beyond$group), function(x) data.frame(
    routes = length(x), median = stats::median(x, na.rm = TRUE), p90 = stats::quantile(x, .9, na.rm = TRUE),
    beyond33 = mean(x > 0.33, na.rm = TRUE), beyond50 = mean(x > 0.5, na.rm = TRUE))))
  far <- cbind(group = rownames(far), far)
  utils::write.csv(far, rumPath("approach_share.csv"), row.names = FALSE)
  logf("the way to the main area as a share of the route's duration:")
  print(format(far, digits = 3), row.names = FALSE)

  cols <- c("dbSTD", "dSTD")
  cluster <- stats::setNames(rAll$resp, seq_len(nrow(rAll)))
  L <- csF$L; LN <- csN$L
  fits <- list()
  fits$pooled   <- clFit(L, cols, b = unname(ENGINE_B), cluster = cluster)
  fits$distance <- clFit(LN, "dSTD", b = ENGINE_B[["bDist"]], cluster = cluster)
  se <- fits$pooled$se

  #respondent-blocked folds, the same for every model
  resp <- unique(rAll$resp[as.integer(unique(L$obs))])
  set.seed(20261020)
  fold <- stats::setNames(sample(rep_len(seq_len(CV_FOLDS), length(resp))), resp)
  foldOf <- function(obs) fold[rAll$resp[obs]]
  grp <- stats::setNames(rAll$group, seq_len(nrow(rAll)))
  grpOf <- function(obs) rAll$group[obs]
  cvModel <- function(fitFun, LL0, colsM){
    tot <- 0
    for(k in seq_len(CV_FOLDS)){
      tr <- LL0[foldOf(LL0$obs) != k, ]; te <- LL0[foldOf(LL0$obs) == k, ]
      tot <- tot + fitFun(tr, te)
    }
    tot
  }
  nFit <- length(unique(L$obs))
  cv <- list(
    engine   = cvModel(function(tr, te) llAt(unname(ENGINE_B), te, cols), L),
    pooled   = cvModel(function(tr, te) llAt(clFit(tr, cols, b = unname(ENGINE_B))$b, te, cols), L),
    engine_distance = cvModel(function(tr, te) llAt(ENGINE_B[["bDist"]], te, "dSTD"), LN),
    distance = cvModel(function(tr, te) llAt(clFit(tr, "dSTD", b = ENGINE_B[["bDist"]])$b, te, "dSTD"), LN))
  #the per-group pairs: lambda by CV
  cvG <- vapply(LAMBDAS, function(lam) cvModel(function(tr, te){
    b0 <- clFit(tr, cols, b = unname(ENGINE_B))$b
    bg <- groupFit(tr, grp, b0, se, lam)
    sum(vapply(names(bg), function(g) llAt(bg[[g]], te[grpOf(te$obs) == g, ], cols), 1))
  }, L), 1)
  lamBest <- LAMBDAS[which.max(cvG)]
  logf("per-group lambda by CV: %s (held-out LL %s)", format(lamBest),
       paste(sprintf("%g:%.1f", LAMBDAS, cvG), collapse = " "))
  cv$types <- max(cvG); cv$types_unshrunk <- cvG[LAMBDAS == 0]
  bG <- groupFit(L, grp, fits$pooled$b, se, lamBest)
  #their SEs, unshrunk fits per group (clustered)
  gFits <- lapply(stats::setNames(names(bG), names(bG)), function(g)
    clFit(L[grpOf(L$obs) == g, ], cols, b = fits$pooled$b, cluster = cluster))

  #sensitivities of the pooled pair
  sens <- list()
  sens$no_reach  <- { cs <- fitSet(Inf); clFit(cs$L, cols, b = unname(ENGINE_B), cluster = cluster) }
  sens$reach_0.5 <- { cs <- fitSet(0.5); clFit(cs$L, cols, b = unname(ENGINE_B), cluster = cluster) }
  sens$car_pt_from_home <- { cs <- fitSet(REACH, use = rep(TRUE, nrow(rAll))); clFit(cs$L, cols, b = unname(ENGINE_B), cluster = cluster) }
  sens$every_visited_area <- local({
    #each visited area of a fitted route is one choice, as the GlattPark fit counted them
    vis <- rt$visits[rt$visits$line %in% rAll$line, ]
    Lv <- longTable(rAll, capF, REACH); Lv <- Lv[Lv$obs %in% which(noCar) & Lv$inReach, ]
    reps <- lapply(seq_len(nrow(vis)), function(j){
      i <- match(vis$line[j], rAll$line)
      x <- Lv[Lv$obs == i, ]
      if(!nrow(x) || !any(x$area == vis$AOI[j])) return(NULL)
      x$chosen <- x$area == vis$AOI[j]; x$obs <- j; x
    })
    Lx <- do.call(rbind, reps)
    clVis <- stats::setNames(rAll$resp[match(vis$line, rAll$line)], seq_len(nrow(vis)))
    clFit(Lx, cols, b = unname(ENGINE_B), cluster = clVis)
  })
  #...and with one route type's reach at 0.5, the pooled and distance fits the
  #conditional reach-0.5 runs would use
  cs5N <- fitSet(0.5, cap = capN)
  fit5 <- list(pooled = sens$reach_0.5, distance = clFit(cs5N$L, "dSTD", b = ENGINE_B[["bDist"]], cluster = cluster))

  #the table
  row <- function(model, f, cvll = NA, n = f$n){
    b <- f$b
    data.frame(model = model, routes = n,
               bAttr = if("dbSTD" %in% names(b)) b[["dbSTD"]] else 0,
               bAttr_se = if(!is.null(f$se) && "dbSTD" %in% names(f$se)) f$se[["dbSTD"]] else NA,
               bDist = b[["dSTD"]], bDist_se = if(!is.null(f$se)) f$se[["dSTD"]] else NA,
               ll = f$ll, ll_per_route = f$ll / n, cv_ll_per_route = cvll / nFit)
  }
  engF <- list(b = c(dbSTD = ENGINE_B[["bAttr"]], dSTD = ENGINE_B[["bDist"]]), ll = llAt(unname(ENGINE_B), L, cols), n = nFit)
  engN <- list(b = c(dSTD = ENGINE_B[["bDist"]]), ll = llAt(ENGINE_B[["bDist"]], LN, "dSTD"), n = length(unique(LN$obs)))
  tab <- rbind(row("engine (GlattPark pair)", engF, cv$engine),
               row("Glatt pooled", fits$pooled, cv$pooled),
               do.call(rbind, lapply(names(gFits), function(g) row(paste0("Glatt ", g, " (own fit)"), gFits[[g]]))),
               data.frame(model = paste0("Glatt ", names(bG), " (shrunk, lambda ", lamBest, ")"),
                          routes = vapply(names(bG), function(g) length(unique(L$obs[grpOf(L$obs) == g])), 1L),
                          bAttr = vapply(bG, `[[`, 1, 1), bAttr_se = NA, bDist = vapply(bG, `[[`, 1, 2), bDist_se = NA,
                          ll = vapply(names(bG), function(g) llAt(bG[[g]], L[grpOf(L$obs) == g, ], cols), 1), ll_per_route = NA,
                          cv_ll_per_route = NA),
               data.frame(model = "Glatt per group (CV of the shrunk pairs)", routes = nFit, bAttr = NA, bAttr_se = NA,
                          bDist = NA, bDist_se = NA, ll = NA, ll_per_route = NA, cv_ll_per_route = cv$types / nFit),
               row("engine distance term only (Nearby)", engN, cv$engine_distance),
               row("Glatt distance only (Nearby)", fits$distance, cv$distance),
               do.call(rbind, lapply(names(sens), function(s) row(paste0("sensitivity: ", s), sens[[s]]))))
  tab$ll_per_route <- tab$ll / tab$routes
  utils::write.csv(tab, rumPath("fits.csv"), row.names = FALSE)
  logf("the fits:")
  print(format(tab, digits = 4), row.names = FALSE)

  #predicted and observed area shares among the fitted routes
  shareOf <- function(b, LL0, colsM){ e <- clEval(b, LL0, colsM); tapply(e$p, LL0$area, sum) }
  A <- sort(unique(L$area))
  obs <- table(factor(L$area[L$chosen], levels = A))
  pick <- function(x) as.numeric(x[A])
  typePred <- local({
    s <- 0
    for(g in names(bG)){ Lg <- L[grpOf(L$obs) == g, ]; if(nrow(Lg)) s <- s + tapply(clEval(bG[[g]], Lg, cols)$p, factor(Lg$area, levels = A), sum) }
    s
  })
  sh <- data.frame(area = A, observed = as.numeric(obs) / nFit,
                   engine = pick(shareOf(unname(ENGINE_B), L, cols)) / nFit,
                   pooled = pick(shareOf(fits$pooled$b, L, cols)) / nFit,
                   types  = as.numeric(typePred) / nFit,
                   engine_distance = pick(shareOf(ENGINE_B[["bDist"]], LN, "dSTD")) / length(unique(LN$obs)),
                   distance = pick(shareOf(fits$distance$b, LN, "dSTD")) / length(unique(LN$obs)))
  sh[is.na(sh)] <- 0
  utils::write.csv(sh, rumPath("shares.csv"), row.names = FALSE)
  logf("area shares, observed vs predicted (fitted routes), top 12 by observed:")
  print(format(head(sh[order(-sh$observed), ], 12), digits = 3), row.names = FALSE)

  #the coefficient file the runs read (VFT_VAL_RUM)
  types <- data.frame(type = AGENT_TYPES, group = unname(GROUP_OF[AGENT_TYPES]),
                      bAttr = vapply(GROUP_OF[AGENT_TYPES], function(g) bG[[g]][[1]], 1),
                      bDist = vapply(GROUP_OF[AGENT_TYPES], function(g) bG[[g]][[2]], 1))
  coefs <- list(pooled = c(bAttr = fits$pooled$b[["dbSTD"]], bDist = fits$pooled$b[["dSTD"]]),
                distance = c(bAttr = 0, bDist = fits$distance$b[["dSTD"]]),
                types = types, lambda = lamBest, reach = REACH, durRange = DUR_RANGE,
                routes = nFit, made = format(Sys.time()))
  saveRDS(coefs, rumPath("coefs.rds"))
  #the reach-0.5 version, for the conditional runs; the per-type set stays the
  #reach-.33 one's shape, centred on the reach-0.5 pooled pair
  coefs5 <- coefs
  coefs5$pooled <- c(bAttr = fit5$pooled$b[["dbSTD"]], bDist = fit5$pooled$b[["dSTD"]])
  coefs5$distance <- c(bAttr = 0, bDist = fit5$distance$b[["dSTD"]])
  coefs5$types$bAttr <- coefs5$types$bAttr - coefs$pooled[["bAttr"]] + coefs5$pooled[["bAttr"]]
  coefs5$types$bDist <- coefs5$types$bDist - coefs$pooled[["bDist"]] + coefs5$pooled[["bDist"]]
  coefs5$reach <- 0.5
  saveRDS(coefs5, rumPath("coefs_r50.rds"))
  #the neutrality check's file: the engine's pair everywhere
  saveRDS(list(pooled = ENGINE_B, distance = c(bAttr = 0, bDist = ENGINE_B[["bDist"]]),
               types = data.frame(type = AGENT_TYPES, bAttr = ENGINE_B[["bAttr"]], bDist = ENGINE_B[["bDist"]])),
          rumPath("coefs_engine.rds"))
  print(types, row.names = FALSE)
  invisible(list(tab = tab, coefs = coefs, shares = sh, far = far, why = why))
}

if(sys.nframe() == 0L){
  rt <- stageRoutes()
  stageDurations(rt)
  stageFit(rt)
  logf("calibrate_aoi_rum: done, %s", RUM_DIR)
}
