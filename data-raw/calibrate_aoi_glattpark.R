#### The area choice re-fitted on the GlattPark survey, on a folder's own DULN ####
#
# The engine's area utility (src/CPP_FUNCTIONS.cpp ~2496; aoiProbsR() in
# data-raw/engine_corrections.R)
#   U = bAttr * dbSTD + bDist * dSTD,   P ~ exp(U) over the areas within reach
# carries the pair 3.23695 / -6.65703 that H:/Jo/Project Visitor Flow Tool/R/
# pathUsageComparisons/Calibration.R fitted on the GlattPark survey in 2024 (logitr,
# the areas of that time and the DULN of that time). This fits the same pair again on
# the same survey, against the areas and the area DULN of VFT_VAL_OUT (the SA5E
# folder, 2026-10-27: the user's request), as Calibration.R built it:
#   respondents  MA_SB_Quellgebiet_Umkodiert.sav: Q9 the minutes in the recreation
#                area (the duration), Q18 the activities, giving the speed: bike >
#                jogging > walking, everyone else left out (Calibration.R's
#                "Possibility 4"); here the engine's types and speeds (bikeSport 20,
#                jogger 7, walkNat 5 km/h)
#   choices      GitterBesuchteNaherholungsorte.csv: each respondent's visited grid
#                cells, unioned; EVERY area they overlap is one choice (as the 2024
#                fit counted them), an area touched only on its edge is not
#   origin       the GlattPark point of Calibration.R, snapped to the nearest node with
#                residents - where the simulation starts its agents
# and, unlike 2024, with the features as the simulation computes them (aoiProbsR():
# the engine's attractivity-weighted distances from findClosestAOI_cpp(), its 1-10
# scaling with DUR_RANGE's bounds) and its choice set (the areas within reach, real
# path time <= max(reach * duration, 5 minutes)), so the fitted pair means in the runs
# what it meant in the fit. The conditional logit, the clustered SEs and the engine
# features are data-raw/calibrate_aoi_rum.R's.
#
# Reported (fits.csv): the 2024 pair scored on the same choices, the refit, its
# respondent-blocked 5-fold CV log-likelihood, and sensitivities (no reach; straight
# distances from the GlattPark point and the population's bounds, as in 2024).
# Written to <VFT_VAL_OUT>/rum_glattPark: coefs.rds (VFT_VAL_RUM: pooled = the refit,
# every agent type the same pair), fits.csv, shares.csv, sample.csv.
#
# Stage time: the time-dependent utilities of data-raw/calibrate_aoi_rum_time.R (fixed,
# share, interaction: rumTimeX() in data-raw/engine_corrections.R) on the same visits,
# with T = Q9, the time IN the area as that script's A4 is. DULN is centred and scaled
# on GlattPark's areas. Written to rum_glattPark: coefs_time.rds (VFT_VAL_RUM_TIME),
# fits_time.csv, tradeoff_time.csv.
#
#   VFT_VAL_OUT=<folder> VFT_VAL_REACH=0.33 Rscript data-raw/calibrate_aoi_glattpark.R
#
# VFT_RUM_REACH   the reach of the choice set (default CORR's, VFT_VAL_REACH)
# VFT_GPC_DATA    the GlattPark survey folder
# VFT_GPC_STAGES  fit and/or time (default fit,time)

.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
#calibrate_aoi_rum.R's features and logit, and calibrate_aoi_rum_time.R's models
source(file.path(.repo, "data-raw", "calibrate_aoi_rum_time.R"))
#the two scripts' own output folders, left empty
for(d in c("rum", "rum_time")) if(!length(list.files(outPath(d)))) unlink(outPath(d), recursive = TRUE)

#rumPath() writes here from now on
RUM_DIR <- outPath("rum_glattPark")
dir.create(RUM_DIR, showWarnings = FALSE)
GPC_DATA <- Sys.getenv("VFT_GPC_DATA", file.path("C:/Users/frueh/Documents/Local Data",
  "Shared visitorFlow Project Data/04_Glattpark_survey"))
GP_CTX <- "glattPark"
#Calibration.R's GlattPark point (WGS84)
GP_POINT <- c(8.562899370224319, 47.42220963386703)

#### the choices ####

#Q18's free text to a type, in Calibration.R's order (bike, then jogging, then walking)
typeOfQ18 <- function(txt){
  w <- strsplit(ifelse(is.na(txt), "", txt), "[, ]")
  vapply(w, function(x){
    if(any(c("velo", "rad", "Rad", "Velo") %in% x)) "bikeSport" else
      if(any(c("joggen", "Joggen") %in% x)) "jogger" else
        if(any(c("Spazieren", "spaizieren", "sspazierengehen", "Spazierern", "Spazierne", "SPazieren") %in% x)) "walkNat" else
          NA_character_
  }, "")
}

stageChoices <- function(){
  q <- haven::read_sav(file.path(GPC_DATA, "MA_SB_Quellgebiet_Umkodiert.sav"))
  q <- data.frame(ID = as.numeric(q$ID), Q9 = as.numeric(q$Q9), type = typeOfQ18(as.character(q$Q18)))
  u <- utils::read.csv2(file.path(GPC_DATA, "GitterBesuchteNaherholungsorte.csv"))
  rc <- grep("^X[0-9]+$", colnames(u), value = TRUE)
  cells <- sf::st_as_sf(u, wkt = "WKT", crs = 2056)
  resp <- do.call(rbind, lapply(rc, function(k){
    g <- sf::st_geometry(cells)[!is.na(u[[k]])]
    if(!length(g)) return(NULL)
    sf::st_sf(ID = as.numeric(sub("^X", "", k)), geometry = sf::st_union(g))
  }))
  aoi <- readRDS(aoiFile(GP_CTX))
  ac  <- clipAois(aoi)
  ints <- suppressWarnings(sf::st_intersection(resp, ac[, "AOI"]))
  ints <- ints[as.numeric(sf::st_area(ints)) > 0, ]
  v <- data.frame(ID = ints$ID, area = ints$AOI, stringsAsFactors = FALSE)
  v <- v[!duplicated(v), ]
  #the respondents' start: the GlattPark point at its nearest node with residents
  net <- readRDS(netFile(GP_CTX))$net
  va  <- igraph::vertex_attr(net)
  okN <- which(va$Residents > 0 & is.finite(va$Residents))
  p   <- sf::sf_project("EPSG:4326", "EPSG:2056", matrix(GP_POINT, 1))
  xy  <- sf::sf_project("EPSG:4326", "EPSG:2056", cbind(va$X[okN], va$Y[okN]))
  d2  <- (xy[, 1] - p[1])^2 + (xy[, 2] - p[2])^2
  home <- as.integer(va$nodeID[okN[which.min(d2)]])
  #one row per (respondent, visited area): calibrate_aoi_rum.R's route columns
  r <- merge(v, q, by = "ID")
  smp <- data.frame(step = c("respondents in the survey", "with a visited cell", "visits to an area (respondent x area)",
                             "visits with Q9", "visits with a bike / jogging / walking activity (Q18)"),
                    n = c(nrow(q), nrow(resp), nrow(v), sum(is.finite(r$Q9) & r$Q9 > 0),
                          sum(is.finite(r$Q9) & r$Q9 > 0 & !is.na(r$type))))
  r <- r[is.finite(r$Q9) & r$Q9 > 0 & !is.na(r$type), ]
  r <- r[order(r$ID, r$area), ]
  r <- data.frame(id = seq_len(nrow(r)), line = seq_len(nrow(r)), resp = as.character(r$ID), area = r$area,
                  type = r$type, group = r$type, T = r$Q9, speed = unname(TYPE_SPEED[r$type]), home = home,
                  stringsAsFactors = FALSE)
  logf("choices: %d visits by %d respondents to %d of %d areas; start node %d, %.0f m from the GlattPark point",
       nrow(r), length(unique(r$resp)), length(unique(r$area)), nrow(ac), home, sqrt(min(d2)))
  list(r = r, sample = smp, home = home, homeOff = sqrt(min(d2)), ac = ac, point = p)
}

#calibrate_aoi_rum.R's captureFeatures(), at GlattPark
captureFeaturesGP <- function(r, type){
  f <- rumPath(paste0("capture_", type, ".rds"))
  if(file.exists(f)){
    got <- readRDS(f)
    if(identical(got$ids, r$id) && identical(got$stamp, readRDS(aoiFile(GP_CTX))$stamp)) return(got)
  }
  sc <- scenarioFor(GP_CTX, type)
  got <- NULL
  nsSet("findClosestAOI_cpp", function(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds,
                                       agentDurations, AOI_aois, AOI_dulns, agentTyps, AOI_areas){
    res <- ORIG$findClosestAOI_cpp(AOIList_o, AOI_v, adj_ptr, as.integer(r$home), r$speed, r$T,
                                   AOI_aois, AOI_dulns, r$type, AOI_areas)
    got <<- list(res = res[c("distances", "aoi")], AOI_aois = AOI_aois, AOI_dulns = AOI_dulns,
                 AOI_areas = AOI_areas)
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
  got$trueD <- trueAreaDistances(GP_CTX, sc$net)
  got$ids <- r$id
  got$stamp <- readRDS(aoiFile(GP_CTX))$stamp
  saveRDS(got, f)
  got
}

#Calibration.R's own features: straight distance from the GlattPark point, its 1-10
#scaling with the population's bounds, every visited area an alternative, no reach
literalTable <- function(r, ch, cap){
  ac <- ch$ac
  pt <- sf::st_sfc(sf::st_point(ch$point), crs = 2056)
  dist <- stats::setNames(as.numeric(sf::st_distance(pt, ac)), ac$AOI)
  k <- match(ac$AOI, as.character(cap$AOI_aois))
  A <- data.frame(area = ac$AOI, DULN = cap$AOI_dulns[k], a = pmin(sqrt(cap$AOI_areas[k]), 7000) / 1000,
                  distance = dist, stringsAsFactors = FALSE)
  A <- A[A$area %in% r$area, ]
  L <- do.call(rbind, lapply(seq_len(nrow(r)), function(i) data.frame(obs = i, area = A$area,
    td = A$distance / (r$speed[i] * 1000), dur = r$T[i], DULN = A$DULN, a = A$a, chosen = A$area == r$area[i])))
  dpd <- L$td / L$dur; dbapd <- L$DULN * L$a / L$dur
  dpdMax <- max(L$td) / min(L$dur); dpdMin <- min(L$td) / max(L$dur)
  dbMax <- max(L$DULN) * max(L$a) / min(L$dur); dbMin <- min(L$DULN) * min(L$a) / max(L$dur)
  L$dbSTD <- 9 * (dbapd - dbMin) / (dbMax - dbMin) + 1
  L$dSTD  <- 9 * (dpd - dpdMin) / (dpdMax - dpdMin) + 1
  L
}

#### stage fit ####

stageFitGP <- function(){
  ch  <- stageChoices()
  r   <- ch$r
  cap <- captureFeaturesGP(r, "full")
  cols <- c("dbSTD", "dSTD")
  cluster <- stats::setNames(r$resp, seq_len(nrow(r)))
  fitSet <- function(reach){ L <- longTable(r, cap, reach); choiceSet(L, r, reach) }
  cs <- fitSet(REACH)
  why <- table(factor(cs$why))
  logf("visits by outcome (reach %.2f):", REACH); print(why)
  L <- cs$L
  nFit <- length(unique(L$obs))
  smp <- rbind(ch$sample, data.frame(step = paste0("fitted: the visited area found and within reach ", REACH), n = nFit))
  utils::write.csv(smp, rumPath("sample.csv"), row.names = FALSE)

  fit <- clFit(L, cols, b = unname(ENGINE_B), cluster = cluster)
  resp <- unique(r$resp[as.integer(unique(L$obs))])
  set.seed(20261027)
  fold <- stats::setNames(sample(rep_len(seq_len(CV_FOLDS), length(resp))), resp)
  foldOf <- function(obs) fold[r$resp[obs]]
  cvOf <- function(fitFun){
    tot <- 0
    for(k in seq_len(CV_FOLDS)){
      tr <- L[foldOf(L$obs) != k, ]; te <- L[foldOf(L$obs) == k, ]
      tot <- tot + fitFun(tr, te)
    }
    tot / nFit
  }
  cvEng  <- cvOf(function(tr, te) llAt(unname(ENGINE_B), te, cols))
  cvFit  <- cvOf(function(tr, te) llAt(clFit(tr, cols, b = unname(ENGINE_B))$b, te, cols))
  cvDist <- cvOf(function(tr, te) llAt(clFit(tr, "dSTD", b = ENGINE_B[["bDist"]])$b, te, "dSTD"))
  cvNull <- cvOf(function(tr, te) llAt(0, te, "dSTD"))
  distOnly <- clFit(L, "dSTD", b = ENGINE_B[["bDist"]], cluster = cluster)

  sens <- list()
  sens$no_reach <- { c0 <- fitSet(Inf); clFit(c0$L, cols, b = unname(ENGINE_B), cluster = cluster) }
  sens$calibration_R_features <- { Ll <- literalTable(r, ch, cap); clFit(Ll, cols, b = unname(ENGINE_B), cluster = cluster) }

  row <- function(model, b, se = c(NA, NA), ll, n, cvll = NA)
    data.frame(model = model, visits = n, bAttr = b[1], bAttr_se = se[1], bDist = b[2], bDist_se = se[2],
               ll_per_visit = ll / n, cv_ll_per_visit = cvll)
  tab <- rbind(
    row("2024 GlattPark pair (scored)", unname(ENGINE_B), ll = llAt(unname(ENGINE_B), L, cols), n = nFit, cvll = cvEng),
    row("refit on this DULN", unname(fit$b), unname(fit$se), fit$ll, fit$n, cvFit),
    row("distance only (refit)", c(0, unname(distOnly$b)), c(NA, unname(distOnly$se)), distOnly$ll, distOnly$n, cvDist),
    row("uniform over the areas in reach", c(0, 0), ll = llAt(0, L, "dSTD"), n = nFit, cvll = cvNull),
    do.call(rbind, lapply(names(sens), function(s) row(paste0("sensitivity: ", s), unname(sens[[s]]$b),
                                                       unname(sens[[s]]$se), sens[[s]]$ll, sens[[s]]$n))))
  rownames(tab) <- NULL
  utils::write.csv(tab, rumPath("fits.csv"), row.names = FALSE)
  logf("the fits (GlattPark survey, %d visits by %d respondents):", nFit, length(resp))
  print(format(tab, digits = 4), row.names = FALSE)

  #observed and predicted shares of the visits, per area
  shareOf <- function(b, cl) tapply(clEval(b, L, cl)$p, L$area, sum)
  A <- sort(unique(L$area))
  sh <- data.frame(area = A, observed = as.numeric(table(factor(L$area[L$chosen], levels = A))) / nFit,
                   pair2024 = as.numeric(shareOf(unname(ENGINE_B), cols)[A]) / nFit,
                   refit = as.numeric(shareOf(unname(fit$b), cols)[A]) / nFit)
  sh$DULN <- cap$AOI_dulns[match(sh$area, as.character(cap$AOI_aois))]
  utils::write.csv(sh, rumPath("shares.csv"), row.names = FALSE)
  logf("area shares of the visits, observed vs predicted, top 12 by observed:")
  print(format(head(sh[order(-sh$observed), ], 12), digits = 3), row.names = FALSE)

  b <- c(bAttr = fit$b[["dbSTD"]], bDist = fit$b[["dSTD"]])
  coefs <- list(pooled = b, distance = c(bAttr = 0, bDist = distOnly$b[["dSTD"]]),
                types = data.frame(type = AGENT_TYPES, bAttr = b[["bAttr"]], bDist = b[["bDist"]]),
                reach = REACH, durRange = DUR_RANGE, visits = nFit, respondents = length(resp),
                stamp = readRDS(aoiFile(GP_CTX))$stamp, source = "GlattPark survey (Q9, Q18, visited grid)",
                made = format(Sys.time()))
  saveRDS(coefs, rumPath("coefs.rds"))
  logf("coefs.rds: bAttr %.3f, bDist %.3f (2024: %.3f, %.3f)", b[["bAttr"]], b[["bDist"]],
       ENGINE_B[["bAttr"]], ENGINE_B[["bDist"]])
  invisible(list(tab = tab, coefs = coefs, shares = sh))
}

#### stage time ####

stageTimeGP <- function(){
  ch  <- stageChoices()
  r   <- ch$r
  cap <- captureFeaturesGP(r, "full")
  if(max(cap$AOI_areas) < 1e4) stop("AOI_areas do not look like square metres")
  du <- tapply(cap$AOI_dulns, cap$AOI_aois, function(x) x[length(x)])
  consts <- list(dulnCenter = mean(du), dulnScale = stats::sd(du), areas = length(du), source = "GlattPark areas")
  cluster <- stats::setNames(r$resp, seq_len(nrow(r)))
  fitAt <- function(reach){
    cs <- choiceSet(timeTable(r, cap, reach), r, reach)
    L <- addX(cs$L, consts)
    L[is.finite(L$tMin), ]
  }
  L <- fitAt(REACH)
  nFit <- length(unique(L$obs))
  ENG <- c("dbSTD", "dSTD")
  fits <- list(engine_refit = clFit(L, ENG, b = unname(ENGINE_B), cluster = cluster),
               distance = clFit(L, "dSTD", b = ENGINE_B[["bDist"]], cluster = cluster))
  for(m in names(MODELS)) fits[[m]] <- clFit(L, MODELS[[m]], cluster = cluster)
  colsOf <- c(list(engine_refit = ENG, distance = "dSTD"), MODELS)
  resp <- unique(r$resp[as.integer(unique(L$obs))])
  set.seed(20261027)
  fold <- stats::setNames(sample(rep_len(seq_len(CV_FOLDS), length(resp))), resp)
  foldOf <- function(obs) fold[r$resp[obs]]
  cvOf <- function(cols, b0 = rep(0, length(cols)), fixedB = NULL){
    tot <- 0
    for(k in seq_len(CV_FOLDS)){
      trn <- L[foldOf(L$obs) != k, ]; tst <- L[foldOf(L$obs) == k, ]
      tot <- tot + llAt(if(!is.null(fixedB)) fixedB else clFit(trn, cols, b = b0)$b, tst, cols)
    }
    tot / nFit
  }
  cv <- c(pair2024 = cvOf(ENG, fixedB = unname(ENGINE_B)), uniform = cvOf("dSTD", fixedB = 0),
          engine_refit = cvOf(ENG, b0 = unname(ENGINE_B)), distance = cvOf("dSTD", b0 = ENGINE_B[["bDist"]]),
          vapply(names(MODELS), function(m) cvOf(MODELS[[m]]), 1))
  tab <- do.call(rbind, c(
    list(data.frame(model = "pair2024", term = ENG, b = unname(ENGINE_B), se = NA, z = NA,
                    ll_per_visit = llAt(unname(ENGINE_B), L, ENG) / nFit, cv_ll_per_visit = cv[["pair2024"]]),
         data.frame(model = "uniform", term = "-", b = 0, se = NA, z = NA,
                    ll_per_visit = llAt(0, L, "dSTD") / nFit, cv_ll_per_visit = cv[["uniform"]])),
    lapply(names(fits), function(m){ f <- fits[[m]]
      data.frame(model = m, term = names(f$b), b = unname(f$b), se = unname(f$se), z = unname(f$b / f$se),
                 ll_per_visit = f$ll / nFit, cv_ll_per_visit = cv[[m]]) })))
  rownames(tab) <- NULL
  utils::write.csv(tab, rumPath("fits_time.csv"), row.names = FALSE)
  logf("time-dependent fits (GlattPark survey, %d visits by %d respondents, T = Q9; per visit, higher is better):",
       nFit, length(resp))
  print(format(tab, digits = 3), row.names = FALSE)

  #the trade-off by time in the area
  bi <- fits$interaction$b; bf <- fits$fixed$b
  Ts <- c(20, 30, 45, 60, 90, 120, 180, 240)
  tau <- log(Ts / 60)
  wA <- bi[["zD"]] + bi[["zD_tau"]] * tau
  wT <- (bi[["t"]] + bi[["t_tau"]] * tau) / 10
  to <- data.frame(T_min = Ts, attr_per_SD = wA, time_per_min = wT, min_per_SD = -wA / wT,
                   size_per_lnha = bi[["lnA"]] + bi[["lnA_tau"]] * tau, fixed_min_per_SD = -bf[["zD"]] / (bf[["t"]] / 10))
  utils::write.csv(to, rumPath("tradeoff_time.csv"), row.names = FALSE)
  logf("trade-off by time in the area (interaction model; minutes of extra travel one SD of DULN is worth):")
  print(format(to, digits = 3), row.names = FALSE)
  saveRDS(list(models = lapply(fits[names(MODELS)], `[[`, "b"), consts = consts,
               se = lapply(fits[names(MODELS)], `[[`, "se"), reach = REACH, visits = nFit, respondents = length(resp),
               cv_ll_per_visit = cv, stamp = readRDS(aoiFile(GP_CTX))$stamp,
               source = "GlattPark survey (Q9 time in the area, Q18, visited grid)", made = format(Sys.time())),
          rumPath("coefs_time.rds"))
  invisible(list(tab = tab, tradeoff = to))
}

if(sys.nframe() == 0L){
  st <- strsplit(Sys.getenv("VFT_GPC_STAGES", "fit,time"), ",", fixed = TRUE)[[1]]
  if("fit" %in% st) stageFitGP()
  if("time" %in% st) stageTimeGP()
  logf("calibrate_aoi_glattpark: done, %s", RUM_DIR)
}
