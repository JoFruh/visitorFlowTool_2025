#### The area choice with time-dependent weights, fitted on the Glatt survey ####
#
# The engine's area utility (src/CPP_FUNCTIONS.cpp ~2384) divides both of its terms by
# the agent's duration T, so T only scales the utility: the trade-off between
# attractivity and proximity is the same for every T. The user's intent (2026-10-23):
# with more time, attractivity weighs more and proximity less. This fits utilities
# that can say so (rumTimeX() in data-raw/engine_corrections.R: fixed scales,
# tau = ln(T / 60)) on the FilBleu routes of the Glatt region, one choice per route
# as data-raw/calibrate_aoi_rum.R (whose routes, homes, engine distances and logit it
# reuses), with T the respondent's STATED time in a recreation area:
#   A4_duration  "Wie lange halten Sie sich durchschnittlich in einem Naherholungs-
#                gebiet auf? (Ohne den Weg dorthin und zurück)", Data_ol.xlsx of the
#                Fil Bleu Glatt survey, 5 classes -> 20, 45, 90, 150, 210 minutes.
#                The time INSIDE the area, which is what the engine's duration is
#                under the default outing (VFT_VAL_OUTING unset), and not derived from
#                the route, so a far choice cannot lengthen it.
#   travel time  the real path length from home to the area at the type's engine speed
#   choice set   the areas within reach as the corrected engine has them, at the
#                stated T (VFT_RUM_REACH, default CORR's)
# Models (respondent-blocked 5-fold CV log-likelihood on the same routes and choice
# sets; SEs clustered by respondent):
#   engine        the engine's pair on its own features at the stated T (scored only)
#   engine_refit  the engine's features, re-fitted
#   fixed         zD + lnA + t                      (no time at all)
#   share         zD + lnA + tShare                 (the way there as a share of T)
#   interaction   zD + lnA + t + their products with tau
# The intent holds if the interaction model puts zD_tau > 0 (attractivity counts more
# with more time) and t_tau > 0 (the time penalty, negative, shrinks with more time).
# Written to <VFT_VAL_OUT>/rum_time: coefs_time.rds (VFT_VAL_RUM_TIME), fits.csv,
# tradeoff.csv, by_class.csv, sample.csv, durations.csv.
#
#   VFT_VAL_OUT=<folder> VFT_VAL_REACH=0.33 Rscript data-raw/calibrate_aoi_rum_time.R
#
# VFT_RUMT_STAGES  fit and/or durations (default fit): durations writes
#                  durations_stated.rds, the stated times as VFT_VAL_DURSRC draws them
#
# VFT_RUM_REACH    the reach of the choice set (default CORR's, VFT_VAL_REACH)
# VFT_RUMT_SURVEY  the survey table with P1_respondent and A4_duration

.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "calibrate_aoi_rum.R"))

#rumPath() of calibrate_aoi_rum.R writes here from now on
RUM_DIR <- outPath("rum_time")
dir.create(RUM_DIR, showWarnings = FALSE)
SURVEY_TAB <- Sys.getenv("VFT_RUMT_SURVEY", file.path("C:/Users/frueh/Documents/Local Data",
  "Shared visitorFlow Project Data/Social Data/Marius_Glatt_Data/Data/Data_ol.xlsx"))
A4_MIN <- c(20, 45, 90, 150, 210)
A4_LABEL <- c("< 30 min", "30-60 min", "1-2 h", "2-3 h", "3 h +")
MODELS <- list(fixed = c("zD", "lnA", "t"), share = c("zD", "lnA", "tShare"),
               interaction = c("zD", "lnA", "t", "zD_tau", "lnA_tau", "t_tau"))

#### the routes, with the stated time ####

timedRoutes <- function(){
  rt <- stageRoutes()
  r0 <- rt$routes
  st <- readxl::read_excel(SURVEY_TAB)[, c("P1_respondent", "A4_duration")]
  r0$A4 <- as.integer(st$A4_duration[match(sub("^f", "", r0$resp), as.character(st$P1_respondent))])
  r0$A4[!r0$A4 %in% seq_along(A4_MIN)] <- NA
  r0$Tstated <- A4_MIN[r0$A4]
  keep <- !is.na(r0$type) & !is.na(r0$area) & r0$len_m >= MIN_ROUTE_M & !is.na(r0$home) & r0$snap_m <= SNAP_MAX
  noCar <- !r0$access %in% c("car", "public transport")
  smp <- data.frame(step = c("routes of the five activities", "with a main area, >= 100 m, home within 500 m",
                             "not reached by car or public transport", "with a stated time in an area (A4)"),
                    routes = c(sum(!is.na(r0$type)), sum(keep), sum(keep & noCar), sum(keep & noCar & !is.na(r0$A4))))
  r <- r0[keep & noCar & !is.na(r0$A4), ]
  rownames(r) <- NULL
  #the durations the features read: the stated time, not the route's
  r$T <- r$Tstated
  list(r = r, sample = smp)
}

#### the long table: the engine's features and rumTimeX()'s ####

timeTable <- function(r, cap, reach){
  L <- longTable(r, cap, reach)
  k <- lastMatch(L$area, cap$AOI_aois)
  L <- L[!is.na(k), ]; k <- k[!is.na(k)]
  L$duln <- cap$AOI_dulns[k]; L$areaM2 <- cap$AOI_areas[k]
  L$tMin <- L$tReach * 60; L$Tmin <- r$T[L$obs]
  L
}
addX <- function(L, consts){
  X <- rumTimeX(L$duln, L$areaM2, L$tMin, L$Tmin, consts, unique(unlist(MODELS)))
  cbind(L, X)
}

#### fit ####

stageFitTime <- function(){
  tr <- timedRoutes()
  r <- tr$r
  cap <- captureFeatures(r, "full")
  if(max(cap$AOI_areas) < 1e4) stop("AOI_areas do not look like square metres")
  #the fixed scale of DULN: Glatt's areas
  du <- tapply(cap$AOI_dulns, cap$AOI_aois, function(x) x[length(x)])
  consts <- list(dulnCenter = mean(du), dulnScale = stats::sd(du), areas = length(du), source = "Glatt areas")
  cluster <- stats::setNames(r$resp, seq_len(nrow(r)))

  fitAt <- function(reach){
    L0 <- timeTable(r, cap, reach)
    cs <- choiceSet(L0, r, reach)
    L <- addX(cs$L, consts)
    L <- L[is.finite(L$tMin), ]
    list(L = L, why = cs$why)
  }
  main <- fitAt(REACH)
  L <- main$L
  why <- table(factor(main$why))
  logf("routes by outcome (reach %.2f, stated time):", REACH); print(why)
  byWhyClass <- as.data.frame.matrix(table(A4_LABEL[r$A4], main$why))
  utils::write.csv(cbind(stated = rownames(byWhyClass), byWhyClass), rumPath("sample_by_class.csv"), row.names = FALSE)
  smp <- rbind(tr$sample, data.frame(step = paste("fitted (main area within reach", REACH, ")"),
                                     routes = length(unique(L$obs))))
  utils::write.csv(smp, rumPath("sample.csv"), row.names = FALSE)
  print(smp, row.names = FALSE)

  #descriptive: what the respondents chose, by stated time
  ch <- L[L$chosen, ]
  avail <- stats::aggregate(cbind(tMin, zD) ~ obs, L, mean)
  bc <- do.call(rbind, lapply(sort(unique(r$A4)), function(a){
    o <- which(r$A4 == a); x <- ch[ch$obs %in% o, ]; av <- avail[avail$obs %in% o, ]
    data.frame(stated = A4_LABEL[a], T_min = A4_MIN[a], routes = nrow(x),
               chosen_tMin_median = stats::median(x$tMin), chosen_tMin_mean = mean(x$tMin),
               chosen_zD_mean = mean(x$zD), chosen_lnA_mean = mean(x$lnA),
               set_size = mean(table(L$obs[L$obs %in% o])),
               set_tMin_mean = mean(av$tMin), set_zD_mean = mean(av$zD))
  }))
  utils::write.csv(bc, rumPath("by_class.csv"), row.names = FALSE)
  logf("chosen areas by stated time (fitted routes):"); print(format(bc, digits = 3), row.names = FALSE)

  #the fits
  fits <- list()
  fits$engine_refit <- clFit(L, c("dbSTD", "dSTD"), b = unname(ENGINE_B), cluster = cluster)
  for(m in names(MODELS)) fits[[m]] <- clFit(L, MODELS[[m]], cluster = cluster)
  #respondent-blocked folds
  resp <- unique(r$resp[as.integer(unique(L$obs))])
  set.seed(20261023)
  fold <- stats::setNames(sample(rep_len(seq_len(CV_FOLDS), length(resp))), resp)
  foldOf <- function(obs) fold[r$resp[obs]]
  cvOf <- function(cols, b0 = NULL, fixedB = NULL){
    tot <- 0
    for(k in seq_len(CV_FOLDS)){
      trn <- L[foldOf(L$obs) != k, ]; tst <- L[foldOf(L$obs) == k, ]
      b <- if(!is.null(fixedB)) fixedB else clFit(trn, cols, b = if(is.null(b0)) rep(0, length(cols)) else b0)$b
      tot <- tot + llAt(b, tst, cols)
    }
    tot
  }
  nFit <- length(unique(L$obs))
  cv <- c(engine = cvOf(c("dbSTD", "dSTD"), fixedB = unname(ENGINE_B)),
          engine_refit = cvOf(c("dbSTD", "dSTD"), b0 = unname(ENGINE_B)),
          vapply(MODELS, cvOf, 1))
  engLL <- llAt(unname(ENGINE_B), L, c("dbSTD", "dSTD"))
  tab <- do.call(rbind, c(list(data.frame(model = "engine (GlattPark pair)", term = c("dbSTD", "dSTD"),
                                          b = unname(ENGINE_B), se = NA, z = NA, ll = engLL, cv_ll = cv[["engine"]])),
    lapply(names(fits), function(m){ f <- fits[[m]]
      data.frame(model = m, term = names(f$b), b = unname(f$b), se = unname(f$se), z = unname(f$b / f$se),
                 ll = f$ll, cv_ll = cv[[m]]) })))
  tab$ll_per_route <- tab$ll / nFit; tab$cv_ll_per_route <- tab$cv_ll / nFit
  utils::write.csv(tab, rumPath("fits.csv"), row.names = FALSE)
  logf("the fits (%d routes; CV log-likelihood per route, higher is better):", nFit)
  print(format(tab, digits = 4), row.names = FALSE)

  #the trade-off by time: minutes of extra travel one SD of DULN is worth, and the
  #share of the far, attractive choice in a two-area example
  bi <- fits$interaction$b; bs <- fits$share$b; bf <- fits$fixed$b
  Ts <- c(20, 30, 45, 60, 90, 120, 150, 210, 240)
  tau <- log(Ts / 60)
  wA <- bi[["zD"]] + bi[["zD_tau"]] * tau
  wT <- (bi[["t"]] + bi[["t_tau"]] * tau) / 10
  to <- data.frame(T_min = Ts,
                   interaction_attr_per_SD = wA, interaction_time_per_min = wT,
                   interaction_min_per_SD = -wA / wT,
                   interaction_size_per_lnha = bi[["lnA"]] + bi[["lnA_tau"]] * tau,
                   share_min_per_SD = -bs[["zD"]] / (bs[["tShare"]] / Ts),
                   fixed_min_per_SD = -bf[["zD"]] / (bf[["t"]] / 10))
  utils::write.csv(to, rumPath("tradeoff.csv"), row.names = FALSE)
  logf("the trade-off by time in the area (minutes of extra travel one SD of DULN is worth):")
  print(format(to, digits = 3), row.names = FALSE)

  #sensitivity: no reach (every area a choice)
  noReach <- fitAt(Inf)
  sens <- clFit(noReach$L, MODELS$interaction, cluster = cluster)
  logf("sensitivity, no reach (%d routes): %s", length(unique(noReach$L$obs)),
       paste(sprintf("%s %.3f (%.3f)", names(sens$b), sens$b, sens$se), collapse = ", "))
  utils::write.csv(data.frame(term = names(sens$b), b = sens$b, se = sens$se), rumPath("fit_noReach.csv"), row.names = FALSE)

  #the stated times against the durations the engine draws (what a run's agents get)
  eng <- local({
    set.seed(1)
    d <- ORIG$vftAgentTimeAndSpeed(data.frame(id = seq_len(7 * 20000), agentTyp = rep(AGENT_TYPES, each = 20000)))
    split(d$timeExt, d$agentTyp)
  })
  dt <- do.call(rbind, lapply(AGENT_TYPES, function(k){
    s <- r$T[r$type == k]; e <- eng[[k]]
    data.frame(type = k, routes = length(s), stated_median = if(length(s)) stats::median(s) else NA,
               stated_mean = if(length(s)) mean(s) else NA, engine_median = stats::median(e), engine_mean = mean(e))
  }))
  utils::write.csv(dt, rumPath("durations.csv"), row.names = FALSE)
  logf("stated time in an area vs the engine's durations (minutes):"); print(format(dt, digits = 3), row.names = FALSE)

  saveRDS(list(models = lapply(fits[names(MODELS)], `[[`, "b"), consts = consts,
               se = lapply(fits[names(MODELS)], `[[`, "se"), reach = REACH, routes = nFit,
               cv_ll_per_route = cv / nFit, survey = SURVEY_TAB, A4_minutes = A4_MIN, made = format(Sys.time())),
          rumPath("coefs_time.rds"))
  invisible(list(tab = tab, tradeoff = to, byClass = bc))
}

#### stage durations: the stated times as the runs' durations ####

#VFT_VAL_DURSRC's file: per agent type, the stated class of every typed route (all
#accesses: car-park starters are agents too), a duration drawn uniformly within it
A4_BOUNDS <- rbind(c(10, 30), c(30, 60), c(60, 120), c(120, 180), c(180, 240))
DUR_DRAWS <- 5000
stageDurationsStated <- function(){
  rt <- stageRoutes()
  r0 <- rt$routes
  st <- readxl::read_excel(SURVEY_TAB)[, c("P1_respondent", "A4_duration")]
  a4 <- as.integer(st$A4_duration[match(sub("^f", "", r0$resp), as.character(st$P1_respondent))])
  ok <- !is.na(r0$type) & a4 %in% seq_len(nrow(A4_BOUNDS))
  set.seed(20261023)
  durs <- lapply(stats::setNames(AGENT_TYPES, AGENT_TYPES), function(k){
    cl <- a4[ok & r0$type == k]
    if(!length(cl)) stop("no stated time for type ", k)
    pick <- cl[sample.int(length(cl), DUR_DRAWS, replace = TRUE)]
    stats::runif(DUR_DRAWS, A4_BOUNDS[pick, 1], A4_BOUNDS[pick, 2])
  })
  saveRDS(list(durs = durs, bounds = A4_BOUNDS, survey = SURVEY_TAB,
               source = "Glatt survey A4_duration (time in a recreation area), per typed route, uniform within the class"),
          rumPath("durations_stated.rds"))
  tab <- do.call(rbind, lapply(AGENT_TYPES, function(k) data.frame(type = k, routes = sum(ok & r0$type == k),
    median = stats::median(durs[[k]]), mean = mean(durs[[k]]), p10 = stats::quantile(durs[[k]], .1)[[1]],
    p90 = stats::quantile(durs[[k]], .9)[[1]])))
  utils::write.csv(tab, rumPath("durations_stated.csv"), row.names = FALSE)
  logf("stated durations per type (minutes):"); print(format(tab, digits = 3), row.names = FALSE)
}

if(sys.nframe() == 0L){
  st <- strsplit(Sys.getenv("VFT_RUMT_STAGES", "fit"), ",", fixed = TRUE)[[1]]
  if("fit" %in% st) stageFitTime()
  if("durations" %in% st) stageDurationsStated()
  logf("calibrate_aoi_rum_time: done, %s", RUM_DIR)
}
