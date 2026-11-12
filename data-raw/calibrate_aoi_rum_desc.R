#### The area choice with area descriptors, fitted on the Glatt survey ####
#
# RUM-time (data-raw/calibrate_aoi_rum_time.R) scores an area on its aggregated DULN, its
# size and the time to it. The DULN's element weights are fitted to where people WALK;
# which area they GO to is another decision (the lakes: Greifensee draws 8.8 % of the
# fitted routes and gets 3.5 %, whatever the DULN). This adds what an area IS to the same
# likelihood, on the same routes, choice sets and folds as the stored RUM-time fit:
#   lakeShare    share of the area in the band of a lake >= 10 ha
#   protShare    share in or near protected nature
#   viewShare    share near a viewpoint or summit
#   forestShare  share of forest
#   forestAttr   forestShare x the forest's mean stand-type score / 10 (Edwards et al.
#                2012, Central Europe: species type x phase x management, 1-10)
#   forestQ      forestShare x (mean score - 8): what the stand types add beyond the share
# (data-raw/area_descriptors.R). Only the coefficients are fitted - they are the model's,
# as zD's is; no descriptor is built from the survey.
#
# Models, all on the interaction model's six terms (base):
#   lake      + lakeShare
#   forestS   + forestShare                 forestA   + forestAttr (score-weighted instead)
#   forestSQ  + forestShare + forestQ       (does the stand-type score add to the share?)
#   allS      + lakeShare + protShare + viewShare + forestShare
#   allA      + lakeShare + protShare + viewShare + forestAttr     -> "interactionD", the
#             model the simulation runs (type fullRumTimeD); set before the fits were seen
#   allSQ     + lakeShare + protShare + viewShare + forestShare + forestQ
#   noDULN    allA without zD and zD_tau    (what the DULN still adds next to descriptors)
#   allAc     allA with the descriptors' coefficients held >= 0, as E2c holds its nature
#             elements: the most negative descriptor is dropped and the model re-fitted
#             until none is negative (redone inside every CV fold) -> "interactionDc"
#             (type fullRumTimeDc)
# Respondent-blocked 5-fold CV log-likelihood per route, the stored fit's seed and folds
# (the base model must reproduce the stored coefficients and CV: the gate). SEs clustered
# by respondent. Also: observed and predicted shares of the lake areas, in sample and out
# of fold.
# Written to <VFT_VAL_OUT>/rum_desc: fits.csv, lake_shares.csv, coefs_time_desc.rds
# (VFT_VAL_RUM_TIME for fullRumTimeD: models interaction + interactionD, consts, areaX
# for every context of VFT_RUMD_CTX).
#
#   VFT_VAL_OUT=<folder> VFT_VAL_REACH=0.33 Rscript data-raw/calibrate_aoi_rum_desc.R
#
# VFT_RUMD_DESC  the descriptors' folder (default Local Data/method_2611/area_desc)
# VFT_RUMD_CTX   contexts the coefficient file carries descriptors for (default
#                glatt,glattPark,wigger)

.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "calibrate_aoi_rum_time.R"))

RUMD_DESC <- Sys.getenv("VFT_RUMD_DESC", "C:/Users/frueh/Documents/Local Data/method_2611/area_desc")
RUMD_CTX  <- strsplit(Sys.getenv("VFT_RUMD_CTX", "glatt,glattPark,wigger"), ",", fixed = TRUE)[[1]]
RUMD_DIR  <- outPath("rum_desc")
dir.create(RUMD_DIR, showWarnings = FALSE)
descPath <- function(f) file.path(RUMD_DIR, f)
SCORE_REF <- 8
DESC_COLS <- c("lakeShare", "protShare", "viewShare", "forestShare", "forestAttr", "forestQ")
BASE <- MODELS$interaction
DESC_MODELS <- list(base = BASE, lake = c(BASE, "lakeShare"),
                    forestS = c(BASE, "forestShare"), forestA = c(BASE, "forestAttr"),
                    forestSQ = c(BASE, "forestShare", "forestQ"),
                    allS = c(BASE, "lakeShare", "protShare", "viewShare", "forestShare"),
                    allA = c(BASE, "lakeShare", "protShare", "viewShare", "forestAttr"),
                    allSQ = c(BASE, "lakeShare", "protShare", "viewShare", "forestShare", "forestQ"),
                    noDULN = c(setdiff(BASE, c("zD", "zD_tau")), "lakeShare", "protShare", "viewShare", "forestAttr"))
SIM_MODEL <- "allA"
#allA with no negative descriptor coefficient: drop the most negative, re-fit, repeat
signedCols <- function(L, cols = DESC_MODELS$allA){
  repeat{
    b <- clFit(L, cols)$b
    neg <- b[names(b) %in% DESC_COLS & b < 0]
    if(!length(neg)) return(cols)
    cols <- setdiff(cols, names(neg)[which.min(neg)])
  }
}

#a context's descriptor matrix (rows = AOI ids), checked against the folder's areas
descMatrix <- function(ctx){
  x <- readRDS(file.path(RUMD_DESC, paste0("desc_", ctx, ".rds")))
  a <- readRDS(aoiFile(ctx))
  if(!identical(x$aoiStamp, a$stamp) || !setequal(x$desc$AOI, as.character(a$polys$AOI)))
    stop(ctx, ": the descriptors are not of this folder's areas (", x$aoiStamp, " vs ", a$stamp, ")")
  d <- x$desc
  sc <- ifelse(is.na(d$forestScore), SCORE_REF, d$forestScore)
  M <- cbind(lakeShare = d$lakeShare, protShare = d$protShare, viewShare = d$viewShare,
             forestShare = d$forestShare, forestAttr = d$forestAttr, forestQ = d$forestShare * (sc - SCORE_REF))
  rownames(M) <- d$AOI
  if(anyNA(M)) stop(ctx, ": a descriptor is NA")
  M
}

stageFitDesc <- function(){
  tr <- timedRoutes()
  r <- tr$r
  cap <- captureFeatures(r, "full")
  du <- tapply(cap$AOI_dulns, cap$AOI_aois, function(x) x[length(x)])
  consts <- list(dulnCenter = mean(du), dulnScale = stats::sd(du), areas = length(du), source = "Glatt areas")
  stored <- readRDS(rumPath("coefs_time.rds"))
  if(!isTRUE(all.equal(unlist(consts[c("dulnCenter", "dulnScale")]), unlist(stored$consts[c("dulnCenter", "dulnScale")]))))
    stop("the DULN scale is not the stored fit's")
  cluster <- stats::setNames(r$resp, seq_len(nrow(r)))
  L0 <- timeTable(r, cap, REACH)
  cs <- choiceSet(L0, r, REACH)
  L <- addX(cs$L, consts)
  L <- L[is.finite(L$tMin), ]
  M <- descMatrix("glatt")
  k <- match(L$area, rownames(M))
  if(anyNA(k)) stop("an area of the choice sets has no descriptors")
  L <- cbind(L, M[k, , drop = FALSE])
  nFit <- length(unique(L$obs))

  DESC_MODELS$allAc <- signedCols(L)
  logf("allAc keeps: %s", paste(setdiff(DESC_MODELS$allAc, BASE), collapse = ", "))
  fits <- lapply(DESC_MODELS, function(cols) clFit(L, cols, cluster = cluster))
  #the stored fit's folds
  resp <- unique(r$resp[as.integer(unique(L$obs))])
  set.seed(20261023)
  fold <- stats::setNames(sample(rep_len(seq_len(CV_FOLDS), length(resp))), resp)
  foldOf <- function(obs) fold[r$resp[obs]]
  pOut <- list()
  cvOf <- function(m){
    cols <- DESC_MODELS[[m]]; tot <- 0; p <- numeric(nrow(L))
    for(kf in seq_len(CV_FOLDS)){
      tst <- foldOf(L$obs) == kf
      if(m == "allAc") cols <- signedCols(L[!tst, ])
      b <- clFit(L[!tst, ], cols)$b
      e <- clEval(b, L[tst, ], cols)
      tot <- tot + e$ll; p[tst] <- e$p
    }
    pOut[[m]] <<- p
    tot
  }
  cv <- vapply(names(DESC_MODELS), cvOf, 1)
  #the gate: the base model is the stored interaction model
  gate <- c(b = max(abs(fits$base$b - stored$models$interaction[names(fits$base$b)])),
            cv = abs(cv[["base"]] / nFit - stored$cv_ll_per_route[["interaction"]]), routes = abs(nFit - stored$routes))
  logf("gate: base vs the stored interaction model - max |b| diff %.2e, CV/route diff %.2e, routes diff %d",
       gate[["b"]], gate[["cv"]], as.integer(gate[["routes"]]))
  if(gate[["b"]] > 1e-6 || gate[["cv"]] > 1e-8 || gate[["routes"]] != 0) stop("the base model does not reproduce the stored fit")

  tab <- do.call(rbind, lapply(names(fits), function(m){ f <- fits[[m]]
    data.frame(model = m, term = names(f$b), b = unname(f$b), se = unname(f$se), z = unname(f$b / f$se),
               ll_per_route = f$ll / nFit, cv_ll_per_route = cv[[m]] / nFit,
               cv_gain_vs_base = (cv[[m]] - cv[["base"]]) / nFit) }))
  utils::write.csv(tab, descPath("fits.csv"), row.names = FALSE)
  sm <- data.frame(model = names(fits), terms = lengths(DESC_MODELS), ll_per_route = vapply(fits, function(f) f$ll, 1) / nFit,
                   cv_ll_per_route = cv / nFit, cv_gain_vs_base = (cv - cv[["base"]]) / nFit)
  #the CV gain's uncertainty: per respondent, paired with the base (out-of-fold log-likelihoods)
  llObs <- function(p) tapply(log(p[L$chosen]), r$resp[L$obs[L$chosen]], sum)
  l0 <- llObs(pOut$base)
  sm$gain_se <- vapply(names(fits), function(m){ d <- llObs(pOut[[m]]) - l0; stats::sd(d) * sqrt(length(d)) / nFit }, 1)
  utils::write.csv(sm, descPath("fits_summary.csv"), row.names = FALSE)
  options(width = 200)
  logf("the fits (%d routes, %d respondents; CV log-likelihood per route, higher is better):", nFit, length(resp))
  print(format(sm, digits = 4), row.names = FALSE)
  logf("coefficients:")
  print(format(tab[tab$model %in% c("lake", "forestSQ", "allS", "allA", "allSQ", "noDULN", "allAc"), c("model", "term", "b", "se", "z")], digits = 3), row.names = FALSE)

  #the lake areas: observed share of the fitted routes' choices, predicted in sample and out of fold
  lakes <- rownames(M)[M[, "lakeShare"] > 0.05]
  ls <- do.call(rbind, lapply(lakes, function(a) do.call(rbind, lapply(names(fits), function(m){
    pin <- clEval(fits[[m]]$b, L, DESC_MODELS[[m]])$p
    data.frame(area = a, lakeShare = M[a, "lakeShare"], model = m, observed = sum(L$chosen & L$area == a) / nFit,
               predicted_in_sample = sum(pin[L$area == a]) / nFit, predicted_out_of_fold = sum(pOut[[m]][L$area == a]) / nFit) }))))
  utils::write.csv(ls, descPath("lake_shares.csv"), row.names = FALSE)
  logf("the lake areas' share of the fitted routes' choices:")
  print(format(ls, digits = 3), row.names = FALSE)

  #the simulation's file: the stored models, interactionD, and every context's descriptors
  bD <- fits[[SIM_MODEL]]$b
  models <- c(stored$models, list(interactionD = bD, interactionDc = fits$allAc$b))
  areaX <- lapply(stats::setNames(RUMD_CTX, RUMD_CTX), function(ctx) descMatrix(ctx)[, setdiff(names(bD), BASE), drop = FALSE])
  saveRDS(list(models = models, consts = stored$consts, areaX = areaX, simModel = SIM_MODEL, reach = REACH, routes = nFit,
               cv_ll_per_route = cv / nFit, descriptors = RUMD_DESC, made = format(Sys.time())),
          descPath("coefs_time_desc.rds"))
  invisible(list(summary = sm, tab = tab, lakes = ls))
}

if(sys.nframe() == 0L){
  stageFitDesc()
  logf("calibrate_aoi_rum_desc: done, %s", RUMD_DIR)
}
