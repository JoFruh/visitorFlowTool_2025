#### The area choice on other area-DULN definitions: fit-only test on the Glatt routes ####
#
# calibrate_aoi_rum_time.R's fit (same 905-odd FilBleu routes, stated time, choice
# sets, respondent-blocked 5-fold CV) with zD read from data-raw/test_area_duln_aggs.R's
# per-area values instead of the engine's single Nature-walker value:
#   agg      stored (what the engine reads), cur, med, top10, top5, p95, max, excess
#            (log1p of the high-value hectares)
#   byType   FALSE: the walkNat value for every route; TRUE: the route's own type's band
# zD stays standardised over Glatt's areas (all types pooled when byType). Models: fixed
# (zD + lnA + t), interaction (+ the tau terms) and noDULN (lnA + t), the floor.
# Reads the folder's rum_time/capture_full.rds and routes.rds (copied, not rebuilt) and
# writes only <VFT_VAL_OUT>/rum_area_test/ and VFT_AD_FITOUT.
#
#   VFT_VAL_OUT=<folder with rum_time/> VFT_AD_AGGS=<aggs rds> VFT_AD_FITOUT=<csv>
#   VFT_VAL_REACH=0.33 Rscript data-raw/test_area_duln_fit.R

AGGS <- Sys.getenv("VFT_AD_AGGS"); FITOUT <- Sys.getenv("VFT_AD_FITOUT")
if(!nzchar(AGGS) || !nzchar(FITOUT)) stop("set VFT_AD_AGGS and VFT_AD_FITOUT")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "calibrate_aoi_rum_time.R"))

#the fit's own caches, copied so nothing in rum_time/ is touched
src <- RUM_DIR
RUM_DIR <- outPath("rum_area_test")
dir.create(RUM_DIR, showWarnings = FALSE)
for(f in c("routes.rds", "capture_full.rds"))
  if(!file.copy(file.path(src, f), rumPath(f), overwrite = TRUE)) stop("no ", f, " in ", src)

ad <- readRDS(AGGS); ag <- ad$aggs
AGG <- c("stored", "cur", "med", "top10", "top5", "p95", "max", "excess")
for(a in AGG) if(a == "excess") ag[[a]] <- log1p(ag[[a]])
wn <- ag[ag$type == "walkNat", ]

tr <- timedRoutes(); r <- tr$r
cap <- captureFeatures(r, "full")
#the capture is this folder's areas: its area DULN must be the variant's stored walkNat value
du <- tapply(cap$AOI_dulns, cap$AOI_aois, function(x) x[length(x)])
dev <- max(abs(du[wn$AOI] - wn$stored), na.rm = TRUE)
logf("%s: capture's area DULN vs the stored walkNat value, max |d| %.2g over %d areas", ad$variant, dev, length(du))
if(dev > 1e-6) stop("the capture is not this variant's areas")
stopifnot(all(unique(r$type) %in% ag$type))

cluster <- stats::setNames(r$resp, seq_len(nrow(r)))
cs <- choiceSet(timeTable(r, cap, REACH), r, REACH)
Lb <- cs$L
resp <- unique(r$resp[as.integer(unique(Lb$obs))])
set.seed(20261023)
fold <- stats::setNames(sample(rep_len(seq_len(CV_FOLDS), length(resp))), resp)
cvOf <- function(L, cols){
  tot <- 0
  for(k in seq_len(CV_FOLDS)){
    inK <- fold[r$resp[L$obs]] == k
    tot <- tot + llAt(clFit(L[!inK, ], cols)$b, L[inK, ], cols)
  }
  tot
}
MOD <- c(MODELS[c("fixed", "interaction")], list(noDULN = c("lnA", "t")))

res <- list()
for(byType in c(FALSE, TRUE)) for(a in AGG){
  key <- paste(Lb$area, if(byType) r$type[Lb$obs] else "walkNat")
  v <- ag[[a]][match(key, paste(ag$AOI, ag$type))]
  pool <- if(byType) ag[[a]] else wn[[a]]
  consts <- list(dulnCenter = mean(pool, na.rm = TRUE), dulnScale = stats::sd(pool, na.rm = TRUE))
  L <- Lb; L$duln <- v
  L <- addX(L, consts)
  L <- L[is.finite(L$tMin), ]
  if(anyNA(L$zD)) stop(a, if(byType) " by type", ": areas without a value in the choice sets")
  nR <- length(unique(L$obs))
  for(m in names(MOD)){
    if(m == "noDULN" && (byType || a != "stored")) next
    f <- clFit(L, MOD[[m]], cluster = cluster)
    g <- function(t) if(t %in% names(f$b)) f$b[[t]] else NA_real_
    s <- function(t) if(t %in% names(f$se)) f$se[[t]] else NA_real_
    res[[length(res) + 1]] <- data.frame(variant = ad$variant, agg = a, byType = byType, model = m, routes = nR,
      cv_ll_per_route = cvOf(L, MOD[[m]]) / nR, ll_per_route = f$ll / nR,
      zD = g("zD"), zD_se = s("zD"), lnA = g("lnA"), t = g("t"), zD_tau = g("zD_tau"))
  }
  logf("%s %s%s: done", ad$variant, a, if(byType) " by type" else "")
}
tab <- do.call(rbind, res)
utils::write.csv(tab, FITOUT, row.names = FALSE)
print(format(tab[tab$model != "interaction", c("agg", "byType", "model", "cv_ll_per_route", "zD", "zD_se", "lnA", "t")], digits = 3), row.names = FALSE)
logf("test_area_duln_fit: done, %s", FITOUT)
