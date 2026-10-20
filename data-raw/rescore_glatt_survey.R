#### Glatt re-scored on the corrected survey ####
#
# The Glatt survey every validation so far was scored on (five Copy_activity_*.shp
# exports) holds no jogging: its jogging export is 79 of the 99 dog-walking routes
# again, so those were counted twice and the 260 jogging routes were missing. This
# rebuilds the Glatt survey from the full export (VFT_VAL_FILBLEU = "full", see
# validate_usage_surveys.R) in its own folder - the validation's files stay as they
# are - and scores, on the old survey and on the corrected one:
#   - the stored runs of the last full validation (5 types, 40 reps);
#   - the tailored-DULN runs of data-raw/test_tailored_duln.R at Glatt (20 reps),
#     paired against the stored runs as there.
#
#   Rscript data-raw/rescore_glatt_survey.R
#
# VFT_VAL_OUT   the validation (default _final_timefix)
# VFT_TD_OUT    the tailored-DULN output (default <VFT_VAL_OUT>/duln_tailored)
# VFT_RS_OUT    the corrected survey's folder (default <VFT_VAL_OUT>/glatt_survey_full)

Sys.setenv(VFT_VAL_WORKER = "1")
LOCAL <- "C:/Users/frueh/Documents/Local Data"
if(!nzchar(Sys.getenv("VFT_VAL_OUT")))
  Sys.setenv(VFT_VAL_OUT = file.path(LOCAL, "pathUsageValidation_2026_final_timefix"))
Sys.setenv(VFT_VAL_ENGINE = "corrected", VFT_VAL_STRAIGHT = "1", VFT_VAL_ALPHA = "2", VFT_VAL_REACH = "0.33",
           VFT_VAL_FILBLEU = "full")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "validate_usage_surveys.R"))

REF_OUT <- OUT
TD  <- Sys.getenv("VFT_TD_OUT", file.path(REF_OUT, "duln_tailored"))
RS  <- Sys.getenv("VFT_RS_OUT", file.path(REF_OUT, "glatt_survey_full"))
CTX_G <- "glatt"
withOut <- function(dir, expr){ old <- OUT; OUT <<- dir; on.exit(OUT <<- old); force(expr) }

#### 1. the exports against the full file ####

full <- readSurveyLines("filBleu")   #FILBLEU_SOURCE = "full"
chk <- do.call(rbind, lapply(FILBLEU_LAYERS, function(ly){
  x <- sf::st_zm(sf::st_read(file.path(PATHS$filBleuDir, paste0("Copy_activity_", ly, ".shp")), quiet = TRUE))
  f <- full[full$layer == ly, ]
  m <- match(x$geoansweri, f$id)
  sameGeom <- vapply(seq_len(nrow(x)), function(i) if(is.na(m[i])) FALSE else
    isTRUE(all.equal(unname(sf::st_coordinates(sf::st_transform(x[i, ], 2056))[, 1:2]),
                     unname(sf::st_coordinates(f[m[i], ])[, 1:2]))), TRUE)
  data.frame(layer = ly, exportLines = nrow(x), fullLines = nrow(f), exportInFull = sum(!is.na(m)),
             sameGeometry = sum(sameGeom))
}))
logf("the five exports against the full file (same activity):")
print(chk, row.names = FALSE)
logf("the full file: %d routes, %d respondents; per activity:", nrow(full), length(unique(full$resp)))
print(table(full$layer))

#### 2. the corrected survey, in its own folder ####

withOut(RS, {
  for(f in c(file.path("aoi", paste0(CTX_G, ".rds")), file.path("network", paste0(CTX_G, c("_edges.rds", "_meta.rds")))))
    if(!file.exists(file.path(RS, f))){
      dir.create(dirname(file.path(RS, f)), recursive = TRUE, showWarnings = FALSE)
      file.copy(file.path(REF_OUT, f), file.path(RS, f))
    }
  if(!file.exists(surveyFile(CTX_G))) stageSurvey(CTX_G)
})
svOld <- readRDS(surveyFile(CTX_G))
svNew <- withOut(RS, readRDS(surveyFile(CTX_G)))
logf("survey glatt: old %d lines / %d respondents, total %.0f; corrected %d lines / %d respondents, total %.0f; r(old, new) over the cells %.3f",
     nrow(svOld$L), length(unique(svOld$resp)), sum(svOld$surveyBase),
     nrow(svNew$L), length(unique(svNew$resp)), sum(svNew$surveyBase),
     stats::cor(svOld$surveyBase, svNew$surveyBase))

#### 3. the scores ####

scOld <- scorerFor(CTX_G)
scNew <- withOut(RS, scorerFor(CTX_G))
both <- function(rr) c(stats::setNames(scOld(rr), paste0("old_", c("r100", "r500"))),
                       stats::setNames(scNew(rr), paste0("new_", c("r100", "r500"))))
types5 <- c("full", "nearby", "nearbyRandom", "random", "fullRandom")
stored <- lapply(stats::setNames(types5, types5), function(t){
  aoi <- readRDS(aoiFile(CTX_G))
  fs <- runFile(CTX_G, aoi$stamp, t, seq_len(40)); fs <- fs[file.exists(fs)]
  lapply(fs, readRDS)
})
st <- do.call(rbind, lapply(types5, function(t) data.frame(type = TYPES[[t]]$label, reps = length(stored[[t]]),
                                                          t(both(stored[[t]])))))
st$old_score <- (st$old_r100 + st$old_r500) / 2
st$new_score <- (st$new_r100 + st$new_r500) / 2
logf("the stored runs of the last validation at Glatt, scored on both surveys:")
print(st, digits = 3, row.names = FALSE)
utils::write.csv(st, file.path(RS, "glatt_stored_runs_rescored.csv"), row.names = FALSE)

#paired bootstrap over replicates, as test_tailored_duln.R does
B <- 1000
NREP <- 20
set.seed(1)
idx <- matrix(sample.int(NREP, NREP * B, replace = TRUE), B)
scoreNew <- function(rr) mean(scNew(rr))
scoreOld <- function(rr) mean(scOld(rr))
boot <- function(rr, f) vapply(seq_len(B), function(b) f(rr[idx[b, ]]), 1)
ref20 <- lapply(stored, function(x) x[seq_len(NREP)])
refB <- list(new = lapply(ref20, boot, f = scoreNew), old = lapply(ref20, boot, f = scoreOld))

#today's Full against the best baseline, on each survey
cmp <- do.call(rbind, lapply(c("old", "new"), function(sv){
  f <- if(sv == "new") scoreNew else scoreOld
  best <- c(nearby = f(ref20$nearby), nearbyRandom = f(ref20$nearbyRandom))
  bt <- names(best)[which.max(best)]
  d <- refB[[sv]]$full - refB[[sv]][[bt]]
  data.frame(survey = sv, full = f(ref20$full), vsBest = TYPES[[bt]]$label, best = best[[bt]],
             dBest = f(ref20$full) - best[[bt]], lo = unname(stats::quantile(d, .025)), hi = unname(stats::quantile(d, .975)))
}))
logf("today's Full against the best baseline at Glatt (20 reps):")
print(cmp, digits = 3, row.names = FALSE)

#the tailored runs at Glatt
tt <- readRDS(file.path(TD, "tailored_runs.rds"))
g <- tt$grid; res <- tt$res
rows <- list()
for(v in setdiff(unique(g$variant), "check")) for(t in c("full", "nearby")){
  i <- which(g$ctx == CTX_G & g$type == t & g$variant == v)
  if(!length(i)) next
  rr <- res[i][order(g$rep[i])]
  if(!all(vapply(rr, function(r) identical(r$status, "done"), logical(1)))) next
  for(sv in c("old", "new")){
    f <- if(sv == "new") scoreNew else scoreOld
    bt <- boot(rr, f)
    best <- c(nearby = f(ref20$nearby), nearbyRandom = f(ref20$nearbyRandom))
    bT <- if(t == "nearby") "nearbyRandom" else names(best)[which.max(best)]
    dS <- bt - refB[[sv]][[t]]; dB <- bt - refB[[sv]][[bT]]
    rows[[length(rows) + 1]] <- data.frame(variant = v, type = TYPES[[t]]$label, survey = sv,
      score = f(rr), ref = f(ref20[[t]]), dSelf = f(rr) - f(ref20[[t]]),
      dSelf_lo = unname(stats::quantile(dS, .025)), dSelf_hi = unname(stats::quantile(dS, .975)),
      vsBest = TYPES[[bT]]$label, dBest = f(rr) - best[[bT]],
      dBest_lo = unname(stats::quantile(dB, .025)), dBest_hi = unname(stats::quantile(dB, .975)))
  }
}
tv <- rbindFill(rows)
utils::write.csv(tv, file.path(RS, "glatt_tailored_runs_rescored.csv"), row.names = FALSE)
options(width = 220)
w <- stats::reshape(tv[tv$type == "Full", c("variant", "survey", "dSelf", "dSelf_lo", "dSelf_hi", "dBest", "dBest_lo", "dBest_hi")],
                    idvar = "variant", timevar = "survey", direction = "wide")
logf("tailored variants at Glatt (Full), old vs corrected survey:")
print(w, digits = 3, row.names = FALSE)
logf("rescored, written to %s", RS)
