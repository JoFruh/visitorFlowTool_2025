#### A validation folder for a DULN variant ####
#
# Builds a VFT_VAL_OUT for data-raw/validate_usage_surveys.R from a finished
# validation (the reference) and a DULN variant folder (aoi/ + network/ as
# data-raw/tailor_duln.R or tailor_duln_types.R write them):
#   aoi/, network/   the variant's (the tailored DULN); without a variant, the reference's
#   survey/          the reference's, Glatt's replaced by the corrected one (the full
#                    FilBleu export, data-raw/rescore_glatt_survey.R)
#   published/       the reference's
#   runs/            the reference's runs of the types that never read DULN
#                    (Nearby-Random, Random, Full-Random), or of every type for a
#                    reference without a variant
# then checks, in the new folder: the copied types' first reps re-simulate
# identically, and - with VFT_PV_CHECK - the variant's Full reproduces the runs of
# its earlier test on the same seeds.
#
#   Rscript data-raw/prepare_variant_validation.R
#
# VFT_PV_OUT      the new folder (required)
# VFT_PV_VARIANT  the variant folder (empty: the reference's DULN)
# VFT_PV_REF      the reference validation (default _final_timefix)
# VFT_PV_GLATT    the corrected Glatt survey (default <REF>/glatt_survey_full/survey/glatt.rds)
# VFT_PV_COPY     the types whose runs are copied, comma-separated (default: the three
#                 that never read DULN with a variant, every type without); "none"
#                 copies no runs and skips the copy check (every run depends on the
#                 new folder's settings)
# VFT_PV_RUNS     the validation the runs are copied from (default VFT_PV_REF)
# VFT_PV_CHECKTYPE the copied type re-simulated for the identity check (default
#                 nearbyRandom if copied, else the first copied type; fullRandom: rep 1
#                 only, it is slow)
# VFT_PV_CHECK    an rds of earlier runs of the variant (list(grid, res), with
#                 variant/ctx/rep/type columns) and the variant name in it, as
#                 path::name

LOCAL <- "C:/Users/frueh/Documents/Local Data"
REF <- Sys.getenv("VFT_PV_REF", file.path(LOCAL, "pathUsageValidation_2026_final_timefix"))
NEW <- Sys.getenv("VFT_PV_OUT", "")
VAR <- Sys.getenv("VFT_PV_VARIANT", "")
GSV <- Sys.getenv("VFT_PV_GLATT", file.path(REF, "glatt_survey_full", "survey", "glatt.rds"))
CHK <- Sys.getenv("VFT_PV_CHECK", "")
RUNS <- Sys.getenv("VFT_PV_RUNS", REF)
COPY <- Sys.getenv("VFT_PV_COPY", "")
if(!nzchar(NEW)) stop("set VFT_PV_OUT")
Sys.setenv(VFT_VAL_WORKER = "1", VFT_VAL_OUT = REF, VFT_VAL_FILBLEU = "full")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "validate_usage_surveys.R"))

cp <- function(from, to){
  dir.create(dirname(to), recursive = TRUE, showWarnings = FALSE)
  if(!file.copy(from, to, overwrite = TRUE)) stop("could not copy ", from)
}
src <- if(nzchar(VAR)) VAR else REF
for(ctx in CONTEXTS){
  cp(file.path(src, "aoi", paste0(ctx, ".rds")), file.path(NEW, "aoi", paste0(ctx, ".rds")))
  g <- file.path(REF, "aoi", paste0(ctx, "_aois.gpkg"))
  if(file.exists(g)) cp(g, file.path(NEW, "aoi", basename(g)))
  for(s in c("_net.rds", "_edges.rds", "_meta.rds"))
    cp(file.path(src, "network", paste0(ctx, s)), file.path(NEW, "network", paste0(ctx, s)))
  cp(if(ctx == "glatt") GSV else file.path(REF, "survey", paste0(ctx, ".rds")),
     file.path(NEW, "survey", paste0(ctx, ".rds")))
}
for(f in list.files(file.path(REF, "published"), full.names = TRUE)) cp(f, file.path(NEW, "published", basename(f)))

#the stamps must agree: the runs directory and every downstream file carry them
for(ctx in CONTEXTS){
  st <- c(aoi = readRDS(file.path(NEW, "aoi", paste0(ctx, ".rds")))$stamp,
          ref = readRDS(file.path(REF, "aoi", paste0(ctx, ".rds")))$stamp,
          survey = readRDS(file.path(NEW, "survey", paste0(ctx, ".rds")))$stamp,
          edges = readRDS(file.path(NEW, "network", paste0(ctx, "_edges.rds")))$stamp)
  if(length(unique(st)) != 1) stop(ctx, ": the stamps differ: ", paste(names(st), st, collapse = ", "))
}

#the runs that do not depend on the DULN
copyTypes <- if(identical(COPY, "none")) character(0) else if(nzchar(COPY)) strsplit(COPY, ",", fixed = TRUE)[[1]] else
  if(nzchar(VAR)) c("nearbyRandom", "random", "fullRandom") else names(TYPES)
stopifnot(all(copyTypes %in% names(TYPES)))
nCopied <- 0L
for(ctx in CONTEXTS){
  stamp <- readRDS(file.path(REF, "aoi", paste0(ctx, ".rds")))$stamp
  for(t in copyTypes){
    d <- file.path(RUNS, "runs", paste0(ctx, "_", stamp), t)
    if(!dir.exists(d)) next
    for(f in list.files(d, full.names = TRUE)){
      cp(f, file.path(NEW, "runs", paste0(ctx, "_", stamp), t, basename(f))); nCopied <- nCopied + 1L
    }
  }
}
logf("prepared %s from %s%s: %d runs copied from %s (%s)", NEW, if(nzchar(VAR)) paste0(VAR, " + ") else "", REF,
     nCopied, RUNS, paste(copyTypes, collapse = ", "))

#### checks, in the new folder ####
if(length(copyTypes) && (nzchar(VAR) || nzchar(COPY))){
  OUT <- NEW
  same <- function(a, b) identical(as.integer(a$cells), as.integer(b$cells)) && identical(as.integer(a$counts), as.integer(b$counts))
  ct <- Sys.getenv("VFT_PV_CHECKTYPE", if("nearbyRandom" %in% copyTypes) "nearbyRandom" else copyTypes[1])
  stopifnot(ct %in% copyTypes)
  for(ctx in CONTEXTS) for(rep in if(ct == "fullRandom") 1L else 1:2){
    r <- simMemory(ctx, ct, rep)
    s <- readRDS(runFile(ctx, readRDS(aoiFile(ctx))$stamp, ct, rep))
    logf("check %s %s rep %d: %s", ctx, TYPES[[ct]]$label, rep, if(same(r, s)) "IDENTICAL to the copied run" else "DIFFERENT")
    if(!same(r, s)) stop("a copied type re-simulates differently - the folder is not consistent")
  }
  if(nzchar(CHK)){
    parts <- strsplit(CHK, "::", fixed = TRUE)[[1]]
    old <- readRDS(parts[1])
    for(ctx in CONTEXTS){
      i <- which(old$grid$variant == parts[2] & old$grid$ctx == ctx & old$grid$rep == 1 & old$grid$type == "full")
      if(length(i) != 1) stop("no earlier Full rep 1 of ", parts[2], " at ", ctx)
      r <- simMemory(ctx, "full", 1)
      ok <- same(r, old$res[[i]])
      logf("check %s Full rep 1: %s the earlier run of %s", ctx, if(ok) "IDENTICAL to" else "DIFFERENT from", parts[2])
      if(!ok) stop("the variant's Full differs from its earlier test")
    }
  }
}
logf("ready: %s", NEW)
