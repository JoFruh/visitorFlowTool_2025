#### Put a tailored DULN variant on a validation folder's own areas ####
#
# The Glatt-calibrated DULN (data-raw/tailor_duln_types.R, variant T_shared_s0.5)
# was built on the app-save areas of _final_timefix: element weights fitted on the
# survey, applied as deltas to the network's edges and nodes and to the areas' DULN.
# This puts the SAME weights on another folder's areas and network - e.g. areas
# made by step 4's "Automatic Cuts" - with tailor_duln_types.R's own
# stageVariantsTypes(), unchanged:
#   1. the folder's aoi/ and network/ (today's DULN, from validate_usage_surveys.R's
#      aoi, survey and network stages) are moved to <BASE>/base_todayDULN/;
#   2. the variant is built from them into <BASE>/duln_variant/variants/<v>/;
#   3. its aoi/ and network/ become the folder's.
# The weights (fit.rds, fit_types.rds) and the element extracts per edge come from
# the tailored folder; the network's edges must be the extracts' (same perimeter).
#
#   VFT_AV_BASE=<folder> Rscript data-raw/apply_duln_variant.R
#
# VFT_AV_BASE     the validation folder (required)
# VFT_AV_VARIANT  the variant (default T_shared_s0.5)
# VFT_AV_TD       the tailored folder (default <_final_timefix>/duln_tailored)
# VFT_AV_CHECK    a variants folder to compare the result with, file by file (the
#                 stored variant, when BASE holds the areas it was built on)
# VFT_AV_SCRIPT   the script that defines the variant (default tailor_duln_types.R;
#                 data-raw/tailor_duln_activities.R for the per-activity variants,
#                 tailor_duln_strava.R for the Strava ones, tailor_duln_osm.R for the
#                 OSM-trace ones, tailor_duln_strava_areas.R for Strava over the OSM areas)
# VFT_AV_TYSUB    the tailored folder's subfolder with fit_types.rds (default types;
#                 activities for the per-activity fit, strava for the Strava fit,
#                 osm/clean for the OSM fit, strava_areas/clean or strava_areas/width/clean
#                 for Strava over the OSM areas)

LOCAL <- "C:/Users/frueh/Documents/Local Data"
BASE  <- Sys.getenv("VFT_AV_BASE", "")
VNAME <- Sys.getenv("VFT_AV_VARIANT", "T_shared_s0.5")
TDIR  <- Sys.getenv("VFT_AV_TD", file.path(LOCAL, "pathUsageValidation_2026_final_timefix", "duln_tailored"))
CHECK <- Sys.getenv("VFT_AV_CHECK", "")
SCRIPT <- Sys.getenv("VFT_AV_SCRIPT", "tailor_duln_types.R")
TYSUB <- Sys.getenv("VFT_AV_TYSUB", "types")
if(!nzchar(BASE)) stop("set VFT_AV_BASE")
TYB <- file.path(BASE, "duln_variant")

#1. today's aoi/network aside, as the variant's base
keep <- file.path(BASE, "base_todayDULN")
for(d in c("aoi", "network")){
  dir.create(file.path(keep, d), recursive = TRUE, showWarnings = FALSE)
  for(f in list.files(file.path(BASE, d), full.names = TRUE))
    if(!file.exists(file.path(keep, d, basename(f))) && !file.copy(f, file.path(keep, d, basename(f))))
      stop("could not copy ", f)
}
dir.create(TYB, recursive = TRUE, showWarnings = FALSE)
for(f in c("fit_types.rds", "survey_types.csv"))
  if(!file.copy(file.path(TDIR, TYSUB, f), file.path(TYB, f), overwrite = TRUE)) stop("no ", f, " in ", TDIR)

#2. the variant, from the base (tailor_duln_types.R's definitions, no stage run)
Sys.setenv(VFT_VAL_OUT = keep, VFT_TD_OUT = TDIR, VFT_TY_OUT = TYB, VFT_TY_STAGES = "none", VFT_ACT_STAGES = "none", VFT_EL_STAGES = "none",
           VFT_ST_STAGES = "none", VFT_OSM_STAGES = "none", VFT_SA_STAGES = "none")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", SCRIPT))
#the contexts of VFT_VAL_CONTEXTS (validate_usage_surveys.R), all three by default
ctxs <- CONTEXTS
stopifnot(VNAME %in% names(TY_VARIANTS))
TY_VARIANTS <- TY_VARIANTS[VNAME]
#a variant built on other areas must not be reused
unlink(file.path(TYB, "variants", VNAME), recursive = TRUE)
stageVariantsTypes()

#3. the variant's aoi/network become the folder's
vd <- file.path(TYB, "variants", VNAME)
for(ctx in ctxs){
  for(f in c(file.path("aoi", paste0(ctx, ".rds")), file.path("network", paste0(ctx, c("_net.rds", "_edges.rds", "_meta.rds")))))
    if(!file.copy(file.path(vd, f), file.path(BASE, f), overwrite = TRUE)) stop("could not copy ", f)
  a <- readRDS(file.path(BASE, "aoi", paste0(ctx, ".rds")))
  logf("apply %s %s: %d areas, stamp %s; area DULN today -> variant r %.3f",
       VNAME, ctx, nrow(a$polys), a$stamp,
       stats::cor(readRDS(file.path(keep, "aoi", paste0(ctx, ".rds")))$polys$DULN, a$polys$DULN))
}

#the check: the stored variant, file by file
if(nzchar(CHECK)){
  for(ctx in ctxs){
    for(f in c(file.path("aoi", paste0(ctx, ".rds")), file.path("network", paste0(ctx, "_net.rds")))){
      x <- readRDS(file.path(vd, f)); y <- readRDS(file.path(CHECK, f))
      same <- if(grepl("_net", f)) identical(igraph::edge_attr(x$net), igraph::edge_attr(y$net)) &&
        identical(igraph::vertex_attr(x$net), igraph::vertex_attr(y$net)) && identical(x$parking, y$parking) else
        isTRUE(all.equal(x$polys, y$polys)) && identical(x$stamp, y$stamp)
      logf("check %s %s: %s", ctx, f, if(same) "IDENTICAL to the stored variant" else "DIFFERENT")
    }
  }
}
