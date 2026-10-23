#### Survey validation of the step-5 recreation simulation, on today's engine ####
#
# The published test - Fig 5 (GlattPark), Fig 5_6 (Wigger + Glatt) and the
# performance bars - ran the OLD engine on hand-made areas of interest
# (finalPolygons_2_5_*.gdb) and lives in
#   H:/Jo/Project Visitor Flow Tool/R/pathUsageComparisons/pathUsageComparisons_final_2.R
# This script redoes it end to end with the engine as it is now, in stages:
#
#   aoi       the tool's own areas: step 3's suggested threshold, then step 4's
#             "Automatic Cuts" (generateAoI2(split = TRUE), within = NULL)
#   survey    the three surveys rebuilt from the raw data, NOT clipped to any areas
#   network   the path network as step 5 loads it, residents = survey respondents
#   selftest  (opt-in) proves the track-capturing hook leaves the simulation alone
#   sim       4 simulation types x N replicates per context; resumable
#   compare   the old recipe (500 m and 100 m), replicate stability, a paired
#             bootstrap over replicates and respondents, and the extra tests
#   figures   look-alikes of Fig 5, Fig 5_6 and the performance bars, plus the
#             extra tests
#
# THE SIM STAGE IS HEAVY - hours for the Glatt region. Smoke-test first:
#
#   VFT_VAL_CONTEXTS=glattPark VFT_VAL_REPS=2 \
#   VFT_VAL_STAGES=aoi,survey,network,selftest,sim,compare,figures \
#   Rscript data-raw/validate_usage_surveys.R
#
# Switches (environment variables, all optional):
#   VFT_VAL_STAGES    comma list; default aoi,survey,network,sim,compare,figures
#   VFT_VAL_CONTEXTS  any of glattPark,wigger,glatt (default all three)
#   VFT_VAL_MODES     any of full,nearby,nearbyRandom,random,fullRandom (default all five),
#                     also fullCal and twoStage (the two-stage simulation, R/two_stage.R)
#   VFT_VAL_REPS      replicates per context and type, default 100
#   VFT_VAL_WORKERS   simulations in parallel (mirai daemons), default 1. Each
#                     daemon holds a whole network: mind the memory on Glatt.
#   VFT_VAL_BOOT      bootstrap draws, default 1000
#   VFT_VAL_OUT       output directory (outside OneDrive on purpose: thousands
#                     of run files)
#   VFT_VAL_FORCE     comma list of stages to redo although their files exist
#   VFT_VAL_ENGINE    "engine" (default) or "corrected": the corrected engine of
#                     data-raw/engine_corrections.R in every type - no straight-on
#                     preference in the walk (Full, Nearby, Nearby-Random, Random);
#                     for Full and Nearby the 10 % reach on the real distance,
#                     floored at 5 minutes, with the closest-area fallback capped
#                     at 33 % of the outing. Random and Full-Random keep their own
#                     area rules. Use a fresh VFT_VAL_OUT: runs carry the engine
#                     and compare refuses to mix them.
#   VFT_VAL_THRESHOLD fixes the step-3 threshold of the aoi stage (default: the
#                     suggested one, vftSuggestAoIThreshold())
#   VFT_VAL_DULN_ALL  the raster the aoi stage thresholds and cuts (default the app's
#                     maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif); e.g. an aggregate of a
#                     tailored DULN (data-raw/aggregate_tailored_duln.R). It must cover
#                     the perimeter + 1 km. Use a fresh VFT_VAL_OUT.
#   VFT_VAL_AOI_CUT   1 (default): the areas are step 4's "Automatic Cuts"; 0: the
#                     unsplit areas step 4 arrives with
#   VFT_VAL_AOI_SAVES comma list of context=path to an app save (.RData) whose
#                     envBase_finalPolygons and envBase_minThresh replace the aoi
#                     stage's own areas for that context. Use a fresh VFT_VAL_OUT:
#                     the stamp gets a "_save" suffix, but aoi/<ctx>.rds is shared.
#   VFT_VAL_DURATION  multiplies every agent's outing duration (timeExt); unset = the
#                     engine's durations. VFT_VAL_DURSCOPE "outing" (default) lets the
#                     reach grow with it, "walk" only the time inside the areas (the
#                     area choice sees the drawn durations; corrected engine, single-
#                     stage types only). Use a fresh VFT_VAL_OUT: runs carry it.
#   VFT_VAL_OUTING    "area" (default, the engine): the duration is spent inside the
#                     area, the walk there and back comes on top. "whole": the duration
#                     is the whole outing - an agent turns home once its time out plus
#                     the walk back (taken as long as its walk there) reaches it
#                     (vftTimeOver() hooked; single-stage types only).
#   VFT_VAL_DURSRC    an rds of outing durations per agent type (data-raw/
#                     calibrate_aoi_rum.R: the Glatt routes'); every agent's duration
#                     is drawn again from its type's after the engine's draw, and the
#                     area choice's standardisation is bounded by DUR_RANGE instead of
#                     the population's shortest and longest outing.
#   VFT_VAL_PARKING   one share of car-park starts for Full at every site (unset: the
#                     contexts' parkingShare).
#   VFT_VAL_RUM       an rds of area-choice coefficients (data-raw/calibrate_aoi_rum.R)
#                     for the types fullRumG, fullRumGT and nearbyRumG.
#   VFT_VAL_RUM_TIME  an rds of time-dependent area-choice models (data-raw/
#                     calibrate_aoi_rum_time.R) for fullRumTime, fullRumShare, fullRumFixed.
#                     Runs carry all four; compare refuses a type that mixes them.
#
# Every stage writes to VFT_VAL_OUT and skips work already done, so an interrupted
# run continues where it stopped. Nothing here is used by the app.
#
#### The four simulation types ####
#
# No package code is changed. Each type removes an INPUT of the engine, not its
# mechanics:
#
#   type          area choice                 routing to/from   inside an area   parking
#   Full          engine utility              engine weights    attractivity     PARKING_SHARE
#   Nearby        distance only               path length       attractivity     off
#   Nearby-Random distance only               path length       none             off
#   Random        uniform, within 1/3 of the  path length       none             off
#                 outing's duration
#   Full-Random   uniform over every area     path length       uniform walk     off
#                 reachable on the network
#
# - "distance only": every area gets the same DULN and area. findClosestAOI_cpp()
#   standardises area with hard-coded bounds (src/CPP_FUNCTIONS.cpp ~2030), so the
#   attractivity term becomes one constant per agent, and the multinomial logit is
#   shift-invariant: the choice then rests on -6.65703 * distPerDurationSTD alone.
#   The engine's own 10 % reach limit stays.
# - "path length": all 21 SHAPE_Leng_* weights become pmax(SHAPE_Leng, 10), the
#   floor vftPrepareNetwork() clamps to. That covers the area distances (_attr),
#   the way out ("distance") and the way home ("little_attr").
# - "none" inside an area: the node DULN_* columns, which only feed
#   chooseBestRoutes_cpp() through DULN_df, are set to 0. The preference for going
#   straight on and the penalty for revisiting stay: a correlated random walk.
# - Random's area choice is the one hook into the engine: findClosestAOI_cpp() is
#   wrapped and its probabilities replaced by a uniform draw among the areas
#   reachable within RANDOM_REACH of the outing's duration (the user's choice: a
#   laxer limit than Nearby's 10 %, so the two differ in more than the weights).
# - Full-Random (added on the user's request after the first full run): the user's
#   original definition of Random, where neither proximity nor attractivity plays
#   any role. The same hook draws uniformly over EVERY area the agent can reach on
#   the network, whatever its outing's duration. Inside the areas a third hook
#   replaces chooseBestRoutes_cpp() by equal weights: no attractivity, no
#   preference for going straight on, no penalty for revisiting. The engine's own
#   filter (filterRouteChoices_cpp) still decides which neighbours are candidates:
#   not the node it just left, not outside every area, not one marked retraced.
# - Parking: only Full starts agents at car parks, and in a SET SHARE of the starts
#   (the user's choice): 0 at GlattPark (a survey of its residents), 8 % at Glatt
#   (110 of the 1,362 FilBleu routes arrived by car) and 8 % at Wigger. The car
#   parks' weights are scaled to give that share. vftSimulateScenario()'s fixed
#   parkingIntensity 0.1 is calibrated to the census residents raster: against the
#   respondent rasters it put 83 % of GlattPark's agents on car parks. The other
#   types start at respondents' homes only.
#
# The second hook wraps historyToPassage_cpp() to copy each agent's edge history
# before launchSim() strips it (R/launchSim_v2.R ~1338). That gives the per-agent
# tracks the old metric counted: an agent counts once in every cell its track
# passes within 50 m of. The selftest stage proves the copy changes nothing.
#
#### The old recipe, as it actually ran ####
#
# The old script aggregated to 500 m with
#   terra::aggregate(x, fact = 5, na.rm = TRUE, method = "sum")
# but aggregate() has no `method` argument: terra ignored it and used its default
# fun = "mean" (checked: values 1, 2, 3, NA give 2, not 6). The published 500 m
# values are therefore MEANS of the 100 m cells inside the areas, and that is what
# is reproduced here, at every scale. GlattPark's resample(method = "sum") did sum.
#
# Two more details of the old survey rasters change those means, and are kept:
# - Glatt: raster_all_withCar_100m had its zeros set to NA before the comparison,
#   so a 500 m survey value is the mean over the cells WITH usage only.
# - Glatt and Wigger: `survey[crop(lakes, perimeter)] <- 0` came after the masks,
#   so lake cells - also outside the areas - count as zeros in the survey's means.
# The zeros-as-NA detail does not touch the 100 m results (the recipe sets NA back
# to 0 there); the lakes do, at every scale, as before.

#### 0. switches and paths ####

envList <- function(name, default){
  v <- trimws(Sys.getenv(name, ""))
  if(nzchar(v)) trimws(strsplit(v, ",", fixed = TRUE)[[1]]) else default
}
envNum <- function(name, default){
  v <- suppressWarnings(as.numeric(Sys.getenv(name, "")))
  if(isTRUE(is.finite(v))) v else default
}

IS_WORKER <- identical(Sys.getenv("VFT_VAL_WORKER"), "1")
STAGES    <- envList("VFT_VAL_STAGES", c("aoi", "survey", "network", "sim", "compare", "figures"))
CONTEXTS  <- envList("VFT_VAL_CONTEXTS", c("glattPark", "wigger", "glatt"))
TYPES_RUN <- envList("VFT_VAL_MODES", c("full", "nearby", "nearbyRandom", "random", "fullRandom"))
N_REPS    <- as.integer(envNum("VFT_VAL_REPS", 100))
WORKERS   <- as.integer(envNum("VFT_VAL_WORKERS", 1))
N_BOOT    <- as.integer(envNum("VFT_VAL_BOOT", 1000))
FORCE     <- envList("VFT_VAL_FORCE", character(0))
OUT       <- Sys.getenv("VFT_VAL_OUT", "C:/Users/frueh/Documents/Local Data/pathUsageValidation_2026")
ENGINE_MODE <- Sys.getenv("VFT_VAL_ENGINE", "engine")
if(!ENGINE_MODE %in% c("engine", "corrected")) stop("VFT_VAL_ENGINE must be engine or corrected")
#Longer (or shorter) outings: every agent's duration (timeExt) times VFT_VAL_DURATION.
#VFT_VAL_DURSCOPE "outing" (default): everything that reads the duration sees the new
#one - the time inside the areas, the reach (CORR$reach of the outing) and the
#fallback cap. "walk": only the time inside the areas; the area choice keeps the drawn
#durations. Unset = no hook; set (even to 1) = the hook is in place, so a factor of 1
#proves it changes nothing.
DURATION <- local({
  f <- envNum("VFT_VAL_DURATION", NA_real_)
  if(is.na(f)) NULL else {
    s <- Sys.getenv("VFT_VAL_DURSCOPE", "outing")
    if(!s %in% c("outing", "walk")) stop("VFT_VAL_DURSCOPE must be outing or walk")
    if(!(f > 0)) stop("VFT_VAL_DURATION must be positive")
    list(factor = f, scope = s)
  }
})
#what the area choices divide the (scaled) durations by
durChoiceDiv <- function() if(!is.null(DURATION) && DURATION$scope == "walk") DURATION$factor else 1
#the engine's agent types, in the order vftAgentTimeAndSpeed() draws them
AGENT_TYPES <- c("walkNat", "walkSoc", "dogNat", "dogProx", "ebikeNat", "bikeSport", "jogger")
#What the duration covers (see the header). "whole": an agent turns home when its time
#out (timer, the walk there included) plus the walk back - taken as long as the walk
#there, timeNotAccounted - reaches its duration.
OUTING <- Sys.getenv("VFT_VAL_OUTING", "area")
if(!OUTING %in% c("area", "whole")) stop("VFT_VAL_OUTING must be area or whole")
#Other outing durations: list(durs = one vector of minutes per agent type). With them
#the drawn durations are no longer the engine's 30-240 classes, so the area choice's
#bounds are fixed at those (a population's shortest drawn route of a few minutes would
#flatten every agent's standardised terms).
DURSRC <- local({
  f <- Sys.getenv("VFT_VAL_DURSRC", "")
  if(!nzchar(f)) NULL else {
    x <- readRDS(f)
    if(!all(AGENT_TYPES %in% names(x$durs)) || !all(vapply(x$durs[AGENT_TYPES], function(d) length(d) > 0 && all(d > 0), TRUE)))
      stop("VFT_VAL_DURSRC: ", f, " needs positive durations for every agent type")
    list(file = normalizePath(f, winslash = "/"), md5 = unname(tools::md5sum(f)), durs = x$durs[AGENT_TYPES])
  }
})
DUR_RANGE <- c(30, 240)
aoiDurRange <- function() if(is.null(DURSRC)) NULL else DUR_RANGE
#one car-park share for every context; NA = CTX's parkingShare
PARKING <- envNum("VFT_VAL_PARKING", NA_real_)
if(!is.na(PARKING) && !(PARKING >= 0 && PARKING < 1)) stop("VFT_VAL_PARKING must be in [0, 1)")
#The RUM types' area-choice coefficients: list(pooled = c(bAttr =, bDist =), distance =
#c(bAttr = 0, bDist =), types = data.frame(type, bAttr, bDist) over AGENT_TYPES).
RUM <- local({
  f <- Sys.getenv("VFT_VAL_RUM", "")
  if(!nzchar(f)) NULL else {
    x <- readRDS(f)
    if(!all(c("pooled", "distance", "types") %in% names(x)) || !all(AGENT_TYPES %in% x$types$type))
      stop("VFT_VAL_RUM: ", f, " needs pooled, distance and types for every agent type")
    c(x[c("pooled", "distance", "types")], list(file = normalizePath(f, winslash = "/"), md5 = unname(tools::md5sum(f))))
  }
})
#The time-dependent area choice (data-raw/calibrate_aoi_rum_time.R; aoiProbsTime() in
#data-raw/engine_corrections.R): list(models = list(<name> = c(<column> = b, ...)),
#consts = list(dulnCenter, dulnScale)), optionally areaDuln = list(<ctx> = c(<AOI> = DULN)):
#the area DULN the choice reads instead of the scenario's.
RUM_TIME <- local({
  f <- Sys.getenv("VFT_VAL_RUM_TIME", "")
  if(!nzchar(f)) NULL else {
    x <- readRDS(f)
    if(!all(c("models", "consts") %in% names(x))) stop("VFT_VAL_RUM_TIME: ", f, " needs models and consts")
    c(x[intersect(c("models", "consts", "areaDuln"), names(x))], list(file = normalizePath(f, winslash = "/"), md5 = unname(tools::md5sum(f))))
  }
})
rumTimeCoefs <- function(spec){
  if(is.null(RUM_TIME)) stop("type ", spec$label, " needs VFT_VAL_RUM_TIME")
  b <- RUM_TIME$models[[spec$rumTime]]
  if(is.null(b)) stop("VFT_VAL_RUM_TIME has no model ", spec$rumTime)
  b
}
#a RUM type's coefficients, one per agent
rumCoefs <- function(spec, agentTyps){
  if(is.null(RUM)) stop("type ", spec$label, " needs VFT_VAL_RUM")
  if(spec$rum %in% c("pooled", "distance")){
    b <- RUM[[spec$rum]]
    return(list(bAttr = unname(b[["bAttr"]]), bDist = unname(b[["bDist"]])))
  }
  k <- match(as.character(agentTyps), RUM$types$type)
  if(anyNA(k)) stop("VFT_VAL_RUM has no coefficients for agent type(s) ",
                    paste(unique(agentTyps[is.na(k)]), collapse = ", "))
  list(bAttr = RUM$types$bAttr[k], bDist = RUM$types$bDist[k])
}
#the aoi stage's DULN_all and whether it cuts
dulnAllFile <- function() Sys.getenv("VFT_VAL_DULN_ALL", ns$vftData("maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif"))
AOI_CUT <- !identical(Sys.getenv("VFT_VAL_AOI_CUT", "1"), "0")
AOI_SAVES <- local({
  v <- envList("VFT_VAL_AOI_SAVES", character(0))
  stats::setNames(sub("^[^=]*=", "", v), sub("=.*$", "", v))
})

REPO <- Sys.getenv("VFT_VAL_REPO", "")
if(!nzchar(REPO)){
  REPO <- if(file.exists("DESCRIPTION") &&
              any(grepl("^Package: visitorFlowTool", readLines("DESCRIPTION", warn = FALSE))))
    normalizePath(".", winslash = "/") else "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025"
}

#where this file is, so mirai daemons can source it
.thisScript <- function(){
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if(length(f)) return(normalizePath(f[1], winslash = "/"))
  for(i in rev(seq_len(sys.nframe()))){
    of <- sys.frame(i)$ofile
    if(!is.null(of)) return(normalizePath(of, winslash = "/"))
  }
  file.path(REPO, "data-raw", "validate_usage_surveys.R")
}
if(!IS_WORKER) SCRIPT <- .thisScript()

ONEDRIVE <- "C:/Users/frueh/OneDrive - Eidg. Forschungsanstalt WSL/Dokumente"
PATHS <- list(
  #the 100 m grid every old comparison was rasterised on (swisscom_5Oct_*_100m)
  swisscom      = file.path(ONEDRIVE, "Local Large Datasets/Swisscom/Days (raster)/raster_2022-10-05_heatmapDailyUniqueCounts.tif"),
  wiggerLines   = "C:/Users/frueh/Documents/Local Data/Shared visitorFlow Project Data/Social Data/Wigger_data/Linien_GEOREF.gdb",
  filBleuDir    = file.path(ONEDRIVE, "Local Data/FilBleuUsage_withCar"),
  #the full FilBleu export: every route with its activity and motives
  filBleuAll    = file.path(ONEDRIVE, "Local Large Datasets/FilBleu/filBleu_allPaths_clean.shp"),
  glattParkHeat = file.path(ONEDRIVE, "Local Large Datasets/GlattPark/GlattPark_heatMap.tif"),
  #reference only: the published survey rasters, old areas and old results.
  #VFT_VAL_OLD_DIR stands in when the H: share is not mounted (any folder holding
  #performance_pearson.RData)
  oldDir        = Sys.getenv("VFT_VAL_OLD_DIR", "H:/Jo/Project Visitor Flow Tool/R/pathUsageComparisons"),
  oldAoiDir     = file.path(ONEDRIVE, "Local Data/pathUsageReplicates")
)
FILBLEU_LAYERS <- c("E_bike", "dogWalking", "bike", "walking", "jogging_nordicWalking")
#Where the Glatt routes come from (VFT_VAL_FILBLEU):
#  "full"    (default since 2026-10-18) filBleu_allPaths_clean.shp, the routes whose
#            activity is walking, biking, e-biking, dog walking or jogging / Nordic
#            walking - commuting, inline skating and routes without an activity
#            answer are left out;
#  "exports" the five Copy_activity_*.shp exports the published raster was built
#            from. Their jogging export holds NO jogging: it is 79 of the 99 dog-walking
#            routes again (same ids and geometry), so those were counted twice and the
#            260 jogging routes (201 respondents) were missing. Kept to reproduce the
#            published numbers.
FILBLEU_SOURCE <- Sys.getenv("VFT_VAL_FILBLEU", "full")
if(!FILBLEU_SOURCE %in% c("full", "exports")) stop("VFT_VAL_FILBLEU must be full or exports")
#the full file's activity answers -> the export layer names
filBleuLayerOf <- function(a){
  out <- rep(NA_character_, length(a))
  out[a %in% "Spazieren / Wandern"] <- "walking"
  out[a %in% "Velo fahren / Biken"] <- "bike"
  out[a %in% c("E-Bike", "E-Biken")] <- "E_bike"
  out[grepl("^Hund ausf", a)] <- "dogWalking"
  out[a %in% "Joggen / Nordic Walking"] <- "jogging_nordicWalking"
  out
}

#Data-directory paths (perimeter, residential) go through vftData().
#nAgents NA = the respondent raster's sum over the network's extent, undivided.
CTX <- list(
  glattPark = list(label = "GlattPark", perimeter = "maps/glattPark_area.shp",
                   residential = "maps/residential/glattPark_residential_raster_final.tif",
                   nAgents = NA, survey = "heatmap", buffer = 0,
                   #the survey IS a 500 m grid: its scales are 500 and 1000 m, and
                   #500 m is both the published and the unaggregated result
                   scales = c(1, 2), oldScale = 1, simTransform = "sqrt",
                   #the four Glattpark cells the old script could drop (Fig 5)
                   dropCells = cbind(row = c(9, 9, 10, 10), col = c(13, 14, 13, 14)),
                   lakesToZero = FALSE, surveyZerosNA = FALSE, parkingShare = 0,
                   savedSurvey = "glattPark_survey.tif",
                   oldAoi = "finalPolygons_2_5_glattPark.gdb"),
  wigger    = list(label = "Wigger", perimeter = "maps/wiggerPerimeter.kml",
                   residential = "maps/residential/wigger_residential_subset_raster_4326_v2.tif",
                   #343 respondents; the raster itself is scaled to a total of 1e5
                   nAgents = 343, survey = "wigger", buffer = 50,
                   scales = c(1, 2, 5, 10), oldScale = 5, simTransform = "none",
                   dropCells = NULL, lakesToZero = TRUE, surveyZerosNA = FALSE,
                   parkingShare = 0.08,
                   savedSurvey = "allWigger_survey.tif",
                   oldAoi = "finalPolygons_2_5_wiggerAll_cutPolys_v3.gdb"),
  glatt     = list(label = "Glatt wider region", perimeter = "maps/filBleu_area_final.shp",
                   residential = "maps/residential/filBleu_residential_subset_raster_4326.tif",
                   nAgents = NA, survey = "filBleu", buffer = 50,
                   scales = c(1, 2, 5, 10), oldScale = 5, simTransform = "none",
                   dropCells = NULL, lakesToZero = TRUE, surveyZerosNA = TRUE,
                   parkingShare = 0.08,
                   savedSurvey = "allGlatt_survey.tif",
                   oldAoi = "finalPolygons_2_5_Glatt.gdb")
)

TYPES <- list(
  full         = list(label = "Full",          aoiChoice = "engine",   routing = "engine", movement = "engine", parking = TRUE),
  nearby       = list(label = "Nearby",        aoiChoice = "distance", routing = "length", movement = "engine", parking = FALSE),
  nearbyRandom = list(label = "Nearby-Random", aoiChoice = "distance", routing = "length", movement = "flat",   parking = FALSE),
  random       = list(label = "Random",        aoiChoice = "uniform",  routing = "length", movement = "flat",   parking = FALSE),
  fullRandom   = list(label = "Full-Random",   aoiChoice = "uniformAll", routing = "length", movement = "uniform", parking = FALSE),
  #the two-stage simulation (R/two_stage.R): Full explores, then a second population
  #chooses among the walks it recorded. Under VFT_VAL_ENGINE=corrected the walk choice
  #has the corrected reach rule (choosePathsCorr() in data-raw/engine_corrections.R).
  #Not in the default VFT_VAL_MODES.
  twoStage     = list(label = "Two-stage",     aoiChoice = "engine",   routing = "engine", movement = "engine", parking = TRUE,
                      twoStage = TRUE),
  #...with 3 and 10 times as many explorers as agents: a richer library of walks
  twoStageX3   = list(label = "Two-stage x3",  aoiChoice = "engine",   routing = "engine", movement = "engine", parking = TRUE,
                      twoStage = TRUE, twoStageParams = list(explorers = 3)),
  twoStageX10  = list(label = "Two-stage x10", aoiChoice = "engine",   routing = "engine", movement = "engine", parking = TRUE,
                      twoStage = TRUE, twoStageParams = list(explorers = 10)),
  #...with a more selective walk choice: a logit scale of 0.5 (P ~ exp(U / 0.5)), or
  #the attractivity coefficient doubled; and the explorers at route alpha 5 instead of
  #the run's (exploreAlpha: phase 1's chooseBestRoutes hook only)
  twoStageS05  = list(label = "Two-st. s0.5",  aoiChoice = "engine",   routing = "engine", movement = "engine", parking = TRUE,
                      twoStage = TRUE, twoStageParams = list(alpha = 0.5)),
  twoStageQ2   = list(label = "Two-st. q2",    aoiChoice = "engine",   routing = "engine", movement = "engine", parking = TRUE,
                      twoStage = TRUE, twoStageParams = list(bAttr = 2 * 3.23695)),
  twoStageA5   = list(label = "Two-st. a5",    aoiChoice = "engine",   routing = "engine", movement = "engine", parking = TRUE,
                      twoStage = TRUE, exploreAlpha = 5),
  twoStageA5S05 = list(label = "Two-st. a5 s0.5", aoiChoice = "engine", routing = "engine", movement = "engine", parking = TRUE,
                      twoStage = TRUE, exploreAlpha = 5, twoStageParams = list(alpha = 0.5)),
  twoStageA5Q2 = list(label = "Two-st. a5 q2", aoiChoice = "engine",   routing = "engine", movement = "engine", parking = TRUE,
                      twoStage = TRUE, exploreAlpha = 5, twoStageParams = list(bAttr = 2 * 3.23695)),
  #...and the other way: a SOFTER walk choice (logit scale 2) after explorers at alpha
  #5, and explorers at alpha 10
  twoStageA5S2 = list(label = "Two-st. a5 s2", aoiChoice = "engine",   routing = "engine", movement = "engine", parking = TRUE,
                      twoStage = TRUE, exploreAlpha = 5, twoStageParams = list(alpha = 2)),
  twoStageA10  = list(label = "Two-st. a10",   aoiChoice = "engine",   routing = "engine", movement = "engine", parking = TRUE,
                      twoStage = TRUE, exploreAlpha = 10),
  #the calibrated model (data-raw/calibrate_full.R): CAL_PAR, see aoiProbsCal() in
  #data-raw/engine_corrections.R. Not in the default VFT_VAL_MODES.
  fullCal      = list(label = "Full-Calibrated", aoiChoice = "cal", routing = "cal", movement = "cal", parking = TRUE),
  #the area choice re-fitted on the Glatt survey's routes (data-raw/calibrate_aoi_rum.R;
  #VFT_VAL_RUM, corrected engine only): Full with the pooled coefficients, with one pair
  #per agent type, and Nearby with the distance-only fit. Not in the default VFT_VAL_MODES.
  fullRumG     = list(label = "Full RUM-G",       aoiChoice = "engine",   routing = "engine", movement = "engine", parking = TRUE,
                      rum = "pooled"),
  fullRumGT    = list(label = "Full RUM-G types", aoiChoice = "engine",   routing = "engine", movement = "engine", parking = TRUE,
                      rum = "types"),
  nearbyRumG   = list(label = "Nearby RUM-G",     aoiChoice = "distance", routing = "length", movement = "engine", parking = FALSE,
                      rum = "distance"),
  #the area choice whose attractivity / proximity trade-off moves with the time in the
  #area (data-raw/calibrate_aoi_rum_time.R; VFT_VAL_RUM_TIME, corrected engine only):
  #the interaction model, the smallest fix (the way there as a share of the time), and
  #the same fixed-scale utility without time (the paired test of the time terms).
  #Not in the default VFT_VAL_MODES.
  fullRumTime  = list(label = "Full RUM-time",       aoiChoice = "engine", routing = "engine", movement = "engine", parking = TRUE,
                      rumTime = "interaction"),
  fullRumShare = list(label = "Full RUM-time share", aoiChoice = "engine", routing = "engine", movement = "engine", parking = TRUE,
                      rumTime = "share"),
  fullRumFixed = list(label = "Full RUM-fixed",      aoiChoice = "engine", routing = "engine", movement = "engine", parking = TRUE,
                      rumTime = "fixed")
)
CONTRASTS <- list(c("full", "nearby"), c("nearby", "nearbyRandom"),
                  c("nearbyRandom", "random"), c("full", "random"),
                  c("random", "fullRandom"), c("full", "fullRandom"),
                  c("fullCal", "nearby"), c("fullCal", "nearbyRandom"), c("fullCal", "full"),
                  c("twoStage", "full"), c("twoStage", "nearby"), c("twoStage", "nearbyRandom"),
                  c("twoStageX3", "full"), c("twoStageX10", "full"),
                  c("twoStageX3", "twoStage"), c("twoStageX10", "twoStage"), c("twoStageX10", "twoStageX3"),
                  c("twoStageS05", "full"), c("twoStageQ2", "full"), c("twoStageA5", "full"),
                  c("twoStageA5S05", "full"), c("twoStageA5Q2", "full"),
                  c("twoStageS05", "twoStage"), c("twoStageQ2", "twoStage"), c("twoStageA5", "twoStage"),
                  c("twoStageA5S05", "twoStage"), c("twoStageA5Q2", "twoStage"),
                  c("twoStageA5S05", "twoStageA5"), c("twoStageA5Q2", "twoStageA5"),
                  c("twoStageA5S2", "full"), c("twoStageA10", "full"),
                  c("twoStageA5S2", "twoStageA5"), c("twoStageA10", "twoStageA5"),
                  c("twoStageA5S2", "nearby"), c("twoStageA5S2", "nearbyRandom"),
                  c("full", "nearbyRandom"),
                  c("fullRumG", "full"), c("fullRumGT", "full"), c("fullRumGT", "fullRumG"),
                  c("nearbyRumG", "nearby"), c("fullRumG", "nearbyRumG"), c("fullRumGT", "nearbyRumG"),
                  c("fullRumG", "nearby"), c("fullRumGT", "nearby"),
                  c("fullRumG", "nearbyRandom"), c("fullRumGT", "nearbyRandom"),
                  c("fullRumTime", "full"), c("fullRumShare", "full"), c("fullRumTime", "fullRumShare"),
                  c("fullRumTime", "nearby"), c("fullRumTime", "nearbyRandom"),
                  c("fullRumShare", "nearby"), c("fullRumShare", "nearbyRandom"),
                  c("fullRumFixed", "full"), c("fullRumTime", "fullRumFixed"), c("fullRumShare", "fullRumFixed"),
                  c("fullRumFixed", "nearby"), c("fullRumFixed", "nearbyRandom"))
#Compare-only aliases: another setting's runs of a type (e.g. route alpha 0.5) copied
#under <type><suffix> into a folder that holds the type's runs at the first setting, so
#the paired bootstrap gives the difference between settings on the same seeds. Never
#simulated: a sim would run the folder's settings. Suffixes: route alpha 0.5 / 5 / 10;
#outings 1.25 / 1.5 times as long (D: the whole outing, so the reach too; W: only the
#time inside the areas - VFT_VAL_DURATION / VFT_VAL_DURSCOPE); IA the engine's outing
#(time inside the area, the engine's durations), WE the whole outing on the engine's
#durations (VFT_VAL_OUTING); L2 / L3 the DULN round the lakes doubled / tripled
#(data-raw/lake_emphasis_duln.R); ACT the DULN per activity, ST3 the stairs x3 for
#bikers alone (data-raw/tailor_duln_activities.R); E2 / E2C the activities on clean
#elements, unconstrained / signed (data-raw/tailor_duln_elements.R); S5 / SS the DULN
#fitted to the Strava heatmap, per group / shared (data-raw/tailor_duln_strava.R), XL
#today's xlsx DULN on the same areas.
ALIAS_SETTINGS <- c(A05 = "a0.5", A5 = "a5", A10 = "a10", D125 = "time x1.25", D150 = "time x1.5",
                    W125 = "walk x1.25", W150 = "walk x1.5", IA = "old outing", WE = "whole eng.dur",
                    L2 = "lakes x2", L3 = "lakes x3", ACT = "4 activities", ST3 = "stairs x3",
                    E2 = "clean elements", E2C = "clean el. signed", S5 = "Strava per group",
                    SS = "Strava shared", XL = "xlsx DULN", S5E = "Strava per group, clean",
                    SSE = "Strava shared, clean", S5EC = "Strava per group, clean signed")
for(sfx in names(ALIAS_SETTINGS)){
  for(t in c("full", "nearby", "nearbyRandom", "random"))
    TYPES[[paste0(t, sfx)]] <- utils::modifyList(TYPES[[t]], list(label = paste(TYPES[[t]]$label, ALIAS_SETTINGS[[sfx]])))
  CONTRASTS <- c(CONTRASTS, lapply(c("full", "nearby", "nearbyRandom", "random"), function(t) c(paste0(t, sfx), t)),
                 list(c(paste0("full", sfx), paste0("nearbyRandom", sfx)), c(paste0("full", sfx), paste0("nearby", sfx))))
}
#the Strava DULN against today's xlsx DULN, the survey fits (with stairs x3: fullST3;
#per activity: fullACT; on the clean elements: fullE2 / fullE2C) and each other
CONTRASTS <- c(CONTRASTS, list(c("fullS5", "fullXL"), c("fullSS", "fullXL"), c("fullSS", "fullST3"),
                               c("fullS5", "fullACT"), c("fullS5", "fullSS"), c("full", "fullXL"),
                               c("fullS5E", "fullXL"), c("fullSSE", "fullXL"), c("fullS5EC", "fullXL"),
                               c("fullS5E", "fullE2"), c("fullS5EC", "fullE2C"), c("fullSSE", "fullST3"),
                               c("fullS5E", "full"), c("fullS5E", "fullS5"), c("fullS5E", "fullSSE"),
                               c("fullS5EC", "fullS5E"), c("fullE2", "fullXL"), c("fullS5EC", "full")))
#the fitted area choices (fullRumFixed / fullRumTime) on the Strava routing (S5E, area
#choice re-fitted on its area DULN), compare-only like the other aliases
for(t in c("fullRumFixed", "fullRumTime"))
  TYPES[[paste0(t, "S5E")]] <- utils::modifyList(TYPES[[t]], list(label = paste(TYPES[[t]]$label, ALIAS_SETTINGS[["S5E"]])))
CONTRASTS <- c(CONTRASTS, list(c("fullRumFixedS5E", "fullRumFixed"), c("fullRumFixedS5E", "fullS5E"),
                               c("fullRumFixedS5E", "full"), c("fullRumTimeS5E", "fullRumTime"),
                               c("fullRumTimeS5E", "fullRumFixedS5E"), c("fullRumTimeS5E", "fullS5E"),
                               c("fullRumTimeS5E", "full"), c("fullRumFixedS5E", "nearby"),
                               c("fullRumTimeS5E", "nearby")))
#...and on the Strava routing with the area choice still reading today's area DULN (S5EA:
#Strava for the routing only, the user's solution 1; the coefficient file's areaDuln)
for(t in c("fullRumFixed", "fullRumTime"))
  TYPES[[paste0(t, "S5EA")]] <- utils::modifyList(TYPES[[t]], list(label = paste(TYPES[[t]]$label, "Strava routing only")))
CONTRASTS <- c(CONTRASTS, list(c("fullRumFixedS5EA", "fullRumFixed"), c("fullRumFixedS5EA", "fullS5E"),
                               c("fullRumFixedS5EA", "full"), c("fullRumFixedS5EA", "fullRumFixedS5E"),
                               c("fullRumTimeS5EA", "fullRumTime"), c("fullRumTimeS5EA", "fullRumFixedS5EA"),
                               c("fullRumTimeS5EA", "fullS5E"), c("fullRumTimeS5EA", "full"),
                               c("fullRumTimeS5EA", "fullRumTimeS5E")))
#the baseline performance is standardised against: (r - r_base) / (1 - r_base)
BASELINE <- "fullRandom"

TRACK_VERSION <- 2         #1 = priorE only (approach and return), 2 = + priorEAOI
RANDOM_REACH  <- 1/3       #Random: travel time to an area <= this share of the outing
BASE_SEED     <- 20261012L #replicate k runs on BASE_SEED + k in EVERY type (paired)
VISIT_MIN_M   <- 100       #a track visits an area with >= this many metres inside it
MIN_AOI_HA    <- 10        #areas smaller than this (inside the perimeter) skip the
                           #destination test
MIN_RESP_AOI  <- 10        #movement test: areas visited by >= this many respondents
MIN_AOI_CELLS <- 5         #...and covering >= this many cells
STABLE_TOL    <- 0.01      #replicate stability: running r within this of the final r
N_SPLITS      <- 200       #noise ceiling: random half splits
N_MC          <- 200       #Monte Carlo SE: draws per replicate count
DULN_NODE_COLS <- c("DULN_DOG_P", "DULN_BIKER", "DULN_DOG_N", "DULN_EBIKE",
                    "DULN_JOGGE", "DULN_WALK_", "DULN_WALK1")
#as in R/step4_server.R:48 (local there, so it is repeated here)
LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))

#### 1. setup ####

#A machine-wide PROJ_LIB (PostgreSQL's) shadows terra's PROJ data on this box and
#empties every terra crs. Set before anything spatial loads.
local({
  tp <- system.file("proj", package = "terra")
  if(nzchar(tp)) Sys.setenv(PROJ_LIB = tp)
})
#the working tree, as data-raw/verify_abm_speed.R does; daemons do not compile
suppressPackageStartupMessages(
  pkgload::load_all(REPO, export_all = TRUE, quiet = TRUE,
                    compile = if(IS_WORKER) FALSE else NA))
ns <- asNamespace("visitorFlowTool")
terra::terraOptions(progress = 0)
#never library(terra): the package imports nothing from it, and an attached terra
#hides the bugs that break the real app (memory: vft-terra-not-imported)

#the pristine engine functions the hooks wrap and restore
ORIG <- list(historyToPassage_cpp = ns$historyToPassage_cpp,
             findClosestAOI_cpp   = ns$findClosestAOI_cpp,
             chooseBestRoutes_cpp = ns$chooseBestRoutes_cpp,
             vftChoosePaths       = ns$vftChoosePaths,
             vftDbgCat            = ns$vftDbgCat,
             vftAgentTimeAndSpeed = ns$vftAgentTimeAndSpeed,
             vftTimeOver          = ns$vftTimeOver)

#the corrected engine's hooks (definitions only)
source(file.path(REPO, "data-raw", "engine_corrections.R"), local = TRUE)
#the fullCal type's parameters: VFT_VAL_CAL names an RDS list (CAL_DEFAULT's names);
#data-raw/calibrate_full.R sets CAL_PAR per task instead
CAL_PAR <- local({
  f <- Sys.getenv("VFT_VAL_CAL", "")
  p <- CAL_DEFAULT
  if(nzchar(f)) p[names(readRDS(f))] <- readRDS(f)
  p
})

GIT <- tryCatch({
  h <- system2("git", c("-C", REPO, "rev-parse", "--short", "HEAD"), stdout = TRUE, stderr = FALSE)
  d <- system2("git", c("-C", REPO, "status", "--porcelain", "--untracked-files=no"),
               stdout = TRUE, stderr = FALSE)
  paste0(h[1], if(length(d)) "+dirty" else "")
}, error = function(e) NA_character_, warning = function(w) NA_character_)

#### 2. helpers ####

logf <- function(fmt, ...){
  cat(format(Sys.time(), "%H:%M:%S "), sprintf(fmt, ...), "\n", sep = "")
  utils::flush.console()
}
fmtDur <- function(s){
  if(!isTRUE(is.finite(s))) return("?")
  sprintf("%dh%02dm", as.integer(s %/% 3600), as.integer((s %% 3600) %/% 60))
}
outPath <- function(...){
  p <- file.path(OUT, ...)
  dir.create(dirname(p[1]), recursive = TRUE, showWarnings = FALSE)
  p
}
#write to a side file, then rename: a killed run never leaves a half file behind
saveAtomic <- function(obj, path){
  tmp <- paste0(path, ".part")
  saveRDS(obj, tmp)
  if(file.exists(path)) unlink(path)
  if(!file.rename(tmp, path)) stop("could not move ", tmp, " to ", path)
  invisible(path)
}
todo <- function(stage, path) (stage %in% FORCE) || !file.exists(path)
aoiFile     <- function(ctx) outPath("aoi", paste0(ctx, ".rds"))
surveyFile  <- function(ctx) outPath("survey", paste0(ctx, ".rds"))
netFile     <- function(ctx) outPath("network", paste0(ctx, "_net.rds"))
edgesFile   <- function(ctx) outPath("network", paste0(ctx, "_edges.rds"))
metaFile    <- function(ctx) outPath("network", paste0(ctx, "_meta.rds"))
runFile     <- function(ctx, stamp, type, rep)
  outPath("runs", paste0(ctx, "_", stamp), type, sprintf("rep_%03d.rds", rep))
#is a downstream file still for the current areas?
current <- function(path, stamp) file.exists(path) && identical(readRDS(path)$stamp, stamp)

#Replace a function in the package namespace (and put it back later). The engine
#calls its helpers through the namespace, so this is what it sees.
nsSet <- function(name, value){
  locked <- bindingIsLocked(name, ns)
  if(locked) unlockBinding(name, ns)
  assign(name, value, envir = ns)
  if(locked) lockBinding(name, ns)
  invisible(NULL)
}

#Run expr with vftDbgCat() recorded instead of printed. generateAoI2() reports a
#failed split there and nowhere else - it then silently returns unsplit areas.
withDbgCapture <- function(expr){
  msgs <- character(0)
  nsSet("vftDbgCat", function(...){
    msgs <<- c(msgs, paste0(..., collapse = ""))
    invisible(NULL)
  })
  on.exit(nsSet("vftDbgCat", ORIG$vftDbgCat), add = TRUE)
  value <- force(expr)
  list(value = value, msgs = msgs)
}

#A progress handle that goes nowhere. launchSim() calls progress$set() unguarded.
fakeProgress <- function(){
  h <- function(...) invisible(NULL)
  list(set = h, inc = h, close = function() invisible(NULL))
}

#grids travel as numbers: wrap() of a value-less raster warns
gridSpec <- function(r) list(nrows = terra::nrow(r), ncols = terra::ncol(r),
                             ext = as.vector(terra::ext(r)), crs = "EPSG:2056")
gridFrom <- function(s) terra::rast(nrows = s$nrows, ncols = s$ncols,
                                    xmin = s$ext[["xmin"]], xmax = s$ext[["xmax"]],
                                    ymin = s$ext[["ymin"]], ymax = s$ext[["ymax"]],
                                    crs = s$crs)

#The stamp ties every downstream file to the areas it was made for. It is also
#part of the run directory, so new areas start fresh runs instead of mixing in
#replicates simulated on the old ones.
aoiStamp <- function(aoi) sprintf("t%.1f_n%d_ha%.0f", aoi$thr, nrow(aoi$polys),
                                  sum(aoi$polys$area) / 1e4)

perimeter2056 <- function(aoi)
  sf::st_union(sf::st_transform(sf::st_geometry(aoi$shape), 2056))

#The areas, clipped to the perimeter and dissolved by letter, in 2056. Used for
#the area visits and the per-area tests; ordered by letter.
clipAois <- function(aoi){
  a <- sf::st_make_valid(sf::st_transform(aoi$polys[, "AOI"], 2056))
  a <- suppressWarnings(sf::st_intersection(a, perimeter2056(aoi)))
  a <- a[!sf::st_is_empty(a), ]
  g <- lapply(split(sf::st_geometry(a), a$AOI), sf::st_union)
  a <- sf::st_sf(AOI = names(g), geometry = do.call(c, g))
  a$areaHa <- as.numeric(sf::st_area(a)) / 1e4
  a
}

#a value-less copy of grid with 1 in the cells the polygons keep - terra::mask()'s
#own semantics (touches = TRUE), as every old mask() call had
maskIndicator <- function(grid, vects){
  r <- terra::init(grid, 1)
  for(v in vects) r <- terra::mask(r, v)
  !is.na(terra::values(r, mat = FALSE))
}

#sparse sum-aggregation from a nr x nc grid to terra::aggregate(fact = f)'s grid
aggMatrix <- function(nr, nc, f){
  cell <- seq_len(nr * nc)
  r <- (cell - 1L) %/% nc
  cc <- (cell - 1L) %% nc
  ncc <- ceiling(nc / f)
  Matrix::sparseMatrix(i = (r %/% f) * ncc + cc %/% f + 1L, j = cell, x = 1,
                       dims = c(ceiling(nr / f) * ncc, nr * nc))
}

#Pearson and Spearman (Pearson of the ranks, ties averaged)
corBoth <- function(s, o){
  if(length(s) < 3 || !isTRUE(stats::sd(s) > 0) || !isTRUE(stats::sd(o) > 0))
    return(c(pearson = NA_real_, spearman = NA_real_))
  c(pearson = stats::cor(s, o), spearman = stats::cor(rank(s), rank(o)))
}
trans <- function(x, how) if(identical(how, "sqrt")) sqrt(x) else x
#a context's display name, and its view at the published scale, for a vector of ids
ctxLabel <- function(x) vapply(x, function(cx) CTX[[cx]]$label, "", USE.NAMES = FALSE)
oldView  <- function(x) vapply(x, function(cx) paste0(CTX[[cx]]$oldScale, "_all"), "",
                               USE.NAMES = FALSE)
std01 <- function(x){
  rg <- range(x, na.rm = TRUE)
  if(!all(is.finite(rg)) || rg[2] == rg[1]) return(x * 0)
  (x - rg[1]) / (rg[2] - rg[1])
}
#rbind for tables whose columns differ by context (GlattPark has no ceiling
#columns, for one): the missing columns are filled with NA
rbindFill <- function(lst){
  lst <- Filter(function(d) !is.null(d) && nrow(d), lst)
  if(!length(lst)) return(NULL)
  cols <- unique(unlist(lapply(lst, names)))
  out <- do.call(rbind, lapply(lst, function(d){
    for(cn in setdiff(cols, names(d))) d[[cn]] <- NA
    d[cols]
  }))
  rownames(out) <- NULL
  out
}
#### spatial agreement beyond r ####
#
#The survey records where respondents went, not everywhere people go: a cell it
#leaves empty may be unused or just unsampled. So, as in presence-only species
#distribution modelling, the scores below treat survey presences as reliable and
#its absences with care:
#  hit     share of the survey's usage inside the simulation's busiest cells, taking
#          as many cells as the survey uses (missed use lowers it)
#  auc     does the simulated intensity rank survey-used cells above unused ones
#          (Mann-Whitney AUC; correctly avoided cells raise it)
#  boyce   continuous Boyce index (Hirzel et al. 2006): does survey use rise
#          steadily from the simulation's quietest to its busiest cells (Spearman of
#          the observed/expected ratio over 10 equal-area classes; needs no absences)
#  empty   share of the simulated usage in cells the survey leaves empty
#  contra  the part of that where the emptiness is informative: a cell the
#          simulation says a share p of all outings pass, which a survey of n
#          respondents would still miss with probability (1 - p)^n < 0.05 - only
#          these absences contradict the simulation (Wigger and Glatt). At 500 m
#          the runs keep no per-agent tracks, so a block's p is its busiest 100 m
#          cell's: a lower bound on the share passing the block, which flags fewer
#          absences as informative than the true share would
#  top5    share of the usage in the busiest 5 % of cells (concentration)
#The survey against itself (half the respondents against the other half) is the
#reference for what a perfect model would score against a survey of that size.
aucScore <- function(score, presence){
  n1 <- sum(presence); n0 <- sum(!presence)
  if(!n1 || !n0) return(NA_real_)
  rk <- rank(score)
  (sum(rk[presence]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
boyceIndex <- function(score, weight, nbin = 10){
  br <- unique(stats::quantile(score, seq(0, 1, length.out = nbin + 1), names = FALSE))
  if(length(br) < 4 || sum(weight) <= 0) return(NA_real_)
  cl <- cut(score, br, include.lowest = TRUE)
  P <- tapply(weight, cl, sum) / sum(weight); E <- tapply(score, cl, length) / length(score)
  f <- as.vector(P / E); f[!is.finite(f)] <- NA
  if(sum(!is.na(f)) < 3) return(NA_real_)
  suppressWarnings(stats::cor(seq_along(f), f, method = "spearman", use = "complete.obs"))
}
#per row of an aggregation matrix (blocks x base cells), the largest `x` among its
#cells; 0 for a block without cells
blockMax <- function(A, x){
  T <- methods::as(A, "TsparseMatrix")
  out <- numeric(nrow(A))
  if(!length(T@i)) return(out)
  m <- tapply(x[T@j + 1L], T@i + 1L, max)
  out[as.integer(names(m))] <- m
  out
}
topShare <- function(x, p = 0.05){
  if(sum(x) <= 0) return(NA_real_)
  x <- sort(x, decreasing = TRUE); sum(x[seq_len(ceiling(p * length(x)))]) / sum(x)
}
#`sim` and `sur` on the same cells; `pOuting` (optional) the share of all simulated
#outings passing each cell and `nResp` the respondents, for `contra`
agreementScores <- function(sim, sur, pOuting = NULL, nResp = NA){
  used <- sur > 0
  n <- sum(used)
  hit <- if(n && sum(sur) > 0) sum(sur[order(sim, decreasing = TRUE)[seq_len(n)]]) / sum(sur) else NA_real_
  contra <- NA_real_
  if(!is.null(pOuting) && is.finite(nResp) && sum(sim) > 0){
    inform <- !used & (1 - pmin(pOuting, 1))^nResp < 0.05
    contra <- sum(sim[inform]) / sum(sim)
  }
  c(pearson = if(stats::sd(sim) > 0 && stats::sd(sur) > 0) stats::cor(sim, sur) else NA_real_,
    spearman = if(stats::sd(sim) > 0 && stats::sd(sur) > 0) stats::cor(sim, sur, method = "spearman") else NA_real_,
    hit = hit, auc = aucScore(sim, used), boyce = boyceIndex(sim, sur),
    empty = if(sum(sim) > 0) sum(sim[!used]) / sum(sim) else NA_real_, contra = contra,
    top5 = topShare(sim), top5_survey = topShare(sur))
}

jsd <- function(p, q){
  m  <- (p + q) / 2
  kl <- function(a, b){ i <- a > 0; sum(a[i] * log2(a[i] / b[i])) }
  0.5 * kl(p, m) + 0.5 * kl(q, m)
}

#The cells of the lakes inside the perimeter, as `survey[crop(lakes, perimeter)] <- 0`
#picked them in the old script (applied to a raster of ones, so the semantics of
#terra's `[<-` are kept whatever they are).
lakeCellsOf <- function(grid, perim){
  lk <- sf::st_read(ns$vftData("maps/lakes.gdb"), query = 'SELECT * FROM "lakes"',
                    wkt_filter = sf::st_as_text(perim), quiet = TRUE)
  if(!nrow(lk)) return(integer(0))
  g <- sf::st_transform(sf::st_make_valid(sf::st_zm(sf::st_geometry(lk))), 2056)
  g <- suppressWarnings(sf::st_intersection(g, perim))
  g <- g[!sf::st_is_empty(g)]
  if(!length(g)) return(integer(0))
  r <- terra::init(grid, 1)
  r[terra::vect(g)] <- 0
  which(terra::values(r, mat = FALSE) == 0)
}

#### 3. stage aoi: steps 1-4 as the app does them ####

#Step 1 on an uploaded file: lon/lat, combined, cast to polygons
#(R/step1_server.R:800). The Wigger KML is a closed LINE, which the old script
#turned into a polygon with terra::as.polygons(); closed here by hand.
readPerimeter <- function(file){
  x <- sf::st_zm(sf::st_read(ns$vftData(file), quiet = TRUE))
  g <- sf::st_geometry(x)
  if(all(sf::st_geometry_type(g) %in% c("LINESTRING", "MULTILINESTRING"))){
    rings <- sf::st_cast(g, "LINESTRING")
    g <- sf::st_sfc(lapply(rings, function(l){
      m <- unname(sf::st_coordinates(l)[, c("X", "Y"), drop = FALSE])
      if(any(m[1, ] != m[nrow(m), ])) m <- rbind(m, m[1, ])
      sf::st_polygon(list(m))
    }), crs = sf::st_crs(g))
  }
  g <- sf::st_make_valid(sf::st_union(sf::st_transform(g, 4326)))
  if(any(sf::st_geometry_type(g) == "GEOMETRYCOLLECTION"))
    g <- sf::st_collection_extract(g, "POLYGON")
  sf::st_sf(geometry = sf::st_cast(g, "POLYGON"))
}

stageAoi <- function(ctx){
  f <- aoiFile(ctx)
  if(!todo("aoi", f)) return(invisible(NULL))
  cfg <- CTX[[ctx]]
  t0 <- proc.time()[["elapsed"]]

  shape <- readPerimeter(cfg$perimeter)
  #R/providers.R:87 - buffered in 3857 metres, left in 3857
  shapeLarger <- sf::st_buffer(sf::st_transform(shape, "EPSG:3857"), dist = 1000)
  if(!inherits(shapeLarger, "sfc")) shapeLarger <- sf::st_as_sfc(shapeLarger)
  shapeLarger <- sf::st_union(shapeLarger)

  if(ctx %in% names(AOI_SAVES)) return(stageAoiFromSave(ctx, shape, shapeLarger, f, t0))

  #the two provider crops, R/providers.R:115-171
  DULN_all <- terra::crop(terra::rast(dulnAllFile()), sf::st_transform(shapeLarger, "epsg:4326"))
  #generateAoI2 masks DULN_all to the perimeter + 1 km: a replacement raster must cover it
  #(the crop above is shapeLarger, 1 km in 3857, which is narrower on the ground)
  if(nzchar(Sys.getenv("VFT_VAL_DULN_ALL"))){
    bufV <- terra::buffer(terra::vect(shape), 1000)
    cov <- terra::mask(terra::crop(terra::rast(dulnAllFile()), bufV, snap = "out"), bufV)
    inBuf <- terra::mask(terra::init(cov, 1), bufV)
    nMiss <- sum(terra::values(is.na(cov) & !is.na(inBuf), mat = FALSE), na.rm = TRUE)
    if(nMiss > 0) stop(ctx, ": ", basename(dulnAllFile()), " has ", nMiss, " NA cells inside the perimeter + 1 km")
  }
  DULN <- terra::crop(terra::rast(ns$vftData("maps/attr/allAttrs_COG_final.tif")),
                      terra::project(terra::vect(shapeLarger), "EPSG:4326"))
  names(DULN) <- c("jog", "dogNat", "ebikeNat", "walkNat", "dogProx", "walkSoc", "bikerSport")

  #step 3's default (R/step3_server.R:479, :500)
  shapeWgs <- sf::st_transform(sf::st_transform(shape, 3857), 4326)
  #VFT_VAL_THRESHOLD fixes it instead (e.g. 3.1, the user's own areas' threshold)
  thrFixed <- suppressWarnings(as.numeric(Sys.getenv("VFT_VAL_THRESHOLD", "")))
  thr <- if(isTRUE(is.finite(thrFixed))) thrFixed else ns$vftSuggestAoIThreshold(DULN_all, shapeWgs)
  if(is.null(thr)) stop(ctx, ": no threshold could be suggested")

  #the areas step 4 arrives with, for the record, then "Automatic Cuts"
  #(R/step4_server.R:293-350, within = NULL because nothing was edited)
  unsplit <- ns$generateAoI2(minThresh = thr, perimeter = shape, walkNat = DULN$walkNat,
                             DULN_all = DULN_all, split = FALSE)$polygons
  if(AOI_CUT){
    cut <- withDbgCapture(ns$generateAoI2(minThresh = thr, perimeter = shape,
                                          walkNat = DULN$walkNat, DULN_all = DULN_all,
                                          split = TRUE)$polygons)
    bad <- grep("WARNING aoi split", cut$msgs, value = TRUE)
    if(length(bad)) stop(ctx, ": the automatic cut fell back to unsplit areas: ", bad[1])
    polys <- cut$value
  }else polys <- unsplit
  if(!nrow(polys)) stop(ctx, ": no areas at threshold ", thr)
  polys$id  <- seq_len(nrow(polys))
  polys$AOI <- LETTERS702[seq_len(nrow(polys))]

  aoi <- list(ctx = ctx, shape = shape, shapeLarger = shapeLarger, thr = thr,
              polys = polys, nUnsplit = nrow(unsplit), nCut = nrow(polys),
              dulnAll = basename(dulnAllFile()), cut = AOI_CUT)
  aoi$stamp <- aoiStamp(aoi)
  saveAtomic(aoi, f)
  sf::st_write(polys, outPath("aoi", paste0(ctx, "_aois.gpkg")), delete_dsn = TRUE, quiet = TRUE)
  logf("aoi %s: %s, threshold %.2f, %d areas unsplit -> %d %s (%.0f ha), %.0f s",
       ctx, basename(dulnAllFile()), thr, nrow(unsplit), nrow(polys), if(AOI_CUT) "after the cut" else "(no cut)",
       sum(polys$area) / 1e4,
       proc.time()[["elapsed"]] - t0)
}

#The areas a user drew or edited in the app, from its save (.RData) instead of
#steps 3-4 here. Taken as saved: the app simulates them unclipped, ids and all.
stageAoiFromSave <- function(ctx, shape, shapeLarger, f, t0){
  src <- AOI_SAVES[[ctx]]
  if(!file.exists(src)) stop(ctx, ": no app save at ", src)
  e <- new.env()
  load(src, envir = e)
  polys <- e$envBase_finalPolygons
  thr   <- e$envBase_minThresh
  if(!inherits(polys, "sf") || !nrow(polys)) stop(ctx, ": ", src, " holds no envBase_finalPolygons")
  if(!isTRUE(is.finite(thr))) stop(ctx, ": ", src, " holds no envBase_minThresh")
  if(anyDuplicated(polys$AOI)) stop(ctx, ": duplicated AOI letters in ", src)
  polys$lakeLoop[is.na(polys$lakeLoop)] <- FALSE

  #the save's step-1 outline should be the perimeter the surveys are cut to
  sv <- sf::st_union(sf::st_make_valid(sf::st_transform(
    sf::st_set_crs(sf::st_zm(sf::st_geometry(e$envBase_shape)), 4326), 2056)))
  pm <- perimeter2056(list(shape = shape))
  dev <- as.numeric(sf::st_area(sf::st_sym_difference(sv, pm))) / max(as.numeric(sf::st_area(pm)), 1)
  if(dev > 0.01) logf("WARNING aoi %s: the save's outline differs from %s by %.1f %% of its area",
                      ctx, CTX[[ctx]]$perimeter, 100 * dev)
  #what the network (cut to the perimeter + 1 km) cannot reach
  u2056 <- sf::st_union(sf::st_make_valid(sf::st_transform(sf::st_geometry(polys), 2056)))
  outLarge <- as.numeric(sum(sf::st_area(sf::st_difference(
    u2056, sf::st_transform(shapeLarger, 2056))))) / 1e4

  aoi <- list(ctx = ctx, shape = shape, shapeLarger = shapeLarger, thr = thr,
              polys = polys, nUnsplit = NA_integer_, nCut = nrow(polys), source = src)
  aoi$stamp <- paste0(aoiStamp(aoi), "_save")
  saveAtomic(aoi, f)
  sf::st_write(polys, outPath("aoi", paste0(ctx, "_aois.gpkg")), delete_dsn = TRUE, quiet = TRUE)
  logf("aoi %s: from the save %s - threshold %.1f, %d areas (%.0f ha, %.0f ha beyond the network's 1 km margin), %.0f s",
       ctx, basename(src), thr, nrow(polys), sum(polys$area) / 1e4, outLarge,
       proc.time()[["elapsed"]] - t0)
}

#### 4. stage survey: rebuilt from the raw data, unclipped ####

#The route lines in 2056 with a respondent id each (the bootstrap's cluster).
#Wigger: CODE repeats across a respondent's lines. Glatt: the five activity
#exports with the car arrivals, as raster_all_withCar_100m was built from.
readSurveyLines <- function(kind){
  if(kind == "wigger"){
    x <- sf::st_zm(sf::st_read(PATHS$wiggerLines, quiet = TRUE))
    sf::st_sf(resp = paste0("w", x$CODE), layer = "wigger",
              geometry = sf::st_geometry(sf::st_transform(x, 2056)))
  }else if(identical(FILBLEU_SOURCE, "exports")){
    parts <- lapply(FILBLEU_LAYERS, function(ly){
      x <- sf::st_zm(sf::st_read(file.path(PATHS$filBleuDir, paste0("Copy_activity_", ly, ".shp")),
                                 quiet = TRUE))
      sf::st_sf(resp = paste0("f", x$respondent), layer = ly,
                geometry = sf::st_geometry(sf::st_transform(x, 2056)))
    })
    do.call(rbind, parts)
  }else{
    x <- sf::st_zm(sf::st_read(PATHS$filBleuAll, quiet = TRUE))
    x$layer <- filBleuLayerOf(x$um_welche_)
    x <- x[!is.na(x$layer) & !duplicated(x$geoansweri), ]
    #the motives travel along: data-raw/tailor_duln_types.R types the routes by them
    sf::st_sf(resp = paste0("f", x$respondent), layer = x$layer, id = x$geoansweri,
              nature = x$natur_erle, social = x$sozialer_a, access = x$wie_gelang,
              geometry = sf::st_geometry(sf::st_transform(x, 2056)))
  }
}

stageSurvey <- function(ctx){
  f <- surveyFile(ctx)
  aoi <- readRDS(aoiFile(ctx))
  if(!("survey" %in% FORCE) && current(f, aoi$stamp)) return(invisible(NULL))
  cfg <- CTX[[ctx]]
  perim  <- perimeter2056(aoi)
  perimV <- terra::vect(perim)

  L <- NULL; resp <- NULL; LV <- NULL; lvAoi <- NULL; lakes <- integer(0)
  if(identical(cfg$survey, "heatmap")){
    heat <- terra::rast(PATHS$glattParkHeat)
    base <- terra::rast(heat)
    #the sim's 100 m grid is the heatmap's cut in 5, so 100 m -> 500 m is exact
    t100 <- terra::disagg(base, 5)
    surveyBase <- terra::values(heat, mat = FALSE)
    surveyBase[is.na(surveyBase)] <- 0
  }else{
    #the old swisscom_5Oct_*_100m crop: the grid of every old comparison
    base <- terra::rast(terra::crop(terra::rast(PATHS$swisscom), perimV))
    t100 <- base
    lines <- readSurveyLines(cfg$survey)
    #one row per line: the cells whose centre lies within 50 m of it. Summed, this
    #is rasterize(buffer(lines, 50), fun = "count") - the old survey raster.
    lc <- terra::cells(base, terra::buffer(terra::vect(lines), cfg$buffer))
    #a route entirely outside the grid comes back as one row with cell NA
    lc <- lc[!is.na(lc[, "cell"]), , drop = FALSE]
    L <- Matrix::sparseMatrix(i = lc[, "ID"], j = lc[, "cell"], x = 1,
                              dims = c(nrow(lines), terra::ncell(base)))
    if(isTRUE(cfg$lakesToZero)){
      lakes <- lakeCellsOf(base, perim)
      if(length(lakes)){ L[, lakes] <- 0; L <- Matrix::drop0(L) }
    }
    surveyBase <- as.vector(Matrix::colSums(L))
    resp <- lines$resp

    #which areas each route visits (>= VISIT_MIN_M inside), for the destination test
    ac <- clipAois(aoi)
    ints <- suppressWarnings(sf::st_intersection(
      sf::st_sf(line = seq_len(nrow(lines)), geometry = sf::st_geometry(lines)), ac[, "AOI"]))
    LV <- Matrix::sparseMatrix(i = integer(0), j = integer(0), x = numeric(0),
                               dims = c(nrow(lines), nrow(ac)))
    if(nrow(ints)){
      len <- rowsum(as.numeric(sf::st_length(ints)),
                    paste(ints$line, match(ints$AOI, ac$AOI)))
      key <- do.call(rbind, strsplit(rownames(len), " ", fixed = TRUE))
      keep <- len[, 1] >= VISIT_MIN_M
      LV <- Matrix::sparseMatrix(i = as.integer(key[keep, 1]), j = as.integer(key[keep, 2]),
                                 x = 1, dims = c(nrow(lines), nrow(ac)))
    }
    lvAoi <- ac$AOI
  }

  #parity with the published survey raster, inside the cells it kept
  parity <- NA_real_
  old <- file.path(PATHS$oldDir, cfg$savedSurvey)
  if(file.exists(old)){
    saved <- terra::resample(terra::rast(old), base, method = "near")
    v <- terra::values(saved, mat = FALSE)
    ok <- !is.na(v)
    parity <- suppressWarnings(stats::cor(v[ok], surveyBase[ok]))
    logf("survey %s: r = %.4f with the published %s over its %d cells", ctx, parity,
         cfg$savedSurvey, sum(ok))
  }

  saveAtomic(list(ctx = ctx, stamp = aoi$stamp, base = gridSpec(base), t100 = gridSpec(t100),
                  surveyBase = surveyBase, L = L, resp = resp, LV = LV, lvAoi = lvAoi,
                  lakeCells = lakes, parity = parity), f)
  logf("survey %s: %s, total %.0f%s", ctx, cfg$survey, sum(surveyBase),
       if(is.null(L)) "" else sprintf(" (%d lines, %d respondents)", nrow(L), length(unique(resp))))
}

#### 5. stage network: the path network as step 5 loads it ####

stageNetwork <- function(ctx){
  aoi <- readRDS(aoiFile(ctx))
  if(!("network" %in% FORCE) && file.exists(netFile(ctx)) && current(edgesFile(ctx), aoi$stamp) &&
     current(metaFile(ctx), aoi$stamp)) return(invisible(NULL))
  cfg <- CTX[[ctx]]
  sv  <- readRDS(surveyFile(ctx))
  t0  <- proc.time()[["elapsed"]]

  #Agents start where the respondents live. sf_to_tidygraph3() reads its residents
  #raster from this global when it exists (R/sf_to_tidygraph3.R:261) and wants the
  #layer called Band_1 - the filBleu raster's is "sum".
  resR <- terra::rast(ns$vftData(cfg$residential))
  names(resR) <- "Band_1"
  assign(".vft_residential_full", resR, envir = .GlobalEnv)
  on.exit(if(exists(".vft_residential_full", envir = .GlobalEnv, inherits = FALSE))
    rm(".vft_residential_full", envir = .GlobalEnv), add = TRUE)

  #the network provider, R/providers.R:212-221
  wkt <- sf::st_as_text(sf::st_transform(aoi$shapeLarger, "epsg:4326"))
  paths <- sf::st_read(ns$vftData("maps/paths/paths_11_24_final_4.gdb"),
                       query = 'SELECT * FROM "paths_11_24_final_4"',
                       wkt_filter = wkt, promote_to_multi = FALSE, type = 2, quiet = TRUE)
  net <- ns$sf_to_tidygraph3(paths, aoi$shapeLarger, directed = FALSE)[[1]][[1]]
  rm(".vft_residential_full", envir = .GlobalEnv)
  rm(paths)
  invisible(gc())

  prep    <- ns$vftPrepareNetwork(net, aoi$polys, aoi$thr)
  net     <- prep$network
  parking <- prep$parking

  #how much of the respondents' raster the nodes caught (cells without a node lose
  #theirs) - and the agent count, undivided unless the context fixes it
  resCrop   <- terra::crop(resR, terra::project(terra::vect(aoi$shapeLarger), "epsg:4326"))
  rasterSum <- terra::global(resCrop, "sum", na.rm = TRUE)[[1]]
  nodeSum   <- sum(igraph::vertex_attr(net, "Residents"), na.rm = TRUE)
  nAgents   <- if(is.na(cfg$nAgents)) round(rasterSum) else cfg$nAgents
  nParking  <- sum(igraph::vertex_attr(net, "parking") > 0, na.rm = TRUE)

  saveAtomic(list(net = net, parking = parking), netFile(ctx))

  #### the edge tables the track summaries need ####
  et   <- ns$vftGraphTibble(net, "edges")
  gcol <- names(et)[vapply(et, inherits, logical(1), "sfc")][1]
  e2056 <- sf::st_transform(sf::st_zm(sf::st_sf(edgeID = et$edgeID, geometry = et[[gcol]])), 2056)
  ev   <- terra::vect(e2056)
  t100 <- gridFrom(sv$t100)
  #the usage metric: cells with their centre within the buffer (Wigger, Glatt), or
  #touched by the line itself (GlattPark, the old toBuffer = FALSE)
  ec <- if(cfg$buffer > 0) terra::cells(t100, terra::buffer(ev, cfg$buffer)) else terra::cells(t100, ev)
  ec <- ec[!is.na(ec[, "cell"]), , drop = FALSE]   #edges off the grid
  edgeCells <- list(edgeID = as.integer(e2056$edgeID[ec[, "ID"]]), cell = as.integer(ec[, "cell"]))
  #the path-cells-only cell set: touched by an edge, unbuffered
  pc <- terra::cells(t100, ev)
  pathCells100 <- sort(unique(as.integer(pc[!is.na(pc[, "cell"]), "cell"])))
  #metres of every edge inside every (clipped) area
  ac   <- clipAois(aoi)
  ints <- suppressWarnings(sf::st_intersection(e2056, ac[, "AOI"]))
  edgeAoi <- data.frame(edgeID = integer(0), AOI = character(0), len = numeric(0))
  if(nrow(ints)){
    len <- rowsum(as.numeric(sf::st_length(ints)), paste(ints$edgeID, ints$AOI))
    key <- do.call(rbind, strsplit(rownames(len), " ", fixed = TRUE))
    edgeAoi <- data.frame(edgeID = as.integer(key[, 1]), AOI = key[, 2], len = len[, 1])
  }
  saveAtomic(list(stamp = aoi$stamp, nEdges = nrow(et), n100 = terra::ncell(t100),
                  edgeCells = edgeCells, pathCells100 = pathCells100,
                  edgeAoi = edgeAoi, aoiLetters = ac$AOI, aoiHa = ac$areaHa), edgesFile(ctx))
  saveAtomic(list(stamp = aoi$stamp, nAgents = nAgents, rasterSum = rasterSum, nodeSum = nodeSum,
                  nNodes = igraph::vcount(net), nEdges = igraph::ecount(net),
                  nParkingNodes = nParking), metaFile(ctx))
  logf("network %s: %d nodes, %d edges; respondents %.0f in the raster, %.0f on nodes (%.1f %% lost); %d agents; %d parking nodes; %.0f s",
       ctx, igraph::vcount(net), igraph::ecount(net), rasterSum, nodeSum,
       100 * (1 - nodeSum / max(rasterSum, 1e-9)), nAgents, nParking,
       proc.time()[["elapsed"]] - t0)
}

#### 6. stage sim ####

#The network and areas of one type, from the prepared base (see the header).
#`share`: the share of the starts that go to car parks, where the type has them.
typeVariant <- function(net, polys, type, share = 0, thr = NULL){
  sp <- TYPES[[type]]
  parkShare <- 0
  #fullCal: the legs' weights ("") at strength CAL_PAR$f, the other two as prepared
  if(identical(sp$routing, "cal")){
    ea <- igraph::edge_attr(net)
    w  <- ns$vftAttrWeights(ea[["SHAPE_Leng"]], ea[ns$VFT_DULN_TYPE_COLS], thr,
                            factors = c(CAL_PAR$f, 4, 2))
    w  <- w[!grepl("_(attr|ATTR)$", names(w))]
    ea[names(w)] <- w
    cls <- class(net)
    igraph::edge_attr(net) <- ea
    igraph::graph_attr(net, "vftWeights") <- ns$VFT_WEIGHTS_VERSION
    class(net) <- cls
  }
  if(identical(sp$routing, "length")){
    len <- pmax(igraph::edge_attr(net, "SHAPE_Leng"), 10)
    for(nm in grep("^SHAPE_Leng_", igraph::edge_attr_names(net), value = TRUE))
      net <- igraph::set_edge_attr(net, nm, value = len)
    #The swap is deliberate: stamp the weights as current, or vftSimulateScenario()
    #recomputes them from DULN on any network prepared before the 2026-10-16 weight
    #fix (see vftWeightsCurrent() in R/prepare_network.R) and undoes it.
    igraph::graph_attr(net, "vftWeights") <- ns$VFT_WEIGHTS_VERSION
  }
  if(identical(sp$movement, "flat"))
    for(nm in DULN_NODE_COLS) net <- igraph::set_vertex_attr(net, nm, value = 0)
  #generatePopulation() draws the starts with weight Residents at homes and
  #parking * parkingAttr * 0.1 at car parks (0.1 fixed in vftSimulateScenario()).
  #The car parks are scaled so they carry `share` of that total; the attribute
  #stays even at 0, so vftNetworkPrepared() still says yes.
  pk <- igraph::vertex_attr(net, "parking")
  k  <- 0
  if(isTRUE(sp$parking) && share > 0){
    rs  <- igraph::vertex_attr(net, "Residents")
    pa  <- igraph::vertex_attr(net, "parkingAttr")
    okR <- rs > 0 & is.finite(rs)
    okP <- pk > 0 & is.finite(pk)
    sumR <- sum(rs[okR]); sumP <- sum(pk[okP] * pa[okP] * 0.1, na.rm = TRUE)
    if(sumP > 0){
      k <- share * sumR / ((1 - share) * sumP)
      parkShare <- k * sumP / (sumR + k * sumP)
    }else warning("no car parks to give ", share, " of the starts to")
  }
  net <- igraph::set_vertex_attr(net, "parking", value = pk * k)
  if(identical(sp$aoiChoice, "distance")){
    polys$DULN <- 1
    polys$area <- 1e6
  }
  list(net = net, polys = polys, parkShare = parkShare)
}

#edge id -> its rows in a table sorted by edge, for fast expansion
edgeIndex <- function(ids, vals, n){
  o <- order(ids)
  len <- tabulate(ids, nbins = n)
  list(start = cumsum(c(1L, len))[seq_len(n)], len = len, val = vals[o])
}
expandEdges <- function(ix, agent, edge){
  cnt  <- ix$len[edge]
  keep <- cnt > 0L
  agent <- agent[keep]; edge <- edge[keep]; cnt <- cnt[keep]
  pos <- rep.int(ix$start[edge], cnt) + sequence(cnt) - 1L
  list(agent = rep.int(agent, cnt), val = ix$val[pos])
}

#One cache per process (a daemon keeps it between its tasks). Holds ONE network:
#the type's variant. The prepared base is read from disk again for the next type
#(seconds) rather than kept beside it - memory is the scarce resource here.
.valCache <- new.env()
scenarioFor <- function(ctx, type){
  #fullCal's network depends on the legs' strength
  key <- paste(ctx, type, if(identical(TYPES[[type]]$routing, "cal")) CAL_PAR$f else "")
  if(identical(.valCache$key, key)) return(.valCache$sc)
  if(!identical(.valCache$ctx, ctx)){
    rm(list = ls(.valCache), envir = .valCache)
    invisible(gc())
    aoi  <- readRDS(aoiFile(ctx))
    meta <- readRDS(metaFile(ctx))
    edg  <- readRDS(edgesFile(ctx))
    if(!identical(meta$stamp, aoi$stamp) || !identical(edg$stamp, aoi$stamp))
      stop(ctx, ": the network was built for other areas - rerun with VFT_VAL_FORCE=network")
    .valCache$ctx  <- ctx
    .valCache$aoi  <- aoi
    .valCache$meta <- meta
    .valCache$ix   <- list(cells = edgeIndex(edg$edgeCells$edgeID, edg$edgeCells$cell, edg$nEdges),
                           aoi   = edgeIndex(edg$edgeAoi$edgeID, seq_len(nrow(edg$edgeAoi)), edg$nEdges),
                           edgeAoi = edg$edgeAoi, n100 = edg$n100, aoiLetters = edg$aoiLetters)
  }
  .valCache$sc <- NULL; .valCache$key <- NULL
  invisible(gc())
  base <- readRDS(netFile(ctx))
  v    <- typeVariant(base$net, .valCache$aoi$polys, type,
                      share = if(is.na(PARKING)) CTX[[ctx]]$parkingShare else PARKING,
                      thr = .valCache$aoi$thr)
  v$parking <- base$parking
  rm(base)
  res <- igraph::vertex_attr(v$net, "Residents")
  sc  <- list(ctx = ctx, type = type, spec = TYPES[[type]], net = v$net, polys = v$polys,
              thr = .valCache$aoi$thr, parking = v$parking,
              stamp = .valCache$aoi$stamp, ix = .valCache$ix,
              nAgents = .valCache$meta$nAgents, parkShare = v$parkShare,
              #so vftSimulateScenario()'s sum(Residents)/residentDivision is the count
              residentDivision = sum(res, na.rm = TRUE) / (.valCache$meta$nAgents + 1e-6),
              nodeID = igraph::vertex_attr(v$net, "nodeID"), residents = res)
  .valCache$key <- key
  .valCache$sc  <- sc
  sc
}

#The random area choices: uniform among the areas within `reach` of the outing's
#duration (Random: RANDOM_REACH; Full-Random: Inf, every area the agent reaches
#on the network). Distances are path lengths here (both route by length), in
#metres; speed km/h; duration minutes.
uniformWithinReach <- function(distances, speeds, durations, reach = RANDOM_REACH){
  Map(function(d, sp, du){
    if(!length(d)) return(numeric(0))
    ok <- (d / (sp * 1000)) <= reach * (du / 60)
    if(any(ok)) as.numeric(ok) / sum(ok) else numeric(length(d))
  }, distances, speeds, durations)
}

#One simulation with the hooks in place, the app's own entry point underneath.
simCore <- function(sc, seed, capture = TRUE){
  pp  <- fakeProgress()
  cap <- new.env(); cap$n <- 0L; cap$priorE <- NULL
  rnd <- new.env(); rnd$n <- 0L; rnd$walk <- 0L
  if(capture){
    nsSet("historyToPassage_cpp", function(agentTable, edgeTable, vertexTable){
      cap$n <- cap$n + 1L
      #The whole track: priorE holds the walk to the area and back home, the walk
      #INSIDE the areas is kept apart in priorEAOI (R/launchSim_v2.R ~1179;
      #historyToPassage_cpp() counts both). Until 2026-10-13 only priorE was taken,
      #so every map missed the in-area walking - TRACK_VERSION 2 has both.
      cap$priorE <- Map(function(a, b) c(a, b), agentTable$priorE, agentTable$priorEAOI)
      ORIG$historyToPassage_cpp(agentTable = agentTable, edgeTable = edgeTable,
                                vertexTable = vertexTable)
    })
    on.exit(nsSet("historyToPassage_cpp", ORIG$historyToPassage_cpp), add = TRUE)
  }
  #other durations (VFT_VAL_DURSRC): drawn again per type, in the engine's type order,
  #after the engine's own draw; longer outings (VFT_VAL_DURATION): the durations are
  #scaled after they are drawn, so the random stream is the unscaled run's. Under the
  #"walk" scope the area choices below divide them back (durChoiceDiv()).
  if(!is.null(DURATION) || !is.null(DURSRC)){
    if(identical(DURATION$scope, "walk") && (isTRUE(sc$spec$twoStage) || !identical(ENGINE_MODE, "corrected")))
      stop("VFT_VAL_DURSCOPE=walk needs the corrected engine and a single-stage type")
    if(!is.null(DURSRC) && (isTRUE(sc$spec$twoStage) || !identical(ENGINE_MODE, "corrected")))
      stop("VFT_VAL_DURSRC needs the corrected engine and a single-stage type")
    dfac <- if(is.null(DURATION)) 1 else DURATION$factor
    nsSet("vftAgentTimeAndSpeed", function(dayPop){
      dp <- ORIG$vftAgentTimeAndSpeed(dayPop)
      if(!is.null(DURSRC)){
        for(k in AGENT_TYPES){
          i <- which(dp$agentTyp == k)
          if(length(i)) dp$timeExt[i] <- DURSRC$durs[[k]][sample.int(length(DURSRC$durs[[k]]), length(i), replace = TRUE)]
        }
        rnd$durDrawn <- TRUE
      }
      if(!is.null(DURATION)){
        dp$timeExt <- dp$timeExt * dfac
        rnd$durScaled <- TRUE
      }
      dp
    })
    on.exit(nsSet("vftAgentTimeAndSpeed", ORIG$vftAgentTimeAndSpeed), add = TRUE)
  }
  #the whole outing (VFT_VAL_OUTING=whole): turn home when the time out plus the walk
  #back, taken as long as the walk there, reaches the duration
  if(identical(OUTING, "whole")){
    if(isTRUE(sc$spec$twoStage)) stop("VFT_VAL_OUTING=whole is for single-stage types only")
    nsSet("vftTimeOver", function(timeExt, timeNotAccounted, timer){
      rnd$outing <- TRUE
      timeExt <= timer + timeNotAccounted
    })
    on.exit(nsSet("vftTimeOver", ORIG$vftTimeOver), add = TRUE)
  }
  if((!is.null(sc$spec$rum) || !is.null(sc$spec$rumTime)) && !identical(ENGINE_MODE, "corrected"))
    stop("type ", sc$spec$label, " needs VFT_VAL_ENGINE=corrected")
  dDiv <- durChoiceDiv()
  if(sc$spec$aoiChoice %in% c("uniform", "uniformAll")){
    reach <- if(identical(sc$spec$aoiChoice, "uniformAll")) Inf else RANDOM_REACH
    nsSet("findClosestAOI_cpp", function(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds,
                                         agentDurations, AOI_aois, AOI_dulns, agentTyps, AOI_areas){
      agentDurations <- agentDurations / dDiv
      res <- ORIG$findClosestAOI_cpp(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds,
                                     agentDurations, AOI_aois, AOI_dulns, agentTyps, AOI_areas)
      rnd$n <- rnd$n + 1L
      res$prob <- uniformWithinReach(res$distances, agentSpeeds, agentDurations, reach)
      res
    })
    on.exit(nsSet("findClosestAOI_cpp", ORIG$findClosestAOI_cpp), add = TRUE)
  }
  #Full-Random's walk: every candidate next node equally likely. sample() in
  #launchSim() normalises the weights.
  if(identical(sc$spec$movement, "uniform")){
    nsSet("chooseBestRoutes_cpp", function(viableRoutes, DULN_df, priorVs, agentTyps,
                                           V_coords_df, currentVs, nodeRow_ptr){
      rnd$walk <- rnd$walk + 1L
      lapply(viableRoutes, function(r) rep(1, length(r)))
    })
    on.exit(nsSet("chooseBestRoutes_cpp", ORIG$chooseBestRoutes_cpp), add = TRUE)
  }
  #the corrected engine: the area choice of the engine-driven types, and the walk
  #of every type that walks with the engine's route choice
  .corrStats$fallback <- 0L
  .corrStats$pathFallback <- 0L
  #the calibrated model, whatever the engine mode
  if(identical(sc$spec$aoiChoice, "cal")){
    trueD <- trueAreaDistances(sc$ctx, sc$net)
    par <- CAL_PAR
    nsSet("findClosestAOI_cpp", function(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds,
                                         agentDurations, AOI_aois, AOI_dulns, agentTyps, AOI_areas){
      agentDurations <- agentDurations / dDiv
      res <- ORIG$findClosestAOI_cpp(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds,
                                     agentDurations, AOI_aois, AOI_dulns, agentTyps, AOI_areas)
      rnd$calAoi <- TRUE
      res$prob <- aoiProbsCal(res, src_v, agentSpeeds, agentDurations, AOI_aois, AOI_dulns,
                              AOI_areas, trueD, par)
      res
    })
    on.exit(nsSet("findClosestAOI_cpp", ORIG$findClosestAOI_cpp), add = TRUE)
  }
  if(identical(sc$spec$movement, "cal")){
    par <- CAL_PAR
    nsSet("chooseBestRoutes_cpp", function(viableRoutes, DULN_df, priorVs, agentTyps,
                                           V_coords_df, currentVs, nodeRow_ptr){
      rnd$calWalk <- TRUE
      chooseRoutesCal(viableRoutes, DULN_df, priorVs, agentTyps, V_coords_df, currentVs, par)
    })
    on.exit(nsSet("chooseBestRoutes_cpp", ORIG$chooseBestRoutes_cpp), add = TRUE)
  }
  if(identical(ENGINE_MODE, "corrected")){
    if(sc$spec$aoiChoice %in% c("engine", "distance")){
      trueD <- trueAreaDistances(sc$ctx, sc$net)
      nsSet("findClosestAOI_cpp", function(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds,
                                           agentDurations, AOI_aois, AOI_dulns, agentTyps, AOI_areas){
        agentDurations <- agentDurations / dDiv
        res <- ORIG$findClosestAOI_cpp(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds,
                                       agentDurations, AOI_aois, AOI_dulns, agentTyps, AOI_areas)
        td <- lapply(seq_along(res$prob), function(i)
          trueD[cbind(rep(src_v[i], length(res$aoi[[i]])), match(res$aoi[[i]], colnames(trueD)))])
        rnd$corrAoi <- TRUE
        if(!is.null(sc$spec$rumTime)){
          rnd$rum <- TRUE
          #the file's own area DULN, if it carries one (an area choice fitted on another
          #DULN than the routing's: Strava routing, the survey-fitted area DULN)
          aD <- RUM_TIME$areaDuln[[sc$ctx]]
          if(!is.null(aD)){
            AOI_dulns <- unname(aD[as.character(AOI_aois)])
            if(anyNA(AOI_dulns)) stop("VFT_VAL_RUM_TIME's areaDuln misses areas of ", sc$ctx)
          }
          res$prob <- aoiProbsTime(res, agentSpeeds, agentDurations, AOI_aois, AOI_dulns, AOI_areas,
                                   CORR$reach, td, rumTimeCoefs(sc$spec), RUM_TIME$consts, floorH = CORR$floorH,
                                   fallback = TRUE, fallbackCap = CORR$fallbackCap)
          return(res)
        }
        b <- if(is.null(sc$spec$rum)) list(bAttr = 3.23695, bDist = -6.65703) else rumCoefs(sc$spec, agentTyps)
        if(!is.null(sc$spec$rum)) rnd$rum <- TRUE
        res$prob <- aoiProbsR(res, agentSpeeds, agentDurations, AOI_aois, AOI_dulns, AOI_areas,
                              CORR$reach, td, floorH = CORR$floorH, fallback = TRUE,
                              fallbackCap = CORR$fallbackCap, bAttr = b$bAttr, bDist = b$bDist,
                              durRange = aoiDurRange())
        res
      })
      on.exit(nsSet("findClosestAOI_cpp", ORIG$findClosestAOI_cpp), add = TRUE)
    }
    if(sc$spec$movement %in% c("engine", "flat")){
      #the edge DULN junction choice, only where the walk reads DULN at all
      edgeTab <- if(identical(CORR$junction, "edge") && identical(sc$spec$movement, "engine")){
        if(is.null(sc$edgeTab)) sc$edgeTab <- edgeDulnTable(sc$net)
        .valCache$sc$edgeTab <- sc$edgeTab
        sc$edgeTab
      } else NULL
      nsSet("chooseBestRoutes_cpp", function(viableRoutes, DULN_df, priorVs, agentTyps,
                                             V_coords_df, currentVs, nodeRow_ptr){
        rnd$corrWalk <- TRUE
        chooseRoutesR(viableRoutes, DULN_df, priorVs, agentTyps, V_coords_df, currentVs, CORR$straight,
                      revisit = CORR$revisit, alpha = runAlpha(sc$spec), wQ = CORR$wQ, edgeTab = edgeTab)
      })
      on.exit(nsSet("chooseBestRoutes_cpp", ORIG$chooseBestRoutes_cpp), add = TRUE)
    }
    #a two-stage run's walk choice: the reach rule of the area choice above (real
    #path length, floor, capped fallback) for the entrances of the recorded walks
    if(isTRUE(sc$spec$twoStage)){
      if(is.null(sc$lenPtr)) sc$lenPtr <- lengthAdjPtr(sc$net)
      .valCache$sc$lenPtr <- sc$lenPtr
      nsSet("vftChoosePaths", function(agents, lib, adjPtr, params, attrWeight = 1){
        rnd$corrPaths <- TRUE
        choosePathsCorr(agents, lib, adjPtr, params, attrWeight, lenPtr = sc$lenPtr,
                        reach = CORR$reach, floorH = CORR$floorH, fallbackCap = CORR$fallbackCap)
      })
      on.exit(nsSet("vftChoosePaths", ORIG$vftChoosePaths), add = TRUE)
    }
  }
  set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  t0  <- proc.time()[["elapsed"]]
  #never a global `dayPop`: launchMultiSim() checks exists("dayPop") and would
  #simulate that instead. The engine's C++ prints debug lines through Rcout
  #(getEdges_cpp's "edgeID == 0", thousands per run); they go nowhere here.
  quiet <- file(nullfile(), open = "wt")
  sink(quiet)
  out <- tryCatch(ns$vftSimulateScenario(sc$net, sc$polys, sc$thr, parking = sc$parking,
                                         residentDivision = sc$residentDivision,
                                         progPrep = pp, progSim = pp,
                                         twoStage = if(isTRUE(sc$spec$twoStage))
                                           (if(is.null(sc$spec$twoStageParams)) TRUE else sc$spec$twoStageParams)),
                  finally = { sink(); close(quiet) })
  list(out = out, cap = cap, rnd = rnd, secs = proc.time()[["elapsed"]] - t0)
}

#Each agent's track -> the cells it counts in (once per agent and cell) and the
#areas it visits (>= VISIT_MIN_M of track inside).
summariseTracks <- function(priorE, ix){
  nE <- length(ix$cells$len)
  agent <- rep.int(seq_along(priorE), lengths(priorE))
  edge  <- suppressWarnings(as.integer(unlist(priorE, use.names = FALSE)))
  ok    <- !is.na(edge) & edge >= 1L & edge <= nE
  agent <- agent[ok]; edge <- edge[ok]
  u     <- !duplicated(agent * (nE + 1) + edge)
  agent <- agent[u]; edge <- edge[u]

  ce  <- expandEdges(ix$cells, agent, edge)
  cl  <- ce$val[!duplicated(ce$agent * (ix$n100 + 1) + ce$val)]
  tab <- tabulate(cl, nbins = ix$n100)
  nz  <- which(tab > 0L)

  nA <- length(ix$aoiLetters)
  visits <- integer(nA)
  ae <- expandEdges(ix$aoi, agent, edge)
  if(length(ae$val)){
    a   <- match(ix$edgeAoi$AOI[ae$val], ix$aoiLetters)
    len <- rowsum(ix$edgeAoi$len[ae$val], ae$agent * (nA + 1) + a)
    k   <- as.numeric(rownames(len))[len[, 1] >= VISIT_MIN_M]
    visits <- tabulate(as.integer(k %% (nA + 1)), nbins = nA)
  }
  list(cells = nz, counts = tab[nz], visits = visits)
}

#the switches' hooks (duration, outing, RUM) fired in a run that needs them
settingHooksFired <- function(sc, r){
  if(!is.null(DURATION) && !isTRUE(r$rnd$durScaled)) stop("the duration hook never fired")
  if(!is.null(DURSRC) && !isTRUE(r$rnd$durDrawn)) stop("the duration source hook never fired")
  if(identical(OUTING, "whole") && !isTRUE(r$rnd$outing)) stop("the whole-outing hook never fired")
  if((!is.null(sc$spec$rum) || !is.null(sc$spec$rumTime)) && !isTRUE(r$rnd$rum)) stop("the RUM area choice never fired")
}

#the run settings a type's runs must share in a comparison (an alias type may carry
#others than its base): outing durations, what they cover, their source, the car
#parks' share and the RUM coefficients
settingOf <- function(x) paste0(
  if(is.null(x$duration)) "x1" else paste0("x", x$duration$factor, " ", x$duration$scope),
  if(is.null(x$outing) || identical(x$outing, "area")) "" else paste0(", ", x$outing, " outing"),
  if(is.null(x$durSource)) "" else paste0(", durations ", basename(x$durSource$file), " ", substr(x$durSource$md5, 1, 8)),
  if(is.null(x$parking)) "" else paste0(", car parks ", x$parking),
  if(is.null(x$rum)) "" else paste0(", RUM ", x$rum$set, " ", substr(x$rum$md5, 1, 8)),
  if(is.null(x$rumTime)) "" else paste0(", RUM-time ", x$rumTime$set, " ", substr(x$rumTime$md5, 1, 8)))

#the route choice's alpha of a type's runs: the run's (CORR), or the type's own for
#its explorers (exploreAlpha, two-stage types)
runAlpha <- function(spec) if(is.null(spec$exploreAlpha)) CORR$alpha else spec$exploreAlpha

simTask <- function(ctx, type, rep){
  sc <- scenarioFor(ctx, type)
  f  <- runFile(ctx, sc$stamp, type, rep)
  if(file.exists(f) && !("sim" %in% FORCE)) return(list(status = "skipped"))
  seed <- BASE_SEED + rep
  r    <- simCore(sc, seed)
  if(r$cap$n != 1L) stop("the track capture fired ", r$cap$n, " times, expected once")
  if(sc$spec$aoiChoice %in% c("uniform", "uniformAll") && r$rnd$n != 1L)
    stop("the uniform area choice fired ", r$rnd$n, " times, expected once")
  if(identical(sc$spec$movement, "uniform") && r$rnd$walk < 1L)
    stop("the uniform walk never fired")
  if(identical(ENGINE_MODE, "corrected")){
    if(sc$spec$aoiChoice %in% c("engine", "distance") && !isTRUE(r$rnd$corrAoi))
      stop("the corrected area choice never fired")
    if(sc$spec$movement %in% c("engine", "flat") && !isTRUE(r$rnd$corrWalk))
      stop("the corrected walk never fired")
    if(isTRUE(sc$spec$twoStage) && !isTRUE(r$rnd$corrPaths))
      stop("the corrected walk choice of the two-stage run never fired")
  }
  if(isTRUE(sc$spec$twoStage) && is.null(r$out$results$twoStage))
    stop("the two-stage run returned no two-stage summary")
  settingHooksFired(sc, r)
  if(identical(sc$spec$aoiChoice, "cal") && !isTRUE(r$rnd$calAoi)) stop("the calibrated area choice never fired")
  if(identical(sc$spec$movement, "cal") && !isTRUE(r$rnd$calWalk)) stop("the calibrated walk never fired")
  dp <- r$out$results$dayPop
  tr <- summariseTracks(r$cap$priorE, sc$ix)
  startRes <- sc$residents[match(dp$startV, sc$nodeID)]
  #the shape of the walks: edges per track, and the share of an agent's edge
  #traversals that go over an edge it has already used (looping back)
  pe <- lapply(r$cap$priorE, function(e) suppressWarnings(as.integer(e)))
  pe <- pe[lengths(pe) > 0]
  walk <- c(trackEdges = if(length(pe)) mean(lengths(pe)) else NA_real_,
            revisitShare = if(length(pe)) 1 - sum(vapply(pe, function(e) length(unique(e)), 1)) / sum(lengths(pe))
                           else NA_real_)
  res <- list(ctx = ctx, type = type, rep = rep, seed = seed, stamp = sc$stamp, git = GIT,
              trackVersion = TRACK_VERSION,
              engine = ENGINE_MODE, revisit = if(identical(ENGINE_MODE, "corrected")) CORR$revisit else NULL,
              alpha = if(identical(ENGINE_MODE, "corrected")) runAlpha(sc$spec) else NULL,
              twoStageParams = sc$spec$twoStageParams,
              reach = if(identical(ENGINE_MODE, "corrected")) CORR$reach else NULL,
              wQ = if(identical(ENGINE_MODE, "corrected")) CORR$wQ else NULL,
              junction = if(identical(ENGINE_MODE, "corrected")) CORR$junction else NULL,
              cal = if(identical(sc$spec$aoiChoice, "cal")) CAL_PAR else NULL,
              duration = DURATION,
              outing = if(identical(OUTING, "area")) NULL else OUTING,
              durSource = if(is.null(DURSRC)) NULL else DURSRC[c("file", "md5")],
              parking = if(is.na(PARKING)) NULL else PARKING,
              rum = if(is.null(sc$spec$rum)) NULL else
                list(set = sc$spec$rum, file = RUM$file, md5 = RUM$md5, coefs = RUM[[sc$spec$rum]]),
              rumTime = if(is.null(sc$spec$rumTime)) NULL else
                list(set = sc$spec$rumTime, file = RUM_TIME$file, md5 = RUM_TIME$md5,
                     coefs = RUM_TIME$models[[sc$spec$rumTime]], consts = RUM_TIME$consts),
              #phase 2 of a two-stage run: the library, who found no walk, and who
              #took another duration's walk (fallback) or the closest one (pathFallback)
              twoStage = if(isTRUE(sc$spec$twoStage))
                c(r$out$results$twoStage[c("nExplorers", "nExplorerNoArea", "nPaths", "nNoPath", "nFallback", "secs")],
                  list(nPathFallback = .corrStats$pathFallback)),
              nFallback = .corrStats$fallback, walk = walk,
              secs = r$secs, nAgents = nrow(dp),
              nNoGoal = sum(dp$AOIGoal %in% "NO_GOAL" | dp$goalV %in% -1),
              nParkStart = sum(!(startRes > 0) | is.na(startRes)), parkShareTarget = sc$parkShare,
              goals = table(dp$AOIGoal),
              cells = tr$cells, counts = tr$counts, visits = tr$visits,
              passage = as.integer(round(igraph::edge_attr(r$out$results$pathUsage, "passage"))))
  saveAtomic(res, f)
  list(status = "done", secs = r$secs, nAgents = res$nAgents, nNoGoal = res$nNoGoal)
}

#One replicate of a type, returned instead of written: data-raw/calibrate_full.R's
#search, which tries many fullCal parameter sets on the same seeds. `par` replaces
#CAL_PAR for this call (and the cached network when its legs' strength differs).
#`corr` overrides entries of CORR (the corrected engine: reach, alpha, straight...) for
#this and later calls in the same process.
simMemory <- function(ctx, type, rep, par = NULL, corr = NULL){
  if(!is.null(par)){ p <- CAL_DEFAULT; p[names(par)] <- par; CAL_PAR <<- p }
  if(!is.null(corr)) CORR[names(corr)] <<- corr
  sc <- scenarioFor(ctx, type)
  r  <- simCore(sc, BASE_SEED + rep)
  if(r$cap$n != 1L) stop("the track capture fired ", r$cap$n, " times, expected once")
  settingHooksFired(sc, r)
  dp <- r$out$results$dayPop
  tr <- summariseTracks(r$cap$priorE, sc$ix)
  list(status = "done", ctx = ctx, type = type, rep = rep, secs = r$secs, nAgents = nrow(dp),
       nNoGoal = sum(dp$AOIGoal %in% "NO_GOAL" | dp$goalV %in% -1), nFallback = .corrStats$fallback,
       goals = table(dp$AOIGoal), cells = tr$cells, counts = tr$counts, visits = tr$visits)
}

#The headline Pearson r of a set of replicates (simMemory() results or run files), as
#the compare stage computes it: 100 m and 500 m, GlattPark 500 m only. For the
#searches and sweeps; data-raw/calibrate_full.R checks it against compare.rds.
HEAD_VIEWS <- function(ctx) if(ctx == "glattPark") "1_all" else c("1_all", "5_all")
scorerFor <- function(ctx){
  cfg <- CTX[[ctx]]
  aoi <- readRDS(aoiFile(ctx)); sv <- readRDS(surveyFile(ctx)); edg <- readRDS(edgesFile(ctx))
  G  <- buildGrid(ctx, aoi, sv, edg)
  vn <- HEAD_VIEWS(ctx)
  surV <- lapply(stats::setNames(vn, vn), function(v) surveyView(G, G$views[[v]], sv$surveyBase))
  n100 <- terra::ncell(G$t100)
  lab  <- stats::setNames(paste0("r", vapply(vn, function(v) G$views[[v]]$scale_m, 1)), vn)
  function(runs){
    x <- numeric(n100)
    for(r in runs) x[r$cells] <- x[r$cells] + r$counts
    r <- vapply(vn, function(v)
      stats::cor(trans(as.vector(G$views[[v]]$Msim %*% x), cfg$simTransform), surV[[v]]), 1)
    stats::setNames(r, lab[vn])
  }
}
runsFrom <- function(ctx, type, reps){
  aoi <- readRDS(aoiFile(ctx))
  lapply(reps, function(k) readRDS(runFile(ctx, aoi$stamp, type, k)))
}

#The capture hook must change nothing: the same seed with and without it.
stageSelftest <- function(){
  ctx <- if("glattPark" %in% CONTEXTS) "glattPark" else CONTEXTS[1]
  sc  <- scenarioFor(ctx, "full")
  #fingerprints only, so two results are never held at once
  fp <- function(r){
    ea <- igraph::edge_attr(r$out$results$pathUsage)
    list(passage = ea[grep("^passage", names(ea))], startV = r$out$results$dayPop$startV,
         fired = r$cap$n, secs = r$secs)
  }
  a <- fp(simCore(sc, BASE_SEED, capture = TRUE));  invisible(gc())
  b <- fp(simCore(sc, BASE_SEED, capture = FALSE)); invisible(gc())
  same <- identical(a$passage, b$passage) && identical(a$startV, b$startV)
  logf("selftest %s: capture hook fired %d time(s); results %s (%.0f s + %.0f s)",
       ctx, a$fired, if(same) "IDENTICAL" else "DIFFERENT", a$secs, b$secs)
  if(!same) stop("the capture hook changed the simulation - do not trust the runs")
  if(a$fired != 1L) stop("the capture hook did not fire exactly once")
}

stageSim <- function(){
  g <- expand.grid(rep = seq_len(N_REPS), type = TYPES_RUN, ctx = CONTEXTS,
                   stringsAsFactors = FALSE)
  g <- g[order(match(g$ctx, CONTEXTS), match(g$type, TYPES_RUN), g$rep), ]
  stamps <- vapply(CONTEXTS, function(cx) readRDS(aoiFile(cx))$stamp, "")
  have <- file.exists(mapply(runFile, g$ctx, stamps[g$ctx], g$type, g$rep))
  g <- if("sim" %in% FORCE) g else g[!have, ]
  if(!nrow(g)){ logf("sim: nothing to do"); return(invisible(NULL)) }
  logf("sim: %d run(s) to do on %d worker(s)", nrow(g), WORKERS)

  report <- function(k, res, secs){
    lbl <- sprintf("[%d/%d] %s %s rep %d", k, nrow(g), g$ctx[k], g$type[k], g$rep[k])
    if(identical(res$status, "done")){
      eta <- mean(utils::tail(secs, 20)) * (nrow(g) - k) / max(1, WORKERS)
      logf("%s: %.0f s, %d agents (%d without an area), ETA %s", lbl, res$secs,
           res$nAgents, res$nNoGoal, fmtDur(eta))
    }else logf("%s: %s", lbl, if(is.null(res$msg)) res$status else paste("FAILED -", res$msg))
  }

  secs <- numeric(0)
  if(WORKERS <= 1){
    for(k in seq_len(nrow(g))){
      res <- tryCatch(simTask(g$ctx[k], g$type[k], g$rep[k]),
                      error = function(e) list(status = "failed", msg = conditionMessage(e)))
      if(identical(res$status, "done")) secs <- c(secs, res$secs)
      report(k, res, secs)
    }
  }else{
    mirai::daemons(WORKERS)
    on.exit(mirai::daemons(0), add = TRUE)
    #everywhere(), not per-task setup: a daemon's cleanup wipes globals a TASK
    #adds, but keeps what everywhere() created (memory: vft-heat-load-speed)
    w <- mirai::everywhere({
      .libPaths(libs)
      Sys.setenv(VFT_VAL_WORKER = "1")
      source(script, local = globalenv())
      Sys.getpid()
    }, libs = .libPaths(), script = SCRIPT)
    mirai::call_mirai(w)
    ms <- lapply(seq_len(nrow(g)), function(k)
      mirai::mirai(tryCatch(simTask(ctx, type, rep),
                            error = function(e) list(status = "failed", msg = conditionMessage(e))),
                   ctx = g$ctx[k], type = g$type[k], rep = g$rep[k]))
    for(k in seq_along(ms)){
      res <- mirai::call_mirai(ms[[k]])$data
      if(mirai::is_error_value(res)) res <- list(status = "failed", msg = format(res))
      if(identical(res$status, "done")) secs <- c(secs, res$secs)
      report(k, res, secs)
    }
  }
  rm(list = ls(.valCache), envir = .valCache)
  invisible(gc())
}

#### 7. stage compare ####

#Everything a context's comparisons share: the masks of the old recipe, one view
#per scale x cell set, and the cells of each area.
#
#A VIEW takes a map straight to the values the old recipe correlates:
#  mask (areas, perimeter) -> aggregate x f (mean of the cells left, see the
#  header) -> NA to 0 -> mask again
#For the sim the cells left are fixed, so that is one sparse matrix (Msim) from the
#100 m counts. For the survey they can depend on the values (Glatt's zeros), so
#surveyView() divides two products of the block matrix A instead. Either way a
#bootstrap draw costs a few matrix products. The literal terra pipeline is run once
#on the final maps to check both (|dr| < 1e-6).
buildGrid <- function(ctx, aoi, sv, edg){
  cfg  <- CTX[[ctx]]
  t100 <- gridFrom(sv$t100); base <- gridFrom(sv$base)
  perimV <- terra::vect(perimeter2056(aoi))
  aoiV   <- terra::vect(sf::st_transform(sf::st_geometry(aoi$polys), 2056))
  n100 <- terra::ncell(t100); nBase <- terra::ncell(base)
  m100 <- maskIndicator(t100, list(aoiV, perimV))
  same <- n100 == nBase
  if(same){
    P <- Matrix::Diagonal(x = as.numeric(m100))
    mBase <- m100
  }else{
    A0 <- aggMatrix(terra::nrow(t100), terra::ncol(t100), 5)
    stopifnot(nrow(A0) == nBase)
    P <- A0 %*% Matrix::Diagonal(x = as.numeric(m100))
    mBase <- maskIndicator(base, list(aoiV, perimV))
  }
  path100 <- logical(n100); path100[edg$pathCells100] <- TRUE
  path100 <- path100 & m100
  pathBase <- if(same) path100 else as.vector(A0 %*% as.numeric(path100)) > 0

  views <- list()
  for(f in cfg$scales){
    if(f == 1){
      Af <- Matrix::Diagonal(nBase); gridF <- base
    }else{
      Af <- aggMatrix(terra::nrow(base), terra::ncol(base), f)
      gridF <- terra::aggregate(terra::init(base, 1), f)
      stopifnot(terra::ncell(gridF) == nrow(Af))
    }
    mF    <- maskIndicator(gridF, list(perimV, aoiV))
    pathF <- as.vector(Af %*% as.numeric(pathBase)) > 0
    #the mean over the block's cells inside the mask; a block with none is NA,
    #which the recipe then sets to 0 - a zero row
    nIn   <- as.vector(Af %*% as.numeric(mBase))
    Mf    <- Matrix::Diagonal(x = ifelse(nIn > 0, 1 / nIn, 0)) %*% Af %*%
      Matrix::Diagonal(x = as.numeric(mBase))
    for(cs in c("all", "path")){
      idx <- which(mF & (cs == "all" | pathF))
      views[[paste0(f, "_", cs)]] <- list(f = f, scale_m = terra::res(base)[1] * f, cellset = cs,
                                          idx = idx, grid = gridSpec(gridF),
                                          A = Af[idx, , drop = FALSE],
                                          Msim = Mf[idx, , drop = FALSE] %*% P)
    }
  }
  if(!is.null(cfg$dropCells)){
    v  <- views[["1_all"]]
    dc <- terra::cellFromRowCol(base, cfg$dropCells[, "row"], cfg$dropCells[, "col"])
    xy <- terra::xyFromCell(base, dc)
    logf("compare %s: 'nocentre' drops cells at %s", ctx,
         paste(sprintf("(%.0f, %.0f)", xy[, 1], xy[, 2]), collapse = " "))
    keep <- !(v$idx %in% dc)
    views[["1_nocentre"]] <- list(f = 1, scale_m = v$scale_m, cellset = "nocentre",
                                  idx = v$idx[keep], grid = v$grid,
                                  A = v$A[keep, , drop = FALSE],
                                  Msim = v$Msim[keep, , drop = FALSE])
  }

  #the survey cells the old means ran over: the masked cells (Glatt: only those
  #with usage), plus every lake cell, which the old script set to 0 after masking
  lakeBase <- logical(nBase); lakeBase[sv$lakeCells] <- TRUE
  surValid <- function(surBase)
    (mBase & (!isTRUE(cfg$surveyZerosNA) | surBase != 0)) | lakeBase

  #the comparison cells (base grid) of each area, by centre
  ac <- clipAois(aoi)
  stopifnot(identical(ac$AOI, edg$aoiLetters))
  cm <- terra::cells(base, terra::vect(ac))
  #an area (partly) off the survey grid gives NA cells (GlattPark's heatmap is
  #smaller than its perimeter)
  cm <- cm[!is.na(cm[, "cell"]), , drop = FALSE]
  cm <- cm[mBase[cm[, "cell"]], , drop = FALSE]
  aoiCells <- lapply(seq_len(nrow(ac)), function(i) cm[cm[, "ID"] == i, "cell"])
  names(aoiCells) <- ac$AOI

  list(views = views, P = P, m100 = m100, mBase = mBase, aoiCells = aoiCells, ac = ac,
       t100 = t100, base = base, aoiV = aoiV, perimV = perimV, surValid = surValid,
       lakeCells = sv$lakeCells)
}

#a survey map (on its own grid, lakes at 0) through one view
surveyView <- function(G, v, surBase){
  ok  <- as.numeric(G$surValid(surBase))
  num <- as.vector(v$A %*% (surBase * ok))
  den <- as.vector(v$A %*% ok)
  ifelse(den > 0, num / den, 0)
}

#The old recipe with terra, literally, on one pair of final maps.
terraOldRecipe <- function(cfg, G, sim100, surveyBase, f){
  sim <- terra::rast(G$t100); terra::values(sim) <- as.numeric(sim100)
  sim <- terra::mask(terra::mask(sim, G$aoiV), G$perimV)
  sur <- terra::rast(G$base); terra::values(sur) <- surveyBase
  if(identical(cfg$survey, "heatmap")){
    sim <- terra::resample(sim, sur, method = "sum")
  }else{
    sur <- terra::mask(terra::mask(sur, G$aoiV), G$perimV)
    #raster_all_withCar_100m[raster_all_withCar_100m == 0] <- NA, then the lakes
    if(isTRUE(cfg$surveyZerosNA)) sur[sur == 0] <- NA
    if(length(G$lakeCells)) sur[G$lakeCells] <- 0
  }
  if(f > 1){
    #fun = "mean": what the old `method = "sum"` call actually ran (see the header)
    sim <- terra::aggregate(sim, f, fun = "mean", na.rm = TRUE)
    sur <- terra::aggregate(sur, f, fun = "mean", na.rm = TRUE)
  }
  sim[is.na(sim)] <- 0; sur[is.na(sur)] <- 0
  sim <- terra::mask(terra::mask(sim, G$perimV), G$aoiV)
  sur <- terra::mask(terra::mask(sur, G$perimV), G$aoiV)
  s <- trans(terra::values(sim, mat = FALSE), cfg$simTransform)
  o <- terra::values(sur, mat = FALSE)
  ok <- which(!is.na(s) & !is.na(o))
  list(cells = ok, sim = s[ok], survey = o[ok])
}

#within-area movement: both maps relative to each area's own mean (removes the
#destination effect and the area size), pooled; plus per-area correlations
movementStats <- function(simBase, surBase, aoiCells, qual, how){
  ps <- po <- numeric(0); per <- wts <- numeric(0)
  for(a in qual){
    cl <- aoiCells[[a]]
    s <- trans(simBase[cl], how); o <- surBase[cl]
    ms <- mean(s); mo <- mean(o)
    if(!(ms > 0) || !(mo > 0)) next
    ps <- c(ps, s / ms); po <- c(po, o / mo)
    if(isTRUE(stats::sd(s) > 0) && isTRUE(stats::sd(o) > 0)){
      per <- c(per, stats::cor(s, o)); wts <- c(wts, sum(o))
    }
  }
  pooled <- corBoth(ps, po)
  c(`move|pooled|pearson` = pooled[["pearson"]], `move|pooled|spearman` = pooled[["spearman"]],
    `move|perArea|pearson` = if(length(per)) stats::weighted.mean(per, wts) else NA_real_)
}

#destination: the distribution of visits (or usage mass) over the areas
destStats <- function(simV, surV, prefix){
  if(sum(simV) <= 0 || sum(surV) <= 0) return(stats::setNames(rep(NA_real_, 3),
    paste0(prefix, c("|pearson", "|spearman", "|jsd"))))
  p <- simV / sum(simV); q <- surV / sum(surV)
  r <- corBoth(p, q)
  stats::setNames(c(r[["pearson"]], r[["spearman"]], jsd(p, q)),
                  paste0(prefix, c("|pearson", "|spearman", "|jsd")))
}

summ <- function(x){
  bt <- x[-1]; bt <- bt[is.finite(bt)]
  c(point = x[1], boot_mean = if(length(bt)) mean(bt) else NA_real_,
    boot_sd = if(length(bt) > 1) stats::sd(bt) else NA_real_,
    ci_lo = if(length(bt)) unname(stats::quantile(bt, 0.025)) else NA_real_,
    ci_hi = if(length(bt)) unname(stats::quantile(bt, 0.975)) else NA_real_)
}

compareContext <- function(ctx){
  cfg <- CTX[[ctx]]
  aoi <- readRDS(aoiFile(ctx)); sv <- readRDS(surveyFile(ctx))
  edg <- readRDS(edgesFile(ctx)); meta <- readRDS(metaFile(ctx))
  for(x in list(sv, edg, meta)) if(!identical(x$stamp, aoi$stamp))
    stop(ctx, ": survey or network made for other areas - rerun those stages")
  types <- intersect(names(TYPES), TYPES_RUN)

  #only the replicates every type has: the design is paired
  avail <- lapply(types, function(t){
    k <- seq_len(N_REPS)
    k[file.exists(runFile(ctx, aoi$stamp, t, k))]
  })
  reps <- Reduce(intersect, avail)
  if(length(reps) < 2){
    logf("compare %s: fewer than 2 complete replicates - skipped", ctx)
    return(NULL)
  }
  N <- length(reps)
  logf("compare %s: %d replicate(s) per type, %d type(s)", ctx, N, length(types))

  G <- buildGrid(ctx, aoi, sv, edg)
  views <- G$views
  n100 <- terra::ncell(G$t100)
  nA <- length(edg$aoiLetters)

  if(!is.null(sv$L)) stopifnot(identical(sv$lvAoi, edg$aoiLetters))
  runs <- lapply(stats::setNames(types, types), function(t)
    lapply(reps, function(i) readRDS(runFile(ctx, aoi$stamp, t, i))))
  tv <- unique(unlist(lapply(runs, function(rr) vapply(rr, function(x)
    if(is.null(x$trackVersion)) 1 else x$trackVersion, 1))))
  if(!identical(tv, TRACK_VERSION))
    stop(ctx, ": runs with track version(s) ", paste(tv, collapse = "/"), " - version 1 runs miss the ",
         "in-area walking (priorEAOI); re-simulate them in a fresh VFT_VAL_OUT")
  engines <- unique(unlist(lapply(runs, function(rr) vapply(rr, function(x)
    if(is.null(x$engine)) "engine" else x$engine, ""))))
  if(!identical(engines, ENGINE_MODE))
    stop(ctx, ": runs of engine(s) ", paste(engines, collapse = "/"), " under VFT_VAL_ENGINE=",
         ENGINE_MODE, " - use the matching setting or a fresh VFT_VAL_OUT")
  #one setting per type (an alias type may carry another than its base)
  for(t in types){
    d <- unique(vapply(runs[[t]], settingOf, ""))
    if(length(d) != 1) stop(ctx, ": the runs of ", t, " mix settings: ", paste(d, collapse = " | "))
    if(d != "x1") logf("compare %s: %s runs on %s", ctx, TYPES[[t]]$label, d)
  }
  S <- lapply(runs, function(rr) Matrix::sparseMatrix(
    i = unlist(lapply(rr, `[[`, "cells")),
    j = rep.int(seq_along(rr), vapply(rr, function(x) length(x$cells), 1L)),
    x = as.numeric(unlist(lapply(rr, `[[`, "counts"))), dims = c(n100, N)))
  Vr <- lapply(runs, function(rr) matrix(as.numeric(unlist(lapply(rr, `[[`, "visits"))), nrow = nA))
  runSummary <- do.call(rbind, lapply(types, function(t) data.frame(
    context = ctx, type = TYPES[[t]]$label, reps = N,
    secs_mean = mean(vapply(runs[[t]], function(x) as.numeric(x$secs), 1)),
    agents_mean = mean(vapply(runs[[t]], function(x) as.numeric(x$nAgents), 1)),
    share_without_area = mean(vapply(runs[[t]], function(x) x$nNoGoal / x$nAgents, 1)),
    share_parking_starts = mean(vapply(runs[[t]], function(x) x$nParkStart / x$nAgents, 1)),
    share_parking_target = {x <- runs[[t]][[1]]$parkShareTarget; if(is.null(x)) NA_real_ else x},
    git = runs[[t]][[1]]$git)))

  surV0 <- lapply(views, function(v) surveyView(G, v, sv$surveyBase))
  hasL  <- !is.null(sv$L)
  respIdx <- if(hasL) match(sv$resp, unique(sv$resp)) else NULL
  nResp   <- if(hasL) length(unique(sv$resp)) else 0L

  #### the literal check: matrix views against terra, on the full maps ####
  #The VALUES are compared, cell by cell, not the correlations: terra's
  #resample(method = "sum") goes through GDAL in floating point (~1e-11 off), which
  #is enough to break exact ties and move a Spearman r in the 3rd decimal. The
  #matrix sums are exact; the published GlattPark numbers carried that noise.
  for(t in types){
    sim100 <- Matrix::rowSums(S[[t]])
    for(f in unique(c(1, cfg$oldScale))){
      vn <- paste0(f, "_all")
      a  <- trans(as.vector(views[[vn]]$Msim %*% sim100), cfg$simTransform)
      b  <- terraOldRecipe(cfg, G, sim100, sv$surveyBase, f)
      rel <- function(x, y) max(abs(x - y)) / max(1, abs(y))
      if(!identical(as.integer(b$cells), as.integer(views[[vn]]$idx)) ||
         rel(a, b$sim) > 1e-6 || rel(surV0[[vn]], b$survey) > 1e-6)
        stop(sprintf("compare %s %s x%d: the matrix pipeline and terra's old recipe disagree (%d vs %d cells, sim %.2g, survey %.2g)",
                     ctx, t, f, length(views[[vn]]$idx), length(b$cells),
                     if(length(a) == length(b$sim)) rel(a, b$sim) else NA,
                     if(length(a) == length(b$sim)) rel(surV0[[vn]], b$survey) else NA))
    }
  }
  logf("compare %s: the matrix pipeline matches terra's old recipe cell by cell", ctx)

  #### replicate stability: running r, and the Monte Carlo SE ####
  set.seed(BASE_SEED + 3L)
  headline <- unique(c(paste0(cfg$oldScale, "_all"), "1_all"))
  conv <- list(); mcse <- list()
  nGrid <- unique(pmax(2L, round(seq(2, N, length.out = min(N - 1, 20)))))
  for(t in types){
    x <- numeric(n100)
    for(i in seq_len(N)){
      x <- x + as.vector(S[[t]][, i])
      for(vn in names(views)){
        r <- corBoth(trans(as.vector(views[[vn]]$Msim %*% x), cfg$simTransform), surV0[[vn]])
        conv[[length(conv) + 1]] <- data.frame(context = ctx, type = TYPES[[t]]$label, view = vn,
                                               scale_m = views[[vn]]$scale_m, cellset = views[[vn]]$cellset,
                                               n = i, pearson = r[["pearson"]], spearman = r[["spearman"]])
      }
    }
    for(n in nGrid){
      rs <- replicate(N_MC, {
        w <- tabulate(sample.int(n, n, replace = TRUE), n)
        x <- as.vector(S[[t]][, seq_len(n), drop = FALSE] %*% w)
        unlist(lapply(headline, function(vn)
          corBoth(trans(as.vector(views[[vn]]$Msim %*% x), cfg$simTransform), surV0[[vn]])))
      })
      rs <- matrix(rs, ncol = N_MC)
      nm <- as.vector(outer(c("pearson", "spearman"), headline, function(m, v) paste(v, m, sep = "|")))
      mcse[[length(mcse) + 1]] <- data.frame(context = ctx, type = TYPES[[t]]$label, n = n,
                                             key = nm, mcse = apply(rs, 1, stats::sd, na.rm = TRUE))
    }
  }
  conv <- do.call(rbind, conv); mcse <- do.call(rbind, mcse)
  stable <- do.call(rbind, lapply(types, function(t) do.call(rbind, lapply(headline, function(vn)
    do.call(rbind, lapply(c("pearson", "spearman"), function(m){
      cv <- conv[conv$type == TYPES[[t]]$label & conv$view == vn, ]
      r  <- cv[[m]]; fin <- r[N]
      mc <- mcse[mcse$type == TYPES[[t]]$label & mcse$key == paste(vn, m, sep = "|"), ]
      mcAt <- if(nrow(mc) > 1) stats::approx(mc$n, mc$mcse, xout = cv$n, rule = 2)$y else rep(NA_real_, N)
      bad  <- !(abs(r - fin) <= STABLE_TOL) | !(mcAt <= STABLE_TOL / 2)
      bad[is.na(bad)] <- TRUE
      nS <- if(bad[N]) NA_integer_ else max(c(0L, cv$n[bad])) + 1L
      data.frame(context = ctx, type = TYPES[[t]]$label, view = vn, scale_m = views[[vn]]$scale_m,
                 method = m, N = N, r_final = fin,
                 mcse_at_N = if(nrow(mc)) mc$mcse[which.max(mc$n)] else NA_real_,
                 sd_last21_old = if(N >= 21) stats::sd(r[(N - 20):N]) else NA_real_,
                 N_stable = nS)
    }))))))

  #### which areas the extra tests use ####
  qualDest <- which(edg$aoiHa >= MIN_AOI_HA)
  if(hasL){
    nRespAoi <- vapply(seq_len(nA), function(k) length(unique(sv$resp[sv$LV[, k] > 0])), 1L)
    qualMove <- edg$aoiLetters[nRespAoi >= MIN_RESP_AOI &
                                 lengths(G$aoiCells) >= MIN_AOI_CELLS]
  }else{
    massA <- vapply(G$aoiCells, function(cl) sum(sv$surveyBase[cl]), 1)
    qualMove <- names(G$aoiCells)[massA > 0 & lengths(G$aoiCells) >= MIN_AOI_CELLS]
  }
  logf("compare %s: %d of %d areas in the destination test, %d in the movement test",
       ctx, length(qualDest), nA, length(qualMove))

  #### one draw of every metric, for given replicate and line weights ####
  drawMetrics <- function(wRep, wLine){
    surBase <- if(hasL) as.vector(Matrix::crossprod(sv$L, wLine)) else sv$surveyBase
    surV    <- lapply(views, function(v) surveyView(G, v, surBase))
    surMass <- vapply(G$aoiCells, function(cl) sum(surBase[cl]), 1)
    surVis  <- if(hasL) as.vector(Matrix::crossprod(sv$LV, wLine)) else NULL
    lapply(stats::setNames(types, types), function(t){
      sim100  <- as.vector(S[[t]] %*% wRep)
      simBase <- as.vector(G$P %*% sim100)
      rr <- unlist(lapply(names(views), function(vn){
        r <- corBoth(trans(as.vector(views[[vn]]$Msim %*% sim100), cfg$simTransform), surV[[vn]])
        stats::setNames(r, paste("r", vn, names(r), sep = "|"))
      }))
      simMass <- vapply(G$aoiCells, function(cl) sum(simBase[cl]), 1)
      dm <- destStats(simMass[qualDest], surMass[qualDest], "dest|mass")
      dv <- if(hasL) destStats(as.vector(Vr[[t]] %*% wRep)[qualDest], surVis[qualDest], "dest|visit") else NULL
      mv <- movementStats(simBase, surBase, G$aoiCells, qualMove, cfg$simTransform)
      c(rr, dm, dv, mv)
    })
  }

  #### the paired two-level bootstrap; draw 0 is the data itself ####
  set.seed(BASE_SEED + 4L)
  draws <- vector("list", N_BOOT + 1)
  for(b in 0:N_BOOT){
    wRep  <- if(b == 0) rep(1, N) else tabulate(sample.int(N, N, replace = TRUE), N)
    wLine <- NULL
    if(hasL){
      wResp <- if(b == 0) rep(1, nResp) else tabulate(sample.int(nResp, nResp, replace = TRUE), nResp)
      wLine <- wResp[respIdx]
    }
    draws[[b + 1]] <- drawMetrics(wRep, wLine)
    if(b %% 100 == 0 && b > 0) logf("compare %s: bootstrap %d/%d", ctx, b, N_BOOT)
  }
  #per type: (B + 1) x metrics
  M <- lapply(stats::setNames(types, types), function(t) do.call(rbind, lapply(draws, `[[`, t)))
  keys <- colnames(M[[1]])

  #### the noise ceiling (Wigger, Glatt) ####
  ceiling <- NULL
  if(hasL){
    set.seed(BASE_SEED + 5L)
    ids  <- unique(sv$resp)
    half <- replicate(N_SPLITS, {
      A  <- sample(length(ids), floor(length(ids) / 2))
      wA <- as.numeric(respIdx %in% A)
      sA <- as.vector(Matrix::crossprod(sv$L, wA)); sB <- as.vector(Matrix::crossprod(sv$L, 1 - wA))
      unlist(lapply(names(views), function(vn)
        corBoth(surveyView(G, views[[vn]], sA), surveyView(G, views[[vn]], sB))))
    })
    half <- matrix(half, ncol = N_SPLITS)
    nm   <- as.vector(outer(c("pearson", "spearman"), names(views), function(m, v) paste(v, m, sep = "|")))
    rhoS <- stats::setNames(vapply(seq_len(nrow(half)), function(i){
      r <- mean(half[i, ], na.rm = TRUE); 2 * r / (1 + r) }, 1), nm)
    ceiling <- do.call(rbind, lapply(types, function(t){
      simHalf <- replicate(N_SPLITS, {
        A <- sample(N, floor(N / 2)); wA <- as.numeric(seq_len(N) %in% A)
        xA <- as.vector(S[[t]] %*% wA); xB <- as.vector(S[[t]] %*% (1 - wA))
        unlist(lapply(names(views), function(vn) corBoth(
          trans(as.vector(views[[vn]]$Msim %*% xA), cfg$simTransform),
          trans(as.vector(views[[vn]]$Msim %*% xB), cfg$simTransform))))
      })
      simHalf <- matrix(simHalf, ncol = N_SPLITS)
      rhoSim  <- vapply(seq_len(nrow(simHalf)), function(i){
        r <- mean(simHalf[i, ], na.rm = TRUE); 2 * r / (1 + r) }, 1)
      key <- strsplit(nm, "|", fixed = TRUE)
      data.frame(context = ctx, type = TYPES[[t]]$label, view = vapply(key, `[`, "", 1),
                 method = vapply(key, `[`, "", 2), rho_survey = rhoS, rho_sim = rhoSim,
                 r_max = sqrt(pmax(rhoS, 0) * pmax(rhoSim, 0)), row.names = NULL)
    }))
  }

  #### spatial agreement beyond r ####
  vSet <- unique(c("1_all", paste0(cfg$oldScale, "_all")))
  nRespAll <- if(hasL) nResp else NA_real_
  spatial <- do.call(rbind, lapply(types, function(t){
    x100  <- Matrix::rowSums(S[[t]])
    walks <- sum(vapply(runs[[t]], function(x) as.numeric(x$nAgents - x$nNoGoal), 1))
    do.call(rbind, lapply(vSet, function(vn){
      v   <- views[[vn]]
      sim <- trans(as.vector(v$Msim %*% x100), cfg$simTransform)
      #the outing share is only meaningful on the unaggregated 100 m counts; a
      #coarser block takes its busiest cell's (see blockMax above agreementScores)
      pO  <- if(!hasL || !identical(cfg$simTransform, "none")) NULL else
        if(vn == "1_all") as.vector(v$Msim %*% x100) / walks else
          blockMax(v$A, as.vector(G$P %*% x100)) / walks
      sc  <- agreementScores(sim, surV0[[vn]], pO, nRespAll)
      data.frame(context = ctx, type = TYPES[[t]]$label, view = vn, scale_m = v$scale_m, t(sc))
    }))
  }))
  #the survey against itself: half the respondents as the "model"
  if(hasL){
    set.seed(BASE_SEED + 6L)
    halfRows <- lapply(vSet, function(vn){
      v <- views[[vn]]
      sc <- replicate(min(N_SPLITS, 100), {
        A  <- sample(nResp, floor(nResp / 2)); wA <- as.numeric(respIdx %in% A)
        rA <- as.vector(Matrix::crossprod(sv$L, wA))
        sA <- surveyView(G, v, rA)
        sB <- surveyView(G, v, as.vector(Matrix::crossprod(sv$L, 1 - wA)))
        pA <- (if(vn == "1_all") sA else blockMax(v$A, rA * G$surValid(rA))) / length(A)
        agreementScores(sA, sB, pA, nResp - length(A))
      })
      data.frame(context = ctx, type = "Survey half vs half", view = vn, scale_m = v$scale_m,
                 t(rowMeans(sc, na.rm = TRUE)))
    })
    spatial <- rbind(spatial, do.call(rbind, halfRows))
  }
  rownames(spatial) <- NULL

  #### summaries ####
  keyParts <- function(k){
    p <- strsplit(k, "|", fixed = TRUE)[[1]]
    if(p[1] == "r") list(test = "map", view = p[2], method = p[3]) else
      list(test = p[1], view = p[2], method = p[3])
  }
  rowsFor <- function(mat, extra){
    do.call(rbind, lapply(colnames(mat), function(k){
      kp <- keyParts(k); s <- summ(mat[, k])
      v  <- views[[kp$view]]
      cbind(extra, data.frame(test = kp$test, view = kp$view,
                              scale_m = if(is.null(v)) NA_real_ else v$scale_m,
                              cellset = if(is.null(v)) NA_character_ else v$cellset,
                              method = kp$method, t(s), row.names = NULL))
    }))
  }
  metrics <- do.call(rbind, lapply(types, function(t)
    rowsFor(M[[t]], data.frame(context = ctx, type = TYPES[[t]]$label))))
  metrics$N <- N

  diffs <- do.call(rbind, lapply(CONTRASTS, function(cn){
    if(!all(cn %in% types)) return(NULL)
    D <- M[[cn[1]]] - M[[cn[2]]]
    out <- rowsFor(D, data.frame(context = ctx,
                                 contrast = paste(TYPES[[cn[1]]]$label, "-", TYPES[[cn[2]]]$label)))
    out$p_not_greater <- vapply(colnames(D), function(k){
      bt <- D[-1, k]; bt <- bt[is.finite(bt)]
      if(length(bt)) mean(bt <= 0) else NA_real_ }, 1)
    out
  }))

  #standardised performance against the baseline (Full-Random), per draw (map
  #correlations only)
  rk <- grep("^r\\|", keys, value = TRUE)
  perf <- rbindFill(lapply(intersect(BASELINE, types), function(base)
    rbindFill(lapply(setdiff(types, base), function(t){
      R0  <- M[[base]][, rk, drop = FALSE]
      Std <- (M[[t]][, rk, drop = FALSE] - R0) / (1 - R0)
      out <- rowsFor(Std, data.frame(context = ctx, baseline = TYPES[[base]]$label,
                                     type = TYPES[[t]]$label))
      names(out)[names(out) %in% c("point", "boot_mean", "boot_sd", "ci_lo", "ci_hi")] <-
        paste0("std_", c("point", "boot_mean", "boot_sd", "ci_lo", "ci_hi"))
      out$r          <- unname(M[[t]][1, rk])
      out$r_baseline <- unname(R0[1, ])
      #the noise ceiling (r_max: its point estimate) on the same scale - a reference
      #line in the figures, the scale itself stays capped at 1
      if(!is.null(ceiling)){
        cm <- ceiling[ceiling$type == TYPES[[t]]$label, ]
        rmax <- vapply(rk, function(k){
          kp <- keyParts(k); x <- cm$r_max[cm$view == kp$view & cm$method == kp$method]
          if(length(x)) x else NA_real_ }, 1)
        out$r_max       <- rmax
        out$std_ceiling <- (rmax - out$r_baseline) / (1 - out$r_baseline)
      }
      out
    }))))

  #the maps the figures draw: sums over all replicates, view 1_all
  v1 <- views[["1_all"]]
  maps <- list(grid = v1$grid, idx = v1$idx, survey = surV0[["1_all"]],
               sim = lapply(S, function(Sm) as.vector(v1$Msim %*% Matrix::rowSums(Sm))))
  if(!is.null(views[["1_nocentre"]])) maps$nocentreIdx <- views[["1_nocentre"]]$idx

  list(ctx = ctx, N = N, metrics = metrics, diffs = diffs, perf = perf, ceiling = ceiling, spatial = spatial,
       conv = conv, mcse = mcse, stable = stable, runSummary = runSummary, maps = maps,
       aoi = aoi, ac = G$ac, meta = meta, parity = sv$parity,
       qual = list(dest = length(qualDest), move = length(qualMove), areas = nA))
}

#new areas against the old ones, inside the perimeter
aoiSummary <- function(ctx, aoi){
  cfg <- CTX[[ctx]]
  perim <- perimeter2056(aoi)
  newU  <- sf::st_union(sf::st_geometry(clipAois(aoi)))
  out <- data.frame(context = ctx, threshold = aoi$thr, areas_unsplit = aoi$nUnsplit,
                    areas_cut = aoi$nCut, new_ha = as.numeric(sf::st_area(newU)) / 1e4,
                    old_ha = NA_real_, overlap_ha = NA_real_, jaccard = NA_real_)
  f <- file.path(PATHS$oldAoiDir, cfg$oldAoi)
  if(file.exists(f)){
    old <- tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) NULL)
    if(!is.null(old) && nrow(old)){
      oldU <- sf::st_union(sf::st_make_valid(sf::st_transform(sf::st_zm(sf::st_geometry(old)), 2056)))
      oldU <- suppressWarnings(sf::st_intersection(oldU, perim))
      ov   <- suppressWarnings(sf::st_intersection(oldU, newU))
      out$old_ha     <- as.numeric(sum(sf::st_area(oldU))) / 1e4
      out$overlap_ha <- if(length(ov)) as.numeric(sum(sf::st_area(ov))) / 1e4 else 0
      out$jaccard    <- out$overlap_ha / (out$old_ha + out$new_ha - out$overlap_ha)
    }
  }
  out
}

#the published values, if the old result file is there
oldPerformance <- function(){
  f <- file.path(PATHS$oldDir, "performance_pearson.RData")
  if(!file.exists(f)) return(NULL)
  e <- new.env(); load(f, envir = e)
  d <- e[[ls(e)[1]]]
  if(!is.data.frame(d)) return(NULL)
  if("index" %in% names(d)) d <- d[d$index == "pearson", ]
  ctxOf <- c(GlattPark = "glattPark", Wigger = "wigger", Glatt = "glatt")
  #the published Random is the user's original definition - neither proximity nor
  #attractivity - which is what Full-Random reproduces here
  type <- as.character(d$sim)
  type[type == "Random"] <- TYPES$fullRandom$label
  data.frame(context = unname(ctxOf[as.character(d$location)]), type = type,
             old_r = d$mean, old_sd_last21 = d$sd,
             old_std = if("stdMean" %in% names(d)) d$stdMean else NA_real_,
             old_std_sd = if("stdSD" %in% names(d)) d$stdSD else NA_real_)
}

stageCompare <- function(){
  res <- list()
  for(ctx in CONTEXTS){
    r <- compareContext(ctx)
    if(!is.null(r)) res[[ctx]] <- r
  }
  if(!length(res)) return(invisible(NULL))
  bind <- function(part) rbindFill(lapply(res, `[[`, part))
  wcsv <- function(d, name) if(!is.null(d) && nrow(d))
    utils::write.csv(d, outPath("results", name), row.names = FALSE)

  metrics <- bind("metrics")
  wcsv(metrics[metrics$test == "map", ], "correlations_long.csv")
  wcsv(metrics[metrics$test == "dest", ], "destination.csv")
  wcsv(metrics[metrics$test == "move", ], "movement.csv")
  wcsv(bind("diffs"), "differences.csv")
  wcsv(bind("ceiling"), "ceiling.csv")
  wcsv(bind("spatial"), "spatial_agreement.csv")
  wcsv(bind("conv"), "convergence.csv")
  wcsv(bind("mcse"), "convergence_mcse.csv")
  wcsv(bind("stable"), "reps_needed.csv")
  wcsv(bind("runSummary"), "runs_summary.csv")

  perf <- bind("perf")
  old  <- oldPerformance()
  if(!is.null(perf) && !is.null(old)){
    #the published values sit beside the comparable rows: old scale, all cells,
    #Pearson (the baseline, Full-Random, is the published Random)
    perf <- merge(perf, old, by = c("context", "type"), all.x = TRUE)
    hit  <- perf$view == oldView(perf$context) & perf$method == "pearson"
    for(cn in c("old_r", "old_sd_last21", "old_std", "old_std_sd")) perf[[cn]][!hit] <- NA
  }
  wcsv(perf, "performance.csv")

  aoiTab <- do.call(rbind, lapply(names(res), function(cx) cbind(aoiSummary(cx, res[[cx]]$aoi),
    respondents_in_raster = res[[cx]]$meta$rasterSum, respondents_on_nodes = res[[cx]]$meta$nodeSum,
    agents = res[[cx]]$meta$nAgents, survey_parity_r = res[[cx]]$parity,
    areas_destination_test = res[[cx]]$qual$dest, areas_movement_test = res[[cx]]$qual$move)))
  wcsv(aoiTab, "aoi_summary.csv")

  #maps for re-plotting
  for(cx in names(res)){
    m <- res[[cx]]$maps
    put <- function(vals, name){
      r <- terra::rast(gridFrom(m$grid)); v <- rep(NA_real_, terra::ncell(r)); v[m$idx] <- vals
      terra::values(r) <- v
      terra::writeRaster(r, outPath("maps", paste0(cx, "_", name, ".tif")), overwrite = TRUE)
    }
    put(m$survey, "survey")
    for(t in names(m$sim)) put(m$sim[[t]], t)
  }

  saveRDS(res, outPath("results", "compare.rds"))
  #the headline, printed: published scale, all cells, Pearson
  hl <- metrics[metrics$test == "map" & metrics$cellset %in% "all" & metrics$method == "pearson" &
                  metrics$view == oldView(metrics$context), ]
  pub <- if(is.null(old)) rep(NA_real_, nrow(hl)) else
    old$old_r[match(paste(hl$context, hl$type), paste(old$context, old$type))]
  logf("compare: Pearson r at the published scale (95 %% CI) [published]")
  for(i in seq_len(nrow(hl)))
    logf("  %-10s %-14s %.3f (%.3f-%.3f) [%s]", hl$context[i], hl$type[i], hl$point[i],
         hl$ci_lo[i], hl$ci_hi[i], if(is.na(pub[i])) "-" else sprintf("%.3f", pub[i]))
}

#### 8. stage figures ####

stageFigures <- function(){
  f <- outPath("results", "compare.rds")
  if(!file.exists(f)){ logf("figures: no compare results"); return(invisible(NULL)) }
  res <- readRDS(f)
  suppressPackageStartupMessages(library(ggplot2))
  fig <- function(name) outPath("figures", name)
  typeLevels <- vapply(TYPES, `[[`, "", "label")
  shown <- c("Full", "Nearby", "Nearby-Random")

  theme_simMaps <- theme(legend.position = "none", axis.text = element_blank(),
                         panel.background = element_rect(fill = "lightgrey"),
                         panel.spacing = unit(0, "mm"), plot.margin = margin(5, 0, 0, 0),
                         plot.title = element_text(margin = margin(0, 0, -1, 0), hjust = 0, size = 11))
  mapRaster <- function(m, vals){
    r <- terra::rast(gridFrom(m$grid)); v <- rep(NA_real_, terra::ncell(r)); v[m$idx] <- std01(vals)
    terra::values(r) <- v; names(r) <- "layer"; r
  }
  mapPanel <- function(r, title){
    ggplot() + ggtitle(title) + tidyterra::geom_spatraster(data = r, aes(fill = layer)) +
      scale_fill_gradientn(name = "Std.\nUsage", colours = c("white", "red", "purple4"),
                           na.value = "lightgrey", limits = c(0, 1)) + theme_bw()
  }
  legendOf <- function(p) ggpubr::get_legend(p + theme(legend.key.width = unit(0.5, "lines"),
    legend.title = element_text(size = 10, margin = margin(0, 0, 10, 0)),
    legend.background = element_rect(fill = "transparent")))

  #### Fig 5_6: Wigger and Glatt, survey + three types, 100 m ####
  rowFor <- function(ctx, letters, label){
    m <- res[[ctx]]$maps
    titles <- c(paste0(letters[1], ") Survey"), paste0(letters[2], ") Full sim."),
                paste0(letters[3], ") Nearby sim."), paste0(letters[4], ") Nearby-Random sim."))
    ps <- list(mapPanel(mapRaster(m, m$survey), titles[1]))
    for(k in seq_along(c("full", "nearby", "nearbyRandom"))){
      t <- c("full", "nearby", "nearbyRandom")[k]
      if(!is.null(m$sim[[t]])) ps[[length(ps) + 1]] <- mapPanel(mapRaster(m, m$sim[[t]]), titles[k + 1])
    }
    leg <- legendOf(ps[[1]])
    ps  <- lapply(ps, function(p) p + theme_simMaps)
    #the region's name in its own strip above the row, so it never sits on a
    #panel title
    gridExtra::arrangeGrob(grobs = c(ps, list(leg)), nrow = 1,
                           widths = c(rep(1, length(ps)), 0.3),
                           top = grid::textGrob(label, x = 0.005, hjust = 0,
                                                gp = grid::gpar(fontsize = 14)))
  }
  rows <- list()
  if(!is.null(res[["wigger"]])) rows$w <- rowFor("wigger", c("a", "b", "c", "d"), "Wigger region")
  if(!is.null(res[["glatt"]]))  rows$g <- rowFor("glatt", c("e", "f", "g", "h"), "Glatt wider region")
  if(length(rows)){
    p <- gridExtra::arrangeGrob(grobs = rows, ncol = 1)
    ggsave(fig("fig_5_6_wigger_glatt.png"), p, width = 20, height = 7 * length(rows),
           units = "cm", dpi = 300, bg = "white")
  }

  #### Fig 5: GlattPark, survey + Full + scatter ####
  if(!is.null(res[["glattPark"]]) && !is.null(res[["glattPark"]]$maps$sim$full)){
    m   <- res[["glattPark"]]$maps
    sim <- sqrt(m$sim$full)
    gS  <- mapPanel(mapRaster(m, m$survey), "a) Survey")
    gM  <- mapPanel(mapRaster(m, sim), "b) Simulation")
    leg <- legendOf(gS)
    mt  <- res[["glattPark"]]$metrics
    rAll <- mt$point[mt$type == "Full" & mt$view == "1_all" & mt$method == "pearson" & mt$test == "map"]
    rNoC <- mt$point[mt$type == "Full" & mt$view == "1_nocentre" & mt$method == "pearson" & mt$test == "map"]
    d <- data.frame(survey = std01(m$survey), sim = std01(sim))
    gC <- ggplot(d, aes(survey, sim)) + geom_point(size = 0.6) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = "red3") +
      annotate("text", x = 0, y = 1, hjust = 0, vjust = 1, size = 3.6,
               label = sprintf("Correlation: %.2f\nwithout the 4 centre cells: %.2f", rAll,
                               if(length(rNoC)) rNoC else NA)) +
      labs(x = "Std survey usage", y = "Std simulation usage [square root]", title = "c) Correlation") +
      theme_classic()
    g <- gridExtra::arrangeGrob(grobs = list(gS + theme_simMaps, gM + theme_simMaps, leg, gC),
                                layout_matrix = rbind(c(1, 3, 4), c(2, 3, 4)), widths = c(0.8, 0.15, 1))
    ggsave(fig("fig_5_glattpark.png"), ggpubr::as_ggplot(g), width = 20, height = 10,
           units = "cm", dpi = 300, bg = "white")
  }

  #### performance bars (bootstrap 95 % CI) ####
  perf <- rbindFill(lapply(res, `[[`, "perf"))
  if(!is.null(perf) && nrow(perf)){
    perf$type <- factor(perf$type, levels = typeLevels)
    order3 <- intersect(c("glattPark", "wigger", "glatt"), names(res))
    perfFig <- function(sel, valueCols, ylab, file){
      ps <- lapply(order3, function(cx){
        d <- perf[perf$context == cx & sel(perf) & perf$type %in% shown, ]
        if(!nrow(d) || all(is.na(d[[valueCols[1]]]))) return(NULL)
        d$v <- d[[valueCols[1]]]; d$lo <- d[[valueCols[2]]]; d$hi <- d[[valueCols[3]]]
        p <- ggplot(d, aes(type, v)) + geom_col() +
          geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.25) + theme_bw() +
          labs(x = NULL, y = ylab, title = sprintf("%s (%s m)", CTX[[cx]]$label, d$scale_m[1])) +
          scale_x_discrete(labels = function(x) sub("-", "-\n", x, fixed = TRUE))
        #the noise ceiling of Full (the types' ceilings differ only in the 4th decimal)
        cl <- if("std_ceiling" %in% names(d)) d$std_ceiling[d$type == "Full"][1] else NA
        if(isTRUE(is.finite(cl)))
          p <- p + geom_hline(yintercept = cl, linetype = "dashed", colour = "grey50")
        p
      })
      ok <- !vapply(ps, is.null, logical(1))
      if(!any(ok)) return(invisible(NULL))
      p <- ggpubr::ggarrange(plotlist = ps[ok], nrow = 1, labels = paste0(letters[seq_len(sum(ok))], ")"))
      ggsave(fig(file), p, width = 20, height = 10, units = "cm", dpi = 300, bg = "white")
    }
    #NA = the published scale; GlattPark's survey is 500 m, so its "1" is 500 m too
    atScale <- function(f) function(d)
      d$view == (if(is.na(f)) oldView(d$context) else paste0(f, "_all")) & d$method == "pearson"
    base <- TYPES[[BASELINE]]$label
    perfFig(atScale(NA), c("std_point", "std_ci_lo", "std_ci_hi"),
            sprintf("Standardised performance (vs %s)", base), "fig_performance_500m.png")
    perfFig(atScale(1), c("std_point", "std_ci_lo", "std_ci_hi"),
            sprintf("Standardised performance (vs %s)", base), "fig_performance_100m.png")
  }

  #### paired differences ####
  diffs <- rbindFill(lapply(res, `[[`, "diffs"))
  if(!is.null(diffs) && nrow(diffs)){
    d <- diffs[diffs$test == "map" & diffs$cellset == "all" & diffs$method == "pearson", ]
    d <- d[d$view == "1_all" | d$view == oldView(d$context), ]
    d$scale <- paste(d$scale_m, "m")
    d$context <- ctxLabel(d$context)
    p <- ggplot(d, aes(contrast, point)) + geom_hline(yintercept = 0, colour = "grey50") +
      geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi)) + coord_flip() +
      facet_grid(context ~ scale) + theme_bw() +
      labs(x = NULL, y = "Difference in Pearson r (paired bootstrap, 95 % CI)")
    ggsave(fig("fig_differences.png"), p, width = 20, height = 14, units = "cm", dpi = 300, bg = "white")
  }

  #### destination vs movement ####
  metrics <- rbindFill(lapply(res, `[[`, "metrics"))
  dm <- metrics[(metrics$test == "dest" & metrics$method == "pearson" &
                   ((metrics$view == "visit") | (metrics$view == "mass" & metrics$context == "glattPark"))) |
                  (metrics$test == "move" & metrics$view == "pooled" & metrics$method == "pearson"), ]
  if(nrow(dm)){
    dm$part <- ifelse(dm$test == "dest", ifelse(dm$view == "visit", "Destination (visit shares)",
                                                 "Destination (usage-mass shares)"),
                      "Movement inside areas (pooled)")
    dm$type <- factor(dm$type, levels = typeLevels)
    dm$context <- ctxLabel(dm$context)
    p <- ggplot(dm, aes(type, point)) + geom_pointrange(aes(ymin = ci_lo, ymax = ci_hi)) +
      facet_grid(part ~ context, scales = "free_y") + theme_bw() +
      labs(x = NULL, y = "Pearson r (95 % CI)")
    ggsave(fig("fig_decomposition.png"), p, width = 20, height = 14, units = "cm", dpi = 300, bg = "white")
  }

  #### r against scale, all cells vs path cells, with the noise ceiling ####
  sc <- metrics[metrics$test == "map" & metrics$method == "pearson" & metrics$cellset %in% c("all", "path"), ]
  if(nrow(sc)){
    #the noise ceiling of Full at each view, placed by the view's own scale
    ce <- rbindFill(lapply(res, `[[`, "ceiling"))
    if(!is.null(ce) && nrow(ce)){
      ce <- merge(ce[ce$type == "Full" & ce$method == "pearson", c("context", "view", "r_max")],
                  unique(sc[, c("context", "view", "scale_m", "cellset")]), by = c("context", "view"))
      ce$context <- ctxLabel(ce$context)
    }
    sc$type <- factor(sc$type, levels = typeLevels)
    sc$context <- ctxLabel(sc$context)
    p <- ggplot(sc, aes(scale_m, point, colour = type, linetype = cellset)) + geom_line() + geom_point() +
      scale_x_log10() + facet_wrap(~ context) + theme_bw() +
      labs(x = "Cell size (m)", y = "Pearson r (black: noise ceiling)", colour = NULL, linetype = "Cells")
    if(!is.null(ce) && nrow(ce))
      p <- p + geom_line(data = ce, aes(scale_m, r_max, linetype = cellset), colour = "black",
                         inherit.aes = FALSE)
    ggsave(fig("fig_scales.png"), p, width = 20, height = 9, units = "cm", dpi = 300, bg = "white")
  }

  #### replicate stability ####
  cv <- rbindFill(lapply(res, `[[`, "conv"))
  mc <- rbindFill(lapply(res, `[[`, "mcse"))
  st <- rbindFill(lapply(res, `[[`, "stable"))
  if(!is.null(cv) && nrow(cv)){
    hv <- unique(c(paste0(names(res), "|", oldView(names(res))), paste0(names(res), "|1_all")))
    cv <- cv[paste0(cv$context, "|", cv$view) %in% hv, ]
    cv$se <- NA_real_
    for(k in unique(paste(cv$context, cv$type, cv$view))){
      i <- which(paste(cv$context, cv$type, cv$view) == k)
      m <- mc[paste(mc$context, mc$type) == paste(cv$context[i[1]], cv$type[i[1]]) &
                mc$key == paste(cv$view[i[1]], "pearson", sep = "|"), ]
      if(nrow(m) > 1) cv$se[i] <- stats::approx(m$n, m$mcse, xout = cv$n[i], rule = 2)$y
    }
    cv$type <- factor(cv$type, levels = typeLevels)
    cv$panel <- paste0(ctxLabel(cv$context), ", ", cv$scale_m, " m")
    stp <- st[st$method == "pearson" & paste0(st$context, "|", st$view) %in% hv & !is.na(st$N_stable), ]
    p <- ggplot(cv, aes(n, pearson, colour = type, fill = type)) +
      geom_ribbon(aes(ymin = pearson - se, ymax = pearson + se), alpha = 0.2, colour = NA) +
      geom_line() + facet_wrap(~ panel, scales = "free_y") + theme_bw() +
      labs(x = "Replicates", y = "Pearson r (running; band = Monte Carlo SE; dashed = stable)",
           colour = NULL, fill = NULL)
    if(nrow(stp)){
      stp$type  <- factor(stp$type, levels = typeLevels)
      stp$panel <- paste0(ctxLabel(stp$context), ", ", stp$scale_m, " m")
      p <- p + geom_vline(data = stp, aes(xintercept = N_stable, colour = type), linetype = 2)
    }
    ggsave(fig("fig_convergence.png"), p, width = 20, height = 14, units = "cm", dpi = 300, bg = "white")
  }

  #### the areas: new (filled) and old (outlined) ####
  ps <- lapply(names(res), function(cx){
    aoi <- res[[cx]]$aoi
    p <- ggplot() + geom_sf(data = sf::st_sf(geometry = perimeter2056(aoi)), fill = NA, colour = "black") +
      geom_sf(data = res[[cx]]$ac, fill = "#1b7f7a", alpha = 0.5, colour = NA)
    f <- file.path(PATHS$oldAoiDir, CTX[[cx]]$oldAoi)
    old <- if(file.exists(f)) tryCatch(sf::st_read(f, quiet = TRUE), error = function(e) NULL) else NULL
    if(!is.null(old) && nrow(old))
      p <- p + geom_sf(data = sf::st_transform(sf::st_zm(old), 2056), fill = NA, colour = "red3",
                       linewidth = 0.3)
    p + coord_sf(datum = NA) + theme_void() +
      ggtitle(sprintf("%s: threshold %.1f, %d areas\n(filled: new; red: published)",
                      CTX[[cx]]$label, aoi$thr, aoi$nCut))
  })
  if(length(ps)){
    p <- ggpubr::ggarrange(plotlist = ps, nrow = 1)
    ggsave(fig("fig_aois.png"), p, width = 24, height = 9, units = "cm", dpi = 300, bg = "white")
  }
  logf("figures written to %s", file.path(OUT, "figures"))
}

#### 9. main ####

checkInputs <- function(){
  need <- c(dulnAllFile(),
            ns$vftData("maps/attr/allAttrs_COG_final.tif"),
            ns$vftData("maps/paths/paths_11_24_final_4.gdb"),
            ns$vftData("maps/lakes.gdb"), PATHS$swisscom)
  for(ctx in CONTEXTS){
    cfg <- CTX[[ctx]]
    if(is.null(cfg)) stop("unknown context ", ctx)
    need <- c(need, ns$vftData(cfg$perimeter), ns$vftData(cfg$residential),
              switch(cfg$survey, heatmap = PATHS$glattParkHeat, wigger = PATHS$wiggerLines,
                     filBleu = if(identical(FILBLEU_SOURCE, "full")) PATHS$filBleuAll else
                       file.path(PATHS$filBleuDir, paste0("Copy_activity_", FILBLEU_LAYERS, ".shp"))))
  }
  bad <- setdiff(TYPES_RUN, names(TYPES))
  if(length(bad)) stop("unknown type(s): ", paste(bad, collapse = ", "))
  miss <- need[!file.exists(need)]
  if(length(miss)) stop("missing input(s):\n  ", paste(miss, collapse = "\n  "))
  if(!dir.exists(PATHS$oldDir)) logf("note: %s not reachable - no parity check or published values", PATHS$oldDir)
}

main <- function(){
  logf("survey validation | stages %s | contexts %s | types %s | %d reps | %d worker(s) | code %s",
       paste(STAGES, collapse = ","), paste(CONTEXTS, collapse = ","),
       paste(TYPES_RUN, collapse = ","), N_REPS, WORKERS, GIT)
  logf("output: %s", OUT)
  checkInputs()
  for(ctx in CONTEXTS){
    if("aoi" %in% STAGES)     stageAoi(ctx)
    if("survey" %in% STAGES)  stageSurvey(ctx)
    if("network" %in% STAGES) stageNetwork(ctx)
  }
  if("selftest" %in% STAGES) stageSelftest()
  if("sim" %in% STAGES)      stageSim()
  if("compare" %in% STAGES)  stageCompare()
  if("figures" %in% STAGES)  stageFigures()
  logf("done")
}

if(!IS_WORKER) main()
