#set global environments and variables
#
# #create environment for polygons (both for step 1 and 5)
# polygonEnv <- new.env(parent = emptyenv())
# polygonEnv$polygonsList <- NULL
#
# #create environment for base elements of app
# envBase <- new.env(parent = emptyenv())
#
# #Global Variables
# envBase$shape <- NULL
# envBase$SM_pres <- NULL
# # envBase$SM_noPres <- NULL
# envBase$network <- NULL
# envBase$parking <- NULL
# envBase$residential
# envBase$networkList <- NULL
# envBase$basemap <- NULL
# envBase$confirm <- NULL
# envBase$minThresh <- NULL
# envBase$finalPolygons <- NULL
# envBase$pathUsage <- NULL
# envBase$versionsUI <- NULL
# envBase$step6FirstRun <- NULL
# envBase$newVersionsFirstRun <- NULL
# envBase$toSelectSpAfter <- NULL
# envBase$triggerNewVersions_nr <- NULL
# envBase$toSelectSpAfter <- NULL
# envBase$toSelectSpAfter <- FALSE
# envBase$step <- NULL
# envBase$step <- 1
# envBase$SMdateTime <- NULL
# envBase$SMColors <- NULL
# # envBase$step1Refreshing <- NULL
# # envBase$step1Refreshing <- FALSE
# envBase$isImported <- NULL
# envBase$isImported <- FALSE
#
# envBase$obsMapClick <- NULL
# envBase$obsMarkerClick <- NULL
# envBase$obsErase <- NULL
#
#
# #step 3 saves
# envBase$filterList <- NULL
# envBase$sdmLayers <- NULL
#
# envBase$df_spInfo <- NULL
# envBase$spChc <- NULL
#
# #checkbox saves
# envBase$checkboxSave <- NULL
# envBase$groupSave_class <- NULL
# envBase$groupSave_sens <- NULL
# envBase$groupSave_type <- NULL
# envBase$groupSave_all <- NULL
# envBase$weightInputs <- NULL
# envBase$weightNames <- NULL
#
# envBase$DULN <- NULL
# envBase$DULN_all <- NULL
#
#
# #variable to activate species selection AFTER recreation modelling
#
# #global variables to control observer creation
# # >> avoids re-creating observers when returning to server function
#
# #global variables for the new versions step
# envNewVersions <- new.env(parent = emptyenv())
#
# envNewVersions$appendedObservers <- NULL
#
# envNewVersions$markerWasClicked <- NULL
#
# envNewVersions$shapeWasClicked <- NULL
#
# envNewVersions$isLinking <- NULL
#
# envNewVersions$firstLinkNode <- NULL
# envNewVersions$secondLinkNode <- NULL
#
# envNewVersions$mapView <- NULL
#
# envNewVersions$trigger <- NULL
#
# #other environments
# envUpdate <- new.env(parent = emptyenv())
# envBtn <- new.env(parent = emptyenv())
#
# envUpdate$updateNetworkPlot <- NULL
# envBtn$versionBtn_nb <- NULL
#

# cppPath <- system.file("src/CPP_FUNCTIONS.cpp", package = "visitorFlowTool")


# if(path.expand("~") == "C:/Users/frueh/Documents"){
#   home <- "C:/Users/frueh/Documents/visitorFlowTool_final"
# }else if(path.expand("~") == "/home/frueh"){
#   home <- "/home/frueh/ShinyApps/visitorFlowTool"
# }

#PROFILING ####
# Start the main-thread stall detector before anything else in this file, so the
# startup work below (plan setup, daemon warming) is itself measured. This is a
# single R process shared by every user: a block on the main thread is a freeze
# for all of them, and this is the only thing that records when that happens.
# See R/perf_helpers.R. Disable with VFT_PERF=0; log location via VFT_PERF_DIR.
visitorFlowTool:::vftPerfInit()

# Optional call-stack sampling, for runs where the stall log comes back mostly
# "unattributed" - i.e. the thread was frozen by code nobody has labelled yet.
# The heartbeat cannot see inside a freeze (it only runs when the thread yields);
# Rprof samples from within it. Off unless VFT_RPROF=1, since it costs a few
# percent and grows ~20 MB/hour. Read back with visitorFlowTool:::vftRprofStop()
# then visitorFlowTool:::vftRprofReport().
visitorFlowTool:::vftRprofStart()

#PREPARE WORKERS ####
# Async backend selection.
#
# Parallel workers (mirai daemons / multisession) are SEPARATE R processes. They can only
# run the app's compiled Rcpp routines (e.g. generateAdjListAndDistTbl_cpp, used by the
# step-6 ABM) if visitorFlowTool is *installed* so the worker can library() it. When the
# app is launched via pkgload::load_all() (dev), the package is NOT installed and its DLL
# lives only in the main session, so a parallel worker cannot find those functions.
#
# Therefore: use parallel mirai daemons only when the package is actually installed
# (production); otherwise fall back to sequential, which runs the futures in the main
# session where the load_all'd compiled code is available. To get real concurrency in
# production, install the package (do not run it via load_all there).
#
# mirai is chosen over future::multisession for its persistent daemons + dispatcher (much
# lower per-task overhead). Existing future({...}) %...>% (...) blocks and
# ipc::AsyncProgress/shinyQueue keep working unchanged either way. Worker count is
# env-configurable via VFT_WORKERS.

# +Inf, and the value is doing real work here - this is not "turn the limit off".
#
# future only enforces a globals ceiling when one is finite: getGlobalsAndPackages()
# guards its sizing pass with `if (is.finite(maxSize))`, and that pass measures each
# captured object with parallelly::serializedSize(), which measures by *serialising
# it*. So a finite ceiling makes the app serialise every captured global once purely
# to weigh it, and then mirai serialises it all over again to actually send it -
# twice the work, all of it on the thread every user is waiting on.
#
# That is not theoretical: in the 2026-08-20 baseline serializedSize was 11.6s of
# self time, 8.4% of everything sampled, making it the largest single piece of real
# work in the profile. Setting +Inf skips the pass entirely.
#
# What is given up is an error when a future captures something enormous. That was
# never worth 11.6s of frozen UI, and it is not how the problem gets caught anyway:
# the profiler shows an oversized capture directly, and Phase 3 removes the cause by
# passing paths into workers instead of objects.
#
# Set unconditionally rather than only on the parallel path, so dev and production
# behave the same and a globals problem cannot hide in the sequential fallback.
options(future.globals.maxSize = +Inf)

vftInstalled <- "visitorFlowTool" %in% rownames(utils::installed.packages())

if (vftInstalled) {
  # Default 2, not 3. The production host has 4 cores, and the main thread has to
  # compete with the daemons for them: at 3 workers a running job starves the very
  # thread that serves everyone else's UI, so the app feels *slower* during async
  # work rather than faster. Main + 2 daemons + Shiny Server overhead fits 4 cores.
  # Raise VFT_WORKERS only along with the core count.
  workers <- as.integer(Sys.getenv("VFT_WORKERS", "2"))
  currentPlan <- future::plan(future.mirai::mirai_multisession, workers = workers)

  # Pre-warm the daemons so each has visitorFlowTool (incl. its compiled DLL) loaded.
  warming <- lapply(seq_len(future::nbrOfWorkers()), function(i) {
    future::future({
      library(visitorFlowTool)
      TRUE
    })
  })
} else {
  # dev / load_all: run async work in the main session (has the compiled code loaded).
  message("visitorFlowTool not installed: using future::sequential (async runs in the main ",
          "session). Install the package to enable parallel mirai daemons.")
  currentPlan <- future::plan("future::sequential")
}
# currentPlan <- future::plan("future::multisession", workers = 4) #previous backend



# Persistent on-disk cache for basemap tiles. maptiles::get_tiles() caches individual
# XYZ tiles here, so re-entering a step or overlapping study areas (across the 2-5
# concurrent sessions in this process) reuse downloaded tiles instead of re-hitting the
# network on the main thread. Lives for the life of the R process (fine for reuse).
vft_tileCacheDir <- file.path(tempdir(), "vft_maptiles_cache")
dir.create(vft_tileCacheDir, showWarnings = FALSE, recursive = TRUE)


#GLOBAL FUNCTIONS
imageMap <- function(inputId, imgsrc, opts, i18n) {
  areas <- lapply(names(opts), function(n)
    shiny::tags$area(title=n, coords=opts[[n]],
                     href="#", shape="poly"))
  js <- paste0("$(document).on('click', 'map area', function(evt) {
  evt.preventDefault();
  var val = evt.target.title;
  Shiny.onInputChange('", inputId, "', val);})")
  list(
    shiny::tags$img(height = 70,src=imgsrc, usemap=paste0("#", inputId),
                    shiny::tags$head(tags$script(shiny::HTML(js)))),
    shiny::tags$map(name=inputId, areas))

  #deactivate imagemap temporarily (until it is more stable)
  #to activate history bar, remove below return function for the function to return list above.
  return(shiny::tags$img(height = 70,src= imgsrc))
}



