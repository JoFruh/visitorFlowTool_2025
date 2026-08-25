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

# The question this must answer is "can a bare Rscript daemon library() this
# package?", and only an installed copy in .libPaths() can. installed.packages()
# was the wrong instrument: it scans metadata, is slow, and its result is easy to
# misread when pkgload::load_all() has the package attached from source. Look for
# the installed DESCRIPTION directly - immune to pkgload's shims of system.file()
# and find.package(), which both point back at the source tree under load_all.
#
# Getting this wrong is not a degraded mode, it is the worst mode: vftFuture()
# then runs every job inline on the shared main thread, freezing all users for
# its full duration. So say so loudly rather than falling back in silence.
vftInstalled <- any(file.exists(file.path(.libPaths(), "visitorFlowTool",
                                          "DESCRIPTION")))

if (vftInstalled) {
  # Worker count and pool startup both live in the package now, so the runtime
  # health check cannot disagree with what was configured here. VFT_WORKERS=1 is
  # honoured: mirai::daemons(1) starts one real daemon. Only unusable values fall
  # back to 2. See .vftWorkerCount() / .vftStartDaemons().
  workers <- visitorFlowTool:::.vftWorkerCount()
  wpids   <- visitorFlowTool:::.vftStartDaemons(workers)

  # Report the pids AND the state that decided them. When this warned before, the
  # pid alone could not say why: installed-but-no-daemons and not-installed look
  # identical from here, and cost a round trip each to tell apart.
  message("visitorFlowTool: main pid ", Sys.getpid(),
          "; async worker pids ", paste(wpids, collapse = ", "),
          "; VFT_WORKERS=", Sys.getenv("VFT_WORKERS", "<unset>"),
          " -> workers=", workers,
          "; mirai connections=",
          tryCatch(mirai::status()$connections, error = function(e) "ERROR"),
          "; job timeout=",
          {tm <- visitorFlowTool:::.vftTimeoutMs()
           if(is.null(tm)) "off" else paste0(tm/1000, "s")})
  if(!length(wpids) || anyNA(wpids) || any(wpids == Sys.getpid()))
    message("*** WARNING: async work is NOT leaving the main process. Every job ",
            "will freeze every connected user for its full duration. ***")
} else {
  # dev / load_all: run async work in the main session (has the compiled code loaded).
  #
  # Loud, and naming .libPaths(), because this is not a degraded mode -- it is the
  # worst one. vftFuture() has no daemons to send to, so it evaluates every job
  # inline on the shared main thread and freezes all connected users for the job's
  # full duration. In dev that is correct and wanted; in production it is the whole
  # concurrency problem, and it had been happening in silence.
  message("*** visitorFlowTool is NOT INSTALLED in .libPaths() ***")
  message("    Async jobs will run INLINE on the shared main thread and freeze every")
  message("    connected user for their full duration. Correct for dev; the worst")
  message("    possible production configuration.")
  message("    .libPaths(): ", paste(.libPaths(), collapse = " ; "))
  #Make the inline path explicit rather than merely implied: with no daemons
  #vftFuture() gates on connections < 1 and evaluates in-process. Ensure that is
  #true even if daemons were started earlier in the same session.
  mirai::daemons(0)
}
# currentPlan <- future::plan("future::multisession", workers = 4) #previous backend



# Pre-build the simplified protected-areas layer before any session exists.
#
# Simplifying it costs ~10s. Left lazy, the first user to reach step 5 would pay
# that as a main-thread freeze felt by everyone connected - precisely the thing
# this whole effort is removing. Doing it here moves the cost into startup, where
# there is nobody to block. Every session afterwards just clips the cache (~0.1s).
#
# Wrapped in try(): a missing or unreadable data directory should stop the layer
# from being pre-warmed, not stop the app from starting. vftProtectedAreas() will
# then build it on first use and surface any real error there.
try(visitorFlowTool:::vftProtectedAreasCached(), silent = TRUE)


# Put tmap in interactive mode once, for the process.
#
# tmap_mode() sets a global option. It takes no arguments that vary per session
# and returns the same state every time, but it was being called INSIDE three
# renderLeaflet blocks - so every re-render, in every session, re-set a global
# that was already set, on the shared main thread. In the 2026-08-21 profile that
# was 3.0s at step1_server.R#427 plus 1.2s in step4's map block: ~4s of pure
# repetition, and the fourth largest cost in the app.
#
# Setting it here is also the more correct place. The option is process-wide, not
# session-wide, so with several sessions connected the per-render calls were all
# writing the same value into shared state anyway - just repeatedly, and always
# while somebody was waiting.
#
# Safe to hoist because 'view' is the only mode this app ever asks for; nothing
# switches to 'plot'. If that ever changes, set the mode around the specific
# plot-mode call rather than putting this back in a render block.
tmap::tmap_mode('view')


# Persistent on-disk cache for basemap tiles. maptiles::get_tiles() caches individual
# XYZ tiles here, so re-entering a step or overlapping study areas (across the 2-5
# concurrent sessions in this process) reuse downloaded tiles instead of re-hitting the
# network on the main thread. Lives for the life of the R process (fine for reuse).
vft_tileCacheDir <- file.path(tempdir(), "vft_maptiles_cache")
dir.create(vft_tileCacheDir, showWarnings = FALSE, recursive = TRUE)





