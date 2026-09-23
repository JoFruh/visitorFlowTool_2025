#### Step 5's simulation: how long it takes, and that it still gives the same answer ####
#
# The step-5 job - vftSimulateScenario(), i.e. network preparation, population,
# agents and the ABM - runs in a daemon, so it does not freeze other users. But it
# IS how long a user waits, and how long everybody queued behind them waits. This
# script times it on real scenarios and checks every speed change against a saved
# reference: same seed, same passage counts on every edge and vertex, same start
# nodes. A speed change that alters one count has changed the model, not its speed.
#
# Offline, and deliberately so: nothing here is instrumentation in the app.
#
# Usage, from the repository root:
#
#   Rscript data-raw/verify_abm_speed.R --save-ref   # record the reference
#   Rscript data-raw/verify_abm_speed.R              # time and compare
#   Rscript data-raw/verify_abm_speed.R --profile    # add an Rprof breakdown
#   Rscript data-raw/verify_abm_speed.R --diff       # compare, report, never fail
#
# --diff is for a change that is SUPPOSED to move the results (a model fix): it
# prints how the passage distribution shifted instead of asserting identity.
#
# It runs the WORKING TREE through pkgload::load_all(), which compiles src/ in
# place, so it needs no install and does not disturb a running app. Whether the
# same code also works inside a real mirai daemon is a separate question -
# answered by --daemon, which needs the package INSTALLED from this tree.
#
# Inputs: step-5 save files carrying a real tbl_graph. Most saves in Downloads
# carry networkList[[1]]$network == NULL (the path network loads lazily) and are
# useless here; the two below are known to carry one. Override with
# VFT_ABM_SAVES (paths separated by ";").
#
# Reference directory: VFT_ABM_REF, default tools::R_user_dir(..., "cache").

args    <- commandArgs(trailingOnly = TRUE)
saveRef <- "--save-ref" %in% args
profile <- "--profile"  %in% args
diffOK  <- "--diff"     %in% args
daemon  <- "--daemon"   %in% args

SEED <- 42L

saves <- Sys.getenv("VFT_ABM_SAVES", "")
saves <- if(nzchar(saves)) strsplit(saves, ";", fixed = TRUE)[[1]] else
  file.path(Sys.getenv("USERPROFILE", path.expand("~")), "Downloads",
            c("visitorFlowSave_step5_simulation_2026_08_31_10_30_42.148.RData",
              "visitorFlowSave_step5_simulation_2026_08_31_18_56_03.708.RData"))
saves <- saves[file.exists(saves)]
if(!length(saves)) stop("no save files found - set VFT_ABM_SAVES")

refDir <- Sys.getenv("VFT_ABM_REF",
                     tools::R_user_dir("visitorFlowTool", which = "cache"))
dir.create(refDir, recursive = TRUE, showWarnings = FALSE)

if(daemon){
  suppressPackageStartupMessages(library(visitorFlowTool))
  ns <- asNamespace("visitorFlowTool")
}else{
  #VFT_ABM_PKG points at another checkout - e.g. a `git worktree` of the commit
  #before a change - so old and new can be timed back to back on the same machine
  #state. Timings from runs made at different times are not comparable here.
  suppressPackageStartupMessages(
    pkgload::load_all(Sys.getenv("VFT_ABM_PKG", "."), export_all = TRUE, quiet = TRUE))
  ns <- asNamespace("visitorFlowTool")
}

#### the inputs one save provides ####
readScenario <- function(f){
  e <- new.env()
  load(f, envir = e)
  net <- e$envBase_network
  if(is.null(net) && length(e$envBase_networkList))
    net <- e$envBase_networkList[[1]]$network
  if(is.null(net)) stop(basename(f), " carries no network")
  list(network       = net,
       finalPolygons = e$envBase_finalPolygons,
       minThresh     = e$envBase_minThresh,
       parking       = e$envBase_parking)
}

#A progress pair that goes nowhere but counts what it was sent - the count is
#itself worth watching, because on the real queue every message is a locked
#file write and a main-thread handler call.
fakeProgress <- function(){
  st <- new.env(); st$n <- 0L
  h  <- function(...){ st$n <- st$n + 1L; invisible(NULL) }
  list(handle = list(set = h, inc = h, close = function() invisible(NULL)),
       count  = function() st$n)
}

runOne <- function(sc){
  pp <- fakeProgress(); ps <- fakeProgress()
  set.seed(SEED, kind = "Mersenne-Twister", normal.kind = "Inversion",
           sample.kind = "Rejection")
  t0  <- proc.time()[["elapsed"]]
  out <- ns$vftSimulateScenario(sc$network, sc$finalPolygons, sc$minThresh,
                                parking = sc$parking, residentDivision = 50,
                                progPrep = pp$handle, progSim = ps$handle)
  list(out = out, secs = proc.time()[["elapsed"]] - t0,
       msgs = pp$count() + ps$count())
}

runInDaemon <- function(sc){
  mirai::daemons(1)
  on.exit(mirai::daemons(0), add = TRUE)
  m <- mirai::mirai({
    .libPaths(libs)
    suppressPackageStartupMessages(library(visitorFlowTool))
    ns <- asNamespace("visitorFlowTool")
    h  <- list(set = function(...) NULL, inc = function(...) NULL,
               close = function() NULL)
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion",
             sample.kind = "Rejection")
    t0  <- proc.time()[["elapsed"]]
    out <- ns$vftSimulateScenario(sc$network, sc$finalPolygons, sc$minThresh,
                                  parking = sc$parking, residentDivision = 50,
                                  progPrep = h, progSim = h)
    list(out = out, secs = proc.time()[["elapsed"]] - t0, msgs = NA_integer_)
  }, sc = sc, seed = SEED, libs = .libPaths(), .timeout = 3600e3)
  res <- mirai::call_mirai(m)$data
  if(inherits(res, "miraiError") || inherits(res, "errorValue"))
    stop("daemon run failed: ", format(res))
  res
}

#### what counts as "the same answer" ####
fingerprint <- function(out){
  g  <- out$results$pathUsage
  ea <- igraph::edge_attr(g);   va <- igraph::vertex_attr(g)
  list(edge   = ea[grep("^passage", names(ea))],
       vertex = va[grep("^passage", names(va))],
       edgeID = ea$edgeID,
       startV = out$results$dayPop$startV,
       nAgents = nrow(out$results$dayPop))
}

compareFP <- function(ref, now){
  bad <- character(0)
  for(part in c("edge", "vertex")){
    for(nm in union(names(ref[[part]]), names(now[[part]]))){
      if(!identical(ref[[part]][[nm]], now[[part]][[nm]]))
        bad <- c(bad, sprintf("%s$%s", part, nm))
    }
  }
  for(nm in c("edgeID", "startV", "nAgents"))
    if(!identical(ref[[nm]], now[[nm]])) bad <- c(bad, nm)
  bad
}

describeShift <- function(ref, now){
  for(nm in intersect(names(ref$edge), names(now$edge))){
    a <- as.numeric(ref$edge[[nm]]); b <- as.numeric(now$edge[[nm]])
    if(length(a) != length(b)){ cat(sprintf("  %-16s length %d -> %d\n", nm, length(a), length(b))); next }
    cat(sprintf("  %-16s total %9.0f -> %9.0f  (%+6.1f%%)  cor %.3f  edges used %d -> %d\n",
                nm, sum(a), sum(b), 100 * (sum(b) - sum(a)) / max(sum(a), 1),
                suppressWarnings(stats::cor(a, b)), sum(a > 0), sum(b > 0)))
  }
}

#### run ####
failed <- FALSE
for(f in saves){
  tag <- sub("[.]RData$", "", basename(f))
  sc  <- readScenario(f)
  cat(sprintf("\n== %s\n   %d nodes, %d edges, prepared: %s\n", tag,
              igraph::vcount(sc$network), igraph::ecount(sc$network),
              ns$vftNetworkPrepared(sc$network)))

  if(profile){
    pf <- tempfile(fileext = ".out")
    Rprof(pf, interval = 0.01, line.profiling = TRUE)
  }
  res <- if(daemon) runInDaemon(sc) else runOne(sc)
  if(profile){
    Rprof(NULL)
    s <- summaryRprof(pf, lines = "hide")
    cat("   -- by.total (top 25)\n")
    print(utils::head(s$by.total[, c("total.time", "self.time")], 25))
    cat("   -- by.self (top 15)\n")
    print(utils::head(s$by.self[, c("self.time", "total.time")], 15))
  }

  cat(sprintf("   %.2f s, %s progress messages, %d agents, result %.1f MB serialised\n",
              res$secs, format(res$msgs), nrow(res$out$results$dayPop),
              length(serialize(res$out$results, NULL)) / 1e6))

  fp  <- fingerprint(res$out)
  ref <- file.path(refDir, paste0(tag, ".rds"))

  #VFT_ABM_SAVE_TO: ALSO record this run as the reference in another directory.
  #For a --diff run of a model change, so the changed model becomes the baseline
  #for whatever comes after it without a second run.
  saveTo <- Sys.getenv("VFT_ABM_SAVE_TO", "")
  if(nzchar(saveTo)){
    dir.create(saveTo, recursive = TRUE, showWarnings = FALSE)
    saveRDS(list(fp = fp, secs = res$secs, msgs = res$msgs),
            file.path(saveTo, paste0(tag, ".rds")))
  }
  if(saveRef){
    saveRDS(list(fp = fp, secs = res$secs, msgs = res$msgs), ref)
    cat("   reference written:", ref, "\n")
  }else if(file.exists(ref)){
    R0  <- readRDS(ref)
    bad <- compareFP(R0$fp, fp)
    cat(sprintf("   reference %.2f s -> now %.2f s  (%.2fx)\n",
                R0$secs, res$secs, R0$secs / res$secs))
    if(length(bad)){
      if(diffOK){
        cat("   results differ (expected with --diff):\n")
        describeShift(R0$fp, fp)
      }else{
        failed <- TRUE
        cat("   FAIL - differs from reference in:", paste(bad, collapse = ", "), "\n")
        describeShift(R0$fp, fp)
      }
    }else{
      cat("   PASS - identical passage counts, start nodes and agent count\n")
    }
  }else{
    cat("   (no reference yet - run with --save-ref first)\n")
  }
}

if(failed) quit(status = 1)
cat("\nOK\n")
