## Verification that the heat job survives the process boundary.
##
## computeHeat() in R/newVersions_server.R dispatches through vftFuture() into a
## mirai daemon, because this deployment is one R process serving every user and
## a synchronous heat computation froze every connected session for its full
## duration - 12.6 s over a 3.6 x 2.6 km area before the model was made faster.
##
## The one thing that cannot be checked by sourcing R/ in a single process is
## the boundary itself: every terra object is an external pointer, and a
## SpatRaster sent or returned unwrapped arrives as a dangling reference rather
## than as an error. Both directions carry one - the version's painted raster
## going out, the heat surface coming back - so both are wrapped, and this runs
## a real daemon to prove it.
##
## It also pins the daemon-side session cache (heatCacheFor), which is where the
## incremental cache went when the work left this process, including that it
## evicts rather than growing without bound: each entry is tens of megabytes and
## a daemon outlives every session it serves.
##
## Run:  Rscript data-raw/verify_heat_async.R
suppressPackageStartupMessages({library(terra); library(sf); library(mirai)})
terraOptions(progress = 0)
RD <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
for (f in c("perf_helpers.R","data_paths.R","paintbrush_helpers.R",
            "heat_helpers.R","shadow_helpers.R","svf_helpers.R"))
  suppressWarnings(try(source(file.path(RD,f)), silent=TRUE))
dyn.load("C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/src/visitorFlowTool.dll")
source(file.path(RD,"RcppExports.R"))

fails <- 0
ok <- function(what, cond, extra="") {
  cat(sprintf("%-58s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

CX <- 2593956; CY <- 1119554
p <- st_sfc(st_polygon(list(rbind(c(CX-700,CY-500),c(CX+700,CY-500),
                                  c(CX+700,CY+500),c(CX-700,CY+500),
                                  c(CX-700,CY-500)))), crs=2056)
aoi <- st_sf(geometry = st_transform(p, 4326))

cat("=== 1. the cache registry ===\n")
c1 <- heatCacheFor("sessionA"); c2 <- heatCacheFor("sessionA")
ok("the same session gets the same cache object", identical(c1, c2))
ok("a different session gets a different one", !identical(c1, heatCacheFor("sessionB")))
ok("a NULL key gets a throwaway cache",
   !identical(heatCacheFor(NULL), heatCacheFor(NULL)))
for (k in paste0("s", 1:8)) heatCacheFor(k)
reg <- get(".vft_heatCaches", envir = .GlobalEnv)
ok(sprintf("at most HEAT_CACHE_SESSIONS caches are kept (%d)", length(reg$.order)),
   length(reg$.order) == HEAT_CACHE_SESSIONS)
ok("...and it is the most recently used ones that survive",
   identical(reg$.order, paste0("s", 5:8)))
ok("...and the evicted ones are really gone from the environment",
   !any(paste0("s", 1:4) %in% ls(reg, all.names = TRUE)))

cat("\n=== 2. wrap/unwrap round trip, same process ===\n")
direct <- heatRaster(aoi, bin = "midday")
packed <- heatRasterPacked(aoi, bin = "midday", key = NULL)
ok("heatRasterPacked returns something serialisable, not a SpatRaster",
   inherits(packed, "PackedSpatRaster"))
back <- terra::unwrap(packed)
ok("...that unwraps to the same surface",
   max(abs(values(back, mat=FALSE) - values(direct, mat=FALSE)), na.rm=TRUE) == 0)
ok("...on the same grid", ext(back) == ext(direct) && all(dim(back) == dim(direct)))

cat("\n=== 3. across a real mirai daemon ===\n")
mirai::daemons(1)
w <- mirai::everywhere({
  .libPaths(..libs..)
  suppressPackageStartupMessages(library(terra))
  for (f in c("perf_helpers.R","data_paths.R","paintbrush_helpers.R",
              "heat_helpers.R","shadow_helpers.R","svf_helpers.R"))
    suppressWarnings(try(source(file.path(..R.., f)), silent = TRUE))
  dyn.load(file.path(dirname(..R..), "src",
                     paste0("visitorFlowTool", .Platform$dynlib.ext)))
  source(file.path(..R.., "RcppExports.R"))
  Sys.getpid()
}, ..libs.. = .libPaths(), ..R.. = RD)
mirai::call_mirai(w)
wpid <- w[[1]]$data
ok(sprintf("a daemon is up and is a different process (%s vs %s)", wpid, Sys.getpid()),
   is.numeric(wpid) && wpid != Sys.getpid())

## a painted stroke has to make the crossing too - that is a SpatRaster as well
stroke <- rast(ext(CX-60, CX+60, CY-60, CY+60), resolution = 1, crs = "EPSG:2056")
values(stroke) <- 7L
ge <- terra::wrap(stroke)

m <- mirai::mirai({
  t0 <- Sys.time()
  out <- heatRasterPacked(aoi, groundEdits = NULL, canopyEdits = ce,
                          bin = "midday", key = "remote")
  list(surface = out, pid = Sys.getpid(),
       secs = as.numeric(difftime(Sys.time(), t0, units="secs")))
}, aoi = aoi, ce = ge)
res <- mirai::call_mirai(m)$data

ok("the job ran and came back without error", !inherits(res, "miraiError"),
   if (inherits(res, "miraiError")) as.character(res) else "")
if (!inherits(res, "miraiError")) {
  ok("it really ran in the daemon, not here", res$pid == wpid)
  ok("the returned surface unwraps on this side",
     inherits(res$surface, "PackedSpatRaster"))
  remote <- terra::unwrap(res$surface)
  local  <- heatRaster(aoi, NULL, stroke, bin = "midday")
  d <- abs(values(remote, mat=FALSE) - values(local, mat=FALSE)); d <- d[is.finite(d)]
  ok(sprintf("...and equals the same job computed here (max %.3g K)", max(d)),
     length(d) > 0 && max(d) < 1e-9)
  ## asked in a SEPARATE task. Asked inside the job, as this check used to be,
  ## it passed while mirai's cleanup = TRUE was deleting the registry between
  ## every pair of jobs - see heatCacheFor().
  kept <- mirai::call_mirai(mirai::mirai(
    exists(".vft_heatCaches") && "remote" %in% ls(.vft_heatCaches)))$data
  ok("the daemon still holds the session's cache in the NEXT task", isTRUE(kept))

  ## second call, same key: the cache must be reused and the answer unchanged
  m2 <- mirai::mirai({
    t0 <- Sys.time()
    out <- heatRasterPacked(aoi, groundEdits = NULL, canopyEdits = ce,
                            bin = "midday", key = "remote")
    list(surface = out, secs = as.numeric(difftime(Sys.time(), t0, units="secs")))
  }, aoi = aoi, ce = ge)
  r2 <- mirai::call_mirai(m2)$data
  d2 <- abs(values(terra::unwrap(r2$surface), mat=FALSE) - values(remote, mat=FALSE))
  d2 <- d2[is.finite(d2)]
  ok("a warm repeat in the daemon is unchanged", length(d2) > 0 && max(d2) < 1e-9)
  ## a repeat of the same job rebuilds nothing but the local term - 0.15 s
  ## against ~2 s here - so a quarter of the cold time is a generous bound.
  ## This is the check that would have caught the registry dying between jobs:
  ## the old one printed 1.52 s and passed.
  ok(sprintf("...and genuinely warm (%.2f s vs %.2f s cold)", r2$secs, res$secs),
     r2$secs < 0.25 * res$secs)
}
mirai::daemons(0)

cat("\n=== 4. two daemons share a session's cache through disk ===\n")
## The production pool has two daemons and a job lands on whichever is free, so
## the second job of a session is often on the daemon that has never seen it.
## Two compute profiles of one daemon each let this test choose: the first job
## on A, the next on B, which must pick A's cache up from `dir` rather than
## building cold - and must still give the surface a cold build would.
dir <- file.path(tempdir(), "vft_heat_async"); unlink(dir, recursive = TRUE)
setup <- function(prof) {
  mirai::daemons(1, .compute = prof)
  w <- mirai::everywhere({
    .libPaths(..libs..)
    suppressPackageStartupMessages(library(terra))
    for (f in c("perf_helpers.R","data_paths.R","paintbrush_helpers.R",
                "heat_helpers.R","shadow_helpers.R","svf_helpers.R"))
      suppressWarnings(try(source(file.path(..R.., f)), silent = TRUE))
    dyn.load(file.path(dirname(..R..), "src",
                       paste0("visitorFlowTool", .Platform$dynlib.ext)))
    source(file.path(..R.., "RcppExports.R"))
    Sys.getpid()
  }, ..libs.. = .libPaths(), ..R.. = RD, .compute = prof)
  mirai::call_mirai(w)
  w[[1]]$data
}
pa <- setup("A"); pb <- setup("B")
ok(sprintf("two distinct daemons (%s, %s)", pa, pb), pa != pb)

job <- function(prof, bin) mirai::call_mirai(mirai::mirai({
  t0 <- Sys.time()
  out <- heatRasterPacked(aoi, bin = bin, key = "shared", cacheDir = dir)
  ## what computeHeat() now does in the daemon: project for the map there
  proj <- terra::wrap(leaflet::projectRasterForLeaflet(terra::unwrap(out), "bilinear"))
  list(surface = out, proj = proj, pid = Sys.getpid(),
       secs = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}, aoi = aoi, bin = bin, dir = dir, .compute = prof))$data

ja <- job("A", "midday")
ok("job 1 ran on daemon A", !inherits(ja, "miraiError") && ja$pid == pa,
   if (inherits(ja, "miraiError")) as.character(ja) else sprintf("(%.2f s cold)", ja$secs))
ok("...and left the session's cache on disk", file.exists(heatCacheFile(dir, "shared")))
jb <- job("B", "afternoon")
ok("job 2 ran on daemon B", !inherits(jb, "miraiError") && jb$pid == pb,
   if (inherits(jb, "miraiError")) as.character(jb) else "")
if (!inherits(jb, "miraiError")) {
  cold <- heatRaster(aoi, bin = "afternoon")
  d <- abs(values(terra::unwrap(jb$surface), mat=FALSE) - values(cold, mat=FALSE))
  d <- d[is.finite(d)]
  ok(sprintf("B's surface equals a cold build (max %.3g K)", max(d)),
     length(d) > 0 && max(d) < 1e-9)
  ok(sprintf("B was warm off A's cache (%.2f s vs %.2f s cold on A)", jb$secs, ja$secs),
     jb$secs < 0.6 * ja$secs)
  ## the projection made in the daemon is the one drawHeat() would have made
  pl <- leaflet::projectRasterForLeaflet(cold, "bilinear")
  pr <- terra::unwrap(jb$proj)
  dp <- abs(values(pr, mat=FALSE) - values(pl, mat=FALSE)); dp <- dp[is.finite(dp)]
  ok("the daemon's leaflet projection equals one made here",
     ext(pr) == ext(pl) && all(dim(pr) == dim(pl)) && length(dp) > 0 && max(dp) < 1e-9)
}
## back on A: its in-memory entry is older than B's file, so the file must win
ja2 <- job("A", "midday")
if (!inherits(ja2, "miraiError")) {
  back <- heatRaster(aoi, bin = "midday")
  d <- abs(values(terra::unwrap(ja2$surface), mat=FALSE) - values(back, mat=FALSE))
  d <- d[is.finite(d)]
  ok(sprintf("A again, after B wrote: still exact (%.2f s)", ja2$secs),
     length(d) > 0 && max(d) < 1e-9)
}
mirai::daemons(0, .compute = "A"); mirai::daemons(0, .compute = "B")
unlink(dir, recursive = TRUE)

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
