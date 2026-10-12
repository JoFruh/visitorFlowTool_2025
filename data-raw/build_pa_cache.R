## Write the simplified protected-areas layer beside its source, to ship with the data.
##
## vftProtectedAreasCached() (R/data_paths.R) reads
## maps/protectedAreas/PA_all_simplified_<tol>m.rds when it is newer than
## PA_all.gpkg, so no host ever has to build it. The app can manage without this
## file: it builds the layer once per host in a daemon and keeps it in the user
## cache. This only saves that first build (7-10 s in a daemon) on a fresh server,
## and covers a server whose shiny user has no writable home.
##
## Re-run whenever PA_all.gpkg or VFT_PA_TOLERANCE_M changes, then copy the .rds
## to the server's data directory next to the gpkg.
##
## Run:  Rscript data-raw/build_pa_cache.R
suppressPackageStartupMessages(library(visitorFlowTool))
src <- visitorFlowTool:::vftData("maps/protectedAreas/PA_all.gpkg")
tol <- visitorFlowTool:::VFT_PA_TOLERANCE_M
out <- visitorFlowTool:::.vftPAShippedFile(src, tol)
t0 <- Sys.time()
hit <- visitorFlowTool:::.vftPABuild(src, tol)
stopifnot(visitorFlowTool:::.vftPAWriteCache(hit, out))
cat(sprintf("wrote %s (%.1f MB) in %.1f s\n", out, file.size(out) / 2^20,
            as.numeric(Sys.time() - t0, units = "secs")))
