## Verification of the cold-start work (2026-10-12).
##
## Shiny Server stops the R process a few seconds after the last visitor leaves,
## so the next visitor waits through a whole cold start - and Shiny only opens
## its port once global.R has returned. global.R used to warm the daemon pool,
## simplify the protected areas and load tmap before that point, and the page
## stayed blank for all of it (89 s on the dev box, while other jobs were running).
## Each of those moved, and each move is checked here:
##
##   1. step 1's base map, written with leaflet, emits the same widget tmap did
##   2. the simplified protected areas are built once, then read from disk
##   3. a job sent while the daemons are still warming waits for them - it does
##      not run inline on the main thread and does not restart the pool
##   4. (with --ttfb) time from spawning the INSTALLED app to the first page
##
## Run:  Rscript data-raw/verify_startup.R [--ttfb] [--quick]   (--quick: section 1 only)
## Sections 3 and 4 use the installed package in the daemons / the app, so
## reinstall first: devtools::install(upgrade = FALSE).
## On this machine set PROJ_LIB to terra's proj dir first (see memory
## visitorflow-proj-lib-shadow).
suppressPackageStartupMessages({library(sf); library(mirai); library(promises)})
RD <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
args <- commandArgs(TRUE)

#the helpers under test come from the working tree; the namespace is attached
#so anything they call that is not sourced here still resolves
suppressPackageStartupMessages(library(visitorFlowTool))
for (f in c("perf_helpers.R", "data_paths.R", "async_helpers.R"))
  sys.source(file.path(RD, f), envir = globalenv())
for (e in parse(file.path(RD, "step1_server.R")))
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      as.character(e[[2]]) %in% c("vftCountryBaseMap", "vftOffscreenDevice"))
    eval(e, globalenv())

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-64s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}
secs <- function(expr) { t0 <- Sys.time(); force(expr); as.numeric(Sys.time() - t0, units = "secs") }
#drive later()'s loop until `cond()` holds or `max` seconds pass
pump <- function(cond, max = 60) {
  t0 <- Sys.time()
  while (!cond() && as.numeric(Sys.time() - t0, units = "secs") < max) later::run_now(0.1)
  cond()
}

cat("=== 1. step 1 base map: leaflet calls == tmap's widget ===\n")
countryshape <- sf::st_read(vftData("maps/countryBorders/swissBorder_final.gpkg"), quiet = TRUE)
sf::st_crs(countryshape) <- 4326
suppressMessages(tmap::tmap_mode("view"))
a <- vftOffscreenDevice(tmap::tmap_leaflet(
  tmap::tm_shape(countryshape) +
    tmap::tm_borders(col = "darkgreen", lwd = 3, zindex = 405),
  options = leaflet::leafletOptions(doubleClickZoom = FALSE), in.shiny = TRUE))
b <- vftCountryBaseMap(countryshape)
#strip classes such as leaflet_crs, which jsonlite has no method for
uncl <- function(x) {
  if (is.list(x)) { nm <- names(x); x <- lapply(unclass(x), uncl); names(x) <- nm }
  x
}
J <- function(x) jsonlite::toJSON(uncl(x), auto_unbox = TRUE, digits = NA, null = "null")
ok("same top-level widget fields", identical(sort(names(a$x)), sort(names(b$x))),
   paste(names(b$x), collapse = ","))
for (k in names(a$x)) if (k != "calls")
  ok(sprintf("  x$%s identical", k), identical(J(a$x[[k]]), J(b$x[[k]])))
ok("same number of calls", length(a$x$calls) == length(b$x$calls))
for (i in seq_along(a$x$calls)) {
  same <- identical(J(a$x$calls[[i]]), J(b$x$calls[[i]]))
  ok(sprintf("  call %d (%s) identical", i, a$x$calls[[i]]$method), same)
  if (!same) { cat("    tmap:   ", substr(J(a$x$calls[[i]]), 1, 400), "\n")
               cat("    leaflet:", substr(J(b$x$calls[[i]]), 1, 400), "\n") }
}
depName <- function(m) vapply(m$dependencies, function(d) paste(d$name, d$version), "")
ok("same html dependencies", identical(depName(a), depName(b)))
ok("tmap was not needed to build it",
   !any(grepl("tmap::", deparse(body(vftCountryBaseMap)), fixed = TRUE)))

cat("\n=== 1b. the page itself (installed app_ui) ===\n")
suppressPackageStartupMessages(library(shiny))
html <- paste(shiny:::renderPage(visitorFlowTool:::app_ui()), collapse = "")
count <- function(pat) lengths(regmatches(html, gregexpr(pat, html, fixed = TRUE)))
ok("the translation table is in the page once, not once per step",
   count("var i18n_translations") == 1, sprintf("(%d)", count("var i18n_translations")))
ok("...one shiny-i18n.js and one i18n-state input",
   count("shiny-i18n.js") == 1 && count('id="i18n-state"') == 1)
ok("the page is under 0.6 MB (was 1.44 MB)", nchar(html, "bytes") < 0.6 * 2^20,
   sprintf("(%.2f MB)", nchar(html, "bytes") / 2^20))
ok("step 1's map ships with its loading card",
   count('id="step1-areaSelectMap-loading"') == 1)
cat("\n=== 1c. a closed ring in step 1 does not load terra ===\n")
#a fresh process: this one has terra loaded already (tmap, above)
fresh <- callr::r(function(){
  suppressPackageStartupMessages(library(visitorFlowTool))
  ring <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(cbind(
    c(8.43, 8.45, 8.44, 8.43), c(47.36, 47.36, 47.35, 47.36)))), crs = 4326))
  a <- visitorFlowTool:::paintAreaTooLarge(ring)
  b <- visitorFlowTool:::aoiTooLargeForServer(ring)
  list(terra = "terra" %in% loadedNamespaces(), a = a, b = b)
})
ok("the area checks run without loading terra", !fresh$terra && identical(fresh$a, FALSE) &&
     identical(fresh$b, FALSE))
set.seed(1)
agree <- vapply(1:200, function(i){
  x <- 2600000 + runif(1, -50000, 50000); y <- 1200000 + runif(1, -30000, 30000)
  w <- runif(1, 1, 20000); h <- runif(1, 1, 20000)
  shp <- sf::st_sfc(sf::st_polygon(list(cbind(c(x, x + w, x + w, x, x), c(y, y, y + h, y + h, y)))),
                    crs = 2056)
  res <- visitorFlowTool:::PAINT_RES
  isTRUE(all.equal(visitorFlowTool:::paintWindowCellsOf(shp, res),
                   visitorFlowTool:::paintWindowCells(visitorFlowTool:::paintWindowExt(shp, res), res)))
}, logical(1))
ok("...and count the same cells as the terra version (200 windows)", all(agree))

cat("\n=== 1d. the page is rendered once ===\n")
serve <- visitorFlowTool:::vftCachedUi(visitorFlowTool:::app_ui())
tFirst  <- secs(r1 <- serve(list()))
tSecond <- secs(r2 <- serve(list()))
ok("the cached ui serves a finished httpResponse", inherits(r1, "httpResponse") &&
     r1$status == 200L)
#nchar, not identical(): tab ids are random per build
ok("...the same page Shiny would render", nchar(paste(r1$content, collapse = "")) == nchar(html))
ok("...rendered once: the second request is free", tSecond < 0.05,
   sprintf("(first %.2f s, second %.3f s)", tFirst, tSecond))

if (!"--quick" %in% args) {
cat("\n=== 2. protected areas: built once, then read from disk ===\n")
cacheDir <- file.path(tempdir(), "vft_pa_cache_test")
unlink(cacheDir, recursive = TRUE)
Sys.setenv(R_USER_CACHE_DIR = cacheDir)
src <- vftData("maps/protectedAreas/PA_all.gpkg")
shipped <- .vftPAShippedFile(src, VFT_PA_TOLERANCE_M)
if (file.exists(shipped))
  cat("  NOTE: a shipped sidecar exists, the build path below reads it instead\n")
ok("no user cache before the first build",
   is.null(.vftPAUserFile(src, VFT_PA_TOLERANCE_M)) ||
     !file.exists(.vftPAUserFile(src, VFT_PA_TOLERANCE_M)))
suppressWarnings(rm(".vft_PA_simplified", ".vft_PA_crs", envir = .GlobalEnv))
tBuild <- secs(s1 <- vftProtectedAreasCached())
uf <- .vftPAUserFile(src, VFT_PA_TOLERANCE_M)
ok("first call writes the user cache", file.exists(uf),
   sprintf("(%.1f s, %s)", tBuild, basename(uf)))
ok("no temp file left behind", !length(list.files(dirname(uf), "\\.tmp")))
suppressWarnings(rm(".vft_PA_simplified", ".vft_PA_crs", envir = .GlobalEnv))
tRead <- secs(s2 <- vftProtectedAreasCached())
ok("second process start reads it back in < 1.5 s", tRead < 1.5, sprintf("(%.2f s)", tRead))
ok("same geometry", isTRUE(all(sf::st_equals(s1, s2, sparse = FALSE)[cbind(seq_len(nrow(s1)), seq_len(nrow(s1)))])))
ok("same area to 4 dp", isTRUE(all.equal(round(as.numeric(sum(sf::st_area(s1))), 4),
                                          round(as.numeric(sum(sf::st_area(s2))), 4))))
ok("same source crs restored", identical(.GlobalEnv$.vft_PA_crs, sf::st_crs(sf::st_read(src, quiet = TRUE))))
shape <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = 7.2, ymin = 46.15, xmax = 7.45, ymax = 46.3), crs = 4326)))
ok("vftProtectedAreas() still clips on the cached layer",
   inherits(vftProtectedAreas(shape), "sf"))
writeLines("not an rds", uf)
suppressWarnings(rm(".vft_PA_simplified", ".vft_PA_crs", envir = .GlobalEnv))
ok("a corrupt cache file is rebuilt, not an error",
   inherits(tryCatch(vftProtectedAreasCached(), error = function(e) e), "sf"))

cat("\n=== 3. a job sent during the warm-up waits for it ===\n")
Sys.setenv(VFT_WARM_DELAY_S = "6")
st <- .vftP()
mainPid <- Sys.getpid()
t0 <- Sys.time()
starts <- logical(0)   #the `wait` of every .vftStartDaemons() call
trace(".vftStartDaemons", quote(starts <<- c(starts, isTRUE(wait))), print = FALSE,
      where = globalenv())
tSoon <- secs(.vftStartDaemonsSoon(2, fallback = 120))
ok("global.R's request for a pool returns at once", tSoon < 0.5, sprintf("(%.2f s)", tSoon))
ok("...spawning nothing yet: pool pending, not ready",
   identical(st$warmState, "pending") && !vftDaemonsReady() && !length(starts))
ok(".vftEnsureDaemons() leaves a pending pool alone",
   is.na(.vftEnsureDaemons()) && !length(starts))
res <- NULL; err <- NULL
st$liveSessions <- 2L   #two users: the job must wait for the pool, not run inline
p <- vftFuture({ Sys.getpid() })
p <- promises::then(p, function(v) res <<- v, function(e) err <<- e)
ok("an early job starts the pool itself, without waiting",
   identical(starts, FALSE) && identical(st$warmState, "warming"))
ok("...and has not run while warming", is.null(res) && is.null(err))
ok(".vftEnsureDaemons() leaves a warming pool alone",
   is.na(.vftEnsureDaemons()) && identical(starts, FALSE))
.vftDaemonsKick()
ok("a second kick does not start a second pool", identical(starts, FALSE))
pump(function() !is.null(res) || !is.null(err), max = 90)
waited <- as.numeric(Sys.time() - t0, units = "secs")
untrace(".vftStartDaemons", where = globalenv())
ok("the job resolved", !is.null(res), if (!is.null(err)) conditionMessage(err) else "")
ok("...in a daemon, not inline on the main thread",
   is.numeric(res) && res != mainPid, sprintf("(pid %s, main %d)", format(res), mainPid))
ok("...after the warm-up delay", waited >= 6, sprintf("(%.1f s)", waited))
ok("the pool was started exactly once", identical(starts, FALSE))
ok("pool reports ready", vftDaemonsReady() && identical(st$warmState, "ready"))
tReady <- secs({res <- NULL
  promises::then(vftFuture({ Sys.getpid() }), function(v) res <<- v)
  pump(function() !is.null(res), 30)})
ok("once warm, a job is dispatched straight away", !is.null(res) && tReady < 3,
   sprintf("(%.2f s)", tReady))

cat("\n=== 3b. ...unless its session is alone: then it runs inline at once ===\n")
mirai::daemons(0)
.vftStartDaemons(1, wait = FALSE)
ok("a fresh pool is warming again", identical(st$warmState, "warming"))
st$liveSessions <- 1L
res <- NULL
tIn <- secs({promises::then(vftFuture({ Sys.getpid() }), function(v) res <<- v)
  pump(function() !is.null(res), 10)})
ok("one session: the job ran on the main thread", identical(res, mainPid),
   sprintf("(pid %s)", format(res)))
ok("...without waiting for the warm-up", tIn < 3 && identical(st$warmState, "warming"),
   sprintf("(%.2f s)", tIn))
st$liveSessions <- 0L
mirai::daemons(0)
Sys.unsetenv("VFT_WARM_DELAY_S")
}

if ("--ttfb" %in% args) {
  cat("\n=== 4. cold start of the installed app: time to first page ===\n")
  port <- 7813L
  t0 <- Sys.time()
  px <- processx::process$new(file.path(R.home("bin"), "Rscript.exe"),
    c("-e", sprintf("shiny::runApp(system.file('app', package = 'visitorFlowTool'), port = %d, launch.browser = FALSE)", port)),
    stdout = "|", stderr = "2>&1", env = c("current", VFT_PERF = "0"))
  code <- NA
  repeat {
    code <- tryCatch(curl::curl_fetch_memory(sprintf("http://127.0.0.1:%d/", port))$status_code,
                     error = function(e) NA)
    if (isTRUE(code == 200) || !px$is_alive() ||
        as.numeric(Sys.time() - t0, units = "secs") > 300) break
    Sys.sleep(0.2)
  }
  ttfb <- as.numeric(Sys.time() - t0, units = "secs")
  ok("first page served", isTRUE(code == 200), sprintf("(%.1f s after spawn)", ttfb))
  Sys.sleep(8)
  out <- px$read_output_lines()
  cat(paste0("  | ", grep("startup|worker pids|Listening", out, value = TRUE), collapse = "\n"), "\n")
  #the whole process tree: the app and its daemons
  kids <- tryCatch(ps::ps_children(ps::ps_handle(px$get_pid()), recursive = TRUE),
                   error = function(e) list())
  for (k in kids) try(ps::ps_kill(k), silent = TRUE)
  px$kill()
}

cat(sprintf("\n%s: %d failure(s)\n", if (fails == 0) "ALL PASS" else "FAILED", fails))
quit(status = if (fails == 0) 0 else 1)
