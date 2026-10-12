## Browser check of the cold start (2026-10-12), in headless Chrome against the
## INSTALLED app, which this script starts itself:
##
##   1. the page shell is on screen soon after the port opens, with a "loading"
##      card over the step 1 map until the map arrives
##   2. the first session gets the startup timings in its browser console
##   3. step 1's base map still has tmap's layer control and border
##   4. a job started while the daemons are still warming (VFT_WARM_DELAY_S
##      stretches the warm-up), by the ONLY user, runs at once on the main
##      thread - no waiting, no red bar; and a closed ring appears promptly
##   5. the same with a second user connected: the red "warming up" bar
##      shows, then goes away and lets the job run in a daemon
##
## Run:  Rscript data-raw/verify_startup_browser.R
## Uses whatever visitorFlowTool R_LIBS / .libPaths() finds, so reinstall first.
## On this machine set PROJ_LIB to terra's proj dir (memory visitorflow-proj-lib-shadow).
PORT  <- 7815L
URL   <- sprintf("http://127.0.0.1:%d", PORT)
DELAY <- 100
SHOTS <- Sys.getenv("VFT_SHOTS", file.path(tempdir(), "startup_shots"))
dir.create(SHOTS, showWarnings = FALSE, recursive = TRUE)

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}
since <- function(t0) as.numeric(Sys.time() - t0, units = "secs")

#### start the app, cold ####
t0 <- Sys.time()
px <- processx::process$new(file.path(R.home("bin"), "Rscript.exe"),
  c("-e", sprintf("shiny::runApp(system.file('app', package = 'visitorFlowTool'), port = %d, launch.browser = FALSE)", PORT)),
  stdout = "|", stderr = "2>&1",
  env = c("current", VFT_WARM_DELAY_S = DELAY, VFT_NAV = "1", VFT_PERF = Sys.getenv("VFT_PERF", "0")))
stopApp <- function() {
  kids <- tryCatch(ps::ps_children(ps::ps_handle(px$get_pid()), recursive = TRUE),
                   error = function(e) list())
  for (k in kids) try(ps::ps_kill(k), silent = TRUE)
  px$kill()
}
repeat {
  up <- tryCatch(curl::curl_fetch_memory(URL)$status_code == 200, error = function(e) FALSE)
  if (up || !px$is_alive() || since(t0) > 300) break
  Sys.sleep(0.2)
}
tPage <- since(t0)
ok("the app serves its first page", up, sprintf("(%.1f s after spawn)", tPage))

b <- chromote::ChromoteSession$new(width = 1600, height = 1000)
console <- character(0)
invisible(b$Runtime$enable())
b$Runtime$consoleAPICalled(callback_ = function(m)
  console <<- c(console, paste(vapply(m$args, function(a) as.character(a$value %||% ""), ""),
                               collapse = " ")))
`%||%` <- function(a, b) if (is.null(a)) b else a
js <- function(x) b$Runtime$evaluate(x, returnByValue = TRUE)$result$value
waitFor <- function(expr, secs = 30, step = 0.2) {
  t1 <- Sys.time()
  repeat {
    v <- tryCatch(js(expr), error = function(e) NULL)
    if (isTRUE(v)) return(TRUE)
    if (since(t1) > secs) return(FALSE)
    later::run_now(step)
  }
}
shot <- function(name) {
  png <- tryCatch(b$Page$captureScreenshot(format = "png")$data, error = function(e) NULL)
  if (is.null(png)) return(invisible(NULL))   #a busy machine; a screenshot is not a check
  writeBin(jsonlite::base64_dec(png), file.path(SHOTS, paste0(name, ".png")))
}
mouse <- function(type, x, y)
  b$Input$dispatchMouseEvent(type = type, x = x, y = y, button = "left", clickCount = 1)
move  <- function(x, y) { b$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y); Sys.sleep(0.06) }
click <- function(x, y) { move(x, y); mouse("mousePressed", x, y); mouse("mouseReleased", x, y); Sys.sleep(0.15) }
rectOf <- function(sel) js(sprintf(
  "(function(){ var e = document.querySelector(%s); if(!e) return null;
     var r = e.getBoundingClientRect(); return [r.left, r.top, r.width, r.height]; })()",
  jsonlite::toJSON(sel, auto_unbox = TRUE)))
clickEl <- function(sel) { r <- unlist(rectOf(sel)); if (is.null(r)) return(FALSE)
  click(r[1] + r[3] / 2, r[2] + r[4] / 2); TRUE }
MAP <- "#step1-areaSelectMap"
mapLoaded <- sprintf("(function(){ var w = HTMLWidgets.find('%s'); var m = w && w.getMap && w.getMap();
                       return !!m && !!m._loaded; })()", MAP)

cat("=== 1. page shell first, then the map ===\n")
tNav <- Sys.time()
invisible(b$Page$navigate(URL, wait_ = FALSE))
ok("the nav bar is on screen", waitFor("!!document.getElementById('vftNav')", 60),
   sprintf("(%.1f s after navigating)", since(tNav)))
ok("...with the loading card over the map",
   isTRUE(js("!!document.getElementById('step1-areaSelectMap-loading')")) ||
     isTRUE(js(mapLoaded)))
shot("1_shell")
ok("the map arrives", waitFor(mapLoaded, 120), sprintf("(%.1f s after navigating)", since(tNav)))
ok("...and the loading card is gone",
   waitFor("!document.getElementById('step1-areaSelectMap-loading')", 10))
Sys.sleep(1); shot("2_map")

cat("\n=== 2. startup timings in the console ===\n")
invisible(waitFor("false", 3))
st <- grep("visitorFlowTool startup", console, value = TRUE)
cat(paste0("  | ", st, collapse = "\n"), "\n")
ok("the first session got the startup table", length(st) >= 4)
ok("...including the port opening and the first session",
   any(grepl("port opens", st)) && any(grepl("first session started", st)))

cat("\n=== 3. step 1's base map ===\n")
ok("tmap's layer control is there (3 basemaps + the border)",
   isTRUE(js("document.querySelectorAll('.leaflet-control-layers-base input').length === 3 &&
              document.querySelectorAll('.leaflet-control-layers-overlays input').length === 1")))
ok("the border is drawn in its own pane",
   isTRUE(js("!!document.querySelector('.leaflet-tmap405-pane path')")))
ok("...in dark green", isTRUE(js(
  "(function(){ var p = document.querySelector('.leaflet-tmap405-pane path');
     return !!p && p.getAttribute('stroke') === '#006400'; })()")))

#draw a triangle near Birmensdorf, confirm it and choose the simulation - which
#enters step 3, whose attractiveness data is the first daemon job of a visit.
#Returns how long the closed ring took to appear on the map.
drawAndGo <- function() {
  invisible(js("localStorage.setItem('vft.tutorial.v1', JSON.stringify({status: 'done', at: Date.now()}))"))
  invisible(b$Page$reload())
  ok("  reconnected", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 60))
  ok("  map again", waitFor(mapLoaded, 60))
  Sys.sleep(1)
  invisible(js("void $('.modal').modal('hide')")); Sys.sleep(0.6)
  invisible(js(sprintf("void HTMLWidgets.find('%s').getMap().setView([47.3565, 8.4375], 15)", MAP)))
  Sys.sleep(1.5)
  mp <- unlist(rectOf(MAP)); cx <- mp[1] + mp[3] / 2; cy <- mp[2] + mp[4] / 2
  pts <- list(c(cx - 120, cy - 90), c(cx + 120, cy - 90), c(cx, cy + 110))
  for (q in pts) click(q[1], q[2])
  move(cx, cy); click(pts[[1]][1], pts[[1]][2])
  tClose <- Sys.time()
  drawn <- waitFor("!!document.querySelector('#step1-areaSelectMap .leaflet-layer2-pane path')", 90, 0.05)
  tRing <- since(tClose)
  ok("  the closed ring appears", drawn, sprintf("(%.2f s after closing it)", tRing))
  ok("  the confirm button shows", waitFor(
    "(function(){ var e = document.getElementById('step1-confirmButton2'); return !!e && e.offsetParent !== null; })()", 30))
  Sys.sleep(0.5); clickEl("#step1-confirmButton2")
  ok("  the next-step modal", waitFor("!!document.getElementById('vftNextSim') && document.getElementById('vftNextSim').offsetParent !== null", 120))
  Sys.sleep(0.8); clickEl("#vftNextSim")
  tRing
}
warmBar <- "(function(){ var q = document.querySelectorAll('.shiny-progress-notification.vft-queued');
  for(var i = 0; i < q.length; i++) if(q[i].textContent.indexOf('hochgefahren') >= 0) return true;
  return false; })()"
step3Up <- "(function(){ var e = document.querySelector('#step3-AOISlider'); return !!e && e.offsetParent !== null; })()"
warmed <- function() any(grepl("daemons warmed", out <<- c(out, px$read_output_lines())))
out <- character(0)

cat("\n=== 4. one user, daemons still warming: the job runs at once, inline ===\n")
tRing1 <- drawAndGo()
sawBar <- FALSE; t1 <- Sys.time()
repeat {
  if (isTRUE(js(warmBar))) sawBar <- TRUE
  if (isTRUE(js(step3Up)) || since(t1) > 120) break
  later::run_now(0.1)
}
ok("step 3's map appears", isTRUE(js(step3Up)), sprintf("(%.1f s after choosing it)", since(t1)))
ok("...before the daemons had finished warming", !warmed())
ok("...with no 'warming up' bar", !sawBar)
shot("3_step3_inline")

cat("\n=== 5. two users, daemons still warming: the job waits behind a red bar ===\n")
b2 <- chromote::ChromoteSession$new(width = 1200, height = 800)
invisible(b2$Page$navigate(URL, wait_ = FALSE))
Sys.sleep(5)   #the second session connects
tRing2 <- drawAndGo()
seen <- waitFor(warmBar, 60)
ok("the red 'warming up' bar appears", seen && !warmed(),
   sprintf("(%.0f s after spawn, warm-up delay %d s)", since(t0), DELAY))
if (seen) shot("4_warming_bar")
ok("...and goes away once the daemons are warm",
   waitFor(sprintf("!(%s)", warmBar), DELAY + 120), sprintf("(%.0f s after spawn)", since(t0)))
ok("the job then runs: step 3's map appears", waitFor(step3Up, 120))
ok("no error notification", isTRUE(js("!document.querySelector('.shiny-notification-error')")))
shot("5_step3")
b2$close()

out <- c(out, px$read_output_lines())
cat(paste0("  | ", grep("startup|worker pids|WARNING|rror", out, value = TRUE), collapse = "\n"), "\n")
b$close(); stopApp()
cat(sprintf("\nScreenshots in %s\n%s: %d failure(s)\n", SHOTS,
            if (fails == 0) "ALL PASS" else "FAILED", fails))
quit(status = if (fails == 0) 0 else 1)
