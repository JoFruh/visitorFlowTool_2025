## Browser check of step 3's tour (inst/app/www/vft-tutorial.js), in the REAL
## app, in headless Chrome, with real mouse events through the DevTools
## protocol. Step 1's tour is walked quickly to get there: the simulation is
## chosen, and step 3's tour has to start by itself when the ring lands on step
## 3 (chaining). Then its five hints: the simulation's three nav buttons, step
## 3's own, the threshold slider (a slide that stays above 8 must not move on,
## one down to 8 or below must), the map, and confirm - whose tap ends the tour.
## data-raw/verify_tutorial_browser.R covers step 1's tour in detail.
##
## Needs the app running with the nav bar on, e.g.
##   VFT_NAV=1, pkgload::load_all("."), shiny::runApp(system.file("app",
##   package = "visitorFlowTool"), port = 7781)
## Run:  Rscript data-raw/verify_tutorial_step3_browser.R [url]
args <- commandArgs(trailingOnly = TRUE)
URL  <- if (length(args)) args[[1]] else Sys.getenv("VFT_URL", "http://127.0.0.1:7781")
SHOTS <- Sys.getenv("VFT_SHOTS", file.path(tempdir(), "tutorial_step3_shots"))
dir.create(SHOTS, showWarnings = FALSE, recursive = TRUE)

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

b <- chromote::ChromoteSession$new(width = 1600, height = 1000)
js <- function(x) b$Runtime$evaluate(x, returnByValue = TRUE)$result$value
waitFor <- function(expr, secs = 30, step = 0.25) {
  t0 <- Sys.time()
  repeat {
    v <- tryCatch(js(expr), error = function(e) NULL)
    if (isTRUE(v)) return(TRUE)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > secs) return(FALSE)
    Sys.sleep(step)
  }
}
shot <- function(name) {
  png <- b$Page$captureScreenshot(format = "png")$data
  writeBin(jsonlite::base64_dec(png), file.path(SHOTS, paste0(name, ".png")))
}
mouse <- function(type, x, y, count = 1)
  b$Input$dispatchMouseEvent(type = type, x = x, y = y, button = "left", clickCount = count)
move  <- function(x, y) { b$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y); Sys.sleep(0.06) }
click <- function(x, y) {
  move(x, y); mouse("mousePressed", x, y); mouse("mouseReleased", x, y); Sys.sleep(0.12)
}
#a drag with the button held, in small steps, as a hand would
drag <- function(x0, y0, x1, y1, n = 12) {
  move(x0, y0); mouse("mousePressed", x0, y0)
  for (s in seq_len(n)) {
    b$Input$dispatchMouseEvent(type = "mouseMoved", x = x0 + (x1 - x0) * s / n,
                               y = y0 + (y1 - y0) * s / n, button = "left", buttons = 1)
    Sys.sleep(0.03)
  }
  mouse("mouseReleased", x1, y1); Sys.sleep(0.12)
}
centre <- function(sel) js(sprintf(
  "(function(){ var e = document.querySelector(%s); if(!e) return null;
     var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()",
  jsonlite::toJSON(sel, auto_unbox = TRUE)))
clickEl <- function(sel) {
  p <- centre(sel); if (is.null(p)) return(invisible(FALSE))
  click(p[[1]], p[[2]]); invisible(TRUE)
}
tut    <- function() js("window.vftTutorialState ? vftTutorialState() : null")
atHint <- function(i, key = "step3") sprintf("(function(){ var s = vftTutorialState();
                                 return s.key === '%s' && s.idx === %d && !s.quiet && !s.pause; })()", key, i - 1)
wins   <- function() {
  w <- js("(function(){ var c = document.querySelector('.vftTutorialCanvas');
             return c ? c.getAttribute('data-windows') : null; })()")
  if (is.null(w)) list() else jsonlite::fromJSON(w, simplifyVector = FALSE)
}
rectOf <- function(sel) js(sprintf(
  "(function(){ var e = document.querySelector(%s); if(!e) return null;
     var r = e.getBoundingClientRect(); return [r.left, r.top, r.width, r.height]; })()",
  jsonlite::toJSON(sel, auto_unbox = TRUE)))
near <- function(w, r, tol = 12) {
  if (is.null(w) || is.null(r)) return(FALSE)
  w <- unlist(w); r <- unlist(r)
  all(abs(c(w[1] - r[1], w[2] - r[2], (w[1] + w[3]) - (r[1] + r[3]),
            (w[2] + w[4]) - (r[2] + r[4]))) <= tol)
}
ringIs <- function(id) sprintf("(function(){ var e = document.querySelector('#vftNav .vft-nav-current');
                                  return !!e && e.id === '%s'; })()", id)
stored <- function() js("(function(){ try { var s = localStorage.getItem('vft.tutorial.v1'); return s ? JSON.parse(s).status : null; } catch(e){ return null; } })()")
hasNext <- function() isTRUE(js("!!document.querySelector('.vftTutorialBox .vftTutorialNext')"))
slider  <- function() js("jQuery('#step3-AOISlider').data('ionRangeSlider').result.from_value")
MAP <- "#step1-areaSelectMap"

#### open the app, no bubble ####
invisible(b$Page$navigate(URL))
ok("the app connects", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
invisible(js("localStorage.setItem('vft.tutorial.v1', JSON.stringify({status: 'done', at: Date.now()}))"))
invisible(b$Page$reload())
Sys.sleep(1)
ok("...again", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
invisible(waitFor(sprintf("(function(){ var w = HTMLWidgets.find('%s'); var m = w && w.getMap && w.getMap();
                            return !!m && !!m._loaded; })()", MAP), 60))
Sys.sleep(1.5)

cat("=== 1. to step 3, through step 1's tour ===\n")
invisible(js("vftTutorialStart('step1')"))
ok("step 1's tour starts", waitFor(atHint(1, "step1"), 20))
clickEl(".vftTutorialNext")
invisible(waitFor(atHint(2, "step1"), 10)); clickEl(".vftTutorialNext")
ok("...hint 3, over Birmensdorf", waitFor(atHint(3, "step1"), 15))
Sys.sleep(0.6)
mp <- unlist(rectOf(MAP)); cx <- mp[1] + mp[3] / 2; cy <- mp[2] + mp[4] / 2
pts <- list(c(cx - 120, cy - 90), c(cx + 120, cy - 90), c(cx, cy + 110))
for (q in pts) click(q[1], q[2])
move(cx, cy); move(pts[[1]][1], pts[[1]][2]); click(pts[[1]][1], pts[[1]][2])
ok("...the polygon is accepted", waitFor(atHint(4, "step1"), 20))
Sys.sleep(0.6); clickEl("#step1-confirmButton2")
ok("...the next-step modal", waitFor(atHint(5, "step1"), 60))
Sys.sleep(0.8); clickEl("#vftNextSim")
ok("...the simulation is held for hint 6", waitFor(atHint(6, "step1"), 10))
Sys.sleep(0.6); clickEl(".vftTutorialNext")
ok("the ring lands on step 3", waitFor(ringIs("vftNav_step3"), 240))

cat("\n=== 2. hint 1: the simulation's three steps ===\n")
ok("step 3's tour starts by itself", waitFor(atHint(1), 120), paste(unlist(tut()), collapse = ","))
ok("...once step 3's page shows, not over step 1's",
   isTRUE(js("(function(){ var e = document.getElementById('step3-AOISlider');
                           return !!e && e.closest('.shiny-input-container').offsetParent !== null; })()")) &&
   !isTRUE(js("(function(){ var e = document.getElementById('step1-areaSelectMap');
                            return !!e && e.offsetParent !== null; })()")))
Sys.sleep(0.8)
three <- js("(function(){ var l = 1e9, t = 1e9, r = -1e9, b = -1e9;
  ['#vftNav_step3', '#vftNav_step4', '#vftNav_step5'].forEach(function (s) {
    var q = document.querySelector(s).getBoundingClientRect();
    l = Math.min(l, q.left); t = Math.min(t, q.top); r = Math.max(r, q.right); b = Math.max(b, q.bottom); });
  return [l, t, r - l, b - t]; })()")
w <- wins()
ok("one window over the three buttons", length(w) == 1 && near(w[[1]], three, 6),
   paste(unlist(w), collapse = ","))
aoi <- js("(function(){ var e = document.querySelector('.vftTutorialText em.vftTutAoi'),
                             t = document.querySelector('.vftTutorialText'); if (!e) return null;
  return [getComputedStyle(e).color, parseFloat(getComputedStyle(e).fontSize), parseFloat(getComputedStyle(t).fontSize)]; })()")
ok("'Zielgebiete' in step 5's green (green4)", !is.null(aoi) && identical(aoi[[1]], "rgb(0, 139, 0)"),
   paste(unlist(aoi), collapse = " "))
ok("...and one size larger", !is.null(aoi) && aoi[[2]] > aoi[[3]])
ok("the card has its Next button", hasNext())
shot("1_sim_steps")
clickEl(".vftTutorialNext")

cat("\n=== 3. hint 2: Define AoIs ===\n")
ok("Next moves on to hint 2", waitFor(atHint(2), 10))
Sys.sleep(0.6)
w <- wins()
ok("one window, on step 3's button", length(w) == 1 && near(w[[1]], rectOf("#vftNav_step3"), 6))
ok("...with a Next button", hasNext())
shot("2_define")
clickEl(".vftTutorialNext")

cat("\n=== 4. hint 3: the slider ===\n")
ok("Next moves on to hint 3", waitFor(atHint(3), 10))
Sys.sleep(0.6)
w <- wins()
box <- js("(function(){ var e = document.getElementById('step3-AOISlider').closest('.shiny-input-container');
           var r = e.getBoundingClientRect(); return [r.left, r.top, r.width, r.height]; })()")
ok("one window, on the slider and its label", length(w) == 1 && near(w[[1]], box, 8))
ok("...and no Next button", !hasNext())
shot("3_slider")
clickEl("#step3-confirmButton3"); Sys.sleep(1)
ok("a tap on confirm, outside the window, is swallowed",
   isTRUE(js(atHint(3))) && isTRUE(js(ringIs("vftNav_step3"))))

#the handle's centre runs from half a handle in from the line's left end (0)
#to half a handle in from its right end (20), over 201 choices
IRS <- "jQuery('#step3-AOISlider').data('ionRangeSlider')"
xFor <- function(v) js(sprintf("(function(){ var d = %s,
  line = d.$cache.line[0].getBoundingClientRect(), hw = d.$cache.s_single[0].getBoundingClientRect().width;
  return line.left + hw / 2 + (line.width - hw) * Math.round(%f * 10) / 200; })()", IRS, v))
handle <- function() js(sprintf("(function(){ var r = %s.$cache.s_single[0].getBoundingClientRect();
  return [r.left + r.width / 2, r.top + r.height / 2]; })()", IRS))
h <- handle()
drag(h[[1]], h[[2]], xFor(10), h[[2]])
Sys.sleep(2)
ok(sprintf("a slide to %s (still above 8) does not move on", slider()), isTRUE(js(atHint(3))))
h <- handle()
drag(h[[1]], h[[2]], xFor(7.9), h[[2]])
ok(sprintf("a slide to %s moves on to hint 4", slider()), waitFor(atHint(4), 20))

cat("\n=== 5. hint 4: the map ===\n")
Sys.sleep(0.6)
ok("the map has redrawn", !isTRUE(js("document.getElementById('step3-AOIMap').classList.contains('recalculating')")))
w <- wins()
ok("one window, on the map", length(w) == 1 && near(w[[1]], rectOf("#step3-AOIMap")))
ok("...with a Next button", hasNext())
shot("4_map")
clickEl(".vftTutorialNext")

cat("\n=== 6. hint 5: confirm ===\n")
ok("Next moves on to hint 5", waitFor(atHint(5), 10))
Sys.sleep(0.6)
w <- wins()
ok("one window, on the confirm button", length(w) == 1 && near(w[[1]], rectOf("#step3-confirmButton3")))
ok("...and no Next button", !hasNext())
shot("5_confirm")
clickEl("#step3-confirmButton3")
ok("the tap ends the tour", waitFor("!document.getElementById('vftTutorial')", 5))
ok("...stored as done", identical(stored(), "done"))
ok("...and the app moves on to step 4", waitFor(ringIs("vftNav_step4"), 120))
Sys.sleep(1.5)
ok("step 4's tour starts by itself", waitFor(atHint(1, "step4"), 120), paste(unlist(tut()), collapse = ","))

cat(sprintf("\nscreenshots in %s\n", SHOTS))
b$close()
cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
