## Browser check of step 5's tour (inst/app/www/vft-tutorial.js), in the REAL
## app, in headless Chrome, with real mouse events through the DevTools
## protocol. Steps 1, 3 and 4 are walked quickly to get there (step 2 is
## skipped, so there is no sensitivity matrix and hint 6 is passed over).
## Then the tour, twice:
##   * with the original scenario only - which simulates itself as the page
##     opens, so hint 1 (launch) is passed over and hint 2 waits for that run,
##     on ONE bar for the data (path network included) and one for the ABM;
##     hint 9 ends the tour on "Manage scenarios";
##   * back from the scenarios page, which seeds a second scenario, so hint 9
##     plays variant b (another card) and hint 10 launches it.
## data-raw/verify_tutorial_step3_browser.R covers step 3's tour in detail.
##
## Needs the app running with the nav bar on, e.g.
##   VFT_NAV=1, pkgload::load_all("."), shiny::runApp(system.file("app",
##   package = "visitorFlowTool"), port = 7781)
## Run:  Rscript data-raw/verify_tutorial_step5_browser.R [url]
args <- commandArgs(trailingOnly = TRUE)
URL  <- if (length(args)) args[[1]] else Sys.getenv("VFT_URL", "http://127.0.0.1:7781")
SHOTS <- Sys.getenv("VFT_SHOTS", file.path(tempdir(), "tutorial_step5_shots"))
dir.create(SHOTS, showWarnings = FALSE, recursive = TRUE)

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

b <- chromote::ChromoteSession$new(width = 1600, height = 1000)
## the image hint's export goes all the way: the TIFF lands here
DL <- file.path(tempdir(), "tutorial_step5_dl")
dir.create(DL, showWarnings = FALSE, recursive = TRUE)
invisible(tryCatch(b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(DL)),
                   error = function(e) NULL))
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
atHint <- function(i, key = "step5") sprintf("(function(){ var s = vftTutorialState();
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
#a window lies inside a rect (a map, say)
inside <- function(w, r, tol = 2) {
  w <- unlist(w); r <- unlist(r)
  w[1] >= r[1] - tol && w[2] >= r[2] - tol &&
    w[1] + w[3] <= r[1] + r[3] + tol && w[2] + w[4] <= r[2] + r[4] + tol
}
ringIs <- function(id) sprintf("(function(){ var e = document.querySelector('#vftNav .vft-nav-current');
                                  return !!e && e.id === '%s'; })()", id)
stored <- function() js("(function(){ try { var s = localStorage.getItem('vft.tutorial.v1'); return s ? JSON.parse(s).status : null; } catch(e){ return null; } })()")
hasNext <- function() isTRUE(js("!!document.querySelector('.vftTutorialBox .vftTutorialNext')"))
nextAndWait <- function(i) { Sys.sleep(0.4); clickEl(".vftTutorialNext"); waitFor(atHint(i), 10) }
labelRect <- function(id) js(sprintf("(function(){ var e = document.getElementById('%s').closest('label');
  var r = e.getBoundingClientRect(); return [r.left, r.top, r.width, r.height]; })()", id))
MAP <- "#step1-areaSelectMap"
LAUNCH <- "#step5-launchSim"

#### open the app, no bubble ####
invisible(b$Page$navigate(URL))
ok("the app connects", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
## the save/load card (after the first tour past step 1's) is verify_tutorial_step3_browser.R's
invisible(js("localStorage.setItem('vft.tutorial.v1', JSON.stringify({status: 'done', at: Date.now()})); localStorage.setItem('vft.tutorial.done.v1', JSON.stringify({saveLoad: Date.now()}))"))
invisible(b$Page$reload())
Sys.sleep(1)
ok("...again", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
invisible(waitFor(sprintf("(function(){ var w = HTMLWidgets.find('%s'); var m = w && w.getMap && w.getMap();
                            return !!m && !!m._loaded; })()", MAP), 60))
Sys.sleep(1.5)

cat("=== 1. to step 5, through steps 1, 3 and 4 ===\n")
invisible(js("vftTutorialStart('step1')"))
ok("step 1's tour starts", waitFor(atHint(1, "step1"), 20))
clickEl(".vftTutorialNext")
invisible(waitFor(atHint(2, "step1"), 10)); clickEl(".vftTutorialNext")
invisible(waitFor(atHint(3, "step1"), 15))
Sys.sleep(0.6)
mp <- unlist(rectOf(MAP)); cx <- mp[1] + mp[3] / 2; cy <- mp[2] + mp[4] / 2
pts <- list(c(cx - 120, cy - 90), c(cx + 120, cy - 90), c(cx, cy + 110))
for (q in pts) click(q[1], q[2])
move(cx, cy); move(pts[[1]][1], pts[[1]][2]); click(pts[[1]][1], pts[[1]][2])
invisible(waitFor(atHint(4, "step1"), 20))
Sys.sleep(0.6); clickEl("#step1-confirmButton2")
invisible(waitFor(atHint(5, "step1"), 60))
Sys.sleep(0.8); clickEl("#vftNextSim")
invisible(waitFor(atHint(6, "step1"), 10))
Sys.sleep(0.6); clickEl(".vftTutorialNext")
ok("step 3's tour starts by itself", waitFor(atHint(1, "step3"), 360))
## step 3's and step 4's own tours are covered by their checks: stopped here,
## so the chain ends and steps 3 and 4 are simply confirmed
Sys.sleep(0.6); invisible(js("document.querySelector('.vftTutorialStop').click(); true"))
ok("...stopped", waitFor("!document.getElementById('vftTutorial')", 5))
Sys.sleep(1); clickEl("#step3-confirmButton3")
ok("on step 4", waitFor(ringIs("vftNav_step4"), 180))
ok("...its confirm button shows", waitFor("(function(){ var e = document.getElementById('step4-confirmButton4');
                                              return !!e && e.offsetParent !== null; })()", 180))
Sys.sleep(2); clickEl("#step4-confirmButton4")
ok("on step 5", waitFor(ringIs("vftNav_step5"), 240))
ok("...its launch button shows", waitFor(sprintf("(function(){ var e = document.querySelector('%s');
                                                    return !!e && e.offsetParent !== null; })()", LAUNCH), 240))
ok("the Original simulates itself on arrival: launch disabled",
   waitFor(sprintf("document.querySelector('%s').disabled", LAUNCH), 20))
ok("...a bar is up", waitFor("document.querySelectorAll('#shiny-notification-panel .shiny-notification').length > 0", 60))
Sys.sleep(1)
ok("no tour runs yet (step 3's was stopped, so nothing chained)", is.null(tut()$key))

## every bar the run raises, sampled in the page: how many at once, and their
## captions (the path network's load used to raise a bar of its own)
invisible(js("window.__bars = {max: 0, msgs: {}};
  window.__barTimer = setInterval(function () {
    var l = document.querySelectorAll('#shiny-notification-panel .shiny-notification');
    var n = 0;
    l.forEach(function (e) {
      if (!e.getBoundingClientRect().width) return;
      /* a bar fading out as the next fades in is the hand-over, not two */
      /* (shiny's notifications stand at opacity .85) */
      if (+getComputedStyle(e).opacity > 0.8) { n++; }
      var m = e.querySelector('.progress-message');
      if (m && m.textContent) { __bars.msgs[m.textContent.trim()] = 1; }
    });
    __bars.max = Math.max(__bars.max, n);
  }, 100); true"))

cat("\n=== 2. hint 1 passed over: the run is already going ===\n")
invisible(js("vftTutorialStart('step5')"))

cat("\n=== 3. hint 2: the progress bars, until the simulation is done ===\n")
ok("step 5's tour opens on hint 2", waitFor(atHint(2), 30), paste(unlist(tut()), collapse = ","))
ok("...hint 1 left out of the count (1 / 8)", identical(tut()$count, "1 / 8"), tut()$count)
Sys.sleep(0.8)
bars <- js("(function(){ var l = 1e9, t = 1e9, r = -1e9, b = -1e9;
  document.querySelectorAll('#shiny-notification-panel .shiny-notification').forEach(function (e) {
    var q = e.getBoundingClientRect(); if (!q.width) return;
    l = Math.min(l, q.left); t = Math.min(t, q.top); r = Math.max(r, q.right); b = Math.max(b, q.bottom); });
  return [l, t, r - l, b - t]; })()")
w <- wins()
ok("one window, on the bars bottom right", length(w) == 1 && near(w[[1]], bars, 10),
   paste(unlist(w), collapse = ","))
ok("...and no Next button", !hasNext())
ok("...the arrival text, 'Naherholungssimulation' teal and larger",
   isTRUE(js("(function(){ var t = document.querySelector('.vftTutorialText'), b = t.querySelector('b em');
     return /Ankunft/.test(t.textContent) && !!b && /Naherholungssimulation/.test(b.textContent) &&
            getComputedStyle(b).color === 'rgb(0, 98, 104)'; })()")))
shot("2_progress")
t0 <- Sys.time()
ok("hint 3 once the simulation is drawn", waitFor(atHint(3), 900, 1),
   sprintf("%.0f s", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
ok("...the card of the original now carries its tick",
   isTRUE(js("!!document.querySelector('#placeholder_step5 .btn.withSim')")))
bars <- js("(function(){ clearInterval(window.__barTimer); return {max: __bars.max, msgs: Object.keys(__bars.msgs)}; })()")
ok("one bar at a time", identical(as.integer(bars$max), 1L), bars$max)
## the ABM rewrites its own bar's caption as it goes, so: the data's caption,
## at least one more (the ABM's), and none of the path network's own bar
ok("...one for the data, then the ABM's - none of the path network's own",
   "Daten werden vorbereitet" %in% unlist(bars$msgs) && length(bars$msgs) >= 2 &&
     !any(grepl("^Wegnetz", unlist(bars$msgs))),
   paste(unlist(bars$msgs), collapse = " | "))

cat("\n=== 4. hint 3: the path usage, in the outline ===\n")
Sys.sleep(0.8)
w <- wins()
ok("one window, inside the map", length(w) == 1 && inside(w[[1]], rectOf("#step5-mapAreaLeaflet")),
   paste(unlist(w), collapse = ","))
ok("...on the outline, not the whole map",
   length(w) == 1 && prod(unlist(w[[1]])[3:4]) < 0.98 * prod(unlist(rectOf("#step5-mapAreaLeaflet"))[3:4]))
ok("...with a Next button", hasNext())
use <- js("(function(){ var e = document.querySelector('.vftTutorialText em.vftTutUsage'),
  t = document.querySelector('.vftTutorialText'); if (!e) return null;
  return [e.textContent, getComputedStyle(e).color, parseFloat(getComputedStyle(e).fontSize) > parseFloat(getComputedStyle(t).fontSize)]; })()")
ok("'Wegnutzung' in a busy path's blue, one size larger",
   !is.null(use) && identical(use[[2]], "rgb(24, 45, 181)") && isTRUE(use[[3]]), paste(unlist(use), collapse = " "))
shot("3_usage")

cat("\n=== 5. hint 4: recreationist types and map layers ===\n")
ok("Next moves on to hint 4", nextAndWait(4))
Sys.sleep(0.6)
w <- wins()
ok("two windows, on the two rail cards",
   length(w) == 2 && near(w[[1]], rectOf(".vft5-rail .vft5-agent"), 8) &&
     near(w[[2]], rectOf(".vft5-rail .vftRailCard:nth-child(2)"), 8))
ok("...with a Next button", hasNext())
shot("4_details")
clickEl("#step5-aoi"); Sys.sleep(0.6)
ok("a read-only hint: a tap on a layer switch is swallowed",
   !isTRUE(js("document.getElementById('step5-aoi').checked")))

cat("\n=== 6. hint 5: the starting points ===\n")
ok("Next moves on to hint 5", nextAndWait(5))
Sys.sleep(0.6)
w <- wins()
ok("one window, on the switch row", length(w) == 1 && near(w[[1]], labelRect("step5-startingCheckbox"), 6))
ok("...and no Next button", !hasNext())
shot("5_start")
p <- labelRect("step5-startingCheckbox"); click(p[[1]] + p[[3]] / 2, p[[2]] + p[[4]] / 2)
## no sensitivity matrix (step 2 skipped): hint 6, its switch, is passed over
ok("switching it on moves on past hint 6, to hint 7", waitFor(atHint(7), 10))
ok("...and it is on", isTRUE(js("document.getElementById('step5-startingCheckbox').checked")))
ok("...hint 6 left out of the count (5 / 8)", identical(tut()$count, "5 / 8"), tut()$count)
ok("the starting points lie above the paths",
   isTRUE(js("(function(){ var s = document.querySelector('#step5-mapAreaLeaflet .leaflet-layerStart-pane'),
                                p = document.querySelector('#step5-mapAreaLeaflet .leaflet-layer2-pane');
     return !!s && !!p && s.querySelector('canvas, path') !== null &&
            +getComputedStyle(s).zIndex > +getComputedStyle(p).zIndex; })()")))

cat("\n=== 8. hints 7 and 8: the image, the scenarios ===\n")
Sys.sleep(0.6)
w <- wins()
ok("one window, on the image button", length(w) == 1 && near(w[[1]], rectOf("#step5-imageButton")))
ok("...with a Next button", hasNext())
clickEl("#step5-imageButton")
ok("a tap on it goes through (live): the name modal", waitFor(
  "(function(){ var e = document.getElementById('step5-nameInput'); return !!e && e.offsetParent !== null; })()", 180))
Sys.sleep(1)
w <- wins()
ok("...which gets the window", length(w) == 1 &&
   near(w[[1]], rectOf("#shiny-modal .modal-content"), tol = 8))
ok("...and the card its own text",
   grepl("name", js("document.querySelector('.vftTutorialBox:not(.vftTutorialTip) .vftTutorialText').textContent"),
         ignore.case = TRUE) || grepl("Namen", js("document.querySelector('.vftTutorialBox .vftTutorialText').textContent")))
shot("7b_name")
clickEl("#step5-nameInput")
b$Input$insertText(text = "tutorial")
Sys.sleep(0.6)
ok("...its name typed through", identical(js("document.getElementById('step5-nameInput').value"), "tutorial"))
clickEl("#step5-confirmName")
dlDone <- function() file.exists(file.path(DL, "tutorial.tif"))
t0 <- Sys.time(); while (!dlDone() && difftime(Sys.time(), t0, units = "secs") < 60) Sys.sleep(0.5)
ok("...and the image is downloaded", dlDone(), paste(list.files(DL), collapse = ","))
ok("...the hint still on", isTRUE(js(atHint(7))))
ok("Next moves on to hint 8", nextAndWait(8))
Sys.sleep(0.6)
w <- wins()
ok("one window, over the column's title and its cards",
   length(w) == 1 && unlist(w[[1]])[2] <= unlist(rectOf(".vft-scencol h4"))[2] &&
     near(w[[1]], js("(function(){ var a = document.querySelector('.vft-scencol h4').getBoundingClientRect(),
                                        b = document.querySelector('.vft-scencol .vft-ws-listrow').getBoundingClientRect();
                                    return [a.left < b.left ? a.left : b.left, a.top,
                                            Math.max(a.right, b.right) - Math.min(a.left, b.left), b.bottom - a.top]; })()"), 10))
ok("...the card on its left", {
  bx <- unlist(rectOf(".vftTutorialBox"))
  bx[1] + bx[3] <= w[[1]][[1]] &&
    identical(js("document.querySelector('.vftTutorialBox').getAttribute('data-side')"), "left")
})
ok("...'Szenarien' teal and larger",
   isTRUE(js("(function(){ var b = document.querySelector('.vftTutorialText b em');
     return !!b && b.textContent === 'Szenarien' && getComputedStyle(b).color === 'rgb(0, 98, 104)'; })()")))
shot("8_scenarios")

cat("\n=== 9. hint 9a: only the original - manage scenarios ===\n")
ok("Next moves on to hint 9", nextAndWait(9))
Sys.sleep(0.6)
ok("the base hint (one scenario)", is.null(tut()$variant))
w <- wins()
ok("one window, on 'Manage scenarios'", length(w) == 1 && near(w[[1]], rectOf("#step5-newVersionsButton")))
ok("...and no Next button", !hasNext())
ok("...'Original' dark grey, 'Szenario' teal, both larger",
   isTRUE(js("(function(){ var g = document.querySelector('.vftTutorialText em.vftTutGrey'),
     b = document.querySelector('.vftTutorialText b em');
     return !!g && /Original/.test(g.textContent) && getComputedStyle(g).color === 'rgb(74, 79, 78)' &&
            !!b && b.textContent === 'Szenario' && getComputedStyle(b).color === 'rgb(0, 98, 104)'; })()")))
shot("9a_manage")
clickEl("#step5-newVersionsButton")
ok("the tap ends the tour", waitFor("!document.getElementById('vftTutorial')", 5))
ok("...stored as done", identical(stored(), "done"))
ok("...and the app moves on to the scenarios", waitFor(ringIs("vftNav_newVersions"), 120))
#its own tour (data-raw/verify_tutorial_newVersions_browser.R) follows there;
#stopped at once, this check is about step 5's
ok("...where the scenarios page's tour follows", waitFor(atHint(1, "newVersions"), 240))
invisible(js("document.querySelector('.vftTutorialStop').click()"))

cat("\n=== 10. again with two scenarios: hints 9b and 10 ===\n")
#the page seeds its default "Neu" scenario as it opens
Sys.sleep(5)
clickEl("#vftNav_step5")
ok("back on step 5", waitFor(ringIs("vftNav_step5"), 120))
ok("...with two cards", waitFor("document.querySelectorAll('#placeholder_step5 .btn').length >= 2", 60),
   js("document.querySelectorAll('#placeholder_step5 .btn').length"))
Sys.sleep(2)
invisible(js("vftTutorialStart('step5')"))
ok("the tour starts again", waitFor(atHint(1), 20))
ok("...on the launch hint to tap: the selected scenario has no simulation",
   is.null(tut()$variant) && !hasNext(), paste(unlist(tut()), collapse = ","))
Sys.sleep(0.6); clickEl(LAUNCH)
ok("hint 2 follows the launch, with its own text (2b)",
   waitFor(atHint(2), 30) && identical(tut()$variant, "b") &&
     isTRUE(js("/Wegdaten/.test(document.querySelector('.vftTutorialText').textContent)")),
   paste(unlist(tut()), collapse = ","))
ok("hint 3 once the (selected) scenario is simulated", waitFor(atHint(3), 900, 1))
for (i in 4:4) ok(sprintf("Next to hint %d", i), nextAndWait(i))
Sys.sleep(0.4); ok("Next to hint 5", nextAndWait(5))
Sys.sleep(0.6)
#already on from the first pass: off, then on again
p <- labelRect("step5-startingCheckbox")
if (isTRUE(js("document.getElementById('step5-startingCheckbox').checked"))) {
  click(p[[1]] + p[[3]] / 2, p[[2]] + p[[4]] / 2); Sys.sleep(0.6)
}
click(p[[1]] + p[[3]] / 2, p[[2]] + p[[4]] / 2)
ok("hint 7 (6 passed over again)", waitFor(atHint(7), 10))
for (i in 8:9) ok(sprintf("Next to hint %d", i), nextAndWait(i))
Sys.sleep(0.6)
ok("hint 9 plays variant b", identical(tut()$variant, "b"))
card <- js("(function(){ var cs = document.querySelectorAll('#placeholder_step5 .btn');
  for (var i = 0; i < cs.length; i++) {
    var c = cs[i]; if (!(c.classList.contains('selected') && !c.classList.contains('notSelected'))) return c.id; }
  return null; })()")
w <- wins()
ok(sprintf("one window, on a card not shown (%s)", card),
   !is.null(card) && length(w) == 1 && near(w[[1]], rectOf(paste0("#", card)), 8))
ok("...and no Next button", !hasNext())
shot("9b_card")
clickEl(paste0("#", card))
ok("selecting it moves on to hint 10", waitFor(atHint(10), 10))
ok("...and the map shows it", waitFor(sprintf("(function(){ var c = document.getElementById('%s');
  return c.classList.contains('selected') && !c.classList.contains('notSelected'); })()", card), 20))
Sys.sleep(0.6)
w <- wins()
ok("hint 10: one window, on the launch button", length(w) == 1 && near(w[[1]], rectOf(LAUNCH)))
shot("10_launch")
clickEl(LAUNCH)
ok("the tap ends the tour", waitFor("!document.getElementById('vftTutorial')", 5))
ok("...and the simulation runs", waitFor(sprintf("document.querySelector('%s').disabled", LAUNCH), 20))

cat("\n=== 11. the scenario shown has its simulation: hint 1 to read, hint 2 passed over ===\n")
ok("...the simulation done", waitFor(sprintf("!document.querySelector('%s').disabled &&
  !document.querySelectorAll('#shiny-notification-panel .shiny-notification').length", LAUNCH), 900, 1))
Sys.sleep(3)
invisible(js("vftTutorialStart('step5')"))
ok("the tour starts on hint 1, variant b", waitFor(atHint(1), 30) && identical(tut()$variant, "b"),
   paste(unlist(tut()), collapse = ","))
Sys.sleep(0.6)
w <- wins()
ok("...one window, on the launch button", length(w) == 1 && near(w[[1]], rectOf(LAUNCH)),
   paste(unlist(w), collapse = ","))
ok("...with a Next button (a hint to read), hint 2 left out of the count (1 / 8)",
   hasNext() && identical(tut()$count, "1 / 8"), tut()$count)
clickEl(LAUNCH); Sys.sleep(1)
ok("...a tap on the button is swallowed", !isTRUE(js(sprintf("document.querySelector('%s').disabled", LAUNCH))) &&
   isTRUE(js(atHint(1))))
shot("11_launch_read")
ok("Next passes over the progress hint, to hint 3", nextAndWait(3) && identical(tut()$count, "2 / 8"),
   paste(unlist(tut()), collapse = ","))
invisible(js("document.querySelector('.vftTutorialStop').click(); true"))

cat(sprintf("\nscreenshots in %s\n", SHOTS))
b$close()
cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
