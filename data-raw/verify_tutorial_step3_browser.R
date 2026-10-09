## Browser check of step 3's tour (inst/app/www/vft-tutorial.js), in the REAL
## app, in headless Chrome, with real mouse events through the DevTools
## protocol. Step 1's tour is walked quickly to get there: the simulation is
## chosen, and step 3's tour has to start by itself when the ring lands on step
## 3 (chaining). Then its five hints: the simulation's three nav buttons, step
## 3's own, the threshold slider (a slide that stays above 8 must not move on;
## one past 8 is put back on exactly 8, its drag ended, and moves on), the map,
## and confirm - whose tap ends the tour. Between hints there is no dim. Then
## the save/load card (once, after the first tour past step 1's), step 4's tour
## by itself, and the data-loss warning's card - over a hint, and between two
## tours (a modal of vftAskCommit()'s markup, put up client-side).
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
## the choice the handle is on (from_value is not kept by update())
slider  <- function() js("(function(){ var d = jQuery('#step3-AOISlider').data('ionRangeSlider');
                                       return String(d.options.values[d.result.from]); })()")
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
aoi <- js("(function(){ var e = document.querySelector('.vftTutorialText em.vftTutAoiRed'),
                             t = document.querySelector('.vftTutorialText'); if (!e) return null;
  return [getComputedStyle(e).color, parseFloat(getComputedStyle(e).fontSize), parseFloat(getComputedStyle(t).fontSize)]; })()")
ok("'Zielgebiete' in the map's paler red (#da4040)", !is.null(aoi) && identical(aoi[[1]], "rgb(218, 64, 64)"),
   paste(unlist(aoi), collapse = " "))
ok("...and one size larger", !is.null(aoi) && aoi[[2]] > aoi[[3]])
ok("'Naherholung simulieren' teal and one size larger",
   isTRUE(js("(function(){ var e = document.querySelector('.vftTutorialText b em');
     return !!e && getComputedStyle(e).color === 'rgb(0, 98, 104)' &&
            parseFloat(getComputedStyle(e).fontSize) > parseFloat(getComputedStyle(document.querySelector('.vftTutorialText')).fontSize); })()")))
ok("the slider's line from the handle up is the same red",
   identical(js("getComputedStyle(document.querySelector('.vft-aoi-slider .irs-line')).backgroundColor"),
             "rgb(218, 64, 64)"))
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
## a slide well past 8, in one drag: the slider is put back on 8 as it passes
## and the rest of the drag does nothing
## the dim, watched from here on: the pause after this hint has no window
invisible(js("(function(){ var c = document.querySelector('.vftTutorialCanvas');
  window.__vftDimLog = [];
  new MutationObserver(function () {
    window.__vftDimLog.push([c.classList.contains('vftTutorialNoDim'), c.getAttribute('data-windows')]);
  }).observe(c, { attributes: true }); return true; })()"))
h <- handle()
x0 <- h[[1]]; x1 <- xFor(4)
move(x0, h[[2]]); mouse("mousePressed", x0, h[[2]])
for (s in seq_len(24)) {
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = x0 + (x1 - x0) * s / 24, y = h[[2]],
                             button = "left", buttons = 1)
  Sys.sleep(0.04)
}
Sys.sleep(0.2)
ok(sprintf("a slide towards 4 stops on exactly 8 (%s)", slider()), identical(slider(), "8"))
## the button is still held: the drag was ended under it
b$Input$dispatchMouseEvent(type = "mouseMoved", x = xFor(2), y = h[[2]], button = "left", buttons = 1)
Sys.sleep(0.2)
ok("...and the drag is over: moving on, still pressed, leaves it on 8", identical(slider(), "8"))
mouse("mouseReleased", xFor(2), h[[2]])
ok("...and moves on to hint 4", waitFor(atHint(4), 20))
## the pause after a hint done, and the map's redraw: no window, so no dim
ok("between hints: no window, so the dim faded out (the layer stayed)",
   isTRUE(js("window.__vftDimLog.some(function (e) { return e[0] && e[1] === '[]'; })")),
   jsonlite::toJSON(js("window.__vftDimLog.slice(0, 6)")))
ok("...and the dim is back with its window",
   isTRUE(js("!document.querySelector('.vftTutorialCanvas').classList.contains('vftTutorialNoDim')")))
ok("Shiny has the 8", isTRUE(js("Shiny.shinyapp.$inputValues['step3-AOISlider'] === '8'")))

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
ok("the tap ends the tour", waitFor("(function(){ var s = vftTutorialState(); return s.key !== 'step3'; })()", 5))
ok("...stored as done", identical(stored(), "done") &&
   isTRUE(js("!!JSON.parse(localStorage.getItem('vft.tutorial.done.v1')).step3")))

cat("\n=== 7. the save and load card, once, after the first tour past step 1's ===\n")
ok("the save/load card follows", waitFor(atHint(1, "saveLoad"), 60), paste(unlist(tut()), collapse = ","))
Sys.sleep(0.6)
w <- wins()
ok("one window, on the save button", length(w) == 1 && near(w[[1]], rectOf("#saveButton"), 8),
   paste(unlist(w), collapse = ","))
ok("...the card right under it", isTRUE(js("(function(){
  var c = document.querySelector('.vftTutorialBox').getBoundingClientRect(),
      s = document.getElementById('saveButton').getBoundingClientRect();
  return c.top > s.bottom && c.top - s.bottom < 40 && document.querySelector('.vftTutorialBox').getAttribute('data-side') === 'below'; })()")))
ok("...'speichern' and 'laden' teal and larger, no counter",
   isTRUE(js("document.querySelectorAll('.vftTutorialText b em').length === 2 &&
              getComputedStyle(document.querySelector('.vftTutorialCount')).display === 'none'")))
ok("...with a Next button", hasNext())
shot("6_save_load")
clickEl(".vftTutorialNext")
ok("...recorded, so it never comes again",
   isTRUE(js("!!JSON.parse(localStorage.getItem('vft.tutorial.done.v1')).saveLoad")))
ok("...and the app moves on to step 4", waitFor(ringIs("vftNav_step4"), 120))
Sys.sleep(1.5)
ok("step 4's tour starts by itself", waitFor(atHint(1, "step4"), 120), paste(unlist(tut()), collapse = ","))


cat("\n=== 8. the data-loss warning, within a tour ===\n")
## The warning is vftAskCommit()'s modal (R/providers.R). Raising it for real
## needs results a confirm would discard, and a new step-1 area cannot be drawn
## on a second visit in headless Chrome, so the modal is put up here with the
## markup showModal(modalDialog()) gives it - the tutorial reads only that: a
## visible #shiny-modal holding #vftInvalidateOk (btn-danger).
FAKE <- "(function(){
  var w = document.getElementById('shiny-modal-wrapper'); if (w) { w.remove(); }
  jQuery('.modal-backdrop').remove();
  w = document.createElement('div'); w.id = 'shiny-modal-wrapper';
  w.innerHTML = '<div id=\"shiny-modal\" class=\"modal fade\" tabindex=\"-1\" data-backdrop=\"static\" data-keyboard=\"false\">' +
    '<div class=\"modal-dialog\"><div class=\"modal-content\"><div class=\"modal-header\"><h4 class=\"modal-title\">Neue Daten uebernehmen?</h4></div>' +
    '<div class=\"modal-body\"><p>Die folgenden Ergebnisse bauen darauf auf und werden verworfen:</p><ul><li>Zielgebiete</li></ul></div>' +
    '<div class=\"modal-footer\"><button type=\"button\" class=\"btn btn-default\" id=\"vftFakeCancel\" data-dismiss=\"modal\">Abbrechen</button>' +
    '<button type=\"button\" class=\"btn btn-default btn-danger\" id=\"vftInvalidateOk\">Neu erstellen und verwerfen</button></div></div></div></div>';
  document.body.appendChild(w);
  jQuery('#shiny-modal').modal('show');
  return true; })()"
COMMIT_UP <- "(function(){ var b = document.getElementById('vftInvalidateOk');
  var t = document.querySelector('.vftTutorialText');
  return !!b && b.offsetParent !== null && !!t && !!t.querySelector('em.vftTutWarn') &&
         !document.getElementById('vftTutorial').classList.contains('vftTutorialQuiet'); })()"
## step 4's first hint is on screen: the warning comes over it
invisible(js(FAKE))
ok("the warning's modal takes the hint's place with a card of its own",
   waitFor(COMMIT_UP, 10), paste(unlist(tut()), collapse = ","))
Sys.sleep(0.6)
w <- wins()
modal <- js("(function(){ var r = document.getElementById('vftInvalidateOk').closest('.modal-content').getBoundingClientRect();
  return [r.left, r.top, r.width, r.height]; })()")
ok("one window, on the whole modal", length(w) == 1 && near(w[[1]], modal, 8), paste(unlist(w), collapse = ","))
warn <- js("(function(){ var e = document.querySelector('.vftTutorialText em.vftTutWarn'),
  t = document.querySelector('.vftTutorialText');
  return [getComputedStyle(e).color, getComputedStyle(document.getElementById('vftInvalidateOk')).backgroundColor,
          parseFloat(getComputedStyle(e).fontSize), parseFloat(getComputedStyle(t).fontSize), getComputedStyle(e).fontWeight]; })()")
ok("'Warnung' in the confirm button's red, bold, one size larger",
   identical(warn[[1]], warn[[2]]) && warn[[3]] > warn[[4]] && as.numeric(warn[[5]]) >= 700,
   paste(unlist(warn), collapse = " | "))
ok("...the card beside the modal, clear of it",
   isTRUE(js("(function(){ var c = document.querySelector('.vftTutorialBox').getBoundingClientRect(),
     m = document.getElementById('vftInvalidateOk').closest('.modal-content').getBoundingClientRect();
     return c.left >= m.right || c.right <= m.left || c.top >= m.bottom || c.bottom <= m.top; })()")))
ok("...no counter, no Next",
   !hasNext() && isTRUE(js("getComputedStyle(document.querySelector('.vftTutorialCount')).display === 'none'")))
ok("...and the tour still on hint 1 underneath", identical(tut()$key, "step4") && identical(tut()$idx, 0L))
ok("its buttons take a tap: the window's hit is the modal",
   isTRUE(js("(function(){ var b = document.getElementById('vftInvalidateOk').getBoundingClientRect();
     var e = document.elementFromPoint(b.left + b.width / 2, b.top + b.height / 2);
     return !!e && !!e.closest('#vftInvalidateOk'); })()")))
shot("8_commit_in_tour")
## Bootstrap ignores a dismiss while the modal is still fading in
Sys.sleep(0.5)
clickEl("#vftFakeCancel")
Sys.sleep(0.5)
ok("the modal gone, hint 1 comes back, Next and all", waitFor(atHint(1, "step4"), 10) &&
   !isTRUE(js("!!document.querySelector('.vftTutorialText em.vftTutWarn')")) && hasNext(),
   paste(c(unlist(tut()), js("jQuery('#shiny-modal').is(':visible')"),
           js("document.querySelector('.vftTutorialText') && document.querySelector('.vftTutorialText').textContent.slice(0, 40)")),
         collapse = ","))
Sys.sleep(0.6)
ok("...its window on step 4's button again", { w <- wins(); length(w) == 1 && near(w[[1]], rectOf("#vftNav_step4"), 6) })
clickEl(".vftTutorialStop")
invisible(waitFor("!document.getElementById('vftTutorial')", 5))

cat("\n=== 9. ...and between two tours ===\n")
## a one-card tour played to its end leaves a chain waiting for the next page;
## a confirm made meanwhile raises the warning, played as a card of its own
invisible(js("vftTutorialStart('saveLoad')"))
ok("a card played to its end...", waitFor(atHint(1, "saveLoad"), 15))
Sys.sleep(0.6); clickEl(".vftTutorialNext")
ok("...leaves the chain waiting",
   waitFor("(function(){ var s = vftTutorialState(); return !s.key && s.chain === 'saveLoad'; })()", 5))
invisible(js(FAKE))
ok("the warning is played as a card of its own",
   waitFor(atHint(1, "commit"), 10) && waitFor(COMMIT_UP, 5), paste(unlist(tut()), collapse = ","))
shot("9_commit_between")
Sys.sleep(0.5)
clickEl("#vftFakeCancel")
ok("its modal gone, the card ends and the chain waits on",
   waitFor("(function(){ var s = vftTutorialState(); return !s.key && s.chain === 'saveLoad'; })()", 10),
   paste(unlist(tut()), collapse = ","))
ok("...nothing recorded for it", !isTRUE(js("!!JSON.parse(localStorage.getItem('vft.tutorial.done.v1')).commit")))
cat(sprintf("\nscreenshots in %s\n", SHOTS))
b$close()
cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
