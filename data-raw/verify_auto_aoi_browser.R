## Browser check of the automatic route to the simulation (R/auto_aoi.R), in
## the REAL app, in headless Chrome, with real mouse events.
##
##   1. Step 1's next-step modal: the simulation choice is two buttons, the big
##      "Naherholung simulieren (Automatische Zielgebiete)" (#vftNextSimAuto,
##      second line in a smaller span) with "Manuelle Zielgebiete" (#vftNextSim)
##      dropping down under it only while the pointer is over it - hidden, so
##      the modal reads as three choices, and still open on the way down to it.
##   2. The automatic button: straight to step 5, with one bar per stage, in
##      order - areas generated, areas cut, data prepared, simulation - and the
##      Original simulated.
##   3. Steps 3 and 4 afterwards, from the nav bar: step 3's slider on 3.1, step
##      4 on the cut areas, Reset going to the plain (uncut) areas; back on step
##      5 the simulation is still there.
##   4. A new session, the manual button: step 3, slider on 3.1, the new tip.
##
## Needs the app running with the nav bar on, e.g.
##   VFT_NAV=1, pkgload::load_all("."), shiny::runApp(system.file("app",
##   package = "visitorFlowTool"), port = 7791)
## Run:  Rscript data-raw/verify_auto_aoi_browser.R [url]
args <- commandArgs(trailingOnly = TRUE)
URL  <- if (length(args)) args[[1]] else Sys.getenv("VFT_URL", "http://127.0.0.1:7791")
SHOTS <- Sys.getenv("VFT_SHOTS", file.path(tempdir(), "auto_aoi_shots"))
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
rectOf <- function(sel) js(sprintf(
  "(function(){ var e = document.querySelector(%s); if(!e) return null;
     var r = e.getBoundingClientRect(); return [r.left, r.top, r.width, r.height]; })()",
  jsonlite::toJSON(sel, auto_unbox = TRUE)))
clickEl <- function(sel) {
  r <- rectOf(sel); if (is.null(r)) return(invisible(FALSE))
  r <- unlist(r); click(r[1] + r[3] / 2, r[2] + r[4] / 2); invisible(TRUE)
}
ringIs <- function(id) sprintf("(function(){ var e = document.querySelector('#vftNav .vft-nav-current');
                                  return !!e && e.id === '%s'; })()", id)
shownJs <- function(sel) sprintf("(function(){ var e = document.querySelector('%s');
                                    return !!e && e.offsetParent !== null; })()", sel)
sliderIs <- function(v) sprintf("(function(){ var x = Shiny.shinyapp.$inputValues['step3-AOISlider'];
                                    return x !== undefined && Number(x) === %s; })()", v)
MAP  <- "#step1-areaSelectMap"
MAP4 <- "#step4-finalAOIMap"
aoiPaths <- function() js(sprintf("document.querySelectorAll('%s .leaflet-layer2-pane path').length", MAP4))

#every caption a progress bar shows, in order, with no repeats in a row
BAR_LOG <- "
  window.__vftBars = [];
  (function(){
    var last = null;
    var grab = function(){
      document.querySelectorAll('#shiny-notification-panel .progress-message').forEach(function(m){
        var t = (m.textContent || '').trim();
        if (t && t !== last) { window.__vftBars.push(t); last = t; }
      });
    };
    new MutationObserver(grab).observe(document.body, {childList: true, subtree: true, characterData: true});
    setInterval(grab, 200);
  })(); true"

openFresh <- function() {
  invisible(b$Page$navigate(URL))
  ok("the app connects", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
  #no tutorial bubble, no save/load card
  invisible(js("localStorage.setItem('vft.tutorial.v1', JSON.stringify({status: 'done', at: Date.now()})); localStorage.setItem('vft.tutorial.done.v1', JSON.stringify({saveLoad: Date.now()}))"))
  invisible(b$Page$reload())
  Sys.sleep(1)
  ok("...again", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
  invisible(waitFor(sprintf("(function(){ var w = HTMLWidgets.find('%s'); var m = w && w.getMap && w.getMap();
                              return !!m && !!m._loaded; })()", MAP), 60))
  Sys.sleep(1.5)
  invisible(js(BAR_LOG))
}

#a triangle round the middle of the step 1 map - Birmensdorf, where the map opens
drawAndConfirm <- function() {
  invisible(js(sprintf("(function(){ var m = HTMLWidgets.find('%s').getMap(); m.setView([47.3553, 8.4378], 14, {animate:false}); return true; })()", MAP)))
  Sys.sleep(1.5)
  mp <- unlist(rectOf(MAP)); cx <- mp[1] + mp[3] / 2; cy <- mp[2] + mp[4] / 2
  pts <- list(c(cx - 160, cy - 110), c(cx + 160, cy - 110), c(cx, cy + 140))
  for (q in pts) click(q[1], q[2])
  move(cx, cy); move(pts[[1]][1], pts[[1]][2]); click(pts[[1]][1], pts[[1]][2])
  ok("the outline is accepted", waitFor(shownJs("#step1-confirmButton2"), 30))
  Sys.sleep(0.8); clickEl("#step1-confirmButton2")
  ok("the next-step modal opens", waitFor(shownJs("#vftNextSimAuto"), 60))
  Sys.sleep(0.8)
}

cat("=== 1. the modal ===\n")
openFresh()
drawAndConfirm()
shot("1_modal")
ok("four buttons: matrix, auto, manual, heat",
   identical(js("Array.prototype.map.call(document.querySelectorAll('#shiny-modal .vft-next-btn'), function(e){ return e.id; }).join(',')"),
             "vftNextStep2,vftNextSimAuto,vftNextSim,vftNextHitze"))
ok("the auto button reads 'Naherholung simulieren'",
   isTRUE(js("document.getElementById('vftNextSimAuto').textContent.indexOf('Naherholung simulieren') === 0")))
ok("...with '(Automatische Zielgebiete)' on a line of its own, smaller",
   isTRUE(js("(function(){ var s = document.querySelector('#vftNextSimAuto .vft-next-sub'); if(!s) return false;
               var b = document.getElementById('vftNextSimAuto');
               return s.textContent === '(Automatische Zielgebiete)' &&
                      getComputedStyle(s).display === 'block' &&
                      parseFloat(getComputedStyle(s).fontSize) < parseFloat(getComputedStyle(b).fontSize); })()")))
visibleIds <- "Array.prototype.filter.call(document.querySelectorAll('#shiny-modal .vft-next-btn'),
                 function(e){ return e.offsetParent !== null; }).map(function(e){ return e.id; }).join(',')"
manualShown <- shownJs("#vftNextSim")
#the pointer off the buttons first: the modal's heading
hd <- unlist(rectOf("#shiny-modal .vft-next-head")); move(hd[1] + 5, hd[2] + 5); Sys.sleep(0.3)
ok("three choices shown: matrix, auto, heat", identical(js(visibleIds), "vftNextStep2,vftNextSimAuto,vftNextHitze"),
   js(visibleIds))
ok("...the manual button hidden", !isTRUE(js(manualShown)))
ok("...and the modal's height leaves room for it",
   isTRUE(js("(function(){ var c = document.querySelector('#shiny-modal .modal-content').getBoundingClientRect();
               var a = document.getElementById('vftNextSimAuto').getBoundingClientRect();
               return c.bottom - a.bottom >= 56; })()")))
ra <- unlist(rectOf("#vftNextSimAuto"))
move(ra[1] + ra[3] / 2, ra[2] + ra[4] / 2); Sys.sleep(0.3)
ok("hovering the auto button drops it down", isTRUE(js(manualShown)))
shot("1b_modal_hover")
rm_ <- unlist(rectOf("#vftNextSim"))
ok("'Manuelle Zielgebiete' sits under it, same column, smaller",
   !is.null(rm_) && rm_[2] >= ra[2] + ra[4] - 1 && abs(rm_[1] - ra[1]) < 2 && rm_[4] < ra[4],
   sprintf("(auto %s / manual %s)", paste(round(ra), collapse = ","), paste(round(rm_), collapse = ",")))
ok("...and says so", isTRUE(js("document.getElementById('vftNextSim').textContent.indexOf('Manuelle Zielgebiete') >= 0")))
#down through the gap, in small steps, as a hand would
for (y in seq(ra[2] + ra[4] - 4, rm_[2] + rm_[4] / 2, by = 3)) {
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = ra[1] + ra[3] / 2, y = y); Sys.sleep(0.02)
}
Sys.sleep(0.2)
ok("...it stays open on the way down to it", isTRUE(js(manualShown)))
ok("...and the pointer is on it", isTRUE(js("document.getElementById('vftNextSim').matches(':hover')")))
move(hd[1] + 5, hd[2] + 5); Sys.sleep(0.3)
ok("leaving closes it again", !isTRUE(js(manualShown)))
ok("the modal is still open", isTRUE(js(shownJs("#vftNextSimAuto"))))

cat("=== 2. the automatic route ===\n")
t0 <- Sys.time()
clickEl("#vftNextSimAuto")
ok("the modal closes", waitFor("!document.querySelector('#shiny-modal .vft-next-btn')", 10))
ok("on step 5", waitFor(ringIs("vftNav_step5"), 600))
ok("...the Original simulates itself: launch disabled",
   waitFor("(function(){ var e = document.getElementById('step5-launchSim'); return !!e && e.disabled; })()", 60))
ok("...and finishes: launch enabled again",
   waitFor("(function(){ var e = document.getElementById('step5-launchSim'); return !!e && !e.disabled; })()", 900))
ok("...its card has a simulation", waitFor("!!document.querySelector('[id^=\"step5-\"].withSim')", 30))
cat(sprintf("   (%.0f s from the click)\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
shot("2_step5")
bars <- unlist(js("window.__vftBars"))
cat("   bars:", paste(bars, collapse = " | "), "\n")
pos <- function(pat) { i <- grep(pat, bars); if (length(i)) i[[1]] else NA }
order <- c(gen = pos("Zielgebiete werden erzeugt"), cut = pos("automatisch zerschnitten"),
           prep = pos("Daten werden vorbereitet"), sim = pos("Simulation l"))
ok("one bar per stage, in order: generate, cut, prepare, simulate",
   !anyNA(order) && !is.unsorted(order), paste(names(order), order, collapse = " "))

cat("=== 3. steps 3 and 4 afterwards ===\n")
ok("step 3 is offered in the bar", waitFor("!document.getElementById('vftNav_step3').disabled", 20))
clickEl("#vftNav_step3")
ok("on step 3", waitFor(ringIs("vftNav_step3"), 120))
ok("...the slider on 3.1", waitFor(sliderIs(3.1), 60))
shot("3_step3")
clickEl("#vftNav_step4")
ok("on step 4", waitFor(ringIs("vftNav_step4"), 120))
ok("...areas drawn", waitFor(sprintf("document.querySelectorAll('%s .leaflet-layer2-pane path').length > 0", MAP4), 120))
Sys.sleep(2)
nCut <- aoiPaths()
shot("4_step4_cut")
clickEl("#step4-resetButton")
Sys.sleep(3)
nPlain <- aoiPaths()
shot("4_step4_reset")
ok("Reset goes to a different (the plain) set", isTRUE(nPlain > 0 && nPlain != nCut),
   sprintf("(cut %s paths, plain %s)", nCut, nPlain))
clickEl("#vftNav_step5")
ok("back on step 5", waitFor(ringIs("vftNav_step5"), 120))
ok("...the simulation is still there", waitFor("!!document.querySelector('[id^=\"step5-\"].withSim')", 30))
ok("...and nothing re-runs it",
   isTRUE(js("(function(){ var e = document.getElementById('step5-launchSim'); return !!e && !e.disabled; })()")))

cat("=== 4. a new session, the manual route ===\n")
openFresh()
drawAndConfirm()
ra <- unlist(rectOf("#vftNextSimAuto")); move(ra[1] + ra[3] / 2, ra[2] + ra[4] / 2); Sys.sleep(0.4)
clickEl("#vftNextSim")
ok("on step 3", waitFor(ringIs("vftNav_step3"), 300))
ok("...the slider on 3.1", waitFor(sliderIs(3.1), 60))
ok("...the tip names it",
   isTRUE(js("document.body.textContent.indexOf('Der voreingestellte Schwellenwert (3.1)') >= 0")))
shot("5_manual_step3")

cat(sprintf("\n%d failure(s). Screenshots: %s\n", fails, SHOTS))
b$close()
quit(status = if (fails) 1 else 0)
