## Browser check of the heat mitigation search ("Bereiche mit
## Hitzeminderungsbedarf erkennen", R/heat_need_helpers.R) in the REAL app, in
## headless Chrome, with real mouse events.
##
##   1. The automatic route to step 5; the Original simulates itself. Heatwave
##      switch on: the button reads "erkennen". Its modal, for the afternoon with
##      no heat map: the checklist sends the user to the Hitzeminderung page.
##   2. There the search runs on its own - the afternoon map, the afternoon
##      heatwave run, the search - behind a modal, and the button becomes a
##      toggle: circles (or the no-result notice), hidden and shown again.
##   3. Back on step 5: the button is a toggle there too, and the circles stay
##      up across the tick and the sun.
##   4. "Beide" from step 5's ask-again button: the morning map is missing, so
##      back to Hitzeminderung, which makes it, runs the morning run and
##      searches both; the labels are from the morning / afternoon / both set.
##
## Needs the app running with the nav bar on, e.g.
##   VFT_NAV=1, pkgload::load_all("."), shiny::runApp(system.file("app",
##   package = "visitorFlowTool"), port = 7794)
## Run:  Rscript data-raw/verify_heat_need_browser.R [url]
args <- commandArgs(trailingOnly = TRUE)
URL  <- if (length(args)) args[[1]] else Sys.getenv("VFT_URL", "http://127.0.0.1:7794")
SHOTS <- Sys.getenv("VFT_SHOTS", file.path(tempdir(), "heat_need_shots"))
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
shownJs <- function(sel) sprintf("(function(){ var e = document.querySelector(\"%s\");
                                    return !!e && e.offsetParent !== null; })()", sel)
MAP <- "#step1-areaSelectMap"

openFresh <- function() {
  invisible(b$Page$navigate(URL))
  ok("the app connects", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
  invisible(js("localStorage.setItem('vft.tutorial.v1', JSON.stringify({status: 'done', at: Date.now()})); localStorage.setItem('vft.tutorial.done.v1', JSON.stringify({saveLoad: Date.now()}))"))
  invisible(b$Page$reload())
  Sys.sleep(1)
  ok("...again", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
  invisible(waitFor(sprintf("(function(){ var w = HTMLWidgets.find('%s'); var m = w && w.getMap && w.getMap();
                              return !!m && !!m._loaded; })()", MAP), 60))
  Sys.sleep(1.5)
}
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

launchFree <- "(function(){ var e = document.getElementById('step5-launchSim'); return !!e && !e.disabled; })()"
launchBusy <- "(function(){ var e = document.getElementById('step5-launchSim'); return !!e && e.disabled; })()"
switchTo <- function(on) {
  st <- "document.getElementById('step5-heatwaveSwitch').checked"
  if (isTRUE(js(st)) != on) {
    if (!isTRUE(clickEl("label:has(#step5-heatwaveSwitch)"))) clickEl("#step5-heatwaveSwitch")
    Sys.sleep(1)
  }
  if (isTRUE(js(st)) != on) {
    invisible(js(sprintf("(function(){ var e = document.getElementById('step5-heatwaveSwitch'); e.checked = %s; $(e).trigger('change'); })()", tolower(on))))
    Sys.sleep(1)
  }
  ok(sprintf("...the switch is %s", if (on) "on" else "off"), isTRUE(js(st)) == on)
}
rowHas <- function(page, cl) sprintf("(function(){ var e = document.getElementById('%s-heatNeedRow'); return !!e && e.classList.contains('%s'); })()", page, cl)
#the circles and labels of group "heatNeed" on a page's map
nCircles <- function(mapId) js(sprintf("(function(){ var w = HTMLWidgets.find('#%s'); var m = w && w.getMap && w.getMap();
  if(!m || !m.layerManager) return -1; var g = m.layerManager.getLayerGroup('heatNeed', false);
  if(!g) return 0; return g.getLayers().filter(function(l){ return l instanceof L.Circle; }).length; })()", mapId))
labels <- function(mapId) js(sprintf("(function(){ var w = HTMLWidgets.find('#%s'); var m = w && w.getMap && w.getMap();
  var g = m && m.layerManager && m.layerManager.getLayerGroup('heatNeed', false); if(!g) return '';
  return g.getLayers().filter(function(l){ return !(l instanceof L.Circle); }).map(function(l){
    var t = l.getTooltip && l.getTooltip(); return t ? t.getContent() : ''; }).join(' | '); })()", mapId))
noneNote <- "Array.prototype.some.call(document.querySelectorAll('#shiny-notification-panel .shiny-notification'), function(e){ return /Nutzungsrückgänge/.test(e.textContent); })"
modalText <- function() js("(function(){ var e = document.querySelector('#shiny-modal .modal-content'); return e ? e.textContent.replace(/\\s+/g, ' ') : ''; })()")
modalOpen <- "!!document.querySelector('.modal.in, .modal.show')"
runModal  <- "(function(){ var e = document.querySelector('.modal.in .modal-title, .modal.show .modal-title'); return !!e && /Erkennung/.test(e.textContent); })()"
chooseRadio <- function(v) invisible(js(sprintf(
  "(function(){ var e = document.querySelector('input[name=\"step5-heatNeedChoice\"][value=\"%s\"]'); if(!e) return false; e.click(); return true; })()", v)))

#waits for a search started on the Hitzeminderung page to finish; TRUE when
#circles or the no-result notice came
awaitHitzeRun <- function(tag) {
  ok(sprintf("%s: on the Hitzeminderung page", tag), waitFor(ringIs("vftNav_hitze"), 300))
  ok(sprintf("%s: the run modal comes up", tag), waitFor(runModal, 120))
  shot(paste0(tag, "_running"))
  ok(sprintf("%s: ...and goes when the run is done", tag), waitFor(sprintf("!(%s)", runModal), 1800, 1))
  Sys.sleep(2)
  ok(sprintf("%s: the button is now a toggle, held down", tag),
     waitFor(sprintf("%s && %s", rowHas("newVersions", "vftHnHas"), rowHas("newVersions", "vftHnShown")), 20))
  n <- nCircles("newVersions-versionMap")
  ok(sprintf("%s: circles on the map, or the no-result notice", tag), isTRUE(n > 0) || isTRUE(js(noneNote)),
     sprintf("(%s circle(s))", n))
  n
}

cat("=== 1. step 5: the button, the modal, the hand-over ===\n")
openFresh()
drawAndConfirm()
clickEl("#vftNextSimAuto")
ok("on step 5", waitFor(ringIs("vftNav_step5"), 600))
ok("...the Original simulates itself and finishes", waitFor(launchBusy, 60) && waitFor(launchFree, 900))
switchTo(TRUE)
ok("the heat mitigation button shows with the switch", waitFor(shownJs("#step5-heatNeedBtn"), 10))
ok("...reading 'erkennen' (nothing stored)", !isTRUE(js(rowHas("step5", "vftHnHas"))) &&
     waitFor(shownJs("#step5-heatNeedBtn .vftHnLblFind"), 5))
ok("...no ask-again button yet", !isTRUE(js(shownJs("#step5-heatNeedRedo"))))
shot("1_step5_button")
clickEl("#step5-heatNeedBtn")
ok("a click opens the modal", waitFor(modalOpen, 10))
ok("...with the choice and its checklist", waitFor("!!document.querySelector('.vftHnPlan')", 10))
chooseRadio("afternoon"); Sys.sleep(1.5)
ok("...the normal run is there", grepl("Normale Simulation.*vorhanden", modalText()))
ok("...the missing map sends to Hitzeminderung", grepl("wird auf der Seite Hitzeminderung berechnet", modalText()) &&
     grepl("Zur Hitzeminderung und starten", modalText()))
shot("1b_modal")
clickEl(".vftHnGo")

cat("=== 2. the search runs on the Hitzeminderung page ===\n")
n1 <- awaitHitzeRun("2")
ok("...afternoon labels only", n1 <= 0 || !grepl("Morgen", labels("newVersions-versionMap")), labels("newVersions-versionMap"))
ok("...the afternoon map was made", waitFor("(function(){ var e = document.querySelector(\".vftHeatIcons .vftHeatIcon[data-bin='afternoon']\"); return !!e && !e.classList.contains('vftHeatNone'); })()", 10))
shot("2_hitze_circles")
clickEl("#newVersions-heatNeedBtn")
ok("the toggle hides them", waitFor(sprintf("!%s", rowHas("newVersions", "vftHnShown")), 10) &&
     waitFor("(function(){ var w = HTMLWidgets.find('#newVersions-versionMap'); var g = w.getMap().layerManager.getLayerGroup('heatNeed', false); return !g || g.getLayers().length === 0; })()", 10))
clickEl("#newVersions-heatNeedBtn")
ok("...and shows them again", waitFor(rowHas("newVersions", "vftHnShown"), 10) &&
     (n1 <= 0 || waitFor(sprintf("(%s) > 0", gsub("\n", " ", sprintf("(function(){ var g = HTMLWidgets.find('#newVersions-versionMap').getMap().layerManager.getLayerGroup('heatNeed', false); return g ? g.getLayers().length : 0; })()"))), 10)))
ok("...fresh: the ask-again button is not orange", !isTRUE(js(rowHas("newVersions", "vftHnStale"))))

cat("=== 3. back on step 5 ===\n")
clickEl("#vftNav_step5")
ok("back on step 5", waitFor(ringIs("vftNav_step5"), 120))
Sys.sleep(2)
switchTo(TRUE)
ok("the button is a toggle here too", waitFor(rowHas("step5", "vftHnHas"), 20))
ok("...off on arrival", !isTRUE(js(rowHas("step5", "vftHnShown"))))
clickEl("#step5-heatNeedBtn")
ok("...a click shows the circles", waitFor(rowHas("step5", "vftHnShown"), 10) &&
     (n1 <= 0 || waitFor("(function(){ var g = HTMLWidgets.find('#step5-mapAreaLeaflet').getMap().layerManager.getLayerGroup('heatNeed', false); return !!g && g.getLayers().length > 0; })()", 15)))
ok("...the same number as on Hitzeminderung", n1 <= 0 || identical(nCircles("step5-mapAreaLeaflet"), n1),
   sprintf("(%s vs %s)", nCircles("step5-mapAreaLeaflet"), n1))
shot("3_step5_circles")
ICON <- function(slot) sprintf("#placeholder_step5 > div:first-child .vft5SimIcon[data-slot='%s']", slot)
clickEl(ICON("afternoon")); Sys.sleep(3)
ok("...still up on the heatwave run", n1 <= 0 || identical(nCircles("step5-mapAreaLeaflet"), n1))
clickEl(ICON("normal")); Sys.sleep(3)
ok("...and on the normal run", n1 <= 0 || identical(nCircles("step5-mapAreaLeaflet"), n1))
ok("...nothing was launched", isTRUE(js(launchFree)))

cat("=== 4. both times of day ===\n")
ok("the ask-again button is there", waitFor(shownJs("#step5-heatNeedRedo"), 5))
clickEl("#step5-heatNeedRedo")
ok("...it opens the modal", waitFor("!!document.querySelector('.vftHnPlan')", 10))
chooseRadio("both"); Sys.sleep(1.5)
ok("...the afternoon pieces are there, the morning map is not",
   grepl("Hitzekarte \\(Nachmittag\\).{0,6}vorhanden", modalText()) &&
     grepl("Hitzekarte \\(Morgen\\).{0,6}wird auf der Seite", modalText()), substr(modalText(), 1, 400))
shot("4_modal_both")
clickEl(".vftHnGo")
n2 <- awaitHitzeRun("4")
lb <- labels("newVersions-versionMap")
ok("...labels from the morning / afternoon / both set",
   n2 <= 0 || all(grepl("^(Morgen|Nachmittag|Morgen und Nachmittag) −[0-9]+ %$", strsplit(lb, " \\| ")[[1]])), lb)
ok("...the legend names the search", n2 <= 0 || waitFor("Array.prototype.some.call(document.querySelectorAll('#newVersions-versionMap .legend'), function(e){ return /Hitzeminderungsbedarf/.test(e.textContent); })", 10))
shot("4_hitze_both")

cat(sprintf("\nshots in %s\n%d failure(s)\n", SHOTS, fails))
b$close()
if (fails) quit(status = 1)
