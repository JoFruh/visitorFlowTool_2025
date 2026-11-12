## Browser check: the heat mitigation search from the Hitzeminderung door, where
## there are no Zielgebiete yet. The run must make the automatic ones itself
## (vftAutoAoISim() without its hand-over to step 5) and go on to the heat map,
## the simulations and the search - all without leaving the page.
##
##   1. Step 1 outline -> "Hitzeminderung" in the next-step modal.
##   2. The search button's modal lists "Zielgebiete - werden automatisch
##      bestimmt" first, and no "go to step 3" offer appears.
##   3. Starten: the run modal's first stage is the Zielgebiete; the page stays
##      Hitzeminderung throughout; circles (or the no-result notice) at the end;
##      the Zielgebiete rail switch is live afterwards.
##
## Needs the app running with the nav bar on, e.g.
##   VFT_NAV=1, pkgload::load_all("."), shiny::runApp(system.file("app",
##   package = "visitorFlowTool"), port = 7794)
## Run:  Rscript data-raw/verify_heat_need_aoi_browser.R [url]
args <- commandArgs(trailingOnly = TRUE)
URL  <- if (length(args)) args[[1]] else Sys.getenv("VFT_URL", "http://127.0.0.1:7794")
SHOTS <- Sys.getenv("VFT_SHOTS", file.path(tempdir(), "heat_need_aoi_shots"))
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
ringId <- "(function(){ var e = document.querySelector('#vftNav .vft-nav-current'); return e ? e.id : ''; })()"
shownJs <- function(sel) sprintf("(function(){ var e = document.querySelector(\"%s\");
                                    return !!e && e.offsetParent !== null; })()", sel)
MAP <- "#step1-areaSelectMap"
rowHas <- function(page, cl) sprintf("(function(){ var e = document.getElementById('%s-heatNeedRow'); return !!e && e.classList.contains('%s'); })()", page, cl)
nCircles <- function(mapId) js(sprintf("(function(){ var w = HTMLWidgets.find('#%s'); var m = w && w.getMap && w.getMap();
  if(!m || !m.layerManager) return -1; var g = m.layerManager.getLayerGroup('heatNeed', false);
  if(!g) return 0; return g.getLayers().filter(function(l){ return l instanceof L.Circle; }).length; })()", mapId))
noneNote <- "Array.prototype.some.call(document.querySelectorAll('#shiny-notification-panel .shiny-notification'), function(e){ return /Nutzungsrückgänge/.test(e.textContent); })"
modalText <- function() js("(function(){ var e = document.querySelector('#shiny-modal .modal-content'); return e ? e.textContent.replace(/\\s+/g, ' ') : ''; })()")
modalOpen <- "!!document.querySelector('.modal.in, .modal.show')"
runModal  <- "(function(){ var e = document.querySelector('.modal.in .modal-title, .modal.show .modal-title'); return !!e && /Erkennung/.test(e.textContent); })()"
stepTitle <- "(function(){ var e = document.querySelector('.modal.in .modal-title, .modal.show .modal-title'); return e ? e.textContent : ''; })()"

cat("=== 1. step 1 -> Hitzeminderung, no Zielgebiete ===\n")
invisible(b$Page$navigate(URL))
ok("the app connects", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
invisible(js("localStorage.setItem('vft.tutorial.v1', JSON.stringify({status: 'done', at: Date.now()})); localStorage.setItem('vft.tutorial.done.v1', JSON.stringify({saveLoad: Date.now()}))"))
invisible(b$Page$reload())
Sys.sleep(1)
ok("...again", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
invisible(waitFor(sprintf("(function(){ var w = HTMLWidgets.find('%s'); var m = w && w.getMap && w.getMap();
                            return !!m && !!m._loaded; })()", MAP), 60))
Sys.sleep(1.5)
invisible(js(sprintf("(function(){ var m = HTMLWidgets.find('%s').getMap(); m.setView([47.3553, 8.4378], 14, {animate:false}); return true; })()", MAP)))
Sys.sleep(1.5)
mp <- unlist(rectOf(MAP)); cx <- mp[1] + mp[3] / 2; cy <- mp[2] + mp[4] / 2
pts <- list(c(cx - 160, cy - 110), c(cx + 160, cy - 110), c(cx, cy + 140))
for (q in pts) click(q[1], q[2])
move(cx, cy); move(pts[[1]][1], pts[[1]][2]); click(pts[[1]][1], pts[[1]][2])
ok("the outline is accepted", waitFor(shownJs("#step1-confirmButton2"), 30))
Sys.sleep(0.8); clickEl("#step1-confirmButton2")
ok("the next-step modal opens", waitFor(shownJs("#vftNextHitze"), 60))
Sys.sleep(0.8)
clickEl("#vftNextHitze")
ok("on the Hitzeminderung page", waitFor(ringIs("vftNav_hitze"), 300))
ok("...the search button is there", waitFor(shownJs("#newVersions-heatNeedBtn"), 60))
ok("...no Zielgebiete: the rail switch is greyed",
   isTRUE(js("document.getElementById('newVersions-showAOI').classList.contains('vftRailOff')")))
Sys.sleep(3)

cat("=== 2. the modal ===\n")
clickEl("#newVersions-heatNeedBtn")
ok("a click opens the modal", waitFor(modalOpen, 10))
ok("...not the 'go to step 3' offer", waitFor("!!document.querySelector('.vftHnPlan')", 10) &&
     !grepl("Zielgebiete nicht vorhanden", modalText()))
invisible(js("(function(){ var e = document.querySelector('input[name=\"newVersions-heatNeedChoice\"][value=\"afternoon\"]'); if(e) e.click(); })()"))
Sys.sleep(1.5)
txt <- modalText()
ok("...the Zielgebiete are listed first, made automatically",
   grepl("Zielgebiete – werden automatisch bestimmt", txt) &&
     regexpr("Zielgebiete", txt) < regexpr("Normale Simulation", txt))
ok("...with the map and both runs to do, and 'Starten'",
   grepl("Hitzekarte \\(Nachmittag\\) – wird berechnet", txt) && grepl(">?Starten", txt))
shot("1_modal")

cat("=== 3. the run, staying on the page ===\n")
clickEl(".vftHnGo")
ok("the run modal comes up", waitFor(runModal, 30))
ok("...its first stage is the Zielgebiete",
   grepl("Zielgebiete \\(automatisch\\)", js("(function(){ var e = document.getElementById('vftHnStage1'); return e ? e.textContent : ''; })()")))
shot("2_running")
left <- character(0); offer <- FALSE
t0 <- Sys.time()
repeat {
  id <- tryCatch(js(ringId), error = function(e) "")
  if (nzchar(id) && id != "vftNav_hitze") left <- unique(c(left, id))
  tt <- tryCatch(js(stepTitle), error = function(e) "")
  if (isTRUE(grepl("Zielgebiete nicht vorhanden", tt))) offer <- TRUE
  if (!isTRUE(js(runModal))) break
  if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > 1800) break
  Sys.sleep(1)
}
ok("...and goes when the run is done", !isTRUE(js(runModal)),
   sprintf("(%.0f s)", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
ok("the page never changed", !length(left), paste(left, collapse = ", "))
ok("...and no step 3 offer appeared", !offer)
Sys.sleep(2)
ok("the button is now a toggle, held down",
   waitFor(sprintf("%s && %s", rowHas("newVersions", "vftHnHas"), rowHas("newVersions", "vftHnShown")), 20))
n <- nCircles("newVersions-versionMap")
ok("circles on the map, or the no-result notice", isTRUE(n > 0) || isTRUE(js(noneNote)), sprintf("(%s circle(s))", n))
ok("the Zielgebiete rail switch is live now",
   !isTRUE(js("document.getElementById('newVersions-showAOI').classList.contains('vftRailOff')")))
ok("still on Hitzeminderung", isTRUE(js(ringIs("vftNav_hitze"))))
shot("3_done")

cat(sprintf("\n%d failure(s)\n", fails))
b$close()
if (fails) quit(status = 1)
