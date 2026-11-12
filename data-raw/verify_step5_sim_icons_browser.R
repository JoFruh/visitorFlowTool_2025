## Browser check of step 5's three simulation icons per card (normal, morning
## and afternoon heatwave; step5SimIconsTag() in R/heatwave_helpers.R), in the
## REAL app, in headless Chrome, with real mouse events.
##
##   1. The automatic route to step 5: the Original simulates itself; its tick
##      is filled, the suns are hidden, the strip spans the card, 3 equal cells.
##   2. Heatwave switch on: the suns show, grey. A grey afternoon sun with no
##      heat map: warning, nothing launched. Switch off: hidden again.
##   3. A heat map for the afternoon (Hitzeminderung, via the card's own grey
##      icon there), back to step 5: the grey afternoon sun launches a heatwave
##      run; it turns colour and is shown, the tick survives; tick and sun
##      switch the map's legend title between the two.
##
## Needs the app running with the nav bar on, e.g.
##   VFT_NAV=1, pkgload::load_all("."), shiny::runApp(system.file("app",
##   package = "visitorFlowTool"), port = 7793)
## Run:  Rscript data-raw/verify_step5_sim_icons_browser.R [url]
args <- commandArgs(trailingOnly = TRUE)
URL  <- if (length(args)) args[[1]] else Sys.getenv("VFT_URL", "http://127.0.0.1:7793")
SHOTS <- Sys.getenv("VFT_SHOTS", file.path(tempdir(), "step5_sim_icons_shots"))
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
  #no tutorial bubble, no save/load card
  invisible(js("localStorage.setItem('vft.tutorial.v1', JSON.stringify({status: 'done', at: Date.now()})); localStorage.setItem('vft.tutorial.done.v1', JSON.stringify({saveLoad: Date.now()}))"))
  invisible(b$Page$reload())
  Sys.sleep(1)
  ok("...again", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
  invisible(waitFor(sprintf("(function(){ var w = HTMLWidgets.find('%s'); var m = w && w.getMap && w.getMap();
                              return !!m && !!m._loaded; })()", MAP), 60))
  Sys.sleep(1.5)
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

ICON <- function(slot) sprintf("#placeholder_step5 > div:first-child .vft5SimIcon[data-slot='%s']", slot)
has  <- function(slot, cl) sprintf("(function(){ var e = document.querySelector(\"%s\"); return !!e && e.classList.contains('%s'); })()", ICON(slot), cl)
cls  <- function(slot) js(sprintf("(function(){ var e = document.querySelector(\"%s\"); return e ? e.className : null; })()", ICON(slot)))
visJs <- function(slot, v) sprintf("(function(){ var e = document.querySelector(\"%s\"); return !!e && getComputedStyle(e).visibility === '%s'; })()", ICON(slot), v)
launchFree <- "(function(){ var e = document.getElementById('step5-launchSim'); return !!e && !e.disabled; })()"
launchBusy <- "(function(){ var e = document.getElementById('step5-launchSim'); return !!e && e.disabled; })()"
legendText <- function() js("(function(){ var t = document.querySelectorAll('#step5-mapAreaLeaflet .legend');
  return Array.prototype.map.call(t, function(e){ return e.textContent; }).join(' | '); })()")
switchTo <- function(on) {
  st <- "document.getElementById('step5-heatwaveSwitch').checked"
  if (isTRUE(js(st)) != on) {
    #the visible pill is the label
    if (!isTRUE(clickEl("label:has(#step5-heatwaveSwitch)"))) clickEl("#step5-heatwaveSwitch")
    Sys.sleep(1)
  }
  if (isTRUE(js(st)) != on) {
    invisible(js(sprintf("(function(){ var e = document.getElementById('step5-heatwaveSwitch'); e.checked = %s; $(e).trigger('change'); })()", tolower(on))))
    Sys.sleep(1)
  }
  ok(sprintf("...the switch is %s", if (on) "on" else "off"), isTRUE(js(st)) == on)
}

cat("=== 1. step 5, the Original simulated ===\n")
openFresh()
drawAndConfirm()
clickEl("#vftNextSimAuto")
ok("on step 5", waitFor(ringIs("vftNav_step5"), 600))
ok("...the Original simulates itself and finishes", waitFor(launchBusy, 60) && waitFor(launchFree, 900))
ok("...its card has a simulation", waitFor("!!document.querySelector('[id^=\"step5-\"].withSim')", 30))
ok("three icons on the card", isTRUE(js("document.querySelectorAll('#placeholder_step5 > div:first-child .vft5SimIcon').length === 3")))
ok("the tick is filled and shown",
   waitFor(sprintf("%s && !%s", has("normal", "vft5SimShown"), has("normal", "vft5SimNone")), 20), cls("normal"))
ok("the suns are hidden with the switch off", isTRUE(js(visJs("morning", "hidden"))) && isTRUE(js(visJs("afternoon", "hidden"))))
geo <- js("(function(){ var c = document.querySelector('#placeholder_step5 > div:first-child .btn').getBoundingClientRect();
  var w = Array.prototype.map.call(document.querySelectorAll('#placeholder_step5 > div:first-child .vft5SimIcon'),
    function(e){ var r = e.getBoundingClientRect(); return [r.left, r.width, r.height, r.bottom]; });
  return {card: [c.left, c.width, c.bottom], icons: w}; })()")
iw <- vapply(geo$icons, function(x) x[[2]], numeric(1))
ok("the three icons are the same size, square", diff(range(iw)) < 0.5 &&
     all(abs(vapply(geo$icons, function(x) x[[2]] - x[[3]], numeric(1))) < 0.5), paste(round(iw, 1), collapse = ","))
span <- geo$icons[[3]][[1]] + geo$icons[[3]][[2]] - geo$icons[[1]][[1]]
ok("...together they span the card's width (less its margins)", span > 0.8 * geo$card[[2]],
   sprintf("(%.0f of %.0f px)", span, geo$card[[2]]))
ok("...inside the card", geo$icons[[1]][[4]] <= geo$card[[3]])
ok("...the tick bigger than the old 18 px", iw[[1]] > 20, sprintf("(%.1f px)", iw[[1]]))
shot("1_step5")

cat("=== 2. the switch, and a sun without a heat map ===\n")
switchTo(TRUE)
ok("switch on: the suns show", waitFor(visJs("afternoon", "visible"), 10))
ok("...grey (no result)", grepl("vft5SimNone", cls("morning")) && grepl("vft5SimNone", cls("afternoon")))
shot("2_switch_on")
clickEl(ICON("afternoon"))
ok("a grey sun without a heat map: a warning",
   waitFor("document.querySelectorAll('#shiny-notification-panel .shiny-notification-warning').length > 0", 10))
Sys.sleep(1)
ok("...and nothing is launched", isTRUE(js(launchFree)))
ok("...the radio follows the sun", isTRUE(js("Shiny.shinyapp.$inputValues['step5-heatwaveBin'] === 'afternoon'")))
ok("...the hint and the create link show", waitFor(shownJs("#step5-heatwaveGo"), 10))
switchTo(FALSE)
ok("switch off: the suns hide again", waitFor(visJs("afternoon", "hidden"), 10))
ok("...the tick is what is shown", waitFor(has("normal", "vft5SimShown"), 10))

cat("=== 3. with an afternoon heat map ===\n")
switchTo(TRUE)
Sys.sleep(1)
ok("the create link", waitFor(shownJs("#step5-heatwaveGo"), 10))
clickEl("#step5-heatwaveGo")
HI <- ".vftHeatIcons .vftHeatIcon[data-bin='afternoon']"
ok("on the Hitzeminderung page", waitFor(shownJs(HI), 300))
#the column refuses clicks while a heat job runs (.vftHeatBusy); click until
#the icon takes it - pending, or already computed
idle <- "!document.querySelector('#topPlaceHolder_newVersion.vftHeatBusy')"
taken <- sprintf("(function(){ var e = document.querySelector(\"%s\"); return !!e && (e.classList.contains('vftHeatPending') || !e.classList.contains('vftHeatNone')); })()", HI)
for (k in 1:20) {
  waitFor(idle, 120); Sys.sleep(1)
  clickEl(HI)
  if (waitFor(taken, 8)) break
}
ok("...the afternoon icon takes the click", isTRUE(js(taken)))
ok("...the afternoon map computes",
   waitFor(sprintf("(function(){ var e = document.querySelector(\"%s\"); return !!e && !e.classList.contains('vftHeatNone') && !e.classList.contains('vftHeatPending'); })()", HI), 600))
shot("3a_hitze")
clickEl("#vftNav_step5")
ok("back on step 5", waitFor(ringIs("vftNav_step5"), 120))
ok("...the tick survived", waitFor(sprintf("!!document.querySelector(\"%s\") && !%s", ICON("normal"), has("normal", "vft5SimNone")), 30))
switchTo(TRUE)
ok("...the afternoon sun still grey", waitFor(has("afternoon", "vft5SimNone"), 10))
clickEl(ICON("afternoon"))
ok("the grey sun launches a heatwave run", waitFor(launchBusy, 60))
ok("...its icon pulses", waitFor(has("afternoon", "vft5SimPending"), 10))
ok("...and finishes", waitFor(launchFree, 900))
ok("...the sun is in colour and shown",
   waitFor(sprintf("%s && !%s && !%s", has("afternoon", "vft5SimShown"), has("afternoon", "vft5SimNone"),
                   has("afternoon", "vft5SimPending")), 30), cls("afternoon"))
ok("...the tick is still filled", !grepl("vft5SimNone", cls("normal")))
Sys.sleep(1.5)
ok("...the legend names the heatwave", isTRUE(grepl("Hitze", legendText())), legendText())
shot("3b_heat_shown")
clickEl(ICON("normal"))
ok("the tick shows the normal run again", waitFor(has("normal", "vft5SimShown"), 20))
Sys.sleep(1.5)
ok("...without launching anything", isTRUE(js(launchFree)))
ok("...the legend is the normal one", !isTRUE(grepl("Hitze", legendText())), legendText())
clickEl(ICON("afternoon"))
ok("the sun shows the heatwave run again", waitFor(has("afternoon", "vft5SimShown"), 20))
Sys.sleep(1.5)
ok("...legend", isTRUE(grepl("Hitze", legendText())), legendText())
clickEl("#placeholder_step5 > div:first-child .btn")
Sys.sleep(1.5)
ok("a click on the card body keeps the sun", isTRUE(js(has("afternoon", "vft5SimShown"))))
switchTo(FALSE)
ok("switch off with the sun shown: back to the tick", waitFor(has("normal", "vft5SimShown"), 20))
shot("3c_off")

cat(sprintf("\nshots in %s\n%d failure(s)\n", SHOTS, fails))
b$close()
if (fails) quit(status = 1)
