## Browser check of the heat mitigation tour (inst/app/www/vft-tutorial.js,
## TOURS.hitze) in the REAL app, in headless Chrome, with real mouse events
## through the DevTools protocol. Step 1's tour is walked to get there, choosing
## heat mitigation as the next step, so the tour starts by chaining. Then:
##   * the tour as most users meet it - the seeded "Neu" scenario is selected -
##     from the nav bar's button, through the palette (grass, a stroke, the
##     level switch, the tree, a height, a stroke over an underground element
##     and its warning), the heat map at two times of day and its noon icon,
##     the tools, and the plan import with the tour's own plan (no file
##     picker): the brown paths made artificial, the result, the darker roof
##     picked and made artificial canopy, Apply, and the plan painted. Behind
##     this door there is no path network, so the confirm hint is passed over;
##   * with the Original selected: the scenarios page's "select a scenario"
##     hint is played before the palette, with its own text.
##
## Needs the app running with the nav bar on, e.g.
##   VFT_NAV=1, pkgload::load_all("."), shiny::runApp(system.file("app",
##   package = "visitorFlowTool"), port = 7781)
## Run:  Rscript data-raw/verify_tutorial_hitze_browser.R [url]
args <- commandArgs(trailingOnly = TRUE)
URL  <- if (length(args)) args[[1]] else Sys.getenv("VFT_URL", "http://127.0.0.1:7781")
SHOTS <- Sys.getenv("VFT_SHOTS", file.path(tempdir(), "tutorial_hitze_shots"))
dir.create(SHOTS, showWarnings = FALSE, recursive = TRUE)

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

b <- chromote::ChromoteSession$new(width = 1600, height = 1000)
## Apply classifies and encodes the whole plan on the page's thread: a
## headless page can be busy past the 10 s default
b$default_timeout <- 60
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
clickAt <- function(p) { p <- unlist(p); click(p[1], p[2]) }
centre <- function(sel) js(sprintf(
  "(function(){ var e = document.querySelector(%s); if(!e) return null;
     var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()",
  jsonlite::toJSON(sel, auto_unbox = TRUE)))
clickEl <- function(sel) {
  p <- centre(sel); if (is.null(p)) return(invisible(FALSE))
  click(p[[1]], p[[2]]); invisible(TRUE)
}
tut    <- function() js("window.vftTutorialState ? vftTutorialState() : null")
atHint <- function(i, key = "hitze") sprintf("(function(){ var s = vftTutorialState();
  return s.key === '%s' && s.idx === %d && !s.quiet && !s.pause; })()", key, i - 1)
still  <- function(i, key = "hitze") isTRUE(js(atHint(i, key)))
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
inside <- function(w, r, tol = 2) {
  w <- unlist(w); r <- unlist(r)
  w[1] >= r[1] - tol && w[2] >= r[2] - tol &&
    w[1] + w[3] <= r[1] + r[3] + tol && w[2] + w[4] <= r[2] + r[4] + tol
}
ringIs <- function(id) sprintf("(function(){ var e = document.querySelector('#vftNav .vft-nav-current');
                                  return !!e && e.id === '%s'; })()", id)
hasNext <- function() isTRUE(js("!!document.querySelector('.vftTutorialBox .vftTutorialNext')"))
nextAndWait <- function(i, key = "hitze") { Sys.sleep(0.4); clickEl(".vftTutorialNext"); waitFor(atHint(i, key), 60) }
modalUp <- function(id) isTRUE(js(sprintf("jQuery('#%s').is(':visible')", id)))
text <- function() js("document.querySelector('.vftTutorialBox .vftTutorialText').innerHTML")
card <- function() unlist(rectOf(".vftTutorialBox"))
count <- function() tut()$count
cards <- function() js("Array.from(document.querySelectorAll('#placeholder .vftCard button[id*=versionBtn]')).map(function(b){
  return b.innerText + (b.classList.contains('selected') && !b.classList.contains('notSelected') ? '*' : ''); }).join(',')")
pageReady <- function(secs = 180) {
  ok <- waitFor("(function(){ var a = document.getElementById('newVersions-addVersionButton');
                   return !!a && !a.disabled && a.offsetParent !== null; })()", secs)
  Sys.sleep(2); ok
}
MAPJS <- "HTMLWidgets.find('#newVersions-versionMap').getMap()"


## the tour's own hints are numbered 1..26; the three it borrows from the
## scenarios page sit after hint 3
hz   <- function(k) atHint(if (k >= 4) k + 3 else k)
win1 <- function() unlist(wins()[[1]])
stopTour <- function() invisible(js("document.querySelector('.vftTutorialStop') && document.querySelector('.vftTutorialStop').click()"))
## a short brush stroke, the button held
stroke <- function(x, y) {
  move(x, y); mouse("mousePressed", x, y)
  for (i in 1:12) {
    b$Input$dispatchMouseEvent(type = "mouseMoved", x = x + 2 * i, y = y + i, button = "left", buttons = 1)
    Sys.sleep(0.03)
  }
  mouse("mouseReleased", x + 24, y + 12)
}
UGBOX <- '[id^="shiny-notification-"][id$="ugWarn"]'
ugUp  <- function() isTRUE(js(sprintf("!!document.querySelector('%s')", UGBOX)))
heatOn <- "document.getElementById('newVersions-heatSwitch').classList.contains('paintToolActive')"
## the middles of the underground elements drawn on the map (pane ugPane)
UG <- sprintf("(function(){ var m = %s, r = m.getContainer().getBoundingClientRect(), out = [];
  m.eachLayer(function(l){ if (l.options && l.options.pane === 'ugPane' && l.getBounds) {
    var c = m.latLngToContainerPoint(l.getBounds().getCenter()); out.push([r.left + c.x, r.top + c.y]); } });
  return out; })()", MAPJS)
## the computed colour, size and weight of the first `em` of a class in the card
emLook <- function(cls) js(sprintf("(function(){ var e = document.querySelector('.vftTutorialText em.%s'),
  t = document.querySelector('.vftTutorialText'); if (!e) return null; var s = getComputedStyle(e);
  return [s.color, parseFloat(s.fontSize) > parseFloat(getComputedStyle(t).fontSize), s.fontWeight]; })()", cls))
## a select of the plan's colour card set as a user would, with a change event
setSelect <- function(expr, value) js(sprintf("(function(){ var s = %s; if (!s) return false;
  s.value = '%s'; s.dispatchEvent(new Event('change', {bubbles: true})); return true; })()", expr, value))
ROW_OF <- function(rgb) sprintf("(function(){ var best = null, bd = 1e9;
  document.querySelectorAll('.vft-plan-card .vft-plan-colrow').forEach(function (r) {
    var m = /(\\d+),\\s*(\\d+),\\s*(\\d+)/.exec(getComputedStyle(r.querySelector('.vft-plan-swatch')).backgroundColor);
    var d = Math.pow(m[1] - %d, 2) + Math.pow(m[2] - %d, 2) + Math.pow(m[3] - %d, 2);
    if (d < bd) { bd = d; best = r; } });
  return best; })()", rgb[1], rgb[2], rgb[3])
rowRect <- function(rowExpr) js(sprintf("(function(){ var r = %s; if (!r) return null;
  var a = r.querySelector('.vft-plan-swatch').getBoundingClientRect(), b = r.querySelector('select').getBoundingClientRect();
  return [Math.min(a.left, b.left), Math.min(a.top, b.top), Math.max(a.right, b.right) - Math.min(a.left, b.left),
          Math.max(a.bottom, b.bottom) - Math.min(a.top, b.top)]; })()", rowExpr))
BROWN <- ROW_OF(c(210, 180, 140))
PICKED <- "(function(){ var r = document.querySelectorAll('.vft-plan-card .vft-plan-colrow.vft-plan-picked'); return r.length ? r[r.length - 1] : null; })()"

invisible(b$Page$navigate(URL))
ok("the app connects", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
invisible(js("localStorage.setItem('vft.tutorial.v1', JSON.stringify({status: 'done', at: Date.now()}));
              localStorage.setItem('vft.tutorial.done.v1', JSON.stringify({saveLoad: Date.now()}));"))
invisible(b$Page$reload())
Sys.sleep(1)
ok("...again", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
MAP1 <- "#step1-areaSelectMap"
invisible(waitFor(sprintf("(function(){ var w = HTMLWidgets.find('%s'); var m = w && w.getMap && w.getMap();
                            return !!m && !!m._loaded; })()", MAP1), 60))
Sys.sleep(1.5)

cat("=== 1. to heat mitigation, through step 1's tour ===\n")
invisible(js("vftTutorialStart('step1')"))
ok("step 1's tour starts", waitFor(atHint(1, "step1"), 20))
clickEl(".vftTutorialNext")
invisible(waitFor(atHint(2, "step1"), 10)); clickEl(".vftTutorialNext")
invisible(waitFor(atHint(3, "step1"), 15))
Sys.sleep(0.6)
mp <- unlist(rectOf(MAP1)); cx <- mp[1] + mp[3] / 2; cy <- mp[2] + mp[4] / 2
pts <- list(c(cx - 120, cy - 90), c(cx + 120, cy - 90), c(cx, cy + 110))
for (q in pts) click(q[1], q[2])
move(cx, cy); move(pts[[1]][1], pts[[1]][2]); click(pts[[1]][1], pts[[1]][2])
invisible(waitFor(atHint(4, "step1"), 20))
Sys.sleep(0.6); clickEl("#step1-confirmButton2")
invisible(waitFor(atHint(5, "step1"), 60))
Sys.sleep(0.8); clickEl("#vftNextHitze")
invisible(waitFor(atHint(6, "step1"), 10))
Sys.sleep(0.6); clickEl(".vftTutorialNext")
ok("the ring lands on heat mitigation", waitFor(ringIs("vftNav_hitze"), 240))

cat("\n=== 2. hints 1-3: the step, the context, the materials ===\n")
ok("the heat mitigation tour follows by itself", waitFor(hz(1), 240))
Sys.sleep(0.8)
ok("...on the seeded 'Neu', selected", identical(cards(), "Original,Neu*"), cards())
w <- wins()
ok("one window, on the Hitzeminderung button of the nav bar",
   length(w) == 1 && near(w[[1]], rectOf("#vftNav_hitze"), 8), jsonlite::toJSON(w, auto_unbox = TRUE))
ok("...with a Next button", hasNext())
hot <- emLook("vftTutHot")
ok("'Hitze' orange (#e05119), one size larger, bold",
   !is.null(hot) && identical(hot[[1]], "rgb(224, 81, 25)") && isTRUE(hot[[2]]) && as.numeric(hot[[3]]) >= 700,
   paste(unlist(hot), collapse = " "))
## passed over from the start: the warning (no stroke yet), hiding the heat
## map (none up) and the confirm (no path network behind this door)
ok("the counter leaves out the scenario hints, the warning, the hide and the confirm: 1 / 23",
   identical(count(), "1 / 23"), count())
shot("01_step")
ok("Next: hint 2", nextAndWait(2)); Sys.sleep(0.6)
w <- wins()
ok("one window, on the heat mitigation context",
   length(w) == 1 && near(w[[1]], js("(function(){ var l = document.querySelector('#newVersions-contextChoice input[value=\"4\"]').closest('label');
     var r = l.getBoundingClientRect(); return [r.left, r.top, r.width, r.height]; })()"), 8))
ok("the text has its blank line before 'However'", grepl("<br><br>", text(), fixed = TRUE), text())
shot("02_context")
ok("Next: hint 3", nextAndWait(3)); Sys.sleep(0.6)
w <- wins(); m <- unlist(rectOf("#newVersions-versionMap"))
ok("one window, inside the map and most of its height",
   length(w) == 1 && inside(w[[1]], m) && w[[1]][[4]] > 0.5 * m[4], jsonlite::toJSON(w, auto_unbox = TRUE))
area <- win1()
shot("03_materials")

cat("\n=== 3. hints 4-5: grass, and a stroke ===\n")
ok("Next passes over the scenario hints, to hint 4", nextAndWait(7)); Sys.sleep(0.6)
ok("one window, on the grass button",
   length(wins()) == 1 && near(wins()[[1]], rectOf("#newVersions-paintColor_grass"), 8))
ok("...no Next button", !hasNext())
ok("'Gras' in its paint's colour", grepl("<em class=\"vftTutGrass\">Gras</em>", text()), text())
clickEl("#newVersions-paintColor_bush"); Sys.sleep(0.8)
ok("a tap on another material is swallowed", still(7))
clickEl("#newVersions-paintColor_grass")
ok("grass tapped: hint 5", waitFor(hz(5), 20)); Sys.sleep(0.6)
ok("the window is the area again", near(wins()[[1]], area, 4))
k0 <- card()
## zoomed in, the window becomes the whole map: the card stays where it was
invisible(js(sprintf("%s.zoomIn(2); null", MAPJS))); Sys.sleep(1.5)
k1 <- card()
ok("zoomed in, the card stays where it first appeared", max(abs(k1[1:2] - k0[1:2])) < 2,
   paste(round(k0[1:2]), "->", round(k1[1:2]), collapse = " "))
invisible(js(sprintf("%s.zoomOut(2); null", MAPJS))); Sys.sleep(1.5)
ok("it waits for a stroke", still(8))
t0 <- Sys.time(); stroke(area[1] + area[3] / 2, area[2] + area[4] * 0.6)
Sys.sleep(1.2)
ok("not at once", still(8))
ok("hint 6 follows the stroke", waitFor(hz(6), 20))
el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
ok("...about 3 s after it began (+ the 1 s pause)", el > 3.5 && el < 7, sprintf("%.1f s", el))
shot("06_level")

cat("\n=== 4. hints 6-9: the canopy, the tree, a height, trees ===\n")
ok("one window, on the level switch",
   near(wins()[[1]], rectOf("#newVersions-paintColorButtonsDiv .paintLevelSwitch"), 8))
clickEl("#newVersions-paintColorButtonsDiv .paintLevelSwitch")
ok("switched: hint 7, the tree", waitFor(hz(7), 20)); Sys.sleep(0.6)
ok("...the canopy opened on the artificial canopy, not the tree",
   isTRUE(js("document.getElementById('newVersions-paintColor_canopyArtificial').classList.contains('colorBtnSelected') &&
              !document.getElementById('newVersions-paintColor_canopyTree').classList.contains('colorBtnSelected')")))
ok("one window, on the tree button", length(wins()) == 1 && near(wins()[[1]], rectOf("#newVersions-paintColor_canopyTree"), 8))
ok("...no Next button", !hasNext())
ok("'Bäume' in the tree's dark green", grepl("<em class=\"vftTutTree\">Bäume</em>", text()), text())
shot("07_tree")
clickEl("#newVersions-paintColor_canopyArtificial"); Sys.sleep(0.8)
ok("a tap on the artificial canopy is swallowed", still(10))
clickEl("#newVersions-paintColor_canopyTree")
ok("the tree tapped: hint 8, its heights", waitFor(hz(8), 20)); Sys.sleep(0.6)
ok("one window, on the tree's height bar",
   near(wins()[[1]], rectOf("#newVersions-paintHeightGroup_canopy_tree"), 8))
clickEl("#newVersions-paintHeightGroup_canopy_tree .colorBtnSelected"); Sys.sleep(0.8)
ok("a tap on the height already armed does not count", still(11))
clickEl("#newVersions-paintHeight_13")
ok("another height: hint 9", waitFor(hz(9), 20)); Sys.sleep(0.6)
shot("09_trees")
ug <- js(UG)
ok("the area has underground elements on the map", length(ug) > 0, length(ug))
hit <- FALSE
for (p in utils::head(ug, 8)) {
  p <- unlist(p)
  if (!inside(c(p[1] - 30, p[2] - 30, 60, 60), area)) next
  stroke(p[1], p[2]); Sys.sleep(0.6)
  if (ugUp()) { hit <- TRUE; break }
  if (!still(12)) break
}
ok("a stroke over one raises the warning", hit)

cat("\n=== 5. hint 10: the underground warning ===\n")
ok("hint 10 follows at once", waitFor(hz(10), 8)); Sys.sleep(0.6)
w <- wins()
ok("one window, on the warning's ignore button",
   length(w) == 1 && near(w[[1]], rectOf(paste(UGBOX, "button[onclick]")), 8))
ok("...with a Next button", hasNext())
k <- card(); n <- unlist(rectOf(UGBOX))
ok("the card keeps off the warning", k[2] + k[4] <= n[2] || k[1] + k[3] <= n[1])
shot("10_underground")
t0 <- Sys.time(); clickEl(paste(UGBOX, "button[onclick]")); Sys.sleep(1.2)
ok("the button works through the window (element ignored)", is.null(rectOf(paste(UGBOX, "button[onclick]"))))
ok("...and the hint stays a moment", still(13))
ok("hint 11 follows", waitFor(hz(11), 20))
el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
ok("...about 3 s after the tap (+ the pause)", el > 3.5 && el < 7, sprintf("%.1f s", el))

cat("\n=== 6. hints 11-15: the heat map ===\n")
Sys.sleep(0.6)
ok("the warning was closed: it sat on the heat controls", !ugUp())
ok("one window, on the heat button",
   length(wins()) == 1 && near(wins()[[1]], rectOf("#newVersions-heatSwitch"), 8))
ok("'Hitze' orange here too, and 'Klicken Sie hier.' at the end",
   identical(emLook("vftTutHot")[[1]], "rgb(224, 81, 25)") && grepl("Klicken Sie hier.$", text()), text())
shot("11_heat")
clickEl("#newVersions-heatSwitch")
ok("hint 12", waitFor(hz(12), 20)); Sys.sleep(0.6)
w <- wins()
ok("one window, on the progress bar",
   length(w) == 1 && near(w[[1]], rectOf("#shiny-notification-panel .shiny-notification"), 8))
hb <- unlist(rectOf("#newVersions-heatSwitch")); k0 <- card()
ok("the card sits above the heat button", k0[2] + k0[4] <= hb[2] + 2, paste(round(k0), "|", round(hb), collapse = " "))
Sys.sleep(2); k1 <- card()
ok("...and stays there as the bars change", !still(15) || max(abs(k1[1:2] - k0[1:2])) < 2,
   paste(round(k0[1:2]), "->", round(k1[1:2]), collapse = " "))
shot("12_progress")
ok("the heat map is up: hint 13", waitFor(hz(13), 180)); Sys.sleep(0.6)
ok("...computed at midday", isTRUE(js("document.querySelector('#newVersions-heatBin input[value=\"midday\"]').checked")))
ok("one window, on the times of day",
   near(wins()[[1]], rectOf("#newVersions-heatBin .shiny-options-group"), 8))
clickEl('#newVersions-heatBin input[value="afternoon"] + span')
ok("another time of day: hint 14, once its map is in", waitFor(hz(14), 180)); Sys.sleep(0.6)
strip <- js("(function(){ var e = document.querySelector('#placeholder button.vftHeatCard').closest('.vftCard').querySelector('.vftHeatIcons');
  var r = e.getBoundingClientRect(); return [r.left, r.top, r.width, r.height]; })()")
ok("one window, on the card's heat icons", length(wins()) == 1 && near(wins()[[1]], strip, 8))
ok("...two of them lit", js("document.querySelectorAll('#placeholder .vftHeatIcon:not(.vftHeatNone)').length") == 2)
k <- card(); cd <- unlist(rectOf("#placeholder button.vftHeatCard"))
ok("the card is under the scenario card, not on it", k[2] >= cd[2] + cd[4])
ok("...no Next button: the noon icon moves on", !hasNext())
shot("14_icons")
iconAt <- function(bin) js(sprintf("(function(){ var s = document.querySelector('#placeholder button.vftHeatCard').closest('.vftCard').querySelector('.vftHeatIcons');
  var e = s.querySelector('.vftHeatIcon[data-bin=\"%s\"]'); var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", bin))
clickAt(iconAt("afternoon")); Sys.sleep(1)
ok("a tap on the afternoon icon is swallowed (its map stays up)", still(17) && isTRUE(js(heatOn)))
clickAt(iconAt("midday"))
ok("the noon icon shows its map again: hint 15", waitFor(hz(15), 30)); Sys.sleep(0.6)
ok("...the noon map, at once", isTRUE(js("!!document.querySelector('#placeholder .vftHeatIcon.vftHeatShown[data-bin=\"midday\"]')")))
ok("one window, on the button, now 'Hitze ausblenden'",
   near(wins()[[1]], rectOf("#newVersions-heatSwitch"), 8) &&
   grepl("ausblenden", js("document.getElementById('newVersions-heatSwitch').innerText")))
ok("...'Damit wir wieder malen können.' on a line of its own", grepl("<br>Damit wir wieder malen", text(), fixed = TRUE), text())
clickEl("#newVersions-heatSwitch")
ok("hidden: hint 16", waitFor(hz(16), 20) && !isTRUE(js(heatOn)))

cat("\n=== 7. hints 16-19: the tools, the plan ===\n")
Sys.sleep(0.6)
w <- wins()
ok("two windows, on the eraser and the reset",
   length(w) == 2 && near(w[[1]], rectOf("#newVersions-paintEraser"), 8) &&
   near(w[[2]], rectOf("#newVersions-paintReset"), 8))
ok("both names in black, the text starting with the eraser",
   lengths(regmatches(text(), gregexpr("<em class=\"vftTutBlack\">", text()))) == 2 &&
   startsWith(text(), "Der <em class=\"vftTutBlack\">Radierer"), text())
clickEl("#newVersions-paintReset"); Sys.sleep(0.8)
ok("a hint to read: the reset takes no tap", still(19) && hasNext())
shot("16_tools")
ok("Next: hint 17", nextAndWait(20)); Sys.sleep(0.6)
ok("one window, on the plan button", near(wins()[[1]], rectOf("#newVersions-paintImport"), 8))
ok("'Das letzte Werkzeug' begins the text", startsWith(text(), "Das letzte Werkzeug"), text())
invisible(js("window.__picker = 0; document.getElementById('newVersions-planFile').addEventListener('click', function(){ window.__picker++; });"))
clickEl("#newVersions-paintImport")
ok("tapped: the tour's plan is being placed - hint 18", waitFor(hz(18), 30)); Sys.sleep(0.6)
ok("...and no file picker was opened", identical(as.integer(js("window.__picker")), 0L))
w <- wins()
ok("two windows: the map, and the box with Next",
   length(w) == 2 && near(w[[1]], rectOf("#newVersions-versionMap"), 8) &&
   near(w[[2]], rectOf("#newVersions-planImportPanel .vft-plan-panel"), 8))
ok("'Weiter' teal and larger", grepl("<b><em>Weiter</em></b>", text()), text())
k <- card(); fr <- unlist(rectOf(".vft-plan-floating"))
ok("the card is clear of the plan", k[2] + k[4] <= fr[2] || k[1] >= fr[1] + fr[3], paste(round(k), collapse = " "))
shot("18_place")
clickEl("#newVersions-planImportPanel .vft-plan-panel .btn-default"); Sys.sleep(0.6)
ok("cancel takes no tap", !is.null(rectOf(".vft-plan-floating")) && still(21))
## drag the plan: the map window takes it
mx <- fr[1] + fr[3] / 2; my <- fr[2] + fr[4] / 2
move(mx, my); mouse("mousePressed", mx, my)
for (i in 1:8) {
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = mx + 5 * i, y = my, button = "left", buttons = 1)
  Sys.sleep(0.03)
}
mouse("mouseReleased", mx + 40, my); Sys.sleep(0.4)
ok("the plan can be dragged through the window", unlist(rectOf(".vft-plan-floating"))[1] > fr[1] + 30)
placed <- unlist(rectOf(".vft-plan-floating"))
clickEl("#newVersions-planImportPanel .vft-plan-panel .btn-success")
ok("Next: the colours - hint 19", waitFor(hz(19), 30)); Sys.sleep(0.6)
ok("one window, on the colour card", length(wins()) == 1 && near(wins()[[1]], rectOf(".vft-plan-card"), 8))
ok("...a hint to read now, with a Next button", hasNext())
rows <- js("Array.from(document.querySelectorAll('.vft-plan-colrow select')).map(function(s){ return s.value; }).join(',')")
ok("the plan's seven colours, each with a material of its own (the darker roof merged into the grey)",
   identical(sort(strsplit(rows, ",")[[1]]), as.character(c(1:5, 7:8))), rows)
ok("the toggle reads 'Ergebnis anzeigen'",
   identical(js("document.querySelector('.vft-plan-card .vft-plan-show-result').textContent"), "Ergebnis anzeigen"))
shot("19_colours")
clickEl(".vft-plan-card .vft-plan-right > .vft-plan-row .btn-success"); Sys.sleep(0.6)
ok("Apply takes no tap yet", !is.null(rectOf(".vft-plan-card")) && still(22))

cat("\n=== 8. hints 20-24: the colours, by hand ===\n")
ok("Next: hint 20, the brown paths", nextAndWait(23)); Sys.sleep(0.6)
ok("one window, on the tan row's swatch and material",
   length(wins()) == 1 && near(wins()[[1]], rowRect(BROWN), 8), jsonlite::toJSON(wins(), auto_unbox = TRUE))
ok("...set to natural ground", identical(js(sprintf("%s.querySelector('select').value", BROWN)), "4"))
art <- emLook("vftTutArtificial")
ok("'künstliche' grey, one size larger, not bold",
   !is.null(art) && identical(art[[1]], "rgb(128, 128, 128)") && isTRUE(art[[2]]) && as.numeric(art[[3]]) < 700,
   paste(unlist(art), collapse = " "))
shot("20_brown")
invisible(setSelect(sprintf("%s.querySelector('select')", BROWN), "2")); Sys.sleep(1)
ok("another material does not move on", still(23))
invisible(setSelect(sprintf("%s.querySelector('select')", BROWN), "3"))
ok("artificial: hint 21", waitFor(hz(21), 15)); Sys.sleep(0.6)
ok("one window, on 'Ergebnis anzeigen'", length(wins()) == 1 && near(wins()[[1]], rectOf(".vft-plan-card .vft-plan-show-result"), 8))
ok("...no Next button", !hasNext())
clickEl(".vft-plan-card .vft-plan-show-result")
ok("the result shown: hint 22", waitFor(hz(22), 15)); Sys.sleep(0.8)
ok("...the plan shown as uploaded again, where the darker grey shows",
   isTRUE(js("document.querySelector('.vft-plan-card .vft-plan-show-original').classList.contains('active')")))
w <- wins(); vc <- unlist(rectOf(".vft-plan-card canvas.vft-plan-view"))
roof <- c(vc[1] + vc[3] * 470 / 720, vc[2] + vc[4] * 160 / 720, vc[3] * 230 / 720, vc[4] * 64 / 720)
ok("two windows: the darker roof on the plan, and 'Farbe aufnehmen'",
   length(w) == 2 && near(w[[1]], roof, 5) && near(w[[2]], rectOf(".vft-plan-card .vft-plan-pickbtn"), 8),
   jsonlite::toJSON(w, auto_unbox = TRUE))
ok("'Farbe aufnehmen' teal and larger", grepl("<b><em>Farbe aufnehmen</em></b>", text()), text())
shot("22_pick")
clickEl(".vft-plan-card .vft-plan-pickbtn"); Sys.sleep(0.4)
click(vc[1] + vc[3] * 0.1, vc[2] + vc[4] * 0.9); Sys.sleep(0.6)
ok("a tap on the plan outside the roof is swallowed", still(25) &&
   isTRUE(js("document.querySelector('.vft-plan-card .vft-plan-pickbtn').classList.contains('active')")))
click(roof[1] + roof[3] / 2, roof[2] + roof[4] / 2)
ok("the roof picked: hint 23", waitFor(hz(23), 15)); Sys.sleep(0.6)
share <- js(sprintf("parseFloat(%s.querySelector('.vft-plan-share').textContent)", PICKED))
ok("...a new row, holding the roof (about 2 %% of the plan)", is.numeric(share) && share > 1.5 && share < 3, share)
ok("one window, on the new row's swatch and material", length(wins()) == 1 && near(wins()[[1]], rowRect(PICKED), 8))
ca <- emLook("vftTutCanopyArt")
ok("'Künstliche Krone' in the artificial canopy's light grey", !is.null(ca) && identical(ca[[1]], "rgb(224, 224, 224)"),
   paste(unlist(ca), collapse = " "))
shot("23_assign")
invisible(setSelect(sprintf("%s.querySelector('select')", PICKED), "6"))
ok("artificial canopy: hint 24", waitFor(hz(24), 15)); Sys.sleep(0.6)
ok("one window, on Apply", length(wins()) == 1 &&
   near(wins()[[1]], rectOf(".vft-plan-card .vft-plan-right > .vft-plan-row .btn-success"), 8))
ok("'Anwenden' teal and larger", grepl("<b><em>Anwenden</em></b>", text()), text())
clickEl(".vft-plan-card .vft-plan-right > .vft-plan-row .btn-default"); Sys.sleep(0.6)
ok("back and cancel take no tap", !is.null(rectOf(".vft-plan-card")) && still(27))
Sys.sleep(1.5)   # the material just set is classified 150 ms after the change
clickEl(".vft-plan-card .vft-plan-right > .vft-plan-row .btn-success")

cat("\n=== 9. hint 25: the plan painted; the confirm is passed over ===\n")
ok("Apply: hint 25", waitFor(hz(25), 20)); Sys.sleep(0.6)
ok("...the plan applied", is.null(rectOf(".vft-plan-card")))
ok("one window, where the plan was placed", length(wins()) == 1 && near(wins()[[1]], placed, 12),
   paste(jsonlite::toJSON(wins(), auto_unbox = TRUE), "|", paste(round(placed), collapse = ",")))
ok("...with a Next button", hasNext())
ok("the confirm is disabled behind this door (no path network)",
   isTRUE(js("document.getElementById('newVersions-newVersionsConfirmButton').disabled")))
shot("25_painted")
clickEl(".vftTutorialNext")
ok("Next ends the tour: hint 26 is passed over", waitFor("vftTutorialState().key === null", 20))
ok("...and the tour recorded as done", grepl("hitze", js("localStorage.getItem('vft.tutorial.done.v1')")))

cat("\n=== 10. with the Original selected ===\n")
Sys.sleep(1.5)
cardRect <- function(i) js(sprintf("(function(){ var r = document.querySelectorAll('#placeholder .vftCard button[id*=versionBtn]')[%d].getBoundingClientRect();
  return [r.left, r.top, r.width, r.height]; })()", i - 1))
## the applied plan is still being stored and checked for underground
## elements: wait for the page to be idle, and tap again if it was not
shot("29_after_tour")
for (attempt in 1:3) {
  invisible(waitFor("!document.documentElement.classList.contains('shiny-busy') && !jQuery('#shiny-modal').is(':visible')", 60))
  Sys.sleep(1)
  cr <- unlist(cardRect(1)); click(cr[1] + cr[3] / 2, cr[2] + cr[4] / 3)
  if (waitFor("(function(){ var g = document.querySelector('#newVersions-paintColor_grass'); return !!g && g.disabled; })()", 20)) break
}
Sys.sleep(2)
ok("the Original is selected: the palette is shut", identical(cards(), "Original*,Neu"), cards())
clickEl("#helpButton")
ok("the help button offers this page's tour", waitFor("jQuery('#shiny-modal .vftTutorialStartBtn').is(':visible')", 10))
Sys.sleep(0.6); clickEl("#shiny-modal .vftTutorialStartBtn")
ok("hint 1", waitFor(hz(1), 30)); Sys.sleep(0.8)
## the plan's trees may have run into an underground element as it was
## applied: its warning's ignore button is then up, and its hint counted
ugOpen <- isTRUE(js(sprintf("!!document.querySelector('%s button[onclick]')", UGBOX)))
want <- if (ugOpen) "1 / 25" else "1 / 24"
ok(sprintf("the counter takes in the 'select a scenario' hint: %s", want), identical(count(), want),
   paste(count(), if (ugOpen) "(an underground warning is open)" else ""))
ok("Next: hint 2", nextAndWait(2))
ok("Next: hint 3", nextAndWait(3))
ok("Next: the scenarios page's hint 4", nextAndWait(6)); Sys.sleep(0.6)
ok("...with its own text ('Original' in grey)", grepl("<em class=\"vftTutGrey\">Original</em>", text()), text())
ok("one window, on the other scenario's card",
   length(wins()) == 1 && near(wins()[[1]], cardRect(2), 8))
shot("30_select")
cr <- unlist(cardRect(2)); click(cr[1] + cr[3] / 2, cr[2] + cr[4] / 3)
ok("selected: hint 4, the palette open again", waitFor(hz(4), 60))
Sys.sleep(0.6)
ok("...on the grass button", near(wins()[[1]], rectOf("#newVersions-paintColor_grass"), 8))
stopTour()

cat(sprintf("\n%d check(s) failed (screenshots in %s)\n", fails, SHOTS))
quit(status = if (fails == 0) 0 else 1)
