## Browser check of step 4's tour (inst/app/www/vft-tutorial.js), in the REAL
## app, in headless Chrome, with real mouse events through the DevTools
## protocol. Steps 1 and 3's tours are walked quickly to get there, and step
## 4's tour has to start by itself when the ring lands on step 4 (chaining).
## Then its six hints: step 4's nav button, a cut with the scissors (the area
## count has to go up), a new area drawn, reset, the automatic corrections
## (tapped, and the tour waits for them to finish), and confirm - whose tap
## ends the tour, and step 5's tour has to follow. On step 5, the help
## button's modal has to grey the "no simulation yet" picture too.
## data-raw/verify_tutorial_step3_browser.R covers step 3's tour in detail.
##
## Needs the app running with the nav bar on, e.g.
##   VFT_NAV=1, pkgload::load_all("."), shiny::runApp(system.file("app",
##   package = "visitorFlowTool"), port = 7781)
## Run:  Rscript data-raw/verify_tutorial_step4_browser.R [url]
## (Rscript's tempdir goes with it: set VFT_SHOTS to keep the screenshots.)
args <- commandArgs(trailingOnly = TRUE)
URL  <- if (length(args)) args[[1]] else Sys.getenv("VFT_URL", "http://127.0.0.1:7781")
SHOTS <- Sys.getenv("VFT_SHOTS", file.path(tempdir(), "tutorial_step4_shots"))
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
drag <- function(x0, y0, x1, y1, n = 12) {
  move(x0, y0); mouse("mousePressed", x0, y0)
  for (s in seq_len(n)) {
    b$Input$dispatchMouseEvent(type = "mouseMoved", x = x0 + (x1 - x0) * s / n,
                               y = y0 + (y1 - y0) * s / n, button = "left", buttons = 1)
    Sys.sleep(0.03)
  }
  mouse("mouseReleased", x1, y1); Sys.sleep(0.12)
}
key <- function(k, code) {
  b$Input$dispatchKeyEvent(type = "keyDown", key = k, code = k, windowsVirtualKeyCode = code)
  b$Input$dispatchKeyEvent(type = "keyUp", key = k, code = k, windowsVirtualKeyCode = code)
  Sys.sleep(0.15)
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
atHint <- function(i, key = "step4") sprintf("(function(){ var s = vftTutorialState();
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
overlap <- function(a, r) {
  a <- unlist(a); r <- unlist(r)
  a[1] < r[1] + r[3] - 1 && a[1] + a[3] > r[1] + 1 && a[2] < r[2] + r[4] - 1 && a[2] + a[4] > r[2] + 1
}
ringIs <- function(id) sprintf("(function(){ var e = document.querySelector('#vftNav .vft-nav-current');
                                  return !!e && e.id === '%s'; })()", id)
stored <- function() js("(function(){ try { var s = localStorage.getItem('vft.tutorial.v1'); return s ? JSON.parse(s).status : null; } catch(e){ return null; } })()")
hasNext <- function() isTRUE(js("!!document.querySelector('.vftTutorialBox .vftTutorialNext')"))
cardRect <- function() rectOf(".vftTutorialBox")
MAP  <- "#step1-areaSelectMap"
MAP4 <- "#step4-finalAOIMap"

#the step 4 map's own helpers, in the page
HELPERS <- "
window.__t4 = {
  map: function () { var w = HTMLWidgets.find('#step4-finalAOIMap'); return w && w.getMap && w.getMap(); },
  features: function () {
    var m = this.map(), out = [];
    var g = m && m.layerManager && m.layerManager.getLayerGroup('eraseable', false);
    if (g) g.eachLayer(function (l) { if (l.eachLayer) l.eachLayer(function (f) { out.push(f); }); else out.push(l); });
    return out;
  },
  count: function () { return this.features().length; },
  // the outline and the areas together, 24 px out, clipped to the map
  frame: function () {
    var m = this.map(), b = null;
    var add = function (l) { var lb = l.getBounds(); b = b ? b.extend(lb) : L.latLngBounds(lb.getSouthWest(), lb.getNorthEast()); };
    var g = m && m.layerManager && m.layerManager.getLayerGroup('perimeter', false);
    if (g) g.eachLayer(add);
    this.features().forEach(add);
    if (!b) return null;
    var c = m.getContainer().getBoundingClientRect();
    var nw = m.latLngToContainerPoint(b.getNorthWest()), se = m.latLngToContainerPoint(b.getSouthEast());
    var l = Math.max(c.left, c.left + nw.x - 24), t = Math.max(c.top, c.top + nw.y - 24);
    var r = Math.min(c.right, c.left + se.x + 24), bt = Math.min(c.bottom, c.top + se.y + 24);
    return [l, t, r - l, bt - t];
  },
  // a point on the map with no area under it (a first click there would erase it)
  free: function (x, y) {
    var c = this.map().getContainer(), e = document.elementFromPoint(x, y);
    if (!e || !c.contains(e)) return false;
    if (e.closest('.vftTutorialBox')) return false;
    return !(e.classList && e.classList.contains('leaflet-interactive'));
  },
  // the largest area on screen, and a line across it from edge to edge whose
  // two ends are both free and inside the window w
  cutLine: function (w) {
    var m = this.map(), self = this, mc = m.getContainer().getBoundingClientRect();
    var c = { left: w[0] + 4, top: w[1] + 4, right: w[0] + w[2] - 4, bottom: w[1] + w[3] - 4 };
    var fs = this.features().map(function (f) {
      var b = f.getBounds(), nw = m.latLngToContainerPoint(b.getNorthWest()), se = m.latLngToContainerPoint(b.getSouthEast());
      return { l: mc.left + nw.x, t: mc.top + nw.y, r: mc.left + se.x, b: mc.top + se.y };
    }).sort(function (a, b) { return (b.r - b.l) * (b.b - b.t) - (a.r - a.l) * (a.b - a.t); });
    for (var i = 0; i < fs.length; i++) {
      var f = fs[i];
      for (var k = 1; k < 10; k++) {
        var y = f.t + (f.b - f.t) * k / 10;
        for (var d = 8; d <= 40; d += 8) {
          var x0 = f.l - d, x1 = f.r + d;
          if (x0 > c.left && x1 < c.right && y > c.top && y < c.bottom &&
              self.free(x0, y) && self.free(x1, y)) return [x0, y, x1, y];
        }
      }
    }
    return null;
  },
  // a free point inside the given rect, on a grid
  freeIn: function (r) {
    for (var fy = 0.15; fy < 0.9; fy += 0.07)
      for (var fx = 0.15; fx < 0.9; fx += 0.07) {
        var x = r[0] + r[2] * fx, y = r[1] + r[3] * fy;
        if (this.free(x, y) && this.free(x + 70, y) && this.free(x + 35, y + 60)) return [x, y];
      }
    return null;
  }
};
true"

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

cat("=== 1. to step 4, through step 1's and step 3's tours ===\n")
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
invisible(waitFor(atHint(6, "step1"), 10))
Sys.sleep(0.6); clickEl(".vftTutorialNext")
ok("step 3's tour starts by itself", waitFor(atHint(1, "step3"), 300))
Sys.sleep(0.6); clickEl(".vftTutorialNext")
invisible(waitFor(atHint(2, "step3"), 10)); Sys.sleep(0.6); clickEl(".vftTutorialNext")
ok("...its slider hint", waitFor(atHint(3, "step3"), 10))
Sys.sleep(0.6)
IRS <- "jQuery('#step3-AOISlider').data('ionRangeSlider')"
xFor <- function(v) js(sprintf("(function(){ var d = %s,
  line = d.$cache.line[0].getBoundingClientRect(), hw = d.$cache.s_single[0].getBoundingClientRect().width;
  return line.left + hw / 2 + (line.width - hw) * Math.round(%f * 10) / 200; })()", IRS, v))
h <- js(sprintf("(function(){ var r = %s.$cache.s_single[0].getBoundingClientRect();
  return [r.left + r.width / 2, r.top + r.height / 2]; })()", IRS))
drag(h[[1]], h[[2]], xFor(7.9), h[[2]])
invisible(waitFor(atHint(4, "step3"), 20)); Sys.sleep(0.6); clickEl(".vftTutorialNext")
ok("...its confirm hint", waitFor(atHint(5, "step3"), 10))
Sys.sleep(0.6); clickEl("#step3-confirmButton3")
ok("the ring lands on step 4", waitFor(ringIs("vftNav_step4"), 120))

cat("\n=== 2. hint 1: Edit AoIs ===\n")
ok("step 4's tour starts by itself", waitFor(atHint(1), 240), paste(unlist(tut()), collapse = ","))
invisible(js(HELPERS))
ok("...once step 4's map is up, not over step 3's page",
   isTRUE(js("(function(){ var m = __t4.map(); return !!m && !!m._loaded &&
                m.getContainer().offsetParent !== null; })()")) &&
   !isTRUE(js("(function(){ var e = document.getElementById('step3-AOIMap');
                            return !!e && e.offsetParent !== null; })()")))
Sys.sleep(0.8)
start <- js("__t4.count()")
ok("the map shows the areas step 3 made", is.numeric(start) && start > 0, start)
w <- wins()
ok("one window, on step 4's nav button", length(w) == 1 && near(w[[1]], rectOf("#vftNav_step4"), 6),
   paste(unlist(w), collapse = ","))
ok("...with a Next button", hasNext())
ok("the text is step 4's first", isTRUE(js("/sub-step|Teilschritt|sous-étape/.test(document.querySelector('.vftTutorialText').textContent)")))
fill <- js("(function(){ var e = document.querySelector('.vftTutorialText em.vftTutAoiFill'),
  t = document.querySelector('.vftTutorialText'); if (!e) return null;
  return [getComputedStyle(e).color, parseFloat(getComputedStyle(e).fontSize), parseFloat(getComputedStyle(t).fontSize),
          getComputedStyle(e).fontWeight]; })()")
ok("'Zielgebiete' in the map's fill green, bold, one size larger",
   !is.null(fill) && identical(fill[[1]], "rgb(77, 166, 77)") && fill[[2]] > fill[[3]] && as.numeric(fill[[4]]) >= 700,
   paste(unlist(fill), collapse = " "))
shot("1_edit_aois")
clickEl(".vftTutorialNext")

cat("\n=== 3. hint 2: cut with the scissors ===\n")
ok("Next moves on to hint 2", waitFor(atHint(2), 10))
Sys.sleep(0.8)
w <- wins()
fr <- js("__t4.frame()")
ok("one window, round the outline and the areas", length(w) == 1 && !is.null(fr) &&
     near(w[[1]], fr, 6), paste(c(unlist(w), "|", round(unlist(fr))), collapse = ","))
ok("...and no Next button", !hasNext())
## the window may be most of the map: the card goes above or below it, or
## else hugs the top or bottom of the screen, covering as little of it as it
## can - never parked in its middle
k <- unlist(cardRect()); vh <- js("window.innerHeight")
ov <- if (length(w) == 1) max(0, min(k[2] + k[4], w[[1]][[2]] + w[[1]][[4]]) - max(k[2], w[[1]][[2]])) else NA
ok("the card is clear of the window, or at the top or bottom of the screen",
   length(k) == 4 && !is.na(ov) && (ov == 0 || k[2] <= 20 || k[2] + k[4] >= vh - 20),
   paste(round(k), collapse = ","))
ok("...overlapping the window by less than half its own height", !is.na(ov) && ov < k[4] / 2,
   sprintf("%.0f of %.0f px", ov, k[4]))
## the same on a window too tall for that: the map zoomed in until the areas
## fill it
invisible(js("(function(){ __t4.map().zoomIn(2, {animate: false}); return null; })()")); Sys.sleep(1.2)
w2 <- wins(); k <- unlist(cardRect())
ov <- if (length(w2) == 1) max(0, min(k[2] + k[4], w2[[1]][[2]] + w2[[1]][[4]]) - max(k[2], w2[[1]][[2]])) else NA
ok("zoomed in (window the whole map): the card hugs the top or bottom",
   length(k) == 4 && (k[2] <= 20 || k[2] + k[4] >= vh - 20) && !is.na(ov) && ov < k[4] / 2,
   sprintf("card %s, overlap %.0f", paste(round(k), collapse = ","), ov))
shot("2_cut_zoomed")
invisible(js("(function(){ __t4.map().zoomOut(2, {animate: false}); return null; })()")); Sys.sleep(1.2)
w <- wins()
ic <- js("(function(){ var row = document.querySelector('.vftTutorialText .vftTutIconRow'); if (!row) return null;
  var t = document.querySelector('.vftTutorialText').getBoundingClientRect();
  var out = Array.from(row.querySelectorAll('i')).map(function (e) {
    var r = e.getBoundingClientRect(); return [e.className, r.width, getComputedStyle(e).backgroundImage.indexOf('svg') >= 0,
      getComputedStyle(e, '::before').content]; });
  var r = row.getBoundingClientRect();
  return {icons: out, centred: Math.abs((r.left + r.right) / 2 - (t.left + t.right) / 2) < 3}; })()")
ok("the three cut pictures are drawn side by side, centred in the text, numbered",
   !is.null(ic) && length(ic$icons) == 3 && all(vapply(ic$icons, function(x) x[[2]] > 40 && isTRUE(x[[3]]), NA)) &&
   isTRUE(ic$centred) &&
   identical(vapply(ic$icons, function(x) gsub("\"", "", x[[4]]), ""), c("1)", "2)", "3)")),
   jsonlite::toJSON(ic, auto_unbox = TRUE))
shot("2_cut")

clickEl("#step4-confirmButton4"); Sys.sleep(1)
ok("a tap on confirm, outside the window, is swallowed",
   isTRUE(js(atHint(2))) && isTRUE(js(ringIs("vftNav_step4"))))

line <- js(sprintf("(function(){ try { return __t4.cutLine([%s]); } catch (e) { return 'ERR ' + e.message; } })()",
                   paste(unlist(w[[1]]), collapse = ",")))
ok("a line across an area, both ends free", is.list(line) && length(line) == 4,
   if (is.character(line)) line else "")
if (!is.list(line)) cat(js(sprintf("(function(){ var w = [%s], out = [];
  [[0.1, 0.5], [0.5, 0.5], [0.9, 0.5], [0.3, 0.2]].forEach(function (f) {
    var x = w[0] + w[2] * f[0], y = w[1] + w[3] * f[1], e = document.elementFromPoint(x, y);
    out.push(Math.round(x) + ',' + Math.round(y) + ' ' + (e ? e.tagName + '.' + (e.getAttribute('class') || '') + '#' + e.id : 'none'));
  });
  return out.join('\\n'); })()", paste(unlist(w[[1]]), collapse = ","))), "\n")
if (!(is.list(line) && length(line) == 4)) { b$close(); quit(status = 1) }
line <- unlist(line)
click(line[1], line[2])
key("Escape", 27)
ok("Esc takes the point back and the tour stays on", isTRUE(js(atHint(2))))

click(line[1], line[2]); click(line[3], line[4]); Sys.sleep(0.4)
ok("the scissors are on the second point",
   isTRUE(js("!!document.querySelector('#step4-finalAOIMap .vft-pd-scissors')")))
shot("2b_scissors")
Sys.sleep(1.5)
ok("...and nothing moves on before they are pressed", isTRUE(js(atHint(2))))
clickEl("#step4-finalAOIMap .vft-pd-scissors")
ok("the scissors split an area: hint 3", waitFor(atHint(3), 30))
cut <- js("__t4.count()")
ok("...the map shows more areas", is.numeric(cut) && cut > start, paste(start, "->", cut))

cat("\n=== 4. hint 3: a new area ===\n")
Sys.sleep(0.8)
w <- wins()
ok("one window, round the outline and the areas again", length(w) == 1 && near(w[[1]], js("__t4.frame()"), 6))
ok("...and no Next button", !hasNext())
pd <- js("(function(){ var e = document.querySelector('.vftTutorialText .vftTutPolyDraw'); if (!e) return null;
  var r = e.getBoundingClientRect(), t = document.querySelector('.vftTutorialText').getBoundingClientRect();
  var prev = e.previousSibling, next = e.nextElementSibling;
  return [Math.abs((r.left + r.right) / 2 - (t.left + t.right) / 2), next && next.tagName === 'BR' ? next.offsetHeight : -1]; })()")
ok("the three-vertex picture is centred, no empty line under it",
   !is.null(pd) && pd[[1]] < 3 && pd[[2]] <= 0, paste(unlist(pd), collapse = " "))
shot("3_new_area")
p <- unlist(js(sprintf("__t4.freeIn([%s])", paste(unlist(w[[1]]), collapse = ","))))
ok("a free spot to draw in", length(p) == 2)
tri <- list(p, p + c(70, 0), p + c(35, 60))
for (q in tri) click(q[1], q[2])
move(p[1] + 30, p[2] + 20); move(p[1], p[2]); click(p[1], p[2])
ok("the new area moves on to hint 4", waitFor(atHint(4), 30))
drawn <- js("__t4.count()")
ok("...and it is on the map", is.numeric(drawn) && drawn >= 1, paste(cut, "->", drawn))

cat("\n=== 5. hint 4: reset ===\n")
Sys.sleep(0.6)
w <- wins()
ok("one window, on the reset button", length(w) == 1 && near(w[[1]], rectOf("#step4-resetButton")))
ok("...and no Next button", !hasNext())
grey <- js("(function(){ var e = document.querySelector('.vftTutorialText em.vftTutGrey'),
  t = document.querySelector('.vftTutorialText'); if (!e) return null;
  return [getComputedStyle(e).color, parseFloat(getComputedStyle(e).fontSize), parseFloat(getComputedStyle(t).fontSize)]; })()")
ok("'reset' dark grey", !is.null(grey) && identical(grey[[1]], "rgb(74, 79, 78)"), paste(unlist(grey), collapse = " "))
ok("...and one size larger", !is.null(grey) && grey[[2]] > grey[[3]])
shot("4_reset")
clickEl("#step4-resetButton")
ok("the tap moves on to hint 5", waitFor(atHint(5), 15))
ok("...and the areas are step 3's again", waitFor(sprintf("__t4.count() === %d", start), 10),
   js("__t4.count()"))

cat("\n=== 6. hint 5: the automatic corrections ===\n")
Sys.sleep(0.6)
w <- wins()
ok("one window, on the 'Automatic Corrections' button",
   length(w) == 1 && near(w[[1]], rectOf("#step4-autoCutButton")))
ok("...and no Next button: it is to be tried", !hasNext())
ok("the button is renamed",
   grepl("Korrekturen|Corrections", js("document.getElementById('step4-autoCutButton').innerText")),
   js("document.getElementById('step4-autoCutButton').innerText"))
ok("'Probieren Sie es aus!' teal and larger, 'Zielgebiete' in the fill green",
   isTRUE(js("(function(){ var b = document.querySelector('.vftTutorialText b em'),
     g = document.querySelector('.vftTutorialText em.vftTutAoiFill');
     return !!b && /Probieren/.test(b.textContent) && getComputedStyle(b).color === 'rgb(0, 98, 104)' && !!g; })()")))
shot("5_autocorrect")
before <- js("__t4.count()")
clickEl("#step4-autoCutButton")
ok("the tap starts them: the button is disabled, the tour waits on hint 5",
   waitFor("document.getElementById('step4-autoCutButton').disabled", 10) && isTRUE(js(atHint(5))))
Sys.sleep(1)
w <- wins()
ok("...the progress bar gets a window of its own beside the button", length(w) == 2,
   paste(unlist(w), collapse = ","))
shot("5b_autocorrect_running")

cat("\n=== 7. hint 6: confirm ===\n")
ok("their end moves on to hint 6", waitFor(atHint(6), 120), paste(unlist(tut()), collapse = ","))
ok("...with the button back and the corrected areas drawn",
   isTRUE(js("!document.getElementById('step4-autoCutButton').disabled")) && is.numeric(js("__t4.count()")),
   paste(before, "->", js("__t4.count()")))
Sys.sleep(0.6)
w <- wins()
ok("one window, on the confirm button", length(w) == 1 && near(w[[1]], rectOf("#step4-confirmButton4")))
ok("...and no Next button", !hasNext())
teal <- js("(function(){ var e = document.querySelector('.vftTutorialText b em'),
  t = document.querySelector('.vftTutorialText'); if (!e) return null;
  return [getComputedStyle(e).color, parseFloat(getComputedStyle(e).fontSize), parseFloat(getComputedStyle(t).fontSize)]; })()")
ok("'confirm' teal", !is.null(teal) && identical(teal[[1]], "rgb(0, 98, 104)"), paste(unlist(teal), collapse = " "))
ok("...and one size larger", !is.null(teal) && teal[[2]] > teal[[3]])
shot("6_confirm")
clickEl("#step4-confirmButton4")
ok("the tap ends the tour", waitFor("(function(){ var s = vftTutorialState(); return s.key !== 'step4'; })()", 5))
ok("...stored as done", identical(stored(), "done"))
ok("...and the app moves on to step 5", waitFor(ringIs("vftNav_step5"), 120))
#the Original simulates itself as step 5 opens, so its launch hint is passed
#over and the tour opens on the progress bars
ok("step 5's tour starts by itself, on the running simulation (hint 2)",
   waitFor(atHint(2, "step5"), 120) && identical(tut()$count, "1 / 8"),
   paste(unlist(tut()), collapse = ","))
ok("...with the arrival text, 'Naherholungssimulation' teal and larger",
   isTRUE(js("(function(){ var t = document.querySelector('.vftTutorialText'), b = t && t.querySelector('b em');
     return /Ankunft/.test(t.textContent) && !!b && /Naherholungssimulation/.test(b.textContent); })()")),
   js("document.querySelector('.vftTutorialText').textContent"))
shot("7_step5")
## The "no simulation yet" picture covers the map while the simulation runs.
## It used to sit above a modal's grey backdrop (z-index 1200 > 1040): opened
## now, the help button's tutorial modal has to grey it like the rest.
px <- function(x, y) {
  f <- tempfile(fileext = ".png")
  writeBin(jsonlite::base64_dec(b$Page$captureScreenshot(format = "png")$data), f)
  p <- png::readPNG(f); round(p[y, x, 1:3] * 255)
}
clickEl(".vftTutorialStop")
invisible(waitFor("!document.getElementById('vftTutorial')", 5))
ph <- js("(function(){ var p = document.getElementById('step5-mapPlaceholder');
  if (!p || p.offsetParent === null) return null; var r = p.getBoundingClientRect();
  return [Math.round(r.left + 8), Math.round(r.top + 8)]; })()")
if (is.null(ph)) {
  cat("   (the picture is gone already - the simulation was done; check skipped)\n")
} else {
  bare <- px(ph[[1]], ph[[2]])
  clickEl("#helpButton")
  invisible(waitFor("jQuery('#shiny-modal').is(':visible') && !!document.querySelector('#shiny-modal .vftTutorialStartBtn')", 10))
  Sys.sleep(1)
  greyed <- px(ph[[1]], ph[[2]])
  ok("the help modal's backdrop greys the 'no simulation yet' picture too",
     all(bare >= 240) && all(greyed < 200), paste(c(bare, "->", greyed), collapse = " "))
  shot("7b_help_over_placeholder")
}

cat(sprintf("\nscreenshots in %s\n", SHOTS))
b$close()
cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
