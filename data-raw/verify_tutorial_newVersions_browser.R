## Browser check of the scenarios page's tour (inst/app/www/vft-tutorial.js,
## TOURS.newVersions) and of the heat mitigation hint it hands on to on step 5
## (TOURS.toHitze), in the REAL app, in headless Chrome, with real mouse events
## through the DevTools protocol. Steps 1, 3 and 4 are walked quickly to get
## there. Then:
##   * the tour as most users meet it - the page's seeded "Neu" scenario is
##     selected, so hints 2-4 are passed over - through the network (a path's
##     qualities, deleting it, nodes, a new path, deleting a node), the parking
##     context, and the confirm back to step 5, where the heat hint follows;
##   * with the Original alone (hints 2-4: make, name and select a scenario),
##     and with the Original selected beside another (hint 4 only).
## Taps on the map are found with Leaflet's own geometry (the probes below),
## always clear of the tutorial's card.
##
## Needs the app running with the nav bar on, e.g.
##   VFT_NAV=1, pkgload::load_all("."), shiny::runApp(system.file("app",
##   package = "visitorFlowTool"), port = 7781)
## Run:  Rscript data-raw/verify_tutorial_newVersions_browser.R [url]
args <- commandArgs(trailingOnly = TRUE)
URL  <- if (length(args)) args[[1]] else Sys.getenv("VFT_URL", "http://127.0.0.1:7781")
SHOTS <- Sys.getenv("VFT_SHOTS", file.path(tempdir(), "tutorial_newVersions_shots"))
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
atHint <- function(i, key = "newVersions") sprintf("(function(){ var s = vftTutorialState();
  return s.key === '%s' && s.idx === %d && !s.quiet && !s.pause; })()", key, i - 1)
still  <- function(i, key = "newVersions") isTRUE(js(atHint(i, key)))
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
nextAndWait <- function(i, key = "newVersions") { Sys.sleep(0.4); clickEl(".vftTutorialNext"); waitFor(atHint(i, key), 60) }
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
selected <- function() js(sprintf("(function(){ var m = %s.layerManager.getLayer('marker','XXX');
  if (!m || !m._icon) return null; var r = m._icon.getBoundingClientRect();
  return [r.left + r.width/2, r.top + r.height/2]; })()", MAPJS))

## Points on the map, inside the hint's first window and clear of the card:
## on a path (away from any node), on a node, on nothing (no path or node
## within 25 px). Leaflet's own projected geometry, as the tutorial's hit test.
PROBES <- sprintf("window.__nvProbe = (function(){
  function map(){ return %s; }
  function win(){ var w = JSON.parse(document.querySelector('.vftTutorialCanvas').getAttribute('data-windows'))[0];
    return {l: w[0] + 20, t: w[1] + 20, r: w[0] + w[2] - 20, b: w[1] + w[3] - 20}; }
  function group(g){ var out = [], lg = map().layerManager.getLayerGroup(g, false);
    if (lg) lg.eachLayer(function(l){ out.push(l); }); return out; }
  function toPage(p){ var r = map().getContainer().getBoundingClientRect();
    var c = map().layerPointToContainerPoint(p); return [r.left + c.x, r.top + c.y]; }
  function toLayer(x, y){ var r = map().getContainer().getBoundingClientRect();
    return map().containerPointToLayerPoint(L.point(x - r.left, y - r.top)); }
  function nearNode(x, y, d){ var p = toLayer(x, y);
    return group('nodes').some(function(n){ return n._point && n._point.distanceTo(p) < d; }); }
  function nearPath(x, y, d){ var p = toLayer(x, y), hit = false;
    map().eachLayer(function(l){ if (!hit && l instanceof L.Polyline && !(l instanceof L.Polygon) && l._parts) {
      l._parts.forEach(function(pt){ for (var i = 1; i < pt.length; i++)
        if (L.LineUtil.pointToSegmentDistance(p, pt[i-1], pt[i]) < d) hit = true; }); } });
    return hit; }
  function ok(q, w){ var c = document.querySelector('.vftTutorialBox'), k = c && c.getBoundingClientRect();
    if (k && q[0] > k.left - 12 && q[0] < k.right + 12 && q[1] > k.top - 12 && q[1] < k.bottom + 12) return false;
    return q[0] > w.l && q[0] < w.r && q[1] > w.t && q[1] < w.b; }
  return {
    path: function(n){ var w = win(), found = [];
      group('paths').forEach(function(l){ (l._parts || []).some(function(pt){
        for (var i = 1; i < pt.length; i++) { var a = pt[i-1], b = pt[i];
          if (a.distanceTo(b) < 50) continue;
          var q = toPage(L.point((a.x + b.x) / 2, (a.y + b.y) / 2));
          if (ok(q, w) && !nearNode(q[0], q[1], 16)) { found.push({q: q, id: String(l.options.layerId)}); return true; } }
        return false; }); });
      return found[n || 0] || null; },
    node: function(n){ var w = win(), found = [];
      group('nodes').forEach(function(l){ if (!l._point) return; var q = toPage(l._point); if (ok(q, w)) found.push(q); });
      return found[n || 0] || null; },
    empty: function(){ var w = win();
      for (var y = w.t; y < w.b; y += 7) for (var x = w.l; x < w.r; x += 7)
        if (ok([x, y], w) && !nearNode(x, y, 25) && !nearPath(x, y, 25)) return [x, y];
      return null; },
    nodes: function(){ return group('nodes').length; },
    hasShape: function(id){ return !!map().layerManager.getLayer('shape', id); }
  };
})()", MAPJS)
probe <- function(expr) { invisible(js(PROBES)); js(paste0("__nvProbe.", expr)) }

#### open the app, no bubble ####
invisible(b$Page$navigate(URL))
ok("the app connects", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
invisible(js("localStorage.setItem('vft.tutorial.v1', JSON.stringify({status: 'done', at: Date.now()}));
              localStorage.removeItem('vft.tutorial.done.v1');"))
invisible(b$Page$reload())
Sys.sleep(1)
ok("...again", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
MAP1 <- "#step1-areaSelectMap"
invisible(waitFor(sprintf("(function(){ var w = HTMLWidgets.find('%s'); var m = w && w.getMap && w.getMap();
                            return !!m && !!m._loaded; })()", MAP1), 60))
Sys.sleep(1.5)

cat("=== 1. to the scenarios page, through steps 1, 3, 4 and 5 ===\n")
stopTour <- function() invisible(js("document.querySelector('.vftTutorialStop') && document.querySelector('.vftTutorialStop').click()"))
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
Sys.sleep(0.8); clickEl("#vftNextSim")
invisible(waitFor(atHint(6, "step1"), 10))
Sys.sleep(0.6); clickEl(".vftTutorialNext")
ok("step 3's tour follows", waitFor(atHint(1, "step3"), 360))
stopTour(); Sys.sleep(1)
clickEl("#step3-confirmButton3")
ok("on step 4", waitFor(ringIs("vftNav_step4"), 180))
invisible(waitFor("(function(){ var e = document.getElementById('step4-confirmButton4');
                     return !!e && e.offsetParent !== null; })()", 180))
Sys.sleep(3); stopTour(); Sys.sleep(1)
clickEl("#step4-confirmButton4")
ok("on step 5", waitFor(ringIs("vftNav_step5"), 240))
invisible(waitFor("(function(){ var e = document.querySelector('#step5-launchSim');
                     return !!e && e.offsetParent !== null; })()", 240))
Sys.sleep(3); stopTour(); Sys.sleep(1)
clickEl("#step5-newVersionsButton")
ok("on the scenarios page", waitFor(ringIs("vftNav_newVersions"), 120))
ok("...ready", pageReady())
ok("...on the seeded 'Neu', selected", identical(cards(), "Original,Neu*"), cards())

cat("\n=== 2. the help button, hint 1 ===\n")
clickEl("#helpButton")
ok("the help button offers this page's tour", waitFor("jQuery('#shiny-modal .vftTutorialStartBtn').is(':visible')", 10))
Sys.sleep(0.6); clickEl("#shiny-modal .vftTutorialStartBtn")
ok("hint 1 starts", waitFor(atHint(1), 30))
Sys.sleep(0.8)
w <- wins()
ok("one window, on this page's nav button", length(w) == 1 && near(w[[1]], rectOf("#vftNav_newVersions"), 8))
ok("...with a Next button", hasNext())
ok("'Scenarios' teal and larger", grepl("<b><em>Szenarien</em></b>", text()), text())
ok("the counter leaves out hints 2-4 ('Neu' is selected): 1 / 20", identical(count(), "1 / 20"), count())
shot("01_nav")

cat("\n=== 3. hints 5-7: the contexts, Paths/Roads, the network ===\n")
ok("Next passes over hints 2-4, to hint 5", nextAndWait(5))
Sys.sleep(0.6)
w <- wins()
ok("one window, on the contexts",
   length(w) == 1 && near(w[[1]], rectOf("#newVersions-contextChoice .shiny-options-group"), 8))
ok("...counted 2 / 20", identical(count(), "2 / 20"), count())
ok("...'many different things in a scenario'", grepl("Szenario viele verschiedene Dinge", text()), text())
ok("Next to hint 6", nextAndWait(6))
Sys.sleep(0.6)
lab1 <- js("(function(){ var r = document.querySelector('#newVersions-contextChoice input[value=\"1\"]').closest('label').getBoundingClientRect();
  return [r.left, r.top, r.width, r.height]; })()")
w <- wins()
ok("one window, on 'Wegen/Strassen'", length(w) == 1 && near(w[[1]], lab1, 6))
ok("...'Wegen/Strassen' dark grey and larger", grepl("<em class=\"?vftTutGrey\"?>Wegen/Strassen</em>", text()), text())
ok("...with a Next button", hasNext())
clickEl("#newVersions-contextChoice input[value='3']"); Sys.sleep(0.8)
ok("a tap on another context is swallowed", still(6) && isTRUE(js("document.querySelector('#newVersions-contextChoice input[value=\"1\"]').checked")))
shot("06_pathsRoads")
lab1 <- unlist(lab1); click(lab1[1] + lab1[3] / 2, lab1[2] + lab1[4] / 2)
ok("a tap on it moves on to hint 7", waitFor(atHint(7), 60))
Sys.sleep(1)
w <- wins()
mr <- unlist(rectOf("#newVersions-versionMap"))
sq <- if (length(w)) unlist(w[[1]]) else rep(0, 4)
ok("two windows: a square inside the map...", length(w) == 2 && inside(w[[1]], mr) && abs(sq[3] - sq[4]) <= 3,
   paste(sq, collapse = ","))
kmPx <- js(sprintf("(function(){ var m = %s, c = m.getCenter();
  return m.latLngToContainerPoint(c).distanceTo(m.latLngToContainerPoint([c.lat, c.lng + 1000 / (111320 * Math.cos(c.lat * Math.PI / 180))])); })()", MAPJS))
#boundsWindow() adds 8 px on each side
ok("...1 km wide", abs(sq[3] - 16 - kmPx) < 6,sprintf("window %d px, 1 km = %.0f px", sq[3], kmPx))
leg <- js("(function(){ var l = 1e9, t = 1e9, r = -1e9, b = -1e9;
  document.querySelectorAll('#newVersions-versionMap .leaflet-control').forEach(function(c){
    if (!c.matches('.vft-legend-signage, .vft-legend-surface')) return; var q = c.getBoundingClientRect();
    l = Math.min(l, q.left); t = Math.min(t, q.top); r = Math.max(r, q.right); b = Math.max(b, q.bottom); });
  return [l, t, r - l, b - t]; })()")
ok("...and one on the signage and surface legends", length(w) == 2 && near(w[[2]], leg, 8))
ok("...the legends translated (Beschilderung, Belag)",
   isTRUE(js("/Beschilderung:/.test(document.querySelector('.vft-legend-signage').textContent) && /Belag:/.test(document.querySelector('.vft-legend-surface').textContent)")))
ok("...the card above the square, off the nodes", card()[2] + card()[4] <= sq[2], paste(card(), collapse = ","))
ok("...a hint to read", hasNext())
ok("...the node icon before '(Knoten)'", grepl("vftTutNode\"?></i>\\(Knoten\\)", text()), text())
shot("07_network")

cat("\n=== 4. hints 8-11: a path, its qualities, deleting it ===\n")
ok("Next to hint 8", nextAndWait(8))
Sys.sleep(0.8)
ok("no Next button: a path has to be clicked", !hasNext())
n0 <- probe("nodes()")
clickAt(probe("empty()")); Sys.sleep(1.5)
ok("a tap on the empty map is swallowed (no node made)", still(8) && probe("nodes()") == n0 && !modalUp("shiny-modal"))
clickAt(probe("node(0)")); Sys.sleep(1.5)
ok("a tap on a node is swallowed (none selected)", still(8) && is.null(selected()))
p <- probe("path(0)")
clickAt(p$q)
ok(sprintf("a tap on a path (%s) opens its modal", p$id), waitFor("jQuery('#newVersions-deleteEdge').is(':visible')", 10))
ok("hint 9", waitFor(atHint(9), 10))
Sys.sleep(0.8)
modalCentred <- function() {
  md <- unlist(rectOf("#shiny-modal .modal-dialog"))
  vw <- js("document.documentElement.clientWidth")
  abs((md[1] + md[3] / 2) - vw / 2) < 20
}
ok("...the modal where it always is: centred across the page", modalCentred())
ok("...over a faint backdrop", isTRUE(js("parseFloat(getComputedStyle(document.querySelector('.modal-backdrop')).opacity) < 0.2")))
radios <- js("(function(){ var l = 1e9, t = 1e9, r = -1e9, b = -1e9;
  ['pathSignage','pathType','pathWidth'].forEach(function(i){ var q = document.getElementById('newVersions-' + i).getBoundingClientRect();
    l = Math.min(l, q.left); t = Math.min(t, q.top); r = Math.max(r, q.right); b = Math.max(b, q.bottom); });
  return [l, t, r - l, b - t]; })()")
w <- wins()
ok("one window, on the three quality radio groups", length(w) == 1 && near(w[[1]], radios, 8))
ok("...a hint to read", hasNext())
ok("...the card off the delete button",
   !isTRUE(js("(function(){ var a = document.querySelector('.vftTutorialBox').getBoundingClientRect(),
     b = document.getElementById('newVersions-deleteEdge').getBoundingClientRect();
     return a.left < b.right && a.right > b.left && a.top < b.bottom && a.bottom > b.top; })()")))
clickEl("#shiny-modal input[name='newVersions-pathWidth'][value='c2']"); Sys.sleep(0.5)
ok("a radio in the window takes the tap", isTRUE(js("document.querySelector(\"input[name='newVersions-pathWidth'][value='c2']\").checked")))
clickEl("#newVersions-deleteEdge"); Sys.sleep(1)
ok("the delete button, outside the window, is swallowed", still(9) && probe(sprintf("hasShape('%s')", p$id)))
shot("09_qualities")
ok("Next to hint 10", nextAndWait(10))
Sys.sleep(0.8)
w <- wins()
edge <- js(sprintf("(function(){ var m = %s, l = m.layerManager.getLayer('shape', '%s'), r = m.getContainer().getBoundingClientRect();
  var b = l.getBounds(), a = m.latLngToContainerPoint(b.getNorthWest()), c = m.latLngToContainerPoint(b.getSouthEast());
  return [r.left + a.x, r.top + a.y, c.x - a.x, c.y - a.y]; })()", MAPJS, p$id))
ok("one window only, on the delete button", length(w) == 1 && near(w[[1]], rectOf("#newVersions-deleteEdge"), 8),
   paste(length(w), "window(s)"))
ok("'delete' in its button's red", grepl("<em class=\"?vftTutDelete\"?>", text()), text())
shot("10_delete")
clickEl("#shiny-modal .modal-footer .btn:last-child"); Sys.sleep(1)
ok("cancel is swallowed", still(10) && modalUp("shiny-modal"))
clickEl("#newVersions-deleteEdge")
ok("hint 11 once the path is deleted", waitFor(atHint(11), 10))
Sys.sleep(0.6)
ok("...it is gone from the map", !probe(sprintf("hasShape('%s')", p$id)))
w2 <- wins()
ok("...one window, where the path was", length(w2) == 1 && inside(edge, w2[[1]]), paste(unlist(edge), collapse = ","))
ok("...a hint to read", hasNext())
shot("11_gone")

cat("\n=== 5. hints 12-16: nodes, a new path ===\n")
ok("Next to hint 12", nextAndWait(12))
Sys.sleep(0.8)
ok("the node icon in the text", grepl("vftTutNode", text()))
clickAt(probe("path(1)")$q); Sys.sleep(1.5)
ok("a tap on a path is swallowed", still(12) && !modalUp("shiny-modal"))
clickAt(probe("node(10)"))
ok("a tap on a node selects it: hint 13", waitFor(atHint(13), 10) && !is.null(selected()))
Sys.sleep(0.8)
ok("...the card above the square", card()[2] + card()[4] <= unlist(wins()[[1]])[2], paste(card(), collapse = ","))
clickAt(selected()); Sys.sleep(1.5)
ok("a tap on the selected node (it would delete it) is swallowed", still(13) && !is.null(selected()))
clickAt(probe("path(2)")$q); Sys.sleep(1.5)
ok("a tap on a path (it would drop the selection) is swallowed", still(13) && !is.null(selected()))
shot("13_selected")
from <- unlist(selected())
to <- unlist(probe("empty()"))
clickAt(to)
ok("a tap on the empty map makes a node and a path: its modal",
   waitFor("jQuery('#newVersions-submitNewPath').is(':visible')", 10))
ok("hint 14", waitFor(atHint(14), 10))
Sys.sleep(0.8)
w <- wins()
ok("one window, on the modal", length(w) == 1 && near(w[[1]], rectOf("#shiny-modal .modal-content"), 8))
ok("...the modal centred across the page, not at its edge", modalCentred())
shot("14_newpath")
clickEl("#newVersions-cnclEdgNode"); Sys.sleep(1)
ok("its cancel is swallowed", still(14) && modalUp("shiny-modal"))
clickEl("#newVersions-submitNewPath")
ok("submitted: hint 15", waitFor(atHint(15), 10))
Sys.sleep(0.8)
w <- wins()
seg <- c(min(from[1], to[1]), min(from[2], to[2]), abs(from[1] - to[1]), abs(from[2] - to[2]))
ok("one window, around the new path", length(w) == 1 && inside(seg, w[[1]], 4) &&
     unlist(w[[1]])[3] < unlist(rectOf("#newVersions-versionMap"))[3] / 2,
   paste(unlist(w), collapse = ","))
ok("...'Ihr neuer Weg existiert jetzt!'", grepl("neuer Weg existiert", text()), text())
ok("...a hint to read", hasNext())
shot("15_newPathExists")
ok("Next to hint 16", nextAndWait(16))
Sys.sleep(0.8)
clickAt(probe("node(5)"))
ok("a tap on a node selects it",
   waitFor(sprintf("(function(){ var m = %s.layerManager.getLayer('marker','XXX'); return !!m && !!m._icon; })()", MAPJS), 10))
Sys.sleep(0.6)
clickAt(probe("node(15)")); Sys.sleep(1.5)
ok("then another node (it would link them) is swallowed", still(16) && !modalUp("shiny-modal") && !is.null(selected()))
clickAt(selected())
ok("a tap on the selected node deletes it: hint 17", waitFor(atHint(17), 10) && is.null(selected()))

cat("\n=== 6. hints 17-19: parking and residences ===\n")
Sys.sleep(0.6)
lab <- js("(function(){ var r = document.querySelector('#newVersions-contextChoice input[value=\"3\"]').closest('label').getBoundingClientRect();
  return [r.left, r.top, r.width, r.height]; })()")
w <- wins()
ok("one window, on 'Parken/Wohnen'", length(w) == 1 && near(w[[1]], lab, 6))
ok("...'Klicken Sie auf den neuen Kontext.' on a line of its own",
   grepl("<br>Klicken Sie auf den neuen Kontext\\.", text()), text())
clickEl("#newVersions-contextChoice input[value='1']"); Sys.sleep(0.8)
ok("a tap on another context is swallowed", still(17))
lab <- unlist(lab); click(lab[1] + lab[3] / 2, lab[2] + lab[4] / 2)
ok("the tap moves on to hint 18 once the context is drawn", waitFor(atHint(18), 60))
Sys.sleep(1.2)
w <- wins()
outline <- js(sprintf("(function(){ var m = %s, b = null, r = m.getContainer().getBoundingClientRect();
  m.eachLayer(function(l){ if (!b && l instanceof L.Polygon && l.options.color === 'black' && l.options.fill === false) b = l.getBounds(); });
  var a = m.latLngToContainerPoint(b.getNorthWest()), c = m.latLngToContainerPoint(b.getSouthEast());
  return [r.left + a.x, r.top + a.y, c.x - a.x, c.y - a.y]; })()", MAPJS))
ok("one window, on the study area's outline, framed whole", length(w) == 1 && near(w[[1]], outline, 12),
   paste("window", paste(unlist(w), collapse = ","), "outline", paste(round(unlist(outline)), collapse = ",")))
ok("'shape' in the drawing's blue", grepl("<em class=\"?vftTutShape\"?>Form</em>", text()), text())
ok("the drawer is on the map (polydraw.js)",
   isTRUE(js("!!(window.vftPolyDraw && document.getElementById('newVersions-versionMap'))")))
#an existing parking or residential area: before the first vertex a tap on it
#would delete it (obsShapeClick, context 3)
areas <- function() js("document.querySelectorAll('#newVersions-versionMap .leaflet-layer2-pane path.leaflet-interactive').length")
q <- js("(function(){ var es = document.querySelectorAll('#newVersions-versionMap .leaflet-layer2-pane path.leaflet-interactive');
  for (var i = 0; i < es.length; i++) { var e = es[i], r = e.getBoundingClientRect();
    for (var y = r.top + 2; y < r.bottom; y += 3) for (var x = r.left + 2; x < r.right; x += 3) {
      var h = document.elementFromPoint(x, y); if (h === e) return [x, y]; } }
  return null; })()")
if (is.null(q)) {
  cat("(no parking or residential area in view - the tap on one is not tried)\n")
} else {
  n0 <- areas(); clickAt(q); Sys.sleep(1.5)
  ok("a tap on an existing area, before the first vertex, is swallowed", still(18) && areas() == n0)
}
free <- function(x, y) isTRUE(js(sprintf("(function(){ var e = document.elementFromPoint(%f, %f);
  return !!e && !e.closest('.leaflet-layer2-pane .leaflet-interactive') && !e.closest('.vftTutorialBox') &&
         !!e.closest('#newVersions-versionMap'); })()", x, y)))
ww <- unlist(w[[1]]); base <- NULL
for (dx in seq(-150, 150, 30)) for (dy in seq(-100, 100, 30)) {
  p0 <- c(ww[1] + ww[3] / 2 + dx, ww[2] + ww[4] / 2 + dy)
  if (is.null(base) && free(p0[1], p0[2]) && free(p0[1] + 40, p0[2]) && free(p0[1] + 20, p0[2] + 40)) base <- p0
}
n0 <- areas()
click(base[1], base[2]); Sys.sleep(0.5)
ok("the first tap starts a drawing in the browser",
   isTRUE(js("document.getElementById('newVersions-versionMap').classList.contains('vft-pd-drawing')")))
for (q in list(base + c(40, 0), base + c(20, 40))) { click(q[1], q[2]); Sys.sleep(0.5) }
ok("...three vertices, no round trip to R (no modal yet)",
   isTRUE(js("document.querySelectorAll('#newVersions-versionMap .vft-pd-v').length === 3")) && still(18))
#Escape takes the last vertex back (escPass), and does not stop the tour
b$Input$dispatchKeyEvent(type = "keyDown", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27)
b$Input$dispatchKeyEvent(type = "keyUp", key = "Escape", code = "Escape", windowsVirtualKeyCode = 27)
Sys.sleep(0.5)
ok("Escape takes a vertex back, the tour goes on",
   isTRUE(js("document.querySelectorAll('#newVersions-versionMap .vft-pd-v').length === 2")) && still(18))
q <- base + c(20, 40); click(q[1], q[2]); Sys.sleep(0.5)
shot("18_drawing")
move(base[1] + 10, base[2] + 10); click(base[1], base[2])
ok("closing it on its first vertex asks for its type: hint 19",
   waitFor("jQuery('#newVersions-chooseParking').is(':visible')", 10) && waitFor(atHint(19), 10))
Sys.sleep(0.8)
ok("...the closed area waits on the map", isTRUE(js(sprintf("(function(){ var g = %s.layerManager.getLayerGroup('pending', false);
  var n = 0; if (g) g.eachLayer(function(){ n++; }); return n === 1; })()", MAPJS))))
w <- wins()
ok("two windows, on parking and residence",
   length(w) == 2 && near(w[[1]], rectOf("#newVersions-chooseParking"), 8) &&
     near(w[[2]], rectOf("#newVersions-chooseResidential"), 8))
ok("'residence' and 'parking space' in their buttons' colours",
   grepl("vftTutResidence", text()) && grepl("vftTutParking", text()))
clickEl("#newVersions-cancelNewPolygon"); Sys.sleep(1)
ok("the type modal's cancel is swallowed", still(19) && modalUp("shiny-modal"))
shot("19_type")
clickEl("#newVersions-chooseResidential")
ok("a residence: hint 20", waitFor(atHint(20), 15))
Sys.sleep(0.6)
ok("...the area is on the map, the waiting one gone",
   areas() == n0 + 1 && isTRUE(js(sprintf("(function(){ var g = %s.layerManager.getLayerGroup('pending', false);
     var n = 0; if (g) g.eachLayer(function(){ n++; }); return n === 0; })()", MAPJS))),
   paste(n0, "->", areas()))

cat("\n=== 7. hints 20-23, and the heat mitigation hint on step 5 ===\n")
SEL <- "#placeholder .vftCard button.selected:not(.notSelected)"
w <- wins()
ok("one window, on the selected scenario ('Neu')", length(w) == 1 && near(w[[1]], rectOf(SEL), 8))
ok("...'saved in the selected scenario'", grepl("ausgewählten Szenario gespeichert", text()), text())
shot("20_selected")
ok("Next to hint 21 (the seeded 'Neu' is selected)", nextAndWait(21))
Sys.sleep(0.6)
w <- wins()
ok("one window, on the selected scenario", length(w) == 1 && near(w[[1]], rectOf(SEL), 8))
ok("'Neu' light grey and italic", grepl("<em class=\"?vftTutNew\"?>Neu</em>", text()), text())
ok("...with a Next button", hasNext())
shot("21_new")
clickEl("#placeholder .vftProvisionalName")
ok("a tap on its name opens the rename modal", waitFor("jQuery('#newVersions-renameName').is(':visible')", 10))
Sys.sleep(0.8)
w <- wins()
ok("...one window, on the whole modal", length(w) == 1 && near(w[[1]], rectOf("#shiny-modal .modal-content"), 8))
ok("...with a text of its own (21b)", grepl("Geben Sie dem Szenario einen Namen", text()), text())
invisible(js("var e = document.getElementById('newVersions-renameName'); e.focus(); e.select(); true"))
invisible(b$Input$insertText(text = "Mein Szenario"))
Sys.sleep(0.4); clickEl("#newVersions-submitRename")
ok("renamed: hint 22", waitFor(atHint(22), 15) && grepl("Mein Szenario", cards()), cards())
Sys.sleep(0.6)
w <- wins()
ORIG <- "#placeholder .vftCardSlot:nth-child(1) button[id*=versionBtn]"
ok("one window, on the Original", length(w) == 1 && near(w[[1]], rectOf(ORIG), 8))
ok("'Original' dark grey and larger", grepl("<em class=\"?vftTutGrey\"?>Original</em>", text()), text())
ok("...with a Next button", hasNext())
shot("22_original")
ok("Next to hint 23", nextAndWait(23))
Sys.sleep(0.6)
w <- wins()
ok("one window, on the confirm button", length(w) == 1 && near(w[[1]], rectOf("#newVersions-newVersionsConfirmButton"), 8))
ok("...counted 20 / 20", identical(count(), "20 / 20"), count())
clickEl("#newVersions-newVersionsConfirmButton")
ok("the tap ends the tour", waitFor("!document.getElementById('vftTutorial')", 5))
ok("...stored as done", isTRUE(js("!!JSON.parse(localStorage.getItem('vft.tutorial.done.v1')).newVersions")))
ok("...and the app goes back to step 5", waitFor(ringIs("vftNav_step5"), 120))
ok("there the heat mitigation hint follows", waitFor(atHint(1, "toHitze"), 120))
Sys.sleep(0.8)
w <- wins()
ok("one window, on the Hitzeminderung button", length(w) == 1 && near(w[[1]], rectOf("#vftNav_hitze"), 8),
   paste(jsonlite::toJSON(w, auto_unbox = TRUE), "|", paste(round(unlist(rectOf("#vftNav_hitze"))), collapse = ",")))
ok("'Heat mitigation' in dark red", grepl("vftTutHeat", text()), text())
ok("...counted 1 / 1", identical(count(), "1 / 1"))
shot("23_toHitze")
clickEl("#step5-launchSim"); Sys.sleep(1)
ok("a tap on launch is swallowed", still(1, "toHitze"))
heatOff <- isTRUE(js("document.getElementById('vftNav_hitze').classList.contains('vft-nav-btn--off')"))
clickEl("#vftNav_hitze")
ok("the tap ends it", waitFor("!document.getElementById('vftTutorial')", 5))
if (heatOff) {
  ok("heat mitigation is off: its 'not implemented' modal", waitFor("jQuery('#shiny-modal').is(':visible')", 10))
} else {
  ok("...and opens Hitzeminderung", waitFor(ringIs("vftNav_hitze"), 120))
}
Sys.sleep(5)
#heat mitigation has a tour of its own (verify_tutorial_hitze_browser.R): it
#follows there, and is stopped; step 5's must not be replayed either way
if (heatOff) {
  ok("no other tour starts (step 5's is not replayed)", is.null(tut()$key), paste(tut()$key))
  invisible(js("jQuery('#shiny-modal').modal('hide')"))
} else {
  ok("heat mitigation's own tour follows (not step 5's)",
     waitFor("(function(){ var s = vftTutorialState(); return s.key === 'hitze'; })()", 120), paste(tut()$key))
  invisible(js("var b = document.querySelector('.vftTutorialStop'); if (b) b.click(); true"))
  ok("...stopped", waitFor("!document.getElementById('vftTutorial')", 5))
}

cat("\n=== 8. the Original alone: hints 2-4 ===\n")
Sys.sleep(1)
#the nav refuses a move while the page is busy (Hitzeminderung may still be
#building its land cover): tap again until it goes
back <- FALSE
for (k in 1:12) {
  if (isTRUE(js("jQuery('#shiny-modal').is(':visible')"))) invisible(js("jQuery('#shiny-modal').modal('hide')"))
  clickEl("#vftNav_newVersions")
  if (waitFor(ringIs("vftNav_newVersions"), 15)) { back <- TRUE; break }
}
ok("back on the scenarios page", back && pageReady())
clickEl("#placeholder .vftCardDel")
ok("'Neu' deleted: the Original alone", waitFor(sprintf("(function(){ return %s; })()",
   "document.querySelectorAll('#placeholder .vftCard button[id*=versionBtn]').length === 1"), 20) && pageReady())
invisible(js("vftTutorialStart('newVersions')"))
ok("hint 1, counted 1 / 22", waitFor(atHint(1), 60) && identical(count(), "1 / 22"), count())
ok("Next to hint 2", nextAndWait(2))
Sys.sleep(0.8)
w <- wins()
ok("one window, on the '+' tile", length(w) == 1 && near(w[[1]], rectOf("#newVersions-addVersionButton"), 8))
ok("'Scenario' teal, 'Original' dark grey", grepl("<b><em>Szenario</em></b>", text()) && grepl("vftTutGrey", text()))
clickEl("#newVersions-addVersionButton")
ok("its name modal: hint 3", waitFor(atHint(3), 10))
Sys.sleep(0.8)
w <- wins()
ok("two windows, on the name field and submit",
   length(w) == 2 && near(w[[2]], rectOf("#newVersions-submitName"), 8))
clickEl("#shiny-modal .modal-footer button[data-dismiss]"); Sys.sleep(1)
ok("its cancel is swallowed", still(3) && modalUp("shiny-modal"))
invisible(js("document.getElementById('newVersions-name').focus()"))
invisible(b$Input$insertText(text = "Tutorial"))
Sys.sleep(0.4); clickEl("#newVersions-submitName")
ok("submitted: hint 4", waitFor(atHint(4), 15))
Sys.sleep(0.8)
w <- wins()
new <- "#placeholder .vftCardSlot:nth-child(2) button[id*=versionBtn]"
ok("one window, on the new card", length(w) == 1 && near(w[[1]], rectOf(new), 8), cards())
clickEl(new)
ok("selecting it: hint 5, counted 5 / 22", waitFor(atHint(5), 15) && identical(count(), "5 / 22"), count())
stopTour()

cat("\n=== 9. the Original selected beside another: hint 4 alone ===\n")
Sys.sleep(1); invisible(pageReady())
clickEl("#placeholder .vftCardSlot:nth-child(1) button[id*=versionBtn]")
ok("the Original selected", waitFor("(function(){ var c = document.querySelector('#placeholder .vftCardSlot:nth-child(1) button');
  return c.classList.contains('selected') && !c.classList.contains('notSelected'); })()", 20) && pageReady())
invisible(js("vftTutorialStart('newVersions')"))
ok("hint 1, counted 1 / 20", waitFor(atHint(1), 60) && identical(count(), "1 / 20"), count())
ok("Next goes straight to hint 4", nextAndWait(4))
ok("...'select a new Scenario'", grepl("auswählen", text()), text())
stopTour()

cat(sprintf("\nscreenshots in %s\n", SHOTS))
invisible(b$close())
cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
