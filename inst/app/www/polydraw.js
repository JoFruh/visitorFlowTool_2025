/* Client-side polygon drawing for the step 1 and step 4 maps.
 *
 * The browser owns the drawing. Every vertex, the rubber band that follows the
 * pointer and the translucent preview of the area are Leaflet layers added here,
 * so placing a point costs no round trip. R hears about a drawing exactly once,
 * when it is finished:
 *
 *   <ns>polyDrawn  {lng: [...], lat: [...]}   the closed ring, first vertex not repeated
 *   <ns>polyCut    {lng: [a, b], lat: [a, b]} step 4's scissors: a two-point cut line
 *
 * Closing: click the first vertex, or double-click anywhere (the first click
 * of a double-click places a vertex under the pointer and the second lands on
 * it, which is why a second click on a vertex closes rather than removes).
 *
 * Taking vertices back: Escape or Backspace drops the last one, and keeps
 * dropping until the drawing is gone. The pointer over any vertex but the
 * first turns it into a red X, and a click on the X removes that vertex. The
 * vertex just placed shows no X until the pointer has left it once - it sits
 * right under the pointer, and every click would otherwise flash an X there.
 *
 * Live area check (step 1 only, opts.area): the bounding box the land cover
 * baseline would be built for is measured on every pointer move, over the
 * vertices plus the pointer, against two ceilings. Past the first (heat
 * mitigation) the drawing turns red and a warning element shows. Past the
 * second (the server's) it turns grey, another warning shows, and the drawing
 * is BLOCKED: no vertex can be placed and the ring cannot be closed where
 * either would leave it over. Taking vertices back still works - that, and
 * moving the pointer back in, is the way out. See areaLevel().
 *
 * Attached by R through htmlwidgets::onRender (vftPolyDraw() in
 * R/polydraw_helpers.R). leaflet's renderValue() throws the old L.Map away and
 * builds a new one on every render, so attach() runs again for each render and
 * keeps its state per element, not per map.
 *
 * Existing areas. The polygons R draws sit in the pane named by opts.polyPane
 * and a click on one of them is R's business (it deletes the area). So a click
 * that lands on one does NOT start a drawing - and once a drawing has started,
 * every other layer on the map is made click-through, so that vertices can be
 * placed on top of an existing area (that is how step 4 extends one).
 */
(function () {
  "use strict";

  var LINE  = "#5ab4f0";   //light blue: edges, rubber band, vertices
  var FILL  = "#5ab4f0";
  var CUT   = "#8b0000";   //dark red: the scissors button and its dashed line
  var WARN  = "#dd1717";   //the app's warning red: X buttons, an area past opts.area's first ceiling
  var GREY  = "#8a8a8a";   //an area past the second: blocked
  var PANE     = "vftDrawPane";
  var PANE_TOP = "vftDrawTopPane";
  //consecutive vertices closer than this (screen px) are one vertex: what a
  //double-click that missed the vertex it had just placed leaves behind
  var SAME_PX = 4;
  //a click on the map this soon after an X click, that is itself the second
  //click of a double-click, is the rest of a double-click on the X: the vertex
  //has gone from under the pointer and the click must not put down a new one
  var AFTER_REMOVE_MS = 800;

  var CROSS =
    '<svg width="10" height="10" viewBox="0 0 10 10" aria-hidden="true">' +
    '<path d="M2 2 8 8M8 2 2 8" stroke="#fff" stroke-width="2" stroke-linecap="round"/></svg>';

  var SCISSORS =
    '<svg width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" ' +
    'stroke-width="2.4" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">' +
    '<circle cx="6" cy="6" r="3"/><circle cx="6" cy="18" r="3"/><path d="M20 4 8.1 15.9"/>' +
    '<path d="M14.5 14.5 20 20"/><path d="M8.1 8.1 12 12"/></svg>';

  var states = {};   //element id -> drawing state

  // ── styles, once ────────────────────────────────────────────────────────────
  (function injectCss() {
    var css =
      //while drawing, nothing but the drawing itself takes a click
      ".vft-pd-drawing .leaflet-interactive { pointer-events: none !important; }" +
      ".vft-pd-drawing .leaflet-interactive.vft-pd-hit { pointer-events: auto !important; }" +
      ".vft-pd-drawing { cursor: crosshair; }" +
      ".vft-pd-drawing .leaflet-grab { cursor: crosshair; }" +
      ".vft-pd-scissors { box-sizing: border-box; width: 30px; height: 30px; border-radius: 50%;" +
      "  border: 3px solid " + CUT + "; background: #fff; color: " + CUT + ";" +
      "  display: flex; align-items: center; justify-content: center; cursor: pointer;" +
      "  box-shadow: 0 1px 4px rgba(0,0,0,.35); }" +
      ".vft-pd-scissors:hover { background: #fbe9e9; }" +
      //vertices: a dot, and for all but the first an X that the pointer uncovers
      ".vft-pd-v { display: flex; align-items: center; justify-content: center; }" +
      ".vft-pd-dot { box-sizing: border-box; width: 10px; height: 10px; border-radius: 50%;" +
      "  background: " + LINE + "; }" +
      ".vft-pd-first .vft-pd-dot { width: 13px; height: 13px; border: 2px solid #fff;" +
      "  box-shadow: 0 0 0 1px " + LINE + "; }" +
      ".vft-pd-drawing .vft-pd-first { cursor: crosshair; }" +
      ".vft-pd-drawing .vft-pd-first.vft-pd-closable { cursor: pointer; }" +
      ".vft-pd-x { display: none; box-sizing: border-box; width: 18px; height: 18px; border-radius: 50%;" +
      "  background: " + WARN + "; border: 2px solid #fff; box-shadow: 0 1px 3px rgba(0,0,0,.4);" +
      "  align-items: center; justify-content: center; }" +
      ".vft-pd-drawing .vft-pd-rm { cursor: pointer; }" +
      ".vft-pd-drawing .vft-pd-rm.vft-pd-fresh { cursor: crosshair; }" +
      ".vft-pd-rm:not(.vft-pd-fresh):hover .vft-pd-dot { display: none; }" +
      ".vft-pd-rm:not(.vft-pd-fresh):hover .vft-pd-x { display: flex; }" +
      //past the server's ceiling a click places nothing; the X still works
      ".vft-pd-drawing.vft-pd-blocked, .vft-pd-drawing.vft-pd-blocked .leaflet-grab," +
      " .vft-pd-drawing.vft-pd-blocked .vft-pd-first { cursor: not-allowed; }" +
      //and the vertices go grey with the rest of the drawing
      ".vft-pd-blocked .vft-pd-dot { background: " + GREY + "; }" +
      ".vft-pd-blocked .vft-pd-first .vft-pd-dot { box-shadow: 0 0 0 1px " + GREY + "; }";
    var el = document.createElement("style");
    el.appendChild(document.createTextNode(css));
    (document.head || document.documentElement).appendChild(el);
  })();

  // ── attach ──────────────────────────────────────────────────────────────────

  /* `ctx` is whatever htmlwidgets hands onRender as `this`: the widget instance
   * (which has getMap) in this leaflet build, the L.Map itself in others. */
  function resolveMap(el, ctx) {
    if (ctx && typeof ctx.getMap === "function") return ctx.getMap();
    if (ctx && typeof ctx.getContainer === "function") return ctx;
    if (typeof HTMLWidgets !== "undefined") {
      var w = HTMLWidgets.find("#" + el.id);
      if (w && w.getMap) return w.getMap();
    }
    return null;
  }

  function attach(el, ctx, opts) {
    var map = resolveMap(el, ctx);
    if (!map || typeof L === "undefined") return;

    var old = states[el.id];
    if (old && old.map === map) { old.opts = opts; reset(old); return; }
    if (old) clearTimeout(old.dblTimer);

    if (!map.getPane(PANE))     map.createPane(PANE).style.zIndex = 640;
    if (!map.getPane(PANE_TOP)) map.createPane(PANE_TOP).style.zIndex = 660;

    var s = {
      el: el, map: map, opts: opts || {},
      pts: [],            //L.LatLng, in drawing order
      cursor: null,
      group: L.layerGroup().addTo(map),
      band: null, preview: null, edges: null, cutLine: null,
      fresh: -1,          //index of the vertex just placed, which shows no X yet
      freshEl: null,
      removedAt: 0,       //Date.now() of the last X click
      level: 0,           //the live area check's last answer, see areaLevel()
      dblWasEnabled: false, dblTimer: null
    };
    states[el.id] = s;

    map.on("click", function (e) { onMapClick(s, e); });
    //a move anywhere but on the vertex just placed means the pointer has left
    //it. (The map hears mousemoves over markers too: bubblingMouseEvents:false
    //stops only clicks and hover events, so the target has to be asked.)
    map.on("mousemove", function (e) {
      var t = e.originalEvent && e.originalEvent.target;
      if (!(s.freshEl && t && s.freshEl.contains(t))) clearFresh(s);
      s.cursor = e.latlng; updateBand(s);
    });
    map.on("mouseout", function () { s.cursor = null; updateBand(s); });
    //the second click of a double-click normally lands on the vertex the first
    //one placed and closes the ring there; this catches the one that missed it
    //this catches the one that missed it. Past the server's ceiling the first
    //click placed nothing, and the ring must not close on the vertices it had
    //before - that is not the shape the user was pointing at.
    map.on("dblclick", function (e) {
      if (Date.now() - s.removedAt < AFTER_REMOVE_MS) return;
      if (e.latlng && blocked(s, s.pts.concat([e.latlng]))) return;
      if (s.pts.length >= 3) finish(s);
    });
  }

  // ── clicks ──────────────────────────────────────────────────────────────────

  /* A click on one of R's polygons, rather than on the map: those delete the
   * area in R, and must not also put down a first vertex. */
  function onPolygon(s, e) {
    var t = e.originalEvent && e.originalEvent.target;
    if (!t || !t.closest || !t.classList) return false;
    if (!t.classList.contains("leaflet-interactive")) return false;
    return !!t.closest(".leaflet-" + (s.opts.polyPane || "layer2") + "-pane");
  }

  /* 1 for a single click, 2 for the second click of a double-click. */
  function clicks(e) { return (e.originalEvent && e.originalEvent.detail) || 1; }

  function onMapClick(s, e) {
    if (!e.latlng) return;
    if (clicks(e) > 1 && Date.now() - s.removedAt < AFTER_REMOVE_MS) return;
    if (s.pts.length === 0 && onPolygon(s, e)) return;
    //asked of the click's own position, not of the last pointer move
    if (blocked(s, s.pts.concat([e.latlng]))) return;
    addVertex(s, e.latlng);
  }

  /* A click on vertex i.
   *   - the first vertex closes the ring (with fewer than three there is no
   *     area to close yet, and the click adds nothing);
   *   - the second click of a double-click closes it wherever it lands - that
   *     is how a double-click ends on the vertex its first click placed;
   *   - any other vertex is removed, unless it is the one just placed, whose X
   *     is not showing yet: a click whose result the user cannot see does
   *     nothing. */
  function onVertexClick(s, i, e) {
    L.DomEvent.stop(e);
    if (i === 0 || clicks(e) > 1) {
      if (s.pts.length >= 3) finish(s);
      return;
    }
    if (i === s.fresh) return;
    s.removedAt = Date.now();
    removeVertex(s, i);
  }

  function onScissorsClick(s, e) {
    L.DomEvent.stop(e);
    //the second click of a double-click that placed vertex 2 lands here too;
    //a cut is only ever a deliberate single click
    if (clicks(e) > 1) return;
    if (s.pts.length !== 2) return;
    send(s, "polyCut", s.pts);
    reset(s);
  }

  // ── state changes ───────────────────────────────────────────────────────────

  function addVertex(s, latlng) {
    if (s.pts.length === 0) begin(s);
    s.pts.push(latlng);
    s.fresh = s.pts.length - 1;
    render(s);
  }

  /* Taking vertex i back; taking the last one ends the drawing. */
  function removeVertex(s, i) {
    if (i < 0 || i >= s.pts.length) return;
    s.pts.splice(i, 1);
    s.fresh = -1;
    if (s.pts.length === 0) reset(s); else render(s);
  }

  /* The vertex just placed starts showing its X. */
  function clearFresh(s) {
    if (s.fresh < 0) return;
    s.fresh = -1;
    if (s.freshEl) L.DomUtil.removeClass(s.freshEl, "vft-pd-fresh");
    s.freshEl = null;
  }

  function begin(s) {
    L.DomUtil.addClass(s.map.getContainer(), "vft-pd-drawing");
    clearTimeout(s.dblTimer);
    if (s.map.doubleClickZoom && s.map.doubleClickZoom.enabled()) {
      s.dblWasEnabled = true;
      s.map.doubleClickZoom.disable();
    }
  }

  function finish(s) {
    //every close goes through here; vertices that could only be placed while
    //under the ceiling cannot make a ring over it, but this is the last word
    if (blocked(s, s.pts)) return;
    var pts = dedupe(s, s.pts);
    if (pts.length < 3) { s.pts = pts; s.fresh = -1; render(s); return; }
    send(s, "polyDrawn", pts);
    reset(s);
  }

  function reset(s) {
    s.pts = [];
    s.fresh = -1; s.freshEl = null;
    s.group.clearLayers();
    s.band = s.preview = s.edges = s.cutLine = null;
    L.DomUtil.removeClass(s.map.getContainer(), "vft-pd-drawing");
    setWarning(s, false, 0);
    //not at once: the dblclick that follows a closing click would otherwise
    //zoom the map on its way out
    if (s.dblWasEnabled) {
      clearTimeout(s.dblTimer);
      s.dblTimer = setTimeout(function () {
        if (s.pts.length === 0 && s.map.doubleClickZoom) s.map.doubleClickZoom.enable();
      }, 500);
      s.dblWasEnabled = false;
    }
  }

  function dedupe(s, pts) {
    var out = [];
    for (var i = 0; i < pts.length; i++) {
      var prev = out[out.length - 1];
      if (prev && s.map.latLngToContainerPoint(prev)
                   .distanceTo(s.map.latLngToContainerPoint(pts[i])) < SAME_PX) continue;
      out.push(pts[i]);
    }
    //and the ring's closing edge, for a last vertex dropped on the first
    if (out.length > 3 && s.map.latLngToContainerPoint(out[0])
          .distanceTo(s.map.latLngToContainerPoint(out[out.length - 1])) < SAME_PX) out.pop();
    return out;
  }

  function send(s, what, pts) {
    if (typeof Shiny === "undefined" || !Shiny.setInputValue) return;
    Shiny.setInputValue((s.opts.ns || "") + what, {
      lng: pts.map(function (p) { return p.lng; }),
      lat: pts.map(function (p) { return p.lat; }),
      nonce: Math.random()
    }, { priority: "event" });
  }

  // ── drawing ─────────────────────────────────────────────────────────────────

  function cutArmed(s) { return !!s.opts.cut && s.pts.length === 2; }

  /* Everything but the rubber band, which follows the pointer on its own. */
  function render(s) {
    s.group.clearLayers();
    s.freshEl = null;
    var n = s.pts.length;
    if (n === 0) return;

    //the layers are rebuilt here, so they start in the colour the area check
    //last settled on; setWarning() only restyles on a change of answer
    var col = levelColour(s.level);
    s.preview = L.polygon([], {
      pane: PANE, interactive: false, stroke: false,
      fill: true, fillColor: s.level ? col : FILL, fillOpacity: 0.25
    }).addTo(s.group);

    if (cutArmed(s)) {
      s.edges = null;
      s.cutLine = L.polyline(s.pts, {
        pane: PANE, interactive: false, color: CUT, weight: 3, opacity: 1, dashArray: "8 6"
      }).addTo(s.group);
    } else if (n >= 2) {
      s.edges = L.polyline(s.pts, {
        pane: PANE, interactive: false, color: col, weight: 3, opacity: 1
      }).addTo(s.group);
    }

    s.band = L.polyline([], {
      pane: PANE, interactive: false, color: col, weight: 2, opacity: 1
    }).addTo(s.group);

    //vertices are divIcon markers rather than circles: the X that replaces a
    //dot under the pointer is plain CSS :hover, so no mouseover/mouseout
    //bookkeeping can leave one stuck on screen
    for (var i = 0; i < n; i++) {
      var first = i === 0, last = i === n - 1;
      if (last && cutArmed(s)) continue;          //drawn as the scissors below
      var cls = "vft-pd-hit vft-pd-v " +
        (first ? "vft-pd-first" + (n >= 3 ? " vft-pd-closable" : "")
               : "vft-pd-rm" + (i === s.fresh ? " vft-pd-fresh" : ""));
      var v = L.marker(s.pts[i], {
        pane: PANE_TOP, interactive: true, bubblingMouseEvents: false, keyboard: false,
        icon: L.divIcon({
          className: cls, iconSize: [20, 20], iconAnchor: [10, 10],
          html: '<div class="vft-pd-dot"></div>' +
                (first ? "" : '<div class="vft-pd-x">' + CROSS + "</div>")
        })
      }).addTo(s.group);
      v.on("click", onVertexClick.bind(null, s, i));
      //a double-click on a vertex must not reach the map either
      v.on("dblclick", function (e) { L.DomEvent.stop(e); });
      //(no mouseout handler for it: the map's mousemove notices the pointer
      //leaving, and a mouseout can fire while the layers are being rebuilt)
      if (i === s.fresh) s.freshEl = v.getElement();
    }

    if (cutArmed(s)) {
      var sc = L.marker(s.pts[1], {
        pane: PANE_TOP, interactive: true, bubblingMouseEvents: false, keyboard: false,
        icon: L.divIcon({
          className: "vft-pd-hit", iconSize: [30, 30], iconAnchor: [15, 15],
          html: '<div class="vft-pd-scissors">' + SCISSORS + "</div>"
        })
      }).addTo(s.group);
      sc.on("click", onScissorsClick.bind(null, s));
      //a double-click on the button must not reach the map either
      sc.on("dblclick", function (e) { L.DomEvent.stop(e); });
    }

    updateBand(s);
  }

  /* Last vertex -> pointer -> first vertex, and the area that would result. */
  function updateBand(s) {
    if (!s.band || !s.pts.length) return;
    var pts = s.pts, first = pts[0], last = pts[pts.length - 1];
    if (s.cursor) {
      s.band.setLatLngs([last, s.cursor, first]);
      s.preview.setLatLngs(pts.concat([s.cursor]));
    } else {
      s.band.setLatLngs([]);
      s.preview.setLatLngs(pts.length >= 3 ? pts : []);
    }
    setWarning(s, true, areaLevel(s.opts.area, s.cursor ? pts.concat([s.cursor]) : pts));
  }

  // ── the live area check ─────────────────────────────────────────────────────

  /* WGS84 -> LV95 (EPSG:2056) by swisstopo's approximate formulas: within about
   * 1 m of PROJ anywhere in Switzerland, which is a warning's precision. */
  function lv95(p) {
    var phi = (p.lat * 3600 - 169028.66) / 10000;
    var lam = (p.lng * 3600 - 26782.5) / 10000;
    return {
      e: 2600072.37 + 211455.93 * lam - 10938.51 * lam * phi
         - 0.36 * lam * phi * phi - 44.54 * lam * lam * lam,
      n: 1200147.07 + 308807.95 * phi + 3745.25 * lam * lam + 76.63 * phi * phi
         - 194.56 * lam * lam * phi + 119.79 * phi * phi * phi
    };
  }

  /* How many cells the land cover baseline for this outline would take:
   * paintAreaTooLarge() in R/paintbrush_helpers.R, restated. The outline in
   * LV95, buffered by `buffer` m, its bounding box snapped outwards to the
   * `res` grid, counted in cells. A buffer grows a bounding box by the buffer
   * on every side, so the buffer itself is never built. R measures the
   * finished outline again and has the last word; this only has to agree with
   * it away from the edge, to within the ~1 m of lv95(). */
  function areaCells(a, pts) {
    if (!a || !pts || !pts.length) return 0;
    var x0 = Infinity, y0 = Infinity, x1 = -Infinity, y1 = -Infinity;
    for (var i = 0; i < pts.length; i++) {
      var q = lv95(pts[i]);
      if (q.e < x0) x0 = q.e;
      if (q.e > x1) x1 = q.e;
      if (q.n < y0) y0 = q.n;
      if (q.n > y1) y1 = q.n;
    }
    var b = a.buffer || 0, r = a.res || 1;
    var w = Math.ceil((x1 + b) / r) - Math.floor((x0 - b) / r);
    var h = Math.ceil((y1 + b) / r) - Math.floor((y0 - b) / r);
    return w * h;
  }

  /* 0 under both ceilings, 1 past the heat mitigation one (a.maxCells), 2 past
   * the server's (a.hardCells). A ceiling R did not hand over is not checked -
   * step 1 drops maxCells while heat mitigation is switched off. */
  function areaLevel(a, pts) {
    var n = areaCells(a, pts);
    if (a && a.hardCells != null && n > a.hardCells) return 2;
    if (a && a.maxCells  != null && n > a.maxCells)  return 1;
    return 0;
  }

  /* The heat ceiling alone, as R's paintAreaTooLarge() answers it. */
  function areaOver(a, pts) { return !!a && a.maxCells != null && areaCells(a, pts) > a.maxCells; }

  /* Would a ring through these points be past the server's ceiling? */
  function blocked(s, pts) { return areaLevel(s.opts.area, pts) === 2; }

  function levelColour(level) { return level === 2 ? GREY : level === 1 ? WARN : LINE; }

  /* The drawing's colour, the map's blocked state, and the page's two warning
   * elements (opts.area.warn for level 1, opts.area.hardWarn for level 2):
   * `vft-pd-live` on each while a drawing is in progress, `vft-pd-over` while
   * the drawing has reached that element's level. What the classes mean for
   * visibility is the page's CSS - step 1 shows one warning at a time, the
   * highest (R/layout_helpers.R). */
  function setWarning(s, live, level) {
    if (!s.opts.area) return;
    if (level !== s.level) {
      s.level = level;
      var c = levelColour(level);
      if (s.preview) s.preview.setStyle({ fillColor: level ? c : FILL });
      if (s.band)    s.band.setStyle({ color: c });
      if (s.edges)   s.edges.setStyle({ color: c });
      L.DomUtil[level === 2 ? "addClass" : "removeClass"](s.map.getContainer(), "vft-pd-blocked");
    }
    tag(s.opts.area.warn, live, live && level >= 1);
    tag(s.opts.area.hardWarn, live, live && level === 2);
  }

  function tag(id, live, over) {
    var el = id && document.getElementById(id);
    if (!el) return;
    L.DomUtil[live ? "addClass" : "removeClass"](el, "vft-pd-live");
    L.DomUtil[over ? "addClass" : "removeClass"](el, "vft-pd-over");
  }

  // ── from R and the keyboard ─────────────────────────────────────────────────

  function cancel(id) {
    var s = states[id];
    if (s && s.pts.length) reset(s);
  }

  /* Escape or Backspace take back the last vertex of the drawing on screen.
   * Not while the user is typing - Backspace in a text box is theirs - and not
   * on a map in a hidden tab, whose half-drawn ring the user cannot see. */
  function typing(t) {
    if (!t || !t.tagName) return false;
    return /^(INPUT|TEXTAREA|SELECT)$/.test(t.tagName) || !!t.isContentEditable;
  }

  document.addEventListener("keydown", function (ev) {
    if (ev.key !== "Escape" && ev.key !== "Backspace") return;
    if (typing(ev.target)) return;
    var hit = false;
    for (var id in states) {
      if (!states.hasOwnProperty(id)) continue;
      var s = states[id];
      if (!s.pts.length || !s.el.offsetParent) continue;
      removeVertex(s, s.pts.length - 1);
      hit = true;
    }
    if (hit) ev.preventDefault();
  });

  if (typeof Shiny !== "undefined" && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler("vft-polydraw-cancel", function (m) { cancel(m.id); });
  }

  window.vftPolyDraw = { attach: attach, cancel: cancel, areaOver: areaOver,
                         areaLevel: areaLevel, areaCells: areaCells, lv95: lv95 };
})();
