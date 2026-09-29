/* ------------------------------------------------------------------ *
 * Guided tutorial - one tour per step (R/tutorial.R)
 *
 * Ported from FRISCH's first-use tutorial (FRISCH_tutorial.js). A grey layer
 * over the whole app with windows cut into it. Each hint opens windows over
 * the control to use next and shows a text card beside them. The user does
 * each hint in the real app, through a window; the hint moves on when the app
 * shows it was done (a polygon accepted, a modal open...). A hint that is only
 * there to be read has a Next button instead, and its windows take no taps.
 * Taps outside the windows are swallowed, so nothing else can be reached while
 * a hint is on - except the language selector in the nav bar, which keeps a
 * window of its own (no outline) from start to end: the texts follow it.
 *
 * Between two hints the windows and the card are gone for a second, so the
 * user sees what their tap did (Next moves on at once). Then the next hint's
 * windows pop in.
 *
 * TOURS below holds one tour per key. A key is the nav bar button the ring is
 * on, minus `vftNav_`: step1..step5, newVersions, hitze. A tour that ends
 * hands on to whichever step the user goes to next: if that step has a tour,
 * it starts there (see "chaining"). R/tutorial.R's VFT_TUTORIAL_TOURS has to
 * list the same keys - data-raw/verify_tutorial.R checks it does.
 *
 * On the first visit a bubble under the help button offers step 1's tour.
 * Finishing or stopping a tour, or closing the bubble, is stored on the
 * device (vft.tutorial.v1), and the bubble does not come back. The help
 * button's modal starts the tour of the step being shown
 * (vftTutorialStart(), called from vftTutorialModal() in R/tutorial.R).
 *
 * The texts come from the server in the current language
 * (vftTutorialServer()); the card's look is set by the variables in
 * vft-tutorial.css.
 * ------------------------------------------------------------------ */
(function () {
  'use strict';

  var STORE_KEY = 'vft.tutorial.v1';

  // Switzerland's bounding box, and Birmensdorf ZH (the village, beside WSL)
  var CH_BOUNDS = [[45.818, 5.956], [47.808, 10.492]];
  var BIRMENSDORF = [47.3553, 8.4378];
  var BIRMENSDORF_ZOOM = 14;

  var PAD = 6;            // default margin of a window around its target
  var RADIUS = 10;        // corner radius of a window
  var POP_GROW = 260;     // a window grows from a point past its size, ms
  var POP_SETTLE = 200;   // ... then settles on its size, ms
  var PAUSE = 1000;       // nothing shown between two hints, ms
  var WAIT_SHOW = 1000;   // a hint waiting for its target shows its card after, ms
  var GUTTER = 12;        // the card's least distance to the screen edge
  var GAP = 14;           // between a window and the card
  var MISSING_WAIT = 150; // how long a target may blink out before its window closes
  var CHAIN_MAX = 10 * 60 * 1000; // how long a finished tour waits for the next step, ms
  var OFFER_WAIT = 60 * 1000;     // how long the first visit's bubble waits for its moment, ms
  var OFFER_CARET_MIN = 26;       // the bubble's caret: least distance to its sides, px

  var PLAY = '<svg class="vftTutorialPlay" viewBox="0 0 10 12" aria-hidden="true">' +
             '<path d="M0 0L10 6L0 12Z"/></svg>';

  // from the server: {lang, next_, stop, offer, start, tours: {step1: [...], step2: [...]}}
  var texts = null;
  var state = null;       // the running tour, null when none
  var chain = null;       // a finished tour waiting for the next step's

  var reducedMotion = window.matchMedia &&
    window.matchMedia('(prefers-reduced-motion: reduce)');

  function still() { return !!(reducedMotion && reducedMotion.matches); }

  // a text from the CSVs: new lines may come as <br>, a real one, or "\n"
  function fmt(html) {
    return String(html || '').replace(/\r?\n|\\n/g, '<br>');
  }

  /* ---------------------------- storage ----------------------------- */

  // best-effort: unreadable storage counts as a first visit
  function storedStatus() {
    try {
      var raw = window.localStorage.getItem(STORE_KEY);
      return raw ? JSON.parse(raw) : null;
    } catch (e) {
      return null;
    }
  }

  function storeStatus(status) {
    try {
      window.localStorage.setItem(STORE_KEY, JSON.stringify({ status: status, at: Date.now() }));
    } catch (e) { /* storage unavailable - the bubble will simply come again */ }
  }

  /* ------------------------ reading the app ------------------------- */

  function getMap(id) {
    var w = window.HTMLWidgets && HTMLWidgets.find('#' + id);
    return (w && w.getMap && w.getMap()) || null;
  }

  function step1Map() { return getMap('step1-areaSelectMap'); }

  // Bootstrap 3: an open modal is a displayed one (no .show class here)
  function modalOpen() {
    var m = document.getElementById('shiny-modal');
    return !!(m && window.jQuery && jQuery(m).is(':visible'));
  }

  function hideModal() {
    var m = document.getElementById('shiny-modal');
    if (m && window.jQuery) { try { jQuery(m).modal('hide'); } catch (e) { /* nothing more to try */ } }
  }

  function shown(el) {
    return !!(el && el.isConnected && el.offsetParent !== null && el.getClientRects().length);
  }

  // which tour the page is on: the nav bar's ring (vftNavCurrentId() in R)
  function ringKey() {
    var el = document.querySelector('#vftNav .vft-nav-current');
    return el && el.id ? el.id.replace(/^vftNav_/, '') : null;
  }

  // whether the texts have caught up with the language picked
  function textsBehind() {
    var el = document.getElementById('languageSelect');
    return !!el && !!el.value && !!texts && !!texts.lang && texts.lang !== el.value;
  }

  /* ----------------------------- rects ------------------------------ */
  // {l, t, r, b} in viewport pixels

  function elRect(el, pad) {
    if (!el || !el.isConnected || !el.getClientRects().length) { return null; }
    var r = el.getBoundingClientRect();
    if (!r.width && !r.height) { return null; }
    pad = pad == null ? PAD : pad;
    return { l: r.left - pad, t: r.top - pad, r: r.right + pad, b: r.bottom + pad };
  }

  function union(rects) {
    var u = null;
    rects.forEach(function (r) {
      if (!r) { return; }
      u = u ? { l: Math.min(u.l, r.l), t: Math.min(u.t, r.t),
                r: Math.max(u.r, r.r), b: Math.max(u.b, r.b) }
            : { l: r.l, t: r.t, r: r.r, b: r.b };
    });
    return u;
  }

  function intersect(a, b) {
    var r = { l: Math.max(a.l, b.l), t: Math.max(a.t, b.t),
              r: Math.min(a.r, b.r), b: Math.min(a.b, b.b) };
    return r.r > r.l && r.b > r.t ? r : null;
  }

  function overlaps(a, b) {
    return a.l < b.r - 1 && a.r > b.l + 1 && a.t < b.b - 1 && a.b > b.t + 1;
  }

  function contains(r, x, y) {
    return !!r && x >= r.l && x <= r.r && y >= r.t && y <= r.b;
  }

  function dot(r) {
    var x = (r.l + r.r) / 2, y = (r.t + r.b) / 2;
    return { l: x - 1, t: y - 1, r: x + 1, b: y + 1 };
  }

  function inflate(r, d) {
    return { l: r.l - d, t: r.t - d, r: r.r + d, b: r.b + d };
  }

  function lerp(a, b, s) {
    return { l: a.l + (b.l - a.l) * s, t: a.t + (b.t - a.t) * s,
             r: a.r + (b.r - a.r) * s, b: a.b + (b.b - a.b) * s };
  }

  function clamp(x, lo, hi) { return Math.max(lo, Math.min(x, hi)); }

  /* ---------------------------- targets ----------------------------- */
  // A target returns a list of {key, rect, hit}: nothing while it is not on
  // screen. `hit` (optional): the elements a tap in the window has to land on.
  // A window is a little larger than its target, and a tap in that margin
  // would reach whatever lies under it.

  function sel(selector, pad) {
    return function () {
      var el = document.querySelector(selector);
      if (!shown(el)) { return []; }
      var r = elRect(el, pad);
      return r ? [{ key: selector, rect: r, hit: [el] }] : [];
    };
  }

  // A row of buttons, one window each. The margin shrinks with the gap between
  // them, so the windows (and their outlines) never run into each other.
  // keep(el): which of them get a window (all when omitted).
  function buttonRow(selector, keep) {
    return function () {
      var found = [];
      Array.prototype.forEach.call(document.querySelectorAll(selector), function (el, i) {
        if (keep && !keep(el)) { return; }
        var r = shown(el) && elRect(el, 0);
        if (r) { found.push({ key: el.id || selector + i, rect: r, el: el }); }
      });
      var sorted = found.slice().sort(function (a, b) { return a.rect.l - b.rect.l; });
      var pad = PAD;
      for (var i = 1; i < sorted.length; i++) {
        var gap = sorted[i].rect.l - sorted[i - 1].rect.r;
        if (gap > 0) { pad = Math.min(pad, Math.max(0, Math.floor((gap - 3) / 2))); }
      }
      return found.map(function (f) { return { key: f.key, rect: inflate(f.rect, pad), hit: [f.el] }; });
    };
  }

  // lat/lng bounds on a map, as a window clipped to the map
  function boundsWindow(mapFn, latlngs, key) {
    return function () {
      var map = mapFn();
      if (!map || !map._loaded || !window.L) { return []; }
      var c = map.getContainer();
      var cr = elRect(c, 0);
      if (!cr) { return []; }
      var b = L.latLngBounds(latlngs);
      var nw = map.latLngToContainerPoint(b.getNorthWest());
      var se = map.latLngToContainerPoint(b.getSouthEast());
      var r = intersect({ l: cr.l + nw.x - 8, t: cr.t + nw.y - 8,
                          r: cr.l + se.x + 8, b: cr.t + se.y + 8 }, cr);
      return r ? [{ key: key, rect: r }] : [];
    };
  }

  // A map move started by the tutorial: the hints show nothing until it ends,
  // so their windows pop in where they belong rather than riding along.
  function holdForMove(map, move) {
    var token = {};
    var release = function () {
      map.off('moveend', release);
      if (state && state.moving === token) { state.moving = false; }
    };
    state.moving = token;
    // on before the move: a move without animation ends at once
    map.on('moveend', release);
    setTimeout(release, 2500);
    move();
  }

  /* ------------------------- step 1's hints ------------------------- */

  // the whole of Switzerland on the map - unless it is already framed
  function viewSwitzerland() {
    var map = step1Map();
    if (!map || !window.L) { return; }
    var b = L.latLngBounds(CH_BOUNDS);
    if (map.getBounds().contains(b) && map.getZoom() >= map.getBoundsZoom(b) - 1) { return; }
    holdForMove(map, function () {
      if (still()) { map.fitBounds(b, { animate: false }); }
      else { map.flyToBounds(b, { duration: 0.8 }); }
    });
  }

  function viewBirmensdorf() {
    var map = step1Map();
    if (!map) { return; }
    holdForMove(map, function () {
      if (still()) { map.setView(BIRMENSDORF, BIRMENSDORF_ZOOM, { animate: false }); }
      else { map.flyTo(BIRMENSDORF, BIRMENSDORF_ZOOM, { duration: 1.2 }); }
    });
  }

  // A polygon closed on the map and ACCEPTED: polydraw.js sends it as
  // step1-polyDrawn, R answers - refusing it with a modal, or showing the
  // confirm button - and Shiny goes idle once it has. Asked in that order,
  // because the confirm button may already be showing from an earlier
  // outline, and a refused polygon leaves it where it was.
  function watchDrawing(ctx) {
    viewBirmensdorf();
    ctx.jq('shiny:inputchanged', function (e) {
      if (e.name === 'step1-polyDrawn') { ctx.drawn = true; ctx.idle = false; }
    });
    ctx.jq('shiny:idle', function () { if (ctx.drawn) { ctx.idle = true; } });
  }

  function drawingAccepted(ctx) {
    return !!ctx.drawn && !!ctx.idle && !modalOpen() &&
           shown(document.getElementById('step1-confirmButton2'));
  }

  function nextStepModalUp() {
    return modalOpen() && !!document.querySelector('#shiny-modal .vft-next-btn');
  }

  // A choice in the "choose your next step" modal is held back: the modal
  // closes, and the choice is sent only once the last hint has been read
  // (sendChoice). Registered after the tap filter, so it only ever sees a tap
  // the filter let through.
  function holdChoice(ctx) {
    ctx.on(window, 'click', function (e) {
      var btn = e.target && e.target.closest && e.target.closest('#shiny-modal .vft-next-btn');
      if (!btn) { return; }
      e.stopImmediatePropagation();
      e.stopPropagation();
      if (e.cancelable) { e.preventDefault(); }
      state.data.choice = btn.id;
      hideModal();
      ctx.flag = true;
    }, true);
  }

  // what the held-back button would have sent: its observer in
  // vftNavBarServer() (R/navigation.R) closes the modal and moves the user on
  function sendChoice(ctx, done) {
    var id = state && state.data.choice;
    if (id && window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue(id, Date.now(), { priority: 'event' });
    }
    done();
  }

  function flagged(ctx) { return !!ctx.flag; }

  var UPLOAD_CARD = '.vft-step1-options > .vft-step1-opt:first-child';
  var DRAW_CARD = '.vft-step1-options > .vft-step1-opt:last-child';

  /* ------------------------- step 2's hints ------------------------- */

  // the Amphibians class checkbox: its value is the group's name in the
  // language the list was built in (i18n()$t(group_de) in step2_server.R)
  var AMPHIBIANS = ['Amphibien', 'Amphibiens', 'Amphibians'];
  // the species the weight hints use: VU, Emerald and priority 1, so every
  // icon hint 7 talks about is on its row
  var TOAD = 'Bombina variegata';
  // the group boxes are reset as the step settles - "all species" is ticked
  // 1.5 s after the species scan (step2_server.R), which clears them - so hint
  // 4 only opens its window once the checkbox has been there this long
  var SETTLE = 2000;

  var SPECIES = '#step2-speciesCheckbox';

  // Arriving from step 1, the ring can reach step 2 while step 1's page is
  // still showing: wait for step 2's own page (as step3NotYet() does)
  function step2NotYet() { return !shown(document.getElementById('step2-SDMmap')); }

  // a window on el, cut to what its scrolling container shows
  function clipped(el, box, pad, key, hit) {
    var r = shown(el) && elRect(el, pad);
    var c = r && box && elRect(box, 0);
    r = r && (c ? intersect(r, c) : r);
    return r ? [{ key: key, rect: r, hit: hit || [el] }] : [];
  }

  // scroll a list so that el sits in the middle of it (the list only)
  function centreIn(el, box) {
    if (!el || !box) { return; }
    var r = el.getBoundingClientRect(), b = box.getBoundingClientRect();
    if (r.top >= b.top && r.bottom <= b.bottom) { return; }
    box.scrollTop += (r.top - b.top) - (box.clientHeight - r.height) / 2;
  }

  function amphibianBox() {
    var boxes = document.querySelectorAll('#step2-groupCheckbox_class input[type=checkbox]');
    for (var i = 0; i < boxes.length; i++) {
      if (AMPHIBIANS.indexOf(boxes[i].value) >= 0) { return boxes[i]; }
    }
    // an area without amphibians: the first group, rather than a tour stuck
    return boxes[0] || null;
  }

  function amphibianTarget(ctx) {
    var box = amphibianBox();
    var label = box && box.closest('label');
    if (!shown(label)) { ctx.seen = null; return []; }
    var now = performance.now();
    if (!ctx.seen || ctx.seen.el !== label) { ctx.seen = { el: label, at: now }; }
    if (now - ctx.seen.at < SETTLE) { return []; }
    var col = label.closest('.vft-step2-groups');
    if (!ctx.scrolled) { ctx.scrolled = true; centreIn(label, col); }
    return clipped(label, col, 4, 'amphibians', [label]);
  }

  // a tick on the Amphibians box, by the user
  function watchAmphibians(ctx) {
    ctx.on(document, 'change', function (e) {
      if (e.target && e.target === amphibianBox() && e.target.checked) { ctx.flag = true; }
    }, true);
  }

  // the species list, from "most widespread" above it to "least widespread"
  // below it
  function speciesList() {
    var list = document.querySelector(SPECIES);
    var box = list && list.closest('.vft-fit-species');
    if (!shown(box)) { return []; }
    var parts = [box];
    var up = box.previousElementSibling, down = box.nextElementSibling;
    for (var i = 0; i < 2; i++) {
      if (up) { parts.push(up); up = up.previousElementSibling; }
      if (down) { parts.push(down); down = down.nextElementSibling; }
    }
    var r = union(parts.filter(shown).map(function (el) { return elRect(el, 4); }));
    return r ? [{ key: 'species', rect: r }] : [];
  }

  // the toad's row; failing that the first ticked row, then the first row
  function toadRow() {
    var rows = document.querySelectorAll(SPECIES + ' .checkbox');
    var ticked = null;
    for (var i = 0; i < rows.length; i++) {
      if (rows[i].textContent.indexOf(TOAD) >= 0) { return rows[i]; }
      if (!ticked) {
        var cb = rows[i].querySelector('input[type=checkbox]');
        if (cb && cb.checked) { ticked = rows[i]; }
      }
    }
    return ticked || rows[0] || null;
  }

  function toadList(row) { return row && row.closest('.vft-fit-species'); }

  function showToad() {
    var row = toadRow();
    centreIn(row, toadList(row));
  }

  function toadTarget() {
    var row = toadRow();
    return clipped(row, toadList(row), 2, 'toad');
  }

  function toadWeight() {
    var row = toadRow();
    return row && row.querySelector('input[type=number]');
  }

  function weightTarget() {
    var input = toadWeight();
    var wrap = input && input.closest('.form-group');
    return clipped(wrap || input, toadList(toadRow()), 4, 'weight', [input]);
  }

  // typed or stepped by the user, not set by the app
  function watchWeight(ctx) {
    showToad();
    var mark = function (e) { if (e.target && e.target === toadWeight()) { ctx.touched = true; } };
    ctx.on(document, 'input', mark, true);
    ctx.on(document, 'change', mark, true);
  }

  function weightRaised(ctx) {
    var input = toadWeight();
    return !!ctx.touched && !!input && Number(input.value) >= 3;
  }

  // a tap on this element moves on (the element works as usual)
  function watchClick(selector) {
    return function (ctx) {
      ctx.on(window, 'click', function (e) {
        if (e.target && e.target.closest && e.target.closest(selector)) { ctx.flag = true; }
      }, true);
    };
  }

  function sliderBox() {
    var input = document.getElementById('step2-minValThreshold');
    return input && input.closest('.shiny-input-container');
  }

  // the value Shiny sends once the slider comes to rest
  function watchThreshold(ctx) {
    ctx.jq('shiny:inputchanged', function (e) {
      if (e.name === 'step2-minValThreshold' && Number(e.value) >= 25) { ctx.flag = true; }
    });
  }

  /* ------------------------- step 3's hints ------------------------- */

  // the three steps of the recreation simulation in the nav bar, as one
  // window: the chevrons overlap each other, so one window each would too
  function simSteps() {
    var r = union(['#vftNav_step3', '#vftNav_step4', '#vftNav_step5'].map(function (s) {
      var el = document.querySelector(s);
      return shown(el) ? elRect(el, 4) : null;
    }));
    return r ? [{ key: 'simSteps', rect: r }] : [];
  }

  function aoiSliderBox() {
    var input = document.getElementById('step3-AOISlider');
    return input && input.closest('.shiny-input-container');
  }

  // Arriving from step 1, the ring is on step 3 while its data is still being
  // prepared and step 1's page is still showing: wait for step 3's own page.
  function step3NotYet() { return !shown(aoiSliderBox()); }

  // The slider runs from 20 on the left down to 0 on the right, and starts on
  // 11: "reaching 8" is a value of 8 or below, sent once the slider rests.
  function watchAoiThreshold(ctx) {
    ctx.jq('shiny:inputchanged', function (e) {
      if (e.name === 'step3-AOISlider' && Number(e.value) <= 8) { ctx.flag = true; }
    });
  }

  // the map redraws for the new threshold (debounced 400 ms in step3_server.R)
  function aoiMapRedrawing() {
    var el = document.getElementById('step3-AOIMap');
    return !!el && el.classList.contains('recalculating');
  }

  /* ------------------------- step 4's hints ------------------------- */

  function step4Map() { return getMap('step4-finalAOIMap'); }

  // Arriving from step 3, the ring is on step 4 while its areas are still
  // being generated and step 3's page is still showing: wait for the map.
  function step4NotYet() {
    var el = document.getElementById('step4-finalAOIMap');
    var map = step4Map();
    return !shown(el) || el.classList.contains('recalculating') || !map || !map._loaded;
  }

  // the layers of one of the map's groups (leaflet's layerManager)
  function groupLayers(map, group) {
    var g = map && map.layerManager && map.layerManager.getLayerGroup(group, false);
    var out = [];
    if (g) { g.eachLayer(function (l) { out.push(l); }); }
    return out;
  }

  // How many areas the map shows. .vftDrawAOI() (step4_server.R) redraws them
  // all as one GeoJSON layer, one feature per area.
  function areaCount() {
    var n = 0;
    groupLayers(step4Map(), 'eraseable').forEach(function (l) {
      if (l.eachLayer) { l.eachLayer(function () { n++; }); } else { n++; }
    });
    return n;
  }

  // The step-1 outline and the areas of interest, as one window clipped to the
  // map. The areas, too: step 3 finds them in a wide band round the outline,
  // so a window on the outline alone leaves most of them out of reach. Its
  // margin takes the scissors button (30 px) on a point placed just outside an
  // area - boundsWindow()'s would cut it in half. The whole map when there is
  // nothing to frame.
  function areasWindow() {
    var map = step4Map();
    if (!map || !map._loaded || !window.L) { return []; }
    var c = map.getContainer();
    var cr = elRect(c, 0);
    if (!cr) { return []; }
    var b = null;
    groupLayers(map, 'perimeter').concat(groupLayers(map, 'eraseable')).forEach(function (l) {
      var lb = l.getBounds && l.getBounds();
      if (lb && lb.isValid()) { b = b ? b.extend(lb) : L.latLngBounds(lb.getSouthWest(), lb.getNorthEast()); }
    });
    var r = cr;
    if (b) {
      var nw = map.latLngToContainerPoint(b.getNorthWest());
      var se = map.latLngToContainerPoint(b.getSouthEast());
      r = intersect({ l: cr.l + nw.x - 24, t: cr.t + nw.y - 24,
                      r: cr.l + se.x + 24, b: cr.t + se.y + 24 }, cr);
    }
    return r ? [{ key: 'areas', rect: r, hit: [c] }] : [];
  }

  // A cut that split an area: polydraw.js sends the line as step4-polyCut and
  // R redraws the pieces. Counted from the moment it is sent, so a line that
  // missed every area, or only cut a slit into one, does not count.
  function watchCut(ctx) {
    ctx.jq('shiny:inputchanged', function (e) {
      if (e.name === 'step4-polyCut') { ctx.base = areaCount(); ctx.cut = true; }
    });
  }

  function cutSplitArea(ctx) { return !!ctx.cut && areaCount() > ctx.base; }

  // A new area closed and drawn. Merging it into the areas it touches may
  // leave fewer of them, so this waits for R to be done instead of counting.
  function watchNewArea(ctx) {
    ctx.jq('shiny:inputchanged', function (e) {
      if (e.name === 'step4-polyDrawn') { ctx.drawn = true; ctx.idle = false; }
    });
    ctx.jq('shiny:idle', function () { if (ctx.drawn) { ctx.idle = true; } });
  }

  function newAreaDrawn(ctx) { return !!ctx.drawn && !!ctx.idle; }

  /* ------------------------- step 5's hints ------------------------- */

  var LAUNCH = '#step5-launchSim';
  var CARDS = '#placeholder_step5 .btn';
  var SIM_QUIET = 5000;   // a simulation never seen running counts as done after, ms
  var BAR_WAIT = 4000;    // hint 2 waits this long for its first progress bar, ms

  function step5Map() { return getMap('step5-mapAreaLeaflet'); }

  // Entering step 5 from the nav bar, the ring moves before the page shows
  function step5NotYet() { return !shown(document.querySelector(LAUNCH)); }

  // several targets as one hint's windows
  function each() {
    var fns = Array.prototype.slice.call(arguments);
    return function (ctx) {
      return fns.reduce(function (out, fn) { return out.concat(fn(ctx)); }, []);
    };
  }

  // the progress bars, bottom right (vftProgressPair() in R/async_helpers.R):
  // the path data's download, the preparation, then the simulation
  function progressBars() {
    return Array.prototype.filter.call(
      document.querySelectorAll('#shiny-notification-panel .shiny-notification'), shown);
  }

  function progressTarget(ctx) {
    var r = union(progressBars().map(function (el) { return elRect(el, 4); }));
    if (r) { ctx.barSeen = true; }
    return r ? [{ key: 'progress', rect: r }] : [];
  }

  function launchBusy() {
    var b = document.querySelector(LAUNCH);
    return !!b && b.disabled;
  }

  // the "no simulation yet" picture is off the map
  function step5MapShown() {
    return !shown(document.getElementById('step5-mapPlaceholder')) && !!step5Map();
  }

  function watchSim(ctx) { ctx.t0 = performance.now(); }

  // hidden until the first bar is up - the path data may take a moment
  function noBarYet(ctx) {
    return !ctx.barSeen && !progressBars().length && performance.now() - ctx.t0 < BAR_WAIT;
  }

  // The launch button is disabled from the click until the result is drawn
  // (obsEvent_sim in step5_server.R), and the bars are gone with it. Busy has
  // to be seen first: the hint may start before the click's round trip.
  function simDone(ctx) {
    if (launchBusy() || progressBars().length) { ctx.busySeen = true; return false; }
    if (!step5MapShown()) { return false; }
    return !!ctx.busySeen || performance.now() - ctx.t0 > SIM_QUIET;
  }

  // the perimeter: the thick black outline plotPathUsage() draws round it
  function outlineTarget() {
    var map = step5Map();
    if (!map || !window.L) { return []; }
    var b = null;
    map.eachLayer(function (l) {
      if (!b && l instanceof L.Polygon && l.options.color === 'black' && l.options.fill === false) {
        b = l.getBounds();
      }
    });
    return b && b.isValid() ? boundsWindow(step5Map, b, 'outline')() : [];
  }

  // a switch row of the layers card: its label, switch and all
  function switchRow(id) {
    return function () {
      var input = document.getElementById(id);
      var row = input && input.closest('label');
      var r = shown(row) && elRect(row, 3);
      return r ? [{ key: id, rect: r, hit: [row] }] : [];
    };
  }

  // switched on by the user
  function watchSwitch(id) {
    return function (ctx) {
      ctx.on(document, 'change', function (e) {
        if (e.target && e.target.id === id && e.target.checked) { ctx.flag = true; }
      }, true);
    };
  }

  // Whether this area has a sensitivity matrix: enter() in step5_server.R
  // marks the switch
  function smMissing() {
    var el = document.getElementById('step5-SMcheckbox');
    return !(el && el.classList.contains('vftSmReady'));
  }

  // the scenario column: its title and the list of cards under it
  function scenarioColumn() {
    var col = document.querySelector('.vft-scencol');
    var parts = col ? [col.querySelector('h4'), col.querySelector('.vft-ws-listrow')] : [];
    var r = union(parts.filter(shown).map(function (el) { return elRect(el, 4); }));
    return r ? [{ key: 'scenarios', rect: r }] : [];
  }

  // The card the map shows has `selected` and no `notSelected` - the server
  // adds notSelected to the card it leaves without taking selected off
  function isSelected(card) {
    return card.classList.contains('selected') && !card.classList.contains('notSelected');
  }

  function cardList() { return Array.prototype.slice.call(document.querySelectorAll(CARDS)); }

  // a card other than the one shown - past the original if there is one
  function otherCard() {
    var cards = cardList();
    var free = cards.filter(function (c) { return !isSelected(c); });
    return free.filter(function (c) { return c !== cards[0]; })[0] || free[0] || null;
  }

  function onlyOriginal() { return cardList().length < 2; }

  // picked once, so the window does not jump once it is selected
  function watchOtherCard(ctx) {
    ctx.card = otherCard();
    centreIn(ctx.card, ctx.card && ctx.card.closest('.vft-ws-list'));
    ctx.on(window, 'click', function (e) {
      if (ctx.card && e.target && ctx.card.contains(e.target)) { ctx.flag = true; }
    }, true);
  }

  function otherCardTarget(ctx) {
    var card = ctx.card;
    return clipped(card, card && card.closest('.vft-ws-list'), 4, 'card');
  }

  /* ------------------------------ tours ----------------------------- */
  // targets: the windows. advance(ctx): polled every frame, true moves on.
  // enter(ctx): runs as the hint starts; ctx.on()/ctx.jq() listeners go with
  //   the hint.
  // look: a hint to read - a Next button moves on, the windows take no taps.
  //   onNext(ctx, done): runs before it moves on. live: its windows take taps
  //   all the same (a control that may be used, but need not be).
  // pass: a selector whose taps go through all the same.
  // escPass: Escape is left to the page (polydraw.js takes a vertex back
  //   with it) instead of stopping the tour.
  // avoid: selectors the card must not cover. cardOver: a selector the card
  //   sits on instead of beside the windows. wide: a wider card, for a hint
  //   with little height to spare.
  // hold(ctx): true keeps the hint hidden. center: the text is centred.
  // end: the tour ends once this hint is done, whatever comes after it.
  // variant(): asked once as the hint starts; a name it returns lays
  //   variants[name] over the hint, and its text is the alternative one.
  // Texts are texts.tours[<key>][<index>]; a variant's are
  // texts.alts[<key>][<hint number><name>], e.g. alts.step5['6b'].

  var TOURS = {
    step1: [
      { // 1 - a word of welcome over Switzerland, framed whole on the map
        targets: boundsWindow(step1Map, CH_BOUNDS, 'switzerland'),
        enter: viewSwitzerland,
        hold: textsBehind,
        center: true,
        look: true
      },
      { // 2 - the upload card
        targets: sel(UPLOAD_CARD),
        look: true
      },
      { // 3 - draw around Birmensdorf. The map and the drawing card; the card
        // sits over the upload card, out of both windows. A refused polygon's
        // modal can be dismissed through the dim.
        targets: function () {
          var map = step1Map();
          var c = map && map.getContainer();
          var out = sel(DRAW_CARD)();
          var r = shown(c) && elRect(c, 2);
          if (r) { out.push({ key: 'map', rect: r, hit: [c] }); }
          return out;
        },
        cardOver: UPLOAD_CARD,
        wide: true,
        enter: watchDrawing,
        advance: drawingAccepted,
        escPass: true,
        pass: '#shiny-modal'
      },
      { // 4 - confirm. Confirming a new area over an older one first asks
        // whether to discard it: that modal takes taps too.
        targets: sel('#step1-confirmButton2'),
        advance: nextStepModalUp,
        pass: '#shiny-modal'
      },
      { // 5 - one window per choice; the choice is held for hint 6
        targets: buttonRow('#shiny-modal .vft-next-btn'),
        avoid: ['#shiny-modal .vft-next-head'],
        enter: holdChoice,
        advance: flagged
      },
      { // 6 - the help button, before the app moves on to the choice
        targets: sel('#helpButton', 8),
        look: true,
        onNext: sendChoice
      }
    ],

    step2: [
      { // 1 - this step's button in the nav bar
        targets: sel('#vftNav_step2', 4),
        hold: step2NotYet,
        look: true
      },
      { // 2 - the other steps: one window per group of the bar, step 2's left out
        targets: buttonRow('#vftNav .vft-nav-center > .vft-nav-group', function (el) {
          return !el.querySelector('#vftNav_step2');
        }),
        look: true
      },
      { // 3 - the sensitivity map
        targets: sel('#step2-SDMmap'),
        look: true
      },
      { // 4 - tick Amphibians
        targets: amphibianTarget,
        enter: watchAmphibians,
        advance: flagged
      },
      { // 5 - the map, amphibians only
        targets: sel('#step2-SDMmap'),
        look: true
      },
      { // 6 - the species list
        targets: speciesList,
        look: true
      },
      { // 7 - the toad's row, scrolled into sight
        targets: toadTarget,
        enter: showToad,
        look: true
      },
      { // 8 - its weight up to 3
        targets: weightTarget,
        enter: watchWeight,
        advance: weightRaised
      },
      { // 9 - weights by red list status
        targets: sel('#step2-redListWeights'),
        enter: watchClick('#step2-redListWeights'),
        advance: flagged
      },
      { // 10 - hide the bottom 25 %
        targets: function () {
          var box = sliderBox();
          var r = shown(box) && elRect(box, 4);
          return r ? [{ key: 'threshold', rect: r, hit: [box] }] : [];
        },
        enter: watchThreshold,
        advance: flagged
      },
      { // 11 - the download: may be used, Next moves on either way
        targets: sel('#step2-SMbutton'),
        look: true,
        live: true
      },
      { // 12 - confirm. Confirming over later steps' work first asks whether
        // to discard it: that modal takes taps too.
        targets: sel('#step2-confirmButton2'),
        advance: nextStepModalUp,
        pass: '#shiny-modal'
      },
      { // 13 - one window per choice; the tour ends with the tap, and the
        // chosen step's tour follows if it has one
        targets: buttonRow('#shiny-modal .vft-next-btn'),
        avoid: ['#shiny-modal .vft-next-head'],
        enter: watchClick('#shiny-modal .vft-next-btn'),
        advance: flagged
      }
    ],

    step3: [
      { // 1 - the recreation simulation's three steps in the nav bar
        targets: simSteps,
        hold: step3NotYet,
        look: true
      },
      { // 2 - this sub-step's button
        targets: sel('#vftNav_step3', 4),
        look: true
      },
      { // 3 - the threshold slider, down to 8
        targets: function () {
          var box = aoiSliderBox();
          var r = shown(box) && elRect(box, 4);
          return r ? [{ key: 'aoiThreshold', rect: r, hit: [box] }] : [];
        },
        enter: watchAoiThreshold,
        advance: flagged
      },
      { // 4 - the map, once it has redrawn for the new threshold
        targets: sel('#step3-AOIMap'),
        hold: aoiMapRedrawing,
        look: true
      },
      { // 5 - confirm. The tour ends with the tap, and step 4's tour follows
        // if it has one. Confirming over later steps' work first asks whether
        // to discard it: that modal takes taps too.
        targets: sel('#step3-confirmButton3'),
        enter: watchClick('#step3-confirmButton3'),
        advance: flagged,
        pass: '#shiny-modal'
      }
    ],

    step4: [
      { // 1 - this sub-step's button, once step 4's map is up
        targets: sel('#vftNav_step4', 4),
        hold: step4NotYet,
        look: true
      },
      { // 2 - cut an area in two with the scissors
        targets: areasWindow,
        enter: watchCut,
        advance: cutSplitArea,
        wide: true,
        escPass: true
      },
      { // 3 - draw a new area
        targets: areasWindow,
        enter: watchNewArea,
        advance: newAreaDrawn,
        wide: true,
        escPass: true
      },
      { // 4 - back to the areas step 3 made
        targets: sel('#step4-resetButton'),
        enter: watchClick('#step4-resetButton'),
        advance: flagged
      },
      { // 5 - confirm. The tour ends with the tap, and step 5's tour follows.
        // Confirming over later steps' work first asks whether to discard it:
        // that modal takes taps too.
        targets: sel('#step4-confirmButton4'),
        enter: watchClick('#step4-confirmButton4'),
        advance: flagged,
        pass: '#shiny-modal'
      }
    ],

    step5: [
      { // 1 - launch the simulation. A failure's modal can be dismissed.
        targets: sel(LAUNCH),
        hold: step5NotYet,
        enter: watchClick(LAUNCH),
        advance: flagged,
        pass: '#shiny-modal'
      },
      { // 2 - the progress bars, until the result is on the map
        targets: progressTarget,
        enter: watchSim,
        hold: noBarYet,
        advance: simDone,
        pass: '#shiny-modal'
      },
      { // 3 - the path usage, framed by the area's outline
        targets: outlineTarget,
        look: true
      },
      { // 4 - the recreationist types and the map layers
        targets: each(sel('.vft5-rail .vft5-agent', 4),
                      sel('.vft5-rail .vftRailCard:nth-child(2)', 4)),
        look: true
      },
      { // 5 - switch the starting points on
        targets: switchRow('step5-startingCheckbox'),
        enter: watchSwitch('step5-startingCheckbox'),
        advance: flagged
      },
      { // 6 - a: switch the sensitivity matrix on. b: there is none to show
        // (the switch would offer to go and make one) - read only
        targets: switchRow('step5-SMcheckbox'),
        enter: watchSwitch('step5-SMcheckbox'),
        advance: flagged,
        variant: function () { return smMissing() ? 'b' : null; },
        variants: { b: { enter: null, advance: null, look: true } }
      },
      { // 7 - the export
        targets: sel('#step5-imageButton'),
        look: true
      },
      { // 8 - the scenario column
        targets: scenarioColumn,
        look: true
      },
      { // 9 - a: only the original - go and make a scenario; the tour ends
        // there, and the next page's tour follows if it has one. b: select
        // another one.
        targets: sel('#step5-newVersionsButton'),
        enter: watchClick('#step5-newVersionsButton'),
        advance: flagged,
        end: true,
        variant: function () { return onlyOriginal() ? null : 'b'; },
        variants: { b: { targets: otherCardTarget, enter: watchOtherCard, end: false } }
      },
      { // 10 - and simulate it
        targets: sel(LAUNCH),
        enter: watchClick(LAUNCH),
        advance: flagged,
        pass: '#shiny-modal'
      }
    ]
  };

  function tourTexts(key) {
    var t = texts && texts.tours && texts.tours[key];
    if (t == null) { return []; }
    return Array.isArray(t) ? t : [t];
  }

  // the text of the hint being shown, its variant's if it has one
  function hintText() {
    var alts = state.variant && texts && texts.alts && texts.alts[state.key];
    var alt = alts && alts[(state.idx + 1) + state.variant];
    return alt || tourTexts(state.key)[state.idx] || '';
  }

  // the hint as it plays: variants[name] laid over it, if variant() names one
  function resolve(base) {
    var name = null;
    if (base.variant) {
      try { name = base.variant() || null; } catch (e) { name = null; }
    }
    var over = name && base.variants && base.variants[name];
    if (!over) { return { step: base, variant: null }; }
    var step = {};
    Object.keys(base).forEach(function (k) { step[k] = base[k]; });
    Object.keys(over).forEach(function (k) { step[k] = over[k]; });
    return { step: step, variant: name };
  }

  /* ----------------------------- overlay ---------------------------- */

  function buildOverlay() {
    var root = document.createElement('div');
    root.id = 'vftTutorial';
    root.className = 'vftTutorial vftTutorialQuiet';

    // The dim is painted on a canvas, windows cut out of it: redrawn whole
    // whenever a window changes, so nothing of an old window can linger.
    var canvas = document.createElement('canvas');
    canvas.className = 'vftTutorialCanvas';
    canvas.setAttribute('aria-hidden', 'true');
    root.appendChild(canvas);

    var box = document.createElement('div');
    box.className = 'vftTutorialBox';
    box.setAttribute('role', 'dialog');
    box.setAttribute('aria-live', 'polite');
    root.appendChild(box);

    document.body.appendChild(root);
    return { root: root, canvas: canvas, box: box };
  }

  // the dim's colours, from the CSS variables
  function readLook() {
    var cs = window.getComputedStyle(state.root);
    var v = function (name, fallback) { return cs.getPropertyValue(name).trim() || fallback; };
    state.look = {
      dim: v('--vftTut-dim', 'rgba(60, 60, 60, 0.35)'),
      ring: v('--vftTut-ring', 'rgba(255, 255, 255, 0.95)'),
      ringWidth: parseFloat(v('--vftTut-ring-width', '2.5')) || 2.5
    };
    state.dimSig = null;
  }

  function hints() { return TOURS[state.key]; }

  function renderBox() {
    if (!state) { return; }
    var step = state.step;
    var t = texts || {};
    var text = hintText();
    var n = hints().length;

    state.box.classList.toggle('vftTutorialCentered', !!step.center);
    state.box.classList.toggle('vftTutorialWide', !!step.wide);
    state.box.innerHTML =
      '<div class="vftTutorialCount">' + (state.idx + 1) + ' / ' + n + '</div>' +
      '<div class="vftTutorialText">' + fmt(text) + '</div>' +
      '<div class="vftTutorialActions">' +
        '<span class="vftTutorialDots" aria-hidden="true"><i></i><i></i><i></i></span>' +
        '<button type="button" class="vftTutorialStop"></button>' +
        (step.look ? '<button type="button" class="vftTutorialNext"><span></span>' + PLAY + '</button>' : '') +
      '</div>';
    var stopBtn = state.box.querySelector('.vftTutorialStop');
    stopBtn.textContent = t.stop || 'Stop tutorial';
    stopBtn.addEventListener('click', function () { stop('stopped'); });
    var nextBtn = state.box.querySelector('.vftTutorialNext');
    if (nextBtn) {
      nextBtn.firstChild.textContent = t.next_ || 'Next';
      nextBtn.addEventListener('click', onNext);
    }
    // measure the new texts before they are placed again
    unplace();
  }

  // Next: on at once, without the pause - there is nothing to see happen
  function onNext() {
    var ctx = state && state.ctx;
    if (!ctx || ctx.busy || state.pause) { return; }
    var step = state.step;
    if (!step.onNext) { ctx.next = true; return; }
    ctx.busy = true;
    step.onNext(ctx, function () {
      if (state && state.ctx === ctx) { ctx.next = true; }
    });
  }

  /* ---------------------------- windows ----------------------------- */
  // A window pops in: from a point to a little more than its target, then
  // back to its size. After that it simply sits on its target.

  function popRect(goal, age) {
    if (still() || age >= POP_GROW + POP_SETTLE) { return goal; }
    var size = Math.max(goal.r - goal.l, goal.b - goal.t);
    var big = inflate(goal, Math.min(14, 5 + 0.08 * size));
    if (age < POP_GROW) {
      var s = age / POP_GROW;
      return lerp(dot(goal), big, 1 - Math.pow(1 - s, 3));
    }
    var u = (age - POP_GROW) / POP_SETTLE;
    return lerp(big, goal, u < 0.5 ? 2 * u * u : 1 - Math.pow(-2 * u + 2, 2) / 2);
  }

  function updateWindows(goals, now) {
    goals.forEach(function (g) {
      var w = null;
      for (var i = 0; i < state.wins.length && !w; i++) {
        if (state.wins[i].key === g.key) { w = state.wins[i]; }
      }
      if (!w) {
        w = { key: g.key, born: now };
        state.wins.push(w);
      }
      w.goal = g.rect;
      w.hit = g.hit || null;
      w.seenAt = now;
    });
    state.wins = state.wins.filter(function (w) { return now - w.seenAt <= MISSING_WAIT; });
    state.wins.forEach(function (w) { w.cur = popRect(w.goal, now - w.born); });
  }

  function roundRect(g, r) {
    var w = Math.max(0, r.r - r.l), h = Math.max(0, r.b - r.t);
    var rad = Math.min(RADIUS, w / 2, h / 2);
    g.beginPath();
    g.moveTo(r.l + rad, r.t);
    g.arcTo(r.r, r.t, r.r, r.b, rad);
    g.arcTo(r.r, r.b, r.l, r.b, rad);
    g.arcTo(r.l, r.b, r.l, r.t, rad);
    g.arcTo(r.l, r.t, r.r, r.t, rad);
    g.closePath();
  }

  // The dim with the windows cut out, and an outline round each. Only redrawn
  // when something changed.
  function drawDim() {
    var c = state.canvas;
    var W = document.documentElement.clientWidth || window.innerWidth;
    var H = window.innerHeight;
    var dpr = window.devicePixelRatio || 1;
    var rects = state.wins.map(function (w) { return w.cur; });
    var lang = state.langWins;
    var sig = [W, H, dpr, rects.length].concat(rects.concat(lang).map(function (r) {
      return [r.l, r.t, r.r, r.b].map(function (v) { return Math.round(v * 4); }).join(',');
    })).join('|');
    if (sig === state.dimSig) { return; }
    state.dimSig = sig;

    var cw = Math.round(W * dpr), ch = Math.round(H * dpr);
    if (c.width !== cw || c.height !== ch) { c.width = cw; c.height = ch; }
    var g = c.getContext('2d');
    g.setTransform(dpr, 0, 0, dpr, 0, 0);
    g.clearRect(0, 0, W, H);
    g.globalCompositeOperation = 'source-over';
    g.fillStyle = state.look.dim;
    g.fillRect(0, 0, W, H);
    // a window cut out: overlapping windows stay open
    g.globalCompositeOperation = 'destination-out';
    g.fillStyle = '#000';
    rects.concat(lang).forEach(function (r) { roundRect(g, r); g.fill(); });
    g.globalCompositeOperation = 'source-over';
    // the outline goes round the hint's windows only, not the language's
    g.strokeStyle = state.look.ring;
    g.lineWidth = state.look.ringWidth;
    rects.forEach(function (r) { roundRect(g, r); g.stroke(); });
    // where the windows are, for whoever tests this page
    var px = function (list) {
      return JSON.stringify(list.map(function (r) {
        return [r.l, r.t, r.r - r.l, r.b - r.t].map(Math.round);
      }));
    };
    c.setAttribute('data-windows', px(rects));
    c.setAttribute('data-lang-windows', px(lang));
  }

  // Taps go through a window, but not in a hint that is only to read (unless
  // it is live), and only onto the window's own target (el: the element tapped)
  function inWindow(x, y, el) {
    var step = state.step;
    if (state.pause || (step.look && !step.live)) { return false; }
    return state.wins.some(function (w) {
      if (!contains(w.cur, x, y) && !contains(w.goal, x, y)) { return false; }
      return !w.hit || w.hit.some(function (h) { return !!h && !!el && h.contains(el); });
    });
  }

  // The language can be changed at any moment: a window stays open on the
  // nav bar's selector, and on its dropdown while that is open. Not while a
  // modal is up - its backdrop is over the bar.
  function languageWindows() {
    if (modalOpen()) { return []; }
    var wrap = document.querySelector('#vftNav .vft-nav-lang');
    var drop = wrap && wrap.querySelector('.selectize-dropdown');
    return [elRect(wrap, 3), shown(drop) ? elRect(drop, 2) : null].filter(Boolean);
  }

  /* ------------------------------ cards ----------------------------- */

  function viewport() {
    var W = document.documentElement.clientWidth || window.innerWidth;
    var H = window.innerHeight;
    return { l: GUTTER, t: GUTTER, r: W - GUTTER, b: H - GUTTER, W: W, H: H };
  }

  // Where a card of bw x bh goes for these windows: above them; below them if
  // there is no room above. "Room" means on screen and clear of every window
  // and of the hint's avoid areas (plus `extra` rects).
  function cardPos(bw, bh, goals, step, vp, extra) {
    var rects = goals.map(function (g) { return g.rect; });
    // centred on that element, but never down into a window below it - the
    // card is usually taller than what it sits on
    if (step.cardOver) {
      var over = elRect(document.querySelector(step.cardOver), 0);
      if (over) {
        var floor = vp.b;
        rects.forEach(function (r) { if (r.t >= over.t) { floor = Math.min(floor, r.t - GAP); } });
        var top0 = Math.min((over.t + over.b - bh) / 2, floor - bh);
        return { left: clamp((over.l + over.r - bw) / 2, vp.l, vp.r - bw),
                 top: Math.max(vp.t, top0), side: 'none', cx: 0 };
      }
    }
    var avoid = rects.concat(extra || []);
    (step.avoid || []).forEach(function (s) {
      Array.prototype.forEach.call(document.querySelectorAll(s), function (el) {
        var r = elRect(el, 0);
        if (r) { avoid.push(r); }
      });
    });

    var cands = [];
    if (rects.length) {
      var u = union(rects);
      cands.push({ side: 'above', a: u }, { side: 'below', a: u });
      rects.slice().sort(function (a, b) { return a.t - b.t; }).forEach(function (r) {
        cands.push({ side: 'above', a: r }, { side: 'below', a: r });
      });
    }

    for (var i = 0; i < cands.length; i++) {
      var c = cands[i];
      var cx = (c.a.l + c.a.r) / 2;
      var left = clamp(cx - bw / 2, vp.l, vp.r - bw);
      var top = c.side === 'above' ? c.a.t - GAP - bh : c.a.b + GAP;
      var br = { l: left, t: top, r: left + bw, b: top + bh };
      // an area to keep clear pushes the card further out on the same side,
      // the caret still pointing at the windows
      for (var n = 0; n < 4; n++) {
        var hit = avoid.filter(function (r) { return overlaps(br, r); });
        if (!hit.length) { break; }
        top = c.side === 'above'
          ? Math.min.apply(null, hit.map(function (r) { return r.t; })) - GAP - bh
          : Math.max.apply(null, hit.map(function (r) { return r.b; })) + GAP;
        br = { l: left, t: top, r: left + bw, b: top + bh };
      }
      if (top < vp.t || top + bh > vp.b) { continue; }
      if (avoid.some(function (r) { return overlaps(br, r); })) { continue; }
      return { left: left, top: top, side: c.side, cx: cx };
    }
    // nowhere beside them: the top or bottom edge if either is clear,
    // otherwise the middle of the screen
    var mid = Math.max(vp.l, (vp.W - bw) / 2);
    var edge = null;
    [vp.t, vp.b - bh].some(function (top) {
      var br = { l: mid, t: top, r: mid + bw, b: top + bh };
      if (!rects.length || avoid.some(function (r) { return overlaps(br, r); })) { return false; }
      edge = { left: mid, top: top, side: 'none', cx: 0 };
      return true;
    });
    return edge || { left: mid, top: Math.max(vp.t, (vp.H - bh) / 2), side: 'none', cx: 0 };
  }

  function applyPos(el, pos) {
    var w = el.offsetWidth;
    var caret = Math.round(clamp(pos.cx - pos.left, 20, w - 20));
    var last = el._vftTutPos;
    if (last && last.side === pos.side && Math.abs(last.left - pos.left) < 2 &&
        Math.abs(last.top - pos.top) < 2 && Math.abs(last.caret - caret) < 2) { return; }
    el.style.left = Math.round(pos.left) + 'px';
    el.style.top = Math.round(pos.top) + 'px';
    el.setAttribute('data-side', pos.side);
    el.style.setProperty('--vftTut-caret-x', caret + 'px');
    if (!last) {
      // the first placement of a card jumps; later ones glide
      void el.offsetWidth;
      el.classList.add('vftTutorialPlaced');
    }
    pos.caret = caret;
    el._vftTutPos = pos;
  }

  // the card jumps to its next place instead of gliding there
  function unplace() {
    state.box._vftTutPos = null;
    state.box.classList.remove('vftTutorialPlaced');
  }

  function placeCard(goals, step) {
    var vp = viewport();
    var bw = state.box.offsetWidth, bh = state.box.offsetHeight;
    if (!bw || !bh) { return; }
    applyPos(state.box, cardPos(bw, bh, goals, step, vp, state.langWins));
  }

  // nothing but the dim: between hints, while the map moves, and for a short
  // while as a hint waits for its target to appear
  function setQuiet(q) {
    if (q === state.quiet) { return; }
    state.quiet = q;
    state.root.classList.toggle('vftTutorialQuiet', q);
    if (q) { unplace(); }
  }

  /* ------------------------ blocking the rest ----------------------- */

  var BLOCKED = ['pointerdown', 'pointerup', 'mousedown', 'mouseup', 'click', 'dblclick',
                 'contextmenu', 'touchstart', 'touchend', 'wheel'];
  var DOWN = { pointerdown: 1, mousedown: 1, touchstart: 1 };
  var UP = { pointerup: 1, mouseup: 1, touchend: 1 };

  function eventPoint(e) {
    var t = (e.touches && e.touches[0]) || (e.changedTouches && e.changedTouches[0]);
    return t ? [t.clientX, t.clientY] : [e.clientX, e.clientY];
  }

  function allowed(e) {
    var el = e.target;
    var closest = function (s) { return !!(el && el.closest && el.closest(s)); };
    // the card, and the language selector with its dropdown, whatever the hint
    if (closest('.vftTutorialBox')) { return true; }
    if (!modalOpen() && closest('#vftNav .vft-nav-lang')) { return true; }
    var step = state.step;
    if (step.pass && !state.pause && closest(step.pass)) { return true; }
    // a click from the keyboard has no position: judge it by its element
    if (e.type === 'click' && e.detail === 0 && el && el.getBoundingClientRect) {
      var r = el.getBoundingClientRect();
      return inWindow((r.left + r.right) / 2, (r.top + r.bottom) / 2, el);
    }
    var p = eventPoint(e);
    return inWindow(p[0], p[1], el);
  }

  function filter(e) {
    // scripted clicks (the app's own) are not the user's
    if (!state || !e.isTrusted) { return; }
    var ok;
    // a drag that started in a window may end outside it
    if (UP[e.type]) { ok = state.downOk || allowed(e); }
    else { ok = allowed(e); }
    if (DOWN[e.type]) { state.downOk = ok; }
    if (ok) { return; }
    e.stopImmediatePropagation();
    e.stopPropagation();
    if (e.cancelable) { e.preventDefault(); }
  }

  function onKey(e) {
    if (!state || e.key !== 'Escape') { return; }
    // drawing on the map: Escape takes the last vertex back (polydraw.js)
    if (state.step.escPass) { return; }
    // stop the tour, and do not let the same key close a modal
    e.stopImmediatePropagation();
    e.preventDefault();
    stop('stopped');
  }

  function listen(on) {
    var fn = on ? 'addEventListener' : 'removeEventListener';
    BLOCKED.forEach(function (type) {
      window[fn](type, filter, { capture: true, passive: false });
    });
    window[fn]('keydown', onKey, true);
  }

  /* ------------------------------ run ------------------------------- */

  function go(idx, now) {
    // listeners of the hint that ends go with it
    if (state.ctx) { state.ctx.off(); }
    state.idx = idx;
    state.stepStart = now;
    state.wins = [];

    var offs = [];
    var ctx = state.ctx = {
      flag: false,
      next: false,
      busy: false,
      on: function (target, type, fn, capture) {
        target.addEventListener(type, fn, capture);
        offs.push(function () { target.removeEventListener(type, fn, capture); });
      },
      // Shiny's own events are jQuery events, which addEventListener never hears
      jq: function (type, fn) {
        if (!window.jQuery) { return; }
        var ns = type + '.vftTut';
        jQuery(document).on(ns, fn);
        offs.push(function () { jQuery(document).off(ns, fn); });
      },
      off: function () { offs.forEach(function (f) { f(); }); offs = []; }
    };

    var hint = resolve(hints()[idx]);
    state.step = hint.step;
    state.variant = hint.variant;
    var step = state.step;
    // a hint that may wait for its texts starts hidden
    if (step.hold) { setQuiet(true); }
    renderBox();
    if (step.enter) {
      try { step.enter(ctx); } catch (e) { /* a missing element must not stop the tour */ }
    }
  }

  // the hint is done: take everything away for a moment, so the user sees
  // what their tap did, then show the next one
  function pauseBefore(idx, now) {
    state.wins = [];
    setQuiet(true);
    drawDim();
    state.pause = { until: now + PAUSE, next: idx };
  }

  function frame(now) {
    if (!state) { return; }
    state.raf = window.requestAnimationFrame(frame);
    state.langWins = languageWindows();
    var last = hints().length - 1;

    if (state.pause) {
      if (now < state.pause.until) { drawDim(); return; }
      var next = state.pause.next;
      state.pause = null;
      go(next, now);
    } else if (state.ctx && state.ctx.next) {
      if (state.idx >= last || state.step.end) { finish(); return; }
      go(state.idx + 1, now);
    }

    var step = state.step;
    var done = false;
    try { done = !!(step.advance && step.advance(state.ctx)); } catch (e) { done = false; }
    if (done) {
      if (state.ctx) { state.ctx.off(); }
      if (state.idx >= last || step.end) { finish(); return; }
      pauseBefore(state.idx + 1, now);
      return;
    }

    var held = false;
    try { held = !!(step.hold && step.hold(state.ctx)); } catch (e) { held = false; }
    var raw = [];
    try { raw = step.targets ? step.targets(state.ctx) : []; } catch (e) { raw = []; }
    var moving = !!state.moving;
    var goals = !held && !moving ? raw : [];
    var quiet = held || moving ||
                (!!step.targets && !goals.length && now - state.stepStart < WAIT_SHOW);

    setQuiet(quiet);
    updateWindows(goals, now);
    drawDim();
    state.box.classList.toggle('vftTutorialWaiting', !!step.targets && !goals.length);
    if (!quiet) { placeCard(goals, step); }
  }

  function start(key) {
    if (state || !TOURS[key]) { return; }
    chain = null;
    var ui = buildOverlay();
    state = {
      key: key, root: ui.root, canvas: ui.canvas, box: ui.box,
      wins: [], langWins: [], data: {}, idx: 0, step: null, variant: null, ctx: null,
      quiet: true, pause: null,
      moving: false, downOk: false, dimSig: null
    };
    readLook();
    listen(true);
    go(0, performance.now());
    state.raf = window.requestAnimationFrame(frame);
  }

  function stop(status) {
    if (!state) { return; }
    if (status) { storeStatus(status); }
    if (state.ctx) { state.ctx.off(); }
    window.cancelAnimationFrame(state.raf);
    listen(false);
    state.root.remove();
    state = null;
  }

  // The last hint is done. The tour hands on to wherever the user goes next.
  function finish() {
    var from = state.key;
    stop('done');
    waitForNext(from);
  }

  /* ---------------------------- chaining ---------------------------- */
  // A finished tour watches the nav bar's ring. When it lands on another
  // step, that step's tour starts - if it has one - once no modal is up (a
  // step can open on one) and the texts are in. The move can take a while:
  // entering a step may first derive its data behind a progress bar.

  function waitForNext(from) {
    var token = chain = { from: from, since: Date.now() };
    (function poll() {
      if (chain !== token || state) { return; }
      if (Date.now() - token.since > CHAIN_MAX) { chain = null; return; }
      var key = ringKey();
      if (key && key !== from) {
        if (!TOURS[key]) { chain = null; return; }
        if (!modalOpen() && texts) {
          chain = null;
          // a moment for the step's own page to settle
          setTimeout(function () { if (!state) { start(key); } }, 600);
          return;
        }
      }
      setTimeout(poll, 400);
    })();
  }

  // The help button's modal: close it, then start this step's tour.
  window.vftTutorialStart = function (key) {
    if (state || !TOURS[key]) { return; }
    closeOffer();
    var go = function () {
      if (state) { return; }
      if (texts) { start(key); return; }
      // the texts are sent at session start; give them a moment
      var n = 0;
      (function wait() {
        if (texts) { start(key); } else if (n++ < 25) { setTimeout(wait, 200); }
      })();
    };
    var modal = document.getElementById('shiny-modal');
    if (modalOpen() && window.jQuery) {
      var fired = false;
      var once = function () { if (!fired) { fired = true; go(); } };
      jQuery(modal).one('hidden.bs.modal', once);
      setTimeout(once, 1000);
      hideModal();
    } else {
      go();
    }
  };

  // for tests
  window.vftTutorialTours = function () {
    var out = {};
    Object.keys(TOURS).forEach(function (k) { out[k] = TOURS[k].length; });
    return out;
  };
  window.vftTutorialState = function () {
    var lang = texts ? texts.lang : null;
    return state ? { key: state.key, idx: state.idx, variant: state.variant, quiet: state.quiet,
                     pause: !!state.pause, choice: state.data.choice || null, lang: lang }
                 : { key: null, chain: chain ? chain.from : null, lang: lang };
  };

  /* -------------------------- first visit --------------------------- */
  // Once step 1's map is drawn and the texts are in - and any modal the
  // session opened with (the crash-recovery prompt) is closed - a bubble
  // under the help button (#helpButton, vftStepNav() in R/app_ui.R) offers
  // step 1's tour. It stays until its button or its x is tapped, or the help
  // button itself, whose modal has the same button - or until the user
  // leaves step 1.

  var mapSeen = false;
  var offerTried = false;
  var offer = null;       // the bubble, null when none
  var offerRaf = null;
  var offerSig = null;

  function renderOffer() {
    if (!offer || !texts) { return; }
    offer.querySelector('.vftTutorialOfferText').innerHTML = fmt(texts.offer || '');
    offer.querySelector('.vftTutorialStartBtn').textContent = texts.start || '';
    offerSig = null;
    placeOffer();
  }

  // right under the help button, its caret's tip on the button's middle
  function placeOffer() {
    if (!offer) { return; }
    var help = document.getElementById('helpButton');
    var r = shown(help) && help.getBoundingClientRect();
    var caretH = parseFloat(getComputedStyle(offer).getPropertyValue('--vftTut-caret-size')) || 0;
    var w = offer.offsetWidth;
    var vw = document.documentElement.clientWidth;
    var cx = r ? r.left + r.width / 2 : vw / 2;
    var left = Math.round(Math.max(GUTTER, Math.min(cx - w / 2, vw - GUTTER - w)));
    var top = Math.round(r ? r.bottom + 4 + caretH : 0);
    // the caret stays clear of the bubble's rounded corners
    var caret = Math.round(Math.max(OFFER_CARET_MIN, Math.min(cx - left, w - OFFER_CARET_MIN)));
    var sig = [!!r, left, top, caret].join();
    if (sig === offerSig) { return; }
    offerSig = sig;
    offer.style.visibility = r ? '' : 'hidden';
    offer.style.left = left + 'px';
    offer.style.top = top + 'px';
    offer.style.setProperty('--vftTut-caret-x', caret + 'px');
    // it pops out of the caret's tip
    offer.style.transformOrigin = caret + 'px -' + caretH + 'px';
  }

  function followHelp() {
    offerRaf = null;
    if (!offer) { return; }
    // gone with step 1 - it offers step 1's tour
    var key = ringKey();
    if (key && key !== 'step1') { closeOffer(); return; }
    placeOffer();
    offerRaf = window.requestAnimationFrame(followHelp);
  }

  function closeOffer() {
    if (offerRaf) { window.cancelAnimationFrame(offerRaf); offerRaf = null; }
    document.removeEventListener('click', helpTapped, true);
    if (offer) { offer.remove(); offer = null; }
    offerSig = null;
  }

  function helpTapped(e) {
    var help = document.getElementById('helpButton');
    if (help && help.contains(e.target)) { closeOffer(); }
  }

  function showOffer() {
    if (offer || state) { return; }
    offer = document.createElement('div');
    offer.className = 'vftTutorialOffer';
    offer.setAttribute('role', 'dialog');
    offer.innerHTML =
      '<button type="button" class="vftTutorialOfferClose" aria-label="Close">&times;</button>' +
      '<div class="vftTutorialOfferText"></div>' +
      '<button type="button" class="btn vftTutorialStartBtn"></button>';
    offer.querySelector('.vftTutorialOfferClose').addEventListener('click', function () {
      storeStatus('declined');
      closeOffer();
    });
    offer.querySelector('.vftTutorialStartBtn').addEventListener('click', function () {
      closeOffer();
      window.vftTutorialStart('step1');
    });
    document.body.appendChild(offer);
    document.addEventListener('click', helpTapped, true);
    renderOffer();
    followHelp();
  }

  function maybeOffer() {
    if (offerTried || !mapSeen || !texts) { return; }
    offerTried = true;
    var seen = storedStatus();
    if (seen && seen.status) { return; }
    var since = Date.now();
    (function wait() {
      if (state || offer || Date.now() - since > OFFER_WAIT) { return; }
      var key = ringKey();
      if (!document.getElementById('helpButton') || key !== 'step1' || modalOpen()) {
        setTimeout(wait, 500);
        return;
      }
      showOffer();
    })();
  }

  if (window.jQuery) {
    jQuery(document).on('shiny:value', function (e) {
      if (mapSeen || e.name !== 'step1-areaSelectMap') { return; }
      mapSeen = true;
      // the map needs a moment to draw; the bubble then gets its turn
      setTimeout(maybeOffer, 1200);
    });
  }

  function onTexts(msg) {
    texts = msg || null;
    if (state) { renderBox(); }
    renderOffer();
    maybeOffer();
  }

  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('vft-tutorial-texts', onTexts);
  } else if (window.jQuery) {
    jQuery(document).one('shiny:connected', function () {
      Shiny.addCustomMessageHandler('vft-tutorial-texts', onTexts);
    });
  }
})();
