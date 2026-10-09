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
 * it starts there (see "chaining") - or, where NEXT says so, another tour:
 * the scenarios page's hands on to toHitze, one hint on step 5 that is no
 * page's own. R/tutorial.R's VFT_TUTORIAL_TOURS has to list the same keys -
 * data-raw/verify_tutorial.R checks it does. Which tours were played to their
 * end is kept on the device too (vft.tutorial.done.v1).
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
  var DONE_KEY = 'vft.tutorial.done.v1';

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

  // the second card of a hint with `langTip`, under the language selector: in
  // all three languages at once, since it is there for whoever cannot read the
  // one being shown
  var LANG_TIP = 'Change language here.<br>Modifiez la langue ici.<br>Ändern Sie die Sprache hier.';

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

  // the tours played to their end on this device: {key: when}
  function toursDone() {
    try {
      var raw = window.localStorage.getItem(DONE_KEY);
      return (raw && JSON.parse(raw)) || {};
    } catch (e) {
      return {};
    }
  }

  function storeDone(key) {
    var done = toursDone();
    done[key] = Date.now();
    try { window.localStorage.setItem(DONE_KEY, JSON.stringify(done)); } catch (e) { /* as above */ }
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

  // The list's first two rows, and above it the "most widespread" caption
  // and its arrow (two h5s, step2_ui.R), as one window
  function topSpecies() {
    var list = document.querySelector(SPECIES);
    var box = list && list.closest('.vft-fit-species');
    if (!shown(box)) { return []; }
    var heads = [], up = box.previousElementSibling;
    for (var i = 0; i < 2 && up; i++) { heads.push(up); up = up.previousElementSibling; }
    var rows = Array.prototype.slice.call(list.querySelectorAll('.checkbox'), 0, 2).filter(shown);
    var top = union(rows.map(function (el) { return elRect(el, 2); }));
    var c = elRect(box, 0);
    top = top && c ? intersect(top, c) : top;
    var r = union(heads.filter(shown).map(function (el) { return elRect(el, 4); }).concat([top]));
    return r ? [{ key: 'topSpecies', rect: r }] : [];
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

  // The slider runs from 0 on the left up to 20 on the right, and starts on
  // 11: "reaching 8" is a value of 8 or below, sent once the slider rests.
  function watchAoiThreshold(ctx) {
    ctx.jq('shiny:inputchanged', function (e) {
      if (e.name === 'step3-AOISlider' && Number(e.value) <= 8) { ctx.flag = true; }
    });
  }

  // Past 8 the slider is put back on 8 exactly and the drag is ended there,
  // so the hint's value is the one the map shows. The slider is a
  // sliderTextInput (step3_ui.R): ion.rangeSlider over the choices "0" ..
  // "20", its `from` an index into them. Its drag ends when `dragging` is
  // off (pointerMove() reads it); the change event is what its binding sends
  // to Shiny on (forceIonSliderTextUpdate() in shinyWidgets does the same).
  var AOI_STOP = 8;

  function clampAoi(ctx) {
    if (ctx.clamped || !window.jQuery) { return; }
    var input = document.getElementById('step3-AOISlider');
    var s = input && jQuery(input).data('ionRangeSlider');
    var values = s && s.options && s.options.values;
    if (!s || !s.result || !values || !values.length) { return; }
    // moved by the user since the hint began
    if (ctx.startFrom == null) { ctx.startFrom = s.result.from; }
    if (s.result.from === ctx.startFrom) { return; }
    if (!(Number(values[s.result.from]) <= AOI_STOP)) { return; }
    var at = values.map(Number).indexOf(AOI_STOP);
    if (at < 0) { return; }
    ctx.clamped = true;
    s.dragging = false;
    s.is_active = false;
    if (s.result.from !== at) { s.update({ from: at }); }
    jQuery(input).trigger('change');
    // done: the value Shiny already had may be this one, and then it sends
    // nothing for watchAoiThreshold() to hear
    ctx.flag = true;
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

  var AUTO_CUT = '#step4-autoCutButton';
  var CUT_SETTLE = 600;   // the corrections count as done once all is quiet this long, ms

  // The automatic corrections, tapped and done: the server disables the
  // button (and reset and confirm) from the tap until the new areas are
  // drawn, behind a progress bar (step4_server.R). Busy has to be seen
  // first, as for a simulation - unless nothing happens at all (nothing to
  // cut returns at once).
  function autoCutDone(ctx) {
    if (!ctx.flag) { return false; }
    var now = performance.now();
    if (ctx.tapAt == null) { ctx.tapAt = now; }
    var b = document.querySelector(AUTO_CUT);
    var busy = (!!b && b.disabled) || progressBars().length > 0 ||
               document.documentElement.classList.contains('shiny-busy');
    if (busy) { ctx.busySeen = true; ctx.quietAt = null; return false; }
    if (!ctx.busySeen && now - ctx.tapAt < SIM_QUIET) { return false; }
    if (ctx.quietAt == null) { ctx.quietAt = now; }
    return now - ctx.quietAt >= CUT_SETTLE;
  }

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

  // A simulation under way: the page starts one for an Original without one
  // as the user arrives (autoLaunch in step5_server.R), so the launch hint
  // has nothing to ask. The button alone, not the bars: a bar on arrival may
  // be some other data's.
  function simRunning() { return launchBusy(); }

  // the "no simulation yet" picture is off the map
  function step5MapShown() {
    return !shown(document.getElementById('step5-mapPlaceholder')) && !!step5Map();
  }

  // the scenario shown has a simulation, and none is running
  function simShown() { return step5MapShown() && !simRunning(); }

  // arriving: the page not up yet, or still busy drawing what it has
  function step5Arriving() {
    return step5NotYet() || document.documentElement.classList.contains('shiny-busy');
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

  // the image button's name modal (obsGenImage in step5_server.R), once up
  function imageModalUp() {
    var input = document.getElementById('step5-nameInput');
    return !!input && modalOpen() && shown(input);
  }

  // the image button; once its name modal is up, that modal instead - the
  // card then points at it, with its own text (7b)
  function imageTarget(ctx) {
    var m = imageNameModal();
    return m.length ? m : sel('#step5-imageButton')(ctx);
  }

  function imageNameModal() {
    var input = document.getElementById('step5-nameInput');
    var content = input && modalOpen() && input.closest('.modal-content');
    var r = shown(content) && elRect(content, 4);
    return r ? [{ key: 'nameModal', rect: r, hit: [content] }] : [];
  }

  // Next on the image hint waits for the image the button may have started:
  // the server draws it before its name modal opens, and that modal would
  // otherwise land in the next hint, whose taps it cannot take
  function afterImage(ctx, done) {
    (function wait() {
      if (!state || state.ctx !== ctx) { return; }
      if (modalOpen() || document.documentElement.classList.contains('shiny-busy')) {
        setTimeout(wait, 200);
        return;
      }
      done();
    })();
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

  /* ---------------------- the scenarios page's hints -------------------- */

  var NV = 'newVersions-';
  var NV_MAP = NV + 'versionMap';
  var NV_CARDS = '#placeholder .vftCard button[id*="versionBtn"]';
  var SQUARE_M = 1000;    // the side of the square the network hints frame, m
  var SQUARE_MARGIN = 150; // map left above and below the square, for the card, px
  var EDGE_MIN = 44;      // a window on one path is at least this big, px

  function nvMap() { return getMap(NV_MAP); }
  function nvEl(id) { return document.getElementById(NV + id); }

  function nvCards() { return Array.prototype.slice.call(document.querySelectorAll(NV_CARDS)); }

  // the Original is the one card without a delete X (appendVersion())
  function isOriginal(card) {
    var c = card.closest('.vftCard');
    return !c || !c.querySelector('.vftCardDel');
  }

  function nvOthers() { return nvCards().filter(function (c) { return !isOriginal(c); }); }

  // no card but the Original is selected - it cannot be edited
  function originalSelected() {
    return !nvCards().some(function (c) { return isSelected(c) && !isOriginal(c); });
  }

  function nvMapReady() {
    var el = document.getElementById(NV_MAP);
    var map = nvMap();
    return shown(el) && !el.classList.contains('recalculating') && !!map && !!map._loaded;
  }

  // A hold that lets go only once `notYet` has stayed false for `ms`: the page
  // draws its map twice on arrival, disabling its controls again in between.
  function settled(notYet, ms) {
    return function (ctx) {
      if (notYet(ctx)) { ctx.readyAt = null; return true; }
      var now = performance.now();
      if (ctx.readyAt == null) { ctx.readyAt = now; }
      return now - ctx.readyAt < ms;
    };
  }

  // Arriving from step 5, the ring is on this page while its network is still
  // being prepared (a progress bar) and step 5's page may still show. The
  // controls come back enabled once the map is drawn (obsFinishRender).
  function nvNotYet() {
    var add = nvEl('addVersionButton');
    return !nvMapReady() || !nvCards().length || !shown(add) || add.disabled;
  }

  function nameModalUp() { return modalOpen() && shown(nvEl('name')); }

  function contextInput(v) {
    return document.querySelector('#' + NV + 'contextChoice input[value="' + v + '"]');
  }

  function contextLabel(v) {
    return function () {
      var i = contextInput(v);
      var lab = i && i.closest('label');
      var r = shown(lab) && elRect(lab, 3);
      return r ? [{ key: 'context' + v, rect: r, hit: [lab] }] : [];
    };
  }

  // the context the map shows: chosen, and the controls enabled again
  function contextOn(v) {
    var i = contextInput(v);
    return !!i && i.checked && !i.disabled;
  }

  // A scripted click on a context radio, until the page is on it: the radios
  // are disabled while the map is drawn, and a click on them then does nothing.
  function ensureContext(ctx, v) {
    var i = contextInput(v);
    if (!i || i.checked || i.disabled) { return; }
    var now = performance.now();
    if (ctx.clickedAt && now - ctx.clickedAt < 1000) { return; }
    ctx.clickedAt = now;
    i.click();
  }

  /* where a tap lands on the map - Leaflet's own hit tests, because the paths
     and the nodes are drawn on canvases and every tap has the same target */

  function layerPointAt(map, x, y) {
    var r = map.getContainer().getBoundingClientRect();
    return map.containerPointToLayerPoint(L.point(x - r.left, y - r.top));
  }

  function hits(layer, p) {
    return !!layer && !!layer._map && typeof layer._containsPoint === 'function' &&
           layer._containsPoint(p);
  }

  // a path: every line of the map, the ones added since the render as well
  // (they are in no group) - not the outline, which is a polygon
  function onPath(x, y) {
    var map = nvMap();
    if (!map || !window.L) { return false; }
    var p = layerPointAt(map, x, y), found = false;
    map.eachLayer(function (l) {
      if (!found && l instanceof L.Polyline && !(l instanceof L.Polygon) &&
          l.options.interactive !== false && hits(l, p)) { found = true; }
    });
    return found;
  }

  function onNode(x, y) {
    var map = nvMap();
    if (!map || !window.L) { return false; }
    var p = layerPointAt(map, x, y);
    return groupLayers(map, 'nodes').some(function (l) { return hits(l, p); });
  }

  // the selected node: a marker with a red X in place of the node's circle
  // (obsMarkerClick in newVersions_server.R), layerId "XXX"
  function selectedNode() {
    var map = nvMap();
    var lm = map && map.layerManager;
    var m = lm && lm.getLayer && lm.getLayer('marker', 'XXX');
    return m && m._map && m._icon ? m : null;
  }

  function onSelectedNode(x, y) {
    var m = selectedNode();
    return !!m && contains(elRect(m._icon, 2), x, y);
  }

  function nvSquare() {
    var map = nvMap();
    var nodes = map ? groupLayers(map, 'nodes') : [];
    if (!nodes.length || !window.L) { return null; }
    // centred on the node nearest the middle of the network, so there are
    // paths and nodes in it
    var mid = L.latLngBounds(nodes.map(function (n) { return n.getLatLng(); })).getCenter();
    var c = null, best = Infinity;
    nodes.forEach(function (n) {
      var d = mid.distanceTo(n.getLatLng());
      if (d < best) { best = d; c = n.getLatLng(); }
    });
    var dLat = SQUARE_M / 2 / 111320;
    var dLng = SQUARE_M / 2 / (111320 * Math.cos(c.lat * Math.PI / 180));
    return L.latLngBounds([c.lat - dLat, c.lng - dLng], [c.lat + dLat, c.lng + dLng]);
  }

  // The network's paths and nodes are on the map (context 1, a scenario that
  // is not the Original - that one is drawn without nodes). Takes the page to
  // context 1 first if it is on another one.
  function networkNotYet(ctx) {
    ensureContext(ctx, '1');
    var map = nvMap();
    return !nvMapReady() || !contextOn('1') || !groupLayers(map, 'nodes').length ||
           !groupLayers(map, 'paths').length;
  }

  // The map zoomed onto the square, with SQUARE_MARGIN px of map left above
  // and below it: the card goes above the square rather than onto the nodes in
  // it. The zoom that gives that is fractional, so the map's zoom snap is off
  // for the move (the zoom buttons step on from there).
  function frameSquare(map, sq) {
    holdForMove(map, function () {
      var size = map.getSize();
      var want = clamp(Math.min(size.x, size.y) - 2 * SQUARE_MARGIN, 320, 640);
      var nw = map.project(sq.getNorthWest(), 0), se = map.project(sq.getSouthEast(), 0);
      var z = Math.log(want / Math.max(se.x - nw.x, se.y - nw.y)) / Math.LN2;
      z = Math.min(z, map.getMaxZoom());
      var snap = map.options.zoomSnap;
      map.options.zoomSnap = 0;
      map.once('moveend', function () { map.options.zoomSnap = snap; });
      if (still()) { map.setView(sq.getCenter(), z, { animate: false }); }
      else { map.flyTo(sq.getCenter(), z, { duration: 0.8 }); }
    });
  }

  // The 1 km square in the middle of the network, as a window on the map. The
  // first time, the map is zoomed onto it. `hitAt`: which taps in it go
  // through (all of them without).
  function networkSquare(hitAt) {
    return function () {
      var map = nvMap();
      if (!map || !map._loaded || !window.L) { return []; }
      if (!state.data.square) {
        state.data.square = nvSquare();
        if (!state.data.square) { return []; }
        frameSquare(map, state.data.square);
        return [];
      }
      var out = boundsWindow(nvMap, state.data.square, 'square')();
      if (out.length && hitAt) { out[0].hitAt = hitAt; }
      return out;
    };
  }

  // the signage and surface legends, as one window (networkLegends() in
  // newVersions_server.R; translated, so found by class)
  function pathLegends() {
    var el = document.getElementById(NV_MAP);
    var parts = el ? Array.prototype.slice.call(
      el.querySelectorAll('.vft-legend-signage, .vft-legend-surface')) : [];
    var r = union(parts.filter(shown).map(function (c) { return elRect(c, 4); }));
    return r ? [{ key: 'legends', rect: r }] : [];
  }

  // the path clicked, remembered for the hints that show where it was
  function watchPathClick(ctx) {
    ctx.jq('shiny:inputchanged', function (e) {
      if (e.name !== NV + 'versionMap_shape_click' || !e.value || e.value.id == null) { return; }
      var map = nvMap();
      var lm = map && map.layerManager;
      var l = lm && lm.getLayer && lm.getLayer('shape', String(e.value.id));
      if (l && l.getBounds) { state.data.edge = l.getBounds(); }
    });
  }

  // A path remembered in state.data[name] (its bounds), however short, as a
  // window; `hit` [] takes no taps
  function pathWindow(name) {
    return function () {
      var map = nvMap();
      var b = state.data[name];
      if (!map || !map._loaded || !b) { return []; }
      var cr = elRect(map.getContainer(), 0);
      var nw = map.latLngToContainerPoint(b.getNorthWest());
      var se = map.latLngToContainerPoint(b.getSouthEast());
      var r = { l: cr.l + nw.x - 10, t: cr.t + nw.y - 10, r: cr.l + se.x + 10, b: cr.t + se.y + 10 };
      var dx = Math.max(0, EDGE_MIN - (r.r - r.l)) / 2, dy = Math.max(0, EDGE_MIN - (r.b - r.t)) / 2;
      r = intersect({ l: r.l - dx, t: r.t - dy, r: r.r + dx, b: r.b + dy }, cr);
      return r ? [{ key: name, rect: r, hit: [] }] : [];
    };
  }

  // the clicked path
  var edgeWindow = pathWindow('edge');

  // Where the new path goes: from the selected node to the tap that made it -
  // a new node on the map, or another node (that tap reaches the map's click
  // too, so the node's wins). Remembered for the hint after its modal.
  function watchNewPath(ctx) {
    ctx.jq('shiny:inputchanged', function (e) {
      var m = selectedNode();
      var v = e.value;
      if (!m || !v || v.lat == null || v.lng == null) { return; }
      var toNode = e.name === NV + 'versionMap_marker_click';
      if (toNode && v.id === 'XXX') { return; }
      if (!toNode && (e.name !== NV + 'versionMap_click' || ctx.toNode)) { return; }
      if (toNode) { ctx.toNode = true; }
      state.data.newPath = L.latLngBounds([m.getLatLng(), L.latLng(v.lat, v.lng)]);
    });
  }

  var newPathWindow = pathWindow('newPath');

  function modalWith(id) { return function () { return modalOpen() && shown(nvEl(id)); }; }

  // a click on this button of a modal, and the modal gone after it
  function clickedAndClosed(ctx) { return !!ctx.flag && !modalOpen(); }

  // the quality radios of the path modal, as one window
  function qualityRadios() {
    var els = ['pathSignage', 'pathType', 'pathWidth'].map(nvEl).filter(shown);
    var r = union(els.map(function (el) { return elRect(el, 4); }));
    return r ? [{ key: 'qualities', rect: r, hit: els }] : [];
  }

  // the new path's modal: its body and its submit button - not its cancel
  function newPathModal() {
    var content = document.querySelector('#shiny-modal .modal-content');
    var r = shown(content) && elRect(content, 4);
    if (!r) { return []; }
    return [{ key: 'newPath', rect: r,
              hit: [content.querySelector('.modal-body'), nvEl('submitNewPath')] }];
  }

  function watchNodeDelete(ctx) {
    ctx.jq('shiny:inputchanged', function (e) {
      if (e.name === NV + 'versionMap_marker_click' && e.value && e.value.id === 'XXX') {
        ctx.flag = true;
      }
    });
  }

  function watchContext(v) {
    return function (ctx) {
      ctx.on(document, 'change', function (e) {
        if (e.target && e.target === contextInput(v) && e.target.checked) { ctx.flag = true; }
      }, true);
    };
  }

  // the outline of the study area on the map (drawn on every context)
  function outlineBounds(map) {
    var b = null;
    map.eachLayer(function (l) {
      if (!b && l instanceof L.Polygon && l.options.color === 'black' && l.options.fill === false) {
        b = l.getBounds();
      }
    });
    return b && b.isValid() ? b : null;
  }

  // context 3 drawn: its parking polygons in place of the network
  function parkingNotYet() {
    var map = nvMap();
    return !nvMapReady() || !contextOn('3') || groupLayers(map, 'nodes').length > 0 ||
           !outlineBounds(map);
  }

  // Before the first vertex, a tap on a parking or residential area deletes it
  // (obsShapeClick, context 3); once polydraw.js has a drawing under way, the
  // areas are click-through and the tap adds a vertex.
  function drawTap(x, y, el) {
    var map = nvMap();
    var drawing = !!map && map.getContainer().classList.contains('vft-pd-drawing');
    return drawing || !(el && el.closest && el.closest('.leaflet-layer2-pane .leaflet-interactive'));
  }

  // the study area's outline, framed whole on the map the first time
  function outlineWindow(ctx) {
    var map = nvMap();
    if (!map || !map._loaded || !window.L) { return []; }
    var b = outlineBounds(map);
    if (!b) { return []; }
    // once per map instance: a render that lands after the framing builds a
    // new map, at the view the page remembered
    if (ctx.framed !== map) {
      ctx.framed = map;
      holdForMove(map, function () {
        if (still()) { map.fitBounds(b, { animate: false, padding: [20, 20] }); }
        else { map.flyToBounds(b, { duration: 0.8, padding: [20, 20] }); }
      });
      return [];
    }
    var out = boundsWindow(nvMap, b, 'outline')();
    if (out.length) { out[0].hitAt = drawTap; }
    return out;
  }

  // the first scenario after the Original, picked once
  function watchFirstOther(ctx) {
    ctx.card = nvOthers()[0] || null;
    centreIn(ctx.card, ctx.card && ctx.card.closest('.vft-ws-list'));
  }

  function firstOtherTarget(ctx) {
    if (!ctx.card || !ctx.card.isConnected) { watchFirstOther(ctx); }
    var card = ctx.card;
    return clipped(card, card && card.closest('.vft-ws-list'), 4, 'card');
  }

  // a tap on a context's button - also when it is the one already on, which
  // changes nothing and so fires no change
  function watchContextTap(v) {
    return function (ctx) {
      ctx.on(window, 'click', function (e) {
        var i = contextInput(v);
        var lab = i && i.closest('label');
        if (lab && e.target && lab.contains(e.target)) { ctx.flag = true; }
      }, true);
    };
  }

  function selectedCard() { return nvCards().filter(isSelected)[0] || null; }

  function originalCard() { return nvCards().filter(isOriginal)[0] || null; }

  // a card as a window, cut to the list it scrolls in
  function cardWindow(card, key, hit) {
    return clipped(card, card && card.closest('.vft-ws-list'), 4, key, hit);
  }

  // a card brought into sight in its list, as the hint starts
  function showCard(pick) {
    return function () {
      var card = pick();
      centreIn(card, card && card.closest('.vft-ws-list'));
    };
  }

  function selectedTarget() { return cardWindow(selectedCard(), 'selected'); }

  function originalTarget() { return cardWindow(originalCard(), 'original'); }

  // The seeded scenario's placeholder name, on the selected card: a click on
  // it asks for a real one (versionLabel() in newVersions_server.R)
  function provisionalName() {
    var card = selectedCard();
    return card && card.querySelector('.vftProvisionalName');
  }

  function renameModalUp() { return modalOpen() && shown(nvEl('renameName')); }

  // The selected card, only its name taking a tap; once that has opened the
  // rename modal, the whole modal (and the text of its own, b)
  function provisionalTarget(ctx) {
    if (renameModalUp()) {
      ctx.renaming = true;
      var content = document.querySelector('#shiny-modal .modal-content');
      var r = shown(content) && elRect(content, 4);
      return r ? [{ key: 'rename', rect: r, hit: [content] }] : [];
    }
    var name = provisionalName();
    return name ? cardWindow(selectedCard(), 'selected', [name]) : [];
  }

  /* ---------------------- heat mitigation's hints ---------------------- */
  // The scenarios page on its fourth context (the ring is on `hitze`): the
  // land cover as paint, the palette under the map, the heat read-out.

  var PAINT_PANEL = '#' + NV + 'paintColorButtonsDiv';
  var GRASS = '#' + NV + 'paintColor_grass';
  var TREE = '#' + NV + 'paintColor_canopyTree';
  var TREE_HEIGHTS = '#' + NV + 'paintHeightGroup_canopy_tree';
  var HEAT = '#' + NV + 'heatSwitch';
  var HEAT_BINS = '#' + NV + 'heatBin';
  var PLAN_BTN = '#' + NV + 'paintImport';
  var PLAN_PANEL = '#' + NV + 'planImportPanel .vft-plan-panel';
  var PLAN_CARD = '.vft-plan-card';
  // the plan import's "leave" buttons: cancel while placing; back and cancel
  // on the colour card (its Apply and the placing's Next are btn-success)
  var PLAN_LEAVE = PLAN_PANEL + ' .btn-default, ' +
                   PLAN_CARD + ' .vft-plan-right > .vft-plan-row .btn-default';
  var PLAN_APPLY = PLAN_CARD + ' .vft-plan-right > .vft-plan-row .btn-success';
  // the plan the tour hands over in place of an upload (data-raw/make_tutorial_plan.R)
  var PLAN_URL = 'www/vft-tutorial-plan.png';
  // the underground warning (ugShowWarning() in newVersions_server.R), a
  // notification that stays until closed, and the button in it
  var UG_BOX = '[id^="shiny-notification-"][id$="ugWarn"]';
  var UG_BTN = UG_BOX + ' .shiny-notification-content button';
  var PAINT_FOR = 3000;   // a painting hint moves on this long after the stroke began, ms

  // a map is on screen: the heat button is held down (showHeat())
  function heatOn() {
    var b = document.querySelector(HEAT);
    return !!b && b.classList.contains('paintToolActive');
  }

  // a heat job is in flight (heatWorking())
  function heatBusy() {
    var b = document.querySelector(HEAT);
    return !!b && b.classList.contains('paintToolBusy');
  }

  // A scripted click, at most one a second, until the page has followed - as
  // ensureContext() does for the context radios.
  function nudge(ctx, el) {
    var now = performance.now();
    if (!el || el.disabled || (ctx.clickedAt && now - ctx.clickedAt < 1000)) { return; }
    ctx.clickedAt = now;
    el.click();
  }

  // Through the nav bar's heat mitigation button the ring is here before the
  // page is: its map, its cards and the palette under the map.
  function hitzeNotYet() {
    return !nvMapReady() || !contextOn('4') || !nvCards().length ||
           !shown(document.querySelector(PAINT_PANEL));
  }

  // The materials are on the map and the palette is up: a heat map left on
  // from before the tour is taken down, since it hides both.
  function materialsNotYet(ctx) {
    if (hitzeNotYet()) { return true; }
    if (heatBusy()) { return true; }
    if (heatOn()) { nudge(ctx, document.querySelector(HEAT)); return true; }
    return false;
  }

  // ...and the ground materials can be picked: the palette is on the ground
  // level (the other level's buttons are disabled)
  function groundNotYet(ctx) {
    if (materialsNotYet(ctx)) { return true; }
    var level = nvEl('paintLevel');
    if (level && level.checked) { nudge(ctx, level); return true; }
    var grass = document.querySelector(GRASS);
    return !shown(grass) || grass.disabled;
  }

  // The extent of the land cover on the map, in viewport pixels. The paint
  // window (paintbrush.js) is a rectangle of LV95 cells, and the brush's own
  // fit says where a point of the map is in LV95 - near enough affine over one
  // view of the map to be inverted from three of its corners.
  function materialsRect(map, cr) {
    var H = window.__vftPaintHooks;
    var win = H && H.ready() && H.window();
    if (!win) { return null; }
    var res = H.res(), size = map.getSize();
    var lv = function (x, y) {
      var m = L.CRS.EPSG3857.project(map.containerPointToLatLng(L.point(x, y)));
      return H.mercatorToLV95(m.x, m.y);
    };
    var o = lv(0, 0), px = lv(size.x, 0), py = lv(0, size.y);
    var a = (px.E - o.E) / size.x, b = (py.E - o.E) / size.y;
    var c = (px.N - o.N) / size.x, d = (py.N - o.N) / size.y;
    var det = a * d - b * c;
    if (!det) { return null; }
    var E = [win.col0 * res, (win.col0 + win.w) * res];
    var N = [(win.rowTop + 1 - win.h) * res, (win.rowTop + 1) * res];
    var xs = [], ys = [];
    E.forEach(function (e) {
      N.forEach(function (n) {
        xs.push((d * (e - o.E) - b * (n - o.N)) / det);
        ys.push((a * (n - o.N) - c * (e - o.E)) / det);
      });
    });
    return { l: cr.l + Math.min.apply(null, xs), t: cr.t + Math.min.apply(null, ys),
             r: cr.l + Math.max.apply(null, xs), b: cr.t + Math.max.apply(null, ys) };
  }

  // The study area as a window on the map, which takes the brush: its outline
  // and the land cover round it, which reaches a little further out. `frame`:
  // the map is first moved to show it whole. Zoomed in until none of it is on
  // the map, the window is the map itself.
  function heatArea(frame) {
    return function (ctx) {
      var map = nvMap();
      if (!map || !map._loaded || !window.L) { return []; }
      var c = map.getContainer();
      var cr = elRect(c, 0);
      if (!cr) { return []; }
      var b = outlineBounds(map);
      var nw = b && map.latLngToContainerPoint(b.getNorthWest());
      var se = b && map.latLngToContainerPoint(b.getSouthEast());
      var r = union([materialsRect(map, cr),
                     b ? { l: cr.l + nw.x, t: cr.t + nw.y, r: cr.l + se.x, b: cr.t + se.y } : null]);
      if (!r) { return []; }
      if (frame && !ctx.framed) {
        ctx.framed = true;
        if (r.l < cr.l || r.t < cr.t || r.r > cr.r || r.b > cr.b) {
          var whole = L.latLngBounds(
            map.containerPointToLatLng(L.point(r.l - cr.l, r.t - cr.t)),
            map.containerPointToLatLng(L.point(r.r - cr.l, r.b - cr.t)));
          holdForMove(map, function () {
            if (still()) { map.fitBounds(whole, { animate: false, padding: [20, 20] }); }
            else { map.flyToBounds(whole, { duration: 0.8, padding: [20, 20] }); }
          });
          return [];
        }
      }
      return [{ key: 'area', rect: intersect(inflate(r, 8), cr) || cr, hit: [c] }];
    };
  }

  // A stroke begun on the map: the brush's own layer takes the press
  // (paintbrush.js). Registered after the tap filter, so it only ever sees a
  // press the filter let through.
  function watchPaint(ctx) {
    ctx.on(window, 'pointerdown', function (e) {
      if (!ctx.paintAt && e.button === 0 && e.target && e.target.closest &&
          e.target.closest('.paint-input-overlay')) { ctx.paintAt = performance.now(); }
    }, true);
  }

  function painted(ctx) {
    return !!ctx.paintAt && performance.now() - ctx.paintAt >= PAINT_FOR;
  }

  function watchLevel(ctx) {
    ctx.on(document, 'change', function (e) {
      if (e.target && e.target === nvEl('paintLevel')) { ctx.flag = true; }
    }, true);
  }

  // on the canopy level, and the server has followed: its materials are
  // enabled and the remembered one is armed (the paintLevel observer)
  function canopyOn(ctx) {
    var level = nvEl('paintLevel');
    var tree = document.querySelector(TREE);
    return !!ctx.flag && !!level && level.checked && !!tree && !tree.disabled;
  }

  function treeArmed() {
    var tree = document.querySelector(TREE);
    return !!tree && tree.classList.contains('colorBtnSelected');
  }

  // another height than the one armed
  function watchHeight(ctx) {
    ctx.on(window, 'click', function (e) {
      var btn = e.target && e.target.closest && e.target.closest(TREE_HEIGHTS + ' .paintHeightBtn');
      if (btn && !btn.classList.contains('colorBtnSelected')) { ctx.flag = true; }
    }, true);
  }

  // the warning's "ignore this element" button; once ignored it is replaced
  // by a disabled one, which has no onclick
  function ugIgnoreButton() {
    var b = document.querySelector(UG_BTN);
    return shown(b) && !b.disabled && b.hasAttribute('onclick') ? b : null;
  }

  function ugTarget() {
    var b = document.querySelector(UG_BTN);
    var r = shown(b) && elRect(b, 4);
    return r ? [{ key: 'ugIgnore', rect: r, hit: [b] }] : [];
  }

  function watchUgIgnore(ctx) {
    ctx.on(window, 'click', function (e) {
      var b = ugIgnoreButton();
      if (b && e.target && b.contains(e.target)) { ctx.ignoredAt = performance.now(); }
    }, true);
  }

  // The warning sits bottom right, over the heat controls, and stays until it
  // is closed: closed here, before they are needed.
  function ugBoxUp(ctx) {
    var box = document.querySelector(UG_BOX);
    if (!shown(box)) { return false; }
    nudge(ctx, box.querySelector('.shiny-notification-close'));
    return true;
  }

  // The heat map is computed at midday unless the user has chosen otherwise:
  // the hint after it says "at noon".
  function watchHeatLaunch(ctx) {
    var noon = document.querySelector(HEAT_BINS + ' input[value="midday"]');
    if (noon && !noon.checked && !noon.disabled) { noon.click(); }
    watchClick(HEAT)(ctx);
  }

  // the heat job's progress bar - not the underground warning, which is a
  // notification too
  function heatBars() {
    return progressBars().filter(function (el) { return !el.matches(UG_BOX); });
  }

  // While it runs, the bar; if it ended without a map (a failed job), the
  // button again, for another try.
  function heatProgress(ctx) {
    var r = union(heatBars().map(function (el) { return elRect(el, 4); }));
    if (r) { ctx.barSeen = true; return [{ key: 'progress', rect: r }]; }
    return ctx.barSeen && !heatBusy() && !heatOn() ? sel(HEAT)() : [];
  }

  function noHeatBarYet(ctx) {
    return !ctx.barSeen && !heatBars().length && performance.now() - ctx.t0 < BAR_WAIT;
  }

  function watchHeatBin(ctx) {
    ctx.on(document, 'change', function (e) {
      if (e.target && e.target.closest && e.target.closest(HEAT_BINS)) { ctx.flag = true; }
    }, true);
  }

  // The heat icons of the card whose map is up (red border, showHeat()), or
  // else of the selected card. The server replaces the strip as maps arrive.
  function heatIconsStrip() {
    var cards = nvCards();
    var card = cards.filter(function (c) { return c.classList.contains('vftHeatCard'); })[0] ||
               cards.filter(isSelected)[0];
    var wrap = card && card.closest('.vftCard');
    return (wrap && wrap.querySelector('.vftHeatIcons')) || null;
  }

  function heatIcons() {
    var strip = heatIconsStrip();
    return clipped(strip, strip && strip.closest('.vft-ws-list'), 4, 'heatIcons');
  }

  // The plan button opens the file picker (its inline onclick). Here the tap
  // is taken from it, and the tutorial's own plan goes to the import as if it
  // had been picked (planimport.js).
  function givePlan(ctx) {
    ctx.on(window, 'click', function (e) {
      if (!(e.target && e.target.closest && e.target.closest(PLAN_BTN))) { return; }
      e.stopImmediatePropagation();
      e.stopPropagation();
      if (e.cancelable) { e.preventDefault(); }
      var start = window.__vftPlanImportStart;
      if (!start || !window.fetch) { return; }
      fetch(PLAN_URL).then(function (res) {
        if (!res.ok) { throw new Error(PLAN_URL + ': ' + res.status); }
        return res.blob();
      }).then(function (blob) {
        start(new File([blob], 'vft-tutorial-plan.png', { type: 'image/png' }));
      }).catch(function (err) { if (window.console) { console.error('tutorial plan:', err); } });
    }, true);
  }

  function notLeaving(x, y, el) { return !(el && el.closest && el.closest(PLAN_LEAVE)); }

  // the map the plan floats on, and the box under it with its Next
  function planPlacing() {
    var map = nvMap();
    var c = map && map.getContainer();
    var panel = document.querySelector(PLAN_PANEL);
    var out = [];
    var r = shown(c) && elRect(c, 2);
    if (r) { out.push({ key: 'map', rect: r, hit: [c] }); }
    r = shown(panel) && elRect(panel, 4);
    if (r) { out.push({ key: 'planPanel', rect: r, hitAt: notLeaving }); }
    return out;
  }

  // the colour card, laid over the map
  function planColours() {
    var card = document.querySelector(PLAN_CARD);
    var r = shown(card) && elRect(card, 4);
    return r ? [{ key: 'planCard', rect: r, hitAt: notLeaving }] : [];
  }

  function shownNow(selector) {
    return function () { return shown(document.querySelector(selector)); };
  }

  // The heat icons with only the noon one taking a tap: shown again at once
  // from the scenario's store (heatIconClick in newVersions_server.R)
  var NOON_ICON = '.vftHeatIcon[data-bin="midday"]';

  function noonIconTarget() {
    var out = heatIcons();
    var strip = heatIconsStrip();
    var noon = strip && strip.querySelector(NOON_ICON);
    if (out.length) { out[0].hit = noon ? [noon] : []; }
    return out;
  }

  function noonShown() {
    var strip = heatIconsStrip();
    var noon = strip && strip.querySelector(NOON_ICON);
    return !!noon && noon.classList.contains('vftHeatShown') && heatOn() && !heatBusy();
  }

  /* -- the plan's colour card, after the automatic assignment -- */

  // the colour rows (renderRows() in planimport.js), rebuilt on every pick
  function planRows() {
    return Array.prototype.slice.call(document.querySelectorAll(PLAN_CARD + ' .vft-plan-colrow'));
  }

  function rowSelect(row) { return row && row.querySelector('select'); }

  // a row as a window: its swatch and its material, only the latter taking taps
  function rowWindow(row, key) {
    var sw = row && row.querySelector('.vft-plan-swatch');
    var se = rowSelect(row);
    var list = row && row.closest('.vft-plan-rows');
    var r = shown(sw) && shown(se) && union([elRect(sw, 4), elRect(se, 4)]);
    var c = r && list && elRect(list, 2);
    r = r && (c ? intersect(r, c) : r);
    return r ? [{ key: key, rect: r, hit: [se] }] : [];
  }

  function swatchRgb(row) {
    var sw = row && row.querySelector('.vft-plan-swatch');
    var m = sw && /rgba?\((\d+),\s*(\d+),\s*(\d+)/.exec(getComputedStyle(sw).backgroundColor);
    return m ? [+m[1], +m[2], +m[3]] : null;
  }

  // The tour's plan draws its paths in tan (data-raw/make_tutorial_plan.R),
  // which the import calls natural ground: the row nearest that colour,
  // picked once.
  var PLAN_PATH_RGB = [210, 180, 140];

  function brownRow(ctx) {
    if (ctx.row && ctx.row.isConnected) { return ctx.row; }
    var best = null, bd = Infinity;
    planRows().forEach(function (row) {
      var c = swatchRgb(row);
      if (!c) { return; }
      var d = Math.pow(c[0] - PLAN_PATH_RGB[0], 2) + Math.pow(c[1] - PLAN_PATH_RGB[1], 2) +
              Math.pow(c[2] - PLAN_PATH_RGB[2], 2);
      if (d < bd) { bd = d; best = row; }
    });
    ctx.row = best;
    return best;
  }

  function rowSetTo(row, material) {
    var s = rowSelect(row);
    return !!s && s.value === material;
  }

  // the material ids of the import's select (MATERIAL_ORDER in planimport.js)
  var MAT_ARTIFICIAL = '3', MAT_CANOPY_ARTIFICIAL = '6';

  // the toggle that shows the plan in its assigned materials, and the one
  // that shows it as uploaded
  var PLAN_RESULT = PLAN_CARD + ' .vft-plan-show-result';
  var PLAN_ORIGINAL = PLAN_CARD + ' .vft-plan-show-original';
  var PLAN_PICK = PLAN_CARD + ' .vft-plan-pickbtn';
  var PLAN_VIEW = PLAN_CARD + ' canvas.vft-plan-view';
  // the darker grey roof of the tour's plan, as fractions of it (x0, y0, x1,
  // y1; the plan is 720 px square, the roof 470..700 x 160..224)
  var PLAN_ROOF = [470 / 720, 160 / 720, 700 / 720, 224 / 720];

  function planResultOn() {
    var b = document.querySelector(PLAN_RESULT);
    return !!b && b.classList.contains('active');
  }

  // back to the plan as uploaded, where the darker grey can be seen
  function showOriginalPlan() {
    var b = document.querySelector(PLAN_ORIGINAL);
    if (b && !b.classList.contains('active')) { b.click(); }
  }

  // the roof on the card's picture of the plan, and the pick button
  function roofAndPick() {
    var cv = document.querySelector(PLAN_VIEW);
    var out = [];
    var r = shown(cv) && elRect(cv, 0);
    if (r) {
      var w = r.r - r.l, h = r.b - r.t;
      out.push({ key: 'roof', hit: [cv],
                 rect: { l: r.l + PLAN_ROOF[0] * w - 3, t: r.t + PLAN_ROOF[1] * h - 3,
                         r: r.l + PLAN_ROOF[2] * w + 3, b: r.t + PLAN_ROOF[3] * h + 3 } });
    }
    return out.concat(sel(PLAN_PICK)());
  }

  // the colour the pick added: the last of the picked rows
  function pickedRow() {
    var rows = document.querySelectorAll(PLAN_CARD + ' .vft-plan-colrow.vft-plan-picked');
    return rows.length ? rows[rows.length - 1] : null;
  }

  // Apply: where the plan lay is kept for the hint after it, which shows it
  // painted (the import forgets it as it closes)
  function watchApply(ctx) {
    ctx.on(window, 'click', function (e) {
      if (!(e.target && e.target.closest && e.target.closest(PLAN_APPLY))) { return; }
      var f = window.__vftPlanImportBounds;
      var b = null;
      try { b = f ? f() : null; } catch (err) { b = null; }
      state.data.planBounds = b ? [[b.getSouth(), b.getWest()], [b.getNorth(), b.getEast()]] : null;
      ctx.flag = true;
    }, true);
  }

  function planArea() {
    var b = state && state.data.planBounds;
    return b ? boundsWindow(nvMap, b, 'planArea')() : heatArea(false)();
  }

  var NV_CONFIRM = '#' + NV + 'newVersionsConfirmButton';

  // Confirming leads to step 5's simulation, so it stays disabled while the
  // page has no path network (Hitzeminderung entered from step 1)
  function confirmShut() {
    var b = document.querySelector(NV_CONFIRM);
    return !shown(b) || b.disabled;
  }

  /* ------------------------------ tours ----------------------------- */
  // targets: the windows. A window's `hit` lists the elements a tap in it has
  //   to land on; its `hitAt(x, y, el)` decides instead, for a map whose
  //   layers are all one canvas (a wheel is always let through).
  // advance(ctx): polled every frame, true moves on.
  // enter(ctx): runs as the hint starts; ctx.on()/ctx.jq() listeners go with
  //   the hint.
  // look: a hint to read - a Next button moves on, the windows take no taps.
  //   onNext(ctx, done): runs before it moves on. live: its windows take taps
  //   all the same (a control that may be used, but need not be).
  // pass: a selector whose taps go through all the same.
  // escPass: Escape is left to the page (polydraw.js takes a vertex back
  //   with it) instead of stopping the tour.
  // avoid: selectors the card must not cover. cardOver: a selector the card
  //   sits on instead of beside the windows. cardNear: a selector the card
  //   goes above (or below) instead of the windows - it stays put while they
  //   come and go. wide: a wider card, for a hint with little height to spare.
  // hug: when there is no room above or below the windows (a whole map), the
  //   card goes to the top or bottom of the screen, covering as little of
  //   them as it can, rather than into the middle.
  // stay: the card keeps the place it first took (the windows move as the
  //   user zooms).
  // below: the card goes right under the windows, nowhere else (as any card
  //   does when that has no room).
  // side: 'left' or 'right' - the card beside the windows (or beside the
  //   element `sideOf` names), pointing at them, rather than above or below.
  // langTip: a second card, left of the language selector, in all three
  //   languages (LANG_TIP).
  // delay: ms added to the pause before this hint shows.
  // hold(ctx): true keeps the hint hidden. center: the text is centred.
  // end: the tour ends once this hint is done, whatever comes after it.
  // skip(): asked as the hint is reached; true passes over it. The counter
  //   leaves out the hints passed over, and those it expects to pass over.
  // skipWhen(ctx): asked every frame while the hint is on; true passes over
  //   it then - as skip() does if it was not shown yet, as done if it was.
  // bodyClass: a class on <body> while the hint is on (vft-tutorial.css).
  // variant(): asked once as the hint starts; a name it returns lays
  //   variants[name] over the hint, and its text is the alternative one.
  // textWhen(ctx): asked every frame; a name it returns shows the variant
  //   text of that name (as variant() does) for as long as it returns it.
  // text: [key, index] - the hint shows another tour's text (borrowed()).
  // alt: a name - a hint added between two numbered ones, whose text is
  //   texts.alts[<key>][<name>] (step 2's '6b'), so the rows after it keep
  //   their numbers.
  // Texts are texts.tours[<key>][<n>], n counting the tour's own hints (a
  // hint with `text` or `alt` has no number); a variant's are
  // texts.alts[<key>][<hint number><name>], e.g. alts.step5['6b']. A hint
  // with a hold re-asks its variant() until it is first shown.
  //
  // Any hint is interrupted by the data-loss warning (vftAskCommit() in
  // R/providers.R) while that modal is up: the warning's own card and a
  // window on the modal take its place, and the hint comes back once the
  // modal is gone. Between two tours, the warning is a tour of one hint
  // (`commit`), and the chain carries on after it.

  var TOURS = {
    step1: [
      { // 1 - a word of welcome over Switzerland, framed whole on the map,
        // and a second card on the language selector
        targets: boundsWindow(step1Map, CH_BOUNDS, 'switzerland'),
        enter: viewSwitzerland,
        hold: textsBehind,
        center: true,
        langTip: true,
        look: true
      },
      { // 2 - the upload card
        targets: sel(UPLOAD_CARD),
        look: true
      },
      { // 3 - draw around Birmensdorf. The map and the drawing card; the card
        // sits left of the drawing card, pointing at it. A refused polygon's
        // modal can be dismissed through the dim.
        targets: function () {
          var map = step1Map();
          var c = map && map.getContainer();
          var out = sel(DRAW_CARD)();
          var r = shown(c) && elRect(c, 2);
          if (r) { out.push({ key: 'map', rect: r, hit: [c] }); }
          return out;
        },
        side: 'left',
        sideOf: DRAW_CARD,
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
      { // 6 - the help button, before the app moves on to the choice; the
        // card right under it
        targets: sel('#helpButton', 8),
        below: true,
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
      { // 6b - the top of the list, the most widespread species, with the
        // caption and the arrow above it
        targets: topSpecies,
        enter: function () {
          var list = document.querySelector(SPECIES);
          var box = list && list.closest('.vft-fit-species');
          if (box) { box.scrollTop = 0; }
        },
        alt: '6b',
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
      { // 10 - hide the bottom 25 %, a second later than usual (the weights
        // just changed redraw the map). The card goes right of the slider,
        // off the map.
        targets: function () {
          var box = sliderBox();
          var r = shown(box) && elRect(box, 4);
          return r ? [{ key: 'threshold', rect: r, hit: [box] }] : [];
        },
        delay: 1000,
        side: 'right',
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
      { // 3 - the threshold slider, down to 8 - and no further: past it, it
        // is put back on 8 and the drag ends
        targets: function () {
          var box = aoiSliderBox();
          var r = shown(box) && elRect(box, 4);
          return r ? [{ key: 'aoiThreshold', rect: r, hit: [box] }] : [];
        },
        enter: watchAoiThreshold,
        advance: function (ctx) { clampAoi(ctx); return flagged(ctx); }
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
      { // 2 - cut an area in two with the scissors. The window is most of
        // the map: the card goes to the top or bottom of the screen.
        targets: areasWindow,
        enter: watchCut,
        advance: cutSplitArea,
        wide: true,
        hug: true,
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
      { // 5 - run the automatic corrections; on once they are done. While
        // they run, the button and the progress bar are the windows.
        targets: each(sel(AUTO_CUT), progressTarget),
        enter: watchClick(AUTO_CUT),
        advance: autoCutDone
      },
      { // 6 - confirm. The tour ends with the tap, and step 5's tour follows.
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
        // Passed over while one runs: the page launches the Original's itself
        // when it has none. b: the scenario shown has its simulation already -
        // the button is only pointed at, Next moves on, and hint 2 is passed
        // over.
        targets: sel(LAUNCH),
        hold: settled(step5Arriving, 700),
        skipWhen: simRunning,
        enter: watchClick(LAUNCH),
        advance: function (ctx) {
          if (ctx.flag) { state.data.userLaunch = true; }
          return ctx.flag;
        },
        pass: '#shiny-modal',
        variant: function () { return simShown() ? 'b' : null; },
        variants: { b: { look: true, advance: null } }
      },
      { // 2 - the progress bars, until the result is on the map: the
        // simulation the page started on arrival. b: the one launched in
        // hint 1, with a text of its own (2b).
        targets: progressTarget,
        skip: function () {
          return !state.data.userLaunch && !simRunning() && !progressBars().length && simShown();
        },
        enter: watchSim,
        hold: noBarYet,
        advance: simDone,
        pass: '#shiny-modal',
        variant: function () { return state.data.userLaunch ? 'b' : null; },
        variants: { b: {} }
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
      { // 6 - switch the sensitivity matrix on; passed over when there is
        // none to show
        targets: switchRow('step5-SMcheckbox'),
        skip: smMissing,
        enter: watchSwitch('step5-SMcheckbox'),
        advance: flagged
      },
      { // 7 - the export: may be used - its name modal too, which gets the
        // window and a text of its own (7b) - Next moves on either way, once
        // the image is done
        targets: imageTarget,
        textWhen: function () { return imageModalUp() ? 'b' : null; },
        look: true,
        live: true,
        pass: '#shiny-modal',
        onNext: afterImage
      },
      { // 8 - the scenario column, the card on its left
        targets: scenarioColumn,
        side: 'left',
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
    ],

    newVersions: [
      { // 1 - this page's button in the nav bar, once its map is drawn
        targets: sel('#vftNav_newVersions', 4),
        hold: settled(nvNotYet, 1500),
        look: true
      },
      { // 2 - only the Original: make a scenario. Its name modal opens.
        targets: sel('#' + NV + 'addVersionButton'),
        skip: function () { return nvOthers().length > 0; },
        advance: nameModalUp
      },
      { // 3 - ...and name it. Its cancel is left out of the window.
        targets: each(function () {
          var input = nvEl('name');
          var box = input && input.closest('.form-group');
          var r = shown(box) && elRect(box, 4);
          return r ? [{ key: 'name', rect: r, hit: [box] }] : [];
        }, sel('#' + NV + 'submitName')),
        skip: function () { return !nameModalUp() && nvOthers().length > 0; },
        enter: watchClick('#' + NV + 'submitName'),
        advance: function (ctx) { return clickedAndClosed(ctx) && nvOthers().length > 0; }
      },
      { // 4 - the Original is selected: select the first scenario after it
        targets: firstOtherTarget,
        skip: function () { return !originalSelected(); },
        enter: watchFirstOther,
        advance: function () { return !originalSelected(); }
      },
      { // 5 - the contexts
        targets: sel('#' + NV + 'contextChoice .shiny-options-group', 4),
        look: true
      },
      { // 6 - Paths/Roads first: Next, or a tap on it, moves on
        targets: contextLabel('1'),
        enter: watchContextTap('1'),
        advance: flagged,
        look: true,
        live: true
      },
      { // 7 - the network, 1 km in its middle, and its legends. On context 1,
        // the map zoomed onto the square.
        targets: each(networkSquare(), pathLegends),
        hold: networkNotYet,
        look: true
      },
      { // 8 - click a path: its modal opens, where it always does, over a
        // faint backdrop (see bodyClass)
        targets: networkSquare(function (x, y) { return onPath(x, y) && !onNode(x, y); }),
        enter: watchPathClick,
        advance: modalWith('deleteEdge'),
        bodyClass: 'vftTutModalLight'
      },
      { // 9 - its qualities: may be changed, Next moves on. The card keeps
        // off the delete button and the title above them.
        targets: qualityRadios,
        avoid: ['#shiny-modal .modal-body h3', '#' + NV + 'deleteEdge'],
        look: true,
        live: true,
        bodyClass: 'vftTutModalLight'
      },
      { // 10 - delete it
        targets: sel('#' + NV + 'deleteEdge'),
        enter: watchClick('#' + NV + 'deleteEdge'),
        advance: clickedAndClosed,
        bodyClass: 'vftTutModalLight'
      },
      { // 11 - it is gone
        targets: edgeWindow,
        look: true
      },
      { // 12 - select a node
        targets: networkSquare(function (x, y) { return onNode(x, y) && !onSelectedNode(x, y); }),
        advance: function () { return !!selectedNode(); }
      },
      { // 13 - a new path from it: to a new node on the map, or to another
        // node. Not onto a path (that cancels the selection) nor onto the
        // selected node (that deletes it). Its modal opens in its usual place.
        targets: networkSquare(function (x, y) {
          return !onSelectedNode(x, y) && (onNode(x, y) || !onPath(x, y));
        }),
        wide: true,
        enter: watchNewPath,
        advance: modalWith('submitNewPath'),
        bodyClass: 'vftTutModalLight'
      },
      { // 14 - its qualities, submitted
        targets: newPathModal,
        enter: watchClick('#' + NV + 'submitNewPath'),
        advance: clickedAndClosed,
        bodyClass: 'vftTutModalLight'
      },
      { // 15 - the new path, on the map
        targets: newPathWindow,
        hold: modalOpen,
        look: true
      },
      { // 16 - delete a node: select one, then tap it again. Once one is
        // selected, only it takes a tap - another node would link the two.
        targets: networkSquare(function (x, y) {
          return selectedNode() ? onSelectedNode(x, y) : onNode(x, y);
        }),
        wide: true,
        enter: watchNodeDelete,
        advance: function (ctx) { return !!ctx.flag && !selectedNode(); }
      },
      { // 17 - the parking and residences context
        targets: contextLabel('3'),
        enter: watchContext('3'),
        advance: flagged
      },
      { // 18 - draw an area (polydraw.js, as in step 1), closed on its first
        // vertex or with a double-click: the type modal. Escape takes a
        // vertex back.
        targets: outlineWindow,
        hold: parkingNotYet,
        advance: modalWith('chooseParking'),
        escPass: true
      },
      { // 19 - parking or residence. A refused overlap raises a modal of its
        // own, which can be dismissed; the type modal's cancel is kept shut.
        targets: buttonRow('#' + NV + 'chooseParking, #' + NV + 'chooseResidential'),
        avoid: ['#shiny-modal .modal-body h4'],
        enter: watchClick('#' + NV + 'chooseParking, #' + NV + 'chooseResidential'),
        advance: clickedAndClosed,
        pass: '#shiny-modal [data-dismiss]'
      },
      { // 20 - the selected scenario keeps all this
        targets: selectedTarget,
        enter: showCard(selectedCard),
        look: true
      },
      { // 21 - only on the seeded scenario: its name can be changed. A tap on
        // it opens the rename modal, which then gets the window and a text of
        // its own (21b); its submit or cancel moves on, as Next does.
        targets: provisionalTarget,
        skip: function () { return !provisionalName(); },
        enter: showCard(selectedCard),
        textWhen: function () { return renameModalUp() ? 'b' : null; },
        advance: function (ctx) { return !!ctx.renaming && !modalOpen(); },
        look: true,
        live: true
      },
      { // 22 - the Original cannot be altered
        targets: originalTarget,
        enter: showCard(originalCard),
        look: true
      },
      { // 23 - confirm, back to step 5. The tour ends with the tap, and the
        // heat mitigation hint follows there (NEXT below).
        targets: sel('#' + NV + 'newVersionsConfirmButton'),
        enter: watchClick('#' + NV + 'newVersionsConfirmButton'),
        advance: flagged,
        pass: '#shiny-modal'
      }
    ],

    // One hint on step 5, played on the way back from the scenarios page while
    // the heat mitigation tour has not been done. Not a page of its own: NEXT
    // starts it instead of step 5's tour.
    toHitze: [
      { // the Hitzeminderung button in the nav bar; its tour follows the tap
        targets: sel('#vftNav_hitze', 4),
        hold: step5NotYet,
        enter: watchClick('#vftNav_hitze'),
        advance: flagged,
        pass: '#shiny-modal'
      }
    ],

    // The data-loss warning between two tours: the tap that ended one (a
    // confirm) raised it. On once the modal is gone, confirmed or cancelled.
    // (Within a tour, COMMIT plays the same card over the hint.)
    commit: [
      {
        targets: commitModal,
        side: 'right',
        hug: true,
        advance: function (ctx) {
          if (commitUp()) { ctx.upSeen = true; return false; }
          return !!ctx.upSeen || performance.now() - state.stepStart > 2000;
        }
      }
    ],

    // Save and load, once per device: after the first tour finished past
    // step 1's (finish()), once no modal is up.
    saveLoad: [
      {
        targets: buttonRow('#vftNav .vft-nav-session', shown),
        hold: settled(modalOpen, 800),
        below: true,
        look: true
      }
    ],

    // Heat mitigation. Three of the scenarios page's hints are played after
    // hint 3 when there is no scenario to paint on (see below the tours).
    hitze: [
      { // 1 - the Hitzeminderung button in the nav bar: what this step is for
        targets: sel('#vftNav_hitze', 4),
        hold: settled(hitzeNotYet, 1500),
        look: true
      },
      { // 2 - this context's button, beside parking/residences
        targets: contextLabel('4'),
        hold: settled(hitzeNotYet, 1500),
        look: true
      },
      { // 3 - the materials on the map, the area framed whole
        targets: heatArea(true),
        hold: materialsNotYet,
        look: true
      },
      { // 4 - pick grass in the palette (armed already on a first visit: the
        // tap is what moves on)
        targets: sel(GRASS),
        hold: groundNotYet,
        enter: watchClick(GRASS),
        advance: flagged
      },
      { // 5 - paint with it; on 3 s after the stroke began. An attempt the
        // page refuses says why in a modal, which can be dismissed. The card
        // stays where it first appeared as the user zooms in.
        targets: heatArea(false),
        enter: watchPaint,
        advance: painted,
        stay: true,
        pass: '#shiny-modal'
      },
      { // 6 - the level switch, up to the canopy
        targets: sel(PAINT_PANEL + ' .paintLevelSwitch', 4),
        enter: watchLevel,
        advance: canopyOn
      },
      { // 7 - arm the tree. The canopy level opens on the artificial canopy
        // on a first visit (newVersions_server.R), so this is a real choice.
        targets: sel(TREE),
        enter: watchClick(TREE),
        advance: function (ctx) { return !!ctx.flag && treeArmed(); }
      },
      { // 8 - its height bar: another height
        targets: sel(TREE_HEIGHTS, 4),
        enter: watchHeight,
        advance: flagged
      },
      { // 9 - paint trees; on 3 s after the stroke began, or as soon as it is
        // stopped by an underground element
        targets: heatArea(false),
        enter: watchPaint,
        advance: function (ctx) { return painted(ctx) || (!!ctx.paintAt && !!ugIgnoreButton()); },
        pass: '#shiny-modal'
      },
      { // 10 - only if the stroke ran into an underground element: the
        // warning's ignore button. It may be used (on 3 s later); Next moves
        // on either way.
        targets: ugTarget,
        skip: function () { return !ugIgnoreButton(); },
        avoid: [UG_BOX],
        enter: watchUgIgnore,
        advance: function (ctx) {
          return !!ctx.ignoredAt && performance.now() - ctx.ignoredAt >= PAINT_FOR;
        },
        look: true,
        live: true
      },
      { // 11 - calculate the heat
        targets: sel(HEAT),
        hold: ugBoxUp,
        enter: watchHeatLaunch,
        advance: flagged,
        pass: '#shiny-modal'
      },
      { // 12 - the progress bar, until the map is up. Passed over when it
        // already is: a stored map is shown at once. The card sits above the
        // heat button, so it stays put as the bars come and go.
        targets: heatProgress,
        skip: heatOn,
        enter: watchSim,
        hold: noHeatBarYet,
        advance: heatOn,
        cardNear: HEAT,
        pass: '#shiny-modal'
      },
      { // 13 - the times of day: another one
        targets: sel(HEAT_BINS + ' .shiny-options-group', 4),
        hold: heatBusy,
        enter: watchHeatBin,
        advance: flagged
      },
      { // 14 - the icons on the scenario's card, once that map is in: the
        // noon one shows its map again, at once. The card goes under the
        // scenario cards, not over them.
        targets: noonIconTarget,
        avoid: ['#placeholder .vftCard'],
        hold: settled(heatBusy, 600),
        advance: noonShown
      },
      { // 15 - hide the heat map
        targets: sel(HEAT),
        skip: function () { return !heatOn(); },
        advance: function () { return !heatOn(); }
      },
      { // 16 - the eraser and the reset
        targets: buttonRow('#' + NV + 'paintEraser, #' + NV + 'paintReset'),
        look: true
      },
      { // 17 - load a plan: the tour's own, no file picker
        targets: sel(PLAN_BTN),
        enter: givePlan,
        advance: shownNow(PLAN_PANEL)
      },
      { // 18 - place it and go on. The card sits on the scenario column, off
        // the plan; Escape would cancel the import, so it stops the tour only.
        targets: planPlacing,
        cardOver: '#topPlaceHolder_newVersion',
        advance: shownNow(PLAN_CARD)
      },
      { // 19 - the colours' materials, to read
        targets: planColours,
        cardOver: '#topPlaceHolder_newVersion',
        look: true
      },
      { // 20 - the tour plan's brown paths, natural ground: make them artificial
        targets: function (ctx) { return rowWindow(brownRow(ctx), 'brownRow'); },
        cardOver: '#topPlaceHolder_newVersion',
        advance: function (ctx) { return rowSetTo(brownRow(ctx), MAT_ARTIFICIAL); }
      },
      { // 21 - the plan in its materials
        targets: sel(PLAN_RESULT),
        cardOver: '#topPlaceHolder_newVersion',
        enter: watchClick(PLAN_RESULT),
        advance: function (ctx) { return !!ctx.flag && planResultOn(); }
      },
      { // 22 - the darker grey roof was merged into the road grey: pick it.
        // The plan is shown as uploaded again, where the darker grey shows.
        targets: roofAndPick,
        cardOver: '#topPlaceHolder_newVersion',
        enter: showOriginalPlan,
        advance: function () { return !!pickedRow(); }
      },
      { // 23 - the picked grey: artificial canopy (a roof)
        targets: function () { return rowWindow(pickedRow(), 'pickedRow'); },
        cardOver: '#topPlaceHolder_newVersion',
        advance: function () { return rowSetTo(pickedRow(), MAT_CANOPY_ARTIFICIAL); }
      },
      { // 24 - apply
        targets: sel(PLAN_APPLY),
        cardOver: '#topPlaceHolder_newVersion',
        enter: watchApply,
        advance: flagged
      },
      { // 25 - where the plan was placed, now painted
        targets: planArea,
        hold: modalOpen,
        delay: 600,
        look: true
      },
      { // 26 - confirm, on to step 5. Passed over while it is disabled (no
        // path network: Hitzeminderung entered from step 1), which ends the
        // tour on hint 25.
        targets: sel(NV_CONFIRM),
        skip: confirmShut,
        enter: watchClick(NV_CONFIRM),
        advance: flagged,
        pass: '#shiny-modal'
      }
    ]
  };

  // A hint of another tour, played with that tour's text.
  function borrowed(key, number) {
    var hint = { text: [key, number - 1] };
    Object.keys(TOURS[key][number - 1]).forEach(function (k) { hint[k] = TOURS[key][number - 1][k]; });
    return hint;
  }

  // Painting needs a scenario that is not the Original. The page seeds one
  // and selects it, so there usually is one; if not, the scenarios page's
  // hints 2-4 make, name and select one before the palette is used (after
  // hint 3, the materials on the map).
  TOURS.hitze.splice(3, 0, borrowed('newVersions', 2), borrowed('newVersions', 3),
                     borrowed('newVersions', 4));

  // a hint's number among its tour's own hints, which is its text's
  Object.keys(TOURS).forEach(function (key) {
    var n = 0;
    TOURS[key].forEach(function (hint) { if (!hint.text && !hint.alt) { hint.n = n++; } });
  });

  // The tours of one card, played between two others: no counter, not
  // recorded as played, and no help button offers them (no ring key is theirs)
  var BARE = { commit: true, saveLoad: true };

  // heat mitigation switched off (HEAT_MITIGATION, R/features.R): its button
  // in the nav bar is greyed by this class (vftNavBarServer())
  function heatOff() {
    var b = document.getElementById('vftNav_hitze');
    return !b || b.classList.contains('vft-nav-btn--off');
  }

  // Where a finished tour hands on to, when not simply to the tour of the step
  // the ring lands on: key of the finished tour -> function(ring key) returning
  // the tour to start, or null for none.
  var NEXT = {
    // back on step 5, the heat mitigation context is offered - unless it is
    // switched off (HEAT_MITIGATION in R/features.R: its button greyed), when
    // the tutorial simply ends there
    newVersions: function (key) {
      if (key !== 'step5') { return key; }
      return heatOff() || toursDone().hitze ? null : 'toHitze';
    },
    // heat mitigation ends on its confirm, which leads to step 5: its tour
    // only if it has not been played on this device yet
    hitze: function (key) { return toursDone()[key] ? null : key; }
  };

  function tourTexts(key) {
    var t = texts && texts.tours && texts.tours[key];
    if (t == null) { return []; }
    return Array.isArray(t) ? t : [t];
  }

  // the hint on screen: the data-loss warning's while it interrupts, else the
  // tour's own
  function cur() { return state.over || state.step; }

  // the text of the hint being shown, its variant's if it has one
  function hintText() {
    var step = cur();
    if (step.text) { return tourTexts(step.text[0])[step.text[1]] || ''; }
    if (step.alt) {
      var own = texts && texts.alts && texts.alts[state.key];
      return (own && own[step.alt]) || '';
    }
    var name = state.textAlt || state.variant;
    var alts = name && texts && texts.alts && texts.alts[state.key];
    var alt = alts && alts[(step.n + 1) + name];
    return alt || tourTexts(state.key)[step.n] || '';
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

    // the language card (`langTip`), shown only by the hints that have one
    var tip = document.createElement('div');
    tip.className = 'vftTutorialBox vftTutorialTip vftTutorialCentered';
    tip.setAttribute('aria-hidden', 'true');
    tip.innerHTML = '<div class="vftTutorialText">' + LANG_TIP + '</div>';
    root.appendChild(tip);

    document.body.appendChild(root);
    return { root: root, canvas: canvas, box: box, tip: tip };
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
    var step = cur();
    var t = texts || {};
    var text = hintText();

    state.box.classList.toggle('vftTutorialCentered', !!step.center);
    state.box.classList.toggle('vftTutorialWide', !!step.wide);
    state.box.innerHTML =
      '<div class="vftTutorialCount">' + countText() + '</div>' +
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

  // "3 / 17": the hints passed over are not counted, nor those ahead that
  // would be passed over as things stand
  function countText() {
    // a card of one hint, or the warning interrupting a hint, counts nothing
    if (state.over || BARE[state.key]) { return ''; }
    var list = hints(), at = 0, n = 0;
    for (var i = 0; i < list.length; i++) {
      var out = i < state.idx ? !!state.skipped[i] : i > state.idx && willSkip(list[i]);
      if (out) { continue; }
      n++;
      if (i <= state.idx) { at++; }
    }
    return at + ' / ' + n;
  }

  function willSkip(hint) {
    if (!hint.skip) { return false; }
    try { return !!hint.skip(); } catch (e) { return false; }
  }

  // the counter follows the page (a scenario made, or already there)
  function updateCount() {
    var el = state.box.querySelector('.vftTutorialCount');
    var t = countText();
    if (el && el.textContent !== t) { el.textContent = t; }
  }

  // Next: on at once, without the pause - there is nothing to see happen
  function onNext() {
    var ctx = state && state.ctx;
    if (!ctx || ctx.busy || state.pause || state.over) { return; }
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
      w.hitAt = g.hitAt || null;
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
  // when something changed. With no window open there is no dim at all: the
  // canvas fades out on its last picture (the windows of the hint just done)
  // and the layer stays, transparent, still swallowing the taps.
  function drawDim() {
    var c = state.canvas;
    var W = document.documentElement.clientWidth || window.innerWidth;
    var H = window.innerHeight;
    var dpr = window.devicePixelRatio || 1;
    var rects = state.wins.map(function (w) { return w.cur; });
    var lang = state.langWins;
    if (!rects.length) {
      if (state.dimSig !== 'off') {
        state.dimSig = 'off';
        c.classList.add('vftTutorialNoDim');
        c.setAttribute('data-windows', '[]');
      }
      return;
    }
    c.classList.remove('vftTutorialNoDim');
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
  // - or where its hitAt() says. A wheel over such a window zooms the map.
  function inWindow(x, y, el, type) {
    var step = cur();
    if (state.pause || (step.look && !step.live)) { return false; }
    return state.wins.some(function (w) {
      if (!contains(w.cur, x, y) && !contains(w.goal, x, y)) { return false; }
      if (w.hitAt) {
        if (type === 'wheel') { return true; }
        try { return !!w.hitAt(x, y, el); } catch (e) { return false; }
      }
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
    // beside them, level with their middle, when the screen has room on that
    // side - otherwise as any other card
    if (step.side === 'left' || step.side === 'right') {
      var at = step.sideOf ? elRect(document.querySelector(step.sideOf), PAD)
                           : (rects.length ? union(rects) : null);
      if (at) {
        var x = step.side === 'left' ? at.l - GAP - bw : at.r + GAP;
        var cy = (at.t + at.b) / 2;
        if (x >= vp.l && x + bw <= vp.r) {
          return { left: x, top: clamp(cy - bh / 2, vp.t, vp.b - bh), side: step.side, cx: 0, cy: cy };
        }
      }
    }
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
    // above or below that element rather than the windows, clear of them too
    var near = step.cardNear && elRect(document.querySelector(step.cardNear), PAD);
    if (step.below && rects.length) {
      cands.push({ side: 'below', a: union(rects) });
    } else if (near) {
      cands.push({ side: 'above', a: near }, { side: 'below', a: near });
    } else if (rects.length) {
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
    // A window too tall for the card to fit above or below it (a whole map):
    // the card goes to the top or the bottom of the screen, whichever covers
    // less of it, rather than into its middle
    if (step.hug && rects.length) {
      var all = union(rects);
      var hx = clamp((all.l + all.r) / 2 - bw / 2, vp.l, vp.r - bw);
      var best = null;
      [vp.t, vp.b - bh].forEach(function (top) {
        var br = { l: hx, t: top, r: hx + bw, b: top + bh };
        if ((extra || []).some(function (r) { return overlaps(br, r); })) { return; }
        var o = intersect(br, all);
        var cover = o ? (o.r - o.l) * (o.b - o.t) : 0;
        if (!best || cover < best.cover) {
          best = { cover: cover, pos: { left: hx, top: top, side: 'none', cx: 0 } };
        }
      });
      if (best) { return best.pos; }
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

  // The caret's tip, in the card's own pixels: the point the card pops out of
  function popOrigin(el, side, caret) {
    var size = parseFloat(getComputedStyle(el).getPropertyValue('--vftTut-caret-size')) || 0;
    var w = el.offsetWidth, h = el.offsetHeight;
    switch (side) {
      case 'above': return caret + 'px ' + (h + size) + 'px';
      case 'below': return caret + 'px ' + (-size) + 'px';
      case 'left':  return (w + size) + 'px ' + caret + 'px';
      case 'right': return (-size) + 'px ' + caret + 'px';
      default:      return '50% 50%';
    }
  }

  // A card beside its windows has its caret on a side edge, at the height of
  // their middle (cy); above or below them, on the top or bottom edge (cx).
  function applyPos(el, pos) {
    var sideways = pos.side === 'left' || pos.side === 'right';
    var len = sideways ? el.offsetHeight : el.offsetWidth;
    var caret = Math.round(sideways ? clamp(pos.cy - pos.top, 20, len - 20)
                                    : clamp(pos.cx - pos.left, 20, len - 20));
    var last = el._vftTutPos;
    if (last && last.side === pos.side && Math.abs(last.left - pos.left) < 2 &&
        Math.abs(last.top - pos.top) < 2 && Math.abs(last.caret - caret) < 2) { return; }
    el.style.left = Math.round(pos.left) + 'px';
    el.style.top = Math.round(pos.top) + 'px';
    el.setAttribute('data-side', pos.side);
    el.style.setProperty(sideways ? '--vftTut-caret-y' : '--vftTut-caret-x', caret + 'px');
    if (!last) {
      // the first placement of a card jumps, popping in out of its caret as
      // the windows do; later ones glide
      el.style.transformOrigin = popOrigin(el, pos.side, caret);
      el.classList.remove('vftTutorialPop');
      void el.offsetWidth;
      el.classList.add('vftTutorialPlaced', 'vftTutorialPop');
    }
    pos.caret = caret;
    el._vftTutPos = pos;
  }

  // the card jumps to its next place (and pops in) instead of gliding there
  function unplace(el) {
    el = el || state.box;
    el._vftTutPos = null;
    el.classList.remove('vftTutorialPlaced', 'vftTutorialPop');
  }

  function placeCard(goals, step, extra) {
    var vp = viewport();
    var bw = state.box.offsetWidth, bh = state.box.offsetHeight;
    if (!bw || !bh) { return; }
    // where it first appeared, however the windows change after that
    if (step.stay && state.box._vftTutPos) { return; }
    applyPos(state.box, cardPos(bw, bh, goals, step, vp, state.langWins.concat(extra || [])));
  }

  // The language card, left of the selector with its caret on it - not under
  // it, where the selector's dropdown opens; the area it covers, for the
  // hint's own card to keep clear of. Not while a modal is up - its backdrop
  // is over the bar.
  function placeTip() {
    var el = state.tip;
    var wrap = document.querySelector('#vftNav .vft-nav-lang');
    var r = !modalOpen() && shown(wrap) && elRect(wrap, 3);
    if (!r) { hideTip(); return null; }
    el.classList.add('vftTutorialTipOn');
    var vp = viewport();
    var w = el.offsetWidth, h = el.offsetHeight;
    var cy = (r.t + r.b) / 2;
    var pos = { left: Math.max(vp.l, r.l - GAP - w), top: clamp(cy - h / 2, vp.t, vp.b - h),
                side: 'left', cx: 0, cy: cy };
    applyPos(el, pos);
    return { l: pos.left, t: pos.top, r: r.l, b: pos.top + h };
  }

  function hideTip() {
    if (!state.tip.classList.contains('vftTutorialTipOn')) { return; }
    state.tip.classList.remove('vftTutorialTipOn');
    unplace(state.tip);
  }

  // nothing but the dim: between hints, while the map moves, and for a short
  // while as a hint waits for its target to appear
  function setQuiet(q) {
    if (q === state.quiet) { return; }
    state.quiet = q;
    state.root.classList.toggle('vftTutorialQuiet', q);
    if (q) { unplace(); hideTip(); }
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
    var step = cur();
    if (step.pass && !state.pause && closest(step.pass)) { return true; }
    // a click from the keyboard has no position: judge it by its element
    if (e.type === 'click' && e.detail === 0 && el && el.getBoundingClientRect) {
      var r = el.getBoundingClientRect();
      return inWindow((r.left + r.right) / 2, (r.top + r.bottom) / 2, el, e.type);
    }
    var p = eventPoint(e);
    return inWindow(p[0], p[1], el, e.type);
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
    if (cur().escPass) { return; }
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
    if (state.ctx) { state.ctx.off(); state.ctx = null; }
    // hints the page does not need are passed over
    var list = hints();
    while (idx < list.length && willSkip(list[idx])) { state.skipped[idx] = true; idx++; }
    if (idx >= list.length) { finish(); return; }
    state.idx = idx;
    state.stepStart = now;
    state.wins = [];
    state.seen = false;
    state.textAlt = null;

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

    state.base = list[idx];
    var hint = resolve(list[idx]);
    state.step = hint.step;
    state.variant = hint.variant;
    var step = state.step;
    setBodyClass(step.bodyClass || null);
    // a hint that may wait for its texts starts hidden
    if (step.hold) { setQuiet(true); }
    renderBox();
    if (step.enter) {
      try { step.enter(ctx); } catch (e) { /* a missing element must not stop the tour */ }
    }
  }

  function setBodyClass(cls) {
    if (cls === state.bodyClass) { return; }
    if (state.bodyClass) { document.body.classList.remove(state.bodyClass); }
    if (cls) { document.body.classList.add(cls); }
    state.bodyClass = cls;
  }

  // the hint is done: take everything away for a moment, so the user sees
  // what their tap did, then show the next one
  function pauseBefore(idx, now) {
    state.wins = [];
    setQuiet(true);
    drawDim();
    var next = hints()[idx];
    state.pause = { until: now + PAUSE + ((next && next.delay) || 0), next: idx };
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
    // the rest of the tour was passed over
    if (!state) { return; }

    // the data-loss warning has the screen while its modal is up
    if (interrupted(now)) { return; }

    // a hint held until the page is ready reads its variant off the page it
    // will be shown on, not the one it started on
    if (!state.seen && state.base && state.base.variant && state.base.hold) {
      var again = resolve(state.base);
      if (again.variant !== state.variant) {
        state.step = again.step;
        state.variant = again.variant;
        renderBox();
      }
    }

    var step = state.step;
    var gone = false;
    try { gone = !!(step.skipWhen && step.skipWhen(state.ctx)); } catch (e) { gone = false; }
    if (gone && !state.seen) {
      // never on screen: passed over, and not counted
      state.skipped[state.idx] = true;
      if (state.idx >= last || step.end) { finish(); return; }
      go(state.idx + 1, now);
      return;
    }
    var done = gone;
    try { done = done || !!(step.advance && step.advance(state.ctx)); } catch (e) { done = false; }
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
    // a moment of nothing after the warning, as between two hints
    var resting = now < state.resumeAt;
    var goals = !held && !moving && !resting ? raw : [];
    var quiet = held || moving || resting ||
                (!!step.targets && !goals.length && now - state.stepStart < WAIT_SHOW);

    var alt = null;
    try { alt = (step.textWhen && step.textWhen(state.ctx)) || null; } catch (e) { alt = null; }
    if (alt !== state.textAlt) { state.textAlt = alt; renderBox(); }

    setQuiet(quiet);
    updateWindows(goals, now);
    drawDim();
    state.box.classList.toggle('vftTutorialWaiting', !!step.targets && !goals.length);
    updateCount();
    if (quiet) { return; }
    state.seen = true;
    var tip = step.langTip ? placeTip() : (hideTip(), null);
    placeCard(goals, step, tip ? [tip] : []);
  }

  // The data-loss warning (vftAskCommit() in R/providers.R): its modal, as
  // one window whose every tap goes through (cancel or confirm).
  function commitModal() {
    var ok = document.getElementById('vftInvalidateOk');
    var content = ok && modalOpen() && ok.closest('.modal-content');
    var r = shown(content) && elRect(content, 4);
    return r ? [{ key: 'commit', rect: r, hit: [content] }] : [];
  }

  function commitUp() { return commitModal().length > 0; }

  // the warning as it interrupts a hint, with the text of the `commit` tour
  var COMMIT = { text: ['commit', 0], targets: commitModal, side: 'right', hug: true };

  // While the warning's modal is up, its card replaces the hint's, which is
  // neither advanced nor shown until the modal is gone (and a pause after).
  // True while it has the screen.
  function interrupted(now) {
    var up = state.key !== 'commit' && commitUp();
    if (up !== !!state.over) {
      state.over = up ? COMMIT : null;
      state.wins = [];
      if (!up) { state.resumeAt = now + PAUSE / 2; }
      renderBox();
    }
    if (!state.over) { return false; }
    var goals = [];
    try { goals = commitModal(); } catch (e) { goals = []; }
    setQuiet(!goals.length);
    updateWindows(goals, now);
    drawDim();
    state.box.classList.remove('vftTutorialWaiting');
    if (goals.length) {
      hideTip();
      placeCard(goals, COMMIT, []);
    }
    return true;
  }

  function start(key) {
    if (state || !TOURS[key]) { return; }
    chain = null;
    var ui = buildOverlay();
    state = {
      key: key, root: ui.root, canvas: ui.canvas, box: ui.box, tip: ui.tip,
      wins: [], langWins: [], data: {}, idx: 0, step: null, base: null, variant: null,
      ctx: null, seen: false, textAlt: null, over: null, resumeAt: 0, resume: null,
      skipped: {}, bodyClass: null, quiet: true, pause: null,
      moving: false, downOk: false, dimSig: null
    };
    readLook();
    listen(true);
    go(0, performance.now());
    if (state) { state.raf = window.requestAnimationFrame(frame); }
  }

  // A tour of one card played between two tours (the warning, save and
  // load): once it ends, the chain it interrupted carries on - `resume` is
  // that chain's {from, here, since}.
  function startAside(key, resume) {
    start(key);
    if (state && state.key === key) { state.resume = resume; }
  }

  function stop(status) {
    if (!state) { return; }
    if (status) { storeStatus(status); }
    if (state.ctx) { state.ctx.off(); }
    setBodyClass(null);
    window.cancelAnimationFrame(state.raf);
    listen(false);
    state.root.remove();
    state = null;
  }

  // The last hint is done. The tour hands on to wherever the user goes next.
  function finish() {
    var from = state.key;
    // the page it ends on: a tour that is no page's (toHitze) ends on another
    // page's, and must not start that page's tour
    var here = ringKey();
    var resume = state.resume;
    if (BARE[from]) {
      stop(null);
    } else {
      storeDone(from);
      stop('done');
    }
    // a card between two tours: the chain it interrupted carries on
    if (resume) { waitForNext(resume.from, resume.here, resume.since); return; }
    // The first tour finished after step 1's is followed by the save and load
    // card, once on this device, before the chain goes on
    if (!BARE[from] && from !== 'step1' && from !== 'toHitze' && !toursDone().saveLoad) {
      storeDone('saveLoad');
      startAside('saveLoad', { from: from, here: here, since: Date.now() });
      return;
    }
    waitForNext(from, here);
  }

  /* ---------------------------- chaining ---------------------------- */
  // A finished tour watches the nav bar's ring. When it lands on another
  // step, that step's tour starts - if it has one - once no modal is up (a
  // step can open on one) and the texts are in. The move can take a while:
  // entering a step may first derive its data behind a progress bar.

  function waitForNext(from, here, since) {
    var token = chain = { from: from, since: since || Date.now() };
    (function poll() {
      if (chain !== token || state) { return; }
      if (Date.now() - token.since > CHAIN_MAX) { chain = null; return; }
      // the data-loss warning, raised by the tap that ended the tour (a
      // confirm): its card, then the chain carries on
      if (texts && commitUp()) {
        startAside('commit', { from: from, here: here, since: token.since });
        return;
      }
      var key = ringKey();
      if (key && key !== from && key !== here) {
        var tour = NEXT[from] ? NEXT[from](key) : key;
        if (!tour || !TOURS[tour]) { chain = null; return; }
        if (!modalOpen() && texts) {
          chain = null;
          // a moment for the step's own page to settle
          setTimeout(function () { if (!state) { start(tour); } }, 600);
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
    // the hints with a text of their own
    Object.keys(TOURS).forEach(function (k) {
      out[k] = TOURS[k].filter(function (hint) { return !hint.text && !hint.alt; }).length;
    });
    return out;
  };
  // A card still popping in counts as quiet here: its box is scaled, so a
  // test measuring a button in it would aim at the wrong place
  function popping() {
    if (!state || !document.getAnimations) { return false; }
    return document.getAnimations().some(function (a) {
      return a.animationName === 'vftTutCardPop' && a.playState === 'running';
    });
  }
  window.vftTutorialState = function () {
    var lang = texts ? texts.lang : null;
    return state ? { key: state.key, idx: state.idx, variant: state.variant,
                     quiet: state.quiet || popping(),
                     pause: !!state.pause, choice: state.data.choice || null, lang: lang,
                     count: countText() }
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
