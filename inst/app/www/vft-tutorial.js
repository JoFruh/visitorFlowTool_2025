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

  // from the server: {lang, next_, stop, offer, start, tours: {step1: [...]}}
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
  function buttonRow(selector) {
    return function () {
      var found = [];
      Array.prototype.forEach.call(document.querySelectorAll(selector), function (el, i) {
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

  /* ------------------------------ tours ----------------------------- */
  // targets: the windows. advance(ctx): polled every frame, true moves on.
  // enter(ctx): runs as the hint starts; ctx.on()/ctx.jq() listeners go with
  //   the hint.
  // look: a hint to read - a Next button moves on, the windows take no taps.
  //   onNext(ctx, done): runs before it moves on.
  // pass: a selector whose taps go through all the same.
  // escPass: Escape is left to the page (polydraw.js takes a vertex back
  //   with it) instead of stopping the tour.
  // avoid: selectors the card must not cover. cardOver: a selector the card
  //   sits on instead of beside the windows. wide: a wider card, for a hint
  //   with little height to spare.
  // hold(ctx): true keeps the hint hidden. center: the text is centred.
  // Texts are texts.tours[<key>][<index>].

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
    ]
  };

  function tourTexts(key) {
    var t = texts && texts.tours && texts.tours[key];
    if (t == null) { return []; }
    return Array.isArray(t) ? t : [t];
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
    var step = hints()[state.idx];
    var t = texts || {};
    var text = tourTexts(state.key)[state.idx] || '';
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
    var step = hints()[state.idx];
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

  // Taps go through a window, but not in a hint that is only to read, and
  // only onto the window's own target (el: the element tapped)
  function inWindow(x, y, el) {
    if (state.pause || hints()[state.idx].look) { return false; }
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
    var step = hints()[state.idx];
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
    if (hints()[state.idx].escPass) { return; }
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

    var step = hints()[idx];
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
      if (state.idx >= last) { finish(); return; }
      go(state.idx + 1, now);
    }

    var step = hints()[state.idx];
    var done = false;
    try { done = !!(step.advance && step.advance(state.ctx)); } catch (e) { done = false; }
    if (done) {
      if (state.ctx) { state.ctx.off(); }
      if (state.idx >= last) { finish(); return; }
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
      wins: [], langWins: [], data: {}, idx: 0, ctx: null, quiet: true, pause: null,
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
    return state ? { key: state.key, idx: state.idx, quiet: state.quiet,
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
