/* Plan import for the heat-mitigation map (newVersions, context 4).
 *
 * A user uploads a design plan (PDF, PNG, JPG or TIFF) and it becomes paint:
 *
 *   1. PLACE. A plan carries no coordinates, so it floats, half transparent, at
 *      the centre of the map. The user pans and zooms the map underneath it
 *      until the two agree, adjusting only the plan's size. Plans are expected
 *      north-up, so there is no rotation. "Next" reads the plan's position off
 *      the map and fits image pixels -> LV95.
 *   2. MAP COLOURS. A card over the map shows the plan opaque, next to its main
 *      colours. Each colour gets a material, suggested automatically and
 *      correctable by hand; clicking the plan finds a pixel's colour row.
 *   3. APPLY. Every paint cell under the plan takes the material of the plan
 *      pixel at its centre, and the result is written into the paint grids.
 *
 * All of that runs here, not in R: the deployment is one R process shared by
 * every session, and nothing about an uploaded image needs R. R receives only
 * the result, as one class-id PNG per level (see the paintImport observer).
 *
 * The plan replaces its footprint. A ground material also clears the canopy
 * above it (the canopy_cleared hole id), so a plan showing lawn where a tree
 * stands today removes the tree. A tree in the plan sets the canopy and leaves
 * the ground to the baseline, since a plan does not say what is under a crown.
 *
 * Everything the brush owns is reached through window.__vftPaintHooks
 * (paintbrush.js), which must be loaded first.
 */
(function () {
  "use strict";

  var NS       = "newVersions-";
  var VENDOR   = "www/vendor/";
  var MAX_SIDE = 4096;       //working resolution cap, px on the longer side
  var SAMPLE   = 200000;     //pixels sampled for the palette
  var K        = 12;         //initial palette size, before merging
  var MERGE_DE = 8;          //centres closer than this (CIE76) are one colour
  var MIN_SHARE = 0.005;     //colours covering less than this are dropped
  var ACK_TIMEOUT = 20000;

  //German fallbacks, used until R sends the translated set
  var L_ = {
    maxCells: 4e6,
    place: "Plan platzieren",
    placeHint: "Verschieben und zoomen Sie die Karte, bis der Plan passt. Der Plan muss nach Norden ausgerichtet sein.",
    size: "Grösse", page: "Seite", nextStep: "Weiter", back: "Zurück",
    apply: "Anwenden", cancel: "Abbrechen", mapColors: "Farben zuordnen",
    original: "Original", assigned: "Zuordnung", smooth: "Glätten",
    readError: "Die Datei konnte nicht gelesen werden.",
    outside: "Der Plan liegt ausserhalb des Untersuchungsgebiets.",
    tooLarge: "Der Plan ist zu gross.",
    saveError: "Der Plan konnte nicht gespeichert werden.",
    materials: { "0": "Ignorieren", "7": "Baum", "6": "Kuenstliche Krone", "1": "Gras",
                 "2": "Busch", "3": "Kuenstlich", "4": "Natuerlich", "5": "Wasser",
                 "8": "Kuenstlicher Block" }
  };
  var MATERIAL_ORDER = ["0", "7", "6", "1", "2", "3", "4", "5", "8"];

  /* Reference colours for the automatic suggestion: each plan colour takes the
   * material of its nearest reference. Several per material, because plans
   * differ in how light a lawn or how dark a tree is drawn. 0 = ignore, which
   * is where paper white and the usual annotation colours go. */
  var REFERENCES = [
    [7, "#1f5a2a"], [7, "#2e6b34"], [7, "#3a7d44"], [7, "#14532d"],
    [2, "#6aa84f"], [2, "#7fb069"], [2, "#5e8c3a"],
    [1, "#90ee90"], [1, "#a8d08d"], [1, "#b5e0a0"], [1, "#c8e6b0"], [1, "#b8d86b"],
    [4, "#a05a3c"], [4, "#8b5a2b"], [4, "#c49a6c"], [4, "#d2b48c"], [4, "#e0c9a6"],
    [5, "#1e90ff"], [5, "#5b9bd5"], [5, "#87ceeb"], [5, "#a6cee3"], [5, "#2b5fa8"],
    [3, "#808080"], [3, "#a0a0a0"], [3, "#c0c0c0"], [3, "#d3d3d3"],
    [8, "#000000"], [8, "#1f1f1f"], [8, "#404040"],
    [0, "#ffffff"], [0, "#f5f5f5"], [0, "#ff0000"], [0, "#ffff00"],
    [0, "#ff00ff"], [0, "#ffa500"], [0, "#e31a1c"]
  ];

  var H = null;              //paintbrush.js hooks
  var labels = L_;
  var imp = null;            //the import in progress, or null
  var seq = 0;
  var inflight = new Map();  //seq -> {payload, timer, tries}

  // ── Small helpers ───────────────────────────────────────────────────────────

  function el(tag, attrs, children) {
    var e = document.createElement(tag);
    if (attrs) Object.keys(attrs).forEach(function (k) {
      if (k === "style") e.style.cssText = attrs[k];
      else if (k === "text") e.textContent = attrs[k];
      else if (k.slice(0, 2) === "on") e.addEventListener(k.slice(2), attrs[k]);
      else e.setAttribute(k, attrs[k]);
    });
    (children || []).forEach(function (c) { if (c) e.appendChild(c); });
    return e;
  }

  function button(text, onclick, primary) {
    return el("button", {
      type: "button", "class": "btn " + (primary ? "btn-success" : "btn-default"),
      text: text, onclick: onclick
    });
  }

  var scriptPromises = {};
  function loadScript(src) {
    if (!scriptPromises[src]) {
      scriptPromises[src] = new Promise(function (resolve, reject) {
        var s = document.createElement("script");
        s.src = src;
        s.onload = resolve;
        s.onerror = function () { delete scriptPromises[src]; reject(new Error("could not load " + src)); };
        document.head.appendChild(s);
      });
    }
    return scriptPromises[src];
  }

  function cssColorToRgb(color) {
    var cv = document.createElement("canvas");
    cv.width = cv.height = 1;
    var cx = cv.getContext("2d");
    cx.fillStyle = color;
    cx.fillRect(0, 0, 1, 1);
    var d = cx.getImageData(0, 0, 1, 1).data;
    return [d[0], d[1], d[2]];
  }

  function hex(rgb) {
    return "#" + rgb.map(function (v) { return ("0" + Math.round(v).toString(16)).slice(-2); }).join("");
  }

  /* sRGB -> CIE Lab (D65), for colour distances that match what the eye sees. */
  function lin(c) { c /= 255; return c <= 0.04045 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4); }
  function f3(t) { return t > 0.008856 ? Math.cbrt(t) : 7.787 * t + 16 / 116; }
  function lab(r, g, b) {
    var R = lin(r), G = lin(g), B = lin(b);
    var x = (0.4124 * R + 0.3576 * G + 0.1805 * B) / 0.95047;
    var y =  0.2126 * R + 0.7152 * G + 0.0722 * B;
    var z = (0.0193 * R + 0.1192 * G + 0.9505 * B) / 1.08883;
    var fx = f3(x), fy = f3(y), fz = f3(z);
    return [116 * fy - 16, 500 * (fx - fy), 200 * (fy - fz)];
  }
  function de2(a, b) {
    var d0 = a[0] - b[0], d1 = a[1] - b[1], d2 = a[2] - b[2];
    return d0 * d0 + d1 * d1 + d2 * d2;
  }

  //deterministic, so the same plan always gives the same palette
  function rng(seed) {
    return function () {
      seed |= 0; seed = seed + 0x6D2B79F5 | 0;
      var t = Math.imul(seed ^ seed >>> 15, 1 | seed);
      t = t + Math.imul(t ^ t >>> 7, 61 | t) ^ t;
      return ((t ^ t >>> 14) >>> 0) / 4294967296;
    };
  }

  // ── Decoding ────────────────────────────────────────────────────────────────

  /* Draw a source onto a white working canvas no larger than MAX_SIDE. White,
   * because transparent regions of a PNG or PDF are paper, not a colour. */
  function toWorkingCanvas(src, w, h) {
    var s  = Math.min(1, MAX_SIDE / Math.max(w, h));
    var cv = document.createElement("canvas");
    cv.width  = Math.max(1, Math.round(w * s));
    cv.height = Math.max(1, Math.round(h * s));
    var cx = cv.getContext("2d");
    cx.fillStyle = "#ffffff";
    cx.fillRect(0, 0, cv.width, cv.height);
    cx.imageSmoothingEnabled = true;
    cx.drawImage(src, 0, 0, cv.width, cv.height);
    return cv;
  }

  /* Returns {pages, render(n) -> Promise<canvas>} for any supported file. */
  function openFile(file) {
    var name = (file.name || "").toLowerCase();
    var type = file.type || "";

    if (type === "application/pdf" || /\.pdf$/.test(name)) {
      return loadScript(VENDOR + "pdfjs/pdf.min.js").then(function () {
        var lib = window.pdfjsLib || window["pdfjs-dist/build/pdf"];
        lib.GlobalWorkerOptions.workerSrc = VENDOR + "pdfjs/pdf.worker.min.js";
        return file.arrayBuffer().then(function (buf) {
          return lib.getDocument({ data: new Uint8Array(buf) }).promise;
        });
      }).then(function (doc) {
        return {
          pages: doc.numPages,
          render: function (n) {
            return doc.getPage(n).then(function (page) {
              var v0 = page.getViewport({ scale: 1 });
              var scale = MAX_SIDE / Math.max(v0.width, v0.height);
              var vp = page.getViewport({ scale: scale });
              var cv = document.createElement("canvas");
              cv.width = Math.round(vp.width); cv.height = Math.round(vp.height);
              var cx = cv.getContext("2d");
              cx.fillStyle = "#ffffff";
              cx.fillRect(0, 0, cv.width, cv.height);
              return page.render({ canvasContext: cx, viewport: vp }).promise
                .then(function () { return cv; });
            });
          }
        };
      });
    }

    if (/^image\/tiff?$/.test(type) || /\.tiff?$/.test(name)) {
      return loadScript(VENDOR + "pako/pako_inflate.min.js")
        .then(function () { return loadScript(VENDOR + "utif/UTIF.js"); })
        .then(function () { return file.arrayBuffer(); })
        .then(function (buf) {
          var U = window.UTIF;
          //only full images: UTIF also lists thumbnails and masks as IFDs
          var ifds = U.decode(buf).filter(function (d) {
            var sub = d.t254 ? d.t254[0] : 0;
            return !(sub & 5);
          });
          if (!ifds.length) throw new Error("no image in TIFF");
          return {
            pages: ifds.length,
            render: function (n) {
              var ifd = ifds[n - 1];
              U.decodeImage(buf, ifd);
              var rgba = U.toRGBA8(ifd);
              var raw = document.createElement("canvas");
              raw.width = ifd.width; raw.height = ifd.height;
              raw.getContext("2d").putImageData(
                new ImageData(new Uint8ClampedArray(rgba.buffer, rgba.byteOffset, rgba.length),
                              ifd.width, ifd.height), 0, 0);
              return Promise.resolve(toWorkingCanvas(raw, ifd.width, ifd.height));
            }
          };
        });
    }

    return new Promise(function (resolve, reject) {
      var url = URL.createObjectURL(file);
      var img = new Image();
      img.onload = function () {
        URL.revokeObjectURL(url);
        var cv = toWorkingCanvas(img, img.naturalWidth, img.naturalHeight);
        resolve({ pages: 1, render: function () { return Promise.resolve(cv); } });
      };
      img.onerror = function () { URL.revokeObjectURL(url); reject(new Error("not an image")); };
      img.src = url;
    });
  }

  // ── Palette ─────────────────────────────────────────────────────────────────

  /* Main colours of the plan: k-means in Lab over a sample, then near-identical
   * centres merged and rare ones dropped. Plans are flat colour fills, so this
   * lands on the fills; anti-aliased edges and labels are what the share
   * threshold and the smoothing step are there for. */
  function extractPalette(cv) {
    var w = cv.width, h = cv.height;
    var data = cv.getContext("2d").getImageData(0, 0, w, h).data;
    var n = w * h;
    var step = Math.max(1, Math.floor(n / SAMPLE));
    var pts = [];
    for (var p = 0; p < n; p += step) {
      var o = p * 4;
      pts.push(lab(data[o], data[o + 1], data[o + 2]));
    }

    //k-means++ seeding
    var rand = rng(1);
    var centres = [pts[Math.floor(rand() * pts.length)]];
    var d = new Float64Array(pts.length);
    while (centres.length < K) {
      var sum = 0;
      for (var i = 0; i < pts.length; i++) {
        var best = Infinity;
        for (var c = 0; c < centres.length; c++) best = Math.min(best, de2(pts[i], centres[c]));
        d[i] = best; sum += best;
      }
      if (sum === 0) break;
      var r = rand() * sum, acc = 0, pick = pts.length - 1;
      for (i = 0; i < pts.length; i++) { acc += d[i]; if (acc >= r) { pick = i; break; } }
      centres.push(pts[pick]);
    }

    var assign = new Int32Array(pts.length);
    for (var it = 0; it < 15; it++) {
      var sums = centres.map(function () { return [0, 0, 0, 0]; });
      for (i = 0; i < pts.length; i++) {
        var bi = 0, bd = Infinity;
        for (c = 0; c < centres.length; c++) {
          var dd = de2(pts[i], centres[c]);
          if (dd < bd) { bd = dd; bi = c; }
        }
        assign[i] = bi;
        var s = sums[bi];
        s[0] += pts[i][0]; s[1] += pts[i][1]; s[2] += pts[i][2]; s[3]++;
      }
      centres = centres.map(function (ct, k) {
        var s = sums[k];
        return s[3] ? [s[0] / s[3], s[1] / s[3], s[2] / s[3]] : ct;
      });
    }
    var counts = centres.map(function () { return 0; });
    for (i = 0; i < pts.length; i++) counts[assign[i]]++;

    //merge close centres, heaviest first
    var items = centres.map(function (ct, k) { return { lab: ct, n: counts[k] }; })
                       .filter(function (x) { return x.n > 0; })
                       .sort(function (a, b) { return b.n - a.n; });
    var merged = [];
    items.forEach(function (x) {
      var into = null;
      for (var m = 0; m < merged.length; m++) {
        if (de2(merged[m].lab, x.lab) < MERGE_DE * MERGE_DE) { into = merged[m]; break; }
      }
      if (into) {
        var t = into.n + x.n;
        into.lab = [0, 1, 2].map(function (j) { return (into.lab[j] * into.n + x.lab[j] * x.n) / t; });
        into.n = t;
      } else {
        merged.push({ lab: x.lab.slice(), n: x.n });
      }
    });
    var total = pts.length;
    var kept = merged.filter(function (x) { return x.n / total >= MIN_SHARE; });
    if (!kept.length) kept = merged.slice(0, 1);

    //per-pixel nearest centre, through a 15-bit RGB lookup table: 32k Lab
    //distances instead of one per pixel of a 16 MP image
    var lut = new Uint8Array(32768);
    for (var q = 0; q < 32768; q++) {
      var lq = lab(((q >> 10) & 31) * 255 / 31, ((q >> 5) & 31) * 255 / 31, (q & 31) * 255 / 31);
      var bk = 0, bdist = Infinity;
      for (c = 0; c < kept.length; c++) {
        var dq = de2(lq, kept[c].lab);
        if (dq < bdist) { bdist = dq; bk = c; }
      }
      lut[q] = bk;
    }
    var labels_ = new Uint8Array(n);
    var sums2 = kept.map(function () { return [0, 0, 0]; });
    var cnt2  = kept.map(function () { return 0; });
    for (p = 0; p < n; p++) {
      o = p * 4;
      var k = lut[((data[o] >> 3) << 10) | ((data[o + 1] >> 3) << 5) | (data[o + 2] >> 3)];
      labels_[p] = k;
      cnt2[k]++;
      sums2[k][0] += data[o]; sums2[k][1] += data[o + 1]; sums2[k][2] += data[o + 2];
    }

    //The swatch is the mean of the pixels actually assigned, which is the colour
    //the user will recognise from the plan. Colours that won no pixel at full
    //resolution are dropped here rather than listed at 0 %.
    var palette = [], remap = new Uint8Array(kept.length);
    kept.forEach(function (x, k2) {
      if (!cnt2[k2]) return;
      remap[k2] = palette.length;
      palette.push({
        rgb: [sums2[k2][0] / cnt2[k2], sums2[k2][1] / cnt2[k2], sums2[k2][2] / cnt2[k2]],
        share: cnt2[k2] / n,
        material: suggest(x.lab)
      });
    });
    for (p = 0; p < n; p++) labels_[p] = remap[labels_[p]];
    return { palette: palette, labels: labels_ };
  }

  var refLabs = null;
  function suggest(labc) {
    if (!refLabs) refLabs = REFERENCES.map(function (r) {
      var c = cssColorToRgb(r[1]);
      return { id: r[0], lab: lab(c[0], c[1], c[2]) };
    });
    var best = 0, bd = Infinity;
    refLabs.forEach(function (r) {
      var d = de2(r.lab, labc);
      if (d < bd) { bd = d; best = r.id; }
    });
    return String(best);
  }

  // ── Classification ──────────────────────────────────────────────────────────

  /* Plan pixels -> material ids, optionally with a 3x3 majority filter. The
   * filter runs on materials rather than palette entries, so two colours that
   * both mean "grass" do not fight over a boundary. */
  function materialMap() {
    var P = imp.pal, W = imp.canvas.width, Hh = imp.canvas.height;
    var lut = new Uint8Array(P.palette.length);
    P.palette.forEach(function (p, k) { lut[k] = +p.material; });
    var mat = new Uint8Array(W * Hh);
    for (var i = 0; i < mat.length; i++) mat[i] = lut[P.labels[i]];
    if (!imp.smooth) return mat;

    var out = new Uint8Array(mat.length);
    var cnt = new Uint16Array(16);
    for (var y = 0; y < Hh; y++) {
      for (var x = 0; x < W; x++) {
        var own = mat[y * W + x], bestN = 0, best = own;
        for (var dy = -1; dy <= 1; dy++) {
          var yy = y + dy;
          if (yy < 0 || yy >= Hh) continue;
          for (var dx = -1; dx <= 1; dx++) {
            var xx = x + dx;
            if (xx < 0 || xx >= W) continue;
            var v = mat[yy * W + xx];
            var c = ++cnt[v];
            if (c > bestN || (c === bestN && v === own)) { bestN = c; best = v; }
          }
        }
        //reset only what was touched
        for (dy = -1; dy <= 1; dy++) {
          yy = y + dy;
          if (yy < 0 || yy >= Hh) continue;
          for (dx = -1; dx <= 1; dx++) {
            xx = x + dx;
            if (xx >= 0 && xx < W) cnt[mat[yy * W + xx]] = 0;
          }
        }
        out[y * W + x] = best;
      }
    }
    return out;
  }

  /* Materials -> the two level arrays over the footprint window. */
  function classifyCells() {
    var mat = materialMap();
    imp.mat = mat;
    var f = imp.foot, A = imp.fit, res = imp.res;
    var W = imp.canvas.width, Hh = imp.canvas.height;
    var levels = H.levels();
    var hole = H.holeIds()[0] || 0;
    var ground = new Uint8Array(f.w * f.h);
    var canopy = new Uint8Array(f.w * f.h);

    for (var j = 0; j < f.h; j++) {
      var N = (f.rowTop - j + 0.5) * res;
      for (var i = 0; i < f.w; i++) {
        var E = (f.col0 + i + 0.5) * res;
        var dE = E - A.e0, dN = N - A.n0;
        var px = A.i11 * dE + A.i12 * dN;
        var py = A.i21 * dE + A.i22 * dN;
        if (px < 0 || py < 0 || px >= W || py >= Hh) continue;
        var id = mat[Math.floor(py) * W + Math.floor(px)];
        if (!id) continue;
        var lv = levels[id], k = j * f.w + i;
        if (lv === "ground")      { ground[k] = id; canopy[k] = hole; }
        else if (lv === "canopy") { canopy[k] = id; }
        else if (lv === "both")   { ground[k] = id; canopy[k] = id; }
      }
    }
    imp.ground = ground;
    imp.canopy = canopy;
    H.preview("ground", ground, f.w, f.h, f.col0, f.rowTop);
    H.preview("canopy", canopy, f.w, f.h, f.col0, f.rowTop);
  }

  var classifyTimer = null;
  function scheduleClassify() {
    if (classifyTimer) clearTimeout(classifyTimer);
    classifyTimer = setTimeout(function () {
      classifyTimer = null;
      if (!imp || imp.stage !== "map") return;
      classifyCells();
      drawView();
    }, 150);
  }

  // ── Placement ───────────────────────────────────────────────────────────────

  function mapContainer() { return H.map().getContainer(); }

  function showPaintButtons(show) {
    var btns = document.getElementById(NS + "paintColorButtonsDiv");
    if (!btns) return;
    if (!show) {
      imp.btnDisplay = btns.style.display;
      btns.style.display = "none";
    } else if (imp && imp.btnDisplay !== undefined) {
      btns.style.display = imp.btnDisplay;
    }
  }

  function panel() { return document.getElementById(NS + "planImportPanel"); }

  function enterPlace() {
    imp.stage = "place";
    closeCard();
    H.preview("ground", null); H.preview("canopy", null);

    var cont = mapContainer();
    if (!imp.size) imp.size = Math.round(0.7 * Math.min(cont.clientWidth,
                               cont.clientHeight * imp.canvas.width / imp.canvas.height));
    var fl = imp.floating = imp.canvas;
    fl.className = "vft-plan-floating";
    fl.style.width = imp.size + "px";
    cont.appendChild(fl);

    var slider = el("input", { type: "range", min: 40, max: Math.round(4 * cont.clientWidth),
                               value: imp.size, step: 1, style: "width:260px;",
                               oninput: function () { setSize(+slider.value); } });
    function setSize(v) {
      imp.size = Math.max(10, Math.round(v));
      fl.style.width = imp.size + "px";
      slider.value = imp.size;
    }
    var minus = button("−", function () { setSize(imp.size / 1.02); });
    var plus  = button("+",      function () { setSize(imp.size * 1.02); });

    var pageSel = null;
    if (imp.doc.pages > 1) {
      pageSel = el("select", { "class": "form-control", style: "width:auto;display:inline-block;",
                               onchange: function () { loadPage(+pageSel.value); } });
      for (var n = 1; n <= imp.doc.pages; n++) {
        var o = el("option", { value: n, text: String(n) });
        if (n === imp.page) o.selected = true;
        pageSel.appendChild(o);
      }
    }

    var p = panel();
    p.innerHTML = "";
    p.appendChild(el("div", { "class": "vft-plan-panel" }, [
      el("strong", { text: labels.place }),
      el("div", { "class": "vft-plan-hint", text: labels.placeHint }),
      el("div", { "class": "vft-plan-row" }, [
        pageSel ? el("span", { text: labels.page + " " }) : null, pageSel,
        el("span", { text: labels.size + " " }), minus, slider, plus
      ]),
      el("div", { "class": "vft-plan-row" }, [
        button(labels.cancel, cancel), button(labels.nextStep, fromPlace, true)
      ])
    ]));
    p.style.display = "";
  }

  function loadPage(n) {
    imp.doc.render(n).then(function (cv) {
      if (!imp) return;
      var old = imp.canvas;
      imp.page = n;
      imp.canvas = cv;
      imp.pal = null;
      if (imp.stage === "place" && old.parentNode) {
        cv.className = old.className;
        cv.style.width = imp.size + "px";
        old.parentNode.replaceChild(cv, old);
        imp.floating = cv;
      }
    }).catch(function () { alert(labels.readError); });
  }

  /* Read the plan's position off the map: image pixel (x, y) -> LV95 (E, N),
   * as a least-squares affine over a grid of points on the plan. Every point
   * goes through the exact path a brush stroke does, so an applied plan lands
   * where a hand-painted copy of it would. */
  function fitPlacement() {
    var map = H.map(), cont = mapContainer();
    var cw = cont.clientWidth, ch = cont.clientHeight;
    var W = imp.canvas.width, Hh = imp.canvas.height;
    var dw = imp.size, dh = imp.size * Hh / W;
    var left = cw / 2 - dw / 2, top = ch / 2 - dh / 2;

    //normal equations for E = a0 + a1 x + a2 y and likewise N
    var M = [[0, 0, 0], [0, 0, 0], [0, 0, 0]], vE = [0, 0, 0], vN = [0, 0, 0];
    var corners = [];
    for (var gy = 0; gy <= 4; gy++) {
      for (var gx = 0; gx <= 4; gx++) {
        var x = W * gx / 4, y = Hh * gy / 4;
        var ll = map.containerPointToLatLng(L.point(left + dw * gx / 4, top + dh * gy / 4));
        var m  = L.CRS.EPSG3857.project(ll);
        var p  = H.mercatorToLV95(m.x, m.y);
        if ((gx === 0 || gx === 4) && (gy === 0 || gy === 4)) corners.push(p);
        var b = [1, x, y];
        for (var r = 0; r < 3; r++) {
          vE[r] += b[r] * p.E; vN[r] += b[r] * p.N;
          for (var c = 0; c < 3; c++) M[r][c] += b[r] * b[c];
        }
      }
    }
    var a = solve3(M, vE), bN = solve3(M, vN);
    //inverse of the linear part, about the origin pixel
    var det = a[1] * bN[2] - a[2] * bN[1];
    return {
      fit: { e0: a[0], n0: bN[0],
             i11:  bN[2] / det, i12: -a[2] / det,
             i21: -bN[1] / det, i22:  a[1] / det },
      corners: corners
    };
  }

  function solve3(M, v) {
    var A = M.map(function (row, i) { return row.concat([v[i]]); });
    for (var c = 0; c < 3; c++) {
      var piv = c;
      for (var r = c + 1; r < 3; r++) if (Math.abs(A[r][c]) > Math.abs(A[piv][c])) piv = r;
      var t = A[c]; A[c] = A[piv]; A[piv] = t;
      for (r = 0; r < 3; r++) {
        if (r === c) continue;
        var f = A[r][c] / A[c][c];
        for (var k = c; k < 4; k++) A[r][k] -= f * A[c][k];
      }
    }
    return [A[0][3] / A[0][0], A[1][3] / A[1][1], A[2][3] / A[2][2]];
  }

  function fromPlace() {
    var fp  = fitPlacement();
    var res = imp.res = H.res();
    var Es = fp.corners.map(function (p) { return p.E; });
    var Ns = fp.corners.map(function (p) { return p.N; });
    var c0 = Math.floor(Math.min.apply(null, Es) / res);
    var c1 = Math.ceil(Math.max.apply(null, Es) / res) - 1;
    var r0 = Math.floor(Math.min.apply(null, Ns) / res);
    var r1 = Math.ceil(Math.max.apply(null, Ns) / res) - 1;

    //clip to the study area's window: the land cover, and so the heat model,
    //ends there
    var win = H.window();
    if (win) {
      c0 = Math.max(c0, win.col0);
      c1 = Math.min(c1, win.col0 + win.w - 1);
      r1 = Math.min(r1, win.rowTop);
      r0 = Math.max(r0, win.rowTop - win.h + 1);
    }
    if (c1 < c0 || r1 < r0) { alert(labels.outside); return; }
    var foot = { col0: c0, rowTop: r1, w: c1 - c0 + 1, h: r1 - r0 + 1 };
    if (foot.w * foot.h > labels.maxCells) { alert(labels.tooLarge); return; }

    imp.fit  = fp.fit;
    imp.foot = foot;
    if (imp.floating && imp.floating.parentNode) imp.floating.parentNode.removeChild(imp.floating);
    imp.floating = null;
    panel().style.display = "none";

    if (!imp.pal) imp.pal = extractPalette(imp.canvas);
    enterMap();
  }

  // ── Colour mapping card ─────────────────────────────────────────────────────

  function closeCard() {
    if (imp && imp.card && imp.card.parentNode) imp.card.parentNode.removeChild(imp.card);
    if (imp) imp.card = null;
  }

  function enterMap() {
    imp.stage = "map";
    closeCard();
    if (imp.smooth === undefined) imp.smooth = true;
    if (!imp.view) imp.view = "original";

    var cont = mapContainer();
    var viewCv = el("canvas", { "class": "vft-plan-view" });
    var rows = el("div", { "class": "vft-plan-rows" });
    imp.viewCv = viewCv;
    imp.rowEls = [];

    imp.pal.palette.forEach(function (p, k) {
      var sel = el("select", { "class": "form-control input-sm",
                               onchange: function () { p.material = sel.value; scheduleClassify(); } });
      MATERIAL_ORDER.forEach(function (id) {
        var o = el("option", { value: id, text: labels.materials[id] || id });
        if (id === p.material) o.selected = true;
        sel.appendChild(o);
      });
      var row = el("div", { "class": "vft-plan-colrow",
                            onmouseenter: function () { imp.hover = k; drawView(); },
                            onmouseleave: function () { imp.hover = null; drawView(); } }, [
        el("span", { "class": "vft-plan-swatch", style: "background:" + hex(p.rgb) + ";" }),
        el("span", { "class": "vft-plan-share", text: (100 * p.share).toFixed(1) + " %" }),
        sel
      ]);
      imp.rowEls.push(row);
      rows.appendChild(row);
    });

    var tOrig = button(labels.original, function () { imp.view = "original"; syncToggle(); drawView(); });
    var tAss  = button(labels.assigned, function () { imp.view = "assigned"; syncToggle(); drawView(); });
    function syncToggle() {
      tOrig.classList.toggle("active", imp.view === "original");
      tAss.classList.toggle("active", imp.view === "assigned");
    }
    syncToggle();

    var smooth = el("input", { type: "checkbox",
                               onchange: function () { imp.smooth = smooth.checked; scheduleClassify(); } });
    smooth.checked = imp.smooth;

    viewCv.addEventListener("click", function (e) {
      var r = viewCv.getBoundingClientRect();
      var x = Math.floor((e.clientX - r.left) / r.width * imp.canvas.width);
      var y = Math.floor((e.clientY - r.top) / r.height * imp.canvas.height);
      if (x < 0 || y < 0 || x >= imp.canvas.width || y >= imp.canvas.height) return;
      var k = imp.pal.labels[y * imp.canvas.width + x];
      var row = imp.rowEls[k];
      if (!row) return;
      row.scrollIntoView({ block: "nearest" });
      row.classList.remove("vft-plan-flash");
      void row.offsetWidth;
      row.classList.add("vft-plan-flash");
    });

    var card = imp.card = el("div", { "class": "vft-plan-card" }, [
      el("div", { "class": "vft-plan-head" }, [
        el("strong", { text: labels.mapColors }),
        el("span", { "class": "btn-group", style: "margin-left:auto;" }, [tOrig, tAss])
      ]),
      el("div", { "class": "vft-plan-body" }, [
        el("div", { "class": "vft-plan-left" }, [viewCv]),
        el("div", { "class": "vft-plan-right" }, [
          rows,
          el("label", { "class": "vft-plan-smooth" }, [smooth, el("span", { text: " " + labels.smooth })]),
          el("div", { "class": "vft-plan-row" }, [
            button(labels.back, enterPlace),
            button(labels.cancel, cancel),
            button(labels.apply, apply, true)
          ])
        ])
      ])
    ]);
    //the card is inside the map container, so keep Leaflet from treating
    //clicks, drags and wheel on it as map gestures
    if (window.L && L.DomEvent) {
      L.DomEvent.disableClickPropagation(card);
      L.DomEvent.disableScrollPropagation(card);
    }
    ["pointerdown", "mousedown", "touchstart", "dblclick"].forEach(function (t) {
      card.addEventListener(t, function (e) { e.stopPropagation(); });
    });
    cont.appendChild(card);

    sizeView();
    classifyCells();
    drawView();
  }

  function sizeView() {
    var box = imp.viewCv.parentNode.getBoundingClientRect();
    var W = imp.canvas.width, Hh = imp.canvas.height;
    //fit the plan to the pane, scaling a small plan up as well as a large one down
    var s = Math.min((box.width - 4) / W, (box.height - 4) / Hh);
    imp.viewCv.width  = Math.max(1, Math.floor(W * s));
    imp.viewCv.height = Math.max(1, Math.floor(Hh * s));
  }

  /* Draw the plan in the card, always opaque: as uploaded, or recoloured with
   * the chosen materials. Hovering a colour row fades everything else. */
  function drawView() {
    if (!imp || !imp.viewCv) return;
    var cv = imp.viewCv, ctx = cv.getContext("2d");
    var vw = cv.width, vh = cv.height;
    ctx.fillStyle = "#ffffff";
    ctx.fillRect(0, 0, vw, vh);
    ctx.imageSmoothingEnabled = true;
    ctx.drawImage(imp.canvas, 0, 0, vw, vh);
    if (imp.view !== "assigned" && imp.hover == null) return;

    var W = imp.canvas.width, Hh = imp.canvas.height;
    var img = ctx.getImageData(0, 0, vw, vh), d = img.data;
    var colors = H.colors(), cache = {};
    function rgbOf(id) {
      if (!cache[id]) cache[id] = cssColorToRgb(colors[id] || "#ffffff");
      return cache[id];
    }
    for (var y = 0; y < vh; y++) {
      var sy = Math.min(Hh - 1, Math.floor(y * Hh / vh));
      for (var x = 0; x < vw; x++) {
        var sx = Math.min(W - 1, Math.floor(x * W / vw));
        var si = sy * W + sx, o = (y * vw + x) * 4;
        var fade = false;
        if (imp.view === "assigned") {
          var m = imp.mat ? imp.mat[si] : 0;
          if (m) {
            var c = rgbOf(m);
            d[o] = c[0]; d[o + 1] = c[1]; d[o + 2] = c[2];
          } else {
            fade = true;
          }
        }
        if (imp.hover != null && imp.pal.labels[si] !== imp.hover) fade = true;
        if (fade) {
          d[o]     = 255 - (255 - d[o]) * 0.25;
          d[o + 1] = 255 - (255 - d[o + 1]) * 0.25;
          d[o + 2] = 255 - (255 - d[o + 2]) * 0.25;
        }
      }
    }
    ctx.putImageData(img, 0, 0);
  }

  // ── Apply and send ──────────────────────────────────────────────────────────

  /* A class array as a PNG whose pixel value is the class id, the format R's
   * paintDecodeClassPNG() reads. Fully opaque, so the canvas keeps the values
   * exactly (premultiplied alpha would round them). */
  function encodeClasses(arr, w, h) {
    var any = false;
    for (var i = 0; i < arr.length; i++) if (arr[i]) { any = true; break; }
    if (!any) return null;
    var cv = document.createElement("canvas");
    cv.width = w; cv.height = h;
    var ctx = cv.getContext("2d");
    var img = ctx.createImageData(w, h), d = img.data;
    for (i = 0; i < arr.length; i++) {
      var o = i * 4;
      d[o] = d[o + 1] = d[o + 2] = arr[i];
      d[o + 3] = 255;
    }
    ctx.putImageData(img, 0, 0);
    return cv.toDataURL("image/png");
  }

  function send(payload, tries) {
    var entry = {
      payload: payload, tries: tries,
      timer: setTimeout(function () {
        inflight.delete(payload.seq);
        if (tries < 2) send(payload, tries + 1);
        else alert(labels.saveError);
      }, ACK_TIMEOUT)
    };
    inflight.set(payload.seq, entry);
    Shiny.setInputValue(NS + "paintImport", payload, { priority: "event" });
  }

  function apply() {
    if (classifyTimer) { clearTimeout(classifyTimer); classifyTimer = null; classifyCells(); }
    var f = imp.foot;
    H.flush();
    H.preview("ground", null); H.preview("canopy", null);
    H.commit("ground", imp.ground, f.w, f.h, f.col0, f.rowTop);
    H.commit("canopy", imp.canopy, f.w, f.h, f.col0, f.rowTop);

    seq += 1;
    send({
      seq: seq, version: H.version(),
      col0: f.col0, rowTop: f.rowTop, w: f.w, h: f.h,
      ground: encodeClasses(imp.ground, f.w, f.h),
      canopy: encodeClasses(imp.canopy, f.w, f.h)
    }, 1);
    finish();
  }

  // ── Lifecycle ───────────────────────────────────────────────────────────────

  function finish() {
    if (!imp) return;
    if (classifyTimer) { clearTimeout(classifyTimer); classifyTimer = null; }
    closeCard();
    if (imp.floating && imp.floating.parentNode) imp.floating.parentNode.removeChild(imp.floating);
    var p = panel();
    if (p) { p.style.display = "none"; p.innerHTML = ""; }
    showPaintButtons(true);
    imp = null;
    H.setImporting(false);
  }

  function cancel() {
    if (!imp) return;
    H.preview("ground", null); H.preview("canopy", null);
    finish();
  }

  function start(file) {
    if (!H || !H.ready() || !H.editable() || imp) return;
    openFile(file).then(function (doc) {
      return doc.render(1).then(function (cv) { return { doc: doc, canvas: cv }; });
    }).then(function (o) {
      if (!H.ready() || !H.editable() || imp) return;
      imp = { doc: o.doc, page: 1, canvas: o.canvas, pal: null };
      H.setImporting(true);
      showPaintButtons(false);
      enterPlace();
    }).catch(function (e) {
      if (window.console) console.error("plan import:", e);
      alert(labels.readError);
    });
  }

  function init() {
    H = window.__vftPaintHooks;
    if (!H || !window.Shiny || !Shiny.addCustomMessageHandler) {
      setTimeout(init, 200);
      return;
    }
    H.on("cancel", function () { cancel(); });

    Shiny.addCustomMessageHandler("plan-import-labels", function (msg) {
      labels = Object.assign({}, L_, msg || {});
      labels.materials = Object.assign({}, L_.materials, (msg && msg.materials) || {});
    });
    Shiny.addCustomMessageHandler("paint-import-ack", function (msg) {
      var entry = inflight.get(msg.seq);
      if (entry) { clearTimeout(entry.timer); inflight.delete(msg.seq); }
    });

    document.addEventListener("change", function (e) {
      if (!e.target || e.target.id !== NS + "planFile") return;
      var file = e.target.files && e.target.files[0];
      e.target.value = "";   //so choosing the same file again fires again
      if (file) start(file);
    });
    document.addEventListener("keydown", function (e) {
      if (imp && e.key === "Escape") cancel();
    });
    window.addEventListener("resize", function () {
      if (imp && imp.stage === "map") { sizeView(); drawView(); }
    });

    document.head.appendChild(el("style", { text: [
      ".vft-plan-floating{position:absolute;left:50%;top:50%;transform:translate(-50%,-50%);",
      "  height:auto;opacity:.6;pointer-events:none;z-index:650;box-shadow:0 0 0 2px #069869;}",
      ".vft-plan-panel{display:inline-flex;flex-direction:column;gap:6px;margin-top:10px;",
      "  padding:8px 12px;border:1px solid #bdbdbd;border-radius:6px;background:#fff;}",
      ".vft-plan-hint{font-size:12px;color:#555;max-width:520px;}",
      ".vft-plan-row{display:flex;gap:8px;align-items:center;justify-content:center;flex-wrap:wrap;}",
      ".vft-plan-card{position:absolute;left:5%;top:5%;width:90%;height:90%;z-index:1100;",
      "  background:#fff;border-radius:8px;box-shadow:0 4px 18px rgba(0,0,0,.35);",
      "  display:flex;flex-direction:column;padding:10px;cursor:default;text-align:left;}",
      ".vft-plan-head{display:flex;align-items:center;gap:10px;margin-bottom:8px;}",
      ".vft-plan-head .btn.active{background:#069869;color:#fff;}",
      ".vft-plan-body{display:flex;gap:12px;flex:1 1 auto;min-height:0;flex-wrap:wrap;}",
      ".vft-plan-left{flex:3 1 300px;min-height:200px;display:flex;align-items:center;justify-content:center;}",
      ".vft-plan-view{max-width:100%;max-height:100%;cursor:crosshair;border:1px solid #ddd;}",
      ".vft-plan-right{flex:2 1 240px;display:flex;flex-direction:column;gap:8px;min-height:0;}",
      ".vft-plan-rows{overflow-y:auto;flex:1 1 auto;min-height:0;}",
      ".vft-plan-colrow{display:flex;align-items:center;gap:8px;padding:3px 4px;border-radius:4px;}",
      ".vft-plan-colrow:hover{background:#eef7f2;}",
      ".vft-plan-colrow select{flex:1 1 auto;width:auto;}",
      ".vft-plan-swatch{width:32px;height:22px;border:1px solid #888;border-radius:3px;flex:none;}",
      ".vft-plan-share{width:56px;text-align:right;font-size:12px;color:#555;flex:none;}",
      ".vft-plan-smooth{font-weight:normal;margin:0;}",
      "@keyframes vftPlanFlash{from{background:#ffe08a;}to{background:transparent;}}",
      ".vft-plan-flash{animation:vftPlanFlash 1.2s ease-out;}"
    ].join("\n") }));
  }

  init();
})();
