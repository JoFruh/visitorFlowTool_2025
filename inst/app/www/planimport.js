/* Plan import for the heat-mitigation map (newVersions, context 4).
 *
 * A user uploads a design plan (PDF, PNG, JPG or TIFF) and it becomes paint:
 *
 *   1. PLACE. A plan carries no coordinates, so it floats at the centre of the
 *      map, multiplied onto it so the map stays visible through it. The user pans and zooms the map underneath it
 *      and drags the plan's corners until the two agree. Plans are expected
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
  var K        = 24;         //candidate colours, before merging
  //Candidates closer than this (CIE76) are not seeded twice. Small on purpose:
  //a clean plan can draw white buildings on a ground only 3-5 apart.
  var MIN_SEP  = 2.5;
  //Ceiling for merging. Two colours further apart than this are never merged,
  //whatever else is true of them. 20 is about the limit: typical fills of
  //*different* materials come as close as 21-22 (light grey vs sand or white,
  //bush vs a light tree green).
  var MERGE_DE = 14;
  //Below the ceiling, two colours are merged only if they are the same fill
  //by one of two tests (see mergePalette):
  //  - their pixel spreads overlap, which is what shading and anti-aliasing do;
  //  - one is small (< MIX_SMALL of the plan) and its pixels are interleaved
  //    with the other's (MIX_T), which is what JPEG blocks do - compression
  //    turns noise into flat 8x8 patches, so each shade is tightly bunched and
  //    the spread test alone would keep every one of them.
  //Two large fills are never merged by interleaving: white buildings on grey
  //ground touch all along their edges without being one colour.
  var MIX_T     = 0.15;
  var MIX_SMALL = 0.05;
  var MIN_SHARE = 0.01;      //colours covering less than this are dropped
  //Strength of the floating plan while it is placed. Full by default: the plan
  //is multiplied onto the map rather than faded over it (see planPane),
  //so its white paper is already see-through and its fills keep their contrast.
  var DEFAULT_OPACITY = 1;
  var ACK_TIMEOUT = 20000;

  //German fallbacks, used until R sends the translated set
  var L_ = {
    maxCells: 4e6,
    place: "Plan platzieren",
    placeHint: "Ziehen Sie den Plan an seinen Platz und ziehen Sie an seinen Ecken, bis er passt. Die Karte kann weiterhin verschoben und gezoomt werden. Der Plan muss nach Norden ausgerichtet sein.",
    opacity: "Deckkraft", page: "Seite", nextStep: "Weiter", back: "Zurück",
    apply: "Anwenden", cancel: "Abbrechen", mapColors: "Farben zuordnen",
    original: "Original", assigned: "Zuordnung",
    pickColor: "Farbe aufnehmen", removeColor: "Farbe entfernen",
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
    [1, "#d8e6d0"], [1, "#e2eedc"],   //pale lawn tints, which illustrative plans use a lot
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

  /* Lab of every 15-bit RGB bin (5 bits a channel), computed once. Pixels are
   * matched to colours through these bins: 32k Lab conversions instead of one
   * per pixel of a 16 MP image. */
  var binLabs = null;
  function binLab() {
    if (binLabs) return binLabs;
    binLabs = new Float32Array(32768 * 3);
    for (var q = 0; q < 32768; q++) {
      var l = lab(((q >> 10) & 31) * 255 / 31, ((q >> 5) & 31) * 255 / 31, (q & 31) * 255 / 31);
      binLabs[q * 3] = l[0]; binLabs[q * 3 + 1] = l[1]; binLabs[q * 3 + 2] = l[2];
    }
    return binLabs;
  }
  function binOf(d, o) { return ((d[o] >> 3) << 10) | ((d[o + 1] >> 3) << 5) | (d[o + 2] >> 3); }

  /* For every bin, the nearest of `centres` (Lab triples). */
  function nearestLut(centres) {
    var BL = binLab(), lut = new Uint8Array(32768);
    for (var q = 0; q < 32768; q++) {
      var l0 = BL[q * 3], l1 = BL[q * 3 + 1], l2 = BL[q * 3 + 2];
      var bk = 0, bd = Infinity;
      for (var c = 0; c < centres.length; c++) {
        var x = l0 - centres[c][0], y = l1 - centres[c][1], z = l2 - centres[c][2];
        var dd = x * x + y * y + z * z;
        if (dd < bd) { bd = dd; bk = c; }
      }
      lut[q] = bk;
    }
    return lut;
  }

  /* Main colours of the plan.
   *
   *   1. Candidates: the most populated colour bins, at least MIN_SEP apart.
   *      Seeding from the histogram rather than at random is what gives a pale
   *      fill its own candidate when it sits next to a larger one.
   *   2. k-means in Lab on a sample, to centre the candidates on their fills.
   *   3. Every pixel labelled with its nearest candidate.
   *   4. Candidates that are the same fill merged (mergePalette), rare ones
   *      dropped, and every pixel relabelled to the colour it ended up in. */
  function extractPalette(cv) {
    var w = cv.width, h = cv.height, n = w * h;
    var data = cv.getContext("2d").getImageData(0, 0, w, h).data;

    //1. candidates from the histogram, each at its bin's mean colour
    var cnt = new Uint32Array(32768), sr = new Float64Array(32768),
        sg = new Float64Array(32768), sb = new Float64Array(32768);
    for (var p = 0, o = 0; p < n; p++, o += 4) {
      var q = binOf(data, o);
      cnt[q]++; sr[q] += data[o]; sg[q] += data[o + 1]; sb[q] += data[o + 2];
    }
    var bins = [];
    for (q = 0; q < 32768; q++) if (cnt[q] >= 0.1 * MIN_SHARE * n) bins.push(q);
    bins.sort(function (x, y) { return cnt[y] - cnt[x]; });
    var centres = [];
    for (var i = 0; i < bins.length && centres.length < K; i++) {
      var bq = bins[i];
      var lb = lab(sr[bq] / cnt[bq], sg[bq] / cnt[bq], sb[bq] / cnt[bq]);
      var far = centres.every(function (c) { return de2(c, lb) >= MIN_SEP * MIN_SEP; });
      if (far) centres.push(lb);
    }
    if (!centres.length) {   //a plan with no bin that large: take the biggest
      var top = 0;
      for (q = 1; q < 32768; q++) if (cnt[q] > cnt[top]) top = q;
      centres.push(lab(sr[top] / cnt[top], sg[top] / cnt[top], sb[top] / cnt[top]));
    }

    //2. k-means on a random sample (random, not a stride: hatching and dot
    //patterns would alias with a regular one)
    var rand = rng(1), pts = [];
    var m = Math.min(n, SAMPLE);
    for (i = 0; i < m; i++) {
      o = Math.floor(rand() * n) * 4;
      pts.push(lab(data[o], data[o + 1], data[o + 2]));
    }
    var C = centres.length, assign = new Int32Array(pts.length);
    function assignAll() {
      for (var i2 = 0; i2 < pts.length; i2++) {
        var bi = 0, bd = Infinity;
        for (var c = 0; c < C; c++) {
          var dd = de2(pts[i2], centres[c]);
          if (dd < bd) { bd = dd; bi = c; }
        }
        assign[i2] = bi;
      }
    }
    for (var it = 0; it < 15; it++) {
      assignAll();
      var sums = centres.map(function () { return [0, 0, 0, 0]; });
      for (i = 0; i < pts.length; i++) {
        var su = sums[assign[i]];
        su[0] += pts[i][0]; su[1] += pts[i][1]; su[2] += pts[i][2]; su[3]++;
      }
      centres = centres.map(function (ct, k) {
        var su2 = sums[k];
        return su2[3] ? [su2[0] / su2[3], su2[1] / su2[3], su2[2] / su2[3]] : ct;
      });
    }
    assignAll();
    //spread: RMS distance of a candidate's sample pixels from it
    var ss = new Float64Array(C), sn = new Float64Array(C);
    for (i = 0; i < pts.length; i++) { ss[assign[i]] += de2(pts[i], centres[assign[i]]); sn[assign[i]]++; }

    //3. label every pixel with its candidate; count pixels and 4-neighbour
    //contacts between candidates
    var lut = nearestLut(centres);
    var pre = new Uint8Array(n), N = new Float64Array(C), A = new Float64Array(C * C);
    for (p = 0, o = 0; p < n; p++, o += 4) { pre[p] = lut[binOf(data, o)]; N[pre[p]]++; }
    for (var y = 0; y < h; y++) {
      var row = y * w;
      for (var x = 0; x < w; x++) {
        var l0 = pre[row + x];
        if (x + 1 < w) { var lr = pre[row + x + 1]; if (lr !== l0) { A[l0 * C + lr]++; A[lr * C + l0]++; } }
        if (y + 1 < h) { var ld = pre[row + w + x]; if (ld !== l0) { A[l0 * C + ld]++; A[ld * C + l0]++; } }
      }
    }
    var cands = centres.map(function (c, k) {
      return { lab: c, n: N[k], s: sn[k] ? Math.sqrt(ss[k] / sn[k]) : 0 };
    });

    //4. merge, drop the rare, relabel
    var groups = mergePalette(cands, A, n);
    var kept = groups.filter(function (g) { return g.n / n >= MIN_SHARE; })
                     .sort(function (g1, g2) { return g2.n - g1.n; });
    if (!kept.length) kept = groups.slice(0, 1);
    var keptLabs = kept.map(function (g) { return g.lab; });
    //a candidate goes to the kept colour its group became, or - if its group
    //was dropped as rare - to the nearest kept colour
    var toKept = new Uint8Array(C);
    groups.forEach(function (g) {
      var idx = kept.indexOf(g);
      g.members.forEach(function (k) {
        if (idx >= 0) { toKept[k] = idx; return; }
        var bi = 0, bd = Infinity;
        keptLabs.forEach(function (kl, j) { var dd = de2(kl, centres[k]); if (dd < bd) { bd = dd; bi = j; } });
        toKept[k] = bi;
      });
    });
    var base = new Uint8Array(n);
    for (p = 0; p < n; p++) base[p] = toKept[pre[p]];

    var P = {
      data: data, w: w, h: h, base: base, labels: new Uint8Array(n), nBase: kept.length,
      palette: kept.map(function (g) {
        return { lab: g.lab, rgb: [0, 0, 0], share: 0, material: suggest(g.lab), picked: false };
      })
    };
    relabel(P);
    return P;
  }

  /* Merge candidates that are the same fill, closest pair first. See MIX_T and
   * MIX_SMALL for the two tests. A merged colour keeps the spread of its larger
   * part - the fill itself - rather than the spread of the union or the largest
   * spread among its parts. Either of those grows with every tint absorbed, and
   * a fill that has swallowed a faint tint would then "overlap" the next fill:
   * on a real plan that is how white buildings disappeared into a ground only
   * 4 units darker. */
  function mergePalette(cands, A, n) {
    var C = cands.length;
    var groups = cands.map(function (c, k) {
      return { lab: c.lab.slice(), n: c.n, s: c.s, id: k, members: [k] };
    }).filter(function (g) { return g.n > 0; });
    A = Float64Array.from(A);   //merged rows are summed in place
    for (;;) {
      var best = null, bd = Infinity;
      for (var i = 0; i < groups.length; i++) {
        for (var j = i + 1; j < groups.length; j++) {
          var ga = groups[i], gb = groups[j];
          var d = Math.sqrt(de2(ga.lab, gb.lab));
          if (d >= MERGE_DE || d >= bd) continue;
          var small = Math.min(ga.n, gb.n);
          var overlap = d < ga.s + gb.s;
          var mixed = small / n < MIX_SMALL && A[ga.id * C + gb.id] / small >= MIX_T;
          if (overlap || mixed) { bd = d; best = [i, j]; }
        }
      }
      if (!best) break;
      var g1 = groups[best[0]], g2 = groups[best[1]], t = g1.n + g2.n;
      g1.lab = [0, 1, 2].map(function (k) { return (g1.lab[k] * g1.n + g2.lab[k] * g2.n) / t; });
      if (g2.n > g1.n) g1.s = g2.s;
      g1.n = t;
      g1.members = g1.members.concat(g2.members);
      for (var k = 0; k < C; k++) {
        A[g1.id * C + k] += A[g2.id * C + k];
        A[k * C + g1.id] += A[k * C + g2.id];
      }
      A[g1.id * C + g1.id] = 0;
      groups.splice(best[1], 1);
    }
    return groups;
  }

  /* Labels from the automatic palette, then each picked colour in turn takes
   * the pixels that are nearer to it than to the colour they had. Recomputed
   * from the automatic labels every time, so removing a pick is exact. Also
   * refreshes every colour's share and swatch (the mean of its pixels - the
   * colour the user will recognise from the plan). */
  function relabel(P) {
    var labels = P.labels, data = P.data, n = P.w * P.h, E = P.palette.length;
    labels.set(P.base);
    var BL = binLab();
    for (var j = P.nBase; j < E; j++) {
      var pl = P.palette[j].lab;
      //per bin: squared distance to the pick, and to each earlier colour
      var dp = new Float32Array(32768), de = [];
      for (var e = 0; e < j; e++) de.push(new Float32Array(32768));
      for (var q = 0; q < 32768; q++) {
        var l0 = BL[q * 3], l1 = BL[q * 3 + 1], l2 = BL[q * 3 + 2];
        var x = l0 - pl[0], y = l1 - pl[1], z = l2 - pl[2];
        dp[q] = x * x + y * y + z * z;
        for (e = 0; e < j; e++) {
          var c = P.palette[e].lab;
          x = l0 - c[0]; y = l1 - c[1]; z = l2 - c[2];
          de[e][q] = x * x + y * y + z * z;
        }
      }
      for (var p = 0, o = 0; p < n; p++, o += 4) {
        var qq = binOf(data, o);
        if (dp[qq] < de[labels[p]][qq]) labels[p] = j;
      }
    }
    var cnt = new Float64Array(E), sums = new Float64Array(E * 3);
    for (p = 0, o = 0; p < n; p++, o += 4) {
      var k = labels[p];
      cnt[k]++; sums[k * 3] += data[o]; sums[k * 3 + 1] += data[o + 1]; sums[k * 3 + 2] += data[o + 2];
    }
    P.palette.forEach(function (en, k2) {
      en.share = cnt[k2] / n;
      if (cnt[k2]) en.rgb = [sums[k2 * 3] / cnt[k2], sums[k2 * 3 + 1] / cnt[k2], sums[k2 * 3 + 2] / cnt[k2]];
    });
  }

  /* Add the colour under plan pixel (x, y) - the mean of its 3x3 neighbourhood,
   * so one anti-aliased pixel does not decide it. Returns the row index: the
   * existing one if that colour was already picked. */
  function pickColour(x, y) {
    var P = imp.pal, d = P.data, r = 0, g = 0, b = 0, c = 0;
    for (var dy = -1; dy <= 1; dy++) {
      for (var dx = -1; dx <= 1; dx++) {
        var xx = x + dx, yy = y + dy;
        if (xx < 0 || yy < 0 || xx >= P.w || yy >= P.h) continue;
        var o = (yy * P.w + xx) * 4;
        r += d[o]; g += d[o + 1]; b += d[o + 2]; c++;
      }
    }
    var rgb = [r / c, g / c, b / c], lb = lab(rgb[0], rgb[1], rgb[2]);
    for (var k = P.nBase; k < P.palette.length; k++) {
      if (de2(P.palette[k].lab, lb) < 1) return k;
    }
    P.palette.push({ lab: lb, rgb: rgb, share: 0, material: suggest(lb), picked: true });
    relabel(P);
    return P.palette.length - 1;
  }

  function unpickColour(k) {
    var P = imp.pal;
    if (k < P.nBase || k >= P.palette.length) return;
    P.palette.splice(k, 1);
    relabel(P);
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

  /* Plan pixels -> material ids, then a 3x3 majority filter. The filter is
   * always on: it removes anti-aliased edges and thin label text, which would
   * otherwise land as speckles of the wrong material. It runs on materials
   * rather than palette entries, so two colours that both mean "grass" do not
   * fight over a boundary. */
  function materialMap() {
    var P = imp.pal, W = imp.canvas.width, Hh = imp.canvas.height;
    var lut = new Uint8Array(P.palette.length);
    P.palette.forEach(function (p, k) { lut[k] = +p.material; });
    var mat = new Uint8Array(W * Hh);
    for (var i = 0; i < mat.length; i++) mat[i] = lut[P.labels[i]];

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

  /* The plan's frame on screen, in map-container pixels: {left, top, w}. The
   * height always follows from the image's aspect ratio - a plan is drawn to
   * one scale, so stretching it along one axis could only make it wrong. */
  function boxHeight(w) { return w * imp.canvas.height / imp.canvas.width; }

  var MIN_FRAME = 30;   //px; a smaller plan could no longer be grabbed
  //a moved plan keeps at least this much of itself on the map, so it can
  //always be grabbed again (corners may go anywhere: the map pans to them)
  var KEEP = 40;

  /* The plan is pinned to the map: imp.geo holds its NW and SE corners as
   * LatLngs, and imp.box is only the current view of them. Mercator scales
   * both axes alike, so the box keeps the plan's aspect at every zoom. */
  function boxAt(ll, center, zoom) {
    var map = H.map();
    if (center == null) return map.latLngToContainerPoint(ll);
    return map.project(ll, zoom).subtract(map.project(center, zoom))
              .add(map.getSize().divideBy(2));
  }
  function boxFromGeo(center, zoom) {
    var a = boxAt(imp.geo.nw, center, zoom), b = boxAt(imp.geo.se, center, zoom);
    return { left: a.x, top: a.y, w: b.x - a.x };
  }
  function setBox(bx) {
    var map = H.map();
    imp.box = bx;
    imp.geo = {
      nw: map.containerPointToLatLng(L.point(bx.left, bx.top)),
      se: map.containerPointToLatLng(L.point(bx.left + bx.w, bx.top + boxHeight(bx.w)))
    };
    layoutFrame();
  }
  function geoBounds() {
    return L.latLngBounds([imp.geo.se.lat, imp.geo.nw.lng], [imp.geo.nw.lat, imp.geo.se.lng]);
  }

  function placeElems(bx) {
    [imp.floating, imp.mover].forEach(function (e) {
      e.style.left   = bx.left + "px";
      e.style.top    = bx.top + "px";
      e.style.width  = bx.w + "px";
      e.style.height = boxHeight(bx.w) + "px";
    });
  }

  function layoutFrame() {
    placeElems(imp.box);
    //the image is a Leaflet overlay, so it pans and zooms with the map itself
    imp.overlay.setBounds(geoBounds());
    imp.overlay.setOpacity(imp.opacity);
  }

  //Map events while placing: the handles and the move surface follow the plan.
  //During a zoom animation they are sent straight to where the plan will end
  //up, and a CSS transition matching Leaflet's carries them there.
  function onMapView() {
    if (!imp || !imp.geo || !imp.floating) return;
    imp.box = boxFromGeo();
    placeElems(imp.box);
  }
  function onZoomAnim(e) {
    if (!imp || !imp.geo || !imp.floating) return;
    placeElems(boxFromGeo(e.center, e.zoom));
  }
  var MAP_EVENTS = "move zoom viewreset resize";

  function removeFloating() {
    if (!imp) return;
    [imp.floating, imp.mover].forEach(function (e) {
      if (e && e.parentNode) e.parentNode.removeChild(e);
    });
    //the map the plan was placed on: a context switch may already have
    //replaced the widget's map by then
    if (imp.overlay) imp.overlay.remove();
    if (imp.placeMap) {
      imp.placeMap.off(MAP_EVENTS, onMapView);
      imp.placeMap.off("zoomanim", onZoomAnim);
    }
    imp.floating = imp.mover = imp.shown = imp.overlay = imp.placeMap = null;
  }

  function planPane() {
    var map = H.map();
    var pane = map.getPane("vftPlanPane");
    if (!pane) {
      pane = map.createPane("vftPlanPane");
      pane.style.zIndex = 640;               //above paint (415/425) and markers
      pane.style.pointerEvents = "none";
      //the pane, not the image, is blended: every Leaflet pane is its own
      //stacking context, so an image blended inside it would see nothing
      pane.style.mixBlendMode = "multiply";
    }
    return "vftPlanPane";
  }

  /* What is shown while placing: the plan in grey, histogram-equalised. Plans
   * are mostly pale fills a few shades apart, which multiply onto the map as
   * an almost invisible tint; equalising spreads those shades over the whole
   * grey range, so every fill edge shows as a clear step. The paper (the
   * lightest tone) stays white and so drops out under multiply. The darkest
   * tone is kept at SHOWN_FLOOR so the map stays readable under it.
   *
   * Blue keeps its colour. Water is sparse, it barely moves between plan
   * editions and the map shows it too, so it is the surest thing to line a
   * plan up against - as grey it would be one more pale fill. A blue pixel is
   * rebuilt rather than passed through: a plan blue is pale and would multiply
   * onto the map as almost nothing, while the equalised tone that fits the
   * greys turns it near-black. It is redrawn flat in BLUE_SHOWN instead - a
   * water body reads as one shape, not as a tonal range, and a fixed light
   * blue keeps the map under it legible. Blues are left out of the grey
   * histogram as well, so a large lake does not eat the grey range.
   * A copy: imp.canvas keeps the real colours for the palette. */
  var SHOWN_MAX = 2048;    //px, long side; the screen never shows more
  var SHOWN_FLOOR = 60;
  var SHOWN_CAP = 0.02;    //most weight one tone can have, as a share of pixels
  var BLUE_DELTA = 16;     //B must beat R by this much to count as blue
  var BLUE_SLACK = 24;     //how far G may pass B before it is green, not cyan
  var BLUE_MIN = 48;       //below this a bluish pixel is just a dark line
  var BLUE_SHOWN = [126, 198, 222];   //#7ec6de, the colour every blue is drawn in
  function planDisplay(src) {
    var s = Math.min(1, SHOWN_MAX / Math.max(src.width, src.height));
    var w = Math.max(1, Math.round(src.width * s)), h = Math.max(1, Math.round(src.height * s));
    var cv = document.createElement("canvas");
    cv.width = w; cv.height = h;
    var ctx = cv.getContext("2d");
    ctx.fillStyle = "#fff";              //transparent parts count as paper
    ctx.fillRect(0, 0, w, h);
    ctx.drawImage(src, 0, 0, w, h);
    var img = ctx.getImageData(0, 0, w, h), d = img.data, n = w * h;
    var gray = new Uint8Array(n), blue = new Uint8Array(n), hist = new Float64Array(256);
    for (var i = 0, j = 0; i < n; i++, j += 4) {
      var v = (d[j] * 299 + d[j + 1] * 587 + d[j + 2] * 114 + 500) / 1000 | 0;
      gray[i] = v;
      if (d[j + 2] - d[j] >= BLUE_DELTA && d[j + 2] + BLUE_SLACK >= d[j + 1] &&
          d[j + 2] >= BLUE_MIN) blue[i] = 1;
      else hist[v]++;
    }
    //Each tone's weight is capped first: otherwise one large fill would take
    //most of the range and push its neighbours together. A tone then maps to
    //the middle of its own share, rescaled so the darkest tone present gets
    //SHOWN_FLOOR and the lightest white.
    var cap = n * SHOWN_CAP, mid = new Float64Array(256), cum = 0, lo = -1, hi = -1;
    for (v = 0; v < 256; v++) {
      var h = Math.min(hist[v], cap);
      mid[v] = cum + h / 2;
      cum += h;
      if (hist[v]) { if (lo < 0) lo = v; hi = v; }
    }
    var lut = new Uint8Array(256), span = mid[hi] - mid[lo];
    for (v = 0; v < 256; v++) {
      var t = span > 0 ? Math.min(1, Math.max(0, (mid[v] - mid[lo]) / span)) : 1;
      lut[v] = Math.round(SHOWN_FLOOR + t * (255 - SHOWN_FLOOR));
    }
    for (i = 0, j = 0; i < n; i++, j += 4) {
      if (blue[i]) {
        d[j] = BLUE_SHOWN[0]; d[j + 1] = BLUE_SHOWN[1]; d[j + 2] = BLUE_SHOWN[2];
      } else {
        d[j] = d[j + 1] = d[j + 2] = lut[gray[i]];
      }
      d[j + 3] = 255;
    }
    ctx.putImageData(img, 0, 0);
    return cv;
  }

  /* Dragging the plan itself moves it, with the grab point held under the
   * pointer. The surface sits under Leaflet's controls (unlike the handles),
   * so a plan covering the zoom buttons does not disable them; mouse-wheel
   * zoom still reaches the map, which listens on its container. */
  function moveSurface() {
    var mv = el("div", { "class": "vft-plan-mover" });
    var drag = null;
    function stop(e) { e.stopPropagation(); e.preventDefault(); }
    mv.addEventListener("pointerdown", function (e) {
      if (e.button !== 0) return;
      stop(e);
      drag = { x: e.clientX, y: e.clientY, left: imp.box.left, top: imp.box.top };
      mv.setPointerCapture(e.pointerId);
      mv.classList.add("vft-plan-moving");
    });
    mv.addEventListener("pointermove", function (e) {
      if (!drag) return;
      stop(e);
      var cont = mapContainer(), w = imp.box.w, h = boxHeight(w);
      var left = drag.left + e.clientX - drag.x, top = drag.top + e.clientY - drag.y;
      setBox({
        left: Math.min(Math.max(left, Math.min(0, KEEP - w)), cont.clientWidth - Math.min(KEEP, w)),
        top:  Math.min(Math.max(top,  Math.min(0, KEEP - h)), cont.clientHeight - Math.min(KEEP, h)),
        w:    w
      });
    });
    function end(e) {
      if (!drag) return;
      drag = null;
      mv.classList.remove("vft-plan-moving");
      if (mv.hasPointerCapture && mv.hasPointerCapture(e.pointerId)) mv.releasePointerCapture(e.pointerId);
    }
    mv.addEventListener("pointerup", end);
    mv.addEventListener("pointercancel", end);
    ["mousedown", "touchstart", "dblclick", "click", "contextmenu"].forEach(function (t) {
      mv.addEventListener(t, function (e) { e.stopPropagation(); });
    });
    return mv;
  }

  /* One corner handle. Dragging it resizes the plan about the OPPOSITE corner,
   * which stays where it is: the user lines up one corner of the plan with the
   * map, then drags the other to its match. The dragged corner follows the
   * pointer along whichever axis it has moved further, keeping the aspect. */
  function cornerHandle(sx, sy) {
    //sx/sy: -1 = left/top corner, +1 = right/bottom corner
    var hd = el("div", { "class": "vft-plan-handle",
                         style: (sx < 0 ? "left" : "right") + ":-8px;" +
                                (sy < 0 ? "top" : "bottom") + ":-8px;" +
                                "cursor:" + (sx === sy ? "nwse" : "nesw") + "-resize;" });
    var drag = null;
    function pt(e) {
      var r = mapContainer().getBoundingClientRect();
      return { x: e.clientX - r.left, y: e.clientY - r.top };
    }
    function stop(e) { e.stopPropagation(); e.preventDefault(); }

    hd.addEventListener("pointerdown", function (e) {
      stop(e);
      var bx = imp.box, h = boxHeight(bx.w), p = pt(e);
      var cx = sx < 0 ? bx.left : bx.left + bx.w;
      var cy = sy < 0 ? bx.top  : bx.top + h;
      drag = {
        ax: sx < 0 ? bx.left + bx.w : bx.left,   //the corner that stays put
        ay: sy < 0 ? bx.top + h     : bx.top,
        ox: p.x - cx, oy: p.y - cy               //grab offset, so it does not jump
      };
      hd.setPointerCapture(e.pointerId);
    });
    hd.addEventListener("pointermove", function (e) {
      if (!drag) return;
      stop(e);
      var p = pt(e), aspect = imp.canvas.width / imp.canvas.height;
      var dx = Math.max(0, sx * (p.x - drag.ox - drag.ax));
      var dy = Math.max(0, sy * (p.y - drag.oy - drag.ay));
      var w  = Math.max(MIN_FRAME, dx, dy * aspect);
      setBox({
        left: sx < 0 ? drag.ax - w : drag.ax,
        top:  sy < 0 ? drag.ay - boxHeight(w) : drag.ay,
        w:    w
      });
    });
    function end(e) {
      if (!drag) return;
      drag = null;
      if (hd.hasPointerCapture && hd.hasPointerCapture(e.pointerId)) hd.releasePointerCapture(e.pointerId);
    }
    hd.addEventListener("pointerup", end);
    hd.addEventListener("pointercancel", end);
    //keep Leaflet from starting a map drag or a zoom off the handle
    ["mousedown", "touchstart", "dblclick", "click"].forEach(function (t) {
      hd.addEventListener(t, function (e) { e.stopPropagation(); });
    });
    return hd;
  }

  function enterPlace() {
    imp.stage = "place";
    closeCard();
    H.preview("ground", null); H.preview("canopy", null);

    var cont = mapContainer(), map = H.map();
    if (imp.opacity === undefined) imp.opacity = DEFAULT_OPACITY;

    //Three pieces. The image is a Leaflet overlay in its own multiplied pane,
    //so it pans and zooms with the map. The move surface and the handles are
    //plain elements in the map container that follow it (onMapView): the
    //handles above Leaflet's controls, unblended - multiplied, their white
    //fill would vanish into the map.
    imp.mover = moveSurface();
    imp.floating = el("div", { "class": "vft-plan-floating" }, [
      cornerHandle(-1, -1), cornerHandle(1, -1), cornerHandle(-1, 1), cornerHandle(1, 1)
    ]);
    cont.appendChild(imp.mover);
    cont.appendChild(imp.floating);
    imp.overlay = L.imageOverlay(planDisplay(imp.canvas).toDataURL("image/png"),
                                 [[0, 0], [0, 0]],
                                 { pane: planPane(), className: "vft-plan-image",
                                   interactive: false, opacity: imp.opacity }).addTo(map);
    imp.shown = imp.overlay.getElement();
    imp.placeMap = map;
    map.on(MAP_EVENTS, onMapView);
    map.on("zoomanim", onZoomAnim);

    if (imp.geo) {
      //back from the colour card: where the user left it on the map
      setBox(boxFromGeo());
    } else {
      //start centred, at 70 % of whichever side of the map binds
      var cw = cont.clientWidth, ch = cont.clientHeight;
      var w0 = Math.round(0.7 * Math.min(cw, ch * imp.canvas.width / imp.canvas.height));
      setBox({ left: (cw - w0) / 2, top: (ch - boxHeight(w0)) / 2, w: w0 });
    }

    //opacity: how strongly the plan is multiplied in. 10 % floor so the plan
    //can never vanish entirely.
    var opac = el("input", { type: "range", min: 10, max: 100, step: 1,
                             value: Math.round(imp.opacity * 100), style: "width:160px;",
                             "class": "vft-plan-opacity",
                             oninput: function () {
                               imp.opacity = +opac.value / 100;
                               imp.overlay.setOpacity(imp.opacity);
                             } });

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
        el("span", { text: labels.opacity + " ", style: "margin-left:12px;" }), opac
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
      imp.page = n;
      imp.canvas = cv;
      imp.pal = null;
      if (imp.stage === "place" && imp.overlay) {
        //same width and top-left corner; the height follows the new page
        imp.overlay.setUrl(planDisplay(cv).toDataURL("image/png"));
        setBox(boxFromGeo());
      }
    }).catch(function () { alert(labels.readError); });
  }

  /* Read the plan's position off the map: image pixel (x, y) -> LV95 (E, N),
   * as a least-squares affine over a grid of points on the plan. Every point
   * goes through the exact path a brush stroke does, so an applied plan lands
   * where a hand-painted copy of it would. */
  function fitPlacement() {
    var map = H.map();
    imp.box = boxFromGeo();
    var W = imp.canvas.width, Hh = imp.canvas.height;
    var dw = imp.box.w, dh = boxHeight(imp.box.w);
    var left = imp.box.left, top = imp.box.top;

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
    removeFloating();
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
    if (!imp.view) imp.view = "original";

    var cont = mapContainer();
    var viewCv = el("canvas", { "class": "vft-plan-view" });
    var rows = el("div", { "class": "vft-plan-rows" });
    imp.viewCv = viewCv;
    imp.rowEls = [];

    renderRows(rows);

    var tOrig = button(labels.original, function () { imp.view = "original"; syncToggle(); drawView(); });
    var tAss  = button(labels.assigned, function () { imp.view = "assigned"; syncToggle(); drawView(); });
    function syncToggle() {
      tOrig.classList.toggle("active", imp.view === "original");
      tAss.classList.toggle("active", imp.view === "assigned");
    }
    syncToggle();


    //the colour picker: one click on the plan adds that colour as a row
    var pickBtn = button(labels.pickColor, function () { setPicking(!imp.picking); });
    pickBtn.classList.add("vft-plan-pickbtn");
    function setPicking(on) {
      imp.picking = on;
      pickBtn.classList.toggle("active", on);
      viewCv.classList.toggle("vft-plan-picking", on);
    }
    setPicking(false);

    //click the plan: find a colour's row, or add the colour in picking mode
    viewCv.addEventListener("click", function (e) {
      var r = viewCv.getBoundingClientRect();
      var x = Math.floor((e.clientX - r.left) / r.width * imp.canvas.width);
      var y = Math.floor((e.clientY - r.top) / r.height * imp.canvas.height);
      if (x < 0 || y < 0 || x >= imp.canvas.width || y >= imp.canvas.height) return;
      var k;
      if (imp.picking) {
        k = pickColour(x, y);
        setPicking(false);
        renderRows(rows);
        scheduleClassify();
      } else {
        k = imp.pal.labels[y * imp.canvas.width + x];
      }
      flashRow(k);
    });

    var card = imp.card = el("div", { "class": "vft-plan-card" }, [
      el("div", { "class": "vft-plan-head" }, [
        el("strong", { text: labels.mapColors }),
        el("span", { style: "margin-left:auto;" }, [pickBtn]),
        el("span", { "class": "btn-group" }, [tOrig, tAss])
      ]),
      el("div", { "class": "vft-plan-body" }, [
        el("div", { "class": "vft-plan-left" }, [viewCv]),
        el("div", { "class": "vft-plan-right" }, [
          rows,
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

  /* The colour rows, rebuilt whenever a colour is picked or removed. */
  function renderRows(rows) {
    rows.innerHTML = "";
    imp.rowEls = [];
    imp.pal.palette.forEach(function (p, k) {
      var sel = el("select", { "class": "form-control input-sm",
                               onchange: function () { p.material = sel.value; scheduleClassify(); } });
      MATERIAL_ORDER.forEach(function (id) {
        var o = el("option", { value: id, text: labels.materials[id] || id });
        if (id === p.material) o.selected = true;
        sel.appendChild(o);
      });
      //picked colours can be taken out again; the automatic ones cannot, since
      //every pixel has to belong to some colour
      var remove = p.picked ? el("button", {
        type: "button", "class": "btn btn-default btn-xs vft-plan-unpick",
        title: labels.removeColor, text: "\u00d7",
        onclick: function () {
          unpickColour(k);
          imp.hover = null;
          renderRows(rows);
          scheduleClassify();
        }
      }) : el("span", { "class": "vft-plan-unpick-spacer" });
      var row = el("div", { "class": "vft-plan-colrow" + (p.picked ? " vft-plan-picked" : ""),
                            onmouseenter: function () { imp.hover = k; drawView(); },
                            onmouseleave: function () { imp.hover = null; drawView(); } }, [
        el("span", { "class": "vft-plan-swatch", style: "background:" + hex(p.rgb) + ";" }),
        el("span", { "class": "vft-plan-share", text: (100 * p.share).toFixed(1) + " %" }),
        sel, remove
      ]);
      imp.rowEls.push(row);
      rows.appendChild(row);
    });
  }

  function flashRow(k) {
    var row = imp.rowEls[k];
    if (!row) return;
    row.scrollIntoView({ block: "nearest" });
    row.classList.remove("vft-plan-flash");
    void row.offsetWidth;
    row.classList.add("vft-plan-flash");
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
   * the chosen materials. Hovering a colour row blanks every other colour. */
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
          if (imp.hover != null && imp.pal.labels[si] !== imp.hover) {
          //hovering a colour shows that colour alone
          d[o] = d[o + 1] = d[o + 2] = 255;
        } else if (fade) {
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
    removeFloating();
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
      /* above Leaflet's control corners (z-index 1000), or a handle under the zoom
       * buttons or the attribution could not be grabbed. The frame itself takes
       * no pointer events, so the controls still work through it. */
      ".vft-plan-floating{position:absolute;pointer-events:none;z-index:1001;",
      "  box-shadow:0 0 0 2px #069869;}",
      /* the zoom animation: handles glide with Leaflet's own transition */
      ".leaflet-zoom-anim .vft-plan-floating,.leaflet-zoom-anim .vft-plan-mover{",
      "  transition:left .25s cubic-bezier(0,0,.25,1),top .25s cubic-bezier(0,0,.25,1),",
      "  width .25s cubic-bezier(0,0,.25,1),height .25s cubic-bezier(0,0,.25,1);}",
      /* just above the image, still below the controls */
      ".vft-plan-mover{position:absolute;z-index:651;cursor:move;touch-action:none;}",
      ".vft-plan-mover.vft-plan-moving{cursor:grabbing;}",
      ".vft-plan-handle{position:absolute;width:16px;height:16px;box-sizing:border-box;",
      "  background:#fff;border:3px solid #069869;border-radius:50%;pointer-events:auto;",
      "  touch-action:none;box-shadow:0 1px 3px rgba(0,0,0,.4);}",
      ".vft-plan-handle:hover{background:#069869;}",
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
      "@keyframes vftPlanFlash{from{background:#ffe08a;}to{background:transparent;}}",
      ".vft-plan-flash{animation:vftPlanFlash 1.2s ease-out;}",
      ".vft-plan-pickbtn.active{background:#069869;color:#fff;}",
      ".vft-plan-view.vft-plan-picking{cursor:copy;outline:2px dashed #069869;outline-offset:2px;}",
      ".vft-plan-unpick,.vft-plan-unpick-spacer{width:24px;flex:none;}",
      ".vft-plan-picked .vft-plan-swatch{box-shadow:0 0 0 2px #069869;}"
    ].join("\n") }));
  }

  init();
})();
