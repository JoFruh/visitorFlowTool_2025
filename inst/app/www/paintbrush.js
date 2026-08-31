/* Client-side paint rendering for the heat-mitigation map (newVersions, context 4).
 *
 * The browser owns the display. Painted cells live here, in the same global 5 m
 * EPSG:2056 grid that R persists them to, and are drawn straight into two
 * georeferenced Leaflet canvas layers. R is never in the path between a brush
 * stroke and a visible pixel: it only receives run-encoded cell deltas on a
 * debounced flush, so that the raster survives a reload or a version switch.
 *
 * Coordinates. R sends one closed-form fit (paintTransform2056) mapping Web
 * Mercator metres to EPSG:2056, accurate to millimetres over tens of kilometres.
 * Brush strokes go through it directly; drawing linearises it about the viewport
 * centre and composes it with Leaflet's own (exactly affine) Mercator -> pixel
 * mapping, so a redraw costs one drawImage per CHUNK-square chunk no matter how
 * much has been painted.
 *
 * Opacity lives on the map *panes*, not on the layers, and chunk pixels are fully
 * opaque. That is what keeps overlapping strokes from compounding into darker
 * patches, and what makes the canopy toggle a single CSS write.
 */
(function () {
  "use strict";

  var MAP_ID      = "newVersions-versionMap";
  //cells per chunk side. A chunk covers CHUNK*res metres, so at 1 m this is
  //512 m rather than the 1280 m it was at 5 m: enough to keep a viewport-sized
  //redraw to ~100 drawImage calls. The total canvas memory for a view is set by
  //its cell count, not by this, so 1 m painting costs ~25x a 5 m view whatever
  //value is chosen here - CHUNK only trades draw calls against canvas size.
  var CHUNK       = 512;
  //2^22; key = row*COL_BITS + col, ordering keys row-major. This has to exceed
  //the largest global column index, and at PAINT_RES = 1 that is the LV95
  //easting itself: Switzerland runs to E 2840000, so 2^21 would overflow col
  //into the row field and silently corrupt every stroke in the country.
  var COL_BITS    = 4194304;
  var FLUSH_IDLE  = 800;        //ms of no painting before the delta is sent to R
  var ACK_TIMEOUT = 8000;       //ms before an unacked flush is put back in the queue
  var PANES       = { ground: "paintPaneGround", canopy: "paintPaneCanopy" };

  var state = {
    map:          null,
    res:          1,          //overwritten by msg.res from paintInitPayload
    transform:    null,
    colors:       {},
    levels:       {},   //category id -> "ground" | "canopy" | "both"
    opacity:      { ground: 0.5, groundDimmed: 0.2, canopy: 0.7 },
    grids:        { ground: new PaintGrid(), canopy: new PaintGrid() },
    bases:        { ground: new BaseGrid(),  canopy: new BaseGrid()  },
    layers:       { ground: null, canopy: null },
    lastBase:     null,
    erasing:      false,
    overlay:      null,
    canopyActive: false,
    active:       false,
    //armed, but looking only. The original scenario is the surveyed baseline
    //every version is compared against, so it is displayed and not painted -
    //the same rule the other two contexts apply to the network and the parking
    //areas. Separate from `active` because active=false hides both panes (see
    //applyLevelStyles), which would take the land cover off the screen with the
    //brush and leave the original looking like an empty map.
    readonly:     false,
    //rolling record of the events that decide whether paint is armed and drawn.
    //Reasoning backwards from a single end-state flag repeatedly gave the wrong
    //answer here, because the flag says what is true now and not who last set
    //it; this says who set it.
    trace:        [],
    brushRadius:  18,
    categoryId:   1,
    version:      null,
    pending:      { ground: new Map(), canopy: new Map() },
    inflight:     new Map(),   //seq -> {cells, timer}, awaiting acknowledgement
    seq:          0,
    flushTimer:   null,
    lastLoad:     null,
    LayerClass:   null
  };

  // ── The painted grid ────────────────────────────────────────────────────────

  /* Chunk allocation, shared by the painted grid and the land cover baseline so
   * both land on the same chunk lattice - which is what lets one layer class
   * draw either of them without knowing which it has. */
  function chunkAt(store, cx, cy) {
    var key = cx * 65536 + cy;
    var ch  = store.get(key);
    if (!ch) {
      var cv = document.createElement("canvas");
      cv.width = CHUNK; cv.height = CHUNK;
      ch = { cx: cx, cy: cy, canvas: cv, ctx: cv.getContext("2d") };
      store.set(key, ch);
    }
    return ch;
  }

  /* Cell values plus the offscreen canvases they are drawn on. Chunks are
   * CHUNK cells square, so a park spans a handful of them. */
  function PaintGrid() {
    this.cells  = new Map();   //key -> categoryId
    this.chunks = new Map();   //chunkKey -> {cx, cy, canvas, ctx}
  }

  PaintGrid.prototype.reset = function () {
    this.cells.clear();
    this.chunks.clear();
  };

  PaintGrid.prototype.chunkAt = function (cx, cy) {
    return chunkAt(this.chunks, cx, cy);
  };

  // ── The land cover baseline ─────────────────────────────────────────────────

  /* The surveyed land cover under the paint, decoded from a PNG in which the
   * *pixel value is the class id*.
   *
   * Why an image and not the row-run encoding the painted grid uses: runs were
   * designed for brush strokes, which are contiguous blobs. Real 1 m land cover
   * is the opposite - every kerb and building edge breaks a run - so one square
   * kilometre encodes to ~150k runs and megabytes of JSON. The same square as a
   * PNG is ~150 KB, because a 9-value class raster is exactly what PNG's filters
   * compress well.
   *
   * It exposes `chunks` and nothing else, which is the whole interface the
   * layer class needs. The baseline is immutable once decoded: paint never
   * carves holes in it, it is simply drawn over in the shared canvas, so
   * erasing a stroke reveals it again with nothing to restore. That is why
   * there is no `cells` map and no per-cell id array - the baseline is never
   * edited, diffed or sent back, so nothing has to remember what it held. */
  function BaseGrid() {
    this.chunks = new Map();
  }

  BaseGrid.prototype.reset = function () {
    this.chunks.clear();
  };

  /* CSS colour -> [r,g,b], resolved by the browser so "lightgreen" and "#6aa84f"
   * work the same way. One 1x1 canvas, memoised per id. */
  var rgbCache = {};
  function rgbFor(id) {
    if (rgbCache[id]) return rgbCache[id];
    var color = state.colors[id];
    if (!color) return null;
    var cv = document.createElement("canvas");
    cv.width = cv.height = 1;
    var cx = cv.getContext("2d");
    cx.fillStyle = color;
    cx.fillRect(0, 0, 1, 1);
    var d = cx.getImageData(0, 0, 1, 1).data;
    rgbCache[id] = [d[0], d[1], d[2]];
    return rgbCache[id];
  }

  /* Decode one baseline image onto the chunk lattice.
   *
   * `col0`/`rowTop` are the global grid indices of the image's top-left cell, in
   * the same convention rasterToRuns() uses, so the baseline lands cell-for-cell
   * on top of anything painted. Image row 0 is north, matching terra's row
   * order; grid rows count north, canvas rows count south, hence the py flip -
   * the same flip fillRun() does.
   *
   * Pixels are written into per-chunk ImageData buffers and blitted once at the
   * end. Per-pixel fillRect() would be millions of canvas calls for an AOI this
   * size; this is two passes over a typed array. */
  BaseGrid.prototype.loadImage = function (img, col0, rowTop) {
    this.reset();
    var w = img.width, h = img.height;
    if (!w || !h) return;

    var tmp = document.createElement("canvas");
    tmp.width = w; tmp.height = h;
    var tctx = tmp.getContext("2d");
    tctx.drawImage(img, 0, 0);

    //Read the image back in horizontal strips rather than in one getImageData.
    //RGBA is 4 bytes a pixel, so a whole-image read is 4x the window - 86 MB for
    //a 5 km study area at 1 m, allocated in one go purely to be thrown away. A
    //strip bounds that to w x STRIP x 4 regardless of how tall the window is.
    //
    //Chunk canvases, by contrast, are only created where a non-zero pixel
    //actually lands, so scattered study areas cost their content and not their
    //bounding box.
    var STRIP = 256;
    var bufs = new Map();   //chunkKey -> {ch, data}

    for (var y0 = 0; y0 < h; y0 += STRIP) {
      var sh  = Math.min(STRIP, h - y0);
      var src = tctx.getImageData(0, y0, w, sh).data;

      for (var jj = 0; jj < sh; jj++) {
        var grow = rowTop - (y0 + jj);
        var cy   = Math.floor(grow / CHUNK);
        var py   = CHUNK - 1 - (grow - cy * CHUNK);
        for (var i = 0; i < w; i++) {
          var id = src[(jj * w + i) * 4];     //grayscale: R channel is the class id
          if (!id) continue;                  //0 = unclassified / open sky = transparent
          var rgb = rgbFor(id);
          if (!rgb) continue;

          var gcol = col0 + i;
          var cx   = Math.floor(gcol / CHUNK);
          var key  = cx * 65536 + cy;
          var buf  = bufs.get(key);
          if (!buf) {
            var ch = chunkAt(this.chunks, cx, cy);
            buf = { ch: ch, data: ch.ctx.createImageData(CHUNK, CHUNK) };
            bufs.set(key, buf);
          }
          var o = (py * CHUNK + (gcol - cx * CHUNK)) * 4;
          buf.data.data[o]     = rgb[0];
          buf.data.data[o + 1] = rgb[1];
          buf.data.data[o + 2] = rgb[2];
          buf.data.data[o + 3] = 255;
        }
      }
    }
    bufs.forEach(function (b) { b.ch.ctx.putImageData(b.data, 0, 0); });
  };

  /* Paint one horizontal run of cells. The chunk canvas is filled span-wise
   * (cheap, and repainting a cell that already had this color is harmless),
   * while the value map is updated per cell so only genuine *changes* reach the
   * pending delta. */
  PaintGrid.prototype.fillRun = function (row, colStart, colEnd, catId, color, pending) {
    var cy  = Math.floor(row / CHUNK);
    var py  = CHUNK - 1 - (row - cy * CHUNK);   //grid rows go north, canvas rows go south
    var col = colStart;

    while (col <= colEnd) {
      var cx     = Math.floor(col / CHUNK);
      var segEnd = Math.min(colEnd, (cx + 1) * CHUNK - 1);
      var ch     = this.chunkAt(cx, cy);

      ch.ctx.fillStyle = color;
      ch.ctx.fillRect(col - cx * CHUNK, py, segEnd - col + 1, 1);

      for (var c = col; c <= segEnd; c++) {
        var key = row * COL_BITS + c;
        if (this.cells.get(key) !== catId) {
          this.cells.set(key, catId);
          if (pending) pending.set(key, catId);
        }
      }
      col = segEnd + 1;
    }
  };

  /* Erase a horizontal run of painted cells.
   *
   * The pending value is 0, not "absent": R has to be told the cell was cleared,
   * and rasterToRuns() already treats 0 as unpainted, so 0 is the erase symbol
   * on both sides. The cells entry is deleted rather than set to 0 so that
   * repainting the same material afterwards still counts as a change. */
  PaintGrid.prototype.clearRun = function (row, colStart, colEnd, pending) {
    var cy  = Math.floor(row / CHUNK);
    var py  = CHUNK - 1 - (row - cy * CHUNK);
    var col = colStart;

    while (col <= colEnd) {
      var cx     = Math.floor(col / CHUNK);
      var segEnd = Math.min(colEnd, (cx + 1) * CHUNK - 1);
      var ch     = this.chunks.get(cx * 65536 + cy);
      if (ch) ch.ctx.clearRect(col - cx * CHUNK, py, segEnd - col + 1, 1);

      for (var c = col; c <= segEnd; c++) {
        var key = row * COL_BITS + c;
        if (this.cells.has(key)) {
          this.cells.delete(key);
          if (pending) pending.set(key, 0);
        }
      }
      col = segEnd + 1;
    }
  };

  /* Rebuild a grid from R's row-run encoding (a version's stored raster). */
  PaintGrid.prototype.loadRuns = function (entries) {
    this.reset();
    if (!entries) return;
    for (var i = 0; i < entries.length; i++) {
      var id    = entries[i].id;
      var runs  = entries[i].runs || [];
      var color = state.colors[id];
      if (!color) continue;
      for (var j = 0; j + 2 < runs.length; j += 3) {
        this.fillRun(runs[j], runs[j + 1], runs[j + 1] + runs[j + 2] - 1, id, color, null);
      }
    }
  };

  // ── Grid -> screen ──────────────────────────────────────────────────────────

  /* Mercator metres -> EPSG:2056, second-order fit supplied by R
   * (paintTransform2056). Coefficients are in Mercator kilometres relative to the
   * reference point, against the basis (1, u, v, u^2, u*v, v^2). */
  function mercatorToLV95(mx, my) {
    var T = state.transform;
    var u = (mx - T.X0) / 1000, v = (my - T.Y0) / 1000;
    var b = [1, u, v, u * u, u * v, v * v];
    var E = 0, N = 0;
    for (var i = 0; i < 6; i++) { E += T.E[i] * b[i]; N += T.N[i] * b[i]; }
    return { E: E, N: N };
  }

  /* d(E,N)/d(Mercator metres), by differentiating the fit above. */
  function lv95Jacobian(mx, my) {
    var T = state.transform;
    var u = (mx - T.X0) / 1000, v = (my - T.Y0) / 1000;
    return {
      dEdX: (T.E[1] + 2 * T.E[3] * u + T.E[4] * v) / 1000,
      dEdY: (T.E[2] + T.E[4] * u + 2 * T.E[5] * v) / 1000,
      dNdX: (T.N[1] + 2 * T.N[3] * u + T.N[4] * v) / 1000,
      dNdY: (T.N[2] + T.N[4] * u + 2 * T.N[5] * v) / 1000
    };
  }

  /* Mercator metres -> container pixels: container.x = k*X + tx, container.y = -k*Y + ty.
   * Built from map.project() rather than latLngToContainerPoint(), because the
   * latter rounds to whole pixels and would wreck the scale estimate. */
  function mercatorToContainer(map, ax, ay) {
    var origin = map.getPixelOrigin();
    var off    = map.layerPointToContainerPoint(L.point(0, 0));
    var p0     = map.project(L.CRS.EPSG3857.unproject(L.point(ax, ay)));
    var p1     = map.project(L.CRS.EPSG3857.unproject(L.point(ax + 1000, ay)));
    var k      = (p1.x - p0.x) / 1000;
    return {
      k:  k,
      tx: p0.x - origin.x + off.x - k * ax,
      ty: p0.y - origin.y + off.y + k * ay
    };
  }

  /* The chunk-pixel -> container-pixel affine.
   *
   * Drawing has to be one canvas transform per chunk, so the 2056 -> Mercator
   * direction is linearised - but only across the current viewport, and about its
   * own centre, where the second-order fit's Jacobian is exact. The residual is
   * ~0.01 m at street zoom and ~0.1 m across a 2 km view, i.e. far below a pixel,
   * so what is drawn sits exactly where it was painted.
   *
   * The linear part is shared by every chunk; only the translation differs. */
  function chunkTransform(map) {
    if (!state.transform) return null;
    var res = state.res;
    var mc  = L.CRS.EPSG3857.project(map.getCenter());
    var m   = mercatorToContainer(map, mc.x, mc.y);
    var k   = m.k;

    var J   = lv95Jacobian(mc.x, mc.y);
    var det = J.dEdX * J.dNdY - J.dEdY * J.dNdX;
    if (!det) return null;
    var b11 =  J.dNdY / det, b12 = -J.dEdY / det;   //dX = b11*(E-Ec) + b12*(N-Nc)
    var b21 = -J.dNdX / det, b22 =  J.dEdX / det;   //dY = b21*(E-Ec) + b22*(N-Nc)
    var C   = mercatorToLV95(mc.x, mc.y);

    return {
      a:  k * b11 * res,
      b: -k * b21 * res,
      c: -k * b12 * res,
      d:  k * b22 * res,
      originFor: function (cx, cy) {
        var eC = res * cx * CHUNK - C.E;
        var nC = res * (cy + 1) * CHUNK - C.N;
        return {
          e:  k * (mc.x + b11 * eC + b12 * nC) + m.tx,
          f: -k * (mc.y + b21 * eC + b22 * nC) + m.ty
        };
      }
    };
  }

  /* Defined lazily: this file is loaded from the UI, which can run before
   * Leaflet's own assets are on the page. */
  function paintLayerClass() {
    if (state.LayerClass) return state.LayerClass;

    state.LayerClass = L.Layer.extend({
      /* `grids` is drawn in order into ONE canvas: land cover baseline first,
       * then the paint over it.
       *
       * One canvas, not two stacked panes, because a pane carries the layer
       * opacity. Two translucent panes would leave a painted cell showing
       * paint-over-baseline while its neighbour showed baseline alone, so the
       * two would never look alike. Composited first and made translucent once,
       * a painted grass cell is indistinguishable from a surveyed grass cell -
       * which is the point: the map should read as one surface, not as edits
       * highlighted against a backdrop.
       *
       * Keeping them as separate *grids* is what the eraser needs. Clearing a
       * paint cell reveals the baseline underneath on the next redraw, with
       * nothing to restore, because the baseline was never written to. */
      initialize: function (grids, paneName) {
        this._grids = [].concat(grids);
        L.setOptions(this, { pane: paneName });
      },

      onAdd: function () {
        var canvas = this._canvas = L.DomUtil.create("canvas", "paint-grid-layer");
        canvas.style.position      = "absolute";
        canvas.style.pointerEvents = "none";
        //REQUIRED for _animateZoom: this class is what gives Leaflet's animated
        //elements `transform-origin: 0 0`. Without it the browser scales the canvas
        //about its centre, and the paint slides towards the bottom-right when
        //zooming out and the top-left when zooming in. The class also carries the
        //transition that keeps the canvas in step with the tiles.
        if (this._zoomAnimated) L.DomUtil.addClass(canvas, "leaflet-zoom-animated");
        this.getPane().appendChild(canvas);
        this._ctx = canvas.getContext("2d");
        this._reset();
      },

      onRemove: function () {
        L.DomUtil.remove(this._canvas);
      },

      /* moveend rather than move, as L.Canvas does: during a drag Leaflet
       * translates the whole map pane, so the canvas travels with it and only
       * needs redrawing once the pane position is reset. Redrawing on every move
       * would also fight _animateZoom for control of the canvas transform. */
      getEvents: function () {
        var events = { viewreset: this._reset, resize: this._reset, moveend: this._reset, zoomend: this._reset };
        if (this._zoomAnimated) events.zoomanim = this._animateZoom;
        return events;
      },

      /* Ride the tiles' zoom animation: put the canvas's top-left where its
       * geographic anchor will be at the target zoom, and scale by the zoom ratio.
       * Anchor and zoom are the ones the canvas was last *drawn* at, not the map's
       * current ones, so this stays correct however the animation was triggered. */
      _animateZoom: function (e) {
        if (!this._canvas || !this._anchor) return;
        var map    = this._map,
            scale  = map.getZoomScale(e.zoom, this._drawZoom),
            offset = map.project(this._anchor, e.zoom)
                        .subtract(map._getNewPixelOrigin(e.center, e.zoom));
        L.DomUtil.setTransform(this._canvas, offset, scale);
      },

      _reset: function () {
        //getEvents() is wired up before onAdd(), so the canvas may not exist yet
        if (!this._map || !this._canvas) return;
        var map = this._map, size = map.getSize(), canvas = this._canvas;
        if (canvas.width !== size.x)  canvas.width  = size.x;
        if (canvas.height !== size.y) canvas.height = size.y;
        //what the canvas's top-left pixel means, for _animateZoom to anchor on
        this._anchor   = map.containerPointToLatLng([0, 0]);
        this._drawZoom = map.getZoom();
        //setPosition writes a plain translate, clearing any scale left by _animateZoom
        L.DomUtil.setPosition(canvas, map.containerPointToLayerPoint([0, 0]));
        this.redraw();
      },

      /* Painting fires several pointermove events per frame; there is no point
       * repainting the canvas more often than the screen updates. */
      requestRedraw: function () {
        if (this._rafPending) return;
        var self = this;
        this._rafPending = true;
        L.Util.requestAnimFrame(function () {
          self._rafPending = false;
          self.redraw();
        });
      },

      redraw: function () {
        if (!this._map || !this._canvas) return;
        var ctx = this._ctx, canvas = this._canvas;
        ctx.setTransform(1, 0, 0, 1, 0, 0);
        ctx.clearRect(0, 0, canvas.width, canvas.height);

        var t = chunkTransform(this._map);
        if (!t) return;

        //below one screen pixel per cell the grid is sub-pixel; smoothing then
        //averages it instead of dropping most of it
        ctx.imageSmoothingEnabled = Math.hypot(t.a, t.b) < 1;

        //baseline first, paint second: later grids occlude earlier ones
        this._grids.forEach(function (grid) {
          grid.chunks.forEach(function (ch) {
            var o = t.originFor(ch.cx, ch.cy);
            ctx.setTransform(t.a, t.b, t.c, t.d, o.e, o.f);
            ctx.drawImage(ch.canvas, 0, 0);
          });
        });
        ctx.setTransform(1, 0, 0, 1, 0, 0);
      }
    });
    return state.LayerClass;
  }

  function redrawLayers() {
    if (state.layers.ground) state.layers.ground.redraw();
    if (state.layers.canopy) state.layers.canopy.redraw();
  }

  // ── Painting ────────────────────────────────────────────────────────────────

  /* Screen -> fractional grid cell, straight through the second-order fit: this
   * is where absolute accuracy matters, since it decides which cell R will store. */
  function containerToCell(map, x, y) {
    var m = L.CRS.EPSG3857.project(map.containerPointToLatLng(L.point(x, y)));
    var p = mercatorToLV95(m.x, m.y);
    return { c: p.E / state.res, r: p.N / state.res };
  }

  function metresPerPixel(map, x, y) {
    return map.distance(map.containerPointToLatLng(L.point(x, y)),
                        map.containerPointToLatLng(L.point(x + 1, y)));
  }

  function activeLevel() {
    return state.canopyActive ? "canopy" : "ground";
  }

  /* Which grids a material's strokes go into. Materials declare their own level
   * (PAINT_CATEGORIES), so this does not depend on the switch: a "both" material
   * such as a building block fills the ground and canopy grids together, whichever
   * level is being edited. Falls back to the switch only if R has not sent the
   * level table yet. */
  function targetLevels(catId) {
    var level = state.levels[catId];
    if (level === "both") return ["ground", "canopy"];
    if (level === "canopy" || level === "ground") return [level];
    return [activeLevel()];
  }

  /* Stamp a disc in cell space. Cell centres are at (col+0.5, row+0.5), so the
   * result is exactly the set of grid cells whose centre falls inside the brush -
   * the same binary rule R's raster would have applied. */
  function stampDisc(level, fc, fr, rCells, catId, color, pending) {
    var grid = state.grids[level];
    var r2 = rCells * rCells;
    var r0 = Math.floor(fr - rCells), r1 = Math.floor(fr + rCells);
    for (var row = r0; row <= r1; row++) {
      var dy = (row + 0.5) - fr;
      var w2 = r2 - dy * dy;
      if (w2 < 0) continue;
      var w  = Math.sqrt(w2);
      var cs = Math.ceil(fc - w - 0.5);
      var ce = Math.floor(fc + w - 0.5);
      if (ce < cs) continue;

      //only the paint grid is ever touched. The baseline underneath is left
      //exactly as decoded - hidden by opaque paint, revealed again by the eraser.
      if (state.erasing) grid.clearRun(row, cs, ce, pending);
      else               grid.fillRun(row, cs, ce, catId, color, pending);
    }
  }

  /* Stamp along the segment between two pointer samples. Without this, fast
   * strokes leave gaps wherever the browser skipped a pointermove. */
  function stampSegment(map, from, to) {
    var catId = state.categoryId;
    var color = state.colors[catId];
    //the eraser needs no material, so a missing colour only blocks painting
    if ((!color && !state.erasing) || !state.transform) return;

    var rCells = (state.brushRadius * metresPerPixel(map, to.x, to.y)) / state.res;
    if (!(rCells > 0)) return;

    var a = containerToCell(map, from.x, from.y);
    var b = containerToCell(map, to.x, to.y);
    var dc = b.c - a.c, dr = b.r - a.r;
    var dist  = Math.sqrt(dc * dc + dr * dr);
    var steps = Math.max(1, Math.ceil(dist / Math.max(0.5, rCells * 0.5)));

    //erasing acts on the level being edited, not on the selected material's
    //level: rubbing out a building should not also clear the canopy above it
    //unless that is the level you are on
    var levels = state.erasing ? [activeLevel()] : targetLevels(catId);
    levels.forEach(function (level) {
      var pending = state.pending[level];
      for (var i = 1; i <= steps; i++) {
        var t = i / steps;
        stampDisc(level, a.c + dc * t, a.r + dr * t, rCells, catId, color, pending);
      }
      if (state.layers[level]) state.layers[level].requestRedraw();
    });
    scheduleFlush();
  }

  // ── Sending deltas to R ─────────────────────────────────────────────────────

  /* Keys sort row-major (key = row*COL_BITS + col), so consecutive keys are
   * horizontal neighbours and collapse straight into (row, colStart, count). */
  function encodeDelta(pending) {
    if (!pending || pending.size === 0) return null;

    var byCat = new Map();
    pending.forEach(function (id, key) {
      var arr = byCat.get(id);
      if (!arr) { arr = []; byCat.set(id, arr); }
      arr.push(key);
    });

    var out = [];
    byCat.forEach(function (keys, id) {
      keys.sort(function (a, b) { return a - b; });
      var runs = [], i = 0;
      while (i < keys.length) {
        var start = keys[i], j = i + 1;
        while (j < keys.length && keys[j] === keys[j - 1] + 1) j++;
        runs.push(Math.floor(start / COL_BITS), start % COL_BITS, j - i);
        i = j;
      }
      out.push({ id: id, runs: runs });
    });
    return out;
  }

  function scheduleFlush() {
    if (state.flushTimer) clearTimeout(state.flushTimer);
    state.flushTimer = setTimeout(flush, FLUSH_IDLE);
  }

  /* Send everything painted since the last flush.
   *
   * Flushes are not serialised against each other: a delta is a set of cell
   * writes, so applying one twice is a no-op and several may be in flight at
   * once. That matters because the commonest reason to flush is a click that is
   * about to make R read the raster - waiting for an earlier ack first would be
   * exactly the wrong moment to stall.
   *
   * ORDERING: R reads paintedRaster/canopyRaster synchronously when the user
   * switches version or context, and cannot ask for a flush mid-observer. So the
   * flush has to have happened already - hence the document-level pointerdown
   * listener installed in capture phase below, which runs before Shiny's input
   * bindings see the click on any control. */
  function flush() {
    if (state.flushTimer) { clearTimeout(state.flushTimer); state.flushTimer = null; }

    var ground = encodeDelta(state.pending.ground);
    var canopy = encodeDelta(state.pending.canopy);
    if (!ground && !canopy) return;

    state.seq += 1;
    var seq   = state.seq;
    var cells = state.pending;
    state.pending = { ground: new Map(), canopy: new Map() };

    state.inflight.set(seq, {
      cells: cells,
      timer: setTimeout(function () { requeue(seq); }, ACK_TIMEOUT)
    });

    Shiny.setInputValue("newVersions-paintCells", {
      seq:     seq,
      version: state.version,
      ground:  ground || [],
      canopy:  canopy  || []
    }, { priority: "event" });
  }

  /* No ack arrived (the observer errored, or the connection stalled): put the
   * cells back at the *bottom* of the queue, so anything repainted since keeps
   * its newer value. */
  function requeue(seq) {
    var entry = state.inflight.get(seq);
    if (!entry) return;
    state.inflight.delete(seq);
    ["ground", "canopy"].forEach(function (level) {
      entry.cells[level].forEach(function (id, key) {
        if (!state.pending[level].has(key)) state.pending[level].set(key, id);
      });
    });
    flush();
  }

  function dropInflight() {
    state.inflight.forEach(function (entry) { clearTimeout(entry.timer); });
    state.inflight.clear();
  }

  // ── Cursor and pointer input ────────────────────────────────────────────────

  function updateCursor() {
    if (!state.overlay) return;
    //readonly as well as disarmed: a brush cursor over a scenario that takes no
    //strokes is an invitation to try
    if (!state.active || state.readonly) { state.overlay.style.cursor = "default"; return; }
    var r    = state.brushRadius;
    var size = r * 2 + 4;
    //the eraser shows an empty dashed ring: nothing is being added, and the
    //brush must not look like it is about to lay down whatever material happens
    //to still be selected underneath
    var svg  = "<svg xmlns='http://www.w3.org/2000/svg' width='" + size + "' height='" + size + "'>"
             + "<circle cx='" + size / 2 + "' cy='" + size / 2 + "' r='" + r + "' "
             + "stroke='black' stroke-width='1.5' "
             + (state.erasing ? "stroke-dasharray='4 3' fill='none'"
                              : "fill='" + (state.colors[state.categoryId] || "#888") + "' fill-opacity='0.35'")
             + "/></svg>";
    state.overlay.style.cursor =
      "url(\"data:image/svg+xml," + encodeURIComponent(svg) + "\") " + size / 2 + " " + size / 2 + ", crosshair";
  }

  /* A transparent hit target over the map. Its z-index keeps it above the map
   * panes but below Leaflet's controls, and pointer-events are off unless paint
   * mode is on - so when the brush is disarmed the map behaves completely
   * normally. */
  function buildOverlay(map) {
    var el = document.createElement("div");
    el.className = "paint-input-overlay";
    el.style.position      = "absolute";
    el.style.top           = "0";
    el.style.left          = "0";
    el.style.width         = "100%";
    el.style.height        = "100%";
    el.style.zIndex        = "500";
    el.style.pointerEvents = "none";
    map.getContainer().appendChild(el);

    var painting = false, panning = false, last = null, panLast = null;

    function pt(e) {
      var rect = map.getContainer().getBoundingClientRect();
      return L.point(e.clientX - rect.left, e.clientY - rect.top);
    }

    el.addEventListener("contextmenu", function (e) { e.preventDefault(); });

    el.addEventListener("pointerdown", function (e) {
      if (!state.active) return;
      if (e.button === 2 || e.button === 1) {
        //right/middle drag pans: Leaflet's own dragging only handles the left
        //button, and the left button is the brush
        panning = true; panLast = pt(e);
        el.setPointerCapture(e.pointerId);
        e.preventDefault(); e.stopPropagation();
        return;
      }
      if (e.button !== 0) return;
      e.preventDefault(); e.stopPropagation();
      el.setPointerCapture(e.pointerId);
      painting = true;
      last = pt(e);
      stampSegment(map, last, last);
    });

    el.addEventListener("pointermove", function (e) {
      if (panning) {
        var now = pt(e);
        map.panBy(L.point(panLast.x - now.x, panLast.y - now.y), { animate: false });
        panLast = now;     //container point is unchanged by the pan; the delta is per-move
        e.preventDefault(); e.stopPropagation();
        return;
      }
      if (!painting || !state.active) return;
      e.preventDefault(); e.stopPropagation();
      var now = pt(e);
      stampSegment(map, last, now);
      last = now;
    });

    function endPointer(e) {
      if (painting) scheduleFlush();
      painting = false;
      panning  = false;
      if (e && e.pointerId != null && el.hasPointerCapture && el.hasPointerCapture(e.pointerId)) {
        el.releasePointerCapture(e.pointerId);
      }
    }
    el.addEventListener("pointerup", endPointer);
    el.addEventListener("pointercancel", endPointer);
    //leaving the map is a good moment to send, but it must not cut a stroke short:
    //with pointer capture the stroke legitimately continues outside the container
    el.addEventListener("pointerleave", function () {
      if (!painting && !panning) flush();
    });

    return el;
  }

  // ── Wiring to the Leaflet widget ────────────────────────────────────────────

  /* This file is loaded from the UI body, so Leaflet and htmlwidgets may not be
   * on the page yet the first few times the poll below runs. */
  function liveMap() {
    if (typeof HTMLWidgets === "undefined" || typeof L === "undefined") return null;
    var w = HTMLWidgets.find("#" + MAP_ID);
    var m = (w && w.getMap) ? w.getMap() : null;
    return (m && m.getContainer && m.getContainer()) ? m : null;
  }

  function ensurePane(map, name, zIndex) {
    var pane = map.getPane(name);
    if (!pane) {
      pane = map.createPane(name);
      pane.style.zIndex = zIndex;
    }
    pane.style.pointerEvents = "none";
    return pane;
  }

  /* The whole show/hide/dim story, in CSS on the panes.
   *
   * Ground is dimmed rather than removed while canopy is being edited, so you can
   * see what you are painting canopy over; canopy is hidden rather than removed
   * when editing ground. Both are hidden outside paint mode, since R only arms the
   * brush in context 4 and the painted layers have no business on the other
   * contexts' maps. Nothing here touches the grids, so none of it costs a redraw. */
  function applyLevelStyles() {
    var map = state.map;
    if (!map) return;
    /* One pane per level, carrying baseline and paint together, so a single
     * opacity applies to the finished surface and painted cells are
     * indistinguishable from surveyed ones. The only dimming is the ground
     * level while canopy is being edited. */
    var g = map.getPane(PANES.ground), c = map.getPane(PANES.canopy);
    if (g) {
      g.style.opacity = state.canopyActive ? state.opacity.groundDimmed : state.opacity.ground;
      g.style.display = state.active ? "" : "none";
    }
    if (c) {
      c.style.opacity = state.opacity.canopy;
      c.style.display = (state.active && state.canopyActive) ? "" : "none";
    }
  }

  /* Drop everything bound to the previous map instance. The old map is usually
   * already gone, so every step is best-effort - the point is not to leak a
   * second input overlay into a container Leaflet reused. */
  function detach() {
    if (state.overlay && state.overlay.parentNode) {
      state.overlay.parentNode.removeChild(state.overlay);
    }
    state.overlay = null;
    ["ground", "canopy"].forEach(function (level) {
      var layer = state.layers[level];
      if (layer && layer._map) { try { layer.remove(); } catch (err) { /* map already torn down */ } }
      state.layers[level] = null;
    });
    if (state.map) { try { state.map.off("zoomstart movestart", flush); } catch (err) { /* ditto */ } }
    state.map = null;
  }

  /* (Re)bind everything to the live map instance. renderLeaflet re-runs on every
   * context and version switch and builds a brand new map, taking our panes and
   * layers with it, so this is called whenever the instance changes. */
  function attach(map) {
    detach();
    state.map = map;
    ensurePane(map, PANES.ground, 415);
    ensurePane(map, PANES.canopy, 425);

    //one layer per level, drawing [baseline, paint] into a single canvas
    var Layer = paintLayerClass();
    state.layers.ground = new Layer([state.bases.ground, state.grids.ground], PANES.ground);
    state.layers.canopy = new Layer([state.bases.canopy, state.grids.canopy], PANES.canopy);
    state.layers.ground.addTo(map);
    state.layers.canopy.addTo(map);

    state.overlay = buildOverlay(map);
    //flush before the map moves: the delta is in grid coordinates, but the map
    //moving is a strong signal the user has stopped painting for now
    map.on("zoomstart movestart", flush);

    applyLevelStyles();
    applyActive();
    redrawLayers();
    trace("attach (active=" + state.active + ")");
    reportDebug("attach");
  }

  function applyActive() {
    applyLevelStyles();
    if (!state.overlay) return;
    //`readonly` takes the input overlay out of the way, so clicks and drags go
    //to the map (pan and zoom keep working) and no stroke is ever registered.
    //Pane visibility is left to `active` alone, so the layers stay on screen.
    state.overlay.style.pointerEvents = (state.active && !state.readonly) ? "auto" : "none";
    updateCursor();
  }

  /* A re-render either hands us a brand new map object or reuses the old one
   * after tearing its panes down, so identity alone is not enough to go on -
   * check that what we added is still in the document. */
  function syncMap() {
    var map = liveMap();
    if (!map) return;
    var ground = state.layers.ground;
    var stale  = map !== state.map ||
                 !state.overlay || !state.overlay.isConnected ||
                 !ground || !ground._canvas || !ground._canvas.isConnected;
    if (!stale) return;
    attach(map);
    //a re-render builds new panes and canvases, so both the paint and the
    //baseline have to be replayed onto them
    if (state.lastBase) loadBase(state.lastBase);
    if (state.lastLoad) loadPayload(state.lastLoad);
  }

  /* Cheap poll: catches the first render and every re-render, without depending
   * on the exact order of Shiny's output and custom-message flushes. */
  setInterval(syncMap, 250);

  /* Read-only view of the internals, for diagnosing from the browser console.
   *
   * Everything in this file is closed over by the IIFE, which is right for
   * production and useless when the map is blank and the question is *which*
   * invariant broke - whether the widget was found, whether the panes exist,
   * whether the coordinate fit ever arrived. Guessing at those from the R side
   * is not possible, so this hands them over on request. It exposes copies and
   * counts, never the grids themselves, so nothing here can be used to mutate
   * what is drawn.
   *
   * Its other job is telling you the file is current: if window.__paintDebug is
   * undefined, the browser is running a cached paintbrush.js. */
  window.__paintDebug = function () {
    var map = liveMap();
    var paneNames = ["paintPaneGround", "paintPaneCanopy"];
    var panes = {};
    paneNames.forEach(function (n) {
      var p = map && map.getPane ? map.getPane(n) : null;
      panes[n] = p ? { opacity: p.style.opacity, display: p.style.display,
                       zIndex: p.style.zIndex, connected: !!p.isConnected }
                   : "MISSING";
    });
    return {
      mapFound:     !!map,
      mapId:        MAP_ID,
      attached:     map === state.map && !!state.map,
      overlay:      !!state.overlay && !!state.overlay.isConnected,
      active:       state.active,
      readonly:     state.readonly,
      canopyActive: state.canopyActive,
      erasing:      state.erasing,
      res:          state.res,
      hasTransform: !!state.transform,
      colors:       Object.keys(state.colors).length,
      version:      state.version,
      panes:        panes,
      layers: {
        ground: !!state.layers.ground && !!state.layers.ground._canvas,
        canopy: !!state.layers.canopy && !!state.layers.canopy._canvas
      },
      chunks: {
        baseGround:  state.bases.ground.chunks.size,
        baseCanopy:  state.bases.canopy.chunks.size,
        paintGround: state.grids.ground.chunks.size,
        paintCanopy: state.grids.canopy.chunks.size
      },
      lastBase: state.lastBase ? { w: state.lastBase.w, h: state.lastBase.h,
                                   col0: state.lastBase.col0, rowTop: state.lastBase.rowTop,
                                   ground: !!state.lastBase.ground,
                                   canopy: !!state.lastBase.canopy } : null,
      trace: state.trace.slice()
    };
  };

  /* Send that snapshot to R, so it lands in the R console.
   *
   * The browser console is not always reachable - the app may be running in an
   * embedded viewer - and a blank map is precisely the moment the state is worth
   * seeing. R prints it in the one place that is always in front of you.
   *
   * Debounced, because the interesting moment is *after* things settle: attach,
   * the level switch and the baseline's asynchronous decode all fire within a
   * few hundred ms of each other, and only the last of them describes the state
   * you actually end up in. Wrapped in try/catch on principle: a diagnostic that
   * can break the thing it is diagnosing is worse than none. */
  /* Append to the rolling event record. Capped, because it lives for the life
   * of the page and is only ever read by a human. */
  function trace(what) {
    state.trace.push(what);
    if (state.trace.length > 25) state.trace.shift();
  }

  var debugTimer = null;
  function reportDebug(why) {
    if (debugTimer) clearTimeout(debugTimer);
    debugTimer = setTimeout(function () {
      debugTimer = null;
      try {
        Shiny.setInputValue("newVersions-paintDebug",
          { why: why, state: window.__paintDebug() }, { priority: "event" });
      } catch (e) { /* diagnostics must never take the app down */ }
    }, 900);
  }

  $(document).on("shiny:value", function (e) {
    if (e.name === MAP_ID) setTimeout(syncMap, 0);
  });

  //see the ORDERING note on flush(): this runs before Shiny's input bindings see
  //a click on the version list, the level switch or a material button
  document.addEventListener("pointerdown", function (e) {
    if (state.overlay && state.overlay.contains(e.target)) return;
    flush();
  }, true);

  window.addEventListener("beforeunload", flush);

  // ── Message handlers ────────────────────────────────────────────────────────

  /* Register a handler and record that the message arrived.
   *
   * Every inbound message is traced, not just the ones currently under
   * suspicion. Diagnosing this by reasoning about which messages *should* have
   * arrived went wrong repeatedly; the useful question is which ones did, and
   * that is only answerable by writing it down at the door.
   *
   * Bracket notation on purpose - it must survive a blanket rename of the
   * direct Shiny.addCustomMessageHandler calls below. */
  function on(name, fn) {
    Shiny["addCustomMessageHandler"](name, function (msg) {
      trace("msg " + name);
      return fn(msg);
    });
  }

  on("paint-grid-init", function (msg) {
    state.res       = msg.res;
    state.transform = msg.transform;
    state.colors    = msg.colors || {};
    state.levels    = msg.levels || {};
    state.opacity   = msg.opacity || state.opacity;
    applyLevelStyles();
    updateCursor();
    redrawLayers();
  });

  function loadPayload(msg) {
    //a re-render of the *same* version must not throw away paint that has not
    //reached R yet; only an actual version switch invalidates the queues
    if (msg.version !== state.version) {
      state.pending = { ground: new Map(), canopy: new Map() };
      dropInflight();
    }
    state.lastLoad = msg;
    state.version  = msg.version;
    state.grids.ground.loadRuns(msg.ground);
    state.grids.canopy.loadRuns(msg.canopy);
    redrawLayers();
  }

  on("paint-grid-load", function (msg) {
    loadPayload(msg);
  });

  /* The land cover baseline for this version's area. Images decode
   * asynchronously, so each one redraws as it arrives rather than waiting for
   * both - the ground layer is the one the user looks at first.
   *
   * A null/absent image clears that level, which is what makes "no rasters
   * built yet" and "AOI too big" degrade to the old blank canvas rather than
   * leaving a stale baseline from the previous version on screen. */
  function loadBase(msg) {
    state.lastBase = msg;
    ["ground", "canopy"].forEach(function (level) {
      var uri = msg && msg[level];
      if (!uri) {
        state.bases[level].reset();
        if (state.layers[level]) state.layers[level].redraw();
        return;
      }
      var img = new Image();
      img.onload = function () {
        state.bases[level].loadImage(img, msg.col0, msg.rowTop);
        if (state.layers[level]) state.layers[level].redraw();
        reportDebug("baseline-decoded");
      };
      //a PNG that fails to decode would otherwise be indistinguishable from one
      //that was never sent
      img.onerror = function () { reportDebug("baseline-decode-FAILED"); };
      img.src = uri;
    });
  }

  on("paint-base-load", function (msg) {
    loadBase(msg);
  });

  /* Eraser mode. The material stays selected underneath, so turning the eraser
   * off returns to whatever was being painted before. */
  on("set-paint-erase", function (msg) {
    flush();
    state.erasing = !!msg.erasing;
    updateCursor();
  });

  /* Drop every stroke on this version, revealing the land cover again.
   *
   * Clearing the paint grids is the whole job: the baseline was never altered,
   * so it needs no restoring and is not even touched here. R clears its own
   * rasters in the observer that sent this, so the two ends agree without the
   * browser having to enumerate what it is discarding. */
  on("paint-reset", function () {
    state.pending = { ground: new Map(), canopy: new Map() };
    dropInflight();
    ["ground", "canopy"].forEach(function (level) {
      state.grids[level].reset();
      if (state.layers[level]) state.layers[level].redraw();
    });
  });

  on("paint-cells-ack", function (msg) {
    var entry = state.inflight.get(msg.seq);
    if (entry) {
      clearTimeout(entry.timer);
      state.inflight.delete(msg.seq);
    }
  });

  on("set-paint-level", function (msg) {
    flush();
    state.canopyActive = !!msg.canopy;
    applyLevelStyles();
  });

  on("set-paint-active", function (active) {
    //the raw value as received, before coercion: a message arriving as [true]
    //or "false" would coerce differently than it reads
    trace("set-paint-active(" + JSON.stringify(active) + ")");
    if (!active) flush();   //leaving context 4 - get the last strokes to R first
    state.active = !!active;
    applyActive();
    reportDebug(state.active ? "paint-armed" : "paint-disarmed");
  });

  on("set-paint-readonly", function (msg) {
    var ro = !!(msg && msg.readonly);
    trace("set-paint-readonly(" + ro + ")");
    if (ro) flush();   //whatever was painted before the switch still belongs to R
    state.readonly = ro;
    applyActive();
    reportDebug(ro ? "paint-readonly" : "paint-writable");
  });

  on("set-paint-color", function (msg) {
    flush();
    state.categoryId = msg.id;
    updateCursor();
  });

  on("set-brush-radius", function (radius) {
    state.brushRadius = radius;
    updateCursor();
  });
})();
