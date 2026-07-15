var paintbrush = null;

function updateCursor() {
  var r = paintbrush.brushRadius;
  var size = r * 2 + 4;
  var svg = `<svg xmlns='http://www.w3.org/2000/svg' width='${size}' height='${size}'>`
    + `<circle cx='${size/2}' cy='${size/2}' r='${r}' `
    + `stroke='black' stroke-width='1.5' fill='rgba(${paintbrush.color},0.3)'/>`
    + `</svg>`;
  var encoded = encodeURIComponent(svg);
  paintbrush.canvas.style.cursor =
    `url("data:image/svg+xml,${encoded}") ${size/2} ${size/2}, crosshair`;
}

function drawCircle(x, y) {
  var ctx = paintbrush.ctx;
  var r   = paintbrush.brushRadius;
  ctx.beginPath();
  ctx.arc(x, y, r, 0, Math.PI * 2);
  ctx.fillStyle = "rgba(" + paintbrush.color + ",0.5)";
  ctx.fill();
  paintbrush.strokeActive = true;
  // track the stroke's bounding box (with brush-radius margin) so only the
  // painted region gets sent to R, not the whole canvas
  var m = r + 2;
  var sb = paintbrush.strokeBounds;
  if (!sb) {
    paintbrush.strokeBounds = { minX: x - m, minY: y - m, maxX: x + m, maxY: y + m };
  } else {
    if (x - m < sb.minX) sb.minX = x - m;
    if (y - m < sb.minY) sb.minY = y - m;
    if (x + m > sb.maxX) sb.maxX = x + m;
    if (y + m > sb.maxY) sb.maxY = y + m;
  }
}

function disableMapInteractions(map) {
  map.dragging.disable();
  map.touchZoom.disable();
  map.doubleClickZoom.disable();
  map.scrollWheelZoom.disable();
  map.boxZoom.disable();
  map.keyboard.disable();
}

function enableMapInteractions(map) {
  map.dragging.enable();
  map.touchZoom.enable();
  map.doubleClickZoom.enable();
  map.scrollWheelZoom.enable();
  map.boxZoom.enable();
  map.keyboard.enable();
}

function sendPaintStroke() {
  if (!paintbrush || !paintbrush.strokeBounds) return;
  // re-fetch the live map instance: the leaflet widget may have been fully
  // re-rendered since initPaintbrush ran, leaving paintbrush.map stale
  var mapWidget = HTMLWidgets.find("#newVersions-versionMap");
  var map = mapWidget ? mapWidget.getMap() : null;
  if (map) { paintbrush.map = map; } else { map = paintbrush.map; }

  // send only the painted region, not the whole canvas: processing cost in R
  // scales with the sent area (a 5m grid over the full viewport is huge when
  // zoomed out), so cropping here is what keeps stroke conversion fast
  var canvas = paintbrush.canvas;
  var sb = paintbrush.strokeBounds;
  var x0 = Math.max(0, Math.floor(sb.minX));
  var y0 = Math.max(0, Math.floor(sb.minY));
  var x1 = Math.min(canvas.width,  Math.ceil(sb.maxX));
  var y1 = Math.min(canvas.height, Math.ceil(sb.maxY));
  var w = x1 - x0;
  var h = y1 - y0;
  if (w <= 0 || h <= 0) return;

  // downscale before encoding: R snaps the final raster to a fixed 5m grid
  // (buildStrokeTemplate), so sending pixel-for-pixel screen resolution just
  // wastes encode/transfer/decode/classification time on precision that gets
  // thrown away anyway. Capping the longest side bounds that cost regardless
  // of stroke size or zoom level.
  var MAX_DIM = 250;
  var scale = Math.min(1, MAX_DIM / Math.max(w, h));
  var outW = Math.max(1, Math.round(w * scale));
  var outH = Math.max(1, Math.round(h * scale));

  var crop = document.createElement("canvas");
  crop.width  = outW;
  crop.height = outH;
  var cctx = crop.getContext("2d");
  // nearest-neighbor, not bilinear: keeps painted regions as solid category
  // colors instead of blending them at edges into colors that won't match
  // any PAINT_CATEGORIES entry
  cctx.imageSmoothingEnabled = false;
  cctx.drawImage(canvas, x0, y0, w, h, 0, 0, outW, outH);

  // canvas pixels coincide with map container points (canvas sits at the map
  // container's top-left, sized to map.getSize()), so the crop corners can be
  // georeferenced exactly with containerPointToLatLng
  var nw = map.containerPointToLatLng([x0, y0]);
  var se = map.containerPointToLatLng([x1, y1]);

  Shiny.setInputValue("newVersions-paintStroke", {
    dataUrl:    crop.toDataURL(),
    bounds:     { west: nw.lng, east: se.lng, south: se.lat, north: nw.lat },
    width:      outW,
    height:     outH,
    categoryId: paintbrush.categoryId
  }, { priority: "event" });
}

function initPaintbrush(mapId) {
  var mapDiv = document.getElementById(mapId);
  if (!mapDiv) { console.warn("initPaintbrush: mapDiv not found"); return; }

  var mapWidget = HTMLWidgets.find("#" + mapId);
  if (!mapWidget) { console.warn("initPaintbrush: mapWidget not found"); return; }

  var map = mapWidget.getMap();
  if (!map) { console.warn("initPaintbrush: map instance not found"); return; }

  var drawing     = false;
  var brushRadius = 18;

  var canvas = document.createElement("canvas");
  canvas.width  = map.getSize().x;
  canvas.height = map.getSize().y;
  canvas.style.position      = "absolute";
  canvas.style.top           = "0";
  canvas.style.left          = "0";
  canvas.style.pointerEvents = "none";
  canvas.style.zIndex        = "500";
  mapDiv.style.position      = "relative";
  mapDiv.appendChild(canvas);

  var ctx = canvas.getContext("2d");

  canvas.addEventListener("pointerdown", function(e) {
    if (!paintbrush.active) return;
    e.preventDefault();
    e.stopPropagation();
    canvas.setPointerCapture(e.pointerId);
    drawing = true;
    paintbrush.strokeActive = false;
    var rect = paintbrush.mapDiv.getBoundingClientRect();
    drawCircle(e.clientX - rect.left, e.clientY - rect.top);
  });

  canvas.addEventListener("pointermove", function(e) {
    if (!paintbrush.active || !drawing) return;
    e.preventDefault();
    e.stopPropagation();
    var rect = paintbrush.mapDiv.getBoundingClientRect();
    drawCircle(e.clientX - rect.left, e.clientY - rect.top);
  });

  canvas.addEventListener("pointerup", function(e) {
    e.stopPropagation();
    var hadStroke = paintbrush.strokeActive;
    drawing = false;
    paintbrush.strokeActive = false;
    if (hadStroke) {
      try {
        sendPaintStroke();
      } catch (err) {
        console.error("sendPaintStroke failed:", err);
      }
    }
  });

  canvas.addEventListener("pointerleave", function() {
    drawing = false;
  });

  paintbrush = {
    canvas:      canvas,
    ctx:         ctx,
    map:         map,
    mapDiv:      mapDiv,
    brushRadius: brushRadius,
    active:      false,
    color:       "144,238,144",
    categoryId:  1,
    strokeActive: false,
    strokeBounds: null,
    clear: function() {
      ctx.clearRect(0, 0, canvas.width, canvas.height);
      paintbrush.strokeBounds = null;
    }
  };

  console.log("initPaintbrush: success ✅");
}

// ── Message handlers ──────────────────────────────────────────────────────────

$(document).on("shiny:connected", function() {
  var checkMap = setInterval(function() {
    var mapWidget = HTMLWidgets.find("#newVersions-versionMap");
    if (!mapWidget) return;
    var map = mapWidget.getMap();
    if (!map) return;
    var mapPane = map.getPanes().mapPane;
    if (!mapPane) return;
    clearInterval(checkMap);
    initPaintbrush("newVersions-versionMap");
  }, 200);
});

Shiny.addCustomMessageHandler("set-paint-active", function(active) {
  if (!paintbrush) { console.warn("set-paint-active: paintbrush not initialised"); return; }
  paintbrush.active = active;
  if (active) {
    disableMapInteractions(paintbrush.map);
    paintbrush.canvas.style.pointerEvents = "auto";
    updateCursor();
  } else {
    enableMapInteractions(paintbrush.map);
    paintbrush.canvas.style.pointerEvents = "none";
    paintbrush.canvas.style.cursor = "default";
  }
});

Shiny.addCustomMessageHandler("set-brush-radius", function(radius) {
  if (!paintbrush) return;
  paintbrush.brushRadius = radius;
  if (paintbrush.active) updateCursor();
});

Shiny.addCustomMessageHandler("set-paint-color", function(msg) {
  if (!paintbrush) return;
  paintbrush.color = msg.rgb;
  paintbrush.categoryId = msg.id;
  if (paintbrush.active) updateCursor();
});

Shiny.addCustomMessageHandler("clear-paint-canvas", function(msg) {
  if (!paintbrush) return;
  paintbrush.clear();
});