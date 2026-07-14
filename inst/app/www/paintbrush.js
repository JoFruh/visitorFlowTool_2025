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
  var btnDiv = document.getElementById("newVersions-confirmPaintDiv");
  if (btnDiv && btnDiv.style.display === "none") btnDiv.style.display = "block";
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

function sendPaintMask(id) {
  if (!paintbrush) return;
  Shiny.setInputValue(id, paintbrush.canvas.toDataURL(), { priority: "event" });
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
    drawing = false;
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
    clear: function() { ctx.clearRect(0, 0, canvas.width, canvas.height); }
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

Shiny.addCustomMessageHandler("set-paint-color", function(rgb) {
  if (!paintbrush) return;
  paintbrush.color = rgb;
  if (paintbrush.active) updateCursor();
});

Shiny.addCustomMessageHandler("send-paint-mask", function(inputId) {
  sendPaintMask(inputId);
});