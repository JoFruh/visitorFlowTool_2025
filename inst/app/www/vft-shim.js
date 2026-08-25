// Drop leaflet's marker/shape hover inputs before they reach the server.
//
// leaflet's binding calls Shiny.onInputChange() from its mouseHandler for
// marker_mouseover / marker_mouseout / <shape>_mouseover / <shape>_mouseout, and
// stamps every event with `".nonce": Math.random()` so Shiny cannot dedupe
// consecutive ones. Each hover is therefore one guaranteed input message, and
// every input message batch makes ShinySession$manageInputs() call
// manageHiddenOutputs(), which walks the entire registered-output list and runs
// the suspend test against each one. On the step-5 map, dragging the pointer
// across the network fires those continuously - on the single R process that is
// serving every connected user.
//
// Nothing in this app reads a `_mouseover` or `_mouseout` input (verified across
// R/ and inst/), so the whole round trip buys nothing.
//
// EXACTLY these two suffixes are dropped. `_click`, `_bounds`, `_center`,
// `_zoom` and `_groups` are all read by this app and must keep working, so do
// not widen the list. If a feature ever does need hover, it has to be taken off
// this list first - it will otherwise fail silently, which is the price of
// patching the global entry point rather than each map.
(function () {
  var DROPPED = ["_mouseover", "_mouseout"];
  var installed = false;

  function isDropped(name) {
    if (typeof name !== "string") return false;
    // setInputValue names can carry an input handler suffix ("id:handler"),
    // so compare against the part before the colon.
    var base = name.split(":")[0];
    for (var i = 0; i < DROPPED.length; i++) {
      var suffix = DROPPED[i];
      if (base.length >= suffix.length &&
          base.slice(base.length - suffix.length) === suffix) return true;
    }
    return false;
  }

  // Shiny assigns setInputValue (and its older alias onInputChange, the same
  // function object) inside initShiny(), not when shiny.js is parsed - so this
  // cannot run at script load time. Both hooks below fire after initShiny; the
  // flag makes the second one a no-op.
  function install() {
    if (installed) return;
    if (typeof Shiny === "undefined" || typeof Shiny.setInputValue !== "function") return;
    installed = true;

    var original = Shiny.setInputValue;
    var guarded = function (name) {
      if (isDropped(name)) return;
      return original.apply(this, arguments);
    };

    Shiny.setInputValue = guarded;
    // leaflet's binding calls the alias, and it reads the property off the Shiny
    // object at call time, so replacing it here is enough.
    Shiny.onInputChange = guarded;
  }

  if (window.jQuery) {
    jQuery(document).on("shiny:connected", install);
    jQuery(install);
  } else if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", install);
  } else {
    install();
  }
})();
