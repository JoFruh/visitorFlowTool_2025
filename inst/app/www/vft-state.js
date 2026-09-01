// Crash recovery in the browser's own storage.
//
// The server pushes a small snapshot of the session (the perimeter, the areas
// of interest, and every step-1..3 choice - see VFT_SNAPSHOT_KEYS in
// R/state.R) at each step confirmation. This file is the whole client half:
// write it to localStorage, hand it back on the next connection, and delete it
// when the user says no.
//
// It never goes anywhere else. localStorage is per-origin and per-device, so
// the snapshot stays on the machine that made it - nothing is written to the
// server's disk, and no other user or browser can reach it.
//
// WHY localStorage AND NOT IndexedDB. The snapshot is choices plus two sf
// geometries; everything expensive is left out precisely so it fits the ~5 MB
// per-origin quota. R/state.R refuses to send anything over
// VFT_SNAPSHOT_MAX_CHARS, so the quota should never be reached - and if the
// areas of interest ever do turn out to blow it in the field, the swap is
// entirely inside this file: same two message handlers, same input name.
//
// EVERY localStorage access is wrapped. A private window, a browser set to
// block site data, or a storage-partitioned iframe can throw on the plain READ,
// not only on the write, and a session must open normally when that happens.
(function () {
  var KEY = "vftState.v1";
  var installed = false;

  function read() {
    try {
      return window.localStorage.getItem(KEY);
    } catch (e) {
      return null;
    }
  }

  function write(value) {
    try {
      window.localStorage.setItem(KEY, value);
      return true;
    } catch (e) {
      // QuotaExceededError, or storage disabled. Nothing to do about it here -
      // the app is fully usable without a snapshot.
      return false;
    }
  }

  function clear() {
    try {
      window.localStorage.removeItem(KEY);
    } catch (e) {
      /* nothing to clear */
    }
  }

  // Offer whatever is stored, once, at the start of the session. Sent only when
  // there IS something: the server observer is ignoreNULL, so "nothing stored"
  // is simply silence rather than a message that has to be told apart from a
  // real one.
  function offer() {
    var raw = read();
    if (!raw) return;

    var parsed;
    try {
      parsed = JSON.parse(raw);
    } catch (e) {
      // Half-written or from an incompatible build. Drop it rather than hand
      // the server something it cannot use.
      clear();
      return;
    }

    if (!parsed || !parsed.payload) {
      clear();
      return;
    }

    Shiny.setInputValue("vftStoredState", parsed, { priority: "event" });
  }

  // NOTHING IN HERE MAY THROW.
  //
  // This runs from a jQuery "shiny:connected" handler, and jQuery dispatches
  // the handlers for one event in a plain loop with no try/catch - so an
  // exception raised here does not just break crash recovery, it aborts the
  // loop and every handler registered after this one is silently skipped. The
  // page would come up with outputs unbound: an empty map and a nav bar that
  // never gets its ring. A convenience feature must not be able to do that to
  // the app, so the whole body is wrapped and the failure is a console warning.
  function install() {
    if (installed) return;
    if (typeof Shiny === "undefined" ||
        typeof Shiny.addCustomMessageHandler !== "function") return;
    installed = true;

    try {
      Shiny.addCustomMessageHandler("vft-state-save", function (m) {
        if (!m || !m.payload) return;
        try {
          write(JSON.stringify(m));
        } catch (e) {
          /* JSON.stringify can throw on a message this never produces */
        }
      });

      Shiny.addCustomMessageHandler("vft-state-clear", function () {
        clear();
      });
    } catch (e) {
      if (window.console) console.warn("vft-state: handlers not installed", e);
      return;
    }

    // Off this call stack as well as inside a try. Reading storage and setting
    // an input are the two things here that touch the outside world, and doing
    // them in a later tick means that even a failure mode not thought of here
    // lands after Shiny has finished connecting rather than in the middle of it.
    setTimeout(function () {
      try {
        offer();
      } catch (e) {
        if (window.console) console.warn("vft-state: no snapshot offered", e);
      }
    }, 0);
  }

  // Same hooks as vft-shim.js, and for the same reason: Shiny's client object
  // is not finished when this script is parsed. The flag makes whichever hook
  // fires second a no-op.
  if (window.jQuery) {
    jQuery(document).on("shiny:connected", function () {
      try {
        install();
      } catch (e) {
        if (window.console) console.warn("vft-state: install failed", e);
      }
    });
  } else if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", install);
  } else {
    install();
  }
})();
