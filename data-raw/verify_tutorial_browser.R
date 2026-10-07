## Browser half of the guided tutorial (inst/app/www/vft-tutorial.js): step 1's
## tour walked in the REAL app, in headless Chrome, with real mouse and key
## events through the DevTools protocol - the first visit's bubble, the six
## hints, the choice held back until the last hint, the hand-off to the chosen
## step, the help button's modal and Escape.
## data-raw/verify_tutorial.R covers the R side.
##
## Needs the app running with the nav bar on, e.g.
##   VFT_NAV=1, pkgload::load_all("."), shiny::runApp(system.file("app",
##   package = "visitorFlowTool"), port = 7781)
## Run:  Rscript data-raw/verify_tutorial_browser.R [url]
##
## Screenshots go through Page$captureScreenshot, never b$screenshot(): that
## one resizes the viewport, and the map then loses the pointer mid-drawing.
args <- commandArgs(trailingOnly = TRUE)
URL  <- if (length(args)) args[[1]] else Sys.getenv("VFT_URL", "http://127.0.0.1:7781")
SHOTS <- Sys.getenv("VFT_SHOTS", file.path(tempdir(), "tutorial_shots"))
dir.create(SHOTS, showWarnings = FALSE, recursive = TRUE)

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

b <- chromote::ChromoteSession$new(width = 1600, height = 1000)
js <- function(x) b$Runtime$evaluate(x, returnByValue = TRUE)$result$value
waitFor <- function(expr, secs = 30, step = 0.25) {
  t0 <- Sys.time()
  repeat {
    v <- tryCatch(js(expr), error = function(e) NULL)
    if (isTRUE(v)) return(TRUE)
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > secs) return(FALSE)
    Sys.sleep(step)
  }
}
shot <- function(name) {
  png <- b$Page$captureScreenshot(format = "png")$data
  writeBin(jsonlite::base64_dec(png), file.path(SHOTS, paste0(name, ".png")))
}
mouse <- function(type, x, y, count = 1)
  b$Input$dispatchMouseEvent(type = type, x = x, y = y, button = "left", clickCount = count)
move  <- function(x, y) { b$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y); Sys.sleep(0.06) }
click <- function(x, y) {
  move(x, y); mouse("mousePressed", x, y); mouse("mouseReleased", x, y); Sys.sleep(0.12)
}
#the centre of an element, in page pixels
centre <- function(sel) js(sprintf(
  "(function(){ var e = document.querySelector(%s); if(!e) return null;
     var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()",
  jsonlite::toJSON(sel, auto_unbox = TRUE)))
clickEl <- function(sel) {
  p <- centre(sel); if (is.null(p)) return(invisible(FALSE))
  click(p[[1]], p[[2]]); invisible(TRUE)
}
key <- function(k, code) {
  b$Input$dispatchKeyEvent(type = "rawKeyDown", key = k, code = k, windowsVirtualKeyCode = code)
  b$Input$dispatchKeyEvent(type = "keyUp", key = k, code = k, windowsVirtualKeyCode = code)
  Sys.sleep(0.1)
}
tut     <- function() js("window.vftTutorialState ? vftTutorialState() : null")
atHint  <- function(i) sprintf("(function(){ var s = vftTutorialState();
                                 return s.key === 'step1' && s.idx === %d && !s.quiet && !s.pause; })()", i - 1)
wins    <- function() {
  w <- js("(function(){ var c = document.querySelector('.vftTutorialCanvas');
             return c ? c.getAttribute('data-windows') : null; })()")
  if (is.null(w)) list() else jsonlite::fromJSON(w, simplifyVector = FALSE)
}
rectOf <- function(sel) js(sprintf(
  "(function(){ var e = document.querySelector(%s); if(!e) return null;
     var r = e.getBoundingClientRect(); return [r.left, r.top, r.width, r.height]; })()",
  jsonlite::toJSON(sel, auto_unbox = TRUE)))
#does window `w` ([x, y, w, h]) sit on rect `r`, within `tol` px on each edge?
near <- function(w, r, tol = 12) {
  if (is.null(w) || is.null(r)) return(FALSE)
  w <- unlist(w); r <- unlist(r)
  all(abs(c(w[1] - r[1], w[2] - r[2], (w[1] + w[3]) - (r[1] + r[3]),
            (w[2] + w[4]) - (r[2] + r[4]))) <= tol)
}
ring   <- function() js("(function(){ var e = document.querySelector('#vftNav .vft-nav-current'); return e ? e.id : null; })()")
stored <- function() js("(function(){ try { var s = localStorage.getItem('vft.tutorial.v1'); return s ? JSON.parse(s).status : null; } catch(e){ return null; } })()")
modalUp <- function() js("jQuery('#shiny-modal').is(':visible')")
MAP <- "#step1-areaSelectMap"

#### open the app on a clean device ####
invisible(b$Page$navigate(URL))
ok("the app connects", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
invisible(js("localStorage.removeItem('vft.tutorial.v1')"))
invisible(b$Page$reload())
Sys.sleep(1)
ok("...again after clearing the tutorial's storage",
   waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))

cat("=== 1. the first visit's bubble ===\n")
ok("the bubble appears", waitFor("!!document.querySelector('.vftTutorialOffer')", 60))
Sys.sleep(0.6)
help <- rectOf("#helpButton"); bub <- rectOf(".vftTutorialOffer")
ok("...under the help button",
   !is.null(bub) && bub[[2]] >= help[[2]] + help[[4]] &&
   bub[[1]] <= help[[1]] + help[[4]] && bub[[1]] + bub[[3]] >= help[[1]])
ok("...with the translated offer", nzchar(js("document.querySelector('.vftTutorialOfferText').textContent")))
shot("0_offer")
clickEl(".vftTutorialOffer .vftTutorialStartBtn")

cat("\n=== 2. hint 1: Switzerland ===\n")
ok("the tour starts at hint 1", waitFor(atHint(1), 15))
Sys.sleep(0.8)
ch <- js(sprintf("(function(){ var m = HTMLWidgets.find('%s').getMap(), c = m.getContainer().getBoundingClientRect();
  var nw = m.latLngToContainerPoint([47.808, 5.956]), se = m.latLngToContainerPoint([45.818, 10.492]);
  var l = Math.max(c.left, c.left + nw.x - 8), t = Math.max(c.top, c.top + nw.y - 8),
      r = Math.min(c.right, c.left + se.x + 8), b = Math.min(c.bottom, c.top + se.y + 8);
  return [l, t, r - l, b - t]; })()", MAP))
w <- wins()
ok("one window, on Switzerland's bounds", length(w) == 1 && near(w[[1]], ch, 6),
   paste(unlist(w), collapse = ","))
ok("...and the bubble is gone", !isTRUE(js("!!document.querySelector('.vftTutorialOffer')")))
ok("the card has its Next button", isTRUE(js("!!document.querySelector('.vftTutorialBox .vftTutorialNext')")))
ok("'anywhere in Switzerland' is emphasised", nzchar(js("(document.querySelector('.vftTutorialText em')||{}).textContent || ''")))
#left of the selector, clear of it, level with it - not under it, where its
#dropdown opens
ok("a second card left of the language selector, in three languages", {
  tip <- unlist(rectOf(".vftTutorialTip.vftTutorialTipOn")); lang <- unlist(rectOf("#vftNav .vft-nav-lang"))
  txt <- js("(document.querySelector('.vftTutorialTip')||{}).textContent || ''")
  length(tip) == 4 && tip[1] + tip[3] <= lang[1] && tip[2] <= lang[2] + lang[4] && tip[2] + tip[4] >= lang[2] &&
    all(vapply(c("Change language", "Modifiez la langue", "Sprache"), grepl, logical(1), x = txt, fixed = TRUE))
})
ok("...the cards do not overlap", {
  a <- unlist(rectOf(".vftTutorialTip")); b2 <- unlist(rectOf(".vftTutorialBox:not(.vftTutorialTip)"))
  a[1] + a[3] <= b2[1] || b2[1] + b2[3] <= a[1] || a[2] + a[4] <= b2[2] || b2[2] + b2[4] <= a[2]
})
ok("...and both popped in", isTRUE(js("document.querySelectorAll('.vftTutorialBox.vftTutorialPop').length === 2")))
shot("1_switzerland")
p <- centre(MAP); click(p[[1]], p[[2]])
ok("a tap on the map in a hint to read does nothing", isTRUE(js(atHint(1))) &&
   js("document.querySelectorAll('#step1-areaSelectMap .vft-pd-v').length") == 0)
clickEl(".vftTutorialNext")

cat("\n=== 3. hint 2: the upload card ===\n")
ok("Next moves on to hint 2", waitFor(atHint(2), 10))
ok("...and the language card goes with hint 1", isTRUE(js("!document.querySelector('.vftTutorialTip.vftTutorialTipOn')")))
Sys.sleep(0.6)
w <- wins()
ok("one window, on the upload card",
   length(w) == 1 && near(w[[1]], rectOf(".vft-step1-options > .vft-step1-opt:first-child")))
shot("2_upload")
clickEl(".vftTutorialNext")

cat("\n=== 4. hint 3: draw around Birmensdorf ===\n")
ok("Next moves on to hint 3 (after the flight)", waitFor(atHint(3), 15))
Sys.sleep(0.6)
view <- js(sprintf("(function(){ var m = HTMLWidgets.find('%s').getMap(), c = m.getCenter();
                    return [m.getZoom(), c.lat, c.lng]; })()", MAP))
ok(sprintf("the map is at zoom 14 over Birmensdorf (%.2f, %.4f, %.4f)", view[[1]], view[[2]], view[[3]]),
   abs(view[[1]] - 14) < 0.05 && abs(view[[2]] - 47.3553) < 0.002 && abs(view[[3]] - 8.4378) < 0.002)
w <- wins()
ok("two windows: the draw card and the map", length(w) == 2 &&
   any(vapply(w, near, logical(1), r = rectOf(".vft-step1-options > .vft-step1-opt:last-child"))) &&
   any(vapply(w, near, logical(1), r = rectOf(MAP))))
ok("the card sits left of the draw card, pointing at it", {
  bx <- unlist(rectOf(".vftTutorialBox"))
  dc <- unlist(rectOf(".vft-step1-options > .vft-step1-opt:last-child"))
  bx[1] + bx[3] <= dc[1] && identical(js("document.querySelector('.vftTutorialBox').getAttribute('data-side')"), "left")
})
shot("3_draw")
clickEl("#infoButton"); Sys.sleep(1)
ok("a tap outside the windows is swallowed (no info modal)", !isTRUE(modalUp()))

mp <- unlist(rectOf(MAP)); cx <- mp[1] + mp[3] / 2; cy <- mp[2] + mp[4] / 2
pts <- list(c(cx - 120, cy - 90), c(cx + 120, cy - 90), c(cx, cy + 110))
for (q in pts) click(q[1], q[2])
ok("three vertices through the map window",
   js("document.querySelectorAll('#step1-areaSelectMap .vft-pd-v').length") == 3)
key("Escape", 27)
ok("Escape takes a vertex back and keeps the tour", isTRUE(js(atHint(3))) &&
   js("document.querySelectorAll('#step1-areaSelectMap .vft-pd-v').length") == 2)
click(pts[[3]][1], pts[[3]][2])
move(cx, cy); move(pts[[1]][1], pts[[1]][2]); click(pts[[1]][1], pts[[1]][2])

cat("\n=== 5. hint 4: confirm ===\n")
ok("an accepted polygon moves on to hint 4", waitFor(atHint(4), 20))
Sys.sleep(0.6)
w <- wins()
ok("one window, on the confirm button", length(w) == 1 && near(w[[1]], rectOf("#step1-confirmButton2")))
shot("4_confirm")
clickEl("#step1-confirmButton2")

cat("\n=== 6. hint 5: choose ===\n")
ok("the next-step modal moves on to hint 5", waitFor(atHint(5), 60))
Sys.sleep(0.8)
nBtn <- js("document.querySelectorAll('#shiny-modal .vft-next-btn').length")
w <- wins()
ok(sprintf("one window per choice (%d)", nBtn), nBtn >= 2 && length(w) == nBtn)
shot("5_choose")
clickEl("#vftNextStep2")

cat("\n=== 7. hint 6: the help button, before moving on ===\n")
ok("the choice moves on to hint 6", waitFor(atHint(6), 10))
Sys.sleep(0.6)
ok("...with the modal closed", !isTRUE(modalUp()))
ok("...and the user still on step 1", identical(ring(), "vftNav_step1"))
ok("...holding the choice", identical(tut()$choice, "vftNextStep2"))
w <- wins()
ok("one window, on the help button", length(w) == 1 && near(w[[1]], rectOf("#helpButton"), 10))
shot("6_help")
clickEl(".vftTutorialNext")

cat("\n=== 8. the hand-off ===\n")
ok("Next sends the choice: the ring moves to step 2",
   waitFor("(function(){ var e = document.querySelector('#vftNav .vft-nav-current'); return !!e && e.id === 'vftNav_step2'; })()", 60))
ok("the overlay is gone", !isTRUE(js("!!document.getElementById('vftTutorial')")))
ok("the tour is stored as done", identical(stored(), "done"))
ok("step 2's tour follows by itself",
   waitFor("(function(){ var s = vftTutorialState(); return s.key === 'step2' && !s.quiet; })()", 120))

cat("\n=== 9. the help button's modal, and Escape ===\n")
invisible(b$Page$reload())
ok("the app reconnects", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
Sys.sleep(8)
ok("no bubble once the tour has been done", !isTRUE(js("!!document.querySelector('.vftTutorialOffer')")))
invisible(waitFor("!!document.querySelector('#vftNav .vft-nav-current')", 20))
clickEl("#helpButton")
ok("the help button opens the tutorial modal",
   waitFor("jQuery('#shiny-modal').is(':visible') && !!document.querySelector('#shiny-modal .vftTutorialStartBtn')", 10))
shot("9_help_modal")
clickEl("#shiny-modal .vftTutorialStartBtn")
ok("its button closes it and starts hint 1", waitFor(atHint(1), 15) && !isTRUE(modalUp()))

invisible(js("jQuery('#languageSelect')[0].selectize.setValue('fr')"))
ok("the card follows a language change",
   waitFor("(document.querySelector('.vftTutorialText')||{}).textContent.indexOf('Bienvenue') === 0", 15))
invisible(js("jQuery('#languageSelect')[0].selectize.setValue('de')"))
ok("...and back to German",
   waitFor("(document.querySelector('.vftTutorialText')||{}).textContent.indexOf('Willkommen') === 0", 15),
   js("(document.querySelector('.vftTutorialText')||{}).textContent.slice(0, 30)"))

key("Escape", 27)
ok("Escape stops the tour", waitFor("!document.getElementById('vftTutorial')", 5))
ok("...and is stored", identical(stored(), "stopped"))

cat(sprintf("\nscreenshots in %s\n", SHOTS))
b$close()
cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
