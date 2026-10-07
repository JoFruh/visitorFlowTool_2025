## Browser check of step 2's tour (inst/app/www/vft-tutorial.js), in the REAL
## app, in headless Chrome, with real mouse and key events through the DevTools
## protocol. Step 1's tour is walked quickly to get there: step 2 is chosen, and
## its tour has to start by itself when the ring lands on step 2 (chaining).
## Then its thirteen hints: the step's nav button, the other steps, the map,
## Amphibians ticked, the map again, the species list, the toad's row, its
## weight up to 3 (2 must not move on), the red list weights, the threshold
## (15 must not move on, 25 or more must), the download (live: a tap on it
## goes through, Next moves on), confirm, and the next-step modal - whose
## choice ends the tour and hands on to step 3's.
## data-raw/verify_tutorial_browser.R covers step 1's tour in detail.
##
## Needs the app running with the nav bar on, e.g.
##   VFT_NAV=1, pkgload::load_all("."), shiny::runApp(system.file("app",
##   package = "visitorFlowTool"), port = 7781)
## Run:  Rscript data-raw/verify_tutorial_step2_browser.R [url]
args <- commandArgs(trailingOnly = TRUE)
URL  <- if (length(args)) args[[1]] else Sys.getenv("VFT_URL", "http://127.0.0.1:7781")
SHOTS <- Sys.getenv("VFT_SHOTS", file.path(tempdir(), "tutorial_step2_shots"))
dir.create(SHOTS, showWarnings = FALSE, recursive = TRUE)

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

b <- chromote::ChromoteSession$new(width = 1600, height = 1000)
## the download hint's tap reaches the app, and the zip lands here
DL <- file.path(tempdir(), "tutorial_step2_dl")
dir.create(DL, showWarnings = FALSE, recursive = TRUE)
invisible(tryCatch(b$Browser$setDownloadBehavior(behavior = "allow", downloadPath = normalizePath(DL)),
                   error = function(e) NULL))
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
#a drag with the button held, in small steps, as a hand would
drag <- function(x0, y0, x1, y1, n = 12) {
  move(x0, y0); mouse("mousePressed", x0, y0)
  for (s in seq_len(n)) {
    b$Input$dispatchMouseEvent(type = "mouseMoved", x = x0 + (x1 - x0) * s / n,
                               y = y0 + (y1 - y0) * s / n, button = "left", buttons = 1)
    Sys.sleep(0.03)
  }
  mouse("mouseReleased", x1, y1); Sys.sleep(0.12)
}
key <- function(k, code) {
  b$Input$dispatchKeyEvent(type = "rawKeyDown", key = k, code = k, windowsVirtualKeyCode = code)
  b$Input$dispatchKeyEvent(type = "keyUp", key = k, code = k, windowsVirtualKeyCode = code)
  Sys.sleep(0.15)
}
q <- function(sel) jsonlite::toJSON(sel, auto_unbox = TRUE)
centre <- function(sel) js(sprintf(
  "(function(){ var e = document.querySelector(%s); if(!e) return null;
     var r = e.getBoundingClientRect(); return [r.left + r.width/2, r.top + r.height/2]; })()", q(sel)))
clickEl <- function(sel) {
  p <- centre(sel); if (is.null(p)) return(invisible(FALSE))
  click(p[[1]], p[[2]]); invisible(TRUE)
}
tut    <- function() js("window.vftTutorialState ? vftTutorialState() : null")
atHint <- function(i, key = "step2") sprintf("(function(){ var s = vftTutorialState();
                                 return s.key === '%s' && s.idx === %d && !s.quiet && !s.pause; })()", key, i - 1)
wins   <- function() {
  w <- js("(function(){ var c = document.querySelector('.vftTutorialCanvas');
             return c ? c.getAttribute('data-windows') : null; })()")
  if (is.null(w)) list() else jsonlite::fromJSON(w, simplifyVector = FALSE)
}
rectOf <- function(sel) js(sprintf(
  "(function(){ var e = document.querySelector(%s); if(!e) return null;
     var r = e.getBoundingClientRect(); return [r.left, r.top, r.width, r.height]; })()", q(sel)))
near <- function(w, r, tol = 12) {
  if (is.null(w) || is.null(r)) return(FALSE)
  w <- unlist(w); r <- unlist(r)
  all(abs(c(w[1] - r[1], w[2] - r[2], (w[1] + w[3]) - (r[1] + r[3]),
            (w[2] + w[4]) - (r[2] + r[4]))) <= tol)
}
inside <- function(w, r, tol = 2) {
  w <- unlist(w); r <- unlist(r)
  w[1] >= r[1] - tol && w[2] >= r[2] - tol &&
    w[1] + w[3] <= r[1] + r[3] + tol && w[2] + w[4] <= r[2] + r[4] + tol
}
ringIs <- function(id) sprintf("(function(){ var e = document.querySelector('#vftNav .vft-nav-current');
                                  return !!e && e.id === '%s'; })()", id)
stored  <- function() js("(function(){ try { var s = localStorage.getItem('vft.tutorial.v1'); return s ? JSON.parse(s).status : null; } catch(e){ return null; } })()")
hasNext <- function() isTRUE(js("!!document.querySelector('.vftTutorialBox .vftTutorialNext')"))
MAP <- "#step1-areaSelectMap"
#the toad's row as the tour finds it, and its weight
ROW <- "(function(){ var rows = document.querySelectorAll('#step2-speciesCheckbox .checkbox');
  for (var i = 0; i < rows.length; i++) if (rows[i].textContent.indexOf('Bombina variegata') >= 0) return rows[i];
  return null; })()"
rowRect <- function() js(sprintf("(function(){ var e = %s; if(!e) return null;
  var r = e.getBoundingClientRect(); return [r.left, r.top, r.width, r.height]; })()", ROW))
weight  <- function() js(sprintf("(function(){ var e = %s; var i = e && e.querySelector('input[type=number]');
  return i ? Number(i.value) : null; })()", ROW))

#### open the app, no bubble ####
invisible(b$Page$navigate(URL))
ok("the app connects", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
invisible(js("localStorage.setItem('vft.tutorial.v1', JSON.stringify({status: 'done', at: Date.now()}))"))
invisible(b$Page$reload())
Sys.sleep(1)
ok("...again", waitFor("!!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected())", 120))
invisible(waitFor(sprintf("(function(){ var w = HTMLWidgets.find('%s'); var m = w && w.getMap && w.getMap();
                            return !!m && !!m._loaded; })()", MAP), 60))
Sys.sleep(1.5)

cat("=== 1. to step 2, through step 1's tour ===\n")
invisible(js("vftTutorialStart('step1')"))
ok("step 1's tour starts", waitFor(atHint(1, "step1"), 20))
clickEl(".vftTutorialNext")
invisible(waitFor(atHint(2, "step1"), 10)); clickEl(".vftTutorialNext")
ok("...hint 3, over Birmensdorf", waitFor(atHint(3, "step1"), 15))
Sys.sleep(0.6)
mp <- unlist(rectOf(MAP)); cx <- mp[1] + mp[3] / 2; cy <- mp[2] + mp[4] / 2
pts <- list(c(cx - 120, cy - 90), c(cx + 120, cy - 90), c(cx, cy + 110))
for (p in pts) click(p[1], p[2])
move(cx, cy); move(pts[[1]][1], pts[[1]][2]); click(pts[[1]][1], pts[[1]][2])
ok("...the polygon is accepted", waitFor(atHint(4, "step1"), 20))
Sys.sleep(0.6); clickEl("#step1-confirmButton2")
ok("...the next-step modal", waitFor(atHint(5, "step1"), 60))
Sys.sleep(0.8); clickEl("#vftNextStep2")
ok("...step 2 is held for hint 6", waitFor(atHint(6, "step1"), 10))
Sys.sleep(0.6); clickEl(".vftTutorialNext")
ok("the ring lands on step 2", waitFor(ringIs("vftNav_step2"), 240))

cat("\n=== 2. hint 1: this step ===\n")
ok("step 2's tour starts by itself", waitFor(atHint(1), 120), paste(unlist(tut()), collapse = ","))
ok("...once step 2's page shows, not over step 1's",
   isTRUE(js("document.getElementById('step2-SDMmap').offsetParent !== null")) &&
   !isTRUE(js("(function(){ var e = document.getElementById('step1-areaSelectMap');
                            return !!e && e.offsetParent !== null; })()")))
Sys.sleep(0.8)
w <- wins()
ok("one window, on step 2's nav button", length(w) == 1 && near(w[[1]], rectOf("#vftNav_step2"), 6),
   paste(unlist(w), collapse = ","))
em <- js("(function(){ var e = document.querySelector('.vftTutorialText b em'),
                            t = document.querySelector('.vftTutorialText'); if (!e) return null;
  return [e.textContent, getComputedStyle(e).color, parseFloat(getComputedStyle(e).fontSize), parseFloat(getComputedStyle(t).fontSize)]; })()")
ok("the step's name in teal (#006268)", !is.null(em) && identical(em[[2]], "rgb(0, 98, 104)"),
   paste(unlist(em), collapse = " | "))
ok("...and one size larger", !is.null(em) && em[[3]] > em[[4]])
ok("the card has its Next button", hasNext())
shot("01_step")
clickEl(".vftTutorialNext")

cat("\n=== 3. hint 2: the other steps ===\n")
ok("Next moves on to hint 2", waitFor(atHint(2), 10))
Sys.sleep(0.8)
w <- wins()
groups <- js("(function(){ var n = 0; document.querySelectorAll('#vftNav .vft-nav-center > .vft-nav-group').forEach(function (g) {
  if (g.offsetParent !== null && !g.querySelector('#vftNav_step2')) n++; }); return n; })()")
s2 <- unlist(rectOf("#vftNav_step2"))
clear <- all(vapply(w, function(x) { x <- unlist(x)
  x[1] + x[3] <= s2[1] + 1 || x[1] >= s2[1] + s2[3] - 1 }, logical(1)))
ok(sprintf("one window per other group of the bar (%d)", groups), groups >= 2 && length(w) == groups)
ok("...none of them on step 2's own button", clear)
shot("02_others")
clickEl(".vftTutorialNext")

cat("\n=== 4. hint 3: the map ===\n")
ok("Next moves on to hint 3", waitFor(atHint(3), 10))
Sys.sleep(0.6)
w <- wins()
ok("one window, on the sensitivity map", length(w) == 1 && near(w[[1]], rectOf("#step2-SDMmap")))
shot("03_map")
clickEl(".vftTutorialNext")

cat("\n=== 5. hint 4: tick Amphibians ===\n")
ok("Next moves on to hint 4", waitFor(atHint(4), 10))
## the species scan may still be running, and the window waits for the list
## to settle
ok("its window opens once the group list has settled",
   waitFor("(function(){ var c = document.querySelector('.vftTutorialCanvas');
              return !!c && JSON.parse(c.getAttribute('data-windows') || '[]').length === 1; })()", 180))
Sys.sleep(0.6)
AMPH <- "(function(){ var b = document.querySelectorAll('#step2-groupCheckbox_class input[type=checkbox]');
  for (var i = 0; i < b.length; i++) if (['Amphibien','Amphibiens','Amphibians'].indexOf(b[i].value) >= 0) return b[i];
  return null; })()"
ok("the area has an Amphibians box", isTRUE(js(sprintf("!!%s", AMPH))))
w <- wins()
lab <- js(sprintf("(function(){ var r = %s.closest('label').getBoundingClientRect(); return [r.left, r.top, r.width, r.height]; })()", AMPH))
ok("one window, on its label", length(w) == 1 && near(w[[1]], lab, 8), paste(unlist(w), collapse = ","))
ok("...and no Next button", !hasNext())
ok("'Amphibians' in the text is teal and larger",
   isTRUE(js("(function(){ var e = document.querySelector('.vftTutorialText b em'); return !!e &&
     getComputedStyle(e).color === 'rgb(0, 98, 104)'; })()")))
shot("04_amphibians")
clickEl("#step2-redListWeights"); Sys.sleep(1)
ok("a tap outside the window is swallowed", isTRUE(js(atHint(4))))
p <- js(sprintf("(function(){ var r = %s.closest('label').getBoundingClientRect(); return [r.left + 30, r.top + r.height/2]; })()", AMPH))
click(p[[1]], p[[2]])
ok("ticking it moves on to hint 5", waitFor(atHint(5), 20))
Sys.sleep(1.5)
ok("...and it stays ticked, 'All' off",
   isTRUE(js(sprintf("%s.checked && !document.getElementById('step2-groupCheckbox_all').checked", AMPH))))

cat("\n=== 6. hint 5: the map, amphibians only ===\n")
w <- wins()
ok("one window, on the map", length(w) == 1 && near(w[[1]], rectOf("#step2-SDMmap")))
ok("...with a Next button", hasNext())
shot("05_map")
clickEl(".vftTutorialNext")

cat("\n=== 7. hint 6: the species list ===\n")
ok("Next moves on to hint 6", waitFor(atHint(6), 10))
Sys.sleep(0.6)
w <- wins()
ok("one window, over the list", length(w) == 1 && inside(rectOf(".vft-fit-species"), w[[1]]))
shot("06_list")
clickEl(".vftTutorialNext")

cat("\n=== 8. hint 7: the toad's row ===\n")
ok("Next moves on to hint 7", waitFor(atHint(7), 10))
Sys.sleep(0.6)
ok("the list has Bombina variegata", !is.null(rowRect()))
w <- wins()
list <- unlist(rectOf(".vft-fit-species")); rr <- unlist(rowRect())
ok("its row is scrolled into the list's sight",
   !is.null(rr) && rr[2] >= list[2] - 1 && rr[2] + rr[4] <= list[2] + list[4] + 1)
ok("one window, on the row, cut to the list",
   length(w) == 1 && inside(w[[1]], list) && abs(unlist(w[[1]])[2] - (rr[2] - 2)) <= 3)
ok("...with a Next button", hasNext())
shot("07_toad")
clickEl(".vftTutorialNext")

cat("\n=== 9. hint 8: its weight to 3 ===\n")
ok("Next moves on to hint 8", waitFor(atHint(8), 10))
Sys.sleep(0.6)
WSEL <- sprintf("(function(){ return %s.querySelector('input[type=number]'); })()", ROW)
wr <- js(sprintf("(function(){ var r = %s.closest('.form-group').getBoundingClientRect(); return [r.left, r.top, r.width, r.height]; })()", WSEL))
w <- wins()
ok("one window, on the weight", length(w) == 1 && near(w[[1]], wr, 8), paste(unlist(w), collapse = ","))
ok("...and no Next button", !hasNext())
shot("08_weight")
p <- js(sprintf("(function(){ var r = %s.getBoundingClientRect(); return [r.left + 12, r.top + r.height/2]; })()", WSEL))
click(p[[1]], p[[2]])
w0 <- weight()
key("ArrowUp", 38)
Sys.sleep(1.5)
ok(sprintf("up once (%s -> %s) does not move on", w0, weight()), isTRUE(js(atHint(8))) && weight() == w0 + 1)
while (!is.null(weight()) && weight() < 3) key("ArrowUp", 38)
ok(sprintf("up to %s moves on to hint 9", weight()), waitFor(atHint(9), 10))

cat("\n=== 10. hint 9: red list weights ===\n")
Sys.sleep(0.4)
w <- wins()
ok("one window, on the red list button", length(w) == 1 && near(w[[1]], rectOf("#step2-redListWeights")))
shot("09_redlist")
clickEl("#step2-redListWeights")
t9 <- Sys.time()
ok("its tap pauses, then moves on to hint 10",
   isTRUE(js("vftTutorialState().pause")) && waitFor(atHint(10), 10))
ok("...two seconds later, not one (delay)", as.numeric(difftime(Sys.time(), t9, units = "secs")) >= 1.9)
## the default filter lists VU-CR species only, which the red list weighs 3-5
ok("...and the weights were set by red list status (all 3 or more)",
   waitFor("(function(){ var w = document.querySelectorAll('#step2-speciesCheckbox input[type=number]');
              return w.length > 0 && Array.prototype.every.call(w, function (i) { return Number(i.value) >= 3; }); })()", 5))

cat("\n=== 11. hint 10: the threshold ===\n")
Sys.sleep(0.6)
box <- js("(function(){ var r = document.getElementById('step2-minValThreshold').closest('.shiny-input-container').getBoundingClientRect();
           return [r.left, r.top, r.width, r.height]; })()")
w <- wins()
ok("one window, on the slider and its label", length(w) == 1 && near(w[[1]], box, 8))
ok("...the card on its right, off the map", {
  bx <- unlist(rectOf(".vftTutorialBox"))
  bx[1] >= w[[1]][[1]] + w[[1]][[3]] &&
    identical(js("document.querySelector('.vftTutorialBox').getAttribute('data-side')"), "right")
})
shot("10_threshold")
IRS <- "jQuery('#step2-minValThreshold').data('ionRangeSlider')"
xFor <- function(v) js(sprintf("(function(){ var d = %s,
  line = d.$cache.line[0].getBoundingClientRect(), hw = d.$cache.s_single[0].getBoundingClientRect().width;
  return line.left + hw / 2 + (line.width - hw) * %f / 100; })()", IRS, v))
handle <- function() js(sprintf("(function(){ var r = %s.$cache.s_single[0].getBoundingClientRect();
  return [r.left + r.width / 2, r.top + r.height / 2]; })()", IRS))
slider <- function() js(sprintf("%s.result.from", IRS))
h <- handle(); drag(h[[1]], h[[2]], xFor(15), h[[2]])
Sys.sleep(2)
ok(sprintf("a slide to %s does not move on", slider()), isTRUE(js(atHint(10))))
h <- handle(); drag(h[[1]], h[[2]], xFor(25.4), h[[2]])
ok(sprintf("a slide to %s moves on to hint 11", slider()), waitFor(atHint(11), 20))

cat("\n=== 12. hint 11: the download ===\n")
Sys.sleep(0.6)
w <- wins()
ok("one window, on the download button", length(w) == 1 && near(w[[1]], rectOf("#step2-SMbutton")))
ok("...with a Next button", hasNext())
shot("11_download")
invisible(js("window.__vftSM = 0; jQuery(document).on('shiny:inputchanged.vftTest', function (e) {
               if (e.name === 'step2-SMbutton') window.__vftSM++; });"))
clickEl("#step2-SMbutton")
ok("a tap on it goes through (live)", waitFor("window.__vftSM > 0", 5))
dlDone <- function() { f <- list.files(DL, pattern = "[.]zip$"); length(f) > 0 }
t0 <- Sys.time(); while (!dlDone() && difftime(Sys.time(), t0, units = "secs") < 30) Sys.sleep(0.5)
ok("...and the GeoTIFF zip is downloaded", dlDone(), paste(list.files(DL), collapse = ","))
ok("...and the hint stays", isTRUE(js(atHint(11))))
invisible(js("jQuery(document).off('shiny:inputchanged.vftTest')"))
clickEl(".vftTutorialNext")

cat("\n=== 13. hint 12: confirm ===\n")
ok("Next moves on to hint 12", waitFor(atHint(12), 10))
Sys.sleep(0.6)
w <- wins()
ok("one window, on the confirm button", length(w) == 1 && near(w[[1]], rectOf("#step2-confirmButton2")))
ok("...and no Next button", !hasNext())
shot("12_confirm")
clickEl("#step2-confirmButton2")
ok("the next-step modal moves on to hint 13", waitFor(atHint(13), 60))

cat("\n=== 14. hint 13: choose ===\n")
Sys.sleep(0.8)
nBtn <- js("document.querySelectorAll('#shiny-modal .vft-next-btn').length")
w <- wins()
ok(sprintf("one window per choice (%d)", nBtn), nBtn >= 1 && length(w) == nBtn)
ok("...and no Next button", !hasNext())
shot("13_choose")
clickEl("#vftNextSim")
ok("the choice ends the tour", waitFor("!document.getElementById('vftTutorial') || vftTutorialState().key !== 'step2'", 5))
ok("...stored as done", identical(stored(), "done"))
ok("...the app moves on to step 3", waitFor(ringIs("vftNav_step3"), 240))
ok("...and step 3's tour follows", waitFor(atHint(1, "step3"), 180), paste(unlist(tut()), collapse = ","))
key("Escape", 27)

cat("\n=== 15. the help button on step 2 ===\n")
clickEl("#vftNav_step2")
invisible(waitFor(ringIs("vftNav_step2"), 60))
Sys.sleep(2)
invisible(waitFor("!jQuery('#shiny-modal').is(':visible')", 10))
clickEl("#helpButton")
ok("it offers the tutorial",
   waitFor("jQuery('#shiny-modal').is(':visible') && !!document.querySelector('#shiny-modal .vftTutorialStartBtn')", 10))
clickEl("#shiny-modal .vftTutorialStartBtn")
ok("...whose button starts step 2's tour at hint 1", waitFor(atHint(1), 15))
key("Escape", 27)

cat(sprintf("\nscreenshots in %s\n", SHOTS))
b$close()
cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
