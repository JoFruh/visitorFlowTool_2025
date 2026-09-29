## Browser half of the polygon drawer (inst/app/www/polydraw.js), in headless
## Chrome: vertices, the X that removes one, Escape/Backspace, closing on the
## first vertex or a double-click, and step 1's live area warning.
## data-raw/verify_polydraw.R covers what R does with the result.
##
## A bare leaflet widget with the drawer attached, saved to a temp page; Shiny
## is a stub that records what would have been sent. Real mouse and key events
## go through the DevTools protocol, so :hover, click counts and dblclick are
## the browser's own.
##
## Run:  Rscript data-raw/verify_polydraw_browser.R
suppressPackageStartupMessages({library(htmltools)})
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
env <- new.env(parent = globalenv())
for (f in sort(list.files(R, pattern = "[.][Rr]$", full.names = TRUE))) {
  suppressWarnings(try(sys.source(f, envir = env), silent = TRUE))
}
attach(env, warn.conflicts = FALSE)

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

#### the page ####
dir <- file.path(tempdir(), "polydraw_browser"); dir.create(dir, showWarnings = FALSE)
map <- leaflet::leaflet(width = "800px", height = "600px", elementId = "t-map",
                        options = leaflet::leafletOptions(doubleClickZoom = FALSE)) |>
  leaflet::setView(8.54, 47.37, 13)
map <- vftPolyDraw(map, list(ns = function(x) paste0("t-", x)), areaWarn = "aw")
page <- tagList(
  tags$script(HTML("window.sent = []; window.Shiny = { setInputValue: function(n, v){ window.sent.push([n, v]); } };")),
  tags$script(HTML(paste(readLines(file.path(dirname(R), "inst/app/www/polydraw.js"), encoding = "UTF-8"), collapse = "\n"))),
  tags$input(id = "box", type = "text"),
  tags$div(id = "t-aw", "warning"),
  map)
save_html(page, file.path(dir, "index.html"), libdir = "lib")

b <- chromote::ChromoteSession$new(width = 1000, height = 800)
invisible(b$Page$navigate(paste0("file:///", normalizePath(file.path(dir, "index.html"), winslash = "/"))))
Sys.sleep(2)
js <- function(x) b$Runtime$evaluate(x, returnByValue = TRUE)$result$value

#map pixel -> page pixel
r0 <- js("(function(){ var r = document.getElementById('t-map').getBoundingClientRect(); return [r.left, r.top]; })()")
at <- function(x, y) c(r0[[1]] + x, r0[[2]] + y)
move <- function(x, y) { p <- at(x, y); b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[1], y = p[2]); Sys.sleep(0.05) }
click <- function(x, y, count = 1) {
  p <- at(x, y)
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = p[1], y = p[2])
  b$Input$dispatchMouseEvent(type = "mousePressed", x = p[1], y = p[2], button = "left", clickCount = count)
  b$Input$dispatchMouseEvent(type = "mouseReleased", x = p[1], y = p[2], button = "left", clickCount = count)
  Sys.sleep(0.08)
}
dbl <- function(x, y) { click(x, y, 1); click(x, y, 2); Sys.sleep(0.1) }
key <- function(k, code) {
  b$Input$dispatchKeyEvent(type = "rawKeyDown", key = k, code = k, windowsVirtualKeyCode = code)
  b$Input$dispatchKeyEvent(type = "keyUp", key = k, code = k, windowsVirtualKeyCode = code)
  Sys.sleep(0.05)
}
nV     <- function() js("document.querySelectorAll('#t-map .vft-pd-v').length")
nSent  <- function() js("window.sent.length")
lastN  <- function() js("window.sent.length ? window.sent[window.sent.length - 1][1].lng.length : 0")
drawing <- function() js("document.getElementById('t-map').classList.contains('vft-pd-drawing')")
#is the X of the vertex nearest (x, y) showing?
xShown <- function() js("(function(){ var h = document.querySelectorAll('#t-map .vft-pd-rm:hover .vft-pd-x');
                        return h.length === 1 && getComputedStyle(h[0]).display === 'flex'; })()")
hovered <- function() js("document.querySelectorAll('#t-map .vft-pd-v:hover').length")

cat("=== 1. placing and taking back ===\n")
ok("the drawer attached", isTRUE(js("!!window.vftPolyDraw")))
click(200, 200); click(400, 200); click(400, 400)
ok("three clicks, three vertices", nV() == 3)
ok("...and a drawing in progress", isTRUE(drawing()))

#the vertex just placed is under the pointer: no X on it
move(401, 401)
ok("the vertex just placed shows no X under the pointer", hovered() == 1 && !isTRUE(xShown()))
click(400, 400)
ok("...and a click on it does nothing", nV() == 3 && nSent() == 0)
move(300, 300); move(400, 400)
ok("once the pointer has left it, it shows its X", isTRUE(xShown()))

move(300, 300); move(400, 200)
ok("the pointer over a middle vertex shows its X", isTRUE(xShown()))
click(400, 200)
ok("...a click on the X removes that vertex", nV() == 2 && nSent() == 0)

move(300, 300); move(200, 200)
ok("the first vertex never shows an X", hovered() == 1 && !isTRUE(xShown()))

key("Backspace", 8)
ok("Backspace takes back the last vertex", nV() == 1)
key("Escape", 27)
ok("Escape the one before it, which ends the drawing", nV() == 0 && !isTRUE(drawing()))

click(200, 200); click(400, 200)
invisible(js("document.getElementById('box').focus()"))
key("Backspace", 8)
ok("Backspace in a text box is left to the text box", nV() == 2)
invisible(js("document.getElementById('box').blur()"))
key("Escape", 27); key("Escape", 27)

cat("\n=== 2. closing ===\n")
click(200, 200); click(400, 200); click(400, 400)
move(300, 300); move(400, 400); click(400, 400)
ok("a click on the LAST vertex no longer closes: it removes it", nV() == 2 && nSent() == 0)
click(400, 400)
move(300, 300); move(200, 200); click(200, 200)
ok("a click on the first vertex closes the ring", nSent() == 1 && lastN() == 3 && nV() == 0)

Sys.sleep(0.6)
click(200, 200); click(400, 200); click(400, 400)
dbl(200, 400)
ok("a double-click closes it, with the vertex it placed", nSent() == 2 && lastN() == 4 && nV() == 0)

Sys.sleep(0.6)
click(200, 200); click(400, 200); click(400, 400); click(200, 400)
move(300, 300); move(400, 200)
dbl(400, 200)
ok("a double-click on an X removes one vertex and adds none", nV() == 3)
ok("...and closes nothing", nSent() == 2)
key("Escape", 27); key("Escape", 27); key("Escape", 27)

cat("\n=== 3. the live area warning ===\n")
aw <- function() js("document.getElementById('t-aw').className")
fill <- function() js("(function(){ var p = document.querySelector('#t-map .leaflet-vftDraw-pane path[fill]');
                     return p ? p.getAttribute('fill') : null; })()")
ok("nothing drawn, nothing on the warning", identical(aw(), ""))
click(300, 300)
move(350, 350)
ok("a small ring is live but not over", identical(aw(), "vft-pd-live"))
ok("...and drawn blue", identical(tolower(fill()), "#5ab4f0"))
invisible(js("(function(){ var m = HTMLWidgets.find('#t-map').getMap(); m.setZoom(10, {animate: false}); })()"))
Sys.sleep(0.3)
move(300, 300); move(790, 590)           #~ 13 x 10 km at zoom 10
ok("the pointer pulled out past the ceiling raises it", grepl("vft-pd-over", aw()))
ok("...and turns the preview red", identical(tolower(fill()), "#dd1717"))
move(310, 310)
ok("back in, it drops again", !grepl("vft-pd-over", aw()) && identical(tolower(fill()), "#5ab4f0"))
move(790, 590)
key("Escape", 27)
ok("ending the drawing clears both classes", identical(aw(), ""))

cat("\n=== 4. step 4's scissors ===\n")
#the same map, re-attached as step 4 attaches it: attach() on a map it already
#knows only swaps the options
invisible(js("(function(){ var el = document.getElementById('t-map');
  window.vftPolyDraw.attach(el, HTMLWidgets.find('#t-map'), {ns: 't-', cut: true}); })()"))
invisible(js("void HTMLWidgets.find('#t-map').getMap().setZoom(13, {animate: false})"))
Sys.sleep(0.3)
n0 <- nSent()
click(200, 200); click(400, 200)
ok("two vertices arm the scissors", js("document.querySelectorAll('#t-map .vft-pd-scissors').length") == 1)
ok("...and the second is not also a removable vertex", nV() == 1)
move(300, 300); move(400, 200); click(400, 200)
ok("a click on the scissors sends a cut", nSent() == n0 + 1 &&
     identical(js("window.sent[window.sent.length - 1][0]"), "t-polyCut") && nV() == 0)
Sys.sleep(0.6)
click(200, 200); dbl(400, 200)
ok("a double-click that places vertex 2 does not cut", nSent() == n0 + 1 && nV() == 1)
key("Escape", 27)
ok("Escape takes the scissors' vertex back", nV() == 1 &&
     js("document.querySelectorAll('#t-map .vft-pd-scissors').length") == 0)

invisible(b$close())
cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
