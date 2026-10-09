## The browser half of the heat-mitigation switch (HEAT_MITIGATION in
## R/features.R), in real headless Chrome but WITHOUT the app: the two pieces
## below are extracted from the sources they ship in, so this checks the code
## that runs, not a copy of it.
##
##  1. the nav bar's greyed Hitzeminderung button - that `.vft-nav-btn--off`
##     wins over the plain and the [disabled] rules it sits beside, in its own
##     colour and with an italic label (the label is a span INSIDE the button,
##     so the italic has to be inherited);
##  2. newVersions' context radios - that the Hitzeminderung choice is disabled
##     and STAYS disabled when the group is enabled wholesale, which is what
##     obsFinishRender's shinyjs::enable("contextChoice") does on every map
##     redraw, and that a click on it is reported as input$disabledClick (the
##     server answers that with vftNotImplementedModal()).
##
## Run:  Rscript data-raw/verify_heat_off.R
fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-62s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

#--- the shipped sources ------------------------------------------------------
lines <- function(f) readLines(f, warn = FALSE)

#one `sprintf("(function(){ ... })();", x)` block, found by a line inside it
jsBlock <- function(file, anchor) {
  txt <- lines(file)
  i <- grep(anchor, txt, fixed = TRUE)
  stopifnot(length(i) == 1)
  a <- max(grep('"(function(){', txt[1:i], fixed = TRUE))
  b <- i - 1L + grep('})();"', txt[i:length(txt)], fixed = TRUE)[1]
  s <- paste(txt[a:b], collapse = "\n")
  s <- sub('^[[:space:]]*"', "", s)
  s <- sub('\\}\\)\\(\\);".*$', "})();", s)
  gsub('\\\\"', '"', s)
}

#the nav bar's whole stylesheet, as vftStepNav() emits it
navCSS <- function() {
  txt <- lines("R/app_ui.R")
  a <- grep("tags$style(shiny::HTML(", txt, fixed = TRUE)
  a <- a[a > 400][1]
  b <- a - 1L + grep('^[[:space:]]*"\\)\\),?[[:space:]]*$', txt[a:length(txt)])[1]
  gsub('\\\\"', '"', paste(txt[(a + 1L):(b - 1L)], collapse = "\n"))
}

lockJS <- sprintf(jsBlock("R/newVersions_server.R",
                          "var g = document.getElementById('%s'); if(!g) return;"),
                  "newVersions-contextChoice")
whyJS  <- sprintf(jsBlock("R/newVersions_ui.R", "if(window.__vftWhyDisabled) return;"),
                  "newVersions-")
ok("the radio lock extracted", grepl("MutationObserver", lockJS))
ok("the disabled-click listener extracted", grepl("disabledClick", whyJS))
ok("the nav stylesheet extracted", grepl("vft-nav-btn--off", navCSS()))

#--- the page ----------------------------------------------------------------
#The banner buttons in their three states, and the context radios exactly as
#shiny::radioButtons(inline = TRUE) writes them: the input INSIDE its label,
#which is what makes a click on the label reach it (and what makes a disabled
#one swallow that click).
radio <- function(v, lab, checked = FALSE) sprintf(
  '<label class="radio-inline"><input type="radio" name="newVersions-contextChoice" value="%s"%s/><span>%s</span></label>',
  v, if (checked) " checked" else "", lab)

page <- sprintf('<!doctype html><html><head><meta charset="utf-8"><style>%s</style></head>
<body>
<div id="vftNav"><div class="vft-nav-center"><div class="vft-nav-group">
  <button id="plain" class="btn action-button vft-nav-btn"><span class="vft-nav-lab">Schritt 1</span></button>
  <button id="unreach" class="btn action-button vft-nav-btn" disabled><span class="vft-nav-lab">Schritt 4</span></button>
  <button id="off" class="btn action-button vft-nav-btn vft-nav-btn--off" aria-disabled="true"><span class="vft-nav-lab">Hitzeminderung</span></button>
</div></div></div>
<div class="vft-nv-ctx"><div id="newVersions-contextChoice" class="form-group shiny-input-radiogroup">
  <div class="shiny-options-group">%s%s%s</div>
</div></div>
<script>
  window.__sent = [];
  window.Shiny = { setInputValue: function(k, v){ window.__sent.push({k: k, v: v}); } };
</script>
</body></html>',
  navCSS(), radio("1", "Wegen/Strassen", TRUE), radio("3", "Parken/Wohnen"),
  radio("4", "Hitzeminderung"))

b <- chromote::ChromoteSession$new(width = 1200, height = 400)
js <- function(x) b$Runtime$evaluate(x, returnByValue = TRUE)$result$value
#a file, not a data: URL - the stylesheet alone is some 30 kB
html <- file.path(tempdir(), "vft_heat_off.html")
writeLines(page, html, useBytes = TRUE)
invisible(b$Page$navigate(paste0("file:///", gsub("\\\\", "/", normalizePath(html, winslash = "/")))))
Sys.sleep(1.5)
ok("page up", identical(js("!!document.getElementById('off')"), TRUE))

css <- function(id, prop) js(sprintf(
  "getComputedStyle(document.getElementById('%s')).%s", id, prop))
labCss <- function(id, prop) js(sprintf(
  "getComputedStyle(document.querySelector('#%s .vft-nav-lab')).%s", id, prop))

cat("\n=== 1. the greyed Hitzeminderung button ===\n")
ok("a reachable step is the pale fill",
   identical(css("plain", "backgroundColor"), "rgb(179, 208, 210)"),
   paste(css("plain", "backgroundColor")))
ok("an unreachable step outlines in black",
   identical(css("unreach", "borderTopColor"), "rgb(0, 0, 0)"),
   paste(css("unreach", "borderTopColor")))
ok("...and is not italic", identical(labCss("unreach", "fontStyle"), "normal"))
offCol <- css("off", "borderTopColor")
ok("the switched-off button outlines in grey-teal",
   identical(offCol, "rgb(143, 171, 173)"), paste(offCol))
ok("...the same colour as its label", identical(css("off", "color"), offCol),
   paste(css("off", "color")))
ok("...neither black nor the unreachable colour",
   !identical(offCol, css("unreach", "borderTopColor")))
ok("...on no fill of its own",
   identical(css("off", "backgroundColor"), "rgba(0, 0, 0, 0)"),
   paste(css("off", "backgroundColor")))
ok("...and its label is italic", identical(labCss("off", "fontStyle"), "italic"),
   paste(labCss("off", "fontStyle")))

cat("\n=== 2. the Hitzeminderung context radio ===\n")
r4 <- "document.querySelector('#newVersions-contextChoice input[value=\"4\"]')"
lab4 <- paste0(r4, ".closest('label')")
allLive <- "Array.prototype.every.call(document.querySelectorAll('#newVersions-contextChoice input:not([value=\"4\"])'), function(i){ return !i.disabled; })"
invisible(js(lockJS))
invisible(js(whyJS))
ok("the choice is disabled", identical(js(paste0(r4, ".disabled")), TRUE))
ok("...and greyed", identical(js(paste0(lab4, ".style.opacity")), "0.5"))
ok("the other two are not", identical(js(allLive), TRUE))

#a real click, where the label is
hit <- function(sel) {
  p <- js(sprintf("(function(){ var e = document.querySelector(%s); var r = e.getBoundingClientRect();
                     return [r.left + r.width/2, r.top + r.height/2]; })()",
                  jsonlite::toJSON(sel, auto_unbox = TRUE)))
  x <- p[[1]]; y <- p[[2]]
  b$Input$dispatchMouseEvent(type = "mouseMoved", x = x, y = y)
  b$Input$dispatchMouseEvent(type = "mousePressed", x = x, y = y, button = "left", clickCount = 1)
  b$Input$dispatchMouseEvent(type = "mouseReleased", x = x, y = y, button = "left", clickCount = 1)
  Sys.sleep(0.2)
}
hit("#newVersions-contextChoice input[value=\"4\"] + span")
ok("a click does not select it", identical(js(paste0(r4, ".checked")), FALSE))
sent <- js("window.__sent")
ok("...it is reported as input$disabledClick",
   length(sent) == 1 && identical(sent[[1]]$k, "newVersions-disabledClick"),
   paste(jsonlite::toJSON(sent, auto_unbox = TRUE)))
ok("...naming the choice, so the server knows which modal",
   length(sent) == 1 && identical(sent[[1]]$v$id, "newVersions-contextChoice") &&
     identical(sent[[1]]$v$value, "4"))

cat("\n=== 3. the group enabled wholesale (shinyjs::enable) ===\n")
#what shinyjs does to an input group: every input in it, by the property -
#this is what used to leave the choice greyed but live
invisible(js("(function(){ document.querySelectorAll('#newVersions-contextChoice input').forEach(function(i){ i.disabled = false; }); })()"))
ok("the lock puts it back", identical(js(paste0(r4, ".disabled")), TRUE))
ok("...and leaves the other two alone", identical(js(allLive), TRUE))
invisible(js("window.__sent = []"))
hit("#newVersions-contextChoice input[value=\"4\"] + span")
ok("a click after the enable still does not select it",
   identical(js(paste0(r4, ".checked")), FALSE))
ok("...and is still reported", length(js("window.__sent")) == 1)
hit("#newVersions-contextChoice input[value=\"3\"] + span")
ok("a live choice still selects", identical(js(
  "document.querySelector('#newVersions-contextChoice input[value=\"3\"]').checked"), TRUE))

invisible(b$close())
cat(sprintf("\n%s  (%d failed)\n", if (fails == 0) "ALL PASS" else "FAILURES", fails))
if (fails > 0) quit(status = 1)
