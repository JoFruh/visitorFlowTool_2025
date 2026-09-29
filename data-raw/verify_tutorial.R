## R half of the guided tutorial (R/tutorial.R, inst/app/www/vft-tutorial.js):
## its texts in three languages, the tour keys R and the JS agree on, and the
## help button answering with the tutorial modal on a step that has a tour.
## data-raw/verify_tutorial_browser.R walks the tour itself in the real app.
##
## Run:  Rscript data-raw/verify_tutorial.R
suppressPackageStartupMessages({library(shiny)})
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
LANGS <- c("de", "fr", "en")

cat("=== 1. the tours R and the JS know of ===\n")
## the working tree's copy, never system.file(): that is the installed
## package's, which can be stale
js <- file.path(dirname(R), "inst/app/www/vft-tutorial.js")
ctx <- V8::v8()
ctx$eval("var window = {}; var document = {}; var performance = { now: function(){ return 0; } };")
ctx$eval(paste(readLines(js, encoding = "UTF-8"), collapse = "\n"))
tours <- ctx$call("window.vftTutorialTours")
ok(sprintf("JS TOURS keys = VFT_TUTORIAL_TOURS (%s)", paste(names(tours), collapse = ", ")),
   setequal(names(tours), VFT_TUTORIAL_TOURS))
for (k in VFT_TUTORIAL_TOURS)
  ok(sprintf("%s: %d hints in the JS, %d texts in R", k, tours[[k]], VFT_TUTORIAL_HINTS[[k]]),
     identical(as.integer(tours[[k]]), VFT_TUTORIAL_HINTS[[k]]))
keys <- c(unlist(lapply(VFT_TUTORIAL_TOURS, function(k)
          sprintf(":tut_%s_%d:", k, seq_len(VFT_TUTORIAL_HINTS[[k]])))),
          ":tut_next:", ":tut_stop:", ":tut_offer:", ":tut_start:", ":tut_help_title:")
ok("every key has an English fallback", all(keys %in% names(VFT_TUTORIAL_FALLBACK)))

cat("\n=== 2. the translation rows ===\n")
tables <- tryCatch(vftData("tables"), error = function(e) "")
if (!nzchar(tables) || !dir.exists(tables)) {
  cat("translation tables not present - skipping sections 2-3\n")
} else {
  files <- file.path(tables, paste0("translation_", c("de", "en", "fr", "or"), ".csv"))
  for (f in files) {
    k <- sub(";.*$", "", readLines(f, encoding = "UTF-8"))
    ok(sprintf("all %d keys in %s", length(keys), basename(f)), all(keys %in% k))
  }
  i18n <- suppressWarnings(shiny.i18n::Translator$new(translation_csvs_path = tables,
                                                      separator_csv = ";"))
  ok("a real Translator$new() round-trip", inherits(i18n, "Translator"))

  cat("\n=== 3. the payload, after usei18n (the tag trap) ===\n")
  invisible(shiny.i18n::usei18n(i18n))
  sess <- list(userData = list(vftI18n = i18n))
  pay <- lapply(stats::setNames(LANGS, LANGS), function(lg) {
    i18n$set_translation_language(lg)
    suppressWarnings(vftTutorialTexts(sess, lg))
  })
  i18n$set_translation_language("de")
  for (lg in LANGS) {
    p <- pay[[lg]]
    s1 <- as.character(p$tours$step1)
    ok(sprintf("%s: six plain strings for step 1", lg),
       is.character(s1) && length(s1) == 6 && all(nzchar(s1)))
    ok(sprintf("%s: none of them a bare key or a fallback", lg),
       !any(grepl("^:.*:$", s1)) &&
       (lg == "en" || !any(s1 %in% unlist(VFT_TUTORIAL_FALLBACK))))
    ok(sprintf("%s: next/stop/offer/start are strings (%s)", lg, p$next_),
       all(vapply(p[c("next_", "stop", "offer", "start")],
                  function(x) is.character(x) && length(x) == 1 && !grepl("^:", x), logical(1))))
    ok(sprintf("%s: the payload names its language", lg), identical(p$lang, lg))
  }
  ## the bug this guards: on a language change the server asks for the new
  ## language while t() still answers in the old one (update_lang() lands a
  ## round trip later), so switching fr -> de sent French texts labelled "de"
  i18n$set_translation_language("fr")
  lagged <- suppressWarnings(vftTutorialTexts(sess, "de"))
  i18n$set_translation_language("de")
  ok("texts follow the language asked for, not the Translator's",
     identical(as.character(lagged$tours$step1), as.character(pay$de$tours$step1)) &&
     identical(lagged$next_, "Weiter"))

  firsts <- vapply(pay, function(p) as.character(p$tours$step1)[1], character(1))
  ok("hint 1 differs in all three languages", length(unique(firsts)) == 3)
  ok("hint 1 carries its <em> in every language", all(grepl("<em>.+</em>", firsts)))
  ok("the tours array survives JSON as an array",
     grepl("\"step1\":\\[", as.character(jsonlite::toJSON(pay$de, auto_unbox = TRUE))))
}

cat("\n=== 4. a missing row falls back, it never shows a key ===\n")
bare <- list(userData = list(vftI18n = NULL))
ok("no Translator: the English fallback",
   identical(vftTutorialTr(":tut_next:", bare), "Next"))
ok("an unknown key with no fallback comes back as itself",
   identical(vftTutorialTr(":tut_nothing:", bare), ":tut_nothing:"))

cat("\n=== 5. which tour a page plays ===\n")
rv <- function(...) { r <- reactiveValues(...); r }
isolate({
  ok("before any navigation: step1", identical(vftTutorialKey(rv()), "step1"))
  ok("on step 2: step2", identical(vftTutorialKey(rv(navStep = "step2")), "step2"))
  ok("behind the Hitzeminderung door: hitze",
     identical(vftTutorialKey(rv(navStep = "newVersions", navContext = "4")), "hitze"))
  ok("on Neue Versionen: newVersions",
     identical(vftTutorialKey(rv(navStep = "newVersions", navContext = "1")), "newVersions"))
})

cat("\n=== 6. the help button ===\n")
Sys.setenv(VFT_NAV = "1")
modals <- character(0); clicks <- character(0)
assign("vftTutorialModal", function(session, key, lang = NULL) modals <<- c(modals, key), envir = env)
realClick <- shinyjs::click
utils::assignInNamespace("click", function(id, ...) clicks <<- c(clicks, id), "shinyjs")
testServer(function(input, output, session) {
  r <- reactiveValues(navStep = "step1")
  vftNavBannerProxyServer(r, input, session)
  session$userData$r <- r
}, {
  session$flushReact()
  session$setInputs(helpButton = 1)
  ok("on step 1 the help button raises the tutorial modal",
     identical(modals, "step1") && length(clicks) == 0)
  session$userData$r$navStep <- "step2"
  session$setInputs(helpButton = 2)
  ok("on step 2 (no tour yet) it still opens step 2's own help",
     identical(modals, "step1") && identical(clicks, "step2-helpButton2"),
     paste(clicks, collapse = ","))
})
utils::assignInNamespace("click", realClick, "shinyjs")
Sys.unsetenv("VFT_NAV")

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
