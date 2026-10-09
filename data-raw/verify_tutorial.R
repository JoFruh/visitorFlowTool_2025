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
          unlist(lapply(names(VFT_TUTORIAL_VARIANTS), function(k)
          sprintf(":tut_%s_%s:", k, VFT_TUTORIAL_VARIANTS[[k]]))),
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
  alts <- unlist(lapply(pay, function(p) c(p$alts$step5[["7b"]], p$alts$step5[["9b"]])))
  ok("step 5's variant texts are there, and differ by language",
     length(alts) == 6 && length(unique(alts)) == 6 && !any(grepl("^:", alts)))
  ok("...and reach the browser as an object keyed '2b', '7b', '9b'",
     grepl("\"alts\":\\{\"step5\":\\{\"2b\":\"[^\"]+\",\"7b\":\"[^\"]+\",\"9b\":\"[^\"]+\"\\}",
           as.character(jsonlite::toJSON(pay$fr, auto_unbox = TRUE))))
  ok("step 2's added hint (6b) in every language, as an alt of its own",
     length(unique(vapply(pay, function(p) as.character(p$alts$step2[["6b"]]), ""))) == 3 &&
     !any(grepl("^:", vapply(pay, function(p) as.character(p$alts$step2[["6b"]]), ""))))
  ok("the data-loss warning: 'warning' in its button's red, in every language",
     all(vapply(pay, function(p) grepl("<em class=vftTutWarn>", p$tours$commit[1]), NA)) &&
     length(unique(vapply(pay, function(p) p$tours$commit[1], ""))) == 3)
  ok("save and load: both words teal and larger, in every language",
     all(vapply(pay, function(p) lengths(regmatches(p$tours$saveLoad[1],
                  gregexpr("<b><em>[^<]+</em></b>", p$tours$saveLoad[1]))) == 2, NA)))
  ok("the new term colours survive the CSV (sensitivity, AoI fill, path usage)",
     all(vapply(pay, function(p) grepl("<em class=vftTutSens>", p$tours$step2[3]) &&
                  grepl("<em class=vftTutAoiFill>", p$tours$step4[1]) &&
                  grepl("<em class=vftTutAoiFill>", p$tours$step4[5]) &&
                  grepl("<em class=vftTutUsage>", p$tours$step5[3]) &&
                  grepl("<i class=vftTutPolyDraw></i>", p$tours$step1[3]), NA)))
  ok("step 3's areas of interest in its map red, step 4's shapes in the drawing blue",
     all(vapply(pay, function(p) grepl("<em class=vftTutAoiRed>", p$tours$step3[2]) &&
                  grepl("<em class=vftTutShape>.+<i class=vftTutPolyDraw></i>", p$tours$step4[3]) &&
                  grepl("<em class=vftTutGrey>", p$tours$step4[4]), NA)))
  ok("'Biodiversity Sensitivity' carries its dark-red class",
     all(grepl("<em class=vftTutBio>", vapply(pay, function(p) p$tours$step5[6], ""))))
  s2 <- lapply(pay, function(p) as.character(p$tours$step2))
  ok("step 2: thirteen translated strings in every language",
     all(vapply(s2, function(s) length(s) == 13 && all(nzchar(s)) && !any(grepl("^:.*:$", s)), NA)) &&
     !any(unlist(s2[c("de", "fr")]) %in% unlist(VFT_TUTORIAL_FALLBACK)))
  ok("step 2: the step's name and 'Amphibians' teal and larger (<b><em>)",
     all(vapply(s2, function(s) all(grepl("<b><em>.+</em></b>", s[c(1, 4)])), NA)))
  nv <- lapply(pay, function(p) as.character(p$tours$newVersions))
  ok("newVersions: twenty-three translated strings in every language",
     all(vapply(nv, function(s) length(s) == 23 && all(nzchar(s)) && !any(grepl("^:.*:$", s)), NA)) &&
     !any(unlist(nv[c("de", "fr")]) %in% unlist(VFT_TUTORIAL_FALLBACK)))
  ok("newVersions: the classes of its terms and icons survive the CSV",
     all(vapply(nv, function(s) grepl("<em class=vftTutGrey>", s[6]) &&
                  grepl("<i class=vftTutNode></i>", s[7]) &&
                  grepl("<em class=vftTutDelete>", s[10]) &&
                  grepl("<i class=vftTutNode></i>", s[12]) &&
                  grepl("<i class=vftTutNodeSel></i>", s[16]) &&
                  grepl("<em class=vftTutShape>", s[18]) &&
                  grepl("<em class=vftTutResidence>.+<em class=vftTutParking>", s[19]) &&
                  grepl("<em class=vftTutNew>", s[21]) &&
                  grepl("<em class=vftTutGrey>", s[22]), NA)))
  ok("newVersions: the rename modal's text (21b) in every language",
     all(vapply(pay, function(p) {
       a <- as.character(p$alts$newVersions[["21b"]])
       length(a) == 1 && nzchar(a) && !grepl("^:.*:$", a)
     }, NA)) &&
     length(unique(vapply(pay, function(p) as.character(p$alts$newVersions[["21b"]]), ""))) == 3)
  ok("the heat mitigation hint (toHitze) is there in every language",
     all(vapply(pay, function(p) grepl("<em class=vftTutHeat>", p$tours$toHitze[1]), NA)) &&
     length(unique(vapply(pay, function(p) p$tours$toHitze[1], ""))) == 3)
  s4 <- lapply(pay, function(p) as.character(p$tours$step4))
  ok("step 4: six translated strings in every language, the cut's three pictures in hint 2",
     all(vapply(s4, function(s) length(s) == 6 && all(nzchar(s)) && !any(grepl("^:.*:$", s)) &&
                  grepl("<span class=vftTutIconRow><i class=vftTutCut1></i><i class=vftTutCut2></i><i class=vftTutCut3></i></span>",
                        s[2], fixed = TRUE) &&
                  grepl("<b><em>", s[6], fixed = TRUE), NA)) &&
     !any(unlist(s4[c("de", "fr")]) %in% unlist(VFT_TUTORIAL_FALLBACK)))
  hz <- lapply(pay, function(p) as.character(p$tours$hitze))
  ok("hitze: twenty-six translated strings in every language",
     all(vapply(hz, function(s) length(s) == 26 && all(nzchar(s)) && !any(grepl("^:.*:$", s)), NA)) &&
     !any(unlist(hz[c("de", "fr")]) %in% unlist(VFT_TUTORIAL_FALLBACK)))
  ok("hitze: the classes of its materials and tools survive the CSV",
     all(vapply(hz, function(s) all(grepl("<em class=vftTutHot>", s[c(1, 11)])) &&
                  grepl("<em class=vftTutGrass>", s[4]) &&
                  all(grepl("<em class=vftTutTree>", s[7:8])) &&
                  grepl("<em class=vftTutBlack>.+<em class=vftTutBlack>", s[16]) &&
                  grepl("<em class=vftTutBlack>", s[17]) &&
                  grepl("<b><em>.+</em></b>", s[18]) &&
                  grepl("<em class=vftTutArtificial>", s[20]) &&
                  grepl("<em class=vftTutCanopyArt>", s[23]) &&
                  all(grepl("<b><em>.+</em></b>", s[c(22, 24, 26)])), NA)))
  tb <- i18n$get_translations()
  ok("hitze: the plan import's buttons by the names the import gives them (Next, Pick, Apply)",
     all(mapply(function(s, lg) grepl(sprintf("<em>%s</em>", tb["Weiter", lg]), s[18], fixed = TRUE) &&
                  grepl(sprintf("<em>%s</em>", tb["Farbe aufnehmen", lg]), s[22], fixed = TRUE) &&
                  grepl(sprintf("<em>%s</em>", tb["Anwenden", lg]), s[24], fixed = TRUE),
                hz, names(hz))))
  ok("hitze: the plan import's material names in its hints (Artificial canopy)",
     all(mapply(function(s, lg) grepl(sprintf(">%s<", tb["Kuenstliche Krone", lg]), s[23], fixed = TRUE),
                hz, names(hz))))
}
ok("the plan the heat mitigation tour hands over is in www",
   file.exists(file.path(dirname(R), "inst/app/www/vft-tutorial-plan.png")))
ok("...and the JS asks for it by that name",
   any(grepl("www/vft-tutorial-plan.png", readLines(js, encoding = "UTF-8"), fixed = TRUE)))

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
  ok("on step 2 it raises step 2's tutorial modal",
     identical(modals, c("step1", "step2")) && length(clicks) == 0)
  session$userData$r$navStep <- "newVersions"
  session$setInputs(helpButton = 3)
  ok("on Neue Versionen it raises the scenarios page's tutorial modal",
     identical(modals, c("step1", "step2", "newVersions")) && length(clicks) == 0)
  session$userData$r$navContext <- "4"
  session$setInputs(helpButton = 4)
  ok("behind the Hitzeminderung door it raises the heat mitigation tour's modal",
     identical(modals, c("step1", "step2", "newVersions", "hitze")) && length(clicks) == 0,
     paste(clicks, collapse = ","))
})
utils::assignInNamespace("click", realClick, "shinyjs")
Sys.unsetenv("VFT_NAV")

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
