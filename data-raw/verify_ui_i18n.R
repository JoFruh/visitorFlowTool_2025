## Verification for the one i18n trap that reaches the UI:
##   shiny.i18n::usei18n() switches the Translator into client-side mode, after
##   which i18n$t() returns a <span class="i18n" data-key="..."> TAG instead of
##   a string. A tag is a three-element list, so anywhere a bare string was
##   expected it silently becomes three values - and three of them in a row
##   become nine. That is how the Hitzeminderung time-of-day selectInput came to
##   fail the whole app at startup with
##     'names' attribute [9] must be the same length as the vector [3]
##
## Run:  Rscript data-raw/verify_ui_i18n.R
##
## The order below is the point. Everything is checked AFTER usei18n() has run,
## because before it every one of these passes whether the code is right or not.
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
  cat(sprintf("%-62s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

tables <- tryCatch(vftData("tables"), error = function(e) "")
if (!nzchar(tables) || !dir.exists(tables)) {
  cat("translation tables not present - skipping\n")
  cat(sprintf("\n%d check(s) failed\n", fails)); quit(status = if (fails == 0) 0 else 1)
}
i18n <- shiny.i18n::Translator$new(translation_csvs_path = tables, separator_csv = ";")
LANGS <- c("de", "fr", "en")
i18n$set_translation_language("de")

cat("=== 1. before usei18n (the state that hid the bug) ===\n")
ok("t() is a plain string here", is.character(suppressWarnings(i18n$t("Morgen"))))

cat("\n=== 2. after usei18n - t() is a tag ===\n")
invisible(shiny.i18n::usei18n(i18n))
ok("t() now returns a shiny.tag, not a string",
   inherits(suppressWarnings(i18n$t("Morgen")), "shiny.tag"))
ok("...and that tag is length 3, which is what broke setNames",
   length(suppressWarnings(i18n$t("Morgen"))) == 3)

cat("\n=== 3. vftTrText unwraps it ===\n")
for (lg in LANGS) {
  i18n$set_translation_language(lg)
  v <- vftTrText(i18n, "Morgen")
  ok(sprintf("vftTrText is one string in %s (%s)", lg, v),
     is.character(v) && length(v) == 1L && nzchar(v))
}
i18n$set_translation_language("de")
ok("an unknown key falls back to the key itself",
   identical(vftTrText(i18n, "::no such key::"), "::no such key::"))
ok("a NULL Translator falls back to the key", identical(vftTrText(NULL, "Morgen"), "Morgen"))

cat("\n=== 4. the three languages really differ ===\n")
## the whole point of unwrapping rather than returning the key: a fallback that
## quietly hands back German for every language would pass every check above
lab <- sapply(LANGS, function(lg) { i18n$set_translation_language(lg)
                                   vftTrText(i18n, "Morgen") })
ok(sprintf("Morgen differs by language (%s)", paste(lab, collapse = " / ")),
   length(unique(lab)) == 3)
i18n$set_translation_language("de")

cat("\n=== 5. vftTrAll survives usei18n ===\n")
## It is only correct today because vftStepNav() happens to run before the first
## step module calls usei18n(). Nothing at the call site says so, so it is
## checked in the harder order.
all_de <- vftTrAll(i18n, "Morgen")
ok(sprintf("vftTrAll gives three different languages (%s)",
           paste(all_de, collapse = " / ")), length(unique(all_de)) == 3)
ok("vftTrAll leaves the language it found", identical(i18n$get_translation_language(), "de"))

cat("\n=== 6. the select's choices ===\n")
for (lg in LANGS) {
  i18n$set_translation_language(lg)
  ch <- heatBinChoices(i18n)
  ok(sprintf("heatBinChoices is 3 named strings in %s (%s)", lg,
             paste(names(ch), collapse = "/")),
     length(ch) == 3 && length(names(ch)) == 3 && is.character(names(ch)) &&
       all(nzchar(names(ch))))
  ok(sprintf("...and its values are still HEAT_BINS in %s", lg),
     identical(unname(ch), HEAT_BINS))
}
ok("heatBinChoices works with no Translator at all",
   length(heatBinChoices(NULL)) == 3)

cat("\n=== 7. no stray files in the translation folder ===\n")
## shiny.i18n globs the folder with pattern = "translation_.*[.]csv", which is
## NOT anchored: translation_de.csv.bak_whatever matches it too, and every such
## backup is loaded and merged as if it were live. A backup identical to the
## live file merges away harmlessly, which is why 12 of them sat here for weeks
## doing nothing. One that differs in a single value does not: the same key then
## carries two different translations, and the Translator dies at construction
## with "duplicate 'row.names' are not allowed" - taking the whole app with it,
## before any of this file's other checks get a chance to run.
##
## That is what happened when a French accent was corrected in the live file
## while a backup beside it still held the old spelling. Backups belong in
## tables/translation_backup/, which already exists and is outside the glob.
stray <- list.files(tables, pattern = "translation_.*[.]csv")
extra <- setdiff(stray, paste0("translation_", c("de", "fr", "en", "or"), ".csv"))
ok(sprintf("only the four live CSVs match shiny.i18n's glob (%d files)", length(stray)),
   length(extra) == 0,
   if (length(extra)) paste("stray:", paste(utils::head(extra, 4), collapse = ", ")) else "")

cat("\n=== 8. the UI actually builds ===\n")
i18n$set_translation_language("de")
r1 <- try(newVersions_ui("nv", i18n), silent = TRUE)
ok("newVersions_ui() builds after usei18n", !inherits(r1, "try-error"),
   if (inherits(r1, "try-error")) conditionMessage(attr(r1, "condition")) else "")
r2 <- try(app_ui(), silent = TRUE)
ok("app_ui() builds", !inherits(r2, "try-error"),
   if (inherits(r2, "try-error")) conditionMessage(attr(r2, "condition")) else "")

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
