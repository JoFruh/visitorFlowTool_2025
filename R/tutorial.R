#### The guided tutorial ####
#
# One tour per step, played in the browser by inst/app/www/vft-tutorial.js - a
# port of FRISCH's first-use tutorial. A grey layer is laid over the page with
# windows cut into it over the control to use next, and a card beside them says
# what to do. The user does it for real, through the window, and the tour moves
# on when the page shows it was done. A hint that is only there to be read has a
# Next button instead.
#
# Everything about the tour itself - the hints, where their windows go, what
# counts as done - lives in the JS, because every one of those questions is
# about the page as drawn. What it needs from here is its texts in the language
# the user is in, and one R-side door: the help button's modal, which offers the
# tour of the step being shown.
#
# Two ways in:
#   * a bubble under the help button offers step 1's tour on the first visit.
#     Finishing, stopping or closing it is stored on the device
#     (localStorage `vft.tutorial.v1`) and it does not come back.
#   * the help button, on a step that has a tour, opens vftTutorialModal()
#     instead of that step's own help modal, and its button starts the tour from
#     that step's first hint.
#
# Tours CHAIN: step 1's last hint sends the user to the step they chose, and the
# JS starts that step's tour when the nav bar's ring lands there - if it has one.
# Adding a tour for another step is therefore a `TOURS.<key>` entry in the JS,
# its key here, and its texts; nothing about step 1's has to change.
#
# Depends on the nav bar (VFT_NAV): the help button, and the next-step modal
# step 1's tour walks through, only exist with it.

#' The tour keys that have a tour. Must match the keys of `TOURS` in
#' inst/app/www/vft-tutorial.js - data-raw/verify_tutorial.R checks they do.
#'
#' A key is the nav bar button the ring is on, minus its `vftNav_` prefix
#' (`vftTutorialKey()`): `step1`..`step5`, `newVersions`, or `hitze` for the
#' Hitzeminderung door, which is the newVersions tab on another context and so
#' needs a tour of its own.
VFT_TUTORIAL_TOURS <- c("step1", "step3")

#' How many hints each tour has; its texts are `:tut_<key>_1:` .. `_<n>:`.
VFT_TUTORIAL_HINTS <- c(step1 = 6L, step3 = 5L)

#' English stand-ins for a translation row that has not been added yet.
#'
#' `:token:` keys, not German literals, because the step texts are long HTML
#' and would make unwieldy keys - so a missing row would otherwise put
#' ":tut_step1_3:" on screen. English rather than German because English is
#' what the tour was written in. The CSVs carry all three languages.
#'
#' Step texts may use `<br>`, `<b>` (teal), `<em>` (bold, one size larger) and
#' `<em class=vftTutAoi>` (the same, in the areas of interest's green) - see
#' inst/app/www/vft-tutorial.css. The class is left unquoted: the CSV reader
#' drops a `"` inside a field, so a quoted one would not match the fallback.
VFT_TUTORIAL_FALLBACK <- list(
  ":tut_step1_1:" = "Welcome to Visitor Flow Tool.<br>Here you can quickly and easily explore the impacts of planning on biodiversity and heat mitigation, <em>anywhere in Switzerland</em>.<br>Let's explore how!",
  ":tut_step1_2:" = "You can upload shapefiles or a .kml file to determine your area of interest.",
  ":tut_step1_3:" = "Or quickly draw your own area on the map.<br>Let's take this area as an example.<br>Draw a polygon around Birmensdorf.",
  ":tut_step1_4:" = "Now that we have an area, we can hit <b>Confirm</b> to finish this step.",
  ":tut_step1_5:" = "There are now various stages you can choose from, depending on your interests.<br>Choose the one you're interested in!",
  ":tut_step1_6:" = "The tutorial will continue to your chosen next step.<br>If you interrupt the tutorial, you can restart it at any step you wish.<br>Simply click the help button on that step!",
  ":tut_step3_1:" = "To simulate recreation, we first need to specify <em class=vftTutAoi>Areas of Interest</em>.<br>These are the areas recreationists go to, to recreate.<br>For example: parks, forests, lakesides.",
  ":tut_step3_2:" = "In this sub-step, we quickly define Areas of Interest by sliding a bar.",
  ":tut_step3_3:" = "Slide the bar to the value of <b>8</b>.",
  ":tut_step3_4:" = "Now all areas with an attractivity above 8 are Areas of Interest.",
  ":tut_step3_5:" = "<b>Confirm</b> to go to the next sub-step and precisely edit the Areas of Interest.",
  ":tut_next:"       = "Next",
  ":tut_stop:"       = "Stop tutorial",
  ":tut_offer:"      = "New here? Take a short guided tour.",
  ":tut_start:"      = "Start tutorial",
  ":tut_help_title:" = "Need help with this step?"
)

#' One tutorial text, in `lang` or else the session's current language.
#'
#' With `lang`, straight out of the Translator's table - NOT through `t()`.
#' After usei18n(), `t()` answers in the per-session language update_lang()
#' keeps, and on a language change that is set by the step's own observer, a
#' round trip through the browser AFTER `r$currentLang` has moved. Asked from
#' the r$currentLang observer, `t()` therefore gave the texts of the language
#' before: French after switching back to German. The table has no such timing.
#'
#' Without `lang`, through .vftT() (R/async_helpers.R), which handles the
#' usei18n() tag trap - that is the modal's case, shown well after any change.
#' Either way a key the CSVs do not carry comes back as itself, the `:...:`
#' shape tested here, the same test vftNextStepModal()'s lab() makes.
vftTutorialTr <- function(key, session = shiny::getDefaultReactiveDomain(), lang = NULL){
  out <- if(!is.null(lang)) vftTutorialLookup(key, lang, session)
         else tryCatch(.vftT(session)(key), error = function(e) key)
  if(length(out) != 1L || is.na(out) || !nzchar(out) || grepl("^:.*:$", out)){
    fb <- VFT_TUTORIAL_FALLBACK[[key]]
    return(if(is.null(fb)) key else fb)
  }
  as.character(out)
}

#' A key's row in the session Translator's table, for one language; NA when
#' there is no Translator, no such row or no such language.
vftTutorialLookup <- function(key, lang, session = shiny::getDefaultReactiveDomain()){
  tr <- tryCatch(session$userData$vftI18n, error = function(e) NULL)
  if(is.null(tr)) return(NA_character_)
  tb <- tryCatch(tr$get_translations(), error = function(e) NULL)
  if(is.null(tb) || !key %in% rownames(tb) || !lang %in% colnames(tb)) return(NA_character_)
  as.character(tb[key, lang])
}

#' Everything the browser needs to say, in `lang` (the current language when
#' NULL).
#'
#' `lang` lets the JS hold a hint back until the texts of a language just picked
#' have arrived, rather than showing the old ones for a moment.
vftTutorialTexts <- function(session = shiny::getDefaultReactiveDomain(),
                             lang = NULL){
  tr <- function(key) vftTutorialTr(key, session, lang)
  tours <- lapply(stats::setNames(VFT_TUTORIAL_TOURS, VFT_TUTORIAL_TOURS), function(k)
    vapply(sprintf(":tut_%s_%d:", k, seq_len(VFT_TUTORIAL_HINTS[[k]])),
           tr, character(1), USE.NAMES = FALSE))
  list(lang  = if(is.null(lang)) "" else as.character(lang),
       next_ = tr(":tut_next:"),
       stop  = tr(":tut_stop:"),
       offer = tr(":tut_offer:"),
       start = tr(":tut_start:"),
       #a list of character vectors; each goes to the browser as an array, even
       #a tour of one hint (I() keeps jsonlite from unboxing it)
       tours = lapply(tours, I))
}

#' Which tour the step being shown would play.
#'
#' The ring's button rather than `r$navStep`, because Hitzeminderung is the
#' newVersions tab on another context and needs its own tour - the same
#' distinction vftNavCurrentId() exists to make. Before the first navigation the
#' ring is not set yet, and the user is on step 1.
vftTutorialKey <- function(r){
  id <- shiny::isolate(vftNavCurrentId(r))
  if(is.null(id)) "step1" else sub("^vftNav_", "", id)
}

#' The help button's modal, on a step that has a tour.
#'
#' Teal, like the next-step modal it sits beside (VFT_NEXT_CSS in
#' R/navigation.R), and scoped to #shiny-modal the same way so it cannot reach
#' any other modal. Its button is plain markup calling vftTutorialStart(), which
#' closes this modal and starts the tour from `key`'s first hint - the tour runs
#' in the browser, so there is nothing for R to do on the click.
#'
#' Translated at show time through vftTutorialTr(), for the reason
#' vftNextStepModal() gives: a modal built per click is always in the language
#' the user is in now.
VFT_TUTORIAL_MODAL_CSS <- "
#shiny-modal .modal-content { background-color:#006268; border:none;
    border-radius:4px; box-shadow:0 10px 40px rgba(0,0,0,.35); }
#shiny-modal .modal-body { padding:34px 30px 34px; text-align:center; }
#shiny-modal .vft-tut-head { color:#ffffff; font-size:26px; font-weight:700;
    line-height:1.25; margin:0 0 24px;
    font-family:'FranklinGFB','FranklinGothic','Franklin Gothic Book',
                'Libre Franklin',Arial,sans-serif; }
"

vftTutorialModal <- function(session = shiny::getDefaultReactiveDomain(), key = "step1",
                             lang = NULL){
  shiny::showModal(shiny::modalDialog(
    footer = NULL, easyClose = TRUE,
    shiny::tags$style(shiny::HTML(VFT_TUTORIAL_MODAL_CSS)),
    shiny::tags$div(class = "vft-tut-head", vftTutorialTr(":tut_help_title:", session, lang)),
    shiny::tags$button(type = "button", class = "btn vftTutorialStartBtn",
                       onclick = sprintf("vftTutorialStart('%s')", key),
                       vftTutorialTr(":tut_start:", session, lang))
  ), session = session)
  invisible(NULL)
}

#' Keep the browser's tutorial texts in the current language.
#'
#' Sent once at session start - which is also what lets the offer bubble appear
#' - and again on every language change. `r$currentLang` is written by
#' vftLangServer() alongside the Translator's language, so by the time this
#' runs the Translator already answers in the new one.
#'
#' Does nothing without the nav bar: there is no help button to start a tour
#' from, and no next-step modal for step 1's tour to walk through.
vftTutorialServer <- function(r, session = shiny::getDefaultReactiveDomain()){
  if(!vftNavEnabled()) return(invisible(NULL))
  shiny::observeEvent(r$currentLang, {
    lang <- r$currentLang
    if(is.null(lang)) lang <- "de"
    session$sendCustomMessage("vft-tutorial-texts", vftTutorialTexts(session, lang))
  }, ignoreNULL = FALSE)
  invisible(NULL)
}
