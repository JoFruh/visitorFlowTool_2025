#### Moving between steps ####

# Every step transition used to be written out by hand at its call site: set
# r$step, pick the right one of eight `trigger*` reactiveVals, test whether it
# was NULL, and either seed it with 1 or add 1 to it - then, somewhere else, in
# the observer that watches it, call updateTabsetPanel(). That is about twenty
# copies of the same four lines, plus five near-identical blocks mapping the
# banner letters "A".."E" back onto earlier steps, and the copies had already
# drifted apart: one of them bumped triggerStep4 with triggerStep5()'s value.
#
# It is all one function now. vftGoToStep() is the ONLY thing in the app that
# calls updateTabsetPanel(), so "which tab is showing" and "which step's server
# has been asked to run" cannot disagree.
#
# The triggers themselves live in session$userData rather than in app_server()'s
# frame. That is what lets the entry point be a plain function taking (r, step,
# session) instead of a closure over eight variables, and session$userData is
# per-session and non-reactive, so reading it costs no dependency.

#' The steps, in the order the user walks them.
#'
#' `tab` is the tabPanel value in the hidden tabsetPanel built by `app_ui()`.
#'
#' `code` is what gets written to `r$step`, which is the number the save file
#' records and the restore path switches on. It is NOT the position in this
#' list: `newVersions` is a side trip off step 5 and deliberately leaves
#' `r$step` alone, so that a save taken there restores to step 5.
VFT_STEPS <- list(
  step1       = list(tab = "tab_step1",       code = 1L),
  step2       = list(tab = "tab_step2",       code = 2L),
  step3       = list(tab = "tab_step3",       code = 3L),
  step4       = list(tab = "tab_step4",       code = 4L),
  step5       = list(tab = "tab_step5",       code = 5L),
  newVersions = list(tab = "tab_newVersions", code = NA_integer_),
  finalStep   = list(tab = "tab_finalStep",   code = 6L)
)

#' Where each banner letter goes back to.
#'
#' The step banners carry up to five clickable areas, and each step maps its own
#' click to a letter: "A" is step 1, "B" step 2, and so on. Only the steps
#' BEFORE the one being clicked from are reachable, which is why
#' `vftBackTarget()` takes the step it is being called from.
VFT_BANNER_STEPS <- c(A = "step1", B = "step2", C = "step3", D = "step4", E = "step5")

#' Rank of a step for the purpose of "is this letter a step backwards".
#' newVersions has no banner back-navigation, so it is not listed.
VFT_BANNER_RANK <- c(step1 = 1L, step2 = 2L, step3 = 3L, step4 = 4L, step5 = 5L,
                     finalStep = 6L)

#' Create the per-step navigation triggers for this session.
#'
#' Called once from `app_server()`. Each step gets one reactiveVal holding a
#' visit counter; the observer that builds that step's module server watches it
#' through `vftStepTrigger()`.
#'
#' The counters start at 0 so that bumping one is always `n + 1` - the
#' `is.null()` test at twenty call sites is what this replaces.
vftNavInit <- function(session){
  session$userData$vftNav <- lapply(
    stats::setNames(names(VFT_STEPS), names(VFT_STEPS)),
    function(ignored) shiny::reactiveVal(0L)
  )
  invisible(NULL)
}

#' The visit counter for one step, as a reactive read.
#'
#' Meant to be the expression of the `observeEvent()` that builds that step:
#' `observeEvent(vftStepTrigger(session, "step3"), { ... })`.
#'
#' A step that has not been visited reads as NULL rather than 0, so that the
#' default `ignoreNULL = TRUE` keeps every step observer from firing at session
#' start - which is what the old `reactiveVal()`s (NULL until first set) did,
#' and what the `if(triggerStepN() > 0)` guard inside each of them relied on.
#' step1 opts out with `ignoreNULL = FALSE`, because it does build at start.
vftStepTrigger <- function(session, step){
  n <- session$userData$vftNav[[step]]()
  if(n == 0L) NULL else n
}

#' Go to a step: record it, show its tab, and ask its server to run.
#'
#' This is the only supported way to change step. It is deliberately not a
#' reactive - call it from an observer.
#'
#' @param r the app-level `reactiveValues`.
#' @param step one of `names(VFT_STEPS)`.
#' @param session the app-level session (not a module's namespaced proxy).
vftGoToStep <- function(r, step, session = shiny::getDefaultReactiveDomain()){
  if(!step %in% names(VFT_STEPS))
    stop("vftGoToStep(): unknown step '", step, "'")

  spec <- VFT_STEPS[[step]]

  #r$step is the save file's idea of where the user is. newVersions has no code
  #of its own on purpose - see VFT_STEPS.
  if(!is.na(spec$code)) r$step <- spec$code

  vftDbg(paste0("NAV -> ", step))
  shiny::updateTabsetPanel(session = session, inputId = "tabs", selected = spec$tab)

  #bumping the counter is what makes the step's own observer run and build (or
  #rebuild) its module server.
  counter <- session$userData$vftNav[[step]]
  counter(shiny::isolate(counter()) + 1L)

  invisible(step)
}

#' Which step a banner letter means, when clicked from `from`.
#'
#' Returns NULL for anything that is not a letter naming an EARLIER step, which
#' covers the ordinary case of `confirm` holding a button count rather than a
#' letter. Callers reset the banner input themselves: the letter sticks on the
#' client, so leaving it would re-fire on the next visit.
vftBackTarget <- function(confirm, from){
  if(length(confirm) != 1L || !is.character(confirm)) return(NULL)
  if(!confirm %in% names(VFT_BANNER_STEPS)) return(NULL)
  target <- VFT_BANNER_STEPS[[confirm]]
  if(VFT_BANNER_RANK[[target]] >= VFT_BANNER_RANK[[from]]) return(NULL)
  target
}

#' Handle a step's `confirm` when it holds a banner letter.
#'
#' Returns TRUE if it did navigate, so a caller can tell "went back" from
#' "not a letter". Clears the banner input first, for the reason in
#' `vftBackTarget()`.
vftGoBack <- function(r, confirm, from, bannerId, session = shiny::getDefaultReactiveDomain()){
  target <- vftBackTarget(confirm, from)
  if(is.null(target)) return(FALSE)

  shinyjs::runjs(sprintf("Shiny.setInputValue('%s', 'O');", bannerId))
  vftGoToStep(r, target, session)
  TRUE
}
