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

#' The step registry - `VFT_STEPS`, and everything that reads it - lives in
#' R/steps.R. It moved there when it grew the `needs` field, because "what does
#' this step require" is answered in several places (the nav bar, the
#' availability check below, Stage 4's providers) while "how do I get there" is
#' answered only here.

#' Where each banner letter goes back to.
#'
#' The step banners carry up to five clickable areas, and each step maps its own
#' click to a letter: "A" is step 1, "B" step 2, and so on. Only the steps
#' BEFORE the one being clicked from are reachable, which is why
#' `vftBackTarget()` takes the step it is being called from.
VFT_BANNER_STEPS <- c(A = "step1", B = "step2", C = "step3", D = "step4", E = "step5")

#' Rank of a step for the purpose of "is this letter a step backwards".
#' newVersions has no banner back-navigation, so it is not listed - nor is there
#' anything after step 5 to be listed: the Resultate page was removed 2026-08-27.
VFT_BANNER_RANK <- c(step1 = 1L, step2 = 2L, step3 = 3L, step4 = 4L, step5 = 5L)

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

#' Forward the single app-level banner controls to whichever step is current.
#'
#' The banner used to be six copies, one per step, each wired to that step's
#' own languageSelect_N / helpButtonN / infoButtonN and the (large, per-step)
#' behaviour hanging off them - zoom warnings, HTML re-rendered in the chosen
#' language, and so on. Rewriting all of that into one shared implementation
#' was judged too large and risky to bundle with hoisting the banner to app
#' level (decided 2026-08-27), so instead every step keeps its own input,
#' just hidden - see the six steps' *_ui.R - and this is what drives whichever
#' one belongs to `r$navStep`, as if the user had clicked it there directly.
#' Nothing about the six steps' own server code had to change for this.
#'
#' Fully-qualified ids (`shiny::NS(step, ...)`), not module-relative ones:
#' this observer runs at app level, outside every module namespace, the same
#' way app_server() already talks to `step2-banner` etc. directly.
#'
#' `current()` falls back to step1 before any navigation has happened (or for
#' an unrecognised step, which should not occur) rather than doing nothing -
#' step1 is always live from session start, so the language selector and
#' help/info buttons work immediately.
vftNavBannerProxyServer <- function(r, input, session = shiny::getDefaultReactiveDomain()){
  if(!vftNavEnabled()) return(invisible(NULL))

  current <- function(){
    step <- shiny::isolate(r$navStep)
    if(is.null(step) || is.null(VFT_BANNER_PROXY[[step]])) "step1" else step
  }

  shiny::observeEvent(input$languageSelect, {
    step <- current()
    shiny::updateSelectInput(session, inputId = shiny::NS(step, VFT_BANNER_PROXY[[step]]$lang),
                             selected = input$languageSelect)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$helpButton, {
    step <- current()
    shinyjs::click(id = shiny::NS(step, VFT_BANNER_PROXY[[step]]$help))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$infoButton, {
    step <- current()
    shinyjs::click(id = shiny::NS(step, VFT_BANNER_PROXY[[step]]$info))
  }, ignoreInit = TRUE)

  invisible(NULL)
}

#' The chosen language, as ONE app-level fact.
#'
#' `r$currentLang` is what every step's enter() reads to decide which banner to
#' show, which language to put its own selector in, and - through
#' `shiny.i18n::update_lang()` - which language the client-side translation of
#' the whole page runs in. So a stale value there does not merely mislabel one
#' control: entering a step REWRITES the page's language from it.
#'
#' Nothing kept it current. It was written in seven places, all of them confirm
#' handlers in app_server(), all of them from the module's returned
#' `currentLang`, and that return is `reactive(i18n()$get_translation_language())`
#' - a reactive over an R6 field. The field is mutated in place, which is not a
#' reactive event, and the only reactive it reads (`i18n()`) is a constant. So it
#' caches the first language it is ever asked for and answers that forever. While
#' the six modules were rebuilt on every visit that was invisible: a fresh module
#' meant a fresh reactive, evaluated on the spot. Stage 5 made them singletons,
#' the reactives stopped being rebuilt, and the cached "de" they were holding
#' became the value every confirm wrote back.
#'
#' The second half of the same defect: with the selector hoisted into the nav bar
#' (vftNavBannerProxyServer above), a language change is a change to the CURRENT
#' step's hidden selector, and three of the six steps' own observers never wrote
#' `r$currentLang` at all. Choosing French on step 3 and walking to step 4 there-
#' fore carried "de" into step 4's enter(), which called `update_lang("de")` and
#' put the page back into German - with the nav selector still reading Français,
#' because nothing had asked it to change.
#'
#' Hence one owner. Every way a language can be chosen is an input event on a
#' select: the app-level one in the nav bar, or one of the six per-step ones
#' (which are what the nav bar drives, and what the user clicks directly in a
#' build without the bar). All seven are observed here, and this is the only
#' place `r$currentLang` is written outside a restore.
#'
#' The Translator is set here too. It is the same R6 object every module holds,
#' so setting it once covers the steps whose own observers forget to - step 3's
#' English branch never called it, and no "it" branch anywhere does.
#'
#' The mirror at the end is for the path that has no input event behind it: a
#' restore writes `r$currentLang` out of a save file or the browser snapshot, and
#' the nav bar - static markup that swaps its labels on `shiny:inputchanged` -
#' would otherwise go on showing the language the session started in. Guarded on
#' the selector's own value so it sends nothing on the ordinary path, where the
#' selector is already where the user put it.
vftLangServer <- function(r, i18n, input, session = shiny::getDefaultReactiveDomain()){

  langs <- c("de", "fr", "en")

  set <- function(lang){
    if(is.null(lang) || !nzchar(lang)) return(invisible(NULL))
    r$currentLang <- lang
    #not every language the selectors can name is one the CSVs carry ("it" is in
    #two of the step observers and in none of the translation files), and
    #set_translation_language() stops with an error on those. The banner and the
    #client-side swap still do what they can with it, exactly as before.
    if(lang %in% langs) try(i18n$set_translation_language(lang), silent = TRUE)
    invisible(NULL)
  }

  #the nav bar's own selector. NULL - and so never firing - in a build where
  #vftStepNav() rendered nothing.
  shiny::observeEvent(input$languageSelect, set(input$languageSelect))

  #the six hidden per-step selectors. Fully-qualified ids, same as the proxy
  #above: this runs at app level, outside every module namespace. local() so
  #each observer closes over its own id rather than over the loop variable.
  for(step in names(VFT_BANNER_PROXY)) local({
    id <- shiny::NS(step, VFT_BANNER_PROXY[[step]]$lang)
    shiny::observeEvent(input[[id]], set(input[[id]]))
  })

  if(vftNavEnabled()){
    shiny::observeEvent(r$currentLang, {
      lang <- r$currentLang
      if(is.null(lang) || identical(shiny::isolate(input$languageSelect), lang))
        return(invisible(NULL))
      shiny::updateSelectInput(session, inputId = "languageSelect", selected = lang)
    })
  }

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

#' Which BUTTON is "you are here"?
#'
#' Not the same question as which STEP the user is on, and the bar has to answer
#' this one. Hitzeminderung is not a step - it is a second door into the
#' newVersions page (see vftStepNav() in R/app_ui.R) - so a bar keyed on
#' `r$navStep` alone rings "Neue Versionen" whichever door was used, and goes on
#' ringing it while the user paints.
#'
#' `r$navContext` is the newVersions page's own contextChoice, published into the
#' app by the mirror in app_server(). Reading it here rather than remembering
#' which button was clicked is what makes the ring follow the RADIO as well: pick
#' Hitzeminderung on the page and the bar moves, pick Wegen/Strassen and it moves
#' back, without either of those knowing the bar exists.
#'
#' Reads `r` reactively on purpose - its caller is the bar's observe(), which has
#' to re-run when either half changes. NULL before the first navigation.
vftNavCurrentId <- function(r){
  step <- r$navStep
  if(is.null(step)) return(NULL)
  if(identical(step, "newVersions") &&
     identical(as.character(r$navContext), "4")) return("vftNav_hitze")
  vftNavInputId(step)
}

#' Has this session already built this step's module?
#'
#' The visit counters are the record: `vftGoToStep()` bumps one every time it
#' sends the user somewhere, and each step's observer builds its module when that
#' counter moves. A counter above 0 therefore means "a module server for this
#' step exists in this session" - which, until Stage 5, means entering again
#' would build a SECOND one.
#'
#' step 1 is the exception in both directions: it is built at session start by
#' its own `ignoreNULL = FALSE` observer, without anyone calling vftGoToStep(),
#' so its counter reads 0 while its module is very much live.
#'
#' isolate(): callers either have no reactive context (vftGoToStep, from an
#' isolated handler) or re-run for another reason anyway - the nav bar's observe
#' reads `r$navStep`, which changes on every navigation, so it re-evaluates this
#' at exactly the moments it can have changed.
vftStepEntered <- function(step, session = shiny::getDefaultReactiveDomain()){
  if(identical(step, "step1")) return(TRUE)
  nav <- tryCatch(session$userData$vftNav, error = function(e) NULL)
  if(is.null(nav) || is.null(nav[[step]])) return(FALSE)
  shiny::isolate(nav[[step]]()) > 0L
}

#' Show or hide the "Choose a next action" line under the banner.
#'
#' Steps 1 and 2 confirm without moving anywhere (their `then =` callbacks in
#' app_server()), so the only sign the write happened would otherwise be the nav
#' bar quietly lighting another button up. This is the line that says so - see
#' the markup and the CSS in `vftStepNav()`.
#'
#' Shown by those two confirms, hidden by `vftGoToStep()`: once the user has
#' chosen, the instruction to choose has served its purpose. Nothing else calls
#' it, and no step needs to know it exists.
#'
#' Showing ALWAYS sends, even when the line is already up, and it restarts the
#' arrow's animation on the way - a second confirm on the same step is exactly
#' the case where the hint is already showing and the user still needs to see
#' that their press did something. That is one `runjs` and not addClass, because
#' a CSS animation only re-runs if the class is removed, the element is
#' reflowed, and the class is put back; shinyjs would send those as two messages
#' in one batch, the browser would apply them in the same frame, and the arrow
#' would sit still. HIDING is filtered against the remembered state, so the
#' navigation that follows a confirm sends nothing when no hint is up - the same
#' economy `vftNavBarServer()` practises on its toggleState calls, and it
#' matters here for the same reason: every message batch the client answers
#' costs a full manageHiddenOutputs() sweep.
#'
#' `withReactiveDomain()`, because shinyjs namespaces against the DOMAIN rather
#' than against any session it is handed, and this is called from confirm
#' handlers and from `vftGoToStep()` - both of which can run under a domain that
#' is not the app session (a provider's callback, a module's enter()). The id is
#' app level and unnamespaced, so getting that wrong would silently address
#' nothing.
vftNavHint <- function(on, session = shiny::getDefaultReactiveDomain()){
  if(!vftNavEnabled() || is.null(session)) return(invisible(NULL))
  on <- isTRUE(on)
  if(!on && !isTRUE(session$userData$vftNavHintOn)) return(invisible(NULL))

  shiny::withReactiveDomain(session, {
    if(on)
      shinyjs::runjs(paste0(
        "(function(){var el=document.getElementById('vftNavHint'); if(!el) return;",
        "el.classList.remove('vft-nav-hint--on'); void el.offsetWidth;",
        "el.classList.add('vft-nav-hint--on');})();"))
    else
      shinyjs::removeClass(id = "vftNavHint", class = "vft-nav-hint--on")
  })
  session$userData$vftNavHintOn <- on
  invisible(NULL)
}

#' Go to a step: record it, show its tab, and ask its server to run.
#'
#' This is the only supported way to change step. It is deliberately not a
#' reactive - call it from an observer.
#'
#' @param r the app-level `reactiveValues`.
#' @param step one of `names(VFT_STEPS)`.
#' @param session the app-level session (not a module's namespaced proxy).
#' @param check re-test the step's prerequisites and refuse the move if they are
#'   not met. TRUE for anything a user can ask for - which today is exactly the
#'   nav bar, whose `shinyjs::disable()` is cosmetic: the input can be fired from
#'   the browser console, so the gate has to exist on this side too. FALSE (the
#'   default) for the app moving itself forward out of a confirm handler, which
#'   has just set that step's inputs in the same tick and is trusted; step 3's
#'   skip path in particular hands step 4 a deliberately unset `minThresh`.
#' @param needs the keys this ONE move requires, overriding the step's registry
#'   `needs`. Only the Hitzeminderung door passes it (VFT_HITZE_NEEDS in
#'   R/steps.R): it lands on the newVersions TAB but on a context that reads
#'   none of the three keys newVersions is registered as needing, and without
#'   this the move is refused by the "not derivable" branch below - which fires
#'   for unchecked callers too, so the door could not be opened at all. NULL
#'   (the default) means the registry answer, which is every other caller.
vftGoToStep <- function(r, step, session = shiny::getDefaultReactiveDomain(),
                        check = FALSE, needs = NULL){
  if(!step %in% names(VFT_STEPS))
    stop("vftGoToStep(): unknown step '", step, "'")

  #What this move needs, and what of it is not there yet. Everything below asks
  #these two rather than vftStepMissing()/vftStepReachable() directly, so the
  #override applies to the checked gate and the unchecked one alike.
  stepNeeds  <- if(is.null(needs)) VFT_STEPS[[step]]$needs else needs
  missingNow <- function(){
    if(!length(stepNeeds)) return(character(0))
    stepNeeds[!vapply(stepNeeds, function(k) vftKeyReady(r, k), logical(1))]
  }

  if(isTRUE(check)){
    if(!vftNavAllows(step)){
      vftDbg(paste0("NAV BLOCKED -> ", step, " (not in VFT_NAV)"))
      return(invisible(NULL))
    }
    #Entering a step builds its module, and building a module dispatches its jobs
    #- so navigating while work is in flight is how one session ends up with
    #several heavy raster crops queued against a single daemon. The bar greys
    #itself out while this is true, but the input can still be fired from the
    #console, so the refusal has to live here too. isolate(): this must never
    #take a reactive dependency on the counter.
    #
    #Deliberately NOT applied to the app's own forward transitions (check =
    #FALSE): a confirm handler runs after the work it depends on has resolved,
    #and blocking it would strand the user on a step they have finished.
    #Re-entry builds a second module server on top of the live one. Blocked here
    #rather than only greyed in the bar, for the same reason as everything else
    #in this branch: the input can be fired from the console. Lifts per step as
    #Stage 5 converts them - see VFT_REENTRANT_STEPS.
    if(!vftStepReentrant(step) && vftStepEntered(step, session)){
      vftDbg(paste0("NAV BLOCKED -> ", step, " (already built this session)"))
      try(shiny::showNotification(
        "Dieser Schritt wurde bereits besucht - der Weg zurück folgt später.",
        type = "message", duration = 4, session = session), silent = TRUE)
      return(invisible(NULL))
    }
    if(shiny::isolate(vftSessionBusy(session))){
      vftDbg(paste0("NAV BLOCKED -> ", step, " (async job in flight)"))
      try(shiny::showNotification(
        "Bitte warten - eine Berechnung läuft noch.",
        type = "message", duration = 4, session = session), silent = TRUE)
      return(invisible(NULL))
    }
    #Reachable, not available: a step whose last missing input has a provider is
    #a step the user is allowed to ask for, and asking is what starts the
    #derivation. The deferral is handled below, for every caller rather than only
    #this branch - step 3's confirm button reaches step 4 with check = FALSE and
    #has exactly the same wait to do.
    reachNow <- missingNow()
    if(!all(vapply(reachNow, function(k) vftKeyDerivable(r, k), logical(1)))){
      vftDbg(paste0("NAV BLOCKED -> ", step, " (missing: ",
                    paste(reachNow, collapse = ", "), ")"))
      return(invisible(NULL))
    }
  }

  #### going back costs nothing ####
  #
  #This is where the "you are about to discard..." modal used to be: a backward
  #move named everything downstream of the step being returned to and threw it
  #away on confirm. Navigation is the wrong event to hang that on, in both
  #directions - see the note above vftCommit() in R/providers.R - and it is gone.
  #Moving between steps now reads state; it never destroys it. What destroys
  #derived results is the confirm handler that writes new ones, and that is where
  #the question is asked.
  if(vftStepIsBack(shiny::isolate(r$navStep), step))
    vftDbg(paste0("NAV BACK -> ", step, " (nothing is discarded by looking)"))

  missing   <- missingNow()
  derivable <- missing[vapply(missing, function(k) vftKeyDerivable(r, k), logical(1))]

  #### a step that cannot run is not a destination ####
  #
  #Keys that are missing and that NO provider can derive used to fall through
  #here, on the reasoning that a checked caller had already been refused above
  #and an unchecked one is the app moving itself forward out of a confirm
  #handler "that has just set them". That second half is not true, and step 4 is
  #where it breaks: its confirm writes finalPolygons and networkList, while
  #step 5 also needs the sensitivity matrix, and the bar offers step 3 as soon
  #as the perimeter exists - DULN_all is derivable from `shape` alone - so step
  #2 can be walked straight past. Confirming step 4 then LANDED the user on a
  #step 5 that the bar itself grades unreachable: current, and greyed, and
  #refusing every click back to it, with "Neue Versionen" dark beside it for the
  #same missing keys. The step was reached by a door that does not check and
  #guarded by one that does.
  #
  #So the test moves here, where it covers every caller. A checked caller cannot
  #reach this line with anything blocked - vftStepReachable() refused it above -
  #which makes this purely the unchecked half, and it costs the user nothing:
  #the confirm handler's vftCommit() has already written its results, and step 4
  #re-enables its own two buttons before it hands over, so a refusal leaves them
  #on a working step with the missing one named rather than on a dead one.
  #
  #NOT a modal: this is the app declining to move, not a question. The
  #notification names the STEP to go and do, not the keys - "benoetigt:
  #2 Sensibilitaet" is actionable, "missing: SM_pres, SMcolors" is not.
  blocked <- setdiff(missing, derivable)
  if(length(blocked)){
    vftDbg(paste0("NAV BLOCKED -> ", step, " (not derivable: ",
                  paste(blocked, collapse = ", "), ")"))
    todo <- unique(VFT_KEY_SOURCE[blocked])
    todo <- todo[!is.na(todo) & todo != step]
    #back into registry order, so it reads "2 ..., 3 ..." rather than the order
    #the keys happen to sit in `needs`. Same shaping as vftStepPrereqLabels().
    todo <- names(VFT_STEPS)[names(VFT_STEPS) %in% todo]
    labs <- vapply(todo, function(s) VFT_STEPS[[s]]$label, character(1),
                   USE.NAMES = FALSE)
    msg <- if(length(labs))
      paste0("\"", VFT_STEPS[[step]]$label, "\" benoetigt zuerst: ",
             paste(labs, collapse = ", "))
    else
      paste0("\"", VFT_STEPS[[step]]$label, "\" kann noch nicht geoeffnet werden.")
    try(shiny::showNotification(msg, type = "warning", duration = 8,
                                session = session), silent = TRUE)
    return(invisible(NULL))
  }

  #### hold the move until the step's data exists ####
  #
  #Nothing is awaited: vftEnsure() dispatches and returns, and the provider
  #observe performs this navigation when the last key lands. The user stays where
  #they are meanwhile, with the progress bar the provider opened - entering a tab
  #whose module would be built against NULLs is the failure this replaces.
  if(length(derivable)){
    vftDbg(paste0("NAV DEFERRED -> ", step, " (deriving: ",
                  paste(derivable, collapse = ", "), ")"))
    if(isTRUE(vftEnsure(r, derivable, session))){
      vftSetPendingStep(session, step)
      return(invisible(NULL))
    }
    #no provider server on this session (tests, or a build without it): fall
    #through and behave exactly as before Stage 4 rather than stranding the user.
    vftDbg("NAV DEFERRED -> no provider server; proceeding unlazily")
  }

  spec <- VFT_STEPS[[step]]

  #r$step is the save file's idea of where the user is. newVersions has no code
  #of its own on purpose - see VFT_STEPS.
  if(!is.na(spec$code)) r$step <- spec$code

  #where the user IS, as opposed to what the save file calls it: newVersions has
  #no `code`, and the nav bar still has to highlight it. Reactive on purpose -
  #it is what makes the bar follow navigation it did not itself initiate.
  r$navStep <- step

  #The user has chosen, so the line asking them to choose comes down. Here and
  #not in the two confirm handlers because EVERY way off steps 1 and 2 arrives
  #here - a nav bar button, a banner letter, the restore path, a provider
  #completing a deferred move - and the hint has to answer all of them.
  vftNavHint(FALSE, session)

  vftDbg(paste0("NAV -> ", step))
  shiny::updateTabsetPanel(session = session, inputId = "tabs", selected = spec$tab)

  #bumping the counter is what makes the step's own observer run and build (or,
  #for a step not yet converted to a singleton, rebuild) its module server.
  counter <- session$userData$vftNav[[step]]
  counter(shiny::isolate(counter()) + 1L)

  #A converted module is built once and reused, so everything that has to happen
  #per VISIT rather than per session - the banner, the language, re-enabling the
  #confirm buttons, re-rendering against inputs that may have changed - lives in
  #its enter() closure and is run here.
  #
  #Nothing happens on the first visit: the counter write above is deferred to the
  #flush, so the module does not exist yet, and construction does this same work
  #inline. See R/modules.R.
  vftModuleEnter(session, step)

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
#' BEING RETIRED - the nav bar replaces it. Decided 2026-08-25, so do not tidy
#' this: `check = FALSE` below means a banner letter bypasses the checks the nav
#' bar enforces (VFT_NAV, the busy guard, the prerequisites). What it can no
#' longer do is build a second module on top of a live one - every step a letter
#' can name is a singleton now, step 1 included - and it discards nothing on the
#' way, because nothing does except the write that supersedes it. Gating what is
#' left would be the wrong investment: it is going away.
#'
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

#' `vftConfirmInvalidation()` used to live here.
#'
#' It asked "you are about to go back - may I discard X?" and, on confirm, called
#' vftInvalidate() and completed the move. The question was right and the moment
#' was wrong: it fired for a user who only wanted to re-read an earlier step, and
#' it never fired for the write that actually replaced the data. Its replacement
#' is vftAskCommit() in R/providers.R, raised from the confirm handler that is
#' about to write, and answered once per session by vftCommitServer().


#### The nav bar ####

#' Wire up the step nav bar built by `vftStepNav()`.
#'
#' Called once from `app_server()`, after `vftNavInit()`. Does nothing at all
#' unless `VFT_NAV=1`, including registering no observers - so with the flag off
#' this stage adds exactly one function call to a session.
#'
#' Two pieces:
#'
#' 1. one click observer per button. They all land in `vftGoToStep(check = TRUE)`,
#'    which re-tests the prerequisites server-side; the disabled state of the
#'    button is a hint to the user, not a security boundary, because the input
#'    can be fired from the browser console.
#'
#' 2. ONE `observe()` for the state of the whole bar. It reads the `needs` of
#'    every step, so it re-runs whenever any of those keys changes - but it only
#'    sends a message to the client for the buttons whose state actually moved.
#'    Without that filter a single change to `r` would push one toggleState call
#'    per step plus two class changes down the socket, and every message batch the
#'    client answers costs another full manageHiddenOutputs() sweep, which is the
#'    overhead Stage 1 just spent itself removing.
#'
#' The bar itself is static markup and is NOT an output - that is the whole
#' design constraint here. See vftStepNav() in R/app_ui.R.
#'
#' @param r the app-level `reactiveValues`.
#' @param input the app-level `input`.
#' @param session the app-level session.
vftNavBarServer <- function(r, input, session = shiny::getDefaultReactiveDomain()){
  if(!vftNavEnabled()) return(invisible(NULL))

  #only the steps this build lets the bar reach - see vftNavSteps(). The others
  #keep the disabled button vftStepNav() shipped and never get a click observer,
  #so a half-converted build cannot be talked into entering them.
  steps <- vftNavSteps()

  #the app opens on step 1 without anyone calling vftGoToStep(), so the marker
  #has to be seeded here or the bar would highlight nothing until the first move.
  #
  #isolate() is NOT optional and NOT cosmetic: this runs in the body of
  #app_server(), which is not a reactive consumer, and READING a reactiveValues
  #there is an error ("Can't access reactive value 'navStep' outside of reactive
  #consumer") that kills the session before any step is built. Writing is fine;
  #it is the is.null() test that needs the isolate. shiny::testServer() does not
  #catch this - it runs the server body inside a mock reactive context, so the
  #bare read is legal under test and fatal in the app.
  if(is.null(shiny::isolate(r$navStep))) r$navStep <- "step1"

  for(s in steps){
    local({
      step <- s
      shiny::observeEvent(input[[vftNavInputId(step)]], {
        #clicking the step you are already on would rebuild that module server -
        #a fresh observer set on top of the live one, until Stage 5 makes the
        #modules singletons. Nothing to do, so do nothing.
        #
        #Asked of the BUTTON, not the step: with the ring on Hitzeminderung the
        #user IS on newVersions, so a step test swallows the click on "Neue
        #Versionen" and that button is dead - the one way out of context 4 that
        #does not go through the radio. newVersions is re-entrant, so the
        #re-entry is a plain enter(), and with no contextPreset set it opens on
        #context 1 (or stays on 4 if there are still no Zielgebiete to edit).
        if(identical(vftNavCurrentId(r), vftNavInputId(step))) return(invisible(NULL))
        vftGoToStep(r, step, session, check = TRUE)
      }, ignoreInit = TRUE)
    })
  }

  #Hitzeminderung: a second door into newVersions, not a step of its own - see
  #the button's own comment in vftStepNav(). Gated behind the same rollout
  #switch as newVersions itself, since it goes nowhere else. r$vftContextPreset
  #is read once, by newVersions_server.R's contextChoice_ui render, and cleared
  #there; check = TRUE so it gets the same busy/reentrant/reachable refusal as
  #every other click in this bar.
  if("newVersions" %in% steps){
    shiny::observeEvent(input$vftNav_hitze, {
      #already behind this door - the same "nothing to do" the six step buttons
      #make, asked the same way. Without it, clicking the ringed Hitzeminderung
      #button re-enters newVersions and redraws the map for no reason.
      if(identical(vftNavCurrentId(r), "vftNav_hitze")) return(invisible(NULL))
      #Set, then cleared AFTER the flush this navigation runs in - not on the
      #next line. The clear has to outlive whichever of the two ways the module
      #reads it:
      #
      #  * already built (a singleton, so the common case): vftGoToStep() calls
      #    enter() synchronously, and the preset has been read by the time this
      #    observer's next line runs;
      #  * NOT yet built - the FIRST visit, which is now the ordinary way in,
      #    because this door no longer waits for steps 3 to 5. There
      #    vftGoToStep() only bumps the step counter, and the observer that
      #    constructs the module (and with it enter()) runs later in the flush.
      #    Clearing on the next line handed that construction a NULL.
      #
      #Cleared at all because a later PLAIN return to newVersions - the "Neue
      #Versionen" button, a save/restore - would otherwise find it still "4" and
      #preselect Hitzeminderung when nothing asked for that.
      r$vftContextPreset <- "4"
      #`needs = VFT_HITZE_NEEDS` is what makes this door open on the perimeter
      #alone. Without it the move is refused for finalPolygons / minThresh -
      #newVersions' registry `needs`, which context 4 does not read. The page
      #then runs in heat-only mode; see enter() in R/newVersions_server.R.
      vftGoToStep(r, "newVersions", session, check = TRUE,
                  needs = VFT_HITZE_NEEDS)
      #Nothing takes a reactive dependency on this key outside enter(), which is
      #isolated, so the write costs a flush and no re-render.
      session$onFlushed(function() r$vftContextPreset <- NULL, once = TRUE)
    }, ignoreInit = TRUE)
  }

  #last state pushed to the client, so the observe below can send only changes.
  #A list rather than a vector: the entries start NULL, which is not identical()
  #to TRUE or FALSE, so the first run always sends.
  sent      <- stats::setNames(vector("list", length(steps)), steps)
  sentHitze <- NULL
  sentFold  <- NULL
  current   <- NULL

  #### folded or unfolded ####
  #
  #The bar ships with `vft-nav-folded` in the markup (vftStepNav()), so this
  #starts TRUE and the first unfold is a real change rather than a message sent
  #into a state the client is already in.
  #
  #THREE things ask for a change - the fold button's click, the ring landing on
  #a member of the group, and a new outline being committed - and this is the
  #only thing that writes the class, for the same reason the observe below
  #filters its toggleState calls: every message batch the client answers costs a
  #full manageHiddenOutputs() sweep, and "unfold" arrives from two of those three
  #in the same flush on the ordinary path.
  folded <- TRUE
  setFolded <- function(on){
    if(identical(on, folded)) return(invisible(NULL))
    if(on) shinyjs::addClass(id = "vftNav", class = "vft-nav-folded")
    else   shinyjs::removeClass(id = "vftNav", class = "vft-nav-folded")
    folded <<- on
    invisible(NULL)
  }

  #the folded group's button ids, for the "is the ring inside the group" test
  #below. Computed once - VFT_NAV_FOLD is a constant.
  foldIds <- vftNavInputId(VFT_NAV_FOLD)

  #Clicking the stand-in button unfolds the chain AND enters its first reachable
  #member, which is step 3 in every ordinary session. Two things at once because
  #a disclosure that only discloses would leave the user to make a second choice
  #they have already made: they asked for the simulation half of the tool.
  #
  #Unfold FIRST, then navigate: vftGoToStep() may DEFER the move while a provider
  #derives DULN_all, and the chain has to open under the user's click rather than
  #when the derivation lands. check = TRUE for the same reason every other click
  #in this bar uses it - the input can be fired from the browser console.
  #
  #Gated on the group being in `steps` (a VFT_NAV=step1,step2 build has no fold
  #to open), and the button then keeps the disabled attribute vftStepNav()
  #shipped, since nothing below re-enables it either.
  if(any(VFT_NAV_FOLD %in% steps)){
    shiny::observeEvent(input[[VFT_NAV_FOLD_ID]], {
      target <- shiny::isolate(vftNavFoldTarget(r, steps))
      setFolded(FALSE)
      if(is.null(target)){
        vftDbg("NAV FOLD -> no reachable member")
        return(invisible(NULL))
      }
      vftDbg(paste0("NAV FOLD -> unfolded, entering ", target))
      vftGoToStep(r, target, session, check = TRUE)
    }, ignoreInit = TRUE)

    #### a new outline folds it back up ####
    #
    #Confirming a NEW perimeter in step 1 discards everything downstream -
    #vftCommit() in R/providers.R names what that costs and asks first - so the
    #walk starts again and the bar goes back to its four simple choices. The
    #WRITE is the event, not a visit to step 1: coming back to look at the area
    #and pressing straight through hands the same shape back, vftCommit() finds
    #nothing changed and never writes, and the chain stays open. Cancelling the
    #modal is the same story for the same reason.
    #
    #ignoreInit so the first confirm of a fresh session is a no-op (already
    #folded); ignoreNULL = FALSE so a shape being cleared counts too.
    #
    #Guarded on where the user IS. Confirming step 1 leaves them standing on it
    #now - it no longer moves anyone to step 2 - so r$navStep still reads
    #"step1" when this deferred observer runs, which is outside the group, and
    #the chain folds. The restore path also writes r$shape, and it can land the
    #session directly on step 5 - there the guard holds the chain open, and the
    #ring test below would reopen it anyway.
    shiny::observeEvent(r$shape, {
      if(shiny::isolate(r$navStep) %in% VFT_NAV_FOLD) return(invisible(NULL))
      vftDbg("NAV FOLD -> new outline, folded back")
      setFolded(TRUE)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  }

  shiny::observe({
    #greyed while this session has async work outstanding, so "wait" is something
    #the user can see rather than something they discover by clicking. Reactive:
    #this observe re-runs when the count moves in either direction.
    busy <- vftSessionBusy(session)

    #VFT_BUSY_MAX_S is wall-clock, and nothing invalidates when it expires, so
    #without a tick a promise that never settles would leave the bar dead for the
    #session. Only while busy, so an idle session pays nothing.
    if(busy) shiny::invalidateLater(5000, session)

    for(s in steps){
      #REACHABLE, not available: a step whose last missing input has a provider
      #lights up, and clicking it starts the derivation. That is the whole point
      #of the provider layer from the user's side - step 3 is offered as soon as
      #the perimeter exists, rather than after a network build it does not use.
      #
      #Evaluated FIRST and unconditionally, not short-circuited behind !busy: it
      #is what takes the dependency on each step's `needs`, and skipping it while
      #busy would drop those dependencies.
      #
      #A step already built this session greys out permanently (until Stage 5
      #makes it re-entrant), so the bar shows what it will actually do rather
      #than inviting a click it is going to refuse.
      ok <- vftStepReachable(r, s) && !busy &&
            (vftStepReentrant(s) || !vftStepEntered(s, session))
      if(!identical(ok, sent[[s]])){
        shinyjs::toggleState(id = vftNavInputId(s), condition = ok)
        sent[[s]] <<- ok
        #the click-time "NAV BLOCKED (missing: ...)" message only ever fires
        #for a button that WAS enabled, so it says nothing about why one stays
        #disabled. This is the same information at the other end of that gap.
        if(!ok && !busy)
          vftDbg(paste0("NAV GREY -> ", s, " (missing: ",
                        paste(vftStepMissing(r, s), collapse = ", "), ")"))
      }
    }

    #Hitzeminderung goes to the same TAB newVersions does, but not to the same
    #work: it lands on context 4, which paints a land cover baseline derived
    #from the step-1 perimeter and reads neither the path network, nor the
    #confirmed areas of interest, nor the step-3 threshold. It used to mirror
    #newVersions' `ok`, so it stayed dark until all three existed - which put
    #the whole of steps 3 and 4 in front of a feature that needs none of them.
    #
    #vftHitzeReachable() asks VFT_HITZE_NEEDS instead (R/steps.R): the perimeter,
    #and nothing else. Outside the loop now, because it is no longer an answer
    #about `s`. The reentrancy half of the step test is dropped rather than
    #copied - newVersions is a converted singleton, so that half was always TRUE.
    if("newVersions" %in% steps){
      okHitze <- vftHitzeReachable(r) && !busy
      if(!identical(okHitze, sentHitze)){
        shinyjs::toggleState(id = "vftNav_hitze", condition = okHitze)
        sentHitze <<- okHitze
      }
    }

    #The stand-in button for the folded group. Live when ANY member of the group
    #could be entered - vftNavFoldTarget() in R/steps.R, which is the same
    #question as "where would clicking it go", asked once so the two cannot
    #disagree. In practice that is step 3, so this lights up the moment step 1
    #confirms. Outside the loop: it is not an answer about `s`, and it is not one
    #of VFT_STEPS.
    if(any(VFT_NAV_FOLD %in% steps)){
      okFold <- !is.null(vftNavFoldTarget(r, steps)) && !busy
      if(!identical(okFold, sentFold)){
        shinyjs::toggleState(id = VFT_NAV_FOLD_ID, condition = okFold)
        sentFold <<- okFold
      }
    }

    #the ring. `now` is a BUTTON id, not a step: Hitzeminderung and Neue
    #Versionen are two doors into one step, and which of them the user is behind
    #is a question about the page's context. See vftNavCurrentId().
    now <- vftNavCurrentId(r)

    #A ring inside the folded group means the user is standing on a button they
    #cannot see, so the chain opens. This is the self-correcting half of the fold
    #and it covers every way into the group that is not the fold button's own
    #click: step 4's confirm handing off to step 5, a restored save landing
    #there, the fold button's own DEFERRED navigation finally completing. None of
    #those has to know the bar folds.
    #
    #Not an `else` - leaving the group does NOT fold it. Only a new outline does;
    #see the r$shape observer above.
    if(!is.null(now) && now %in% foldIds) setFolded(FALSE)

    if(!is.null(now) && !identical(now, current)){
      if(!is.null(current))
        shinyjs::removeClass(id = current, class = "vft-nav-current")
      shinyjs::addClass(id = now, class = "vft-nav-current")
      current <<- now
    }
  })

  invisible(NULL)
}
