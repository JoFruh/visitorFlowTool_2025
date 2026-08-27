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
vftGoToStep <- function(r, step, session = shiny::getDefaultReactiveDomain(),
                        check = FALSE){
  if(!step %in% names(VFT_STEPS))
    stop("vftGoToStep(): unknown step '", step, "'")

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
    if(!vftStepReachable(r, step)){
      vftDbg(paste0("NAV BLOCKED -> ", step, " (missing: ",
                    paste(vftStepMissing(r, step), collapse = ", "), ")"))
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

  #### hold the move until the step's data exists ####
  #
  #Nothing is awaited: vftEnsure() dispatches and returns, and the provider
  #observe performs this navigation when the last key lands. The user stays where
  #they are meanwhile, with the progress bar the provider opened - entering a tab
  #whose module would be built against NULLs is the failure this replaces.
  #
  #Keys that are missing and NOT derivable fall through: a checked caller was
  #already refused above, and an unchecked one is the app moving itself forward
  #out of a confirm handler that has just set them - step 3's skip path hands
  #step 4 a deliberately unset minThresh.
  missing   <- vftStepMissing(r, step)
  derivable <- missing[vapply(missing, function(k) vftKeyDerivable(r, k), logical(1))]
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
        if(identical(r$navStep, step)) return(invisible(NULL))
        vftGoToStep(r, step, session, check = TRUE)
      }, ignoreInit = TRUE)
    })
  }

  #last state pushed to the client, so the observe below can send only changes.
  #A list rather than a vector: the entries start NULL, which is not identical()
  #to TRUE or FALSE, so the first run always sends.
  sent    <- stats::setNames(vector("list", length(steps)), steps)
  current <- NULL

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
      }
    }

    now <- r$navStep
    if(!is.null(now) && !identical(now, current)){
      if(!is.null(current))
        shinyjs::removeClass(id = vftNavInputId(current), class = "vft-nav-current")
      shinyjs::addClass(id = vftNavInputId(now), class = "vft-nav-current")
      current <<- now
    }
  })

  invisible(NULL)
}
