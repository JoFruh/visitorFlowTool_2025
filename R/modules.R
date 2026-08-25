#### First-touch singleton module servers ####

# Navigation used to mean "call the module server again". Every visit built a
# fresh set of observers, outputs and captured values on top of the live ones,
# and only hand-maintained $destroy() lists cleaned any of it up - inconsistently
# enough that one exit path from step 5 destroyed 2 of the 11 observers the other
# path destroyed. The visible result was two step-4 instances answering one
# confirm click, the older one writing the network it had frozen BEFORE the user
# changed the area of interest back into r$, and two "Original" scenarios in step
# 5 - one simulated against an area that was no longer on screen.
#
# So: build a step's module the first time it is entered, and reuse it. Not
# eagerly at session start - output registration happens at construction, and
# building all seven up front takes the registered-output sweep from ~6 to ~31
# from t=0, which is a 5x regression on the exact metric Stage 1 existed to
# improve. First touch, then cached.
#
# THE SWITCH IS `VFT_REENTRANT_STEPS` IN R/steps.R, and it now means three things
# at once: the nav bar offers the step, vftGoToStep() allows the return, and the
# module is reused rather than rebuilt. Adding a step to that vector is the whole
# act of converting it, which is what makes this safe to do one module at a time:
# an unconverted step keeps the old rebuild-per-visit behaviour exactly, because
# the paths that still rely on it - step5 <-> newVersions bouncing, the restore
# ladder - are unchanged until its turn comes.
#
# A converted module owes two things:
#
#   1. it must read its inputs as REACTIVES, not as plain values frozen at
#      construction, and
#   2. it must return an `enter()` closure holding everything that has to happen
#      per visit rather than per session - the banner, the language, re-enabling
#      the confirm buttons, resetting the step's own state, re-rendering a map.
#
# enter() is called by vftGoToStep() on every visit AFTER the first. The first
# visit is construction, which does the same work inline, so a module calls its
# own enter() at the end of its body rather than duplicating it.

#' Per-session store of built module handles.
#'
#' Deliberately NOT `session$userData$.vftModules`, which is
#' vftModuleInstance()'s instantiation TALLY and only exists when VFT_PERF is on.
#' That tally is the acceptance test for this stage - after conversion, walking
#' 1 -> 5 -> 1 -> 5 -> newVersions -> 5 must leave every module at exactly 1 -
#' and a mechanism that read its own scoreboard could not be checked by it.
.vftModuleStore <- function(session){
  if(is.null(session)) return(NULL)
  ud <- tryCatch(session$userData, error = function(e) NULL)
  if(is.null(ud)) return(NULL)
  if(is.null(ud$vftModuleHandles)) ud$vftModuleHandles <- new.env(parent = emptyenv())
  ud$vftModuleHandles
}

#' The handle a converted module returned, or NULL if it has not been built.
vftModuleHandle <- function(session, step){
  store <- .vftModuleStore(session)
  if(is.null(store)) return(NULL)
  get0(step, envir = store, ifnotfound = NULL)
}

#' Build a step's module server, or hand back the one this session already has.
#'
#' `build` is a zero-argument function that calls the module server AND registers
#' whatever app-level observers watch its return handle. Both happen exactly once
#' for a converted step, which is what disposes of the `once = TRUE` confirm
#' observers: they were there to stop a rebuilt module's handlers stacking up, and
#' with one instantiation there is nothing to stack.
#'
#' For a step that is NOT yet in VFT_REENTRANT_STEPS this is a plain call - the
#' handle is still recorded, so vftModuleInstance()'s tally and this store agree,
#' but it is never reused. That is what lets the conversion be done one module at
#' a time without the unconverted ones changing behaviour at all.
vftModuleOnce <- function(session, step, build){
  store <- .vftModuleStore(session)
  singleton <- vftStepReentrant(step)

  if(singleton){
    existing <- vftModuleHandle(session, step)
    if(!is.null(existing)){
      vftDbg(paste0("REUSE MODULE ", step))
      return(existing)
    }
  }

  h <- build()
  if(!is.null(store)) assign(step, h, envir = store)
  h
}

#' Run a converted module's per-visit side effects.
#'
#' A no-op for a step that has not been built yet (the first visit IS the build,
#' and the module runs its own enter() at the end of its body) and for one that
#' has not been converted (no enter() in its handle).
#'
#' Errors are NOT swallowed. enter() is where the banner, the language and the
#' confirm buttons are restored; a silent failure there would leave the user on a
#' step that looks right and cannot be left.
vftModuleEnter <- function(session, step){
  h <- vftModuleHandle(session, step)
  if(is.null(h)) return(invisible(FALSE))
  fn <- h$enter
  if(!is.function(fn)) return(invisible(FALSE))

  vftDbg(paste0("ENTER ", step))
  fn()
  invisible(TRUE)
}

#' Wrap a module's per-visit body into an enter() closure.
#'
#' TWO things have to be true of every enter(), and neither is obvious at the
#' call site, which is why this exists instead of a hand-written closure:
#'
#' 1. **The module's own session must be the default reactive domain.** enter()
#'    is called from vftGoToStep(), whose domain is the APP session - and
#'    shinyjs::enable/disable/reset and shiny::update*Input() all namespace
#'    against `getDefaultReactiveDomain()`, not against the session their caller
#'    can see. shinyjs only prefixes at all when the domain
#'    `inherits(., "session_proxy")`; the app session does not, so
#'    `enable("confirmButton3")` sent from a re-entry addressed a control named
#'    "confirmButton3" rather than "step3-confirmButton3". No error, no warning,
#'    no effect - the button the module had disabled on the way out simply stayed
#'    disabled. Every update*Input() in an enter() had been silently missing for
#'    the same reason.
#' 2. **The whole body must be isolated.** enter() runs inside observers - among
#'    them Stage 4's provider `observe()`, which is NOT isolated the way an
#'    observeEvent handler is. A bare reactive read there makes that observer
#'    depend on a value enter() also assigns, and step 4 re-enters itself
#'    forever.
#'
#' Both are one-line mistakes that produce no diagnostic, so they are made
#' unmakeable: converted modules write
#' `enter <- vftModuleEnterFn(session, function(){ ... })`.
vftModuleEnterFn <- function(session, body){
  force(session); force(body)
  function() shiny::withReactiveDomain(session, shiny::isolate(body()))
}
