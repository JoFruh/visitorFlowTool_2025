#### The automatic route to the simulation ####
#
# Step 1's next-step modal offers the simulation as two buttons (see
# VFT_NEXT_CHOICES in R/navigation.R): "Naherholung simulieren (Automatische
# Zielgebiete)" and, under it, "Manuelle Zielgebiete". The manual one walks
# steps 3 and 4 as before. This file is the automatic one: it does what a user
# would do on those two steps with their automatic answers - the threshold
# VFT_AUTO_AOI_THRESHOLD on step 3, "Automatic Cuts" on step 4, confirm - and
# lands on step 5, whose enter() then simulates the Original by itself.
#
# It makes the SAME writes those two confirms make, through the same vftCommit(),
# so nothing downstream can tell which route was taken: steps 3 and 4 are
# reachable from the nav bar afterwards, step 3's slider opens on the threshold
# (step3_server.R's obsSuggest), and step 4 opens on the cut areas with Reset
# going to the plain ones (the `autoAoI` branch of .vftStep4Launch()).
#
# What the user sees, in order - one bar per stage:
#   the two attractiveness crops, if this perimeter has none yet (providers)
#   "Zielgebiete werden erzeugt..."                  bar 1 of this job
#   "Zielgebiete werden automatisch zerschnitten..." bar 2 of this job
#   "Daten werden vorbereitet" / "Simulation laeuft"  step 5's two, as ever
#
# The two halves of the job are one dispatch and one queue position with two
# bars, the same arrangement as step 5's launch (vftProgressPair() in
# R/async_helpers.R).

#' Column names step 4's confirm gives the areas of interest, A..Z, AA..ZZ.
#'
#' The same expression step4_server() builds for itself - that is where
#' finalPolygons$AOI has always come from, and the letters are what
#' vftPrepareNetwork() and determineAgentCharacteristics() read.
.vftAoILetters <- function(n){
  letters702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
  letters702[seq_len(n)]
}

#' The scenario list a confirmation of the areas of interest writes.
#'
#' Lifted out of step 4's confirm handler in app_server(), which still calls it,
#' so the automatic route writes the identical list rather than a second copy of
#' the rule.
#'
#' A fresh list holding one empty scenario - unless EVERYTHING on hand is a
#' canvas (vftIsCanvasList() in R/steps.R): a user who came in through the
#' Hitzeminderung door has painted heat designs on scenarios with no network and
#' no Zielgebiete. Those are kept, same entries in the same order, each gaining
#' the network and losing its `heatOnly` tag - which is what turns the canvas
#' back into a scenario list.
#'
#' @param prior r$networkList as it stands
#' @param network r$network - usually NULL here, which is fine: the network is
#'   loaded by the first simulation (vftPrepareThen()), and a scenario made after
#'   it has been loaded gets the cached copy for free
vftScenarioListFor <- function(prior, network){
  if(vftIsCanvasList(prior)){
    vftDbg(paste0("AOI CONFIRM: merging the Zielgebiete into ", length(prior),
                  " painted scenario(s) rather than replacing them"))
    return(lapply(prior, function(v){
      #built as a literal, exactly like the fresh scenario below, rather than by
      #editing `v` in place: assigning NULL to a list element REMOVES it, so
      #`v$network <- network` on the usual NULL would leave a scenario with no
      #`network` field at all. The two rasters are the paint, and they are the
      #whole reason this branch exists.
      list(network = network, pathUsage = NULL, parking = NULL,
           residential = NULL, newAttr = NULL,
           paintedRaster = v$paintedRaster, canopyRaster = v$canopyRaster)
    }))
  }
  list(list(network = network, pathUsage = NULL, parking = NULL,
            residential = NULL, newAttr = NULL))
}

#' What happens once the areas of interest are committed: on to step 5.
#'
#' The `then =` of step 4's vftCommit() in app_server(), lifted out so the
#' automatic route hands over the same way.
vftStep4Handover <- function(r, session = shiny::getDefaultReactiveDomain()){
  r$step6FirstRun <- TRUE
  r$newVersionsFirstRun <- TRUE

  #vftGoToStep sets r$step, which the snapshot records, so it has to come first
  #- a snapshot taken before it would name step 4 and resume the user one step
  #behind where they were.
  vftGoToStep(r, "step5", session)

  #The Zielgebiete are in VFT_SNAPSHOT_KEYS, so this is the confirmation that
  #puts real geometry in the browser. The network and the (empty) scenario list
  #are not: both are rebuilt from `finalPolygons` and `minThresh` by the
  #provider layer and vftPrepareThen().
  vftSnapshotWrite(r, session)

  r$triggerStep5_nr <- 1
  invisible(NULL)
}

#' Is an automatic run already under way in this session?
.vftAutoAoIRunning <- function(session, on = NULL){
  ud <- tryCatch(session$userData, error = function(e) NULL)
  if(is.null(ud)) return(FALSE)
  if(!is.null(on)) ud$vftAutoAoIRunning <- isTRUE(on)
  isTRUE(ud$vftAutoAoIRunning)
}

#' Generate the areas of interest automatically, cut them, and go to step 5.
#'
#' Called by the "Naherholung simulieren (Automatische Zielgebiete)" button of
#' the next-step modal (vftNavBarServer() in R/navigation.R). Returns at once;
#' the work runs in the worker behind two progress bars, and the user stays on
#' the step they confirmed until it lands.
#'
#' The commit is ONE vftCommit() for everything steps 3 and 4 would have written
#' - the threshold, the skip flag, the Zielgebiete and the scenario list - so a
#' session holding results that this replaces sees the "will be discarded" modal
#' once, and cancelling it writes nothing and leaves the user where they are.
#'
#' Two cases end on step 3 instead, with a notification: nothing in the
#' perimeter reaches the threshold (no areas to simulate towards), and a
#' perimeter that changed while the job ran (its result is dropped - the new
#' outline's own confirm has already said what comes next).
#'
#' @param r the app-level reactiveValues
#' @param session the APP session
vftAutoAoISim <- function(r, session = shiny::getDefaultReactiveDomain()){
  if(is.null(session)) return(invisible(NULL))
  tr <- .vftT(session)
  note <- function(key, type = "message", duration = 6)
    try(shiny::showNotification(tr(key), type = type, duration = duration,
                                session = session), silent = TRUE)

  #the same refusal the nav bar makes: one job at a time per session
  if(.vftAutoAoIRunning(session) || shiny::isolate(vftSessionBusy(session))){
    vftDbg("AUTO AOI: refused - a job is in flight")
    note("Bitte warten - eine Berechnung läuft noch.", duration = 4)
    return(invisible(NULL))
  }
  shape0 <- shiny::isolate(r$shape)
  if(is.null(shape0)) return(invisible(NULL))

  .vftAutoAoIRunning(session, TRUE)
  done <- function() .vftAutoAoIRunning(session, FALSE)

  thr <- VFT_AUTO_AOI_THRESHOLD

  run <- function(){
    #the perimeter changed while the crops were being made
    if(!identical(shiny::isolate(r$shape), shape0)){ done(); return(invisible(NULL)) }

    DULN     <- shiny::isolate(r$DULN)
    DULN_all <- shiny::isolate(r$DULN_all)
    if(is.null(DULN) || is.null(DULN_all)){ done(); return(invisible(NULL)) }

    vftDbg(paste0("AUTO AOI: generating at threshold ", thr, ", then cutting"))

    #ONE BAND, not seven - generateAoI2() reads walkNat alone. Same as step 4.
    walkNat_wrapped  <- terra::wrap(DULN$walkNat)
    DULN_all_wrapped <- terra::wrap(DULN_all)
    shape_ <- shape0
    thr_   <- thr

    progress <- vftProgressPair(
      message  = "Zielgebiete werden erzeugt...",
      detail   = vftMsg("Dies sollte weniger als %d Sekunden dauern", 30),
      message2 = "Zielgebiete werden automatisch zerschnitten...",
      detail2  = vftMsg("Dies sollte weniger als %d Sekunden dauern", 30))
    progGen <- progress$prep
    progCut <- progress$sim

    vftFuture({
      walkNat  <- terra::unwrap(walkNat_wrapped)
      DULN_all <- terra::unwrap(DULN_all_wrapped)

      #step 3 + the generation step 4 runs on arrival: the plain threshold areas
      progGen$set(1/2)
      plain <- generateAoI2(minThresh = thr_, perimeter = shape_,
                            walkNat = walkNat, DULN_all = DULN_all,
                            split = FALSE)
      progGen$set(2/2)
      progGen$close()

      #step 4's "Automatic Cuts" on that untouched set: `within = NULL` cuts the
      #threshold itself, exactly as the button does when nothing was edited
      progCut$set(1/2)
      cut <- generateAoI2(minThresh = thr_, perimeter = shape_,
                          walkNat = walkNat, DULN_all = DULN_all,
                          split = TRUE)
      progCut$set(2/2)
      progCut$close()

      list(plain = plain$polygons, cut = cut$polygons)
    }, seed = TRUE, progress = progress) %...>% (function(out){
      shiny::withReactiveDomain(session, shiny::isolate({
        done()
        #both bars off the screen before step 5's appears (the queue is only
        #read once a second, and a fast run resolves before it is)
        vftProgressDrain(progress)

        if(!identical(r$shape, shape0)){
          vftDbg("AUTO AOI: result dropped - the perimeter changed meanwhile")
          return(invisible(NULL))
        }

        cut <- out$cut
        n   <- if(inherits(cut, "sf")) nrow(cut) else 0L
        if(n == 0L){
          vftDbg("AUTO AOI: no areas of interest at this threshold - to step 3")
          note("Keine Zielgebiete über der Schwelle gefunden – bitte manuell festlegen.",
               type = "warning", duration = 10)
          vftGoToStep(r, "step3", session)
          return(invisible(NULL))
        }

        #what step 4 gives its areas: the ids "Automatic Cuts" assigns, then
        #the letters its confirm adds
        cut$id  <- seq_len(n)
        cut$AOI <- .vftAoILetters(n)
        plain <- out$plain
        if(inherits(plain, "sf") && nrow(plain) > 0) plain$id <- seq_len(nrow(plain))

        vftCommit(r,
                  list(minThresh     = thr,
                       isSkip        = 0,
                       finalPolygons = cut,
                       networkList   = vftScenarioListFor(r$networkList, r$network)),
                  session, step = "step3",
                  then = function(){
                    #for step 4, should the user go there: Reset goes to the
                    #plain areas. The key is the one step 4 builds for its own
                    #inputs - threshold, skip, perimeter - so a later change to
                    #any of them leaves it unmatched. Only once the write has
                    #happened: a cancelled commit made nothing to reset to.
                    r$aoiAutoPlain <- list(key = list(minThresh = thr, skip = FALSE,
                                                      shape = shape0),
                                           polygons = plain)
                    vftStep4Handover(r, session)
                  })
      }))
    }) %...!% (function(e){
      done()
      shiny::withReactiveDomain(session,
        vftAsyncError(progress, "Automatic areas of interest")(e))
    })
    invisible(NULL)
  }

  #the two crops step 4 would need - derived first if this perimeter has none
  #yet, each behind the provider's own bar; at once if it has
  #(a failed crop reports itself - vftAsyncError() in the provider layer - so
  #onFail only lets the button be pressed again)
  ok <- vftEnsureThen(c("DULN", "DULN_all"), then = run, onFail = done,
                      session = session)
  if(!isTRUE(ok)) done()
  invisible(NULL)
}
