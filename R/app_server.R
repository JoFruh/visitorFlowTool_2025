#' server function for visitorFlowTool app
app_server <- function(input, output, session){

  #count this session against the shared process for the duration of its life, so
  #every logged stall carries the concurrency it happened under. A 3s freeze with
  #five users connected costs 15 user-seconds; that weighting is what ranks the
  #per-step optimisation work. Registers its own onSessionEnded. See perf_helpers.R.
  vftPerfSessionStart(session)

  #prepare multilingual functions
  i18n <- shiny.i18n::Translator$new(translation_csvs_path = vftData("tables"), separator_csv = ";" )
  i18n$set_translation_language('de')

  #Park the translator where code without an `i18n` in scope can reach it. The
  #queue display in R/async_helpers.R is driven from providers.R and
  #prepare_network.R, neither of which is handed one, and threading it through
  #five signatures for a caption is not worth it. userData is shared with every
  #module proxy (the same property .vftBusyStore() relies on), and the Translator
  #is an R6 object the language observers mutate in place, so a language switch
  #reaches it with no further wiring. See .vftT().
  session$userData$vftI18n <- i18n

  r <- shiny::reactiveValues()

  #give banners an original value.
  #One runjs, not five: each setInputValue is an input message from the client,
  #and every input message batch makes ShinySession$manageInputs() call
  #manageHiddenOutputs(), which sweeps the whole registered-output list. Sent
  #together they arrive in one batch and cost one sweep instead of five.
  shinyjs::runjs(paste0(
    "Shiny.setInputValue('step2-banner', 'O');",
    "Shiny.setInputValue('step3-banner', 'O');",
    "Shiny.setInputValue('step4-banner', 'O');",
    "Shiny.setInputValue('step5-banner', 'O');"
  ))

  #one visit counter per step, in session$userData. Everything that moves the
  #user between steps goes through vftGoToStep() in R/navigation.R, which is
  #also the only caller of updateTabsetPanel() in the app.
  vftNavInit(session)

  #the step nav bar: which steps are reachable, and the click handlers that
  #reach them. Registers nothing unless VFT_NAV=1. The step registry it gates on
  #(which r$ keys each step needs) is in R/steps.R.
  vftNavBarServer(r, input, session)

  #the single visible language-select/help/info controls now live in the nav
  #bar itself (it doubles as the page banner - see vftStepNav()); this is what
  #forwards a click or a selection there to whichever step's own, still
  #unchanged, hidden proxy input belongs to r$navStep. See R/navigation.R.
  vftNavBannerProxyServer(r, input, session)

  #the one owner of `r$currentLang` and of the Translator's language: every
  #selector that can choose one - the nav bar's and the six hidden per-step ones
  #- is observed in one place, so a language chosen on any step is the language
  #the next step is entered in. This replaces the seven `r$currentLang <-
  #stepNreturn$currentLang()` writes that used to sit in the confirm handlers
  #below, none of which could report a change made after the module was built.
  #See R/navigation.R.
  vftLangServer(r, i18n, input, session)

  #the lazy data layer: one observe that derives whatever a step is about to
  #need, and completes a navigation that was waiting for it. Nothing is computed
  #because of where the user has been, only because something is about to read
  #it. Registry and reasoning in R/providers.R.
  vftProviderServer(r, session)

  #the two buttons of the "this will be discarded" modal. One pair of observers
  #for the session, reading whichever commit is currently parked in
  #session$userData - see vftAskCommit() in R/providers.R for why it is not a
  #fresh pair per modal. Every step's confirm handler below hands its results to
  #vftCommit(), which is what raises that modal when the write costs something.
  vftCommitServer(r, session)

  #the disk icon's save dialog, and the crash-recovery prompt. One pair of
  #observers for the session. The snapshot itself is written from the `then =`
  #of each vftCommit() below, not from here - see R/state_browser.R.
  vftStateServer(r, input, session)

  #accompanying non reactive (nr) variables, to allow for back and forth access between step5 and newVersions
  r$triggerNewVersions_nr <- 1
  r$triggerStep5_nr <- 1

  #keep track if tutorial needed
  r$needHelp <- FALSE


  #### the explicit save ####
  #
  #The FULL session state, written to a file the user names and the browser
  #downloads. This is the only thing that preserves a finished simulation, and
  #it is the file step 1's loader reads back.
  #
  #It is no longer clicked from R. It used to be a hidden button fired by
  #shinyjs::click() at two step confirmations, so every user collected
  #timestamped .RData files in their Downloads folder without asking for them;
  #automatic saving is the browser snapshot's job now (vftSnapshotWrite() in
  #R/state_browser.R), and this link exists only inside the save dialog the disk
  #icon opens. suspendWhenHidden = FALSE because the dialog is not on screen
  #when the session starts.
  #
  #What used to be 130 lines of envBase_ assembly here is VFT_STATE_KEYS plus
  #vftStateFromR()/vftStateWrite() in R/state.R - one place that knows the
  #format, shared with the restore path and with the browser snapshot. The
  #format itself is unchanged: the same 30 envBase_ names, the same gzip level.
  output$downloadSave <- shiny::downloadHandler(
    filename = function(){
      #The name the user typed in the dialog, sanitised. Evaluated when the
      #link is requested, so it is whatever is in the box at that moment.
      #
      #This replaces a switch() on r$step, an INTEGER, which therefore selected
      #by POSITION and ignored its own "2" =, "3" = names entirely - they
      #disagreed with the positions and it was never clear which reading was
      #meant. Nobody has to decide now: vftSaveDefaultName() prefills the box
      #from the step registry and the user can overwrite the whole thing.
      vftSaveFileName(input$saveName, r)
    },
    content = function(file){
      #known general-setup hot spot: a save() of the whole session state on the
      #thread every other user is waiting on. It fires once, when a user asks
      #for it, rather than at eight (then three) step transitions. Labelled so
      #the stall log attributes it by name rather than to "unattributed".
      vftTime("app:downloadSave", {
        vftStateWrite(vftStateFromR(r, VFT_STATE_KEYS), file)
      })
    }
  )
  outputOptions(output, "downloadSave", suspendWhenHidden = FALSE)

  # output$downloadSaveRaster <- shiny::downloadHandler(
  #   filename = function(){
  #     browser()
  #     SM_name <- paste0("visitorFlowSave_SensitiveMatrix_", r$SMdateTime, ".tif")
  #     return(SM_name)
  #   },
  #   content = function(file){
  #
  #     return(terra::writeRaster(r$SM_pres, file) )
  #
  #   }
  # )
  # outputOptions(output, "downloadSaveRaster", suspendWhenHidden = FALSE)


  # INTERNAL FUNCTION - IMAGE MAP (make image clickable) ####

  #STEP 1:
  #ask for user input regarding area,
  #returns a tidy shapefile (shape) and a reactive confirm from step 1 confirm button (confirm)

  # step1return <- step1_server("step1")

  #ignoreInit = FALSE: this one fires once at session start to build step 1,
  #which is the tab the page already opens on - so no navigation happens and
  #r$step stays unset until step 1 is confirmed. Later visits arrive through
  #vftGoToStep(r, "step1", session), which bumps the counter and re-runs this
  #observer - and finds the module already built.
  shiny::observeEvent(vftStepTrigger(session, "step1"), {
    vftDbg("BUILD STEP 1")

    #FIRST-TOUCH SINGLETON (Stage 5). Step 1 is the one step whose module is
    #built at SESSION START rather than by a navigation, so "first touch" here is
    #the page opening; every later visit reuses it and runs its enter().
    #
    #`shape` and `shapeLarger` are passed in because a converted module has to be
    #able to put the state that is in force back on screen: coming back to step 1
    #shows the perimeter the app currently holds. They are REACTIVES - a plain
    #value would be frozen for the life of the session, and this module is no
    #longer rebuilt to unfreeze it.
    vftModuleOnce(session, "step1", function(){
    step1return <- vftTime("module:step1", step1_server("step1",
                                i18n        = shiny::reactive(i18n),
                                shape       = shiny::reactive(r$shape),
                                shapeLarger = shiny::reactive(r$shapeLarger)))#, lang = reactive(input$lang_pick)


    #REACTIVES

    #`once = TRUE` is gone with the rebuild it existed for: it stopped a REBUILT
    #module's handlers stacking on the live ones, and there is now exactly one of
    #each per session. Leaving it on would mean step 1 could be confirmed once
    #and never left again - which is the whole of what returning to it is for.
    #See also enter(), which clears r1$confirm so the second confirmation is not
    #deduped into silence.
    shiny::observeEvent( step1return$confirm(), {
      # isolate({
      #   shinyjs::runjs("Shiny.onInputChange('step1-confirmButton1', 0);")
      #   shinyjs::runjs("Shiny.onInputChange('step1-confirmButton2', 0);")
      # }
      # )
      shinyjs::reset(id = "step1-confirmButton1")
      shinyjs::reset(id = "step1-confirmButton2")


      vftDbg("REACTIVE::: STEP1$CONFIRM")
      #button indirectly triggers step 2, to allow for program intervention
      if(step1return$confirm() == 1){


        vftDbg("PRE-TRIGGER STEP 2")
        #Step 1 now hands over the perimeter and nothing else. `network`, `DULN`
        #and `DULN_all` used to be read here because step 1 built all three
        #eagerly, inside one future, before the user had done anything but draw a
        #shape - and step 2 reads none of them. They are providers now
        #(R/providers.R) and are derived when a step that actually reads them is
        #entered. step1_server still returns them for call-site compatibility;
        #reading them here would only put NULLs into `r` and make every one of
        #them look "not derivable but present".
        r$needHelp <- step1return$needHelp()
        # r$parking <- step1return$parking()

        #A new perimeter is the most expensive write in the app: everything
        #further on was cut from it. vftCommit() names what that costs and asks
        #before doing it - which, now that step 1 can be returned to, it does:
        #this is the one confirm in the app whose modal can name the entire rest
        #of the walk. And it asks ONLY for a new outline. Come back to step 1 to
        #look at the area, press on through, and step1_server hands the same two
        #objects straight back; vftCommit() finds nothing changed and nothing is
        #discarded. See reconfirmUnchanged() in R/step1_server.R.
        #
        #shapeLarger: the 1 km buffered perimeter every crop and the path query
        #are cut against. step 1 computes it because the shapefile and drawing
        #branches normalise the shape differently; a provider derives it from
        #`shape` alone on the restore path, where no save file has ever carried it.
        #
        #### where the crash snapshot is written ####
        #
        #In the `then =` below, and in the `then =` of every other vftCommit()
        #in this file. Two reasons it goes there and not beside the commit:
        #`then` does not run when the invalidation modal is cancelled, so a
        #snapshot is never taken of a state the user declined to create; and a
        #confirm is exactly the moment worth not losing.
        #
        #What it costs is nothing like what the old autosave cost. That
        #materialised the sensitivity raster and save()d the whole session
        #state - 17.8-24.1 MB gzipped - on the thread every other user is
        #waiting on, and downloaded it to their Downloads folder unasked.
        #The snapshot is the perimeter, the areas of interest and the choices
        #(VFT_SNAPSHOT_KEYS in R/state.R), and it goes to the browser.
        vftCommit(r,
                  list(shape       = step1return$ffshape(),
                       shapeLarger = step1return$shapeLarger()),
                  session, step = "step1",
                  #### confirming step 1 does NOT move the user on ####
                  #
                  #It used to be `then = function() vftGoToStep(r, "step2",
                  #session)`. Steps 1 and 2 are the two places in the walk where
                  #what comes next is a genuine choice - the perimeter alone
                  #opens the sensitivity matrix, the whole simulation chain and
                  #Hitzeminderung, and none of them is more "next" than the
                  #others - so the app writes the result and leaves the user
                  #standing on the step to make it. Steps 3 and 4 and the
                  #Szenarien page still hand over by themselves: each of those
                  #is a stage of one piece of work, not a fork.
                  #
                  #What the user then sees is the nav bar: the buttons this
                  #write unlocks light up, and vftNavHint() puts an arrow and
                  #"Choose a next action" under them, because a confirm that
                  #moves nothing needs to say that it did something. The line
                  #comes down again inside vftGoToStep(), whichever button they
                  #press. See vftStepNav() in R/app_ui.R.
                  then = function(){
                    vftNavHint(TRUE, session)
                    vftSnapshotWrite(r, session)
                  })

      }else if(step1return$confirm() == -1){

        #### restoring an uploaded save file ####
        #
        #confirm() == -1 is step 1's sentinel for "a save file was chosen"; the
        #module stashes the temp path and does not read the file itself.
        #
        #This was a load() into the observer's own frame followed by forty
        #`if(exists("envBase_x")){ r$x <- envBase_x }` lines and the resume
        #ladder. All of it is two calls now, in R/state.R, shared with the
        #browser snapshot - which is the point: there is one function that
        #knows how to write state into `r`, and one registry that knows what
        #state is.
        #
        #vftStateRead() also fixes something the ladder got wrong. exists() was
        #tested against the OBSERVER's frame, which persists between two loads
        #in one session, so a second file silently inherited the first one's
        #value for every key it happened not to carry. Reading into a fresh
        #empty environment makes that impossible by construction.
        vals <- vftStateRead(step1return$datapath())

        #The language comes from the selector, not from the file: what the user
        #is reading the app in now beats what whoever made the file was reading
        #it in. Written onto the list rather than onto `r` around the call, so
        #it cannot depend on where in vftApplyState() the key loop happens to
        #sit.
        #
        #`r$currentLang` and not `step1return$currentLang()`: the module's return
        #is a reactive over an R6 field that never invalidates, so it answers
        #with the language of the FIRST time anything asked it - see
        #vftLangServer() in R/navigation.R, which owns this key now.
        vals$currentLang <- shiny::isolate(r$currentLang)

        vftApplyState(r, vals, session)
      }
    }, ignoreInit = TRUE)

    step1return
    })
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  #
  #
  #STEP 2:
  #determine SDM selection, generate sensitivity matrix

  #When confirmation button clicked
  shiny::observeEvent(vftStepTrigger(session, "step2"), {
    vftDbg("BUILD STEP 2")
    #use shape information to clip, prepare and present SDM information
    #The whole confirmed selection goes back in, not just checkboxSave. These six
    #are written together when step 2 confirms and restored together from a save
    #file, and step 2's delayed restore block reads all six - but only
    #checkboxSave used to be passed, so a second visit ran that block against a
    #module that still had NULLs and died twice over. app_server has held these
    #values all along; they simply never travelled back.
    #FIRST-TOUCH SINGLETON (Stage 5). Built on the first visit and reused
    #afterwards; vftGoToStep() calls its enter() on every return. The module AND
    #the observer on its handle are created exactly once per session, which is
    #why that observer is no longer `once = TRUE`: the flag was there to stop a
    #REBUILT module's handlers stacking on the live ones, and it would now mean
    #step 2 could be confirmed once and never used again.
    #
    #Every input is a REACTIVE. The six save-state ones are read once, at
    #construction, by design - see the note on step2_server().
    vftModuleOnce(session, "step2", function(){
    step2return <- vftTime("module:step2", step2_server("step2",
                                fshape          = shiny::reactive(r$shape),
                                needHelp        = shiny::reactive(r$needHelp),
                                filterList      = shiny::reactive(r$filterList),
                                checkboxSave    = shiny::reactive(r$checkboxSave),
                                groupSave_all   = shiny::reactive(r$groupSave_all),
                                groupSave_sens  = shiny::reactive(r$groupSave_sens),
                                groupSave_type  = shiny::reactive(r$groupSave_type),
                                groupSave_class = shiny::reactive(r$groupSave_class),
                                weightInputs    = shiny::reactive(r$weightInputs),
                                weightNames     = shiny::reactive(r$weightNames),
                                i18n            = shiny::reactive(i18n),
                                currentLang     = shiny::reactive(r$currentLang)))


    shiny::observeEvent(step2return$confirm(), {

      vftDbg("REACTIVE::: STEP2RETURN$CONFIRM")
      vftDbg(step2return$confirm())

      vftDbgCat("STEP 3 STARTED")
      #button indirectly triggers step 2, to allow for program intervention
      if(is.integer(step2return$confirm()) & step2return$confirm() > 0){

        # shinyjs::runjs("Shiny.onInputChange('step2-confirmButton2', 0);")
        shinyjs::reset(id = "step2-confirmButton2")

        vftDbg("PRE-TRIGGER STEP 2")
        #save returns that are not part of the dependency graph
        r$needHelp <- step2return$needHelp()

        vftDbgCat("STEP 3_2")

        #The sensitivity matrix and the whole selection behind it, in one write.
        #Nothing downstream is derived from any of them - a simulation is a
        #function of the network, and every consumer of the matrix is a display
        #overlay that reads the current value (see VFT_DERIVED_FROM) - so this
        #commit never raises the modal. The one thing it does discard is
        #SM_pres_packed, the save file's cached copy, which used to be NULLed by
        #hand right here and is now an edge in the graph.
        vftCommit(r,
                  list(SM_pres         = step2return$SM_pres(),
                       # SM_noPres     = step2return$SM_noPres(),
                       SMcolors        = step2return$SMcolors(),
                       minCutThresh    = step2return$minCutThresh(),
                       species         = step2return$species(),
                       toSelectSpAfter = step2return$toSelectSpAfter(),
                       groupSave_all   = step2return$groupSave_all(),
                       groupSave_sens  = step2return$groupSave_sens(),
                       groupSave_type  = step2return$groupSave_type(),
                       groupSave_class = step2return$groupSave_class(),
                       checkboxSave    = step2return$checkboxSave(),
                       filterList      = step2return$filterList(),
                       weightInputs    = step2return$weightInputs(),
                       weightNames     = step2return$weightNames()),
                  session, step = "step2",
                  then = function(){
                    #### confirming step 2 does NOT move the user on ####
                    #
                    #Same as step 1's confirm above, and for the same reason:
                    #with the matrix written, the simulation chain and the
                    #Szenarien page are both open and neither is the obvious
                    #next thing. The bar is where that is decided, and this is
                    #the line that points at it. The `vftGoToStep(r, "step3",
                    #session)` that stood here is gone.
                    vftNavHint(TRUE, session)

                    #The sensitivity matrix itself is far too big for the
                    #browser (~4 MB packed, most of a localStorage quota on its
                    #own) and is deliberately not in VFT_SNAPSHOT_KEYS. What
                    #this preserves is the work that produced it: the species,
                    #the groups, the filters and the weights. A recovered
                    #session re-runs the SDM from exactly those choices.
                    vftSnapshotWrite(r, session)
                  })

        vftDbgCat("STEP 3_3")

        vftDbg(input$`step2-confirmButton2`)
        vftDbgCat("STEP 3_3")


      }else{
        #a banner letter, meaning "go back to an earlier step"; anything else is
        #left alone. See vftGoBack() in R/navigation.R.
        vftGoBack(r, step2return$confirm(), from = "step2",
                  bannerId = "step2-banner", session = session)
      }
    },ignoreInit = TRUE)

    step2return
    })
  })

  #STEP 3:
  #determine Areas of Interest
  shiny::observeEvent(vftStepTrigger(session, "step3"), {
    vftDbg("BUILD STEP 3")
    vftDbg(r$toSelectSpAfter)
    vftDbg(r$shape)

    #use shape information to clip, prepare and present SDM information
    vftDbgCat(paste0("DULN ALL 2: ", r$DULN_all))

    #PREFETCH THE 7-BAND CROP WHILE THE USER IS ON THIS STEP.
    #
    #Step 4 needs `DULN` and step 3 needs only `DULN_all`, so `r$DULN` was still
    #NULL when the confirm button was pressed - and vftGoToStep() therefore
    #DEFERRED the whole navigation while the provider cropped the national COG
    #(R/navigation.R, "hold the move until the step's data exists"). The user
    #waited for it on step 3 with nothing to look at, and with VFT_WORKERS=1 it
    #ran strictly BEFORE the areas-of-interest job rather than beside it.
    #
    #It depends on `shapeLarger` alone, which has existed since step 1, and this
    #is a page the user spends time on moving a slider. Asking for it here costs
    #the confirm click nothing and usually saves it the whole crop: vftEnsure()
    #only adds to the wanted set, the provider's own in-flight marker stops a
    #second dispatch, and if the crop has not landed by confirm time the
    #existing vftSetPendingStep() path completes the navigation when it does -
    #which is exactly what happened before, only started earlier.
    #
    #Outside vftModuleOnce() on purpose: this re-arms on every entry to step 3,
    #including a return after the perimeter changed and invalidated the crop.
    vftEnsure(r, "DULN", session)

    #FIRST-TOUCH SINGLETON (Stage 5). Built on the first visit and reused
    #afterwards; vftGoToStep() calls its enter() on every return. Everything in
    #this block - the module AND the two observers on its handle - runs exactly
    #once per session, which is why neither observer is `once = TRUE` any more:
    #that flag was there to stop a REBUILT module's handlers stacking on the live
    #ones, and with one instantiation there is nothing to stack. Leaving it on
    #would now mean step 3 could be confirmed once and never left again.
    #
    #The four inputs are REACTIVES. A plain value here is frozen for the life of
    #the session, and this module is no longer rebuilt to unfreeze it.
    vftModuleOnce(session, "step3", function(){
    step3return <- vftTime("module:step3", step3_server("step3",
                                shape       = shiny::reactive(r$shape),
                                i18n        = shiny::reactive(i18n),
                                currentLang = shiny::reactive(r$currentLang),
                                needHelp    = shiny::reactive(r$needHelp),
                                DULN_all    = shiny::reactive(r$DULN_all)
                                ))


    shiny::observeEvent(step3return$confirm(), {
      vftDbg("REACTIVE::: STEP4RETURN$CONFIRM")
      #button indirectly triggers step 2, to allow for program intervention
      if(is.integer(step3return$confirm()) & step3return$confirm() > 0 & step3return$isSkip() == 0 ){
        vftDbg("PRE-TRIGGER STEP 4")
        #reset confirm button (even if button is destroyed, input stays in memory)
        shinyjs::runjs("Shiny.onInputChange('step3-confirmButton3', 0);")
        # shinyjs::reset(id = "step3-confirmButton3", asis = TRUE)

        #save returns that are not part of the dependency graph
        # r$DULN <- step3return$DULN()
        # r$DULN_all <- step3return$DULN_all()
        r$needHelp <- step3return$needHelp()

        #THE write this whole change is about. A new attractiveness threshold is
        #a new set of areas of interest, and everything cut from them - the
        #Zielgebiete, the parking, the scenarios, the simulations, the saved
        #versions - stops describing what is on screen. That used to happen
        #silently, on the way IN to this step; it happens here now, when the new
        #threshold is actually confirmed, and only if it differs from the one
        #already held. Walk back through step 3, read it, confirm the same
        #number, and nothing at all is discarded.
        #
        #Cancelling leaves everything as it was, so step 3's own buttons - which
        #its confirm observer disabled on the way out - have to come back, or the
        #user would be left on a step they cannot leave.
        #
        vftDbg(input$`step3-confirmButton3`)

        vftCommit(r,
                  list(minThresh = step3return$minThresh(),
                       isSkip    = step3return$isSkip()),
                  session, step = "step3",
                  then     = function(){
                    vftGoToStep(r, "step4", session)
                    vftSnapshotWrite(r, session)
                  },
                  onCancel = function(){
                    shinyjs::enable(id = "step3-confirmButton3")
                    shinyjs::enable(id = "step3-skipButton")
                  })
      }else if(step3return$isSkip() == TRUE ){

        #Skipping produces no threshold, so it supersedes nothing and never
        #raises the modal: whatever areas of interest exist are kept and step 4
        #opens on them, which is what "skip" has always meant. Through
        #vftCommit() anyway, so that the rule "a step's results are written in
        #one place, by one function" holds for every exit from every step.
        vftCommit(r, list(isSkip = step3return$isSkip()), session, step = "step3",
                  then = function(){
                    vftGoToStep(r, "step4", session)
                    vftSnapshotWrite(r, session)
                  })
      }else{
        #a banner letter, meaning "go back to an earlier step". See
        #vftGoBack() in R/navigation.R.
        vftGoBack(r, step3return$confirm(), from = "step3",
                  bannerId = "step3-banner", session = session)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(step3return$skip(), {
      if(step3return$isSkip() > 0 ){

        #the skip BUTTON, as opposed to the confirm handler's skip branch above.
        #Same write, same reasoning: nothing is superseded by skipping.
        vftCommit(r, list(isSkip = step3return$isSkip()), session, step = "step3",
                  then = function(){
                    vftGoToStep(r, "step4", session)
                    vftSnapshotWrite(r, session)
                  })
      }
    }, ignoreInit = TRUE)

    step3return
    })
  })


  #STEP 4

  shiny::observeEvent(vftStepTrigger(session, "step4"), {
    vftDbg("BUILD STEP 4")
    #There used to be a second call here for the skip-step-3 path, reached by
    #setting the trigger to -1, and it differed from this one in exactly one
    #way: it did not pass `shape`. step4_server reads `shape` unconditionally in
    #its body (st_transform of the perimeter, to clip the lakes), so that call
    #could only ever have errored - the skip path was broken. Whether the user
    #skipped is already carried by `skip = r$isSkip`, so one call does both.
    #FIRST-TOUCH SINGLETON (Stage 5). Built once, re-entered through its enter()
    #closure. This is the module the duplicate-instance defect was observed in:
    #two live step 4s answering one confirm click, the older one writing the
    #network it had frozen before the area of interest changed.
    vftModuleOnce(session, "step4", function(){
    step4return <- vftTime("module:step4", step4_server("step4",
                                #`network` is not passed and is not wanted: this
                                #step edits AOI polygons cut out of the DULN
                                #rasters and never reads a path. See R/steps.R.
                                minThresh     = shiny::reactive(r$minThresh),
                                skip          = shiny::reactive(r$isSkip),
                                DULN          = shiny::reactive(r$DULN),
                                DULN_all      = shiny::reactive(r$DULN_all),
                                needHelp      = shiny::reactive(r$needHelp),
                                i18n          = shiny::reactive(i18n),
                                currentLang   = shiny::reactive(r$currentLang),
                                shape         = shiny::reactive(r$shape),
                                #The areas of interest already confirmed, if any.
                                #This used to be left at its NULL default, so
                                #step 4's enter() reset its polygons to NULL on
                                #every visit and .vftStep4Launch() generated a
                                #fresh set - i.e. merely LOOKING at step 4 threw
                                #the Zielgebiete away and replaced them, which is
                                #the behaviour this change exists to remove. With
                                #the real value passed, a return shows what was
                                #confirmed; they are regenerated only when
                                #something upstream invalidated them, which
                                #leaves this NULL again.
                                finalPolygons = shiny::reactive(r$finalPolygons)))



    shiny::observeEvent( step4return$confirm() , {
      vftDbg("REACTIVE::: STEP5RETURN$CONFIRM")
      #button indirectly triggers step 2, to allow for program intervention
      if(is.integer(step4return$confirm()) & step4return$confirm() > 0){
        # shinyjs::runjs("Shiny.onInputChange('step4-confirmButton4', 0);")
        # shinyjs::reset(id = "step4-confirmButton4", asis = TRUE)
        shinyjs::runjs("Shiny.onInputChange('step4-confirmButton4', 0);")

        vftDbg("PRE-TRIGGER STEP 5")


        # shinyjs::reset
        #save returns that are not part of the dependency graph
        r$needHelp <- step4return$needHelp()

        #CREATE NETWORK LIST ####
        #Package together all aspects that can be altered and results (network,
        #usage, parking, attractivity..). This list IS the simulation's input, so
        #building a new one is what ends the old simulations - which is why it
        #goes through vftCommit() with everything else step 4 confirms rather
        #than being assigned here. If the user has simulations or saved versions,
        #this is the write that asks them first.
        #
        #The network in it is the RAW one the provider derived, and `parking` is
        #NULL. Both used to be step 4's own output, from the ~30s job that has
        #moved to R/prepare_network.R - it now runs when the first simulation is
        #launched, which is where the first read of either happens. Everything
        #between here and there works without paths at all: the scenario cards
        #are labelled buttons, the map before a simulation is a static
        #placeholder, and every display checkbox that would want the prepared
        #columns is disabled until a pathUsage exists.
        #
        #AND r$network IS USUALLY NULL AT THIS POINT, which is deliberate and is
        #why this is a plain read rather than a vftEnsure(). No step's `needs`
        #names the network any more, so nothing on the way here has loaded it;
        #vftPrepareThen() loads it on the first simulation or the first visit to
        #newVersions and caches it in r$network, and a scenario built after that
        #gets the cached copy for free. Reading it here is "use it if it has
        #already been paid for", never "go and fetch it".
        #
        #AND IT MAY NOT BE A NEW LIST AT ALL. A user who came in through the
        #Hitzeminderung door has been painting heat designs on a scenario list
        #that has no network and no Zielgebiete - a canvas, vftIsCanvasList() in
        #R/steps.R - and took this page's own offer to go and draw them. Their
        #versions are the reason they are here; replacing them with one empty
        #scenario would throw the work away at the moment it became usable.
        #
        #So the confirmation ARRIVES INTO them: same entries, same order (card i
        #still names scenario i, and Original is still 1, which is what lets step
        #5's createOriginalVersion() overwrite its card in place instead of
        #appending one with no scenario behind it), each one gaining the network
        #and losing the `heatOnly` tag - which is precisely what turns the canvas
        #back into a scenario list for VFT_KEY_READY. The paint rides along
        #untouched in paintedRaster/canopyRaster.
        #
        #Only when EVERYTHING on hand is a canvas. A real scenario list here means
        #step 4 is being re-confirmed with changed areas of interest, and those
        #scenarios were computed against the old ones: replacing them is right,
        #and vftCommit() below asks first.
        prior <- shiny::isolate(r$networkList)
        if(vftIsCanvasList(prior)){
          vftDbg(paste0("STEP4: merging the Zielgebiete into ", length(prior),
                        " painted scenario(s) rather than replacing them"))
          newList <- lapply(prior, function(v){
            #built as a literal, exactly like the fresh scenario below, rather
            #than by editing `v` in place: assigning NULL to a list element
            #REMOVES it, so `v$network <- r$network` on the usual NULL would
            #leave a scenario with no `network` field at all. Every read of it
            #answers NULL either way, but a scenario should have the same shape
            #whichever branch made it. The two rasters are the paint, and they
            #are the whole reason this branch exists.
            list(network = r$network, pathUsage = NULL, parking = NULL,
                 residential = NULL, newAttr = NULL,
                 paintedRaster = v$paintedRaster, canopyRaster = v$canopyRaster)
          })
        }else{
          newList <- list(list(network = r$network, pathUsage = NULL,
                               parking = NULL, residential = NULL,
                               newAttr = NULL))
        }

        #`network` and `parking` are no longer committed here. r$network is the
        #step-1 provider's key and step 4 was overwriting it with an
        #AOI-annotated copy; r$parking is now filled by whichever page ran the
        #preparation, through vftMirror() below. Invalidation is unchanged -
        #`parking` and `networkList` are both dependents of `finalPolygons`
        #(R/providers.R), so confirming CHANGED areas of interest still asks
        #before it discards simulations and saved versions.
        vftCommit(r,
                  list(finalPolygons = step4return$finalPolygons(),
                       networkList   = newList),
                  session, step = "step4",
                  then = function(){
                    r$step6FirstRun <- TRUE
                    r$newVersionsFirstRun <- TRUE

                    #vftGoToStep sets r$step, which the snapshot records, so it
                    #has to come first - a snapshot taken before it would name
                    #step 4 and resume the user one step behind where they were.
                    vftGoToStep(r, "step5", session)

                    #The Zielgebiete are in VFT_SNAPSHOT_KEYS, so this is the
                    #confirmation that puts real geometry in the browser. The
                    #network and the (empty) scenario list are not: both are
                    #rebuilt from `finalPolygons` and `minThresh` by the
                    #provider layer and vftPrepareThen().
                    vftSnapshotWrite(r, session)

                    r$triggerStep5_nr <- 1
                  },
                  #No onCancel: step 4's confirm observer re-enables its own two
                  #buttons before handing the result over, and it no longer
                  #destroys the map observers on the way out - enter() does that,
                  #once, on each visit. So a cancel leaves the step exactly as
                  #the user left it, polygons and all, and they can go on editing
                  #and confirm again.
                  onCancel = NULL)
      }else{
        #a banner letter, meaning "go back to an earlier step". See
        #vftGoBack() in R/navigation.R.
        vftGoBack(r, step4return$confirm(), from = "step4",
                  bannerId = "step4-banner", session = session)
      }
    }, ignoreInit = TRUE)

    step4return
    })
  })

  #STEP 5

  shiny::observeEvent(vftStepTrigger(session, "step5"), {
    vftDbg("BUILD STEP 5")
    # cat(file = stderr(), paste0("contents of envBase: ", ls(envBase)))
    #FIRST-TOUCH SINGLETON (Stage 5). Built once and re-entered through its
    #enter() closure - which matters more here than anywhere else, because the
    #newVersions page is a side trip off this step in both directions and every
    #round trip used to build another step 5 beside the live one. That is where
    #the two "Original" scenarios came from: two modules, each holding the
    #networkList it had frozen, both answering the same click.
    #
    #Every input is a REACTIVE; the module snapshots them in enter().
    vftModuleOnce(session, "step5", function(){
    step5return <- vftTime("module:step5", step5_server("step5",
                                networkList     = shiny::reactive(r$networkList),
                                SM_pres         = shiny::reactive(r$SM_pres),
                                SMcolors        = shiny::reactive(r$SMcolors),
                                shape           = shiny::reactive(r$shape),
                                finalPolygons   = shiny::reactive(r$finalPolygons),
                                versionsUI      = shiny::reactive(r$versionsUI),
                                isFirstRun_stp6 = shiny::reactive(r$step6FirstRun),
                                needHelp        = shiny::reactive(r$needHelp),
                                species         = shiny::reactive(r$species),
                                i18n            = shiny::reactive(i18n),
                                currentLang     = shiny::reactive(r$currentLang),
                                minCutThresh    = shiny::reactive(r$minCutThresh),
                                #the step-3 attractiveness threshold. New here:
                                #the attractivity-weighted edge distances are
                                #computed at the simulation launch now, not at
                                #step 4's confirm. See R/prepare_network.R.
                                minThresh       = shiny::reactive(r$minThresh),
                                selectedVersion = shiny::reactive(r$selectedVersion)))

    #### step 5's results, published into `r` as they are produced ####
    #
    #A simulation is written to r$networkList[[i]]$pathUsage INSIDE the module,
    #and the module's copy used to reach `r` only through the two handlers below
    #- the "Neue Versionen" button and the confirm button. Leaving step 5 by the
    #nav bar goes through neither, so the app never learned about the simulation;
    #then enter() refreshed the module's list from `r` on the way back in and
    #overwrote it with the version that has no results. Run a simulation, step
    #out, step back: gone. (The confirm door has never worked either - step 5 has
    #no live confirmButton5 observer - so in practice only the newVersions round
    #trip ever saved anything.)
    #
    #See vftMirror() in R/modules.R for the two guards and for why this is a
    #plain write rather than a vftCommit().
    #
    #`pathUsage` has a second consequence, deliberate: it is what the discard
    #warning names as "Simulationsergebnisse", and it was NULL at every point the
    #user could have been warned. It also means a save taken at step 5 now
    #carries it - the slot has always been in the save list and the restore path
    #has always read it back, so nothing about the format changes; the file grows
    #by one copy of the path network, which is the price of the warning being
    #true.
    vftMirror(r, "networkList", step5return$networkList)
    vftMirror(r, "versionsUI",  step5return$versionsUI)
    #which scenario card the two pages open on. Shared rather than per-page: the
    #newVersions page is a side trip off this step and the user is working on ONE
    #scenario across both. See vftVersionPosition() in R/modules.R.
    vftMirror(r, "selectedVersion", step5return$selectedVersion)
    vftMirror(r, "pathUsage",   step5return$pathUsage)
    vftMirror(r, "shp_PA",      step5return$shp_PA)
    #the parking table, once the preparation this page triggers has produced it.
    #Step 4 used to commit it; the job moved to R/prepare_network.R and the
    #scenario carries the authoritative copy, so this only keeps the app-level
    #key - which is what the save file records - in step with it.
    vftMirror(r, "parking",     step5return$parking)

    #From step 5, go to New Versions
    shiny::observeEvent(step5return$newVersions(), {

      vftDbg("From step 5, go to New Versions")
      r$triggerStep5_nr <- step5return$trigger()
      #the networkList / versionsUI / shp_PA writes that used to be here are
      #vftMirror()'s job now, and have already happened. Leaving them would not
      #be wrong, only a second copy of the rule.


      #
      if(step5return$newVersions() > 0 & r$triggerStep5_nr == 1){

        #save returns (but only if larger than null: Avoid overwriting with default empty returns)
        #get latest change in networkList (ex: if pathUsage was updated)
        # if(length(step5return$networkList()) > 0 ){
        #   networkList <<- step5return$networkList()
        # }
        # if(length(step5return$versionsUI()) > 0 ){
        #   versionsUI <<- step5return$versionsUI()
        # }

        r$triggerNewVersions_nr <- 1

        #the card the newVersions page should open on. Explicit for the same
        #reason as the writes in that page's confirm handler: vftGoToStep() calls
        #the destination's enter() in this same tick, ahead of the mirror.
        if(length(step5return$selectedVersion() ) > 0){
          r$selectedVersion <- step5return$selectedVersion()
        }

        vftGoToStep(r, "newVersions", session)

        #guards the step5 <-> newVersions handlers against firing each other;
        #nothing to do with navigation, which is vftGoToStep()'s job now.
        r$triggerStep5_nr <- 0

      }
    }, ignoreInit = TRUE)



    #Step 5's banner. There is nothing forward of here any more: the Resultate
    #page (module lastStep_server) was removed on 2026-08-27 as non-functional,
    #and step 5 - or its side trip to Neue Versionen - is where the walk ends.
    #
    #So this handler carries banner letters only, which is all it has ever
    #actually carried: `confirm` is r$confirm inside step5_server, and the one
    #line that would have set it from confirmButton5 has been commented out at
    #step5_server.R:2196 for as long as the file has existed. The forward branch
    #that used to be here - go to finalStep, then click downloadSave - was
    #therefore never once reached, which is also what became of the third
    #autosave checkpoint.
    #
    #Settled 2026-09-01, and it is settled by not existing: a finished
    #simulation is preserved by the user pressing the disk icon, because it is
    #the one thing in this app that cannot go in the browser snapshot (a
    #pathUsage graph per scenario - see VFT_SNAPSHOT_KEYS in R/state.R). Do not
    #re-add an automatic save here; nothing should reach a user's Downloads
    #folder that they did not ask for.
    shiny::observeEvent(step5return$confirm(), {
      #a banner letter, meaning "go back to an earlier step". See
      #vftGoBack() in R/navigation.R.
      vftGoBack(r, step5return$confirm(), from = "step5",
                bannerId = "step5-banner", session = session)
    }, ignoreInit = TRUE)

    #### "make me a sensitivity matrix" ####
    #
    #Step 2 is optional (see VFT_STEPS in R/steps.R), so step 5 can be reached
    #with no matrix and its overlay checkbox is an offer rather than a toggle.
    #The module raises the modal and counts the "Ja"; the navigation has to
    #happen out here, because step5_server has neither the app's `r` nor the
    #app's session.
    #
    #`check = FALSE`, like every other transition app_server makes. `check` also
    #gates on VFT_NAV (vftNavAllows), and this offer must not go dead in a build
    #with the bar switched off - a modal whose "Ja" does nothing is worse than
    #no modal. Nothing is skipped by it either: step 2 needs only `shape`, which
    #exists by definition if the user is standing on step 5, and the new
    #"not derivable" refusal in vftGoToStep() applies to unchecked callers too.
    shiny::observeEvent(step5return$smCreate(), {
      vftDbg("From step 5, go to step 2 to build a sensitivity matrix")
      vftGoToStep(r, "step2", session)
    }, ignoreInit = TRUE)

    step5return
    })

    #Deliberately OUTSIDE the vftModuleOnce() block, so it runs on every visit
    #rather than once at construction. The module's enter() has already read this
    #flag by the time this line executes - vftGoToStep() calls enter() directly,
    #while the counter write that re-runs this observer is deferred to the flush -
    #so the order is "ask, then clear", on every entry. It has to be re-asked
    #every time because vftInvalidate() re-arms it whenever the saved versions are
    #discarded.
    r$step6FirstRun <- FALSE

  })


  #NEW VERSIONS PAGE
  shiny::observeEvent(vftStepTrigger(session, "newVersions"), {
    vftDbg("BUILD NEW VERSIONS")
    #FIRST-TOUCH SINGLETON (Stage 5, sixth module). This page is a side trip off
    #step 5 in both directions and every round trip used to call the 3900-line
    #server again, stacking a second set of observers and outputs on the live
    #ones. Every input is a REACTIVE; the module snapshots them in enter().
    vftModuleOnce(session, "newVersions", function(){
    newVersionsReturn <- newVersions_server("newVersions",
                                            networkList   = shiny::reactive(r$networkList),
                                            SM_pres       = shiny::reactive(r$SM_pres),
                                            SMcolors      = shiny::reactive(r$SMcolors),
                                            shp_PA        = shiny::reactive(r$shp_PA),
                                            finalPolygons = shiny::reactive(r$finalPolygons),
                                            versionsUI    = shiny::reactive(r$versionsUI),
                                            isFirstRun    = shiny::reactive(r$newVersionsFirstRun),
                                            DULN          = shiny::reactive(r$DULN),
                                            #the step-1 perimeter, for cropping the land cover under the
                                            #paint. step5_server takes it the same way
                                            shape         = shiny::reactive(r$shape),
                                            i18n = shiny::reactive(i18n),
                                            currentLang   = shiny::reactive(r$currentLang),
                                            #the step-3 threshold, for the same
                                            #reason step 5 now takes it: this
                                            #page can be the first to need the
                                            #prepared network. R/prepare_network.R.
                                            minThresh     = shiny::reactive(r$minThresh),
                                            selectedVersion = shiny::reactive(r$selectedVersion),
                                            #the nav bar's "Hitzeminderung" button writes r$vftContextPreset
                                            #("4") immediately before navigating here and clears it right
                                            #after - see vftNavBarServer() in R/navigation.R. Passed in like
                                            #every other app-level value this module reads, since its own `r`
                                            #is module-local (see the note above newVersions_server()).
                                            contextPreset = shiny::reactive(r$vftContextPreset))

    #### this page's results, published into `r` as they are produced ####
    #
    #Same defect step 5 had, and the same fix: a new version reached the app's
    #`r` only through the handler on the confirm button, so leaving by the NAV
    #BAR lost it - and then enter() refreshed the module from an `r` that had
    #never heard of it. See vftMirror() in R/modules.R for the two guards.
    #
    #The explicit writes in the confirm handler below are NOT redundant with
    #this and must stay: the mirror is an observe() and runs at the next flush,
    #while the confirm handler calls vftGoToStep(r, "step5") - and therefore
    #step 5's enter(), which reads r$networkList - in this same tick. The
    #identical() guard inside vftMirror() makes the second write free.
    vftMirror(r, "networkList", newVersionsReturn$networkList)
    vftMirror(r, "versionsUI",  newVersionsReturn$versionsUI)
    vftMirror(r, "selectedVersion", newVersionsReturn$selectedVersion)
    #this page can be the first to run the preparation - see the context
    #observer in newVersions_server.R - so it publishes parking too.
    vftMirror(r, "parking",     newVersionsReturn$parking)
    #which context the page is showing, for the nav bar alone: "Hitzeminderung"
    #and "Neue Versionen" are two doors into this one module, and the ring has to
    #sit on the one the user came through - and move when they change the radio.
    #See vftNavCurrentId() in R/navigation.R; nothing else reads this key.
    vftMirror(r, "navContext",  newVersionsReturn$context)

    #### the newVersions page's two offers ####
    #
    #Same arrangement as step 5's smCreate above, and for the same reason: the
    #module raises the modal and counts the "Ja", but it has neither the app's
    #`r` nor the app's session, so the navigation has to happen out here.
    #
    #"Sensitivitätsmatrix anzeigen" with no matrix -> step 2, exactly as on
    #step 5. "Wegen/Strassen" or "Parken/Wohnen" with no confirmed Zielgebiete
    #-> step 3, which is where the areas of interest are begun; the user came in
    #through the Hitzeminderung door, which opens on the perimeter alone
    #(VFT_HITZE_NEEDS in R/steps.R), so steps 3 and 4 are still ahead of them.
    #
    #`check = FALSE`, like every other transition app_server makes, and for the
    #reason spelled out at step 5's smCreate: `check` also gates on VFT_NAV, and
    #an offer whose "Ja" does nothing in a build with the bar switched off is
    #worse than no offer. Nothing is skipped by it - step 2 and step 3 both need
    #only what a user standing on this page already has, and vftGoToStep()'s
    #"not derivable" refusal still applies to unchecked callers.
    shiny::observeEvent(newVersionsReturn$smCreate(), {
      vftDbg("From new versions, go to step 2 to build a sensitivity matrix")
      vftGoToStep(r, "step2", session)
    }, ignoreInit = TRUE)

    shiny::observeEvent(newVersionsReturn$aoiCreate(), {
      vftDbg("From new versions, go to step 3 to determine the areas of interest")
      vftGoToStep(r, "step3", session)
    }, ignoreInit = TRUE)

    shiny::observeEvent(newVersionsReturn$confirm(), {

      vftDbgCat(paste0("newVersionsReturn$trigger_1() ", newVersionsReturn$trigger_1()))

      r$triggerNewVersions_nr <- newVersionsReturn$trigger_1()
      vftDbgCat("TESTD")

      #save returns (but only if non NULL, to avoid overwriting with default)
      if(length(newVersionsReturn$networkList() ) > 0){
        r$networkList <- newVersionsReturn$networkList()
      }
      vftDbgCat("TESTE")

      if(length(newVersionsReturn$versionsUI() ) > 0){
        r$versionsUI <- newVersionsReturn$versionsUI()
      }

      #and the card step 5 should open on, for the same reason as the two above:
      #this handler reaches step 5's enter() in the SAME tick, so the mirror is
      #too late. It has to travel WITH the version list it names - a name written
      #a flush later than the list it belongs to is a name step 5 resolves against
      #the wrong list.
      if(length(newVersionsReturn$selectedVersion() ) > 0){
        r$selectedVersion <- newVersionsReturn$selectedVersion()
      }

      vftDbg("From new versions, return to step 5")
      vftDbgCat("TESTF")
      vftDbgCat(paste0("newVersionsReturn$confirm() : ", newVersionsReturn$confirm()))
      vftDbgCat(paste0("r$triggerNewVersions_nr : ", r$triggerNewVersions_nr))

      #button indirectly triggers step 5, to allow for program intervention
      if(newVersionsReturn$confirm() > 0 & r$triggerNewVersions_nr == 1 ){
        vftDbg("PRE-TRIGGER STEP 5 return")
        vftDbgCat("TESTFb")

        #No snapshot on this path, and not by omission. Coming back from the
        #Szenarien page produces scenarios, and scenarios are exactly what the
        #browser snapshot cannot hold - one copy of the path network per
        #version (see VFT_SNAPSHOT_KEYS in R/state.R). Nothing else changed
        #here that is not already in the browser from the step-4 confirm, so a
        #write would cost a serialisation and preserve nothing new. Scenarios
        #are preserved by the disk icon's full save.
        vftGoToStep(r, "step5", session)
        vftDbgCat("TESTFc")

        r$triggerNewVersions_nr <- 0
        # isolate(tiggerNewVersions(0))#reset value without trigger

      }
      #`once = TRUE` had to go with the rebuild it existed for. It was there so a
      #REBUILT module's handler would not stack on the live one; with one
      #instantiation it would mean the page could be left exactly once per
      #session and then never again.
    }, ignoreInit = TRUE)

    newVersionsReturn
    })

    #Deliberately OUTSIDE the vftModuleOnce() block, so it runs on every visit
    #rather than once at construction. The module's enter() has already read this
    #flag by the time this line executes - vftGoToStep() calls enter() directly,
    #while the counter write that re-runs this observer is deferred to the flush -
    #so the order is "ask, then clear", on every entry. It has to be re-asked
    #every time because vftInvalidate() re-arms it whenever the saved versions are
    #discarded.
    r$newVersionsFirstRun <- FALSE

  }, ignoreInit = TRUE)


  #The "GO TO FINAL STEP" observer that used to be here is gone, with the step
  #itself: `finalStep` - the Resultate page, module lastStep_server, R/lastStep_ui
  #and R/lastStep_server.R - was removed on 2026-08-27 because it had gone
  #non-functional. newVersions is the last step, and nothing in the registry,
  #the nav bar or the restore path names a step 6 any more.


  # #### TRIGGERS WHEN CHANGING TABS
  # observeEvent(input$tabs){
  #
  #   if(tabs == "tab_step4"){
  #     #check if important element exists already
  #
  #   }
  #
  # }


  #### open on step 1, explicitly ####
  #
  #A session used to arrive on step 1 by three coincidences rather than by a
  #decision: the tabsetPanel shows its first panel by default, step 1's observer
  #has ignoreInit = FALSE so it builds on the first flush, and vftNavBarServer()
  #SEEDS r$navStep to "step1" when it finds it NULL purely so the bar has
  #something to ring. Nothing navigated. r$step stayed NULL, so until the first
  #confirm the app could not say which step it was on - and every one of those
  #three had to keep being true independently.
  #
  #This says it once instead. vftGoToStep() shows the tab, sets r$step and
  #r$navStep, bumps step 1's visit counter and runs the module's enter() - the
  #same path every other arrival at every other step takes. `check = FALSE`
  #because step 1 has no `needs` and there is nothing to gate on.
  #
  #LAST in the body, after every observer above is registered, so the counter
  #bump has something to reach. It is also before any client message: the
  #crash-recovery prompt (vftStateServer, above) cannot answer until the browser
  #has connected and sent its snapshot back, so a restore that lands on step 4
  #still wins - it simply happens later.
  vftGoToStep(r, "step1", session)

}
