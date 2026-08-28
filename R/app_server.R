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

  #accompanying non reactive (nr) variables, to allow for back and forth access between step5 and newVersions
  r$triggerNewVersions_nr <- 1
  r$triggerStep5_nr <- 1

  #keep track if tutorial needed
  r$needHelp <- FALSE


  #SAVE ENVBASE TO USER (OFFER OPTION TO DOWNLOAD)
  # Downloadable csv of selected dataset ----
  output$downloadSave <- shiny::downloadHandler(
    filename = function(){
      dateTime <- gsub(":|-| ", "_",Sys.time())
      dateTime <- substr(dateTime, 1,nchar(dateTime)-3)

      #save version of SM that will need to be uploaded
      # if(r$step == 4){r$SMdateTime <- dateTime}
      #stepName not same name as programming name
      #
      #READ THIS BEFORE CHANGING IT, and preferably decide it in the save
      #rewrite rather than here. `r$step` is an INTEGER (VFT_STEPS' `code`), and
      #switch() on a number selects BY POSITION and ignores the names entirely -
      #so "2", "3", ... are decoration and what actually happens is
      #stepName <- c(...)[r$step]. It is not obvious which of the two readings
      #was meant, and they disagree: the two checkpoints that still fire are the
      #step-2 confirm (r$step is 3 by then) and the step-4 confirm (r$step is 5),
      #and positionally those come out "3_schnell" and "5_simulation" while the
      #names would make them "ignore" and "4_genau". The positional answer is the
      #one users have been getting and the one that reads sensibly, so it is left
      #exactly as it was.
      #
      #Codes 6-8 were the Resultate page and its variants and are gone with it
      #(2026-08-27); nothing can write them, so the two entries are dropped. The
      #five that remain cover positions 1-5, which is every value `code` now
      #takes.
      stepName <- switch(r$step,
        "2" = "2_SM",
        "3" = "ignore",
        "4" = "3_schnell",
        "5" = "4_genau",
        "6" = "5_simulation"
      )
#"2_SM","4_ZG_genau"

      return(paste0("visitorFlowSave_step",stepName, "_", dateTime, ".RData"))
    },
    content = function(file){
      #known general-setup hot spot: a save() of the whole session state on the
      #thread every other user is waiting on. It used to fire at all eight step
      #transitions; it now runs at three checkpoints only (see the dropped-site
      #comments below). Labelled so the stall log attributes it by name rather
      #than to "unattributed".
      vftTime("app:downloadSave", {
      #save all elements of envBase (a bit of a detour by saving as variables first)
      envBase_step <- r$step
      envBase_shape <- r$shape
      envBase_toSelectSpAfter <- r$toSelectSpAfter
      # envBase_SM_pres <- r$SM_pres
      # envBase_SM_noPres <- r$SM_noPres
      envBase_SMcolors <- r$SMcolors
      envBase_network <- r$network
      envBase_parking <- r$parking
      envBase_residential <- r$residential
      envBase_minThresh <- r$minThresh
      envBase_confirm <- r$confirm
      envBase_finalPolygons <- r$finalPolygons
      envBase_networkList <- r$networkList
      envBase_versionsUI <- r$versionsUI
      envBase_step6FirstRun <- r$step6FirstRun
      envBase_weightInputs <- r$weightInputs
      envBase_weightNames <- r$weightNames

      envBase_needHelp <- r$needHelp
      envBase_species <- r$species

      #need to keep step6FirstRun TRUE, if versionsUI is NULL
      #reason: original version is generated only on first run (otherwise versionsUI is used)
      if(is.null(envBase_versionsUI)){
        envBase_step6FirstRun <- TRUE
      }

      envBase_isSkip <- r$isSkip
      envBase_triggerNewVersions_nr <- r$triggerNewVersions_nr
      envBase_triggerstep6_nr <- r$triggerstep6_nr
      envBase_pathUsage <- r$pathUsage
      envBase_newVersionsFirstRun <- r$newVersionsFirstRun
      # envBase_SMdateTime <- r$SMdateTime #not needed anymore?
      envBase_groupSave_all <- r$groupSave_all
      envBase_groupSave_sens <- r$groupSave_sens
      envBase_groupSave_type <- r$groupSave_type
      envBase_groupSave_class <- r$groupSave_class

      envBase_checkboxSave <- r$checkboxSave
      envBase_filterList <- r$filterList
      # envBase_weightInputs <- r$weightInputs

      #The sensitivity raster has to travel in the save file as a plain R object,
      #because a SpatRaster is an external pointer and does not survive save().
      #
      #This used to be terra::as.data.frame(xy = TRUE, na.rm = FALSE) - one row
      #per cell INCLUDING NAs, carrying two full-precision coordinate doubles per
      #cell that are entirely redundant for a regular grid, and dropping the CRS
      #(hence the explicit crs<- on the restore path below). terra::wrap() holds
      #the same information as a PackedSpatRaster. Measured at 100 m resolution
      #over a 100 km area (1M cells): 20.0 MB -> 4.0 MB retained per session and
      #0.11s -> 0.01s to build.
      #
      #The RAM is the point, not the time: this is cached for the life of the
      #session, on a host where daemons have already been OOM-killed.
      #
      #Backward compatible on purpose: terra::rast() has methods for BOTH a
      #data.frame and a PackedSpatRaster, so the restore path below reads old and
      #new save files without branching.
      #
      #Cached because SM_pres only changes in step 2 (and on resume), so this is
      #built once and reused by every later checkpoint rather than recomputed on
      #the main thread each time.
      #
      #A checkpoint taken before step 2 has been confirmed has no sensitivity
      #matrix, and that is a legitimate save file: the restore path below already
      #tests `!is.null(envBase_SM_pres)` and skips it. The WRITE side did not, and
      #terra::wrap(NULL) does not return NULL - it aborts ("unable to find an
      #inherited method for 'wrap' for signature x = \"NULL\""), out of a download
      #handler, so the browser gets an error page instead of the file. Reachable
      #since the nav bar started offering step 3 straight after step 1: nothing
      #between there and the step-4 checkpoint needs SM_pres, so nothing stops a
      #user routing around step 2 and reaching this line with it unset.
      if(is.null(r$SM_pres_packed) && !is.null(r$SM_pres)){
        r$SM_pres_packed <- terra::wrap(r$SM_pres)
      }
      envBase_SM_pres <- r$SM_pres_packed

      #save MinCutThreshold
      envBase_minCutThresh <- r$minCutThresh

      return(save(envBase_step,
                  envBase_shape,
                  envBase_toSelectSpAfter,
                  envBase_SM_pres,
                  # envBase_SM_noPres,
                  envBase_SMcolors,
                  envBase_network,
                  envBase_parking,
                  envBase_residential,
                  envBase_minThresh,
                  envBase_isSkip,
                  envBase_confirm,
                  envBase_finalPolygons,
                  envBase_networkList,
                  envBase_versionsUI,
                  envBase_triggerNewVersions_nr,
                  envBase_triggerstep6_nr,
                  envBase_pathUsage,
                  envBase_step6FirstRun,
                  envBase_newVersionsFirstRun,
                  envBase_groupSave_all ,
                  envBase_groupSave_sens,
                  envBase_groupSave_type,
                  envBase_groupSave_class,
                  envBase_checkboxSave,
                  envBase_filterList,
                  envBase_weightInputs,
                  envBase_weightNames,
                  envBase_needHelp,
                  envBase_species,
                  envBase_minCutThresh, file = file,
                  #save() defaults to gzip at compression_level 6, which is pure
                  #main-thread CPU over the whole session state -- the raster,
                  #the network, the polygons, the basemap -- on the thread every
                  #other user is waiting on. Measured on a
                  #91.6 MB representative payload:
                  #  level 6 (default) 4.94 s -> 17.8 MB
                  #  level 3           2.03 s -> 20.2 MB
                  #  level 1           0.87 s -> 24.1 MB
                  #  none              0.11 s -> 91.6 MB
                  #Level 1 is 5.7x less blocking for a 35% larger file, and the
                  #format is unchanged so load() reads it exactly as before.
                  #Uncompressed would be faster still but quadruples what the
                  #user has to download.
                  compress = "gzip", compression_level = 1)) #envBase_SMdateTime,
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
        r$currentLang <- step1return$currentLang()
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
        #autosave dropped here, on leaving step 1 (shape + path network). downloadSave
        #materialises the raster and save()s the whole session state on the
        #shared main thread, and it used to fire at all eight step transitions.
        #It now runs only at the three checkpoints where losing work is
        #expensive: the sensitivity matrix, the confirmed network + parking,
        #and the finished simulation. Restore with:
        #  shinyjs::click("downloadSave", asis = FALSE)
        vftCommit(r,
                  list(shape       = step1return$ffshape(),
                       shapeLarger = step1return$shapeLarger()),
                  session, step = "step1",
                  then = function() vftGoToStep(r, "step2", session))

      }else if(step1return$confirm() == -1){

        #load objects
        load(step1return$datapath())

        #populate envBase with saved data
        if(exists("envBase_step")){r$step <- envBase_step}
        if(exists("envBase_shape")){r$shape <- envBase_shape}
        if(exists("envBase_toSelectSpAfter")){r$toSelectSpAfter <- envBase_toSelectSpAfter}
        # if(exists("envBase_SM_noPres")){r$SM_noPres <- envBase_SM_noPres}
        if(exists("envBase_SMcolors")){r$SMcolors <- envBase_SMcolors}
        if(exists("envBase_network")){r$network <- envBase_network}
        if(exists("envBase_parking")){r$parking <- envBase_parking}
        if(exists("envBase_residential")){r$residential <- envBase_residential}
        if(exists("envBase_minThresh")){r$minThresh <- envBase_minThresh}
        if(exists("envBase_isSkip")){r$isSkip <- envBase_isSkip}

        if(exists("envBase_confirm")){r$confirm <- envBase_confirm}
        if(exists("envBase_finalPolygons")){r$finalPolygons <- envBase_finalPolygons}
        if(exists("envBase_networkList")){r$networkList <- envBase_networkList}
        if(exists("envBase_versionsUI")){r$versionsUI <- envBase_versionsUI}
        if(exists("envBase_step6FirstRun")){r$step6FirstRun <- envBase_step6FirstRun}

        #double check that step5FirstRun is TRUE when versionsUI is empty (but this should never occur)
        if(is.null(r$versionsUI)){
          r$step6FirstRun <- TRUE
        }

        if(exists("envBase_triggerNewVersions_nr")){r$triggerNewVersions_nr <- envBase_triggerNewVersions_nr}
        if(exists("envBase_triggerstep6_nr")){r$triggerstep6_nr <- envBase_triggerstep6_nr}
        if(exists("envBase_pathUsage")){r$pathUsage <- envBase_pathUsage}
        if(exists("envBase_newVersionsFirstRun")){r$newVersionsFirstRun <- envBase_newVersionsFirstRun}
        #envBase_basemap is deliberately NOT read back. step1 never assigned it,
        #so every save file that carries it carries a NULL; load() tolerates the
        #extra object, and current save files no longer write it at all.
        if(exists("envBase_dateTime")){r$dateTime <- envBase_dateTime}

        if(exists("envBase_groupSave_all")){r$groupSave_all <- envBase_groupSave_all}
        if(exists("envBase_groupSave_sens")){r$groupSave_sens <- envBase_groupSave_sens}
        if(exists("envBase_groupSave_type")){r$groupSave_type <- envBase_groupSave_type}
        if(exists("envBase_groupSave_class")){r$groupSave_class <- envBase_groupSave_class}
        if(exists("envBase_checkboxSave")){r$checkboxSave <- envBase_checkboxSave}
        if(exists("envBase_filterList")){r$filterList <- envBase_filterList}
        if(exists("envBase_weightNames")){r$weightNames <- envBase_weightNames}
        if(exists("envBase_weightInputs")){r$weightInputs <- envBase_weightInputs}
        if(exists("envBase_needHelp")){r$needHelp <- envBase_needHelp}
        if(exists("envBase_species")){r$species <- envBase_species}
        if(exists("envBase_minCutThresh")){r$minCutThresh <- envBase_minCutThresh}


        #The two national COGs used to be opened and cropped right here, on the
        #main thread, for every restore - including restores to steps that read
        #neither of them. They are providers now, so vftGoToStep() below derives
        #whatever the restored step actually needs, in a daemon, and holds the
        #navigation until it lands. Save files have never carried the rasters, so
        #nothing is lost and nothing about the format changes.
        #
        #envBase_network arrives with its node columns already attached, so
        #VFT_KEY_READY's column test marks `networkNodes` done and no restore ever
        #rebuilds the network.
        r$currentLang <- step1return$currentLang()

        #Load the saved sensitivity matrix. terra::rast() has methods for both
        #shapes this can arrive in - a PackedSpatRaster from a current save file,
        #or the xy data.frame older files carry - so both restore here without
        #branching. Only the data.frame form loses the CRS, hence the crs<-
        #below; on a packed raster it is a harmless no-op (it is already 4326).
        #
        #The guard used to be `length(envBase_SM_pres > 0)`, which compares the
        #whole object against 0 and takes the length of the RESULT. That is never
        #0 for a non-empty data.frame, so it never actually guarded anything -
        #and it ERRORS outright on a PackedSpatRaster ("comparison (>) is
        #possible only for atomic and list types"), which would have made every
        #new save file unloadable.
        if(exists("envBase_SM_pres")){
          if(!is.null(envBase_SM_pres)){
            r$SM_pres <- terra::rast(envBase_SM_pres)
            terra::crs(r$SM_pres) <- "epsg:4326"
            #Deliberately NOT reusing the loaded object as the save cache: an old
            #file hands back a 20 MB data.frame, and keeping it would both retain
            #it for the session and write the old fat form again at the next
            #checkpoint. Leaving this NULL costs one terra::wrap() (~0.01s) and
            #means every file this session writes is the compact form.
            r$SM_pres_packed <- NULL
          }
        }

        #### STAGE 6: resume ####
        #
        #This was five hand-written branches, one per step code, with no branch
        #at all for the last step and one (`r$step == 1`) that bumped a
        #reactiveVal nothing observed - so a save taken at step 1 restored its
        #data and then sat on whatever tab was already showing. All of it is two
        #calls now, and neither of them is a list of steps: vftRestoreStep()
        #reads the registry, so a step added or removed there needs nothing here.
        #
        #It answers a question the ladder never asked. The number in the file
        #says where the user WAS; whether that step can be entered is a question
        #about what else the file carried, and the registry already knows. A save
        #that names step 5 but has no `species` in it used to be honoured: step 5
        #was built, read NULL, and failed somewhere in the middle. It now resumes
        #at the furthest step that can actually run and says so.
        #
        #REACHABLE, not available, which is the capability this buys: a save
        #carrying nothing but `shape` is a legal file now. It names step 2, step 2
        #needs only `shape`, and the buffered perimeter, the attractiveness crop
        #and the path network are derived by the provider layer when a step that
        #reads them is entered - not rebuilt eagerly on the way in. vftGoToStep()
        #holds the navigation until they land, with the progress bar showing.
        wanted <- vftStepForCode(r$step)
        resume <- vftRestoreStep(r)

        if(!is.null(wanted) && !identical(wanted, resume)){
          vftDbg(paste0("RESTORE: save names ", wanted, ", resuming at ", resume,
                        " (missing: ",
                        paste(vftStepMissing(r, wanted), collapse = ", "), ")"))
          #Said out loud rather than logged only: landing somewhere other than
          #where the file was taken is confusing enough to be worth a line, and
          #the alternative the ladder took - honour the number and let the module
          #fail - is worse.
          try(shiny::showNotification(
            paste0("Die Datei reicht nur bis \u201e", VFT_STEPS[[resume]]$label,
                   "\u201c - dort geht es weiter."),
            type = "warning", duration = 8, session = session), silent = TRUE)
        }

        vftGoToStep(r, resume, session)
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
        r$currentLang <- step2return$currentLang()

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
                    #activate download. vftGoToStep sets r$step, which
                    #downloadSave reads to name the file, so it has to come
                    #before the click.
                    vftGoToStep(r, "step3", session)
                    shinyjs::click("downloadSave", asis = FALSE)
                    # shinyjs::click("downloadSaveRaster", asis = FALSE)
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
        r$currentLang <- step3return$currentLang()

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
        #autosave dropped here, on leaving step 3 (threshold choice). downloadSave
        #materialises the raster and save()s the whole session state on the
        #shared main thread, and it used to fire at all eight step transitions.
        #It now runs only at the three checkpoints where losing work is
        #expensive: the sensitivity matrix, the confirmed network + parking,
        #and the finished simulation. Restore with:
        #  shinyjs::click("downloadSave", asis = FALSE)

        vftDbg(input$`step3-confirmButton3`)

        vftCommit(r,
                  list(minThresh = step3return$minThresh(),
                       isSkip    = step3return$isSkip()),
                  session, step = "step3",
                  then     = function() vftGoToStep(r, "step4", session),
                  onCancel = function(){
                    shinyjs::enable(id = "step3-confirmButton3")
                    shinyjs::enable(id = "step3-skipButton")
                  })
      }else if(step3return$isSkip() == TRUE ){
        r$currentLang <- step3return$currentLang()

        #Skipping produces no threshold, so it supersedes nothing and never
        #raises the modal: whatever areas of interest exist are kept and step 4
        #opens on them, which is what "skip" has always meant. Through
        #vftCommit() anyway, so that the rule "a step's results are written in
        #one place, by one function" holds for every exit from every step.
        vftCommit(r, list(isSkip = step3return$isSkip()), session, step = "step3",
                  then = function() vftGoToStep(r, "step4", session))
        #autosave dropped here, on leaving step 3 via the skip path. downloadSave
        #materialises the raster and save()s the whole session state on the
        #shared main thread, and it used to fire at all eight step transitions.
        #It now runs only at the three checkpoints where losing work is
        #expensive: the sensitivity matrix, the confirmed network + parking,
        #and the finished simulation. Restore with:
        #  shinyjs::click("downloadSave", asis = FALSE)
      }else{
        #a banner letter, meaning "go back to an earlier step". See
        #vftGoBack() in R/navigation.R.
        vftGoBack(r, step3return$confirm(), from = "step3",
                  bannerId = "step3-banner", session = session)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(step3return$skip(), {
      if(step3return$isSkip() > 0 ){
        r$currentLang <- step3return$currentLang()

        #the skip BUTTON, as opposed to the confirm handler's skip branch above.
        #Same write, same reasoning: nothing is superseded by skipping.
        vftCommit(r, list(isSkip = step3return$isSkip()), session, step = "step3",
                  then = function() vftGoToStep(r, "step4", session))
        #autosave dropped here, on leaving step 3 via the skip button. downloadSave
        #materialises the raster and save()s the whole session state on the
        #shared main thread, and it used to fire at all eight step transitions.
        #It now runs only at the three checkpoints where losing work is
        #expensive: the sensitivity matrix, the confirmed network + parking,
        #and the finished simulation. Restore with:
        #  shinyjs::click("downloadSave", asis = FALSE)
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
                                network       = shiny::reactive(r$network),
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
        r$currentLang <- step4return$currentLang()

        #CREATE NETWORK LIST ####
        #Package together all aspects that can be altered and results (network,
        #usage, parking, attractivity..). This list IS the simulation's input, so
        #building a new one is what ends the old simulations - which is why it
        #goes through vftCommit() with everything else step 4 confirms rather
        #than being assigned here. If the user has simulations or saved versions,
        #this is the write that asks them first.
        #
        #The network in it is the RAW one the step-1 provider derived, and
        #`parking` is NULL. Both used to be step 4's own output, from the ~30s
        #job that has moved to R/prepare_network.R - it now runs when the first
        #simulation is launched, which is where the first read of either happens.
        #Everything between here and there works on the raw network: the scenario
        #cards are labelled buttons, the map before a simulation is a static
        #placeholder, and every display checkbox that would want the prepared
        #columns is disabled until a pathUsage exists.
        newList <- list(list(network = r$network, pathUsage = NULL,
                             parking = NULL, residential = NULL,
                             newAttr = NULL))

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

                    #activate download. vftGoToStep sets r$step, which
                    #downloadSave reads to name the file, so it has to come
                    #before the click.
                    vftGoToStep(r, "step5", session)
                    shinyjs::click("downloadSave", asis = FALSE)

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

      r$currentLang <- step5return$currentLang()

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
    #autosave checkpoint. Where the finished simulation gets written is the save
    #rewrite's to decide; do not re-add a click here without deciding it.
    shiny::observeEvent(step5return$confirm(), {
      #a banner letter, meaning "go back to an earlier step". See
      #vftGoBack() in R/navigation.R.
      vftGoBack(r, step5return$confirm(), from = "step5",
                bannerId = "step5-banner", session = session)
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
                                            selectedVersion = shiny::reactive(r$selectedVersion))

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

        #activate download
        #autosave dropped here, on returning from newVersions. downloadSave
        #materialises the raster and save()s the whole session state on the
        #shared main thread, and it used to fire at all eight step transitions.
        #It now runs only at the three checkpoints where losing work is
        #expensive: the sensitivity matrix, the confirmed network + parking,
        #and the finished simulation. Restore with:
        #  shinyjs::click("downloadSave", asis = FALSE)

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



}
