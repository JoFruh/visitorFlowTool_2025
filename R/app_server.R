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
    "Shiny.setInputValue('step5-banner', 'O');",
    "Shiny.setInputValue('lastStep-banner', 'O');"
  ))

  #one visit counter per step, in session$userData. Everything that moves the
  #user between steps goes through vftGoToStep() in R/navigation.R, which is
  #also the only caller of updateTabsetPanel() in the app.
  vftNavInit(session)

  #the step nav bar: which steps are reachable, and the click handlers that
  #reach them. Registers nothing unless VFT_NAV=1. The step registry it gates on
  #(which r$ keys each step needs) is in R/steps.R.
  vftNavBarServer(r, input, session)

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
      stepName <- switch(r$step,
        "2" = "2_SM",
        "3" = "ignore",
        "4" = "3_schnell",
        "5" = "4_genau",
        "6" = "5_simulation",
        "7" = "finalStep",
        "8" = "finalStep+"
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
      if(is.null(r$SM_pres_packed)){
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
  #vftGoToStep(r, "step1", session).
  shiny::observeEvent(vftStepTrigger(session, "step1"), {
    vftDbg("BUILD STEP 1")
    step1return <- vftTime("module:step1", step1_server("step1", i18n = shiny::reactive(i18n)))#, lang = reactive(input$lang_pick)


    #REACTIVES

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
        r$shape <- step1return$ffshape()
        r$network <- step1return$network()
        r$needHelp <- step1return$needHelp()
        r$DULN <- step1return$DULN()
        r$DULN_all <- step1return$DULN_all()
        r$currentLang <- step1return$currentLang()
        # r$parking <- step1return$parking()
        #activate download
        #autosave dropped here, on leaving step 1 (shape + path network). downloadSave
        #materialises the raster and save()s the whole session state on the
        #shared main thread, and it used to fire at all eight step transitions.
        #It now runs only at the three checkpoints where losing work is
        #expensive: the sensitivity matrix, the confirmed network + parking,
        #and the finished simulation. Restore with:
        #  shinyjs::click("downloadSave", asis = FALSE)
        vftGoToStep(r, "step2", session)

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


        #get r$DULN — use global session cache (populated by sf_to_tidygraph or on first restore)
        if (!exists(".vft_DULN_full", envir = .GlobalEnv)) {
          .GlobalEnv$.vft_DULN_full     <- terra::rast(vftData("maps/attr/allAttrs_COG_final.tif"))
          .GlobalEnv$.vft_DULN_all_full <- terra::rast(vftData("maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif"))
        }
        r$DULN <- terra::crop(.GlobalEnv$.vft_DULN_full, terra::project(terra::vect(r$shape), "EPSG:4326"))
        names(r$DULN) <- c("jog", "dogNat", "ebikeNat", "walkNat","dogProx","walkSoc","bikerSport")
        r$DULN_all <- terra::crop(.GlobalEnv$.vft_DULN_all_full, sf::st_transform(r$shape, "epsg:4326"))

        r$currentLang <- step1return$currentLang()


vftDbgCat(paste0("DULN ALL: ", r$DULN_all))

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

        #trigger the correct step. r$step == 1 used to bump a reactiveVal that
        #nothing observed; it routes for free now that every step is reached the
        #same way. There is still no branch for r$step == 6 (a save taken after
        #the simulation) - that is Stage 6's job, along with this ladder.
        if(r$step == 1){
          vftGoToStep(r, "step1", session)
        }else if(r$step == 2){
          vftGoToStep(r, "step2", session)
        }else if(r$step == 3){
          vftGoToStep(r, "step3", session)
        }else if(r$step == 4){
          vftGoToStep(r, "step4", session)
        }else if(r$step == 5){
          vftGoToStep(r, "step5", session)
        }
      }
    }, ignoreInit = TRUE, once = TRUE)
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  #
  #
  #STEP 2:
  #determine SDM selection, generate sensitivity matrix

  #When confirmation button clicked
  shiny::observeEvent(vftStepTrigger(session, "step2"), {
    vftDbg("BUILD STEP 2")
    #use shape information to clip, prepare and present SDM information
    step2return <- vftTime("module:step2", step2_server("step2", fshape = r$shape,
                                needHelp = r$needHelp, filterList = r$filterList, checkboxSave = r$checkboxSave,
                                i18n = shiny::reactive(i18n), currentLang = r$currentLang))


    shiny::observeEvent(step2return$confirm(), {

      vftDbg("REACTIVE::: STEP2RETURN$CONFIRM")
      vftDbg(step2return$confirm())

      vftDbgCat("STEP 3 STARTED")
      #button indirectly triggers step 2, to allow for program intervention
      if(is.integer(step2return$confirm()) & step2return$confirm() > 0){

        # shinyjs::runjs("Shiny.onInputChange('step2-confirmButton2', 0);")
        shinyjs::reset(id = "step2-confirmButton2")

        vftDbg("PRE-TRIGGER STEP 2")
        #save returns
        r$toSelectSpAfter <- step2return$toSelectSpAfter()
        #save chosen sensitivity matrix

        #
        r$SM_pres <- step2return$SM_pres()
        #new sensitivity raster from step 2: invalidate the packed copy so the next
        #checkpoint repacks it once (then reuses it for later checkpoints)
        r$SM_pres_packed <- NULL
        # r$SM_noPres <- step2return$SM_noPres()
        r$SMcolors <- step2return$SMcolors()
        r$minCutThresh <- step2return$minCutThresh()

        vftDbgCat("STEP 3_2")

        r$needHelp <- step2return$needHelp()
        r$groupSave_all <- step2return$groupSave_all()
        r$groupSave_sens <- step2return$groupSave_sens()
        r$groupSave_type <- step2return$groupSave_type()
        r$groupSave_class <- step2return$groupSave_class()
        r$checkboxSave <- step2return$checkboxSave()
        r$filterList <- step2return$filterList()
        r$weightInputs <- step2return$weightInputs()
        r$weightNames <- step2return$weightNames()
        r$species <- step2return$species()

        r$currentLang <- step2return$currentLang()


        vftDbgCat("STEP 3_3")


        #activate download. vftGoToStep sets r$step, which downloadSave reads to
        #name the file, so it has to come before the click.
        vftGoToStep(r, "step3", session)
        shinyjs::click("downloadSave", asis = FALSE)
        # shinyjs::click("downloadSaveRaster", asis = FALSE)

        vftDbg(input$`step2-confirmButton2`)
        vftDbgCat("STEP 3_3")


      }else{
        #a banner letter, meaning "go back to an earlier step"; anything else is
        #left alone. See vftGoBack() in R/navigation.R.
        vftGoBack(r, step2return$confirm(), from = "step2",
                  bannerId = "step2-banner", session = session)
      }
    },ignoreInit = TRUE, once = TRUE)
  })

  #STEP 3:
  #determine Areas of Interest
  shiny::observeEvent(vftStepTrigger(session, "step3"), {
    vftDbg("BUILD STEP 3")
    vftDbg(r$toSelectSpAfter)
    vftDbg(r$shape)

    #use shape information to clip, prepare and present SDM information
    vftDbgCat(paste0("DULN ALL 2: ", r$DULN_all))

    step3return <- vftTime("module:step3", step3_server("step3", shape = r$shape,
                                i18n = shiny::reactive(i18n), currentLang = r$currentLang,
                                needHelp = r$needHelp, DULN_all = r$DULN_all
                                ))


    shiny::observeEvent(step3return$confirm(), {
      vftDbg("REACTIVE::: STEP4RETURN$CONFIRM")
      #button indirectly triggers step 2, to allow for program intervention
      if(is.integer(step3return$confirm()) & step3return$confirm() > 0 & step3return$isSkip() == 0 ){
        vftDbg("PRE-TRIGGER STEP 4")
        #reset confirm button (even if button is destroyed, input stays in memory)
        shinyjs::runjs("Shiny.onInputChange('step3-confirmButton3', 0);")
        # shinyjs::reset(id = "step3-confirmButton3", asis = TRUE)

        #save returns
        r$minThresh <- step3return$minThresh()
        r$isSkip <- step3return$isSkip()
        # r$DULN <- step3return$DULN()
        # r$DULN_all <- step3return$DULN_all()
        r$needHelp <- step3return$needHelp()
        r$currentLang <- step3return$currentLang()

        #activate download
        #autosave dropped here, on leaving step 3 (threshold choice). downloadSave
        #materialises the raster and save()s the whole session state on the
        #shared main thread, and it used to fire at all eight step transitions.
        #It now runs only at the three checkpoints where losing work is
        #expensive: the sensitivity matrix, the confirmed network + parking,
        #and the finished simulation. Restore with:
        #  shinyjs::click("downloadSave", asis = FALSE)

        vftDbg(input$`step3-confirmButton3`)

        vftGoToStep(r, "step4", session)
      }else if(step3return$isSkip() == TRUE ){
        r$currentLang <- step3return$currentLang()

        #activate download
        r$isSkip <- step3return$isSkip()
        #autosave dropped here, on leaving step 3 via the skip path. downloadSave
        #materialises the raster and save()s the whole session state on the
        #shared main thread, and it used to fire at all eight step transitions.
        #It now runs only at the three checkpoints where losing work is
        #expensive: the sensitivity matrix, the confirmed network + parking,
        #and the finished simulation. Restore with:
        #  shinyjs::click("downloadSave", asis = FALSE)

        vftGoToStep(r, "step4", session)
      }else{
        #a banner letter, meaning "go back to an earlier step". See
        #vftGoBack() in R/navigation.R.
        vftGoBack(r, step3return$confirm(), from = "step3",
                  bannerId = "step3-banner", session = session)
      }
    }, ignoreInit = TRUE, once = TRUE)

    shiny::observeEvent(step3return$skip(), {
      if(step3return$isSkip() > 0 ){
        r$currentLang <- step3return$currentLang()

        #activate download
        r$isSkip <- step3return$isSkip()
        #autosave dropped here, on leaving step 3 via the skip button. downloadSave
        #materialises the raster and save()s the whole session state on the
        #shared main thread, and it used to fire at all eight step transitions.
        #It now runs only at the three checkpoints where losing work is
        #expensive: the sensitivity matrix, the confirmed network + parking,
        #and the finished simulation. Restore with:
        #  shinyjs::click("downloadSave", asis = FALSE)

        vftGoToStep(r, "step4", session)
      }
    }, ignoreInit = TRUE, once = TRUE)


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
    step4return <- vftTime("module:step4", step4_server("step4", network = r$network, minThresh = r$minThresh, skip = r$isSkip,
                                DULN = r$DULN, DULN_all = r$DULN_all, needHelp = r$needHelp,
                                i18n = shiny::reactive(i18n), currentLang = r$currentLang, shape = r$shape))



    shiny::observeEvent( step4return$confirm() , {
      vftDbg("REACTIVE::: STEP5RETURN$CONFIRM")
      #button indirectly triggers step 2, to allow for program intervention
      if(is.integer(step4return$confirm()) & step4return$confirm() > 0){
        # shinyjs::runjs("Shiny.onInputChange('step4-confirmButton4', 0);")
        # shinyjs::reset(id = "step4-confirmButton4", asis = TRUE)
        shinyjs::runjs("Shiny.onInputChange('step4-confirmButton4', 0);")

        vftDbg("PRE-TRIGGER STEP 5")


        # shinyjs::reset
        #save returns
        r$finalPolygons <- step4return$finalPolygons()
        r$network <- step4return$network()
        r$needHelp <- step4return$needHelp()
        r$parking <- step4return$parking()
        r$currentLang <- step4return$currentLang()


        #CREATE NETWORK LIST ####
        #Package together all aspects that can be altered and results (network, usage, parking, attractivity..)
        r$networkList <- list(list(network = r$network, pathUsage = NULL, parking = r$parking, residential = NULL, newAttr = NULL))

        r$step6FirstRun <- TRUE
        r$newVersionsFirstRun <- TRUE

        #activate download. vftGoToStep sets r$step, which downloadSave reads to
        #name the file, so it has to come before the click.
        vftGoToStep(r, "step5", session)
        shinyjs::click("downloadSave", asis = FALSE)

        r$triggerStep5_nr <- 1
      }else{
        #a banner letter, meaning "go back to an earlier step". See
        #vftGoBack() in R/navigation.R.
        vftGoBack(r, step4return$confirm(), from = "step4",
                  bannerId = "step4-banner", session = session)
      }
    }, ignoreInit = TRUE, once = TRUE)
  })

  #STEP 5

  shiny::observeEvent(vftStepTrigger(session, "step5"), {
    vftDbg("BUILD STEP 5")
    # cat(file = stderr(), paste0("contents of envBase: ", ls(envBase)))
    step5return <- vftTime("module:step5", step5_server("step5", networkList = r$networkList, SM_pres = r$SM_pres, SMcolors = r$SMcolors, shape = r$shape, finalPolygons = r$finalPolygons, versionsUI = r$versionsUI, isFirstRun_stp6 = r$step6FirstRun,
                                needHelp = r$needHelp, species = r$species,
                                i18n = shiny::reactive(i18n), currentLang = r$currentLang, minCutThresh = r$minCutThresh))
    r$step6FirstRun <- FALSE

    #From step 5, go to New Versions
    shiny::observeEvent(step5return$newVersions(), {

      vftDbg("From step 5, go to New Versions")
      r$triggerStep5_nr <- step5return$trigger()
      #save returns (but only if larger than null: Avoid overwriting with default empty returns)
      if(length(step5return$networkList()) > 0 ){
        r$networkList <- step5return$networkList()
      }
      if(length(step5return$versionsUI()) > 0 ){
        r$versionsUI <- step5return$versionsUI()
      }
      r$shp_PA <- step5return$shp_PA()


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

        vftGoToStep(r, "newVersions", session)

        #guards the step5 <-> newVersions handlers against firing each other;
        #nothing to do with navigation, which is vftGoToStep()'s job now.
        r$triggerStep5_nr <- 0

      }
    }, ignoreInit = TRUE, once = TRUE)



    #From step5, go to Last Step
    shiny::observeEvent(step5return$confirm(), {
      #button indirectly triggers step 5, to allow for program intervention
      if(is.integer(step5return$confirm()) & step5return$confirm() > 0){
        # shinyjs::runjs("Shiny.onInputChange('step5-confirmButton5', 0);")
        shinyjs::reset(id = "step5-confirmButton5")

        #Check if simulations need to be run,
        #if so, run all simulations before going to last step

        #calculate time for 1 simulation, assume time for X simulations,
        #show over current timeframe

        #CONFIRM SIMULATIONS
        #save returns
        r$pathUsage <- step5return$pathUsage()
        r$versionsUI <- step5return$versionsUI()
        r$networkList <- step5return$networkList()
        r$shp_PA <- step5return$shp_PA()
        #vftGoToStep sets r$step, which downloadSave reads to name the file, so
        #it has to come before the click.
        vftGoToStep(r, "finalStep", session)
        shinyjs::click("downloadSave", asis = FALSE)

      }else{
        #a banner letter, meaning "go back to an earlier step". See
        #vftGoBack() in R/navigation.R.
        vftGoBack(r, step5return$confirm(), from = "step5",
                  bannerId = "step5-banner", session = session)
      }
    }, ignoreInit = TRUE, once = TRUE)

  })


  #NEW VERSIONS PAGE
  shiny::observeEvent(vftStepTrigger(session, "newVersions"), {
    vftDbg("BUILD NEW VERSIONS")
    newVersionsReturn <- newVersions_server("newVersions", networkList = r$networkList, SM_pres = r$SM_pres,  SMcolors =  r$SMcolors, shp_PA = r$shp_PA,
                                            finalPolygons = r$finalPolygons, versionsUI = r$versionsUI, isFirstRun = r$newVersionsFirstRun,
                                            DULN = r$DULN,
                                            #the step-1 perimeter, for cropping the land cover under the
                                            #paint. step5_server and lastStep_server already take it this way
                                            shape = r$shape,
                                            i18n = shiny::reactive(i18n), currentLang = r$currentLang)

    r$newVersionsFirstRun <- FALSE

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

      vftDbg("From new versions, return to step 5")
      vftDbgCat("TESTF")
      vftDbgCat(paste0("newVersionsReturn$confirm() : ", newVersionsReturn$confirm()))
      vftDbgCat(paste0("r$triggerNewVersions_nr : ", r$triggerNewVersions_nr))

      #button indirectly triggers step 5, to allow for program intervention
      if(newVersionsReturn$confirm() > 0 & r$triggerNewVersions_nr == 1 ){
        vftDbg("PRE-TRIGGER STEP 5 return")
        vftDbgCat("TESTFb")

        #save returns (but only if non NULL, to avoid overwriting with default)
        # if(length(newVersionsReturn$networkList() ) > 0){
        #   networkList <<- newVersionsReturn$networkList()
        # }

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
    }, ignoreInit = TRUE, once = TRUE)


  }, ignoreInit = TRUE)


  #GO TO FINAL STEP ####


  shiny::observeEvent(vftStepTrigger(session, "finalStep"), {
    vftDbg("BUILD FINAL STEP")
    finalStepReturn <- lastStep_server("finalStep", networkList = r$networkList, versionsUI = r$versionsUI,
                                       SM_pres = r$SM_pres, shape = r$shape,
                                       finalPolygons = r$finalPolygons)


    shiny::observeEvent( finalStepReturn$confirm() , {
      vftDbg("REACTIVE::: STEP5RETURN$CONFIRM")
      #button indirectly triggers step 2, to allow for program intervention
      if(is.integer(finalStepReturn$confirm()) & finalStepReturn$confirm() > 0){
        #the last step has nowhere forward to go; its confirm button does nothing.

      }else{
        #a banner letter, meaning "go back to an earlier step". See
        #vftGoBack() in R/navigation.R.
        vftGoBack(r, finalStepReturn$confirm(), from = "finalStep",
                  bannerId = "lastStep-banner", session = session)
      }
    }, ignoreInit = TRUE, once = TRUE)
  })


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
