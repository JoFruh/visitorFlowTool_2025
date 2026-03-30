#' server function for visitorFlowTool app
app_server <- function(input, output, session){


  #prepare multilingual functions
  i18n <- shiny.i18n::Translator$new(translation_csvs_path = "www/data/tables", separator_csv = ";" )
  i18n$set_translation_language('de')

  r <- shiny::reactiveValues()
  #shortcut for accessing tool's various steps instantly
  step <- 1

  #give banners an original value
  shinyjs::runjs("Shiny.setInputValue('step3-banner', 'O')")
  shinyjs::runjs("Shiny.setInputValue('step4-banner', 'O')")
  shinyjs::runjs("Shiny.setInputValue('step5-banner', 'O')")
  shinyjs::runjs("Shiny.setInputValue('step6-banner', 'O')")
  shinyjs::runjs("Shiny.setInputValue('lastStep-banner', 'O')")

  #initialise triggers
  restartSteps <- shiny::reactiveVal()
  triggerStep1 <- shiny::reactiveVal()
  triggerStep2 <- shiny::reactiveVal()
  triggerStep3 <- shiny::reactiveVal()
  triggerStep4 <- shiny::reactiveVal()
  triggerStep5 <- shiny::reactiveVal()
  triggerStep6 <- shiny::reactiveVal()
  triggerFinalStep <- shiny::reactiveVal()
  triggerNewVersions <- shiny::reactiveVal()

  #accompanying non reactive (nr) variables, to allow for back and forth access between step6 and newVersions
  r$triggerNewVersions_nr <- 1
  r$triggerStep6_nr <- 1

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
        "3" = "1_bereich",
        "4" = "ignore",
        "5" = "2_SM",
        "6" = "3_schnell",
        "7" = "4_genau",
        "8" = "5_simulation",
        "9" = "finalStep"
      )
#"2_SM","4_ZG_genau"

      return(paste0("visitorFlowSave_step",stepName, "_", dateTime, ".RData"))
    },
    content = function(file){
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
      envBase_basemap <- r$basemap
      # envBase_SMdateTime <- r$SMdateTime #not needed anymore?
      envBase_groupSave_all <- r$groupSave_all
      envBase_groupSave_sens <- r$groupSave_sens
      envBase_groupSave_type <- r$groupSave_type
      envBase_groupSave_class <- r$groupSave_class

      envBase_checkboxSave <- r$checkboxSave
      envBase_filterList <- r$filterList
      # envBase_weightInputs <- r$weightInputs

      #save spatRaster as a data.frame with geo ref information
      envBase_SM_pres <- terra::as.data.frame(r$SM_pres, xy = TRUE, na.rm = FALSE)

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
                  envBase_basemap,
                  envBase_groupSave_all ,
                  envBase_groupSave_sens,
                  envBase_groupSave_type,
                  envBase_groupSave_class,
                  envBase_checkboxSave,
                  envBase_filterList,
                  envBase_weightInputs,
                  envBase_weightNames,
                  envBase_needHelp,
                  envBase_finalPolygons,
                  envBase_species,
                  envBase_minCutThresh, file = file)) #envBase_SMdateTime,
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

  shiny::observeEvent(restartSteps(), {
    print("RE-TRIGGER STEP 1")
    if(!is.null(restartSteps() ) ){
      # r$step1Refreshing <- TRUE
      shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step1" )
    }
    step1return <- step1_server("step1", i18n = shiny::reactive(i18n))#, lang = reactive(input$lang_pick)


    #REACTIVES

    shiny::observeEvent( step1return$confirm(), {
      # isolate({
      #   shinyjs::runjs("Shiny.onInputChange('step1-confirmButton1', 0);")
      #   shinyjs::runjs("Shiny.onInputChange('step1-confirmButton2', 0);")
      # }
      # )
      shinyjs::reset(id = "step1-confirmButton1")
      shinyjs::reset(id = "step1-confirmButton2")


      print("REACTIVE::: STEP1$CONFIRM")
      #button indirectly triggers step 2, to allow for program intervention
      if(step1return$confirm() == 1){


        print("PRE-TRIGGER STEP 2")
        r$basemap <- step1return$basemap()
        r$basemap_bw <- step1return$basemap_bw()
        r$shape <- step1return$ffshape()
        r$network <- step1return$network()
        r$needHelp <- step1return$needHelp()
        r$DULN <- step1return$DULN()
        r$DULN_all <- step1return$DULN_all()
        r$currentLang <- step1return$currentLang()
        # r$parking <- step1return$parking()
        #activate download
        r$step <- 3
        shinyjs::click("downloadSave", asis = FALSE)
        #change reactiveVal
        if(is.null(triggerStep3()) ){
          triggerStep3(1)
        }else{
          triggerStep3(triggerStep3()+1)
        }

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
        if(exists("envBase_basemap")){r$basemap <- envBase_basemap}
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
          .GlobalEnv$.vft_DULN_full     <- terra::rast("www/data/maps/attr/allAttrs_COG_final.tif")
          .GlobalEnv$.vft_DULN_all_full <- terra::rast("www/data/maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif")
        }
        r$DULN <- terra::crop(.GlobalEnv$.vft_DULN_full, terra::project(terra::vect(r$shape), "EPSG:4326"))
        names(r$DULN) <- c("jog", "dogNat", "ebikeNat", "walkNat","dogProx","walkSoc","bikerSport")
        r$DULN_all <- terra::crop(.GlobalEnv$.vft_DULN_all_full, sf::st_transform(r$shape, "epsg:4326"))

        r$currentLang <- step1return$currentLang()


cat(file = stderr(), paste0("DULN ALL: ", r$DULN_all))

        #load saved sensitivity matrix (converted back from dataframe)
        if(exists("envBase_SM_pres")){
          if(length(envBase_SM_pres > 0)){
            r$SM_pres <- terra::rast(envBase_SM_pres)
            terra::crs(r$SM_pres) <- "epsg:4326"
          }
        }

        #trigger the correct step
        if(r$step == 1){
          triggerStep1(1)
        }else if(r$step == 2){
          triggerStep2(1)
        }else if(r$step == 3){
          triggerStep3(1)
        }else if(r$step == 4){
          triggerStep4(1)
        }else if(r$step == 5){
          triggerStep5(1)
        }else if(r$step == 6){
          triggerStep6(1)
        }
      }
    }, once = TRUE)
  }, ignoreInit = FALSE, ignoreNULL = FALSE)
  #
  #
  #STEP 3:
  #determine SDM selection, generate sensitivity matrix

  #When confirmation button clicked
  shiny::observeEvent(triggerStep3(), {
    print("TRIGGERSTEP3()")
    if(triggerStep3() > 0){

      #change tabs
      #use shape information to clip, prepare and present SDM information
      step3return <- step3_server("step3", fshape = r$shape, confirm = step1return$confirm(),
                                  needHelp = r$needHelp, filterList = r$filterList, checkboxSave = r$checkboxSave,
                                  i18n = shiny::reactive(i18n), currentLang = r$currentLang)
      #Update UI
      shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step3" )
    }


    shiny::observeEvent(step3return$confirm(), {

      print("REACTIVE::: STEP2RETURN$CONFIRM")
      print(step3return$confirm())

      cat(file = stderr(), "STEP 4 STARTED")
      #button indirectly triggers step 2, to allow for program intervention
      if(is.integer(step3return$confirm()) & step3return$confirm() > 0){

        # shinyjs::runjs("Shiny.onInputChange('step3-confirmButton3', 0);")
        shinyjs::reset(id = "step3-confirmButton3")

        print("PRE-TRIGGER STEP 3")
        #save returns
        r$toSelectSpAfter <- step3return$toSelectSpAfter()
        #save chosen sensitivity matrix

        #
        r$SM_pres <- step3return$SM_pres()
        # r$SM_noPres <- step3return$SM_noPres()
        r$SMcolors <- step3return$SMcolors()
        r$minCutThresh <- step3return$minCutThresh()

        cat(file = stderr(), "STEP 4_2")

        r$needHelp <- step3return$needHelp()
        r$groupSave_all <- step3return$groupSave_all()
        r$groupSave_sens <- step3return$groupSave_sens()
        r$groupSave_type <- step3return$groupSave_type()
        r$groupSave_class <- step3return$groupSave_class()
        r$checkboxSave <- step3return$checkboxSave()
        r$filterList <- step3return$filterList()
        r$weightInputs <- step3return$weightInputs()
        r$weightNames <- step3return$weightNames()
        r$species <- step3return$species()

        r$currentLang <- step3return$currentLang()


        cat(file = stderr(), "STEP 4_3")


        #activate download
        r$step <- 4
        shinyjs::click("downloadSave", asis = FALSE)
        # shinyjs::click("downloadSaveRaster", asis = FALSE)

        print(input$`step3-confirmButton3`)
        #change reactiveVal
        if(is.null(triggerStep4()) ){
          triggerStep4(1)
        }else{
          triggerStep4(triggerStep4()+1)
        }

        cat(file = stderr(), "STEP 4_3")


      }else if(step3return$confirm() == "A"){
        #send app to step 1
        r$step <- 1
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # reset banner value
        shinyjs::runjs("Shiny.onInputChange('step3-banner', 'O');")

        #Update UI (go to step 1)
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step1" )
        # shinyjs::reset(id = "step3-banner", asis = TRUE)
        print("RESTART STEP 1")
        if(is.null(restartSteps())){
          restartSteps(1)
        }else{
          restartSteps(restartSteps() + 1)
        }
      }
    },ignoreInit = TRUE, once = TRUE)
  })

  #STEP 4:
  #determine Areas of Interest
  shiny::observeEvent(triggerStep4(), {
    print("TRIGGERSTEP4()")
    print(r$toSelectSpAfter)
    print(r$network)
    print(r$shape)

    cat(file = stderr(), "STEP 4_4")

    if(triggerStep4() > 0){
      #change tabs

      # cat(file = stderr(), paste0("DULN_all: ", r$DULN_all) )

      cat(file = stderr(), "STEP 4_5")

      #use shape information to clip, prepare and present SDM information
      cat(file = stderr(), paste0("DULN ALL 2: ", r$DULN_all))

      step4return <- step4_server("step4", network = r$network, shape = r$shape, confirm = r$confirm,
                                  i18n = shiny::reactive(i18n), currentLang = r$currentLang,
                                  needHelp = r$needHelp, DULN_all = r$DULN_all
                                  )
      #Update UI
      shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step4" )

      cat(file = stderr(), "STEP 4_6")

    }


    shiny::observeEvent(step4return$confirm(), {
      print("REACTIVE::: STEP4RETURN$CONFIRM")
      #button indirectly triggers step 2, to allow for program intervention
      if(is.integer(step4return$confirm()) & step4return$confirm() > 0 & step4return$isSkip() == 0 ){
        print("PRE-TRIGGER STEP 5")
        #reset confirm button (even if button is destroyed, input stays in memory)
        shinyjs::runjs("Shiny.onInputChange('step4-confirmButton4', 0);")
        # shinyjs::reset(id = "step4-confirmButton4", asis = TRUE)

        #save returns
        r$minThresh <- step4return$minThresh()
        r$naturalAreas <- step4return$naturalAreas()
        r$isSkip <- step4return$isSkip()
        # r$DULN <- step4return$DULN()
        # r$DULN_all <- step4return$DULN_all()
        r$needHelp <- step4return$needHelp()
        r$currentLang <- step4return$currentLang()

        #change reactiveVal

        #activate download
        r$step <- 5
        shinyjs::click("downloadSave", asis = FALSE)

        print(input$`step4-confirmButton4`)

        #change reactiveVal
        if(is.null(triggerStep5()) ){
          triggerStep5(1)
        }else{
          triggerStep5(triggerStep5()+1)
        }
      }else if(step4return$confirm() == "A"){
        r$step <- 1
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        shinyjs::runjs("Shiny.onInputChange('step4-banner', 'O');")

        # shinyjs::reset()
        #Update UI
        print("RESTART STEP 1")
        if(is.null(restartSteps())){
          restartSteps(1)
        }else{
          restartSteps(restartSteps() + 1)
        }
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step1" )
        #
        # polygonCreator("areaSelectMap",  input = input, startingPolygons = NULL, numberOfPolygons = "single") #requires "polygons" global variable
        # polygonEraser("areaSelectMap", input = input, startingPolygons = NULL, numberOfPolygons = "single")

        # restartSteps(restartSteps() + 1)
      }else if(step4return$confirm() == "B"){
        r$step <- 3
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('step4-banner', 'O');")

        #Update UI
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step3" )
        if(is.null(triggerStep3())){
          triggerStep3(1)
        }else{
          triggerStep3(triggerStep3() + 1)
        }
        # restartSteps(restartSteps() + 1)
      }else if(step4return$isSkip() == TRUE ){
        r$currentLang <- step4return$currentLang()

        #activate download
        r$isSkip <- step4return$isSkip()
        r$step <- 5
        shinyjs::click("downloadSave", asis = FALSE)

        triggerStep5(-1)
      }
    }, once = TRUE)

    shiny::observeEvent(step4return$skip(), {
      if(step4return$isSkip() > 0 ){
        r$currentLang <- step4return$currentLang()

        #activate download
        r$isSkip <- step4return$isSkip()
        r$step <- 5
        shinyjs::click("downloadSave", asis = FALSE)

        triggerStep5(-1)
      }
    }, once = TRUE)


  })


  #STEP 5

  shiny::observeEvent(triggerStep5(), {
    print("TRIGGERSTEP5()")
    if(triggerStep5() > 0){
      # r$isSkip <- 0
      #Skip polygon generation = FALSE
      step5return <- step5_server("step5", network = r$network, minThresh = r$minThresh, naturalAreas = r$naturalAreas, confirm = r$confirm, skip = r$isSkip,
                                  DULN = r$DULN, DULN_all = r$DULN_all, needHelp = r$needHelp,
                                  i18n = shiny::reactive(i18n), currentLang = r$currentLang, shape = r$shape)
      #Update UI
      shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step5" )
    }else if (triggerStep5() == -1){
      #Skip polygon generation = TRUE
      #change tabs

      # r$isSkip <- 1
      step5return <- step5_server("step5", network = r$network, minThresh = r$minThresh, naturalAreas = r$naturalAreas, confirm = r$confirm, skip = r$isSkip,
                                  DULN = r$DULN, DULN_all = r$DULN_all, needHelp = r$needHelp,
                                  i18n = shiny::reactive(i18n), currentLang = r$currentLang)
      #Update UI
      shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step5" )
    }



    shiny::observeEvent( step5return$confirm() , {
      print("REACTIVE::: STEP5RETURN$CONFIRM")
      #button indirectly triggers step 2, to allow for program intervention
      if(is.integer(step5return$confirm()) & step5return$confirm() > 0){
        # shinyjs::runjs("Shiny.onInputChange('step5-confirmButton5', 0);")
        # shinyjs::reset(id = "step5-confirmButton5", asis = TRUE)
        shinyjs::runjs("Shiny.onInputChange('step5-confirmButton5', 0);")

        print("PRE-TRIGGER STEP 6")


        # shinyjs::reset
        #save returns
        r$finalPolygons <- step5return$finalPolygons()
        r$network <- step5return$network()
        r$needHelp <- step5return$needHelp()
        r$parking <- step5return$parking()
        r$currentLang <- step5return$currentLang()


        #CREATE NETWORK LIST ####
        #Package together all aspects that can be altered and results (network, usage, parking, attractivity..)
        r$networkList <- list(list(network = r$network, pathUsage = NULL, parking = r$parking, residential = NULL, newAttr = NULL))

        r$step6FirstRun <- TRUE
        r$newVersionsFirstRun <- TRUE

        #activate download
        r$step <- 6
        shinyjs::click("downloadSave", asis = FALSE)

        #change reactiveVal
        #change reactiveVal
        if(is.null(triggerStep6()) ){
          triggerStep6(1)
        }else{
          triggerStep6(triggerStep6()+1)
        }
        r$triggerStep6_nr <- 1
      }else if(step5return$confirm() == "A"){
        r$step <- 1
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('step5-banner', 'O');")

        #Update UI
        print("RESTART STEP 1")
        if(is.null(restartSteps())){
          restartSteps(1)
        }else{
          restartSteps(restartSteps() + 1)
        }
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step1" )

        # restartSteps(restartSteps() + 1)
      }else if(step5return$confirm() == "B"){
        r$step <- 3
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('step5-banner', 'O');")

        #Update UI
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step3" )
        if(is.null(triggerStep3())){
          triggerStep3(1)
        }else{
          triggerStep3(triggerStep3() + 1)
        }

        # restartSteps(restartSteps() + 1)
      }else if(step5return$confirm() == "C"){
        r$step <- 4
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('step5-banner', 'O');")

        #Update UI
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step4" )
        print("RESTART STEP 4")
        if(is.null(triggerStep4())){
          triggerStep4(1)
        }else{
          triggerStep4(triggerStep4() + 1)
        }


        # restartSteps(restartSteps() + 1)
      }
    }, once = TRUE)
  })

  #STEP 6

  shiny::observeEvent(triggerStep6(), {
    print("TRIGGERSTEP6()")
    if(triggerStep6() > 0 ){
      cat(file = stderr(), "TEST7")
      print("triggerStep6() > 0 ")
      #Update UI
      shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step6" )
      #change tabs
      cat(file = stderr(), "TEST8\n")
      # cat(file = stderr(), paste0("contents of envBase: ", ls(envBase)))
      step6return <- step6_server("step6", networkList = r$networkList, SM_pres = r$SM_pres, SMcolors = r$SMcolors, shape = r$shape, confirm = r$confirm, finalPolygons = r$finalPolygons, versionsUI = r$versionsUI, isFirstRun_stp6 = r$step6FirstRun,
                                  needHelp = r$needHelp, basemap = r$basemap, species = r$species,
                                  i18n = shiny::reactive(i18n), currentLang = r$currentLang, minCutThresh = r$minCutThresh)
      r$step6FirstRun <- FALSE
    }

    #From step 6, go to New Versions
    shiny::observeEvent(step6return$newVersions(), {

      print("From step 6, go to New Versions")
      r$triggerStep6_nr <- step6return$trigger()
      #save returns (but only if larger than null: Avoid overwriting with default empty returns)
      if(length(step6return$networkList()) > 0 ){
        r$networkList <- step6return$networkList()
      }
      if(length(step6return$versionsUI()) > 0 ){
        r$versionsUI <- step6return$versionsUI()
      }
      r$shp_PA <- step6return$shp_PA()


      r$currentLang <- step6return$currentLang()

      #
      if(step6return$newVersions() > 0 & r$triggerStep6_nr == 1){

        #save returns (but only if larger than null: Avoid overwriting with default empty returns)
        #get latest change in networkList (ex: if pathUsage was updated)
        # if(length(step6return$networkList()) > 0 ){
        #   networkList <<- step6return$networkList()
        # }
        # if(length(step6return$versionsUI()) > 0 ){
        #   versionsUI <<- step6return$versionsUI()
        # }

        r$triggerNewVersions_nr <- 1


        if(length(triggerNewVersions()) > 0){
          triggerNewVersions(triggerNewVersions()+1)
        }else{
          triggerNewVersions(1)
        }


        r$triggerStep6_nr <- 0 #alter triggerStep6 indirectly without triggering reaction (avoids chain reaction)

        # isolate(triggerStep6(0)) #reset value without trigger

      }
    }, ignoreInit = TRUE, once = TRUE)



    #From step6, go to Last Step
    shiny::observeEvent(step6return$confirm(), {
      #button indirectly triggers step 6, to allow for program intervention
      if(is.integer(step6return$confirm()) & step6return$confirm() > 0){
        # shinyjs::runjs("Shiny.onInputChange('step6-confirmButton6', 0);")
        shinyjs::reset(id = "step6-confirmButton6")

        #Check if simulations need to be run,
        #if so, run all simulations before going to last step

        #calculate time for 1 simulation, assume time for X simulations,
        #show over current timeframe

        #CONFIRM SIMULATIONS
        #save returns
        r$pathUsage <- step6return$pathUsage()
        r$versionsUI <- step6return$versionsUI()
        r$networkList <- step6return$networkList()
        r$shp_PA <- step6return$shp_PA()
        r$step <- 7
        shinyjs::click("downloadSave", asis = FALSE)

        triggerFinalStep(1)

      }else if(step6return$confirm() == "A"){
        r$step <- 1
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('step6-banner', 'O');")

        #Update UI
        print("RESTART STEP 1")
        if(is.null(restartSteps())){
          restartSteps(1)
        }else{
          restartSteps(restartSteps() + 1)
        }
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step1" )

        # restartSteps(restartSteps() + 1)
      }else if(step6return$confirm() == "B"){
        r$step <- 3
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('step6-banner', 'O');")

        #Update UI
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step3" )
        if(is.null(triggerStep3())){
          triggerStep3(1)
        }else{
          triggerStep3(triggerStep3() + 1)
        }

        # restartSteps(restartSteps() + 1)
      }else if(step6return$confirm() == "C"){
        r$step <- 4
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('step6-banner', 'O');")

        #Update UI
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step4" )
        print("RESTART STEP 4")
        if(is.null(triggerStep4())){
          triggerStep4(1)
        }else{
          triggerStep4(triggerStep4() + 1)
        }
        # restartSteps(restartSteps() + 1)
      }else if(step6return$confirm() == "D"){
        r$step <- 5
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('step6-banner', 'O');")

        #Update UI
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step5" )
        print("RESTART STEP 5")
        if(is.null(triggerStep5())){
          triggerStep5(1)
        }else{
          triggerStep5(triggerStep5() + 1)
        }
        # restartSteps(restartSteps() + 1)
      }
    }, ignoreInit = TRUE, once = TRUE)

  })


  #NEW VERSIONS PAGE
  shiny::observeEvent(triggerNewVersions(), {

    print("TRIGGERNEWVERSIONS()")


    if(triggerNewVersions() > 0 ){

      print("newVersions_server")
      newVersionsReturn <- newVersions_server("newVersions", networkList = r$networkList, SM_pres = r$SM_pres,  SMcolors =  r$SMcolors, shp_PA = r$shp_PA,
                                              finalPolygons = r$finalPolygons, confirm = r$confirm, versionsUI = r$versionsUI, isFirstRun = r$newVersionsFirstRun,
                                              DULN = r$DULN,
                                              i18n = shiny::reactive(i18n), currentLang = r$currentLang)

      r$newVersionsFirstRun <- FALSE

      #Update UI
      shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_newVersions" )

    }

    shiny::observeEvent(newVersionsReturn$confirm(), {

      cat(file = stderr(), paste0("newVersionsReturn$trigger_1() ", newVersionsReturn$trigger_1()))

      r$triggerNewVersions_nr <- newVersionsReturn$trigger_1()
      cat(file = stderr(), "TESTD")

      #save returns (but only if non NULL, to avoid overwriting with default)
      if(length(newVersionsReturn$networkList() ) > 0){
        r$networkList <- newVersionsReturn$networkList()
      }
      cat(file = stderr(), "TESTE")

      if(length(newVersionsReturn$versionsUI() ) > 0){
        r$versionsUI <- newVersionsReturn$versionsUI()
      }

      print("From new versions, return to step 6")
      cat(file = stderr(), "TESTF")
      cat(file = stderr(), paste0("newVersionsReturn$confirm() : ", newVersionsReturn$confirm()))
      cat(file = stderr(), paste0("r$triggerNewVersions_nr : ", r$triggerNewVersions_nr))

      #button indirectly triggers step 6, to allow for program intervention
      if(newVersionsReturn$confirm() > 0 & r$triggerNewVersions_nr == 1 ){
        print("PRE-TRIGGER STEP 6 return")
        cat(file = stderr(), "TESTFb")

        #save returns (but only if non NULL, to avoid overwriting with default)
        # if(length(newVersionsReturn$networkList() ) > 0){
        #   networkList <<- newVersionsReturn$networkList()
        # }

        #activate download
        r$step <- 6
        shinyjs::click("downloadSave", asis = FALSE)

        #change reactiveVal
        if(length(triggerStep6()) > 0){
          triggerStep6(triggerStep6()+1)
        }else{
          triggerStep6(1)
        }
        cat(file = stderr(), "TESTFc")

        r$triggerNewVersions_nr <- 0
        # isolate(tiggerNewVersions(0))#reset value without trigger

      }
    }, ignoreInit = TRUE, once = TRUE)


  }, ignoreInit = TRUE)


  #GO TO FINAL STEP ####


  shiny::observeEvent(triggerFinalStep(), {
    print("TRIGGERFINALSTEP()")
    if(triggerFinalStep() > 0){

      #Update UI
      shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_finalStep" )

      #Skip polygon generation = FALSE
      finalStepReturn <- lastStep_server("finalStep", networkList = r$networkList, versionsUI = r$versionsUI,
                                         SM_pres = r$SM_pres, SMColors = r$SMColors, shape = r$shape,
                                         basemap = r$basemap, finalPolygons = r$finalPolygons, species = r$spChc)


    }


    shiny::observeEvent( finalStepReturn$confirm() , {
      print("REACTIVE::: STEP5RETURN$CONFIRM")
      #button indirectly triggers step 2, to allow for program intervention
      if(is.integer(finalStepReturn$confirm()) & finalStepReturn$confirm() > 0){


        #change reactiveVal
        #change reactiveVal
        # if(is.null(triggerStep6()) ){
        #   triggerStep6(1)
        # }else{
        #   triggerStep6(triggerStep6()+1)
        # }
        #

      }else if(step6return$confirm() == "A"){
        r$step <- 1
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('lastStep-banner', 'O');")

        #Update UI
        print("RESTART STEP 1")
        if(is.null(restartSteps())){
          restartSteps(1)
        }else{
          restartSteps(restartSteps() + 1)
        }
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step1" )

        # restartSteps(restartSteps() + 1)
      }else if(step6return$confirm() == "B"){
        r$step <- 3
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('lastStep-banner', 'O');")

        #Update UI
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step3" )
        if(is.null(triggerStep3())){
          triggerStep3(1)
        }else{
          triggerStep3(triggerStep3() + 1)
        }

        # restartSteps(restartSteps() + 1)
      }else if(step6return$confirm() == "C"){
        r$step <- 4
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('lastStep-banner', 'O');")

        #Update UI
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step4" )
        print("RESTART STEP 4")
        if(is.null(triggerStep4())){
          triggerStep4(1)
        }else{
          triggerStep4(triggerStep4() + 1)
        }
        # restartSteps(restartSteps() + 1)
      }else if(step6return$confirm() == "D"){
        r$step <- 5
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('lastStep-banner', 'O');")

        #Update UI
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step5" )
        print("RESTART STEP 5")
        if(is.null(triggerStep5())){
          triggerStep5(1)
        }else{
          triggerStep5(triggerStep5() + 1)
        }
        # restartSteps(restartSteps() + 1)
      }else if(step6return$confirm() == "E"){
        r$step <- 5
        # step1return$confirm <- reactive(0) #reset confirm
        #reset all inputs
        # shinyjs::reset()
        shinyjs::runjs("Shiny.onInputChange('lastStep-banner', 'O');")

        #Update UI
        # shiny::updateTabsetPanel(inputId = "tabs", selected = "tab_step5" )
        print("RESTART STEP 6")
        if(is.null(triggerStep6())){
          triggerStep5(1)
        }else{
          triggerStep5(triggerStep6() + 1)
        }
        # restartSteps(restartSteps() + 1)
      }
    }, once = TRUE)
  })


  #TODO: REMOVE?

  #SHORTCUTS:
  if(step == 3){

    shp <- sf::st_read("www/data/maps/wiggerPerimeter_shp/wiggerPerimeter_LV95.shp")
    shp <- sf::st_as_sf(shp, coords = c("long", "lat"), crs = sf::st_crs(4326))
    shp <- sf::st_combine(shp)
    shp <- sf::st_cast(shp, "POLYGON")

    shp <- sf::st_transform(shp, crs = 4326)
    #shpWGS <- st_transform(shp, crs = "EPSG:3857")

    bb <- sf::st_bbox(shp)
    names(bb) <- c("left", "bottom", "right", "top")
    # basemap <<- get_map(location=bb,  maptype = 'terrain', source = 'google')


    #transform to web Mercator
    shp <- sf::st_transform(shp, crs = "EPSG:3857")

    r$shape <- shp

    #place buffer around polygon for some margin
    shape_larger <- sf::st_buffer(shp, dist = 1000)

    #use shape to prepare spatial filter as wkt (well-known text), grow slightly for buffer
    wkt <- sf::st_as_text( shape_larger )

    #extract relevant foot paths
    loadedPaths <- sf::st_read("www/data/maps/paths/paths_DULN_final2.shp",
                               query = 'SELECT * FROM "paths_DULN_final2"',
                               wkt_filter = wkt
    )

    #use function to prepare node-edge table
    r$network <- sf_to_tidygraph(loadedPaths, shape_larger, directed = FALSE)

    r$confirm <- shiny::reactiveVal(1)

    #trigger step 3
    print("TRIGGER STEP 3")
    triggerStep3(1)


  }else if (step == 4){
    shp <- sf::st_read("www/data/maps/wiggerPerimeter_shp/wiggerPerimeter_LV95.shp")
    shp <- sf::st_as_sf(shp, coords = c("long", "lat"), crs = sf::st_crs(4326))
    shp <- sf::st_combine(shp)
    shp <- sf::st_cast(shp, "POLYGON")

    shp <- sf::st_transform(shp, crs = 4326)
    #shpWGS <- st_transform(shp, crs = "EPSG:3857")

    bb <- sf::st_bbox(shp)
    names(bb) <- c("left", "bottom", "right", "top")
    # basemap <<- get_map(location=bb,  maptype = 'terrain', source = 'google')


    #transform to web Mercator
    shp <- sf::st_transform(shp, crs = "EPSG:3857")

    r$shape <- shp

    #place buffer around polygon for some margin
    shape_larger <- sf::st_buffer(shp, dist = 1000)

    #use shape to prepare spatial filter as wkt (well-known text), grow slightly for buffer
    wkt <- sf::st_as_text( shape_larger )

    #extract relevant foot paths
    loadedPaths <- sf::st_read("www/data/maps/paths/archive/paths_DULN_final2.shp",
                               query = 'SELECT * FROM "paths_DULN_final2"',
                               wkt_filter = wkt
    )

    #use function to prepare node-edge table
    r$network <- sf_to_tidygraph(loadedPaths, shape_larger, directed = FALSE)

    r$confirm <- shiny::reactiveVal(1)
    #trigger step 3
    print("TRIGGER STEP 4")
    triggerStep4(1)
  }else if(step == 5){

    load("envBase_step5.RData")

    print("TRIGGERSTEP5")
    print(r$finalPolygons)
    triggerStep5(1)
  }else if (step == 6){
    cat(file = stderr(), paste0("CURRENT WD: ",getwd()) )


    # r$newVersionsFirstRun <- TRUE
    # r$step6FirstRun <- TRUE
    #
    # areasOfInterest = NULL
    # #make default area shape
    # shp <- sf::st_read("www/data/maps/wiggerPerimeter_shp/wiggerPerimeter_LV95.shp")
    # shp <- sf::st_as_sf(shp, coords = c("long", "lat"), crs = sf::st_crs(4326))
    # shp <- sf::st_combine(shp)
    # shp <- sf::st_cast(shp, "POLYGON")
    #
    # r$shape <- shp
    #
    # r$SM_pres <- terra::rast("www/defaultRaster_SM_pres.tif")
    # r$SM_noPres <- terra::rast("www/defaultRaster_SM_noPres.tif")
    # r$SMcolors <- load("www/SMcolors_default.RData")
    # networkList <- NULL
    # load("www/objectsForStep6_2.RData")
    # r$networkList <- networkList
    # r$confirm <- shiny::reactiveVal(1)


    #try instead
    load("envBase_step6.RData")

    print("TRIGGERSTEP6")
    triggerStep6(1)
    r$triggerStep6_nr <- 1

  }else if(step == 62){
    #make default area shape
    shp <- sf::st_read("www/data/maps/wiggerPerimeter_shp/wiggerPerimeter_LV95.shp")
    shp <- sf::st_as_sf(shp, coords = c("long", "lat"), crs = sf::st_crs(4326))
    shp <- sf::st_combine(shp)
    shp <- sf::st_cast(shp, "POLYGON")

    r$shape <- shp

    load("objectsForStep6.RData")
    r$networkList <- list(network = r$network, pathUsage = NULL, parking = r$parking, residential = NULL, newAttr = NULL)

    r$confirm <- shiny::reactiveVal(1)

    triggerNewVersions(1)
    r$triggerNewVersions_nr <- 1
  }

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
