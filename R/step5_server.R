

# Define server logic
#' CONVERTED TO A FIRST-TOUCH SINGLETON (Stage 5, fourth module).
#'
#' This is the module the duplicate-instance symptom was actually *seen* in: two
#' step 5s answering one click, each with its own frozen `networkList`, showing
#' two "Original" scenarios - one simulated against an area that was no longer on
#' screen. It is also the module that is left and re-entered most often, because
#' the newVersions page is a side trip off it in both directions.
#'
#' Every argument except `id` and `i18n` is a REACTIVE now, and none is read
#' directly by the body: enter() snapshots them into locals of the same names, so
#' the ~2200 lines below are unchanged and still see plain values. A visit works
#' against a fixed network list; only BETWEEN visits may it change - which is
#' exactly what coming back from newVersions with a new version in hand is.
#'
#' The per-visit work is bigger here than in any other module, and all of it is in
#' enter() at the bottom:
#'
#'   * the version cards. They are `insertUI()`d into `#placeholder_step5`, one
#'     per entry in `versionsUI`, each with an observer of its own on `r$obsEventSelList`.
#'     Re-entering without clearing the placeholder first would insert a second
#'     copy of every card and a second observer behind each. The clear used to
#'     live in the "go to new versions" handler, i.e. on the way OUT and only
#'     once; it is on the way IN now, which also covers leaving by the nav bar.
#'   * `r$networkList` and `r$versionsUI`, which is the whole point of the side
#'     trip: newVersions writes them back into the app's `r` and step 5 must pick
#'     them up.
#'   * the "Original" scenario, created when `isFirstRun_stp6` says so.
#'   * the SELECTION. `r$selectedVersion` names the scenario the user last
#'     clicked - here or on the newVersions page, which shares the key - and
#'     generateVersionImages() resolves it back to a card. Everything a click on
#'     that card would set has to be set on the way in too, or the green border
#'     is the only part of "selected" that is true.
#'
#' No observer destroys its siblings any more. They did that so a REBUILT module's
#' handlers would not stack on the live ones; with one instantiation, destroying
#' them would mean the simulation could be launched exactly once per session.
step5_server <- function(id, networkList, SM_pres, SMcolors, shape, i18n, currentLang, isFirstRun_stp6,
                         finalPolygons = shiny::reactive(NULL),
                         versionsUI = shiny::reactive(list()),
                         triggerStp6 = shiny::reactive(0),
                         needHelp = shiny::reactive(FALSE),
                         species = shiny::reactive(NULL),
                         minCutThresh = shiny::reactive(NULL),
                         #the step-3 attractiveness threshold. Needed because the
                         #attractivity-weighted edge distances are computed when a
                         #simulation is first launched now, not at step 4's
                         #confirm. See R/prepare_network.R.
                         minThresh = shiny::reactive(NULL),
                         #the scenario card to come back with selected, shared with the
                         #newVersions page through r$selectedVersion
                         selectedVersion = shiny::reactive(NULL)){

  #count this instantiation. A module server should be created once per
  #session; this app re-calls it from an observeEvent on a trigger, so any
  #count above 1 means a duplicate set of observers and outputs is now live
  #alongside the previous one. See vftModuleInstance() in perf_helpers.R.
  vftModuleInstance("step5")

  #The reactives, held under different names so that the locals inside can shadow
  #them. Everything after this point reads plain values.
  #`triggerStp6` is not among them: it is a formal nothing has ever read - the
  #module writes its own `r$triggerStp6` - and it is kept only so the call site
  #does not have to change.
  .rx <- list(networkList = networkList, SM_pres = SM_pres, SMcolors = SMcolors,
              shape = shape, currentLang = currentLang,
              isFirstRun_stp6 = isFirstRun_stp6, finalPolygons = finalPolygons,
              versionsUI = versionsUI, needHelp = needHelp, species = species,
              minCutThresh = minCutThresh, minThresh = minThresh,
              selectedVersion = selectedVersion)

  shiny::moduleServer(id, function(input, output, session) {

    #per-visit snapshots. enter() refills these; the body and every closure in it
    #resolve them lexically from here, so nothing else in this file changes.
    networkList     <- NULL
    SM_pres         <- NULL
    SMcolors        <- NULL
    shape           <- NULL
    currentLang     <- NULL
    isFirstRun_stp6 <- FALSE
    finalPolygons   <- NULL
    versionsUI      <- list()
    needHelp        <- NULL
    species         <- NULL
    minCutThresh    <- NULL
    minThresh       <- NULL
    selectedVersion <- NULL

    #the protected-areas layer, and what it was last clipped for. See the note at
    #its assignment in enter(): this is display-only geometry keyed on the
    #perimeter, so it is derived when the PERIMETER changes rather than per visit.
    shp_PA <- NULL
    cache  <- new.env(parent = emptyenv())
    cache$shape <- NULL

    #CONSTANT:division of total residents to get number of agents
    CONST_residentDivision <- 50

    r <- shiny::reactiveValues()

    #### the sensitivity matrix is optional ####
    #
    # Step 2 produces it, and step 2 can be walked straight past: step 3 needs
    # only the perimeter and the attractiveness crop derived from it, so the nav
    # bar lights step 3 up the moment step 1 is confirmed. `SM_pres` and
    # `SMcolors` used to sit in step 5's `needs` for that reason, which made the
    # whole step unreachable for anyone who skipped step 2 - and, because step
    # 4's confirm moves the user on with check = FALSE, it put them ON a step 5
    # the nav bar then refused to let them back into. See VFT_STEPS in R/steps.R
    # for the other half of this: the keys are out of `needs` now.
    #
    # They can be out of `needs` because every consumer of the matrix in this
    # module is a DISPLAY consumer behind this one checkbox - the leaflet
    # overlay, the terra overlay on the export image, its legend, the species
    # caption beside it - plus the GeoTIFF download. Nothing a simulation reads.
    # So the step runs perfectly well without it and exactly two controls do not.

    #' Is the overlay switched on AND drawable?
    #'
    #' Every site that used to test `input$SMcheckbox` directly tests this. The
    #' checkbox is briefly TRUE with no matrix - the update() that puts it back
    #' is a client round trip - and a redraw landing in that window would reach
    #' terra::plot(NULL) / addRasterImage(NULL). Also tolerates the NULL the
    #' input reads as before the UI has initialised, which `== 1` did not.
    smOn <- function(){
      isTRUE(as.logical(input$SMcheckbox)) && !is.null(SM_pres)
    }

    #' "Go and make a sensitivity matrix" - a rising count app_server turns into
    #' the navigation to step 2. See the module's return value.
    smCreate <- shiny::reactiveVal(0L)

    #' Put the matrix-dependent control into the state the data justifies.
    #'
    #' Only ever takes state AWAY. The map-present observer enables every display
    #' control together and this runs after it; strengthening here instead would
    #' enable things while the rest of the panel is still dead.
    #'
    #' The CHECKBOX is deliberately not touched: it stays live whether or not
    #' there is a matrix, because that click is the offer to go and make one, and
    #' declining the offer must leave it exactly as it was - unchecked, and
    #' clickable again. Only the download is disabled, and only because it has
    #' nothing to write.
    applySMState <- function(){
      if(is.null(SM_pres)) shinyjs::disable(id = "SMbutton")
    }

    #' Offer to go and build one.
    smAskCreate <- function(){
      shiny::showModal(shiny::modalDialog(
        title = "Sensitivitätsmatrix nicht vorhanden",
        shiny::tags$p(paste0(
          "Für dieses Gebiet wurde noch keine Sensitivitätsmatrix erstellt - ",
          "Schritt 2 (Sensibilität) wurde übersprungen.")),
        shiny::tags$p("Möchten Sie jetzt eine erstellen?"),
        footer = shiny::tagList(
          shiny::actionButton(session$ns("smCreateNo"), "Nein, ohne fortfahren"),
          shiny::actionButton(session$ns("smCreateYes"), "Ja, zu Schritt 2",
                              class = "btn-primary")
        ),
        easyClose = FALSE
      ), session = session)
      invisible(NULL)
    }

    #One pair of observers for the module, not one pair per modal: a per-modal
    #pair left armed by a dismissed modal answers the NEXT one too. Same trap,
    #and the same fix, as vftCommitServer() in R/providers.R - including the
    #`v == 0` guard, because re-showing the modal re-renders both buttons and a
    #re-rendered actionButton reports 0, which observeEvent reads as a click.
    shiny::observeEvent(input$smCreateYes, {
      v <- input$smCreateYes
      if(is.null(v) || v == 0) return(invisible(NULL))
      shiny::removeModal(session = session)
      vftDbg("STEP5: sensitivity matrix wanted -> step 2")
      smCreate(shiny::isolate(smCreate()) + 1L)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$smCreateNo, {
      v <- input$smCreateNo
      if(is.null(v) || v == 0) return(invisible(NULL))
      shiny::removeModal(session = session)
      #Nothing else to do, and deliberately so. The checkbox was already put
      #back to unchecked before the modal went up, which is the whole of "no":
      #it is not disabled, not remembered, and ticking it again asks again.
      vftDbg("STEP5: sensitivity matrix declined")
    }, ignoreInit = TRUE)

    #the banner, the language bar and the per-visit state are in enter() now.

    #LOAD PROTECTED AREAS DATA ####
    #Display-only layer, handed straight to leaflet::addPolygons() below and in
    #newVersions. It used to be read and then st_crop()ed here, in EPSG:4326,
    #which was the single largest main-thread blocker in the app: ~5.7s per
    #session, 11.4s across the two sessions that reached step 5 in the
    #2026-08-20 profile. The layer is only 3 features but they carry 889,260
    #vertices, and in a lon/lat CRS sf routes the clip through s2's spherical
    #geometry. (The read itself was never the problem - 0.10s, the gpkg has an
    #r-tree index.)
    #
    #vftProtectedAreas() clips a process-wide, pre-simplified copy held in
    #EPSG:2056 and hands back lon/lat for leaflet: ~0.10s here instead of ~5.7s,
    #and 58% less GeoJSON for htmlwidgets to serialise. See data_paths.R.
    #(derived in enter(), and only when the perimeter has changed - see there.)

    # Activate outputUI checkbox at start
    # if(currentLang == "de"){
    #
    #   output$agentCheckbox_ui <- shiny::renderUI({
    #     shinyjs::disabled(
    #
    #       shiny::radioButtons(shiny::NS(id, "agentCheckbox"), shiny::HTML("<h5><strong>Welcher Agententyp wird angezeigt</strong></h5>"),
    #                           choices = c("alle" = "1", "Wanderer" = "2", "Radfahrer" = "3", "Hundespaziergänger" = "4", "Jogger" = "5") ,
    #                           selected = 1
    #       )
    #     )
    #   })
    #
    # }else if(currentLang == "fr"){
    #   output$agentCheckbox_ui <- shiny::renderUI({
    #     shinyjs::disabled(
    #       shiny::radioButtons(shiny::NS(id, "agentCheckbox"), shiny::HTML("<h5><strong>Type d'agent à afficher</strong></h5>"),
    #                           choices = c("tous" = "1", "Randonneurs" = "2", "Cyclistes" = "3", "Promeneurs de chien" = "4", "Joggeurs" = "5") ,
    #                           selected = 1
    #       )
    #     )
    #   })
    #
    #   # output$agentCheckbox_ui <- shiny::renderUI({
    #   #   shinyjs::disabled(
    #   #
    #   #     shiny::radioButtons(shiny::NS(id, "agentCheckbox"), shiny::HTML("<h5><strong>Quel type d'agent afficher</strong></h5>"),
    #   #                         choices = c("tous" = "1", "Randonneurs" = "2", "Cyclistes" = "3", "Promeneurs de chien" = "4", "Joggeurs" = "5") ,
    #   #                         selected = character(0)
    #   #     )
    #   #   )
    #   # })
    # }


    # # PARKING DATA ####
    # #fetch parking data
    #           #retrieve parking areas and crop
    #           r$parkingShapes <- sf::st_read(vftData("maps/parking/parkingShapes.shp"),
    #                                                 query = 'SELECT * FROM "parkingShapes"',
    #                                                 wkt_filter = wkt
    #           )
    #           r$parkingShapes <-  r1$parkingShapes |>
    #             dplyr::rename(polygons = .data$`_ogr_geometry_`) |>
    #             dplyr::select(.data$polygons)
    #           r$parkingShapes$id <- as.character(1:nrow(r1$parkingShapes))
    #           r$parkingShapes$isNew <- 0
    # r$parkingShapes$type = "parking"
    #
    #   #PARKING DATA ####
    #   #similar to residential, but sampling parkingPolygons
    #   #populate nodes with 0s in $parking
    #   newnodes$parking <- 0
    #   newnodes$newResidential <- 0
    #
    # filteredNodes <- sf::st_filter(newnodes, parkingPolygons)
    #
    #   #cycle through parking polygons
    #   for(polyNb in 1:nrow(parkingPolygons)){
    #
    #
    #     progress$inc((1/nrow(parkingPolygons))  / 4, detail = "Determining parking potential...")
    #     #get polygon size (should be in meters if proj = 4326)
    #     polyArea <- sf::st_area(parkingPolygons[polyNb,])
    #     #convert to nb of agents (1agent / 30m^2)
    #     agentNb <- polyArea/30
    #     #get nodes within polygon and distribute number of agents equally among nodes (decimals allowed)
    #     # ADD the values to nodes (pre-populated with 0s). This is to avoid conflicts with nodes already filled due to exception below.
    #     nodeCount <- nrow(filteredNodes[parkingPolygons[polyNb,], op = sf::st_within])
    #
    #     if(nodeCount > 0){
    #       #use sf polygon to select terra vect nodes (may have to convert to sf first)
    #       isWithin <- sf::st_within(filteredNodes, parkingPolygons[polyNb,])
    #       isWithin <- lengths(isWithin)
    #       filteredNodes[isWithin > 0,]$parking <- filteredNodes[isWithin > 0,]$parking + ( as.numeric(agentNb)/nodeCount )
    #     }else{
    #       #CAPTURE EXCEPTION : no nodes in polygon
    #       #in this case, find a single closest node outside polygon
    #       nearestIndex <- sf::st_nearest_feature(parkingPolygons[polyNb,], sf::st_as_sf(filteredNodes))
    #       filteredNodes$parking[[nearestIndex]] <- agentNb
    #     }
    #
    #
    #
    #
    #   }
    #     #
    #
    # newnodes$parking[newnodes$nodeID %in% filteredNodes$nodeID] <- filteredNodes$parking

    #PREPARE TIFF DOWNLOAD ####
    output$downloadTIFF <- shiny::downloadHandler(
      filename = function(){

        name <- input$nameInput

        #make sure .tif at the end
        if(substr(name, nchar(name)-3, nchar(name)) == ".tif"){
          name <- name
        }else{
          name <- paste0(name, ".tif")
        }

        vftDbg("BUILDING NAME")

        return(name)
      },
      content = function(file){

        vftDbg("COPYING FILE")
       file.copy(r$tiffFile, file)

      }
    )
    outputOptions(output, "downloadTIFF", suspendWhenHidden = FALSE)

    #PREPARE PATHS DOWNLOAD ####
    output$downloadPaths <- shiny::downloadHandler(
      filename = function(){

        if(r$currentLang == "de"){
        name <- "visitorFlow_heruntergeladenWege.zip"
        }else if(r$currentLang == "fr"){
          name <- "visitorFlow_cheminsTelecharges.zip"
        }else if(r$currentLang == "en"){
          name <- "visitorFlow_pathsDownload.zip"
        }

        return(name) # nolint: return_linter.
      },
      content = function(file){
        # print("COPYING FILE")
        #save current version paths as file
        pathUsage <- r$networkList[[selectedNetwork_position]]$pathUsage
        dayPop <- r$networkList[[selectedNetwork_position]]$dayPop


        # first print blank usageMap (with white color)
        #this is to set plot parameters. Above which a sensitivity matrix can be first plotted if needed
        # pathUsageColor <- c("white", "white")
        passageTable <- sf::st_zm(sf::st_as_sf(dplyr::as_tibble(pathUsage |> tidygraph::activate(edges) ) ), drop = T, what = "ZM")

        vertexTable <- sf::st_zm(sf::st_as_sf(dplyr::as_tibble(pathUsage |> tidygraph::activate(nodes) ) ) )
        startingPoints <- sf::st_geometry(sf::st_as_sf(vertexTable[vertexTable$nodeID %in% r$result$dayPop$startV , ]))

        # Create a dedicated temp folder with a clean name
        tmpDir <- tempfile(pattern = "paths_download")
        dir.create(tmpDir)

        # Define clean file names inside that folder
        pathFile  <- file.path(tmpDir, "paths.gpkg")
        startFile  <- file.path(tmpDir, "startingPoints.gpkg")
        perimeterFile  <- file.path(tmpDir, "perimeter.gpkg")
        aoiFile  <- file.path(tmpDir, "AOIs.gpkg")

        txtFile  <- file.path(tmpDir, "INFO_paths_and_aois.txt")

        # tempGDB_paths <- tempfile(pattern = "paths_" , fileext = ".gpkg")
        sf::st_write(passageTable[,c("passage", "passageAOI", "passageWalk", "passageWalkAOI", "passageDog",
                                     "passageDogAOI", "passageBike", "passageBikeAOI", "passageJogAOI", "passageAOI2")], dsn = pathFile )

        # tempGDB_startPts <- tempfile(pattern = "startPts_", fileext = ".gpkg")
        sf::st_write(startingPoints, dsn = startFile , driver = "GPKG")

        #put outline as file
        # tempGDB_outline <- tempfile(pattern = "outline_", fileext = ".gpkg")
        sf::st_write(shape, dsn = perimeterFile , driver = "GPKG")

        #put areas of interest
        # tempGDB_aoi <- tempfile(pattern = "zielgebiet_", fileext = ".gpkg")
        sf::st_write(finalPolygons, dsn = aoiFile , driver = "GPKG")


        #include a INFO.txt
        # tempTXT_info <- tempfile(pattern = "INFO_", fileext = ".txt")

        # fileConn<-file(tempTXT_info)
        writeLines("outline is the precised contours of the study.
        paths are the simulated path usages with various passage information.
        Aois are the determined Areas of Interest for recreation.", txtFile)
        # close(fileConn)

        # Zip using relative paths by setting wd to tmpDir
        oldWd <- setwd(tmpDir)
        on.exit(setwd(oldWd), add = TRUE)  # always restore wd


        #zip both
        utils::zip(file, c("paths.gpkg", "startingPoints.gpkg", "perimeter.gpkg", "AOIs.gpkg", "INFO_paths_and_aois.txt"))

      }
    )


    #PREPARE SM DOWNLOAD ####
    output$downloadSM <- shiny::downloadHandler(
      filename = function(){

        if(r$currentLang == "de"){
          name <- "visitorFlow_SensitivitaetMatrix.zip"
        }else if(r$currentLang == "fr"){
          name <- "visitorFlow_MatriceDeSensibilite.zip"
        }else if(r$currentLang == "en"){
          name <- "visitorFlow_SensitivityMatrix.zip"
        }

        return(name)
      },
      content = function(file){

        # Create a dedicated temp folder with a clean name
        tmpDir <- tempfile(pattern = "SM_download")
        dir.create(tmpDir)

        tiffFile <- file.path(tmpDir, "SM.tif")

      # tempTIF_SM <- tempfile(pattern = "SM_", fileext = ".tif")
      terra::writeRaster(SM_pres, filename = tiffFile, filetype = "GTiff")

      #text info
      txtFile  <- file.path(tmpDir, "INFO_SM.txt")


      # tempTXT_info <- tempfile(pattern = "INFO_", fileext = ".txt")
      # fileConn<-file(tempTXT_info)
      writeLines(c("Information about the sensitivity matrix.",
                   "Values represent sensitivity (number of considered species present * weight given",
                   "ex: if a pixel has 3 species with weight of 1, and 3 species with weight of 2, it would have a sensitivity of 9",
                   "---",
                   ifelse(minCutThresh == 0,"", paste0("Important: This Sensitivity Matrix only shows the top ", minCutThresh, "% sensitivity values")),
                   "The species included are:",
                   paste(species)
                   ), txtFile)
      # close(fileConn)

      # Zip using relative paths by setting wd to tmpDir
      oldWd <- setwd(tmpDir)
      on.exit(setwd(oldWd), add = TRUE)  # always restore wd

      #zip both
      utils::zip(file, c("SM.tif", "INFO_SM.txt"), flags = NULL)


      }
    )
    #make invisible buttons still function
    outputOptions(output, "downloadPaths", suspendWhenHidden = FALSE)
    outputOptions(output, "downloadTIFF", suspendWhenHidden = FALSE)
    outputOptions(output, "downloadSM", suspendWhenHidden = FALSE)

    #variables. r$networkList, r$versionsUI, r$triggerStp6 and r$lastSelectedImage
    #are per-VISIT and are set by enter(); these are the declarations only.
    obsEvent_sim <- NULL
    r$obsEventSelList <- list()

    selectedNetwork_position <- 1

    # envStep6 <- new.env(parent = emptyenv())

    r$selectedNetwork_r <- shiny::reactiveVal()
    # plotResults <- shiny::reactiveVal(0)

    # result <- new.env(parent = emptyenv())
    r$result <- NULL

    # lastSelectedImage <- NULL
    #internal function to generate version selection boxes
    updateVersions <- function(name, inputId_select, id_ui_name, position,
                               selected = FALSE){

      vftDbgCat("TEST9")

      #A card carries two independent states, and both are DATA rather than
      #decoration:
      #
      #  * withSim / noSim  - does this scenario have a simulation behind it,
      #  * selected / notSelected - is it the one the map is currently showing.
      #
      #Both are baked into the markup here instead of being applied afterwards
      #with shinyjs, and that is not a style choice: insertUI() is deferred to the
      #end of the flush while addClass() is sent immediately, so a class added
      #after this call would reach the browser BEFORE the card it names exists and
      #be dropped. The green border on the entry path has to come from here.
      #
      #`selected` is the CALLER's decision, and it used to be `name == "Original"`
      #here: whatever the user had been working on, the page came back showing the
      #original - and a scenario the user happened to call "Original" further down
      #the list came back wearing the green border as well. generateVersionImages()
      #resolves it from the remembered selection now. The guard on
      #length(networkList) is for the same reason as the one in the select observer
      #below: versionsUI and networkList are written by two different modules and a
      #short networkList would abort enter() from inside insertUI().
      hasSim <- position <= length(r$networkList) &&
                !is.null(r$networkList[[position]]$pathUsage)

      vftDbgCat(paste0("TEST9a card ", position, " ",
                       if(hasSim) "withSim" else "noSim"))

      shiny::insertUI(
        selector = '#placeholder_step5',
        ## wrap element in a div with id for ease of removal
        ui = shiny::tags$div(id = paste0("step5", id_ui_name),
                      shiny::div(style = "height: 5px"),

                      shiny::actionButton(inputId = shiny::NS(id, inputId_select),
                                          label = name,
                                          width = "120px",
                                          style = "height: 120px; position: relative; text-align: center; ",
                                          class = paste(if(isTRUE(selected)) "selected" else "notSelected",
                                                        if(hasSim) "withSim" else "noSim")),

                      shiny::div(style = "height: 5px")
        )

      )

      #`r$lastSelectedImage <- 1` used to sit here - an integer where every reader
      #passes the value straight to shinyjs::addClass() as an inputId. It was
      #harmless only because generateVersionImages() overwrites it with the real
      #inputId immediately after this loop, which is where it belongs.

      vftDbgCat("TEST10")

      #keep track of ids and inputIds
      # inserted_id_ui <<- c(inserted_id_ui, id_ui_name)
      # print(inserted_id_ui)
      # inserted_inputId_select <<- c(inserted_inputId_select, inputId_select)
      # print(inserted_inputId_select)
      # inserted_inputId_removal <<- c(inserted_inputId_removal, inputId_removal )
      # print(inserted_inputId_removal)


      #OBSERVER FOR SELECT VERSION ####
      #
      #`ignoreInit` AND THE `> 0` GUARD ARE BOTH LOAD-BEARING ON THE RETURN PATH.
      #This observer is created fresh on every visit (enter() destroys the
      #previous visit's set), but `input$versionBtnN` is NOT: removeUI() takes the
      #card out of the DOM and leaves the input value on the server, so a card the
      #user clicked last visit still reads 1, 2, 3... when the new observer is
      #built. Without ignoreInit an observeEvent fires on its FIRST evaluation, so
      #every card re-selected itself the moment enter() rebuilt it - and it did so
      #from inside flushReact(), i.e. BEFORE the deferred insertUI() had put the
      #cards back in the DOM, so the shinyjs class changes below addressed
      #elements that did not exist yet and were dropped. What survived was the
      #half that does not touch the DOM: `selectedNetwork_position` and the map.
      #
      #That is the "Original is highlighted but the map is somebody else's"
      #report exactly. The border came from the markup updateVersions() inserts;
      #the map came from whichever card had a non-zero count. Leave with the
      #original selected and the two happen to agree, which is why it only showed
      #up after selecting another card first.
      #
      #The second fire is the re-inserted button reporting its own initial value:
      #a fresh actionButton starts at 0, so the count drops (1 -> 0) and that is a
      #change like any other. ignoreInit does not cover it - it is the second
      #evaluation, not the first - hence the guard. An actionButton at 0 has not
      #been clicked since it was inserted, and a click is the only thing this
      #observer is for.
      r$obsEventSelList[[length(r$obsEventSelList)+1]] <- list(
          shiny::observeEvent(input[[inputId_select]], {

        if(!isTRUE(shiny::isolate(input[[inputId_select]]) > 0)){
          vftDbg(paste0("SELECT ignored (not a click): ", inputId_select))
          return(invisible(NULL))
        }

        vftDbg("SELECTED")
        vftDbg(r$versionsUI)
        #select button (outline in green?)
        shinyjs::removeClass(inputId_select, "notSelected")
        shinyjs::addClass(inputId_select, "selected")

        vftDbg(paste0("lastSelectedImage: ", r$lastSelectedImage))

        #grey the card that WAS selected - unless that is this one. Re-clicking
        #the selected card used to add "notSelected" to the same button two lines
        #after "selected" was added to it, and `.notSelected` is declared after
        #`.selected` in step5_ui.R, so equal specificity handed the border to
        #grey: the card the map was showing went grey when you clicked it again.
        if(!is.null(r$lastSelectedImage) &&
           !identical(r$lastSelectedImage, inputId_select)){
          shinyjs::addClass(r$lastSelectedImage, "notSelected")
        }

        #make this button the lastSelected button
        r$lastSelectedImage <- inputId_select

        #change selected network
        #use number of version_x


        # SELECT NETWORK ####
        #Determine position in list of current inputIdSelect
        x <- NULL
        for(vn in 1:length(r$versionsUI)){
          if(r$versionsUI[[vn]]$inputId_select == inputId_select){
            x <- vn
          }
        }

        #select same position in networkList
        if(x <= length(r$networkList)){

          r$selectedNetwork_r( list(r$networkList[[x]]$network) )
          #keep track position to easily insert results in networkList
          selectedNetwork_position <<- x

          #remember the choice for the next visit - to this step and to the
          #newVersions page, which draws the same scenarios and reads the same
          #key. By NAME, because the newVersions page can delete a version and
          #move every position after it; see vftVersionPosition() in R/modules.R.
          r$selectedVersion <- r$versionsUI[[x]]$name

        }else{
          vftDbg("ERROR: less networks than version buttons")

        }

        # PLOT SELECTED SIMULATIONS

       plotPathUsage()

       #plot with Leaflet instead



      }, ignoreInit = TRUE)
        )

    }
    vftDbgCat("TEST11")



    # Cache for the materialized edge sf ("passageTable"). It is a pure function of
    # r$result$pathUsage, so build it once and reuse across cosmetic checkbox toggles /
    # re-renders instead of re-converting the whole tidygraph each time. Invalidated
    # (r$passageTable <- NULL) wherever r$result$pathUsage is reassigned.
    getPassageTable <- function(){
      if(is.null(r$passageTable)){
        r$passageTable <- sf::st_zm(sf::st_as_sf(dplyr::as_tibble(r$result$pathUsage |> tidygraph::activate(edges) ) ), drop = T, what = "ZM")
      }
      r$passageTable
    }

    # Same idea for the agents' starting points, and for the same reason: it is a
    # pure function of r$result (the node table and dayPop$startV), it was spelled
    # out at three separate sites, and one of them is obsAgentStart - so every
    # toggle of the starting-points checkbox rescanned the WHOLE vertex table.
    #
    # The scan is `vertexTable[vertexTable$nodeID %in% dayPop$startV , ]`, which
    # is a `%in%` over every node followed by a data.frame row subset. In the
    # 2026-08-25 line profile `[.data.frame` was 8.72s self / 28.80s total and
    # `%in%` another 3.60s - together the largest pure-R cost in the app, ahead
    # of everything spatial.
    #
    # Returns the sf object; callers take st_coordinates() or st_geometry() from
    # it as they did before. Invalidated (r$startingPointsSf <- NULL) at the same
    # three places as r$passageTable, which are where r$result is reassigned.
    getStartingPoints <- function(){
      if(is.null(r$startingPointsSf)){
        vertexTable <- dplyr::as_tibble(r$result$pathUsage |> tidygraph::activate(nodes) )
        r$startingPointsSf <- sf::st_as_sf( vertexTable[vertexTable$nodeID %in% r$result$dayPop$startV , ])
      }
      r$startingPointsSf
    }

    plotPathUsage <- function(){



      vftDbgCat("TEST12")
      # output$pathUsageMap <- leaflet::renderLeaflet({
      vftDbg("ACTIVATING RENDERPLOT")

      bboxUsage <- NULL
      map <- NULL
      # PLOT SELECTED PATHUSAGE (IF PRESENT) ####
      if(shiny::isolate(!is.null(r$networkList[[selectedNetwork_position]]$pathUsage )) ){

        #use proxy if map already present


          #make current result that of selected version
          shiny::isolate(r$result$pathUsage <- r$networkList[[selectedNetwork_position]]$pathUsage)
          shiny::isolate(r$result$dayPop <- r$networkList[[selectedNetwork_position]]$dayPop)
          #selected version changed: invalidate the cached tables so they rebuild below
          shiny::isolate(r$passageTable <- NULL)
          shiny::isolate(r$startingPointsSf <- NULL)

          #reactiveVal to manually trigger plotting
          vftDbg("PLOTRESULTS()")
          # first print blank usageMap (with white color)
          #this is to set plot parameters. Above which a sensitivity matrix can be first plotted if needed
          # pathUsageColor <- c("white", "white")
          passageTable <- getPassageTable()

          startingPoints <- sf::st_coordinates(getStartingPoints())



          if(input$onlyAOIcheckbox == 1){

            if(is.null(input$agentCheckbox)){
              agentTypePassage <- "passageAOI"
            }else{
              #determine agent to focus on
              if(input$agentCheckbox == "1"){
                agentTypePassage <- "passageAOI"
              }else if(input$agentCheckbox == "2"){
                agentTypePassage <- "passageWalkAOI"
              }else if(input$agentCheckbox == "3"){
                agentTypePassage <- "passageBikeAOI"
              }else if(input$agentCheckbox == "4"){
                agentTypePassage <- "passageDogAOI"
              }else if(input$agentCheckbox == "5"){
                agentTypePassage <- "passageJogAOI"
              }
            }

          }else{

            if(is.null(input$agentCheckbox)){
              agentTypePassage <- "passage"
            }else{
              #determine agent to focus on
              if(input$agentCheckbox == "1"){
                agentTypePassage <- "passage"
              }else if(input$agentCheckbox == "2"){
                agentTypePassage <- "passageWalk"
              }else if(input$agentCheckbox == "3"){
                agentTypePassage <- "passageBike"
              }else if(input$agentCheckbox == "4"){
                agentTypePassage <- "passageDog"
              }else if(input$agentCheckbox == "5"){
                agentTypePassage <- "passageJog"
              }
            }

          }



          #test (not run)
          # leaflet::previewColors(pa<l, values = passageTable$passage)
          pal <- leaflet::colorNumeric(c("darkgrey",colorRampPalette(c("lightblue", "steelblue", "#182db5", "#37046e"))(max(passageTable$passage)-1) ), domain = c(0,max(passageTable$passage)) )

          #if there is no map, make one. If there is, update it
          if(!is.null(r$mapPresent)){
          if(r$mapPresent == FALSE | r$refreshMap == TRUE){
          map <- vftTime("step5:buildBaseMap",
            leaflet::leaflet(data = passageTable, options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE), height = 500 ) |>
            leaflet::addMapPane("layer_SM", zIndex = 415)|>
            leaflet::addMapPane("layer1", zIndex = 410)|> leaflet::addMapPane("layer2", zIndex = 420)|> leaflet::addMapPane("layer3", zIndex = 450) |>
            leaflet::addProviderTiles("OpenStreetMap.CH", options = leaflet::providerTileOptions(opacity = 0.5, zIndex = 400)))

            map <- map |>
              leaflegend::addLegendImage(position = "topright",title = i18n()$t("Formen:"),
                                         images = c("www/AOI.png", "www/parking.png", "www/bewohnen.png", "www/Start.png",
                                                    "www/PA_1.png", "www/PA_2.png", "www/PA_3.png"),
                                         labels = c(i18n()$t("Zielgebiete"),i18n()$t("Parkplatz"), i18n()$t("Neue Wohngebiete"), i18n()$t("Agenten Ausgangspunkte"),
                                                    i18n()$t("Schutzgebiete – streng"), i18n()$t("Schutzgebiete – umfassend"), i18n()$t("Schutzgebiete – teilweise")),
                                         labelStyle = "font-size: 15px; text-align: left")|>
              leaflet::addLegend(title = i18n()$t("Wegnutzung:"), position = "topright", labels = c(i18n()$t("kein"), i18n()$t("niedrigste"), i18n()$t("mittlere"), i18n()$t("hohe"), i18n()$t("höchste")) , colors = c("darkgrey", "lightblue", "steelblue", "#182db5", "#37046e"))|>
              leaflet.extras::setMapWidgetStyle(list(background = "white"))

          }else{
            #This is the rebuild every checkbox toggle goes through (sensitivity
            #matrix, AOI, within-AOI, starting points, parking, ...): wipe the
            #overlays, then re-add them below from the current inputs.
            #
            #vftClearNetworkLines() is NOT optional here. clearShapes() used to
            #remove the network because it was SVG polylines, but it does not
            #touch WebGL layers - they live in their own canvases - so without
            #this each toggle left the old network behind and stacked another set
            #of canvases on top of it, in a pane above the AOI polygons, the
            #raster and the starting points. A few toggles and the browser is
            #holding a dozen live WebGL contexts (most cap around 16) and the
            #other layers stop showing.
            map <- leaflet::leafletProxy("mapAreaLeaflet")|>
              leaflet::clearShapes()|>leaflet::clearGeoJSON()|>leaflet::clearImages()|>
              vftClearNetworkLines(group = "paths")
          }
          }else{
            #create map if null
            map <- leaflet::leaflet(data = passageTable, options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE), height = 500 ) |>
              leaflet::addMapPane("layer_SM", zIndex = 415)|>
              leaflet::addMapPane("layer1", zIndex = 410)|> leaflet::addMapPane("layer2", zIndex = 420)|> leaflet::addMapPane("layer3", zIndex = 450) |>
              leaflet::addProviderTiles("OpenStreetMap.CH", options = leaflet::providerTileOptions(opacity = 0.5, zIndex = 400))

            map <- map |>
              leaflegend::addLegendImage(position = "topright",title = "Formen:", images = c("www/AOI.png", "www/parking.png", "www/bewohnen.png", "www/Start.png",
                                                                                             "www/PA_1.png", "www/PA_2.png", "www/PA_3.png"),
                                         labels = c(i18n()$t("Zielgebiete"),i18n()$t("Parkplatz"), i18n()$t("Neue Wohngebiete"), i18n()$t("Agenten Ausgangspunkte"),
                                                    i18n()$t("Schutzgebiete – streng"), i18n()$t("Schutzgebiete – umfassend"), i18n()$t("Schutzgebiete – teilweise")),
                                         labelStyle = "font-size: 15px; text-align: left")|>
              leaflet::addLegend(title = "Wegnutzung:", position = "topright", labels = c(i18n()$t("kein"), i18n()$t("niedrigste"), i18n()$t("mittlere"), i18n()$t("hohe"), i18n()$t("höchste")) , colors = c("darkgrey", "lightblue", "steelblue", "#182db5", "#37046e"))|>
              leaflet.extras::setMapWidgetStyle(list(background = "white"))
            }

            #the network is the single largest main-thread cost in the app:
            #addPolylines() encodes every edge into nested JSON on the shared
            #thread and scales with edge count, so a 50k-edge network froze every
            #session for ~15s. vftAddNetworkLines() draws it through WebGL in
            #~0.6s and keeps both the colour ramp and the 2-4px width. See
            #data_paths.R; VFT_GL=0 restores this exact addPolylines call.
            map <- vftAddNetworkLines(map, passageTable,
                                      values    = passageTable[,agentTypePassage,drop = TRUE],
                                      weightRef = passageTable[,"passageAOI",drop = TRUE],
                                      pal = pal, group = "paths", pane = "layer2")
            if(input$aoi == TRUE){
              map <- map |> leaflet::addPolygons(data = finalPolygons,
                                                  weight = 3,
                                                  color = "green4",
                                                  fillColor = "green",
                                                  fill = TRUE,
                                                  stroke = TRUE,
                                                  options = leaflet::pathOptions(pane = "layer1"),
                                                  opacity = 0.3,
                                                  fillOpacity = 0.1,
                                                  group = "AOI")
            }

            if(input$PA_Checkbox == TRUE){
              map <- map |> leaflet::addPolygons(data = shp_PA,
                                                  weight = 5,
                                                  color = "green4",
                                                  fillColor = NA,
                                                  fill = FALSE,
                                                  stroke = TRUE,
                                                  options = leaflet::pathOptions(pane = "layer1"),
                                                  opacity = 1,
                                                  group = "PA")
            }



            if(smOn()){
              map <- map |> leaflet::addRasterImage(raster::raster(SM_pres),
                                           colors = SMcolors,
                                           opacity = 1,
                                           group = "SM")
            }

          if(input$ParkingCheckbox == TRUE){
            map <- map|>
              leaflet::addPolygons(data = sf::st_geometry(r$networkList[[selectedNetwork_position]]$parking), stroke = TRUE, fill = TRUE,
                                   fillColor = "blue", opacity = 1, fillOpacity = 0.3, group = "parking")
          }

          if(input$ResidentialCheckbox == TRUE & length(r$networkList[[selectedNetwork_position]]$residential) > 0 ){
            map <- map|>
              leaflet::addPolygons(data = sf::st_geometry(r$networkList[[selectedNetwork_position]]$residential), stroke = TRUE, fill = TRUE,
                                   fillColor = "#8a722b", opacity = 1, fillOpacity = 0.3, group = "residential")
          }

          map <- map |> leaflet::addPolygons(data= sf::st_zm(sf::st_transform(shape, "epsg:4326"), drop = TRUE, what = "ZM" ), stroke = TRUE, fill = FALSE, color = "black",
                                              weight = 5, options = leaflet::pathOptions(pane = "layer2"))

          if(input$startingCheckbox == TRUE){
            map <- map |> leaflet::addCircleMarkers(lng = startingPoints[,"X"], lat = startingPoints[,"Y"] , group = "startingPoints",
                                                     color = "red", fill = FALSE, stroke = TRUE, opacity = 1,
                                                     options = leaflet::markerOptions(pane = "layer1"), weight = 2,
                                                     radius = 3)
          }

          if(!is.null(r$mapPresent)){
          if(r$mapPresent == FALSE | r$refreshMap == TRUE){
            # output$mapArea <- shiny::renderUI({
            #
            #   leaflet::leafletOutput(NS(id, "mapAreaLeaflet"), height = 500)
            #
            # })
            # output$mapScript <- renderUI({
            #   tags$script(HTML(paste0(
            #     'document.getElementById("step5-mapAreaLeaflet").style.height="500px";',
            #     'document.getElementById("step5-mapArea").style.height="0px";'
            #   )))
            # })
            # output$mapArea <- renderUI({return(NULL)})
            #the leafletOutput used to be emitted HERE, from mapArea_UI. It is
            #static in step5_ui.R now - see the note there - and all that is left
            #to do is uncover it.
            shinyjs::hide(id = "mapPlaceholder")

            #the block is just `map` - the map was built eagerly above - so all the
            #cost here is htmlwidgets turning it into JSON, which happens outside
            #the block. vftTimeRender spans that; vftTime inside would read ~0.
            output$mapAreaLeaflet <- vftTimeRender("step5:mapAreaLeaflet",
                                                   leaflet::renderLeaflet({
              map
            }))
          }
          }else{
            #if r$mapPresent is null
            # output$mapArea <- shiny::renderUI({
            #
            #   leaflet::leafletOutput(NS(id, "mapAreaLeaflet"), height = 500)
            #
            # })
            # output$mapScript <- renderUI({
            #   tags$script(HTML(paste0(
            #     'document.getElementById("step5-mapAreaLeaflet").style.height="500px";',
            #     'document.getElementById("step5-mapArea").style.height="0px";',
            #     'document.getElementById("step5-mapAreaLeaflet").style.width="884px";',
            #     'document.getElementById("step5-mapArea").style.width="0px";'
            #   )))
            # })
            #see the sibling site above: the container is static in step5_ui.R,
            #so uncovering it is the whole job.
            shinyjs::hide(id = "mapPlaceholder")
            #see the note at the sibling site above
            output$mapAreaLeaflet <- vftTimeRender("step5:mapAreaLeaflet",
                                                   leaflet::renderLeaflet({
              map
            }))

          }


          # #activate "Make a Picture" button
          # shinyjs::enable(id = "imageButton")
          #
          # #ACTIVATE checkboxs:
          # shinyjs::enable(id = "agentCheckbox")
          # r$agentCheckboxIsDisabled <- FALSE
          # shinyjs::enable(id = "onlyAOIcheckbox")
          # shinyjs::enable(id = "SMcheckbox" )
          # shinyjs::enable(id = "startingCheckbox" )
          # shinyjs::enable(id = "aoi")
          # shinyjs::enable(id =  "ParkingCheckbox")
          # shinyjs::enable(id =  "ResidentialCheckbox")
          # shinyjs::enable(id = "Bewohnen")

          r$mapPresent <- TRUE



      }else{


        # Not plot available

        # #DISABLE checkboxes:
        # shinyjs::disable(id = "agentCheckbox")
        # r$agentCheckboxIsDisabled <- TRUE
        #
        # shinyjs::disable(id = "onlyAOIcheckbox")
        # shinyjs::disable(id = "SMcheckbox" )
        # shinyjs::disable(id = "startingCheckbox" )
        # shinyjs::disable(id = "aoi")
        # shinyjs::disable(id =  "ParkingCheckbox")
        # shinyjs::disable(id =  "ResidentialCheckbox")
        # shinyjs::disable(id = "Bewohnen")
        # shinyjs::disable(id = "newVersionsButton")
        # shinyjs::disable(id = "imageButton")



        #The "no simulation yet" placeholder is a static image, and it used to
        #cost 0.83s of the shared main thread per session to show one: 0.22s for
        #png::readPNG to decode a 1865x2748x4 PNG into a ~20 MB array, then 0.61s
        #for renderPlot to open an 884x600 device, rasterImage it and re-encode
        #the result to PNG. That was the stall users reported between step 5
        #loading and the placeholder appearing.
        #
        #The browser can fetch the file itself. www is registered as a resource
        #path in zzz.R, so an <img> costs the main thread nothing at all - no
        #decode, no device, no re-encode, no output binding to maintain.
        #only these three placeholders exist in www; the old code left noSimPic
        #undefined for anything else and failed in the render, so fall back
        #rather than asking the browser for a file that is not there.
        noSimLang <- if(isTRUE(r$currentLang %in% c("de", "fr", "en"))) r$currentLang else "de"

        #`mapArea_UI` is the OVERLAY now, not the map container - it fills
        ##mapPlaceholder, which sits on top of the static leafletOutput. The map
        #underneath is left alone on purpose: hiding it would take its
        #offsetWidth to 0 and leave leaflet waiting for a resize() it will not
        #get. See step5_ui.R.
        #sized by the frame, not by itself. It used to be drawn at a hardcoded
        #884x600 - the frame's old fixed size - so once the frame started
        #tracking the screen height the image kept its full size and spilled out
        #over the scenario sidebar to its right. 100%/100% with object-fit
        #keeps it inside the frame at any height and keeps its aspect ratio;
        #.vft-map5-frame's overflow:hidden is the backstop. See
        #R/layout_helpers.R.
        output$mapArea_UI <- renderUI({
          shiny::tags$img(src   = paste0("noSimYet_", noSimLang, ".png"),
                          style = "width:100%; height:100%; object-fit:contain;",
                          alt   = i18n()$t("Noch keine Simulation"))
        })
        shinyjs::show(id = "mapPlaceholder")

          r$mapPresent <- FALSE
      }




      # else{

      #   })
      # }


      r$refreshMap <- FALSE
    # })
    }





      results <- NULL

      #CREATE ORIGINAL VERSIONS (FOR FIRST RUN ONLY) ####
      #Per VISIT, so it is a function enter() calls rather than a statement here.
      #`isFirstRun_stp6` is the app's r$step6FirstRun, which app_server clears
      #immediately after each visit and vftInvalidate() re-arms when the saved
      #versions are discarded - so this has to be asked again on every entry, not
      #once at construction.
      createOriginalVersion <- function(){
      vftDbgCat("TEST15")

      vftDbgCat(paste0("isFirstRun_stp6 : ", isFirstRun_stp6))

      if(isTRUE(isFirstRun_stp6) | is.null(r$versionsUI) ){
        vftDbgCat("TEST15b")



        name = "Original"
        inputId_select <- paste0("versionBtn", 0)
        id_ui_name <- paste0('version_', 0)

        # updateVersions(name, inputId_select, id_ui_name)

        r$versionsUI[[name]] <- list(name = name,
                                       inputId_removal = NULL,
                                       inputId_select = inputId_select,
                                       id_ui_name = id_ui_name
        )
        # noSimPic <- png::readPNG("www/noSimYet.png")
        #
        # #plot an image of empty simulation
        # output$pathUsageMap <- shiny::renderPlot({
        #
        #   plot(1, type = "n", xlab = "",
        #        ylab = "", xlim = c(0, 10),
        #        ylim = c(0, 10), bty ="n",axes=F,frame.plot=F, xaxt='n', ann=FALSE, yaxt='n')
        #
        #   graphics::rasterImage(noSimPic,1,1,9,9)
        #
        #   shiny::isolate(r$lastSelectedImage <- inputId_select)
        #
        #   print("EMPTY RENDERPLOT")
        # })

        r$step6FirstRun <- FALSE
      }
      }


      #### OBSERVERS ####
      #dismiss Modal
      obs_dimissModal <- shiny::observeEvent(input$dismissModal, {
        shiny::removeModal()
      })

      #observe info Button ####
      obs_info5 <- shiny::observeEvent(input$infoButton5, {
        shiny::showModal(
          shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                             h2(i18n()$t("Zusätzliche Informationen:") ),
                             h3(),
                             h3(i18n()$t("Die Simulation schickt virtuelle Personen ('Agenten') zur Erkundung von Zielgebieten und folgt dabei der bestehenden Weg- und Strasseninfrastruktur, wie sie von Swisstopo im Jahr 2024 kartiert wurde.") ),
                             h3(),
                             h4(i18n()$t("Das Zielgebiet wird aufgrund seiner Attraktivität, Grösse und Nähe zum Wohngebiet des Agenten ausgewählt.") ),
                             h3(),
                             h4(i18n()$t("Die Wohngebiete der Agenten werden anhand der vom BFS zur Verfügung gestellten Schweizer Wohndaten aus dem Jahr 2024 ermittelt.") ),
                             h3(),
                             h4(i18n()$t("Die Agenten folgen dem kürzesten Weg (leicht gewichtet nach Attraktivität) zu ihrem Interessengebiet. Auf diese Weise können sie einen kleinen Umweg in Kauf nehmen, wenn dies bedeutet, einer attraktiveren Straße zu folgen (z. B. entlang eines Flusses).") ),
                             h3(),
                             h4(i18n()$t("Innerhalb eines Interessengebiets folgen die Agenten den attraktivsten Wegen. Sie wählen die attraktivsten Pfade probabilistisch aus. Das heißt, wenn ein Weg etwas attraktiver ist als ein anderer, werden die Agenten nicht immer den attraktiveren Weg wählen, aber sie werden ihn häufiger wählen. Der Zufall erlaubt es den Agenten, manchmal auch weniger attraktive Wege zu wählen. Dies kann dazu beitragen, die weniger vorhersehbaren Aspekte des menschlichen Verhaltens besser darzustellen.") )

          )
        )
      })

      #observe help ####
      obs_help5 <- shiny::observeEvent(input$helpButton5, {
        shiny::showModal(
          shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                             h2(i18n()$t("Simulieren Sie die Naherholung.")),
                             h3(),
                             div(style = "text-align:right",
                                 h3(shiny::HTML(i18n()$t("Auf der <b>rechten</b> Seite können Sie die aktuellen Szenarien sehen.")), shiny::img(src = "www/arrowRight.png", style = "float:right;height:50px;margin-right:-70px")),
                                 h4(shiny::HTML(i18n()$t("Sie verfügen derzeit nur über die Originalkarte."))),
                                 h3(),
                                 h3(shiny::HTML(i18n()$t("Wenn Sie möchten, können Sie neue Szenarien erstellen."))),
                                 h4(shiny::HTML(i18n()$t("So können Sie die Infrastruktur (Wege, Parkplätze usw.) ändern."))),
                                 h3(shiny::HTML(i18n()$t("Sie können dann diese neuen Szenarien auswählen und eine Simulation durchführen."))),
                                 h4(shiny::HTML(i18n()$t("Bei der Simulation werden alle Änderungen berücksichtigt."))),
                                 h3(),
                                 h3(i18n()$t("Wenn ein Szenario simuliert wurde, wird ihr Kreis mit einem Häkchen versehen!")),
                                 div(style = "text-align:center",
                                     img(src = "www/simulated.png", style = "height:85px")
                                 )
                             ),

                             shiny::img(src = "www/arrowLeft.png", style = "float:left;height:50px;margin-left:-70px"),h3(shiny::HTML(i18n()$t(":linken:"))),


          )
        )
      })

      #Language Change ####
      langChangeObs <- observeEvent(input$languageSelect_5, {
        vftDbg("CHANGE LANGUAGE")
        if(input$languageSelect_5 == "de"){
          # i18n$set_translation_language('de')
          shiny.i18n::update_lang("de")
          i18n()$set_translation_language("de")
          vftDbg("DE")
          vftSetBanner(id, "www/step5_wsl.png")

          output$agentCheckbox_ui <- shiny::renderUI({


            shiny::radioButtons(shiny::NS(id, "agentCheckbox"), "Welcher Agententyp wird angezeigt",
                                choices = c("alle" = "1", "Wanderer" = "2", "Radfahrer" = "3", "Hundespaziergänger" = "4", "Jogger" = "5") ,
                                selected = 1)
            })

          r$currentLang <- "de"


        }else if(input$languageSelect_5 == "fr"){
          # i18n$set_translation_language('fr')
          shiny.i18n::update_lang("fr")
          i18n()$set_translation_language("fr")

          vftSetBanner(id, "www/step5_wsl_fr.png")

          output$agentCheckbox_ui <- shiny::renderUI({


            shiny::radioButtons(inputId = NS(id, "agentCheckbox"), label = "Type d'agent à afficher",
                                choices = c("tous" = "1", "Randonneurs" = "2", "Cyclistes" = "3", "Promeneurs de chien" = "4", "Joggeurs" = "5") ,
                                selected = 1)
          })


          r$currentLang <- "fr"

          vftDbg("FR")
        }else if(input$languageSelect_5 == "en"){
          # i18n$set_translation_language('en')
          shiny.i18n::update_lang("en")
          i18n()$set_translation_language("en")


          vftSetBanner(id, "www/step5_wsl_en.png")

          output$agentCheckbox_ui <- shiny::renderUI({


            shiny::radioButtons(inputId = NS(id, "agentCheckbox"), label = "Agent type to display",
                                choices = c("all" = "1", "Walkers" = "2", "Cyclists" = "3", "Dog walkers" = "4", "Joggers" = "5") ,
                                selected = 1)
          })


          r$currentLang <- "en"

          vftDbg("EN")
        }else if(input$languageSelect_5 == "it"){
          # i18n$set_translation_language('it')
          shiny.i18n::update_lang("it")
          vftSetBanner(id, "www/step5_wsl.png")


          vftDbg("IT")
        }

        r$refreshMap <- TRUE
        plotPathUsage()
      }, ignoreInit = TRUE)

      #observe banner click (choosing to step back in history)
      #
      #NOTE ON THE MISSING $destroy() LISTS AND THE MISSING return()s, HERE AND IN
      #obsNewVersions BELOW.
      #
      #Both handlers used to tear down the module's observers and then return a
      #handle. Neither did what it looks like: an observeEvent HANDLER's return
      #value goes nowhere - the module's handle is the one built at the bottom of
      #this function - and the teardown existed only because a re-entered step 5
      #used to build a SECOND set of observers on top of the live ones. There is
      #one set now, for the life of the session, so destroying it would mean the
      #simulation could be launched exactly once per session. The per-VERSION
      #observers are different: those really are recreated on every visit, and
      #enter() destroys them before it re-inserts the cards.
      obsBanner <- observeEvent(input$banner,  {
        vftDbg("MAPPED IMAGE CLICKED")
        #determine where to go back in history
        r$confirm <- input$banner

        #treat next step 6 as first run. (This is the MODULE's r, not the app's,
        #so it has never had that effect - left as found: the banner is being
        #retired in favour of the nav bar. See vftGoBack() in R/navigation.R.)
        r$step6FirstRun <- TRUE

        #trigger return to past (return with specific confirm value?)
      }, ignoreInit = TRUE)

      # Observe Map presence ####
      obsEvent_map <- shiny::observeEvent(r$mapPresent, {
        #if there's a map..
        if(r$mapPresent == TRUE){

          #disable agentCheckbox radio buttons (complicated because its a rendered UI)
          if(currentLang == "de"){

            output$agentCheckbox_ui <- shiny::renderUI({


                shiny::radioButtons(shiny::NS(id, "agentCheckbox"), "Welcher Agententyp wird angezeigt",
                                    choices = c("alle" = "1", "Wanderer" = "2", "Radfahrer" = "3", "Hundespaziergänger" = "4", "Jogger" = "5") ,
                                    selected = 1
                )

            })

          }else if(currentLang == "fr"){
            output$agentCheckbox_ui <- shiny::renderUI({

                shiny::radioButtons(shiny::NS(id, "agentCheckbox"), "Type d'agent à afficher",
                                    choices = c("tous" = "1", "Randonneurs" = "2", "Cyclistes" = "3", "Promeneurs de chien" = "4", "Joggeurs" = "5") ,
                                    selected = 1
                )

            })

          }else if(currentLang == "en"){
            output$agentCheckbox_ui <- shiny::renderUI({

              shiny::radioButtons(shiny::NS(id, "agentCheckbox"), "Type d'agent à afficher",
                                  choices = c("all" = "1", "Walkers" = "2", "Cyclists" = "3", "Dog Walkers" = "4", "Joggers" = "5") ,
                                  selected = 1
              )

            })

          }

          #...enable checkboxes
          # shinyjs::enable(id = "agentCheckbox")
          shinyjs::enable(id = "launchSim")
          shinyjs::enable(id = "onlyAOIcheckbox")
          shinyjs::enable(id = "SMcheckbox" )
          shinyjs::enable(id = "startingCheckbox" )
          shinyjs::enable(id = "aoi")
          shinyjs::enable(id =  "ParkingCheckbox")
          shinyjs::enable(id =  "PA_Checkbox")
          shinyjs::enable(id =  "ResidentialCheckbox")
          shinyjs::enable(id = "Bewohnen")
          shinyjs::enable(id = "newVersionsButton")
          shinyjs::enable(id = "imageButton")
          shinyjs::enable(id = "pathsDwnldButton")
          shinyjs::enable(id = "SMDwnldButton")
          #...and take back the two the data does not justify. This branch
          #enables every display control in one go, so it has to run after it.
          applySMState()

          r$agentCheckboxIsDisabled <- FALSE

        }else{

          if(currentLang == "de"){

            #disable agentCheckbox radio buttons
            output$agentCheckbox_ui <- shiny::renderUI({
              shinyjs::disabled(

                shiny::radioButtons(shiny::NS(id, "agentCheckbox"), "Welcher Agententyp wird angezeigt",
                                    choices = c("alle" = "1", "Wanderer" = "2", "Radfahrer" = "3", "Hundespaziergänger" = "4", "Jogger" = "5") ,
                                    selected = 1
                )
              )
            })

          }else if(currentLang == "fr"){
            output$agentCheckbox_ui <- shiny::renderUI({
              shinyjs::disabled(
                shiny::radioButtons(shiny::NS(id, "agentCheckbox"), "Type d'agent à afficher",
                                    choices = c("tous" = "1", "Randonneurs" = "2", "Cyclistes" = "3", "Promeneurs de chien" = "4", "Joggeurs" = "5") ,
                                    selected = 1
                )
              )
            })

          }else if(currentLang == "en"){
            output$agentCheckbox_ui <- shiny::renderUI({
              shinyjs::disabled(
                shiny::radioButtons(shiny::NS(id, "agentCheckbox"), "Agent type to display",
                                    choices = c("all" = "1", "Walkers" = "2", "Cyclists" = "3", "Dog walkers" = "4", "Joggers" = "5") ,
                                    selected = 1
                )
              )
            })

          }

          #select appropriate agentCheckbox
          # shiny::updateRadioButtons(inputId = "agentCheckbox", select = 1)

          #deselect all checkboxes
          shiny::updateCheckboxInput(inputId = "onlyAOIcheckbox", value = FALSE)
          shiny::updateCheckboxInput(inputId = "SMcheckbox", value = FALSE)
          shiny::updateCheckboxInput(inputId = "startingCheckbox", value = FALSE)
          shiny::updateCheckboxInput(inputId = "aoi", value = FALSE)
          shiny::updateCheckboxInput(inputId = "ParkingCheckbox", value = FALSE)
          shiny::updateCheckboxInput(inputId = "PA_Checkbox", value = FALSE)
          shiny::updateCheckboxInput(inputId = "ResidentialCheckbox", value = FALSE)
          shiny::updateCheckboxInput(inputId = "Bewohnen", value = FALSE)

          #disable them
          # shinyjs::disable(id = "agentCheckbox")
          shinyjs::disable(id = "onlyAOIcheckbox")
          shinyjs::disable(id = "SMcheckbox" )
          shinyjs::disable(id = "startingCheckbox" )
          shinyjs::disable(id = "aoi")
          shinyjs::disable(id =  "PA_Checkbox")
          shinyjs::disable(id =  "ParkingCheckbox")
          shinyjs::disable(id =  "ResidentialCheckbox")
          shinyjs::disable(id = "Bewohnen")

          shinyjs::disable(id = "imageButton")
          shinyjs::disable(id = "pathsDwnldButton")

          r$agentCheckboxIsDisabled <- TRUE

        }
      })

      #LAUNCH SIMULATION ####
      #
      #ONE DISPATCH, TWO BARS.
      #
      #This used to be two dispatches - vftPrepareThen() for the network, and
      #then a second future for the ABM inside its callback - so a single click
      #took two places in the FIFO worker queue (behind OTHER USERS, not just
      #behind itself), sent the prepared graph across the process boundary twice
      #more than it had to, and raised and dropped two unrelated progress bars
      #in the middle of one action. Preparation and simulation are one job now;
      #see vftSimulateScenario() in R/simulate_scenario.R.
      #
      #The path network LOAD stays separate, and stays where it was. It is a
      #PROVIDER key: cached in the app-level r$network for the rest of the
      #session, shared with the newVersions page and the nav bar, and paid for at
      #most once per session. vftScenarioNetworkThen() is stage 1 of what
      #vftPrepareThen() used to do in one piece, and the newVersions page still
      #goes through vftPrepareThen() unchanged.
      #
      #This is also the first read, in the whole app, of anything step 4's
      #confirm handler used to build: the AOI letters on the nodes, the parking
      #capacity distributed across them, and the attractivity-weighted edge
      #distances. generatePopulation() wants the first two,
      #determineShortestPath() the third. So the ~30s that used to be spent by
      #every user who confirmed their areas of interest is spent here, by the
      #ones who actually simulate - and only once per SCENARIO, because
      #vftNetworkPrepared() tests the graph's own attribute names and the result
      #is written back into r$networkList[[pos]].
      obsEvent_sim <- shiny::observeEvent(input$launchSim, {

        #disable launch sim button
        shinyjs::disable("launchSim")

        vftDbgCat("TEST16")

        vftDbg("LAUNCH SIMULATION")

        vftScenarioNetworkThen(r, selectedNetwork_position,
                               enable = "launchSim",
                               then = function(scenario){

        #Read from the SCENARIO rather than from r$selectedNetwork_r(), which is
        #the older copy taken when the card was clicked.
        network <- scenario$network
        parking <- scenario$parking

        #TWO BARS OVER ONE QUEUE POSITION. The first covers everything that is
        #DATA - the AOI letters, the parking, the twenty-one weight columns, the
        #population, the adjacency lists and the agents' goals. The second covers
        #the ABM, and is not created until the ABM sends its first message, so
        #there is one bar on screen at a time. See vftProgressPair() in
        #R/async_helpers.R.
        #
        #vftProgressPair, not ipc::AsyncProgress: the latter was the worst site
        #in the app - 385 MB of session state serialised into the ABM worker,
        #against 3.6 MB for the `network` the job actually needs.
        progress <- vftProgressPair(value    = 0,
                                    message  = "Daten werden vorbereitet",
                                    detail   = "Dies sollte weniger als 30 Sekunden dauern",
                                    message2 = "Simulation läuft")
        progPrep <- progress$prep
        progSim  <- progress$sim

        #LAUNCH PROMISE####
        vftFuture({
          vftSimulateScenario(network, finalPolygons, minThresh,
                              parking          = parking,
                              residentDivision = CONST_residentDivision,
                              progPrep         = progPrep,
                              progSim          = progSim)
        }, seed = TRUE, progress = progress)%...>%(function(out){

        ## TREAT PROMISE RESULT ####

        #The prepared network and its parking table - but only when THIS run
        #prepared them. A re-run of an already-prepared card gets NULL for both
        #and the graph never makes the return trip. Written first, because
        #everything below reads r$networkList.
        if(!is.null(out$network))
          r$networkList[[selectedNetwork_position]]$network <- out$network
        if(!is.null(out$parking))
          r$networkList[[selectedNetwork_position]]$parking <- out$parking

        r$selectedNetwork_r(list(
          shiny::isolate(r$networkList[[selectedNetwork_position]]$network)))

        r$result <- out$results
        #fresh simulation result: invalidate cached passageTable + starting points
        r$passageTable <- NULL
        r$startingPointsSf <- NULL

        # gc()
        vftDbg("RESULTS DONE")



        #insert result into networkList
        r$networkList[[selectedNetwork_position]]$pathUsage <- r$result$pathUsage
        r$networkList[[selectedNetwork_position]]$dayPop <- r$result$dayPop

        #update button to reflect presence of pathUsage
        inputid <- r$versionsUI[[selectedNetwork_position]]$inputId_select

        vftDbg(paste0("inputId: ", inputid))

        shinyjs::removeClass(inputid, "noSim")
        shinyjs::addClass(inputid, "withSim")

        # updateActionButton(session = session, inputId = inputid, label = tags$div(
        #   tags$img(src = "noSim.png", height = "120px") ,
        #   tags$text(name, style = "position: absolute;top: 50%;left: 50%;transform: translate(-50%, -50%);"),
        #   width = "120px",  style = "height: 120px; position: relative; text-align: center;  ",
        #   class = "selected") )

        # PLOT SELECTED SIMULATIONS

        vftDbg("Trying to plot:")

        #PLOT OUTPUT####

        # IF NO AGENT TYPE SELECTED,
        #SELECT 1
        # if(is.null(input$agentCheckbox)){
        #   shiny::updateCheckboxGroupInput(inputId = "agentCheckbox",
        #                                   selected = 1)
        # }

        plotPathUsage()

        #enable launch sim button
        shinyjs::enable("launchSim")

        #vftAsyncError() closes BOTH bars of the pair - a job that dies during
        #preparation must not leave the ABM bar armed for the rest of the
        #session. See R/async_helpers.R.
        })%...!%(vftAsyncError(progress, "Agent-Based Model", "launchSim"))

        })  #end vftScenarioNetworkThen(then = ...)

      }, ignoreInit = TRUE)


#observe usage ####
      obsUsage <- shiny::observeEvent(input$onlyAOIcheckbox, {
        if(input$onlyAOIcheckbox == 1){
          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          )|>
            vftClearNetworkLines(group = "paths")

          #determine agent to focus on
          if(input$agentCheckbox == "1"){
            agentTypePassage <- "passageAOI"
          }else if(input$agentCheckbox == "2"){
            agentTypePassage <- "passageWalkAOI"
          }else if(input$agentCheckbox == "3"){
            agentTypePassage <- "passageBikeAOI"
          }else if(input$agentCheckbox == "4"){
            agentTypePassage <- "passageDogAOI"
          }else if(input$agentCheckbox == "5"){
            agentTypePassage <- "passageJogAOI"
          }
          passageTable <- getPassageTable()
          pal <- leaflet::colorNumeric(c("darkgrey", colorRampPalette(c("lightblue", "steelblue", "#182db5", "#37046e"))(max(passageTable$passageAOI)-1)), domain = c(0,max(passageTable$passageAOI)) )
          #drawn through WebGL: addPolylines() encodes every edge into
          #nested JSON on the shared main thread and scales with edge
          #count. See vftAddNetworkLines() in data_paths.R; VFT_GL=0
          #restores the original addPolylines call.
          proxy |> vftAddNetworkLines(passageTable,
                             values    = passageTable[,agentTypePassage,drop = TRUE],
                             weightRef = passageTable[,"passageAOI",drop = TRUE],
                             pal = pal, group = "paths", pane = "layer2")
        }else{
          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          )|>
            vftClearNetworkLines(group = "paths")

          #determine agent to focus on
          if(input$agentCheckbox == "1"){
            agentTypePassage <- "passage"
          }else if(input$agentCheckbox == "2"){
            agentTypePassage <- "passageWalk"
          }else if(input$agentCheckbox == "3"){
            agentTypePassage <- "passageBike"
          }else if(input$agentCheckbox == "4"){
            agentTypePassage <- "passageDog"
          }else if(input$agentCheckbox == "5"){
            agentTypePassage <- "passageJog"
          }
          passageTable <- getPassageTable()
          pal <- leaflet::colorNumeric(c("darkgrey", colorRampPalette(c("lightblue", "steelblue", "#182db5", "#37046e"))(max(passageTable$passage)-1)), domain = c(0,max(passageTable$passage)) )

          #drawn through WebGL: addPolylines() encodes every edge into
          #nested JSON on the shared main thread and scales with edge
          #count. See vftAddNetworkLines() in data_paths.R; VFT_GL=0
          #restores the original addPolylines call.
          proxy |> vftAddNetworkLines(passageTable,
                             values    = passageTable[,agentTypePassage,drop = TRUE],
                             weightRef = passageTable[,"passage",drop = TRUE],
                             pal = pal, group = "paths", pane = "layer2")
        }

      }, ignoreInit = TRUE)

      #observe Agent checkbox ####
      obsAgent <- shiny::observeEvent(input$agentCheckbox, {
        vftDbg("OBS AGENT")
        if(r$agentCheckboxIsDisabled == FALSE){

          if(input$onlyAOIcheckbox == 1){
            proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
            )|>
              vftClearNetworkLines(group = "paths")

            #determine agent to focus on
            if(input$agentCheckbox == "1"){
              agentTypePassage <- "passageAOI"
            }else if(input$agentCheckbox == "2"){
              agentTypePassage <- "passageWalkAOI"
            }else if(input$agentCheckbox == "3"){
              agentTypePassage <- "passageBikeAOI"
            }else if(input$agentCheckbox == "4"){
              agentTypePassage <- "passageDogAOI"
            }else if(input$agentCheckbox == "5"){
              agentTypePassage <- "passageJogAOI"
            }
            passageTable <- getPassageTable()
            pal <- leaflet::colorNumeric(c("darkgrey", colorRampPalette(c("lightblue", "steelblue", "#182db5", "#37046e"))(max(passageTable$passageAOI)-1)), domain = c(0,max(passageTable$passageAOI)) )
            #drawn through WebGL: addPolylines() encodes every edge into
            #nested JSON on the shared main thread and scales with edge
            #count. See vftAddNetworkLines() in data_paths.R; VFT_GL=0
            #restores the original addPolylines call.
            proxy |> vftAddNetworkLines(passageTable,
                               values    = passageTable[,agentTypePassage,drop = TRUE],
                               weightRef = passageTable[,"passageAOI",drop = TRUE],
                               pal = pal, group = "paths", pane = "layer2")
          }else{

            #determine agent to focus on
            if(input$agentCheckbox == "1"){
              agentTypePassage <- "passage"
            }else if(input$agentCheckbox == "2"){
              agentTypePassage <- "passageWalk"
            }else if(input$agentCheckbox == "3"){
              agentTypePassage <- "passageBike"
            }else if(input$agentCheckbox == "4"){
              agentTypePassage <- "passageDog"
            }else if(input$agentCheckbox == "5"){
              agentTypePassage <- "passageJog"
            }
            proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
            )|>
              vftClearNetworkLines(group = "paths")

            passageTable <- getPassageTable()
            pal <- leaflet::colorNumeric(c("darkgrey", colorRampPalette(c("lightblue", "steelblue", "#182db5", "#37046e"))(max(passageTable$passage)-1)), domain = c(0,max(passageTable$passage)) )
            #drawn through WebGL: addPolylines() encodes every edge into
            #nested JSON on the shared main thread and scales with edge
            #count. See vftAddNetworkLines() in data_paths.R; VFT_GL=0
            #restores the original addPolylines call.
            proxy |> vftAddNetworkLines(passageTable,
                               values    = passageTable[,agentTypePassage,drop = TRUE],
                               weightRef = passageTable[,"passage",drop = TRUE],
                               pal = pal, group = "paths", pane = "layer2")
          }

        }

      }, ignoreInit = TRUE)

      #observe AOI ####
      obsAOI <- shiny::observeEvent(input$aoi, {
        if(input$aoi == TRUE){
          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          )|>leaflet::addPolygons(data = finalPolygons,
                                        weight = 3,
                                        color = "green",
                                        fillColor = "green",
                                        fill = TRUE,
                                        stroke = TRUE,
                                        options = leaflet::pathOptions(pane = "layer1"),
                                        opacity = 0.3,
                                        fillOpacity = 0.1,
                                        group = "AOI")
        }else{
          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          ) |> leaflet::clearGroup("AOI")
        }

      }, ignoreInit = TRUE)

      #observe PA ####
      obsPA <- shiny::observeEvent(input$PA_Checkbox, {
        if(input$PA_Checkbox == TRUE){

          # Define color palette for each value
          pal <- leaflet::colorFactor(
            palette = c("#a2e08a", "#4a8636", "#105200"),
            levels = c(3, 2, 1)
          )

          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          )|>leaflet::addPolygons(data = shp_PA,
                                   weight = 5,
                                   color = ~pal(PA_type),
                                   fillColor = "white",
                                   fill = TRUE,
                                   stroke = TRUE,
                                   options = leaflet::pathOptions(pane = "layer1"),
                                   opacity = 1,
                                   fillOpacity = 0.2,
                                   group = "PA")
        }else{
          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          ) |> leaflet::clearGroup("PA")
        }

      }, ignoreInit = TRUE)

      #observe SM ####
      obsSM <- shiny::observeEvent(input$SMcheckbox, {
        #### no matrix: the switch is an OFFER, not a display toggle ####
        #
        #Step 2 is skippable - the bar offers step 3 as soon as the perimeter
        #exists - so step 5 can legitimately be reached with no sensitivity
        #matrix at all. Rather than lock the user out of the whole step (which
        #is what listing SM_pres in step 5's `needs` used to do), the step opens
        #and only this one overlay is unavailable. Ticking it asks whether to go
        #and make one; the tick itself is undone either way, because there is
        #nothing to draw.
        if(isTRUE(as.logical(input$SMcheckbox)) && is.null(SM_pres)){
          shiny::updateCheckboxInput(inputId = "SMcheckbox", value = FALSE)
          smAskCreate()
          return(invisible(NULL))
        }

        vftDbg("OBS SM")

        if(smOn()){
          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          )|> leaflet::addRasterImage(raster::raster(SM_pres),
                                       colors = SMcolors,
                                          opacity = 1,
                                          group = "SM")

        }else{
          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          )|>leaflet::clearGroup(group = "SM")
        }


      }, ignoreInit = TRUE)

      obsParking <- shiny::observeEvent(input$ParkingCheckbox, {
        vftDbg("OBS PARKING")

        if(length(r$networkList[[selectedNetwork_position]]$parking) > 0){

          if(input$ParkingCheckbox == TRUE){
            proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet")|>
              leaflet::addPolygons(data = sf::st_geometry(r$networkList[[selectedNetwork_position]]$parking), stroke = TRUE, fill = TRUE,
                                   color = "steelblue", opacity = 1, fillOpacity = 0.3, group = "parking")
          }else{
            proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet")|>
              leaflet::clearGroup("parking")
          }
        }

      }, ignoreInit = TRUE)

      obsResidential <- shiny::observeEvent(input$ResidentialCheckbox, {

        if(length(r$networkList[[selectedNetwork_position]]$residential) > 0){
          if(input$ResidentialCheckbox == TRUE){
            proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet")|>
              leaflet::addPolygons(data = sf::st_geometry(r$networkList[[selectedNetwork_position]]$residential), stroke = TRUE, fill = TRUE,
                                   color = "#8a722b", opacity = 1, fillOpacity = 0.3, group = "residential")
          }else{
            proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet")|>
              leaflet::clearGroup("residential")
          }
        }

      }, ignoreInit = TRUE)

      #observe agent starting points ####
      obsAgentStart <- shiny::observeEvent(input$startingCheckbox, {
        startingPoints <- sf::st_coordinates(getStartingPoints())

        if(input$startingCheckbox == TRUE){
          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          ) |> leaflet::addCircleMarkers(lng = startingPoints[,"X"], lat = startingPoints[,"Y"] , group = "startingPoints",
                                                   color = "red", fill = FALSE, stroke = TRUE, opacity = 1,
                                                   options = leaflet::markerOptions(pane = "layer1"), weight = 2,
                                                   radius = 3)
        }else{
          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          ) |> leaflet::clearGroup("startingPoints")
        }

      }, ignoreInit = TRUE)

      #GENERATE FINAL TIFF ####
      #generate images
      obsGenImage <- shiny::observeEvent(input$imageButton, {

        #Modal asking for image name
        shiny::showModal(
          shiny::modalDialog(shiny::textInput(NS(id,"nameInput"), label = i18n()$t("Bild Name:") ),
            footer = shiny::tagList(shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("Stornieren") ),
                       shiny::actionButton(inputId = shiny::NS(id, "confirmName"), label = i18n()$t("Name bestätigen"), style = "background-color:#006268; color:#ffffff"  ) )
          )
        )

        #get map bounds from leaflet
        mapBounds <- input$mapAreaLeaflet_bounds

        vftDbg(paste0("mapBounds: ", mapBounds))

        vftDbgCat("START TIFF\n")

        #Generate image (A3 format)

        r$tiffName <- gsub("[ ]|:|[.]|-", "", paste0("TIFF_",Sys.time()))
        r$tiffFile <- paste0(tempdir(), "/",r$tiffName)

        vftDbgCat(paste0("file location: ",r$tiffFile) )


        tiff(filename = r$tiffFile,  compression = "lzw", height = 29.7, width = 42, units = "cm", res = 300)
        #determine layout
        #determine how many legends (parking, residential and starting points are combined)
        legendNb <- sum(
                        smOn(),
                        input$startingCheckbox,
                        input$PA_Checkbox,
                        (input$aoi |
                        input$ParkingCheckbox |
                        input$ResidentialCheckbox )
                        ) + 1 #usage always present

        #determine layout matrix based on legendNb
        mat <- cbind(rbind(matrix(rep(1, legendNb), nrow = legendNb),
                            matrix(rep(1, 2), nrow = 2 ) ),
                     rbind(matrix(2:(legendNb+1),nrow = legendNb),
                           matrix(rep(legendNb+2, 2), nrow = 2 ) ),
                     rbind(matrix(rep(legendNb+3, legendNb), nrow = legendNb),
                            matrix(rep(legendNb+3, 2), nrow = 2) ) )

        vftDbgCat("SMCheckbox\n")


        if(!smOn()){
          layout(mat,
                 widths = c(5, 2.5, 0))
        }else{
          layout(mat,
                 widths = c(5, 1.5, 1))
        }

        passageTable <- getPassageTable()

        vertexTable <- dplyr::as_tibble(r$result$pathUsage |> tidygraph::activate(nodes) )
        # startingPoints <- sf::st_geometry(sf::st_coordinates(sf::st_as_sf( vertexTable[vertexTable$nodeID %in% r$result$dayPop$startV , ]) ) )

        vftDbgCat("AOI\n")

        if(input$onlyAOIcheckbox == 1){

          #determine agent to focus on
          if(input$agentCheckbox == "1"){
            agentTypePassage <- "passageAOI"
          }else if(input$agentCheckbox == "2"){
            agentTypePassage <- "passageWalkAOI"
          }else if(input$agentCheckbox == "3"){
            agentTypePassage <- "passageBikeAOI"
          }else if(input$agentCheckbox == "4"){
            agentTypePassage <- "passageDogAOI"
          }else if(input$agentCheckbox == "5"){
            agentTypePassage <- "passageJogAOI"
          }

        }else{

          #determine agent to focus on
          if(input$agentCheckbox == "1"){
            agentTypePassage <- "passage"
          }else if(input$agentCheckbox == "2"){
            agentTypePassage <- "passageWalk"
          }else if(input$agentCheckbox == "3"){
            agentTypePassage <- "passageBike"
          }else if(input$agentCheckbox == "4"){
            agentTypePassage <- "passageDog"
          }else if(input$agentCheckbox == "5"){
            agentTypePassage <- "passageJog"
          }

        }
        vftDbgCat("SM\n")
        vftDbgCat(paste0("input$SM: ", input$SMcheckbox) )
        # if(input$SMcheckbox == FALSE){
          pal <- leaflet::colorNumeric(c("darkgrey",colorRampPalette(c("lightblue", "steelblue", "#182db5", "#37046e"))(max(passageTable$passage)-1) ), domain = c(0,max(passageTable$passage)) )
        # }else{
        #   cat(file = stderr(), "pal\n")
        #
        #   pal <- leaflet::colorNumeric(c("grey",colorRampPalette(c( "darkgrey", "black"))(max(passageTable$passage)-1) ), domain = c(0,max(passageTable$passage)) )
        # }
        vftDbgCat("basemap\n")


        basemap <- maptiles::get_tiles(x = terra::ext(c(mapBounds[[4]], mapBounds[[2]], mapBounds[[3]], mapBounds[[1]])),
                                       provider = "OpenStreetMap", cachedir = vft_tileCacheDir)

        vftDbgCat("plot basemap 1\n")

        vftDbgCat(paste0("basemap: ", basemap) )

        # terra::plot(terra::rast(matrix(1, nrow = 10, ncol = 10), ext = terra::ext(basemap)), col = "white")
        terra::plot(basemap,  alpha = 0.5, ext = terra::ext(c(mapBounds[[4]], mapBounds[[2]], mapBounds[[3]], mapBounds[[1]]) ) )

        vftDbgCat("plot basemap 2\n")

        terra::plot(basemap,1, col = "white",  alpha = 0.5, add = TRUE, legend = FALSE)

        vftDbgCat("aoi\n")

        if(input$aoi == TRUE ){
          plot(sf::st_geometry(finalPolygons), col = "#29ed1f30", border = "#0b630650", lwd = 5, add = TRUE)
        }

        vftDbgCat("parking\n")


        if(input$ParkingCheckbox == TRUE & length(r$networkList[[selectedNetwork_position]]$parking) > 0){
          plot(sf::st_geometry(r$networkList[[selectedNetwork_position]]$parking ),
               col = "#3289a880", border = "#3289a8", lwd = 3, add = TRUE)
        }
        vftDbgCat("residential\n")

        if(input$ResidentialCheckbox == TRUE & length(r$networkList[[selectedNetwork_position]]$residential) > 0){
          plot(sf::st_geometry(r$networkList[[selectedNetwork_position]]$residential),
               col = "#8a722b80", border = "#8a722b", lwd = 3, add = TRUE)
        }

        vftDbgCat("sm\n")

        if(smOn()){
          terra::plot(SM_pres,alpha = 0.4, add = TRUE, col = SMcolors, legend = FALSE)
        }

        plot(x = sf::st_geometry(passageTable), col = pal(as.numeric(passageTable[,agentTypePassage,drop = TRUE])),
                    lwd = 2 + (as.numeric(passageTable[,agentTypePassage,drop = TRUE]) / max(as.numeric(passageTable[,"passageAOI",drop = TRUE])) ) *2,
                    xlim = c(mapBounds[[2]], mapBounds[[4]]),
                    ylim = c(mapBounds[[3]], mapBounds[[1]]) , add = TRUE, legend = FALSE)


        vftDbgCat("starting\n")

        if(input$startingCheckbox){
          startingPoints <- getStartingPoints()

          terra::plot(x = sf::st_geometry(startingPoints), add = TRUE, col = "red")
        }

        plot(x = shape, border = "black", lwd = 3, add = TRUE, col = NA)

        terra::sbar(1, xy = "bottomright", lonlat = TRUE, type = "bar", label = "kilometers")
        terra::north(xy = "bottomleft")

        #test (not run)
        # leaflet::previewColors(pa<l, values = passageTable$passage)
        # pal <- leaflet::colorNumeric(c("grey",colorRampPalette(c("yellow3", "orange2", "red2", "purple"))(max(passageTable$passage)-1) ), domain = c(0,max(passageTable$passage)) )
        #
        # map <- leaflet::leaflet(data = passageTable, options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE), height = 500 ) |>
        #   leaflet::addMapPane("layer_SM", zIndex = 415)|>
        #   leaflet::addMapPane("layer1", zIndex = 410)|> leaflet::addMapPane("layer2", zIndex = 420)|> leaflet::addMapPane("layer3", zIndex = 450) |>
        #   leaflet::addProviderTiles("OpenStreetMap.CH", options = leaflet::providerTileOptions(opacity = 0.5, zIndex = 400)) |>
        #   leaflet::addPolylines(stroke = TRUE,
        #                         weight = 2 + (as.numeric(passageTable[,agentTypePassage,drop = TRUE]) / max(as.numeric(passageTable[,"passageAOI",drop = TRUE])) ) *2,
        #                         color = ~pal(as.numeric(passageTable[,agentTypePassage,drop = TRUE])),
        #                         fill = FALSE,
        #                         opacity = 1,
        #                         options = leaflet::pathOptions(pane = "layer2"),
        #                         group = "paths")|>
        #   leaflet::addPolygons(data = finalPolygons,
        #                        weight = 3,
        #                        color = "green",
        #                        fillColor = "green",
        #                        fill = TRUE,
        #                        stroke = TRUE,
        #                        options = leaflet::pathOptions(pane = "layer1"),
        #                        opacity = 0.3,
        #                        fillOpacity = 0.1)|>
        #
        #   leaflet::addPolygons(data = sf::st_geometry(r$networkList[[selectedNetwork_position]]$parking), stroke = TRUE, fill = TRUE,
        #                        fillColor = "steelblue", opacity = 1, fillOpacity = 0.3)|>
        #   leaflegend::addLegendImage(position = "topright",title = "Formen:", images = c("www/AOI.png", "www/parking.png", "www/Start.png"), labels = c("Zielgebiete","parkplatz", "Startposition des Agenten"),
        #                              labelStyle = "font-size: 15px; text-align: left")|>
        #   leaflet::addLegend(title = "Wegnutzung:", position = "topright", labels = c("kein", "niedrigste", "mittlere", "hohe", "höchste") , colors = c("grey", "#dec402", "#de9802", "#e00417", "purple"))|>
        #
        #   leaflet.extras::setMapWidgetStyle(list(background = "white"))
        #
        # map <- map |> leaflet::addPolygons(data= sf::st_zm(sf::st_transform(shape, "epsg:4326"), drop = TRUE, what = "ZM" ), stroke = TRUE, fill = FALSE, color = "black",
        #                                     weight = 5, options = leaflet::pathOptions(pane = "layer2"))
        #
        # if(input$startingCheckbox == TRUE){
        #   map <- map |> leaflet::addCircleMarkers(lng = startingPoints[,"X"], lat = startingPoints[,"Y"] , group = "startingPoints",
        #                                            color = "red", fill = FALSE, stroke = TRUE, opacity = 1,
        #                                            options = leaflet::markerOptions(pane = "layer1"), weight = 2,
        #                                            radius = 3)
        # }
        #ADD LEGENDS ####
        # map legend ####
        vftDbgCat("LEGENDS\n")

        par(mar = c(0, 0, 2, 0))
        #paths
        plot.new()
        # if(input$SMcheckbox == FALSE){
        if(r$currentLang == "de"){
          legend("topleft",
                 legend = c("kein", "niedrigste", "mittlere", "hohe", "höchste"),
                 col = c("darkgrey", "lightblue", "steelblue", "#182db5", "#37046e"),
                 lty = 1,
                 lwd = 3,
                 cex = 2,
                 bty = "n",
                 inset = 0)


          mtext("Wegnutzung:", cex = 1.5,  adj = 0)

          par(mar = c(0, 0, 0, 0))

          #starting points
          if(input$startingCheckbox == TRUE){
            plot.new()

            legend("topleft",
                   legend = "Startposition",
                   col = "red",
                   pch = 1,
                   cex = 2,
                   bty = "n",
                   inset = 0)
            mtext("Agenten:", cex = 1.5,  adj = 0)


          }

          #parking, starting, residential, aoi
          if(sum(input$aoi,
                 input$ParkingCheckbox,
                 input$ResidentialCheckbox ) > 0){
            plot.new()

            colors = c()
            borders = c()
            legends = c()
            points = c()


            #aoi
            if(input$aoi == 1){
              colors = c(colors, "#29ed1f30")
              borders = c(borders, "#0b630650")
              legends = c(legends, "Zielgebiete")
              points = c(points, 2)
            }


            #parking
            if(input$ParkingCheckbox == 1){
              colors = c(colors, "#3289a880")
              borders = c(borders, "#3289a8")
              legends = c(legends, "Parkplatz")
              points = c(points, 2)
            }

            #residential
            if(input$ResidentialCheckbox == 1){
              colors = c(colors, "#8a722b80")
              borders = c(borders, "#8a722b")
              legends = c(legends, "Neue Wohngebiete")
              points = c(points, 2)
            }

            legend("topleft",
                   legend = legends,
                   pt.bg = colors,
                   col = borders,
                   pch = 22,
                   pt.lwd = 3,
                   pt.cex = 3,
                   cex = 2,
                   bty = "n",
                   inset = 0)

            mtext("Formen:", cex = 1.5,  adj = 0)
          }

          # protected areas
          if(input$PA_Checkbox == 1){
            plot.new()
            legend("topleft",
                   legend = c("streng", "umfassend", "teilweise"),
                   border = c("#a2e08a", "#4a8636", "#105200"),
                   cex = 2,
                   bty = "n",
                   inset = 0)
            mtext("Schutzgebiete:", cex = 1.5,  adj = 0)
          }

          #SM
          if(smOn()){
            plot.new()
            legend("topleft",
                   legend = c("niedrigste", "mittlere", "hohe", "höchste"),
                   fill = c("#FFD700FF", "#FF8800FF", "#F40000FF", "#8B0000FF"),
                   border = c("#FFD700FF", "#FF8800FF", "#F40000FF", "#8B0000FF"),
                   cex = 2,
                   bty = "n",
                   inset = 0)
            mtext(ifelse(minCutThresh != 0, paste0("Sensitivitätsmatrix\n(oberer ", minCutThresh,"%):"),
                         "Sensitivitätsmatrix:"), cex = 1.5,  adj = 0)
          }

          # Add Map information (scale bare etc.)
          plot.new()




          # species names (if SM active)####
          if(smOn()){
            plot.new()

            #write species names on right hand side
            lineSpace <- 1
            text(0, lineSpace, labels = "Enthaltene Arten:", adj = 0, font = 2)
            for(sp in species){
              lineSpace <- lineSpace - 0.02
              text(0, lineSpace, labels = paste(sp, sep = "\n"), adj = 0, font = 3)
            }



          }

        }else if(r$currentLang == "fr"){

          legend("topleft",
                 legend = c("aucun", "minimum", "moyen", "haut", "maximum"),
                 col = c("darkgrey", "lightblue", "steelblue", "#182db5", "#37046e"),
                 lty = 1,
                 lwd = 3,
                 cex = 2,
                 bty = "n",
                 inset = 0)



          mtext("Usage des chemins:", cex = 1.5,  adj = 0)

          par(mar = c(0, 0, 0, 0))

          #starting points
          if(input$startingCheckbox == TRUE){
            plot.new()

            legend("topleft",
                   legend = "Position de départ",
                   col = "red",
                   pch = 1,
                   cex = 2,
                   bty = "n",
                   inset = 0)
            mtext("Agenten:", cex = 1.5,  adj = 0)


          }

          #parking, starting, residential, aoi
          if(sum(input$aoi,
                 input$ParkingCheckbox,
                 input$ResidentialCheckbox ) > 0){
            plot.new()

            colors = c()
            borders = c()
            legends = c()
            points = c()


            #aoi
            if(input$aoi == 1){
              colors = c(colors, "#29ed1f30")
              borders = c(borders, "#0b630650")
              legends = c(legends, "Zone cible")
              points = c(points, 2)
            }

            #parking
            if(input$ParkingCheckbox == 1){
              colors = c(colors, "#3289a880")
              borders = c(borders, "#3289a8")
              legends = c(legends, "Parking")
              points = c(points, 2)
            }

            #residential
            if(input$ResidentialCheckbox == 1){
              colors = c(colors, "#8a722b80")
              borders = c(borders, "#8a722b")
              legends = c(legends, "Nouvelle zone d'habitation")
              points = c(points, 2)
            }

            legend("topleft",
                   legend = legends,
                   pt.bg = colors,
                   col = borders,
                   pch = 22,
                   pt.lwd = 3,
                   pt.cex = 3,
                   cex = 2,
                   bty = "n",
                   inset = 0)

            mtext("Formen:", cex = 1.5,  adj = 0)
          }

          # protected areas
          if(input$PA_Checkbox == 1){
            plot.new()
            legend("topleft",
                   legend = c("stricte", "compréhensif", "partiel"),
                   border = c("#a2e08a", "#4a8636", "#105200"),
                   cex = 2,
                   bty = "n",
                   inset = 0)
            mtext("Zones protégées:", cex = 1.5,  adj = 0)
          }

          #SM
          if(smOn()){
            plot.new()
            legend("topleft",
                   legend = c("minimum", "moyen", "haut", "maximum"),
                   fill = c("#FFD700FF", "#FF8800FF", "#F40000FF", "#8B0000FF"),
                   border = c("#FFD700FF", "#FF8800FF", "#F40000FF", "#8B0000FF"),
                   cex = 2,
                   bty = "n",
                   inset = 0)
            mtext(ifelse(minCutThresh != 0, paste0("Matrice de sensibilité\n(", minCutThresh,"% supérieurs):"),
                         "Matrice de sensibilité:"), cex = 1.5,  adj = 0)
          }

          # Add Map information (scale bare etc.)
          plot.new()




          # species names (if SM active)####
          if(smOn()){
            plot.new()

            #write species names on right hand side
            lineSpace <- 1
            text(0, lineSpace, labels = "Espèces incluses:", adj = 0, font = 2)
            for(sp in species){
              lineSpace <- lineSpace - 0.02
              text(0, lineSpace, labels = paste(sp, sep = "\n"), adj = 0, font = 3)
            }



          }
        }else if(r$currentLang == "en"){

          legend("topleft",
                 legend = c("none", "minimum", "medium", "high", "maximum"),
                 col = c("darkgrey", "lightblue", "steelblue", "#182db5", "#37046e"),
                 lty = 1,
                 lwd = 3,
                 cex = 2,
                 bty = "n",
                 inset = 0)



          mtext("Path usage:", cex = 1.5,  adj = 0)

          par(mar = c(0, 0, 0, 0))

          #starting points
          if(input$startingCheckbox == TRUE){
            plot.new()

            legend("topleft",
                   legend = "Agent starting points",
                   col = "red",
                   pch = 1,
                   cex = 2,
                   bty = "n",
                   inset = 0)
            mtext("Agents:", cex = 1.5,  adj = 0)


          }

          #parking, starting, residential, aoi
          if(sum(input$aoi,
                 input$ParkingCheckbox,
                 input$ResidentialCheckbox ) > 0){
            plot.new()

            colors = c()
            borders = c()
            legends = c()
            points = c()


            #aoi
            if(input$aoi == 1){
              colors = c(colors, "#29ed1f30")
              borders = c(borders, "#0b630650")
              legends = c(legends, "Area of Interest")
              points = c(points, 2)
            }

            #parking
            if(input$ParkingCheckbox == 1){
              colors = c(colors, "#3289a880")
              borders = c(borders, "#3289a8")
              legends = c(legends, "Parking")
              points = c(points, 2)
            }

            #residential
            if(input$ResidentialCheckbox == 1){
              colors = c(colors, "#8a722b80")
              borders = c(borders, "#8a722b")
              legends = c(legends, "New Residential Area")
              points = c(points, 2)
            }

            legend("topleft",
                   legend = legends,
                   pt.bg = colors,
                   col = borders,
                   pch = 22,
                   pt.lwd = 3,
                   pt.cex = 3,
                   cex = 2,
                   bty = "n",
                   inset = 0)

            mtext("Shapes:", cex = 1.5,  adj = 0)
          }

          # protected areas
          if(input$PA_Checkbox == 1){
            plot.new()
            legend("topleft",
                   legend = c("strict", "comprehensive", "partial"),
                   border = c("#a2e08a", "#4a8636", "#105200"),
                   cex = 2,
                   bty = "n",
                   inset = 0)
            mtext("Protected areas:", cex = 1.5,  adj = 0)
          }

          #SM
          if(smOn()){
            plot.new()
            legend("topleft",
                   legend = c("minimum", "medium", "high", "maximum"),
                   fill = c("#FFD700FF", "#FF8800FF", "#F40000FF", "#8B0000FF"),
                   border = c("#FFD700FF", "#FF8800FF", "#F40000FF", "#8B0000FF"),
                   cex = 2,
                   bty = "n",
                   inset = 0)
            mtext(ifelse(minCutThresh != 0, paste0("Sensitivity Matrix\n(Top ", minCutThresh,"%):"),
                         "Sensitivity Matrix:"), cex = 1.5,  adj = 0)
          }

          # Add Map information (scale bare etc.)
          plot.new()




          # species names (if SM active)####
          if(smOn()){
            plot.new()

            #write species names on right hand side
            lineSpace <- 1
            text(0, lineSpace, labels = "Included species:", adj = 0, font = 2)
            for(sp in species){
              lineSpace <- lineSpace - 0.02
              text(0, lineSpace, labels = paste(sp, sep = "\n"), adj = 0, font = 3)
            }



          }
        }

        # }else{
        #   legend("topleft",
        #          legend = c("niedrigste","mittlere",  "höchste"),
        #          col = c("grey", "gray50", "black"),
        #          lty = 1,
        #          lwd = 3,
        #          cex = 2,
        #          bty = "n",
        #          inset = 0)
        # }



        dev.off()

vftDbgCat("FINISHED TIFF\n")

        #create final plot

        #create legend bar

        #output tif to download folder


#         #clean up
#         obsEvent_sim$destroy()
#         obsFinalConfirm$destroy()
#         obsNewVersions$destroy()
#
#         #TO DO FINAL INTERFACE####
# print("FINAL CONFIRM")
#         r$confirm <- input$confirmButton5
#         return(list(pathUsage = shiny::reactive({r$pathUsage}), networkList = shiny::reactive({r$networkList}), confirm = shiny::reactive({r$confirm}), newVersions = shiny::reactive({input$newVersionsButton}), trigger = shiny::reactive(r$triggerStp6), versionsUI = shiny::reactive(versionsUI) ) )

      }, ignoreInit = TRUE)

      #footer confirm button
      # observe tiff download click ####
      obsConfirmName <- shiny::observeEvent(input$confirmName, {
        shiny::removeModal()

        shinyjs::click("downloadTIFF", asis = FALSE)

      }, ignoreInit = TRUE)

      # observe paths download click ####
      obsDownloadPaths <- shiny::observeEvent(input$pathsDwnldButton, {

        shinyjs::click("downloadPaths", asis = FALSE)

      }, ignoreInit = TRUE)

      # observe SM download click ####
      obsDownloadSM <- shiny::observeEvent(input$SMbutton, {
        #writeRaster(NULL) aborts out of a download handler and the browser gets
        #an error page. The button is disabled without a matrix; this is the
        #same refusal on the server side, for the console-fired click.
        if(is.null(SM_pres)){
          smAskCreate()
          return(invisible(NULL))
        }
        shinyjs::click("downloadSM", asis = FALSE)

      }, ignoreInit = TRUE)


      # GO TO VERSIONS TAB ####
      obsNewVersions <- shiny::observeEvent(input$newVersionsButton, {
        #the flag app_server's handler tests before it navigates. enter() clears
        #it again on the way back in, so a return does not read as a fresh click.
        r$triggerStp6 <- 1

        #The version cards and their observers used to be cleared here, on the way
        #OUT. They are cleared by enter(), on the way IN, which does the same job
        #for the newVersions round trip and also covers leaving this step by the
        #nav bar - which does not come through here at all.

        #no confirm button pressed
        r$confirm <- 0
      }, ignoreInit = TRUE)



      vftDbgCat("TEST17")

    #GENERATE VERSION IMAGES ####
    #Per VISIT: every entry in versionsUI gets a card insertUI'd into
    ##placeholder_step5 and an observer of its own. enter() clears both before
    #calling this, or a second visit shows every version twice.
    #
    #It also SELECTS a card, and that half is the point of the function rather
    #than an afterthought. The card is inserted wearing the green border
    #(updateVersions(), above), so everything a click on it would have done has to
    #happen here too or the border is the only thing that is true: the module
    #comes back with `selectedNetwork_position` pointing somewhere else and the
    #map still showing whatever it showed last, which is what "selected but not
    #really" looked like on the way back into this step.
    #
    #Same order and same writes as the select observer in updateVersions() - the
    #network, the position, the result, the two caches - and then ONE
    #plotPathUsage(). enter() used to call plotPathUsage() itself before this
    #function ran, which made two draws per entry: the first built a whole new
    #leaflet widget (r$mapPresent is NULL at that point) and the second, finding
    #r$mapPresent TRUE again, went down the leafletProxy branch and cleared and
    #re-added the network on a map that Shiny was replacing in the same flush.
    #The draw that survived was the one whose WebGL layer had been attached to a
    #container the browser had not yet sized - a base map and a legend with no
    #simulation on them. There is one draw now, and with r$mapPresent left NULL by
    #enter() it is a full render rather than a proxy update.
    generateVersionImages <- function(){
    if(length(r$versionsUI) > 0){

      #which card to come back with selected. r$selectedVersion is the scenario
      #the user last clicked, HERE or on the newVersions page - the two share the
      #key - and it resolves to the first card when it names a version that is no
      #longer in the list. Clamped to networkList as well: the select observer
      #refuses a position it cannot find a network for, and so must this.
      pos <- vftVersionPosition(r$versionsUI, r$selectedVersion)
      if(pos > length(r$networkList)){
        vftDbg("ERROR: less networks than version buttons - falling back to the original")
        pos <- 1
      }

      for(i in 1:length(r$versionsUI) ){
        updateVersions(r$versionsUI[[i]]$name, r$versionsUI[[i]]$inputId_select, r$versionsUI[[i]]$id_ui_name,
                       position = i, selected = (i == pos))



      }

      #the network the rest of the module works against
      selectedNetwork_position <<- pos

      #the card whose border is green, for the select observer to un-green when
      #the user picks another one. Set BEFORE the draw, so that an observer woken
      #by plotPathUsage() cannot read the NULL enter() left here.
      r$lastSelectedImage <- r$versionsUI[[pos]]$inputId_select

      #write the resolved name back: `pos` may be the fallback rather than what
      #was remembered, and leaving a name that no longer exists in the key would
      #hand the newVersions page the same dead reference to resolve again.
      r$selectedVersion <- r$versionsUI[[pos]]$name

      r$selectedNetwork_r( list(r$networkList[[pos]]$network) )
      r$result$pathUsage <- r$networkList[[pos]]$pathUsage
      r$result$dayPop    <- r$networkList[[pos]]$dayPop
      #pathUsage (re)assigned: invalidate cached passageTable + starting points
      r$passageTable <- NULL
      r$startingPointsSf <- NULL

      vftDbg(paste0("ENTER: selecting version ", pos, " (", r$versionsUI[[pos]]$name, ")"))
      vftDbg(r$lastSelectedImage)

      plotPathUsage()
    }
    }

    #### enter(): everything that happens per VISIT rather than per session ####
    #
    # Called by vftGoToStep() on every return to this step - including the return
    # from newVersions, which is the busiest path in the app - and once here at
    # the end of construction, so the first visit and the fifth run the same code.
    #
    # vftModuleEnterFn() supplies the two properties this body must have and
    # neither of which is visible in it: the module's own session as the default
    # reactive domain (or the shinyjs:: and update*Input() calls below silently
    # address unnamespaced controls that do not exist), and isolate() around the
    # whole body (or the observers enter() is called from - among them Stage 4's
    # provider observe(), which is not isolated - take a dependency on values
    # enter() itself assigns, and this one assigns r$networkList). See R/modules.R.
    enter <- vftModuleEnterFn(session, function(){

      #--- 1. refresh the snapshots the rest of this module reads
      networkList     <<- .rx$networkList()
      SM_pres         <<- .rx$SM_pres()
      SMcolors        <<- .rx$SMcolors()
      shape           <<- .rx$shape()
      currentLang     <<- .rx$currentLang()
      isFirstRun_stp6 <<- isTRUE(as.logical(.rx$isFirstRun_stp6()))
      finalPolygons   <<- .rx$finalPolygons()
      versionsUI      <<- .rx$versionsUI()
      needHelp        <<- .rx$needHelp()
      species         <<- .rx$species()
      minCutThresh    <<- .rx$minCutThresh()
      minThresh       <<- .rx$minThresh()
      selectedVersion <<- .rx$selectedVersion()

      #SM_pres has just been refreshed above, so this visit's matrix - or its
      #absence - is what the download button reflects. The checkbox is not
      #touched here or anywhere: see applySMState().
      applySMState()

      #--- 2. tear down the previous visit's version cards and their observers.
      #One observer per card, created by updateVersions(), and the cards are
      #insertUI'd - so without this a second visit shows every version twice and
      #a single click on a card runs its handler twice. The "go to new versions"
      #handler used to do this on the way out; the nav bar does not go through it.
      for(obs in r$obsEventSelList){
        if(!is.null(obs)) try(obs[[1]]$destroy(), silent = TRUE)
      }
      r$obsEventSelList <- list()
      shiny::removeUI(selector = "div#placeholder_step5")
      shiny::insertUI(selector = "#topPlaceHolder",
                      ui = shiny::tags$div(
                        id = "placeholder_step5"
                      )
      )

      #--- 3. banner and language
      if(is.null(currentLang)) currentLang <<- "de"
      vftDbgCat("CURRENTLANG : ")
      vftDbgCat(currentLang)
      shiny.i18n::update_lang(currentLang)
      shiny::updateSelectInput(inputId = "languageSelect_5", selected = currentLang)
      if(currentLang == "de"){
        vftSetBanner(id, "www/step5_wsl.png")
      }else if(currentLang == "fr"){
        vftSetBanner(id, "www/step5_wsl_fr.png")
      }

      #--- 4. this visit's state. networkList and versionsUI are the point of the
      #whole exercise: newVersions writes them back into the app's `r`, and this
      #is where step 5 picks them up.
      r$networkList        <- networkList
      r$versionsUI         <- versionsUI
      #the scenario card to open on. Picked up from the app the same way as the
      #two lists above, because the newVersions page may have changed it since
      #this module last ran - and NOT reset below with the rest of this visit's
      #state, which is the whole point of it.
      r$selectedVersion    <- selectedVersion
      r$needHelp           <- needHelp
      r$currentLang        <- currentLang
      r$triggerStp6        <- 0
      r$confirm            <- NULL
      r$lastSelectedImage  <- NULL
      r$result             <- NULL
      r$passageTable       <- NULL
      r$startingPointsSf   <- NULL
      r$mapPresent         <- NULL
      r$refreshMap         <- FALSE
      #the agent-type radio buttons follow the simulation map, which has just
      #been cleared with r$result above
      r$agentCheckboxIsDisabled <- TRUE
      selectedNetwork_position <<- 1

      #--- 5. the protected areas, only when the perimeter actually changed.
      #Display-only geometry handed straight to leaflet, and a pure function of
      #the perimeter: deriving it per visit would pay ~0.10s of main-thread clip
      #for a layer that cannot have changed. (It used to be ~5.7s - see the note
      #at the top of this file.)
      if(!is.null(shape) && !identical(cache$shape, shape)){
        cache$shape <- shape
        shp_PA <<- vftTime("step5:protectedAreas", vftProtectedAreas(shape))
      }

      #--- 6. the "Original" scenario if this is a first run, then the cards -
      #and the map exactly once, from inside generateVersionImages(), after the
      #selection it draws is settled.
      #
      #The bare plotPathUsage() that used to lead this block is gone. It ran
      #before the cards existed and before anything had been selected, and
      #generateVersionImages() then drew a SECOND time; see the note there for
      #what the second draw did to the first one's map. Nothing between here and
      #that call reads r$result or touches the map - createOriginalVersion() only
      #adds an entry to r$versionsUI, updateVersions() only reads
      #r$networkList[[i]]$pathUsage - so removing it costs the entry path nothing.
      createOriginalVersion()
      generateVersionImages()

      #createOriginalVersion() guarantees at least one entry, so the branch below
      #is unreachable in practice. It is here because the map is now drawn from
      #inside generateVersionImages(), and a version list that somehow came back
      #empty would otherwise leave the previous visit's map on screen with no card
      #claiming it - which is the failure this section is fixing, in reverse.
      if(!length(r$versionsUI)){
        vftDbg("ENTER: no versions to select - drawing the map on its own")
        plotPathUsage()
      }

      vftDbg("RETURNING STEP 6")
      vftDbg(r$lastSelectedImage)
      invisible(NULL)
    })

    enter()

    return(list(pathUsage = shiny::reactive(r$result$pathUsage), networkList = shiny::reactive(r$networkList), confirm = shiny::reactive({r$confirm}), newVersions = shiny::reactive(input$newVersionsButton), trigger = shiny::reactive(r$triggerStp6), versionsUI = shiny::reactive(r$versionsUI),
                #a rising count meaning "the user asked to go and make a
                #sensitivity matrix". app_server turns it into the navigation -
                #this module has neither the app's `r` nor the app's session, so
                #it cannot call vftGoToStep() itself. Same shape as
                #`newVersions`/`trigger` above.
                smCreate = shiny::reactive(smCreate()),
                selectedVersion = shiny::reactive(r$selectedVersion),
                #the parking table, once vftPrepareThen() has produced it. The
                #scenario holds the authoritative copy; this is what keeps the
                #app-level r$parking - the key the save file records - in step.
                #
                #Guarded rather than a bare [[ ]]: vftMirror() wraps this in an
                #observe() that re-runs on every change to r$networkList, and
                #the list is briefly empty while an invalidation rebuilds it and
                #briefly SHORTER than this position while the newVersions page
                #deletes a scenario. A subscript error there would not return
                #NULL, it would kill the mirror for the rest of the session.
                parking = shiny::reactive({
                  nl <- r$networkList
                  if(is.null(nl) || selectedNetwork_position > length(nl)) return(NULL)
                  nl[[selectedNetwork_position]]$parking
                }),
                currentLang = shiny::reactive(i18n()$get_translation_language()), shp_PA = shiny::reactive( shp_PA),
                enter = enter) )

  })
}
