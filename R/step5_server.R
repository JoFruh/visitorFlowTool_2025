

# Define server logic
step5_server <- function(id, networkList, SM_pres, SM_noPres, SMcolors, shape, confirm, i18n, currentLang, isFirstRun_stp6, finalPolygons = NULL, versionsUI = list(), triggerStp6 = 0,
                         basemap = NULL, needHelp = FALSE, species = NULL, minCutThresh = NULL){


  shiny::moduleServer(id, function(input, output, session) {

    #CONSTANT:division of total residents to get number of agents
    CONST_residentDivision <- 50

    r <- shiny::reactiveValues()

    r$needHelp <- needHelp


    #handle language bar
    r$needHelp <- needHelp
    r$currentLang <- currentLang

    cat(file = stderr(),"CURRENTLANG : " )
    cat(file = stderr(), r$currentLang )

    #keep track of checkbox
    r$agentCheckboxIsDisabled <- TRUE

    #variable to determine if there is a simulation map
    r$mapPresent <- NULL
    r$refreshMap <- FALSE


    shiny.i18n::update_lang(r$currentLang)
    shiny::updateSelectInput(inputId = "languageSelect_5", selected = currentLang)


    #reset map UI if already present
    output$mapAreaLeaflet <- renderUI({return(NULL)})

    #render banner image from start
    if(r$currentLang == "de"){
      output$bannerUI_5 <- shiny::renderUI({
        imgMap <- imageMap(NS(id, "banner_5"), i18n()$t("www/step5_wsl.png"), list() )
        #replace /" with ', to avoid problems
        return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
      })
    }else if(r$currentLang == "fr"){
      output$bannerUI_5 <- shiny::renderUI({
        imgMap <- imageMap(NS(id, "banner_5"), i18n()$t("www/step5_wsl_fr.png"), list() )
        #replace /" with ', to avoid problems
        return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
      })
    }


    #LOAD PROTECTED AREAS DATA ####
    bbox <- sf::st_bbox(shape)
    shp_PA <-  sf::st_read( "www/data/maps/protectedAreas/PA_all.gpkg", wkt_filter = sf::st_as_text(sf::st_as_sfc(bbox)))

    shp_PA <- sf::st_crop(shp_PA, shape)

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
    #           r$parkingShapes <- sf::st_read("www/data/maps/parking/parkingShapes.shp",
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

        print("BUILDING NAME")

        return(name)
      },
      content = function(file){

        print("COPYING FILE")
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

    #variables
    r$networkList <- networkList
    r$versionsUI <- versionsUI

    r$triggerStp6 <- 0
    obsEvent_sim <- NULL
    r$obsEventSelList <- list()
    r$lastSelectedImage <- NULL

    selectedNetwork_position <- 1

    # envStep6 <- new.env(parent = emptyenv())

    r$selectedNetwork_r <- shiny::reactiveVal()
    # plotResults <- shiny::reactiveVal(0)

    # result <- new.env(parent = emptyenv())
    r$result <- NULL

    # lastSelectedImage <- NULL
    #internal function to generate version selection boxes
    updateVersions <- function(name, inputId_select, id_ui_name, position){

      cat(file = stderr(), "TEST9")

      shiny::insertUI(
        selector = '#placeholder_step5',
        ## wrap element in a div with id for ease of removal
        ui = shiny::tags$div(id = paste0("step5", id_ui_name),
                      shiny::div(style = "height: 5px"),

                      cat(file = stderr(), "TEST9a"),

                      if(name == "Original" ){
                        if(is.null(r$networkList[[position]]$pathUsage) ){
                          cat(file = stderr(), "TEST9b")
                          shiny::actionButton(inputId = shiny::NS(id, inputId_select), label =  name, width = "120px",  style = "height: 120px; position: relative; text-align: center; ",
                                       class = "selected noSim" )


                        }else{
                          cat(file = stderr(), "TEST9c")

                          shiny::actionButton(inputId = shiny::NS(id, inputId_select), label =  name, width = "120px",  style = "height: 120px; position: relative; text-align: center; ",
                                       class = "selected withSim" )


                        }
                      }else{
                        if(is.null(r$networkList[[position]]$pathUsage)){
                          cat(file = stderr(), "TEST9d")

                          shiny::actionButton(inputId = shiny::NS(id, inputId_select), label = name, width = "120px",  style = "height: 120px; position: relative; text-align: center; ",
                                       class = "notSelected noSim" )


                          #tags$div(
                          # tags$img(src = "noSim.png", height = "120px") ,
                          # tags$text(name, style = "position: absolute;top: 50%;left: 50%;transform: translate(-50%, -50%);"),
                          # width = "120px",  style = "height: 120px; position: relative; text-align: center; ",
                          # class = "notSelected")
                        }else{
                          cat(file = stderr(), "TEST9e")

                          shiny::actionButton(inputId = shiny::NS(id, inputId_select), label = name, width = "120px",  style = "height: 120px; position: relative; text-align: center; ",
                                       class = "notSelected withSim" )
                        }
                      },
                      shiny::div(style = "height: 5px")
        )

      )

      #record Original as selected
      r$lastSelectedImage <- 1

      cat(file = stderr(), "TEST10")

      #keep track of ids and inputIds
      # inserted_id_ui <<- c(inserted_id_ui, id_ui_name)
      # print(inserted_id_ui)
      # inserted_inputId_select <<- c(inserted_inputId_select, inputId_select)
      # print(inserted_inputId_select)
      # inserted_inputId_removal <<- c(inserted_inputId_removal, inputId_removal )
      # print(inserted_inputId_removal)


      #OBSERVER FOR SELECT VERSION ####
      r$obsEventSelList[[length(r$obsEventSelList)+1]] <- list(
          shiny::observeEvent(input[[inputId_select]], {

        print("SELECTED")
        print(r$versionsUI)
        #select button (outline in green?)
        shinyjs::removeClass(inputId_select, "notSelected")
        shinyjs::addClass(inputId_select, "selected")

        print(paste0("lastSelectedImage: ", r$lastSelectedImage))

        if(!is.null(r$lastSelectedImage)){
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

        }else{
          print("ERROR: less networks than version buttons")

        }

        # PLOT SELECTED SIMULATIONS

       plotPathUsage()

       #plot with Leaflet instead



      })
        )

    }
    cat(file = stderr(), "TEST11")



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

    plotPathUsage <- function(){



      cat(file = stderr(), "TEST12")
      # output$pathUsageMap <- leaflet::renderLeaflet({
      print("ACTIVATING RENDERPLOT")

      bboxUsage <- NULL
      map <- NULL
      # PLOT SELECTED PATHUSAGE (IF PRESENT) ####
      if(shiny::isolate(!is.null(r$networkList[[selectedNetwork_position]]$pathUsage )) ){

        #use proxy if map already present


          #make current result that of selected version
          shiny::isolate(r$result$pathUsage <- r$networkList[[selectedNetwork_position]]$pathUsage)
          shiny::isolate(r$result$dayPop <- r$networkList[[selectedNetwork_position]]$dayPop)
          #selected version changed: invalidate the cached passageTable so it rebuilds below
          shiny::isolate(r$passageTable <- NULL)

          #reactiveVal to manually trigger plotting
          print("PLOTRESULTS()")
          # first print blank usageMap (with white color)
          #this is to set plot parameters. Above which a sensitivity matrix can be first plotted if needed
          # pathUsageColor <- c("white", "white")
          passageTable <- getPassageTable()

          vertexTable <- dplyr::as_tibble(r$result$pathUsage |> tidygraph::activate(nodes) )
          startingPoints <- sf::st_coordinates(sf::st_as_sf( vertexTable[vertexTable$nodeID %in% r$result$dayPop$startV , ]) )



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
          map <- leaflet::leaflet(data = passageTable, options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE), height = 500 ) |>
            leaflet::addMapPane("layer_SM", zIndex = 415)|>
            leaflet::addMapPane("layer1", zIndex = 410)|> leaflet::addMapPane("layer2", zIndex = 420)|> leaflet::addMapPane("layer3", zIndex = 450) |>
            leaflet::addProviderTiles("OpenStreetMap.CH", options = leaflet::providerTileOptions(opacity = 0.5, zIndex = 400))

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
            map <- leaflet::leafletProxy("mapAreaLeaflet")|>
              leaflet::clearShapes()|>leaflet::clearGeoJSON()|>leaflet::clearImages()
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

            map <- map|>leaflet::addPolylines(stroke = TRUE,
                                  weight = 2 + (as.numeric(passageTable[,agentTypePassage,drop = TRUE]) / max(as.numeric(passageTable[,"passageAOI",drop = TRUE])) ) *2,
                                  color = ~pal(as.numeric(passageTable[,agentTypePassage,drop = TRUE])),
                                  fill = FALSE,
                                  opacity = 1,
                                  options = leaflet::pathOptions(pane = "layer2"),
                                  group = "paths",
                                  data = passageTable)
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



            if(input$SMcheckbox == 1){
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
            output$mapArea_UI <- renderUI({
              leaflet::leafletOutput(NS(id, "mapAreaLeaflet"), height = 600, width = 884)
            })

            output$mapAreaLeaflet <- leaflet::renderLeaflet({
              map
            })
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
            output$mapArea_UI <- renderUI({
                leaflet::leafletOutput(NS(id, "mapAreaLeaflet"), height = 600, width = 884)
            })
            output$mapAreaLeaflet <- leaflet::renderLeaflet({
              map
            })

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



        if(r$currentLang == "de"){
          noSimPic <- png::readPNG( "www/noSimYet_de.png")
        }else if(r$currentLang == "fr"){
          noSimPic <- png::readPNG( "www/noSimYet_fr.png")
        }else if(r$currentLang == "en"){
          noSimPic <- png::readPNG( "www/noSimYet_en.png")
        }

        # output$mapScript <- shiny::renderUI({
        #   tags$script(HTML(paste0(
        #     'document.getElementById("step5-mapAreaLeaflet").style.height="0px";',
        #     'document.getElementById("step5-mapArea").style.height="600px";',
        #     'document.getElementById("step5-mapAreaLeaflet").style.width="0px";',
        #     'document.getElementById("step5-mapArea").style.width="884x";'
        #
        #   )))
        # })
          #plot an image of empty simulation
          # output$mapArea <- shiny::renderUI({})

        output$mapArea_UI <- renderUI({

          shiny::plotOutput(NS(id, "mapArea"), height = 600, width = 884)

        })
          output$mapArea <- shiny::renderPlot({

              plot(1, type = "n", xlab = "",
                   ylab = "", xlim = c(0, 10),
                   ylim = c(0, 10), bty ="n",axes=F,frame.plot=F, xaxt='n', ann=FALSE, yaxt='n')

              graphics::rasterImage(noSimPic,0,0,10,10)

              print("EMPTY RENDERPLOT")
            }, height = 600, width = 884)

          r$mapPresent <- FALSE
      }




      # else{

      #   })
      # }


      r$refreshMap <- FALSE
    # })
    }





      results <- NULL


      plotPathUsage()

      cat(file = stderr(), "TEST15")

      cat(file = stderr(), paste0("isFirstRun_stp6 : ", isFirstRun_stp6))

      #CREATE ORIGINAL VERSIONS (FOR FIRST RUN ONLY) ####
      if(isFirstRun_stp6 | is.null(r$versionsUI) ){
        cat(file = stderr(), "TEST15b")



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
        print("CHANGE LANGUAGE")
        if(input$languageSelect_5 == "de"){
          # i18n$set_translation_language('de')
          shiny.i18n::update_lang("de")
          i18n()$set_translation_language("de")
          print("DE")
          output$bannerUI_5 <- shiny::renderUI({
            imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step5_wsl.png"), list() )
            #replace /" with ', to avoid problems
            return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
          })

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

          output$bannerUI_5 <- shiny::renderUI({
            imgMap <- imageMap(NS(id, "banner"), "www/step5_wsl_fr.png", list() )
            #replace /" with ', to avoid problems
            return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
          })

          output$agentCheckbox_ui <- shiny::renderUI({


            shiny::radioButtons(inputId = NS(id, "agentCheckbox"), label = "Type d'agent à afficher",
                                choices = c("tous" = "1", "Randonneurs" = "2", "Cyclistes" = "3", "Promeneurs de chien" = "4", "Joggeurs" = "5") ,
                                selected = 1)
          })


          r$currentLang <- "fr"

          print("FR")
        }else if(input$languageSelect_5 == "en"){
          # i18n$set_translation_language('en')
          shiny.i18n::update_lang("en")
          i18n()$set_translation_language("en")


          output$bannerUI_5 <- shiny::renderUI({
            imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step5_wsl_en.png"), list() )
            #replace /" with ', to avoid problems
            return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
          })

          output$agentCheckbox_ui <- shiny::renderUI({


            shiny::radioButtons(inputId = NS(id, "agentCheckbox"), label = "Agent type to display",
                                choices = c("all" = "1", "Walkers" = "2", "Cyclists" = "3", "Dog walkers" = "4", "Joggers" = "5") ,
                                selected = 1)
          })


          r$currentLang <- "en"

          print("EN")
        }else if(input$languageSelect_5 == "it"){
          # i18n$set_translation_language('it')
          shiny.i18n::update_lang("it")
          output$bannerUI_5 <- shiny::renderUI({
            imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step5_wsl.png"), list() )
            #replace /" with ', to avoid problems
            return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
          })


          print("IT")
        }

        r$refreshMap <- TRUE
        plotPathUsage()
      }, ignoreInit = TRUE)

      #observe banner click (choosing to step back in history)
      obsBanner <- observeEvent(input$banner,  {
        print("MAPPED IMAGE CLICKED")
        #determine where to go back in history
        r$confirm <- input$banner

        #clean up
        obsEvent_sim$destroy()
        obsBanner$destroy()
        #remove all versions
        for(obs in r$obsEventSelList){
          print(paste0("obs", obs) )
          obs[[1]]$destroy()
        }
        #treat next step 6 as first run
        r$step6FirstRun <- TRUE

        return(list(pathUsage = shiny::reactive({r$pathUsage}), networkList = shiny::reactive({r$networkList}), confirm = shiny::reactive({r$confirm}), newVersions = shiny::reactive({input$newVersionsButton}), trigger = shiny::reactive(r$triggerStp6), versionsUI = shiny::reactive(versionsUI) ,
                    shp_PA = shiny::reactive(shp_PA)) )

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
      obsEvent_sim <- shiny::observeEvent(input$launchSim, {

        #disable launch sim button
        shinyjs::disable("launchSim")

        cat(file = stderr(), "TEST16")

        print("LAUNCH SIMULATION")

        #use selected network to launch simulation
        network <- r$selectedNetwork_r()[[1]]

        progress <- ipc::AsyncProgress$new(value = 0, message = "Running Agent-Based Model")

        #LAUNCH PROMISE####
        future::future({
        #determine sum of residents in area of focus
        nbResidents <- sum(igraph::V(network)$Residents, na.rm = TRUE)
        #determine number of agents
        # do not divide by CONST for glatt/wigger subset
        nbAgents <- nbResidents/CONST_residentDivision


        print("GENERATE POPULATION")


        #get dataframe of all agents, their characteristics and their starting positions
        pop <- generatePopulation(network, nAgents = nbAgents, parkingIntensity = 0.1)


        print("LAUNCH MULTISIM")

        #launch simulations
        results <- launchMultiSim(pop, network, days = "1wk", finalPolygons = finalPolygons, progress = progress)
        progress$close()

        results
        }, seed = TRUE)%...>%(function(results){

        ## TREAT PROMISE RESULT ####

        r$result <- results
        #fresh simulation result: invalidate cached passageTable
        r$passageTable <- NULL

        # gc()
        print("RESULTS DONE")



        #insert result into networkList
        r$networkList[[selectedNetwork_position]]$pathUsage <- r$result$pathUsage
        r$networkList[[selectedNetwork_position]]$dayPop <- r$result$dayPop

        #update button to reflect presence of pathUsage
        inputid <- r$versionsUI[[selectedNetwork_position]]$inputId_select

        print(paste0("inputId: ", inputid))

        shinyjs::removeClass(inputid, "noSim")
        shinyjs::addClass(inputid, "withSim")

        # updateActionButton(session = session, inputId = inputid, label = tags$div(
        #   tags$img(src = "noSim.png", height = "120px") ,
        #   tags$text(name, style = "position: absolute;top: 50%;left: 50%;transform: translate(-50%, -50%);"),
        #   width = "120px",  style = "height: 120px; position: relative; text-align: center;  ",
        #   class = "selected") )

        # PLOT SELECTED SIMULATIONS

        print("Trying to plot:")

        #PLOT OUTPUT####

        # IF NO AGENT TYPE SELECTED,
        #SELECT 1
        # if(is.null(input$agentCheckbox)){
        #   shiny::updateCheckboxGroupInput(inputId = "agentCheckbox",
        #                                   selected = 1)
        # }

        plotPathUsage()

        #
        # print(networkList[[selectedNetwork_position]])
        # print(networkList[[selectedNetwork_position]]$pathUsage)
        #
        #
        # if(!is.null(networkList[[selectedNetwork_position]]$pathUsage )){
        #
        #   if(length(plotResults()) > 0){
        #     print("ACTIVATE PLOT3")
        #     plotResults(plotResults()+1)
        #
        #     print(plotResults())
        #
        #   }else{
        #     print("ACTIVATE PLOT4")
        #
        #     plotResults(1)
        #   }
        #
        # }

        #enable launch sim button
        shinyjs::enable("launchSim")

        })

      }, ignoreInit = TRUE)


#observe usage ####
      obsUsage <- shiny::observeEvent(input$onlyAOIcheckbox, {
        if(input$onlyAOIcheckbox == 1){
          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          )|>
            leaflet::clearGroup(group = "paths")

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
          proxy |> leaflet::addPolylines(data = passageTable,
                                          stroke = TRUE,
                                          weight = 2 + (as.numeric(passageTable[,agentTypePassage,drop = TRUE]) / max(as.numeric(passageTable[,"passageAOI",drop = TRUE])) ) *2,
                                          color = ~pal(as.numeric(passageTable[,agentTypePassage,drop = TRUE])),
                                          fill = FALSE,
                                          opacity = 1,
                                          options = leaflet::pathOptions(pane = "layer2"),
                                          group = "paths")
        }else{
          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          )|>
            leaflet::clearGroup(group = "paths")

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

          proxy |> leaflet::addPolylines(data = passageTable,
                                          stroke = TRUE,
                                          weight = 2 + (as.numeric(passageTable[,agentTypePassage,drop = TRUE]) / max(as.numeric(passageTable[,"passage",drop = TRUE])) ) *2,
                                          color = ~pal(as.numeric(passageTable[,agentTypePassage,drop = TRUE])),
                                          fill = FALSE,
                                          opacity = 1,
                                          options = leaflet::pathOptions(pane = "layer2"),
                                          group = "paths")
        }

      }, ignoreInit = TRUE)

      #observe Agent checkbox ####
      obsAgent <- shiny::observeEvent(input$agentCheckbox, {
        print("OBS AGENT")
        if(r$agentCheckboxIsDisabled == FALSE){

          if(input$onlyAOIcheckbox == 1){
            proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
            )|>
              leaflet::clearGroup(group = "paths")

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
            proxy |> leaflet::addPolylines(data = passageTable,
                                            stroke = TRUE,
                                            weight = 2 + (as.numeric(passageTable[,agentTypePassage,drop = TRUE]) / max(as.numeric(passageTable[,"passageAOI",drop = TRUE])) ) *2,
                                            color = ~pal(as.numeric(passageTable[,agentTypePassage,drop = TRUE])),
                                            fill = FALSE,
                                            opacity = 1,
                                            options = leaflet::pathOptions(pane = "layer2"),
                                            group = "paths")
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
              leaflet::clearGroup(group = "paths")

            passageTable <- getPassageTable()
            pal <- leaflet::colorNumeric(c("darkgrey", colorRampPalette(c("lightblue", "steelblue", "#182db5", "#37046e"))(max(passageTable$passage)-1)), domain = c(0,max(passageTable$passage)) )
            proxy |> leaflet::addPolylines(data = passageTable,
                                            stroke = TRUE,
                                            weight = 2 + (as.numeric(passageTable[,agentTypePassage,drop = TRUE]) / max(as.numeric(passageTable[,"passage",drop = TRUE])) ) *2,
                                            color = ~pal(as.numeric(passageTable[,agentTypePassage,drop = TRUE])),
                                            fill = FALSE,
                                            opacity = 1,
                                            options = leaflet::pathOptions(pane = "layer2"),
                                            group = "paths")
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
        print("OBS SM")

        if(input$SMcheckbox == 1){
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
        print("OBS PARKING")

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
        vertexTable <- dplyr::as_tibble(r$result$pathUsage |> tidygraph::activate(nodes) )
        startingPoints <- sf::st_coordinates(sf::st_as_sf( vertexTable[vertexTable$nodeID %in% r$result$dayPop$startV , ]) )

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

        print(paste0("mapBounds: ", mapBounds))

        cat(file = stderr(), "START TIFF\n")

        #Generate image (A3 format)

        r$tiffName <- gsub("[ ]|:|[.]|-", "", paste0("TIFF_",Sys.time()))
        r$tiffFile <- paste0(tempdir(), "/",r$tiffName)

        cat(file = stderr(), paste0("file location: ",r$tiffFile) )


        tiff(filename = r$tiffFile,  compression = "lzw", height = 29.7, width = 42, units = "cm", res = 300)
        #determine layout
        #determine how many legends (parking, residential and starting points are combined)
        legendNb <- sum(
                        input$SMcheckbox,
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

        cat(file = stderr(), "SMCheckbox\n")


        if(input$SMcheckbox == FALSE){
          layout(mat,
                 widths = c(5, 2.5, 0))
        }else{
          layout(mat,
                 widths = c(5, 1.5, 1))
        }

        passageTable <- getPassageTable()

        vertexTable <- dplyr::as_tibble(r$result$pathUsage |> tidygraph::activate(nodes) )
        # startingPoints <- sf::st_geometry(sf::st_coordinates(sf::st_as_sf( vertexTable[vertexTable$nodeID %in% r$result$dayPop$startV , ]) ) )

        cat(file = stderr(), "AOI\n")

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
        cat(file = stderr(), "SM\n")
        cat(file = stderr(), paste0("input$SM: ", input$SMcheckbox) )
        # if(input$SMcheckbox == FALSE){
          pal <- leaflet::colorNumeric(c("darkgrey",colorRampPalette(c("lightblue", "steelblue", "#182db5", "#37046e"))(max(passageTable$passage)-1) ), domain = c(0,max(passageTable$passage)) )
        # }else{
        #   cat(file = stderr(), "pal\n")
        #
        #   pal <- leaflet::colorNumeric(c("grey",colorRampPalette(c( "darkgrey", "black"))(max(passageTable$passage)-1) ), domain = c(0,max(passageTable$passage)) )
        # }
        cat(file = stderr(), "basemap\n")


        basemap <- maptiles::get_tiles(x = terra::ext(c(mapBounds[[4]], mapBounds[[2]], mapBounds[[3]], mapBounds[[1]])),
                                       provider = "OpenStreetMap", cachedir = vft_tileCacheDir)

        cat(file = stderr(), "plot basemap 1\n")

        cat(file = stderr(), paste0("basemap: ", basemap) )

        # terra::plot(terra::rast(matrix(1, nrow = 10, ncol = 10), ext = terra::ext(basemap)), col = "white")
        terra::plot(basemap,  alpha = 0.5, ext = terra::ext(c(mapBounds[[4]], mapBounds[[2]], mapBounds[[3]], mapBounds[[1]]) ) )

        cat(file = stderr(), "plot basemap 2\n")

        terra::plot(basemap,1, col = "white",  alpha = 0.5, add = TRUE, legend = FALSE)

        cat(file = stderr(), "aoi\n")

        if(input$aoi == TRUE ){
          plot(sf::st_geometry(finalPolygons), col = "#29ed1f30", border = "#0b630650", lwd = 5, add = TRUE)
        }

        cat(file = stderr(), "parking\n")


        if(input$ParkingCheckbox == TRUE & length(r$networkList[[selectedNetwork_position]]$parking) > 0){
          plot(sf::st_geometry(r$networkList[[selectedNetwork_position]]$parking ),
               col = "#3289a880", border = "#3289a8", lwd = 3, add = TRUE)
        }
        cat(file = stderr(), "residential\n")

        if(input$ResidentialCheckbox == TRUE & length(r$networkList[[selectedNetwork_position]]$residential) > 0){
          plot(sf::st_geometry(r$networkList[[selectedNetwork_position]]$residential),
               col = "#8a722b80", border = "#8a722b", lwd = 3, add = TRUE)
        }

        cat(file = stderr(), "sm\n")

        if(input$SMcheckbox == TRUE){
          terra::plot(SM_pres,alpha = 0.4, add = TRUE, col = SMcolors, legend = FALSE)
        }

        plot(x = sf::st_geometry(passageTable), col = pal(as.numeric(passageTable[,agentTypePassage,drop = TRUE])),
                    lwd = 2 + (as.numeric(passageTable[,agentTypePassage,drop = TRUE]) / max(as.numeric(passageTable[,"passageAOI",drop = TRUE])) ) *2,
                    xlim = c(mapBounds[[2]], mapBounds[[4]]),
                    ylim = c(mapBounds[[3]], mapBounds[[1]]) , add = TRUE, legend = FALSE)


        cat(file = stderr(), "starting\n")

        if(input$startingCheckbox){
          vertexTable <- dplyr::as_tibble(r$result$pathUsage |> tidygraph::activate(nodes) )
          startingPoints <- sf::st_as_sf( vertexTable[vertexTable$nodeID %in% r$result$dayPop$startV , ])

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
        cat(file = stderr(), "LEGENDS\n")

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
          if(input$SMcheckbox == 1){
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
          if(input$SMcheckbox == TRUE){
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
          if(input$SMcheckbox == 1){
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
          if(input$SMcheckbox == TRUE){
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
          if(input$SMcheckbox == 1){
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
          if(input$SMcheckbox == TRUE){
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

cat(file = stderr(), "FINISHED TIFF\n")

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
        shinyjs::click("downloadSM", asis = FALSE)

      }, ignoreInit = TRUE)


      # GO TO VERSIONS TAB ####
      obsNewVersions <- shiny::observeEvent(input$newVersionsButton, {
        r$triggerStp6 <- 1
        #destroy observers
        obsEvent_sim$destroy()
        obsConfirmName$destroy()
        obsGenImage$destroy()
        obsAgentStart$destroy()
        obsResidential$destroy()
        obsParking$destroy()
        obsSM$destroy()
        obsAOI$destroy()
        obsAgent$destroy()
        obsUsage$destroy()
        obsEvent_map$destroy()


        #destroy all observers in list
        print(paste0("obsEventSelList: ", r$versionsUI))
        for(obs in r$obsEventSelList){
          print(paste0("obs", obs) )
          obs[[1]]$destroy()
        }
        #REMOVE ALL VERSIONS ####
        # they are entirely refreshed when returning here from newVersions
        shiny::removeUI(selector = "div#placeholder_step5")
        shiny::insertUI(selector = "#topPlaceHolder",
                        ui = shiny::tags$div(
                          id = "placeholder_step5"
                        )
        )

        #no confirm button pressed
        r$confirm <- 0
        return(list(pathUsage = shiny::reactive({r$pathUsage}), networkList = shiny::reactive({r$networkList}), confirm = shiny::reactive({r$confirm}), newVersions = shiny::reactive({input$newVersionsButton}), trigger = shiny::reactive(r$triggerStp6), versionsUI = shiny::reactive(versionsUI),
                    currentLang = shiny::reactive(i18n()$get_translation_language()), shp_PA = shiny::reactive(shp_PA) ) )

      }, ignoreInit = TRUE, once = TRUE)



      cat(file = stderr(), "TEST17")

    #GENERATE VERSION IMAGES ####
    if(length(r$versionsUI) > 0){

      for(i in 1:length(r$versionsUI) ){
        updateVersions(r$versionsUI[[i]]$name, r$versionsUI[[i]]$inputId_select, r$versionsUI[[i]]$id_ui_name, position = i)



      }


      #select original network
      r$selectedNetwork_r( list(r$networkList[[1]]$network) )
      r$result$pathUsage <- r$networkList[[1]]$pathUsage
      #pathUsage (re)assigned: invalidate cached passageTable
      r$passageTable <- NULL

      plotPathUsage()



      #automatically plot original
      r$lastSelectedImage <- r$versionsUI[[1]]$inputId_select

      print("determine last selected image")
      print(r$versionsUI[[1]]$inputId_select)
      print(r$lastSelectedImage)
    }

    print("RETURNING STEP 6")
    print(r$lastSelectedImage)
#no confirmation at first
    r$confirm <- 0
    return(list(pathUsage = shiny::reactive(r$result$pathUsage), networkList = shiny::reactive(r$networkList), confirm = shiny::reactive({r$confirm}), newVersions = shiny::reactive(input$newVersionsButton), trigger = shiny::reactive(r$triggerStp6), versionsUI = shiny::reactive(r$versionsUI),
                currentLang = shiny::reactive(i18n()$get_translation_language()), shp_PA = shiny::reactive( shp_PA) ) )

  })
}
