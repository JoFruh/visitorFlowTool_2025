# Define server logic
lastStep_server <- function(id, networkList , versionsUI ,
                            SM_pres, SMColors , shape ,
                            basemap , finalPolygons, species, pathUsage ){

  #count this instantiation. A module server should be created once per
  #session; this app re-calls it from an observeEvent on a trigger, so any
  #count above 1 means a duplicate set of observers and outputs is now live
  #alongside the previous one. See vftModuleInstance() in perf_helpers.R.
  vftModuleInstance("finalStep")


  shiny::moduleServer(id, function(input, output, session) {

    r <- shiny::reactiveValues()

    r$confirm <- NULL

    r$obsEventSelList <- list()

    r$lastSelectedImage <- NULL

    r$selectedNetwork_position <- NULL
    r$selectedNetwork_position <- 1

    r$networkList <- networkList



    envLastStep <- new.env(parent = emptyenv())

    envLastStep$selectedNetwork_r <- shiny::reactiveVal()

    result <- new.env(parent = emptyenv())
    result$result <- NULL

    # lastSelectedImage <- NULL
    #internal function to generate version selection boxes
    updateVersions <- function(name, inputId_select, id_ui_name, position){

      vftDbgCat("TEST9")


      shiny::insertUI(
        selector = '#placeholder_lastStep',
        ## wrap element in a div with id for ease of removal
        ui = shiny::tags$div(id = paste0("step5", id_ui_name),
                             shiny::div(style = "height: 5px"),

                             vftDbgCat("TEST9a"),

                             if(name == "Original" ){
                               if(is.null(r$networkList[[position]]$pathUsage) ){
                                 vftDbgCat("TEST9b")
                                 shiny::actionButton(inputId = shiny::NS(id, inputId_select), label =  name, width = "120px",  style = "height: 120px; position: relative; text-align: center; ",
                                                     class = "selected noSim" )


                               }else{
                                 vftDbgCat("TEST9c")

                                 shiny::actionButton(inputId = shiny::NS(id, inputId_select), label =  name, width = "120px",  style = "height: 120px; position: relative; text-align: center; ",
                                                     class = "selected withSim" )


                               }
                             }else{
                               if(is.null(r$networkList[[position]]$pathUsage)){
                                 vftDbgCat("TEST9d")

                                 shiny::actionButton(inputId = shiny::NS(id, inputId_select), label = name, width = "120px",  style = "height: 120px; position: relative; text-align: center; ",
                                                     class = "notSelected noSim" )


                                 #tags$div(
                                 # tags$img(src = "noSim.png", height = "120px") ,
                                 # tags$text(name, style = "position: absolute;top: 50%;left: 50%;transform: translate(-50%, -50%);"),
                                 # width = "120px",  style = "height: 120px; position: relative; text-align: center; ",
                                 # class = "notSelected")
                               }else{
                                 vftDbgCat("TEST9e")

                                 shiny::actionButton(inputId = shiny::NS(id, inputId_select), label = name, width = "120px",  style = "height: 120px; position: relative; text-align: center; ",
                                                     class = "notSelected withSim" )
                               }
                             },
                             shiny::div(style = "height: 5px")
        )

      )

      vftDbgCat("TEST10")

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

          vftDbg("SELECTED")
          vftDbg(versionsUI)
          #select button (outline in green?)
          shinyjs::removeClass(inputId_select, "notSelected")
          shinyjs::addClass(inputId_select, "selected")

          vftDbg(paste0("lastSelectedImage: ", r$lastSelectedImage))
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
          for(vn in 1:length(versionsUI)){
            if(versionsUI[[vn]]$inputId_select == inputId_select){
              x <- vn
            }
          }

          #select same position in networkList
          if(x <= length(r$networkList)){

            envLastStep$selectedNetwork_r( list(r$networkList[[x]]$network) )
            #keep track position to easily insert results in networkList
            r$selectedNetwork_position <- x

          }else{
            vftDbg("ERROR: less networks than version buttons")

          }



          #update Leaflet

          if(input$usageSwitch == 1){

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

          proxy <- leaflet::leafletProxy(mapId = "mapAreaLeaflet"
          )|>
            leaflet::clearGroup(group = "paths")

          #update map with new version polylines
          passageTable <- sf::st_zm(sf::st_as_sf(dplyr::as_tibble(r$result$pathUsage |> tidygraph::activate(edges) ) ), drop = T, what = "ZM")
          pal <- leaflet::colorNumeric(c("grey", colorRampPalette(c("yellow2", "orange2", "red2", "purple", "purple3"))(max(passageTable$passage)-1)), domain = c(0,max(passageTable$passage)) )
          proxy |> leaflet::addPolylines(data = passageTable,
                                          stroke = TRUE,
                                          weight = 2 + (as.numeric(passageTable[,agentTypePassage,drop = TRUE]) / max(as.numeric(passageTable[,"passage",drop = TRUE])) ) *2,
                                          color = ~pal(as.numeric(passageTable[,agentTypePassage,drop = TRUE])),
                                          fill = FALSE,
                                          opacity = 1,
                                          options = leaflet::pathOptions(pane = "layer2"),
                                          group = "paths")




        })
      )

    }



      #GENERATE VERSION IMAGES ####
      if(length(versionsUI) > 0){

        for(i in 1:length(versionsUI) ){
          updateVersions(versionsUI[[i]]$name, versionsUI[[i]]$inputId_select, versionsUI[[i]]$id_ui_name, position = i)



        }


        #select original network
        envLastStep$selectedNetwork_r( list(networkList[[1]]$network) )
        result$result <- networkList[[1]]$pathUsage


        #automatically plot original
        r$lastSelectedImage <- versionsUI[[1]]$inputId_select

        vftDbg("determine last selected image")
        vftDbg(versionsUI[[1]]$inputId_select)
        vftDbg(r$lastSelectedImage)
      }






    # RENDER COMBINED MAPS ####

    output$mapArea <- shiny::renderPlot({

      bboxUsage <- NULL
      map <- NULL
      # PLOT SELECTED PATHUSAGE (IF PRESENT) ####
      if(shiny::isolate(!is.null(r$networkList[[r$selectedNetwork_position]]$pathUsage ) ) ){

        #make current result that of selected version
        shiny::isolate(r$result <- r$networkList[[r$selectedNetwork_position]]$pathUsage)

        #reactiveVal to manually trigger plotting
        vftDbg("PLOTRESULTS()")
        # first print blank usageMap (with white color)
        #this is to set plot parameters. Above which a sensitivity matrix can be first plotted if needed
        # pathUsageColor <- c("white", "white")
        passageTable <- sf::st_zm(sf::st_as_sf(dplyr::as_tibble(r$result$pathUsage |> tidygraph::activate(edges) ) ), drop = T, what = "ZM")

        vertexTable <- dplyr::as_tibble(r$result$pathUsage |> tidygraph::activate(nodes) )
        startingPoints <- sf::st_coordinates(sf::st_as_sf( vertexTable[vertexTable$nodeID %in% r$result$dayPop$startV , ]) )

        #test (not run)
        # leaflet::previewColors(pa<l, values = passageTable$passage)
        pal <- leaflet::colorNumeric(c("grey",colorRampPalette(c("yellow3", "orange2", "red2", "purple"))(max(passageTable$passage)-1) ), domain = c(0,max(passageTable$passage)) )
        map <- leaflet::leaflet(data = passageTable, options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE), height = 500 ) |>
          leaflet::addMapPane("layer_SM", zIndex = 405)|>
          leaflet::addMapPane("layer1", zIndex = 410)|> leaflet::addMapPane("layer2", zIndex = 420)|> leaflet::addMapPane("layer3", zIndex = 450) |>
          leaflet::addProviderTiles("OpenStreetMap.CH", options = leaflet::providerTileOptions(opacity = 0.5, zIndex = 400)) |>
          leaflet::addPolylines(stroke = TRUE,
                                weight = 2 + (passageTable$passage / max(passageTable$passage) ) *4,
                                color = pal(passageTable$passage),
                                fill = FALSE,
                                opacity = 1,
                                options = leaflet::pathOptions(pane = "layer2"),
                                group = "paths")|>
          leaflet::addPolygons(data = finalPolygons,
                               weight = 3,
                               color = "green",
                               fillColor = "green",
                               fill = TRUE,
                               stroke = TRUE,
                               options = leaflet::pathOptions(pane = "layer1"),
                               opacity = 0.3,
                               fillOpacity = 0.1)|>
          leaflet::addPolygons(data = sf::st_geometry(r$networkList[[r$selectedNetwork_position]]$parking), stroke = TRUE, fill = TRUE,
                               fillColor = "steelblue", opacity = 1, fillOpacity = 0.3)|>
        # |>
        #   leaflegend::addLegendImage(position = "topright",title = "Formen:", images = c("www/AOI.png", "www/parking.png", "www/Start.png"), labels = c("Zielgebiete","parkplatz", "Startposition des Agenten"),
        #                              labelStyle = "font-size: 15px; text-align: left")|>
        #   leaflet::addLegend(title = "Wegnutzung:", position = "topright", labels = c("kein", "niedrigste", "mittlere", "hohe", "höchste") , colors = c("grey", "#dec402", "#de9802", "#e00417", "purple"))|>

          leaflet.extras::setMapWidgetStyle(list(background = "white"))

        map <- map |> leaflet::addPolygons(data= sf::st_zm(sf::st_transform(shape, "epsg:4326"), drop = TRUE, what = "ZM" ), stroke = TRUE, fill = FALSE, color = "black",
                                            weight = 5, options = leaflet::pathOptions(pane = "layer2"))

        output$mapArea <- shiny::renderUI({

          leaflet::leafletOutput(NS(id, "mapAreaLeaflet"), height = 500)

        })

        output$mapAreaLeaflet <- leaflet::renderLeaflet({
          map

        })


        # bboxUsage <- sf::st_bbox(passageTable["passage"]$`_ogr_geometry_`)
        # terra::plot(x = passageTable["passage"][1,],
        #      pal = pathUsageColor,
        #      xlim = bboxUsage[c(1, 3)],
        #      ylim = bboxUsage[c(2, 4)],
        #      nbreaks = 2,
        #      reset = FALSE)

        # SMgradient <- colorRampPalette(c( "white", "brown"))(length(SMcolors))

        #print SM underlay
        # if(input$SMswitch == 1){
        # terra::plot(SM_noPres, add = TRUE, col = SMcolors, xlim = bboxUsage[c(1, 3)], ylim = bboxUsage[c(2, 4)], box = FALSE, axes = FALSE, legend = FALSE, alpha = 0.2)
        # terra::plot(SM_pres, add = TRUE, col = SMcolors,  xlim = bboxUsage[c(1, 3)], ylim = bboxUsage[c(2, 4)], box = FALSE, axes = FALSE, legend = FALSE, alpha = 0.2)

        # }
        # cat(file = stderr(), "TEST13")

        # if(input$usageSwitch == 0){

        # PLOT ALL PASSAGE

        # usageLvls <- nrow( unique(dplyr::as_tibble(result$result$pathUsage |> tidygraph::activate(edges) ) ["passage"]) )
        # pathUsageColor <- c("dark grey", grDevices::colorRampPalette(c( "yellow", "red", "red4", "purple"))(usageLvls-1) )
        #
        # # passageTable <- st_as_sf(as_tibble(result$pathUsage |> activate(edges) ) )
        #
        # bboxUsage <- sf::st_bbox(passageTable["passage"]$`_ogr_geometry_`)
        #
        #           terra::plot(x =  passageTable["passage"],
        #                pal = pathUsageColor,
        #                lwd = ceiling( (igraph::E(result$result$pathUsage)$passage+2) /( max(igraph::E(result$result$pathUsage)$passage+2)/8 ) ),
        #                nbreaks = usageLvls,
        #                add = TRUE)
        #
        #           vertexTable <- dplyr::as_tibble(result$result$pathUsage |> tidygraph::activate(nodes) )
        #
        #           dayPop <- result$result$dayPop
        #
        #           #plot starting locations
        #           terra::plot(x = sf::st_geometry( sf::st_as_sf( vertexTable[vertexTable$nodeID %in% dayPop$startV , ]) ), add = TRUE, col = "red",pch = 4, lwd = 0.5)
        #           #plot AOI polygons
        #           if(!is.null(finalPolygons) ){plot(sf::st_geometry(finalPolygons), border = "dark green", lwd = 2, add = TRUE)}
        #           #plot parking locations
        #           if(!is.null(r$networkList[[selectedNetwork_position]]$parking) ){plot(sf::st_geometry(r$networkList[[selectedNetwork_position]]$parking), border = "darkblue", lwd = 1, add = TRUE)}

        # }
        #         else{
        # #
        # #           #PLOT ONLY AOI PASSAGE
        # #
        # #           usageLvls <- nrow( unique(dplyr::as_tibble(result$result$pathUsage |> tidygraph::activate(edges) ) ["passageAOI"]) )
        # #           pathUsageColor <- c("grey", grDevices::colorRampPalette(c( "yellow", "red", "red4", "purple"))(usageLvls-1) )
        # #
        # #           # passageTable <- st_as_sf(as_tibble(result$pathUsage |> activate(edges) ) )
        # #
        # #           bboxUsage <- dplyr::as_tibble(passageTable["passage"]$`_ogr_geometry_`)
        # #
        # #           terra::plot(x =  passageTable["passageAOI"],
        # #                pal = pathUsageColor,
        # #                lwd = ceiling( (igraph::E(result$result$pathUsage)$passageAOI+1) /( max(igraph::E(result$result$pathUsage)$passageAOI+1)/8 ) ),
        # #                nbreaks = usageLvls,
        # #                add = TRUE)
        # #
        # #           vertexTable <- dplyr::as_tibble(result$result$pathUsage |> tidygraph::activate(nodes) )
        # #
        # #           dayPop <- result$dayPop
        # #           cat(file = stderr(), "TEST14")
        # #
        # #           #plot starting locations
        # #           terra::plot(x = sf::st_geometry( sf::st_as_sf( vertexTable[vertexTable$nodeID %in% dayPop$startV , ]) ), add = TRUE, col = "red", pch = 4,lwd = 0.5)
        # #           #plot AOI polygons
        # #           if(!is.null(finalPolygons) ){plot(sf::st_geometry(finalPolygons), border = "dark green", lwd = 2, add = TRUE)}
        # #           #plot parking areas
        # #           if(!is.null(r$networkList[[selectedNetwork_position]]$parking) ){plot(sf::st_geometry(r$networkList[[selectedNetwork_position]]$parking), border = "darkblue", lwd = 1, add = TRUE)}
        #
        #
        #         }



        #
        # if(length(plotResults()) > 0){
        #   print("ACTIVATE PLOT1")
        #
        #   plotPathUsage(result = result)
        #
        #   # plotResults(plotResults()+1)
        #
        # }else{
        #   print("ACTIVATE PLOT2")
        #
        #   plotResults(1)
        # }

      }else{
        #plot empty basemap


      }

    })

    #OBSERVERS####
    #observe banner click (choosing to step back in history)
    obsBanner <- observeEvent(input$banner,  {

      shinyjs::disable(id = "banner")

      vftDbg("MAPPED IMAGE CLICKED")
      #determine where to go back in history
      r$confirm <- input$banner



      obsConfirm$destroy()
      r$obsMapClick$destroy()
      r$obsMarkerClick$destroy()
      r$obsErase$destroy()
      obsBanner$destroy()

      r$finalPolygons <- NULL



      # shinyjs::enable("banner")

      return(shiny::reactive(r$confirm))

      #trigger return to past (return with specific confirm value?)
    }, ignoreInit = TRUE)


    return(list(confirm = shiny::reactive(r$confirm)))
  })


}
