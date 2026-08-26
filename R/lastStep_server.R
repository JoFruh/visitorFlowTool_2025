# Define server logic
#' CONVERTED TO A FIRST-TOUCH SINGLETON (Stage 5, seventh and last module).
#'
#' The smallest of the seven, and the one with the least per-visit work: it
#' produces nothing the app reads back (its only return is `confirm`, a banner
#' letter), so unlike step 5 and newVersions it needs no vftMirror(). What it
#' does own is the version cards - insertUI'd into `#placeholder_lastStep`, one
#' per entry in `versionsUI`, each with an observer of its own on
#' `r$obsEventSelList` - and those have to be torn down and rebuilt per visit or
#' a return shows every version twice and one click runs its handler twice.
#'
#' Every argument except `id` is a REACTIVE now, and none is read directly by the
#' body: enter() snapshots them into locals of the same names. This step is
#' downstream of everything, so its inputs change every time the user goes back,
#' edits and returns - which is precisely what a frozen `networkList` got wrong.
lastStep_server <- function(id, networkList, versionsUI,
                            SM_pres, shape, finalPolygons){

  #count this instantiation. A module server should be created once per
  #session; this app re-calls it from an observeEvent on a trigger, so any
  #count above 1 means a duplicate set of observers and outputs is now live
  #alongside the previous one. See vftModuleInstance() in perf_helpers.R.
  vftModuleInstance("finalStep")

  #The reactives, held under different names so the locals inside can shadow
  #them. Everything after this point reads plain values. `SM_pres` is not among
  #them: it is a formal nothing in this file has ever read, kept so the call site
  #need not change.
  .rx <- list(networkList = networkList, versionsUI = versionsUI,
              shape = shape, finalPolygons = finalPolygons)

  shiny::moduleServer(id, function(input, output, session) {

    #per-visit snapshots. enter() refills these; the body and every closure in it
    #resolve them lexically from here, so nothing else in this file changes.
    networkList   <- NULL
    versionsUI    <- list()
    shape         <- NULL
    finalPolygons <- NULL

    r <- shiny::reactiveValues()

    #Everything this block used to assign - the answer, the observer list, the
    #selected card and the network list - is per VISIT and is in enter() at the
    #bottom of this file. `r` itself is per session, so it is still made here.
    #
    #r$mapRedraw is new and is the nudge output$mapArea needs: that render reads
    #r$networkList and r$selectedNetwork_position through isolate() and takes its
    #geometry from plain locals, so nothing in it would invalidate on a return
    #visit. Seeded here rather than in enter(), because enter() INCREMENTS it and
    #NULL + 1 is numeric(0).
    r$mapRedraw <- 0
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
            vftClearNetworkLines(group = "paths")

          #update map with new version polylines
          passageTable <- sf::st_zm(sf::st_as_sf(dplyr::as_tibble(r$result$pathUsage |> tidygraph::activate(edges) ) ), drop = T, what = "ZM")
          pal <- leaflet::colorNumeric(c("grey", colorRampPalette(c("yellow2", "orange2", "red2", "purple", "purple3"))(max(passageTable$passage)-1)), domain = c(0,max(passageTable$passage)) )
          #drawn through WebGL: addPolylines() encodes every edge into
          #nested JSON on the shared main thread and scales with edge
          #count. See vftAddNetworkLines() in data_paths.R; VFT_GL=0
          #restores the original addPolylines call.
          proxy |> vftAddNetworkLines(passageTable,
                             values    = passageTable[,agentTypePassage,drop = TRUE],
                             weightRef = passageTable[,"passage",drop = TRUE],
                             pal = pal, group = "paths", pane = "layer2")




        })
      )

    }



      #GENERATE VERSION IMAGES ####
      #Per VISIT: every entry in versionsUI gets a card insertUI'd into
      ##placeholder_lastStep and an observer of its own on r$obsEventSelList.
      #enter() destroys those observers and empties the placeholder before
      #calling this, or a second visit shows every version twice.
      generateVersionImages <- function(){
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
      invisible(NULL)
      }






    # RENDER COMBINED MAPS ####

    output$mapArea <- shiny::renderUI({

      #the per-visit nudge. Everything below reads r$ through isolate() or reads
      #a plain local, so without this a return visit would show the map built for
      #whatever version was selected the LAST time, against a networkList that
      #may since have been replaced. enter() bumps it.
      r$mapRedraw

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
          leaflet::addProviderTiles("OpenStreetMap.CH", options = leaflet::providerTileOptions(opacity = 0.5, zIndex = 400))

        #drawn through WebGL - see vftAddNetworkLines() in data_paths.R. This map
        #uses a wider stroke than step 5 (0-4 px, hence span = 4), which is why
        #the helper takes the span rather than hard-coding one.
        map <- vftAddNetworkLines(map, passageTable,
                                  values = passageTable$passage, pal = pal,
                                  group = "paths", span = 4, pane = "layer2")

        map <- map |>
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

        output$mapAreaLeaflet <- leaflet::renderLeaflet({
          map

        })

        leafletSlot <- leaflet::leafletOutput(NS(id, "mapAreaLeaflet"), height = 500)


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

        leafletSlot

      }else{
        #no simulation selected yet: nothing to show in the map slot
        NULL
      }

    })

    #OBSERVERS####
    #observe banner click (choosing to step back in history)
    #
    #The banner is being retired in favour of the nav bar (decided 2026-08-26),
    #so this is left as it is - except for the five $destroy() calls that used to
    #be here. They were unrunnable: `obsConfirm`, `r$obsMapClick`,
    #`r$obsMarkerClick` and `r$obsErase` do not exist in this file, so the first
    #of them aborted the observer - and an error in an observer takes the session
    #with it. That went unnoticed because the banner has never been clickable
    #(imageMap() early-returns a bare <img>). With one instantiation the
    #obsBanner$destroy() would also have been wrong on its own terms: it would
    #have disarmed the only handler this module has, for the rest of the session.
    obsBanner <- observeEvent(input$banner,  {

      shinyjs::disable(id = "banner")

      vftDbg("MAPPED IMAGE CLICKED")
      #determine where to go back in history
      r$confirm <- input$banner

      r$finalPolygons <- NULL

      #trigger return to past (return with specific confirm value?)
    }, ignoreInit = TRUE)


    #### enter(): everything that happens per VISIT rather than per session ####
    #
    # Called by vftGoToStep() on every return to this step, and once here at the
    # end of construction, so the first visit and the fifth run the same code.
    #
    # vftModuleEnterFn() supplies the two properties this body must have and
    # neither of which is visible in it: the module's own session as the default
    # reactive domain (or the shinyjs:: calls below silently address unnamespaced
    # controls that do not exist), and isolate() around the whole body (or the
    # observers enter() is called from take a dependency on values it assigns).
    # See R/modules.R.
    enter <- vftModuleEnterFn(session, function(){

      #--- 1. refresh the snapshots the rest of this module reads
      networkList   <<- .rx$networkList()
      versionsUI    <<- .rx$versionsUI()
      shape         <<- .rx$shape()
      finalPolygons <<- .rx$finalPolygons()

      #--- 2. tear down the previous visit's version cards and their observers.
      #One observer per card, created by updateVersions(), and the cards are
      #insertUI'd - so without this a second visit shows every version twice and
      #a single click runs its handler twice.
      for(obs in r$obsEventSelList){
        if(!is.null(obs)) try(obs[[1]]$destroy(), silent = TRUE)
      }
      r$obsEventSelList <- list()
      shiny::removeUI(selector = "div#placeholder_lastStep")
      shiny::insertUI(selector = "#topPlaceHolder_lastStep",
                      ui = shiny::tags$div(
                        id = "placeholder_lastStep"
                      )
      )

      #--- 3. this visit's state. networkList is the point: this step is
      #downstream of everything, so a user who went back, edited and returned has
      #a different one every time.
      r$networkList              <- networkList
      r$confirm                  <- NULL
      r$lastSelectedImage        <- NULL
      r$selectedNetwork_position <- 1
      r$result                   <- NULL
      #obsBanner disables the strip on its way out and nothing ever re-enabled it
      shinyjs::enable(id = "banner")

      #--- 4. the cards, then the map
      generateVersionImages()
      r$mapRedraw <- r$mapRedraw + 1

      invisible(NULL)
    })

    enter()

    return(list(confirm = shiny::reactive(r$confirm), enter = enter))
  })


}
