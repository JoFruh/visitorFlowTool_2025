

#' @importFrom dplyr .data
#' @importFrom dplyr %>%

# Define server logic

# envUpdate <- new.env(parent = emptyenv())
# envBtn <- new.env(parent = emptyenv())

#' @param shape the overall study perimeter from step 1 (app-level `r$shape`).
#'   Used to crop the land cover baseline under the paint. It has to be passed in
#'   rather than read off `r`: this module makes its own reactiveValues, so the
#'   `r` in here is module-local and its `polygonsList` is the AoI polygons, not
#'   the perimeter. step5_server and lastStep_server take `shape` the same way.
newVersions_server <- function(id, networkList, confirm, i18n, currentLang, isFirstRun, SM_pres, SMcolors, shp_PA, finalPolygons = NULL, versionsUI = list(), trigger = 0,
                               DULN = NULL, shape = NULL){

  #count this instantiation. A module server should be created once per
  #session; this app re-calls it from an observeEvent on a trigger, so any
  #count above 1 means a duplicate set of observers and outputs is now live
  #alongside the previous one. See vftModuleInstance() in perf_helpers.R.
  vftModuleInstance("newVersions")

  # r$mapRefresh <- 0
  shiny::moduleServer(id, function(input, output, session) {

    # RENDER UI
    # if(currentLang == "de"){
    #   output$contextChoice_ui <- shiny::renderUI({
    #       shiny::radioButtons(inputId = NS(id,"contextChoice"), label = NULL, inline = TRUE, choices = list("Wegen/Strassen" = 1,  "Parken/Wohnen" = 3), selected = 1)
    #     #"Beschilderung/Attraktivität" = 2,
    #   })
    # }else if(currentLang == "fr"){
    #   output$contextChoice_ui <- shiny::renderUI({
    #     shiny::radioButtons(inputId = NS(id,"contextChoice"), label = NULL, inline = TRUE, choices = list("Chemins/Routes" = 1,  "Parkings/Habitations" = 3), selected = 1)
    #     #"Beschilderung/Attraktivität" = 2,
    #
    #   })
    # }

    # #on start up
    # session$onFlushed(function() {
    #   session$sendCustomMessage("init-paintbrush", "newVersions-versionMap")
    # })


    output$contextChoice_ui <- shiny::renderUI({
      if(currentLang == "de"){
        shiny::radioButtons(
          inputId = NS(id,"contextChoice"),
          label = NULL,
          inline = TRUE,
          choices = list(
            "Wegen/Strassen" = 1,
            "Parken/Wohnen" = 3
            ,
            "Hitzeminderung" = 4  # new option in dev.
          ),
          selected = 1
        )
      } else if(currentLang == "fr"){
        shiny::radioButtons(
          inputId = NS(id,"contextChoice"),
          label = NULL,
          inline = TRUE,
          choices = list(
            "Chemins/Routes" = 1,
            "Parkings/Habitations" = 3
            ,
            "Attenuation de chaleur" = 4  # new option
          ),
          selected = 1
        )
      } else if(currentLang == "en"){
        shiny::radioButtons(
          inputId = NS(id,"contextChoice"),
          label = NULL,
          inline = TRUE,
          choices = list(
            "Paths/Roads" = 1,
            "Parking/Residences" = 3
            ,
            "Attenuation de chaleur" = 4  # new option
          ),
          selected = 1
        )
      }
    })


    # ATTR TABLE ####
    #Prepare table for determining attractivity
    translateAttrTable <- as.data.frame(matrix(byrow = TRUE, data = c(

      0, 0, 0, 0,0, 0, 0, 0,
      3,1,0,1,1,2,3,1,
      2,1,3,3,3,2,2,1,
      0,0,3,2,2,0,-1,0,
      0,2,2,2,2,2,0,1,
      3,2,-1,3,3,3,2,2,
      2,1,-2,-2,1,2,2,1,
      2,3,3,3,3,2,2,3,
      -3,-2,-1,-1,-2,-3,-3,-2,
      -3,-2,-1,-1,-2,-3,-3,-2
    ), nrow = 10),
    row.names = c("walkBike1","walkBike2","walkBike3","walkBike4","hardNatur1", "hardNatur2", "width1","width2","width3","width4")
    )
    names(translateAttrTable) <-  c("DULN_WALK_", "DULN_WALK1", "DULN_BIKER", "DULN_EBIKE", "DULN_ALL", "DULN_JOGGE", "DULN_DOG_N", "DULN_DOG_P")

    translateAttrTable_walkBike <- translateAttrTable[1:4,]
    translateAttrTable_hardNatur <- translateAttrTable[5:6,]
    translateAttrTable_roadWidth <- translateAttrTable[7:10,]


    # envNewVersions <- new.env(parent = emptyenv())

    # nodeID <- NULL
    # edgeID <- NULL
    nodes <- NULL
    edges <- NULL

    r <- shiny::reactiveValues()

    r$mapPoints <- NULL
    r$DULN <- DULN

    r$trigger <- 1

    r$networkList <- networkList
    r$position <- 1
    r$versionsUI <- versionsUI
    r$context <- 1 #infrastructure #2 = signage/attractivity, 3 = housing/parking
    r$oldContext <- 0 #save prior context (0 = no context)
    #variable to determine if render needs to be updated
    #made FALSE by ex: changing from context 1 to 2 or 2 to 1 with Original versions.
    r$updateRender <- 0

    #keeps track of specific edges and nodes across functions
    r$edgID <- NULL
    r$nodeID <- NULL
    r$originalID <- NULL

    r$currentLang <- currentLang

    shiny.i18n::update_lang(r$currentLang)
    shiny::updateSelectInput(inputId = "languageSelect_7", selected = currentLang)

    #this step never set its banner on entry - the old renderUI only ran from
    #the language selector, so the strip stayed blank until the user touched it.
    #The UI ships the German image, so only the other two need saying.
    if(identical(currentLang, "fr")){
      vftSetBanner(id, "www/stepNewVersions_wsl_fr.png")
    }else if(identical(currentLang, "en")){
      vftSetBanner(id, "www/stepNewVersions_wsl_en.png")
    }

    # parkingShape <- shiny::isolate(r$networkList[[r$position]]$parking)
    # parkingShape <- parkingShape %>% dplyr::rename(polygons = .data$`_ogr_geometry_`)
    # parkingShape <- parkingShape %>% dplyr::select(.data$polygons)
    # r$parkingPolygons <- parkingShape
    # r$parkingPolygons$id <- 1:nrow(r$parkingPolygons)
    # r$parkingPolygons$isNew <- 0

    r$polyFinished <- FALSE
    #call polygon creator and eraser
    # polygonCreator("versionMap",  input = input, startingPolygons = parkingShape, inputConditionName = "contextChoice", inputConditionValue = c(2,3)) #requires "polygons" global variable
    # polygonEraser("versionMap", input = input, startingPolygons = parkingShape)


    #empty list to hold Submit observer (destroyed when leaving tab)
    obsEvent_submit <- NULL
    obsEvent_addVersion <- NULL
    obsFinishRender <- NULL

    #TEMPORARY
    # igraph::E(r$networkList[[1]]$network)$roadWidth <- igraph::E(r$networkList[[1]]$network)$roadWidth-20
    # igraph::E(r$networkList[[1]]$network)$roadWidth <- abs(igraph::E(r$networkList[[1]]$network)$roadWidth-5)
    #TODO:
    #make environment?
    # appendedObservers <- NULL
    r$appendedObservers <- list()
#
#     r$markerWasClicked <- NULL
    r$markerWasClicked <- FALSE
#
#     r$shapeWasClicked <- NULL
    r$shapeWasClicked <- FALSE
#
#     r$isLinking <- NULL
    r$isLinking <- FALSE
#
#     r$firstLinkNode <- NULL
#     r$secondLinkNode <- NULL
#
#     r$mapView <- NULL
#
#     r$trigger <- NULL

    vftDbg("NETWORK LIST:")
vftDbg(r$networkList)


    # selectedNetwork_position <- NULL

    # networkLst <- NULL
    # networkLst <- networkList

    # networkLst <- NULL

    # ntwrkLst_r <- reactiveVal({networkLst})

    # networkLst <- networkList

    # ntwrkLst_r(networkLst)

    # updatedNetworkList <- reactive({
    #   networkLst <- ntwrkLst_r
    #   networkLst
    # })


#prepare updateNework variable if not existing (first run or shortcut)
if(is.null(r$updateNetworkPlot)){
  #variable used to force network to update
  r$updateNetworkPlot <- shiny::reactiveVal(0)
}

    #ONLY FIRST RUN####
    if(isFirstRun){


      vftDbg("NEW VERSIONS FIRST RUN")

      r$versionBtn_nb <- 1


      # r$startNetworkPlot <- shiny::reactiveVal()

      name <- NULL
      # baseEnv$inserted_id_ui <- c()
      # baseEnv$inserted_inputId_select <- c()
      # baseEnv$inserted_inputId_removal <- c()

      #select original network at start (reactive)
      # networkLst <- ntwrkLst_r()
      # selectedNetwork_r <- reactiveVal(list(networkLst[[1]]$network) )
      # selectedNetwork_position <- 1
      r$position <- 1

      #initialize memory of last selected button (to easily unselect it if another button is selected)

      r$lastSelectedButton <- NULL

      #initialize memory of last selected paint color button, per paint level, so flipping the
      #ground/canopy switch restores that level's own previously selected material
      r$lastSelectedGroundButton <- "paintColor_grass"
      r$lastSelectedCanopyButton <- "paintColor_canopyTree"
      #the one button currently highlighted, which may be a "both" material belonging
      #to neither level. Matches the class the UI ships paintColor_grass with.
      r$selectedPaintButton      <- "paintColor_grass"

      shinyjs::disable("newVersionsConfirmButton")
      shinyjs::disable("addVersionButton")
      vftDbg("BTN$INPUTID: ")

      for(btn in r$versionsUI){
        vftDbg(btn$inputId_select)
        shinyjs::disable(btn$inputId_select)
        if(!is.null(btn$inputId_removal)){
          shinyjs::disable(btn$inputId_removal)
        }
      }
    }

    #PLOT CURRENTLY SELECTED CONTEXT ####
    #depending on context
    output$versionMap <- leaflet::renderLeaflet({
      r$updateRender #create reactive link to control render

      # print(paste0("contextchoice:",input$contextChoice))


        # RENDER INFRASTRUCTURE ####

        # shinyjs::runjs(paste0("document.getElementById('newVersions-versionMap').style.borderColor ='red'"))
        # shinyjs::runjs(paste0("document.getElementById('newVersions-versionMap').style.borderWidth = 'thick'"))

        #default bounds
        #use shape to determine starting bounds

        #save to, from, edgeID and nodeID as new columns
        #the plot uses these IDs in a fixed way, whereas the original columns can automatically change
        shiny::isolate(network <- r$networkList[[r$position]]$network)
        network <- network %>% tidygraph::activate(nodes) %>% dplyr::mutate(nodeID_2 = .data$nodeID)
        network <- network %>% tidygraph::activate(edges) %>% dplyr::mutate(edgeID_2 = .data$edgeID, to_2 = .data$to, from_2 = .data$from)


        shiny::isolate(r$networkList[[r$position]]$network <- network )

        #interactive map: mode is set once for the process in global.R

        #use this reactive value here only to trigger plotting when value is changed
        r$updateNetworkPlot()
        # r$mapRefresh
        #
        # print("r$networkList: ")
        # print(isolate(r$networkList))
        #
        # print("r$position: ")
        # print(isolate(r$position))
        #







        leaflet::leafletOptions(doubleClickZoom= FALSE)


        # map <- leaflet()%>% addTiles() %>% leaflet::fitBounds(7.69, 47.37, 8.15, 47.23 )%>%
        #   leaflet.extras2::addSpinner()%>%
        #   leaflet.extras2:: startSpinner()
        #
        # map
        #
        #
        # map <- addCircleMarkers(map, data = nodeTbl %>% st_as_sf(), radius = 5, fillColor = "white", weight = 1, color = "grey")%>%
        #   leaflet::addPolylines(data = edgeTbl %>% st_as_sf(), weight = 3, color = "black")%>%
        #   addMapPane("layer1", zIndex = 410)%>% addMapPane("layer2", zIndex = 420)%>% addMapPane("layer3", zIndex = 450) %>%
        #   leaflet.extras2::stopSpinner()
        #
        # map
        #Arm or disarm paint mode from the context itself, in one place.
        #
        #This used to send FALSE here and rely on the context 4 branch below to
        #send TRUE again, which made the brush depend on both messages arriving
        #in one render. Any render that reached this line without reaching that
        #branch - and the dummy-group trick further down deliberately causes
        #re-renders - left paint disarmed with nothing to turn it back on. The
        #symptom is silent and total: `active` false hides both paint panes and
        #switches off the input overlay, so the land cover vanishes and the brush
        #stops responding, with no error anywhere.
        #
        #Deriving it from contextChoice removes the ordering entirely: whatever
        #else a render does, it leaves paint armed if and only if the user is on
        #the paint context.
        #
        #"Unknown" is not "no". input$contextChoice is empty on a render that
        #runs before the radio button has reported in - which happens, because
        #the dummy-group trick below deliberately provokes re-renders - and
        #treating that as "not context 4" is what disarms the brush behind the
        #user's back. Silence leaves the flag alone instead, so only a render
        #that positively knows the context can change it.
        ctxChoice    <- shiny::isolate(input$contextChoice)
        ctxKnown     <- length(ctxChoice) == 1 && !is.na(ctxChoice)
        paintContext <- ctxKnown && isTRUE(ctxChoice == 4)

        #only the unusual case is worth a line: a render that cannot tell which
        #context it is in, which is the one that used to disarm the brush
        if(!ctxKnown || isTRUE(getOption("vft.paintDebug", FALSE))){
          message(sprintf("paint: render, contextChoice=%s -> %s",
                          if(!ctxKnown) "<unset>" else as.character(ctxChoice),
                          if(!ctxKnown) "left as is" else if(paintContext) "ARMED" else "disarmed"))
        }

        if(ctxKnown){
          session$sendCustomMessage(type = "set-paint-active", message = paintContext)
          if(paintContext) shinyjs::show(id = "paintColorButtonsDiv")
          else             shinyjs::hide(id = "paintColorButtonsDiv")
        }

        #RENDERING INFRASTRUCTURE/SIGNAGE
        # if(shiny::isolate(!is.null(input$contextChoice))){
          if(shiny::isolate(input$contextChoice == 1)){

            if(r$position != 1){

              #prepare data of current version
              edgeTbl <- shiny::isolate(r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% dplyr::as_tibble())
              edgeTbl <- edgeTbl %>% dplyr::relocate(.data$edgeID)

              # edgesShape <- edgeTbl  %>% sf::st_as_sf()

              vftDbg("NETWORK EDGES:")
              vftDbg(edgeTbl)

              nodeTbl <- shiny::isolate(r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% dplyr::as_tibble())
              #PLOT INTERACTIVE NETWORK MAP (if not original)
              sfData <- sf::st_zm(sf::st_as_sf(dplyr::as_tibble(network %>% tidygraph::activate(edges)) ), drop = T, what = "ZM")
              pal <- leaflet::colorNumeric(c("black", "#e8e22e", "#3ddb68", "#35caf0"), domain = 1:4)
              palDash <- plyr::mapvalues(sfData$hardNatur, from = c(1, 2), to = c("1", "4 6") )

              map <- leaflet::leaflet(data = sfData, options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE) ) %>%
                leaflet::addMapPane("layer_SM", zIndex = 405)%>%
                leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450) %>%
                leaflet::addProviderTiles("OpenStreetMap.CH", options = leaflet::providerTileOptions(opacity = 0.3, zIndex = 400)) %>%
                leaflet::addPolylines(stroke = TRUE,
                                      weight = ~roadWidth,
                                      color = ~pal(sfData$walkBike),
                                      fill = FALSE,
                                      opacity = 1,
                                      options = leaflet::pathOptions(pane = "layer1"),
                                      layerId = as.character(sfData$edgeID_2),
                                      dashArray = palDash,
                                      highlightOptions = leaflet::highlightOptions(weight = 9),
                                      group = "paths")%>%
                leaflet::addCircleMarkers(lat = shiny::isolate(sf::st_coordinates(igraph::V(r$networkList[[r$position]]$network)$geometry) [,"Y"] ) ,
                                          lng = shiny::isolate(sf::st_coordinates(igraph::V(r$networkList[[r$position]]$network)$geometry )[,"X"]) ,
                                          color = "grey",
                                          opacity = 1,
                                          fillOpacity = 1,
                                          radius = 5,
                                          fillColor = "white",
                                          stroke = TRUE,
                                          layerId = shiny::isolate(as.character(igraph::V(r$networkList[[r$position]]$network)$nodeID_2)),
                                          weight = 1,
                                          group = "nodes",
                                          options = leaflet::pathOptions(pane = "layer3")

                )%>%
                leaflet::addLayersControl(overlayGroups = c("paths", "nodes"))%>%
                leaflet::addLegend(position = "topright", title = "Signage:",  values = c(1, 2, 4, 3), colors = c("black", "#e8e22e", "#35caf0",  "#3ddb68"), labels = c("none", "walking routes", "cycling routes", "both"))%>%
                leaflegend::addLegendImage(position = "topright", title = "Surface:", images = c("www/solid.png", "www/dashed.png"), labels = c("asphalt", "natural"),
                                           labelStyle = "font-size: 15px; vertical-align: left")

              #
              #
              # map <- tmap::tmap_leaflet(
              #
              #   tmap::tm_shape(edgesShape[edgesShape$walkBike == 1 & edgesShape$hardNatur == 1,]) +
              #     tmap::tm_lines(col = "black", lwd = "roadWidth", popup.vars = FALSE, group = "edges", zindex = 420, scale = 4, lty = 1) +
              #
              #     tmap::tm_shape(edgesShape[edgesShape$walkBike %in% c(2,3,4) & edgesShape$hardNatur == 1,]) +
              #     tmap::tm_lines(col = "walkBike", style = "cat", lwd = "roadWidth", popup.vars = FALSE, lty = 1, group = "edges", zindex = 420, scale = 4,
              #                    palette = c("#e8e22e", "#3ddb68", "#35caf0")) +
              #     tmap::tm_shape(edgesShape[edgesShape$walkBike == 1 & edgesShape$hardNatur == 2,]) +
              #     tmap::tm_lines(col = "black", lwd = "roadWidth", lty = 3, popup.vars = FALSE, group = "edges", zindex = 420, scale = 4) +
              #
              #     tmap::tm_shape(edgesShape[edgesShape$walkBike %in% c(2,3,4) & edgesShape$hardNatur == 2,]) +
              #     tmap::tm_lines(col = "walkBike", style = "cat", lwd = "roadWidth", lty = 3, popup.vars = FALSE, group = "edges", zindex = 420, scale = 4,
              #                    palette = c("#e8e22e", "#3ddb68", "#35caf0")) +
              #     tmap::tm_shape(nodeTbl %>% sf::st_as_sf()) +
              #     tmap::tm_dots(size = 0.05, col = "white", popup.vars = FALSE, group = "nodes", zindex = 420) +
              #
              #     tmap::tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = c(0.3) ),
              #   options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE),
              #   in.shiny = TRUE) %>%
              #   leaflet::addMapPane("layer_SM", zIndex = 405)%>%
              #   leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450)
              # #

              # %>%
              #   htmlwidgets::onRender(
              #
              #       "function(el, x) {
              #       var myMap = this;
              #       const canvasRenderer = L.canvas({tolerance: 20});
              #       myMap.map = L.map({padding: 0.3; renderer: canvasRenderer});}")

              # leaflet::fitBounds(7.69, 47.37, 8.15, 47.23 )

              #create Leaflet options
              canvasMap <- leaflet::leaflet(options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE) ) #preferCanvas = TRUE,
              #apply them to existing leaflet object
              map$x$options <- canvasMap$x$options

            }else{
              #PLOT ORIGINAL PATHS ####

              edgeTbl <- shiny::isolate(r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% dplyr::as_tibble())

              edgeTbl <- edgeTbl %>% dplyr::relocate(.data$edgeID)

              # edgesShape <- edgeTbl  %>% sf::st_as_sf()

              vftDbg("NETWORK EDGES:")
              vftDbg(edgeTbl)

              # nodeTbl <- shiny::isolate(r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% dplyr::as_tibble())
              #PLOT INTERACTIVE NETWORK MAP (if not original)
              sfData <- sf::st_zm(sf::st_as_sf(dplyr::as_tibble(network %>% tidygraph::activate(edges)) ), drop = T, what = "ZM")
              # map <- tmap::tmap_leaflet(
              #
              #   tmap::tm_shape(edgeTbl  %>% sf::st_as_sf()) +
              #     tmap::tm_lines(col = "darkgrey", lwd = 3, style = "fixed", popup.vars = FALSE, group = "edges", zindex = 420, interactive = FALSE) +
              #
              #
              #     tmap::tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = c(0.5) ),
              #   options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE),
              #   in.shiny = TRUE) %>%
              #   leaflet::addMapPane("layer_SM", zIndex = 405)%>%
              #   leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450)

              map <- leaflet::leaflet(data = sfData, options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE) ) %>%
                leaflet::addMapPane("layer_SM", zIndex = 405)%>%
                leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450) %>%
                leaflet::addProviderTiles("OpenStreetMap.CH", options = leaflet::providerTileOptions(opacity = 0.3, zIndex = 400)) %>%
                leaflet::addPolylines(stroke = TRUE,
                                      weight = ~roadWidth,
                                      color = "#7a7a7a",
                                      fill = FALSE,
                                      opacity = 1,
                                      options = leaflet::pathOptions(pane = "layer1"),
                                      layerId = as.character(sfData$edgeID_2),
                                      dashArray = 1,
                                      highlightOptions = leaflet::highlightOptions(weight = 9),
                                      group = "paths")%>%
                leaflet::addLayersControl(overlayGroups = c("paths", "nodes"))
            }
          }else if(shiny::isolate(input$contextChoice == 2)){
            # RENDER ATTRACTIVITY ####
            if(r$position != 1){

              newAttrShapes <-  shiny::isolate(r$networkList[[r$position]]$newAttr)


              # #PLOT INTERACTIVE PATHS (if not original)
              # map <- tmap::tmap_leaflet(
              #   tmap::tm_shape(newAttrShapes) +
              #     tmap::tm_borders(col = "darkgreen", lwd = 3, group = "polys", zindex = 420, lty = 7) +
              #
              #     tmap::tm_fill(col = "green", alpha = 0.3)+
              #
              #     tmap::tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = c(0.5) ),
              #   options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE),
              #   in.shiny = TRUE) %>%
              #   leaflet::addMapPane("layer_SM", zIndex = 405)%>%
              #   leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450)
              # # %>%
              # # leaflet::fitBounds(7.69, 47.37, 8.15, 47.23 )
            }else{
              #PLOT ORIGINAL PATHS ####
              # map <- tmap::tmap_leaflet(

              # tmap::tm_shape(edgeTbl  %>% sf::st_as_sf()) +
              #   tmap::tm_lines(col = "darkgrey", lwd = 3, style = "fixed", popup.vars = FALSE, group = "edges", zindex = 420, interactive = FALSE) +
              #
              #
              #   tmap::tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = c(0.5) ),
              # options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE),
              # in.shiny = TRUE) %>%
              # leaflet::addMapPane("layer_SM", zIndex = 405)%>%
              # leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450)

            }
          }else if(shiny::isolate(input$contextChoice == 3)){

            #reset mapPoints
            shiny::isolate(r$mapPoints <- NULL)

            # RENDER PARKING/HOUSING ####
            if(r$position != 1){
              #PLOT INTERACTIVE PARKING (if not original)

              # map <- tmap::tmap_leaflet(
              #   tmap::tm_shape(parkingShape) +
              #     tmap::tm_borders(col = "#1127b8", lwd = 3, group = "eraseable", zindex = 420) +
              #     tmap::tm_fill(col = "#1127b8", alpha = 0.3, popup.vars = FALSE, group = "eraseable")+
              #     tmap::tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = c(0.5) ),
              #   options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE),
              #   in.shiny = TRUE)
              #prepare color palette
              pal <- leaflet::colorNumeric(c("#1127b8", "#e83017"), domain = c(1,0))
              shiny::isolate( r$parkingPolygons <- r$networkList[[r$position]]$parking)
              shiny::isolate( r$residentialPolygons <- r$networkList[[r$position]]$residential)

              #if the retrieved parking is empty, use the Original source
              if( shiny::isolate(nrow(r$parkingPolygons) == 0) ){

                shiny::isolate(r$parkingPolygons <- shiny::isolate(r$networkList[[1]]$parking) )
              }

              map <- leaflet::leaflet(data = shiny::isolate(r$parkingPolygons))%>%
                leaflet::addMapPane("layer_SM", zIndex = 405)%>%
                leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450) %>%
                leaflet::addProviderTiles("OpenStreetMap.CH", options = leaflet::providerTileOptions(opacity = 0.3, zIndex = 400)) %>%
                leaflet::addPolygons(
                  layerId = ~paste0("p",id),
                  stroke = TRUE,
                  weight = 5,
                  color = ~pal(shiny::isolate(r$parkingPolygons$isNew) ),
                  fill = TRUE,
                  fillColor = "#1127b8",
                  opacity = 1,
                  fillOpacity = 0.3,
                  group = "eraseable",
                  options = leaflet::pathOptions(pane = "layer2"),
                  highlightOptions = leaflet::highlightOptions(fillColor = "#f2778d"))%>%
                leaflet::addMapPane("layer_SM", zIndex = 405)%>%
                leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450)

              #add residential polygon data (but only if it is not empty)
              if(shiny::isolate(!is.null(r$residentialPolygons))){
                map <- map %>%
                  leaflet::addPolygons(data = shiny::isolate(r$residentialPolygons),
                                       layerId = ~paste0("r",id),
                                       stroke = TRUE,
                                       weight = 5,
                                       color = "#ba8e16",
                                       fill = TRUE,
                                       fillColor = "#ba8e16",
                                       opacity = 1,
                                       fillOpacity = 0.3,
                                       group = "eraseable",
                                       options = leaflet::pathOptions(pane = "layer2"),
                                       highlightOptions = leaflet::highlightOptions(fillColor = "#f2778d")

                  )
              }
              # %>%
              # leaflet::fitBounds(7.69, 47.37, 8.15, 47.23 )
            }else{

              shiny::isolate(r$parkingPolygons <- r$networkList[[r$position]]$parking)

              #PLOT ORIGINAL PARKING ####
              map <- leaflet::leaflet(shiny::isolate(r$parkingPolygons))%>%
                leaflet::addMapPane("layer_SM", zIndex = 405)%>%
                leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450) %>%
                leaflet::addProviderTiles("OpenStreetMap.CH", options = leaflet::providerTileOptions(opacity = 0.3, zIndex = 400)) %>%
                leaflet::addPolygons(
                  layerId = ~paste0("p", id),
                  stroke = TRUE,
                  weight = 5,
                  color = "darkgrey",
                  fill = TRUE,
                  fillColor = "grey",
                  opacity = 1,
                  fillOpacity = 0.3,
                  options = leaflet::pathOptions(pane = "layer2"))%>%
                leaflet::addMapPane("layer_SM", zIndex = 405)%>%
                leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450)
              # %>%
            }
          }else if(shiny::isolate(input$contextChoice == 4)){
          #RENDER HEAT MITIGATION ####
            shiny::isolate(r$parkingPolygons <- r$networkList[[r$position]]$parking)
            if(shiny::isolate(nrow(r$parkingPolygons) == 0)){
              shiny::isolate(r$parkingPolygons <- r$networkList[[1]]$parking)
            }

            map <- leaflet::leaflet(shiny::isolate(r$parkingPolygons))%>%
              leaflet::addMapPane("layer_SM", zIndex = 405)%>%
              leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450) %>%
              leaflet::addProviderTiles("OpenStreetMap.CH", options = leaflet::providerTileOptions(opacity = 0.3, zIndex = 400)) %>%
              leaflet::addMapPane("layer_SM", zIndex = 405)%>%
              leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450)%>%
              #panes keeping the canopy layer above the ground layer. One pane per
              #level: the land cover baseline and the paint share a canvas inside
              #it, so that paint occludes the baseline instead of blending with it
              leaflet::addMapPane("paintPaneGround", zIndex = 415)%>% leaflet::addMapPane("paintPaneCanopy", zIndex = 425)%>%
              #the heat surface sits above the paint (it is opaque, and it is what
              #you are reading while it is on) but below layer3, so the path
              #network stays visible over it
              leaflet::addMapPane("heatPane", zIndex = 430)

            session$sendCustomMessage(type="set-paint-active", message=TRUE)

            #show paint color buttons, restoring the material remembered for the active level
            canopyActive <- shiny::isolate(isTRUE(input$paintLevel))
            shinyjs::show(id = "paintColorButtonsDiv")
            setPaintLevelButtons(canopyActive)
            shiny::isolate(applyPaintLevelColor(session, r, if(canopyActive) "canopy" else "ground"))

            #hand this version's painted state to the browser, which owns the display from
            #here on: R draws no raster layers at all. The coordinate fit is anchored on the
            #study area rather than the current view, so it stays valid as the user pans
            #around. The browser holds these messages until the new map instance exists,
            #so it does not matter that they are sent from inside the render.
            #The area to work on is the overall perimeter from step 1, passed in
            #as `shape`. Deliberately not r$polygonsList: `r` here is this
            #module's own reactiveValues and its polygonsList holds the AoI
            #polygons - a scatter of small areas whose bounding box spans the
            #whole site while covering almost none of it. Cropping to those gives
            #a handful of disconnected patches instead of the working area.
            #
            #The parking polygons remain the fallback, so a session that somehow
            #arrives here without a perimeter still gets an anchor rather than an
            #error out of st_bbox().
            paintAOI <- shape
            if(is.null(paintAOI) || length(sf::st_geometry(paintAOI)) == 0){
              paintAOI <- shiny::isolate(r$parkingPolygons)
            }

            #paint-grid-init is the message the browser cannot do without: it
            #carries the coordinate fit, and with no fit chunkTransform() returns
            #null, so nothing draws AND no stroke registers. Anything that throws
            #on the way to sending it therefore takes out the whole brush, and
            #does it silently - the map just sits there looking empty.
            #
            #So it is sent defensively, and a failure says which of the two it
            #was rather than leaving a blank canvas to be interpreted.
            paintOK <- tryCatch({
              paintBB <- sf::st_bbox(sf::st_transform(paintAOI, 4326))
              stopifnot(all(is.finite(as.numeric(paintBB))))
              session$sendCustomMessage("paint-grid-init", paintInitPayload(
                mean(c(paintBB[["xmin"]], paintBB[["xmax"]])),
                mean(c(paintBB[["ymin"]], paintBB[["ymax"]]))
              ))
              TRUE
            }, error = function(e){
              message("paint: could NOT initialise the paint grid - the brush and ",
                      "the land cover will both be inert. ", conditionMessage(e))
              message("paint: AOI was ", if(is.null(paintAOI)) "NULL" else
                      paste(class(paintAOI), collapse = "/"),
                      "; shape arg was ", if(is.null(shape)) "NULL" else
                      paste(class(shape), collapse = "/"))
              FALSE
            })
            #the surveyed land cover for this area, as a read-only layer under the
            #paint. It is deliberately NOT merged into paintedRaster: keeping the
            #two apart is what leaves "what was already there" and "what the user
            #changed" still separable afterwards, which is the whole point of a
            #heat-mitigation before/after. It also keeps saves small, since a
            #version stores only edits.
            #
            #Cropped and masked to the step-1 outline, so the land cover appears
            #in the shape of the study area rather than as a rectangle around it.
            #
            #NULL (rasters not built, or an area past paintLandcoverSeed()'s
            #ceiling) sends NULL images, which clears the baseline rather than
            #leaving the previous version's on screen.
            baseMsg <- if(paintOK) tryCatch(paintLandcoverBaselinePNG(paintAOI),
                                            error = function(e){
                                              message("paint: baseline encode failed - ",
                                                      conditionMessage(e))
                                              NULL
                                            }) else NULL
            if(is.null(baseMsg)){
              #print the whole diagnosis here rather than inviting the user to run
              #it: the app owns the console while it is running, so "call this
              #function to find out why" is advice that cannot be taken. A blank
              #canvas looks identical whether the rasters are missing, the area is
              #too big, or step 1 left no outline, so the reason has to arrive
              #unasked. Wrapped because a diagnostic must never break the render.
              message("paint: no land cover baseline for this area -")
              try(paintLandcoverDiagnose(paintAOI), silent = FALSE)
              baseMsg <- list(ground = NULL, canopy = NULL)
            }else{
              message(sprintf("paint: land cover baseline %d x %d, %.0f KB",
                              baseMsg$w, baseMsg$h,
                              (nchar(baseMsg$ground) + nchar(baseMsg$canopy)) / 1e3))
            }
            session$sendCustomMessage("paint-base-load", baseMsg)

            session$sendCustomMessage("paint-grid-load", list(
              version = shiny::isolate(r$position),
              ground  = rasterToRuns(shiny::isolate(r$networkList[[r$position]]$paintedRaster)),
              canopy  = rasterToRuns(shiny::isolate(r$networkList[[r$position]]$canopyRaster))
            ))
            session$sendCustomMessage("set-paint-level", list(canopy = canopyActive))
          }

        #add or remove dummy group (this is to trigger an observer that determines when the map finished rendering)
        #in isolation to avoid linking input$versionMap_groups
        vftDbg("GROUP LENGTH: ")
        vftDbg(shiny::isolate(length(input$versionMap_groups)) )
        if( shiny::isolate(!"dummy" %in% input$versionMap_groups ) ){
          vftDbg("ADDING GROUP")
          #add a group (with invisible marker)
          leaflet::leafletProxy("versionMap" )%>%
            leaflet::addCircles(lng = 7.69, lat = 47.37,
                                opacity = 0, fill = FALSE,
                                group = "dummy")
        }else{
          vftDbg("REMOVING GROUP")
          #remove the group
          leaflet::leafletProxy("versionMap" )%>%
            leaflet::clearGroup("dummy")
        }


        #if there is a saved map view (bounds), use to to refocus map
        if(shiny::isolate(!is.null(r$mapView$zoom))){

          map <- shiny::isolate(leaflet::setView(map, r$mapView$center_lng, r$mapView$center_lat, r$mapView$zoom) )
        }


        vftDbg("GROUPS: ")
        shiny::isolate(vftDbg(input$versionMap_groups))

    # }else{
      # #prepare data of current version
      # edgeTbl <- shiny::isolate(r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% dplyr::as_tibble())
      # edgeTbl <- edgeTbl %>% dplyr::relocate(.data$edgeID)
      #
      # # edgesShape <- edgeTbl  %>% sf::st_as_sf()
      #
      # print("NETWORK EDGES:")
      # print(edgeTbl)
      #
      # nodeTbl <- shiny::isolate(r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% dplyr::as_tibble())
      # #PLOT INTERACTIVE NETWORK MAP (if not original)
      # sfData <- sf::st_zm(sf::st_as_sf(dplyr::as_tibble(network %>% tidygraph::activate(edges)) ), drop = T, what = "ZM")
      # pal <- leaflet::colorNumeric(c("black", "#e8e22e", "#3ddb68", "#35caf0"), domain = 1:4)
      # palDash <- plyr::mapvalues(sfData$hardNatur, from = c(1, 2), to = c("1", "4 6") )
      #
      # map <- leaflet::leaflet(data = sfData, options = leaflet::leafletOptions(doubleClickZoom = FALSE, preferCanvas = TRUE) ) %>%
      #   leaflet::addMapPane("layer_SM", zIndex = 405)%>%
      #   leaflet::addMapPane("layer1", zIndex = 410)%>% leaflet::addMapPane("layer2", zIndex = 420)%>% leaflet::addMapPane("layer3", zIndex = 450) %>%
      #   leaflet::addProviderTiles("OpenStreetMap.CH", options = leaflet::providerTileOptions(opacity = 0.3, zIndex = 400))
      # }

      map

    })


#OBSERVERS ####

# PAINT MODE OBSERVERS

#which button drives which paint category. Everything else about a material - its
#color and its level - is read from PAINT_CATEGORIES (paintbrush_helpers.R), which
#the browser also holds a copy of, so a color is defined in exactly one place.
PAINT_BUTTONS <- data.frame(
  inputId = c("paintColor_grass", "paintColor_bush", "paintColor_artificial",
              "paintColor_natural", "paintColor_water",
              "paintColor_canopyArtificial", "paintColor_canopyTree",
              "paintColor_block"),
  id      = PAINT_CATEGORIES$id,
  level   = PAINT_CATEGORIES$level,
  stringsAsFactors = FALSE
)

#name of the reactiveValues slot remembering the selected button of a given level,
#or NULL for a material that belongs to no level ("both") and so is never the thing
#the level switch restores
lastColorButtonSlot <- function(level){
  switch(level, canopy = "lastSelectedCanopyButton", ground = "lastSelectedGroundButton", NULL)
}

#toggle mutually-exclusive paint color buttons. Selection is global - only one
#material is ever armed - while the per-level memory is what the level switch
#restores, so a "both" material can be selected without displacing either level's
#remembered choice.
#`force` re-sends the color to the browser even when the button is already the
#selected one - needed when the level switch flips, since the newly active level's
#remembered button is usually unchanged but the brush still has to be re-pointed at it.
setPaintColor <- function(session, r, inputId, id, level = "ground", force = FALSE){
  if(!force && identical(r$selectedPaintButton, inputId)) return(invisible(NULL))

  if(!is.null(r$selectedPaintButton) && r$selectedPaintButton != inputId){
    shinyjs::removeClass(r$selectedPaintButton, "colorBtnSelected")
    shinyjs::addClass(r$selectedPaintButton, "colorBtnNotSelected")
  }
  shinyjs::removeClass(inputId, "colorBtnNotSelected")
  shinyjs::addClass(inputId, "colorBtnSelected")
  r$selectedPaintButton <- inputId

  slot <- lastColorButtonSlot(level)
  if(!is.null(slot)) r[[slot]] <- inputId

  #only the id travels: the browser already has every material's color and level
  session$sendCustomMessage("set-paint-color", list(id = id))
}

#select the remembered material of `level` and point the brush at it
applyPaintLevelColor <- function(session, r, level, force = TRUE){
  btn <- shiny::isolate(r[[lastColorButtonSlot(level)]])
  row <- PAINT_BUTTONS[match(btn, PAINT_BUTTONS$inputId), ]
  setPaintColor(session, r, row$inputId, row$id, level = level, force = force)
}

#enable the buttons of the active level and dim/disable the other level's.
#"both" materials are always available - the switch says which layer you are
#editing, and they edit every layer regardless.
setPaintLevelButtons <- function(canopyActive){
  activeLevel <- if(canopyActive) "canopy" else "ground"
  for(i in seq_len(nrow(PAINT_BUTTONS))){
    btn <- PAINT_BUTTONS$inputId[i]
    if(PAINT_BUTTONS$level[i] %in% c(activeLevel, "both")){
      shinyjs::enable(btn)
      shinyjs::removeClass(btn, "paintBtnDisabled")
    }else{
      shinyjs::disable(btn)
      shinyjs::addClass(btn, "paintBtnDisabled")
    }
  }
}

shiny::observeEvent(input$paintColor_grass, {
  setPaintColor(session, r, "paintColor_grass", 1)
})
shiny::observeEvent(input$paintColor_bush, {
  setPaintColor(session, r, "paintColor_bush", 2)
})
shiny::observeEvent(input$paintColor_artificial, {
  setPaintColor(session, r, "paintColor_artificial", 3)
})
shiny::observeEvent(input$paintColor_natural, {
  setPaintColor(session, r, "paintColor_natural", 4)
})
shiny::observeEvent(input$paintColor_water, {
  setPaintColor(session, r, "paintColor_water", 5)
})
shiny::observeEvent(input$paintColor_canopyArtificial, {
  setPaintColor(session, r, "paintColor_canopyArtificial", 6, level = "canopy")
})
shiny::observeEvent(input$paintColor_canopyTree, {
  setPaintColor(session, r, "paintColor_canopyTree", 7, level = "canopy")
})
#fills the ground and canopy rasters at once; selecting it leaves both levels'
#remembered materials alone, so flipping the switch returns to the last real
#ground/canopy material rather than staying on the block
shiny::observeEvent(input$paintColor_block, {
  setPaintColor(session, r, "paintColor_block", 8, level = "both")
})

# The browser's own view of the paint state, printed in the R console.
#
# Sent unasked whenever the paint layers attach, arm, or finish decoding a
# baseline. The browser console is not always reachable - an embedded viewer may
# have none - and a blank map is exactly when its internals matter, so they are
# brought to the console that is always there rather than left behind a
# developer-tools window.
#
# Read it top down: mapFound false means the widget was never located and
# nothing else ran; hasTransform false means the coordinate fit never arrived,
# which disables drawing AND painting together; panes MISSING means attach never
# completed; baseGround 0 chunks means the land cover never decoded.
shiny::observeEvent(input$paintDebug, {
  d <- input$paintDebug
  s <- d$state
  if(is.null(s)) return(NULL)

  yn  <- function(x) if(isTRUE(x)) "yes" else "NO"
  pane <- function(p){
    if(is.character(p)) return(p)
    sprintf("opacity %s, display '%s'",
            if(is.null(p$opacity) || !nzchar(as.character(p$opacity))) "unset" else p$opacity,
            if(is.null(p$display)) "" else p$display)
  }

  #Only speak up when something is actually wrong, or when explicitly asked.
  #A healthy paint step reports on three events per render, and a diagnostic
  #that prints on every success trains you to skim past it - which is precisely
  #when you stop noticing the run where it says something different. Set
  #options(vft.paintDebug = TRUE) to see it regardless.
  healthy <- isTRUE(s$mapFound) && isTRUE(s$attached) && isTRUE(s$overlay) &&
             isTRUE(s$active)   && isTRUE(s$hasTransform) &&
             !identical(s$panes$paintPaneGround, "MISSING") &&
             (is.null(s$lastBase) || s$chunks$baseGround > 0)
  if(healthy && !isTRUE(getOption("vft.paintDebug", FALSE))) return(NULL)

  message("\n--- paint state in the browser (", d$why, ") ---")
  message(sprintf("  map found        : %s   attached: %s   overlay: %s",
                  yn(s$mapFound), yn(s$attached), yn(s$overlay)))
  message(sprintf("  paint armed      : %s   canopy level: %s   erasing: %s",
                  yn(s$active), yn(s$canopyActive), yn(s$erasing)))
  message(sprintf("  coordinate fit   : %s   res: %s   colours: %s",
                  yn(s$hasTransform), s$res, s$colors))
  message(sprintf("  layers on map    : ground %s, canopy %s",
                  yn(s$layers$ground), yn(s$layers$canopy)))
  message(sprintf("  pane ground      : %s", pane(s$panes$paintPaneGround)))
  message(sprintf("  pane canopy      : %s", pane(s$panes$paintPaneCanopy)))
  message(sprintf("  chunks drawn     : baseline %s/%s, paint %s/%s (ground/canopy)",
                  s$chunks$baseGround, s$chunks$baseCanopy,
                  s$chunks$paintGround, s$chunks$paintCanopy))
  if(is.null(s$lastBase)){
    message("  baseline message : none received")
  }else{
    message(sprintf("  baseline message : %s x %s at col0 %s rowTop %s (ground %s, canopy %s)",
                    s$lastBase$w, s$lastBase$h, s$lastBase$col0, s$lastBase$rowTop,
                    yn(s$lastBase$ground), yn(s$lastBase$canopy)))
  }

  #the order events actually happened in. A single end-state flag says what is
  #true, not who made it so, and that distinction is the whole difficulty when
  #something arms and is then quietly disarmed.
  if(length(s$trace)){
    message("  event trace (oldest first):")
    for(i in seq_along(s$trace)) message(sprintf("    %2d. %s", i, s$trace[[i]]))
  }

  #say what the numbers mean, so the first line of the diagnosis is not left to
  #whoever is reading the log at the time
  if(!isTRUE(s$mapFound)){
    message("  => the map widget was not found; nothing attaches. Everything below is moot.")
  }else if(!isTRUE(s$hasTransform)){
    message("  => no coordinate fit: paint-grid-init never arrived. Brush and drawing are both inert.")
  }else if(identical(s$panes$paintPaneGround, "MISSING")){
    message("  => panes missing: attach() did not complete.")
  }else if(isTRUE(s$active) && s$chunks$baseGround == 0 && !is.null(s$lastBase)){
    message("  => baseline was sent but decoded to 0 chunks: the PNG is empty or failed to decode.")
  }else if(isTRUE(s$active) && s$chunks$baseGround > 0){
    message("  => browser state looks healthy; if the map is blank the issue is in drawing/transform.")
  }
}, ignoreInit = TRUE)

# HEAT LAYER
#
# Draws or clears the heat surface through leafletProxy rather than by
# re-rendering: a re-render would rebuild the map and take the paint canvas with
# it, so toggling heat would cost the user their view and force the browser to
# replay the whole baseline.
drawHeat <- function(){
  heat <- shiny::isolate(r$heatRaster)
  if(is.null(heat)) return(invisible(NULL))
  pal <- heatPalette(heat)
  leaflet::leafletProxy("versionMap") %>%
    leaflet::clearGroup("heat") %>%
    leaflet::removeControl("heatLegend") %>%
    #a plain list, not gridOptions(): that helper silently drops `pane` and
    #returns FALSE, and tileOptions() carries pane but bolts on zIndex and
    #detectRetina, which mean nothing to an image overlay. The options list is
    #handed straight to L.imageOverlay, where `pane` is all that is needed.
    leaflet::addRasterImage(raster::raster(heat), colors = pal, group = "heat",
                            opacity = HEAT_OPACITY, project = TRUE,
                            options = list(pane = "heatPane")) %>%
    leaflet::addLegend(layerId = "heatLegend", position = "bottomright",
                       pal = pal, values = terra::values(heat),
                       title = i18n()$t("Hitze"), opacity = 1)
  invisible(NULL)
}

clearHeat <- function(){
  leaflet::leafletProxy("versionMap") %>%
    leaflet::clearGroup("heat") %>%
    leaflet::removeControl("heatLegend")
  invisible(NULL)
}

#' Recompute from the current version's composite. Returns TRUE on success.
#'
#' Wrapped, and deliberately not fatal: the heat model is a read-out of the
#' design, so a failure here must cost the read-out and nothing else - never the
#' map or the paint the user has already done.
computeHeat <- function(){
  pos <- shiny::isolate(r$position)
  aoi <- shape
  if(is.null(aoi) || length(sf::st_geometry(aoi)) == 0){
    aoi <- shiny::isolate(r$parkingPolygons)
  }
  ok <- tryCatch({
    t0 <- Sys.time()
    h  <- heatRaster(aoi,
                     groundEdits = shiny::isolate(r$networkList[[pos]]$paintedRaster),
                     canopyEdits = shiny::isolate(r$networkList[[pos]]$canopyRaster))
    if(is.null(h)){
      message("heat: no land cover for this area - nothing to compute from")
      FALSE
    }else{
      r$heatRaster <- h
      message(sprintf("heat: computed %d x %d at %g m in %.1f s",
                      terra::nrow(h), terra::ncol(h), HEAT_RES,
                      as.numeric(difftime(Sys.time(), t0, units = "secs"))))
      TRUE
    }
  }, error = function(e){
    message("heat: computation failed - ", conditionMessage(e))
    FALSE
  })
  ok
}

# The switch. Computes on first activation, then reuses the cached raster, so
# toggling it off and back on is instant; only Refresh pays again.
shiny::observeEvent(input$heatSwitch, {
  on <- !isTRUE(shiny::isolate(r$heatOn))
  if(on){
    if(is.null(shiny::isolate(r$heatRaster)) && !computeHeat()){
      return(NULL)   #nothing to show - leave the switch off rather than lying
    }
    r$heatOn <- TRUE
    shinyjs::addClass("heatSwitch", "paintToolActive")
    drawHeat()
  }else{
    r$heatOn <- FALSE
    shinyjs::removeClass("heatSwitch", "paintToolActive")
    clearHeat()
  }
}, ignoreInit = TRUE)

# Refresh: pick up whatever has been painted since. Recomputes even when the
# layer is off, so turning it on afterwards shows the current design rather than
# a stale one.
shiny::observeEvent(input$heatRefresh, {
  if(computeHeat() && isTRUE(shiny::isolate(r$heatOn))) drawHeat()
}, ignoreInit = TRUE)

# ERASER: a toggle, not a material.
#
# It deliberately does not touch r$paintEraser's remembered colour, so switching
# the eraser off returns to whatever was being painted before. The browser owns
# the actual rubbing out; all that happens here is the mode flag and the button's
# held-down state.
shiny::observeEvent(input$paintEraser, {
  erasing <- !isTRUE(shiny::isolate(r$erasing))
  r$erasing <- erasing
  if(erasing){
    shinyjs::addClass("paintEraser", "paintToolActive")
  }else{
    shinyjs::removeClass("paintEraser", "paintToolActive")
  }
  session$sendCustomMessage("set-paint-erase", list(erasing = erasing))
}, ignoreInit = TRUE)

# RESET: drop every stroke on this version, revealing the land cover underneath.
#
# Both ends are cleared from here. R drops its two rasters - the version stores
# only edits, so clearing them *is* "back to the baseline" - and the browser is
# told to clear its grids and redecode the baseline PNG it already holds. Doing
# it in one observer is what keeps the two from disagreeing.
shiny::observeEvent(input$paintReset, {
  pos <- shiny::isolate(r$position)
  if(is.null(pos) || pos < 1 || pos > length(shiny::isolate(r$networkList))) return(NULL)
  r$networkList[[pos]]$paintedRaster <- NULL
  r$networkList[[pos]]$canopyRaster  <- NULL
  #the cached heat describes a design that no longer exists; drop it so the next
  #switch-on recomputes rather than showing the erased scenario back to the user
  r$heatRaster <- NULL
  if(isTRUE(shiny::isolate(r$heatOn))) clearHeat()
  session$sendCustomMessage("paint-reset", list(version = pos))
  message("paint: reset version ", pos, " to the land cover baseline")
}, ignoreInit = TRUE)

# Switch between painting the ground layer and the canopy layer.
# Both layers are permanently on the map in the browser, so switching level only
# changes their CSS: the canopy layer is hidden rather than removed, and the ground
# layer is dimmed rather than redrawn. No raster work happens here at all.
shiny::observeEvent(input$paintLevel, {
  canopyActive <- isTRUE(input$paintLevel)
  setPaintLevelButtons(canopyActive)
  applyPaintLevelColor(session, r, if(canopyActive) "canopy" else "ground")
  session$sendCustomMessage("set-paint-level", list(canopy = canopyActive))
}, ignoreInit = TRUE)

# Persist the cells the browser has painted since its last flush.
#
# The browser has already drawn them - this observer is purely about making the
# paint survive a version switch or a reload, which is why it does no projection,
# no image encoding and sends nothing back but an acknowledgement. If it throws,
# the ack is skipped and the browser puts the same cells back in its queue.
observeEvent(input$paintCells, {
  tryCatch({
    delta <- input$paintCells

    #the flush carries the version it was painted on. A flush racing a version
    #switch must land on that version, not on whichever one is selected by the
    #time the observer runs
    pos <- suppressWarnings(as.integer(delta$version))
    if(length(pos) != 1 || is.na(pos) || pos < 1 ||
       pos > length(shiny::isolate(r$networkList))) return(NULL)

    for(fld in c("paintedRaster", "canopyRaster")){
      runs <- if(fld == "canopyRaster") delta$canopy else delta$ground
      if(is.null(runs) || length(runs) == 0) next
      r$networkList[[pos]][[fld]] <- applyPaintRuns(
        shiny::isolate(r$networkList[[pos]][[fld]]), runs
      )
    }

    session$sendCustomMessage("paint-cells-ack", list(seq = delta$seq))
  }, error = function(e){
    warning("paintCells observer failed: ", conditionMessage(e))
    message("paintCells observer failed: ", conditionMessage(e))
  })
})




#dismiss Modal
obs_dimissModal <- shiny::observeEvent(input$dismissModal, {
  shiny::removeModal()
})

#observe info Button ####
obs_info6 <- shiny::observeEvent(input$infoButton6, {
  tbl <- read.csv2( vftData("tables/attractivity_description.csv"),
                    check.names = FALSE)

  shiny::showModal(
    shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                       h2(i18n()$t("Attraktivitätsmodell für Naherholung:") ),
                       h4(i18n()$t("Die Attraktivitätsmodelle werden dynamisch angepasst, um Änderungen in den Szenarien Rechnung zu tragen.") ),
                       h4(i18n()$t("Im Folgenden beschreiben wir, wie Landschaftselemente die Attraktivität Naherholungsgebiete beeinflussen, indem wir ihre relative Rangfolge darstellen:") ),
                       HTML(knitr::kable(tbl, format = "html") |>
                        kableExtra::kable_styling(
                           bootstrap_options = c("striped", "condensed"),
                           full_width = TRUE)|>
                          kableExtra::row_spec(0, align = "c")),
                       size = "l")
    )
})

#observe help ####
obs_help6 <- shiny::observeEvent(input$helpButton6, {
  shiny::showModal(
    shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                       h2(i18n()$t("Ändern Sie die Infrastruktur!")),
                       shiny::img(src = "www/arrowLeft.png", style = "float:left;height:50px;margin-left:-70px"),h3(shiny::HTML(as.character( i18n()$t("Links können Sie die <b>Sensitivitätsmatrix anzeigen</b>.")) ) ),

                       div(style = "text-align:right",
                       h3(shiny::HTML(as.character( i18n()$t("Rechts können Sie <b>neue Szenarien</b> für die Infrastruktur erstellen."))), shiny::img(src = "www/arrowRight.png", style = "float:right;height:50px;margin-right:-70px")),
                       h4(shiny::HTML(as.character( i18n()$t("In diesen neuen Szenarien können Sie <b>Wege ändern, Parkplätze hinzufügen/entfernen und neue Wohngebäude erstellen</b>")))),
                       h4(shiny::HTML(as.character( i18n()$t("<span style='color: #ff0000'> Wichtig:</span> Sie können die ursprüngliche Karte nicht verändern! Sie müssen zunächst ein neues Szenario erstellen.")))),
                       ),
                       h3(),
                       h3(shiny::HTML(as.character( i18n()$t("<u>In den neuen Szenarien:</u>")))),

                       h4(shiny::HTML(as.character( i18n()$t("Um einen <b>Weg zu ändern</b>, <b>klicken</b> Sie auf den Weg.")))),
                       h5(shiny::HTML(as.character( i18n()$t("Es öffnet sich dann ein Fenster, in dem Sie den Weg entweder <b>löschen</b> oder seine <b>Qualitäten ändern</b> können.")))),
                       h4(),
                       h4(shiny::HTML(as.character( i18n()$t("Sie können auch auf einen <b>Knoten (Kreis) klicken</b>.")))),
                       h5(shiny::HTML(paste0(i18n()$t("Dadurch wird es <b>ausgewählt</b>:"), "<img src ='www/selectedNode.png' style ='height:20px'>"))),
h4(shiny::HTML(as.character( i18n()$t("Sie können dann <b>3 Dinge tun</b>:")))),
h4(shiny::HTML(as.character(i18n()$t("<br><br><b>1)</b> Klicken Sie <b>erneut auf den Knoten</b>, um ihn zu <b>löschen</b> und alle mit ihm verbundenen Wege zu entfernen.
<br><br><b>2)</b> Klicken Sie auf einen <b>zweiten Knoten</b>, um die beiden Knoten mit <b>einem neuen Weg</b> zu verbinden.
<br><br><b>3)</b> Klicken Sie auf einen <b>leeren Kartenbereich</b>, um einen <b>neuen verbundenen Knoten</b> mit einem neuen Weg zu erstellen.")))),
h4(shiny::HTML(as.character( i18n()$t("<br>Die Änderung von <b>Parkplätzen und Wohngebieten</b> erfolgt <b>ähnlich</b> wie bei den <b>Schritten 1 und 4</b>."))))

    )
  )
})

#Language Change ####
langChangeObs <- observeEvent(input$languageSelect_7, {
  vftDbg("CHANGE LANGUAGE")
  if(input$languageSelect_7 == "de"){
    # i18n$set_translation_language('de')
    shiny.i18n::update_lang("de")
    i18n()$set_translation_language("de")



    vftSetBanner(id, "www/stepNewVersions_wsl_de.png")


    r$currentLang <- "de"

    vftDbg("DE")
  }else if(input$languageSelect_7 == "fr"){
    # i18n$set_translation_language('fr')
    shiny.i18n::update_lang("fr")


    vftSetBanner(id, "www/stepNewVersions_wsl_fr.png")


    r$currentLang <- "fr"
    i18n()$set_translation_language("fr")
  }else if(input$languageSelect_7 == "en"){
    # i18n$set_translation_language('fr')
    shiny.i18n::update_lang("en")
    i18n()$set_translation_language("en")


    vftSetBanner(id, "www/stepNewVersions_wsl_en.png")


    r$currentLang <- "en"
  }
})
##Observe end of render ####
      #observe event when map finishes rendering
      obsFinishRender <- shiny::observeEvent(input$versionMap_groups,{
        vftDbg("GROUPS CHANGED")
        vftDbg(input$versionMap_groups)
        shinyjs::enable("newVersionsConfirmButton")
        shinyjs::enable("addVersionButton")
        shinyjs::enable("contextChoice")
        for(btn in r$versionsUI){
          shinyjs::enable(btn$inputId_select)
          if(!is.null(btn$inputId_removal)){
            shinyjs::enable(btn$inputId_removal)
          }
        }
      }, ignoreInit = TRUE, ignoreNULL = FALSE)


      #observe if context changes####
      obsContext <- observeEvent(input$contextChoice, {

        #save map zoom
        r$mapView <- list(center_lng = input[["versionMap_center"]]$lng,
                                       center_lat = input[["versionMap_center"]]$lat,
                                       zoom = input[["versionMap_zoom"]])

        vftDbg("CONTEXT CHANGED")
        vftDbg(input$contextChoice)
        vftDbg("---")
        r$oldContext <- r$context

        r$context <- input$contextChoice

        # if original version
        if(r$position == 1){
          if( (r$oldContext == 1 & r$context == 2) |
              (r$oldContext == 2 & r$context == 1) ){
            #avoid rendering map again
            # shiny::isolate(r$toRender <- FALSE)

            #do not render
            return()
          }else{
            r$updateRender <- r$updateRender + 1
          }

        }else{
          r$updateRender <- r$updateRender + 1
        }

        #disable elements while map is loading
        #select button (outline in green?)

        shinyjs::disable("newVersionsConfirmButton")
        shinyjs::disable("addVersionButton")
        shinyjs::disable("contextChoice")
        #cycle through version names and disable them all

        for(btn in r$versionsUI){
          shinyjs::disable(btn$inputId_select)
          if(!is.null(btn$inputId_removal)){
            shinyjs::disable(btn$inputId_removal)
          }
        }

      })


      #INTERNAL FUNCTIONS ####


      #creates version buttons using insertUI.
      #requires name for version and IDs for removal and select button.
      #versionNb allows to keep track of which nb button is created, determines Id nb
      appendVersion <- function(name, inputId_removal, inputId_select, id_ui_name, isStart = TRUE){

        shiny::insertUI(
          selector = '#placeholder',
          ## wrap element in a div with id for ease of removal
          ui = shiny::tags$div(id = id_ui_name,
                        shiny::div(style = "height: 5px"),

                        if(name == "Original"){
                          if(isStart == TRUE){
                            shinyjs::disabled(
                              shiny::actionButton(inputId = shiny::NS(id, inputId_select), label = name, width = "100px", style = "height: 100px", class = "selected")
                            )
                          }else{
                            shiny::actionButton(inputId = shiny::NS(id, inputId_select), label = name, width = "100px", style = "height: 100px", class = "selected")
                          }

                        }else{
                          if(isStart == TRUE){
                            shinyjs::disabled(
                              shiny::actionButton(inputId = shiny::NS(id, inputId_select), label = name, width = "100px", style = "height: 100px", class = "notSelected")
                            )
                          }else{
                            shiny::actionButton(inputId = shiny::NS(id, inputId_select), label = name, width = "100px", style = "height: 100px", class = "notSelected")
                          }

                        },
                        if(name != "Original"){
                          if(isStart == TRUE){
                            shinyjs::disabled(
                              shiny::actionButton(inputId = shiny::NS(id, inputId_removal), label = "X", width = "30px", style = "height: 30px")
                            )
                          }else{
                            shiny::actionButton(inputId = shiny::NS(id, inputId_removal), label = "X", width = "30px", style = "height: 30px")
                          }

                          },
                          shiny::div(style = "height: 5px")
          )
        )




        #keep track of ids and inputIds
        # inserted_id_ui <- c(inserted_id_ui, id_ui_name)
        # print(inserted_id_ui)
        # inserted_inputId_select <- c(inserted_inputId_select, inputId_select)
        # print(inserted_inputId_select)
        # inserted_inputId_removal <- c(inserted_inputId_removal, inputId_removal )
        # print(inserted_inputId_removal)

        #UIVERSION OBSERVERS ####




        # OBSERVER FOR REMOVAL OF VERSION ####
        # no removal button for Original version
        if(!is.null(inputId_removal)){
          #Removal button
          r$appendedObservers[[length(r$appendedObservers) + 1]] <- list(
            shiny::observeEvent(input[[inputId_removal]], {

            shiny::removeUI(selector = paste0("div#", id_ui_name ) )

            #remove relevant duplicate network from list
            x <- which(names(r$versionsUI) == name)
            # networkLst <- ntwrkLst_r()

            # networkLst[[x]] <- NULL
            r$networkList[[x]] <- NULL

            #keep track of removed UI
            r$versionsUI[[name]] <- NULL

          }, ignoreInit = TRUE, once = TRUE)
          )
        }

        #OBSERVER FOR SELECT VERSION ####
        r$appendedObservers[[length(r$appendedObservers) + 1]] <- list(
          shiny::observeEvent(input[[inputId_select]], {
            #do something ONLY if version clicked is NOT last selection
            if(r$lastSelectedButton != inputId_select){

              #update global variable with reactive value
              # networkLst <- r$networkList

              #select button (outline in green?)
              shinyjs::removeClass(inputId_select, "notSelected")
              shinyjs::addClass(inputId_select, "selected")

              vftDbg("lastSelectedButton: ")
              vftDbg(r$lastSelectedButton)
              if(!is.null(r$lastSelectedButton)){
                shinyjs::addClass(r$lastSelectedButton, "notSelected")
              }

              #make this button the lastSelected button
              r$lastSelectedButton <- inputId_select

              #change selected network
              #use number of version_x


              # chars <- strsplit( inputId_select, NULL)
              # x <- as.integer( chars[[1]][length(chars[[1]])] )

              #find which versionsUI contains inputId_select
              x <- NULL
              for(vn in 1:length(r$versionsUI)){
                if(r$versionsUI[[vn]]$inputId_select == inputId_select){
                  x <- vn
                }
              }

              if(x <= length(r$networkList)){

                # selectedNetwork_r( list(r$networkList[[x]]$network) )
                # selectedNetwork_position <- x

                r$position <- x

                vftDbg("POSITION AND NETWORK")
                vftDbg(r$position)
                vftDbg(r$networkList[[r$position]])

              }else{
                vftDbg("ERROR: less networks than version buttons")


              }
              # output$versionMap <- renderLeaflet({

              #interactive map
              # tmap_mode('view')
              #trigger plot update
              # shinyjs::disable("placeholder")

              # r$mapRefresh <- r$mapRefresh + 1

              #save view of Map
              r$mapView <- list(center_lng = input[["versionMap_center"]]$lng,
                                             center_lat = input[["versionMap_center"]]$lat,
                                             zoom = input[["versionMap_zoom"]])

              #generate plot
              #(but not on first run, as this is done with initialisation of RenderLeaflet)

              r$updateNetworkPlot(r$updateNetworkPlot()+1)




              shinyjs::disable("newVersionsConfirmButton")
              shinyjs::disable("addVersionButton")
              shinyjs::disable("contextChoice")
              #cycle through version names and disable them all

              for(btn in r$versionsUI){
                shinyjs::disable(btn$inputId_select)
                if(!is.null(btn$inputId_removal)){
                  shinyjs::disable(btn$inputId_removal)
                }
              }

              # edgeTbl <- isolate(r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% as_tibble())
              # nodeTbl <- isolate(r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% as_tibble())
              #
              # print("NETWORK EDGES2:")
              # print(edgeTbl)
              #
              # edgeTbl <- edgeTbl[2,]
              #
              # leafletOptions(doubleClickZoom= FALSE)
              # map <- tmap_leaflet(
              #   tm_shape(edgeTbl  %>% st_as_sf()) +
              #     tm_lines(col = "black", lwd = 3, style = "fixed", popup.vars = FALSE, group = "edges") +
              #     tm_shape(nodeTbl %>% st_as_sf()) +
              #     tm_dots(size = 0.1, col = "white", popup.vars = FALSE, group = "nodes") +
              #     tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = c(0.5) ),
              #   options = leafletOptions(doubleClickZoom = FALSE),
              #   in.shiny = TRUE
              # )%>% addMapPane("layer1", zIndex = 410)%>% addMapPane("layer2", zIndex = 420)
              #
              # map
              # updateNetworkPlot(updateNetworkPlot()+1)
              # })
            }

          }, ignoreInit = TRUE)
        )




      }

    #remove appended Oberservers
    removeObservers <- function(appndObs){

      for(obs in appndObs){
        vftDbg(obs)
        obs[[1]]$destroy()
      }

    }





      #Check if bringing versions UI from prior page
      #which should always happen (Original)

      #UPDATE VERSIONS ####

      if(length(r$versionsUI) != 0){

        # print ("UI VERSIONS NOT EMPTY!")
        #
        # btn <- input$addVersionButton
        # id_ui_name <- paste0('version_', btn)
        # inputId_select <- paste0("versionBtn", versionBtn_nb)

        for(i in 1:length(r$versionsUI) ){
          vftDbg("appended removal details::")

          appendVersion(name = r$versionsUI[[i]]$name,
                         inputId_select = r$versionsUI[[i]]$inputId_select,
                         inputId_removal = r$versionsUI[[i]]$inputId_removal,
                         id_ui_name = r$versionsUI[[i]]$id_ui_name)

          if(r$versionsUI[[i]]$name == "Original"){
            #make original button lastSelectedButton (unselected when another button is clicked)
            r$lastSelectedButton <- r$versionsUI[[i]]$inputId_select

          }
        }

        for(btn in r$versionsUI){
          shinyjs::disable(btn$inputId_select)
          if(!is.null(btn$inputId_removal)){
            shinyjs::disable(btn$inputId_removal)
          }
        }

        # #use internal function to append UI
        # appendVersion(name = "Original", inputId_removal = NULL, inputId_select = inputId_select, id_ui_name = id_ui_name)




        #test without rebuilding buttons

        # #if versions UI present, use it to generate buttons
        #
        # #keep track of number of buttons
        # versionBtn_nb <- length(versionsUI)
        #
        # for(version in versionsUI){
        #   #generate buttons
        #   name = versionsUI$name
        #   inputId_removal = versionsUI$inputID_removal
        #   inputId_select = versionsUI$inputID_select
        #   id_ui_name = versionsUI$id_ui_name
        #
        #   if(version$name == "Original"){
        #
        #     #start with Original selected
        #     lastSelectedButton <- inputId_select
        #     selectedNetwork_r( list(networkList[[1]]) )
        #
        #   }
        #
        #   #use internal function to generate all versions
        #   appendVersion(name, inputId_removal, inputId_select, id_ui_name, versionNb = NULL)
        #
        #
        # }
        # #trigger plot update
        # # shinyjs::disable("placeholder")
        # updateNetworkPlot(updateNetworkPlot()+1)
      }else{
            vftDbg("ERROR: Original does not exist")
      }

    #INITIALIZATION ####

    vftDbg("INITIALIZATION")

    shinyjs::disable("versionBtn0")


    if(!isFirstRun){

      vftDbg("UPDATE NETWORK 4")
      r$updateNetworkPlot(r$updateNetworkPlot()+1)

      shinyjs::disable("newVersionsConfirmButton")
      shinyjs::disable("addVersionButton")
      vftDbg("BTN$INPUTID: ")
      for(btn in r$versionsUI){

        vftDbg(btn$inputId_select)
        shinyjs::disable(btn$inputId_select)
        if(!is.null(btn$inputId_removal)){
          shinyjs::disable(btn$inputId_removal)
        }
      }



    }


vftDbg("output")




vftDbg("add versions")

      #ADDING VERSIONS ####

      #CREATE GENERAL OBSERVERS

        #Prompt for Name ####
        obsEvent_addVersion <- shiny::observeEvent(input$addVersionButton, {

          #as for name of new version
          shiny::showModal(shiny::modalDialog(
            shiny::tags$h2(i18n()$t('Geben Sie der neuen Version einen Namen:') ),
            shiny::textInput(shiny::NS(id, 'name'), i18n()$t('Name der neuen Version')),
            footer=shiny::tagList(
              shiny::actionButton(inputId = shiny::NS(id, 'submitName'), label = i18n()$t('Einreichen'), style = "background-color:#006268; color:#ffffff"   ),
              shiny::modalButton(i18n()$t('Abbrechen')) )
            )
          )
        }, ignoreInit = TRUE)

        #only created on first run to avoid bug of multiple submissions

        #Use name to create version ####
        #add observer to variable, destroy it when leaving tab (recreated upon return)
        obsEvent_submit <- shiny::observeEvent(input$submitName, {


          shiny::removeModal()
          name <- input$name
          prefix <- name
          x <- 1
          nameIsUnique <- FALSE
          #get name
          while(nameIsUnique == FALSE){
            #if name same as another existing name, append _x to it
            if(name %in% names(r$versionsUI) ){
              #add +1 to _x if name_x exists
              x <- x + 1
              name <- paste0(prefix,"_", x)
            }else{
              nameIsUnique <- TRUE
            }
          }




          btn <- input$addVersionButton

          #add 1 to number of buttons (except if length is 0, when its empty)
          if(length(r$versionBtn_nb) == 0){
            #then give it the length of the button list
            r$versionBtn_nb <- length(r$versionsUI) + 1
          }else{
            r$versionBtn_nb <- r$versionBtn_nb + 1
          }

          #use number of buttons to determine button and ui names
          id_ui_name <- paste0('version_', r$versionBtn_nb)



          inputId_select <- paste0("versionBtn", r$versionBtn_nb)
          inputId_removal <- paste0("removeBtn", r$versionBtn_nb)
          #use internal function to append UI
          appendVersion(name = name, inputId_removal = inputId_removal, inputId_select = inputId_select, id_ui_name = id_ui_name, isStart = FALSE)

          #keep track of appended UI
          r$versionsUI[[name]] <- list(name = name,
                                         inputId_removal = inputId_removal,
                                         inputId_select = inputId_select,
                                         id_ui_name = id_ui_name
          )


          # CREATE COPY OF NETWORK ####
          # not for original, as it already exists
          if(name != "Original"){
            #(for now: copy original TODO: copy currently selected?)
            # networkLst <- ntwrkLst_r()
            # networkLst[[length(networkLst)+1]] <- list(network = networkLst[[1]]$network, pathUsage = NULL)

            #TODO: copy a group of elements (network, attractivity rasters, residential raster, parking polygons)
            r$networkList[[length(r$networkList)+1]] <- list(network = r$networkList[[1]]$network, pathUsage = NULL, parking = r$networkList[[1]]$parking, paintedRaster = NULL, canopyRaster = NULL)
            #update reactive
            # ntwrkLst_r(networkLst)


            vftDbg(paste0("LENGTH OF NETWORKLIST: ", length(r$networkList)) )
          }
        }, ignoreInit = TRUE)


        #CREATE NETWORK MODIFICATION OBSERVERS ####
        #Marker was clicked

        # MARKER WAS CLICKED ####
        obsMarkerClick <- shiny::observeEvent(input[["versionMap_marker_click"]], {

          #save map state
          r$mapView <- list(center_lng = input[["versionMap_center"]]$lng,
                                         center_lat = input[["versionMap_center"]]$lat,
                                         zoom = input[["versionMap_zoom"]])

          vftDbg(input[["versionMap_marker_click"]])
          #only allow interaction if not Original
          if(r$position != 1){

            if(r$context == 1){
              #### CONTEXT 1: INFRASTRUCTURE ####

              vftDbg("MARKER WAS CLICKED")
              r$markerWasClicked <- TRUE
              #not linking
              if(r$isLinking == FALSE){

                vftDbg("LINKING NOT ACTIVE")

                #initialise linking (between nodes)
                r$isLinking <- TRUE

                vftDbg(paste0("isLinking: ", r$isLinking))

                #record links
                if(is.null(r$firstLinkNode)){
                  r$firstLinkNode <- input[["versionMap_marker_click"]]

                  #highlight source node
                  #remove clicked node
                  #and replace with highlighted node
                  leaflet::leafletProxy("versionMap" )%>%
                    leaflet::removeMarker(r$firstLinkNode$id) %>%
                    leaflet::addMarkers(
                      icon = list(iconUrl = "www/selectedNode.png", iconSize = c(20, 20)),
                      lng = r$firstLinkNode$lng,
                      lat = r$firstLinkNode$lat,
                      group = "removable",
                      layerId = "XXX",
                      options = leaflet::markerOptions(pane = "layer3"))
                  # leaflet::addCircleMarkers(lng = firstLinkNode$lng,
                  #                           lat = firstLinkNode$lat,
                  #                           group = "nodes",
                  #                           fillColor = "white",
                  #                           color = "red",
                  #                           radius = 6,
                  #                           opacity = 1,
                  #                           layerId = firstLinkNode$id,
                  #                           options = markerOptions(pane = "layer3"))
                }else{

                  vftDbg("ERROR: firstLinkNode meant to be empty")
                  r$firstLinkNode <- NULL
                }


              }else{
                #linking is TRUE

                #check if X marker was clicked (in which case remove node)
                # DELETE NODE ####
                vftDbg("MAP CLICK:")
                vftDbg(input[["versionMap_marker_click"]]$id)
                if(input[["versionMap_marker_click"]]$id == "XXX"){

                  network <- r$networkList[[r$position]]$network
                  vftDbg("TO REMOVE NODE")

                  originalID <- r$firstLinkNode$id
                  #split string on "." (requires [.] rather than .)
                  vftDbg(paste0("originalID 3: ", originalID) )
                  # splt <- strsplit(originalID, "[.]")
                  # ptID_1 <- splt[[1]][[1]]
                  # ptID_1 <- substr(ptID_1, 2, nchar(ptID_1) )

                  ptID_1 <- originalID

                  #detect edges to remove from plot
                  remainingEdges <- network %>% tidygraph::activate(edges) %>% dplyr::as_tibble()
                  remainingEdges <- remainingEdges %>% dplyr::filter(.data$to_2 == ptID_1 | .data$from_2 == ptID_1)
                  vftDbg("ptID_1")
                  vftDbg(ptID_1)
                  vftDbg("REMAINING EDGES: ")
                  vftDbg(remainingEdges)
                  edgesToRemove <- remainingEdges$edgeID_2

                  vftDbg("remaining Edge IDs:")
                  vftDbg(remainingEdges$edgeID)
                  # edgesToRemove <- sapply(edgesToRemove, function(x) paste0("X", x))
                  vftDbg(edgesToRemove)

                  #remove node from network (removes edges linked to node automatically)
                  vftDbg("NETWORK BEFORE:")
                  vftDbg(network)

                  network <- network %>% tidygraph::activate(nodes) %>% dplyr::filter(.data$nodeID_2 !=  as.double(ptID_1) )

                  #correct continuity of nodeID
                  network <- network %>% tidygraph::activate(nodes) %>% dplyr::mutate(nodeID = 1:length(igraph::V(network)$nodeID))



                  r$networkList[[r$position]] <- list(network = network,  pathUsage = r$networkList[[r$position]]$pathUsage, parking = r$networkList[[r$position]]$parking, residential = r$networkList[[r$position]]$residential , newAttr = r$networkList[[r$position]]$newAttr, paintedRaster = r$networkList[[r$position]]$paintedRaster, canopyRaster = r$networkList[[r$position]]$canopyRaster )
                  # r$networkList[[r$position]]$network <- network

                  #remove pathUsage results, as new results must be simulated
                  r$networkList[[r$position]]$pathUsage <- NULL

                  vftDbg("NETWORK AFTER:")
                  vftDbg(network)



                  #remove visuals
                  leaflet::leafletProxy("versionMap")%>%
                    leaflet::removeMarker(layerId = "XXX")%>%
                    leaflet::removeMarker(layerId = as.character(ptID_1) )%>%
                    leaflet::removeShape(layerId = edgesToRemove)

                  #reset linking
                  r$isLinking <- FALSE
                  r$firstLinkNode <- NULL
                  r$secondLinkNode <- NULL

                  #reset everything (clicking on erase icon does not click elements behind:: maybe due to opacity)
                  #thus need to reset here
                  r$markerWasClicked <- FALSE
                  r$shapeWasClicked <- FALSE

                }else{
                  #another node was clicked, thus finalise link
                  #FINALISE LINKING ####
                  vftDbg("LINKING IS ACTIVE")
                  r$secondLinkNode <- input[["versionMap_marker_click"]]




                  #finalise linking
                  #replace clicked nodes

                  #get highest ID
                  networkEdges <- r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% dplyr::as_tibble()
                  maxID <- max(networkEdges$edgeID_2 )

                  newID = maxID +1

                  #TODO: sample DULN raster to determine DULN level for node
                  DULNlevel <- 1



                  #TO DO: use AOI shapes to determine if this point is within an AOI
                  AOInb <- "0"
                  shapeLeng <- as.numeric(raster::distance(
                    matrix(
                      data = c(r$firstLinkNode$lon, r$firstLinkNode$lat, r$secondLinkNode$lon, r$secondLinkNode$lat),
                      nrow = 2, ncol = 2,
                      dimnames = list(c("row1", "row2"), c("lon", "lat"))
                    ),
                    lonlat = TRUE
                  ))

                  vftDbg(paste0("distance: ", shapeLeng))

                  #create geometric edge
                  geo <- sf::st_sfc(
                    sf::st_linestring(matrix(
                      c(
                        r$firstLinkNode$lng,
                        r$firstLinkNode$lat,
                        r$secondLinkNode$lng,
                        r$secondLinkNode$lat
                      ), nrow = 2, ncol = 2, byrow = TRUE)
                    )
                    , crs = 4326)



                  #convert id to integer id (remove X and number after ".")
                  originalID <- r$firstLinkNode$id
                  #split string on "." (requires [.] rather than .)
                  vftDbg(paste0("originalID 2: ", originalID) )
                  # splt <- strsplit(originalID, "[.]")
                  # ptID_1 <- splt[[1]][[1]]
                  # ptID_1 <- substr(ptID_1, 2, nchar(ptID_1) )

                  ptID_1 <- originalID

                  originalID <- r$secondLinkNode$id
                  #split string on "." (requires [.] rather than .)
                  # splt <- strsplit(originalID, "[.]")
                  # ptID_2 <- splt[[1]][[1]]
                  # ptID_2 <- substr(ptID_2, 2, nchar(ptID_2) )
                  ptID_2 <- originalID

                  newLine <- data.frame(from = igraph::V(r$networkList[[r$position]]$network)$nodeID[igraph::V(r$networkList[[r$position]]$network)$nodeID_2 == as.double(ptID_1)],
                                        to = igraph::V(r$networkList[[r$position]]$network)$nodeID[igraph::V(r$networkList[[r$position]]$network)$nodeID_2 == as.double(ptID_2)],
                                        edgeID = newID, DULN_final = DULNlevel, AOI = AOInb,
                                        SHAPE_Leng = shapeLeng,
                                        from_2 =  igraph::V(r$networkList[[r$position]]$network)$nodeID[igraph::V(r$networkList[[r$position]]$network)$nodeID_2 == as.double(ptID_1)],
                                        to_2 = igraph::V(r$networkList[[r$position]]$network)$nodeID[igraph::V(r$networkList[[r$position]]$network)$nodeID_2 == as.double(ptID_2)],
                                        edgeID_2 = newID
                  )


                  geoLine <-dplyr::tibble(from = as.double(ptID_1),
                                          to = as.double(ptID_2),
                                          SHAPE = geo)
                  #add new edge to edge table
                  # networkEdges <- networkEdges %>% bind_rows(newLine)

                  networkNodes <- r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>%  dplyr::as_tibble()

                  newNetwork <- r$networkList[[r$position]]$network #%>% tidygraph::tidygraph::activate(edges)

                  #save geometry column
                  geometryColumn <- newNetwork %>% tidygraph::activate(edges) %>% dplyr::as_tibble()%>% dplyr::select( .data$SHAPE)

                  #remove geometry column (causes bugs when binding edges)
                  newNetwork <- newNetwork %>% tidygraph::activate(edges) %>% tidygraph::select(-.data$SHAPE)

                  #bind edge without geometry column
                  newNetwork <- newNetwork %>%   tidygraph::bind_edges(newLine)

                  #determine position where newLine was inserted
                  pos <- which(igraph::E(newNetwork)$edgeID_2 == newLine$edgeID_2)

                  #bind Linestring to geometry at correct position
                  if(pos < nrow(geometryColumn) ){
                    #if position is within geometryColumn
                    geometryColumn <- rbind(geometryColumn[1:(pos-1),], geoLine$SHAPE , geometryColumn[-(1:(pos-1)),])
                  }else{
                    #if not, append geometry to end
                    geometryColumn <- rbind(geometryColumn, list(geoLine$SHAPE) )
                  }

                  #re-integrate geometry column to network
                  newNetworkEdges <- newNetwork %>% tidygraph::activate(edges) %>%  dplyr::as_tibble()
                  newNetworkEdges$SHAPE <- geometryColumn$SHAPE

                  newNetwork <- tidygraph::tbl_graph(nodes = networkNodes, edges = newNetworkEdges, directed = FALSE)

                  #re-insert network in networkList
                  r$networkList[[r$position]] <- list(network = newNetwork, pathUsage = r$networkList[[r$position]]$pathUsage, parking = r$networkList[[r$position]]$parking, residential = r$networkList[[r$position]]$residential , newAttr = r$networkList[[r$position]]$newAttr, paintedRaster = r$networkList[[r$position]]$paintedRaster, canopyRaster = r$networkList[[r$position]]$canopyRaster )
                  # r$networkList[[r$position]]$network <- newNetwork

                  # tbl <- tbl_graph(edges = networkEdges , nodes = r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% as_tibble())
                  # r$networkList[[r$position]] <- list(network = tbl, pathUsage = NULL)


                  #insert node in dataframe
                  # networkNodes <- networkNodes %>% add_row(nodeID = newID, DULN = DULNlevel, AOI = AOInb,
                  # geometry = st_sfc(
                  #   st_point(c(input[["versionMap_click"]]$lng,
                  #              input[["versionMap_click"]]$lat))
                  #   , crs = 4326)
                  # )

                  # r$networkList[[r$position]] <- list(network = networkNodes, pathUsage = NULL)

                  #draw new edge
                  vftDbg(paste0("newID: ", newID))
                  leaflet::leafletProxy("versionMap")%>%
                    leaflet::addPolylines(lng = c(r$firstLinkNode$lng,
                                                  r$secondLinkNode$lng),
                                          lat = c(r$firstLinkNode$lat,
                                                  r$secondLinkNode$lat),color = "red", weight = 3,
                                          layerId = as.character(newID),
                                          options = leaflet::pathOptions(pane = "layer1"))

                  #make node1 normal again
                  leaflet::leafletProxy("versionMap" )%>%
                    leaflet::removeMarker(layerId = r$firstLinkNode$id)%>%
                    leaflet::removeMarker(layerId = "XXX")%>%
                    leaflet::addCircleMarkers(lng = r$firstLinkNode$lng,
                                              lat = r$firstLinkNode$lat,
                                              layerId = r$firstLinkNode$id,
                                              group = "nodes",
                                              fill = TRUE,
                                              fillColor = "white",
                                              color = "grey",
                                              radius = 5,
                                              opacity = 1,
                                              fillOpacity = 1,
                                              weight = 1,
                                              options = leaflet::markerOptions(pane = "layer3")
                    )

                  #make node2 normal again
                  leaflet::leafletProxy("versionMap" )%>%
                    leaflet::removeMarker(layerId = r$secondLinkNode$id)%>%
                    leaflet::addCircleMarkers(lng = r$secondLinkNode$lng,
                                              lat = r$secondLinkNode$lat,
                                              layerId = r$secondLinkNode$id,
                                              group = "nodes",
                                              fill = TRUE,
                                              fillColor = "white",
                                              color = "grey",
                                              radius = 5,
                                              opacity = 1,
                                              fillOpacity = 1,
                                              weight = 1,
                                              options = leaflet::markerOptions(pane = "layer3")
                    )
                  #DETERMINE EDGE QUALITY ####
                  #id from click has "X" in front and sometimes ".", remove these

                  #get created edge id (this is NOT clicked edge id)
                  originalID <- newID
                  r$edgID <- originalID
                  r$originalID <- originalID


                  #launch Modal (with a slight lag, this allows the user to see the path created first)
                  shinyjs::delay(0.3, {
                    shiny::showModal(
                      shiny::modalDialog(

                        fluidRow(
                          shiny::column(12, align = "center",
                                        shiny::tags$h3('Wählen Sie die Merkmale des neuen Weges:')
                          )
                        ),
                        shiny::radioButtons(shiny::NS(id, 'pathSignage'), 'Beschilderung',
                                            choices = c("Wanderwege" = "c1", "Wander- und Velowege" = "c2", "Veloweg" = "c3", "Nichts" = "c4"),
                                            selected = NULL, inline = TRUE),
                        shiny::radioButtons(shiny::NS(id, 'pathType'), 'Wegtyp',
                                            choices = c("Natur" = "c1", "Hardt" = "c2"),
                                            selected = NULL, inline = TRUE),
                        shiny::radioButtons(shiny::NS(id, 'pathWidth'), 'die Wegbreite',
                                            choices = c("gross" = "c1", "mittel (3m)" = "c2", "schmal (2m)" = "c3", "sehr schmal (1m)" = "c4"),
                                            selected = NULL, inline = TRUE),
                        # shiny::checkboxInput(shiny::NS(id,"areStairs"), label = "Are they stairs?", value = FALSE),

                        footer=shiny::tagList(
                          shiny::actionButton(inputId = shiny::NS(id, 'submitNewPath'), 'Einreichen'),
                          shiny::actionButton(inputId = shiny::NS(id,'cnclEdg'), "Abbrechen") )
                      ) )
                  })

                  # EXTRACT ATTR FOR NEW EDGE ####
                  #this is done after Modal in case its a bit slow
                  edgID <- newID
                  edgeSF <- r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% dplyr::filter(.data$edgeID_2 == edgID ) %>%
                    dplyr::as_tibble() %>% sf::st_as_sf()

                  edgeExtract <- terra::extract(r$DULN, edgeSF, fun = "mean")

                  #replace values for edge (using walkNat as "all")
                  # igraph::E(r$networkList[[r$position]]$network)$DULN_WALK_[[igraph::E(r$networkList[[r$position]]$network)$edgeID_2 == edgID]] <- edgeExtract[["walkNat"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_WALK_ <- edgeExtract[["walkNat"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_WALK1 <- edgeExtract[["walkSoc"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_BIKER <- edgeExtract[["bikerSport"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_EBIKE <- edgeExtract[["ebikeNat"]]
                  # igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_ALL <- edgeExtract[["walkNat"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_JOGGE <- edgeExtract[["jog"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_DOG_N <- edgeExtract[["dogNat"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_DOG_P <- edgeExtract[["dogProx"]]

                  # #precise AOI of new path
                  AOInb <- sf::st_intersects(igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$SHAPE, finalPolygons)
                  #Error when path inbetween multiple AoIs (AoInb becomes a list of two or more elements)
                  #solution, choose first element within list
                  if(any("list" %in% class(AOInb))){AOInb <- AOInb[[1]][1]}
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$AOI <- finalPolygons$AOI[as.numeric(AOInb)]
                  #

                }



                #reset linking
                r$isLinking <- FALSE
                r$firstLinkNode <- NULL
                r$secondLinkNode <- NULL

                #remove pathUsage results, as new results must be simulated
                r$networkList[[r$position]]$pathUsage <- NULL

              }


            }else if(input$contextChoice == 2){

              vftDbg("CONTEXT IS NOW SIGNAGE")
            #### CONTEXT 2: SIGNAGE/ATTRACTIVITY ####

            }else if(input$contextChoice == 3){

            #### CONTEXT 3: HOUSING/PARKING ####
              vftDbg("CONTEXT IS NOW HOUSING/PARKING")

              r$markerWasClicked <- TRUE
              if(!is.null(input[["versionMap_marker_click"]]$group) ){#& r$step1Refreshing != TRUE
                #FINALISE POLYGON ####
                #If first vertex of polygon is clicked, Finalise polygon
                if( input[["versionMap_marker_click"]]$group == "first"){
                  if(nrow(r$mapPoints) > 2){

                    #DETERMINE TYPE (PARKING OR RESIDENTIAL)
                    shiny::showModal(
                      shiny::modalDialog(
                        shiny::fluidRow(
                          shiny::column(12, align = "center",
                                        h4("Welche Art von Polygon möchten Sie erstellen?")),
                          shiny::column(6, align = "center",
                                        shiny::actionButton(shiny::NS(id, "chooseParking"), label = "Parkplatz",
                                                            style = "border-color: #000000;background-color: #1127b8; color: #ffffff; font-weight: bold;")),
                          shiny::column(6, align = "center",
                                        shiny::actionButton(shiny::NS(id, "chooseResidential"), label = "Wohnen",
                                                                      style = "border-color: #000000;background-color: #ba8e16; color: #ffffff; font-weight: bold;")))
                        )
                      )


                }
              }

              }
            }
          }else{
            vftDbg("ORIGINAL CANNOT BE ALTERED")
            }

        }, ignoreInit = TRUE)

        # SHAPE WAS CLICKED ####
        obsShapeClick <- shiny::observeEvent(input[["versionMap_shape_click"]], {
          #save map state
          r$mapView <- list(center_lng = input[["versionMap_center"]]$lng,
                                         center_lat = input[["versionMap_center"]]$lat,
                                         zoom = input[["versionMap_zoom"]])
vftDbg("EDGE CLICK")
          vftDbg(input[["versionMap_shape_click"]])
          # EDGE CLICK IN INFRASTRUCTURE CONTEXT  ####
          if(input$contextChoice == 1){

            #REMOVE EDGE ####

            #if this is not Original
            if(r$position != 1){

              r$shapeWasClicked <- TRUE

                if(r$markerWasClicked == FALSE){

                  #if linking was in process, cancel it
                  if(r$isLinking == TRUE){

                    leaflet::leafletProxy("versionMap" )%>%
                      leaflet::removeMarker(layerId = "XXX") %>%
                      leaflet::removeMarker(layerId = r$firstLinkNode$id)%>%
                      leaflet::addCircleMarkers(lng = r$firstLinkNode$lng,
                                                lat = r$firstLinkNode$lat,
                                                layerId = r$firstLinkNode$id,
                                                group = "nodes",
                                                fill = TRUE,
                                                fillColor = "white",
                                                color = "grey",
                                                radius = 5,
                                                opacity = 1,
                                                fillOpacity = 1,
                                                weight = 1,
                                                options = leaflet::markerOptions(pane = "layer3")
                      )

                    r$firstLinkNode <- NULL
                    isLinking <- FALSE

                  }else{


                    vftDbg("EDGE CLICKED IN INFRASTRUCTURE/SIGNAGE CONTEXT")

                    # ACTIVATE MODAL
                    #where you can choose either to delete path or path quality

                    r$shapeWasClicked <- TRUE

                    #id from click has "X" in front and sometimes ".", remove these
                    originalID <- input[["versionMap_shape_click"]]$id
                    #split string on "." (requires [.] rather than .), and remove numbers after
                    # splt <- strsplit(originalID, "[.]")

                    #by avoiding tmap, IDs are now non-text (numbers)

                    r$edgID <- originalID

                    r$originalID <- originalID
                    #remove X at start
                    # r$edgID <- substr(edgID, 2, nchar(edgID) )

                    network <- r$networkList[[r$position]]$network
                    #get current edge
                    currentEdge <- network %>% tidygraph::activate(edges) %>% dplyr::filter(.data$edgeID_2 ==  as.double(r$edgID) ) %>% igraph::E()
                    walkBikeStatus <- switch(
                      currentEdge$walkBike,
                      "1" = "c4",
                      "2" = "c1",
                      "3" = "c2",
                      "4" = "c3"
                    )
                    hardNaturStatus <- switch(
                      currentEdge$hardNatur,
                      "1" = "c2",
                      "2" = "c1"
                    )

                    #use if and not switch as we can't use doubles
                    if(currentEdge$roadWidth == 2){
                      roadWidthStatus <- "c4"
                    }else if(currentEdge$roadWidth == 3){
                      roadWidthStatus <- "c3"
                    }else if(currentEdge$roadWidth == 4){
                      roadWidthStatus <- "c2"
                    }else if(currentEdge$roadWidth == 5){
                      roadWidthStatus <- "c1"
                    }else{
                      roadWidthStatus <- "c1"
                    }


                    #determine current version
                    vftDbg(paste0("walkBikeStatus:", walkBikeStatus))
                    vftDbg(paste0("hardNaturStatus:", hardNaturStatus))
                    vftDbg(paste0("roadWidthStatus:", roadWidthStatus))

                    shiny::showModal(
                      shiny::modalDialog(
                        shiny::fluidRow(
                          shiny::column(12, align = "center",
                                        shiny::actionButton(inputId = NS(id, "deleteEdge"), label = i18n()$t("Weg löschen"),
                                                            style = "border-color: #000000;background-color: #ed3737; color: #ffffff; font-weight: bold;"),
                          )
                        ),
                        h3("ODER", align = "center"),
                        fluidRow(
                          shiny::column(12, align = "center",
                                        shiny::tags$h3(i18n()$t('Wählen Sie die Qualitäten des angeklickten Weges:'))
                          )
                        ),
                        shiny::radioButtons(shiny::NS(id, 'pathSignage'), 'Signage',
                                            choices = c("Wanderwege" = "c1", "Wander- und Velowege" = "c2", "Velowege" = "c3", "Nichts" = "c4"),
                                            selected = walkBikeStatus, inline = TRUE),
                        shiny::radioButtons(shiny::NS(id, 'pathType'), 'Path Type',
                                            choices = c("Natur" = "c1", "Hardt" = "c2"),
                                            selected = hardNaturStatus, inline = TRUE),
                        shiny::radioButtons(shiny::NS(id, 'pathWidth'), 'die Wegbreite',
                                            choices = c("gross" = "c1", "mittel (3m)" = "c2", "schmal (2m)" = "c3", "sehr schmal (1m)" = "c4"),
                                            selected = roadWidthStatus, inline = TRUE),

                        footer=shiny::tagList(
                          shiny::actionButton(inputId = shiny::NS(id, 'submitPath'), i18n()$t('Einreichen')),
                          shiny::modalButton('Abbrechen'))
                      )
                    )


                  }

                }
            }

          # EDGE CLICK IN ATTRACTIVITY/SIGNAGE CONTEXT  ####
          }else if(input$contextChoice == 2){

            # print("EDGE CLICKED IN ATTRACTIVITY CONTEXT")
            #
            # r$shapeWasClicked <- TRUE
            #
            # #id from click has "X" in front and sometimes ".", remove these
            # originalID <- input[["versionMap_shape_click"]]$id
            # #split string on "." (requires [.] rather than .), and remove numbers after
            # splt <- strsplit(originalID, "[.]")
            # edgID <- splt[[1]][[1]]
            # #remove X at start
            # edgID <- substr(edgID, 2, nchar(edgID) )
            #
            # network <- r$networkList[[r$position]]$network
            #
            # #get current edge
            # currentEdge <- network %>% tidygraph::activate(edges) %>% dplyr::filter(.data$edgeID_2 ==  as.double(edgID) ) %>% igraph::E()
            # walkBikeStatus <- switch(
            #   currentEdge$walkBike,
            #   "1" = "c4",
            #   "2" = "c1",
            #   "3" = "c2",
            #   "4" = "c3"
            #   )
            # hardNaturStatus <- switch(
            #   currentEdge$hardNatur,
            #   "1" = "c2",
            #   "2" = "c1"
            # )
            # roadWidthStatus <- switch(
            #   currentEdge$roadWidth,
            #   "1" = "c4",
            #   "2" = "c3",
            #   "3" = "c2",
            #   "4" = "c1", "c1"
            # )
            # #determine current version
            #
            # print(paste0("walkBikeStatus:", walkBikeStatus))
            # print(paste0("hardNaturStatus:", hardNaturStatus))
            # print(paste0("roadWidthStatus:", roadWidthStatus))
            #
            # shiny::showModal(
            #   shiny::modalDialog(
            #
            #     shiny::tags$h2('Choose qualities of clicked path:'),
            #     shiny::radioButtons(shiny::NS(id, 'pathSignage'), 'Signage',
            #                               choices = c("Walking Path" = "c1", "Walking and Biking trails" = "c2", "Biking path" = "c3", "None" = "c4"),
            #                               selected = walkBikeStatus, inline = TRUE),
            #     shiny::radioButtons(shiny::NS(id, 'pathType'), 'Path Type',
            #                               choices = c("Nature" = "c1", "Asphalt" = "c2"),
            #                               selected = hardNaturStatus, inline = TRUE),
            #     shiny::radioButtons(shiny::NS(id, 'pathWidth'), 'Path Width',
            #                               choices = c("Large" = "c1", "Medium (3m)" = "c2", "Narrow (2m)" = "c3", "Very Narrow (1m)" = "c4"),
            #                               selected = roadWidthStatus, inline = TRUE),
            #
            #     footer=shiny::tagList(
            #       shiny::actionButton(inputId = shiny::NS(id, 'submitPath'), 'Submit'),
            #       shiny::modalButton('cancel'))
            #   )
            # )

          }else if(input$contextChoice == 3){
            #CONTEXT 3: PARKING ####
            #if parking polygon is clicked, and no polygon was being generated
            if(input[["versionMap_shape_click"]]$group == "eraseable" & is.null(nrow(r$mapPoints) ) ){

              ###REMOVE PARKING SPACE####
              idToRemove <- input[["versionMap_shape_click"]]$id
              prefix <- substr(idToRemove, 1, 1)

              #update proxy using full id
              proxy <- leaflet::leafletProxy("versionMap")%>%
                leaflet::removeShape(layerId = idToRemove)

              #change id to numeric to alter tables
              idToRemove <- substr(idToRemove, 2, nchar(idToRemove))

              #determine if its parking or residential
              if(prefix == "p"){

                #remove from network (keep all but id)
                polyToRemove <- r$networkList[[r$position]]$parking %>% dplyr::filter(r$networkList[[r$position]]$parking$id == idToRemove)
                r$parkingPolygons <- r$networkList[[r$position]]$parking %>%
                  dplyr::filter(r$networkList[[r$position]]$parking$id != idToRemove)

                r$networkList[[r$position]]$parking <- r$parkingPolygons

                #determine number to remove (/15 for residential, /30 for parking)
                amountToReduce <- as.numeric(sf::st_area(polyToRemove))/30

              }else if(prefix == "r"){

                #remove from network (keep all but id)
                polyToRemove <- r$networkList[[r$position]]$residential %>% dplyr::filter(r$networkList[[r$position]]$residential$id == idToRemove)
                r$residentialPolygons <- r$networkList[[r$position]]$residential %>%
                  dplyr::filter(r$networkList[[r$position]]$residential$id != idToRemove)

                r$networkList[[r$position]]$residential <- r$residentialPolygons
                #determine number to remove (/15 for residential, /30 for parking)
                amountToReduce <- as.numeric(sf::st_area(polyToRemove))/30

                }
              #update node parking characteristic
              allNodes <- r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% dplyr::as_tibble() %>% sf::st_as_sf()
              nodesToUpdate <- sf::st_within(allNodes, polyToRemove)

              #indirect way of determining TRUE, FALSE
              nodesToUpdate <- lengths(nodesToUpdate) > 0
              #if no nodes, reset closest node
              if(sum(nodesToUpdate) == 0){
                nodesToUpdate <- sf::st_nearest_feature( polyToRemove, allNodes)
              }else{
                nodesToUpdate <- which(nodesToUpdate)
              }


              igraph::V(r$networkList[[r$position]]$network)$parking[nodesToUpdate] <- igraph::V(r$networkList[[r$position]]$network)$parking[nodesToUpdate] - amountToReduce

              #remove pathUsage results, as new results must be simulated
              r$networkList[[r$position]]$pathUsage <- NULL

              r$shapeWasClicked <- TRUE
            }


          }

        }, ignoreInit = TRUE)

#OBSERVE DELETE / SUBMIT EDGE####
## OBSERVE DELETE EDGE ####
obsEvent_deleteEdge <- shiny::observeEvent(input$deleteEdge, {

  vftDbg("DELETE PATH")
  ## REMOVE EDGE FROM SELECTED NETWORK

  network <- r$networkList[[r$position]]$network
  network <- network %>% tidygraph::activate(edges) %>% dplyr::filter(.data$edgeID_2 !=  r$edgID )

  r$networkList[[r$position]] <- list(network = network,  pathUsage = r$networkList[[r$position]]$pathUsage, parking = r$networkList[[r$position]]$parking, residential = r$networkList[[r$position]]$residential , newAttr = r$networkList[[r$position]]$newAttr, paintedRaster = r$networkList[[r$position]]$paintedRaster, canopyRaster = r$networkList[[r$position]]$canopyRaster)

  #remove pathUsage results, as new results must be simulated
  r$networkList[[r$position]]$pathUsage <- NULL

  # r$networkList[[r$position]]$network <- network
  #redraw edges
  leaflet::leafletProxy("versionMap" )%>%
    leaflet::removeShape(layerId =as.character( r$originalID) )

  shiny::removeModal()

}, ignoreInit = TRUE)

## OBSERVE SUBMIT PATH ####
obsEvent_submitPath <- shiny::observeEvent(input$submitPath, {


  #alter selected path
  network <- r$networkList[[r$position]]$network

  newWalkBike <- switch(input$pathSignage,
                        c1 = 2,
                        c2 = 3,
                        c3 = 4,
                        c4 = 1, 1)
  newColor <- c("black", "#e8e22e", "#3ddb68", "#35caf0")[newWalkBike]

  newHardNatur <- switch(input$pathType,
                        c1 = 2,
                        c2 = 1, 2)
  newLine <- c("1", "4 6")[newHardNatur]

  newPathWidth <- switch(input$pathWidth,
                        c1 = 5,
                        c2 = 4,
                        c3 = 3,
                        c4 = 2, 5)

  #get old values
  oldWalkBike <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  as.double(r$edgID)]$walkBike
  oldPathWidth <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  as.double(r$edgID)]$roadWidth
  oldHardNatur <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  as.double(r$edgID)]$hardNatur

  #update walkBike
  igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  as.double(r$edgID)]$walkBike <- newWalkBike
  igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  as.double(r$edgID)]$roadWidth <- newPathWidth
  igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  as.double(r$edgID)]$hardNatur <- newHardNatur

  #adapt pathWidth for next steps (go from 2-5 to 1-4)
  oldPathWidth_corr <- oldPathWidth -1
  newPathWidth_corr <- newPathWidth -1

  proxy = leaflet::leafletProxy("versionMap") %>%
  #remove edge
  leaflet::removeShape(layerId = r$originalID)


  proxy = leaflet::leafletProxy("versionMap") %>%
  leaflet::addPolylines(data = sf::st_zm( sf::st_as_sf(dplyr::as_tibble(network %>% tidygraph::activate(edges) %>% dplyr::filter(.data$edgeID_2 ==  as.double(r$edgID)) ) ) ),
                      stroke = TRUE,
                      weight = newPathWidth,
                      color = newColor,
                      fill = FALSE,
                      opacity = 1,
                      options = leaflet::pathOptions(pane = "layer2"),
                      layerId = as.character(r$originalID),
                      dashArray = newLine,
                      highlightOptions = leaflet::highlightOptions(weight = 9))

  #replace with new edge
  # leaflet::addGeoJSON(geojson = geojsonsf::sf_geojson(sf::st_as_sf(tibble::as_tibble(network %>% tidygraph::activate(edges) %>% dplyr::filter(.data$edgeID_2 ==  as.double(r$edgID)) ) ) ),
  #                     stroke = TRUE,
  #                     weight = newWidth,
  #                     color = newColor,
  #                     fill = FALSE,
  #                     opacity = 1,
  #                     options = leaflet::pathOptions(pane = "layer1"),
  #                     layerId = r$originalID,
  #                     dashArray = newLine)
  shiny::removeModal()
  #UPDATE NETWORK ATTRACTIVITY
  #get values by which attractivities are summed (or substracted)
  oldAttrs <- colSums(rbind(
    translateAttrTable_walkBike[oldWalkBike,],
    translateAttrTable_hardNatur[oldHardNatur,],
    translateAttrTable_roadWidth[oldPathWidth_corr,]
    )
  )
  newAttrs <- colSums(rbind(
    translateAttrTable_walkBike[newWalkBike,],
    translateAttrTable_hardNatur[newHardNatur,],
    translateAttrTable_roadWidth[newPathWidth_corr,]
  )
  )
  #sum removal and addition of element

  combinedAtrrs <- colSums(
    rbind(
      oldAttrs * -1,
      newAttrs
    )


  )

  #update edges
  dblEdgeId <- as.double(r$edgID)
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_WALK_ <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 == dblEdgeId]$DULN_WALK_ + combinedAtrrs["DULN_WALK_"]
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_WALK1 <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_WALK1 + combinedAtrrs["DULN_WALK1"]
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_BIKER <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_BIKER + combinedAtrrs["DULN_BIKER"]
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_EBIKE <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_EBIKE + combinedAtrrs["DULN_EBIKE"]
  # igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_ALL <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_ALL + combinedAtrrs["DULN_ALL"]
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_JOGGE <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_JOGGE + combinedAtrrs["DULN_JOGGE"]
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_DOG_N <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_DOG_N + combinedAtrrs["DULN_DOG_N"]
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_DOG_P <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_DOG_P + combinedAtrrs["DULN_DOG_P"]

  # ALTER NODES AT EDGE
  #use from and to as ids
  node1_id <- igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$to_2
  node2_id <- igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$from_2
  #update nodes
  #node1
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_WALK_ <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_WALK_ + combinedAtrrs["DULN_WALK_"]
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_WALK1 <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_WALK1 + combinedAtrrs["DULN_WALK1"]
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_BIKER <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_BIKER + combinedAtrrs["DULN_BIKER"]
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_EBIKE <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_EBIKE + combinedAtrrs["DULN_EBIKE"]
  # igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_ALL <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_ALL + combinedAtrrs["DULN_ALL"]
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_JOGGE <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_JOGGE + combinedAtrrs["DULN_JOGGE"]
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_DOG_N <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_DOG_N + combinedAtrrs["DULN_DOG_N"]
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_DOG_P <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_DOG_P+ combinedAtrrs["DULN_DOG_P"]
  #node2
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_WALK_ <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_WALK_ + combinedAtrrs["DULN_WALK_"]
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_WALK1 <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_WALK1 + combinedAtrrs["DULN_WALK1"]
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_BIKER <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_BIKER + combinedAtrrs["DULN_BIKER"]
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_EBIKE <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_EBIKE + combinedAtrrs["DULN_EBIKE"]
  # igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_ALL <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_ALL + combinedAtrrs["DULN_ALL"]
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_JOGGE <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_JOGGE + combinedAtrrs["DULN_JOGGE"]
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_DOG_N <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_DOG_N + combinedAtrrs["DULN_DOG_N"]
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_DOG_P <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_DOG_P+ combinedAtrrs["DULN_DOG_P"]

  #remove pathUsage results, as new results must be simulated
  r$networkList[[r$position]]$pathUsage <- NULL

  }, ignoreInit = TRUE)

## OBSERVE SUBMIT NEW PATH ####
obsEvent_submitNewPath <- shiny::observeEvent(input$submitNewPath, {
  #alter created path
  network <- r$networkList[[r$position]]$network

  newWalkBike <- switch(input$pathSignage,
                        c1 = 2,
                        c2 = 3,
                        c3 = 4,
                        c4 = 1, 1)
  newColor <- c("black", "#e8e22e", "#3ddb68", "#35caf0")[newWalkBike]

  newHardNatur <- switch(input$pathType,
                         c1 = 2,
                         c2 = 1, 2)
  newLine <- c("1", "4 6")[newHardNatur]

  newPathWidth <- switch(input$pathWidth,
                         c1 = 5,
                         c2 = 4,
                         c3 = 3,
                         c4 = 2, 5)
  areStairs <- input$areStairs

  #update walkBike
  igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  as.double(r$edgID)]$walkBike <- newWalkBike
  igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  as.double(r$edgID)]$roadWidth <- newPathWidth
  igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  as.double(r$edgID)]$hardNatur <- newHardNatur

  #adapt pathWidth for next steps (go from 2-5 to 1-4)
  newPathWidth_corr <- newPathWidth -1

  proxy = leaflet::leafletProxy("versionMap") %>%
    #remove edge
    leaflet::removeShape(layerId = r$originalID)


  proxy = leaflet::leafletProxy("versionMap") %>%
    leaflet::addPolylines(data = sf::st_as_sf(dplyr::as_tibble(network %>% tidygraph::activate(edges) %>% dplyr::filter(.data$edgeID_2 ==  as.double(r$edgID)) ) ),
                          stroke = TRUE,
                          weight = newPathWidth,
                          color = newColor,
                          fill = FALSE,
                          opacity = 1,
                          options = leaflet::pathOptions(pane = "layer2"),
                          layerId = as.character(r$originalID),
                          dashArray = newLine,
                          highlightOptions = leaflet::highlightOptions(weight = 9))

  #replace with new edge
  # leaflet::addGeoJSON(geojson = geojsonsf::sf_geojson(sf::st_as_sf(tibble::as_tibble(network %>% tidygraph::activate(edges) %>% dplyr::filter(.data$edgeID_2 ==  as.double(r$edgID)) ) ) ),
  #                     stroke = TRUE,
  #                     weight = newWidth,
  #                     color = newColor,
  #                     fill = FALSE,
  #                     opacity = 1,
  #                     options = leaflet::pathOptions(pane = "layer1"),
  #                     layerId = r$originalID,
  #                     dashArray = newLine)
  shiny::removeModal()


  #UPDATE NETWORK ATTRACTIVITY
  #get values by which attractivities are summed (or substracted)

  newAttrs <- colSums(rbind(
    translateAttrTable_walkBike[newWalkBike,],
    translateAttrTable_hardNatur[newHardNatur,],
    translateAttrTable_roadWidth[newPathWidth_corr,]
  )
  )
  #sum removal and addition of element

  combinedAtrrs <- newAttrs
  #update edges
  dblEdgeId <- as.double(r$edgID)
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_WALK_ <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 == dblEdgeId]$DULN_WALK_ + combinedAtrrs["DULN_WALK_"]
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_WALK1 <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_WALK1 + combinedAtrrs["DULN_WALK1"]
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_BIKER <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_BIKER + combinedAtrrs["DULN_BIKER"]
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_EBIKE <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_EBIKE + combinedAtrrs["DULN_EBIKE"]
  # igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_ALL <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_ALL + combinedAtrrs["DULN_ALL"]
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_JOGGE <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_JOGGE + combinedAtrrs["DULN_JOGGE"]
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_DOG_N <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_DOG_N + combinedAtrrs["DULN_DOG_N"]
  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$DULN_DOG_P <- igraph::E(r$networkList[[r$position]]$network)[.data$edgeID_2 ==  dblEdgeId]$DULN_DOG_P + combinedAtrrs["DULN_DOG_P"]

  #add AOI label
  AOInb <- sf::st_intersects(igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$SHAPE, finalPolygons)
  if(any("list" %in% class(AOInb))){AOInb <- AOInb[[1]][1]}

  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$AOI <- finalPolygons$AOI[as.numeric(AOInb)]

  # ALTER NODES AT EDGE
  #use from and to as ids
  node1_id <- igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$to_2
  node2_id <- igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  dblEdgeId]]$from_2

  #update nodes
  #node1
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_WALK_ <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_WALK_ + combinedAtrrs["DULN_WALK_"]
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_WALK1 <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_WALK1 + combinedAtrrs["DULN_WALK1"]
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_BIKER <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_BIKER + combinedAtrrs["DULN_BIKER"]
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_EBIKE <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_EBIKE + combinedAtrrs["DULN_EBIKE"]
  # igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_ALL <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_ALL + combinedAtrrs["DULN_ALL"]
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_JOGGE <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_JOGGE + combinedAtrrs["DULN_JOGGE"]
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_DOG_N <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_DOG_N + combinedAtrrs["DULN_DOG_N"]
  igraph::V(r$networkList[[r$position]]$network)[[node1_id]]$DULN_DOG_P <- igraph::V(r$networkList[[r$position]]$network)[node1_id]$DULN_DOG_P+ combinedAtrrs["DULN_DOG_P"]
  #node2
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_WALK_ <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_WALK_ + combinedAtrrs["DULN_WALK_"]
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_WALK1 <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_WALK1 + combinedAtrrs["DULN_WALK1"]
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_BIKER <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_BIKER + combinedAtrrs["DULN_BIKER"]
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_EBIKE <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_EBIKE + combinedAtrrs["DULN_EBIKE"]
  # igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_ALL <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_ALL + combinedAtrrs["DULN_ALL"]
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_JOGGE <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_JOGGE + combinedAtrrs["DULN_JOGGE"]
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_DOG_N <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_DOG_N + combinedAtrrs["DULN_DOG_N"]
  igraph::V(r$networkList[[r$position]]$network)[[node2_id]]$DULN_DOG_P <- igraph::V(r$networkList[[r$position]]$network)[node2_id]$DULN_DOG_P+ combinedAtrrs["DULN_DOG_P"]

  #remove pathUsage results, as new results must be simulated
  r$networkList[[r$position]]$pathUsage <- NULL
  #remove new edge from network

  #update proxy

}, ignoreInit = TRUE)

#OBSERVE CHOOSE PARKING ####
obsEvent_chooseParking <- shiny::observeEvent(input$chooseParking, {
  shiny::removeModal()
  #polygon creation is done by observe events using r$mapPoints

  #create polygon with points
  poly <- sf::st_cast(sf::st_combine(r$mapPoints), "POLYGON")
  poly <- sf::st_sf(poly)

  # poly$type <- "parking"

  if(length(r$parkingPolygons$id) > 0){
    poly$id <- as.character(max(as.numeric(r$parkingPolygons$id))+1)
  }else{
    poly$id <- "1"
  }
  vftDbg(paste0("NEWPOLY ID: ", poly$id, " ", class(poly$id)))
  # poly <- concaveman(mapPoints, 1) Doesn't work well
  poly <- dplyr::rename(poly, polygons = "poly")



  ##CHECK INTERSECTIONS ####
  #check if new polygon intersects or overlaps with any other
  intersectingPolysID <- NULL

  #first check if parking polygon intersects with residential (in which case abort)
  #if residential polygons is empty, no need to check
  if(!is.null(nrow(r$residentialPolygons)) ){
    #check intersections with residential polygons
    residentialIntersectCount <- length(which(sf::st_intersects(poly, r$residentialPolygons, sparse = FALSE)))

    if(residentialIntersectCount > 0){
      #abort
      #TODO: Modal window with warning (you cannot overlap parking and residential areas)
      r$mapPoints <- sf::st_sfc(crs = 4326)

      # proxy <- leaflet::leafletProxy("versionMap", data = r$parkingPolygons)%>%
      #   leaflet::clearGroup("eraseable")
      proxy <- leaflet::leafletProxy("versionMap")
      #clear points for creating polygon
      leaflet::clearGroup(proxy, "first")
      leaflet::clearGroup(proxy, "after")

      shiny::showModal(
        shiny::modalDialog(
          shiny::h3("Sie können Parkflächen und Wohngebiete nicht überschneiden!")
        )
      )
      return()
    }
  }

  #if there is no other parking polygon, no need to check intersection
  if(!is.null(r$parkingPolygons)){
  intersectingPolys <- which(sf::st_intersects(poly, r$parkingPolygons, sparse = FALSE))
  if(length(intersectingPolys) > 0){

    # Check if interesecting polygons are ALL of same type (parking or residential)

    #if a single different type intersects, abort and mention error
    # if(!all(poly$type %in% r$parkingPolygons$type[intersectingPolys]) ){
    #   r$mapPoints <- sf::st_sfc(crs = 4326)
    #
    #   # proxy <- leaflet::leafletProxy("versionMap", data = r$parkingPolygons)%>%
    #   #   leaflet::clearGroup("eraseable")
    #   proxy <- leaflet::leafletProxy("versionMap")
    #   #clear points for creating polygon
    #   leaflet::clearGroup(proxy, "first")
    #   leaflet::clearGroup(proxy, "after")
    #
    #   #abort
    #   return()
    # }else{
      #otherwise continue with union
      #if so, combine it into a single polygon
      newPoly <- sf::st_as_sf(sf::st_union(c(poly$polygons, r$parkingPolygons[intersectingPolys,]$polygons ) ) )

      #determine its general attractivity (with popup)

      #determine id
      newPoly$id <- as.character(max(as.numeric(r$parkingPolygons$id))+1)
      vftDbg(paste0("NEWPOLY ID: ", newPoly$id, " ", class(newPoly$id)))

      #get removed polygon's id
      intersectingPolysID <- r$parkingPolygons[intersectingPolys,]$id
      #remove intersecting polys
      r$parkingPolygons <- r$parkingPolygons[-intersectingPolys,]
      #add new polygon
      # r$parkingPolygons[nrow(r$parkingPolygons) + 1, ] <- newPoly
      poly <- newPoly
    # }
  }
  }
    #keep a table of polygons
    #mark as new$
    poly$isNew <- 1
    #rename if needed
    if("x" %in% names(poly)){
      poly <- poly %>% dplyr::rename(polygons = .data$x)
    }
    r$parkingPolygons <- rbind(r$parkingPolygons, poly)


    r$polyFinished <- TRUE

    r$mapPoints <- sf::st_sfc(crs = 4326)
    vftDbg(r$parkingPolygons)

    # proxy <- leaflet::leafletProxy("versionMap", data = r$parkingPolygons)%>%
    #   leaflet::clearGroup("eraseable")
    proxy <- leaflet::leafletProxy("versionMap", data = r$parkingPolygons[nrow(r$parkingPolygons),])%>%
      leaflet::addPolygons(
        layerId =paste0("p", r$parkingPolygons[nrow(r$parkingPolygons),]$id),
        stroke = TRUE,
        weight = 5,
        color = "#e83017",
        fill = TRUE,
        fillColor = "#1127b8",
        opacity = 1,
        fillOpacity = 0.3,
        group = "eraseable",
        options = leaflet::pathOptions(pane = "layer2"),
        highlightOptions = leaflet::highlightOptions(fillColor = "#f2778d")) %>%
      leaflet::removeShape(layerId = intersectingPolysID)

    leaflet::clearGroup(proxy, "first")
    leaflet::clearGroup(proxy, "after")


    #insert parkingPolygons into r$networkList
    r$networkList[[r$position]]$parking <- r$parkingPolygons
    #update nodes based on new polygon
    #get nodes
    nodes <- r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% dplyr::as_tibble() %>%
      sf::st_as_sf()

    finishedPoly <- r$parkingPolygons[nrow(r$parkingPolygons),]
    #get polygon size (should be in meters if proj = 4326)
    polyArea <- sf::st_area(finishedPoly)
    #convert to nb of agents (1agent / 30m^2)
    agentNb <- polyArea/30
    #get nodes within polygon and distribute number of agents equally among nodes (decimals allowed)
    # ADD the values to nodes (pre-populated with 0s). This is to avoid conflicts with nodes already filled due to exception below.
    nodeCount <- nrow(nodes[finishedPoly, op = sf::st_within])
    if(nodeCount > 0){
      #use sf polygon to select terra vect nodes (may have to convert to sf first)
      isWithin <- sf::st_within(nodes, finishedPoly)
      isWithin <- lengths(isWithin)
      nodes[isWithin > 0,]$parking <- nodes[isWithin > 0,]$parking + ( as.numeric(agentNb)/nodeCount )
    }else{
      #CAPTURE EXCEPTION : no nodes in polygon
      #in this case, find a single closest node outside polygon
      nearestIndex <- sf::st_nearest_feature(finishedPoly, sf::st_as_sf(nodes))
      nodes$parking[[nearestIndex]] <- agentNb
    }
    #update changes to networkList
    r$networkList[[r$position]]$network <- tidygraph::tbl_graph(nodes = dplyr::as_tibble(nodes),
                                                                edges = dplyr::as_tibble(r$networkList[[r$position]]$network %>% tidygraph::activate(edges)),
                                                                directed = FALSE)

    #remove pathUsage results, as new results must be simulated
    r$networkList[[r$position]]$pathUsage <- NULL


}, ignoreInit = TRUE)

#OBSERVE CHOOSE RESIDENTIAL ####
obsEvent_chooseResidential <- shiny::observeEvent(input$chooseResidential, {

  shiny::removeModal()
  #polygon creation is done by observe events using r$mapPoints

  #create polygon with points
  poly <- sf::st_cast(sf::st_combine(r$mapPoints), "POLYGON")
  poly <- sf::st_sf(poly)

  # poly$type <- "residential"

  if(length(r$residentialPolygons$id) > 0){
    poly$id <- as.character(max(as.numeric(r$residentialPolygons$id))+1)
  }else{
    poly$id <- "1"
  }
  vftDbg(paste0("NEWPOLY ID: ", poly$id, " ", class(poly$id)))
  # poly <- concaveman(mapPoints, 1) Doesn't work well
  poly <- dplyr::rename(poly, polygons = "poly")


  ##CHECK INTERSECTIONS ####
  #check if new polygon intersects or overlaps with any other
  intersectingPolysID <- NULL

  #first check if parking polygon intersects with residential (in which case abort)
  #if residential polygons is empty, no need to check
  if(!is.null(nrow(r$parkingPolygons)) ){
    #check intersections with residential polygons
    parkingIntersectCount <- length(which(sf::st_intersects(poly, r$parkingPolygons, sparse = FALSE)))

    if(parkingIntersectCount > 0){
      #abort
      #TODO: Modal window with warning (you cannot overlap parking and residential areas)
      r$mapPoints <- sf::st_sfc(crs = 4326)

      # proxy <- leaflet::leafletProxy("versionMap", data = r$parkingPolygons)%>%
      #   leaflet::clearGroup("eraseable")
      proxy <- leaflet::leafletProxy("versionMap")
      #clear points for creating polygon
      leaflet::clearGroup(proxy, "first")
      leaflet::clearGroup(proxy, "after")

      shiny::showModal(
        shiny::modalDialog(
          shiny::h3("You cannot overlap parking areas and residential areas!")
        )
      )

      return()
    }
  }

if(!is.null(r$residentialPolygons)){
  intersectingPolys <- which(sf::st_intersects(poly, r$residentialPolygons, sparse = FALSE))
  if(length(intersectingPolys) > 0){

    # Check if interesecting polygons are ALL of same type (parking or residential)

    #if a single different type intersects, abort and mention error
    # if(!all(poly$type %in% r$residentialPolygons$type[intersectingPolys]) ){
    #   r$mapPoints <- sf::st_sfc(crs = 4326)
    #
    #   # proxy <- leaflet::leafletProxy("versionMap", data = r$parkingPolygons)%>%
    #   #   leaflet::clearGroup("eraseable")
    #   proxy <- leaflet::leafletProxy("versionMap")
    #   #clear points for creating polygon
    #   leaflet::clearGroup(proxy, "first")
    #   leaflet::clearGroup(proxy, "after")
    #
    #   #abort
    #   return()
    # }else{
      #otherwise continue with union
      #if so, combine it into a single polygon
      newPoly <- sf::st_as_sf(sf::st_union(c(poly$polygons, r$residentialPolygons[intersectingPolys,]$polygons ) ) )

      #determine its general attractivity (with popup)

      #determine id
      newPoly$id <- as.character(max(as.numeric(r$residentialPolygons$id))+1)
      vftDbg(paste0("NEWPOLY ID: ", newPoly$id, " ", class(newPoly$id)))

      #get removed polygon's id
      intersectingPolysID <- r$residentialPolygons[intersectingPolys,]$id
      #remove intersecting polys
      r$residentialPolygons <- r$residentialPolygons[-intersectingPolys,]
      #add new polygon
      # r$parkingPolygons[nrow(r$parkingPolygons) + 1, ] <- newPoly
      poly <- newPoly
    # }
  }
}

  #keep a table of polygons
  #mark as new
  poly$isNew <- 1
  #rename if needed
  if("x" %in% names(poly)){
    poly <- poly %>% dplyr::rename(polygons = .data$x)
  }
  r$residentialPolygons <- rbind(r$residentialPolygons, poly)


  r$polyFinished <- TRUE

  r$mapPoints <- sf::st_sfc(crs = 4326)
  vftDbg(r$residentialPolygons)

  # proxy <- leaflet::leafletProxy("versionMap", data = r$parkingPolygons)%>%
  #   leaflet::clearGroup("eraseable")
  proxy <- leaflet::leafletProxy("versionMap", data = r$residentialPolygons[nrow(r$residentialPolygons),])%>%
    leaflet::addPolygons(
      layerId = paste0("r", as.character(r$residentialPolygons[nrow(r$residentialPolygons),]$id) ) ,
      stroke = TRUE,
      weight = 5,
      color = "#ba8e16",
      fill = TRUE,
      fillColor = "#ba8e16",
      opacity = 1,
      fillOpacity = 0.3,
      group = "eraseable",
      options = leaflet::pathOptions(pane = "layer2"),
      highlightOptions = leaflet::highlightOptions(fillColor = "#f2778d"))

  #remove intersecting polygons if they were present
  if(!is.null(intersectingPolysID)){
    leaflet::removeShape(proxy, layerId = intersectingPolysID)
  }

  leaflet::clearGroup(proxy, "first")
  leaflet::clearGroup(proxy, "after")


  #insert parkingPolygons into r$networkList
  r$networkList[[r$position]]$residential <- r$residentialPolygons
  #update nodes based on new polygon
  #get nodes
  nodes <- r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% dplyr::as_tibble() %>%
    sf::st_as_sf()

  finishedPoly <- r$residentialPolygons[nrow(r$residentialPolygons),]
  #get polygon size (should be in meters if proj = 4326)
  polyArea <- sf::st_area(finishedPoly)
  #convert to nb of agents (6agents / 100m^2)
  agentNb <- polyArea/15
  #get nodes within polygon and distribute number of agents equally among nodes (decimals allowed)
  # ADD the values to nodes (pre-populated with 0s). This is to avoid conflicts with nodes already filled due to exception below.
  nodeCount <- nrow(nodes[finishedPoly, op = sf::st_within])
  if(nodeCount > 0){
    #use sf polygon to select terra vect nodes (may have to convert to sf first)
    isWithin <- sf::st_within(nodes, finishedPoly)
    isWithin <- lengths(isWithin)
    nodes[isWithin > 0,]$newResidential <- nodes[isWithin > 0,]$newResidential + ( as.numeric(agentNb)/nodeCount )
  }else{
    #CAPTURE EXCEPTION : no nodes in polygon
    #in this case, find a single closest node outside polygon
    nearestIndex <- sf::st_nearest_feature(finishedPoly, sf::st_as_sf(nodes))
    nodes$newResidential[[nearestIndex]] <- agentNb
  }
  #update changes to networkList
  r$networkList[[r$position]]$network <- tidygraph::tbl_graph(nodes = dplyr::as_tibble(nodes),
                                                              edges = dplyr::as_tibble(r$networkList[[r$position]]$network %>% tidygraph::activate(edges)),
                                                              directed = FALSE)

  #remove pathUsage results, as new results must be simulated
  r$networkList[[r$position]]$pathUsage <- NULL

}, ignoreInit = TRUE)

# CANCEL EDGE ####
obsEvent_cnclEdg <- observeEvent(input$cnclEdg, {
  #remove edge
  r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% dplyr::filter(.data$edgeID_2 != r$edgID )

  #update proxy
  proxy <- leaflet::leafletProxy("versionMap") %>%
    leaflet::removeShape(layerId = as.character(r$edgID))

  shiny::removeModal()
}, ignoreInit = TRUE)
# CANCEL EDGE AND NODE ####
obsEvent_cnclEdgNode <- observeEvent(input$cnclEdgNode, {
  #remove new edge from network
  r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% dplyr::filter(.data$edgeID_2 != r$edgID )
  #remove new node from network
  r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% dplyr::filter(.data$nodeID_2 != r$nodeID )

  #update proxy
  proxy <- leaflet::leafletProxy("versionMap") %>%
    leaflet::removeShape(layerId = as.character(r$edgID)) %>%
    leaflet::removeMarker(layerId = as.character(r$nodeID))

  shiny::removeModal()

}, ignoreInit = TRUE)

        # EMPTY MAP WAS CLICKED ####
        obsMapClick <- shiny::observeEvent(input[["versionMap_click"]], {

          vftDbg("CLICK!!!!!")

          vftDbg(r$markerWasClicked)
          vftDbg(r$shapeWasClicked)
          vftDbg(r$isLinking)
          #if this is not Original
          if(r$position != 1){

            # EMPTY CLICK IN INFRASTRUCTURE CONTEXT ####
            if(input$contextChoice == 1){
              if(r$markerWasClicked == FALSE & r$shapeWasClicked == FALSE){

                if(r$isLinking == TRUE){
                  ## ADD NEW EDGE AND NODE ####
                  #if linking was in process, create a linked node
                  #first create node
                  networkNodes <- r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% dplyr::as_tibble()

                  #get max ID
                  maxID <- max( igraph::V(r$networkList[[r$position]]$network)$nodeID_2 )
                  vftDbg(maxID)

                  newID = maxID +1

                  #TODO: sample DULN raster to determine DULN level for node
                  DULNlevel <- 1
                  #TO DO: use AOI shapes to determine if this point is within an AOI
                  AOInb <- "0"

                  #add new node to node table
                  # r$networkList[[r$position]]$network <- r$networkList[[r$position]]$network %>%
                  #   bind_nodes(data.frame(nodeID = newID, DULN = DULNlevel, AOI = AOInb,
                  #                         geometry = st_sfc(
                  #                         st_point(c(input[["versionMap_click"]]$lng,
                  #                                    input[["versionMap_click"]]$lat))
                  #                         , crs = 4326),
                  #                         nodeID_2 = newID))


                  newNetwork <- r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) #%>% #tidygraph::tidygraph::activate(nodes)

                  #create new line to add
                  #convert id to integer id (remove X and number after ".")
                  originalID <- r$firstLinkNode$id
                  #split string on "." (requires [.] rather than .)
                  vftDbg(paste0("originalID 2: ", originalID) )
                  # splt <- strsplit(originalID, "[.]")
                  # ptID_1 <- splt[[1]][[1]]
                  # ptID_1 <- substr(ptID_1, 2, nchar(ptID_1) )

                  ptID_1 <- originalID

                  newLine <- data.frame( nodeID = igraph::V(newNetwork)$nodeID[length( igraph::V(newNetwork)$nodeID)] + 1, DULN = DULNlevel, AOI = AOInb,
                                         ID = newID,
                                         nodeID_2 = newID
                  )

                  geometryColumn <- newNetwork %>% tidygraph::activate(nodes) %>% dplyr::as_tibble()%>% dplyr::select( .data$geometry )

                  #remove geometry column (causes bugs when binding edges)
                  newNetwork <- newNetwork %>% tidygraph::select(-.data$geometry)

                  #bind nodes without geometry column
                  newNetwork <- newNetwork %>% tidygraph::bind_nodes(newLine)



                  #determine position where newLine was inserted
                  pos <- which(igraph::V(newNetwork)$nodeID_2 == newLine$nodeID_2)

                  #create geometry to add
                  geo <- list(geometry = sf::st_sfc(
                    sf::st_point(c(input[["versionMap_click"]]$lng,
                                   input[["versionMap_click"]]$lat)
                    ), crs = 4326
                  )
                  )

                  #bind Linestring to geometry at correct position
                  if(pos < nrow(geometryColumn) ){
                    geometryColumn <- rbind(geometryColumn[1:(pos-1),], list(geo$geometry) , geometryColumn[-(1:(pos-1)),])
                  }else{
                    geometryColumn <- rbind(geometryColumn, list(geo$geometry) )
                  }

                  #re-integrate geometry column to network
                  newNetworkNodes <- newNetwork %>% tidygraph::activate(nodes) %>% dplyr::as_tibble()
                  newNetworkNodes$geometry <- geometryColumn$geometry


                  # #recreate network graph and insert in reactives
                  tbl <- tidygraph::tbl_graph(edges = r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% dplyr::as_tibble(), nodes = newNetworkNodes, directed = FALSE)


                  r$networkList[[r$position]] <- list(network = tbl,  pathUsage = r$networkList[[r$position]]$pathUsage, parking = r$networkList[[r$position]]$parking, residential = r$networkList[[r$position]]$residential , newAttr = r$networkList[[r$position]]$newAttr, paintedRaster = r$networkList[[r$position]]$paintedRaster, canopyRaster = r$networkList[[r$position]]$canopyRaster )
                  # r$networkList[[r$position]]$network <- network




                  #remove pathUsage results, as new results must be simulated
                  r$networkList[[r$position]]$pathUsage <- NULL

                  proxy = leaflet::leafletProxy("versionMap")

                  circleMarker <- leaflet::addCircleMarkers(map = proxy,
                                                            lng = input[["versionMap_click"]]$lng, lat = input[["versionMap_click"]]$lat,
                                                            radius = 7.5,
                                                            color = "black",
                                                            fillColor = "white",
                                                            stroke = TRUE,
                                                            opacity = 0.5,
                                                            group = "nodes",
                                                            layerId = as.character(newID) )#,options = pathOptions(pane = "layer2")




                  #then create edge linking nodes

                  r$secondLinkNode <- list(id = newID,
                                                        lng = input[["versionMap_click"]]$lng,
                                                        lat = input[["versionMap_click"]]$lat)



                  vftDbg("SECOND LINK NODE:")
                  vftDbg(r$secondLinkNode)
                  vftDbg(r$secondLinkNode$lng)
                  vftDbg(r$secondLinkNode$lat)

                  #finalise linking

                  #replace clicked nodes

                  #get highest ID
                  networkEdges <- r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% dplyr::as_tibble()
                  maxID <- max(networkEdges$edgeID_2 )

                  newID = maxID +1

                  #TODO: sample DULN raster to determine DULN level for node
                  DULNlevel <- 1
                  #TO DO: use AOI shapes to determine if this point is within an AOI
                  AOInb <- "0"

                  shapeLeng <- as.numeric(raster::distance(
                    matrix(
                      data = c(r$firstLinkNode$lng, r$firstLinkNode$lat, r$secondLinkNode$lng, r$secondLinkNode$lat),
                      nrow = 2, ncol = 2,
                      dimnames = list(c("row1", "row2"), c("lon", "lat"))
                    ),
                    lonlat = TRUE
                  ))


                  #create geometric edge
                  geo <- sf::st_sfc(
                    sf::st_linestring(matrix(
                      c(
                        r$firstLinkNode$lng,
                        r$firstLinkNode$lat,
                        r$secondLinkNode$lng,
                        r$secondLinkNode$lat
                      ), nrow = 2, ncol = 2, byrow = TRUE)
                    )
                    , crs = 4326)



                  #convert id to integer id (remove X and number after ".")
                  originalID <- r$firstLinkNode$id
                  #split string on "." (requires [.] rather than .)
                  vftDbg(paste0("originalID 2: ", originalID) )
                  # splt <- strsplit(originalID, "[.]")
                  # ptID_1 <- splt[[1]][[1]]
                  # ptID_1 <- substr(ptID_1, 2, nchar(ptID_1) )

                  ptID_1 <- originalID

                  originalID <- r$secondLinkNode$id
                  #split string on "." (requires [.] rather than .)
                  # splt <- strsplit(originalID, "[.]")
                  # ptID_2 <- splt[[1]][[1]]
                  # ptID_2 <- substr(ptID_2, 2, nchar(ptID_2) )
                  ptID_2 <- originalID
                  #nodeID can be different from nodeID_2.
                  #we need to refer to the plotted node ids (nodeID_2) and update nodeID by linking nodeID to nodeID_2
                  newLine <- data.frame(from =  igraph::V(r$networkList[[r$position]]$network)$nodeID[igraph::V(r$networkList[[r$position]]$network)$nodeID_2 == as.double(ptID_1)],
                                        to = igraph::V(r$networkList[[r$position]]$network)$nodeID[igraph::V(r$networkList[[r$position]]$network)$nodeID_2 == as.double(ptID_2)],
                                        edgeID = newID, DULN_final = DULNlevel, AOI = AOInb,
                                        SHAPE_Leng = shapeLeng,
                                        from_2 =   igraph::V(r$networkList[[r$position]]$network)$nodeID[igraph::V(r$networkList[[r$position]]$network)$nodeID_2 == as.double(ptID_1)],
                                        to_2 = igraph::V(r$networkList[[r$position]]$network)$nodeID[igraph::V(r$networkList[[r$position]]$network)$nodeID_2 == as.double(ptID_2)],
                                        edgeID_2 = newID
                  )

                  geoLine <-dplyr::tibble(from = as.double(ptID_1),
                                          to = as.double(ptID_2),
                                          SHAPE = geo)

                  #add new edge to edge table
                  # networkEdges <- networkEdges %>% bind_rows(newLine)

                  networkNodes <- r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% dplyr::as_tibble()

                  newNetwork <- r$networkList[[r$position]]$network

                  #save geometry column
                  geometryColumn <- newNetwork %>% tidygraph::activate(edges) %>% dplyr::as_tibble()%>% dplyr::select( .data$SHAPE)

                  #remove geometry column (causes bugs when binding edges)
                  newNetwork <- newNetwork %>% tidygraph::activate(edges) %>% dplyr::select(-.data$SHAPE)

                  #bind edge without geometry column
                  newNetwork <- newNetwork %>% tidygraph::bind_edges(newLine)

                  #determine position where newLine was inserted
                  pos <- which(igraph::E(newNetwork)$edgeID_2 == newLine$edgeID_2)

                  #bind Linestring to geometry at correct position
                  if(pos < nrow(geometryColumn) ){
                    geometryColumn <- rbind(geometryColumn[1:(pos-1),], geoLine$SHAPE , geometryColumn[-(1:(pos-1)),])
                  }else{
                    geometryColumn <- rbind(geometryColumn, list(geoLine$SHAPE) )
                  }

                  #re-integrate geometry column to network
                  newNetworkEdges <- newNetwork %>% tidygraph::activate(edges) %>% dplyr::as_tibble()
                  newNetworkEdges$SHAPE <- geometryColumn$SHAPE



                  #recreate network graph and insert in reactives
                  tbl <- tidygraph::tbl_graph(edges = newNetworkEdges , nodes = r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% dplyr::as_tibble(), directed = FALSE)

                  r$networkList[[r$position]] <- list(network = tbl,  pathUsage = r$networkList[[r$position]]$pathUsage, parking = r$networkList[[r$position]]$parking, residential = r$networkList[[r$position]]$residential , newAttr = r$networkList[[r$position]]$newAttr, paintedRaster = r$networkList[[r$position]]$paintedRaster, canopyRaster = r$networkList[[r$position]]$canopyRaster )
                  # r$networkList[[r$position]]$network <- network

                  #insert node in dataframe
                  # networkNodes <- networkNodes %>% add_row(nodeID = newID, DULN = DULNlevel, AOI = AOInb,
                  # geometry = st_sfc(
                  #   st_point(c(input[["versionMap_click"]]$lng,
                  #              input[["versionMap_click"]]$lat))
                  #   , crs = 4326)
                  # )

                  # r$networkList[[r$position]] <- list(network = networkNodes, pathUsage = NULL)


                  #draw new edge
                  leaflet::leafletProxy("versionMap")%>%
                    leaflet::addPolylines(lng = c(r$firstLinkNode$lng,
                                                  r$secondLinkNode$lng),
                                          lat = c(r$firstLinkNode$lat,
                                                  r$secondLinkNode$lat),color = "red", weight = 3,
                                          layerId = as.character(newID),
                                          options = leaflet::pathOptions(pane = "layer1"))

                  #make node1 normal again
                  leaflet::leafletProxy("versionMap" )%>%
                    leaflet::removeMarker(layerId = "XXX") %>%
                    leaflet::removeMarker(layerId = r$firstLinkNode$id)%>%
                    leaflet::addCircleMarkers(lng = r$firstLinkNode$lng,
                                              lat = r$firstLinkNode$lat,
                                              layerId = r$firstLinkNode$id,
                                              group = "nodes",
                                              fill = TRUE,
                                              fillColor = "white",
                                              color = "grey",
                                              radius = 5,
                                              opacity = 1,
                                              fillOpacity = 1,
                                              weight = 1,
                                              options = leaflet::markerOptions(pane = "layer3")
                    )

                  #make node2 normal again
                  vftDbg(paste0("newID: ", newID))
                  leaflet::leafletProxy("versionMap" )%>%
                    leaflet::removeMarker(layerId = as.character(r$secondLinkNode$id))%>%
                    leaflet::addCircleMarkers(lng = r$secondLinkNode$lng,
                                              lat = r$secondLinkNode$lat,
                                              layerId = as.character(r$secondLinkNode$id),
                                              group = "nodes",
                                              fill = TRUE,
                                              fillColor = "white",
                                              color = "grey",
                                              radius = 5,
                                              opacity = 1,
                                              fillOpacity = 1,
                                              weight = 1,
                                              options = leaflet::markerOptions(pane = "layer3")
                    )


                  #reset the visual of everything (REMOVE?)
                  leaflet::leafletProxy("versionMap" )%>%
                    leaflet::removeMarker(layerId = r$firstLinkNode$id)%>%
                    leaflet::addCircleMarkers(lng = r$firstLinkNode$lng,
                                              lat = r$firstLinkNode$lat,
                                              layerId = r$firstLinkNode$id,
                                              group = "nodes",
                                              fill = TRUE,
                                              fillColor = "white",
                                              color = "grey",
                                              radius = 5,
                                              opacity = 1,
                                              fillOpacity = 1,
                                              weight = 1,
                                              options = leaflet::markerOptions(pane = "layer3")
                    )

                  #DETERMINE EDGE QUALITY ####
                  #id from click has "X" in front and sometimes ".", remove these

                  #get created edge id (this is NOT clicked edge id)
                  originalID <- newID
                  r$edgID <- originalID
                  r$nodeID <- r$secondLinkNode
                  r$originalID <- originalID


                  #launch Modal (with a slight lag, this allows the user to see the path created first)
                  shinyjs::delay(0.3, {
                    shiny::showModal(
                      shiny::modalDialog(

                        fluidRow(
                          shiny::column(12, align = "center",
                                        shiny::tags$h3('Choose qualities of new path:')
                          )
                        ),
                        shiny::radioButtons(shiny::NS(id, 'pathSignage'), 'Signage',
                                            choices = c("Wanderwege" = "c1", "Wander- und Velowege" = "c2", "Velowege" = "c3", "Nichts" = "c4"),
                                            selected = NULL, inline = TRUE),
                        shiny::radioButtons(shiny::NS(id, 'pathType'), 'Path Type',
                                            choices = c("Natur" = "c1", "Hardt" = "c2"),
                                            selected = NULL, inline = TRUE),
                        shiny::radioButtons(shiny::NS(id, 'pathWidth'), 'Path Width',
                                            choices = c("gross" = "c1", "mittel (3m)" = "c2", "schmal (2m)" = "c3", "sehr schmal (1m)" = "c4"),
                                            selected = NULL, inline = TRUE),
                        shiny::checkboxInput(shiny::NS(id,"areStairs"), label = "Are they stairs?", value = FALSE),

                        footer=shiny::tagList(
                          shiny::actionButton(inputId = shiny::NS(id, 'submitNewPath'), 'Einreichen'),
                          shiny::actionButton(inputId = shiny::NS(id,'cnclEdgNode'), "Abbrechen") )
                      ) )
                  })
                  # EXTRACT ATTR FOR NEW EDGE AND NODE####
                  #this is done after Modal in case its a bit slow
                  edgID <- newID
                  edgeSF <- r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% dplyr::filter(.data$edgeID_2 == edgID ) %>%
                    dplyr::as_tibble() %>% sf::st_as_sf()
                  edgeExtract <- terra::extract(r$DULN, edgeSF, fun = "mean")

                  nodeSF <- r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% dplyr::filter(.data$nodeID_2 == r$secondLinkNode$id ) %>%
                    dplyr::as_tibble() %>% sf::st_as_sf()
                  nodeExtract <- terra::extract(r$DULN, nodeSF)
                  #replace values for edge
                  # igraph::E(r$networkList[[r$position]]$network)$DULN_WALK_[[igraph::E(r$networkList[[r$position]]$network)$edgeID_2 == edgID]] <- edgeExtract[["walkNat"]]

                  #new edge
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_WALK_ <- edgeExtract[["walkNat"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_WALK1 <- edgeExtract[["walkSoc"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_BIKER <- edgeExtract[["bikerSport"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_EBIKE <- edgeExtract[["ebikeNat"]]
                  # igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_ALL <- edgeExtract[["all"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_JOGGE <- edgeExtract[["jog"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_DOG_N <- edgeExtract[["dogNat"]]
                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$DULN_DOG_P <- edgeExtract[["dogProx"]]

                  #add AOI label
                  AOInb <- sf::st_intersects(igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$SHAPE, finalPolygons)
                  if(any("list" %in% class(AOInb))){AOInb <- AOInb[[1]][1]}

                  igraph::E(r$networkList[[r$position]]$network)[[.data$edgeID_2 ==  edgID]]$AOI <- finalPolygons$AOI[as.numeric(AOInb)]


                  #new node
                  igraph::V(r$networkList[[r$position]]$network)[[.data$nodeID_2 ==  r$secondLinkNode$id]]$DULN_WALK_ <- nodeExtract[["walkNat"]]
                  igraph::V(r$networkList[[r$position]]$network)[[.data$nodeID_2 ==  r$secondLinkNode$id]]$DULN_WALK1 <- nodeExtract[["walkSoc"]]
                  igraph::V(r$networkList[[r$position]]$network)[[.data$nodeID_2 ==  r$secondLinkNode$id]]$DULN_BIKER <- nodeExtract[["bikerSport"]]
                  igraph::V(r$networkList[[r$position]]$network)[[.data$nodeID_2 ==  r$secondLinkNode$id]]$DULN_EBIKE <- nodeExtract[["ebikeNat"]]
                  # igraph::V(r$networkList[[r$position]]$network)[[.data$nodeID_2 ==  r$secondLinkNode$id]]$DULN_ALL <- nodeExtract[["all"]]
                  igraph::V(r$networkList[[r$position]]$network)[[.data$nodeID_2 ==  r$secondLinkNode$id]]$DULN_JOGGE <- nodeExtract[["jog"]]
                  igraph::V(r$networkList[[r$position]]$network)[[.data$nodeID_2 ==  r$secondLinkNode$id]]$DULN_DOG_N <- nodeExtract[["dogNat"]]
                  igraph::V(r$networkList[[r$position]]$network)[[.data$nodeID_2 ==  r$secondLinkNode$id]]$DULN_DOG_P <- nodeExtract[["dogProx"]]


                  AOInb_pt <- sf::st_intersects(igraph::V(r$networkList[[r$position]]$network)[[.data$nodeID_2 ==  r$secondLinkNode$id]]$geometry, finalPolygons)
                  if(any("list" %in% class(AOInb))){AOInb <- AOInb[[1]][1]}

                  igraph::V(r$networkList[[r$position]]$network)[[.data$nodeID_2 ==  r$secondLinkNode$id]]$AOI <- finalPolygons$AOI[as.numeric(AOInb)]



                  r$isLinking <- FALSE
                  r$firstLinkNode <- NULL
                  r$secondLinkNode <- NULL

                  #remove pathUsage results, as new results must be simulated
                  r$networkList[[r$position]]$pathUsage <- NULL

                }else{

                  ##ADD NEW NODE ####

                  vftDbg("EMPTY SPACE CLICKED")

                  networkNodes <- r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% dplyr::as_tibble()

                  #get max ID
                  maxID <- max(networkNodes$nodeID_2 )
                  vftDbg(maxID)

                  newID = maxID +1

                  #TODO: sample DULN raster to determine DULN level for node
                  DULNlevel <- 1

                  #determine AOI in which point is placed
                  AOInb_pt <- sf::st_intersects(sf::st_point(c(input[["versionMap_click"]]$lng,
                                                               input[["versionMap_click"]]$lat)), finalPolygons)


                  vftDbg(networkNodes)
                  #add new node to node table
                  networkNodes <- networkNodes %>% dplyr::bind_rows(data.frame(nodeID = networkNodes$nodeID[nrow(networkNodes)] + 1,
                                                                               DULN = DULNlevel, AOI = finalPolygons$AOI[as.numeric(AOInb_pt)],
                                                                               geometry = sf::st_sfc(
                                                                                 sf::st_point(c(input[["versionMap_click"]]$lng,
                                                                                                input[["versionMap_click"]]$lat))
                                                                                 , crs = 4326),
                                                                               nodeID_2 = newID))
                  vftDbg(networkNodes)

                  #recreate network graph and insert in reactives
                  tbl <- tidygraph::tbl_graph(edges = r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% dplyr::as_tibble(), nodes = networkNodes, directed = FALSE)

                  r$networkList[[r$position]] <- list(network = tbl, pathUsage = r$networkList[[r$position]]$pathUsage, parking = r$networkList[[r$position]]$parking, residential = r$networkList[[r$position]]$residential ,newAttr = r$networkList[[r$position]]$newAttr, paintedRaster = r$networkList[[r$position]]$paintedRaster, canopyRaster = r$networkList[[r$position]]$canopyRaster )
                  # r$networkList[[r$position]]$network <- network

                  vftDbg(r$networkList[[r$position]]$network)
                  #insert node in dataframe
                  # networkNodes <- networkNodes %>% add_row(nodeID = newID, DULN = DULNlevel, AOI = AOInb,
                  # geometry = st_sfc(
                  #   st_point(c(input[["versionMap_click"]]$lng,
                  #              input[["versionMap_click"]]$lat))
                  #   , crs = 4326)
                  # )

                  # r$networkList[[r$position]] <- list(network = networkNodes, pathUsage = NULL)

                  proxy = leaflet::leafletProxy("versionMap")

                  circleMarker <- leaflet::addCircleMarkers(map = proxy,
                                                            lng = input[["versionMap_click"]]$lng, lat = input[["versionMap_click"]]$lat,
                                                            radius = 7.5,
                                                            color = "black",
                                                            fillColor = "white",
                                                            stroke = TRUE,
                                                            opacity = 0.5,
                                                            group = "nodes",
                                                            layerId = as.character(newID))#,options = pathOptions(pane = "layer2")
                  vftDbg("created node: ")


                }
              }

              r$shapeWasClicked <- FALSE
              r$markerWasClicked <- FALSE

            }else if(input$contextChoice == 2){
              # EMPTY CLICK IN ATTRACTIVITY CONTEXT ####


            }else if(input$contextChoice == 3){
              #EMPTY CLICK IN PARKING/HOUSING CONTEXT ####

              if(!r$markerWasClicked ){ #& !r$shapeWasClicked
                if(!r$shapeWasClicked | !is.null(nrow(r$mapPoints) ) ){
                  if( !is.null(input[["versionMap_click"]]$lng) ){
                    #add points
                    r$mapPoints <- rbind(r$mapPoints,sf::st_as_sf( sf::st_sfc( sf::st_point(x = c(input[["versionMap_click"]]$lng, input[["versionMap_click"]]$lat)), crs = 4326) ) )
                    #draw points
                    vftDbg(r$mapPoints)

                    proxy = leaflet::leafletProxy("versionMap" )

                    circleMarker <- leaflet::addCircleMarkers(map = proxy,
                                                              lng = input[["versionMap_click"]]$lng, lat = input[["versionMap_click"]]$lat,
                                                              radius = ifelse(nrow(r$mapPoints) == 1, 7, 4),
                                                              color = ifelse(nrow(r$mapPoints) == 1, "red", "blue"),
                                                              stroke = ifelse(nrow(r$mapPoints) == 1, TRUE, FALSE),
                                                              fillOpacity = 0.5,
                                                              group = ifelse(nrow(r$mapPoints) == 1, "first", "after"),
                                                              options = leaflet::pathOptions(pane = "layer2"))
                  }
              }

              }
              #reset information if a marker was clicked
              r$markerWasClicked <- FALSE
              r$shapeWasClicked <- FALSE
              # }

            }
          }else{
            #if Original was clicked
            shiny::showModal(shiny::modalDialog(shiny::h3(shiny::HTML(i18n()$t("Sie können das <b>originale</b> Wegenetz <b>nicht</b> ändern."))),
                                                h3(shiny::HTML(i18n()$t("Bitte wählen Sie zuerst ein <b>anderes Szenario</b> aus und erstellen Sie es.")), shiny::img(src = "www/arrowRight.png", style = "float:right;height:100px;margin-right:-100px")),

                                                footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ))
            )
          }
        }, ignoreInit = TRUE)


        # obsMapRefresh <- observeEvent(updateNetworkPlot(),{
        #
        #
        #   proxy <- leafletProxy("versionMap")%>%
        #     clearGroup( "nodes")%>%
        #     clearGroup("edges")
        #
        #   # leafletProxy( "versionMap", data = r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% as_tibble()  %>% st_as_sf())%>%
        #   #   addPolylines(color = "black", weight = 3, group = "edges")
        #   #
        #   # leafletProxy( "versionMap", data = r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% as_tibble()  %>% st_as_sf())%>%
        #   #   addCircleMarkers(radius = 3, fillColor = "white", opacity = 1, weight = 2, group = "nodes")
        #
        #   leafletProxy("versionMap") %>%
        #
        #       tm_shape(r$networkList[[r$position]]$network %>% tidygraph::activate(edges) %>% as_tibble()  %>% st_as_sf()) +
        #       tm_lines(col = "black", lwd = 3, style = "fixed", popup.vars = FALSE, group = "edges", zindex = 420) +
        #       tm_shape(r$networkList[[r$position]]$network %>% tidygraph::activate(nodes) %>% as_tibble() %>% st_as_sf()) +
        #       tm_dots(size = 0.1, col = "white", popup.vars = FALSE, group = "nodes", zindex = 420) +
        #       tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = c(0.5) )
        #
        #
        #
        #
        # })











#
#     #IF ONE OF CREATED BUTTONS IS PRESSED
#
#     observeEvent(input[[inserted_inputId_removal]], {
#
#       print("inserted_inputId_removal PRESSED")
#
#
#     })
#
#     observeEvent(input[[inserted_inputId_select]], {
#
#       print("inserted_inputId_select PRESSED")
#
#
#     })



        # SHOW SENSITIVITY MATRIX ####

        obsSM <- shiny::observeEvent(input$showSM, {
          #show SM when switch is turned on (and there is a SM)
          if(input$showSM == 1 ){
            if( !is.null(SM_pres)){
              #show SM
              leaflet::leafletProxy("versionMap" )%>%
                leaflet::addRasterImage(x = raster::raster(SM_pres), colors = SMcolors, group = "SM", opacity = 0.7)
            }else if(is.null(SM_pres)){
              #write error precising that there is no SM or default SM is used
              #TODO write error
              return()
            }

          }else{
            #remove SM when switched is turned off
            leaflet::leafletProxy("versionMap" )%>%

              leaflet::clearGroup(group = "SM")
          }

        })

        obsPA <- shiny::observeEvent(input$showPA, {
          #show SM when switch is turned on (and there is a SM)
          if(input$showPA == 1 ){

            if( !is.null(shp_PA)){

              #prepare pal
              pal <- leaflet::colorFactor(
                palette = c("#a2e08a", "#4a8636", "#105200"),
                levels = c(3, 2, 1)
              )

              #show PA
              leaflet::leafletProxy("versionMap", data = shp_PA )%>%
                leaflet::addPolygons(group = "PA", opacity = 1, color = ~pal(PA_type), weight = 5)%>%
                leaflet::addLegend(layerId = "legend", title = "Zielgebiete:", position = "topright", labels = c("streng", "umfassend", "teilweise") , colors = c("#105200", "#4a8636", "#a2e08a"))
            }else if(is.null(SM_pres)){
              #write error precising that there is no SM or default SM is used
              #TODO write error
              return()
            }
          }else{
            #remove SM when switched is turned off
            leaflet::leafletProxy("versionMap" )%>%

              leaflet::clearGroup(group = "PA")%>%
              leaflet::removeControl(layerId = "legend")
          }
        })

        obsAOI <- shiny::observeEvent(input$showAOI, {
          #show ziegebiete when switch is turned on
          if(input$showAOI == 1 ){

            if( !is.null(shp_PA)){


              #show PA
              leaflet::leafletProxy("versionMap", data = finalPolygons )%>%
                leaflet::addPolygons(data = finalPolygons,
                                     weight = 3,
                                     color = "green",
                                     fillColor = "green",
                                     fill = TRUE,
                                     stroke = TRUE,
                                     options = leaflet::pathOptions(pane = "layer1"),
                                     opacity = 1,
                                     fillOpacity = 0.1,
                                     group = "AOI")

            }else if(is.null(SM_pres)){
              #write error precising that there is no SM or default SM is used
              #TODO write error
              return()
            }
          }else{
            #remove SM when switched is turned off
            leaflet::leafletProxy("versionMap" )%>%

              leaflet::clearGroup(group = "AOI")
          }
        })

      # CONFIRM NEW VERSIONS ####

      obsConfirm <- shiny::observeEvent( input$newVersionsConfirmButton, {


        # #similar to residential, but sampling parkingPolygons
        # #populate nodes with 0s in $parking
        # newnodes$parking <- 0
        # #cycle through parking polygons
        # for(polyNb in 1:nrow(parkingPolygons)){
        #
        #   #get polygon size (should be in meters if proj = 4326)
        #   polyArea <- sf::st_area(parkingPolygons[polyNb,])
        #   #convert to nb of agents (1agent / 30m^2)
        #   agentNb <- polyArea/30
        #   #get nodes within polygon and distribute number of agents equally among nodes (decimals allowed)
        #   # ADD the values to nodes (pre-populated with 0s). This is to avoid conflicts with nodes already filled due to exception below.
        #   nodeCount <- nrow(newnodes[parkingPolygons[polyNb,], op = sf::st_within])
        #   if(nodeCount > 0){
        #     #use sf polygon to select terra vect nodes (may have to convert to sf first)
        #     isWithin <- sf::st_within(newnodes, parkingPolygons[polyNb,])
        #     isWithin <- lengths(isWithin)
        #     newnodes[isWithin > 0,]$parking <- newnodes[isWithin > 0,]$parking + ( as.numeric(agentNb)/nodeCount )
        #   }else{
        #     #CAPTURE EXCEPTION : no nodes in polygon
        #     #in this case, find a single closest node outside polygon
        #     nearestIndex <- sf::st_nearest_feature(parkingPolygons[polyNb,], sf::st_as_sf(newnodes))
        #     newnodes$parking[[nearestIndex]] <- agentNb
        #   }



        r$trigger <- 1

        #update reactive
        # networkLst <- ntwrkLst_r()

        #REMOVE ALL VERSIONS ####
        # they are entirely refreshed when returning here from newVersions
        shiny::removeUI(selector = "div#placeholder")
        shiny::insertUI(selector = "#topPlaceHolder_newVersion",
                 ui = shiny::tags$div(
                   id = "placeholder"
                 )
        )

        #remove observer event (is rebuilt everytime this tab is revisited)
        #avoids congestion
        # cat(file = stderr(), "TESTA")

        if(!is.null(obsEvent_submit)){
          obsEvent_submit$destroy()
        }
        if(!is.null(obsEvent_addVersion)){
          obsEvent_addVersion$destroy()
        }
        if(!is.null(obsShapeClick)){
          obsShapeClick$destroy()
        }
        if(!is.null(obsMarkerClick)){
          obsMarkerClick$destroy()
        }
        if(!is.null(obsMapClick)){
          obsMapClick$destroy()
        }
        # cat(file = stderr(), "TESTB")

        if(!is.null(obsFinishRender)){
          obsFinishRender$destroy()
        }

        if(!is.null(obsEvent_deleteEdge)){
          obsEvent_deleteEdge$destroy()
        }

        if(!is.null(obsEvent_chooseParking)){
          obsEvent_chooseParking$destroy()
        }
        if(!is.null(obsEvent_submitPath)){
          obsEvent_submitPath$destroy()
        }

        if(!is.null(obsContext)){
          obsContext$destroy()
        }

        if(!is.null(obsEvent_chooseResidential)){
          obsEvent_chooseResidential$destroy()
        }

        if(!is.null(obsEvent_cnclEdg)){
          obsEvent_cnclEdg$destroy()
        }

        if(!is.null(obsConfirm)){
          obsConfirm$destroy()
        }

        if(!is.null(obsSM)){
          obsSM$destroy()
        }

        if(!is.null(obsEvent_cnclEdgNode)){
          obsEvent_cnclEdgNode$destroy()
        }


        if(length(r$appendedObservers) > 0 ){
          removeObservers(r$appendedObservers)
        }
        vftDbgCat("TESTC")

        #remove all non-linked segments in r$networkList
        for(networkNb in 1:length(r$networkList)){

          # graph <- igraph::as.igraph(r$networkList[[networkNb]]$network)
          # graph <- igraph::as.undirected(igraph::largest_component(graph, mode = "weak"))
          # r$networkList[[networkNb]]$network <- tidygraph::as_tbl_graph(graph, directed = FALSE)
          #
          # keep main component
          r$networkList[[networkNb]]$network <- r$networkList[[networkNb]]$network %>% tidygraph::convert(tidygraph::to_largest_component, .clean = TRUE)
          #correct node continuity (important to avoid errors)
          r$networkList[[networkNb]]$network <- r$networkList[[networkNb]]$network %>% tidygraph::activate(nodes) %>% dplyr::mutate(nodeID = 1:length(igraph::V(r$networkList[[networkNb]]$network)$nodeID))

          # r$networkList[[networkNb]]$pathUsage <- NULL
          #remove related elements

          }

        return(list(networkList = shiny::reactive({r$networkList}), confirm = shiny::reactive({input$newVersionsConfirmButton}, label = "TESTLABEL"), trigger_1 = shiny::reactive(r$trigger), versionsUI =  shiny::reactive(r$versionsUI)) )


      }, ignoreInit = TRUE, once = TRUE)




        shinyjs::disable("versionBtn0")

    return(list(networkList = shiny::reactive({r$networkList}), confirm = shiny::reactive({input$newVersionsConfirmButton}, label = "TESTLABEL"), trigger_1 = shiny::reactive(r$trigger), versionsUI =  shiny::reactive(r$versionsUI)) )

  })
}


