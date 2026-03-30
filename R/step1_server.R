
# Define server logic
step1_server <- function(id, i18n){
  shiny::moduleServer(id, function(input, output, session){

    cat(file = stderr(), "START STEP 1 SERVER\n")






    #make title for tab
    shiny::titlePanel("Besucherlenkungs-Tool")

    #render banner image from start
    output$bannerUI <- shiny::renderUI({
      imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step1_wsl.png"), list() )
#replace /" with ', to avoid problems
      return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
    })

    #Welcome window with info
    shinyjs::delay(500, {
      shiny::showModal(
        shiny::modalDialog(footer = actionButton(inputId = NS(id, "begin1"), label = i18n()$t("Los geht's!"), class = "btn-lg", style = "background-color:#006268; color:#ffffff"), size = "l" ,
                           shiny::selectInput(inputId = shiny::NS(id, "languageSelect_2"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr" , "English" = "en"),
                                              selected = "de", width = 100 ),
                           # h2(i18n()$t("Willkommen beim Visitor Flow Tool!"), align = "center"),
                           # h4(shiny::HTML(as.character(i18n()$t("(Entwickelt für Computerbildschirme und <b>nicht</b> für Handys)") ) ), align = "center" ),
                           # h4(),
                           # h3(i18n()$t("Entdecken Sie die Möglichkeiten zur Planung Biodiversitätsschutz und Naherholungsnutzung in der ganzen Schweiz!"), style = "text-align:center" ),
                           # h4(),
                           # h3(i18n()$t("Bei jedem Schritt können Sie oben rechts auf Hilfe und zusätzliche Informationen zugreifen:"), shiny::img(src = "www/arrowShow.png", style = "text-align:right;float:right;display:inline-block;height:150px; padding-left:70px;margin-right:-70px;vertical-align:middle;margin-top:-70px")),
                           # h4(),
                           # h4((i18n()$t("Die Hilfe und die Informationen, die Sie erhalten, beziehen sich auf Ihren aktuellen Schritt!") ) , style = "text-align:right;")

        )
      )
    })

    #DEPRECATED
    # simplified welcome message, but commented out for later reference

#     h4(shiny::HTML(as.character(i18n()$t("Diese Web-App befindet sich noch in einer <b>Testphase</b>.<br>
# Das heißt, es können noch Probleme auftreten und einige Funktionalitäten müssen noch implementiert werden.
# Sollten Probleme auftauchen, schreiben Sie bitte an den Ersteller der App (johan.frueh@wsl.ch) mit Details über die aufgetretenen Probleme.<br>
# <br>
# <b>Derzeit können Sie:</b><br>
# 1) Das Tool überall in der Schweiz verwenden<br>
# 2) Artenvielfaltskarten zu einer einzigen Sensitivitätsmatrix kombinieren,<br>
# 3) Attraktive Orte bestimmen, an welchen sih die Bewohner bevorzugt erholen (Zielgebiete),<br>
# 4) Manuell ‚Zielgebiete‘ anpassen,<br>
# 5) Die Naherholungsnutzung simulieren,<br>
# 6) Die Vielfalt der Informationen kombinieren, um einzelne Karten für die Kommunikation zu erstellen.<br>
# <br><b>Wir arbeiten bereits an folgenden Punkten:</b><br>
# 1) Das Tool mehrsprachig gestalten (Deutsch, Französisch, Italienisch und Englisch),<br>
# 2) Einführung einiger weiterer Funktionen,<br>
# 3) Einbeziehung weiterer Arten,<br>
# 4) Verbesserung von Geschwindigkeit und Stabilität.
# ")) ) ),
#     tags$style("
#       .checkbox { /* checkbox is a div class*/
#         line-height: 30px;
#         margin-bottom: 40px; /*set the margin, so boxes don't overlap*/
#       }
#       input[type='checkbox']{ /* style for checkboxes */
#         width: 20px; /*Desired width*/
#         height: 20px; /*Desired height*/
#         line-height: 20px;
#       }"),
#     div(style = "font-size: 13pt",
#         shiny::checkboxInput(inputId = shiny::NS(id, "needHelp"), label = strong(i18n()$t("Dies ist mein erstes Mal, ich brauche Hilfe!")) )
#     )

    #make upload limit ~200MB
    options(shiny.maxRequestSize=200*1024^2)

    r1 <- shiny::reactiveValues()
    r1$triggerNewShape <- 0
    r1$confirm <- NULL
    r1$needHelp <- FALSE

    rv <- shiny::reactiveValues(output = NULL)

    leafletMapID <- "areaSelectMap"
    numberOfPolygons <- "single"
    startingPolygons <- NULL


    #PREPARE DATA####
    ch_basemap2 <- NULL
    # Cache country border — same file for every user, load only once per R session
    if (!exists(".vft_countryshape", envir = .GlobalEnv)) {
      .GlobalEnv$.vft_countryshape <- sf::st_read(dsn = "www/data/maps/countryBorders/SwissBorder.shp", quiet = TRUE)
      sf::st_crs(.GlobalEnv$.vft_countryshape) <- 4326
    }
    countryshape <- .GlobalEnv$.vft_countryshape

    r1$pathNetwork <- NULL

    r1$confirmBtn1 <- NULL
    r1$confirmBtn2 <- NULL

    #assigning input value to reactive Values
    confirmVal <- shiny::reactiveVal(0)

    button1Visible <- shiny::reactiveVal(value = FALSE)
    button2Visible <- shiny::reactiveVal(value = FALSE)

    #the type of data given for the area (shapefile or coordinates)
    r1$shapeType <- ""
    #global variables for gps coordinates
    r1$gps_ls_tl <- NULL
    r1$gps_ls_br <- NULL
    #global variable for imported shapefile
    envStep1 <- new.env(parent = emptyenv())

    r1$shape <- NULL
    r1$finalShape <- NULL

    r1$basemap <- NULL
    r1$basemap_bw <-NULL
    r1$network <- NULL

    # RESET BUTTONS####
    #reset all button values
    shinyjs::reset("confirmButton1")
    shinyjs::reset("confirmButton2")

    #### CLICKABLE MAP FUNCTIONALITY ####

    #Polygon Creation####

    r <- shiny::reactiveValues()

    #no starting polygons at step1
    r$polygonsList <- NULL

    #container for all vertices of a polygon that is to be created
    r$mapPoints <- NULL
    r$mapPoints <-sf::st_sfc(crs = 4326) #empty list of generated sf points

    r$launchProgress <- 0
    r$promiseFinished <- FALSE
    r$ABMprogress <- NULL

    #variable to help avoid creating markers at the same time as a polygon is finalised by clicking on a marker.
    r$markerWasClicked <- FALSE

    r$polyFinished <- FALSE

    #populate given list container with variables called by the observed reactives.
    #This allows an outside variable to communicate between leaflet map and the reactives of this function.
    # r$markerWasClicked <- r$markerWasClicked

    r$DULN <- NULL
    r$DULN_all <- NULL

    #variable to determine choice of DULN to download
    r$attrToDownload <- "new"

    r$currentLang <- "de"

    mapMarkerClick <- paste0(leafletMapID, "_marker_click")
    mapGeojsonClick <- paste0(leafletMapID, "_geojson_click")
    mapClick <- paste0(leafletMapID, "_click")


    # MAP CLICK OBSERVERS ####
    ##shape click ####
    # simply remove shape (users may use this way to delete shape out of habit)
    r1$obsGeojsonClick <- shiny::observeEvent(input[[mapGeojsonClick]], {
      r$markerWasClicked <- TRUE

      #erase the existing polygons
      r$polygonsList <- NULL
      r1$finalPolygon <- NULL

      #replot
      leaflet::leafletProxy(leafletMapID )|>
        leaflet::clearGroup("eraseable")

      #remove confirm button
      button2Visible(FALSE)
      button1Visible(FALSE)

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    ## marker click ####
    r1$obsMarkerClick <- shiny::observeEvent(input[[mapMarkerClick]], {
      # variables$
      r$markerWasClicked <- TRUE
      if(!is.null(input[[mapMarkerClick]]$group) ){#& envBase$step1Refreshing != TRUE
        #FINALISE POLYGON ####
        #If first vertex of polygon is clicked, Finalise polygon
        if( input[[mapMarkerClick]]$group == "first"){
          if(nrow(r$mapPoints) > 2){

            #create polygon with points
            poly <- sf::st_cast(sf::st_combine(r$mapPoints), "POLYGON") #variables$
            poly <- sf::st_sf(poly)
            #check if polygon is not valid
            if(!sf::st_is_valid(poly)){
              #erase points
              r$mapPoints <- sf::st_sfc(crs = 4326)
              leaflet::leafletProxy(leafletMapID )|>
                leaflet::clearGroup("first")|>
                leaflet::clearGroup("after")

              shiny::showModal(
                shiny::modalDialog(
                  shiny::h3(shiny::HTML(as.character(i18n()$t("Das gezeichnete Polygon war <b>nicht brauchbar</b>, bitte zeichnen Sie ein <b>einfacheres Polygon</b>. <br>(z.B.: keine gekreuzten Linien)"))))
                )
              )
            }else if(!sf::st_intersects(poly, countryshape, sparse = FALSE)[[1, 1]]){
              #check if polygon entirely outside CH

              #erase points
              r$mapPoints <- sf::st_sfc(crs = 4326)
              leaflet::leafletProxy(leafletMapID )|>
                leaflet::clearGroup("first")|>
                leaflet::clearGroup("after")

              shiny::showModal(
                shiny::modalDialog(
                  shiny::h3(shiny::HTML(paste0(i18n()$t("Dieses Tool ist auf die <b>Schweiz</b> beschränkt.<br>"),
                                        i18n()$t("Bitte wählen Sie ein Bereich <b>innerhalb</b> seiner Grenzen."))))
                )
              )
            }else{
              #if so, error message and do not create polygon


              if(is.finite(max(r$polygonsList$id))){ #polygonEnv$
                poly$id <- max(r$polygonsList$id)+1 #polygonEnv$
              }else{
                poly$id <- 1
              }
              # poly <- concaveman(r$mapPoints, 1) Doesn't work well
              poly <- dplyr::rename(poly, polygons = "poly")

              # if(numberOfPolygons == "multi"){
              #   #check if new polygon intersects or overlaps with any other
              #   intersectingPolys <- which(sf::st_intersects(poly, r$polygonsList, sparse = FALSE)) #polygonEnv$
              #   if(length(intersectingPolys) > 0){
              #     #if so, combine it into a single polygon
              #     newPoly <- sf::st_as_sf(sf::st_union(c(poly$polygons, r$polygonsList[intersectingPolys,]$polygons) ) ) #polygonEnv$
              #     newPoly <- newPoly |> dplyr::rename(polygons = .data$x)
              #     #determine its general attractivity (with popup)
              #     newPoly$DULN <- 1
              #     #determine id
              #     newPoly$id <- max(r$polygonsList$id)+1 #polygonEnv$
              #     #remove intersecting polys
              #     r$polygonsList <- r$polygonsList[-intersectingPolys,]
              #     #add new polygon
              #     # polygonEnv$r$polygonsList[nrow(polygonEnv$r$polygonsList) + 1, ] <- newPoly
              #     poly <- newPoly
              #   }
              #
              #   #generate DULN value for polygon on the fly
              #   values <- terra::extract(r1$DULN$all, poly)
              #   poly$DULN <- mean(values$all[values$all > -20], na.rm = TRUE) #avoid values <= -20 as these are symbolic
              #
              #   #keep a table of polygons
              #   r$polygonsList <- rbind(r$polygonsList, poly)
              # }else{

              #keep a single polygon
              r$polygonsList <- poly

              # }
              r$polyFinished <- TRUE
              r$mapPoints <- sf::st_sfc(crs = 4326)
              proxy <- leaflet::leafletProxy(leafletMapID)|>
                leaflet::clearGroup("eraseable")|>
                leaflet::addGeoJSON(geojson = geojsonsf::sf_geojson(r$polygonsList),
                                    stroke = TRUE,
                                    weight = 5,
                                    color = "black",
                                    fill = TRUE,
                                    fillColor = "green",
                                    opacity = 1,
                                    group = "eraseable",
                                    options = leaflet::pathOptions(pane = "layer2"))

              leaflet::clearGroup(proxy, "first")
              leaflet::clearGroup(proxy, "after")

              # makeButton2()


            }

          }else{
            #TODO: write error (need more points)
            shiny::showModal(
              shiny::modalDialog(
                shiny::h3(shiny::HTML(paste0(i18n()$t("Die Polygone müssen <b>mindestens 3 Punkte</b> haben."), i18n()$t("<br>Bitte fügen Sie vor der Fertigstellung der Form einen <b>zusätzlichen Punkt</b> hinzu.") )) )
              )
            )
            cat(file = stderr(), "ERROR: not enough points.\n")
          }

        }
      }else if(r1$step1Refreshing == TRUE){
        r1$step1Refreshing <- FALSE
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    ## empty map click ####
    r1$obsMapClick <- shiny::observeEvent(input[[mapClick]], {
      #precised condition (default always evaluates as TRUE)
      # if(  inputConditionName == "DEFAULT" | input[[inputConditionName]] %in% inputConditionValue){

      if(!r$markerWasClicked){
        if( !is.null(input[[mapClick]]$lng) ){
          #add point
          r$mapPoints <- rbind(r$mapPoints,sf::st_as_sf( sf::st_sfc( sf::st_point(x = c(input[[mapClick]]$lng, input[[mapClick]]$lat)), crs = 4326) ) )
          #draw points
          print(r$mapPoints)

          proxy = leaflet::leafletProxy(leafletMapID )

          circleMarker <- leaflet::addCircleMarkers(map = proxy,
                                                    lng = input[[mapClick]]$lng, lat = input[[mapClick]]$lat,
                                                    radius = ifelse(nrow(r$mapPoints) == 1, 7, 4),
                                                    color = ifelse(nrow(r$mapPoints) == 1, "red", "blue"),
                                                    stroke = ifelse(nrow(r$mapPoints) == 1, TRUE, FALSE),
                                                    fillOpacity = 0.5,
                                                    group = ifelse(nrow(r$mapPoints) == 1, "first", "after"),
                                                    options = leaflet::pathOptions(pane = "layer2"))
        }

      }
      #reset information if a marker was clicked
      r$markerWasClicked <- FALSE
      # }
    }, ignoreInit = TRUE, ignoreNULL = FALSE)


    ##Polygon Deletion####

    #populate global variable
    if(is.null(r$polygonsList) ){
      r$polygonsList <- startingPolygons
    }



    mapGeojsonClick <- paste0(leafletMapID, "_geojson_click")



      r1$obsErase <- shiny::observeEvent(input[[paste0(leafletMapID, "_click")]], {

        #create variable if missing
        if(is.null(r$polyFinished) ){r$polyFinished <- FALSE}

        #when map is clicked but NO polygon was finalised
        if(r$polyFinished == FALSE){
          #and a polygon already exists
          if(!is.null(r$polygonsList) | !is.null(r1$shape)){

            #erase the existing polygon
            r$polygonsList <- NULL
            r1$shape <- NULL

            button1Visible(FALSE)
            button2Visible(FALSE)

            #replot
            leaflet::leafletProxy(leafletMapID )|>
              leaflet::clearGroup("eraseable")

            button2Visible(FALSE)


          }

        }else{
          #reset global variable
          r$polyFinished <- FALSE
        }

      })

    # }


    shiny::observeEvent(input$resetButton, {
      if(!is.null(startingPolygons)){
        r$polygonsList <- startingPolygons

        #replot polygons
        map <- leaflet::leafletProxy(leafletMapID )|>
          leaflet::clearGroup("eraseable")
        if(!is.null(nrow(startingPolygons)) ){
          map |> leaflet::addGeoJSON(
            geojson = geojsonsf::sf_geojson(startingPolygons),
            stroke = TRUE,
            weight = 5,
            color = "black",
            fill = TRUE,
            fillColor = "green",
            opacity = 1,
            group = "eraseable",
            options = leaflet::pathOptions(pane = "layer2")
          )
        }
        map
      }
    }, ignoreInit = TRUE)


    # FUNCTIONS ####

    # RENDER MAP ####
    output$areaSelectMap <- leaflet::renderLeaflet({

      tmap::tmap_mode('view')

      #if a shape exists already, draw it and zoom to it
      if(shiny::isolate(!is.null(r1$shape)) ){

        shape <- sf::st_as_sf(sf::st_transform(r1$shape, "epsg:4326"))
        bb <- sf::st_bbox(shape)
        #map with saved shape
        tmap::tmap_leaflet(
          tmap::tm_shape(countryshape) +
            tmap::tm_borders(col = "darkgreen", lwd = 3, zindex = 405) ,
          # +
            # tmap::tmap_options(basemap.server = 'OpenStreetMap', basemap.alpha = c(0.5) ),
          options = leaflet::leafletOptions(doubleClickZoom = FALSE,
                                            zoomSnap = 0.05, zoomDelta = 0.05,
                                            wheelPxPerZoomLevel = 60),
          in.shiny = TRUE) |>
          leaflet::addMapPane("layer1", zIndex = 410)|> leaflet::addMapPane("layer2", zIndex = 420)|> leaflet::addMapPane("layer3", zIndex = 450) |>
          leaflet::clearGroup("eraseable")|>
          leaflet::addGeoJSON(geojson = geojsonsf::sf_geojson( shape ),
                              stroke = TRUE,
                              weight = 5,
                              color = "black",
                              fill = TRUE,
                              fillColor = "green",
                              opacity = 1,
                              group = "eraseable",
                              options = leaflet::pathOptions(pane = "layer2"))|>
          leaflet::fitBounds(lng1 = bb[[1]], lat1 = bb[[2]], lng2 = bb[[3]], lat2 = bb[[4]]) |>
          leaflet::addProviderTiles(
            leaflet::providers$OpenStreetMap,
            options = leaflet::providerTileOptions(noWrap = TRUE)
          )

      }else{

        #if no shape exists, do original (empty) map
        tmap::tmap_leaflet(
          tmap::tm_shape(countryshape) +
            tmap::tm_borders(col = "darkgreen", lwd = 3, zindex = 405) ,
          options = leaflet::leafletOptions(doubleClickZoom = FALSE,
                                            zoomSnap = 0.05, zoomDelta = 0.05,
                                            wheelPxPerZoomLevel = 60),
          in.shiny = TRUE) |>
          leaflet::addMapPane("layer1", zIndex = 410)|> leaflet::addMapPane("layer2", zIndex = 420)|> leaflet::addMapPane("layer3", zIndex = 450)|>
          leaflet::addProviderTiles(
            leaflet::providers$OpenStreetMap,
            options = leaflet::providerTileOptions(noWrap = TRUE) )
      }
    })

    # OBSERVERS ####

    #dismiss Modal
    obs_dimissModal <- shiny::observeEvent(input$dismissModal, {
      shiny::removeModal()
    })

    #help Button ####
    obs_help1 <- shiny::observeEvent(input$helpButton1, {
      shiny::showModal(
        shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                           h2(i18n()$t("Wählen Sie eine Region in der Schweiz") ),
                           h3(shiny::strong(i18n()$t("Es gibt zwei Möglichkeiten:")) ),
                           h3(strong("1) "),i18n()$t("Laden Sie eine bestehende Datei (entweder "), shiny::strong(".kml"), i18n()$t("oder ein "), shiny::strong("shapefile"), ".)" ),
                           h4(i18n()$t("Für das "),shiny::strong("shapefile"), i18n()$t("müssen Sie mindestens 3 Dateien zusammen laden ("),shiny::strong(".shp, .shx, .dbf"),").") ,
                           h4(),
                           h4(),
                           h3(strong("2)"), i18n()$t("Klicken Sie auf verschiedene Punkte auf der Karte.") ),
                           h4(),
                           h4(i18n()$t("Der erste Punkt, den Sie auf diese Weise erstellen, ist groß und rot.") ),
                           h4(i18n()$t("Die nächsten Punkte sind kleiner und blau.") ),
                           shiny::div(style = "text-align:center",
                                      shiny::img(src = "www/firstSecondThirdClick.png", style = "height:75px")
                           ),
                           h4(i18n()$t("Wenn Sie erneut auf den roten Punkt klicken, wird der von Ihnen erstellte Ausschnitt auf der Karte fertiggestellt ( " ), strong(i18n()$t("Sie benötigen mindestens 3 Punkte!") ), ")"),
                           shiny::div(style = "text-align:center",
                                      shiny::img(src = "www/thirdLastClick.png", style = "height:75px")
                           ),
                           h4(i18n()$t("Probieren Sie es aus! Sie können jederzeit eine neue Form erstellen, indem Sie erneut auf die Karte klicken!") )

        )
      )
    })

    #observe info Button ####
    obs_info1 <- shiny::observeEvent(input$infoButton1, {
      shiny::showModal(
        shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                           h2(i18n()$t("Zusätzliche Informationen:") ),
                           h3(),
                           h3(i18n()$t("Das Tool kann in der ganzen Schweiz angewendet werden.") ),
                           h3(),
                           h4(i18n()$t("Die Genauigkeit der Naherholungssimulation wurde jedoch nur in zwei spezifischen Fallstudien evaluiert (entlang der Glatt in Zürich und entlang der Wigger im Aargau).") ),
                           h3(),
                           h4(i18n()$t("Außerdem beschränkte sich die Auswertung auf größere Muster (über 1 Hektar und alle Erholungsarten zusammen). Weitere Studien wären notwendig, um die Genauigkeit auf kleinerer Ebene zu verbessern und jeden Erholungssuchenden separat zu bewerten.") )


        )
      )
    })


    #Language Change ####
    langChangeObs <- observeEvent(input$languageSelect_1, {
      print("CHANGE LANGUAGE")
      if(input$languageSelect_1 == "de"){
        # i18n$set_translation_language('de')
        shiny.i18n::update_lang("de")
        i18n()$set_translation_language("de")
        r$currentLang <- "de"
        print("DE")
        output$bannerUI <- shiny::renderUI({
          imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step1_wsl.png"), list() )
          #replace /" with ', to avoid problems
          return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
        })

        if(input[[paste0(leafletMapID, "_zoom")]] >= 13){
          output$zoomText <- renderText({
            paste0("")
          })
        }else{
          #below a zoom level, write warning
          output$zoomText <- renderText({
            paste0("<font color=\'#dd1717\' size='3'><b>",i18n()$t("Warnung: In diesem Maßstab können die Berechnungen sehr lange dauern."), "<br>", i18n()$t("Bitte zoomen Sie weiter hinein, bevor Sie einen Bereich auswählen."),  "</b></font>")
          })
        }
      }else if(input$languageSelect_1 == "fr"){
        # i18n$set_translation_language('fr')
        shiny.i18n::update_lang("fr")
        i18n()$set_translation_language("fr")
        r$currentLang <- "fr"

        output$bannerUI <- shiny::renderUI({
          imgMap <- imageMap(NS(id, "banner"), "www/step1_wsl_fr.png", list() )
          #replace /" with ', to avoid problems
          return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
        })
        if(input[[paste0(leafletMapID, "_zoom")]] >= 13){
          output$zoomText <- renderText({
            paste0("")
          })
        }else{
          #below a zoom level, write warning
          output$zoomText <- renderText({
            paste0("<font color=\'#dd1717\' size='3'><b>",i18n()$t("Warnung: In diesem Maßstab können die Berechnungen sehr lange dauern."), "<br>", i18n()$t("Bitte zoomen Sie weiter hinein, bevor Sie einen Bereich auswählen."),  "</b></font>")
          })
        }
        print("FR")
      }else if(input$languageSelect_1 == "en"){
        # i18n$set_translation_language('en')
        shiny.i18n::update_lang("en")
        i18n()$set_translation_language("en")
        r$currentLang <- "en"

        output$bannerUI <- shiny::renderUI({
          imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step1_wsl_en.png"), list() )
          #replace /" with ', to avoid problems
          return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
        })
        if(input[[paste0(leafletMapID, "_zoom")]] >= 13){
          output$zoomText <- renderText({
            paste0("")
          })
        }else{
          #below a zoom level, write warning
          output$zoomText <- renderText({
            paste0("<font color=\'#dd1717\' size='3'><b>",i18n()$t("Warnung: In diesem Maßstab können die Berechnungen sehr lange dauern."), "<br>", i18n()$t("Bitte zoomen Sie weiter hinein, bevor Sie einen Bereich auswählen."),  "</b></font>")
          })
        }
        print("EN")
      }else if(input$languageSelect_1 == "it"){
        # i18n$set_translation_language('it')
        shiny.i18n::update_lang("it")
        output$bannerUI <- shiny::renderUI({
          imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step1_wsl.png"), list() )
          #replace /" with ', to avoid problems
          return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
        })
        if(input[[paste0(leafletMapID, "_zoom")]] >= 13){
          output$zoomText <- renderText({
            paste0("")
          })
        }else{
          #below a zoom level, write warning
          output$zoomText <- renderText({
            paste0("<font color=\'#dd1717\' size='3'><b>",i18n()$t("Warnung: In diesem Maßstab können die Berechnungen sehr lange dauern."), "<br>", i18n()$t("Bitte zoomen Sie weiter hinein, bevor Sie einen Bereich auswählen."),  "</b></font>")
          })
        }
        print("IT")
      }

    }, ignoreInit = TRUE)

    #Language change in initial information
    langChangeObs2 <- observeEvent(input$languageSelect_2, {

      shiny::updateSelectInput(inputId = "languageSelect_1", selected = input$languageSelect_2)

      #close current modal window
      shiny::removeModal()

      #change language
      shiny.i18n::update_lang(input$languageSelect_2)

      #and open new one
      # Welcome Message ####
        shiny::showModal(
          shiny::modalDialog(footer = actionButton(inputId = NS(id, "begin2"), label = i18n()$t("Los geht's!"), class = "btn-lg", style = "background-color:#006268; color:#ffffff"), size = "l" ,
                             shiny::selectInput(inputId = shiny::NS(id, "languageSelect_2"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                                                selected = input$languageSelect_2, width = 100 ),
                             h1(i18n()$t("Willkommen beim Visitor Flow Tool!"), align = "center"),
                             shiny::h4(shiny::HTML(as.character(i18n()$t(":help:"))), shiny::img(src = "www/arrow_show.png", style = "float:right;display:inline-block;position:absolute;height:150px; padding-left:0;margin-right:-280px;vertical-align:middle;margin-top:-60px;"), style = "text-align:right;"),
                             h2(),
                             h3(shiny::HTML(as.character(i18n()$t("Kombinieren Sie empfindliche Biodiversität und Naherholung in der Schweiz<br>in <b>5 einfachen Schritten</b>!"))), style = "text-align:center" ),
                             h2(),

                             shiny::img(src = i18n()$t("www/introBanner_de.png"), style = "align:center; width:871px"),
                             h2(),
                             h4(shiny::HTML(as.character(i18n()$t("Sie können die Ergebnisse nach jedem Schritt <b>speichern</b> und <b>herunterladen</b>.") ) ), style = "text-align:center" )


          )
        )



    }, ignoreInit = TRUE)

    #Zoom warning text ####
    zoomTextObs <- observeEvent(input[[paste0(leafletMapID, "_zoom")]],{
      #above a zoom level, no warning
      if(input[[paste0(leafletMapID, "_zoom")]] >= 13){
        output$zoomText <- renderText({
          paste0("")
        })
      }else{
        #below a zoom level, write warning
          output$zoomText <- renderText({
            paste0("<font color=\'#dd1717\' size='3'><b>",i18n()$t("Warnung: In diesem Maßstab können die Berechnungen sehr lange dauern."), "<br>", i18n()$t("Bitte zoomen Sie weiter hinein, bevor Sie einen Bereich auswählen."),  "</b></font>")
          })
      }
    })

    #make confirm button
    makeButton1 <- shiny::reactive({
      button1Visible(TRUE)
    })
    #hide confirm button
    hideButton1 <- shiny::reactive({
      button1Visible(FALSE)
    })

    #make confirm button
    makeButton2 <- shiny::reactive({
      button2Visible(TRUE)
    })
    #hide confirm button
    hideButton2 <- shiny::reactive({
      button2Visible(FALSE)
    })


    # REACTIVE - fileInput ####
    #determine what to do, when a file is uploaded
    determineConfirm_fileInput <- shiny::reactive({
      shiny::req(input$shp)
      shpdf <- input$shp
      if(is.null(shpdf)){
        return()
      }
      previouswd <- getwd()
      uploaddirectory <- dirname(shpdf$datapath[1])
      setwd(uploaddirectory)

      for(i in 1:nrow(shpdf)){
        file.rename(shpdf$datapath[i], shpdf$name[i])
      }
      setwd(previouswd)

      #cycle through imported extensions
      extList <- list()
      for(i in 1:length(input$shp$name)){
        extList[[i]] <- tools::file_ext(input$shp$name[i])
      }
      if(unique(extList %in% c('shp','dbf','sbn','sbx','shx',"prj", "xml", "cpg")) == TRUE){
        #if all three required files are present
        if( sum( c("shp", "shx", "dbf") %in% unique(extList)) == 3 ){

          r1$shape <- sf::st_read(paste(uploaddirectory, shpdf$name[grep(pattern="*.shp$", shpdf$name)], sep="/"))#,  delete_null_obj=TRUE)
          r1$shape <- sf::st_transform(r1$shape, crs = sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


          r1$shapeType <- "shapefile"
          button1Visible(TRUE)
          button2Visible(FALSE)

          #erase points
          r$mapPoints <- sf::st_sfc(crs = 4326)
          leaflet::leafletProxy(leafletMapID )|>
            leaflet::clearGroup("first")|>
            leaflet::clearGroup("after")

          #TODO: reset text input if necessary
          shiny::updateTextInput(inputId = "gps_tl", value = "X.XXX..., X.XXX...")
          shiny::updateTextInput(inputId = "gps_br", value = "X.XXX..., X.XXX...")

          #trigger plot
          r1$triggerNewShape <- r1$triggerNewShape +1
          #update extent

        }else{

          #Either show error message on page
          # output$zoomText <- shiny::renderText(shiny::HTML(as.character(i18n$t("<font color=\'#dd1717\' size='3'><bold>Fehlende Dateien:</b> Ein <b>Shapefile</b> benötigt <b>mindestens</b> eine <b>.shp, .shx und .dbf</b> Datei. Bitte fügen Sie diese bei.</font>") ) ) )

          #or popup a window to signal error:
          shiny::showModal(
            shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                               shiny::h3(shiny::HTML(paste0("<font color=\'#dd1717\'><b>", i18n()$t("Fehlende Dateien:"), "</b></font>") ) ) ,
                               shiny::h4(shiny::HTML(paste0("<font color=\'#dd1717\'>",i18n()$t("Ein <b>Shapefile</b> benötigt <b>mindestens</b> eine <b>.shp, .shx und .dbf</b> Datei. Bitte fügen Sie diese bei."), "</font>" ) )

                               )
            )
          )
        }



      }else if(unique(extList %in% c('kml')) ){
        r1$shape <- sf::st_read(paste(uploaddirectory, shpdf$name[grep(pattern="*.kml$", shpdf$name)], sep="/"))

        r1$shape <- sf::st_transform(r1$shape, sf::st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))


        r1$shapeType <- "shapefile"
        button1Visible(TRUE)
        button2Visible(FALSE)
        # showMap_shp(r1$shape)
        #TODO: reset text input if necessary
        shiny::updateTextInput(inputId = "gps_tl", value = "X.XXX..., X.XXX...")
        shiny::updateTextInput(inputId = "gps_br", value = "X.XXX..., X.XXX...")

        #trigger plot
        r1$triggerNewShape <- r1$triggerNewShape +1
        #update extent

        }else{
          #or popup a window to signal error:
          shiny::showModal(
            shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                               shiny::h3(shiny::HTML(as.character(i18n()$t("<font color=\'#dd1717\'><b>Falsche Datei:</b></font>") ) ) ),
                               shiny::h4(shiny::HTML(as.character(i18n()$t("<font color=\'#dd1717\'>Bitte laden Sie entweder ein <b>Shapefile</b> (.shp, .shx, .dbf) oder eine <b>kml-Datei</b> (.kml) hoch.</font>") ) ) )

            )
          )
      }
    })

    #modal dismiss
    shiny::observeEvent(input$begin2,
                        {
                          shiny::removeModal()
                        }
                          )



    # ADD SHAPE ####
    shiny::observeEvent(r1$triggerNewShape, {

      # r1$finalShape <- sf::st_as_sf(r1$shape, coords = c("long", "lat"), crs = sf::st_crs(4326)) |> sf::st_combine() |> sf::st_cast( "POLYGON") |> sf::st_sf()
      r1$finalShape <- sf::st_as_sfc(r1$shape, coords = c("long", "lat"), crs = sf::st_crs(4326)) |> sf::st_combine() |> sf::st_cast( "POLYGON") |> sf::st_sf()


      bb <- sf::st_bbox(r1$finalShape)
      #update plot
      proxy <- leaflet::leafletProxy("areaSelectMap")|>
        leaflet::flyToBounds(lng1 = bb[[1]], lat1 = bb[[2]], lng2 = bb[[3]], lat2 = bb[[4]])|>
        leaflet::clearGroup("eraseable")|>
        leaflet::addGeoJSON(geojson = geojsonsf::sf_geojson( r1$finalShape),
                   stroke = TRUE,
                   weight = 5,
                   color = "black",
                   fill = TRUE,
                   fillColor = "green",
                   opacity = 1,
                   group = "eraseable",
                   options = leaflet::pathOptions(pane = "layer2"))

    }, ignoreInit = TRUE)


    # ONCE PROMISED FINISHED ####
    observeEvent(rv$output, {


      shiny::showModal(shiny::modalDialog(
        shiny::p("PROMISE FINISHED")
      ))
      cat(file = stderr(), "PROMISE FINISHED")
      #
      # r$promiseFinished <- TRUE
      # r$ABMprogress$close()

    }, ignoreInit = TRUE, ignoreNULL = TRUE)


    #Button visiblity
    #isButtonVisible <- reactive({return(buttonVisible())})

    obs <- shiny::observe({
      shinyjs::useShinyjs()
      shinyjs::hide(id ="confirmButton1" )
      shinyjs::hide(id ="attrButton" )

      if( button1Visible() ){
        shinyjs::show(id = "confirmButton1")
        shinyjs::show(id ="attrButton" )

      }
    })

    obs <- shiny::observe({
      shinyjs::useShinyjs()
      shinyjs::hide(id ="confirmButton2" )
      shinyjs::hide(id ="attrButton" )

      if( button2Visible() ){
        shinyjs::show(id = "confirmButton2")
        shinyjs::show(id ="attrButton" )

      }
    })

    #trigger primary reactive events (uploading file or inserting coordinates)
    shiny::observeEvent(input$gps_tl, {
      determineConfirm_textInput()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$gps_br, {
      determineConfirm_textInput()
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$shp, {
      determineConfirm_fileInput()
    }, ignoreInit = TRUE)

    ## polygon finalisation ####
    #When polygon is finalised, remove any file data and make Confirm button appear
    # shiny::observeEvent(input[["areaSelectMap_marker_click"]], {
    shiny::observeEvent(r$polyFinished, {
      if(!is.null(r$polygonsList)){
      #determine type of shape
      r1$shapeType <- "drawing"

      button2Visible(TRUE)
      button1Visible(FALSE)
      # input$shp <- NULL
      }

    }, ignoreInit = TRUE)

    if(is.null(r1$obsConfirmBtn1)){
      r1$obsConfirmBtn1 <- shiny::observeEvent(input$confirmButton1, {

        # data_promise(NULL)

        cat(file = stderr(), "CONFIRM BUTTON1")

        openSaveHelpModal("gotSavedHelp1")

      }, ignoreInit = TRUE    )

    }

    # observe attractivity click ####
    obsDownloadAttr <- shiny::observeEvent(input$attrButton, {

      shiny::showModal(
        shiny::modalDialog(
          shiny::fluidPage(
            shiny::fluidRow(align = "center",
              shiny::h2(i18n()$t("Wählen Sie eine Attraktivitätskarte zum Herunterladen")),
              shiny::h2()
            )
          ),
          shiny::fluidRow(align = "center",
          shiny::actionButton(
            inputId = NS(id, "oldAttrButton"),
            label = shiny::HTML(as.character(i18n()$t("<b>alte</b> Naherholungskarte<br>(Kienast et al., 2012)"))),
            class = "btn-warning"

          ),
          shiny::actionButton(
            inputId = NS(id, "newAttrButton"),
            label = shiny::HTML(as.character(i18n()$t("<b>neue</b> Naherholungskarte<br>aus diesem Tool"))),
            class = "btn-success btn-lg"
          ),
          shiny::fluidRow(align = "left",style = "margin-left:20px;margin-right:20px;",
                          shiny::h6(shiny::HTML("Kienast, F., Degenhardt, B., Weilenmann, B., Wäger, Y. and Buchecker, M.(2012)'GIS-assisted mapping of landscape suitability for nearby recreation, <em>Landscape and Urban Planning</em>.")))),
        footer = NULL)
      )

    }, ignoreInit = TRUE)

    obsNewAttrBtn <- shiny::observeEvent(input$newAttrButton,{

      r$attrToDownload <- "new"
      shinyjs::click("downloadAttr", asis = FALSE)
      shiny::removeModal()
    })
    obsOldAttrBtn <- shiny::observeEvent(input$oldAttrButton,{

      r$attrToDownload <- "old"
      shinyjs::click("downloadAttr", asis = FALSE)
      shiny::removeModal()
    })

    openSaveHelpModal <- function(buttonVersion){
      shinyjs::runjs("window.scrollTo(0, 0)")
      shiny::showModal(
        shiny::modalDialog(footer = actionButton(inputId = NS(id, buttonVersion), label = i18n()$t("Weiter zu Schritt 2."), class = "btn-lg", style = "background-color:#006268; color:#ffffff" ),
                           shiny::h2(i18n()$t("Sie haben Schritt 1 geschafft!") ),
                           shiny::h3(shiny::HTML(as.character(i18n()$t("Zu Beginn des nächsten Schrittes wird automatisch eine Datei in Ihren <b>'Download'</b>-Ordner heruntergeladen.") ))),
                           h3(),
                           shiny::h4(shiny::HTML(as.character(i18n()$t("Dies ist eine <b>Speicherung</b> Ihres Fortschritts. Am Ende <b>jedes Schritts</b> wird eine <b>neue Speicherung</b> heruntergeladen.") ))),
                           shiny::h3(),
                           shiny::div(style = "white-space:nowrap",
                                      shiny::h4(shiny::HTML(as.character(i18n()$t("Bei Ihrer <b>nächsten Sitzung</b>, können Sie mit <b>dieser Taste</b><br>gespeicherte Inhalte wieder laden!"))), img(src = "www/arrow_show.png", style = "float:right;display:inline-block;height:150px; margin-right:-200px;vertical-align:middle;margin-top:-90px"), style = "text-align:right"),
                           ),
                           shiny::h4(shiny::HTML(as.character(i18n()$t("Dadurch werden Sie zu dem entsprechenden Schritt <b>weitergeleitet</b>, wobei alle getätigten Eingaben <b>wiederhergestellt</b> werden!"))) )
        )
      )
    }

    #PREPARE ATTRACTIVITY DOWNLOAD ####
    output$downloadAttr <- shiny::downloadHandler(
      filename = function(){

        if(r$currentLang == "de"){
          name <- "visitorFlow_attractivität.zip"
        }else if(r$currentLang == "fr"){
          name <- "visitorFlow_attractivité.zip"
        }else if(r$currentLang == "en"){
          name <- "visitorFlow_attractivity.zip"
        }

        return(name)
      },

      content = function(file) {

        # Create a dedicated temp folder with a clean name
        tmpDir <- tempfile(pattern = "attractivity_download")
        dir.create(tmpDir)

        # Define clean file names inside that folder
        tifFile  <- file.path(tmpDir, "attractivity.tif")
        txtFile  <- file.path(tmpDir, "INFO_attractivity.txt")

        # --- Load raster & write text based on selection ---
        if (r$attrToDownload == "new") {
          attr <- terra::rast("www/data/maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif")
          writeLines(c(
            "Information about new Attractivity map:",
            "The raster was developed by aggregating various features known or assumed to attract recreationists, following the method of Kienast et al. 2012",
            "Relative to Kienast et al. 2012, it is up to date (data from 2025), is more detailed and covers all of Switzerland."

          ), txtFile)

        } else {
          attr <- terra::rast("www/data/maps/DULN/DULN_old_epsg4326.tif")
          writeLines(c(
            "Information about old Attractivity map ('DULN')",
            "Developed by Kienast et al. 2012",
            "'GIS-assisted mapping of landscape suitability for nearby recreation'",
            "Landscape and Urban Planning"
          ), txtFile)
        }

        # --- Crop raster ---
        attrCrop <- NULL
        if (!is.null(r$polygonsList)) {
          attrCrop <- terra::crop(attr, r$polygonsList, mask = TRUE)
        } else if (!is.null(r1$finalShape)) {
          attrCrop <- terra::crop(attr, r1$finalShape, mask = TRUE)
        }

        # --- Write & zip ---
        if (!is.null(attrCrop)) {
          terra::writeRaster(attrCrop, filename = tifFile, overwrite = TRUE)

          # Zip using relative paths by setting wd to tmpDir
          oldWd <- setwd(tmpDir)
          on.exit(setwd(oldWd), add = TRUE)  # always restore wd

          utils::zip(file, files = c("attractivity.tif", "INFO_attractivity.txt"))

        } else {
          print("ERROR: could not crop attractivity layer")
        }
      }


      #old
      # content = function(file){
      #   #put areas of interest
      #   tempGDB_attr <- tempfile(pattern = "attractivity_", fileext = ".tif")
      #
      #   if(r$attrToDownload == "new"){
      #     #load attractivity
      #     attr <- terra::rast( "www/data/maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif")
      #
      #     #text info
      #     tempTXT_info <- tempfile(pattern = "INFO_", fileext = ".txt")
      #     fileConn<-file(tempTXT_info)
      #     writeLines(c("Information about new Attractivity map",
      #                  "The raster was developed by aggregating various features known or assumed to attract recreationists",
      #                  "Table describing these is available at..." ), fileConn)
      #     close(fileConn)
      #   }else{
      #     attr <- terra::rast( "www/data/maps/DULN/DULN_old_epsg4326.tif")
      #
      #     #text info
      #     tempTXT_info <- tempfile(pattern = "INFO_", fileext = ".txt")
      #     fileConn<-file(tempTXT_info)
      #     writeLines(c("Information about old Attractivity map ('DULN')",
      #                  "Developed by Kienast et al. 2012",
      #                  "Table describing these is available at..." ), fileConn)
      #     close(fileConn)
      #   }
      #
      #   #crop to path
      #   if(!is.null(r$polygonsList)){
      #   attrCrop <- terra::crop(attr, r$polygonsList, mask = TRUE)
      #   }else{
      #     if(!is.null(r1$finalShape)){
      #       attrCrop <- terra::crop(attr, r1$finalShape, mask = TRUE)
      #     }
      #
      #   }
      #
      #   if(!is.null(attrCrop)){
      #     terra::writeRaster(attrCrop, filename = tempGDB_attr, overwrite = TRUE)
      #
      #
      #
      #     #zip both
      #     utils::zip(file, c(tempGDB_attr, tempTXT_info), flags = NULL)
      #   }else{
      #     print("ERROR: could not crop attractivity layer")
      #   }
      #
      #
      # }
    )
    outputOptions(output, "downloadAttr", suspendWhenHidden = FALSE)


    # LOAD SAVED DATA ####
    shiny::observeEvent(input$loadSavedData, {

      #as for name of new version
      shiny::showModal(shiny::modalDialog(
        shiny::tags$h3(i18n()$t('Gespeicherte Datei im Download-Dokument finden:')),
        shiny::fileInput(shiny::NS(id, 'data'), i18n()$t('Laden Sie Datei (.RData)'), accept = ".RData"),
        footer=shiny::tagList(
          shiny::actionButton(inputId = shiny::NS(id, 'submit'), i18n()$t('Einreichen')),
          shiny::modalButton(i18n()$t('Stornieren')) )
      )
      )
    }, ignoreInit = TRUE)

    #Use name to create version ####
    #add observer to variable, destroy it when leaving tab (recreated upon return)
    #Submit Loaded Savefile
    obsEvent_submit <- shiny::observeEvent(input$submit, {

      shiny::req(input$data)
      print(input$data)

      shiny::removeModal()

      #load saved data
      # load(input$data$datapath[1])
      r$datapath <- input$data$datapath[1]

      r1$confirm <- -1

      return(
        list(ffshape = shiny::reactive(r1$finalShape),
             confirm = shiny::reactive(r1$confirm),
             basemap = shiny::reactive(r1$basemap),
             basemap_bw = shiny::reactive(r1$basemap_bw),
             network = shiny::reactive(r1$pathNetwork),
             needHelp = shiny::reactive(r1$needHelp),
             DULN = shiny::reactive(r$DULN),
             DULN_all = shiny::reactive(r$DULN_all),
             datapath = shiny::reactive(r$datapath),
             currentLang = shiny::reactive(i18n()$get_translation_language())

        )
      )

    })

    #confirm button 2
    if(is.null(r1$obsConfirmBtn2)){
      r1$obsConfirmBtn2 <- shiny::observeEvent(input$confirmButton2, {
        print("CONFIRM BUTTON")


        openSaveHelpModal("gotSavedHelp2")

      }, ignoreInit = TRUE)

    }

    #model confirm of first help window
    obsSavedHelp1 <- observeEvent(input$gotSavedHelp1, {
      shiny::removeModal()

      #generate a final shapefile using shapefile or coordinates
      if(r1$shapeType == "shapefile" | r1$shapeType == "coordinates"){

        if(!is.null(r$polygonsList) ){
          r1$finalShape <- r$polygonsList
        }else if(r1$shapeType == "shapefile"){


        }

        finalShape <- r1$finalShape

        progress <- ipc::AsyncProgress$new(message = i18n()$t('Laden von relevanten Daten...'),
                                           detail = i18n()$t("Dies sollte weniger als 30 Sekunden dauern." ) ,
                                           queue = ipc::shinyQueue(),
                                           millis = 1000 )

        #launch job in parallel
        future::future({

        #format shape for later use
        #transform to web Mercator
        finalShape <- sf::st_transform(finalShape, crs = "EPSG:3857")

        #place buffer around polygon for some margin
        shape_larger <- sf::st_as_sfc(sf::st_buffer(finalShape, dist = 1000))

        #use shape to prepare spatial filter as wkt (well-known text), grow slightly for buffer
        wkt <- sf::st_as_text( sf::st_transform(shape_larger, "epsg:4326") )

        progress$inc(1/3)

        #extract relevant foot paths
        loadedPaths <- sf::st_read("www/data/maps/paths/paths_11_24_final_4.gdb",
                                   query = 'SELECT * FROM "paths_11_24_final_4"',
                                   wkt_filter = wkt,
                                   promote_to_multi = FALSE, type = 2
        )
        progress$inc(1/3)

        #use function to prepare node-edge table
        sfTidyGraphResults <- sf_to_tidygraph3(loadedPaths, shape_larger, directed = FALSE)

        progress$inc(1/3)
        progress$close()

        sfTidyGraphResults
      }, seed = TRUE) %...>%(function(sfTidyGraphResults){

        r1$pathNetwork <- sfTidyGraphResults[[1]][[1]]
        r$DULN <- terra::unwrap(sfTidyGraphResults[[2]][[1]])#, parkingPolygons = r1$parkingShapes
        r$DULN_all <- terra::unwrap(sfTidyGraphResults[[3]][[1]])
        #cleanup
        #remove observers of marker and map clicks from PolygonCreator
        # envBase$obsMapClick$destroy()
        # envBase$obsMarkerClick$destroy()
        # envBase$obsErase$destroy()
        # obsConfirmBtn2$destroy()
        # obsConfirmBtn1$destroy()

        r$promiseFinished <- TRUE

        # progress$inc(1/4, detail = "finalizing...")
        r1$confirm <- 1
        return(
          list(ffshape = shiny::reactive(r1$finalShape),
               confirm = shiny::reactive(r1$confirm),
               basemap = shiny::reactive(r1$basemap),
               basemap_bw = shiny::reactive(r1$basemap_bw),
               network = shiny::reactive(r1$pathNetwork),
               needHelp = shiny::reactive(r1$needHelp),
               DULN = shiny::reactive(r$DULN),
               DULN_all = shiny::reactive(r$DULN_all),
               datapath = shiny::reactive(r$datapath),
               currentLang = shiny::reactive(i18n()$get_translation_language())

          )
        )
      })


      }else{
        output$errorText <- shiny::renderText(i18n()$t("ERROR: cannot determine shapefile") )
      }


    })

    #observe confirm of second help window
    obsSavedHelp2 <- observeEvent(input$gotSavedHelp2, {
      shiny::removeModal()

      #generate a final shapefile using shapefile or coordinates
      if(r1$shapeType == "drawing"){

        if(!is.null(r$polygonsList) ){
          r1$finalShape <- r$polygonsList
        }else if(r1$shapeType == "shapefile"){
          print("SHAPEFILE")
          print(r1$shape)
          # r1$shape <- broom::tidy(r1$shape)
          r1$finalShape <- r1$shape

        }

        finalShape <- r1$finalShape

        #start progress bar
        progress <- ipc::AsyncProgress$new(message = i18n()$t('Laden von relevanten Daten...'),
                                           detail = i18n()$t("Dies sollte weniger als 30 Sekunden dauern.") ,
                                           queue = ipc::shinyQueue() ,
                                           millis = 1000)

        #launch job in parallel
        future::future({

        #format shape for later use
        shp <- sf::st_as_sf(finalShape, coords = c("long", "lat"), crs = sf::st_crs(4326))
        shp <- sf::st_combine(shp)
        shp <- sf::st_cast(shp, "POLYGON")

        #transform to web Mercator
        finalShape <- sf::st_transform(shp, crs = "EPSG:3857")

        #place buffer around polygon for some margin
        shape_larger <- sf::st_buffer(finalShape, dist = 1000)
        shape_larger <- sf::st_transform(shape_larger, "epsg:4326" )

        #use shape to prepare spatial filter as wkt (well-known text), grow slightly for buffer
        wkt <- sf::st_as_text(sf::st_transform(shape_larger, "epsg:4326") )

        progress$inc(1/3)

        #extract relevant foot paths
        loadedPaths <- sf::st_read("www/data/maps/paths/paths_11_24_final_4.gdb",
                                   query = 'SELECT * FROM "paths_11_24_final_4"',
                                   wkt_filter = wkt,
                                   promote_to_multi = FALSE, type = 2
        )

        progress$inc(1/3)

        #use function to prepare node-edge table
        sfTidyGraphResults <- sf_to_tidygraph3(loadedPaths, shape_larger, directed = FALSE)

        progress$inc(1/3)
        progress$close()

        sfTidyGraphResults

        }, seed = TRUE) %...>%(function(sfTidyGraphResults){

        r1$pathNetwork <- sfTidyGraphResults[[1]][[1]]
        r$DULN <- terra::unwrap(sfTidyGraphResults[[2]][[1]])#, parkingPolygons = r1$parkingShapes
        r$DULN_all <- terra::unwrap(sfTidyGraphResults[[3]][[1]])

        #
        r$markerWasClicked <- FALSE

        r1$confirm <- 1

        r$promiseFinished <- TRUE

        return(
          list(ffshape = shiny::reactive(r1$finalShape),
               confirm = shiny::reactive(r1$confirm),
               basemap = shiny::reactive(r1$basemap),
               basemap_bw = shiny::reactive(r1$basemap_bw),
               network = shiny::reactive(r1$pathNetwork),
               needHelp = shiny::reactive(r1$needHelp),
               DULN = shiny::reactive(r$DULN),
               DULN_all = shiny::reactive(r$DULN_all),
               datapath = shiny::reactive(r$datapath),
               currentLang = shiny::reactive(i18n()$get_translation_language())

          )
        )
        })

      }else{
        output$errorText <- shiny::renderText(i18n()$t("ERROR: cannot determine shapefile") )
      }

    })
    #return temporary NULL as shape is being determined
    return(list(
      ffshape = shiny::reactive(r1$finalShape),
      confirm = shiny::reactive(r1$confirm),
      basemap = shiny::reactive(r1$basemap),
      basemap_bw = shiny::reactive(r1$basemap_bw),
      network = shiny::reactive(r1$pathNetwork),
      needHelp = shiny::reactive(r1$needHelp),
      DULN = shiny::reactive(r$DULN),
      DULN_all = shiny::reactive(r$DULN_all),
      datapath = shiny::reactive(r$datapath),
      currentLang = shiny::reactive(i18n()$get_translation_language())

    )
    )

  })

  }
