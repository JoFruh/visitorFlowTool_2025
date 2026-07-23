# source("R/polygonCreator.R", local = TRUE)
# source("R/polygonEraser.R", local = TRUE)
#
# source("polygonCreator.R", local = TRUE)
# source("polygonEraser.R", local = TRUE)

# Define server logic
step4_server <- function(id, network, minThresh, naturalAreas, confirm, i18n, currentLang, skip = FALSE,
                         needHelp = NULL, finalPolygons = NULL, DULN = NULL, DULN_all = NULL, shape = NULL){
  #prepare LETTERS that go beyond 26 (X, Y, Z, AA, AB etc..)
  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
  #shape is the submitted shapefile, or shape produced by submitted coordinates
  shiny::moduleServer(id, function(input, output, session) {

    #render banner image from start
    if(currentLang == "de"){
      output$bannerUI_4 <- shiny::renderUI({
        imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step4_wsl.png"), list() )
        #replace /" with ', to avoid problems
        return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
      })
    }else if(currentLang == "fr"){
      output$bannerUI_4 <- shiny::renderUI({
        imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step4_wsl_fr.png"), list() )
        #replace /" with ', to avoid problems
        return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
      })
    }

    r <- shiny::reactiveValues()
    r$DULN <- DULN
    r$DULN_all <- DULN_all
    r$needHelp <- needHelp
    r$startingPolygons <- NULL

    r$cutMarkerExists <- FALSE
    r$shapeWasClicked <- FALSE

    r$needHelp <- needHelp
    r$currentLang <- currentLang

    shiny.i18n::update_lang(r$currentLang)
    shiny::updateSelectInput(inputId = "languageSelect_4", selected = currentLang)


    #get lakes and add NAs in place of lakes
    #before extracting values, remove lakes (make them NA)
    wkt <- sf::st_as_text( sf::st_as_sfc(sf::st_transform(shape, "epsg:2056") ) )

    lakes <- sf::st_read( "www/data/maps/lakes.gdb",
                          query = 'SELECT * FROM "lakes"',
                          wkt_filter = wkt)
    lakes <- sf::st_transform(lakes[lakes$SHAPE_Area > 10000, ], "epsg:4326")

    #remove lakes from DULN raster (save seperately)
    r$DULN_na <- r$DULN$walkNat
    r$DULN_na[terra::vect(lakes)] <- NA
    #keep name
    names(r$DULN_na) <- "walkNat"

    r$promiseFinished <- NULL

    # if(r$needHelp == TRUE){
    #
    #   shinyjs::delay(1500,{
    #   shiny::showModal(
    #     shiny::modalDialog(footer = shiny::modalButton(label = "OK!"),
    #       h2(i18n()$t("Endgültige Festlegung von Zielgebiete")),
    #       div(style = "text-align:center",
    #           img(src = "www/goToAOI.png", style = "display:inline;height:150px")
    #       ),
    #       h4(shiny::HTML(i18n()$t("Menschen suchen <b> bestimmte Gebiete</b> auf, um sich <b>zu erholen</b> (<b>Zielgebiete</b>)."))),
    #       h3(),
    #       h4(shiny::HTML(i18n()$t("Hier können Sie die Zielgebiete <b>manuell korrigieren</b>."))),
    #       h3(),
    #       h4(shiny::HTML(i18n()$t("Dies geschieht auf die gleiche Weise wie in Schritt 1:"))),
    #       h5(shiny::HTML(i18n()$t("Sie klicken einfach auf die Karte und dann auf den roten Punkt, um die Korrektur vorzunehmen."))),
    #       h3(),
    #       h4(shiny::HTML(i18n()$t("<b>Ein paar Unterschiede:</b>"))),
    #       h4(shiny::HTML(i18n()$t("<b>1)</b> Sie können Zielgebiete entfernen, indem Sie sie anklicken."))),
    #       h4(shiny::HTML(i18n()$t("<b>2)</b> Sie können bestehende Zielgebiete <b>erweitern</b>, indem Sie ein <b>neues</b> Zielgebiet daüber hinaus ihnen erstellen."))),
    #       shiny::img(src = "www/combineAreas.png", style = "height:75px")
    #
    #
    #     )
    #   )
    #   })
    # }
    #prepare global variables (for polygon creation/editing)

    #### FUNCTIONS ####
    #CHANGED TO AVOID USING <<-
    #prepare global variables (for polygon creation/editing)
    # polygonsList <<- NULL
    # variables <- NULL
    # variables <<- list()
    # variables <- new.env(parent = emptyenv())
    # polygonsList <- new.env(parent = emptyenv())


    r$finalPolygons <- finalPolygons
    r$confirm <- NULL
    # finalPolygons <<- NULL


    r$newNetwork <- NULL
    # finalPolygons2 <- NULL

    plotMap <- function(){


      output$finalAOIMap <- leaflet::renderLeaflet({


        tmap::tmap_mode('view')

        #plot the initial map

        if(skip == TRUE){ #without starting polygons
          tmap::tmap_leaflet(
            tmap::tm_shape(network |> tidygraph::activate(edges) |> dplyr::as_tibble() |> sf::st_as_sf(), options = leaflet::pathOptions(pane = "layer1")) +
              tmap::tm_lines(col = "grey", lwd = 2, palette = c("grey"), popup.vars = FALSE,interactive = FALSE) 
              # +
              # tmap::tmap_options(basemaps = 'OpenStreetMap', basemap.alpha = c(0.5) )
          ) |>leaflet::addMapPane("layer1", zIndex = 410) |> leaflet::addMapPane("layer2", zIndex = 420) |>
          leaflet::addProviderTiles(
            leaflet::providers$OpenStreetMap,
            options = leaflet::providerTileOptions(noWrap = TRUE)
          )


        }else if(skip == FALSE){ #with starting polygons

          tmap::tmap_leaflet(
            tmap::tm_shape(network |> tidygraph::activate(edges) |> dplyr::as_tibble() |> sf::st_as_sf(), options = leaflet::pathOptions(pane = "layer1")) +
              tmap::tm_lines(col = "grey", lwd = 2, palette = c("grey"), popup.vars = FALSE,interactive = FALSE) 
              # +
              # tmap::tmap_options(basemaps = 'OpenStreetMap', basemap.alpha = c(0.5) )
          ) |> leaflet::addMapPane("layer1", zIndex = 410) |> leaflet::addMapPane("layer2", zIndex = 420) |>
            leaflet::addProviderTiles(
              leaflet::providers$OpenStreetMap,
              options = leaflet::providerTileOptions(noWrap = TRUE)
            ) |>
            leaflet::addGeoJSON(
            geojson = geojsonsf::sf_geojson(r$startingPolygons ),
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



      })

      #a little bit clunky:
      #generate polygons global container that is referenced within function.
      #TO DO: try to improve: either name container in input variables, include container in inputs (reactive programming complicates this)

      #
      # leafletProxy("finalAOIMap", deferUntilFlush = FALSE )|>
      #   clearGroup("eraseable")|>
      #   addGeoJSON(
      #     geojson = geojsonsf::sf_geojson(polygonsList),
      #     stroke = TRUE,
      #     weight = 5,
      #     color = "black",
      #     fill = TRUE,
      #     fillColor = "green",
      #     opacity = 1,
      #     group = "eraseable"
      #   )

      # polygons <- st_sfc(crs = 3857)  #use existing polygons generated by generateAOI above
      # polygonCreator("finalAOIMap",  input = input, startingPolygons = startingPolygons) #requires "polygons" global variable
      # polygonEraser("finalAOIMap", input = input, startingPolygons = startingPolygons)

      leafletMapID = "finalAOIMap"

      numberOfPolygons = "multi"

      finalPolygons2 <- NULL

      #### MAP CLICK FUNCTIONALITY ####
      #(copy content of PolygonCreator and PolygonErase functions)

      ##### PolygonCreator ####
      #populate global variable
      if(is.null(r$polygonsList) ){
        r$polygonsList <- r$startingPolygons
      }
      #container for all vertices of a polygon that is to be created
      mapPoints <-sf::st_sfc(crs = 4326) #empty list of generated sf points

      #variable to help avoid creating markers at the same time as a polygon is finalised by clicking on a marker.
      markerWasClicked <- FALSE

      #populate given list container with variables called by the observed reactives.
      #This allows an outside variable to communicate between leaflet map and the reactives of this function.
      r$mapPoints <- mapPoints
      r$markerWasClicked <- markerWasClicked

      mapMarkerClick <- paste0(leafletMapID, "_marker_click")
      mapClick <- paste0(leafletMapID, "_click")

      #CUT MODE turned on ####
      r$obsCutMode <- shiny::observeEvent(input$cutButton, {
        #reset everything
        r$cutMarkerExists <- FALSE
        r$markerWasClicked <- FALSE
        r$shapeWasClicked <- FALSE
        #remove points
        r$mapPoints <- sf::st_sfc(crs = 4326)

        #toggle border between cut mode ON and OFF
        if(input$cutButton == TRUE){
          shinyjs::removeClass(id = "mapFrame", "cutModeOff", asis = TRUE)

          shinyjs::addClass(id = "mapFrame", "cutModeOn", asis = TRUE)
        }else{
          shinyjs::removeClass(id = "mapFrame", "cutModeOn", asis = TRUE)

          shinyjs::addClass(id = "mapFrame", "cutModeOff", asis = TRUE)
        }

        proxy <- leaflet::leafletProxy(leafletMapID)|>
          leaflet::clearGroup("first")|>
          leaflet::clearGroup("after")|>
          leaflet::clearGroup("cut")
      }, ignoreInit = TRUE)

      r$obsMarkerClick <- shiny::observeEvent(input[[mapMarkerClick]], {
        print("MARKER CLICK")
        print(input[[mapMarkerClick]])
        r$markerWasClicked <- TRUE

        if(!is.null(input[[mapMarkerClick]]$group) ){#& r$step1Refreshing != TRUE
          #FINALISE POLYGON ####
          #If first vertex of polygon is clicked, Finalise polygon
          if( input[[mapMarkerClick]]$group == "first"){
            if(nrow(r$mapPoints) > 2){
              #create polygon with points
              poly <- sf::st_cast(sf::st_combine(r$mapPoints), "POLYGON")
              poly <- sf::st_sf(poly)
              # poly$DULN <- 1


              if(is.finite(max(r$polygonsList$id))){
                poly$id <- max(r$polygonsList$id)+1
              }else{
                poly$id <- 1
              }
              # poly <- concaveman(mapPoints, 1) Doesn't work well
              poly <- dplyr::rename(poly, polygons = "poly")

              if(numberOfPolygons == "multi"){
                #check if new polygon intersects or overlaps with any other
                intersectingPolys <- which(sf::st_intersects(poly, r$polygonsList, sparse = FALSE))
                if(length(intersectingPolys) > 0){
                  #if so, combine it into a single polygon
                  newPoly <- sf::st_as_sf(sf::st_union(c(poly$polygons, r$polygonsList[intersectingPolys,]$polygons) ) )
                  newPoly <- newPoly |> dplyr::rename(polygons = .data$x)
                  #determine its general attractivity (with popup)
                  newPoly$DULN <- 1
                  #determine id
                  newPoly$id <- max(r$polygonsList$id)+1
                  #remove intersecting polys
                  r$polygonsList <- r$polygonsList[-intersectingPolys,]
                  #add new polygon
                  # r$polygonsList[nrow(r$polygonsList) + 1, ] <- newPoly
                  poly <- newPoly
                }

                #generate DULN value for polygon on the fly
                values <- terra::extract(r$DULN_na, poly)

                values <- values |> dplyr::group_by(ID)|>dplyr::arrange(desc(walkNat))

                # meanFunc <- function(x){
                #   (median(x[1:(length(x)/4 )] , na.rm = TRUE)+
                #      median(x, na.rm = TRUE) ) / 2
                # }
                # meanValues <- values |> dplyr::group_by(ID)|> dplyr::summarise(mean = meanFunc(Nature_walk))
                # spltPoly$DULN <- meanValues$mean
                poly$DULN <- (median(values$walkNat[1:(length(values$walkNat)/4 )] , na.rm = TRUE)+
                                             median(values$walkNat, na.rm = TRUE) ) / 2


                # poly$DULN <- mean(values$Nature_walk, na.rm = TRUE) #[values$all > -20] no longer need to avoid values <= -20

                if(is.na(poly$DULN)){cat(file = stderr(), "WARNING step4: poly$DULN is NA after extraction\n")}

                 #add area to polygon
                poly$area <- as.numeric(sf::st_area(poly$polygons))

                #keep a table of polygons
                r$polygonsList <- rbind(r$polygonsList, poly)
              }else{
                #keep a single polygon
                r$polygonsList <- poly
              }

              r$polyFinished <- TRUE

              r$mapPoints <- sf::st_sfc(crs = 4326)
              print(r$polygonsList)
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



            }else{
              #TODO: write error (need more points)
            }
          }
        }else if(r$step1Refreshing == TRUE){
          r$step1Refreshing <- FALSE
        }
      }, ignoreInit = TRUE, ignoreNULL = TRUE)

      r$obsMapClick <- shiny::observeEvent(input[[mapClick]], {
        #precised condition (default always evaluates as TRUE)
        # if(  inputConditionName == "DEFAULT" | input[[inputConditionName]] %in% inputConditionValue){
        print("CLICK")
        print(input[[mapClick]])
        print(r$markerWasClicked)

        #if we're not in Polygon Cut mode
        if(input$cutButton == FALSE){
        if(!r$markerWasClicked){
          if( !is.null(input[[mapClick]]$lng) ){
            #clear shapes
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
        }else{
          #POLYGON CUT MODE ####
          #check if cross cut marker exists (if not, create it)
          if(r$cutMarkerExists == FALSE ){
            #only add cross if clicked outside a shape
            # if(r$shapeWasClicked == FALSE){
          r$cutPoints <-  sf::st_as_sf( sf::st_sfc( sf::st_point(x = c(input[[mapClick]]$lng, input[[mapClick]]$lat)), crs = 4326) )
          proxy = leaflet::leafletProxy(leafletMapID )

          crossMarker <- leaflet::addMarkers(map = proxy,
                                                    lng = input[[mapClick]]$lng, lat = input[[mapClick]]$lat,
                                                    group = "cut",
                                             icon = leaflet::icons(iconUrl = "www/cutCross.png", iconWidth = 10, iconHeight = 10),
                                                    options = leaflet::pathOptions(pane = "layer2"))
          r$cutMarkerExists <- TRUE
            # }
            # else{
            #   #reset shape click status
            #   r$shapeWasClicked <- FALSE
            # }

          }else if(r$cutMarkerExists == TRUE ){
            #if cut marker exists, this is second point of line

            #finalize line to cut polygon with
            #add point
            r$cutPoints <- rbind(r$cutPoints,sf::st_as_sf( sf::st_sfc( sf::st_point(x = c(input[[mapClick]]$lng, input[[mapClick]]$lat)), crs = 4326) ) )
            # create line and split polygon
            line <- sf::st_cast(sf::st_union(r$cutPoints[c(1,2),]), "LINESTRING")
            #detect intersecting polygon
            polyIndex <- as.numeric(sf::st_intersects(line, r$polygonsList)[[1]])

            #if second point is NOT within shape (split shape)
            #otherwise, make hole in it
            if( r$shapeWasClicked == FALSE){


              #if there is intersection, do next steps. Otherwise, ignore next steps
              if(length(polyIndex) > 0){

                if(all(!is.na(polyIndex))){
                  intrPoly <- r$polygonsList[polyIndex,]
                  #split polygon with line
                  spltPoly <- lwgeom::st_split(intrPoly, line)
                  spltPoly <- sf::st_collection_extract(spltPoly, "POLYGON")

                  #check if split occurred (more polys after than before), if not, make a hole instead
                  if(nrow(spltPoly) > nrow(intrPoly)){

                    #get new areas
                    spltPoly$id <- seq.int(from = max(r$polygonsList$id)+1, to = max(r$polygonsList$id) + length(spltPoly$id), by = 1)
                    #generate DULN value for polygon on the fly
                    values <- terra::extract(r$DULN_na, spltPoly)

                    #Determine AoI by giving importance to a sizeable portion of the most attractive area (1/4)
                    values <- values |> dplyr::group_by(ID)|>dplyr::arrange(desc(walkNat))
                    meanFunc <- function(x){
                      (median(x[1:(length(x)/4 )] , na.rm = TRUE)+
                         median(x, na.rm = TRUE) ) / 2
                    }
                    meanValues <- values |> dplyr::group_by(ID)|> dplyr::summarise(mean = meanFunc(walkNat))
                    spltPoly$DULN <- meanValues$mean #[values$all > -20] no longer need to avoid values <= -20

                    #add area to polygon
                    spltPoly$area <- as.numeric(sf::st_area(spltPoly$polygons))
                    #append new ones
                    r$polygonsList <- rbind(r$polygonsList, spltPoly)
                    #remove original polygon
                    r$polygonsList <- r$polygonsList[-polyIndex,]

                    #update map
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

                  }else{
                    # split had no results, try hole
                    # => make a hole along line
                    #buffer line
                    line_buff <-sf::st_transform(sf::st_buffer(sf::st_transform(line, "epsg:2056"), 10, endCapStyle = "FLAT" ), "epsg:4326")
                    #subtract from shape
                    newPoly <- sf::st_as_sf(sf::st_difference(r$polygonsList[polyIndex,], line_buff))
                    sf::st_geometry(newPoly) <- "polygons"
                    #replace polys with new polys
                    #get new areas
                    newPoly$id <- seq.int(from = max(r$polygonsList$id)+1, to = max(r$polygonsList$id) + nrow(newPoly), by = 1)
                    #generate DULN value for polygon on the fly
                    values <- terra::extract(r$DULN_na, newPoly)

                    #Determine AoI by giving importance to a sizeable portion of the most attractive area (1/4)
                    values <- values |> dplyr::group_by(ID)|>dplyr::arrange(desc(walkNat))
                    meanFunc <- function(x){
                      (median(x[1:(length(x)/4 )] , na.rm = TRUE)+
                         median(x, na.rm = TRUE) ) / 2
                    }
                    meanValues <- values |> dplyr::group_by(ID)|> dplyr::summarise(mean = meanFunc(walkNat))
                    newPoly$DULN <- meanValues$mean #[values$all > -20] no longer need to avoid values <= -20
                    #add area to polygon
                    newPoly$area <- as.numeric(sf::st_area(newPoly$polygons))
                    #append new ones
                    r$polygonsList <- rbind(r$polygonsList, newPoly)
                    #remove original polygon
                    r$polygonsList <- r$polygonsList[-polyIndex,]

                    #update map
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


                    #reset shape clicked
                    r$shapeWasClicked <- FALSE

                    leaflet::leafletProxy(leafletMapID) |>leaflet::clearGroup("cut")

                    #reset
                    r$cutMarkerExists <- FALSE
                    r$cutPoints <- NULL
                  }



                }
              }else{
                #no intersection, so do nothing

                #potentially check if shape can be cut
              }

              leaflet::leafletProxy(leafletMapID) |>leaflet::clearGroup("cut")


              #reset
              r$cutMarkerExists <- FALSE
              r$cutPoints <- NULL
            }else{
              # line ends inside a shape
              # => make a hole along line
              #buffer line
              line_buff <- sf::st_transform(sf::st_buffer(sf::st_transform(line, "epsg:2056"), 10, endCapStyle = "FLAT"), "epsg:4326")
              #subtract from shape
              newPoly <- sf::st_as_sf(sf::st_difference(r$polygonsList[polyIndex,], line_buff))
              sf::st_geometry(newPoly) <- "polygons"
              #replace polys with new polys
              #get new areas
              newPoly$id <- seq.int(from = max(r$polygonsList$id)+1, to = max(r$polygonsList$id) + nrow(newPoly), by = 1)
              #generate DULN value for polygon on the fly
              values <- terra::extract(r$DULN_na, newPoly)

              #Determine AoI by giving importance to a sizeable portion of the most attractive area (1/4)
              values <- values |> dplyr::group_by(ID)|>dplyr::arrange(desc(walkNat))
              meanFunc <- function(x){
                (median(x[1:(length(x)/4 )] , na.rm = TRUE)+
                   median(x, na.rm = TRUE) ) / 2
              }
              meanValues <- values |> dplyr::group_by(ID)|> dplyr::summarise(mean = meanFunc(walkNat))
              newPoly$DULN <- meanValues$mean #[values$all > -20] no longer need to avoid values <= -20
              #add area to polygon
              newPoly$area <- as.numeric(sf::st_area(newPoly$polygons))
              #append new ones
              r$polygonsList <- rbind(r$polygonsList, newPoly)
              #remove original polygon
              r$polygonsList <- r$polygonsList[-polyIndex,]

              #update map
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


              #reset shape clicked
              r$shapeWasClicked <- FALSE

              leaflet::leafletProxy(leafletMapID) |>leaflet::clearGroup("cut")

              #reset
              r$cutMarkerExists <- FALSE
              r$cutPoints <- NULL
            }
          }
          }



      }, ignoreInit = TRUE, ignoreNULL = FALSE)


      ##### PolygonEraser ####

      #populate global variable
      if(is.null(r$polygonsList) ){
        r$polygonsList <- r$startingPolygons
      }



      mapGeojsonClick <- paste0(leafletMapID, "_geojson_click")


      if(numberOfPolygons == "multi"){

        r$obsErase <- shiny::observeEvent(input[[mapGeojsonClick]], {
          print("SHAPE CLICK")
          print(input[[mapGeojsonClick]])

          r$shapeWasClicked <- TRUE

          # erase shape if clicked AND if mapoints aren't being put down AND polygon cut mode isn't ON
          if(input[[mapGeojsonClick]]$group == "eraseable" & is.null(nrow(r$mapPoints) ) & input$cutButton != TRUE  ){
            #update polygons
            r$polygonsList <- r$polygonsList[!r$polygonsList$id %in% input[[mapGeojsonClick]]$properties$id, ]

            #replot polygons
            leaflet::leafletProxy(leafletMapID )|>
              leaflet::clearGroup("eraseable")|>
              leaflet::addGeoJSON(
                geojson = geojsonsf::sf_geojson(r$polygonsList),
                stroke = TRUE,
                weight = 5,
                color = "black",
                fill = TRUE,
                fillColor = "green",
                opacity = 1,
                group = "eraseable",
                options = leaflet::pathOptions(pane = "layer2")
              )

            r$markerWasClicked <- TRUE

          }

        })

      }else{

        r$obsErase <- shiny::observeEvent(input[[paste0(leafletMapID, "_click")]], {
          print("GENERAL CLICK")

          #create variable if missing
          if(is.null(r$polyFinished) ){r$polyFinished <- FALSE}

          #when map is clicked but NO polygon was finalised
          if(r$polyFinished == FALSE){

            #and a polygon already exists
            if(!is.null(r$polygonsList)){

              #erase the existing polygon

              #update polygons
              r$polygonsList <- NULL



              #replot polygons
              leaflet::leafletProxy(leafletMapID )|>
                leaflet::clearGroup("eraseable")

            }

          }else{
            #reset global variable
            r$polyFinished <- FALSE
          }

        })

      }


      shiny::observeEvent(input$resetButton, {
        if(!is.null(r$startingPolygons)){
          r$polygonsList <- r$startingPolygons

          #replot polygons
          map <- leaflet::leafletProxy(leafletMapID )|>
            leaflet::clearGroup("eraseable")
          if(!is.null(nrow(r$startingPolygons)) ){
            map |> leaflet::addGeoJSON(
              geojson = geojsonsf::sf_geojson(r$startingPolygons),
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
      }, ignoreInit = TRUE, ignoreNULL = TRUE)







      ### OBSERVERS ####
      #dismiss Modal
      obs_dimissModal <- shiny::observeEvent(input$dismissModal, {
        shiny::removeModal()
      })

      #observe info Button ####
      obs_info4 <- shiny::observeEvent(input$infoButton4, {
        shiny::showModal(
          shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                             h2(i18n()$t("Zusätzliche Informationen:") ),
                             h3(),
                             h3(i18n()$t("Die hier festgelegten Zielgebiete werden verwendet, um für die Simulation zu definieren, wohin die Agenten gehen könnten, um sich zu erholen.") ),
                             h3(),
                             h4(i18n()$t("Jeder Agent wählt automatisch ein bestimmtes Zielgebiet, indem dessen Nähe, Attraktivität und Grösse berücksichtigt wird.") ),
                             h3(),
                             h4(i18n()$t("Wenn ein Agent nur wenig Zeit zur Verfügung hat, um sich neu zu erschaffen, wird der Agent dazu tendieren, ein Gebiet in der Nähe zu wählen.") ),
                             h3(),
                             h4(i18n()$t("Wenn ein Agent viel Zeit zur Erholung hat, wird er eher ein großes Gebiet wählen.") ),


          )
        )
      })

      #help observer####
      obs_help4 <- shiny::observeEvent(input$helpButton4, {
        shiny::showModal(
          shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                             h2(i18n()$t("Endgültige Festlegung von Zielgebiete")),
                             div(style = "text-align:center",
                                 img(src = "www/goToAOI.png", style = "display:inline;height:150px")
                             ),
                             h4(shiny::HTML(i18n()$t("Menschen suchen <b> bestimmte Gebiete</b> auf, um sich <b>zu erholen</b> (<b>Zielgebiete</b>)."))),
                             h3(),
                             h4(shiny::HTML(i18n()$t("Hier können Sie die Zielgebiete <b>manuell korrigieren</b>."))),
                             h3(),
                             h4(shiny::HTML(i18n()$t("Dies geschieht auf die gleiche Weise wie in Schritt 1:"))),
                             h5(shiny::HTML(i18n()$t("Sie klicken einfach auf die Karte und dann auf den roten Punkt, um die Korrektur vorzunehmen."))),
                             h3(),
                             h4(shiny::HTML(i18n()$t("<b>Drei Unterschiede:</b>"))),
                             h4(shiny::HTML(i18n()$t("<b>1)</b> Sie können Zielgebiete entfernen, indem Sie sie anklicken."))),
                             h4(shiny::HTML(i18n()$t("<b>2)</b> Sie können bestehende Zielgebiete <b>erweitern</b>, indem Sie ein <b>neues</b> Zielgebiet daüber hinaus ihnen erstellen."))),
                             shiny::img(src = "www/combineAreas.png", style = "height:75px"),
                             h4(shiny::HTML(i18n()$t("<b>3)</b> Sie können <b>Polygone ausschneiden</b>, indem Sie den <b>Polygonschnitt-Modus</b> aktivieren! (oben rechts)"))),
                             h5(shiny::HTML(i18n()$t("In diesem Modus macht ein <b>erster</b> Klick ein <b>Kreuz</b>, der <b>zweite Klick</b> zieht eine Linie vom Kreuz aus und <b>schneidet so entstandene Polygone</b>."))),

                             shiny::img(src = "www/cutClicks.png", style = "height:75px")


          )
        )
      })

      #Language Change ####
      langChangeObs <- observeEvent(input$languageSelect_4, {
        print("CHANGE LANGUAGE")
        if(input$languageSelect_4 == "de"){
          # i18n$set_translation_language('de')
          shiny.i18n::update_lang("de")
          i18n()$set_translation_language("de")
          print("DE")
          output$bannerUI_4 <- shiny::renderUI({
            imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step4_wsl.png"), list() )
            #replace /" with ', to avoid problems
            return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
          })



        }else if(input$languageSelect_4 == "fr"){
          # i18n$set_translation_language('fr')
          shiny.i18n::update_lang("fr")
          i18n()$set_translation_language("fr")

          output$bannerUI_4 <- shiny::renderUI({
            imgMap <- imageMap(NS(id, "banner"), "www/step4_wsl_fr.png", list() )
            #replace /" with ', to avoid problems
            return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
          })


          print("FR")
        }else if(input$languageSelect_4 == "en"){
          # i18n$set_translation_language('en')
          shiny.i18n::update_lang("en")
          output$bannerUI_4 <- shiny::renderUI({
            imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step4_wsl.png"), list() )
            #replace /" with ', to avoid problems
            return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
          })


          print("EN")
        }else if(input$languageSelect_4 == "it"){
          # i18n$set_translation_language('it')
          shiny.i18n::update_lang("it")
          output$bannerUI_4 <- shiny::renderUI({
            imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step4_wsl.png"), list() )
            #replace /" with ', to avoid problems
            return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
          })


          print("IT")
        }

      }, ignoreInit = TRUE)

      #observe banner click (choosing to step back in history)
      obsBanner <- observeEvent(input$banner,  {

        shinyjs::disable(id = "banner")

        print("MAPPED IMAGE CLICKED")
        #determine where to go back in history
        r$confirm <- input$banner



        obsConfirm$destroy()
        r$obsMapClick$destroy()
        r$obsMarkerClick$destroy()
        r$obsErase$destroy()
        obsBanner$destroy()

        r$finalPolygons <- NULL

        # shinyjs::enable("banner")

        return(list(finalPolygons = shiny::reactive({r$finalPolygons}), network = shiny::reactive(newNetwork), confirm = shiny::reactive({r$confirm}),  needHelp = shiny::reactive(r$needHelp),
                    parking = shiny::reactive(r$parking),
                    currentLang = shiny::reactive(i18n()$get_translation_language())) )

        #trigger return to past (return with specific confirm value?)
      }, ignoreInit = TRUE)


      obsConfirm <- shiny::observeEvent(input$confirmButton4, {
        #disable buttons temporarily
        shinyjs::disable("confirmButton4")
        shinyjs::disable("resetButton")

        #check if there are polygons, give a warning otherwise
        if(length(r$polygonsList) > 0){

        # PROMISE - POLYGONS AND PARKING ####

        #from reactive to normal variable
        finalPolygons <- r$polygonsList
        parking <- r$parking

        progress2 <- ipc::AsyncProgress$new(message = "Loading Parking information...",
                                            detail = paste0("Dies sollte weniger als ", 30, "Sekunden dauern"),
                                            queue = ipc::shinyQueue(),
                                            millis = 1000)
        future::future({
          print("CONFIRM5")
          cat(file = stderr(), "TEST1\n")
          # cat(file = stderr(), paste0("polygonEnv is made of : ", ls(polygonEnv)))


          # finalPolygons2 <- polygonEnv:polygonsList
          finalPolygons$AOI <- LETTERS702[1:nrow(finalPolygons)]

          vertices <- sf::st_as_sf(dplyr::as_tibble(tidygraph::activate(network, nodes)))
          vertices <- sf::st_transform(vertices, 4326)
          # node_points <- as_tibble( network_Tbl_allCH|>activate("nodes") )
          vertices_vect <- terra::vect(vertices)

          cat(file = stderr(), "TEST2")
          #sample raster with nodes
          cat(file = stderr(), paste0("finalPolygons are: ", str(finalPolygons)))
          cat(file = stderr(), paste0("x is: ", str(terra::vect(finalPolygons))))
          cat(file = stderr(), paste0("vertices_vect is: ", class(vertices_vect) ) )
          cat(file = stderr(), "TEST2b" )

          vertices_AOI_data <- terra::extract(terra::vect(finalPolygons["AOI"]), vertices_vect)
          cat(file = stderr(), paste0("vertices_AOI_data is: ", class(vertices_AOI_data) ) )

          #add node_DULN data to original nodes
          newvertices <- vertices
          newvertices$AOI <- vertices_AOI_data$AOI
          newvertices$AOI[is.na(newvertices$AOI)] <- 0



          cat(file = stderr(), "TEST3")
          #TODO:
          #somewhere here: for every polygon-extracted nodes, check if they form a single component, otherwise keep largest component
          #cycle through every AOI letter
          for(letter in LETTERS702[1:nrow(finalPolygons)]){
            #get nodes with this letter
            letternode <- newvertices$nodeID[newvertices$AOI == letter]
            #determine components
            subnetwork <- igraph::subgraph(network, letternode)
            comp <- igraph::components(subnetwork, "strong")

            #restored but potential ERROR, keep an eye out
            # as large AOIs would have only one part active
            #keep largest components as is
            #smaller components: replace letter with "0"
            smllerCompIDs <- igraph::V(subnetwork)$nodeID[comp$membership %in% which(comp$csize != max(comp$csize))]
            #apply a "0" to all vertices in smaller components
            newvertices$AOI[newvertices$nodeID %in% smllerCompIDs] <- "0"

            #get largest comp and add letter to neighbouring nodes (buffer)
            largestCompID <- igraph::V(subnetwork)$nodeID[comp$membership %in% which(comp$csize == max(comp$csize))]
            #assign current letter to all neighbhood nodes around current AOI (buffer zone)
            newvertices$AOI[unique(unlist(igraph::neighborhood(network, 1, nodes = largestCompID)))] <- letter
          }

          cat(file = stderr(), "TEST4")

          #add edge AOICol column for Debugging agent movement
          newedges <- sf::st_as_sf(dplyr::as_tibble(tidygraph::activate(network, edges)))
          newedges <- sf::st_transform(newedges, 4326)


          #DULN_WALK_ serves as DULN_ALL
          # Not using AOICol, removed
          # newedges$AOICol <- ifelse(newedges$DULN_WALK_ > minThresh, 1, 0)

          # #correct edge Table geometry name
          # newEdgesTbl <- as_tibble(newedges)
          # newEdgesTbl <- rename(newEdgesTbl, geometry = `_ogr_geometry_`)
          cat(file = stderr(), "TEST5")

          # finalPolygons2 <<- finalPolygons

          cat(file = stderr(), "TEST6")



          if(is.null(parking)){
            # progress <- shiny::Progress$new()
            # Make sure it closes when we exit this reactive, even if there's an error
            # on.exit(progress$close())

            #increment 1: loading paths
            progress2$inc(1/2, detail = "Loading parking info...")
            #LOAD / FILTER PARKING ####
            #filter out all parkings that are not in proximity to an AOI (further than 100m)
            wkt <- sf::st_as_text(sf::st_transform(sf::st_union( sf::st_buffer(sf::st_transform(finalPolygons, 2056), 100) ), "epsg:4326"  ))
            #retrieve parking areas and crop
            parking <- sf::st_read("www/data/maps/parking/parkingShapes.shp",
                                   query = 'SELECT * FROM "parkingShapes"',
                                   wkt_filter = wkt
            )
            if(!is.null(parking )){
              parking <-  parking |>
                dplyr::rename(polygons = .data$`_ogr_geometry_`) |>
                dplyr::select(.data$polygons)
              parking$id <- 1:nrow(parking)
              parking$isNew <- 0

              #PARKING DATA ####
              #similar to residential, but sampling parkingPolygons
              #populate nodes with 0s in $parking
              newvertices$parking <- 0
              newvertices$parkingAttr <- 0
              newvertices$newResidential <- 0

              #problematic to use filtered nodes, use all nodes instead? or buffered areas around parking
              filteredNodes <- sf::st_filter(newvertices, sf::st_buffer(parking, 50) )

              # Pre-compute all spatial relationships ONCE before the loop instead of once per iteration:
              # 1. Area / agentNb for every parking polygon (vectorised)
              polyAreas    <- as.numeric(sf::st_area(parking)) / 30
              # 2. Nearest AOI for every parking polygon (vectorised, single call)
              nearestAOIs  <- sf::st_nearest_feature(parking, finalPolygons)
              parkingAttrs <- finalPolygons$DULN[nearestAOIs]
              # 3. Which filteredNodes fall within each parking polygon (one spatial op for all polygons)
              nodesInParkings <- sf::st_contains(parking, filteredNodes, sparse = TRUE)

              progress2$inc(1 / 2, detail = "Determining parking potential...")

              #cycle through parking polygons
              for(polyNb in seq_len(nrow(parking))){

                nodeIndices <- nodesInParkings[[polyNb]]
                nodeCount   <- length(nodeIndices)
                agentNb     <- polyAreas[[polyNb]]
                nearestAttr <- parkingAttrs[[polyNb]]

                if(nodeCount > 0){
                  #add number of agents a parking can hold (per node within parking)
                  filteredNodes$parking[nodeIndices]    <- filteredNodes$parking[nodeIndices] + (agentNb / nodeCount)
                  filteredNodes$parkingAttr[nodeIndices] <- nearestAttr

                }else{
                  #CAPTURE EXCEPTION : no nodes in polygon
                  #in this case, find a single closest node outside polygon
                  nearestNodeIdx <- sf::st_nearest_feature(parking[polyNb,], sf::st_as_sf(filteredNodes))

                  if(!is.na(nearestNodeIdx)){
                    filteredNodes$parking[[nearestNodeIdx]]    <- filteredNodes$parking[[nearestNodeIdx]] + agentNb
                    filteredNodes$parkingAttr[[nearestNodeIdx]] <- nearestAttr
                  }
                }
              }
              #
              #transfer parking info back to network
              newvertices$parking[newvertices$nodeID %in% filteredNodes$nodeID] <- filteredNodes$parking
              newvertices$parkingAttr[newvertices$nodeID %in% filteredNodes$nodeID] <- filteredNodes$parkingAttr

            }
          }

          # DETERMINE ATTR WEIGHTED DISTANCES ####
          # determine new distances that are weighted by attractivity
          # ex: walkNat, walkNat_attr, walkNat_ATTR => slightly, moderately, heavily weighted by attractivity
          # bigger the attractivity, shorter the "distance".
          # 0 and negatives create problems, thus add min()+1

          #Removed min()+1, as these change weight of various DULNs.
          #Instead, added single value (min(all DULN)) to all DULNs at creation, to bring all values to positive

          #divide distance in a way that, attr values close to the minimum threshold (for AoIs), are halved for ATTR, reduced by 0.25 for attr, and reduced by 0.125 for distance
          #(minThresh has 24.11 added to avoid 0s and negatives in attractivity maps)

          xDist <- 8/(34.2423 + minThresh)
          xattr <- 4/(34.2423 + minThresh)
          xATTR <- 2/(34.2423 + minThresh)

          newedges$SHAPE_Leng_walkNat_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_WALK_ ))) #+ abs(min(newedges$DULN_WALK_)) + 1
          newedges$SHAPE_Leng_walkSoc_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_WALK1 )))
          newedges$SHAPE_Leng_dogNat_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_DOG_N )))
          newedges$SHAPE_Leng_dogProx_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_DOG_P )))
          newedges$SHAPE_Leng_ebikeNat_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_EBIKE)))
          newedges$SHAPE_Leng_bikeSport_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_BIKER )))
          newedges$SHAPE_Leng_jogger_ATTR <- newedges$SHAPE_Leng * (1-(1/(xATTR*newedges$DULN_JOGGE )))

          newedges$SHAPE_Leng_walkNat_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_WALK_ )))
          newedges$SHAPE_Leng_walkSoc_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_WALK1 )))
          newedges$SHAPE_Leng_dogNat_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_DOG_N )))
          newedges$SHAPE_Leng_dogProx_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_DOG_P )))
          newedges$SHAPE_Leng_ebikeNat_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_EBIKE )))
          newedges$SHAPE_Leng_bikeSport_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_BIKER )))
          newedges$SHAPE_Leng_jogger_attr <- newedges$SHAPE_Leng * (1-(1/(xattr*newedges$DULN_JOGGE )))

          newedges$SHAPE_Leng_walkNat <- newedges$SHAPE_Leng* (1-(1/(xDist*newedges$DULN_WALK_ )))
          newedges$SHAPE_Leng_walkSoc <- newedges$SHAPE_Leng* (1-(1/(xDist*newedges$DULN_WALK1 )))
          newedges$SHAPE_Leng_dogNat <- newedges$SHAPE_Leng* (1-(1/(xDist*newedges$DULN_DOG_N )))
          newedges$SHAPE_Leng_dogProx <- newedges$SHAPE_Leng* (1-(1/(xDist*newedges$DULN_DOG_P )))
          newedges$SHAPE_Leng_ebikeNat <- newedges$SHAPE_Leng* (1-(1/(xDist*newedges$DULN_EBIKE )))
          newedges$SHAPE_Leng_bikeSport <- newedges$SHAPE_Leng* (1-(1/(xDist*newedges$DULN_BIKER )))
          newedges$SHAPE_Leng_jogger <- newedges$SHAPE_Leng * (1-(1/(xDist*newedges$DULN_JOGGE )))

          #make sure max distance is not increased
          newedges$SHAPE_Leng_walkNat[newedges$SHAPE_Leng_walkNat > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_walkNat > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_walkSoc[newedges$SHAPE_Leng_walkSoc > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_walkSoc > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_dogNat[newedges$SHAPE_Leng_dogNat > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_dogNat > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_dogProx[newedges$SHAPE_Leng_dogProx > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_dogProx > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_ebikeNat[newedges$SHAPE_Leng_ebikeNat > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_ebikeNat > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_bikeSport[newedges$SHAPE_Leng_bikeSport > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_bikeSport > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_jogger[newedges$SHAPE_Leng_jogger > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_jogger > newedges$SHAPE_Leng]

          newedges$SHAPE_Leng_walkNat_attr[newedges$SHAPE_Leng_walkNat_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_walkNat_attr > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_walkSoc_attr[newedges$SHAPE_Leng_walkSoc_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_walkSoc_attr > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_dogNat_attr[newedges$SHAPE_Leng_dogNat_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_dogNat_attr > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_dogProx_attr[newedges$SHAPE_Leng_dogProx_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_dogProx_attr > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_ebikeNat_attr[newedges$SHAPE_Leng_ebikeNat_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_ebikeNat_attr > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_bikeSport_attr[newedges$SHAPE_Leng_bikeSport_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_bikeSport_attr > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_jogger_attr[newedges$SHAPE_Leng_jogger_attr > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_jogger_attr > newedges$SHAPE_Leng]

          newedges$SHAPE_Leng_walkNat_ATTR[newedges$SHAPE_Leng_walkNat_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_walkNat_ATTR > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_walkSoc_ATTR[newedges$SHAPE_Leng_walkSoc_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_walkSoc_ATTR > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_dogNat_ATTR[newedges$SHAPE_Leng_dogNat_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_dogNat_ATTR > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_dogProx_ATTR[newedges$SHAPE_Leng_dogProx_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_dogProx_ATTR > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_ebikeNat_ATTR[newedges$SHAPE_Leng_ebikeNat_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_ebikeNat_ATTR > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_bikeSport_ATTR[newedges$SHAPE_Leng_bikeSport_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_bikeSport_ATTR > newedges$SHAPE_Leng]
          newedges$SHAPE_Leng_jogger_ATTR[newedges$SHAPE_Leng_jogger_ATTR > newedges$SHAPE_Leng] <- newedges$SHAPE_Leng[newedges$SHAPE_Leng_jogger_ATTR > newedges$SHAPE_Leng]

          #make sure min distance is not 0 or negative
          newedges$SHAPE_Leng_walkNat[newedges$SHAPE_Leng_walkNat < 10] <- 10
          newedges$SHAPE_Leng_walkSoc[newedges$SHAPE_Leng_walkSoc < 10] <- 10
          newedges$SHAPE_Leng_dogNat[newedges$SHAPE_Leng_dogNat < 10] <- 10
          newedges$SHAPE_Leng_dogProx[newedges$SHAPE_Leng_dogProx < 10] <- 10
          newedges$SHAPE_Leng_ebikeNat[newedges$SHAPE_Leng_ebikeNat < 10] <-10
          newedges$SHAPE_Leng_bikeSport[newedges$SHAPE_Leng_bikeSport < 10] <- 10
          newedges$SHAPE_Leng_jogger[newedges$SHAPE_Leng_jogger < 10] <- 10

          newedges$SHAPE_Leng_walkNat_attr[newedges$SHAPE_Leng_walkNat_attr < 10] <- 10
          newedges$SHAPE_Leng_walkSoc_attr[newedges$SHAPE_Leng_walkSoc_attr < 10] <- 10
          newedges$SHAPE_Leng_dogNat_attr[newedges$SHAPE_Leng_dogNat_attr < 10] <- 10
          newedges$SHAPE_Leng_dogProx_attr[newedges$SHAPE_Leng_dogProx_attr < 10] <- 10
          newedges$SHAPE_Leng_ebikeNat_attr[newedges$SHAPE_Leng_ebikeNat_attr < 10] <- 10
          newedges$SHAPE_Leng_bikeSport_attr[newedges$SHAPE_Leng_bikeSport_attr < 10] <- 10
          newedges$SHAPE_Leng_jogger_attr[newedges$SHAPE_Leng_jogger_attr < 10] <- 10

          newedges$SHAPE_Leng_walkNat_ATTR[newedges$SHAPE_Leng_walkNat_ATTR < 10] <- 10
          newedges$SHAPE_Leng_walkSoc_ATTR[newedges$SHAPE_Leng_walkSoc_ATTR < 10] <-10
          newedges$SHAPE_Leng_dogNat_ATTR[newedges$SHAPE_Leng_dogNat_ATTR < 10] <-10
          newedges$SHAPE_Leng_dogProx_ATTR[newedges$SHAPE_Leng_dogProx_ATTR < 10] <- 10
          newedges$SHAPE_Leng_ebikeNat_ATTR[newedges$SHAPE_Leng_ebikeNat_ATTR < 10] <- 10
          newedges$SHAPE_Leng_bikeSport_ATTR[newedges$SHAPE_Leng_bikeSport_ATTR < 10] <-10
          newedges$SHAPE_Leng_jogger_ATTR[newedges$SHAPE_Leng_jogger_ATTR < 10] <-10

          #create tbl_graph
          newNetwork <- tidygraph::tbl_graph( nodes = dplyr::as_tibble(newvertices), edges = dplyr::as_tibble(newedges), directed = FALSE )
          # #try to give a buffer zone to nodes neighbouring AOIs
          # # select all nodes not == 0
          # # increase neighbourhood by 1
          # nbhd <- igraph::neighborhood(newNetwork[igraph::V(newNetwork)$AOI != "0",], 1)
          # # give them a recognizable character ex: '~'
          # igraph::N(nbhd)$AOI <- "~"

          newNetworkParkingFinalP <- list(newNetworkParking = list(newNetwork = newNetwork, parking = parking), finalPolygons = finalPolygons )

          progress2$close()

          newNetworkParkingFinalP
        }, seed = TRUE)%...>%(function(newNetworkParkingFinalP){

          shinyjs::reset("confirmButton4")
          shinyjs::enable("confirmButton4")
          shinyjs::enable("resetButton")

          r$confirm <- input$confirmButton4

          #cleanup
          obsConfirm$destroy()
          r$obsMapClick$destroy()
          r$obsMarkerClick$destroy()
          r$obsErase$destroy()
          obsBanner$destroy()

          #transfer network and parking back to local variables
          r$newNetwork <- newNetworkParkingFinalP$newNetworkParking$newNetwork
          r$parking <- newNetworkParkingFinalP$newNetworkParking$parking
          r$finalPolygons <- newNetworkParkingFinalP$finalPolygons
          return(list(finalPolygons = shiny::reactive({r$finalPolygons}), network = shiny::reactive(r$newNetwork), confirm = shiny::reactive({r$confirm}), needHelp = shiny::reactive(r$needHelp),
                      parking = shiny::reactive(r$parking),
                      currentLang = shiny::reactive(i18n()$get_translation_language())) )

        })
        }else{
          shinyjs::enable("confirmButton4")
          shinyjs::enable("resetButton")
          #if there are no polygons
          shiny::showModal(
            shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                               shiny::h3(shiny::HTML(as.character(i18n()$t("<font color=\'#dd1717\'><b>Keine Zielgebiete:</b></font>") ) ) ),
                               shiny::h4(shiny::HTML(as.character(i18n()$t("<font color=\'#dd1717\'>bitte mindestens ein Zielgebiet einzeichnen.") ) ) )

            )
          )
        }
      }, ignoreInit = TRUE)
    }

    # OBSERVER TO LAUNCH AFTER PROMISE FINALISES####
    observeEvent(r$promiseFinished, {

      print("LAUNCHING POST-PROMISE")
      plotMap()



    }, ignoreInit = TRUE)

    # observe paths download click ####
    obsDownloadAOI <- shiny::observeEvent(input$aoiButton, {

      shinyjs::click("downloadAOI", asis = FALSE)

    }, ignoreInit = TRUE)

    #PREPARE AOI DOWNLOAD ####
    output$downloadAOI <- shiny::downloadHandler(
      filename = function(){

        if(r$currentLang == "de"){
          name <- "visitorFlow_Zielgebiete.zip"
        }else if(r$currentLang == "fr"){
          name <- "visitorFlow_zonesCibles.zip"
        }else if(r$currentLang == "en"){
          name <- "visitorFlow_areasOfInterest.zip"
        }

        return(name)
      },
      content = function(file){

        # Create a dedicated temp folder with a clean name
        tmpDir <- tempfile(pattern = "AOI_download")
        dir.create(tmpDir)

        # Define clean file names inside that folder
        gpkgFile  <- file.path(tmpDir, "AOI.gpkg")
        txtFile  <- file.path(tmpDir, "INFO_AOI.txt")

        #put areas of interest
        # tempGDB_aoi <- tempfile(pattern = "zielgebiet_", fileext = ".gpkg")
        sf::st_write(r$polygonsList, dsn = gpkgFile , driver = "GPKG")

        #text info
        # tempTXT_info <- tempfile(pattern = "INFO_", fileext = ".txt")
        # fileConn<-file(tempTXT_info)
        writeLines(c("Information about AOI.",
                     "DULN field represents attractivity of the area",
                     "Area of surfaces is in m^2" ), txtFile)
        # close(fileConn)

        # Zip using relative paths by setting wd to tmpDir
        oldWd <- setwd(tmpDir)
        on.exit(setwd(oldWd), add = TRUE)  # always restore wd

        utils::zip(file, files = c("AOI.gpkg", "INFO_AOI.txt"))

        #zip both
        # utils::zip(file, c(tempGDB_aoi, tempTXT_info), flags = NULL)

      }
    )
    outputOptions(output, "downloadAOI", suspendWhenHidden = FALSE)


    #generate observer that launches immediately at start
    #if saved polygons exist, do not generate new ones
    if(is.null(r$finalPolygons ) ){
      if(skip == FALSE){

        if(is.null(naturalAreas[[1]])){

          DULN <- r$DULN
          DULN_all <- r$DULN_all

          observeEvent(NULL, {
            # LAUNCH PROMISE - generate AOIs ####
            progress1 <- ipc::AsyncProgress$new(message = "Generating areas of interest...",
                                                detail = paste0("Dies sollte weniger als ", 30, "Sekunden dauern"),
                                                queue = ipc::shinyQueue(),
                                                millis = 1000)



            DULN_wrapped <- terra::wrap(DULN)
            DULN_all_wrapped <- terra::wrap(DULN_all)

            # lake_path <- paste0(home, "/inst/app/www/data/maps/lakes.gdb")

            future::future({

              DULN <- terra::unwrap(DULN_wrapped)
              DULN_all <- terra::unwrap(DULN_all_wrapped)

              progress1$set(1/2)
              finalAOI <- generateAoI2(network, minThresh = minThresh, perimeter = shape,
                                       DULN = DULN, DULN_all = DULN_all) #, lake_path = lake_path
              progress1$set(2/2)
              progress1$close()
              finalAOI

            }, seed = TRUE) %...>% (function(finalAOI){


              #create a local version of the global variable to plot it
              #complications due to observe events being called by functions (TO DO: improve this coding)
              r$startingPolygons <- sf::st_transform(finalAOI, crs = 4326)

              if(is.null(r$startingPolygons$id) & !is.null(nrow(r$startingPolygons)) ){
                r$startingPolygons$id <- 1:nrow(r$startingPolygons)
              }
              r$polygonsList <- r$startingPolygons

              r$promiseFinished <- 1
              print("promise finished")

            })
          }, ignoreInit = FALSE, ignoreNULL = FALSE, once = TRUE)
        }else{
          #if natural areas were selected
          r$startingPolygons <- naturalAreas[[1]]
          #generate polygon ids
          if(is.null(r$startingPolygons$id) & !is.null(nrow(r$startingPolygons)) ){
            r$startingPolygons$id <- 1:nrow(r$startingPolygons)
          }

          r$polygonsList <- r$startingPolygons
          plotMap()
          print("promise finished")

        }







        #create global variable to contain polygons
        # polygonsList <<- st_transform(finalAOI, crs = 4326)

        # #give polygons ids if not already present
        # if(is.null(polygonsList$id) & !is.null(nrow(polygonsList)) ){
        #   polygonsList$id <<- 1:nrow(polygonsList)
        # }




      }else{
        r$startingPolygons <- sf::st_sfc(crs = 4326)
        r$polygonsList <- r$startingPolygons

        plotMap()
        print("promise finished")
        # polygonsList <<-  st_sfc(crs = 4326)
      }

    }else{
      r$startingPolygons <- r$finalPolygons
      plotMap()
      print("promise finished")

    }




    # finalPolygons2 <<- startingPolygons
    r$confirm <- input$confirmButton4
    return(list(finalPolygons = shiny::reactive({r$finalPolygons}), network = shiny::reactive(r$newNetwork), confirm = shiny::reactive({r$confirm}), needHelp = shiny::reactive(r$needHelp),
                parking = shiny::reactive(r$parking),
                currentLang = shiny::reactive(i18n()$get_translation_language())) )

  })
}
