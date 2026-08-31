# source("R/polygonCreator.R", local = TRUE)
# source("R/polygonEraser.R", local = TRUE)
#
# source("polygonCreator.R", local = TRUE)
# source("polygonEraser.R", local = TRUE)

# Define server logic
#' CONVERTED TO A FIRST-TOUCH SINGLETON (Stage 5, second module).
#'
#' Every argument except `id` and `i18n` is now a REACTIVE, and none of them is
#' read directly by the body. enter() snapshots them into locals of the same
#' names, so the ~1300 lines below are unchanged and still see plain values -
#' which is what they want: a visit works against a fixed network and a fixed
#' perimeter, and it is only BETWEEN visits that those may change.
#'
#' That distinction is the whole bug this conversion fixes. The module used to be
#' rebuilt per visit, so the snapshot was taken by the constructor - and the
#' PREVIOUS instance stayed alive holding its own, older snapshot, with its own
#' live confirm observer. Both answered the same click; the older one wrote the
#' network it had frozen before the user changed the area of interest back into
#' r$, which is where step 5's two "Original" scenarios came from.
#'
#' The map-interaction observers were already per-visit - plotMap() creates them
#' and the confirm handler destroys them. Two of them (banner, confirm) lived in
#' plotMap()'s own frame and so were unreachable from outside it, which was fine
#' while every visit got a fresh frame and is a leak now. They are on `r`
#' alongside the other three, and enter() clears all five before plotMap() makes
#' new ones - the user can leave this step by the nav bar without confirming, and
#' then nothing would have destroyed them.
#' `network` is gone from this signature. This step never read it: the map
#' stopped drawing the paths as a grey backdrop, and generateAoI2() took it as an
#' argument without ever touching it. Asking for it made opening step 4 dispatch
#' the ~30s path-network job - see the note on step4's `needs` in R/steps.R.
step4_server <- function(id, minThresh, i18n, currentLang,
                         skip = shiny::reactive(FALSE),
                         needHelp = shiny::reactive(NULL),
                         finalPolygons = shiny::reactive(NULL),
                         DULN = shiny::reactive(NULL),
                         DULN_all = shiny::reactive(NULL),
                         shape = shiny::reactive(NULL)){

  #count this instantiation. A module server should be created once per
  #session; this app re-calls it from an observeEvent on a trigger, so any
  #count above 1 means a duplicate set of observers and outputs is now live
  #alongside the previous one. See vftModuleInstance() in perf_helpers.R.
  vftModuleInstance("step4")
  #prepare LETTERS that go beyond 26 (X, Y, Z, AA, AB etc..)
  LETTERS702 <- c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))
  #shape is the submitted shapefile, or shape produced by submitted coordinates
  #The reactives, held under different names so that the locals below can shadow
  #them. Everything after this point reads plain values.
  .rx <- list(minThresh = minThresh, currentLang = currentLang,
              skip = skip, needHelp = needHelp, finalPolygons = finalPolygons,
              DULN = DULN, DULN_all = DULN_all, shape = shape)

  shiny::moduleServer(id, function(input, output, session) {

    #per-visit snapshots. enter() refills these; the body and every closure in it
    #resolve them lexically from here, so nothing else in this file changes.
    minThresh     <- NULL
    currentLang   <- NULL
    skip          <- FALSE
    needHelp      <- NULL
    finalPolygons <- NULL
    DULN          <- NULL
    DULN_all      <- NULL
    shape         <- NULL

    r <- shiny::reactiveValues()

    #what the lakes cutout was last computed for. sf::st_read() of the lakes GDB
    #is main-thread I/O, so it runs when the perimeter CHANGES, not once per
    #visit - coming back to adjust a polygon must not re-read it.
    cache <- new.env(parent = emptyenv())
    cache$shape <- NULL

    #' The walkNat band with the lakes cut out of it, built on FIRST EDIT.
    #'
    #' This is what the four manual handlers below score a hand-drawn, split or
    #' holed polygon against. It used to be built in enter(), which meant every
    #' ARRIVAL at this step paid a read of lakes.gdb plus a rasterize on the
    #' shared main thread - before the map was drawn, for a raster that is only
    #' touched if the user goes on to edit something.
    #'
    #' On the normal path this body never runs at all: generateAoI2() has to mask
    #' the lakes to score the areas it generates, so it hands the raster back
    #' from the worker and the promise handler in .vftStep4Launch() fills the
    #' cache. What is left here is the skip branch and the replay branch, where
    #' no generation happened and there is nothing to inherit - and there the
    #' cost lands on the first click rather than on the way in.
    #'
    #' Keyed on `shape`, not on the visit: a different perimeter is a different
    #' cutout, the same perimeter re-entered is not.
    .vftDULNna <- function(){
      if(is.null(cache$DULN_na) || !identical(cache$shape, shape)){
        if(is.null(shape) || is.null(DULN)) return(NULL)

        #get lakes and add NAs in place of lakes
        #before extracting values, remove lakes (make them NA)
        wkt <- sf::st_as_text( sf::st_as_sfc(sf::st_transform(shape, "epsg:2056") ) )

        lakes <- sf::st_read( vftData("maps/lakes.gdb"),
                              query = 'SELECT * FROM "lakes"',
                              wkt_filter = wkt)
        lakes <- sf::st_transform(lakes[lakes$SHAPE_Area > 10000, ], "epsg:4326")

        #remove lakes from DULN raster (save seperately)
        DULN_na <- DULN$walkNat
        if(nrow(lakes) > 0) DULN_na[terra::vect(lakes)] <- NA
        #keep name
        names(DULN_na) <- "walkNat"

        cache$shape   <- shape
        cache$DULN_na <- DULN_na
      }
      cache$DULN_na
    }

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


    # (r$finalPolygons and r$confirm are seeded by enter(), at the
    #  bottom of this function, so that a return to this step starts from the
    #  same state a first arrival does.)

    #### Session-lifetime observers ####
    #
    #These five used to be created INSIDE plotMap(), which runs once per visit
    #and again on every r$promiseFinished. enter() destroys the five
    #map-interaction observers before plotMap() makes new ones, but these were
    #never on that list, so every visit stacked another live copy on top of the
    #last: two help modals for one click, two language switches for one
    #selection, and a reset button that redrew the map as many times as the step
    #had been entered.
    #
    #None of them holds per-visit state - they read `r` and this module's own
    #inputs - so they belong out here, created once for the session. Same
    #reasoning as the note on the missing $destroy() calls in R/step3_server.R.
    #
    #r$obsCutMode stays inside plotMap(): that one IS per-visit, because it
    #clears leaflet groups the map has just re-created, and enter() destroys it
    #by name.

    #the id of the one map this module draws. Hoisted out of plotMap() so the
    #reset observer below can still reach it.
    leafletMapID <- "finalAOIMap"

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
      vftDbg("CHANGE LANGUAGE")
      if(input$languageSelect_4 == "de"){
        # i18n$set_translation_language('de')
        shiny.i18n::update_lang("de")
        i18n()$set_translation_language("de")
        vftDbg("DE")
        vftSetBanner(id, "www/step4_wsl.png")



      }else if(input$languageSelect_4 == "fr"){
        # i18n$set_translation_language('fr')
        shiny.i18n::update_lang("fr")
        i18n()$set_translation_language("fr")

        vftSetBanner(id, "www/step4_wsl_fr.png")


        vftDbg("FR")
      }else if(input$languageSelect_4 == "en"){
        # i18n$set_translation_language('en')
        shiny.i18n::update_lang("en")
        vftSetBanner(id, "www/step4_wsl.png")


        vftDbg("EN")
      }else if(input$languageSelect_4 == "it"){
        # i18n$set_translation_language('it')
        shiny.i18n::update_lang("it")
        vftSetBanner(id, "www/step4_wsl.png")


        vftDbg("IT")
      }

    }, ignoreInit = TRUE)

    plotMap <- function(){


      #vftTimeRender, not vftTime inside the block: the JSON serialisation of
      #whatever this returns happens AFTER the block returns, inside the
      #function renderLeaflet produces. A label placed inside would miss it.
      #Reported as user-visible stall right after Confirm in step 4.
      #
      #This used to also draw the whole path network as a grey backdrop (first
      #via tmap::tmap_leaflet()/tm_lines(), later via vftAddNetworkLines()'s
      #WebGL layer) - but step 4 only edits AOI polygons and never reads the
      #network to do it; the backdrop was decorative. The network stays reserved
      #for step 5 and newVersions, where it's actually the data being shown, and
      #dropping it here removes both the tmap/SVG cost AND the sf conversion of
      #the whole edge table that fed it - the map now has nothing to wait on but
      #the AOI polygons themselves.
      output$finalAOIMap <- vftTimeRender("step4:finalAOIMap", leaflet::renderLeaflet({

        map <- leaflet::leaflet() |>
          leaflet::addMapPane("layer2", zIndex = 420) |>
          leaflet::addProviderTiles(
            leaflet::providers$OpenStreetMap,
            options = leaflet::providerTileOptions(noWrap = TRUE)
          )

        #dropping the network also dropped the view it used to imply: tm_shape(network)
        #made tmap fit the map to the network's own extent, so removing that layer left
        #leaflet with nothing to size the view on and it defaulted to the whole world.
        #Fit to the perimeter instead, padded by 15% a side so the outline (and any AOI
        #polygons, which live inside it) isn't flush against the edge of the map.
        if(!is.null(shape)){
          outline <- sf::st_transform(shape, "epsg:4326")
          bb <- sf::st_bbox(outline)
          padX <- max((bb[["xmax"]] - bb[["xmin"]]) * 0.15, 0.01)
          padY <- max((bb[["ymax"]] - bb[["ymin"]]) * 0.15, 0.01)
          map <- map |> leaflet::fitBounds(lng1 = bb[["xmin"]] - padX, lat1 = bb[["ymin"]] - padY,
                                           lng2 = bb[["xmax"]] + padX, lat2 = bb[["ymax"]] + padY)
        }

        #plot the initial map
        if(skip == FALSE){ #with starting polygons
          map <- map |>
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

        map

      }))

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

      #(leafletMapID is at module scope now - the reset observer moved out there
      #and needs it too.)

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
        vftDbg("MARKER CLICK")
        vftDbg(input[[mapMarkerClick]])
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
                values <- terra::extract(.vftDULNna(), poly)

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

                if(is.na(poly$DULN)){vftDbgCat("WARNING step4: poly$DULN is NA after extraction\n")}

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
              vftDbg(r$polygonsList)
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
        vftDbg("CLICK")
        vftDbg(input[[mapClick]])
        vftDbg(r$markerWasClicked)

        #if we're not in Polygon Cut mode
        if(input$cutButton == FALSE){
        if(!r$markerWasClicked){
          if( !is.null(input[[mapClick]]$lng) ){
            #clear shapes
            r$mapPoints <- rbind(r$mapPoints,sf::st_as_sf( sf::st_sfc( sf::st_point(x = c(input[[mapClick]]$lng, input[[mapClick]]$lat)), crs = 4326) ) )
            #draw points
            vftDbg(r$mapPoints)

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
                    values <- terra::extract(.vftDULNna(), spltPoly)

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
                    values <- terra::extract(.vftDULNna(), newPoly)

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
              values <- terra::extract(.vftDULNna(), newPoly)

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
          vftDbg("SHAPE CLICK")
          vftDbg(input[[mapGeojsonClick]])

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
          vftDbg("GENERAL CLICK")

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



      #observe banner click (choosing to step back in history)
      r$obsBanner <- observeEvent(input$banner,  {

        shinyjs::disable(id = "banner")

        vftDbg("MAPPED IMAGE CLICKED")
        #determine where to go back in history
        r$confirm <- input$banner



        r$obsConfirm$destroy()
        r$obsMapClick$destroy()
        r$obsMarkerClick$destroy()
        r$obsErase$destroy()
        r$obsBanner$destroy()

        r$finalPolygons <- NULL

        # shinyjs::enable("banner")

        return(list(finalPolygons = shiny::reactive({r$finalPolygons}), confirm = shiny::reactive({r$confirm}),  needHelp = shiny::reactive(r$needHelp),
                    currentLang = shiny::reactive(i18n()$get_translation_language())) )

        #trigger return to past (return with specific confirm value?)
      }, ignoreInit = TRUE)


      r$obsConfirm <- shiny::observeEvent(input$confirmButton4, {
        #disable buttons temporarily
        shinyjs::disable("confirmButton4")
        shinyjs::disable("resetButton")

        #check if there are polygons, give a warning otherwise
        if(length(r$polygonsList) > 0){

        # CONFIRM ####
        #
        #This used to be a ~300 line future: AOI letters onto every node, the
        #parking shapefile read and distributed across nodes, seventy
        #attractivity-weighted edge columns and a rebuilt tbl_graph - about
        #thirty seconds, behind a progress bar reading "Loading Parking
        #information...", at the end of the step whose job is drawing polygons.
        #
        #It has moved to vftPrepareNetwork() in R/prepare_network.R and now runs
        #when a simulation is first launched (or when the newVersions page needs
        #it), because that is where the first READ of any of it happens. See the
        #note at the top of that file.
        #
        #What is left is what step 4 actually produces.

        #from reactive to normal variable
        finalPolygons <- r$polygonsList

        #The one line of the old job that stayed. It is
        #LETTERS702[1:nrow(finalPolygons)] - deterministic, and microseconds -
        #while determineAgentCharacteristics() reads finalPolygons$AOI beside
        #$DULN and $area. Keeping it here means r$finalPolygons has exactly the
        #shape every consumer already expects, and vftPrepareNetwork() can take
        #the letters as given rather than having to agree on them separately.
        finalPolygons$AOI <- LETTERS702[1:nrow(finalPolygons)]

        r$finalPolygons <- finalPolygons

        #The five destroy() calls that used to sit here are gone. They were
        #there because confirming meant leaving for good and a REBUILT module
        #would have stacked a second set of map handlers on the live one;
        #enter() has torn them down on every visit since Stage 5, so this copy
        #was redundant - and it was actively harmful now that the confirm can
        #be answered with "cancel": tearing the step down before app_server has
        #decided whether the write goes ahead would leave a cancelling user on
        #a frozen map they could neither edit nor confirm again.
        shinyjs::reset("confirmButton4")
        shinyjs::enable("confirmButton4")
        shinyjs::enable("resetButton")

        r$confirm <- input$confirmButton4

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

      vftDbg("LAUNCHING POST-PROMISE")
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


    #### enter(): everything that happens per VISIT rather than per session ####
    #
    # Called by vftGoToStep() on every return to this step, and once at the very
    # bottom of this function so that construction and re-entry run the same code
    # rather than two copies of it.
    #
    # vftModuleEnterFn() supplies what this body cannot state for itself: the
    # module's session as the default reactive domain - without it the
    # shinyjs::enable() calls in part 4 and the updateSelectInput() in part 3 send
    # unnamespaced ids and do nothing at all - and isolate() around everything,
    # because the provider observe() this is called from is not isolated and a
    # bare .rx$DULN() read would make it re-enter step 4 forever. R/modules.R.
    enter <- vftModuleEnterFn(session, function(){

      #--- 1. refresh the snapshots the rest of this module reads
      minThresh     <<- .rx$minThresh()
      currentLang   <<- .rx$currentLang()
      skip          <<- isTRUE(as.logical(.rx$skip()))
      needHelp      <<- .rx$needHelp()
      finalPolygons <<- .rx$finalPolygons()
      DULN          <<- .rx$DULN()
      DULN_all      <<- .rx$DULN_all()
      shape         <<- .rx$shape()

      #--- 2. tear down the previous visit's map interaction observers.
      #The confirm and banner handlers destroy these on the way out, but leaving
      #by the nav bar does not go through either of them - and plotMap() below is
      #about to create a fresh set. Without this, a second visit would leave two
      #marker-click handlers live and a single click would draw two polygons.
      #
      #r$obsCutMode is on this list now. It was created by plotMap() alongside
      #the other five and never destroyed, so cut mode was toggled once per
      #visit ever made - each stale copy clearing the "first"/"after"/"cut"
      #groups of a map it no longer had anything to do with. The four modal and
      #language observers that had the same problem are not here because they
      #have moved OUT of plotMap() to module scope, where nothing recreates
      #them; see the note there.
      for(o in list(r$obsConfirm, r$obsBanner, r$obsMapClick,
                    r$obsMarkerClick, r$obsErase, r$obsCutMode)){
        if(!is.null(o)) try(o$destroy(), silent = TRUE)
      }
      r$obsConfirm <- NULL; r$obsBanner <- NULL; r$obsMapClick <- NULL
      r$obsMarkerClick <- NULL; r$obsErase <- NULL; r$obsCutMode <- NULL

      #--- 3. banner and language
      if(identical(currentLang, "de")){
        vftSetBanner(id, "www/step4_wsl.png")
      }else if(identical(currentLang, "fr")){
        vftSetBanner(id, "www/step4_wsl_fr.png")
      }
      shiny.i18n::update_lang(currentLang)
      shiny::updateSelectInput(inputId = "languageSelect_4", selected = currentLang)

      #--- 4. this visit's state
      r$DULN             <- DULN
      r$DULN_all         <- DULN_all
      r$needHelp         <- needHelp
      r$currentLang      <- currentLang
      r$startingPolygons <- NULL
      r$cutMarkerExists  <- FALSE
      r$shapeWasClicked  <- FALSE
      r$promiseFinished  <- NULL
      r$finalPolygons    <- finalPolygons
      r$confirm          <- NULL

      #the buttons the confirm handler disabled on the way out
      shinyjs::enable("confirmButton4")
      shinyjs::enable("resetButton")
      shinyjs::enable(id = "banner")

      #--- 5. (the lakes cutout used to be built here, on the main thread, on the
      #way in to this step. It is built by .vftDULNna() on first EDIT now - see
      #the note there.)

      #--- 6. draw the map, generating the areas of interest first if needed
      .vftStep4Launch()
      invisible(NULL)
    })

    #generate observer that launches immediately at start
    #if saved polygons exist, do not generate new ones
    .vftStep4Launch <- function(){
    if(is.null(shiny::isolate(r$finalPolygons)) ){

      #Nothing has been CONFIRMED yet - but that does not mean there is nothing
      #on screen. A user who draws three areas, walks back to re-read step 3 and
      #returns must find their three areas, not a fresh generation on top of
      #them: entering a step is not supposed to destroy anything (see
      #vftCommit() in R/providers.R). So the working set is kept whenever it was
      #generated for exactly the inputs still in force.
      #
      #`aoiKey` is what those inputs are. When step 3 confirms a new threshold
      #it invalidates finalPolygons AND the key stops matching, so the areas are
      #regenerated - which is the case the guard must not swallow.
      aoiKey <- list(minThresh = minThresh, skip = skip, shape = shape)
      if(!is.null(shiny::isolate(r$polygonsList)) &&
         identical(cache$aoiKey, aoiKey)){
        r$startingPolygons <- shiny::isolate(r$polygonsList)
        plotMap()
        return(invisible(NULL))
      }
      cache$aoiKey <- aoiKey

      if(skip == FALSE){

        #naturalAreas was always NULL - step 3 set it to NULL and never
        #populated it - so the "natural areas were selected" branch that used
        #to sit opposite this one could never be reached, and is gone with it.

        DULN <- r$DULN
        DULN_all <- r$DULN_all

        observeEvent(NULL, {
          # LAUNCH PROMISE - generate AOIs ####
          #vftProgress, not ipc::AsyncProgress: 117 MB of session state was
          #crossing into the worker from this site. See R/async_helpers.R.
          progress1 <- vftProgress(message = "Generating areas of interest...",
                                   detail = paste0("Dies sollte weniger als ", 30, "Sekunden dauern"),
                                   queue = ipc::shinyQueue(),
                                   millis = 1000)



          #ONE BAND, not seven. generateAoI2() reads walkNat and nothing else, so
          #wrapping the whole DULN raster serialised six bands on this thread,
          #pushed them through mirai's transport and unwrapped them in the worker
          #for nobody. `DULN` itself stays seven bands - sf_to_tidygraph3() and
          #the newVersions page do read the others.
          walkNat_wrapped  <- terra::wrap(DULN$walkNat)
          DULN_all_wrapped <- terra::wrap(DULN_all)

          # lake_path <- paste0(home, "/inst/app/www/data/maps/lakes.gdb")

          vftFuture({

            walkNat  <- terra::unwrap(walkNat_wrapped)
            DULN_all <- terra::unwrap(DULN_all_wrapped)

            progress1$set(1/2)
            finalAOI <- generateAoI2(minThresh = minThresh, perimeter = shape,
                                     walkNat = walkNat, DULN_all = DULN_all) #, lake_path = lake_path
            progress1$set(2/2)
            progress1$close()
            finalAOI

          }, seed = TRUE, progress = progress1) %...>% (function(finalAOI){


            #create a local version of the global variable to plot it
            #complications due to observe events being called by functions (TO DO: improve this coding)
            #
            #No st_transform here any more. Both providers crop in EPSG:4326 and
            #generateAoI2() now returns lon/lat explicitly, so this call walked
            #every vertex of every polygon to produce the same coordinates back.
            r$startingPolygons <- finalAOI$polygons

            if(is.null(r$startingPolygons$id) & !is.null(nrow(r$startingPolygons)) ){
              r$startingPolygons$id <- 1:nrow(r$startingPolygons)
            }
            r$polygonsList <- r$startingPolygons

            #the lakes cutout, for free. generateAoI2() has to mask the lakes out
            #of walkNat to score the polygons, so the raster the manual
            #draw/cut/hole handlers want is already built - in the worker, off
            #this thread. Filling the cache here is what makes .vftDULNna()
            #below a no-op on the normal path.
            cache$shape   <- shape
            cache$DULN_na <- terra::unwrap(finalAOI$walkNatNoLakes)

            r$promiseFinished <- 1
            vftDbg("promise finished")

          })%...!%(vftAsyncError(progress1, "Areas of interest", NULL))
        }, ignoreInit = FALSE, ignoreNULL = FALSE, once = TRUE)







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
        vftDbg("promise finished")
        # polygonsList <<-  st_sfc(crs = 4326)
      }

    }else{
      r$startingPolygons <- shiny::isolate(r$finalPolygons)
      plotMap()
      vftDbg("promise finished")

    }
    invisible(NULL)
    }

    enter()

    #`network` and `parking` are gone from this list. They were the output of the
    #job that has moved to R/prepare_network.R, and the app-level r$network is
    #the step-1 provider's key - step 4 was overwriting it with an AOI-annotated
    #copy that nothing but the simulation ever wanted.
    return(list(finalPolygons = shiny::reactive({r$finalPolygons}), confirm = shiny::reactive({r$confirm}), needHelp = shiny::reactive(r$needHelp),
                currentLang = shiny::reactive(i18n()$get_translation_language()),
                enter = enter) )

  })
}
