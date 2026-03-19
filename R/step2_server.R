
# Define server logic
step2_server <- function(id, fshape, confirm){

  #assigning input value to reactive Values (This is to allow it to be triggered from program)
  confirmVal <- shiny::reactiveVal(0)

  #shape is the submitted shapefile, or shape produced by submitted coordinates
  shiny::moduleServer(id, function(input, output, session) {

    print("STEP 2 SERVER")

    envStep2 <- new.env(parent = emptyenv())
    envStep2$shape <- NULL
    envStep2$pathNetwork <- NULL

    #reactives



    output$plotPaths <- leaflet::renderLeaflet({

      #Make Progressbar
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      progress$set(message = "Calculating Path Network", value = 0)

      #increment 1: loading paths
      progress$inc(1/4, detail = "Loading paths from selected area")

      print("USE SHAPE FROM STEP 1")

      # use fshape supplied by server1
      # make a sf object containing a polygon with CRS WGS84 projection
      shp <- sf::st_as_sf(fshape(), coords = c("long", "lat"), crs = sf::st_crs(4326))
      shp <- sf::st_combine(shp)
      shp <- sf::st_cast(shp, "POLYGON")

      #transform to web Mercator
      shp <- sf::st_transform(shp, crs = "EPSG:3857")

      envStep2$shape <- shp
      #place buffer around polygon for some margin
      shape_larger <- sf::st_buffer(shp, dist = 1000)

      #use shape to prepare spatial filter as wkt (well-known text), grow slightly for buffer
      wkt <- sf::st_as_text( shape_larger )

      #extract relevant foot paths
      loadedPaths <- sf::st_read("www/data/maps/paths/paths_DULN_final2.shp",
                             query = 'SELECT * FROM "paths_DULN_final2"',
                             wkt_filter = wkt
                             )


      # loadedPaths$DULN <- as.double(loadedPaths$DULN_fin_1)



      #project
      #loadedPaths <- st_transform(loadedPaths, crs = "EPSG:2056")

      #increment 2: cleaning paths
      #progress$inc(2/4, detail = "Retrieving paths in given location...")

      #cleanedPaths <- cleanPaths(loadedPaths)

      #increment 3: cleaning paths
      progress$inc(3/4, detail = "Creating Paths Network...")

      #use function to prepare node-edge table
      envStep2$pathNetwork <- sf_to_tidygraph(loadedPaths, shape_larger, directed = FALSE)


      #increment 3: cleaning paths
      progress$inc(4/4, detail = "Generating interative map...")


      shinyjs::show(id = "confirmButton2")

      #interactive map
      tmap::tmap_mode('view')

      leaflet::leafletOptions(doubleClickZoom= FALSE)

      tmap::tmap_leaflet(
        tmap::tm_shape(envStep2$pathNetwork %>% tidygraph::activate(edges) %>% dplyr::as_tibble() %>% sf::st_as_sf() ) +
          tmap::tm_lines(col = "DULN_final", lwd = 2, style = "fixed", breaks = c(0,5, 11),palette = c("black", "green4"), labels = c("not attractive", "attractive")) +
          tmap::tm_shape(envStep2$pathNetwork %>% tidygraph::activate(nodes) %>% dplyr::as_tibble() %>% sf::st_as_sf()) +
          tmap::tm_dots(size = 0.05, col = "white") +
          tmap::tmap_options(basemaps = 'OpenStreetMap', basemaps.alpha = c(0.5) ),
        options = leaflet::leafletOptions(doubleClickZoom = FALSE)
      )


      })

    ### POLYGON CREATION VARIABLES,  METHODS AND REACTIVES

    mapPoints <-sf::st_sfc(crs = 3857) #empty list of generated sf points
    #POINTS Observer(proxy for map: redraw points for polygon creation)

    markerWasClicked <- FALSE

    shiny::observeEvent(input$plotPaths_marker_click, {
      print("MARKER CLICK")
      print(input$plotPaths_marker_click)
      markerWasClicked <<- TRUE
      if(!is.null(input$plotPaths_marker_click$group)){
        if( input$plotPaths_marker_click$group == "first"){
          #create polygon with points

          poly <- sf::st_cast(sf::st_combine(mapPoints), "POLYGON")
          # poly <- concaveman(mapPoints, 1) Doesn't work well

          mapPoints <<- sf::st_sfc(crs = 3857)

          proxy <- leaflet::leafletProxy("plotPaths", data = poly)%>%
            leaflet::addPolygons(stroke = TRUE, weight = 5, color = "black", fill = TRUE, fillColor = "green", opacity = 1)

          leaflet::clearGroup(proxy, "first")
          leaflet::clearGroup(proxy, "after")
        }
      }
    })

    shiny::observeEvent(input$plotPaths_click, {
      print("CLICK")
      print(input$plotPaths_click)
      if(!markerWasClicked){
        if( !is.null(input$plotPaths_click$lng) ){
          #clear shapes
          mapPoints <<- rbind(mapPoints,sf::st_as_sf( sf::st_sfc( sf::st_point(x = c(input$plotPaths_click$lng, input$plotPaths_click$lat)), crs = 3857) ) )
          #draw points
          print(mapPoints)

          proxy = leaflet::leafletProxy("plotPaths" )

          circleMarker <- leaflet::addCircleMarkers(map = proxy,
                                           lng = input$plotPaths_click$lng, lat = input$plotPaths_click$lat,
                                           radius = ifelse(nrow(mapPoints) == 1, 7, 4),
                                           color = ifelse(nrow(mapPoints) == 1, "red", "blue"),
                                           stroke = ifelse(nrow(mapPoints) == 1, TRUE, FALSE),
                                           fillOpacity = 0.5,
                                           group = ifelse(nrow(mapPoints) == 1, "first", "after"))
        }

      }
      #reset information if a marker was clicked
      markerWasClicked <<- FALSE

      }, ignoreInit = TRUE, ignoreNULL = TRUE)



    # shiny::observeEvent(input$plotPaths_dblclick, {
    #
    #     leaflet::leafletProxy("plotPaths", data = points ) %>%
    #       leaflet::clearMarkers()
    # }, ignoreInit = TRUE, ignoreNULL = TRUE)



      output$errorText <- shiny::renderText({
        if(!is.null(input$plotPaths_click)){

          print(paste0("longitude:", input$plotPaths_click$lng, "latitude: ", input$plotPaths_click$lat))

          #plot point on map (using proxy)

        }
      })


    #When confirm2 button pressed, server 2
    shiny::observeEvent(input$confirmButton2, {
      print("CONFIRM BUTTON 2")

      print(envStep2$pathNetwork)

      #return pathNetwork when confirmed
      if(!is.null(envStep2$pathNetwork)){

        #assigning input value to reactive Values (This is to allow it to be triggered from program)
        print("RETURNING SHAPE AND PATHNETWORK")
        return(
          list(pathNetwork = shiny::reactive(envStep2$pathNetwork),
               confirm = shiny::reactive(input$confirmButton2),
               shape = shiny::reactive(envStep2$shape)
               ))

      }
    }, ignoreInit = TRUE)

    print("SERVER 2 RETURNING NULL")
    print(envStep2$pathNetwork)

    return(
      list(pathNetwork = shiny::reactive(envStep2$pathNetwork),
        confirm = shiny::reactive(input$confirmButton2),
        shape = shiny::reactive(envStep2$shape)
    )
    )

  })
}
