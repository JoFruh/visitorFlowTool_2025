#function to enable Polygon Creation and deletion through interactive clicks.

#this function works by calling various "observed" or "observedEvent" reactive scripts that then react to clicks on the map.

#Input:
      #leafletMapID: the id of the leaflet map the polygon creation has to be applied to (a character variable).
      #variables: reference to an empty list. A container that will be populated with necessary variables within the function.
      #polygonsList: reference to an empty sfc. A container for all polygons that are generated.
      #input: reference "input" of shiny UI

#Output:
        #polygonsList: a list of the polygons generated

#unused conditions
#, inputConditionName = "DEFAULT", inputConditionValue = NULL
polygonCreator <- function(leafletMapID, input, startingPolygons = sf::st_sfc(crs = 4326), numberOfPolygons = "multi"){
  #populate global variable
  if(is.null(polygonEnv$polygonsList) ){
    polygonEnv$polygonsList <- startingPolygons
  }
  #container for all vertices of a polygon that is to be created
  mapPoints <-sf::st_sfc(crs = 4326) #empty list of generated sf points

  #variable to help avoid creating markers at the same time as a polygon is finalised by clicking on a marker.
  markerWasClicked <- FALSE

  #populate given list container with variables called by the observed reactives.
  #This allows an outside variable to communicate between leaflet map and the reactives of this function.
  variables$mapPoints <- mapPoints
  variables$markerWasClicked <- markerWasClicked

  mapMarkerClick <- paste0(leafletMapID, "_marker_click")
  mapClick <- paste0(leafletMapID, "_click")


  envBase$obsMarkerClick <- shiny::observeEvent(input[[mapMarkerClick]], {
    print("MARKER CLICK")
    print(input[[mapMarkerClick]])
    variables$markerWasClicked <- TRUE

    if(!is.null(input[[mapMarkerClick]]$group) ){#& envBase$step1Refreshing != TRUE
      #FINALISE POLYGON ####
      #If first vertex of polygon is clicked, Finalise polygon
      if( input[[mapMarkerClick]]$group == "first"){
        if(nrow(variables$mapPoints) > 2){
          #create polygon with points
          poly <- sf::st_cast(sf::st_combine(variables$mapPoints), "POLYGON")
          poly <- sf::st_sf(poly)
          # poly$DULN <- 1


          if(is.finite(max(polygonEnv$polygonsList$id))){
            poly$id <- max(polygonEnv$polygonsList$id)+1
          }else{
            poly$id <- 1
          }
          # poly <- concaveman(mapPoints, 1) Doesn't work well
          poly <- dplyr::rename(poly, polygons = "poly")

          if(numberOfPolygons == "multi"){
            #check if new polygon intersects or overlaps with any other
            intersectingPolys <- which(sf::st_intersects(poly, polygonEnv$polygonsList, sparse = FALSE))
            if(length(intersectingPolys) > 0){
              #if so, combine it into a single polygon
              newPoly <- sf::st_as_sf(sf::st_union(c(poly$polygons, polygonEnv$polygonsList[intersectingPolys,]$polygons) ) )
              newPoly <- newPoly %>% dplyr::rename(polygons = .data$x)
              #determine its general attractivity (with popup)
              newPoly$DULN <- 1
              #determine id
              newPoly$id <- max(polygonEnv$polygonsList$id)+1
              #remove intersecting polys
              polygonEnv$polygonsList <- polygonEnv$polygonsList[-intersectingPolys,]
              #add new polygon
              # polygonEnv$polygonsList[nrow(polygonEnv$polygonsList) + 1, ] <- newPoly
              poly <- newPoly
            }

            #generate DULN value for polygon on the fly
            values <- terra::extract(envBase$DULN$all, poly)
            poly$DULN <- mean(values$all[values$all > -20], na.rm = TRUE) #avoid values <= -20 as these are symbolic

            #keep a table of polygons
            polygonEnv$polygonsList <- rbind(polygonEnv$polygonsList, poly)
          }else{
            #keep a single polygon
            polygonEnv$polygonsList <- poly
          }

          variables$polyFinished <- TRUE

          variables$mapPoints <- sf::st_sfc(crs = 4326)
          print(polygonEnv$polygonsList)
          proxy <- leaflet::leafletProxy(leafletMapID)%>%
            leaflet::clearGroup("eraseable")%>%
            leaflet::addGeoJSON(geojson = geojsonsf::sf_geojson(polygonEnv$polygonsList),
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
    }else if(envBase$step1Refreshing == TRUE){
      envBase$step1Refreshing <- FALSE
    }
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  envBase$obsMapClick <- shiny::observeEvent(input[[mapClick]], {
    #precised condition (default always evaluates as TRUE)
    # if(  inputConditionName == "DEFAULT" | input[[inputConditionName]] %in% inputConditionValue){
    print("CLICK")
    print(input[[mapClick]])
    print(variables$markerWasClicked)
    if(!variables$markerWasClicked){
      if( !is.null(input[[mapClick]]$lng) ){
        #clear shapes
        variables$mapPoints <- rbind(variables$mapPoints,sf::st_as_sf( sf::st_sfc( sf::st_point(x = c(input[[mapClick]]$lng, input[[mapClick]]$lat)), crs = 4326) ) )
        #draw points
        print(variables$mapPoints)

        proxy = leaflet::leafletProxy(leafletMapID )

        circleMarker <- leaflet::addCircleMarkers(map = proxy,
                                                  lng = input[[mapClick]]$lng, lat = input[[mapClick]]$lat,
                                                  radius = ifelse(nrow(variables$mapPoints) == 1, 7, 4),
                                                  color = ifelse(nrow(variables$mapPoints) == 1, "red", "blue"),
                                                  stroke = ifelse(nrow(variables$mapPoints) == 1, TRUE, FALSE),
                                                  fillOpacity = 0.5,
                                                  group = ifelse(nrow(variables$mapPoints) == 1, "first", "after"),
                                                  options = leaflet::pathOptions(pane = "layer2"))
      }

    }
    #reset information if a marker was clicked
    variables$markerWasClicked <- FALSE
  # }

  }, ignoreInit = TRUE, ignoreNULL = FALSE)




}
