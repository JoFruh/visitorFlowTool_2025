#function polygonEraser

#allows the possiblity to click on leaflet polygons to remove them

#input:
#     leafletMapID: the name of the leafletOutput where the map is plotted
#     polygons: the polygons already generated that can be removed

# output:
#     polygons: the modified polygons list.

polygonEraser <- function(leafletMapID, input, startingPolygons = sf::st_sfc(crs = 4326), numberOfPolygons = "multi"){


#populate global variable
  if(is.null(polygonEnv$polygonsList) ){
    polygonEnv$polygonsList <- startingPolygons
  }



  mapGeojsonClick <- paste0(leafletMapID, "_geojson_click")


  if(numberOfPolygons == "multi"){

    envBase$obsErase <- shiny::observeEvent(input[[mapGeojsonClick]], {
      vftDbg("SHAPE CLICK")
      vftDbg(input[[mapGeojsonClick]])

      # erase shape if clicked AND if mapoints aren't being put down
      if(input[[mapGeojsonClick]]$group == "eraseable" & is.null(nrow(variables$mapPoints) ) ){
        #update polygons
        polygonEnv$polygonsList <- polygonEnv$polygonsList[!polygonEnv$polygonsList$id %in% input[[mapGeojsonClick]]$properties$id, ]

        #replot polygons
        leaflet::leafletProxy(leafletMapID )%>%
          leaflet::clearGroup("eraseable")%>%
          leaflet::addGeoJSON(
            geojson = geojsonsf::sf_geojson(polygonEnv$polygonsList),
            stroke = TRUE,
            weight = 5,
            color = "black",
            fill = TRUE,
            fillColor = "green",
            opacity = 1,
            group = "eraseable",
            options = leaflet::pathOptions(pane = "layer2")
          )

        variables$markerWasClicked <- TRUE

      }



    })

  }else{

    envBase$obsErase <- shiny::observeEvent(input[[paste0(leafletMapID, "_click")]], {
      vftDbg("GENERAL CLICK")

      #create variable if missing
      if(is.null(variables$polyFinished) ){variables$polyFinished <- FALSE}

      #when map is clicked but NO polygon was finalised
      if(variables$polyFinished == FALSE){

        #and a polygon already exists
        if(!is.null(polygonEnv$polygonsList)){

          #erase the existing polygon

          #update polygons
          polygonEnv$polygonsList <- NULL



          #replot polygons
          leaflet::leafletProxy(leafletMapID )%>%
            leaflet::clearGroup("eraseable")

      }

      }else{
        #reset global variable
        variables$polyFinished <- FALSE
    }

    })

  }


  shiny::observeEvent(input$resetButton, {
    if(!is.null(startingPolygons)){
      polygonEnv$polygonsList <- startingPolygons

      #replot polygons
      map <- leaflet::leafletProxy(leafletMapID )%>%
        leaflet::clearGroup("eraseable")
      if(!is.null(nrow(startingPolygons)) ){
        map %>% leaflet::addGeoJSON(
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


}
