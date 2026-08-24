#Automatically generate Area of Interest (AoI or Zielgebiete)

#inputs: network (nodes and edges: tbl_graph format) with attractiveness information,
          #raster of AoI areas

#outputs: modified network and collection of polygons (sf)
# library(concaveman)

#source C FUNCTIONS
# Rcpp::sourceCpp("R/utlities/CPP_FUNCTIONS.cpp")

generateAoI2 <- function(network, minThresh, perimeter = NULL, #, lake_path = NULL
                         DULN = NULL, DULN_all = NULL){

  sf::sf_use_s2(TRUE)

  #avoid problems when combining polygons
  polygons <- sf::st_sfc(crs = 4326) #list of final generated sf polygons #


  #vftGraphTibble(), not as_tibble(activate(nodes)): this runs inside the step-4
  #worker and the nodes carry an sf `geometry` column, which tibble < 3.3 refuses
  #as "not a vector" - the same failure that killed the ABM. See graph_helpers.R.
  networkPts <- terra::vect(sf::st_as_sf(vftGraphTibble(network, "nodes")))

  # networkEdgs <- terra::vect(network |> tidygraph::activate(edges) |> dplyr::as_tibble() |> sf::st_as_sf())
  #get vertices above selected threshold (Areas of Interest)

  #nodes <- igraph::V(network)$nodeID[igraph::V(network)$DULN > minThresh]
  #instead, get vertices within selected raster
  rasterSel <- DULN_all
  rasterSel[rasterSel < minThresh] <- NA
  rasterSel[rasterSel >= minThresh] <- 1

  # mask region outside perimeter
  #avoids involving many unseen AoIs
  if(!is.null(perimeter)){
    rasterSel <- terra::mask(rasterSel, terra::buffer(terra::vect(perimeter), 1000))
  }


  # rasterSel <- terra::buffer(rasterSel, 10)
  rasterSel <- terra::as.polygons(rasterSel , aggregate = TRUE , na.rm = TRUE) #TRUE
  rasterSel <- terra::disagg(rasterSel)

  polygons <- sf::st_as_sf(rasterSel)

  #get DULN means (or max) and areas
  polygons$DULN <- NA

  #before extracting values, remove lakes (make them NA)
  wkt <- sf::st_as_text( sf::st_as_sfc(sf::st_transform(perimeter, "epsg:2056") ) )

  lakes <- sf::st_read( vftData("maps/lakes.gdb"), #lake_path
                        query = 'SELECT * FROM "lakes"',
                        wkt_filter = wkt)
  lakes <- sf::st_transform(lakes[lakes$SHAPE_Area > 10000, ], "epsg:4326")

  #remove lakes from DULN raster
  dulnRaster <- terra::subset(DULN, "walkNat")
  dulnRaster[terra::vect(lakes)] <- NA

  for(polyNb in 1:nrow(polygons)){
    # polygons[polyNb,]$DULN <- mean(terra::extract(envBase$DULN, polygons[polyNb,])$all)

    #try with max rather than mean
    #issue with mean is that larger areas may be downgraded, even if their best places make them loved
    #max may better represent how much areas are appreciated

    # polygons[polyNb,]$DULN <- max(terra::extract(envBase$DULN, polygons[polyNb,])$all)

    #try with an intermediate between mean and max.
    #while the most attractive area is important. The general attractivity of the area is as well.
    #This also helps reduce an issue with AoIs: if a very beautiful and pretty large core happens to be surrounded by mediocre, but above threshold land
    #It becomes a large, medicore area

    extraction <- terra::extract(terra::subset(DULN, "walkNat"), polygons[polyNb,])$walkNat
    extraction <- sort(extraction, decreasing = TRUE)
    polygons[polyNb,]$DULN <- (median(extraction[1:(length(extraction)/4 )] , na.rm = TRUE)+
                                 median(extraction, na.rm = TRUE) ) / 2


    #keep it simple
    # polygons[polyNb,]$DULN <- median(terra::extract(dulnRaster, polygons[polyNb,])$Nature_walk, na.rm = TRUE)

  }

  polygons$area <- as.numeric(sf::st_area(polygons))

  polygons <- polygons[polygons$area > 100000,]

  sf::st_geometry(polygons) <- "polygons"
  polygons[,1] <- NULL
  # polygons[,"area"] <- NULL



  return(polygons)
}
