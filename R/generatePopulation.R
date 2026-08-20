#function to determine ALL agents of a region and their charactersitics

#input:
  # shape = sf polygon of area to study
  # network = paths to determine starting vectors of all agents

#output:
  # pop = large dataframe of all agents with their characteristics

generatePopulation <- function(network, parkingIntensity = 1, nAgents = 2000){

  print("GENERATE POPULATION")
  #TODO: generate population based on habitation maps and statistics from the region studies (TLM3D buildings, regional statistics)
  #FOR NOW:¨load excel into a dataframe
  # pop <- readxl::read_excel(vftData("tables/generalPopulation_example_10k.xlsx"))
  pop <- data.frame(startV = rep(NA, times = nAgents))
  pop$id <- 1:nAgents
  #generate pop table (1000 agents by default)
  #col: id, agentTyp, age, walk, run, bike, habit, startV, visitedAOI
  #agentTyp: walkNat, walkProx, dogNat, dogProx, ebikeNat, bikeSport, jogger
  pop$agentTyp <- sample(c("walkNat", "walkSoc", "dogNat", "dogProx", "ebikeNat", "bikeSport", "jogger"),
                         size = nAgents,
                         replace = TRUE,
                         prob = c(0.232, 0.1667, 0.1535, 0.1124, 0.089, 0.05937, 0.2328)
                         )
  #ignored for now: age, walk, run, bike, habit, visitedAOI
  #each agents represents a single statistical outing. They do not represent real individuals.
  #determine starting areas based on raster of residential population and parking
  areResidents <- igraph::V(network)$Residents > 0 & is.finite(igraph::V(network)$Residents)
  areParking <- igraph::V(network)$parking > 0 & is.finite(igraph::V(network)$parking)
  areNewResidents <- igraph::V(network)$newResidential > 0 & is.finite(igraph::V(network)$newResidential)

  #parking intensity (determine ratio of parking to residents)


  #determining start areas by sampling residential and parking areas (parkingIntensity determines ratio parking/residents)
  #parking are weighed by attractivity of nearest AOI
  pop$startV <- sample(c(igraph::V(network)$nodeID[areResidents], igraph::V(network)$nodeID[areParking], igraph::V(network)$nodeID[areNewResidents] ), length(pop$startV), replace = TRUE,
                       prob = c(igraph::V(network)$Residents[areResidents], igraph::V(network)$newResidential[areNewResidents], (igraph::V(network)$parking[areParking] * igraph::V(network)$parkingAttr[areParking] * parkingIntensity)  ) )


  # terra::writeRaster(residential_tif_wgs, filetype = "GTiff", filename = "C:/Users/frueh/Documents/visitorFlowTool/inst/app/www/data/maps/residential/residentialData_raster_final.tif")
  #use tif for statistical sampling

  return(pop)
}
