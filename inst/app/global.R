#set global environments and variables
#
# #create environment for polygons (both for step 1 and 5)
# polygonEnv <- new.env(parent = emptyenv())
# polygonEnv$polygonsList <- NULL
#
# #create environment for base elements of app
# envBase <- new.env(parent = emptyenv())
#
# #Global Variables
# envBase$shape <- NULL
# envBase$SM_pres <- NULL
# # envBase$SM_noPres <- NULL
# envBase$network <- NULL
# envBase$parking <- NULL
# envBase$residential
# envBase$networkList <- NULL
# envBase$basemap <- NULL
# envBase$confirm <- NULL
# envBase$minThresh <- NULL
# envBase$finalPolygons <- NULL
# envBase$pathUsage <- NULL
# envBase$versionsUI <- NULL
# envBase$step6FirstRun <- NULL
# envBase$newVersionsFirstRun <- NULL
# envBase$toSelectSpAfter <- NULL
# envBase$triggerNewVersions_nr <- NULL
# envBase$toSelectSpAfter <- NULL
# envBase$toSelectSpAfter <- FALSE
# envBase$step <- NULL
# envBase$step <- 1
# envBase$SMdateTime <- NULL
# envBase$SMColors <- NULL
# # envBase$step1Refreshing <- NULL
# # envBase$step1Refreshing <- FALSE
# envBase$isImported <- NULL
# envBase$isImported <- FALSE
#
# envBase$obsMapClick <- NULL
# envBase$obsMarkerClick <- NULL
# envBase$obsErase <- NULL
#
#
# #step 3 saves
# envBase$filterList <- NULL
# envBase$sdmLayers <- NULL
#
# envBase$df_spInfo <- NULL
# envBase$spChc <- NULL
#
# #checkbox saves
# envBase$checkboxSave <- NULL
# envBase$groupSave_class <- NULL
# envBase$groupSave_sens <- NULL
# envBase$groupSave_type <- NULL
# envBase$groupSave_all <- NULL
# envBase$weightInputs <- NULL
# envBase$weightNames <- NULL
#
# envBase$DULN <- NULL
# envBase$DULN_all <- NULL
#
#
# #variable to activate species selection AFTER recreation modelling
#
# #global variables to control observer creation
# # >> avoids re-creating observers when returning to server function
#
# #global variables for the new versions step
# envNewVersions <- new.env(parent = emptyenv())
#
# envNewVersions$appendedObservers <- NULL
#
# envNewVersions$markerWasClicked <- NULL
#
# envNewVersions$shapeWasClicked <- NULL
#
# envNewVersions$isLinking <- NULL
#
# envNewVersions$firstLinkNode <- NULL
# envNewVersions$secondLinkNode <- NULL
#
# envNewVersions$mapView <- NULL
#
# envNewVersions$trigger <- NULL
#
# #other environments
# envUpdate <- new.env(parent = emptyenv())
# envBtn <- new.env(parent = emptyenv())
#
# envUpdate$updateNetworkPlot <- NULL
# envBtn$versionBtn_nb <- NULL
#

# cppPath <- system.file("src/CPP_FUNCTIONS.cpp", package = "visitorFlowTool")


# if(path.expand("~") == "C:/Users/frueh/Documents"){
#   home <- "C:/Users/frueh/Documents/visitorFlowTool_final"
# }else if(path.expand("~") == "/home/frueh"){
#   home <- "/home/frueh/ShinyApps/visitorFlowTool"
# }

#PREPARE WORKERS ####
currentPlan <- future::plan("future::multisession", workers = 2) #temporary fix for live
# currentPlan <- future::plan("future::sequential")

# Pre-warm all workers immediately
warming <- lapply(seq_len(future::nbrOfWorkers()), function(i) {
  future::future({
    # Rcpp::sourceCpp(system.file("src/CPP_FUNCTIONS.cpp", package = "visitorFlowTool"))

    library(visitorFlowTool)

    TRUE
  })
})
#waiting for first worker to be ready
# future::value(warming[[1]])



#GLOBAL FUNCTIONS
imageMap <- function(inputId, imgsrc, opts, i18n) {
  areas <- lapply(names(opts), function(n)
    shiny::tags$area(title=n, coords=opts[[n]],
                     href="#", shape="poly"))
  js <- paste0("$(document).on('click', 'map area', function(evt) {
  evt.preventDefault();
  var val = evt.target.title;
  Shiny.onInputChange('", inputId, "', val);})")
  list(
    shiny::tags$img(height = 70,src=imgsrc, usemap=paste0("#", inputId),
                    shiny::tags$head(tags$script(shiny::HTML(js)))),
    shiny::tags$map(name=inputId, areas))

  #deactivate imagemap temporarily (until it is more stable)
  #to activate history bar, remove below return function for the function to return list above.
  return(shiny::tags$img(height = 70,src= imgsrc))
}



