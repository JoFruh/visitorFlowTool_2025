

# envBase <- new.env(parent = emptyenv())

#set global environments and variables

#create environment for polygons (both for step 1 and 5)
# polygonEnv <- new.env(parent = emptyenv())
#
# #create environment for base elements of app
# envBase <- new.env(parent = emptyenv())
#
# #Global Variables
# envBase$shape <- NULL
# envBase$SM_pres <- NULL
# envBase$SM_noPres <- NULL
# envBase$network <- NULL
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
# envBase$toSelectSpAfter <- FALSE
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


#' Function launching a session of visitorFlowTool
#' @importFrom shiny runApp shinyAppDir
#' @export
runVisitorFlowTool <- function(...){

  # #clear everything before starting app
  rm(list = ls(all.names = TRUE))
  detach(package:visitorFlowTool)





  if (interactive()) {

    runApp(appDir = system.file("app",
                                package = "visitorFlowTool"))

  } else {

    shinyAppDir(appDir = system.file("app",
                                     package = "visitorFlowTool"))

  }

}

#' @export
l <- runVisitorFlowTool


