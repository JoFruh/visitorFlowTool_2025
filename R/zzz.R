#' visitorFlowTool
#'
#' Description of your package
#'
#' @docType package
#' @author Johan Fruh <johan.frueh@wsl.ch>
#' @useDynLib visitorFlowTool
#' @importFrom Rcpp sourceCpp
#' @name visitorFlowTool
NULL
#>NULL

# make www folder accessible to package
.onLoad <- function(...) {

  # shiny::addResourcePath(prefix = 'www',
  #                        directoryPath = "./inst/app/www")

  # shiny::addResourcePath("www", system.file("app/www", package = "visitorFlowTool"))



  if(path.expand("~") == "C:/Users/frueh/Documents"){
    shiny::addResourcePath("www", "C:/Users/frueh/Documents/visitorFlowTool_final/inst/app/www")
  }else if(path.expand("~") == "/home/frueh"){
    shiny::addResourcePath("www", "/home/frueh/ShinyApps/visitorFlowTool/inst/app/www")
  }



}
