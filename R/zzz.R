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

  shiny::addResourcePath(prefix = 'www',
                         directoryPath = "./inst/app/www")
}
