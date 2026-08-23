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
.onLoad <- function(libname, pkgname) {

  # www is served from the installed package, which is the one location that is
  # identical on a laptop and on the server. The previous version listed two
  # hardcoded Windows directories and one home path, so under Shiny Server -
  # which runs as the shiny user, from its own working directory - none of the
  # three matched and nothing was registered at all. It survived only because
  # the app happens to be launched with its working directory at inst/app,
  # where Shiny serves www/ itself; that is luck, not design, and it breaks the
  # moment the app is started any other way.
  dir <- system.file("app/www", package = pkgname)

  # pkgload::load_all() maps system.file() onto inst/, so the line above already
  # covers development. These cover a plain source checkout launched from the
  # repo root or from inst/app.
  if(!nzchar(dir) || !dir.exists(dir)){
    cand <- c("inst/app/www", "www", file.path("..", "..", "inst", "app", "www"))
    cand <- cand[dir.exists(cand)]
    dir  <- if(length(cand)) cand[1] else ""
  }

  # never fail to load over a resource path: a missing www makes the UI ugly,
  # an error in .onLoad makes the package unusable.
  if(nzchar(dir) && dir.exists(dir))
    try(shiny::addResourcePath("www", normalizePath(dir, winslash = "/")),
        silent = TRUE)

}
