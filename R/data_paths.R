#### Where the app's data lives ####

# The maps, tables and images the app reads used to sit inside the package, at
# inst/app/www/data, and every reader reached them with a relative
# "www/data/..." path - which only ever worked because the app is launched with
# its working directory at inst/app. That data is gigabytes of rasters and
# shapefiles, so it now lives outside the repo, in a different place on every
# machine:
#
#   local    C:/Users/frueh/OneDrive - .../Dokumente/visitorFlowTool_DATA/data
#   server   /home/frueh/Data/visitorFlowTool_DATA
#
# Everything that reads it goes through vftData(), so the location is decided
# once, here, instead of in fifty string literals.

#' The known data roots, tried in order when nothing is configured explicitly.
#'
#' Listed most-specific-first: the two real installations, then the legacy
#' in-package copy so a checkout that still carries it keeps working.
vftDataCandidates <- c(
  "C:/Users/frueh/OneDrive - Eidg. Forschungsanstalt WSL/Dokumente/visitorFlowTool_DATA/data",
  "/home/frueh/Data/visitorFlowTool_DATA",
  "www/data"
)

#' Cache for the resolved root, keyed by whatever was configured when it was
#' resolved, so setting the option or the env var mid-session still takes
#' effect while the common case costs no filesystem calls.
.vftDataCache <- new.env(parent = emptyenv())

#' Accept a root given either as the data directory itself or as its parent.
#'
#' The server copy is named visitorFlowTool_DATA and the local one has a `data`
#' folder inside it, so both spellings are in circulation. Returns the directory
#' that actually holds `maps`, or "" if this candidate is not it - the presence
#' of `maps` is the test, so a path that merely exists but holds something else
#' is rejected rather than silently accepted.
vftNormaliseDataRoot <- function(root){
  if(!nzchar(root)) return("")

  root <- sub("[\\/]+$", "", root)

  if(dir.exists(file.path(root, "maps"))) return(root)
  if(dir.exists(file.path(root, "data", "maps"))) return(file.path(root, "data"))

  ""
}

#' Absolute path of the data directory (the one containing maps/, tables/, ...).
#'
#' Resolution order:
#'   1. getOption("vft.dataDir"), then Sys.getenv("VFT_DATA_DIR") - set either to
#'      point the app at a copy that is not in the list above. A configured path
#'      that does not hold the data is an error, never a silent fallback.
#'   2. the known local and server locations, first one present.
#'
#' Fails loudly if none is found: every caller reads a file straight after, so a
#' missing root should surface here with the paths that were tried rather than as
#' an st_read() error about one arbitrary file.
vftDataDir <- function(){
  configured <- getOption("vft.dataDir", Sys.getenv("VFT_DATA_DIR", ""))

  if(!is.null(.vftDataCache$root) && identical(.vftDataCache$configured, configured)){
    return(.vftDataCache$root)
  }

  if(nzchar(configured)){
    root <- vftNormaliseDataRoot(configured)
    if(!nzchar(root)){
      stop("The configured visitorFlowTool data directory does not contain a 'maps' ",
           "folder: ", configured, "\n(set via option 'vft.dataDir' or VFT_DATA_DIR)")
    }
  }else{
    root <- ""
    for(candidate in vftDataCandidates){
      root <- vftNormaliseDataRoot(candidate)
      if(nzchar(root)) break
    }
    if(!nzchar(root)){
      stop("Could not find the visitorFlowTool data directory. Tried:\n  ",
           paste(vftDataCandidates, collapse = "\n  "),
           "\nSet option 'vft.dataDir' or the VFT_DATA_DIR environment variable ",
           "to point at it.")
    }
  }

  .vftDataCache$configured <- configured
  .vftDataCache$root <- root
  root
}

#' Build a path inside the data directory.
#'
#' Drop-in replacement for the old relative literals: what used to be
#' "www/data/maps/lakes.gdb" is now vftData("maps/lakes.gdb").
#'
#' @param ... path components below the data directory, as for file.path().
vftData <- function(...){
  file.path(vftDataDir(), ...)
}
