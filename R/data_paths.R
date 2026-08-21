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


#### Shared display layers ####

#' The protected-areas layer, simplified once and reused by every session.
#'
#' This layer is display-only - step 5 and newVersions hand it straight to
#' leaflet::addPolygons() and nothing computes with it - but it is three national
#' multipolygons carrying 889,260 vertices, which is far more detail than any
#' screen can show and a large payload for htmlwidgets to serialise on the shared
#' main thread.
#'
#' Two costs were being paid per session, and both are removed here:
#'
#'   - the clip: cropping 889k vertices took ~5.7s in EPSG:4326, because sf
#'     routes lon/lat geometry through s2's spherical predicates. Held in 2056
#'     the same clip is ~0.10s.
#'   - the payload: 21,113 vertices and 0.77 MB of GeoJSON for a typical area,
#'     against 8,933 vertices and 0.33 MB after simplification - 58% less for
#'     leaflet to serialise and the browser to draw.
#'
#' Simplifying costs ~10s, so it must happen once for the process rather than
#' once per session; global.R warms it at startup so no user ever waits for it.
#' Cached in .GlobalEnv alongside the .vft_* rasters, and safe there on the same
#' terms: it derives purely from an immutable national file and holds no user
#' state.
#'
#' 25 m is the tolerance because it is the knee of the curve - it drops 60% of
#' the vertices for a 0.05% change in area, where 50 m saves little more and
#' costs 0.34%. It is a display tolerance and nothing measures against this
#' layer, but keep it well under the width of the features being drawn.
VFT_PA_TOLERANCE_M <- 25

vftProtectedAreasCached <- function(tolerance = VFT_PA_TOLERANCE_M){
  if(!exists(".vft_PA_simplified", envir = .GlobalEnv)){
    pa <- sf::st_read(vftData("maps/protectedAreas/PA_all.gpkg"), quiet = TRUE)
    .GlobalEnv$.vft_PA_crs <- sf::st_crs(pa)
    #simplify in the projected CRS so the tolerance is in metres, not degrees
    .GlobalEnv$.vft_PA_simplified <-
      sf::st_simplify(sf::st_transform(pa, 2056), dTolerance = tolerance)
  }
  .GlobalEnv$.vft_PA_simplified
}

#' The protected areas within one study area, ready for leaflet.
#'
#' Clips the cached layer to `shape` and returns it in the layer's original CRS
#' (lon/lat), which is what leaflet requires - only the intermediate work happens
#' in 2056.
#'
#' The window is built from `shape`'s bounding box in ITS OWN CRS and then
#' projected as a polygon. Transforming first and taking st_bbox() afterwards is
#' not the same region - it cuts a larger one, and shifted the resulting area by
#' +2.2% when measured - so the order here matters.
vftProtectedAreas <- function(shape){
  cached <- vftProtectedAreasCached()
  win    <- sf::st_transform(sf::st_as_sfc(sf::st_bbox(shape)), 2056)
  clipped <- sf::st_intersection(cached, win)
  sf::st_transform(clipped, .GlobalEnv$.vft_PA_crs)
}
