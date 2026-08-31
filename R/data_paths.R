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

#' Simplification tolerance for the generated areas of interest, in metres.
#'
#' Same lever as VFT_PA_TOLERANCE_M above, applied to the polygons generateAoI2()
#' cuts out of the attractiveness raster. terra::as.polygons() traces CELL
#' BOUNDARIES, so those rings arrive as staircases carrying far more vertices
#' than their shape needs - three features with a huge vertex count, which is
#' exactly the case st_simplify() helps and the opposite of the path network (see
#' vftAddNetworkLines() below, where the cost was edge count and simplifying
#' bought almost nothing).
#'
#' It pays three times over: the GeoJSON step 4 serialises on the main thread for
#' every draw and every edit, the terra::extract() that scores each polygon, and
#' every st_intersects()/st_nearest_feature() the simulation and the newVersions
#' page later run against the confirmed set.
#'
#' 75 m, NOT the 25 m used above, and the difference is the whole point: a
#' tolerance below the cell size cannot remove a staircase step, because every
#' vertex of a staircase is already further off the straight line than that.
#' DULN_nat_majMaxMeanAGGBlur.tif is 0.00096 deg, which at 47 N is 73 x 106 m,
#' so 25 m here would have been a no-op. Measured on a thresholded random field
#' at exactly that resolution (117 polygons, 7301 vertices unsimplified):
#'
#'   tolerance   vertices   dropped   sum area   GeoJSON
#'      25 m        7301      0.0%     -0.000%     286 KB
#'      50 m        5133     29.7%     -0.055%     206 KB
#'      75 m        2623     64.1%     -0.133%     112 KB
#'     100 m        1937     73.5%     -0.122%      87 KB
#'     150 m        1399     80.8%     -1.231%      67 KB
#'
#' 75 m is the knee: two thirds of the vertices and 61% of the payload gone for
#' a 0.13% change in reported area. Past 100 m the area starts to cost real
#' money for very little more. 0 switches simplification off, which is the A/B
#' if the outlines ever look wrong.
VFT_AOI_TOLERANCE_M <- 75

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

#### The path network as a display layer ####

#' How many line-width classes the network is drawn in.
#'
#' The app encodes path usage twice: in colour (a continuous palette) and in line
#' width. WebGL draws one width per layer, so the width is preserved by binning
#' it and issuing one call per bin. Four is enough that the thick/thin reading
#' survives; the cost of another class is one more tiny GL layer, so this is
#' cheap to raise if the map looks stepped.
VFT_GL_WIDTH_CLASSES <- 4L

#' The thinnest a path is ever drawn, in px.
#'
#' Below about 1 px a line is sub-pixel and renders as a barely-there hairline or
#' vanishes altogether, so the least-used paths stop reading as a network at all.
#' This is a floor on the drawn width, applied after the usage-to-width mapping,
#' because the widths come out of a median per class and drift with the data -
#' see vftNetworkWeights(). Raise it if the faintest paths are still too faint.
VFT_GL_MIN_WIDTH <- 0.5

#' Whether to draw network lines through WebGL.
#'
#' On by default. `VFT_GL=0` falls back to `leaflet::addPolylines()` so the two
#' renderings can be compared on the server without a reinstall.
vftUseGl <- function(){
  !identical(Sys.getenv("VFT_GL", "1"), "0")
}

#' The app's line width for a vector of usage values: a hairline at no usage,
#' `span` px at the busiest path. Kept in one place because eight call sites used
#' to spell it out.
#'
#' `weightRef` exists because the call sites do NOT normalise against the column
#' they colour by: they scale width against `passageAOI` while colouring by the
#' selected agent type, so a walkers-only view keeps the widths of the all-agent
#' view. That is deliberate - it makes the agent-type views comparable - so it is
#' preserved here rather than tidied away. Defaults to `values` for the sites
#' that do use one column for both.
#'
#' `minWidth` is the stroke an unused path gets and `span` is what full usage
#' adds on top.
#'
#' WebGL draws these noticeably heavier than the old leaflet rendering did at the
#' same nominal weight, so 1.3 px came off every path on 2026-08-24 (minWidth was
#' 2). 1.3 rather than a rounder number because the four width classes land on
#' the class MEDIANS, not on minWidth itself: at span = 2 the thinnest class sits
#' 0.3 px above minWidth, so 0.7 is what puts it at exactly 1 px. Anything
#' thinner than ~1 px is sub-pixel and renders as an invisible hairline - that is
#' the floor worth protecting, not minWidth.
#'
#' Note this is the width BEFORE the 1 px floor in VFT_GL_MIN_WIDTH - the floor
#' is what actually guarantees the thinnest line stays visible, because the four
#' classes are drawn at their MEDIANS and a median moves with how the usage
#' values happen to be distributed. Tuning minWidth alone cannot hold a floor:
#' the same minWidth = 0.7 produced a thinnest class of 1.00 px on one sample and
#' 0.90 px on another.
vftNetworkWeights <- function(values, weightRef = values, minWidth = 0.7, span = 2){
  v  <- as.numeric(values)
  mx <- suppressWarnings(max(as.numeric(weightRef), na.rm = TRUE))
  if(!is.finite(mx) || mx <= 0) mx <- 1
  minWidth + (v / mx) * span
}

#' The leaflet group names one network layer occupies.
#'
#' In WebGL the layer is split across `nClass` groups, so anything that clears or
#' hides "paths" has to know all of them. Falls back to the single plain group
#' when GL is off.
vftNetworkGroups <- function(group = "paths", nClass = VFT_GL_WIDTH_CLASSES){
  if(!vftUseGl()) return(group)
  paste0(group, "_gl", seq_len(nClass))
}

#' Draw the whole path network on a leaflet map or proxy.
#'
#' This is the single largest main-thread cost the app had: `addPolylines()`
#' encodes every edge into nested JSON on the shared thread, and it scales with
#' EDGE COUNT, not with vertices. Measured on this machine:
#'
#'   edges    addPolylines    addGlPolylines (4 classes)
#'    8,000       2.36 s              0.08 s
#'   20,000       6.19 s              0.19 s
#'   50,000      15.45 s              0.59 s
#'
#' Note that the usual geometry lever does NOT work here, which is why this looks
#' nothing like vftProtectedAreas() above: st_simplify at 25 m took 6.19s to
#' 5.52s and rounding coordinates to 1 m took it to 5.49s. The protected-areas
#' layer is 3 features with 889k vertices and wants fewer vertices; the network is
#' tens of thousands of features with a handful each and wants a different
#' encoder. Same symptom, opposite fix.
#'
#' leafgl sends a flat coordinate array instead, which is ~30% more bytes over
#' the wire (21.4 MB vs 16.6 MB at 50k edges) for ~26x less time on the thread
#' every other session is waiting on. Bytes to one browser are not shared cost.
#'
#' @param map a leaflet map or a leafletProxy.
#' @param net sf object of the edges to draw.
#' @param values numeric usage values, one per row of `net`, driving colour and
#'   width. Length must match `nrow(net)`.
#' @param weightRef numeric values the WIDTH is normalised against, when that is
#'   a different column from the one being coloured (see vftNetworkWeights).
#' @param pal a palette function, as returned by leaflet::colorNumeric().
#' @param group base group name; in GL mode the real groups are
#'   vftNetworkGroups(group).
#' @param pane map pane to draw into. leafgl honours panes, so the app's existing
#'   addMapPane() z-ordering keeps working.
vftAddNetworkLines <- function(map, net, values, pal, group = "paths",
                               weightRef = values, minWidth = 0.7, span = 2,
                               nClass = VFT_GL_WIDTH_CLASSES,
                               minDraw = VFT_GL_MIN_WIDTH, opacity = 1,
                               pane = "layer2"){
  values <- as.numeric(values)
  w      <- vftNetworkWeights(values, weightRef, minWidth, span)

  if(!vftUseGl()){
    return(leaflet::addPolylines(map, data = net, stroke = TRUE,
                                 weight = pmax(w, minDraw),
                                 color = pal(values), fill = FALSE,
                                 opacity = opacity, group = group,
                                 options = leaflet::pathOptions(pane = pane)))
  }

  #cut() needs a spread to split. A network where every edge carries the same
  #usage (a fresh run, or a single-path area) has none and would error, and an
  #all-NA column would make range() warn - so both fall back to one class.
  spread <- if(any(is.finite(w))) diff(range(w, na.rm = TRUE)) else 0
  cls <- if(spread > 0){
    cut(w, breaks = nClass, labels = FALSE, include.lowest = TRUE)
  }else{
    rep(1L, length(w))
  }
  cls[is.na(cls)] <- 1L

  groups <- vftNetworkGroups(group, nClass)
  for(k in sort(unique(cls))){
    idx <- which(cls == k)
    if(!length(idx)) next
    #leafgl wants a 3-column RGB matrix scaled 0-1, one row per feature - which
    #is col2rgb TRANSPOSED. Passing col2rgb's own orientation silently uses only
    #the first colour for the whole layer.
    map <- leafgl::addGlPolylines(map,
      data    = net[idx, ],
      color   = t(grDevices::col2rgb(pal(values[idx]))) / 255,
      weight  = max(stats::median(w[idx]), minDraw),
      opacity = opacity,
      group   = groups[[k]],
      pane    = pane)
  }
  map
}

#' Remove a network layer drawn by vftAddNetworkLines().
#'
#' leaflet::clearShapes() does not touch GL layers - they live in their own
#' canvas - so a proxy redraw that only called clearShapes() would stack a second
#' network on top of the first.
vftClearNetworkLines <- function(map, group = "paths",
                                 nClass = VFT_GL_WIDTH_CLASSES){
  if(!vftUseGl()) return(leaflet::clearGroup(map, group))
  for(g in vftNetworkGroups(group, nClass)) map <- leafgl::clearGlGroup(map, g)
  map
}
