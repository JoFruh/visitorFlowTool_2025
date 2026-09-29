#### Client-side polygon drawing (steps 1 and 4) ####
#
# The vertices, the rubber band and the preview of the area are drawn by
# inst/app/www/polydraw.js, in the browser, and R hears about a drawing once:
# input$polyDrawn when a ring is closed, input$polyCut when step 4's scissors
# button is pressed. Both carry `lng` and `lat` vectors. There is no per-click
# round trip any more, and no "first"/"after" marker groups to clear.
#
# The script is loaded once at app level (R/app_ui.R); vftPolyDraw() is what
# attaches it to one map.

#' Attach the polygon drawer to a leaflet widget.
#'
#' Through htmlwidgets::onRender, because leaflet's renderValue() builds a new
#' L.Map on every render - onRender runs after each one, so the drawer is always
#' on the map that is actually on screen.
#'
#' @param map a leaflet htmlwidget.
#' @param session the module session; its namespace prefixes the two inputs.
#' @param cut offer the scissors button while exactly two vertices are placed.
#' @param polyPane the pane R draws its clickable polygons in. A click on one of
#'   those is left to R (it deletes the area) instead of starting a drawing.
#' @param areaWarn the module-local id of an element to warn in while the ring
#'   being drawn is past paintAreaTooLarge()'s ceiling, or NULL for no live
#'   check. The browser measures the ring on every pointer move against the
#'   ceiling handed over here (vftPolyDrawAreaLimit()), and tags the element
#'   `vft-pd-live` / `vft-pd-over`; the page's CSS decides what that shows.
vftPolyDraw <- function(map, session, cut = FALSE, polyPane = "layer2", areaWarn = NULL){
  o <- list(ns = session$ns(""), cut = isTRUE(cut), polyPane = polyPane)
  if(!is.null(areaWarn)) o$area <- c(vftPolyDrawAreaLimit(), list(warn = session$ns(areaWarn)))
  opts <- jsonlite::toJSON(o, auto_unbox = TRUE, digits = NA)
  htmlwidgets::onRender(map, sprintf(
    "function(el){ if(window.vftPolyDraw) window.vftPolyDraw.attach(el, this, %s); }", opts))
}

#' paintAreaTooLarge()'s ceiling, for polydraw.js's areaOver() to restate.
#'
#' Read from paintAreaTooLarge()'s own defaults rather than written out again,
#' so the live warning and the verdict on the finished outline cannot drift
#' apart when the ceiling is moved.
vftPolyDrawAreaLimit <- function(){
  f <- formals(paintAreaTooLarge)
  list(maxCells = eval(f$max_cells), buffer = eval(f$buffer_m), res = PAINT_RES)
}

#' Abandon a drawing in progress on `mapId` (a module-local output id).
vftPolyDrawCancel <- function(mapId, session = shiny::getDefaultReactiveDomain()){
  session$sendCustomMessage("vft-polydraw-cancel", list(id = session$ns(mapId)))
}

#' The ring polydraw.js sent, as a one-row sf in EPSG:4326 with its geometry
#' column called `polygons` - the shape both steps keep their areas in. NULL for
#' anything that is not at least three finite vertices.
vftPolyDrawSf <- function(value){
  lng <- suppressWarnings(as.numeric(unlist(value$lng)))
  lat <- suppressWarnings(as.numeric(unlist(value$lat)))
  if(length(lng) < 3 || length(lng) != length(lat) || !all(is.finite(c(lng, lat)))) return(NULL)
  ring <- cbind(lng, lat)
  ring <- rbind(ring, ring[1, ])
  sf::st_sf(polygons = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326))
}

#' The two-point line of a step 4 cut, as an sfc LINESTRING in EPSG:4326.
vftPolyDrawLine <- function(value){
  lng <- suppressWarnings(as.numeric(unlist(value$lng)))
  lat <- suppressWarnings(as.numeric(unlist(value$lat)))
  if(length(lng) != 2 || length(lat) != 2 || !all(is.finite(c(lng, lat)))) return(NULL)
  sf::st_sfc(sf::st_linestring(cbind(lng, lat)), crs = 4326)
}
