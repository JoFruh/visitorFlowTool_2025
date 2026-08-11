#' Paint materials, one row per button. `level` says which of the two stacked
#' rasters a stroke of that material lands in: "ground" -> paintedRaster,
#' "canopy" -> canopyRaster. The canopy colors are deliberately darker than
#' their ground counterparts (artificial/tree) so the two levels stay
#' distinguishable when the canopy layer is drawn over the ground layer.
PAINT_CATEGORIES <- data.frame(
  id    = 1:7,
  name  = c("grass", "tree", "artificial", "natural", "water", "canopy_artificial", "canopy_tree"),
  level = c(rep("ground", 5), rep("canopy", 2)),
  r     = c(144,   0, 128, 160,  30, 63, 20),
  g     = c(238, 100, 128,  82, 144, 63, 83),
  b     = c(144,   0, 128,  45, 255, 63, 45)
)

#' One palette for both rasters - the category ids are unique across levels, so
#' a single colorFactor covers ground and canopy alike.
paintPalette <- leaflet::colorFactor(
  palette  = c("lightgreen", "darkgreen", "grey", "#a05a3c", "dodgerblue", "#3f3f3f", "#14532d"),
  levels   = PAINT_CATEGORIES$id,
  na.color = "transparent"
)

#' Opacities of the two painted layers. The ground layer is dimmed (rather than
#' hidden) while canopy is being edited, so you can still see what you are
#' painting canopy over.
PAINT_OPACITY_GROUND        <- 0.5
PAINT_OPACITY_GROUND_DIMMED <- 0.2
PAINT_OPACITY_CANOPY        <- 0.7

#' Which raster a stroke belongs in, derived from the category id the browser
#' echoes back with every stroke - so the level never has to travel as its own
#' payload field.
paintLevelOf <- function(categoryId){
  PAINT_CATEGORIES$level[match(categoryId, PAINT_CATEGORIES$id)]
}

#' Turn an RGBA PNG array's alpha channel into raster cell ids: `categoryId`
#' where painted, NA elsewhere. Only one paint color is ever active at a time
#' (paintbrush.js echoes back the active PAINT_CATEGORIES$id with every stroke),
#' so R never needs to inspect painted pixel colors - painted-or-not is the
#' only distinction the raster needs.
paintedPixelIds <- function(img, categoryId, alpha_threshold = 0.05){
  a <- as.vector(t(img[,,4]))
  ids <- rep(NA_integer_, length(a))
  ids[a > alpha_threshold] <- categoryId
  ids
}

#' Grid-snapped template raster in EPSG:2056, sized to a single stroke's own captured
#' map bounds (list(west, east, south, north) in EPSG:4326). Snapping the extent to
#' multiples of `res` (i.e. an implicit origin at 0,0) means every stroke's template
#' shares the same grid regardless of where it was painted, so accumulated strokes stay
#' aligned when merged - while each stroke's own extent is never clipped to some other,
#' unrelated fixed area (e.g. a study-area bbox that may not cover where you're painting).
buildStrokeTemplate <- function(bounds4326, res = 5){
  pts <- sf::st_sfc(sf::st_multipoint(rbind(
    c(bounds4326$west, bounds4326$south),
    c(bounds4326$east, bounds4326$north)
  )), crs = 4326)
  bb <- sf::st_bbox(sf::st_transform(pts, 2056))
  terra::rast(
    xmin = floor(bb["xmin"] / res) * res,  xmax = ceiling(bb["xmax"] / res) * res,
    ymin = floor(bb["ymin"] / res) * res,  ymax = ceiling(bb["ymax"] / res) * res,
    resolution = res, crs = "EPSG:2056"
  )
}
