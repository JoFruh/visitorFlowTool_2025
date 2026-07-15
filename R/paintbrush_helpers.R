PAINT_CATEGORIES <- data.frame(
  id   = 1:5,
  name = c("grass", "tree", "artificial", "natural", "water"),
  r    = c(144,   0, 128, 160,  30),
  g    = c(238, 100, 128,  82, 144),
  b    = c(144,   0, 128,  45, 255)
)

paintPalette <- leaflet::colorFactor(
  palette  = c("lightgreen", "darkgreen", "grey", "#a05a3c", "dodgerblue"),
  levels   = PAINT_CATEGORIES$id,
  na.color = "transparent"
)

#' Classify an RGBA PNG array (dims height x width x 4, row 1 = top, values 0-1)
#' into PAINT_CATEGORIES$id, returned row-major (matches terra's cell order).
classifyPaintPixels <- function(img, alpha_threshold = 0.05){
  r255 <- as.vector(t(img[,,1])) * 255
  g255 <- as.vector(t(img[,,2])) * 255
  b255 <- as.vector(t(img[,,3])) * 255
  a    <- as.vector(t(img[,,4]))

  distMat <- sapply(seq_len(nrow(PAINT_CATEGORIES)), function(i){
    (r255 - PAINT_CATEGORIES$r[i])^2 + (g255 - PAINT_CATEGORIES$g[i])^2 + (b255 - PAINT_CATEGORIES$b[i])^2
  })
  ids <- max.col(-distMat)
  ids[a <= alpha_threshold] <- NA
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
