#' Paint materials, one row per button. `level` says which of the two stacked
#' rasters a stroke of that material lands in: "ground" -> paintedRaster,
#' "canopy" -> canopyRaster, "both" -> the same cells in each. The canopy colors
#' are deliberately darker than their ground counterparts (artificial/tree) so
#' the two levels stay distinguishable when the canopy layer is drawn over the
#' ground layer.
#'
#' "both" exists for a building: a solid block occupies the ground and everything
#' above it, so one stroke has to fill either raster at once. It belongs to no
#' level, which is why its button is never disabled by the level switch.
#'
#' `hex` is the single source of truth for a material's color: it is what the
#' UI buttons are styled with (newVersions_ui.R), what the brush cursor is drawn
#' in, and what the browser fills painted cells with. There is no server-side
#' palette any more - R never renders the painted layers.
PAINT_CATEGORIES <- data.frame(
  id    = 1:8,
  name  = c("grass", "tree", "artificial", "natural", "water",
            "canopy_artificial", "canopy_tree", "artificial_block"),
  level = c(rep("ground", 5), rep("canopy", 2), "both"),
  hex   = c("lightgreen", "darkgreen", "grey", "#a05a3c", "dodgerblue",
            "#3f3f3f", "#14532d", "#1f1f1f"),
  stringsAsFactors = FALSE
)

#' Resolution of the painted grid, in metres of EPSG:2056. Cells are indexed
#' globally by (col, row) = (floor(E/res), floor(N/res)), so every painted cell
#' - whatever version or session it came from - lands on the same grid.
PAINT_RES <- 5

#' Opacities of the two painted layers. The ground layer is dimmed (rather than
#' hidden) while canopy is being edited, so you can still see what you are
#' painting canopy over. These are applied by the browser as the CSS opacity of
#' the *map pane*, not of individual overlays: that is what keeps overlapping
#' strokes from compounding into darker patches.
PAINT_OPACITY_GROUND        <- 0.5
PAINT_OPACITY_GROUND_DIMMED <- 0.2
PAINT_OPACITY_CANOPY        <- 0.7

#' Everything the browser needs to set itself up: the grid resolution, the
#' EPSG:3857 -> EPSG:2056 transform (see paintTransform2056), the material colors
#' and the pane opacities.
paintInitPayload <- function(refLng, refLat){
  list(
    res       = PAINT_RES,
    transform = paintTransform2056(refLng, refLat),
    colors    = stats::setNames(as.list(PAINT_CATEGORIES$hex), as.character(PAINT_CATEGORIES$id)),
    #which raster(s) a material's strokes land in. Sent as the plain level string
    #rather than an array so it survives Shiny's auto_unbox unambiguously
    levels    = stats::setNames(as.list(PAINT_CATEGORIES$level), as.character(PAINT_CATEGORIES$id)),
    opacity   = list(ground       = PAINT_OPACITY_GROUND,
                     groundDimmed = PAINT_OPACITY_GROUND_DIMMED,
                     canopy       = PAINT_OPACITY_CANOPY)
  )
}

#' Closed-form Web Mercator (EPSG:3857, what Leaflet projects to) -> EPSG:2056,
#' fitted around `refLng`/`refLat` so the browser can georeference brush strokes
#' without shipping a projection library.
#'
#' A plain affine is not good enough: Web Mercator's scale factor grows with
#' latitude, which over +/-5 km already costs ~1.5 m - a third of a 5 m cell, and
#' growing quadratically beyond that. Adding the three second-order terms absorbs
#' exactly that effect and brings the residual down to millimetres across the whole
#' area a user could plausibly pan to.
#'
#' Coefficients are in Mercator *kilometres* relative to the reference point
#' (u = dX/1000, v = dY/1000), against the basis (1, u, v, u^2, u*v, v^2), so
#' they stay well conditioned:
#'   E = sum(E_coef * basis)      N = sum(N_coef * basis)
#' The browser differentiates this analytically to get the local inverse it needs
#' for drawing (see chunkTransform in paintbrush.js).
paintTransform2056 <- function(refLng, refLat, halfSpanKm = 15, n = 9){
  ref <- sf::st_coordinates(
    sf::st_transform(sf::st_sfc(sf::st_point(c(refLng, refLat)), crs = 4326), 3857)
  )
  X0 <- unname(ref[1, 1])
  Y0 <- unname(ref[1, 2])

  g  <- seq(-halfSpanKm, halfSpanKm, length.out = n)
  gr <- expand.grid(u = g, v = g)
  q  <- sf::st_coordinates(sf::st_transform(
    sf::st_sfc(sf::st_multipoint(cbind(X0 + gr$u * 1000, Y0 + gr$v * 1000)), crs = 3857), 2056
  ))

  B <- cbind(1, gr$u, gr$v, gr$u^2, gr$u * gr$v, gr$v^2)
  list(X0 = X0, Y0 = Y0,
       E = unname(as.numeric(qr.solve(B, q[, 1]))),
       N = unname(as.numeric(qr.solve(B, q[, 2]))))
}

#' Row-run encoding of a painted SpatRaster, for shipping to the browser.
#'
#' Returns an unnamed list of list(id = <category>, runs = c(row, colStart, count, ...)),
#' with `row`/`colStart` in global grid indices (see PAINT_RES). Painted areas are
#' contiguous by construction, so this compresses a disc of a few thousand cells
#' into a couple of hundred numbers.
rasterToRuns <- function(rast, res = PAINT_RES){
  if(is.null(rast)) return(list())

  m <- terra::as.matrix(rast, wide = TRUE)
  if(length(m) == 0) return(list())
  m[is.na(m)] <- 0
  if(all(m == 0)) return(list())

  e      <- as.vector(terra::ext(rast))
  col0   <- round(e[["xmin"]] / res)       #global col of raster column 1
  rowTop <- round(e[["ymax"]] / res) - 1   #global row of raster row 1

  #a sentinel column keeps rle() runs from spanning the end of one row into the
  #start of the next, so one rle() over the whole matrix does the whole job
  W  <- ncol(m) + 1L
  rr <- rle(as.vector(t(cbind(m, 0))))

  ends   <- cumsum(rr$lengths)
  starts <- ends - rr$lengths + 1L
  keep   <- rr$values != 0
  if(!any(keep)) return(list())

  starts <- starts[keep]; lens <- rr$lengths[keep]; vals <- as.integer(rr$values[keep])
  gRow <- rowTop - ((starts - 1L) %/% W)
  gCol <- col0   + ((starts - 1L) %%  W)

  unname(lapply(split(seq_along(vals), vals), function(i){
    list(id = vals[i][1], runs = as.vector(rbind(gRow[i], gCol[i], lens[i])))
  }))
}

#' Merge row-run encoded cells from the browser into a version's SpatRaster.
#'
#' This is the whole server-side cost of painting: decode runs to cell indices,
#' grow the raster to cover them, one vectorised write. No reprojection, no PNG,
#' no merge of overlapping rasters - the browser already owns the display, so R
#' only has to persist what was painted.
applyPaintRuns <- function(existing, runsByCat, res = PAINT_RES){
  if(is.null(runsByCat) || length(runsByCat) == 0) return(existing)

  rRow <- rCol <- rLen <- rId <- numeric(0)
  for(entry in runsByCat){
    rn <- as.numeric(unlist(entry$runs))
    if(length(rn) < 3) next
    i <- seq(1, length(rn) - 2, by = 3)
    rRow <- c(rRow, rn[i]); rCol <- c(rCol, rn[i + 1]); rLen <- c(rLen, rn[i + 2])
    rId  <- c(rId, rep(as.numeric(entry$id), length(i)))
  }
  if(length(rRow) == 0) return(existing)

  #extent the incoming cells need. It is grid-aligned by construction, so the
  #union with an existing raster's extent is too - no resampling can creep in.
  box <- c(xmin = min(rCol) * res, xmax = (max(rCol + rLen - 1) + 1) * res,
           ymin = min(rRow) * res, ymax = (max(rRow) + 1) * res)

  out <- if(is.null(existing)){
    terra::rast(terra::ext(box[["xmin"]], box[["xmax"]], box[["ymin"]], box[["ymax"]]),
                resolution = res, crs = "EPSG:2056", vals = NA)
  }else{
    old <- as.vector(terra::ext(existing))
    terra::extend(existing, terra::ext(min(box[["xmin"]], old[["xmin"]]),
                                       max(box[["xmax"]], old[["xmax"]]),
                                       min(box[["ymin"]], old[["ymin"]]),
                                       max(box[["ymax"]], old[["ymax"]])))
  }

  #runs -> individual cells -> terra cell numbers
  cols <- rep(rCol, rLen) + (sequence(rLen) - 1)
  rows <- rep(rRow, rLen)
  ids  <- rep(rId,  rLen)

  eo     <- as.vector(terra::ext(out))
  colIdx <- cols - round(eo[["xmin"]] / res) + 1
  rowIdx <- round(eo[["ymax"]] / res) - rows
  cells  <- (rowIdx - 1) * terra::ncol(out) + colIdx

  v <- terra::values(out)
  v[cells] <- ids
  terra::values(out) <- v
  out
}
