#' Paint materials, one row per button. `level` says which of the two stacked
#' rasters a stroke of that material lands in: "ground" -> paintedRaster,
#' "canopy" -> canopyRaster, "both" -> the same cells in each. The canopy colors
#' are deliberately darker than their ground counterparts (artificial/vegetation)
#' so the two levels stay distinguishable when the canopy layer is drawn over the
#' ground layer.
#'
#' Vegetation is split by height across the two levels rather than duplicated:
#' "bush" is ground-level woody growth that does not reach a canopy (roughly
#' 0.5-3 m), while anything taller is "canopy_tree" on the canopy level. That is
#' why there is no ground-level tree - a tree's crown is the canopy, and what it
#' stands on is the ground class underneath it. The three greens are ordered by
#' height so the map reads bottom-up: lightgreen grass, #6aa84f bush, #14532d
#' canopy.
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
  name  = c("grass", "bush", "artificial", "natural", "water",
            "canopy_artificial", "canopy_tree", "artificial_block"),
  level = c(rep("ground", 5), rep("canopy", 2), "both"),
  hex   = c("lightgreen", "#6aa84f", "grey", "#a05a3c", "dodgerblue",
            "#3f3f3f", "#14532d", "#1f1f1f"),
  stringsAsFactors = FALSE
)

#' Resolution of the painted grid, in metres of EPSG:2056. Cells are indexed
#' globally by (col, row) = (floor(E/res), floor(N/res)), so every painted cell
#' - whatever version or session it came from - lands on the same grid.
#'
#' 1 m so that a 1 m path is one cell rather than a fifth of one; this is the
#' same grid generate_ground_canopy_CH.r builds the national land cover on, and
#' the two have to agree for a painted stroke and a surveyed surface to address
#' the same cell.
#'
#' Changing this invalidates painted rasters saved under a different value: the
#' raster carries its own resolution, but rasterToRuns() converts to *global*
#' indices using this constant, so a 5 m raster read with PAINT_RES = 1 would be
#' drawn at a fifth of its size and in the wrong place. There is no in-repo
#' state affected (the saved .RData files are step-6 network objects, not paint
#' layers), but a user's own saved network from before this change is not
#' portable across it.
PAINT_RES <- 1

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
#' latitude, which over +/-5 km already costs ~1.5 m, growing quadratically
#' beyond that. That was a third of a cell when the grid was 5 m; at PAINT_RES
#' = 1 it is a cell and a half, so the fit carries more weight now, not less.
#' Adding the three second-order terms absorbs exactly that effect and brings
#' the residual down to millimetres across the whole area a user could plausibly
#' pan to - comfortably sub-cell at any resolution this grid is likely to take.
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

#' Where the national land cover rasters built by generate_ground_canopy_CH.r
#' live. They are gigabytes, so they are not in the package - point the option
#' (or VFT_LANDCOVER_DIR) at wherever the build wrote them.
paintLandcoverDir <- function(){
  getOption("vft.landcoverDir",
            Sys.getenv("VFT_LANDCOVER_DIR",
                       "C:/Users/frueh/Documents/Local Data/landcover"))
}

#' Seed a version's paint layers from the national land cover.
#'
#' Returns list(ground, canopy) of SpatRasters on the paint grid, or NULL if the
#' rasters are missing or the area asked for is too big to ship. Feeding the
#' result into `paintedRaster`/`canopyRaster` is what makes a fresh version open
#' pre-filled with reality rather than blank; from then on applyPaintRuns()
#' merges the user's strokes straight onto it.
#'
#' Three things make this a crop and nothing more:
#'   - the rasters are already on the PAINT_RES grid indexed from the LV95
#'     origin, so snapping the window to that grid means the cells line up
#'     exactly and no resample can creep in;
#'   - class ids *are* PAINT_CATEGORIES ids, so no reclassification is needed;
#'   - class 0 means "nothing attested" in the ground raster and "open sky" in
#'     the canopy one, and rasterToRuns() already treats 0 as unpainted, so both
#'     fall out as blank canvas for free.
#'
#' `max_cells` is the guard. What it has to protect against changed with the
#' transport, so the number is worth justifying rather than guessing at.
#'
#' Under the old row-run encoding the binding constraint was payload: 1 m land
#' cover breaks a run at every kerb, so a 1500 m window was ~310k runs and
#' 5.7 MB of JSON, and a 2800 m one 15.3 MB - enough to block the single shared
#' R thread for everyone. That capped useful AOIs at ~1.4 km.
#'
#' paintLandcoverBaselinePNG() sends the same 1500 m window as a 158 KB PNG, so
#' payload is no longer what binds. Browser memory is: the baseline decodes into
#' RGBA chunk canvases at 4 bytes a cell, so 12 M cells is ~48 MB of canvas -
#' about a 3.4 km square, and roughly a 700 KB message. Past that the tab starts
#' paying for land cover it cannot show at a useful zoom anyway.
#'
#' Over the ceiling this returns NULL rather than quietly wedging the browser,
#' because the baseline is a convenience and failing to load it must never cost
#' you the map.
paintLandcoverSeed <- function(aoi, buffer_m = 250, max_cells = 12e6,
                               dir = paintLandcoverDir(), res = PAINT_RES){
  if(is.null(aoi)) return(NULL)
  f_ground <- file.path(dir, sprintf("ground_CH_%gm.tif", res))
  f_canopy <- file.path(dir, sprintf("canopy_CH_%gm.tif", res))
  if(!file.exists(f_ground) || !file.exists(f_canopy)) return(NULL)

  bb <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(aoi)), 2056))
  #snap outward to the paint grid: floor the minima, ceiling the maxima, so the
  #window is a whole number of cells and shares the global grid's cell edges
  e <- terra::ext(floor((bb[["xmin"]] - buffer_m) / res) * res,
                  ceiling((bb[["xmax"]] + buffer_m) / res) * res,
                  floor((bb[["ymin"]] - buffer_m) / res) * res,
                  ceiling((bb[["ymax"]] + buffer_m) / res) * res)

  n <- ((terra::xmax(e) - terra::xmin(e)) / res) * ((terra::ymax(e) - terra::ymin(e)) / res)
  if(n > max_cells){
    warning(sprintf("land cover seed skipped: %.1f M cells > max_cells (%.1f M)",
                    n / 1e6, max_cells / 1e6))
    return(NULL)
  }

  g <- terra::rast(f_ground)
  #an AOI outside the built extent crops to nothing; that is a miss, not an error
  if(terra::relate(terra::ext(g), e, "intersects")[1] == FALSE) return(NULL)
  e <- terra::intersect(e, terra::ext(g))

  list(ground = terra::crop(g, e),
       canopy = terra::crop(terra::rast(f_canopy), e))
}

#' The land cover baseline for an area, as PNGs the browser can decode directly.
#'
#' Returns list(ground, canopy, col0, rowTop, w, h) for the "paint-base-load"
#' message, or NULL if there is nothing to send.
#'
#' The PNG is 8-bit greyscale in which *the pixel value is the class id* - 0..8,
#' not a colour. Colour is applied in the browser from PAINT_CATEGORIES, which
#' keeps `hex` the single source of truth for a material's colour exactly as the
#' rest of the paint system does, and means a palette change needs no rebuild.
#'
#' This is the transport rasterToRuns() is wrong for. Run-length encoding was
#' designed for brush strokes - contiguous discs that collapse to a few hundred
#' numbers. Land cover at 1 m is the opposite: every kerb and building edge
#' breaks a run, so a 1500 m window is ~185k runs and 3.4 MB of JSON, taking
#' ~810 ms to encode. The same window is a 158 KB PNG in ~190 ms, because a
#' 9-value class raster is precisely what PNG's filters are good at.
#'
#' `col0`/`rowTop` are the global grid indices of the top-left cell, in the same
#' convention rasterToRuns() emits, so the baseline lands cell-for-cell under
#' anything painted.
#' Encoded baselines, keyed by the window they cover.
#'
#' Process-wide rather than per session, and safe to be: the entry is derived
#' purely from the national rasters, which are immutable, so it holds no user
#' state and two sessions on the same area legitimately get the same bytes. That
#' matters on this deployment, which is a single R process by design.
#'
#' The observer that sends this re-runs on every version and context switch
#' while the area stays put, so without a cache the same ~1.5 s encode would be
#' repeated on each one, blocking the shared thread every time.
.paintBaseCache <- new.env(parent = emptyenv())
.PAINT_BASE_CACHE_MAX <- 8

paintLandcoverBaselinePNG <- function(aoi, ..., cache = TRUE){
  key <- NULL
  if(cache && !is.null(aoi)){
    bb  <- try(sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(aoi)), 2056)),
               silent = TRUE)
    if(!inherits(bb, "try-error")){
      #rounded to the metre: a bbox that differs in the 9th decimal is the same
      #window, and float noise must not cost a cache miss
      key <- paste(c(round(as.numeric(bb)), PAINT_RES, paintLandcoverDir()),
                   collapse = "|")
      hit <- .paintBaseCache[[key]]
      if(!is.null(hit)) return(hit$value)
    }
  }

  seed <- paintLandcoverSeed(aoi, ...)
  if(is.null(seed)) return(NULL)

  encode <- function(r){
    v <- terra::values(r)
    v[is.na(v)] <- 0
    #byrow: terra hands back cells row-major from the north-west, which is also
    #PNG's row order, so the image needs no flip
    m <- matrix(as.numeric(v), nrow = terra::nrow(r), byrow = TRUE)
    f <- tempfile(fileext = ".png")
    on.exit(unlink(f), add = TRUE)
    #writePNG wants [0,1] and quantises back with round(v * 255), so dividing by
    #255 round-trips the class id exactly
    png::writePNG(m / 255, f)
    paste0("data:image/png;base64,",
           jsonlite::base64_enc(readBin(f, "raw", file.info(f)$size)))
  }

  e   <- terra::ext(seed$ground)
  out <- list(ground = encode(seed$ground),
              canopy = encode(seed$canopy),
              col0   = round(terra::xmin(e) / PAINT_RES),
              rowTop = round(terra::ymax(e) / PAINT_RES) - 1,
              w      = terra::ncol(seed$ground),
              h      = terra::nrow(seed$ground))

  if(!is.null(key)){
    #plain FIFO on insertion time: entries are ~0.5 MB, and the access pattern is
    #a handful of study areas, so there is nothing an LRU would buy here
    ks <- ls(.paintBaseCache)
    if(length(ks) >= .PAINT_BASE_CACHE_MAX){
      stamps <- vapply(ks, function(k) .paintBaseCache[[k]]$t, numeric(1))
      rm(list = ks[which.min(stamps)], envir = .paintBaseCache)
    }
    assign(key, list(value = out, t = as.numeric(Sys.time())), envir = .paintBaseCache)
  }
  out
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
