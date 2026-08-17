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

#' Opacities of the painted layers. The ground layer is dimmed (rather than
#' hidden) while canopy is being edited, so you can still see what you are
#' painting canopy over. These are applied by the browser as the CSS opacity of
#' the *map pane*, not of individual overlays: that is what keeps overlapping
#' strokes from compounding into darker patches.
#'
#' One opacity per level, applied to the land cover baseline and the paint
#' together: they share a canvas, so a painted grass cell and a surveyed grass
#' cell are the same colour and cannot be told apart. The map reads as one
#' surface rather than as edits highlighted against a backdrop.
#'
#' The ground dim is the only distinction drawn anywhere, and it is between
#' levels, not between paint and baseline.
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
#' payload stopped being what binds, and the guard became about browser memory.
#'
#' Bounding-box cells then turned out to be a poor proxy for memory too. Chunk
#' canvases are only allocated where a classified pixel lands, and the image is
#' decoded in strips rather than in one buffer, so a study area of 17 scattered
#' polygons costs its 0.03 km2 of content and not its 21 km2 of bounding box. A
#' 12 M ceiling rejected exactly that case - a real study area whose baseline
#' would have been almost entirely empty, and cheap.
#'
#' What the window size still bounds is the crop read and the decode loop, both
#' linear in it and both cheap, so the ceiling sits at 40 M cells (a ~6.3 km
#' square). Over it this returns NULL rather than quietly wedging the browser,
#' because the baseline is a convenience and failing to load it must never cost
#' you the map.
#'
#' `aoi` is the study area drawn in step 1 (`r$polygonsList`, EPSG:4326). It is
#' buffered by `buffer_m` and the result is used two ways: its bounding box sets
#' the crop window, and the buffered shape itself masks the result, so an
#' irregular study area does not drag in a rectangle of land cover around it.
#' Masked-out cells become NA, which paintLandcoverBaselinePNG() writes as class
#' 0 and the browser renders as nothing - the baseline ends up the shape of the
#' study area. Pass `mask = FALSE` for the plain bounding box.
paintLandcoverSeed <- function(aoi, buffer_m = 250, max_cells = 40e6,
                               dir = paintLandcoverDir(), res = PAINT_RES,
                               mask = TRUE){
  if(is.null(aoi)) return(NULL)
  if(inherits(aoi, c("sf", "data.frame")) && nrow(aoi) == 0) return(NULL)
  f_ground <- file.path(dir, sprintf("ground_CH_%gm.tif", res))
  f_canopy <- file.path(dir, sprintf("canopy_CH_%gm.tif", res))
  if(!file.exists(f_ground) || !file.exists(f_canopy)) return(NULL)

  #union first: step 1 keeps a single polygon today, but the polygon list is a
  #list, and a multi-part area must give one shape rather than one per part
  geom <- sf::st_geometry(aoi)
  #A CRS-less geometry would fail st_transform and take the whole baseline with
  #it. The app already assumes 4326 for geometries that arrive without one (see
  #the border load in step1_server.R), so do the same rather than give up - but
  #say so, because silently guessing a projection is how things end up 100 km
  #from where they belong.
  if(is.na(sf::st_crs(geom))){
    warning("land cover AOI has no CRS; assuming EPSG:4326")
    sf::st_crs(geom) <- 4326
  }
  shp <- try(sf::st_union(sf::st_transform(geom, 2056)), silent = TRUE)
  if(inherits(shp, "try-error")) return(NULL)
  if(buffer_m > 0) shp <- sf::st_buffer(shp, buffer_m)
  bb <- sf::st_bbox(shp)

  #snap outward to the paint grid: floor the minima, ceiling the maxima, so the
  #window is a whole number of cells and shares the global grid's cell edges
  e <- terra::ext(floor(bb[["xmin"]] / res) * res, ceiling(bb[["xmax"]] / res) * res,
                  floor(bb[["ymin"]] / res) * res, ceiling(bb[["ymax"]] / res) * res)

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

  out <- list(ground = terra::crop(g, e),
              canopy = terra::crop(terra::rast(f_canopy), e))
  if(mask){
    mv  <- terra::vect(shp)
    out <- lapply(out, function(r) terra::mask(r, mv))
  }
  out
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
    #the whole outline, not just its bounding box: the result is masked to the
    #shape now, so two different outlines sharing a bbox are different baselines
    #and must not share an entry.
    #
    #Built from rounded coordinates rather than WKT. st_as_text(digits = ) is not
    #a rounding knob - format() rejects digits = 0 outright - and the failure
    #mode is silent: the error lands in try(), the key stays NULL, and caching
    #turns itself off without a word. Rounding the coordinates to the metre is
    #both the snapping we actually want (float noise in the last decimal is the
    #same outline) and something that cannot throw.
    crd <- try(sf::st_coordinates(
                 sf::st_transform(sf::st_union(sf::st_geometry(aoi)), 2056)),
               silent = TRUE)
    if(!inherits(crd, "try-error") && length(crd)){
      key <- paste(c(round(crd[, 1]), round(crd[, 2]),
                     PAINT_RES, paintLandcoverDir()), collapse = ",")
      hit <- .paintBaseCache[[key]]
      if(!is.null(hit)) return(hit$value)
    }
  }

  seed <- paintLandcoverSeed(aoi, ...)
  if(is.null(seed)) return(NULL)

  valid <- c(0L, PAINT_CATEGORIES$id)

  encode <- function(r){
    v <- terra::values(r)
    v[is.na(v)] <- 0
    #Anything that is not a category id becomes 0 (unclassified). The national
    #build produced 776 such cells in 166 billion - always a valid class with a
    #high bit set (3 -> 67, 4 -> 68, 0 -> 128), which is the signature of memory
    #bit-flips during a long saturating run rather than of a crosswalk fault.
    #Too rare to matter statistically, but a stray 128 would miss the palette
    #and draw nothing while still counting as painted, so it is squashed at the
    #edge rather than left to surface as an unexplained hole.
    bad <- !v %in% valid
    if(any(bad)) v[bad] <- 0
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

#' Why is there no land cover baseline?
#'
#' paintLandcoverSeed() returns NULL for half a dozen unrelated reasons and says
#' nothing about which, because on the server the right response to all of them
#' is the same: carry on without a baseline. When you are looking at a blank
#' canvas and want to know why, call this with the same AOI - it walks the same
#' gates in the same order and prints where it stopped.
#'
#'   paintLandcoverDiagnose(shiny::isolate(r$polygonsList))
paintLandcoverDiagnose <- function(aoi, dir = paintLandcoverDir(), res = PAINT_RES,
                                   buffer_m = 250, max_cells = 40e6){
  say <- function(...) cat(sprintf(...), "\n", sep = "")
  say("PAINT_RES              : %s", res)
  say("land cover directory   : %s", dir)
  say("  directory exists     : %s", dir.exists(dir))
  for(w in c("ground", "canopy")){
    f <- file.path(dir, sprintf("%s_CH_%gm.tif", w, res))
    say("  %-20s: %s", basename(f), if(file.exists(f))
        sprintf("found, %.2f GB", file.size(f) / 1e9) else "MISSING")
  }
  if(is.null(aoi)){ say("AOI                    : NULL  <- nothing to crop to"); return(invisible(NULL)) }
  if(inherits(aoi, c("sf", "data.frame")) && nrow(aoi) == 0){
    say("AOI                    : 0 rows  <- nothing to crop to"); return(invisible(NULL))
  }
  geom <- sf::st_geometry(aoi)
  say("AOI class              : %s", paste(class(aoi), collapse = "/"))
  say("AOI rows               : %s", if(is.null(nrow(aoi))) length(geom) else nrow(aoi))
  say("AOI CRS                : %s", if(is.na(sf::st_crs(geom))) "NONE (will assume 4326)"
                                     else paste0("EPSG:", sf::st_crs(geom)$epsg))
  if(is.na(sf::st_crs(geom))) sf::st_crs(geom) <- 4326

  shp <- try(sf::st_union(sf::st_transform(geom, 2056)), silent = TRUE)
  if(inherits(shp, "try-error")){ say("transform to 2056      : FAILED"); return(invisible(NULL)) }
  say("AOI area               : %.3f km2", sum(as.numeric(sf::st_area(shp))) / 1e6)
  bb <- sf::st_bbox(if(buffer_m > 0) sf::st_buffer(shp, buffer_m) else shp)
  say("buffered bbox (LV95)   : %.0f %.0f %.0f %.0f",
      bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]])
  n <- ceiling((bb[["xmax"]] - bb[["xmin"]]) / res) * ceiling((bb[["ymax"]] - bb[["ymin"]]) / res)
  say("cells needed           : %.2f M  (ceiling %.0f M) %s",
      n / 1e6, max_cells / 1e6, if(n > max_cells) " <- OVER, would return NULL" else "")

  s <- suppressWarnings(paintLandcoverSeed(aoi, buffer_m = buffer_m,
                                           max_cells = max_cells, dir = dir, res = res))
  if(is.null(s)){ say("paintLandcoverSeed     : NULL  <- see gates above"); return(invisible(NULL)) }
  for(w in names(s)){
    v  <- terra::values(s[[w]])
    nz <- sum(!is.na(v) & v != 0)
    say("%-6s crop            : %d x %d, %.1f%% masked out, %.1f%% classified",
        w, terra::nrow(s[[w]]), terra::ncol(s[[w]]),
        100 * sum(is.na(v)) / length(v), 100 * nz / length(v))
    if(nz == 0) say("        ^ nothing classified here - the baseline would render blank")
  }
  msg <- suppressWarnings(paintLandcoverBaselinePNG(aoi, buffer_m = buffer_m,
                                                    max_cells = max_cells, dir = dir,
                                                    res = res, cache = FALSE))
  if(is.null(msg)){ say("baseline message       : NULL"); return(invisible(NULL)) }
  say("baseline message       : ok, %.0f KB (%d x %d, col0 %d, rowTop %d)",
      (nchar(msg$ground) + nchar(msg$canopy)) / 1e3, msg$w, msg$h, msg$col0, msg$rowTop)
  say("=> R side is fine. If the map is still blank the message is not reaching")
  say("   the browser, or paintbrush.js is a cached copy without the handler.")
  invisible(msg)
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

#' The effective land cover for a version: baseline with the user's edits on top.
#'
#' This is what downstream work should read, and what "the raster" means from
#' the outside - a version opens as the surveyed land cover, painting replaces
#' cells in it, and a reset returns it to the baseline exactly.
#'
#' Composed on demand rather than stored. A version's `paintedRaster` keeps only
#' the cells the user changed, which is what makes versions cheap (a stroke, not
#' a study area), keeps a reset to a single NULL, and keeps the browser payload
#' split between a PNG baseline and a handful of runs. The composite is fully
#' determined by the AOI and those edits, so storing it as well would be
#' duplicating derivable state - and duplicating it once per version, across
#' every session, on a single-process server.
#'
#' `level` picks which of the two the edits belong to.
paintCompositeRaster <- function(edits, aoi, level = c("ground", "canopy"), ...){
  level <- match.arg(level)
  seed  <- paintLandcoverSeed(aoi, ...)
  if(is.null(seed)) return(edits)
  base <- seed[[level]]
  if(is.null(edits)) return(base)

  #edits are on the same LV95 grid by construction, so this aligns without
  #resampling; extend to the union first so a stroke just outside the AOI is not
  #silently dropped
  e <- terra::union(terra::ext(base), terra::ext(edits))
  base  <- terra::extend(base, e)
  edits <- terra::extend(edits, e)
  #0 is "erased" on the wire as well as "unpainted", so it must not overwrite
  edits <- terra::ifel(edits == 0, NA, edits)
  terra::cover(edits, base)
}

#' Merge row-run encoded cells from the browser into a version's SpatRaster.
#'
#' Category 0 means *erase*: the browser sends it for cells the eraser cleared,
#' and writing it here is what makes that stick, since rasterToRuns() already
#' treats 0 as unpainted on the way back out. So a cleared cell round-trips as
#' "nothing painted here" and the land cover baseline shows through again.
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
