#' Paint materials, one row per button. `level` says which of the two stacked
#' rasters a stroke of that material lands in: "ground" -> paintedRaster,
#' "canopy" -> canopyRaster, "both" -> the same cells in each.
#'
#' Vegetation is split by height across the two levels rather than duplicated:
#' "bush" is ground-level woody growth that does not reach a canopy (roughly
#' 0.5-3 m), while anything taller is a canopy tree. That is why there is no
#' ground-level tree - a tree's crown is the canopy, and what it stands on is
#' the ground class underneath it.
#'
#' "both" exists for a building: a solid block occupies the ground and everything
#' above it, so one stroke has to fill either raster at once. It belongs to no
#' level, which is why its button is never disabled by the level switch.
#'
#' HEIGHT IS THE COLOUR. The three materials that obstruct the sky - a tree, an
#' artificial canopy, a solid block - each come as a ramp of ids, one per step
#' the height bar offers, and the nuance of the colour *is* the metres.
#'
#' That is the whole mechanism, and it is chosen because a painted cell is ONE
#' INTEGER everywhere it travels: the browser's cell map, the {id, runs} wire
#' format, the 8-bit class PNG, the SpatRaster cell value. A second per-cell
#' channel would have meant a third raster beside paintedRaster/canopyRaster, a
#' third wrap() across the async boundary and a new dimension on the heat cache.
#' More ids cost none of that, and paintbrush.js needs no change at all -
#' paintInitPayload() already ships `colors` and `levels` keyed by id, so a new
#' row here is a new brush colour in the browser for free.
#'
#' `base` is what a variant is MADE OF, and it is the seam that keeps every
#' thermal table (heat_materials.csv, heat_decay.csv) keyed on the nine original
#' classes: a 3 m tree crown and a 25 m one are the same stuff, and only their
#' geometry differs. heatRaster() substitutes the variants away with
#' paintBaseRaster() before the local and advective terms, and keeps the RAW ids
#' for shade, SVF and the wall term - those are the three that read a height.
#'
#' `height` is metres, and it is the LABEL the bar prints. heat_geometry.csv
#' holds the value the model actually marches with (one height_<name> row per id
#' here), because that file is where the project retunes its coefficients;
#' verify_heat_model.R asserts the two agree rather than picking one.
#'
#' IDS 1-9 ARE FROZEN. They are what generate_ground_canopy_CH.r wrote into the
#' national land cover and what every version saved before the height bar
#' carries. Each of the three obstruction materials keeps its original id as the
#' DEFAULT step of its ramp, so a surveyed tree and a year-old saved version
#' both still mean exactly what they always did. New steps are APPENDED, never
#' inserted: id 19 is the 15 m block and sits in the middle of its ramp by
#' height, so this table's row order is not the ramp order. vftHeightRamps()
#' sorts by `height`, and nothing should read the ramps off the row order here.
#'
#' `hex` is the single source of truth for a material's color: it is what the
#' brush cursor is drawn in and what the browser fills painted cells with. There
#' is no server-side palette any more - R never renders the painted layers. The
#' eight material buttons in newVersions_ui.R restate their hexes in inline CSS
#' and have to be edited together with this table.
#'
#' THE GREYS DO NOT COLLIDE. Ground "artificial" is plain grey (#808080); the
#' canopy-artificial ramp sits entirely above it (light grey -> grey, darkening
#' with height) and the block ramp entirely below it (grey -> black). That is
#' what replaced the older "canopy is always darker than its ground counterpart"
#' rule, which cannot survive two grey ramps and a grey ground material at once.
#'
#' "canopy_cleared" (9) is the one material with no button. It is what a plan
#' import (planimport.js) writes on the canopy level under a ground material,
#' so that a plan showing lawn where a tree stands today removes the tree: the
#' plan describes its whole footprint. It cannot be 0, because 0 on the wire is
#' "erase", which reveals the baseline - i.e. the very tree being removed. The
#' browser draws it as a hole punched through the baseline (`holes` in
#' paintInitPayload) and the heat model counts it as open sky (HEAT_CANOPY).
PAINT_CATEGORIES <- data.frame(
  id     = 1:19,
  name   = c("grass", "bush", "artificial", "natural", "water",
             "canopy_artificial", "canopy_tree", "artificial_block",
             "canopy_cleared",
             "canopy_tree_3", "canopy_tree_10", "canopy_tree_20",
             "canopy_tree_25", "canopy_artificial_10", "canopy_artificial_15",
             "artificial_block_5", "artificial_block_25", "artificial_block_50",
             "artificial_block_15"),
  level  = c(rep("ground", 5), rep("canopy", 2), "both", "canopy",
             rep("canopy", 6), rep("both", 4)),
  #the three ramps read bottom-up in metres. Tree: five greens from #006400 at
  #3 m to #002100 at 25 m. Canopy artificial: light grey to grey, all lighter
  #than ground grey. Block: grey to black, all darker than it.
  hex    = c("lightgreen", "#6aa84f", "grey", "#a05a3c", "dodgerblue",
             "#e0e0e0", "#004200", "#3d3d3d", "transparent",
             "#006400", "#005300", "#003200", "#002100",
             "#c0c0c0", "#a0a0a0",
             "#5a5a5a", "#202020", "#000000", "#2e2e2e"),
  #only the eight base materials get a button; a height variant is reached
  #through the height bar, which arms it by id (see armBrush() in
  #newVersions_server.R) and so never needs a row in PAINT_BUTTONS
  button = c(rep(TRUE, 8), rep(FALSE, 11)),
  base   = c(1:9, 7L, 7L, 7L, 7L, 6L, 6L, 8L, 8L, 8L, 8L),
  height = c(rep(NA_real_, 5), 5, 15, 10, NA,
             3, 10, 20, 25, 10, 15, 5, 25, 50, 15),
  stringsAsFactors = FALSE
)

#' Materials drawn as a hole in the layer rather than as a colour.
PAINT_HOLE_IDS <- PAINT_CATEGORIES$id[PAINT_CATEGORIES$name == "canopy_cleared"]

#' Ids that are a height variant of some other material.
PAINT_VARIANT_IDS <- PAINT_CATEGORIES$id[PAINT_CATEGORIES$id != PAINT_CATEGORIES$base]

#' Ids that carry a height, i.e. the three obstruction ramps. HEAT_OBSTRUCTION_IDS
#' and heatHeights() are both derived from this, so they cannot drift apart.
PAINT_HEIGHT_IDS <- PAINT_CATEGORIES$id[!is.na(PAINT_CATEGORIES$height)]

#' Class ids that change the obstruction height field.
#'
#' Only these make a shadow, occlude sky or present a wall, so a repaint that
#' touches none of them cannot move the shade, SVF or wall terms - and those are
#' 0.61 s of a 2.4 s read-out over central Sion. Every other id is a ground
#' material that reaches the output through the local and advective terms alone.
#'
#' DERIVED, not written out, and that matters more since the height bar than it
#' did before it. These ids used to be c(6L, 7L, 8L) and a ramp step added to
#' PAINT_CATEGORIES without a matching edit here would not fail, it would give a
#' WRONG ANSWER QUIETLY: repaint a tree from 10 m to 25 m, `touched` is
#' {11, 13}, neither is in the list, geom_dirty comes back FALSE and the cached
#' shade, SVF and wall layers are reused with the old tree still in them. Same
#' shape of bug as the stale per-bin geometry below, which cost 5 K on a real
#' sequence. heatHeights() reads the same column, so the two cannot disagree.
HEAT_OBSTRUCTION_IDS <- as.integer(PAINT_HEIGHT_IDS)

#' What a variant is made of: a height variant -> its base material, anything
#' else -> itself.
#'
#' Unknown ids pass through rather than becoming NA. Both callers are
#' bookkeeping - cache invalidation and raster substitution - and a stray value
#' should be left alone there, not turned into a hole; paintLandcoverBaselinePNG()
#' is where an out-of-range class is squashed, and it does that on the way in.
paintBaseId <- function(ids){
  out <- as.integer(ids)
  i   <- match(out, PAINT_CATEGORIES$id)
  hit <- !is.na(i)
  out[hit] <- as.integer(PAINT_CATEGORIES$base[i[hit]])
  out
}

#' Substitute the height variants away, leaving a raster of base material ids.
#'
#' This is what every THERMAL lookup reads. heat_materials.csv and heat_decay.csv
#' are keyed on the nine original classes and must stay that way - a 3 m tree
#' crown and a 25 m one are made of the same thing, and giving every ramp step
#' its own set of thermal rows would say otherwise. Height reaches the model the other way, through
#' heatObstructionHeight() off the RAW raster.
#'
#' Not optional, and the failure is silent rather than loud: heatLocalTerm()
#' resolves ground classes with `others = NA`, so an unlisted block variant would
#' drop its cells out of the finished map altogether, and canopy classes with
#' `others = 0`, so an unlisted tree variant would read as open sky.
#'
#' `others = NULL` keeps everything that is not a variant, so this is a no-op on
#' a raster with no height painted in it - which is the common case, the national
#' land cover, and every version saved before the height bar existed.
paintBaseRaster <- function(r){
  if(is.null(r) || !length(PAINT_VARIANT_IDS)) return(r)
  terra::subst(r, from = as.integer(PAINT_VARIANT_IDS),
               to   = paintBaseId(PAINT_VARIANT_IDS), others = NULL)
}

#' Readable text over a swatch: black on a light one, white on a dark one.
#'
#' The height ramps run from #e0e0e0 to #000000, so no single fixed foreground
#' works across them. Rec. 601 luma rather than a hand-picked list, so a retuned
#' hex keeps a readable label without anyone remembering to flip it.
#' Named colours ("grey", "lightgreen") come back from col2rgb() too, so the
#' eight base materials can use this as well.
paintSwatchFg <- function(hex){
  vapply(hex, function(h){
    if(is.na(h) || !nzchar(h) || identical(h, "transparent")) return("black")
    rgb <- try(grDevices::col2rgb(h), silent = TRUE)
    if(inherits(rgb, "try-error")) return("black")
    luma <- 0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]
    if(luma > 140) "black" else "white"
  }, character(1), USE.NAMES = FALSE)
}

#' The height ramps, one entry per material that carries a height.
#'
#' What the height bar is built from (newVersions_ui.R) and what armBrush()
#' resolves a choice against (newVersions_server.R). Steps come back in
#' ASCENDING height; the bar reverses them so the tallest sits on top.
#'
#' `default` is the step whose id IS the base material - the surveyed value, what
#' the national land cover means and what a version saved before the height bar
#' replays as. That is what makes this feature additive rather than a migration.
vftHeightRamps <- function(){
  rows <- PAINT_CATEGORIES[!is.na(PAINT_CATEGORIES$height), ]
  lapply(split(rows, rows$base), function(g){
    g <- g[order(g$height), ]
    g$fg <- paintSwatchFg(g$hex)
    base <- g$base[1]
    list(base    = base,
         name    = PAINT_CATEGORIES$name[match(base, PAINT_CATEGORIES$id)],
         level   = g$level[1],
         default = base,
         steps   = g[, c("id", "height", "hex", "fg")])
  })
}

#' The ramp a class id belongs to, or NULL for a material with no height.
vftHeightRampOf <- function(id){
  b <- paintBaseId(id)
  r <- vftHeightRamps()
  r[[as.character(b)]]
}

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
    #ids drawn as holes through the baseline; see canopy_cleared above
    holes     = as.list(PAINT_HOLE_IDS),
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

#' The paint-grid window an area needs, as a terra extent.
#'
#' `shp` is the study area already buffered and in EPSG:2056. The window is
#' snapped outward to the global paint grid - floor the minima, ceiling the
#' maxima - so it is a whole number of cells and shares the national rasters'
#' cell edges. That is what makes paintLandcoverSeed()'s read a crop rather than
#' a resample.
paintWindowExt <- function(shp, res = PAINT_RES){
  bb <- sf::st_bbox(shp)
  terra::ext(floor(bb[["xmin"]] / res) * res, ceiling(bb[["xmax"]] / res) * res,
             floor(bb[["ymin"]] / res) * res, ceiling(bb[["ymax"]] / res) * res)
}

#' How many paint cells a window covers - the number `max_cells` caps. See
#' paintLandcoverSeed() for what the ceiling is actually protecting.
paintWindowCells <- function(e, res = PAINT_RES){
  ((terra::xmax(e) - terra::xmin(e)) / res) * ((terra::ymax(e) - terra::ymin(e)) / res)
}

#' Is this area past the ceiling a land cover baseline can be built for?
#'
#' Answers ahead of time the question paintLandcoverSeed() otherwise answers by
#' returning NULL halfway through a render, which is far too late to tell the
#' user anything useful. Two places need it in advance: step 1 warns while the
#' area is still being chosen, and newVersions disables the Hitzeminderung
#' context for an area that is already too big, so the choice is never offered
#' with nothing behind it.
#'
#' Both have to agree with the seed exactly - a warning for an area that then
#' works is as bad as no warning at all - so the window arithmetic and the
#' default ceiling are shared with it rather than restated here. Only the front
#' half of the seed is repeated (transform, union, buffer), because the rest
#' reads rasters and this must not.
#'
#' Deliberately NA-safe in one direction: an area that cannot be measured - no
#' geometry, a CRS that will not transform - is reported as NOT too large. A
#' false TRUE switches a working feature off behind a misleading label; a false
#' FALSE lands on the blank-canvas path, which already prints its own diagnosis.
paintAreaTooLarge <- function(aoi, buffer_m = 250, max_cells = 40e6,
                              res = PAINT_RES){
  if(is.null(aoi)) return(FALSE)
  if(inherits(aoi, c("sf", "data.frame")) && nrow(aoi) == 0) return(FALSE)
  geom <- try(sf::st_geometry(aoi), silent = TRUE)
  if(inherits(geom, "try-error") || length(geom) == 0) return(FALSE)
  #the same assumption the seed makes, and for the same reason; it warns there,
  #where the projection actually decides what gets read
  if(is.na(sf::st_crs(geom))) sf::st_crs(geom) <- 4326
  n <- try({
    shp <- sf::st_union(sf::st_transform(geom, 2056))
    if(buffer_m > 0) shp <- sf::st_buffer(shp, buffer_m)
    paintWindowCells(paintWindowExt(shp, res), res)
  }, silent = TRUE)
  if(inherits(n, "try-error") || !is.finite(n)) return(FALSE)
  n > max_cells
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
#' `win` narrows the crop to an extent inside the area, for a caller that needs
#' only part of it and does not want to pay for the rest. Everything else is
#' unchanged - the same buffer, the same polygon mask - so a windowed read is
#' cell-for-cell identical to the matching part of a full one. That is what lets
#' heatRaster() rebuild only the cells a brush stroke touched.
paintLandcoverSeed <- function(aoi, buffer_m = 250, max_cells = 40e6,
                               dir = paintLandcoverDir(), res = PAINT_RES,
                               mask = TRUE, win = NULL){
  vftTime("paint:landcoverSeed", {
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
  e <- paintWindowExt(shp, res)
  #narrowed BEFORE the cell count, so a windowed read of a huge area is not
  #refused for the size of the area it is a window into
  if(!is.null(win)){
    if(terra::relate(e, win, "intersects")[1] == FALSE) return(NULL)
    e <- terra::intersect(e, win)
  }

  n <- paintWindowCells(e, res)
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
  })
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
  vftTime("paint:baselinePNG", {
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
  })
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
  vftTime("paint:rasterToRuns", {
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
  })
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
#' Lay a version's edits over a baseline layer.
#'
#' Split out because the heat model needs the same overlay for both levels and
#' would otherwise re-crop the national rasters to get at it.
#'
#' Edits are on the same LV95 grid as the baseline by construction, so this
#' aligns without resampling. Extending to the union first keeps a stroke that
#' runs just outside the cropped baseline from being silently dropped, and 0 is
#' dropped rather than written because it means "erased" on the wire as well as
#' "never painted" - writing it would punch holes in the baseline.
paintOverlayEdits <- function(base, edits){
  if(is.null(edits)) return(base)
  if(is.null(base))  return(terra::ifel(edits == 0, NA, edits))

  e     <- terra::union(terra::ext(base), terra::ext(edits))
  base  <- terra::extend(base, e)
  edits <- terra::extend(edits, e)
  terra::cover(terra::ifel(edits == 0, NA, edits), base)
}

paintCompositeRaster <- function(edits, aoi, level = c("ground", "canopy"), ...){
  level <- match.arg(level)
  seed  <- paintLandcoverSeed(aoi, ...)
  if(is.null(seed)) return(edits)
  paintOverlayEdits(seed[[level]], edits)
}

#' Decode a class-id PNG sent by a plan import into a patch on the paint grid.
#'
#' The reverse of paintLandcoverBaselinePNG()'s transport: an 8-bit image whose
#' pixel value *is* the class id, `w` x `h` cells with its top-left cell at the
#' global indices (`col0`, `rowTop`). The browser writes R = G = B = id with
#' alpha 255, so the red channel is read and the rest ignored.
#'
#' 0 becomes NA, not 0: in an import it means "the plan says nothing here", and
#' writing it would erase whatever was painted before. Anything outside the
#' known ids becomes NA too, for the same reason paintLandcoverBaselinePNG()
#' squashes them - a stray value would count as painted and draw nothing.
#'
#' Returns NULL for an image that does not match the declared size, since a
#' mismatched patch would land shifted rather than fail visibly.
paintDecodeClassPNG <- function(uri, col0, rowTop, w, h, res = PAINT_RES){
  if(is.null(uri) || !nzchar(uri)) return(NULL)
  b64 <- sub("^data:image/png;base64,", "", uri)
  img <- png::readPNG(jsonlite::base64_dec(b64))
  ch  <- if(length(dim(img)) == 3) img[, , 1] else img
  if(!identical(dim(ch), c(as.integer(h), as.integer(w)))) return(NULL)
  v <- round(ch * 255)
  v[!v %in% PAINT_CATEGORIES$id] <- NA
  if(all(is.na(v))) return(NULL)
  xmin <- col0 * res
  ymax <- (rowTop + 1) * res
  terra::rast(terra::ext(xmin, xmin + w * res, ymax - h * res, ymax),
              resolution = res, crs = "EPSG:2056",
              vals = as.vector(t(v)))
}

#' Lay a patch over a version's edits: patch cells win, NA leaves them alone.
#'
#' The same union-extend-cover as paintOverlayEdits(), for the same reason -
#' both sit on the global paint grid, so this aligns without resampling.
paintApplyPatch <- function(existing, patch){
  if(is.null(patch))    return(existing)
  if(is.null(existing)) return(patch)
  e        <- terra::union(terra::ext(existing), terra::ext(patch))
  existing <- terra::extend(existing, e)
  patch    <- terra::extend(patch, e)
  terra::cover(patch, existing)
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
  vftTime("paint:applyRuns", {
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
  })
}

#' Ceiling on a plan import's footprint, in cells per level.
#'
#' Tighter than the baseline's 40 M because an import does not stay an image in
#' the browser: every cell becomes an entry in the paint grid's cell map, which
#' is what lets the eraser and later strokes treat imported cells like painted
#' ones. 4 M cells is a 2 km square at 1 m. planimport.js clips to the study
#' area first and refuses past this; the observer checks it again.
PLAN_IMPORT_MAX_CELLS <- 4e6

#' The words the plan import panel shows, translated.
#'
#' The panel is built by planimport.js rather than by Shiny (its rows depend on
#' the colours found in the uploaded image), so the strings travel in a message.
#' `materials` is keyed by class id, for the per-colour dropdown; 0 is "ignore".
planImportLabels <- function(tr){
  t <- function(x) tr$t(x)
  list(
    maxCells   = PLAN_IMPORT_MAX_CELLS,
    place      = t("Plan platzieren"),
    placeHint  = t("Ziehen Sie den Plan an seinen Platz und ziehen Sie an seinen Ecken, bis er passt. Die Karte kann weiterhin verschoben und gezoomt werden. Der Plan muss nach Norden ausgerichtet sein."),
    opacity    = t("Deckkraft"),
    page       = t("Seite"),
    nextStep   = t("Weiter"),
    back       = t("Zurueck"),
    apply      = t("Anwenden"),
    cancel     = t("Abbrechen"),
    mapColors  = t("Farben zuordnen"),
    original   = t("Original"),
    assigned   = t("Zuordnung"),
    pickColor  = t("Farbe aufnehmen"),
    removeColor = t("Farbe entfernen"),
    readError  = t("Die Datei konnte nicht gelesen werden."),
    outside    = t("Der Plan liegt ausserhalb des Untersuchungsgebiets."),
    tooLarge   = t("Der Plan ist zu gross."),
    saveError  = t("Der Plan konnte nicht gespeichert werden."),
    materials  = list(
      "0" = t("Ignorieren"),
      "7" = t("Baum"),
      "6" = t("Kuenstliche Krone"),
      "1" = t("Gras"),
      "2" = t("Busch"),
      "3" = t("Kuenstlich"),
      "4" = t("Natuerlich"),
      "5" = t("Wasser"),
      "8" = t("Kuenstlicher Block")
    )
  )
}
