#' Build the Switzerland-wide ground and canopy land cover rasters.
#'
#' Two 5 m EPSG:2056 rasters covering all of Switzerland, classified with the
#' ids of PAINT_CATEGORIES (paintbrush_helpers.R) so the result feeds
#' rasterToRuns() directly and the heat-mitigation paint canvas can start
#' pre-filled with reality instead of blank:
#'
#'   ground  0 unclassified  1 grass/fields  2 bush  3 impervious
#'           4 natural/soil  5 water/ice     8 building
#'   canopy  0 open sky      6 artificial (bridges)  7 tree  8 building
#'
#' Classes follow *thermal* behaviour, not naturalness, because the downstream
#' use is heat mitigation: bare rock and scree store and re-radiate heat much
#' like concrete, so they are impervious; glacier and snow are the coldest,
#' highest-albedo surfaces in the country, so they join water.
#'
#' 0 means "swissTLM3D does not attest what is here" - not "outside
#' Switzerland" and not "grass". TLM3D's Bodenbedeckung deliberately omits
#' farmland and settlement open ground, so roughly a third of the country (most
#' of the Mittelland) comes out 0. Filling it with grass would manufacture
#' confidence the data does not support; leaving it at 0 keeps the gap
#' identifiable. Splitting it into farmland vs settlement needs a non-TLM3D
#' source (BFS Arealstatistik, OSM landuse) and is not done here.
#'
#' NOTE: this file lives in R/ and is therefore sourced on every package load.
#' It must stay function definitions only - the driver at the bottom is inside
#' `if (FALSE)` for exactly that reason. Never put a top-level terra::rast() or
#' sf::st_read() here: the sources are a 3.1 GB TIFF and a 10.8 GB GeoPackage.


# ---------------------------------------------------------------- config ----

LC_TLM3D <- paste0("C:/Users/frueh/Documents/Local Data/TLM3D/",
                   "swisstlm3d_2026-02-24_2056_5728.gpkg/SWISSTLM3D_2026_LV95_LN02.gpkg")
LC_VHM     <- "C:/Users/frueh/Documents/Local Data/VHM_ALS_5m.tif"
LC_OUT_DIR <- "C:/Users/frueh/Documents/Local Data/landcover"

#the grid is not ours to choose: PAINT_RES is 5 m in EPSG:2056 indexed from the
#LV95 origin, so every extent below has to be a multiple of 5. VHM_ALS_5m.tif
#already *is* this grid (72000 x 46000 cells), which is why nothing here ever
#resamples - it is the template.
LC_RES <- 5
LC_EXT <- c(xmin = 2480000, xmax = 2840000, ymin = 1070000, ymax = 1300000)

#20000 x 23000 m divides LC_EXT into exactly 18 x 10 tiles of 4000 x 4600
#cells, so no tile is partial.
LC_TILE_M <- c(20000, 23000)

#vegetation height cuts, in metres. Below LC_BUSH_MIN is grass or bare ground,
#in between is bush, at or above LC_CANOPY_MIN it is a crown and belongs to the
#canopy raster instead.
LC_BUSH_MIN   <- 0.5
LC_CANOPY_MIN <- 3

#roads narrower than this are dropped. At 5 m a 1 m footpath is a fifth of a
#cell, and burning it would turn every hiking trail into a solid asphalt ribbon.
LC_MIN_ROAD_WIDTH <- 3

LC_GDAL <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES")

#SWISSIMAGE for the aerial QA pass, read through GDAL's WMTS driver so only the
#requested window is fetched. The service is published in LV95, so nothing is
#reprojected. zoom_level is not optional: the capabilities default to level 28
#(0.1 m), and cropping from there would pull 2500x more pixels than a 5 m cell
#needs. Level 22 is 2.5 m, so exactly 2x2 source pixels per output cell.
LC_SWISSIMAGE <- paste0("WMTS:https://wmts.geo.admin.ch/EPSG/2056/1.0.0/",
                        "WMTSCapabilities.xml,",
                        "layer=ch.swisstopo.swissimage-product,zoom_level=22")


# ------------------------------------------------------------ crosswalks ----

#tlm_bb_bodenbedeckung. Listed in burn order: later entries win where polygons
#overlap, which is why water is last.
LC_BB <- list(
  "3" = c("Fels", "Fels locker", "Felsbloecke", "Felsbloecke locker",
          "Lockergestein", "Lockergestein locker"),
  "4" = c("Wald", "Wald offen", "Gehoelzflaeche"),
  "2" = c("Gebueschwald"),
  "1" = c("Feuchtgebiet"),
  "5" = c("Stehende Gewaesser", "Fliessgewaesser", "Gletscher", "Schneefeld Toteis")
)

#the Areale layers, plus the polygonal traffic structures. Only positively
#attested surfaces appear; anything unlisted keeps whatever the earlier steps
#left it as. Burn order per layer is grass, then soil, then impervious.
LC_AREALE <- list(
  tlm_areale_verkehrsareal = list(
    "1" = c("Flugfeldareal"),
    "3" = c("Oeffentliches Parkplatzareal", "Privates Parkplatzareal",
            "Verkehrsflaeche", "Rastplatzareal", "Privates Fahrareal",
            "Gleisareal", "Flughafenareal", "Flugplatzareal", "Heliport")
  ),
  tlm_areale_nutzungsareal = list(
    "1" = c("Reben", "Obstanlage", "Baumschule", "Schrebergartenareal",
            "Oeffentliches Parkareal", "Friedhof", "Wald nicht bestockt",
            "Historisches Areal", "Klosterareal", "Truppenuebungsplatz"),
    "4" = c("Abbauareal", "Deponieareal"),
    "3" = c("Kraftwerkareal", "Abwasserreinigungsareal", "Unterwerkareal",
            "Kehrichtverbrennungsareal", "Antennenareal", "Messeareal",
            "Schul- und Hochschulareal", "Spitalareal",
            "Massnahmenvollzugsanstaltsareal")
  ),
  tlm_areale_freizeitareal = list(
    "1" = c("Sportplatzareal", "Golfplatzareal", "Campingplatzareal",
            "Standplatzareal", "Zooareal", "Pferderennbahnareal"),
    "3" = c("Schwimmbadareal", "Freizeitanlagenareal")
  ),
  tlm_bauten_verkehrsbaute_ply = list(
    "1" = c("Graspiste", "Rollfeld Gras"),
    "3" = c("Perron", "Rollfeld Hartbelag", "Hartbelagpiste", "Schleuse")
  )
)

#full width in metres; the line is buffered by half of it. Objektarten absent
#from this table (Verbindung, Markierte Spur, Klettersteig, Faehre, Autozug)
#are network connectors or non-surfaces, and are skipped by lookup failure.
LC_ROAD_WIDTH <- c(
  "Autobahn" = 11, "Autostrasse" = 9,
  "10m Strasse" = 10, "8m Strasse" = 8, "6m Strasse" = 6,
  "4m Strasse" = 4, "3m Strasse" = 3,
  "2m Weg" = 2, "2m Wegfragment" = 2, "1m Weg" = 1, "1m Wegfragment" = 1,
  "Platz" = 8, "Raststaette" = 6,
  "Einfahrt" = 5, "Ausfahrt" = 5, "Zufahrt" = 5, "Dienstzufahrt" = 5
)

LC_RAIL_WIDTH <- c("Normalspur" = 5, "Schmalspur" = 3,
                   "Schmalspur mit Normalspur" = 6, "Kleinbahn" = 2)

#a road or track that is not the ground surface at all
LC_KUNSTBAUTE_DROP <- c("Tunnel", "Unterfuehrung", "Unterfuehrung mit Treppe",
                        "Galerie", "in/auf Gebaeude")

#...and one that is a tall artificial structure standing above the ground, so
#it belongs in the canopy and leaves the river or meadow below it intact
LC_KUNSTBAUTE_BRIDGE <- c("Bruecke", "Gedeckte Bruecke", "Bruecke mit Treppe",
                          "Bruecke mit Galerie", "Steg")

LC_BUILDING_DROP <- c("Unterirdisches Gebaeude")


# ---------------------------------------------------------------- helpers ---

#' Tile grid over `ext`, as a data.frame of extents plus an id used for filenames.
lc_tile_grid <- function(ext = LC_EXT, tile_m = LC_TILE_M){
  xs <- seq(ext[["xmin"]], ext[["xmax"]] - 1, by = tile_m[1])
  ys <- seq(ext[["ymin"]], ext[["ymax"]] - 1, by = tile_m[2])
  g  <- expand.grid(xmin = xs, ymin = ys)
  data.frame(
    tile_id = sprintf("%02d_%02d", match(g$xmin, xs), match(g$ymin, ys)),
    xmin = g$xmin, xmax = pmin(g$xmin + tile_m[1], ext[["xmax"]]),
    ymin = g$ymin, ymax = pmin(g$ymin + tile_m[2], ext[["ymax"]]),
    stringsAsFactors = FALSE
  )
}

#' Empty tile raster on the paint grid, filled with `value`.
lc_template <- function(tile, value = 0){
  r <- terra::rast(terra::ext(tile$xmin, tile$xmax, tile$ymin, tile$ymax),
                   resolution = LC_RES, crs = "EPSG:2056")
  terra::values(r) <- value
  r
}

#' Read the part of a swissTLM3D layer that intersects a tile.
#'
#' `layer=` + `wkt_filter=` rather than `query=`: only the layer form uses the
#' GeoPackage's RTree index, and tlm_bauten_gebaeude_footprint has 3.66 M rows
#' in a 10.8 GB file. Attributes are filtered in R after the spatial cut. Same
#' idiom as the path loading in app_server.R.
lc_read_tile <- function(layer, tile, gpkg = LC_TLM3D){
  wkt <- sf::st_as_text(sf::st_as_sfc(sf::st_bbox(
    c(xmin = tile$xmin, ymin = tile$ymin, xmax = tile$xmax, ymax = tile$ymax),
    crs = 2056)))
  x <- try(sf::st_read(gpkg, layer = layer, wkt_filter = wkt, quiet = TRUE),
           silent = TRUE)
  if(inherits(x, "try-error") || nrow(x) == 0) return(NULL)
  #TLM3D geometries are XYZ; terra wants them flat
  sf::st_zm(x, drop = TRUE, what = "ZM")
}

#' Burn `geom` into `x` as `value`, leaving `x` untouched where geom is absent.
lc_burn <- function(x, geom, value){
  if(is.null(geom) || nrow(geom) == 0) return(x)
  v <- terra::vect(sf::st_geometry(geom))
  if(nrow(v) == 0) return(x)
  #touches = FALSE: a cell is claimed only if its centre is covered, which is
  #what keeps a 2 m buffer from inflating into a solid 5 m ribbon
  b <- terra::rasterize(v, x, field = as.numeric(value),
                        background = NA, touches = FALSE)
  terra::cover(b, x)
}

#' Burn a whole objektart -> class crosswalk (a named list whose names are the
#' target classes, in burn order).
lc_burn_crosswalk <- function(x, geom, crosswalk){
  if(is.null(geom) || nrow(geom) == 0) return(x)
  for(cls in names(crosswalk)){
    sel <- geom$objektart %in% crosswalk[[cls]]
    if(any(sel)) x <- lc_burn(x, geom[sel, ], as.numeric(cls))
  }
  x
}

#' Split a road or rail layer into the part that is the ground surface and the
#' part that is a structure above it. Anything underground is dropped outright.
lc_split_structure <- function(geom){
  st <- suppressWarnings(as.integer(geom$stufe))
  st[is.na(st)] <- 0L
  kb <- geom$kunstbaute

  drop   <- kb %in% LC_KUNSTBAUTE_DROP | st < 0
  bridge <- !drop & (kb %in% LC_KUNSTBAUTE_BRIDGE | st > 0)

  list(ground = geom[!drop & !bridge, ], canopy = geom[bridge, ])
}

#' Buffer centrelines to their mapped width, dropping anything unlisted or
#' narrower than `min_width`.
lc_buffer_lines <- function(geom, widths, min_width = LC_MIN_ROAD_WIDTH){
  if(is.null(geom) || nrow(geom) == 0) return(NULL)
  w    <- unname(widths[geom$objektart])
  keep <- !is.na(w) & w >= min_width
  if(!any(keep)) return(NULL)
  sf::st_buffer(geom[keep, ], dist = w[keep] / 2, endCapStyle = "FLAT")
}


# ------------------------------------------------------------- tile build ---

#' Build one tile of both rasters and write them.
lc_build_tile <- function(tile, out_dir = LC_OUT_DIR, overwrite = FALSE,
                          min_road_width = LC_MIN_ROAD_WIDTH){
  f_ground <- file.path(out_dir, "tiles", "ground", paste0("ground_", tile$tile_id, ".tif"))
  f_canopy <- file.path(out_dir, "tiles", "canopy", paste0("canopy_", tile$tile_id, ".tif"))
  if(!overwrite && file.exists(f_ground) && file.exists(f_canopy)){
    return(invisible(c(ground = f_ground, canopy = f_canopy)))
  }
  dir.create(dirname(f_ground), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(f_canopy), recursive = TRUE, showWarnings = FALSE)

  ground <- lc_template(tile, 0)

  #the vegetation height model is already on this exact grid, so cropping it
  #lines up cell for cell - no resample, ever
  vhm <- terra::crop(terra::rast(LC_VHM), ground)
  stopifnot(all(dim(vhm)[1:2] == dim(ground)[1:2]))
  vhm <- terra::ifel(is.na(vhm), -1, vhm)

  # 1. ground cover
  ground <- lc_burn_crosswalk(ground, lc_read_tile("tlm_bb_bodenbedeckung", tile), LC_BB)

  # 2. bush from vegetation height.
  #
  # This has to run *here*, between Bodenbedeckung and the Areale. At this point
  # the only 3s in the tile are rock and scree, so letting 3 be eligible is what
  # allows alpine dwarf-shrub heath on a scree slope to come out as bush rather
  # than as heat-absorbing rock. Run it after the Areale and it would start
  # eating parking lots instead. Water and ice (5) are never eligible.
  eligible <- (ground == 0) | (ground == 3) | (ground == 4)
  ground   <- terra::ifel(eligible & vhm >= LC_BUSH_MIN & vhm < LC_CANOPY_MIN, 2, ground)

  # 3. designated areas
  for(layer in names(LC_AREALE)){
    ground <- lc_burn_crosswalk(ground, lc_read_tile(layer, tile), LC_AREALE[[layer]])
  }

  #canopy starts as the crowns; bridges and buildings are stacked on top below
  canopy <- terra::ifel(vhm >= LC_CANOPY_MIN, 7, 0)

  # 4. rail
  rail <- lc_read_tile("tlm_oev_eisenbahn", tile)
  if(!is.null(rail)){
    parts  <- lc_split_structure(rail)
    ground <- lc_burn(ground, lc_buffer_lines(parts$ground, LC_RAIL_WIDTH, 0), 3)
    canopy <- lc_burn(canopy, lc_buffer_lines(parts$canopy, LC_RAIL_WIDTH, 0), 6)
  }

  # 5. roads. On the ground the surface material decides the class: a hard
  #    surface is impervious, a natural track is bare soil. On a bridge it is
  #    an artificial structure either way.
  road <- lc_read_tile("tlm_strassen_strasse", tile)
  if(!is.null(road)){
    parts <- lc_split_structure(road)
    for(cls in c("3", "4")){
      sel <- parts$ground$belagsart %in% (if(cls == "3") "Hart" else "Natur")
      if(any(sel)){
        ground <- lc_burn(ground,
                          lc_buffer_lines(parts$ground[sel, ], LC_ROAD_WIDTH, min_road_width),
                          as.numeric(cls))
      }
    }
    canopy <- lc_burn(canopy,
                      lc_buffer_lines(parts$canopy, LC_ROAD_WIDTH, min_road_width), 6)
  }

  # 6. buildings. A solid block occupies the ground and everything above it, so
  #    it lands in both rasters - the same semantics as the artificial_block
  #    paint material (level = "both").
  bld <- lc_read_tile("tlm_bauten_gebaeude_footprint", tile)
  if(!is.null(bld)){
    bld    <- bld[!bld$objektart %in% LC_BUILDING_DROP, ]
    ground <- lc_burn(ground, bld, 8)
    canopy <- lc_burn(canopy, bld, 8)
  }

  terra::writeRaster(ground, f_ground, datatype = "INT1U", gdal = LC_GDAL, overwrite = TRUE)
  terra::writeRaster(canopy, f_canopy, datatype = "INT1U", gdal = LC_GDAL, overwrite = TRUE)
  invisible(c(ground = f_ground, canopy = f_canopy))
}


# ------------------------------------------------------------------ build ---

#' Build the national rasters, tile by tile, then merge.
#'
#' Restartable: a tile whose two files already exist is skipped unless
#' `overwrite`, so an interrupted run picks up where it stopped.
build_ground_canopy_CH <- function(out_dir = LC_OUT_DIR, tiles = NULL,
                                   overwrite = FALSE, merge = TRUE,
                                   min_road_width = LC_MIN_ROAD_WIDTH){
  if(is.null(tiles)) tiles <- lc_tile_grid()
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  for(i in seq_len(nrow(tiles))){
    t0 <- Sys.time()
    lc_build_tile(tiles[i, ], out_dir = out_dir, overwrite = overwrite,
                  min_road_width = min_road_width)
    message(sprintf("tile %s (%d/%d) - %.1f min", tiles$tile_id[i], i, nrow(tiles),
                    as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  if(!merge) return(invisible(NULL))

  out <- character(0)
  for(what in c("ground", "canopy")){
    #derived from the tile grid rather than globbed off disk: the tile directory
    #also collects ad-hoc test tiles, and those overlap real ones - merging them
    #in would silently corrupt the national raster
    files <- file.path(out_dir, "tiles", what,
                       paste0(what, "_", tiles$tile_id, ".tif"))
    stopifnot(all(file.exists(files)))
    v     <- terra::vrt(files, file.path(out_dir, paste0(what, "_CH_5m.vrt")), overwrite = TRUE)
    f     <- file.path(out_dir, paste0(what, "_CH_5m.tif"))
    terra::writeRaster(v, f, datatype = "INT1U",
                       gdal = c(LC_GDAL, "BIGTIFF=YES"), overwrite = TRUE)
    out[what] <- f
    message("wrote ", f)
  }
  invisible(out)
}


# --------------------------------------------------------------- aerial QA --

#' Flag cells where SWISSIMAGE disagrees with the assigned ground class.
#'
#' Deliberately not part of the national build: a nationwide pass is on the
#' order of 100k WMTS requests, and the flags are for human review anyway. Run
#' it over an area you care about - the Areal polygons are the usual suspects,
#' since a Klosterareal or Spitalareal is one polygon covering a mix of lawn and
#' concrete, and TLM3D gives no hint which part is which.
#'
#' Writes 0 (agrees, or not checked), -1 (classed grass, imagery says grey) and
#' -3 (classed impervious, imagery says green) into a separate INT1S raster, so
#' ground_CH_5m.tif stays clean INT1U and app-ready.
qa_ground_vs_swissimage <- function(ext, ground_path, canopy_path, filename = NULL,
                                    gli_green = 0.05, gli_grey = 0.00,
                                    shadow_max = 40,
                                    wmts = LC_SWISSIMAGE){
  ground <- terra::crop(terra::rast(ground_path), ext)
  canopy <- terra::crop(terra::rast(canopy_path), ext)

  #the WMTS is served in LV95, so only the requested window is fetched and
  #nothing is reprojected. "average" is what judges a 5 m cell on its mean
  #colour rather than on a single sampled pixel.
  img <- terra::resample(terra::crop(terra::rast(wmts), terra::ext(ground)),
                         ground, method = "average")

  R <- img[[1]]; G <- img[[2]]; B <- img[[3]]
  gli <- (2 * G - R - B) / (2 * G + R + B)

  #deep shadow is dark in all three bands, which drags GLI toward zero and would
  #otherwise flag half of every north-facing urban block
  lit   <- ((R + G + B) / 3) >= shadow_max
  #only where the imagery actually shows the ground: under a crown it shows the crown
  open  <- canopy == 0
  green <- lit & open & gli > gli_green
  grey  <- lit & open & gli < gli_grey

  qa <- terra::ifel(ground == 1 & grey, -1,
                    terra::ifel(ground == 3 & green, -3, 0))

  if(!is.null(filename)){
    terra::writeRaster(qa, filename, datatype = "INT1S", gdal = LC_GDAL, overwrite = TRUE)
  }
  qa
}


# --------------------------------------------------------------- driver -----
# Guarded so package load never touches a 3.1 GB TIFF or a 10.8 GB GeoPackage.

if(FALSE){

  #Zuerich: lake, Limmat, motorway ring, rail yard, Uetliberg. The bridges
  #should appear in the canopy raster with the river intact underneath.
  zh <- data.frame(tile_id = "test_zh", xmin = 2680000, xmax = 2700000,
                   ymin = 1200000, ymax = 1220000, stringsAsFactors = FALSE)
  lc_build_tile(zh, overwrite = TRUE)

  #Aletsch: the only place the rock -> 3, ice -> 5 and bush-on-scree decisions
  #are all visible at once.
  al <- data.frame(tile_id = "test_aletsch", xmin = 2640000, xmax = 2660000,
                   ymin = 1140000, ymax = 1163000, stringsAsFactors = FALSE)
  lc_build_tile(al, overwrite = TRUE)

  g   <- terra::rast(file.path(LC_OUT_DIR, "tiles/ground/ground_test_zh.tif"))
  ids <- sort(unique(terra::values(g)))
  terra::plot(g, col = PAINT_CATEGORIES$hex[match(ids, PAINT_CATEGORIES$id)])

  #the full run: 180 tiles
  build_ground_canopy_CH()

  #coverage: the 0 share should land near the ~36 % of Switzerland that BFS
  #Arealstatistik calls agricultural, plus settlement open ground
  ch <- sf::st_transform(sf::st_set_crs(
    sf::st_read("inst/app/www/data/maps/countryBorders/swissBorder_final.gpkg", quiet = TRUE),
    4326), 2056)
  f <- terra::freq(terra::mask(terra::rast(file.path(LC_OUT_DIR, "ground_CH_5m.tif")),
                               terra::vect(ch)))
  f$pct <- round(100 * f$count / sum(f$count), 1)
  print(f)

  #a Klosterareal or Spitalareal is the best place to see the QA pass earn its keep
  qa_ground_vs_swissimage(terra::ext(2683000, 2686000, 1247000, 1250000),
                          file.path(LC_OUT_DIR, "ground_CH_5m.tif"),
                          file.path(LC_OUT_DIR, "canopy_CH_5m.tif"),
                          filename = file.path(LC_OUT_DIR, "ground_CH_5m_qa.tif"))
}
