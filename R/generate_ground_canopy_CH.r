#' Build the Switzerland-wide ground and canopy land cover rasters.
#'
#' Two 1 m EPSG:2056 rasters covering all of Switzerland, classified with the
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
#' RESOLUTION. 1 m, not 5 m, so that a 1 m Weg is a real ribbon instead of a
#' fifth of a cell. That is the whole reason for the finer grid, and it is worth
#' being precise about what does *not* get sharper with it: the vegetation
#' height model is a 5 m product, so classes 2 (bush) and 7 (tree) are
#' disaggregated from 5 m and carry 5 m blocks inside a 1 m grid. Roads, paths,
#' buildings, streams and walls come from vectors and are genuinely 1 m.
#'
#' COVERAGE. swissTLM3D alone leaves ~42 % of the country at 0: its
#' Bodenbedeckung maps forest, rock, water, wetland and glacier, and
#' deliberately omits farmland and settlement open ground. That is a property of
#' the product, not a gap in the crosswalk below - every objektart the layer
#' carries is already mapped in LC_BB. The residue is therefore filled from OSM
#' landuse (see lc_prepare_osm), but only where TLM3D says nothing at all:
#' attested data always wins, and the backfill runs last so it can never
#' overwrite a surveyed surface.
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

#Geofabrik's pre-polygonised Switzerland extract, and the single reprojected,
#spatially indexed GeoPackage lc_prepare_osm() renders it down to. The raw
#shapefiles are WGS84; the derived file is LV95, so the tile loop never
#reprojects.
LC_OSM_SRC <- "C:/Users/frueh/Documents/Local Data/OSM/ch"
LC_OSM     <- "C:/Users/frueh/Documents/Local Data/OSM/osm_landcover_2056.gpkg"

#the grid is not ours to choose: PAINT_RES is 1 m in EPSG:2056 indexed from the
#LV95 origin. The vegetation height model is a 5 m product on this same origin,
#so it disaggregates onto the grid by an exact factor of 5 - lc_vhm_tile() takes
#that path rather than resample(), because nearest-neighbour disagg of an
#aligned raster is a pure block copy and cannot shift a cell edge.
LC_RES <- 1
LC_VHM_RES <- 5
LC_EXT <- c(xmin = 2480000, xmax = 2840000, ymin = 1070000, ymax = 1300000)

#4000 x 4600 m divides LC_EXT into exactly 90 x 50 tiles. At 1 m that is
#4000 x 4600 = 18.4 M cells per tile - deliberately the same cell count the 5 m
#build used per tile, since that is the size already proven to fit in memory
#alongside the half-dozen intermediate layers lc_build_tile() holds.
LC_TILE_M <- c(4000, 4600)

#vegetation height cuts, in metres. Below LC_BUSH_MIN is grass or bare ground,
#in between is bush, at or above LC_CANOPY_MIN it is a crown and belongs to the
#canopy raster instead.
LC_BUSH_MIN   <- 0.5
LC_CANOPY_MIN <- 3

#roads narrower than this are dropped. At 1 m a 1 m footpath is exactly one
#cell wide, so nothing has to be dropped any more - this is the reason the grid
#was refined in the first place. Keep the knob: it is still how you ask for a
#roads-only or a trails-free variant.
LC_MIN_ROAD_WIDTH <- 1

LC_GDAL <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES")

#SWISSIMAGE for the aerial QA pass, read through GDAL's WMTS driver so only the
#requested window is fetched. The service is published in LV95, so nothing is
#reprojected. zoom_level is not optional: the capabilities default to level 28
#(0.1 m), and cropping from there would pull 100x more pixels than a 1 m cell
#needs. Level 24 is 0.625 m, the finest level that still averages several
#source pixels into each output cell rather than sampling one.
LC_SWISSIMAGE <- paste0("WMTS:https://wmts.geo.admin.ch/EPSG/2056/1.0.0/",
                        "WMTSCapabilities.xml,",
                        "layer=ch.swisstopo.swissimage-product,zoom_level=24")


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
  ),
  #a Sportplatz is the one place TLM3D maps a mown surface precisely, and it is
  #a polygon rather than an Areal, so it lands here rather than in the Areale
  tlm_bauten_sportbaute_ply = list(
    "1" = c("Sportplatz")
  ),
  #Wasserbecken are the reservoirs and settling basins too small for
  #Bodenbedeckung; the dam wall itself is concrete
  tlm_bauten_staubaute = list(
    "5" = c("Wasserbecken"),
    "3" = c("Staumauer", "Wehr")
  )
)

#linear structures that are a surface in their own right at 1 m but vanish at
#5 m. Full width in metres, same convention as LC_ROAD_WIDTH.
#
#Fliessgewaesser carries no width attribute at all, so 2 m is a nominal minimum
#channel: every stream wide enough to matter is already a Bodenbedeckung
#polygon and gets burnt over this. Seeachse is a synthetic centreline through a
#lake and Druckleitung/Druckstollen are pipes, so all three are absent here and
#dropped by lookup failure. A Trockenrinne is a dry gravel channel, not water.
LC_WATER_WIDTH <- c("Fliessgewaesser" = 2)
LC_WATER_DRY   <- c("Trockenrinne" = 2)

#Mauer and the Verbauung family are walls and bank revetments - thin concrete
#and stone, thermally impervious.
LC_WALL_WIDTH <- c("Mauer" = 1, "Trockenmauer" = 1,
                   "Gewaesserverbauung" = 2, "Schutzverbauung" = 2)

#a jetty is a walkable deck standing over water, which is the canopy raster's
#definition of an artificial structure above the ground
LC_PIER_WIDTH <- c("Hafensteg" = 2)

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

#OSM landuse -> class, keyed by Geofabrik's `fclass`. Listed in burn order,
#water last, exactly as LC_BB.
#
#This table only ever fills cells swissTLM3D left at 0, so it is not competing
#with surveyed data - it is answering "what is the least wrong thing to say
#about a cell nothing else describes". Two entries deserve their reasoning
#spelled out, because between them they carry 2 km2 in every 1000:
#
#  residential -> grass. Counter-intuitive until you remember the order: TLM3D
#  has already burnt every building (8) and every road (3) inside the polygon
#  with survey precision. What is still 0 inside a Swiss residential block is
#  therefore the garden, lawn and hedge between them, not the built fabric. On
#  a typical 500 m2 plot that residue really is ~85 % vegetated.
#
#  industrial/commercial/retail -> impervious, by the same argument running the
#  other way: strip the buildings out of a works or a retail park and what is
#  left is yard, apron and parking.
#
#forest -> 4 rather than a canopy class on purpose: this raster is the *ground*,
#and the ground under a crown is soil. The crown itself arrives from the
#vegetation height model, which does not need OSM's help.
LC_OSM_CLASS <- list(
  "4" = c("forest", "quarry", "landfill", "beach"),
  "1" = c("meadow", "farmland", "grass", "vineyard", "orchard", "allotments",
          "park", "cemetery", "recreation_ground", "village_green", "greenfield",
          "residential",
          "wetland", "wetland_marsh", "wetland_bog", "wetland_reedbed",
          "wetland_wet_meadow", "wetland_fen", "wetland_swamp"),
  "2" = c("scrub", "heath"),
  "3" = c("industrial", "commercial", "retail", "military", "farmyard",
          "parking", "service", "fuel", "cliff", "dam", "weir"),
  "5" = c("water", "riverbank", "reservoir", "glacier", "dock")
)

#the Geofabrik layers LC_OSM draws on. `pier` is deliberately absent: like a
#Hafensteg it is a deck over water, so it belongs to the canopy, not the ground.
LC_OSM_LAYERS <- c("gis_osm_landuse_a_free_1", "gis_osm_natural_a_free_1",
                   "gis_osm_water_a_free_1", "gis_osm_traffic_a_free_1")


# ---------------------------------------------------------------- helpers ---

#' Tile subdirectory for the current resolution.
lc_tile_dir <- function(res = LC_RES) paste0("tiles_", res, "m")

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

#' The vegetation height model over one tile, on the 1 m grid.
#'
#' The VHM is 5 m and the target is 1 m, both indexed from the LV95 origin, so
#' the two grids nest exactly. Cropping first and disaggregating second keeps
#' the work proportional to the tile rather than the country, and
#' nearest-neighbour disagg of an already-aligned raster is a block copy - it
#' invents no values and cannot shift a cell edge. It does not invent *detail*
#' either: every 5 x 5 block of the result is one VHM cell, which is why the
#' header warns that classes 2 and 7 stay 5 m products on a 1 m grid.
#' Where tiles built without vegetation data are recorded. One tile id per line.
LC_VHM_GAP_LOG <- function(out_dir = LC_OUT_DIR) file.path(out_dir, "vhm_gaps.txt")

lc_vhm_tile <- function(tile, template, out_dir = LC_OUT_DIR){
  v <- try(suppressWarnings(
         terra::crop(terra::rast(LC_VHM),
                     terra::ext(tile$xmin, tile$xmax, tile$ymin, tile$ymax))),
       silent = TRUE)

  #A damaged source must not take the run down with it. VHM_ALS_5m.tif has at
  #least one strip that fails to inflate ("ZIPDecode: Decoding error"), and a
  #single unreadable scanline anywhere in a tile kills the whole crop - which
  #cost a 3-hour run at tile 2864 of 4500.
  #
  #The fallback is deliberately conservative and deliberately noisy. -1 is what
  #this function already returns for NA, i.e. "no vegetation here", so the tile
  #comes out with no bush and no canopy rather than with invented values. That
  #is wrong, and the point of the log is that it is wrong *traceably*: every
  #affected tile id is appended to vhm_gaps.txt so the exact set can be rebuilt
  #once the source is repaired, instead of the damage dissolving into 4500
  #tiles nobody can tell apart.
  if(inherits(v, "try-error") || terra::ncell(v) == 0){
    message("VHM unreadable for tile ", tile$tile_id, " - built without vegetation")
    cat(tile$tile_id, "\n", sep = "", file = LC_VHM_GAP_LOG(out_dir), append = TRUE)
    out <- template
    terra::values(out) <- -1
    return(out)
  }

  v <- terra::disagg(v, fact = LC_VHM_RES / LC_RES, method = "near")
  stopifnot(all(dim(v)[1:2] == dim(template)[1:2]))
  terra::ext(v) <- terra::ext(template)   #kill sub-micron float drift from disagg
  terra::ifel(is.na(v), -1, v)
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


# -------------------------------------------------------------------- OSM ---

#' Render Geofabrik's Switzerland extract down to one indexed LV95 GeoPackage.
#'
#' Run once, before the national build. The tile loop reads this file 4500
#' times, so everything that can be done ahead of time is done here: the fclass
#' filter, the class assignment, the reprojection out of WGS84, and
#' st_make_valid. OSM polygons are user-drawn and a bow-tie self-intersection is
#' common; terra::rasterize() on one is a hard error, so it has to be repaired
#' now rather than 4500 times later, or worse, halfway through hour nine.
#'
#' The output carries a single integer `cls` column and nothing else. Writing it
#' as a GeoPackage rather than keeping shapefiles is what buys the RTree index
#' that makes lc_read_tile()'s wkt_filter O(features returned).
lc_prepare_osm <- function(src = LC_OSM_SRC, out = LC_OSM,
                           layers = LC_OSM_LAYERS, crosswalk = LC_OSM_CLASS){
  cls_of <- stats::setNames(
    rep(as.integer(names(crosswalk)), lengths(crosswalk)),
    unlist(crosswalk, use.names = FALSE)
  )

  parts <- list()
  for(ly in layers){
    p <- file.path(src, paste0(ly, ".shp"))
    if(!file.exists(p)){ message("skip (missing): ", ly); next }
    x <- sf::st_read(p, quiet = TRUE)
    x <- x[x$fclass %in% names(cls_of), c("fclass")]
    if(nrow(x) == 0) next
    x$cls <- unname(cls_of[x$fclass])
    parts[[ly]] <- sf::st_transform(x[, "cls"], 2056)
    message(sprintf("  %-30s %7d features", ly, nrow(x)))
  }
  stopifnot(length(parts) > 0)

  osm <- do.call(rbind, parts)
  osm <- sf::st_make_valid(osm)
  #make_valid can turn a self-intersecting polygon into a GEOMETRYCOLLECTION
  #carrying stray lines and points; only the surfaces are land cover
  osm <- suppressWarnings(sf::st_collection_extract(osm, "POLYGON"))
  osm <- osm[!sf::st_is_empty(osm), ]

  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  sf::st_write(osm, out, layer = "landcover", delete_dsn = TRUE, quiet = TRUE)
  message("wrote ", out, " (", nrow(osm), " polygons)")
  invisible(out)
}


# ------------------------------------------------------------- tile build ---

#' Build one tile of both rasters and write them.
lc_build_tile <- function(tile, out_dir = LC_OUT_DIR, overwrite = FALSE,
                          min_road_width = LC_MIN_ROAD_WIDTH){
  #the resolution is in the directory name, not decoration: tile ids restart at
  #01_01 for every grid, so a 1 m run whose tiles sat next to the 5 m run's
  #would find ground_01_01.tif already present and skip it - silently welding a
  #5 m tile into a 1 m raster
  f_ground <- file.path(out_dir, lc_tile_dir(), "ground", paste0("ground_", tile$tile_id, ".tif"))
  f_canopy <- file.path(out_dir, lc_tile_dir(), "canopy", paste0("canopy_", tile$tile_id, ".tif"))
  if(!overwrite && file.exists(f_ground) && file.exists(f_canopy)){
    return(invisible(c(ground = f_ground, canopy = f_canopy)))
  }
  dir.create(dirname(f_ground), recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(f_canopy), recursive = TRUE, showWarnings = FALSE)

  ground <- lc_template(tile, 0)
  vhm    <- lc_vhm_tile(tile, lc_template(tile, 0), out_dir = out_dir)

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

  # 6. thin linear structures. Only worth reading at 1 m - at 5 m a wall and a
  #    brook are both narrower than a cell and were left out of the 5 m build.
  #    Streams first so a bank revetment or a jetty can sit on top of one.
  water_ln <- lc_read_tile("tlm_gewaesser_fliessgewaesser", tile)
  if(!is.null(water_ln)){
    st <- suppressWarnings(as.integer(water_ln$stufe)); st[is.na(st)] <- 0L
    water_ln <- water_ln[st >= 0, ]     #culverted reaches are not a surface
    ground <- lc_burn(ground, lc_buffer_lines(water_ln, LC_WATER_DRY,   0), 4)
    ground <- lc_burn(ground, lc_buffer_lines(water_ln, LC_WATER_WIDTH, 0), 5)
  }
  for(ly in c("tlm_bauten_mauer", "tlm_bauten_verbauung")){
    ground <- lc_burn(ground, lc_buffer_lines(lc_read_tile(ly, tile),
                                              LC_WALL_WIDTH, 0), 3)
  }
  canopy <- lc_burn(canopy, lc_buffer_lines(lc_read_tile("tlm_bauten_verkehrsbaute_lin", tile),
                                            LC_PIER_WIDTH, 0), 6)

  # 7. buildings. A solid block occupies the ground and everything above it, so
  #    it lands in both rasters - the same semantics as the artificial_block
  #    paint material (level = "both").
  bld <- lc_read_tile("tlm_bauten_gebaeude_footprint", tile)
  if(!is.null(bld)){
    bld    <- bld[!bld$objektart %in% LC_BUILDING_DROP, ]
    ground <- lc_burn(ground, bld, 8)
    canopy <- lc_burn(canopy, bld, 8)
  }

  # 8. OSM backfill, last and lowest priority. Everything above this line is
  #    surveyed; this is the only step that guesses, so it is confined to cells
  #    still holding 0 and can never overwrite an attested class. Burning it
  #    into a scratch raster first is what enforces that: the crosswalk's own
  #    internal precedence resolves among OSM polygons, then a single ifel()
  #    lets the result through only where nothing else spoke.
  osm <- lc_read_tile("landcover", tile, gpkg = LC_OSM)
  if(!is.null(osm)){
    fill <- lc_template(tile, 0)
    for(cls in names(LC_OSM_CLASS)){
      sel <- osm$cls == as.integer(cls)
      if(any(sel)) fill <- lc_burn(fill, osm[sel, ], as.numeric(cls))
    }
    ground <- terra::ifel(ground == 0, fill, ground)
  }

  terra::writeRaster(ground, f_ground, datatype = "INT1U", gdal = LC_GDAL, overwrite = TRUE)
  terra::writeRaster(canopy, f_canopy, datatype = "INT1U", gdal = LC_GDAL, overwrite = TRUE)
  invisible(c(ground = f_ground, canopy = f_canopy))
}


# ------------------------------------------------------------------ build ---

#' Build the national rasters, tile by tile, then merge.
#'
#' Restartable: a tile whose two files already exist is skipped unless
#' `overwrite`, so an interrupted run picks up where it stopped. That is also
#' the whole recovery story for the parallel path - if a worker dies, rerun and
#' it picks up the tiles that never got written.
#'
#' `workers > 1` forks a PSOCK cluster over the tile list. Tiles are independent
#' by construction (each reads its own window and writes its own two files), and
#' the only shared state is read-only: the GeoPackages, which SQLite is happy to
#' have many readers on, and the VHM. At 1 m the sequential run is ~23 h, so
#' this is not a nicety. Workers re-`source()` this file rather than receiving
#' exports, because the crosswalks and helpers are numerous and a stale export
#' would be an invisible way to build half the country with the wrong table.
#'
#' Progress is reported per chunk rather than per tile: 4500 individual messages
#' is not a progress report, it is a wall of text.
build_ground_canopy_CH <- function(out_dir = LC_OUT_DIR, tiles = NULL,
                                   overwrite = FALSE, merge = TRUE,
                                   min_road_width = LC_MIN_ROAD_WIDTH,
                                   workers = 1,
                                   src = "R/generate_ground_canopy_CH.r"){
  if(is.null(tiles)) tiles <- lc_tile_grid()
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  if(workers > 1){
    stopifnot(file.exists(src))
    src_abs <- normalizePath(src)
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterCall(cl, function(p) suppressMessages(source(p)), src_abs)

    #terra's memfrac is a fraction of *total* RAM, and every worker applies it
    #independently: left at its 0.6 default, 8 workers each believe they may
    #take 60 % of the machine and the run dies with std::bad_alloc partway in.
    #Dividing it out is what makes the memory budget add up to one machine
    #rather than `workers` of them. Below the split each worker spills to its
    #own scratch directory instead of failing - slower per tile, but it
    #finishes.
    #tempdir() is already per process, so each worker spills somewhere different
    parallel::clusterCall(cl, function(frac){
      d <- file.path(tempdir(), "terra_worker")
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      terra::terraOptions(memfrac = frac, tempdir = d, progress = 0)
      NULL
    }, max(0.5 / workers, 0.03))

    #the worker is given an environment whose parent is globalenv, not this
    #function's frame. Serialising a closure drags its enclosing environment
    #along, and this frame holds `cl` - a cluster object wrapping open sockets,
    #which cannot be serialised. Binding it explicitly keeps the payload to the
    #four values the worker actually reads.
    wenv <- new.env(parent = globalenv())
    wenv$tiles <- tiles; wenv$out_dir <- out_dir
    wenv$overwrite <- overwrite; wenv$mrw <- min_road_width
    worker <- function(i){
      lc_build_tile(tiles[i, ], out_dir = out_dir, overwrite = overwrite,
                    min_road_width = mrw)
      NULL
    }
    environment(worker) <- wenv

    #chunked so progress is visible and so a failure costs one chunk, not the run
    idx   <- split(seq_len(nrow(tiles)), ceiling(seq_len(nrow(tiles)) / (workers * 15)))
    t_all <- Sys.time()
    for(k in seq_along(idx)){
      t0 <- Sys.time()
      parallel::parLapplyLB(cl, idx[[k]], worker)
      done <- max(idx[[k]])
      el   <- as.numeric(difftime(Sys.time(), t_all, units = "mins"))
      message(sprintf("chunk %d/%d - %d/%d tiles - %.1f min elapsed, ~%.1f min left",
                      k, length(idx), done, nrow(tiles),
                      el, el / done * (nrow(tiles) - done)))
    }
  }else{
    for(i in seq_len(nrow(tiles))){
      t0 <- Sys.time()
      lc_build_tile(tiles[i, ], out_dir = out_dir, overwrite = overwrite,
                    min_road_width = min_road_width)
      message(sprintf("tile %s (%d/%d) - %.1f min", tiles$tile_id[i], i, nrow(tiles),
                      as.numeric(difftime(Sys.time(), t0, units = "mins"))))
    }
  }
  if(!merge) return(invisible(NULL))

  out <- character(0)
  for(what in c("ground", "canopy")){
    #derived from the tile grid rather than globbed off disk: the tile directory
    #also collects ad-hoc test tiles, and those overlap real ones - merging them
    #in would silently corrupt the national raster
    files <- file.path(out_dir, lc_tile_dir(), what,
                       paste0(what, "_", tiles$tile_id, ".tif"))
    stopifnot(all(file.exists(files)))
    v     <- terra::vrt(files, file.path(out_dir, paste0(what, "_CH_1m.vrt")), overwrite = TRUE)
    f     <- file.path(out_dir, paste0(what, "_CH_1m.tif"))
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
#' ground_CH_1m.tif stays clean INT1U and app-ready.
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

  #once, before anything else: ~500k OSM polygons -> one indexed LV95 file
  lc_prepare_osm()

  #test tiles are one grid tile (4 x 4.6 km), not the 20 x 23 km the 5 m build
  #used - at 1 m that would be 460 M cells and would not fit in memory.
  #
  #Zuerich centre: Limmat, Hauptbahnhof rail yard, bridges over the river, and
  #the residential blocks where the OSM backfill does its most consequential
  #work. The bridges should appear in the canopy raster with the river intact
  #underneath.
  zh <- data.frame(tile_id = "test_zh", xmin = 2682000, xmax = 2686000,
                   ymin = 1247000, ymax = 1251600, stringsAsFactors = FALSE)
  lc_build_tile(zh, overwrite = TRUE)

  #Aletsch: the only place the rock -> 3, ice -> 5 and bush-on-scree decisions
  #are all visible at once.
  al <- data.frame(tile_id = "test_aletsch", xmin = 2644000, xmax = 2648000,
                   ymin = 1150000, ymax = 1154600, stringsAsFactors = FALSE)
  lc_build_tile(al, overwrite = TRUE)

  g   <- terra::rast(file.path(LC_OUT_DIR, lc_tile_dir(), "ground/ground_test_zh.tif"))
  ids <- sort(unique(terra::values(g)))
  terra::plot(g, col = PAINT_CATEGORIES$hex[match(ids, PAINT_CATEGORIES$id)])

  #the full run: 4500 tiles
  build_ground_canopy_CH()

  #coverage: the 0 share should land near the ~36 % of Switzerland that BFS
  #Arealstatistik calls agricultural, plus settlement open ground
  ch <- sf::st_transform(sf::st_set_crs(
    sf::st_read("inst/app/www/data/maps/countryBorders/swissBorder_final.gpkg", quiet = TRUE),
    4326), 2056)
  f <- terra::freq(terra::mask(terra::rast(file.path(LC_OUT_DIR, "ground_CH_1m.tif")),
                               terra::vect(ch)))
  f$pct <- round(100 * f$count / sum(f$count), 1)
  print(f)

  #a Klosterareal or Spitalareal is the best place to see the QA pass earn its keep
  qa_ground_vs_swissimage(terra::ext(2683000, 2686000, 1247000, 1250000),
                          file.path(LC_OUT_DIR, "ground_CH_1m.tif"),
                          file.path(LC_OUT_DIR, "canopy_CH_1m.tif"),
                          filename = file.path(LC_OUT_DIR, "ground_CH_5m_qa.tif"))
}
