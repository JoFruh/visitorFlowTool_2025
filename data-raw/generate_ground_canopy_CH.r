#' Build the Switzerland-wide ground and canopy land cover rasters.
#'
#' Two 1 m EPSG:2056 rasters covering all of Switzerland, classified with the
#' ids of PAINT_CATEGORIES (paintbrush_helpers.R) so the result feeds
#' rasterToRuns() directly and the heat-mitigation paint canvas can start
#' pre-filled with reality instead of blank:
#'
#'   ground  0 unclassified  1 grass/fields  2 bush  3 impervious
#'           4 natural/soil  5 water/ice     16/8/19/17/18 building
#'   canopy  0 open sky      6 artificial (bridges)
#'           10/11/7/12/13 tree             16/8/19/17/18 building
#'
#' HEIGHT IS THE CLASS. A tree is not one id but five, one per step of the
#' paint bar's ramp - 3, 10, 15, 20 and 25 m - and a building likewise carries
#' 5, 10, 15, 25 or 50 m. The id itself is the height, everywhere it travels
#' (this raster, the wire format, the class PNG, the heat model's march), so
#' there is no second band and no height raster to keep in step with this one.
#' See PAINT_CATEGORIES in R/paintbrush_helpers.R, which is where the ramps,
#' their colours and their metres are defined; the two tables below restate only
#' the id/height pairs this file needs, and verify_heat_model.R asserts they
#' agree with the palette and with heat_geometry.csv.
#'
#' Classes follow *thermal* behaviour, not naturalness, because the downstream
#' use is heat mitigation: bare rock and scree store and re-radiate heat much
#' like concrete, so they are impervious; glacier and snow are the coldest,
#' highest-albedo surfaces in the country, so they join water.
#'
#' RESOLUTION. 1 m, not 5 m, so that a 1 m Weg is a real ribbon instead of a
#' fifth of a cell. That is the whole reason for the finer grid, and it is worth
#' being precise about what does *not* get sharper with it: the vegetation
#' height model is a 5 m product, so class 2 (bush) and the tree classes are
#' disaggregated from 5 m and carry 5 m blocks inside a 1 m grid - and that is
#' true of their HEIGHT as well as of their outline. Roads, paths, buildings,
#' streams and walls come from vectors and are genuinely 1 m; a building's
#' height comes from swissBUILDINGS3D and is one number for the whole footprint,
#' so it is as sharp as the footprint is.
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
#' OSM closes most of that but not all: measured over 140 random 1 km windows,
#' 21-25 % of the country was still 0 after step 8. OSM covers settlements well
#' (Sion centre came out at 5.7 % unclassified) and open farmland badly, which
#' is precisely the land TLM3D omits by design. Step 9 fills what is left from
#' ESA WorldCover, a validated 10 m satellite land cover product - see
#' lc_prepare_worldcover() for why satellite rather than the SWISSIMAGE
#' orthophoto the QA pass below uses.
#'
#' NOTE: this file lives in data-raw/ and is *not* sourced on package load - but
#' build_ground_canopy_CH(workers > 1) makes every worker source() it, so it
#' still must stay function definitions only and the driver at the bottom stays
#' inside `if (FALSE)`. Never put a top-level terra::rast() or sf::st_read()
#' here: the sources are a 3.1 GB TIFF and a 10.8 GB GeoPackage.


# ---------------------------------------------------------------- config ----

LC_TLM3D <- paste0("C:/Users/frueh/Documents/Local Data/TLM3D/",
                   "swisstlm3d_2026-02-24_2056_5728.gpkg/SWISSTLM3D_2026_LV95_LN02.gpkg")
LC_VHM     <- "C:/Users/frueh/Documents/Local Data/VHM_ALS_5m.tif"
LC_OUT_DIR <- "C:/Users/frueh/Documents/Local Data/landcover"

#swissBUILDINGS3D 3.0, the national 2026 delivery: the only source that gives a
#Swiss building a height. swissTLM3D does NOT - measured, not assumed: its
#footprint geometry is the outline draped on the terrain, and the per-footprint
#Z spread is a median of 0.88 m, 0.32 m across the Hochhaus in central Zurich. A
#tower's footprint is flat.
#
#What we take from it is the `Floor` layer: an ordinary 2D-able MultiPolygon
#outline per building carrying GESAMTHOEHE, the total height above its own
#terrain point. Not Building_solid or Roof, whose geometry is a TIN that
#terra::vect() refuses and sf exposes as sfc_TIN with no st_coordinates method,
#and not DACH_MAX, which is a roof ELEVATION and would need a terrain to
#subtract. GESAMTHOEHE is already the number the shadow march wants. That is why
#this is a 13.5 GB download rather than the 558 GB swissSURFACE3D would have
#cost for the same answer.
#
#THE DELIVERY HAS TWO ERAS IN IT, and they need opposite treatment. Measured
#over four regions of the 2026 national file:
#
#              UUIDs match TLM3D   GESAMTHOEHE   Floor geometry
#  Zurich              0.0 %          99.5 %     MultiPolygon
#  Bern                0.0 %          99.7 %     MultiPolygon
#  Sion               98.3 %           0.0 %     TIN
#  Ticino             75.4 %           0.0 %     TIN
#
#So where the product has been re-surveyed it has a usable height but has
#reassigned every id, and where it has not, it still carries TLM3D's ids but
#only DACH_MAX, a roof ELEVATION that needs a terrain subtracted. Two routes,
#one per era, and the country needs both - Sion and the whole Valais are in the
#second one.
#
#NEW ERA, per CELL (lc_block_class_tile). The outlines cannot be joined to
#TLM3D's either: 98.7 % of footprints have an interior overlap with a 3D
#building but 59.6 % overlap MORE than one, so taking the tallest would give a
#garage the height of the house it abuts, while a representative point lands
#inside only 62.7 % of them. A cell has no such ambiguity, and the heat model
#reads heights per cell anyway: 90.5 % of TLM3D building cells get a measured
#height that way.
#
#OLD ERA, per FOOTPRINT (lc_block_class_legacy). Here the uuid join works, so
#height = DACH_MAX - the TLM3D footprint's own terrain Z, both LN02. That Z is
#usable because a footprint is draped flat: its per-building spread is a median
#of 0.88 m, and 0.32 m across the Hochhaus of central Zurich.
#
#LC_B3D is the indexed LV95 GeoPackage lc_prepare_buildings3d() writes, on the
#same read-once-reproject-once principle as LC_OSM and LC_WORLDCOVER: the tile
#loop reads it 4500 times.
LC_B3D_SRC <- paste0("https://data.geo.admin.ch/ch.swisstopo.swissbuildings3d_3_0/",
                     "swissbuildings3d_3_0_2026/",
                     "swissbuildings3d_3_0_2026_2056_5728.gdb.zip")
#UNZIP IT. GDAL will read the archive in place over /vsizip/, and it is
#unusably slow at it: a zip entry is one deflate stream with no random access,
#so OpenFileGDB re-inflates from the start of the entry for every seek it makes
#into a 3.6 M-row table. Extracted, the same read is ordinary indexed file I/O.
#Extraction costs 1.4 min and 31 GB of disk, against a read that had not
#finished one layer of five in a quarter of an hour.
LC_B3D_ZIP <- "C:/Users/frueh/Documents/Local Data/swissbuildings3d_3_0_2026.gdb.zip"
LC_B3D_GDB <- paste0("C:/Users/frueh/Documents/Local Data/b3d_2026/",
                     "SWISSBUILDINGS3D_3_0.gdb")
LC_B3D     <- "C:/Users/frueh/Documents/Local Data/buildings3d_2056.gpkg"
LC_B3D_DM  <- "C:/Users/frueh/Documents/Local Data/buildings3d_dachmax.rds"

#A building height derived by SUBTRACTION is only as good as the two datums, and
#when they disagree the answer is not slightly wrong but absurd: a footprint
#surveyed on another datum came out at -447 m. Anything outside this range keeps
#the ramp's default step, which is the class the cell already had. GESAMTHOEHE
#needs no such guard - it is a height, not a difference of elevations.
LC_BUILDING_H_RANGE <- c(2, 300)

#Geofabrik's pre-polygonised Switzerland extract, and the single reprojected,
#spatially indexed GeoPackage lc_prepare_osm() renders it down to. The raw
#shapefiles are WGS84; the derived file is LV95, so the tile loop never
#reprojects.
LC_OSM_SRC <- "C:/Users/frueh/Documents/Local Data/OSM/ch"
LC_OSM     <- "C:/Users/frueh/Documents/Local Data/OSM/osm_landcover_2056.gpkg"

#ESA WorldCover v200 (2021), 10 m, global, free and unauthenticated on S3. The
#tiles are 3 x 3 degrees in EPSG:4326 named after their south-west corner, so
#Switzerland needs the three that cover longitudes 3-12 E at 45-48 N.
#LC_WORLDCOVER is the single reprojected LV95 mosaic lc_prepare_worldcover()
#renders them down to, on the same read-once-reproject-once principle as LC_OSM.
LC_WC_SRC   <- paste0("/vsicurl/https://esa-worldcover.s3.eu-central-1.amazonaws.com/",
                      "v200/2021/map/ESA_WorldCover_10m_2021_v200_")
LC_WC_TILES <- c("N45E003", "N45E006", "N45E009")
LC_WORLDCOVER <- "C:/Users/frueh/Documents/Local Data/worldcover_ch_2056_10m.tif"
LC_WC_RES  <- 10
#Switzerland in WGS84, a little wide on every side
LC_WC_BB   <- c(xmin = 5.9, xmax = 10.55, ymin = 45.75, ymax = 47.85)

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

#The two height ramps, as (class id, metres). These restate the `height` column
#of PAINT_CATEGORIES for the two materials this file writes; verify_heat_model.R
#asserts the restatement is faithful, and lc_check_steps() below does the same
#from here for anyone running a build.
#
#The ORDER is the ramp order, lowest first, because lc_nearest_step() takes the
#midpoints of consecutive entries as its cuts. Keep them sorted by height.
#
#NEAREST STEP, NOT LOWER EDGE, and the difference is the whole accuracy of the
#result. A cell goes to the step it is closest to, so the error is bounded by
#half the local gap and is unbiased rather than one-sided. For a tree that is
#2.5 m over most of the ramp and 3.5 m in the wide 3-10 m band; for a building
#3 m up to 12.5 m - the extra half-metre is the 2 m guard floor below the 5 m
#step, not a cut - then 5 m to 20 m and 12.5 m above that, because the block ramp
#is deliberately coarse where buildings are rare. Binning by lower edge -
#reading class 10 as "3 m or more" - would instead model a 9.9 m tree at 3 m and
#under-shade the whole country by 2.4 m on average.
#
#verify_landcover_heights.R asserts those bounds, so they cannot quietly widen
#when a step is added or moved.
LC_TREE_STEPS <- data.frame(id     = c(10L, 11L,  7L, 12L, 13L),
                            height = c(  3,  10,  15,  20,  25))
LC_BLOCK_STEPS <- data.frame(id     = c(16L,  8L, 19L, 17L, 18L),
                             height = c(  5,  10,  15,  25,  50))


#roads narrower than this are dropped. At 1 m a 1 m footpath is exactly one
#cell wide, so nothing has to be dropped any more - this is the reason the grid
#was refined in the first place. Keep the knob: it is still how you ask for a
#roads-only or a trails-free variant.
LC_MIN_ROAD_WIDTH <- 1

LC_GDAL <- c("COMPRESS=DEFLATE", "PREDICTOR=2", "TILED=YES")

#SWISSIMAGE for the aerial QA pass, read through GDAL's WMTS driver so only the
#requested window is fetched. The service is published in LV95, so nothing is
#reprojected. zoom_level is not optional: the capabilities default to level 28
#(0.1 m), and cropping from there would pull 225x more pixels than a 1 m cell
#needs. Level 24 is 1.5 m - read off the capabilities, where the resolution is
#ScaleDenominator x 0.00028 m; the levels near here are 22 = 2.5 m, 23 = 2 m,
#25 = 1 m, 26 = 0.5 m.
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
  #An aerodrome polygon is a container, and the class here is what lies
  #*between* the surveyed surfaces rather than what covers the whole of it:
  #TLM3D maps the pavement separately as Rollfeld Hartbelag / Hartbelagpiste in
  #tlm_bauten_verkehrsbaute_ply (2914 polygons, 979 ha nationally), and that
  #layer burns after this one. Measured over the 19 Flughafen/Flugplatzareal
  #polygons in Switzerland, surveyed pavement is only 16-49 % of each, so
  #calling the whole 2172 ha impervious painted ~1600 ha of grass as asphalt -
  #90.6 % of the Sion airfield, against 34 % of it really paved. This is the
  #same argument LC_OSM_CLASS makes for "residential": once everything surveyed
  #has been burnt at 1 m, the residue is the lawn in between. Heliport stays
  #impervious - 21 polygons at ~0.5 ha each, and a helipad really is a pad.
  #
  #The burn order needs no change and that is what makes this safe. Within the
  #layer lc_burn_crosswalk() walks "1" then "3", so an airport car park mapped
  #Parkplatzareal still wins; across layers lc_build_tile() reaches
  #tlm_bauten_verkehrsbaute_ply after this one, so the runway burns over the
  #grass; roads (step 5) and buildings (step 7) come later still.
  tlm_areale_verkehrsareal = list(
    "1" = c("Flugfeldareal", "Flughafenareal", "Flugplatzareal"),
    "3" = c("Oeffentliches Parkplatzareal", "Privates Parkplatzareal",
            "Verkehrsflaeche", "Rastplatzareal", "Privates Fahrareal",
            "Gleisareal", "Heliport")
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

#Areal objektarten whose polygon is a *container* rather than a surface: TLM3D
#draws one boundary round a campus and surveys nothing inside it, so the class
#LC_AREALE gives them is a fallback, not an attestation. Where step 9's
#satellite disagrees, the satellite wins - the opposite of the rule everywhere
#else in this file, and defensible only because nothing here was ever surveyed.
#
#Chosen by measurement, not by eye: qa_areal_vs_worldcover() tabulates the
#assigned class against WorldCover on canopy-free cells, and the share of cells
#we call impervious that WorldCover calls grass came out as
#
#  Spitalareal 34.2 %   Schul- und Hochschulareal 29.9 %
#  Freizeitanlagenareal 23.6 %   Oeffentliches Parkplatzareal 22.7 %
#  Schwimmbadareal 15.6 %   Kraftwerkareal 14.1 %   Rastplatzareal 13.3 %
#
#against a control group that is genuinely impervious and agrees:
#Gleisareal 0.3 %, Abwasserreinigungsareal 3.4 %. Those two are the reason to
#believe the rest - the method discriminates, it does not just say "grass".
#Rerun that function before adding or removing a line here.
#
#Measured again after the national rebuild of 2026-09-21, same function, same
#polygons. Every treated type collapsed and every control held to the decimal,
#which is the test that this mask only ever touched container cells:
#
#  treated   Spitalareal 34.2 -> 0.0   Schul-/Hochschul 29.9 -> 0.0
#            Freizeitanlagen 23.6 -> 0.0   Oeff. Parkplatz 22.7 -> 0.1
#            Schwimmbad 15.6 -> 0.0   Kraftwerk 14.1 -> 0.1
#            Rastplatz 13.3 -> 0.1
#  control   Gleisareal 0.3 -> 0.3   Abwasserreinigung 3.4 -> 3.4
#            Golfplatz 0.8 -> 0.8
LC_AREALE_WEAK <- c("Spitalareal", "Schul- und Hochschulareal",
                    "Freizeitanlagenareal", "Schwimmbadareal",
                    "Kraftwerkareal", "Rastplatzareal",
                    "Oeffentliches Parkplatzareal", "Privates Parkplatzareal")

#Below this area a 10 m WorldCover cell is mostly outside the polygon and its
#opinion is smear rather than evidence, so the fallback class is kept. It is
#also what holds the rule off the 14 509 Parkplatzareale under 2 ha, where
#turning a car park into lawn would be the same mistake running backwards.
LC_WEAK_MIN_M2 <- 2e4

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

#ESA WorldCover class -> our class. Lowest priority of all: this table only ever
#sees cells that TLM3D, the vegetation height model and OSM all left at 0.
#
#Each entry was checked against the classes TLM3D *does* attest, over 140 random
#1 km windows (1.4 M cells at 10 m, canopy-free cells only). Row percentages of
#that confusion matrix are quoted below, because they are the only evidence that
#the same rule is right on the cells nobody surveyed.
#
#  10 tree -> 4, not 7. This raster is the *ground*, and the ground under a
#  crown is soil - the identical argument LC_OSM_CLASS makes for "forest". The
#  crown itself comes from the vegetation height model, a 5 m LiDAR product that
#  is far better than a 10 m satellite classification, so the canopy raster is
#  never touched by this step. Confirmed: 84.0 % of attested class 4 reads as
#  WorldCover tree.
#
#  40 cropland -> 1, with the season caveat stated openly. A ploughed or
#  harvested field is bare soil for part of the year and would be class 4 on that
#  day; cropland is a land *use* that is vegetated through the growing season,
#  which is the season this heat tool models. Attested class 1 reads as grass
#  (53.6 %) plus crop (27.2 %) = 80.8 % vegetated.
#
#  60 bare -> 3, not 4. In Switzerland "bare/sparse" is overwhelmingly alpine
#  rock, scree and moraine, and the header's thermal rule puts those with
#  concrete rather than with soil. Confirmed from the other direction: 44.1 % of
#  attested class 3 reads as WorldCover bare. It is also nearly irrelevant to the
#  fill - only 0.3 % of the remaining 0 cells are bare.
#
#  20 shrubland is mapped for completeness but does essentially nothing here:
#  WorldCover assigned it to 46 cells out of 1.4 M in Switzerland, putting Swiss
#  Gebuesch into tree or grassland instead.
#
#  70 snow/ice and 80 water both -> 5, following the header: glacier and snow are
#  the coldest, highest-albedo surfaces in the country.
LC_WC_CLASS <- c("10" = 4, "20" = 2, "30" = 1, "40" = 1, "50" = 3,
                 "60" = 3, "70" = 5, "80" = 5, "90" = 1, "100" = 4)


# ---------------------------------------------------------------- helpers ---

#' Tile subdirectory for the current resolution.
lc_tile_dir <- function(res = LC_RES) paste0("tiles_", res, "m")

#' Is this tile file complete and readable?
#'
#' `file.exists()` is not the same question, and the difference has teeth. Kill a
#' run mid-write - a session ending, a crash - and the tiles in flight are left
#' on disk with content but a truncated TIFF directory. They are not empty, so
#' size checks pass; they exist, so a restart skips them forever; and
#' terra::vrt() treats an unreadable member as NA rather than an error, so they
#' merge into the national raster as silent holes. Eleven tiles reached a
#' finished 82.8 G-cell raster that way, as 4 x 4.6 km squares of nothing.
#'
#' Opening the file is what actually settles it: a truncated directory fails to
#' parse, and a tile that parses but has the wrong cell count was written against
#' a different grid.
lc_tile_ok <- function(f, cells = NULL){
  if(!file.exists(f) || file.size(f) == 0) return(FALSE)
  r <- try(suppressWarnings(terra::rast(f)), silent = TRUE)
  if(inherits(r, "try-error")) return(FALSE)
  if(!is.null(cells) && terra::ncell(r) != cells) return(FALSE)
  TRUE
}

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

# --------------------------------------------------------- height classes ---

#' Cut points for a ramp: the midpoints of consecutive steps.
#'
#' Separate from both users below because the raster path and the vector path
#' must agree to the metre, and the only way to be sure of that is for them to
#' ask the same function.
lc_step_cuts <- function(steps){
  utils::head(steps$height, -1) + diff(steps$height) / 2
}

#' Metres -> class id, by nearest step. A vector of heights in, a vector of ids
#' out; `na_id` is what an unmeasurable height becomes.
lc_nearest_step <- function(h, steps, na_id = NA_integer_){
  out <- rep(as.integer(na_id), length(h))
  ok  <- !is.na(h)
  if(any(ok)) out[ok] <- as.integer(steps$id[findInterval(h[ok], lc_step_cuts(steps)) + 1L])
  out
}

#' A raster of metres -> a raster of class ids, by nearest step.
#'
#' `right = FALSE` makes the intervals [from, to), so a height exactly on a cut
#' rounds UP - the same convention lc_nearest_step() follows, because they are
#' the same cuts. `lo` is a floor below which there is no object at all, and
#' `others` is what a cell below it becomes. NA stays NA either way.
lc_step_raster <- function(r, steps, lo = -Inf, others = NA){
  cuts <- lc_step_cuts(steps)
  rcl  <- cbind(c(lo, cuts), c(cuts, Inf), steps$id)
  terra::classify(r, rcl, right = FALSE, others = others)
}

#' The vegetation height model -> tree classes.
#'
#' Below LC_CANOPY_MIN there is no crown and the cell is open sky, which is also
#' where the -1 sentinel lc_vhm_tile() returns for a damaged VHM strip lands - so
#' a tile built without vegetation comes out with no canopy rather than with an
#' invented one, exactly as it did when the canopy was a single class.
lc_canopy_class <- function(vhm, steps = LC_TREE_STEPS, min_h = LC_CANOPY_MIN){
  lc_step_raster(vhm, steps, lo = min_h, others = 0)
}

#' swissBUILDINGS3D -> a block class per CELL, or NULL if there is nothing here.
#'
#' NOT per building, and that is the whole design. The 3D outlines and the TLM3D
#' footprints are two generalisations of the same town and they cross each
#' other: 59.6 % of footprints overlap more than one 3D building, so any
#' per-footprint rule has to choose between under-covering (a representative
#' point lands in only 62.7 % of them) and over-reaching (the tallest neighbour
#' wins, and a garage inherits the house). A cell has no such ambiguity - it is
#' covered by one building or by none - and the heat model reads heights per
#' cell anyway. Measured on central Zurich, this gives 90.5 % of TLM3D building
#' cells a surveyed height, and only 0.9 % of the 3D cells fall outside a TLM3D
#' building at all.
#'
#' `fun = "max"` where 3D outlines overlap, because for shading the ridge is
#' what casts. NA where the 3D model says nothing, which is what leaves those
#' cells on the ramp's default step.
lc_block_class_tile <- function(tile, template, gpkg = LC_B3D){
  if(is.null(gpkg) || !file.exists(gpkg)) return(NULL)
  b <- lc_read_tile("buildings3d", tile, gpkg = gpkg)
  if(is.null(b) || nrow(b) == 0) return(NULL)
  b <- b[!is.na(b$GESAMTHOEHE), ]
  if(nrow(b) == 0) return(NULL)
  h <- terra::rasterize(terra::vect(b["GESAMTHOEHE"]), template,
                        field = "GESAMTHOEHE", fun = "max",
                        background = NA, touches = FALSE)
  lc_step_raster(h, LC_BLOCK_STEPS)
}

#' The terrain elevation under each footprint, as the median of its own outline.
#'
#' swissTLM3D geometries are XYZ and the Z is the outline draped on the terrain -
#' measured, not assumed: the per-footprint spread is a median of 0.88 m, and
#' 0.32 m across the Hochhaus of central Zurich, so a tower's footprint is as
#' flat as a shed's. That makes it a usable ground reference and saves reading a
#' DTM at all. The median rather than the minimum because a footprint that clips
#' a kerb or a retaining wall should not take its base from the low corner.
lc_footprint_z <- function(geom){
  n <- nrow(geom)
  if(n == 0) return(numeric(0))
  z <- sf::st_coordinates(sf::st_geometry(geom))
  if(!"Z" %in% colnames(z)) return(rep(NA_real_, n))
  #the LAST L column is the feature index, whether the geometry came back as
  #POLYGON (L1, L2) or MULTIPOLYGON (L1, L2, L3); the factor levels keep the
  #result aligned to `geom` even if a feature contributes no coordinates
  fid <- factor(z[, ncol(z)], levels = seq_len(n))
  as.numeric(tapply(z[, "Z"], fid, stats::median))
}

#' Block class per TLM3D footprint, for the era of the product that predates
#' GESAMTHOEHE. NA where there is no answer, which leaves the default step.
#'
#' This is the route the Valais and Ticino take. It works there precisely
#' because those regions have not been re-surveyed, so the 3D buildings still
#' carry TLM3D's uuids - see the header. Where the product HAS been re-surveyed
#' the ids are gone and lc_block_class_tile() answers instead; a cell that both
#' routes can answer takes the newer one.
lc_block_class_legacy <- function(bld, dm, steps = LC_BLOCK_STEPS,
                                  range = LC_BUILDING_H_RANGE){
  if(is.null(bld) || nrow(bld) == 0 || is.null(dm) || nrow(dm) == 0)
    return(integer(0))
  i <- match(bld$uuid, dm$uuid)
  if(all(is.na(i))) return(rep(NA_integer_, nrow(bld)))
  gz <- lc_footprint_z(bld)
  #A caller that flattened the geometry would reach here with a perfectly valid
  #uuid join and no terrain to subtract from it, and the result would be an
  #empty answer rather than an error - see lc_read_tile(drop_z). Say so.
  if(all(is.na(gz)))
    warning("footprints have no Z, so no legacy height can be derived - ",
            "read them with lc_read_tile(drop_z = FALSE)")
  h <- dm$dach_max[i] - gz
  h[!is.finite(h) | h < range[1] | h > range[2]] <- NA_real_
  lc_nearest_step(h, steps)
}

#' Give a tile's building cells their measured height, in both rasters.
#'
#' Shared by the build and the retrofit so the two cannot drift: whatever is
#' class 8 after the TLM3D footprints have been burnt gets refined here, and
#' a cell neither route can answer keeps the 8 it already had.
#'
#' `bld` is the tile's TLM3D footprints, which the caller has usually read
#' already; without them only the new-era route runs.
lc_apply_block_heights <- function(ground, canopy, tile, gpkg = LC_B3D,
                                   dm = lc_dachmax(), bld = NULL){
  bc <- lc_block_classes(tile, gpkg, dm, bld)
  if(is.null(bc)) return(list(ground = ground, canopy = canopy))
  list(ground = terra::ifel(ground == 8 & !is.na(bc), bc, ground),
       canopy = terra::ifel(canopy == 8 & !is.na(bc), bc, canopy))
}

#' Both eras' answers on one raster, NA where neither has one.
#'
#' Split out from lc_apply_block_heights() so that qa_building_heights() can
#' measure the same raster the build applies rather than a second implementation
#' of it - a coverage figure from a reimplementation measures the
#' reimplementation.
lc_block_classes <- function(tile, gpkg = LC_B3D, dm = lc_dachmax(), bld = NULL){
  bc <- lc_block_class_tile(tile, lc_template(tile, NA), gpkg)

  #the legacy route fills in where the re-surveyed one is silent; terra::cover()
  #is what makes "the newer survey wins" true cell by cell rather than by region
  cls <- lc_block_class_legacy(bld, dm)
  if(length(cls) && any(!is.na(cls))){
    lg <- lc_template(tile, NA)
    for(k in sort(unique(cls[!is.na(cls)])))
      lg <- lc_burn(lg, bld[!is.na(cls) & cls == k, ], k)
    bc <- if(is.null(bc)) lg else terra::cover(bc, lg)
  }
  bc
}

#' Process-local cache for the legacy roof elevations.
#'
#' A worker sources this file once and then builds tiles in a loop, so a table
#' opened per tile is opened 4500 times. A GeoPackage does not need this (the
#' read is indexed and lazy); an RDS of roof elevations does.
.lc_cache <- new.env(parent = emptyenv())

lc_dachmax <- function(path = LC_B3D_DM){
  key <- paste0("dm:", path)
  if(exists(key, envir = .lc_cache, inherits = FALSE)) return(get(key, envir = .lc_cache))
  x <- if(file.exists(path)) readRDS(path) else NULL
  assign(key, x, envir = .lc_cache)
  x
}

#' Substitute the height variants away, leaving base material ids.
#'
#' The mirror of paintBaseRaster() in R/paintbrush_helpers.R, restated here
#' because data-raw does not load the package.
lc_base_raster <- function(r){
  from <- c(LC_TREE_STEPS$id, LC_BLOCK_STEPS$id)
  to   <- c(rep(7L, nrow(LC_TREE_STEPS)), rep(8L, nrow(LC_BLOCK_STEPS)))
  keep <- from != to
  if(!any(keep)) return(r)
  terra::subst(r, from = from[keep], to = to[keep], others = NULL)
}

#' Coarsen class ids by a two-stage modal vote: material first, height second.
#'
#' A plain modal on height ids loses the material. Take a 5 m cell holding
#' 8 x tree@10 m, 7 x tree@25 m and 10 x grass: it is 60 % tree, but the
#' commonest single id is grass and a one-stage vote calls it grass. So vote on
#' the MATERIAL first, and let the height through only where the two agree -
#' otherwise fall back to that material's default step.
#'
#' The same function exists as heat_modal_class() in R/heat_helpers.R, where the
#' heat model coarsens 1 m paint onto its 5 m grid. Keep the two in step.
lc_modal_class <- function(r, fact){
  if(fact <= 1) return(r)
  var  <- terra::aggregate(r, fact = fact, fun = "modal", na.rm = TRUE)
  base <- terra::aggregate(lc_base_raster(r), fact = fact, fun = "modal", na.rm = TRUE)
  terra::ifel(lc_base_raster(var) == base, var, base)
}

#' Assert that the two step tables still say what PAINT_CATEGORIES says.
#'
#' The palette is the source of truth and these tables restate it, so they can
#' drift. verify_heat_model.R makes the same assertion from the package side;
#' this one is here so a build run from a checkout with a moved palette stops
#' before it writes 4500 tiles of the wrong ids.
lc_check_steps <- function(paint_helpers = file.path("R", "paintbrush_helpers.R")){
  if(!file.exists(paint_helpers)){
    message("cannot check the ramps: ", paint_helpers, " not found")
    return(invisible(FALSE))
  }
  e <- new.env(parent = globalenv())
  suppressMessages(sys.source(paint_helpers, envir = e))
  pc <- get("PAINT_CATEGORIES", envir = e)
  for(st in list(LC_TREE_STEPS, LC_BLOCK_STEPS)){
    i <- match(st$id, pc$id)
    stopifnot(!anyNA(i), identical(as.numeric(pc$height[i]), as.numeric(st$height)))
  }
  #and every step of the two ramps this file assigns must appear here, or the
  #national data would be unable to express a height the bar offers. The
  #artificial canopy is deliberately not among them: it is bridges, and TLM3D
  #gives a bridge no height, so it stays on its default step.
  writes <- pc$id[!is.na(pc$height) & pc$base %in% c(7L, 8L)]
  stopifnot(setequal(writes, c(LC_TREE_STEPS$id, LC_BLOCK_STEPS$id)))
  invisible(TRUE)
}


# ------------------------------------------------------ building heights ----

#' Render swissBUILDINGS3D down to one indexed LV95 GeoPackage of heights.
#'
#' Run once, before the national build, like lc_prepare_osm() and
#' lc_prepare_worldcover() - and for the same reason: the tile loop reads the
#' result 4500 times and must never touch a 13.5 GB archive to do it.
#'
#' `Floor` is the layer to take, and which layer it is matters:
#'
#'   - its geometry is a MultiPolygon, so terra and sf can both read it. Roof,
#'     Roof_solid and Building_solid are TINs; terra::vect() refuses one outright
#'     and sf hands back an sfc_TIN with no st_coordinates() method.
#'   - it carries GESAMTHOEHE, the total height above the building's own terrain
#'     point, which is the number the shadow march wants. DACH_MAX, the only
#'     height the TIN layers carry, is a roof ELEVATION and would need a terrain
#'     model subtracted from it.
#'
#' TWO ARTEFACTS, because the delivery has two eras in it - see the header.
#'
#'   LC_B3D      the outlines that carry GESAMTHOEHE, as an indexed LV95
#'               GeoPackage. 1.72 M of Floor's 2.57 M buildings.
#'               Rasterised per cell by lc_block_class_tile().
#'   LC_B3D_DM   uuid -> DACH_MAX for the rest, from Building_solid. Joined to
#'               the TLM3D footprints by lc_block_class_legacy(), which is the
#'               only route that works where GESAMTHOEHE is absent.
#'
#' The split is by `GESAMTHOEHE IS NULL`, evaluated in GDAL, so the two are
#' complementary by construction and no building is in both.
#'
#' Download LC_B3D_SRC and EXTRACT it - see LC_B3D_GDB for why reading the zip in
#' place is not a shortcut but a wall. A `src` ending in .zip is still accepted,
#' for a small per-sheet file where it hardly matters.
lc_prepare_buildings3d <- function(src = LC_B3D_GDB, out = LC_B3D,
                                   out_dm = LC_B3D_DM, layer = "Floor"){
  if(!file.exists(src)){
    stop("swissBUILDINGS3D not found at ", src,
         "\n  download it from: ", LC_B3D_SRC,
         "\n  and extract it:    tar -xf <archive>.gdb.zip -C <dir>")
  }
  t0  <- Sys.time()
  dsn <- if(grepl("[.]zip$", src)) paste0("/vsizip/", src) else src
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  unlink(out)
  #-dim XY because the height is an attribute and the Z of the outline is just
  #the terrain again; -nln so the tile loop can name the layer without caring
  #which source layer it came from
  sf::gdal_utils("vectortranslate", source = dsn, destination = out,
                 options = c("-f", "GPKG", "-dim", "XY",
                             "-nlt", "PROMOTE_TO_MULTI",
                             "-nln", "buildings3d",
                             "-select", "GESAMTHOEHE",
                             "-where", "GESAMTHOEHE IS NOT NULL",
                             layer))
  n <- sf::st_layers(out)$features[1]
  message(sprintf("wrote %s (%s buildings with a height, %.1f min)", out,
                  format(n, big.mark = " "),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  #and the other era. Building_solid rather than Floor because its geometry is
  #never read here anyway (-nlt NONE) and it is the layer that carries
  #DACH_MAX; Floor does not. The WHERE is the complement of the one above.
  t1  <- Sys.time()
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  sf::gdal_utils("vectortranslate", source = dsn, destination = tmp,
                 options = c("-f", "CSV", "-nlt", "NONE",
                             "-select", "UUID,DACH_MAX",
                             "-where", "GESAMTHOEHE IS NULL AND DACH_MAX IS NOT NULL",
                             "Building_solid"))
  d <- utils::read.csv(tmp, colClasses = "character")
  names(d) <- tolower(names(d))
  d$dach_max <- suppressWarnings(as.numeric(d$dach_max))
  d <- d[nzchar(d$uuid) & !is.na(d$dach_max), c("uuid", "dach_max")]
  d <- d[!duplicated(d$uuid), ]
  saveRDS(d, out_dm)
  message(sprintf("wrote %s (%s legacy roof elevations, %.1f min)", out_dm,
                  format(nrow(d), big.mark = " "),
                  as.numeric(difftime(Sys.time(), t1, units = "mins"))))
  invisible(c(gpkg = out, dachmax = out_dm))
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

lc_vhm_tile <- function(tile, template, out_dir = LC_OUT_DIR,
                        on_fail = c("flat", "null")){
  on_fail <- match.arg(on_fail)
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
    #`on_fail = "null"` is for a caller that must not treat "no vegetation" as an
    #answer. A fresh build can: it writes a tile with no bush and no canopy, and
    #logs it. A RETROFIT cannot - reclassifying crowns against an all -1 height
    #field would map every existing tree to open sky and quietly delete the
    #canopy of that tile. height_ground_canopy_CH() asks for NULL and skips.
    if(on_fail == "null") return(NULL)
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
lc_read_tile <- function(layer, tile, gpkg = LC_TLM3D, drop_z = TRUE){
  wkt <- sf::st_as_text(sf::st_as_sfc(sf::st_bbox(
    c(xmin = tile$xmin, ymin = tile$ymin, xmax = tile$xmax, ymax = tile$ymax),
    crs = 2056)))
  x <- try(sf::st_read(gpkg, layer = layer, wkt_filter = wkt, quiet = TRUE),
           silent = TRUE)
  if(inherits(x, "try-error") || nrow(x) == 0) return(NULL)
  #TLM3D geometries are XYZ and terra wants them flat, so this drops Z by
  #default. `drop_z = FALSE` is for the one caller that needs it: the TLM3D
  #footprint Z IS the terrain, and lc_block_class_legacy() subtracts it from a
  #roof elevation. Flattening here silently made lc_footprint_z() return all NA
  #and the legacy route assign nothing at all - no error, just an empty answer
  #and a coverage figure of zero across the Valais. lc_burn() flattens whatever
  #it is given, so keeping Z costs the caller nothing.
  if(drop_z) sf::st_zm(x, drop = TRUE, what = "ZM") else x
}

#' Burn `geom` into `x` as `value`, leaving `x` untouched where geom is absent.
lc_burn <- function(x, geom, value){
  if(is.null(geom) || nrow(geom) == 0) return(x)
  #flatten here rather than trusting the caller: terra ignores Z with a warning,
  #and lc_read_tile(drop_z = FALSE) deliberately hands over XYZ geometries
  v <- terra::vect(sf::st_zm(sf::st_geometry(geom), drop = TRUE, what = "ZM"))
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

#' Mark the container polygons of an Areal layer (see LC_AREALE_WEAK).
#'
#' Writes 1 into `x` where a weak areal large enough to matter covers it,
#' leaving `x` alone elsewhere. Kept separate from lc_burn_crosswalk() because
#' the two answer different questions: that one says what the cell is, this one
#' says how much the answer is worth.
lc_burn_weak <- function(x, geom, oa = LC_AREALE_WEAK, min_m2 = LC_WEAK_MIN_M2){
  if(is.null(geom) || nrow(geom) == 0) return(x)
  #st_read(wkt_filter=) returns whole features rather than clipping them, so
  #this is the area of the entire areal and not of the part inside this tile -
  #which is what the threshold is about
  sel <- geom$objektart %in% oa & as.numeric(sf::st_area(geom)) >= min_m2
  if(!any(sel)) return(x)
  lc_burn(x, geom[sel, ], 1)
}

#' Cells a guess is still allowed to write into.
#'
#' Two kinds, and the second is the whole point of LC_AREALE_WEAK:
#'
#'   - `ground == 0`: nothing has spoken at all. This is what steps 8 and 9 have
#'     always filled.
#'   - a weak container cell that no later step touched, under open sky. `g3` is
#'     `ground` frozen at the end of step 3, so a cell a surveyed step (rail,
#'     road, wall, stream, building) rewrote no longer equals it and is
#'     protected - one raster copy, and no extra rasterize() passes.
#'
#' `canopy == 0` is deliberate, not incidental. The measurements behind
#' LC_WC_CLASS and behind LC_AREALE_WEAK are both on cells the satellite can
#' actually see; under a crown it reports the crown. lc_fill_ground()'s rule 1
#' ("the ground under a crown is soil") was validated against *unclassified*
#' cells, not against a container's fallback, so under-canopy weak cells keep
#' their nominal class. That confines this change to exactly what was measured.
lc_soft_cells <- function(ground, g3, weak, canopy){
  (ground == 0) | (weak == 1 & ground == g3 & canopy == 0)
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


# ------------------------------------------------------ WorldCover backfill --

#' Render ESA WorldCover down to one LV95 10 m raster covering Switzerland.
#'
#' Run once, like lc_prepare_osm(), and for the same reason: the tile loop reads
#' this 4500 times and must never reproject. Reprojection is `method = "near"`
#' because the values are class ids - bilinear would average class 30 and class
#' 50 into a class 40 that means nothing.
#'
#' WHY SATELLITE AND NOT THE ORTHOPHOTO. qa_ground_vs_swissimage() below already
#' reads SWISSIMAGE, so the obvious move is to classify green-vs-grey from it and
#' skip a new dependency. It does not work. SWISSIMAGE is a rolling mosaic flown
#' over a three-year cycle, and different regions are flown in different seasons:
#' the Valais tiles are leaf-off spring imagery - bare vineyards, brown
#' hillsides, deep low-sun shadow - while the Mittelland tiles are high summer.
#' Measured on that imagery, the greenness separating grass from asphalt was 0.026
#' vs 0.014 at Sion and 0.073 vs 0.028 at Payerne, so any single threshold
#' classifies most of Valais as unvegetated. Sentinel-2 from one August fixes
#' that - grass NDVI came out at 0.52 / 0.44 / 0.51 across the same three places -
#' and WorldCover is that same instrument already classified and validated, which
#' beats a threshold tuned here by hand.
#'
#' The cost is resolution: a 10 m product on a 1 m grid, carrying 10 m blocks.
#' That is the same compromise the vegetation height model already makes at 5 m
#' (see the header), and it is acceptable for the same reason - the cells this
#' fills are large homogeneous parcels, not the 1 m detail the grid exists for.
lc_prepare_worldcover <- function(src = LC_WC_SRC, tiles = LC_WC_TILES,
                                  out = LC_WORLDCOVER, bb = LC_WC_BB,
                                  res = LC_WC_RES){
  parts <- list()
  for(tl in tiles){
    r <- try(terra::rast(paste0(src, tl, "_Map.tif")), silent = TRUE)
    if(inherits(r, "try-error")){ message("skip (unreachable): ", tl); next }
    ie <- terra::intersect(terra::ext(r), terra::ext(bb[["xmin"]], bb[["xmax"]],
                                                     bb[["ymin"]], bb[["ymax"]]))
    if(is.null(ie)) next
    parts[[tl]] <- terra::crop(r, ie)
    message(sprintf("  %-9s %s", tl, paste(dim(parts[[tl]])[1:2], collapse = " x ")))
  }
  stopifnot(length(parts) > 0)
  wc <- if(length(parts) == 1) parts[[1]] else do.call(terra::merge, unname(parts))

  #Write the merge out before reprojecting, and reopen it. This looks like a
  #pointless round trip and is not: `parts` are crops of /vsicurl sources, so the
  #merge is lazy and every block terra::project() asks for is re-fetched over the
  #network. Left lazy, the projection stops making progress entirely - it spent
  #12 minutes without advancing a byte. Materialised first, it is local I/O.
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  f84 <- file.path(dirname(out), "worldcover_ch_4326.tif")
  wc  <- terra::writeRaster(wc, f84, datatype = "INT1U", gdal = LC_GDAL,
                            overwrite = TRUE)
  wc  <- terra::rast(f84)

  #project onto the LV95 paint grid, snapped so that every WorldCover cell edge
  #is also a 1 m tile edge and the tile loop's crop never needs to resample
  tmpl <- terra::rast(terra::ext(LC_EXT[["xmin"]], LC_EXT[["xmax"]],
                                 LC_EXT[["ymin"]], LC_EXT[["ymax"]]),
                      resolution = res, crs = "EPSG:2056")
  wc <- terra::project(wc, tmpl, method = "near")

  terra::writeRaster(wc, out, datatype = "INT1U", gdal = LC_GDAL, overwrite = TRUE)
  message("wrote ", out)
  invisible(out)
}

#' Open the prepared WorldCover mosaic, or NULL if it has not been built.
#'
#' NULL rather than an error so a build still runs without it - the result is
#' then the pre-WorldCover raster, which is a worse map but not a broken one.
#' Opened per call rather than cached in a global: a SpatRaster is an external
#' pointer and does not survive being exported to a parallel worker, so each
#' worker has to open the file itself. That happens for free here, because this
#' is lc_build_tile()'s default argument and is evaluated inside the worker.
lc_worldcover <- function(path = LC_WORLDCOVER){
  if(!file.exists(path)){
    message("WorldCover mosaic not found at ", path,
            " - run lc_prepare_worldcover(); building without step 9")
    return(NULL)
  }
  terra::rast(path)
}

#' Fill a ground tile's remaining 0 cells. Returns the tile, unchanged where it
#' already said something.
#'
#' Two rules, in order:
#'
#'  1. Under a tree crown the ground is soil, full stop - no imagery is consulted,
#'     because a satellite looking at a forest reports the canopy and cannot see
#'     what it stands on. This is the same rule LC_OSM_CLASS applies to "forest".
#'  2. Everything else takes LC_WC_CLASS.
#'
#' A cell WorldCover cannot classify either stays 0. The class is meant to be
#' honest about ignorance, and inventing a value for it would defeat the point.
#'
#' `soft` widens what counts as fillable. Left NULL it is `ground == 0`, which
#' is what fill_ground_canopy_CH() passes and is exactly the old behaviour -
#' the retrofit path walks finished tiles and has no way to know which cells
#' were containers. lc_build_tile() has that knowledge and hands over the weak
#' container cells as well (lc_soft_cells()), so a campus TLM3D never surveyed
#' can be arbitrated per cell instead of inheriting whatever class the enclosing
#' polygon nominated.
lc_fill_ground <- function(ground, canopy, wc, soft = NULL,
                           crosswalk = LC_WC_CLASS){
  gap <- if(is.null(soft)) ground == 0 else soft
  if(!any(terra::values(gap), na.rm = TRUE)) return(ground)

  w <- terra::crop(wc, terra::ext(ground))
  #disagg rather than resample: LC_WC_RES / LC_RES is a whole number on a shared
  #origin, so this is a block copy that cannot shift a cell edge - the same
  #argument lc_vhm_tile() makes for the vegetation height model
  w <- terra::disagg(w, fact = LC_WC_RES / LC_RES, method = "near")
  #Assert before forcing the extent. lc_prepare_worldcover() projects onto the
  #whole of LC_EXT, so the crop above always returns the full tile - but if that
  #ever stops being true, a partial crop stretched onto the tile extent would
  #silently displace every class it writes. Fail loudly instead.
  if(!all(dim(w)[1:2] == dim(ground)[1:2])){
    stop(sprintf("WorldCover crop is %s, tile is %s - the mosaic does not cover this tile",
                 paste(dim(w)[1:2], collapse = "x"), paste(dim(ground)[1:2], collapse = "x")))
  }
  terra::ext(w) <- terra::ext(ground)
  w <- terra::subst(w, from = as.integer(names(crosswalk)),
                    to = unname(crosswalk), others = NA)

  #rule 1 wins over rule 2.
  #
  #EVERY tree class, not the one this used to test. A crown is ids 10/11/7/12/13
  #since heights became classes, and `canopy == 7` would silently match only the
  #15 m step - so a rerun of fill_ground_canopy_CH() would let the satellite
  #reclassify the ground under four fifths of the forests in Switzerland, having
  #reported success. There is no error to notice: the rule just stops firing.
  w <- terra::ifel(terra::subst(canopy, from = LC_TREE_STEPS$id,
                                to = rep(1L, nrow(LC_TREE_STEPS)),
                                others = 0L) == 1L, 4, w)

  #There was briefly a third rule here forbidding the satellite from writing
  #water into a container cell. It was written to explain a Schwimmbadareal at
  #Sion that came out 72.5 % water, and it was wrong twice over: the water came
  #from step 8, not from here, and the lake is real - TLM3D's own Bodenbedeckung
  #maps Stehende Gewaesser inside that polygon and OSM draws 15.3 of its 20 ha
  #as water. Water inside a container areal is usually a water body the container
  #was painting over. Do not add that rule back without evidence of the opposite.
  terra::ifel(gap & !is.na(w), w, ground)
}


# ------------------------------------------------------------- tile build ---

#' Build one tile of both rasters and write them.
lc_build_tile <- function(tile, out_dir = LC_OUT_DIR, overwrite = FALSE,
                          min_road_width = LC_MIN_ROAD_WIDTH,
                          wc = lc_worldcover(), b3d = LC_B3D){
  #the resolution is in the directory name, not decoration: tile ids restart at
  #01_01 for every grid, so a 1 m run whose tiles sat next to the 5 m run's
  #would find ground_01_01.tif already present and skip it - silently welding a
  #5 m tile into a 1 m raster
  f_ground <- file.path(out_dir, lc_tile_dir(), "ground", paste0("ground_", tile$tile_id, ".tif"))
  f_canopy <- file.path(out_dir, lc_tile_dir(), "canopy", paste0("canopy_", tile$tile_id, ".tif"))
  #readable, not merely present: see lc_tile_ok(). A tile left half-written by an
  #interrupted run must be rebuilt, not skipped for the rest of time.
  cells <- (LC_TILE_M[1] / LC_RES) * (LC_TILE_M[2] / LC_RES)
  if(!overwrite && lc_tile_ok(f_ground, cells) && lc_tile_ok(f_canopy, cells)){
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

  # 3. designated areas. `weak` records which of them are containers rather
  #    than surfaces (LC_AREALE_WEAK) and `g3` freezes what the Areale decided,
  #    so steps 4-7 can be told apart from the fallback further down: a cell a
  #    surveyed step rewrote no longer equals g3. See lc_soft_cells().
  #
  #    An Areal burns over step 1, so a container also overwrites whatever
  #    Bodenbedeckung attested underneath it - and that turns out to be most of
  #    the damage these polygons do. A 20 ha Schwimmbadareal at Sion is a
  #    bathing lake that TLM3D maps as Stehende Gewaesser, painted asphalt; a
  #    39 ha Freizeitanlagenareal is 15 ha of Wald and Gehoelzflaeche, painted
  #    asphalt. Letting steps 8 and 9 back into those cells restores both, which
  #    is why this change moves classes 5 and 4 and not only class 1 - water,
  #    bush and building are NOT invariant here, unlike under the step 9
  #    rollout. Fixing it at the source instead would mean reordering step 1
  #    against step 3 for the whole country, which is a much larger question
  #    than the crosswalk.
  weak <- lc_template(tile, 0)
  for(layer in names(LC_AREALE)){
    ar     <- lc_read_tile(layer, tile)
    ground <- lc_burn_crosswalk(ground, ar, LC_AREALE[[layer]])
    weak   <- lc_burn_weak(weak, ar)
  }
  g3 <- ground

  #canopy starts as the crowns, each at its measured height (lc_canopy_class);
  #bridges and buildings are stacked on top below
  canopy <- lc_canopy_class(vhm)

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
  #
  #    TLM3D decides WHERE a building is, swissBUILDINGS3D how tall it is, and
  #    the two are resolved per cell rather than per footprint - see
  #    lc_block_class_tile(). A cell the 3D model does not cover keeps the
  #    class 8 burnt here, which is the ramp's 10 m default and exactly what the
  #    whole layer was before.
  #drop_z = FALSE: the footprint Z is the terrain the legacy height route
  #subtracts, and lc_burn() flattens it again on the way into the raster
  bld <- lc_read_tile("tlm_bauten_gebaeude_footprint", tile, drop_z = FALSE)
  if(!is.null(bld)){
    bld    <- bld[!bld$objektart %in% LC_BUILDING_DROP, ]
    ground <- lc_burn(ground, bld, 8)
    canopy <- lc_burn(canopy, bld, 8)
  }
  bh     <- lc_apply_block_heights(ground, canopy, tile, b3d, bld = bld)
  ground <- bh$ground
  canopy <- bh$canopy

  # 8. OSM backfill, last and lowest priority. Everything above this line is
  #    surveyed; this is the only step that guesses, so it is confined to the
  #    cells lc_soft_cells() allows and can never overwrite an attested class.
  #    Burning it into a scratch raster first is what enforces that: the
  #    crosswalk's own internal precedence resolves among OSM polygons, then a
  #    single ifel() lets the result through only where nothing else spoke.
  #
  #    "Nothing else spoke" now also covers a weak container cell - a drawn
  #    parcel is a better answer than a campus boundary's fallback, and it comes
  #    ahead of step 9 for the reason step 9's own comment gives: a drawing of a
  #    parcel beats a classified satellite pixel where one exists.
  osm <- lc_read_tile("landcover", tile, gpkg = LC_OSM)
  if(!is.null(osm)){
    fill <- lc_template(tile, 0)
    for(cls in names(LC_OSM_CLASS)){
      sel <- osm$cls == as.integer(cls)
      if(any(sel)) fill <- lc_burn(fill, osm[sel, ], as.numeric(cls))
    }
    soft   <- lc_soft_cells(ground, g3, weak, canopy)
    ground <- terra::ifel(soft & fill > 0, fill, ground)
  }

  # 9. WorldCover backfill, lower still: only what step 8 also declined to
  #    answer. Kept separate from step 8 rather than folded into it because the
  #    two guess from different evidence - OSM is somebody's drawing of a parcel
  #    boundary, this is a classified satellite pixel - and because the order
  #    matters: a drawn parcel is the better answer where one exists.
  #
  #    The predicate is recomputed rather than reused: step 8 may have written
  #    into a weak cell, and a cell OSM has now answered is no longer soft.
  if(!is.null(wc)){
    ground <- lc_fill_ground(ground, canopy, wc,
                             soft = lc_soft_cells(ground, g3, weak, canopy))
  }

  terra::writeRaster(ground, f_ground, datatype = "INT1U", gdal = LC_GDAL, overwrite = TRUE)
  terra::writeRaster(canopy, f_canopy, datatype = "INT1U", gdal = LC_GDAL, overwrite = TRUE)
  invisible(c(ground = f_ground, canopy = f_canopy))
}


# ------------------------------------------------------------------ build ---

#' The objektarten whose class this revision actually changed: the two airport
#' areale that moved to grass, plus everything in LC_AREALE_WEAK. This is the
#' rebuild set - lc_affected_tiles() defaults to it.
LC_CHANGED_DEFAULT <- list(
  tlm_areale_verkehrsareal = c("Flughafenareal", "Flugplatzareal",
                               "Oeffentliches Parkplatzareal",
                               "Privates Parkplatzareal", "Rastplatzareal"),
  tlm_areale_nutzungsareal = c("Spitalareal", "Schul- und Hochschulareal",
                               "Kraftwerkareal"),
  tlm_areale_freizeitareal = c("Freizeitanlagenareal", "Schwimmbadareal")
)

#' The same set plus a control group, for qa_areal_vs_worldcover(). The controls
#' are deliberately *not* in the rebuild set: Gleisareal, Abwasserreinigungsareal
#' and Golfplatzareal did not change class, so rebuilding their tiles would cost
#' ~250 tiles to produce identical output. They are here because a measurement
#' of the treated types means nothing without ground that should not have moved.
LC_AFFECTED_DEFAULT <- utils::modifyList(LC_CHANGED_DEFAULT, list(
  tlm_areale_verkehrsareal = c(LC_CHANGED_DEFAULT$tlm_areale_verkehrsareal,
                               "Gleisareal"),
  tlm_areale_nutzungsareal = c(LC_CHANGED_DEFAULT$tlm_areale_nutzungsareal,
                               "Abwasserreinigungsareal"),
  tlm_areale_freizeitareal = c(LC_CHANGED_DEFAULT$tlm_areale_freizeitareal,
                               "Golfplatzareal")
))

#' Which grid tiles a crosswalk change can possibly alter.
#'
#' Changing LC_AREALE or LC_AREALE_WEAK only affects tiles that intersect the
#' polygons concerned - ~700 of 4500 for the container-areal fix - so the
#' rebuild is 40 minutes rather than the ~23 h a national one costs. Derive that
#' set, never write it down: a hand-listed set goes stale the moment the
#' crosswalk moves again, and the failure mode is a national raster that is half
#' fixed and looks finished.
#'
#' Defaults to LC_CHANGED_DEFAULT and not to the QA set: the control objektarten
#' did not change class, so including them would rebuild ~250 tiles to produce
#' byte-identical output.
lc_affected_tiles <- function(objektarten = LC_CHANGED_DEFAULT,
                              gpkg = LC_TLM3D, min_m2 = LC_WEAK_MIN_M2,
                              tiles = NULL){
  if(is.null(tiles)) tiles <- lc_tile_grid()
  grid <- sf::st_sf(
    row = seq_len(nrow(tiles)),
    geometry = sf::st_sfc(lapply(seq_len(nrow(tiles)), function(i){
      sf::st_polygon(list(cbind(
        c(tiles$xmin[i], tiles$xmax[i], tiles$xmax[i], tiles$xmin[i], tiles$xmin[i]),
        c(tiles$ymin[i], tiles$ymin[i], tiles$ymax[i], tiles$ymax[i], tiles$ymin[i]))))
    }), crs = 2056))

  hit <- integer(0)
  for(ly in names(objektarten)){
    a <- try(sf::st_read(gpkg, layer = ly, quiet = TRUE), silent = TRUE)
    if(inherits(a, "try-error")){ message("skip (unreadable): ", ly); next }
    a <- sf::st_zm(a, drop = TRUE, what = "ZM")
    a <- a[a$objektart %in% objektarten[[ly]], ]
    #an objektart that changed class counts at any size; a weak one only acts
    #above LC_WEAK_MIN_M2, so a smaller polygon cannot have moved and its tiles
    #do not need rebuilding
    if(nrow(a) > 0){
      small <- a$objektart %in% LC_AREALE_WEAK &
               as.numeric(sf::st_area(a)) < min_m2
      a <- a[!small, ]
    }
    if(nrow(a) == 0) next
    hit <- union(hit, unlist(sf::st_intersects(sf::st_geometry(a),
                                               sf::st_geometry(grid))))
    message(sprintf("  %-28s %5d polygons", ly, nrow(a)))
  }
  out <- tiles[sort(unique(hit)), , drop = FALSE]
  message(sprintf("%d of %d tiles affected (%.1f %%)",
                  nrow(out), nrow(tiles), 100 * nrow(out) / nrow(tiles)))
  out
}


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
                                   src = "data-raw/generate_ground_canopy_CH.r"){
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
    #every member is opened before the VRT is built, because terra::vrt() does
    #not check: an unreadable tile becomes NA in the mosaic and the merge reports
    #success. Better to refuse to merge and name the tiles than to hand over a
    #raster with holes in it.
    cells <- (LC_TILE_M[1] / LC_RES) * (LC_TILE_M[2] / LC_RES)
    ok    <- vapply(files, lc_tile_ok, logical(1), cells = cells, USE.NAMES = FALSE)
    if(!all(ok)){
      stop(sprintf("%d unreadable %s tile(s), refusing to merge: %s",
                   sum(!ok), what, paste(tiles$tile_id[!ok], collapse = ", ")))
    }
    v     <- terra::vrt(files, file.path(out_dir, paste0(what, "_CH_1m.vrt")), overwrite = TRUE)
    f     <- file.path(out_dir, paste0(what, "_CH_1m.tif"))
    terra::writeRaster(v, f, datatype = "INT1U",
                       gdal = c(LC_GDAL, "BIGTIFF=YES"), overwrite = TRUE)
    out[what] <- f
    message("wrote ", f)
  }
  invisible(out)
}


# ----------------------------------------------------- retrofit the backfill -

#' Apply step 9 to an already-built set of tiles, without rebuilding them.
#'
#' The national build is ~23 h and reads a 10.8 GB GeoPackage 4500 times. Step 9
#' reads neither, and touches only cells every other step declined to classify,
#' so re-deriving the other eight steps to get it would be wasted work. This
#' walks the finished ground tiles instead, applies lc_fill_ground(), and writes
#' to a *parallel* directory rather than over the originals.
#'
#' Writing beside rather than over is the whole point of the design: it keeps
#' "what was surveyed or drawn" and "what was inferred from a satellite"
#' separable after the fact. Diff the two rasters and you have the provenance
#' mask, at the cost of disk rather than of a second value per cell. The national
#' merge at the end writes ground_CH_1m.{vrt,tif} and moves the previous pair
#' aside to ground_CH_1m_prefill.{vrt,tif} for exactly the same reason - both
#' extensions together, so a VRT never describes different cells from the .tif
#' sharing its name.
#'
#' Restartable on the same terms as the build: a tile whose output already reads
#' back at the right cell count is skipped unless `overwrite`.
#'
#' Two things to know before running this a second time, neither of them new but
#' both easy to be surprised by:
#'
#'   - `tiles` drives the merge as well as the work. The file list below is
#'     built from whatever `tiles` was passed, so calling this on a subset with
#'     `merge = TRUE` replaces the national raster with a raster of that subset.
#'     Partial reruns pass `merge = FALSE` and take a second, full pass to merge.
#'   - ground_CH_1m_prefill.* is written once and then kept. On any later run the
#'     rename block below takes its file.remove() branch, so the prefill pair
#'     still describes the crosswalk that was in force the first time. It is a
#'     snapshot of provenance, not a mirror of the current build.
fill_ground_canopy_CH <- function(out_dir = LC_OUT_DIR, tiles = NULL,
                                  overwrite = FALSE, merge = TRUE,
                                  wc_path = LC_WORLDCOVER, workers = 1,
                                  src = "data-raw/generate_ground_canopy_CH.r"){
  if(is.null(tiles)) tiles <- lc_tile_grid()
  if(!file.exists(wc_path)){
    stop("WorldCover mosaic not found at ", wc_path, " - run lc_prepare_worldcover() first")
  }
  cells <- (LC_TILE_M[1] / LC_RES) * (LC_TILE_M[2] / LC_RES)
  dir.create(file.path(out_dir, lc_tile_dir(), "ground_filled"),
             recursive = TRUE, showWarnings = FALSE)

  one <- function(i){
    tid <- tiles$tile_id[i]
    f_in  <- file.path(out_dir, lc_tile_dir(), "ground",        paste0("ground_", tid, ".tif"))
    f_can <- file.path(out_dir, lc_tile_dir(), "canopy",        paste0("canopy_", tid, ".tif"))
    f_out <- file.path(out_dir, lc_tile_dir(), "ground_filled", paste0("ground_", tid, ".tif"))
    if(!overwrite && lc_tile_ok(f_out, cells)) return(NULL)
    if(!lc_tile_ok(f_in, cells) || !lc_tile_ok(f_can, cells)){
      message("skip (source tile unreadable): ", tid); return(NULL)
    }
    g <- lc_fill_ground(terra::rast(f_in), terra::rast(f_can), terra::rast(wc_path))
    terra::writeRaster(g, f_out, datatype = "INT1U", gdal = LC_GDAL, overwrite = TRUE)
    NULL
  }

  t_all <- Sys.time()
  if(workers > 1){
    stopifnot(file.exists(src))
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterCall(cl, function(p) suppressMessages(source(p)), normalizePath(src))
    parallel::clusterCall(cl, function(frac){
      d <- file.path(tempdir(), "terra_worker")
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      terra::terraOptions(memfrac = frac, tempdir = d, progress = 0)
      NULL
    }, max(0.5 / workers, 0.03))
    wenv <- new.env(parent = globalenv())
    wenv$tiles <- tiles; wenv$out_dir <- out_dir; wenv$overwrite <- overwrite
    wenv$wc_path <- wc_path; wenv$cells <- cells
    environment(one) <- wenv
    idx <- split(seq_len(nrow(tiles)), ceiling(seq_len(nrow(tiles)) / (workers * 15)))
    for(k in seq_along(idx)){
      parallel::parLapplyLB(cl, idx[[k]], one)
      done <- max(idx[[k]])
      el <- as.numeric(difftime(Sys.time(), t_all, units = "mins"))
      message(sprintf("chunk %d/%d - %d/%d tiles - %.1f min elapsed, ~%.1f min left",
                      k, length(idx), done, nrow(tiles), el,
                      el / done * (nrow(tiles) - done)))
    }
  }else{
    for(i in seq_len(nrow(tiles))){
      one(i)
      if(i %% 100 == 0){
        el <- as.numeric(difftime(Sys.time(), t_all, units = "mins"))
        message(sprintf("%d/%d tiles - %.1f min elapsed, ~%.1f min left",
                        i, nrow(tiles), el, el / i * (nrow(tiles) - i)))
      }
    }
  }
  if(!merge) return(invisible(NULL))

  files <- file.path(out_dir, lc_tile_dir(), "ground_filled",
                     paste0("ground_", tiles$tile_id, ".tif"))
  ok <- vapply(files, lc_tile_ok, logical(1), cells = cells, USE.NAMES = FALSE)
  if(!all(ok)){
    stop(sprintf("%d unreadable filled tile(s), refusing to merge: %s",
                 sum(!ok), paste(tiles$tile_id[!ok], collapse = ", ")))
  }
  #The VRT moves aside with the .tif it describes, and the filled one takes the
  #plain name. Anything else leaves ground_CH_1m.vrt pointing at the *unfilled*
  #tiles while ground_CH_1m.tif beside it is filled - a trap, because rebuilding
  #the .tif from the VRT whose name matches it is the obvious thing to do and it
  #would silently undo step 9. The two names now always describe the same cells:
  #
  #  ground_CH_1m.{vrt,tif}          filled - what the app reads
  #  ground_CH_1m_prefill.{vrt,tif}  surveyed and drawn only - the provenance half
  #
  #Diffing the pair still gives the inferred-cell mask, which was the point.
  for(ext in c("vrt", "tif")){
    old  <- file.path(out_dir, paste0("ground_CH_1m.", ext))
    keep <- file.path(out_dir, paste0("ground_CH_1m_prefill.", ext))
    if(!file.exists(old)) next
    if(!file.exists(keep)) file.rename(old, keep) else file.remove(old)
  }
  v <- terra::vrt(files, file.path(out_dir, "ground_CH_1m.vrt"), overwrite = TRUE)
  f <- file.path(out_dir, "ground_CH_1m.tif")
  terra::writeRaster(v, f, datatype = "INT1U",
                     gdal = c(LC_GDAL, "BIGTIFF=YES"), overwrite = TRUE)
  message("wrote ", f)
  invisible(f)
}

# -------------------------------------------------- retrofit real heights ---

#' Give an already-built set of tiles its real tree and building heights.
#'
#' The national build is ~23 h and reads a 10.8 GB GeoPackage 4500 times.
#' Nothing about the heights changes what a cell IS - a tree stays a tree and a
#' building a building - so re-deriving Bodenbedeckung, the Areale, rail, roads,
#' walls, streams, OSM and WorldCover to reach them would be wasted work. This
#' walks the finished tiles instead and rewrites two kinds of cell.
#'
#' WHY THIS IS EXACTLY A REBUILD AND NOT AN APPROXIMATION, which is the only
#' reason to prefer it:
#'
#'   - class 7 can only have come from the canopy seed in lc_build_tile(),
#'     because that is the only line in this file that writes it;
#'   - class 8 can only have come from step 7. No entry in LC_BB, LC_AREALE,
#'     LC_OSM_CLASS or LC_WC_CLASS emits 8, and lc_soft_cells() protects a
#'     building cell from steps 8 and 9 because it no longer equals `g3`.
#'
#' So the cells this touches are precisely the cells a rebuild would recompute,
#' and it reaches them through the same lc_canopy_class() and
#' lc_apply_block_heights() that a rebuild would call. The driver block builds
#' one test tile both ways and asserts the two agree cell for cell; run that
#' before trusting this over 4500.
#'
#' It is also IDEMPOTENT, which is what makes a partial rerun safe: after a pass
#' the 15 m trees are class 7 again and the 10 m buildings class 8 again, so a
#' second pass reclassifies them from the same VHM and the same roof elevations
#' and arrives at the same answer.
#'
#' Reads `ground_filled` and `canopy`, writes `ground_height` and
#' `canopy_height`. That makes it the LAST link in the chain -
#' build_ground_canopy_CH -> fill_ground_canopy_CH -> here - so a rerun of step 9
#' has to be followed by a rerun of this, exactly as a rebuild of a tile has to
#' be followed by a rerun of step 9. The pre-fill tiles and
#' ground_CH_1m_prefill.* are deliberately left alone: they are a provenance
#' snapshot of what was surveyed and drawn, not a mirror of the current build.
#'
#' One consequence of leaving them, worth knowing before reading that diff: the
#' prefill pair still carries flat class 8 buildings, so diffing it against
#' ground_CH_1m.tif now lights up every building cell as well as every inferred
#' one. The inferred-cell mask is that diff MINUS the cells that are a block
#' class on either side.
#'
#' Restartable and merge-scoped on the same terms as fill_ground_canopy_CH():
#' a tile whose output reads back at the right cell count is skipped unless
#' `overwrite`, and `tiles` drives the merge as well as the work, so a partial
#' rerun must pass `merge = FALSE` and be followed by a full pass to merge.
height_ground_canopy_CH <- function(out_dir = LC_OUT_DIR, tiles = NULL,
                                    overwrite = FALSE, merge = TRUE,
                                    b3d_path = LC_B3D, workers = 1,
                                    src = "data-raw/generate_ground_canopy_CH.r"){
  if(is.null(tiles)) tiles <- lc_tile_grid()
  if(!file.exists(b3d_path)){
    stop("building height GeoPackage not found at ", b3d_path,
         " - run lc_prepare_buildings3d() first")
  }
  cells <- (LC_TILE_M[1] / LC_RES) * (LC_TILE_M[2] / LC_RES)
  for(d in c("ground_height", "canopy_height")){
    dir.create(file.path(out_dir, lc_tile_dir(), d), recursive = TRUE, showWarnings = FALSE)
  }

  one <- function(i){
    tile <- tiles[i, ]
    tid  <- tile$tile_id
    f_g  <- file.path(out_dir, lc_tile_dir(), "ground_filled",  paste0("ground_", tid, ".tif"))
    f_c  <- file.path(out_dir, lc_tile_dir(), "canopy",         paste0("canopy_", tid, ".tif"))
    f_go <- file.path(out_dir, lc_tile_dir(), "ground_height",  paste0("ground_", tid, ".tif"))
    f_co <- file.path(out_dir, lc_tile_dir(), "canopy_height",  paste0("canopy_", tid, ".tif"))
    if(!overwrite && lc_tile_ok(f_go, cells) && lc_tile_ok(f_co, cells)) return(NULL)
    if(!lc_tile_ok(f_g, cells) || !lc_tile_ok(f_c, cells)){
      message("skip (source tile unreadable): ", tid); return(NULL)
    }
    g  <- terra::rast(f_g)
    cn <- terra::rast(f_c)

    #trees. `on_fail = "null"` matters: an all -1 height field would classify
    #every crown as open sky and DELETE the canopy of this tile. A tile whose
    #VHM cannot be read therefore keeps its crowns at the 15 m default - which
    #is what they were - and still gets its buildings, because the two halves
    #are independent and failing at one is no reason to skip the other. The two
    #tiles in vhm_gaps.txt have no canopy at all, so for them this is moot.
    vhm <- lc_vhm_tile(tile, lc_template(tile, 0), out_dir = out_dir, on_fail = "null")
    if(is.null(vhm)) message("VHM unreadable, canopy left flat: ", tid)
    else cn <- terra::ifel(cn == 7, lc_canopy_class(vhm), cn)

    #buildings, in both rasters, through the very function the build calls -
    #which is what makes the equivalence structural rather than a coincidence
    #two implementations happen to share. The TLM3D footprints are read for the
    #legacy route's terrain; the re-surveyed route needs nothing but the tile.
    bld <- lc_read_tile("tlm_bauten_gebaeude_footprint", tile, drop_z = FALSE)
    if(!is.null(bld)) bld <- bld[!bld$objektart %in% LC_BUILDING_DROP, ]
    bh <- lc_apply_block_heights(g, cn, tile, b3d_path, bld = bld)
    g  <- bh$ground
    cn <- bh$canopy

    terra::writeRaster(g,  f_go, datatype = "INT1U", gdal = LC_GDAL, overwrite = TRUE)
    terra::writeRaster(cn, f_co, datatype = "INT1U", gdal = LC_GDAL, overwrite = TRUE)

    #Release before the next tile rather than when R happens to feel like it.
    #A worker builds ~750 tiles in a row and each one holds a dozen 18.4 M-cell
    #rasters; a SpatRaster is an external pointer whose memory is freed by the
    #finalizer, so without a collection here the C++ side accumulates across
    #tiles and the run dies with std::bad_alloc several hundred tiles in - which
    #is exactly how it failed at 10 workers (101 tiles) and at 6 (491). The
    #temp files go too: terra spills to scratch under a divided memfrac, and
    #those are not reclaimed either.
    rm(g, cn, vhm, bh, bld)
    terra::tmpFiles(remove = TRUE)
    gc(FALSE)
    NULL
  }

  t_all <- Sys.time()
  if(workers > 1){
    stopifnot(file.exists(src))
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterCall(cl, function(p) suppressMessages(source(p)), normalizePath(src))
    parallel::clusterCall(cl, function(frac){
      d <- file.path(tempdir(), "terra_worker")
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      terra::terraOptions(memfrac = frac, tempdir = d, progress = 0)
      NULL
    }, max(0.5 / workers, 0.03))
    wenv <- new.env(parent = globalenv())
    wenv$tiles <- tiles; wenv$out_dir <- out_dir; wenv$overwrite <- overwrite
    wenv$b3d_path <- b3d_path; wenv$cells <- cells
    environment(one) <- wenv
    idx <- split(seq_len(nrow(tiles)), ceiling(seq_len(nrow(tiles)) / (workers * 15)))
    for(k in seq_along(idx)){
      parallel::parLapplyLB(cl, idx[[k]], one)
      done <- max(idx[[k]])
      el <- as.numeric(difftime(Sys.time(), t_all, units = "mins"))
      message(sprintf("chunk %d/%d - %d/%d tiles - %.1f min elapsed, ~%.1f min left",
                      k, length(idx), done, nrow(tiles), el,
                      el / done * (nrow(tiles) - done)))
    }
  }else{
    for(i in seq_len(nrow(tiles))){
      one(i)
      if(i %% 50 == 0){
        el <- as.numeric(difftime(Sys.time(), t_all, units = "mins"))
        message(sprintf("%d/%d tiles - %.1f min elapsed, ~%.1f min left",
                        i, nrow(tiles), el, el / i * (nrow(tiles) - i)))
      }
    }
  }
  if(!merge) return(invisible(NULL))

  #The same rename convention fill_ground_canopy_CH() uses and for the same
  #reason: the .vrt moves aside with the .tif it describes, so the two names
  #never come to mean different cells. ground_CH_1m_flat.* / canopy_CH_1m_flat.*
  #are the one-height rasters, kept for the diff that proves this changed only
  #the cells it was supposed to.
  out <- character(0)
  for(what in c("ground", "canopy")){
    files <- file.path(out_dir, lc_tile_dir(), paste0(what, "_height"),
                       paste0(what, "_", tiles$tile_id, ".tif"))
    ok <- vapply(files, lc_tile_ok, logical(1), cells = cells, USE.NAMES = FALSE)
    if(!all(ok)){
      stop(sprintf("%d unreadable %s height tile(s), refusing to merge: %s",
                   sum(!ok), what, paste(tiles$tile_id[!ok], collapse = ", ")))
    }
    for(ext in c("vrt", "tif")){
      old  <- file.path(out_dir, sprintf("%s_CH_1m.%s", what, ext))
      keep <- file.path(out_dir, sprintf("%s_CH_1m_flat.%s", what, ext))
      if(!file.exists(old)) next
      if(!file.exists(keep)) file.rename(old, keep) else file.remove(old)
    }
    v <- terra::vrt(files, file.path(out_dir, paste0(what, "_CH_1m.vrt")), overwrite = TRUE)
    f <- file.path(out_dir, paste0(what, "_CH_1m.tif"))
    terra::writeRaster(v, f, datatype = "INT1U",
                       gdal = c(LC_GDAL, "BIGTIFF=YES"), overwrite = TRUE)
    out[what] <- f
    message("wrote ", f)
  }
  invisible(out)
}


#' Rebuild the 5 m rasters from the 1 m ones.
#'
#' They exist because PAINT_RES was 5 m before the grid was refined, and the
#' copies on disk predate both the OSM backfill and step 9 - which is how a heat
#' run over Sion came back reporting 41 % of the town unclassified when the
#' raster the app actually reads says 5.7 %. Anything still reading a 5 m file is
#' reading a different country. `modal` rather than `mean`: these are class ids -
#' and lc_modal_class() rather than a plain modal, because a patch of mixed-height
#' trees must not lose the trees to a vote split across their heights.
downsample_ground_canopy_CH <- function(out_dir = LC_OUT_DIR, fact = 5){
  out <- character(0)
  for(what in c("ground", "canopy")){
    f_in <- file.path(out_dir, paste0(what, "_CH_1m.tif"))
    if(!file.exists(f_in)){ message("missing: ", f_in); next }
    f_out <- file.path(out_dir, sprintf("%s_CH_%gm.tif", what, LC_RES * fact))
    r <- lc_modal_class(terra::rast(f_in), fact)
    terra::writeRaster(r, f_out, datatype = "INT1U",
                       gdal = c(LC_GDAL, "BIGTIFF=YES"), overwrite = TRUE)
    out[what] <- f_out
    message("wrote ", f_out)
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


#' Rank the Areal crosswalk against ESA WorldCover.
#'
#' This is what found the container-areal problem, and it is how LC_AREALE and
#' LC_AREALE_WEAK should be revised in future rather than by eye. For each
#' objektart it takes the largest polygons in a size band, tabulates the class
#' the build assigned against LC_WC_CLASS-mapped WorldCover, and returns the
#' share of cells we call impervious that the satellite calls grass.
#'
#' Read it with the control group in hand. Objektarten whose class 3 is right
#' come out near zero - Gleisareal measured 0.3 % and Abwasserreinigungsareal
#' 3.4 %, and Golfplatzareal, which is mapped to grass, disagrees at 0.8 % in
#' the same direction. Those are what make Flugplatzareal at 63.2 % and
#' Spitalareal at 34.2 % mean something rather than being an artefact of asking
#' a satellite whether Switzerland is green.
#'
#' Two details here are load-bearing, both learned the hard way:
#'
#'   - the crop is per polygon. crop(ground, vect(all_of_them)) crops the
#'     bounding box of the lot, and for a dozen polygons scattered across the
#'     country that is most of Switzerland; it does not come back.
#'   - only canopy == 0 cells count. Under a crown the satellite reports the
#'     crown, so wooded campus edges would otherwise dominate every row.
qa_areal_vs_worldcover <- function(objektarten = LC_AFFECTED_DEFAULT,
                                   ground_path = file.path(LC_OUT_DIR, "ground_CH_1m.tif"),
                                   canopy_path = file.path(LC_OUT_DIR, "canopy_CH_1m.tif"),
                                   wc_path = LC_WORLDCOVER, gpkg = LC_TLM3D,
                                   n = 6, min_ha = 2, max_ha = 120,
                                   crosswalk = LC_WC_CLASS){
  G  <- terra::rast(ground_path)
  Cn <- terra::rast(canopy_path)
  W  <- terra::rast(wc_path)

  one <- function(geom){
    v  <- terra::vect(sf::st_sfc(geom, crs = 2056))
    g  <- terra::mask(terra::crop(G, v), v)
    cn <- terra::mask(terra::crop(Cn, v), v)
    w  <- terra::resample(terra::crop(W, terra::ext(g), snap = "out"), g,
                          method = "near")
    w  <- terra::subst(w, from = as.integer(names(crosswalk)),
                       to = unname(crosswalk), others = NA)
    data.frame(g  = terra::values(g)[, 1],
               w  = terra::values(w)[, 1],
               cn = terra::values(cn)[, 1])
  }

  out <- list()
  for(ly in names(objektarten)){
    a <- sf::st_zm(sf::st_read(gpkg, layer = ly, quiet = TRUE),
                   drop = TRUE, what = "ZM")
    a$ha <- as.numeric(sf::st_area(a)) / 1e4
    for(oa in objektarten[[ly]]){
      sel <- a$objektart == oa & a$ha >= min_ha & a$ha <= max_ha
      if(!any(sel)) next
      sp <- a[sel, ]
      sp <- sp[order(-sp$ha), ][seq_len(min(n, nrow(sp))), ]
      d  <- do.call(rbind, lapply(sf::st_geometry(sp), one))
      d  <- d[!is.na(d$g) & !is.na(d$w) & !is.na(d$cn) & d$cn == 0, ]
      if(nrow(d) < 100) next
      out[[oa]] <- data.frame(
        objektart = oa, n = nrow(sp), ha = round(sum(sp$ha)),
        ground_1 = round(100 * mean(d$g == 1), 1),
        ground_3 = round(100 * mean(d$g == 3), 1),
        wc_1     = round(100 * mean(d$w == 1), 1),
        wc_3     = round(100 * mean(d$w == 3), 1),
        disagree = round(100 * mean(d$g == 3 & d$w == 1), 1),
        stringsAsFactors = FALSE)
      message(sprintf("  %-28s %2d polygons, %5.0f ha, disagree %4.1f %%",
                      oa, out[[oa]]$n, out[[oa]]$ha, out[[oa]]$disagree))
    }
  }
  if(!length(out)) return(NULL)
  res <- do.call(rbind, out)
  res[order(-res$disagree), ]
}


#' How many building CELLS actually get a height, and which class they land in.
#'
#' The honest number to have before a national run, because it is the one thing
#' about this change that cannot be asserted: swissBUILDINGS3D is a survey, and
#' a building it has not reached keeps the 10 m default. Anything short of 100 %
#' is not a bug, it is coverage - but it decides whether the pass is worth its
#' hours, and it is the figure to quote rather than "buildings now have heights".
#'
#' Cells and not buildings, for the reason lc_block_class_tile() gives, and
#' because cells are what the heat model reads. A large building covered by the
#' 3D survey counts for more than a shed that is not, which is the right
#' weighting for a shadow.
#'
#' Samples grid tiles at random rather than walking the country: the coverage is
#' a property of the two surveys, not of any one place.
qa_building_heights <- function(n = 20, seed = 1, out_dir = LC_OUT_DIR,
                                gpkg = LC_B3D, dm = lc_dachmax(), tiles = NULL){
  if(!file.exists(gpkg)) stop("no building heights at ", gpkg,
                              " - run lc_prepare_buildings3d()")
  if(is.null(tiles)) tiles <- lc_tile_grid()
  set.seed(seed)
  pick  <- tiles[sample(nrow(tiles), min(n, nrow(tiles))), ]
  cells <- (LC_TILE_M[1] / LC_RES) * (LC_TILE_M[2] / LC_RES)

  tot <- 0; got <- 0; tab <- integer(0); outside <- 0; nb3d <- 0
  for(i in seq_len(nrow(pick))){
    tile <- pick[i, ]
    f <- file.path(out_dir, lc_tile_dir(), "ground_filled",
                   paste0("ground_", tile$tile_id, ".tif"))
    if(!lc_tile_ok(f, cells)) next
    vg <- terra::values(terra::rast(f))[, 1]
    isb <- !is.na(vg) & vg == 8L
    if(!any(isb)) next
    bld <- lc_read_tile("tlm_bauten_gebaeude_footprint", tile, drop_z = FALSE)
    if(!is.null(bld)) bld <- bld[!bld$objektart %in% LC_BUILDING_DROP, ]
    bc <- lc_block_classes(tile, gpkg, dm, bld)
    vc <- if(is.null(bc)) rep(NA_integer_, length(vg)) else terra::values(bc)[, 1]
    tot <- tot + sum(isb)
    got <- got + sum(isb & !is.na(vc))
    nb3d <- nb3d + sum(!is.na(vc))
    outside <- outside + sum(!is.na(vc) & !isb)
    tab <- c(tab, vc[isb & !is.na(vc)])
  }
  if(tot == 0){ message("no building cells in the sampled tiles"); return(invisible(NULL)) }

  cat(sprintf("\n%s building cells over %d tiles\n", format(tot, big.mark = " "), nrow(pick)))
  cat(sprintf("  with a surveyed height : %5.1f %%  (the rest keep the 10 m default)\n",
              100 * got / tot))
  cat(sprintf("  3D cells on no TLM3D building : %5.1f %% of all 3D cells\n",
              if(nb3d) 100 * outside / nb3d else NA_real_))
  cat("\nclass on the cells that got one (%):\n")
  ft <- table(factor(tab, levels = LC_BLOCK_STEPS$id))
  for(k in seq_along(ft))
    cat(sprintf("  %2s = %2g m  %5.1f %%\n", names(ft)[k], LC_BLOCK_STEPS$height[k],
                100 * ft[k] / max(1, sum(ft))))
  invisible(list(cells = tot, with_height = got, classes = ft))
}

#' Prove the retrofit and a rebuild agree, on one tile, and time both.
#'
#' The retrofit exists to avoid a ~23 h rebuild, and it is only worth having if
#' it produces the same bytes. It reaches the same lc_canopy_class() and
#' lc_apply_block_heights() a rebuild does, but through a different path -
#' reclassifying finished cells instead of deriving them - so "the same by
#' construction" is an argument, not evidence. This is the evidence.
#'
#' Pass a REAL grid tile, not an ad-hoc test one: the whole question is whether
#' the retrofit reproduces a rebuild when it starts from a tile that was built
#' by the OLD code, and only the national tiles are.
#'
#' The rebuild is written into `tmp_dir`, so nothing under `out_dir` is
#' overwritten and the test can be run again on the same tile.
#'
#' The canopy is compared strictly and the ground only on cells that either side
#' calls a block. That is not a softer test, it is the right one: `ground_filled`
#' carries the OSM and satellite backfill and the stored tile may predate a
#' crosswalk revision, so the two can legitimately differ on a meadow - while the
#' building cells, which are all this pass touches, must agree exactly.
qa_height_retrofit_vs_rebuild <- function(tile, out_dir = LC_OUT_DIR,
                                          tmp_dir = file.path(tempdir(), "lc_rebuild")){
  tid <- tile$tile_id
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  t0 <- Sys.time()
  height_ground_canopy_CH(out_dir, tiles = tile, overwrite = TRUE, merge = FALSE)
  t_retro <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  t0 <- Sys.time()
  lc_build_tile(tile, out_dir = tmp_dir, overwrite = TRUE)
  t_build <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  ok <- TRUE
  for(what in c("canopy", "ground")){
    a <- terra::rast(file.path(out_dir, lc_tile_dir(), paste0(what, "_height"),
                               paste0(what, "_", tid, ".tif")))
    b <- terra::rast(file.path(tmp_dir, lc_tile_dir(), what,
                               paste0(what, "_", tid, ".tif")))
    va <- terra::values(a)[, 1]; vb <- terra::values(b)[, 1]
    cmp <- if(what == "canopy") rep(TRUE, length(va)) else
             (va %in% LC_BLOCK_STEPS$id | vb %in% LC_BLOCK_STEPS$id)
    n <- sum(cmp & va != vb, na.rm = TRUE)
    cat(sprintf("%-7s %-9s over %9d cell(s): %s [%d differ]\n", tid, what,
                sum(cmp), if(n == 0) "IDENTICAL" else "DIFFERS", n))
    if(n > 0){
      d <- table(retrofit = va[cmp & va != vb], rebuild = vb[cmp & va != vb])
      print(d)
    }
    ok <- ok && n == 0
  }
  cat(sprintf("\nretrofit %.1f s   rebuild %.1f s   (rebuild is %.1fx)\n",
              t_retro, t_build, t_build / t_retro))
  cat(sprintf("national estimate at 6 workers: retrofit %.1f h, rebuild %.1f h\n",
              t_retro * 4500 / 6 / 3600, t_build * 4500 / 6 / 3600))
  invisible(ok)
}

#' What the height pass changed, nationally.
#'
#' Reads the rasters the merge kept aside. Three questions, in the order they
#' are worth asking: are the ids legal, did the trees and buildings actually
#' spread across their ramps, and did anything move that should not have.
#' The paint palette, for naming ids in a report.
#'
#' PAINT_CATEGORIES lives in the package and this script does not attach it, so
#' source the one file that defines it rather than making every caller do it.
#' Only the names and heights are wanted here; a missing palette degrades the
#' report, it does not stop it.
lc_palette <- function(path = file.path("R", "paintbrush_helpers.R")){
  if(exists("PAINT_CATEGORIES", inherits = TRUE)) return(get("PAINT_CATEGORIES"))
  if(!file.exists(path)) return(NULL)
  e <- new.env()
  suppressWarnings(try(source(path, local = e), silent = TRUE))
  if(exists("PAINT_CATEGORIES", envir = e)) e$PAINT_CATEGORIES else NULL
}

#' Report what the height pass changed, over every tile in the country.
#'
#' Not over the national rasters, deliberately. Switzerland at 1 m is 8.3e10
#' cells: terra::values() cannot hold that, and terra::freq() across the four
#' 1.5 GB files was measured at roughly eight hours, I/O bound, for an answer
#' that is only an aggregate. The TILES are the national rasters - the merge is
#' a vrt over exactly these files - and one tile is 18.4 M cells, so a worker can
#' hold a flat copy and a height copy side by side and compare them cell for
#' cell. That turns the strict test into an exact one over the whole country
#' rather than a sample: a cell may only have changed if it held the flat class
#' that the ramp replaces, class 8 in the ground and 7 or 8 in the canopy.
#'
#' NA is folded into 0 on both sides before comparing. They mean the same thing
#' here - no class - and a tile written INT1U can carry either.
qa_height_change <- function(out_dir = LC_OUT_DIR, tiles = NULL, workers = 6,
                             src = "data-raw/generate_ground_canopy_CH.r"){
  if(is.null(tiles)) tiles <- lc_tile_grid()
  legal <- c(0L, 1:19)
  ramp  <- list(ground = LC_BLOCK_STEPS$id,
                canopy = c(LC_TREE_STEPS$id, LC_BLOCK_STEPS$id))
  from  <- list(ground = 8L, canopy = c(7L, 8L))

  #One flat vector of counters per tile, so the workers' answers simply add up.
  #Each of the four blocks spans the WHOLE INT1U range, 0:255, not 0:19. That is
  #not defensiveness: binning to 20 made tabulate() drop every id above 19 on the
  #floor, and because the legality check then read the same bins it reported "all
  #ids are in PAINT_CATEGORIES" while 572 ground cells and 73 canopy cells held
  #something else. A counter that cannot see a value must not be the thing that
  #certifies there is none. The blocks are 1:256, 257:512, 513:768, 769:1024,
  #then moved/illegal per layer and the cell total.
  NB  <- 256L                         #INT1U spans 0:255
  OFF <- c(ground = 1L, canopy = 2L * NB + 1L)
  MOV <- 4L * NB + 1L                 #moved/illegal per layer, then the total
  one <- function(i){
    tid <- tiles$tile_id[i]
    acc <- numeric(MOV + 4L)
    rd  <- function(p){
      if(!file.exists(p)) return(NULL)
      v <- as.integer(terra::values(terra::rast(p), mat = FALSE))
      v[is.na(v)] <- 0L
      v
    }
    pair <- function(sub_a, sub_b, what, off, moff, fr){
      a <- rd(file.path(out_dir, lc_tile_dir(), sub_a, sprintf("%s_%s.tif", what, tid)))
      b <- rd(file.path(out_dir, lc_tile_dir(), sub_b, sprintf("%s_%s.tif", what, tid)))
      if(is.null(a) || is.null(b) || length(a) != length(b)) return(invisible(NULL))
      acc[off + 0:(NB - 1L)]      <<- acc[off + 0:(NB - 1L)]      + tabulate(a + 1L, NB)
      acc[off + NB + 0:(NB - 1L)] <<- acc[off + NB + 0:(NB - 1L)] + tabulate(b + 1L, NB)
      m  <- a != b
      od <- which(m & !(a %in% fr))
      acc[moff]        <<- acc[moff]     + sum(m)
      acc[moff + 1L]   <<- acc[moff + 1L] + length(od)
      acc[MOV + 4L]    <<- acc[MOV + 4L]  + length(a)
      if(length(od))
        note <<- c(note, sprintf("%s %s: cell %d was %d, now %d", tid, what,
                                 od[seq_len(min(5L, length(od)))],
                                 a[od[seq_len(min(5L, length(od)))]],
                                 b[od[seq_len(min(5L, length(od)))]]))
      ill <- which(!(b %in% 0:19))
      if(length(ill))
        note <<- c(note, sprintf("%s %s: %d cell(s) outside 0:19, values %s", tid, what,
                                 length(ill),
                                 paste(sort(unique(b[ill])), collapse = " ")))
      invisible(NULL)
    }
    note <- character(0)
    pair("ground_filled", "ground_height", "ground", OFF[["ground"]], MOV,      from$ground)
    pair("canopy",        "canopy_height", "canopy", OFF[["canopy"]], MOV + 2L, from$canopy)
    terra::tmpFiles(remove = TRUE)
    gc(FALSE)
    #"3 cells of 82.8 billion failed" is not an actionable answer, so a tile that
    #misbehaves names itself and the cell. Without this the only way back to the
    #offending cell is another full pass.
    list(acc = acc, note = note)
  }

  t0 <- Sys.time()
  if(workers > 1){
    stopifnot(file.exists(src))
    cl <- parallel::makeCluster(workers)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterCall(cl, function(p) suppressMessages(source(p)), normalizePath(src))
    parallel::clusterCall(cl, function(frac){
      terra::terraOptions(memfrac = frac, progress = 0); NULL
    }, max(0.5 / workers, 0.03))
    wenv <- new.env(parent = globalenv())
    wenv$tiles <- tiles; wenv$out_dir <- out_dir; wenv$from <- from
    wenv$NB <- NB; wenv$OFF <- OFF; wenv$MOV <- MOV
    environment(one) <- wenv
    res <- parallel::parLapplyLB(cl, seq_len(nrow(tiles)), one)
  }else{
    res <- lapply(seq_len(nrow(tiles)), one)
  }
  notes <- unlist(lapply(res, `[[`, "note"))
  acc   <- Reduce(`+`, lapply(res, `[[`, "acc"))
  cat(sprintf("\n%d tiles, %.0f cells per layer, read in %.1f min\n",
              nrow(tiles), acc[MOV + 4L] / 2,
              as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  g   <- function(x, k) sum(x[as.character(k)], na.rm = TRUE)
  pal <- lc_palette()
  for(w in c("ground", "canopy")){
    off <- OFF[[w]]
    a <- stats::setNames(acc[off + 0:(NB - 1L)],      as.character(0:(NB - 1L)))
    b <- stats::setNames(acc[off + NB + 0:(NB - 1L)], as.character(0:(NB - 1L)))
    tot <- sum(b)
    cat("\n=== ", w, " ===\n", sep = "")
    for(k in which(a > 0 | b > 0) - 1L){
      i <- if(is.null(pal)) NA_integer_ else match(k, pal$id)
      cat(sprintf("  %2d  %-22s %-6s %15.0f  %6.2f %%   was %15.0f\n", k,
                  if(!is.na(i)) pal$name[i] else "(no class)",
                  if(is.na(i) || is.na(pal$height[i])) ""
                  else sprintf("%gm", pal$height[i]),
                  g(b, k), 100 * g(b, k) / tot, g(a, k)))
    }
    bad <- setdiff(as.integer(names(b))[b > 0 | a > 0], legal)
    cat(if(length(bad))
          sprintf("  ILLEGAL IDS: %s  (%.0f cells now, %.0f before)\n",
                  paste(bad, collapse = ", "), g(b, bad), g(a, bad))
        else "  every id is a PAINT_CATEGORIES class\n")

    #a class the ramp never writes must still have exactly the count it had, and
    #the ramp steps together must account for exactly the flat class they replaced
    for(k in setdiff(0:(NB - 1L), c(ramp[[w]], from[[w]])))
      if(g(a, k) != g(b, k))
        cat(sprintf("  CHANGED AND SHOULD NOT HAVE: id %d, %.0f -> %.0f\n",
                    k, g(a, k), g(b, k)))
    cat(sprintf("  ramp steps now %.0f vs flat classes before %.0f -> %s\n",
                g(b, ramp[[w]]), g(a, from[[w]]),
                if(g(b, ramp[[w]]) == g(a, from[[w]])) "reconciles"
                else "DOES NOT RECONCILE"))

    mo <- if(w == "ground") acc[MOV]      else acc[MOV + 2L]
    od <- if(w == "ground") acc[MOV + 1L] else acc[MOV + 3L]
    cat(sprintf("  cells changed %.0f (%.2f %%), of which from a class the ramp does not replace: %.0f -> %s\n",
                mo, 100 * mo / tot, od,
                if(od == 0) "exact" else "FAILED"))
  }
  if(length(notes)){
    cat("\n=== tiles that need looking at ===\n")
    cat(paste0("  ", notes, collapse = "\n"), "\n")
  }
  invisible(list(counts = acc, notes = notes))
}


# --------------------------------------------------------------- driver -----
# Guarded so package load never touches a 3.1 GB TIFF or a 10.8 GB GeoPackage.

if(FALSE){

  #once, before anything else: ~500k OSM polygons -> one indexed LV95 file
  lc_prepare_osm()

  #and once for the satellite backfill: three 3-degree tiles -> one LV95 10 m file
  lc_prepare_worldcover()

  #and once for the building heights - two artefacts, one per era of the 3D
  #product; see lc_prepare_buildings3d(). The archive is 13.5 GB; download it to
  #disk and EXTRACT it rather than letting GDAL stream it, for the reason
  #LC_B3D_GDB gives.
  options(timeout = 6 * 3600)
  utils::download.file(LC_B3D_SRC, LC_B3D_ZIP, mode = "wb")
  #extract before reading - see LC_B3D_GDB
  system2("tar", c("-xf", shQuote(LC_B3D_ZIP), "-C", shQuote(dirname(LC_B3D_GDB))))
  lc_prepare_buildings3d()

  #the ramps this file writes must still be the ramps the palette offers
  lc_check_steps()

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

  #or, on tiles that are already built, just add step 9 and re-merge. This is
  #minutes rather than the ~23 h a rebuild costs, and keeps the pre-fill raster
  #as ground_CH_1m_prefill.tif so the inferred cells stay identifiable.
  fill_ground_canopy_CH(workers = 6)

  #REAL HEIGHTS, on tiles that are already built. Last link in the chain -
  #build -> fill -> height - so a rerun of either of the first two has to be
  #followed by a rerun of this. A fresh build needs none of it: lc_build_tile()
  #writes the heights itself now, and this exists only to avoid paying ~23 h to
  #reach them on tiles that are otherwise correct.
  #
  #Run the equivalence test below FIRST. It is the only check that the retrofit
  #and a rebuild agree, and everything else here assumes they do.
  #6 workers, not more. 10 died with std::bad_alloc on a 32 GB machine: this
  #pass holds several 18.4 M-cell rasters at once per tile - the two source
  #tiles, the disaggregated VHM, the reclassified canopy, the building class
  #raster and the ifel intermediates - and terraOptions(memfrac) is divided by
  #the worker count, so raising the count lowers each worker's budget and
  #raises the total at the same time.
  height_ground_canopy_CH(workers = 6)

  #the 5 m copies are stale the moment the 1 m ones change
  downsample_ground_canopy_CH()

  #A CROSSWALK REVISION, without rebuilding the country. Changing LC_AREALE or
  #LC_AREALE_WEAK can only move tiles that intersect the polygons concerned -
  #~700 of 4500 for the container-areal fix - so rebuild those and re-merge.
  #
  #merge = FALSE on both subset calls is not optional. Each function builds its
  #merge list from whatever `tiles` it was handed, so a subset call with
  #merge = TRUE would overwrite the national ground_CH_1m.tif with a raster of
  #those 700 tiles and report success. The third call passes no tiles: it skips
  #the ~4500 filled tiles that already read back, and merges all of them.
  #Measured on 2026-09-21 for the container-areal revision: 564 tiles (12.5 %),
  #95.8 min to rebuild them, 2.3 min for step 9, 19.2 min to merge, 36.3 min to
  #downsample - 153.6 min in all, against the ~23 h a national build costs.
  #
  #workers = 6 is what that run used and it left the machine idle: 16 cores, and
  #~2 GB resident across every process. terraOptions(memfrac) is divided by the
  #worker count inside build_ground_canopy_CH(), so each worker was capped at
  #8 % of RAM and spilled to scratch rather than using memory that was free.
  #On a machine with cores to spare, 12 is the better number.
  aff <- lc_affected_tiles()
  build_ground_canopy_CH(tiles = aff, overwrite = TRUE, merge = FALSE, workers = 6)
  fill_ground_canopy_CH (tiles = aff, overwrite = TRUE, merge = FALSE, workers = 6)
  fill_ground_canopy_CH (merge = TRUE, workers = 6)
  downsample_ground_canopy_CH()

  #and the evidence, before and after. The treated rows should collapse toward
  #the control group (Gleisareal, Abwasserreinigungsareal, Golfplatzareal),
  #which must not move at all - if a control moves, the weak mask is leaking.
  print(qa_areal_vs_worldcover())

  #coverage: the 0 share should land near the ~36 % of Switzerland that BFS
  #Arealstatistik calls agricultural, plus settlement open ground
  ch <- sf::st_transform(sf::st_set_crs(
    sf::st_read(vftData("maps/countryBorders/swissBorder_final.gpkg"), quiet = TRUE),
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

  #THE EQUIVALENCE TEST. Build one tile from scratch with the current code and
  #retrofit the same tile from its old files; the two must agree cell for cell,
  #in both rasters. If they can disagree then the retrofit is a second
  #implementation of the build rather than a shortcut through it, and every
  #measurement taken on the national raster afterwards is measuring the wrong
  #thing. This is also the cheapest place to TIME the two, which is what decides
  #whether to retrofit 4500 tiles or rebuild them.
  #how many buildings the 3D survey actually reaches, before paying for a
  #national pass that can only improve the ones it does
  qa_building_heights()

  #a REAL grid tile, and one with both a town and a forest in it: 51_39 is
  #central Zurich with the Zurichberg on its shoulder
  qa_height_retrofit_vs_rebuild(lc_tile_grid()[lc_tile_grid()$tile_id == "51_39", ])

  #what actually changed, nationally, against the rasters kept aside by the
  #merge. Trees should spread across 10/11/7/12/13 and buildings across
  #16/8/19/17/18; the ground must differ ONLY on cells that were class 8.
  qa_height_change()
}
