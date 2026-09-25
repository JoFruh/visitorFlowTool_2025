#' Build the Switzerland-wide layer of underground structures.
#'
#' One indexed GeoPackage, `underground_CH_2056.gpkg`, written beside the land
#' cover rasters (paintLandcoverDir()). The Hitzeminderung context reads it per
#' study area (undergroundSeed() in R/underground_helpers.R) to WARN when a tree
#' or a block is painted over something underground - an underground building,
#' a tunnel, an underpass, a culverted stream. It never alters the paint.
#'
#' WHY A VECTOR FILE AND NOT A THIRD NATIONAL RASTER. Underground objects are
#' sparse and a study area is small, so rasterising per area at runtime costs
#' milliseconds, where a national 1 m raster would be another multi-hour tile
#' run for a layer that is almost entirely empty.
#'
#' SOURCES, measured 2026-09-24 before choosing:
#'
#'   AV (amtliche Vermessung), layer Einzelobjekte (SOSF) on geodienste.ch.
#'     The authoritative source for underground BUILDINGS: 352 of them in one
#'     km2 of central Zurich, where swissTLM3D has 0; 32 in the Sion window
#'     against TLM3D's 8. Free in 21 cantons + FL. JU LU NE NW OW VD need a
#'     release ("Freigabe erforderlich"), and the open WFS returns NOTHING there
#'     - not an error, an empty answer. NW and OW released it to us, JU LU NE VD
#'     did not and are never requested (UG_AV_CLOSED). See UG_AV_GATED and the
#'     `coverage` layer, which is what stops that emptiness reading as "no
#'     constraint".
#'   swissTLM3D (already local). Road and rail tunnels and underpasses, and
#'     culverted streams, as 3D LINES. The Z of such a line is the road or the
#'     stream bed, so terrain minus Z is the DEPTH of the structure's floor -
#'     the one depth that exists nationally. Verified in Zurich: open streams
#'     come out at 0 +- 1 m (the control), culverts at 0-6 m, the Milchbuck
#'     cut-and-cover tunnel at 0-7 m.
#'   OpenStreetMap, via Overpass, ONLY for JU LU NE VD and any released canton
#'     whose AV did not arrive. Underground car parks, buildings tagged
#'     underground or on a negative layer, covered reservoirs, and culverted
#'     streams. Far thinner than AV - VD has ~150 such polygons in the
#'     whole canton, where AV has 352 in one km2 of Zurich - so the canton keeps
#'     its "partly recorded" notice; it is status "osm", not "av".
#'   GWR public export (housing-stat / MADD). The AV object often carries the
#'     EGID of the building it belongs to. Only GKLAS 1242 (Garagengebaeude)
#'     says "garage"; most links point at the building ABOVE (in Zurich 140 of
#'     245 were offices, 43 apartment blocks), which is still worth saying.
#'
#' What no source has: the depth of an underground BUILDING, or the soil cover
#' over any structure. AV and TLM3D are 2D there, swissBUILDINGS3D is
#' above-ground only, and the GWR counts a basement storey only if it is heated
#' or lived in. The warning says so rather than guessing.
#'
#' OUTPUT, layer `underground` - one row per polygon, several rows per element:
#'   ug_id   element id; an element is what the user ignores as a whole
#'   kind    garage building road_tunnel rail_tunnel tunnel underpass culvert
#'           reservoir
#'   name    TLM3D name (a tunnel's, a stream's) where there is one
#'   source  AV / TLM3D / OSM
#'   egid, gklas   GWR link, AV rows only
#'   depth_m floor depth under the terrain; TLM3D line pieces only, NA elsewhere
#'   rank    burn order at runtime: 1 line pieces, 2 TLM3D buildings, 3 AV/OSM
#' and layer `coverage`: one polygon per canton, status "av", "osm" (AV gated,
#' OSM in its place) or "tlm_only" (neither).
#'
#' Same rules as generate_ground_canopy_CH.r: function definitions only, the
#' driver sits inside `if (FALSE)` at the bottom.

if(!exists("lc_tile_grid", mode = "function"))
  source(file.path("data-raw", "generate_ground_canopy_CH.r"))


# ---------------------------------------------------------------- config ----

UG_DIR      <- "C:/Users/frueh/Documents/Local Data/underground"
UG_AV_TILES <- file.path(UG_DIR, "av_tiles")
UG_GWR_DIR  <- file.path(UG_DIR, "gwr")
UG_OUT      <- file.path(LC_OUT_DIR, "underground_CH_2056.gpkg")

UG_DTM <- paste0("C:/Users/frueh/Documents/Local Data/Environmental data (large files)/",
                 "Topographical Map/swissALTI3D_5M_CHLV95_LN02_2020.tif")

UG_BND_URL <- paste0("https://data.geo.admin.ch/ch.swisstopo.swissboundaries3d/",
                     "swissboundaries3d_2026-01/swissboundaries3d_2026-01_2056_5728.gpkg.zip")
UG_BND     <- file.path(UG_DIR, "swissboundaries3d_2026-01_2056_5728.gpkg")

UG_WFS      <- "https://geodienste.ch/db/av_0/deu"
UG_WFS_PAGE <- 5000
#geodienste.ch enforces fair use: an unthrottled run answered 429 "Too Many
#Requests" after ~2000 requests in 2.7 min. A pause between requests, and a long
#back-off when it still happens, keeps the national run polite and unattended.
UG_WFS_PAUSE   <- 1
UG_WFS_BACKOFF <- c(60, 120, 300, 600)
UG_GWR_URL  <- "https://public.madd.bfs.admin.ch/%s.zip"

#AV Einzelobjekt types kept, and the kind each starts as. A tunnel or culvert
#polygon is refined from the TLM3D line it covers - see ug_link_av().
UG_AV_ART <- c(unterirdisches_Gebaeude             = "building",
               Tunnel_Unterfuehrung_Galerie        = "tunnel",
               eingedoltes_oeffentliches_Gewaesser = "culvert",
               Reservoir                           = "reservoir")

#cantons whose AV needs a release; the open WFS returns nothing for them
UG_AV_GATED <- c("JU", "LU", "NE", "NW", "OW", "VD")
#...and those of them that have NOT released it to us (2026-09-25): their AV is
#never requested, neither openly nor with the credentials. OSM stands in.
UG_AV_CLOSED <- c("JU", "LU", "NE", "VD")

#swissBOUNDARIES3D kantonsnummer -> the abbreviation AV's `Kanton` carries
UG_KANTON <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO",
               "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD",
               "VS", "NE", "GE", "JU")

#Galerie is left out on purpose: an avalanche gallery is a roof over a road on a
#mountainside, not something under a plantable surface. `stufe < 0` alone is not
#enough either - on a road it means "passes under another object", i.e. under a
#bridge, which is open air.
UG_TLM_TUNNEL    <- "Tunnel"
UG_TLM_UNDERPASS <- c("Unterfuehrung", "Unterfuehrung mit Treppe")

#watercourses whose culverted reaches count. Druckstollen and Druckleitung are
#hydropower pressure tunnels and penstocks, not culverts: they put the national
#culvert depth at a 90th percentile of 99 m (Druckstollen alone: median 154 m).
#Bisse/Suone are the Valais irrigation channels - in Sion, not in the mountains.
UG_TLM_STREAMS <- c("Fliessgewaesser", "Trockenrinne", "Bisse Suone")

#Pieces of a line, each with one depth. 20 m keeps a cut-and-cover ramp (0 -> 7 m
#over ~100 m) resolved to a couple of metres.
UG_PIECE_M <- 20
#A structure is wider than its carriageway: one wall each side
UG_WALL_M <- 1
#a culvert's pipe plus its trench; TLM3D carries no width for it
UG_CULVERT_WIDTH <- 3

UG_GARAGE_GKLAS <- 1242L

#OSM fallback for gated cantons. overpass-api.de answers 406 to a request with
#no User-Agent, and an EMPTY body when it is busy - both retried like AV's 429.
UG_OSM_DIR      <- file.path(UG_DIR, "osm")
UG_OSM_URL      <- c("https://overpass-api.de/api/interpreter",
                     "https://overpass.private.coffee/api/interpreter")
UG_OSM_UA       <- "visitorFlowTool data-raw/generate_underground_CH.r"
UG_OSM_BACKOFF  <- c(30, 60, 120, 300)
#AV surveys only PUBLIC watercourses; ditch and drain are not, and TLM3D has no
#counterpart for them either
UG_OSM_WATERWAY <- c("river", "stream", "canal")
#an OSM culvert running within this distance of a TLM3D culvert for most of its
#length is the same one, already there with a measured depth
UG_OSM_DUP_M    <- 10


# ------------------------------------------------------------------ fetch ---

#' Canton polygons from swissBOUNDARIES3D, downloaded once.
ug_cantons <- function(path = UG_BND, url = UG_BND_URL){
  if(!file.exists(path)){
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    zip <- paste0(path, ".zip")
    utils::download.file(url, zip, mode = "wb", quiet = TRUE)
    utils::unzip(zip, exdir = dirname(path))
    unlink(zip)
    #the archive's member name is not guaranteed to be the one asked for
    if(!file.exists(path)){
      got <- list.files(dirname(path), pattern = "[.]gpkg$", full.names = TRUE)
      got <- got[grepl("boundaries", got, ignore.case = TRUE)]
      stopifnot(length(got) >= 1)
      file.rename(got[1], path)
    }
  }
  k <- sf::st_read(path, layer = "tlm_kantonsgebiet", quiet = TRUE)
  k <- sf::st_zm(k, drop = TRUE, what = "ZM")
  nr <- suppressWarnings(as.integer(k$kantonsnummer))
  k$kanton <- UG_KANTON[nr]
  k <- k[!is.na(k$kanton), ]
  #a canton is several polygons (exclaves, lake parts); one row each
  g <- lapply(split(sf::st_geometry(k), k$kanton), sf::st_union)
  sf::st_sf(kanton = names(g), geom = do.call(c, g))
}

#' The land cover tiles that touch Switzerland - the only ones worth a request.
#' A tile lying only in cantons in `skip` is left out; one that also reaches
#' an open canton is kept for that canton's part.
ug_tiles_ch <- function(cantons = ug_cantons(), skip = UG_AV_CLOSED){
  cantons <- cantons[!cantons$kanton %in% skip, ]
  g  <- lc_tile_grid()
  bb <- lapply(seq_len(nrow(g)), function(i) sf::st_as_sfc(sf::st_bbox(
    c(xmin = g$xmin[i], ymin = g$ymin[i], xmax = g$xmax[i], ymax = g$ymax[i]), crs = 2056)))
  bb <- do.call(c, bb)
  ch <- sf::st_union(cantons)
  g[lengths(sf::st_intersects(bb, ch)) > 0, ]
}

#' GetFeature URL for one page of one tile. Filtered on the server - bbox AND
#' the four kept types - and returned as GeoJSON: 437 KB for the densest km2 of
#' Zurich against 1.7 MB of unfiltered GML.
ug_av_url <- function(tile, start = 0, page = UG_WFS_PAGE, wfs = UG_WFS){
  ors <- paste0(sprintf(paste0("<fes:PropertyIsEqualTo><fes:ValueReference>Art</fes:ValueReference>",
                               "<fes:Literal>%s</fes:Literal></fes:PropertyIsEqualTo>"),
                        names(UG_AV_ART)), collapse = "")
  filt <- paste0(
    '<fes:Filter xmlns:fes="http://www.opengis.net/fes/2.0"><fes:And><fes:BBOX>',
    '<fes:ValueReference>msGeometry</fes:ValueReference>',
    '<gml:Envelope xmlns:gml="http://www.opengis.net/gml/3.2" srsName="urn:ogc:def:crs:EPSG::2056">',
    sprintf('<gml:lowerCorner>%.0f %.0f</gml:lowerCorner><gml:upperCorner>%.0f %.0f</gml:upperCorner>',
            tile$xmin, tile$ymin, tile$xmax, tile$ymax),
    '</gml:Envelope></fes:BBOX><fes:Or>', ors, '</fes:Or></fes:And></fes:Filter>')
  paste0(wfs, "?SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAMES=ms:SOSF",
         "&OUTPUTFORMAT=", utils::URLencode("application/json; subtype=geojson", reserved = TRUE),
         "&COUNT=", page, "&STARTINDEX=", start,
         "&FILTER=", utils::URLencode(filt, reserved = TRUE))
}

#' Fetch one tile into `dir` as page files, then a `.done` marker.
#'
#' The marker, not the page files, is what makes a tile finished: a run killed
#' between two pages leaves page 1 on disk and would otherwise be taken for a
#' tile with fewer features than it has. Same lesson as lc_tile_ok().
#'
#' `auth` is c(user, password) for the cantons released to you; geodienste.ch
#' answers the same URL with more data once the account is entitled to it.
ug_fetch_av_tile <- function(tile, dir = UG_AV_TILES, auth = ug_auth(),
                             page = UG_WFS_PAGE, overwrite = FALSE){
  done <- file.path(dir, paste0(tile$tile_id, ".done"))
  if(file.exists(done) && !overwrite) return(invisible(TRUE))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  #an overwrite that dies half way must not leave the old marker standing
  unlink(done)
  unlink(list.files(dir, pattern = paste0("^", tile$tile_id, "_p"), full.names = TRUE))

  h <- curl::new_handle(timeout = 300)
  if(!is.null(auth)) curl::handle_setopt(h, userpwd = paste(auth, collapse = ":"),
                                         httpauth = 1L)
  start <- 0; k <- 1
  repeat{
    f <- file.path(dir, sprintf("%s_p%d.geojson", tile$tile_id, k))
    res <- NULL
    for(wait in c(UG_WFS_BACKOFF, NA)){
      Sys.sleep(UG_WFS_PAUSE)
      res <- try(curl::curl_fetch_disk(ug_av_url(tile, start, page), f, handle = h),
                 silent = TRUE)
      if(!inherits(res, "try-error") && res$status_code == 200) break
      #refused credentials do not get better by waiting
      if(!inherits(res, "try-error") && res$status_code == 401)
        stop("geodienste.ch refused the credentials (401) - see ug_auth_ok()")
      if(is.na(wait)) break
      message(sprintf("AV: tile %s answered %s, waiting %d s", tile$tile_id,
                      if(inherits(res, "try-error")) "an error" else res$status_code, wait))
      Sys.sleep(wait)
    }
    if(inherits(res, "try-error") || res$status_code != 200)
      stop("AV fetch failed for tile ", tile$tile_id, " page ", k)
    js <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    n  <- length(js$features)
    if(n == 0){ unlink(f); break }
    if(n < page) break
    start <- start + page; k <- k + 1
  }
  file.create(done)
  invisible(TRUE)
}

#' Credentials for released cantons, from the environment. NULL when unset,
#' which is the open service. Set them in the USER .Renviron (~/.Renviron,
#' C:/Users/frueh/.Renviron here), never in this file:
#'   GEODIENSTE_USER=...
#'   GEODIENSTE_PASS=...
#' NOT in the project's .Renviron: that one is tracked by git. And R reads only
#' one of the two at startup - the project's, when R starts in the project - so
#' the user file is read here explicitly when the variables are still unset.
#' The geodienste.ch website login answered 401 on the WFS (2026-09-25) - check
#' ug_auth_ok() before a long run.
#' Where "~" is depends on who started R: with HOME unset (VS Code, RStudio) it
#' is the OneDrive "Dokumente" folder, with HOME set (Git Bash) the profile
#' folder. Both are tried, the first file found is read.
ug_auth <- function(user_env = c(Sys.getenv("R_ENVIRON_USER"),
                                 path.expand("~/.Renviron"),
                                 file.path(Sys.getenv("USERPROFILE"), ".Renviron"))){
  user_env <- user_env[nzchar(user_env) & file.exists(user_env)]
  if(!nzchar(Sys.getenv("GEODIENSTE_USER")) && length(user_env))
    readRenviron(user_env[1])
  u <- Sys.getenv("GEODIENSTE_USER"); p <- Sys.getenv("GEODIENSTE_PASS")
  if(nzchar(u) && nzchar(p)) c(u, p) else NULL
}

#' One request with the credentials over central Stans (NW), a gated canton
#' released to us: the number of AV objects returned. 0 means the account is
#' accepted but NW is not (yet) released to it; an error means the credentials
#' are refused.
ug_auth_ok <- function(auth = ug_auth()){
  if(is.null(auth)) stop("GEODIENSTE_USER / GEODIENSTE_PASS are not set")
  tile <- list(xmin = 2670000, ymin = 1200500, xmax = 2671500, ymax = 1202000)
  h <- curl::new_handle(timeout = 120)
  curl::handle_setopt(h, userpwd = paste(auth, collapse = ":"), httpauth = 1L)
  r <- curl::curl_fetch_memory(ug_av_url(tile), handle = h)
  if(r$status_code != 200) stop("geodienste.ch answered HTTP ", r$status_code)
  length(jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE)$features)
}

#' Fetch every tile that touches Switzerland. Resumable; ~2200 requests.
ug_fetch_av <- function(tiles = ug_tiles_ch(), dir = UG_AV_TILES, auth = ug_auth(),
                        overwrite = FALSE){
  t0 <- Sys.time()
  for(i in seq_len(nrow(tiles))){
    ug_fetch_av_tile(tiles[i, ], dir = dir, auth = auth, overwrite = overwrite)
    if(i %% 50 == 0)
      message(sprintf("AV: %d / %d tiles, %.1f min", i, nrow(tiles),
                      as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  }
  invisible(dir)
}

#' Every fetched AV object, deduplicated.
#'
#' A WFS bbox returns whole features, so an object straddling a tile edge comes
#' back once per tile. Identical objects have identical bboxes and areas, which
#' is cheap to compare; a geometry-equality test over ~10^5 polygons is not.
ug_read_av <- function(dir = UG_AV_TILES){
  fs <- list.files(dir, pattern = "[.]geojson$", full.names = TRUE)
  if(!length(fs)) return(NULL)
  xs <- lapply(fs, function(f){
    x <- try(sf::st_read(f, quiet = TRUE), silent = TRUE)
    if(inherits(x, "try-error") || nrow(x) == 0) return(NULL)
    x <- sf::st_set_crs(x, 2056)
    #a tile whose objects all lack an attribute comes back without the column
    #at all, and rbind() refuses mismatched frames
    cols <- c("Art", "Gueltigkeit", "GWR_EGID", "Kanton", "BFSNr")
    for(cl in setdiff(cols, names(x))) x[[cl]] <- NA_character_
    x$GWR_EGID <- as.character(x$GWR_EGID)
    x$BFSNr    <- as.character(x$BFSNr)
    sf::st_set_geometry(x[, cols], sf::st_geometry(x))
  })
  xs <- xs[!vapply(xs, is.null, logical(1))]
  if(!length(xs)) return(NULL)
  #NOT do.call(rbind, xs): rbind.sf over ~1300 frames ran for over 15 minutes.
  #Attributes and geometries are stacked separately, which takes seconds.
  att <- data.table::rbindlist(lapply(xs, sf::st_drop_geometry))
  geo <- sf::st_sfc(unlist(lapply(xs, sf::st_geometry), recursive = FALSE), crs = 2056)
  x <- sf::st_sf(as.data.frame(att), geometry = geo)
  if(nrow(x) == 0) return(NULL)
  #"projektiert" is a planned object, not one in the ground
  x <- x[is.na(x$Gueltigkeit) | x$Gueltigkeit == "gueltig", ]
  x <- sf::st_make_valid(x)
  x <- x[sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"), ]
  bb  <- t(vapply(sf::st_geometry(x), function(g) as.numeric(sf::st_bbox(g)), numeric(4)))
  key <- paste(x$Art, round(bb[, 1], 2), round(bb[, 2], 2), round(bb[, 3], 2),
               round(bb[, 4], 2), round(as.numeric(sf::st_area(x)), 1))
  x <- x[!duplicated(key), ]
  x$kind <- unname(UG_AV_ART[x$Art])
  eg <- suppressWarnings(as.integer(trimws(x$GWR_EGID)))
  x$egid <- eg
  x
}


# ----------------------------------------------------------------- TLM3D ---

#' One national attribute query. The whole layer, not a tile: these are
#' attribute filters over the entire country, which is a single table scan.
ug_tlm_query <- function(layer, where, gpkg = LC_TLM3D){
  sf::st_read(gpkg, query = sprintf("SELECT * FROM \"%s\" WHERE %s", layer, where),
              quiet = TRUE)
}

#' Every underground line in swissTLM3D, still 3D, with its kind and width.
ug_tlm_lines <- function(gpkg = LC_TLM3D){
  kb <- paste0("kunstbaute IN (", paste0("'", c(UG_TLM_TUNNEL, UG_TLM_UNDERPASS), "'",
                                          collapse = ","), ")")
  road <- ug_tlm_query("tlm_strassen_strasse", kb, gpkg)
  rail <- ug_tlm_query("tlm_oev_eisenbahn", kb, gpkg)
  strm <- ug_tlm_query("tlm_gewaesser_fliessgewaesser",
                       sprintf("CAST(stufe AS INTEGER) < 0 AND objektart IN (%s)",
                               paste0("'", UG_TLM_STREAMS, "'", collapse = ",")), gpkg)

  road$kind  <- ifelse(road$kunstbaute %in% UG_TLM_UNDERPASS, "underpass", "road_tunnel")
  road$width <- unname(LC_ROAD_WIDTH[road$objektart]) + 2 * UG_WALL_M
  rail$kind  <- ifelse(rail$kunstbaute %in% UG_TLM_UNDERPASS, "underpass", "rail_tunnel")
  rail$width <- unname(LC_RAIL_WIDTH[rail$objektart]) + 2 * UG_WALL_M
  strm$kind  <- "culvert"
  strm$width <- UG_CULVERT_WIDTH

  keep <- c("uuid", "kind", "name", "width")
  out <- rbind(road[, keep], rail[, keep], strm[, keep])
  #objektarten with no width (Verbindung, Markierte Spur, ...) are network
  #connectors, not structures - the same rule lc_buffer_lines() applies
  out[!is.na(out$width), ]
}

#' TLM3D's own underground building footprints, flat.
ug_tlm_buildings <- function(gpkg = LC_TLM3D){
  b <- ug_tlm_query("tlm_bauten_gebaeude_footprint",
                    sprintf("objektart IN (%s)",
                            paste0("'", LC_BUILDING_DROP, "'", collapse = ",")), gpkg)
  b <- sf::st_zm(b, drop = TRUE, what = "ZM")
  b$kind <- "building"
  b[, c("uuid", "kind")]
}

#' Cut 3D lines into pieces of at most `step` metres, each with a floor depth.
#'
#' Pure vector arithmetic, no per-piece R loop: a TLM3D segment longer than
#' `step` gets evenly spaced vertices inserted (X, Y and Z interpolated), and
#' every consecutive vertex pair is then one straight piece. The original
#' vertices are all kept, so a bend is never cut.
#'
#' Depth = terrain at the piece's midpoint minus its mean Z, clamped at 0 (the
#' +-1 m noise of a 5 m terrain model on a line lying on the surface).
#'
#' Each piece becomes a rectangle of the line's width, extended at both ends by
#' half the width so consecutive pieces overlap at a bend instead of leaving a
#' wedge-shaped gap.
ug_pieces <- function(lines, dtm = terra::rast(UG_DTM), step = UG_PIECE_M){
  crd <- sf::st_coordinates(lines)
  if(!"Z" %in% colnames(crd)) stop("ug_pieces() needs 3D lines")
  #one id per part: feature (last L column) and part (L1)
  lcols <- grep("^L", colnames(crd), value = TRUE)
  feat  <- crd[, lcols[length(lcols)]]
  part  <- as.integer(factor(do.call(paste, as.data.frame(crd[, lcols, drop = FALSE]))))

  n  <- nrow(crd)
  same <- part[-1] == part[-n]
  x0 <- crd[-n, "X"]; y0 <- crd[-n, "Y"]; z0 <- crd[-n, "Z"]
  x1 <- crd[-1, "X"]; y1 <- crd[-1, "Y"]; z1 <- crd[-1, "Z"]
  ln <- sqrt((x1 - x0)^2 + (y1 - y0)^2)
  ok <- same & ln > 0
  x0 <- x0[ok]; y0 <- y0[ok]; z0 <- z0[ok]; x1 <- x1[ok]; y1 <- y1[ok]; z1 <- z1[ok]
  ln <- ln[ok]; fe <- feat[-n][ok]

  #split each segment into k equal sub-segments
  k  <- pmax(1L, as.integer(ceiling(ln / step)))
  si <- rep(seq_along(k), k)
  j  <- sequence(k) - 1L
  kk <- k[si]
  a  <- j / kk; b <- (j + 1L) / kk
  px0 <- x0[si] + a * (x1[si] - x0[si]); py0 <- y0[si] + a * (y1[si] - y0[si])
  px1 <- x0[si] + b * (x1[si] - x0[si]); py1 <- y0[si] + b * (y1[si] - y0[si])
  pz  <- z0[si] + (a + b) / 2 * (z1[si] - z0[si])

  mx <- (px0 + px1) / 2; my <- (py0 + py1) / 2
  #sorted so the terrain is read in raster order rather than at random
  o  <- order(-my, mx)
  tz <- numeric(length(mx))
  tz[o] <- terra::extract(dtm, cbind(mx[o], my[o]))[[1]]
  depth <- pmax(0, tz - pz)

  #rectangle around each piece
  w  <- lines$width[fe[si]] / 2
  ux <- (px1 - px0); uy <- (py1 - py0); ul <- sqrt(ux^2 + uy^2)
  ux <- ux / ul; uy <- uy / ul
  ax <- px0 - ux * w; ay <- py0 - uy * w
  bx <- px1 + ux * w; by <- py1 + uy * w
  nx <- -uy * w; ny <- ux * w
  polys <- lapply(seq_along(ax), function(i){
    sf::st_polygon(list(matrix(c(ax[i] + nx[i], ay[i] + ny[i],
                                 bx[i] + nx[i], by[i] + ny[i],
                                 bx[i] - nx[i], by[i] - ny[i],
                                 ax[i] - nx[i], ay[i] - ny[i],
                                 ax[i] + nx[i], ay[i] + ny[i]), ncol = 2, byrow = TRUE)))
  })
  sf::st_sf(line = fe[si], depth_m = round(depth, 1),
            geometry = sf::st_sfc(polys, crs = 2056))
}


# ------------------------------------------------------------------- GWR ---

#' GKLAS for a set of EGIDs, from the public GWR export of the cantons named.
#' One zip per canton, cached; only EGID and GKLAS are read.
ug_gwr_gklas <- function(egid, cantons, dir = UG_GWR_DIR){
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  egid <- unique(stats::na.omit(egid))
  out  <- list()
  for(kt in unique(stats::na.omit(cantons))){
    csv <- file.path(dir, paste0(tolower(kt), "_gebaeude.csv"))
    if(!file.exists(csv)){
      zip <- file.path(dir, paste0(tolower(kt), ".zip"))
      ok <- try(utils::download.file(sprintf(UG_GWR_URL, tolower(kt)), zip,
                                     mode = "wb", quiet = TRUE), silent = TRUE)
      if(inherits(ok, "try-error")){ message("GWR: no export for ", kt); next }
      utils::unzip(zip, files = "gebaeude_batiment_edificio.csv", exdir = dir)
      file.rename(file.path(dir, "gebaeude_batiment_edificio.csv"), csv)
      unlink(zip)
    }
    g <- data.table::fread(csv, select = c("EGID", "GKLAS"), sep = "\t",
                           colClasses = c(EGID = "integer", GKLAS = "integer"))
    out[[kt]] <- g[g$EGID %in% egid, ]
  }
  g <- do.call(rbind, out)
  if(is.null(g)) return(stats::setNames(integer(0), character(0)))
  stats::setNames(g$GKLAS, g$EGID)
}


# ------------------------------------------------------------------- OSM ---

#' Overpass QL for one canton, answered as OSM XML: GDAL's OSM driver then
#' assembles the polygons, multipolygon relations included, so no geometry is
#' built by hand here. Ways and relations only - an underground car park mapped
#' as a node is its entrance, which says nothing about the footprint.
ug_osm_query <- function(kanton, waterways = UG_OSM_WATERWAY){
  sprintf(paste0(
    '[out:xml][timeout:900];area["ISO3166-2"="CH-%s"]->.a;(',
    'wr["amenity"="parking"]["parking"="underground"](area.a);',
    'wr["building"]["location"="underground"](area.a);',
    'wr["building"]["building"!="roof"]["layer"~"^-"](area.a);',
    'wr["man_made"="reservoir_covered"](area.a);',
    'way["waterway"~"^(%s)$"]["tunnel"="culvert"](area.a););',
    '(._;>;);out body;'),
    kanton, paste(waterways, collapse = "|"))
}

#' Overpass reports a timeout or a memory limit as a <remark> inside an HTTP
#' 200, and a busy server as an empty body. Only a closed document without a
#' remark is an answer.
ug_osm_complete <- function(f){
  if(!file.exists(f) || file.size(f) == 0) return(FALSE)
  txt <- readChar(f, file.size(f), useBytes = TRUE)
  grepl("</osm>\\s*$", txt) && !grepl("<remark>", txt, fixed = TRUE)
}

#' Each canton's extract, cached as <dir>/<KT>.osm. Downloaded to a .part file
#' and renamed only when complete, so a killed run never leaves a truncated
#' extract that reads as a canton with fewer objects. Returns the cantons that
#' have one; a canton that cannot be fetched is reported and left out, so the
#' build falls back to "tlm_only" there instead of dying.
ug_fetch_osm <- function(kantone, dir = UG_OSM_DIR, overwrite = FALSE){
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for(kt in kantone){
    f <- file.path(dir, paste0(kt, ".osm"))
    if(file.exists(f) && !overwrite) next
    tmp <- paste0(f, ".part")
    ok  <- FALSE
    for(wait in c(UG_OSM_BACKOFF, NA)){
      for(url in UG_OSM_URL){
        h <- curl::new_handle(timeout = 1200, useragent = UG_OSM_UA,
                              postfields = paste0("data=", curl::curl_escape(ug_osm_query(kt))))
        res <- try(curl::curl_fetch_disk(url, tmp, handle = h), silent = TRUE)
        ok  <- !inherits(res, "try-error") && res$status_code == 200 && ug_osm_complete(tmp)
        if(ok) break
      }
      if(ok || is.na(wait)) break
      message(sprintf("OSM: no complete answer for %s, waiting %d s", kt, wait))
      Sys.sleep(wait)
    }
    if(ok) file.rename(tmp, f) else{ unlink(tmp); message("OSM: giving up on ", kt) }
  }
  kantone[file.exists(file.path(dir, paste0(kantone, ".osm")))]
}

#' One canton's OSM objects, in the columns AV rows carry: kind, name.
#'
#' Clipped to the canton: Overpass's area and swissBOUNDARIES3D disagree by
#' metres along a border, and an object across the line is in the neighbour's
#' AV already. A polygon belongs to the canton its interior point lies in; a
#' culvert is cut at the border.
#'
#' Culverts are dropped where TLM3D already has them (`tlm_culverts`, 2D lines):
#' a TLM3D culvert carries a measured depth and the element the tunnel logic
#' links to, the OSM one would only be a second outline of it. Then buffered to
#' UG_CULVERT_WIDTH like TLM3D's.
ug_read_osm <- function(kanton, cantons, tlm_culverts = NULL, dir = UG_OSM_DIR,
                        dist = UG_OSM_DUP_M, share = 0.5){
  f <- file.path(dir, paste0(kanton, ".osm"))
  kt <- sf::st_geometry(cantons[cantons$kanton == kanton, ])
  tag <- function(other, key){
    m <- regmatches(other, regexec(sprintf('"%s"=>"([^"]*)"', key), other))
    vapply(m, function(z) if(length(z) == 2) z[2] else NA_character_, character(1))
  }
  out <- list()

  pol <- sf::st_read(f, layer = "multipolygons", quiet = TRUE)
  if(nrow(pol) > 0){
    pol <- sf::st_transform(pol, 2056)
    ot  <- pol$other_tags
    lyr <- suppressWarnings(as.numeric(tag(ot, "layer")))
    hgt <- suppressWarnings(as.numeric(tag(ot, "height")))
    ugb <- tag(ot, "location") %in% "underground" |
      #a negative layer alone also marks a building set into a slope or under a
      #bridge; one with a height stands above ground (Delemont: a 6000 m2
      #shopping centre, 2 storeys, layer -1)
      (!is.na(lyr) & lyr < 0 & !(!is.na(hgt) & hgt > 0))
    pol$kind <- ifelse(pol$man_made %in% "reservoir_covered", "reservoir",
                ifelse(pol$amenity %in% "parking" & tag(ot, "parking") %in% "underground", "garage",
                ifelse(!is.na(pol$building) & ugb,
                       ifelse(pol$building %in% c("parking", "garage", "garages"), "garage",
                              "building"), NA)))
    #a member way pulled in with a relation can carry tags of its own
    pol <- pol[!is.na(pol$kind), ]
    pol <- sf::st_make_valid(pol)
    pol <- pol[sf::st_geometry_type(pol) %in% c("POLYGON", "MULTIPOLYGON"), ]
    inside <- lengths(sf::st_intersects(sf::st_point_on_surface(sf::st_geometry(pol)), kt)) > 0
    pol <- pol[inside, ]
    if(nrow(pol) > 0)
      out$pol <- sf::st_sf(kind = pol$kind, name = pol$name, geom = sf::st_geometry(pol))
  }

  lin <- sf::st_read(f, layer = "lines", quiet = TRUE)
  if(nrow(lin) > 0){
    lin <- lin[lin$waterway %in% UG_OSM_WATERWAY & tag(lin$other_tags, "tunnel") %in% "culvert", ]
    lin <- sf::st_transform(lin, 2056)
    lin <- suppressWarnings(sf::st_intersection(lin[, c("name")], kt))
    #a culvert touching the border comes back as a point too
    lin <- lin[sf::st_geometry_type(lin) %in% c("LINESTRING", "MULTILINESTRING"), ]
    lin <- suppressWarnings(sf::st_cast(sf::st_cast(lin, "MULTILINESTRING"), "LINESTRING"))
    if(nrow(lin) > 0 && length(tlm_culverts)){
      #share of points every 5 m that lie within `dist` of a TLM3D culvert
      len <- as.numeric(sf::st_length(lin))
      smp <- sf::st_line_sample(lin, n = pmax(2L, as.integer(ceiling(len / 5))), type = "regular")
      pts <- suppressWarnings(sf::st_cast(sf::st_sf(line = seq_along(smp), geom = smp), "POINT"))
      tc  <- tlm_culverts[lengths(sf::st_intersects(tlm_culverts,
                            sf::st_as_sfc(sf::st_bbox(sf::st_buffer(kt, dist))))) > 0]
      if(length(tc)){
        nb <- sf::st_nearest_feature(pts, tc)
        dd <- as.numeric(sf::st_distance(pts, tc[nb], by_element = TRUE))
        dup <- tapply(dd <= dist, pts$line, mean) >= share
        lin <- lin[!dup[as.character(seq_len(nrow(lin)))] %in% TRUE, ]
      }
    }
    if(nrow(lin) > 0)
      out$lin <- sf::st_sf(kind = "culvert", name = lin$name,
                           geom = sf::st_buffer(sf::st_geometry(lin), UG_CULVERT_WIDTH / 2,
                                                endCapStyle = "FLAT"))
  }
  if(!length(out)) return(NULL)
  x <- do.call(rbind, unname(out))
  x$kanton <- kanton
  x
}


# ------------------------------------------------------------------ build ---

#' Give an AV tunnel or culvert polygon the identity of the TLM3D line it
#' covers: the same ug_id (so ignoring "Milchbucktunnel" ignores all of it), the
#' precise kind (road, rail, underpass) and the name. The line whose pieces hit
#' the polygon most often wins. An AV polygon no line reaches keeps its own id
#' and the generic kind.
ug_link_av <- function(av, pieces){
  cand <- which(av$kind %in% c("tunnel", "culvert"))
  if(!length(cand) || nrow(pieces) == 0) return(av)
  hits <- sf::st_intersects(av[cand, ], pieces)
  compatible <- function(avKind, pKind)
    if(avKind == "culvert") pKind == "culvert" else pKind != "culvert"
  for(i in seq_along(cand)){
    h <- hits[[i]]
    if(!length(h)) next
    a <- cand[i]
    h <- h[vapply(pieces$kind[h], function(k) compatible(av$kind[a], k), logical(1))]
    if(!length(h)) next
    best <- as.integer(names(which.max(table(pieces$ug_id[h]))))
    p    <- match(best, pieces$ug_id)
    av$ug_id[a] <- best
    av$kind[a]  <- pieces$kind[p]
    av$name[a]  <- pieces$name[p]
  }
  av
}

#' Drop a TLM3D underground building that an AV or OSM one (`cover`, an sfc)
#' already covers by at least `share` of its area - the same object recorded
#' twice. The AV/OSM one is kept: it knows a garage from a building.
ug_drop_covered <- function(tlm, cover, share = 0.5){
  if(!nrow(tlm) || !length(cover)) return(tlm)
  hits <- sf::st_intersects(tlm, cover)
  keep <- vapply(seq_len(nrow(tlm)), function(i){
    if(!length(hits[[i]])) return(TRUE)
    inter <- suppressWarnings(sf::st_intersection(sf::st_geometry(tlm)[i],
                                                  sf::st_union(cover[hits[[i]]])))
    a <- if(length(inter)) sum(as.numeric(sf::st_area(inter))) else 0
    a < share * as.numeric(sf::st_area(sf::st_geometry(tlm)[i]))
  }, logical(1))
  tlm[keep, ]
}

#' Assemble and write underground_CH_2056.gpkg.
build_underground_CH <- function(out = UG_OUT, av_dir = UG_AV_TILES,
                                 gpkg = LC_TLM3D, dtm = UG_DTM,
                                 cantons = ug_cantons()){
  t0 <- Sys.time()
  say <- function(...) message(sprintf("[%5.1f min] ", as.numeric(difftime(Sys.time(), t0,
                                                                            units = "mins"))),
                               sprintf(...))

  lines <- ug_tlm_lines(gpkg)
  say("TLM3D: %d underground lines (%s)", nrow(lines),
      paste(names(table(lines$kind)), table(lines$kind), collapse = ", "))
  #one element per TLM3D line
  lines$ug_id <- seq_len(nrow(lines))
  pieces <- ug_pieces(lines, terra::rast(dtm))
  pieces$ug_id <- lines$ug_id[pieces$line]
  pieces$kind  <- lines$kind[pieces$line]
  pieces$name  <- lines$name[pieces$line]
  say("pieces: %d, depth median %.1f m, %.0f %% within 15 m", nrow(pieces),
      stats::median(pieces$depth_m, na.rm = TRUE),
      100 * mean(pieces$depth_m <= 15, na.rm = TRUE))
  next_id <- nrow(lines)

  av <- ug_read_av(av_dir)
  if(is.null(av)) stop("no AV tiles under ", av_dir, " - run ug_fetch_av() first")
  say("AV: %d objects (%s)", nrow(av),
      paste(names(table(av$Art)), table(av$Art), collapse = ", "))
  av$ug_id <- next_id + seq_len(nrow(av))
  av$name  <- NA_character_
  next_id  <- next_id + nrow(av)

  gk <- ug_gwr_gklas(av$egid, av$Kanton)
  av$gklas <- unname(gk[as.character(av$egid)])
  av$kind[av$kind == "building" & !is.na(av$gklas) & av$gklas == UG_GARAGE_GKLAS] <- "garage"
  say("GWR: %d of %d AV objects with an EGID resolved, %d garages",
      sum(!is.na(av$gklas)), sum(!is.na(av$egid)), sum(av$kind == "garage"))

  av <- ug_link_av(av, pieces)
  say("AV tunnels/culverts linked to a TLM3D line: %d of %d",
      sum(av$kind %in% c("road_tunnel", "rail_tunnel", "underpass") |
            (av$kind == "culvert" & av$ug_id <= nrow(lines))),
      sum(av$Art %in% c("Tunnel_Unterfuehrung_Galerie", "eingedoltes_oeffentliches_Gewaesser")))

  #OSM where AV stays gated: always in UG_AV_CLOSED, and in a released canton
  #whose objects have not arrived (yet) - a released canton with no AV object
  #at all would be implausible
  got <- unique(av$Kanton)
  gap <- union(UG_AV_CLOSED, setdiff(UG_AV_GATED, got))
  av  <- av[!av$Kanton %in% UG_AV_CLOSED, ]
  osm <- NULL
  have <- if(length(gap)) ug_fetch_osm(gap) else character(0)
  if(length(have)){
    tc  <- sf::st_geometry(sf::st_zm(lines[lines$kind == "culvert", ], drop = TRUE, what = "ZM"))
    osm <- lapply(have, ug_read_osm, cantons = cantons, tlm_culverts = tc)
    osm <- do.call(rbind, osm[!vapply(osm, is.null, logical(1))])
  }
  if(!is.null(osm) && nrow(osm) > 0){
    osm$ug_id <- next_id + seq_len(nrow(osm))
    next_id   <- next_id + nrow(osm)
    say("OSM: %d objects (%s; %s)", nrow(osm),
        paste(names(table(osm$kanton)), table(osm$kanton), collapse = ", "),
        paste(names(table(osm$kind)), table(osm$kind), collapse = ", "))
  }else osm <- NULL

  cover <- sf::st_geometry(av[av$Art == "unterirdisches_Gebaeude", ])
  if(!is.null(osm)) cover <- c(cover, sf::st_geometry(osm[osm$kind != "culvert", ]))
  tb <- ug_drop_covered(ug_tlm_buildings(gpkg), cover)
  tb$ug_id <- next_id + seq_len(nrow(tb))
  say("TLM3D underground buildings kept beside AV/OSM: %d", nrow(tb))

  cols <- c("ug_id", "kind", "name", "source", "egid", "gklas", "depth_m", "rank")
  pieces$source <- "TLM3D"; pieces$egid <- NA_integer_; pieces$gklas <- NA_integer_
  pieces$rank <- 1L
  tb$name <- NA_character_; tb$source <- "TLM3D"; tb$egid <- NA_integer_
  tb$gklas <- NA_integer_; tb$depth_m <- NA_real_; tb$rank <- 2L
  av$source <- "AV"; av$depth_m <- NA_real_; av$rank <- 3L
  #OSM burns with AV: it stands in for it, and the two never share a canton
  if(!is.null(osm)){
    osm$source <- "OSM"; osm$egid <- NA_integer_; osm$gklas <- NA_integer_
    osm$depth_m <- NA_real_; osm$rank <- 3L
  }
  #the parts name their geometry column differently (geometry / geom), and
  #rbind() matches columns by name
  geo <- function(x) sf::st_sf(sf::st_drop_geometry(x)[, cols],
                               geom = sf::st_geometry(x))
  ug <- rbind(geo(pieces), geo(tb), geo(av))
  if(!is.null(osm)) ug <- rbind(ug, geo(osm))
  ug$ug_id <- as.integer(ug$ug_id); ug$rank <- as.integer(ug$rank)

  cantons$status <- ifelse(!cantons$kanton %in% gap, "av",
                           ifelse(cantons$kanton %in% have, "osm", "tlm_only"))
  say("coverage: osm %s; tlm_only %s",
      paste(cantons$kanton[cantons$status == "osm"], collapse = " "),
      paste(cantons$kanton[cantons$status == "tlm_only"], collapse = " "))

  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(out, ".tmp.gpkg")
  unlink(tmp)
  sf::st_write(ug, tmp, layer = "underground", quiet = TRUE)
  sf::st_write(cantons[, c("kanton", "status")], tmp, layer = "coverage",
               append = TRUE, quiet = TRUE)
  unlink(out); file.rename(tmp, out)
  say("wrote %s: %d rows, %d elements", out, nrow(ug), length(unique(ug$ug_id)))
  invisible(out)
}


# ----------------------------------------------------------------- driver ---

if(FALSE){
  #1. AV, ~2200 requests, resumable. Skips tiles lying only in UG_AV_CLOSED.
  ug_fetch_av()
  #1b. NW and OW, released to us: GEODIENSTE_USER/PASS in C:/Users/frueh/.Renviron
  #    (NOT the project .Renviron, which git tracks; ug_auth() reads the user
  #    file itself), check the account (a count; an error means refused), then
  #    refetch only those cantons' tiles. NOT JU LU NE VD - see UG_AV_CLOSED.
  ug_auth_ok()
  k <- ug_cantons()
  ug_fetch_av(ug_tiles_ch(k[k$kanton %in% c("NW", "OW"), ]), overwrite = TRUE)

  #2. everything else, and the file. Fetches OSM itself for JU LU NE VD, and
  #   for any gated canton AV left empty (cached in UG_OSM_DIR;
  #   ug_fetch_osm(..., overwrite = TRUE)
  #   to refresh).
  build_underground_CH()

  #3. checks
  source("data-raw/verify_underground.R")
}
