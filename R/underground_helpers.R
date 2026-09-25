#' Underground structures under painted trees and blocks (Hitzeminderung).
#'
#' A tree needs soil and a block needs foundations, and neither is a given over
#' an underground garage, a tunnel or a culverted stream. The national layer
#' built by data-raw/generate_underground_CH.r says where those are; this file
#' reads it for one study area and answers, per brush stroke, which of them the
#' stroke put a tree or a block on top of.
#'
#' THE BRUSH STOPS AT AN ELEMENT. undergroundMaskRuns() hands the browser the
#' element cells, and paintbrush.js clips every stroke of a warning material
#' at them and ends the stroke there; the user is told what they were doing and
#' over what, and may ignore that one element, after which paint goes over it
#' (newVersions_server.R keeps the list and resets it whenever a warning
#' material is armed afresh). A plan import is not a stroke and is not clipped:
#' undergroundHitCells() only warns about what it covered.
#'
#' DEPTH. Only structures that come from a swissTLM3D 3D line carry one: the
#' floor (road, track, stream bed) under the terrain, measured per 20 m piece.
#' It is a floor depth, not the soil cover, which is shallower by the height of
#' the structure. Underground BUILDINGS have no depth anywhere in the national
#' data, and the warning says so rather than guessing.

#' Base materials that warn: water (5), tree (7) and block (8). Literal on
#' purpose - a top-level expression reading PAINT_CATEGORIES would run before
#' paintbrush_helpers.R is sourced (files load alphabetically), which fails the
#' whole package load. Artificial canopy (6) is excluded by the user's choice.
UG_WARN_BASES <- c(5L, 7L, 8L)

#' Floor depth past which a tunnel or culvert stops mattering to what is planted
#' or built above it. A road tunnel's floor at 15 m leaves ~8 m of cover over its
#' roof; the Alpine tunnels, hundreds of metres down, would otherwise warn under
#' every tree painted on a mountainside. Only cells with a MEASURED depth are
#' dropped - an unknown depth always warns.
UG_MAX_DEPTH_M <- 15

#' The kinds of element and their i18n key (German literal, ASCII like the rest
#' of the translation table).
UNDERGROUND_KINDS <- data.frame(
  kind  = c("garage", "building", "road_tunnel", "rail_tunnel", "tunnel",
            "underpass", "culvert", "reservoir"),
  label = c("Tiefgarage", "Unterirdisches Bauwerk", "Strassentunnel", "Bahntunnel",
            "Tunnel/Unterfuehrung", "Unterfuehrung", "Eingedoltes Gewaesser",
            "Reservoir"),
  stringsAsFactors = FALSE
)

#' Overlay colour of every element, whatever its kind - the hover label names
#' the kind - and of one the user chose to ignore. The ignored grey is also the
#' "Element ignored" button's, the dark one the "Ignore" button's, so the box
#' and the map speak the same code.
UG_COLOR         <- "#555555"
UG_IGNORED_COLOR <- "#c8c8c8"

#' What the user was doing, per warning base: the sentence of the warning,
#' with %s where the element's name goes (in bold). German literal keys.
UG_DOING <- c("5" = "Sie platzieren Wasser ueber einem unterirdischen Bauwerk: %s.",
              "7" = "Sie pflanzen Baeume ueber einem unterirdischen Bauwerk: %s.",
              "8" = "Sie bauen ueber einem unterirdischen Bauwerk: %s.")

#' GWR building classes (GKLAS), short labels. An AV underground object often
#' carries the EGID of the building it belongs to, so its class says what the
#' underground part serves - "offices" for most of central Zurich. Kept here
#' rather than in the translation table: it is reference data from the BFS code
#' list, not interface text, and 26 rows x 4 files for it would be noise.
UG_GKLAS <- data.frame(
  code = c(1110L, 1121L, 1122L, 1130L, 1211L, 1212L, 1220L, 1230L, 1231L, 1241L,
           1242L, 1251L, 1252L, 1261L, 1262L, 1263L, 1264L, 1265L, 1271L, 1272L,
           1273L, 1274L, 1275L, 1276L, 1277L, 1278L),
  de = c("Einfamilienhaus", "Zweifamilienhaus", "Mehrfamilienhaus",
         "Wohngebäude für Gemeinschaften", "Hotel", "Andere Beherbergung",
         "Bürogebäude", "Handel", "Restaurant", "Verkehr/Kommunikation",
         "Garagengebäude", "Industriegebäude", "Lager/Silo", "Kultur/Freizeit",
         "Museum/Bibliothek", "Schule/Hochschule", "Spital", "Sporthalle",
         "Landwirtschaftlicher Betrieb", "Kirche", "Denkmal", "Sonstiger Hochbau",
         "Kollektive Unterkunft", "Tierhaltung", "Pflanzenbau",
         "Landwirtschaftliches Gebäude"),
  fr = c("Maison individuelle", "Maison à 2 logements", "Immeuble d'habitation",
         "Habitat communautaire", "Hôtel", "Autre hébergement",
         "Immeuble de bureaux", "Commerce", "Restaurant", "Transport/communication",
         "Garage", "Bâtiment industriel", "Entrepôt/silo", "Culture/loisirs",
         "Musée/bibliothèque", "Enseignement/recherche", "Hôpital", "Salle de sport",
         "Exploitation agricole", "Édifice religieux", "Monument", "Bâtiment non classé",
         "Hébergement collectif", "Garde d'animaux", "Cultures végétales",
         "Bâtiment agricole"),
  en = c("Detached house", "Two-family house", "Apartment building",
         "Communal housing", "Hotel", "Other accommodation", "Office building",
         "Retail", "Restaurant", "Transport/communication", "Garage building",
         "Industrial building", "Storage/silo", "Culture/leisure",
         "Museum/library", "School/university", "Hospital", "Sports hall",
         "Farm building", "Church", "Monument", "Other building",
         "Collective accommodation", "Animal husbandry", "Crop production",
         "Agricultural building"),
  stringsAsFactors = FALSE
)

#' Where the national file lives: beside the land cover rasters.
undergroundPath <- function(dir = paintLandcoverDir())
  file.path(dir, "underground_CH_2056.gpkg")

#' The underground structures of one study area, ready for the brush.
#'
#' Returns NULL when the national file is absent (the feature then simply does
#' not exist) or the area is unusable, otherwise a list:
#'   elements  sf in EPSG:4326, one row per element for the overlay: ug_id,
#'             kind, name, egid, gklas, dmin, dmax (the measured floor depth
#'             range of its shallow pieces; NA when unknown)
#'   grid      list(xmin, ymax, ncol, nrow, res) of the vectors below, on the
#'             global paint grid
#'   id        integer vector, row-major: the element under each cell, NA none
#'   depth     numeric vector, same cells: measured floor depth, NA unknown
#'   noData    TRUE when part of the area lies in a canton whose survey data
#'             has not been released, so "nothing here" is not a finding
#'
#' Plain vectors rather than SpatRasters so the whole result crosses the mirai
#' boundary as it is (a SpatRaster is an external pointer and would need a
#' wrap/unwrap on each side), and so undergroundHits() is index arithmetic with
#' no terra call on the shared thread.
#'
#' The grid covers the elements' bounding box, not the whole study area: they
#' are a few percent of it, and a 40 M-cell area would otherwise cost 300 MB
#' of vectors for a map that is mostly empty.
#'
#' Same window arithmetic as paintLandcoverSeed(), so the cells line up with
#' the paint cell for cell.
undergroundSeed <- function(aoi, buffer_m = 250, path = undergroundPath(),
                            res = PAINT_RES, max_depth = UG_MAX_DEPTH_M){
  vftTime("paint:undergroundSeed", {
  if(is.null(aoi) || !file.exists(path)) return(NULL)
  if(inherits(aoi, c("sf", "data.frame")) && nrow(aoi) == 0) return(NULL)
  geom <- sf::st_geometry(aoi)
  if(is.na(sf::st_crs(geom))) sf::st_crs(geom) <- 4326
  shp <- try(sf::st_union(sf::st_transform(geom, 2056)), silent = TRUE)
  if(inherits(shp, "try-error")) return(NULL)
  if(buffer_m > 0) shp <- sf::st_buffer(shp, buffer_m)
  wkt <- sf::st_as_text(sf::st_as_sfc(sf::st_bbox(shp)))

  cov <- try(sf::st_read(path, layer = "coverage", wkt_filter = wkt, quiet = TRUE),
             silent = TRUE)
  #OSM stands in for a gated canton's survey but is far from complete, so it
  #still counts as "partly recorded"
  noData <- !inherits(cov, "try-error") && nrow(cov) > 0 &&
    any(cov$status %in% c("tlm_only", "osm") & lengths(sf::st_intersects(cov, shp)) > 0)

  empty <- list(elements = NULL, grid = NULL, id = integer(0), depth = numeric(0),
                noData = noData)
  x <- try(sf::st_read(path, layer = "underground", wkt_filter = wkt, quiet = TRUE),
           silent = TRUE)
  if(inherits(x, "try-error") || nrow(x) == 0) return(empty)
  x <- sf::st_zm(x, drop = TRUE, what = "ZM")
  x <- x[lengths(sf::st_intersects(x, shp)) > 0, ]
  if(nrow(x) == 0) return(empty)

  #the grid: the elements' extent, snapped outward to the paint grid
  e    <- paintWindowExt(x, res)
  tmpl <- terra::rast(e, resolution = res, crs = "EPSG:2056")

  #LINE PIECES FIRST, and id and depth from the SAME piece. Two tubes of a
  #tunnel, or a culvert crossing an underpass, overlap; rasterising the ids and
  #a min() of the depths separately gave a cell one line's id and the other
  #line's depth. So the pieces are ranked shallowest-last (an unknown depth
  #counts as shallowest - it must warn), the rank is burnt with fun = "max", and
  #the id and the depth are both read back off the winning piece.
  pieces <- x[x$rank == 1L, ]
  idv <- rep(NA_integer_, terra::ncell(tmpl))
  dv  <- rep(NA_real_, terra::ncell(tmpl))
  if(nrow(pieces) > 0){
    ord <- order(-ifelse(is.na(pieces$depth_m), -Inf, pieces$depth_m))
    pieces <- pieces[ord, ]
    pieces$prank <- seq_len(nrow(pieces))
    pr <- terra::rasterize(terra::vect(pieces), tmpl, field = "prank", fun = "max",
                           touches = FALSE)
    w  <- as.integer(terra::values(pr, mat = FALSE))
    idv <- pieces$ug_id[w]
    dv  <- pieces$depth_m[w]
  }
  #the winning piece, kept before the depth limit is applied: an AV polygon
  #linked to the same line takes its id and depth from here
  pid <- idv; pdep <- dv
  deep <- !is.na(dv) & dv > max_depth
  #past the limit the structure no longer matters to what is above it
  idv[deep] <- NA_integer_

  #then the outlines, each over the last: TLM3D buildings, AV tunnels and
  #culverts, AV buildings. A building wins over a tunnel it sits beside or on -
  #Zurich HB's underground station overlaps the rail tunnel's AV polygon, and a
  #tree painted there is over the station.
  x$burn <- x$rank + (x$rank == 3L & x$kind %in% c("garage", "building", "reservoir"))
  lineIds <- unique(pieces$ug_id)
  for(k in sort(unique(x$burn[x$rank > 1L]))){
    b <- terra::rasterize(terra::vect(x[x$burn == k, ]), tmpl, field = "ug_id",
                          touches = FALSE)
    bv <- as.integer(terra::values(b, mat = FALSE))
    hit <- !is.na(bv)
    #A depth belongs to the element whose pieces measured it. An AV garage over
    #a deep rail tunnel takes the garage's id here, and must neither inherit the
    #tunnel's 60 m nor be dropped for it. An AV tunnel polygon linked to its
    #TLM3D line shares the line's id: it keeps the depth measured under it, and
    #stays silent where that depth is past the limit.
    same <- hit & !is.na(pid) & bv == pid
    other <- hit & !same
    dv[other]  <- NA_real_
    idv[other] <- bv[other]
    dv[same]   <- pdep[same]
    idv[same]  <- ifelse(deep[same], NA_integer_, bv[same])
  }

  #AN AV TUNNEL POLYGON IS THE WHOLE TUNNEL, the line pieces a 10-15 m strip
  #down its middle. The Islisbergtunnel's AV outline covers 25 600 cells here,
  #and only 8 % of them lay under a piece: the rest warned "depth unknown" all
  #the way under the hill. Such a cell takes the depth of the nearest piece of
  #its OWN line, and goes silent past the limit like the piece would.
  fillC <- which(!is.na(idv) & idv %in% lineIds & is.na(dv))
  if(length(fillC)){
    xy <- terra::xyFromCell(tmpl, fillC)
    for(id in unique(idv[fillC])){
      i  <- which(idv[fillC] == id)
      pp <- pieces[pieces$ug_id == id & !is.na(pieces$depth_m), ]
      if(!nrow(pp)) next
      pts <- sf::st_as_sf(data.frame(x = xy[i, 1], y = xy[i, 2]), coords = c("x", "y"),
                          crs = 2056)
      d <- pp$depth_m[sf::st_nearest_feature(pts, pp)]
      dv[fillC[i]]  <- d
      idv[fillC[i][d > max_depth]] <- NA_integer_
    }
  }

  #THE OVERLAY IS WHAT WARNS. An element is shown only if some cell still
  #carries its id, and a tunnel or culvert is outlined from those very cells -
  #its vectors would also draw the stretches that are too deep to matter.
  #Everything else keeps its surveyed outline.
  live <- unique(idv[!is.na(idv)])
  if(!length(live)) return(empty)
  attrs <- x[x$ug_id %in% live, ]
  attrs <- attrs[order(attrs$ug_id, -attrs$rank), ]
  attrs <- sf::st_drop_geometry(attrs[!duplicated(attrs$ug_id),
                                      c("ug_id", "kind", "name", "egid", "gklas")])
  #measured depth range over the element's warning cells; NA when unknown
  has <- !is.na(idv) & !is.na(dv)
  lo <- tapply(dv[has], idv[has], min); hi <- tapply(dv[has], idv[has], max)
  attrs$dmin <- unname(lo[as.character(attrs$ug_id)])
  attrs$dmax <- unname(hi[as.character(attrs$ug_id)])

  isLine <- attrs$ug_id %in% lineIds
  geomEl <- vector("list", nrow(attrs))
  if(any(!isLine)){
    vx <- x[x$rank > 1L & x$ug_id %in% attrs$ug_id[!isLine], ]
    g  <- lapply(split(sf::st_geometry(vx), vx$ug_id), function(z) sf::st_union(z)[[1]])
    geomEl[!isLine] <- g[as.character(attrs$ug_id[!isLine])]
  }
  if(any(isLine)){
    lv <- idv; lv[!(lv %in% attrs$ug_id[isLine])] <- NA_integer_
    lr <- terra::rast(tmpl); terra::values(lr) <- lv; names(lr) <- "ug_id"
    lp <- sf::st_as_sf(terra::as.polygons(lr, dissolve = TRUE))
    #one metre of tolerance takes out the cell staircase and nothing else
    lp <- sf::st_simplify(lp, preserveTopology = TRUE, dTolerance = res)
    geomEl[isLine] <- sf::st_geometry(lp)[match(attrs$ug_id[isLine], lp$ug_id)]
  }
  el <- sf::st_sf(attrs, geometry = sf::st_sfc(geomEl, crs = 2056))
  el <- sf::st_transform(el, 4326)

  ev <- as.vector(e)
  list(elements = el,
       grid = list(xmin = ev[["xmin"]], ymax = ev[["ymax"]],
                   ncol = terra::ncol(tmpl), nrow = terra::nrow(tmpl), res = res),
       id = idv, depth = dv, noData = noData)
  })
}

#' The element cells as runs for the browser's brush: a flat integer vector
#' (row, colStart, count, ug_id, ...) in the global grid indices the brush and
#' the paint wire format use. Runs never cross a grid row, and NA is left out,
#' so the vector's length follows the elements' outlines, not the grid's size.
undergroundMaskRuns <- function(seed){
  if(is.null(seed) || is.null(seed$grid) || !length(seed$id)) return(integer(0))
  g <- seed$grid
  v <- seed$id
  v[is.na(v)] <- 0L
  n <- length(v)
  start <- c(TRUE, v[-1L] != v[-n]) | ((seq_len(n) - 1L) %% g$ncol == 0L)
  s   <- which(start)
  len <- diff(c(s, n + 1L))
  ids <- v[s]
  keep <- ids != 0L
  if(!any(keep)) return(integer(0))
  s <- s[keep]; len <- len[keep]; ids <- ids[keep]
  ri <- (s - 1L) %/% g$ncol + 1L
  ci <- (s - 1L) %% g$ncol + 1L
  row <- as.integer(round(g$ymax / g$res)) - ri
  col <- ci + as.integer(round(g$xmin / g$res)) - 1L
  as.integer(rbind(row, col, len, ids))
}

#' The cells of a brush flush that carry a warning material, as global grid
#' indices. `runsByCat` is the wire format applyPaintRuns() reads:
#' list(list(id, runs = c(row, colStart, count, ...)), ...).
undergroundRunCells <- function(runsByCat, bases = UG_WARN_BASES){
  rows <- cols <- base <- integer(0)
  for(entry in runsByCat){
    b <- paintBaseId(entry$id)
    if(length(b) != 1L || is.na(b) || !(b %in% bases)) next
    rn <- as.numeric(unlist(entry$runs))
    if(length(rn) < 3) next
    i <- seq(1, length(rn) - 2, by = 3)
    len <- as.integer(rn[i + 2])
    rows <- c(rows, rep(as.integer(rn[i]), len))
    cols <- c(cols, rep(as.integer(rn[i + 1]), len) + sequence(len) - 1L)
    base <- c(base, rep(b, sum(len)))
  }
  data.frame(row = rows, col = cols, base = base)
}

#' The cells of one flush that put a tree or a block on an underground element.
#'
#' `delta` is list(ground = runsByCat, canopy = runsByCat), the shape of both
#' input$paintCells and rasterToRuns() output wrapped per level. A block is level
#' "both" and arrives in both lists, so a cell is kept once per material.
#' Erasing is id 0, not a warning material, and never counts.
#'
#' Returns a data.frame ug_id, base, k (the cell in seed$grid) without the ids in
#' `ignore`; zero rows when nothing was hit. CELLS, not areas, because a stroke
#' reaches R in several flushes and a user paints over the same spot again: the
#' running list is a set of cells, so the area it reports is the area covered,
#' not the number of times it was covered.
undergroundHitCells <- function(delta, seed, ignore = integer(0), bases = UG_WARN_BASES){
  none <- data.frame(ug_id = integer(0), base = integer(0), k = integer(0))
  if(is.null(seed) || is.null(seed$grid) || !length(seed$id)) return(none)
  cells <- rbind(undergroundRunCells(delta$ground, bases),
                 undergroundRunCells(delta$canopy, bases))
  if(!nrow(cells)) return(none)

  g  <- seed$grid
  ci <- cells$col - round(g$xmin / g$res) + 1L
  ri <- round(g$ymax / g$res) - cells$row
  inb <- ci >= 1L & ci <= g$ncol & ri >= 1L & ri <= g$nrow
  if(!any(inb)) return(none)
  k  <- as.integer((ri[inb] - 1L) * g$ncol + ci[inb])
  id <- seed$id[k]
  keep <- !is.na(id) & !(id %in% ignore)
  if(!any(keep)) return(none)
  out <- data.frame(ug_id = id[keep], base = cells$base[inb][keep], k = k[keep])
  out[!duplicated(out), ]
}

#' Area and measured depth range per element and material, from a set of cells.
undergroundSummarise <- function(cells, seed){
  if(is.null(cells) || !nrow(cells))
    return(data.frame(ug_id = integer(0), base = integer(0), m2 = numeric(0),
                      dmin = numeric(0), dmax = numeric(0)))
  dep <- if(length(seed$depth)) seed$depth[cells$k] else rep(NA_real_, nrow(cells))
  grp <- split(seq_len(nrow(cells)), paste(cells$ug_id, cells$base))
  do.call(rbind, unname(lapply(grp, function(i){
    d <- dep[i]
    data.frame(ug_id = cells$ug_id[i[1]], base = cells$base[i[1]],
               m2 = length(i) * seed$grid$res^2,
               dmin = if(all(is.na(d))) NA_real_ else min(d, na.rm = TRUE),
               dmax = if(all(is.na(d))) NA_real_ else max(d, na.rm = TRUE))
  })))
}

#' Which underground elements one flush put a tree or a block on: one row per
#' element and material with m2, dmin and dmax.
undergroundHits <- function(delta, seed, ignore = integer(0), bases = UG_WARN_BASES)
  undergroundSummarise(undergroundHitCells(delta, seed, ignore, bases), seed)

#' Fold a flush's cells into the running set.
undergroundMergeCells <- function(acc, new){
  if(is.null(acc) || !nrow(acc)) return(new)
  if(is.null(new) || !nrow(new)) return(acc)
  all <- rbind(acc, new)
  all[!duplicated(all), ]
}

#' The precise name of one element, in the current language:
#' "Tiefgarage (EGID 140889)", "Strassentunnel «Milchbucktunnel»",
#' "Unterirdisches Bauwerk (Bürogebäude, EGID 302005846)".
undergroundLabel <- function(el, tr = NULL, lang = "de"){
  k <- match(el$kind, UNDERGROUND_KINDS$kind)
  base <- if(is.na(k)) el$kind else vftTrText(tr, UNDERGROUND_KINDS$label[k])
  if(!is.null(el$name) && !is.na(el$name) && nzchar(el$name))
    base <- paste0(base, " «", el$name, "»")
  extra <- character(0)
  if(identical(el$kind, "building") && !is.null(el$gklas) && !is.na(el$gklas)){
    g <- match(el$gklas, UG_GKLAS$code)
    col <- if(lang %in% c("de", "fr", "en")) lang else "de"
    if(!is.na(g)) extra <- c(extra, UG_GKLAS[[col]][g])
  }
  if(!is.null(el$egid) && !is.na(el$egid)) extra <- c(extra, paste("EGID", el$egid))
  if(length(extra)) base <- paste0(base, " (", paste(extra, collapse = ", "), ")")
  base
}

#' "Sohle ca. 3-6 m unter Terrain", or "Tiefe unbekannt".
undergroundDepthText <- function(dmin, dmax, tr = NULL){
  if(is.null(dmin) || is.na(dmin)) return(vftTrText(tr, "Tiefe unbekannt"))
  lo <- round(dmin); hi <- round(dmax)
  r  <- if(is.na(hi) || hi == lo) format(lo) else paste0(lo, "–", hi)
  sprintf(vftTrText(tr, "Sohle ca. %s m unter Terrain"), r)
}

#' The hover label of an element on the overlay.
undergroundTooltip <- function(el, tr = NULL, lang = "de", ignored = FALSE){
  s <- paste0(undergroundLabel(el, tr, lang), " · ",
              undergroundDepthText(el$dmin, el$dmax, tr))
  if(ignored) s <- paste0(s, " · ", vftTrText(tr, "ignoriert"))
  s
}

#' The one element a set of hit cells is reported as - the one with the most
#' cells - and the warning material that covered most of it. NULL for no cells.
#' Used for a plan import, which can cover several elements at once; a stroke
#' stops at the first and reports that one.
undergroundTopHit <- function(cells){
  if(is.null(cells) || !nrow(cells)) return(NULL)
  n  <- table(cells$ug_id)
  id <- as.integer(names(n)[which.max(n)])
  b  <- table(cells$base[cells$ug_id == id])
  list(ug_id = id, base = as.integer(names(b)[which.max(b)]))
}

#' The body of the warning box: what the user is doing, over which element (in
#' bold), that it is not recommended, and under it the button that ignores the
#' element - or, once it is ignored, a light grey "Element ignored" in its place.
#'
#' `el` is the element's row of seed$elements (NULL/empty falls back to its id),
#' `base` the warning base being painted (5, 7, 8), `ignoreInput` the namespaced
#' input id the button sets.
undergroundWarningUI <- function(el, id, base, ignored, ignoreInput, tr = NULL, lang = "de"){
  lab <- if(!is.null(el) && nrow(el) && !is.na(el$ug_id)) undergroundLabel(el, tr, lang)
         else paste("#", id)
  key <- UG_DOING[as.character(base)]
  if(is.na(key)) key <- UG_DOING[["7"]]
  txt <- vftTrText(tr, key)
  #"over a Underground car park": English wants the article to follow the name
  if(identical(lang, "en") && grepl("^[AEIOUaeiou]", lab)) txt <- sub(" a %s", " an %s", txt, fixed = TRUE)
  parts <- strsplit(txt, "%s", fixed = TRUE)[[1]]
  if(length(parts) < 2) parts <- c(parts, "")
  btnStyle <- "margin-top: 8px; border: none; border-radius: 3px; padding: 3px 10px;"
  btn <- if(ignored){
    shiny::tags$button(type = "button", disabled = NA,
                       style = paste0(btnStyle, " background: ", UG_IGNORED_COLOR,
                                      "; color: #555; cursor: default;"),
                       vftTrText(tr, "Element ignoriert"))
  }else{
    shiny::tags$button(type = "button",
                       style = paste0(btnStyle, " background: ", UG_COLOR, "; color: #fff;"),
                       onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});",
                                         ignoreInput, as.integer(id)),
                       vftTrText(tr, "Dieses Element ignorieren"))
  }
  shiny::tagList(
    #no formatting whitespace around the name: it would render as "garage ."
    shiny::tags$div(parts[1], shiny::tags$b(lab, .noWS = "outside"),
                    paste(parts[-1], collapse = "%s")),
    shiny::tags$div(vftTrText(tr, "Dies wird nicht empfohlen.")),
    shiny::tags$div(btn)
  )
}
