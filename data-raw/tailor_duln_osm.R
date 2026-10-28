#### DULN fitted to OpenStreetMap public GPS traces, pooled over new Swiss areas ####
#
# data-raw/tailor_duln_strava.R fitted the DULN element weights to Strava's heatmap at
# Glatt. This fits them to OSM's public GPS traces instead (the user's choice,
# 2026-10-26), over several Swiss recreation areas, each with its own automatically
# generated areas (the validation's aoi stage: threshold 3.1, the adopted cut). Glatt,
# GlattPark and Wigger are NOT used: every candidate's network extent (shapeLarger)
# keeps >= 2 km from theirs, so all three survey evaluations stay independent of the fit.
#
# THE DATA: GET /api/0.6/trackpoints?bbox=...&page=N (5000 points a page, the only
# source of current traces; JOSM's "Download raw GPS data" makes the same call). The
# timed traces (trackable, identifiable) come first, then the untimed shuffled points of
# the old public / private traces, which are useless here: a box is paged up to its first
# untimed or short page. One request at a time, at least 1 s apart, cached (a rerun
# resumes), with an identifying User-Agent and the user's contact (OSM's API usage
# policy: the API is for editing; heavy users should be reachable). Deep pages time out
# (503 after ~15 s): a box that fails three times running is split in four, its pages
# set aside. GPX files saved from JOSM in pages/<area>/josm/ are read as well.
# Only aggregates (counts per edge and cell, weights, maps) may be published, with
# "(c) OpenStreetMap contributors" (ODbL); never the traces or user names.
#
# THE TRACES: pieces are chained into traces (identifiable ones by trace id, trackable
# ones by continuity: the next piece starts <= 30 s and <= 200 m after), deduplicated
# (same start, end and point count), split at gaps (> 5 min or > 500 m) and at motor
# stretches (60 s speed > 35 km/h); segments of >= 10 points, 300 m and 2 min are
# classed on their 30 s moving speeds (rules set in advance):
#   drop   a vehicle's desc/name (openpilot bots, trains, boats, ...)
#   foot   moving speed median <= 7 km/h and p85 <= 9
#   run    median 7.5-13.5 and p85 <= 16
#   bike   median 12-35 and p95 <= 55        (run first where both hold)
#   keywords in name/desc/tags (jog/run, bike/velo, walk/hike) override the speed class;
#   on the keyword-labelled segments the speed class must agree >= 75 %, else only the
#   shared (all recreational) layer is fitted.
# Hike vs walk and e-bike vs bike cannot be told apart: three groups,
#   foot   walkNat, walkSoc, dogNat, dogProx        run   jogger
#   bike   bikeSport, ebikeNat
# An outing (a trace up to a 30 min pause: an upload can hold "130 bike rides") counts
# once on an edge when a line of it, buffered 15 m, covers >= 50 % of the edge;
# an identifiable user counts at most twice per edge. A cell's target is the
# length-weighted mean of its edges' counts (as the extract's elements), fitted as
# counts (quasi-Poisson ridge, each area's Nearby-Random map per group the offset).
#
# Stages (VFT_OSM_STAGES):
#   areas      the candidate outlines and their contexts (always, before anything else)
#   contexts   the validation's aoi, survey (a stub) and network stages per candidate
#   crawl      the API pages per candidate
#   traces     pages -> traces -> segments -> classes
#   match      segments on the edges; the inventory and the selection of the areas
#   gate       split-half reliability per group over the selected areas
#   extract    element layers and extracts, Nearby-Random runs per type, per selected area
#   fit        the pooled fit (free and signed), leave-one-area-out, weights tables
#   typed, score   tailor_duln_activities.R's, by the 3 groups
# The variants (stairs x3 for bikers) are built by data-raw/apply_duln_variant.R with
# VFT_AV_SCRIPT=tailor_duln_osm.R and VFT_AV_TYSUB=osm/clean (O3E_s0.5, OSE_s0.5) or
# osm/clean/signed (O3Ec_s0.5).
#
#   Rscript data-raw/tailor_duln_osm.R
#
# VFT_OSM_STAGES   which stages (default areas, contexts, crawl, traces, match)
# VFT_OSM_RAW      outlines, pages, traces (default Local Data/osm_traces)
# VFT_OSM_VAL      the validation folder of the areas (default .._2026_osm_areas)
# VFT_OSM_CONTACT  the User-Agent's contact (default the user's, given 2026-10-26)
# VFT_OSM_AREAS    a comma list: crawl only these candidates
# VFT_OSM_SELECT   a comma list: fit these areas instead of the selection rule's
# VFT_OSM_GROUPS   a comma list: fit these groups whatever the gate says (the user,
#                  2026-10-26, after the 3-area test failed the gate: fit foot, run and
#                  bike anyway and judge them by the simulation against the surveys)
# The tailored folder (VFT_TD_OUT) defaults to Local Data/osm_traces/duln_tailored, the
# validation folder (VFT_VAL_OUT) to VFT_OSM_VAL: tailor_duln.R's default, _final_timefix,
# was deleted on purpose (2026-10-23) and must not be recreated.

IS_OSM_WORKER <- identical(Sys.getenv("VFT_TY_WORKER"), "1")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
OSM_SCRIPT <- file.path(.repo, "data-raw", "tailor_duln_osm.R")
if(!IS_OSM_WORKER){
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if(length(f) && grepl("tailor_duln_osm", f[1])) OSM_SCRIPT <- normalizePath(f[1], winslash = "/")
}
.osmStages <- Sys.getenv("VFT_OSM_STAGES", "areas,contexts,crawl,traces,match")
OSM_LOCAL <- "C:/Users/frueh/Documents/Local Data"
OSM_RAW <- Sys.getenv("VFT_OSM_RAW", file.path(OSM_LOCAL, "osm_traces"))
OSM_VAL <- Sys.getenv("VFT_OSM_VAL", file.path(OSM_LOCAL, "pathUsageValidation_2026_osm_areas"))
#the evaluation contexts the areas keep away from
OSM_EVAL <- file.path(OSM_LOCAL, "pathUsageValidation_2026_best_autoCut")
OSM_BORDER <- file.path(OSM_LOCAL, "underground", "swissboundaries3d_2026-01_2056_5728.gpkg")
OSM_CTX_FILE <- file.path(OSM_RAW, "areas", "contexts.rds")
osmRaw <- function(...){ p <- file.path(OSM_RAW, ...); dir.create(dirname(p[1]), recursive = TRUE, showWarnings = FALSE); p }

#### stage areas (plain sf: it must exist before the validation script is sourced) ####

#the candidates, set before any trace was seen (2026-10-26): recreation areas at the
#edge of the large agglomerations; Zuerichberg lies inside Glatt's extent and Uetliberg
#5.9 km from it, so the Zurich area is the Albis south of them
OSM_CANDIDATES <- data.frame(
  ctx = c("albis", "eschenberg", "zugerberg", "baselErlen", "bruderholz", "bremgarten", "bantiger",
          "jorat", "rhone", "dietschiberg", "dreiWeieren", "monteBre"),
  label = c("Albis - Tuerlersee", "Winterthur - Eschenberg", "Zug - Zugerberg", "Basel - Lange Erlen",
            "Basel - Bruderholz", "Bern - Bremgartenwald", "Bern - Bantiger", "Lausanne - Jorat",
            "Geneva - Rhone", "Lucerne - Dietschiberg", "St. Gallen - Drei Weieren", "Lugano - Monte Bre"),
  lon = c(8.500, 8.770, 8.545, 7.650, 7.585, 7.410, 7.535, 6.660, 6.080, 8.320, 9.385, 8.960),
  lat = c(47.225, 47.470, 47.140, 47.585, 47.515, 46.975, 46.965, 46.560, 46.200, 47.045, 47.420, 46.010),
  stringsAsFactors = FALSE)
OSM_RADIUS_M <- 4500
OSM_GAP_M <- 2000

#Each candidate: a 4.5 km circle (LV95) clipped to Switzerland; its shapeLarger built as
#the validation does (1000 m in 3857); dropped if that comes within 2 km of an
#evaluation context's or if its outline overlaps an earlier candidate's.
osmAreas <- function(){
  if(file.exists(OSM_CTX_FILE)) return(invisible(readRDS(OSM_CTX_FILE)))
  ch <- sf::st_read(OSM_BORDER, layer = "tlm_landesgebiet", quiet = TRUE)
  ch <- sf::st_union(sf::st_zm(sf::st_geometry(ch[ch$icc == "CH", ])))
  larger <- function(g) sf::st_transform(sf::st_union(sf::st_buffer(sf::st_transform(g, 3857), 1000)), 2056)
  ref <- lapply(c(glatt = "glatt", glattPark = "glattPark", wigger = "wigger"), function(cx)
    sf::st_transform(sf::st_union(readRDS(file.path(OSM_EVAL, "aoi", paste0(cx, ".rds")))$shapeLarger), 2056))
  ctxs <- list(); tab <- list(); kept <- list()
  for(i in seq_len(nrow(OSM_CANDIDATES))){
    cd <- OSM_CANDIDATES[i, ]
    p <- sf::st_transform(sf::st_sfc(sf::st_point(c(cd$lon, cd$lat)), crs = 4326), 2056)
    o <- sf::st_intersection(sf::st_buffer(p, OSM_RADIUS_M, nQuadSegs = 32), ch)
    if(any(sf::st_geometry_type(o) == "GEOMETRYCOLLECTION")) o <- sf::st_collection_extract(o, "POLYGON")
    o <- sf::st_union(o)
    dRef <- vapply(ref, function(r) as.numeric(sf::st_distance(larger(o), r)), 1)
    overlap <- names(kept)[vapply(kept, function(k) sf::st_intersects(k, o, sparse = FALSE)[1, 1], TRUE)]
    ok <- all(dRef >= OSM_GAP_M) && !length(overlap)
    tab[[i]] <- data.frame(ctx = cd$ctx, label = cd$label, lon = cd$lon, lat = cd$lat,
                           km2 = as.numeric(sf::st_area(o)) / 1e6, kmToGlatt = dRef[["glatt"]] / 1000,
                           kmToGlattPark = dRef[["glattPark"]] / 1000, kmToWigger = dRef[["wigger"]] / 1000,
                           overlaps = paste(overlap, collapse = " "), kept = ok)
    if(!ok) next
    kept[[cd$ctx]] <- o
    f <- osmRaw("areas", paste0(cd$ctx, ".gpkg"))
    sf::st_write(sf::st_sf(name = cd$ctx, geometry = sf::st_transform(o, 4326)), f, delete_dsn = TRUE, quiet = TRUE)
    #as Glatt's entry (validate_usage_surveys.R CTX), without a survey; the national
    #residents raster for the starts, 800 agents
    ctxs[[cd$ctx]] <- list(label = cd$label, perimeter = normalizePath(f, winslash = "/"),
                           residential = "maps/residential/residentialData_raster_final.tif",
                           nAgents = 800, survey = "none", buffer = 50,
                           scales = c(1, 2, 5, 10), oldScale = 5, simTransform = "none",
                           dropCells = NULL, lakesToZero = TRUE, surveyZerosNA = FALSE,
                           parkingShare = 0.08, savedSurvey = NULL, oldAoi = NULL)
  }
  tab <- do.call(rbind, tab)
  utils::write.csv(tab, osmRaw("areas", "candidates.csv"), row.names = FALSE)
  print(tab, digits = 3, row.names = FALSE)
  if(!length(ctxs)) stop("no candidate passes the distance rule")
  saveRDS(ctxs, OSM_CTX_FILE)
  invisible(ctxs)
}
if(!IS_OSM_WORKER && !file.exists(OSM_CTX_FILE)) osmAreas()
OSM_CTXS <- names(readRDS(OSM_CTX_FILE))

#### the chain: the clean element set (as the Strava fits) on the validation script ####

Sys.setenv(VFT_VAL_EXTRA_CTX = OSM_CTX_FILE, VFT_EL_STAGES = "none", VFT_ACT_STAGES = "none")
if(!nzchar(Sys.getenv("VFT_VAL_OUT"))) Sys.setenv(VFT_VAL_OUT = OSM_VAL)
if(!nzchar(Sys.getenv("VFT_TD_OUT"))) Sys.setenv(VFT_TD_OUT = file.path(OSM_RAW, "duln_tailored"))
if(!nzchar(Sys.getenv("VFT_VAL_THRESHOLD"))) Sys.setenv(VFT_VAL_THRESHOLD = "3.1")
if(!nzchar(Sys.getenv("VFT_TY_WORKERS"))) Sys.setenv(VFT_TY_WORKERS = "3")
source(file.path(.repo, "data-raw", "tailor_duln_elements.R"))
TY_SCRIPT <- OSM_SCRIPT
OSM <- file.path(TD, "osm")
OSM_FIT <- file.path(OSM, "clean")
osmPath <- function(...){ p <- file.path(OSM, ...); dir.create(dirname(p[1]), recursive = TRUE, showWarnings = FALSE); p }
fitPath <- function(...){ p <- file.path(OSM_FIT, ...); dir.create(dirname(p[1]), recursive = TRUE, showWarnings = FALSE); p }
ACT <- OSM_FIT
OSM_STAGES <- strsplit(.osmStages, ",", fixed = TRUE)[[1]]
OSM_ONLY <- envList("VFT_OSM_AREAS", OSM_CTXS)
inVal <- function(expr) withOut(OSM_VAL, expr)

#the 3 groups
OSM3 <- c("foot", "run", "bike")
OSM_LABEL <- c(foot = "On foot (walk, hike, dog)", run = "Joggers", bike = "Bikers (bike, e-bike)")
OSM_OF <- c(walkNat = "foot", walkSoc = "foot", dogNat = "foot", dogProx = "foot", jogger = "run",
            bikeSport = "bike", ebikeNat = "bike")
OSM_SAME <- c(walkSoc = "walkNat", dogNat = "walkNat", dogProx = "walkNat", ebikeNat = "bikeSport")
ACT4 <- OSM3; ACT_OF <- OSM_OF; ACT_LABEL <- OSM_LABEL
ACT_COL <- c(foot = "walkNat", run = "jogger", bike = "bikeSport")

#### stage contexts ####

stageContexts <- function(){
  for(ctx in OSM_CTXS) inVal({ stageAoi(ctx); stageSurvey(ctx); stageNetwork(ctx) })
  invisible(NULL)
}

#### stage crawl ####

OSM_API <- "https://api.openstreetmap.org/api/0.6/"
OSM_UA <- sprintf("visitorFlowTool-research/1.0 (DULN calibration from public GPS traces; contact %s)",
                  Sys.getenv("VFT_OSM_CONTACT", "johanfruh@gmail.com"))
OSM_BOX <- c(lon = 0.04, lat = 0.03)
OSM_PAGE <- 5000L

.osmLast <- new.env(); .osmLast$t <- 0
osmGet <- function(url){
  wait <- 1 - (proc.time()[["elapsed"]] - .osmLast$t)
  if(wait > 0) Sys.sleep(wait)
  h <- curl::new_handle(useragent = OSM_UA, timeout = 120, connecttimeout = 30)
  t0 <- proc.time()[["elapsed"]]
  r <- tryCatch(curl::curl_fetch_memory(url, handle = h),
                error = function(e) list(status_code = -1L, content = raw(0), err = conditionMessage(e)))
  .osmLast$t <- proc.time()[["elapsed"]]
  list(status = r$status_code, body = r$content, secs = .osmLast$t - t0, err = r$err)
}
#points and timed points in a GPX page
gpxCounts <- function(txt) c(pts = lengths(regmatches(txt, gregexpr("<trkpt", txt, fixed = TRUE))),
                             timed = lengths(regmatches(txt, gregexpr("<time>", txt, fixed = TRUE))))
provenance <- function(row){
  f <- osmRaw("provenance.csv")
  utils::write.table(row, f, sep = ",", row.names = FALSE, col.names = !file.exists(f), append = file.exists(f))
}
#the boxes over an area's network extent (4326): a grid of OSM_BOX
osmBoxes <- function(ctx){
  bb <- sf::st_bbox(sf::st_transform(inVal(readRDS(aoiFile(ctx)))$shapeLarger, 4326))
  xs <- seq(bb[["xmin"]], bb[["xmax"]], by = OSM_BOX[["lon"]]); ys <- seq(bb[["ymin"]], bb[["ymax"]], by = OSM_BOX[["lat"]])
  g <- expand.grid(i = seq_along(xs), j = seq_along(ys))
  data.frame(box = sprintf("x%02d_y%02d", g$i, g$j), w = xs[g$i], s = ys[g$j],
             e = pmin(xs[g$i] + OSM_BOX[["lon"]], bb[["xmax"]]), n = pmin(ys[g$j] + OSM_BOX[["lat"]], bb[["ymax"]]))
}
#One box, paged until its first untimed or short page. done.txt ends it; split.txt
#sends it to its four quarters (its own pages go to discarded/, the quarters page
#afresh, so no point is counted twice).
crawlBox <- function(ctx, box, bb, depth = 0){
  d <- osmRaw("pages", ctx, box, "x")
  d <- dirname(d)
  if(file.exists(file.path(d, "done.txt"))) return(invisible(NULL))
  quarters <- function(){
    mx <- (bb[["w"]] + bb[["e"]]) / 2; my <- (bb[["s"]] + bb[["n"]]) / 2
    list(c(w = bb[["w"]], s = bb[["s"]], e = mx, n = my), c(w = mx, s = bb[["s"]], e = bb[["e"]], n = my),
         c(w = bb[["w"]], s = my, e = mx, n = bb[["n"]]), c(w = mx, s = my, e = bb[["e"]], n = bb[["n"]]))
  }
  if(file.exists(file.path(d, "split.txt"))){
    q <- quarters()
    for(k in 1:4) crawlBox(ctx, file.path(box, paste0("q", k)), q[[k]], depth + 1)
    return(invisible(NULL))
  }
  pages <- list.files(d, "^p[0-9]+\\.gpx$")
  page <- length(pages)
  fails <- 0L
  repeat{
    url <- sprintf("%strackpoints?bbox=%.6f,%.6f,%.6f,%.6f&page=%d", OSM_API, bb[["w"]], bb[["s"]], bb[["e"]], bb[["n"]], page)
    r <- osmGet(url)
    cnt <- if(r$status == 200) gpxCounts(rawToChar(r$body)) else c(pts = NA, timed = NA)
    provenance(data.frame(time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), ctx = ctx, box = box, page = page,
                          bbox = sprintf("%.6f,%.6f,%.6f,%.6f", bb[["w"]], bb[["s"]], bb[["e"]], bb[["n"]]),
                          status = r$status, secs = round(r$secs, 2), points = cnt[["pts"]], timed = cnt[["timed"]]))
    if(r$status %in% c(429, 509)) stop("the API asks to slow down (", r$status, ") - stopped; rerun later")
    if(r$status == 200 && grepl("<gpx", rawToChar(r$body[seq_len(min(400, length(r$body)))]), fixed = TRUE)){
      fails <- 0L
      writeBin(r$body, file.path(d, sprintf("p%04d.gpx", page)))
      if(cnt[["pts"]] < OSM_PAGE || cnt[["timed"]] < cnt[["pts"]]){
        writeLines(sprintf("%s page %d: %d points, %d timed", format(Sys.time()), page, cnt[["pts"]], cnt[["timed"]]),
                   file.path(d, "done.txt"))
        return(invisible(NULL))
      }
      page <- page + 1L
      next
    }
    if(r$status %in% c(-1L, 500L, 502L, 503L, 504L)){
      fails <- fails + 1L
      if(fails >= 3){
        if(depth >= 4) stop(sprintf("%s %s: page %d fails at the smallest box", ctx, box, page))
        logf("crawl %s %s: page %d failed 3 times - split in four", ctx, box, page)
        dir.create(file.path(d, "discarded"), showWarnings = FALSE)
        for(p in list.files(d, "^p[0-9]+\\.gpx$")) file.rename(file.path(d, p), file.path(d, "discarded", p))
        writeLines(format(Sys.time()), file.path(d, "split.txt"))
        return(crawlBox(ctx, box, bb, depth))
      }
      Sys.sleep(c(20, 60)[fails])
      next
    }
    stop(sprintf("%s %s page %d: status %d %s", ctx, box, page, r$status, if(is.null(r$err)) "" else r$err))
  }
}
stageCrawl <- function(){
  for(ctx in intersect(OSM_ONLY, OSM_CTXS)){
    bx <- osmBoxes(ctx)
    t0 <- proc.time()[["elapsed"]]
    for(i in seq_len(nrow(bx))) crawlBox(ctx, bx$box[i], unlist(bx[i, c("w", "s", "e", "n")]))
    pv <- utils::read.csv(osmRaw("provenance.csv"))
    pv <- pv[pv$ctx == ctx, ]
    logf("crawl %s: %d boxes, %d requests so far (%d not 200), %.1f min this run", ctx, nrow(bx), nrow(pv),
         sum(pv$status != 200), (proc.time()[["elapsed"]] - t0) / 60)
  }
}

#### stage traces ####

OSM_VEHICLE <- "pilot|routes from|\\btrain\\b|s-bahn|\\bbus\\b|\\bcar\\b|\\bauto\\b|\\bboat\\b|schiff|segel|sailing|t\u00f6rn|flight|\\bflug|tram\\b|\\bdrive\\b|driving"
OSM_KEY <- list(run = "jog|\\brun\\b|running|\\blauf|laufen|course \u00e0 pied",
                bike = "bike|\\bvelo|\\brad\\b|radtour|\\bmtb\\b|\\bvtt\\b|cycl|\\bv\u00e9lo",
                foot = "\\bwalk|spazier|wander|\\bhike|hiking|randonn|\\bdog\\b|hund|promenade")

#the pages of an area as pieces (one per trkseg; untimed points dropped) and points
readPages <- function(ctx){
  fs <- list.files(osmRaw("pages", ctx), "\\.gpx$", recursive = TRUE, full.names = TRUE)
  fs <- fs[!grepl("/discarded/", fs)]
  pc <- list(); pt <- list(); k <- 0L
  for(f in fs){
    x <- tryCatch(xml2::read_xml(f), error = function(e) NULL)
    if(is.null(x)) next
    xml2::xml_ns_strip(x)
    for(tk in xml2::xml_find_all(x, "//trk")){
      txt <- function(q){ n <- xml2::xml_find_first(tk, q); if(inherits(n, "xml_missing")) NA_character_ else xml2::xml_text(n) }
      url <- txt("./url"); if(is.na(url)) url <- txt("./link/@href")
      for(sg in xml2::xml_find_all(tk, "./trkseg")){
        p <- xml2::xml_find_all(sg, "./trkpt")
        if(!length(p)) next
        tm <- xml2::xml_text(xml2::xml_find_first(p, "./time"))
        ok <- !is.na(tm)
        if(sum(ok) < 2) next
        k <- k + 1L
        tt <- as.numeric(as.POSIXct(sub("Z$", "", tm[ok]), format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
        pt[[k]] <- data.frame(piece = k, lon = as.numeric(xml2::xml_attr(p[ok], "lon")),
                              lat = as.numeric(xml2::xml_attr(p[ok], "lat")), t = tt)
        pc[[k]] <- data.frame(piece = k, file = substring(f, nchar(osmRaw("pages", ctx)) + 2),
                              traceId = suppressWarnings(as.integer(sub(".*/traces/([0-9]+).*", "\\1", url))),
                              user = if(is.na(url)) NA_character_ else utils::URLdecode(sub("^.*/user/([^/]+)/.*$", "\\1", url)),
                              name = txt("./name"), desc = txt("./desc"), stringsAsFactors = FALSE)
      }
    }
  }
  list(pieces = do.call(rbind, pc), points = do.call(rbind, pt), files = length(fs))
}
#Pieces -> traces: identifiable by trace id; trackable chained (the next piece starts
#<= 30 s and <= 200 m after one ends). Returns the trace of each piece.
chainPieces <- function(pc, pts){
  fl <- do.call(rbind, lapply(split(pts, pts$piece), function(p){ o <- order(p$t); p <- p[o, ]
    data.frame(piece = p$piece[1], t0 = p$t[1], t1 = p$t[nrow(p)], x0 = p$x[1], y0 = p$y[1],
               x1 = p$x[nrow(p)], y1 = p$y[nrow(p)], n = nrow(p)) }))
  fl <- fl[match(pc$piece, fl$piece), ]
  tr <- ifelse(is.na(pc$traceId), NA_character_, paste0("id", pc$traceId))
  anon <- which(is.na(pc$traceId))
  if(length(anon)){
    nxt <- rep(NA_integer_, nrow(pc))
    o <- anon[order(fl$t0[anon])]
    st <- fl$t0[o]
    used <- logical(nrow(pc))
    for(i in anon[order(fl$t1[anon])]){
      lo <- findInterval(fl$t1[i] - 1e-9, st) + 1L; hi <- findInterval(fl$t1[i] + 30, st)
      if(lo > hi) next
      cand <- o[lo:hi]
      cand <- cand[cand != i & !used[cand] & sqrt((fl$x0[cand] - fl$x1[i])^2 + (fl$y0[cand] - fl$y1[i])^2) <= 200]
      if(!length(cand)) next
      j <- cand[1]; nxt[i] <- j; used[j] <- TRUE
    }
    head <- anon[!used[anon]]
    for(h in head){ i <- h; lab <- paste0("an", pc$piece[h]); while(!is.na(i)){ tr[i] <- lab; i <- nxt[i] } }
    tr[anon][is.na(tr[anon])] <- paste0("an", pc$piece[anon][is.na(tr[anon])])
  }
  tr
}
#rolling speed (km/h) over at least `w` seconds ahead of each point
rollSpeed <- function(t, cum, w){
  j <- findInterval(t + w - 1e-9, t) + 1L
  j[j > length(t)] <- NA
  ok <- !is.na(j)
  v <- rep(NA_real_, length(t))
  v[ok] <- 3.6 * (cum[j[ok]] - cum[ok]) / (t[j[ok]] - t[ok])
  v
}
#a trace's points -> segments: split at gaps and motor stretches
segmentTrace <- function(p){
  p <- p[order(p$t), ]; p <- p[!duplicated(p$t), ]
  if(nrow(p) < 10) return(NULL)
  dd <- c(0, sqrt(diff(p$x)^2 + diff(p$y)^2)); dt <- c(0, diff(p$t))
  brk <- dt > 300 | dd > 500
  #an outing ends at a 30 min pause (an upload can hold many: "130 bike rides")
  outing <- cumsum(dt > 1800)
  cum <- cumsum(dd)
  v60 <- rollSpeed(p$t, cum, 60)
  motor <- !is.na(v60) & v60 > 35
  #a motor point and the points of its window
  seg <- cumsum(brk | motor)
  keep <- !motor
  out <- lapply(split(which(keep), seg[keep]), function(i){
    if(length(i) < 10) return(NULL)
    q <- p[i, ]; c2 <- cumsum(c(0, sqrt(diff(q$x)^2 + diff(q$y)^2)))
    len <- c2[length(c2)]; dur <- q$t[nrow(q)] - q$t[1]
    if(len < 300 || dur < 120) return(NULL)
    v <- rollSpeed(q$t, c2, 30); v <- v[!is.na(v) & v > 1]
    if(length(v) < 5) return(NULL)
    list(pts = q[, c("x", "y", "t")], outing = outing[i[1]], len = len, dur = dur, vMed = stats::median(v),
         v85 = unname(stats::quantile(v, .85)), v95 = unname(stats::quantile(v, .95)))
  })
  Filter(Negate(is.null), out)
}
speedClass <- function(m, p85, p95){
  ifelse(m <= 7 & p85 <= 9, "foot",
         ifelse(m >= 7.5 & m <= 13.5 & p85 <= 16, "run",
                ifelse(m >= 12 & m <= 35 & p95 <= 55, "bike", NA_character_)))
}
keyClass <- function(txt){
  if(!length(txt)) return(character(0))
  txt <- tolower(ifelse(is.na(txt), "", txt))
  hit <- sapply(OSM_KEY, function(re) grepl(re, txt, perl = TRUE))
  if(!is.matrix(hit)) hit <- matrix(hit, nrow = 1, dimnames = list(NULL, names(OSM_KEY)))
  out <- rep(NA_character_, length(txt))
  one <- rowSums(hit) == 1
  out[one] <- colnames(hit)[max.col(hit[one, , drop = FALSE])]
  out
}
#an identifiable trace's tags (GET gpx/<id>), cached
traceTags <- function(id){
  f <- osmRaw("meta", paste0(id, ".xml"))
  if(!file.exists(f)){
    r <- osmGet(sprintf("%sgpx/%d", OSM_API, id))
    provenance(data.frame(time = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), ctx = "meta", box = "", page = id,
                          bbox = "", status = r$status, secs = round(r$secs, 2), points = NA, timed = NA))
    if(r$status %in% c(429, 509)) stop("the API asks to slow down (", r$status, ")")
    if(r$status != 200) return(NA_character_)
    writeBin(r$body, f)
  }
  x <- tryCatch(xml2::read_xml(f), error = function(e) NULL)
  if(is.null(x)) return(NA_character_)
  paste(xml2::xml_text(xml2::xml_find_all(x, "//tag")), collapse = " ")
}

stageTraces <- function(){
  for(ctx in intersect(OSM_ONLY, OSM_CTXS)){
    f <- osmPath("traces", paste0(ctx, ".rds"))
    if(file.exists(f)) next
    if(!dir.exists(osmRaw("pages", ctx))) stop(ctx, ": not crawled")
    t0 <- proc.time()[["elapsed"]]
    rp <- readPages(ctx)
    pc <- rp$pieces; pts <- rp$points
    xy <- sf::sf_project("EPSG:4326", "EPSG:2056", cbind(pts$lon, pts$lat))
    pts$x <- xy[, 1]; pts$y <- xy[, 2]
    pc$text <- paste(pc$name, pc$desc)
    pc$vehicle <- grepl(OSM_VEHICLE, tolower(pc$text), perl = TRUE)
    pc$trace <- chainPieces(pc, pts)
    #the traces of non-vehicle pieces; a trace is a vehicle's when any piece says so
    veh <- tapply(pc$vehicle, pc$trace, any)
    keepT <- names(veh)[!veh]
    ip <- pts$piece %in% pc$piece[pc$trace %in% keepT]
    tr <- split(pts[ip, ], pc$trace[match(pts$piece[ip], pc$piece)])
    #duplicated traces (same start, end, point count): one survives
    key <- vapply(tr, function(p) paste(min(p$t), max(p$t), nrow(p)), "")
    tr <- tr[!duplicated(key)]
    segs <- list(); rows <- list()
    for(nm in names(tr)){
      s <- segmentTrace(tr[[nm]])
      if(!length(s)) next
      first <- pc[match(nm, pc$trace), ]
      for(k in seq_along(s)){
        id <- sprintf("%s_%d", nm, k)
        segs[[id]] <- s[[k]]$pts
        rows[[id]] <- data.frame(seg = id, trace = nm, outing = sprintf("%s_o%d", nm, s[[k]]$outing),
                                 traceId = first$traceId, user = first$user,
                                 text = first$text, year = as.integer(format(as.POSIXct(s[[k]]$pts$t[1], origin = "1970-01-01", tz = "UTC"), "%Y")),
                                 len = s[[k]]$len, dur = s[[k]]$dur, vMed = s[[k]]$vMed, v85 = s[[k]]$v85, v95 = s[[k]]$v95,
                                 stringsAsFactors = FALSE)
      }
    }
    sg <- do.call(rbind, rows)
    if(is.null(sg)) sg <- data.frame(seg = character(0))
    saveRDS(list(ctx = ctx, segs = sg, pts = segs,
                 summary = c(files = rp$files, pieces = nrow(pc), points = nrow(pts), vehiclePieces = sum(pc$vehicle),
                             traces = length(unique(pc$trace)), keptTraces = length(tr), segments = nrow(sg))), f)
    logf("traces %s: %d pages, %d pieces (%d points), %d vehicle pieces by name; %d traces, %d after vehicles and duplicates; %d segments; %.0f s",
         ctx, rp$files, nrow(pc), nrow(pts), sum(pc$vehicle), length(unique(pc$trace)), length(tr), nrow(sg), proc.time()[["elapsed"]] - t0)
  }
  stageClassify()
}

#the classes of every area's segments (rules in the header); the keyword check
stageClassify <- function(){
  all <- do.call(rbind, lapply(intersect(OSM_ONLY, OSM_CTXS), function(ctx){
    f <- osmPath("traces", paste0(ctx, ".rds"))
    if(!file.exists(f)) return(NULL)
    s <- readRDS(f)$segs
    if(!nrow(s)) return(NULL)
    s$ctx <- ctx; s
  }))
  all$speed <- speedClass(all$vMed, all$v85, all$v95)
  all$key <- keyClass(all$text)
  #the tags of identifiable traces that speed and name leave open, or in the run/bike overlap
  ask <- which(!is.na(all$traceId) & is.na(all$key) & (is.na(all$speed) | (all$vMed >= 12 & all$vMed <= 13.5)))
  ids <- unique(all$traceId[ask])
  logf("classify: tags of %d identifiable traces", length(ids))
  if(length(ids)){
    tags <- vapply(ids, traceTags, "")
    all$key[ask] <- keyClass(tags)[match(all$traceId[ask], ids)]
  }
  all$class <- ifelse(!is.na(all$key), all$key, all$speed)
  #synthetic tracks: drawn routes with made-up times move at one constant speed (140
  #anonymous 2009 segments at 10.0 km/h in the 3-area test, two thirds of its "run")
  all$synthetic <- all$v95 / all$vMed < 1.05
  all$class[all$synthetic] <- NA
  lab <- !is.na(all$key) & !is.na(all$speed) & !all$synthetic
  conf <- table(keyword = all$key[!is.na(all$key)], speed = addNA(factor(all$speed[!is.na(all$key)], levels = OSM3)))
  agree <- if(any(lab)) mean(all$key[lab] == all$speed[lab]) else NA_real_
  saveRDS(list(segs = all, agree = agree, conf = conf), osmPath("classes.rds"))
  logf("classify: %d synthetic (constant-speed) segments dropped", sum(all$synthetic))
  logf("classify: %d segments; classes %s; keyword-labelled %d, of which the speed class agrees on %d (%.0f %%, rule >= 75 %%)",
       nrow(all), paste(sprintf("%s %d", names(table(all$class, useNA = "ifany")), table(all$class, useNA = "ifany")), collapse = ", "),
       sum(!is.na(all$key)), sum(all$key[lab] == all$speed[lab]), 100 * agree)
  print(conf)
  invisible(all)
}

#### stage match: segments on the edges; inventory; selection ####

OSM_BUF_M <- 15
OSM_COVER <- 0.5
OSM_USER_CAP <- 2

#the cells of an area and the length-weighted edge -> cell aggregation (as cellMeans)
cellAgg <- function(ctx, ea){
  inVal({ aoi <- readRDS(aoiFile(ctx)); sv <- readRDS(surveyFile(ctx)); edg <- readRDS(edgesFile(ctx))
          G <- buildGrid(ctx, aoi, sv, edg) })
  ec <- edg$edgeCells
  er <- match(ec$edgeID, ea$edgeID)
  baseCell <- if(terra::ncell(G$base) == terra::ncell(G$t100)) ec$cell else
    terra::cellFromXY(G$base, terra::xyFromCell(G$t100, ec$cell))
  len <- ea$SHAPE_Leng[er]
  keep <- !is.na(baseCell) & !is.na(er)
  cells <- sort(unique(baseCell[keep]))
  inMask <- G$mBase[cells] & !(cells %in% sv$lakeCells)
  cells <- cells[inMask]
  k <- keep & baseCell %in% cells
  A <- Matrix::sparseMatrix(i = match(baseCell[k], cells), j = er[k], x = len[k], dims = c(length(cells), length(ea$edgeID)))
  A <- Matrix::Diagonal(x = 1 / Matrix::rowSums(A)) %*% A
  list(cells = cells, A = A, G = G)
}
#per-edge values (edges x groups) -> the cells' length-weighted means
cellTargets <- function(ag, E){ out <- as.matrix(ag$A %*% E); colnames(out) <- colnames(E); out }

stageMatch <- function(){
  cl <- readRDS(osmPath("classes.rds"))$segs
  inv <- list()
  for(ctx in intersect(OSM_ONLY, OSM_CTXS)){
    f <- osmPath("match", paste0(ctx, ".rds"))
    #a stored match is kept while it holds every segment classed now (a segment classed
    #no longer, e.g. a synthetic one, only drops out of the counts)
    if(file.exists(f) && !all(cl$seg[cl$ctx == ctx & !is.na(cl$class)] %in% readRDS(f)$segs$seg)) unlink(f)
    if(!file.exists(f)){
      t0 <- proc.time()[["elapsed"]]
      tr <- readRDS(osmPath("traces", paste0(ctx, ".rds")))
      s <- cl[cl$ctx == ctx & !is.na(cl$class), ]
      net <- inVal(readRDS(netFile(ctx)))$net
      ea <- igraph::edge_attr(net)
      eg <- sf::st_sf(e = seq_along(ea$edgeID), geometry = sf::st_transform(sf::st_zm(ea$SHAPE), 2056))
      elen <- as.numeric(sf::st_length(eg))
      M <- Matrix::sparseMatrix(i = integer(0), j = integer(0), x = numeric(0), dims = c(nrow(eg), nrow(s)))
      if(nrow(s)){
        lines <- sf::st_sfc(lapply(s$seg, function(id){ p <- tr$pts[[id]]; sf::st_linestring(cbind(p$x, p$y)) }), crs = 2056)
        bufs <- sf::st_sf(k = seq_len(nrow(s)), geometry = sf::st_buffer(lines, OSM_BUF_M, nQuadSegs = 4))
        ix <- suppressWarnings(sf::st_intersection(eg, bufs))
        cov <- as.numeric(sf::st_length(ix)) / pmax(elen[ix$e], 1e-6)
        ok <- cov >= OSM_COVER
        M <- Matrix::sparseMatrix(i = ix$e[ok], j = ix$k[ok], x = 1, dims = c(nrow(eg), nrow(s)))
        M@x[] <- 1
      }
      saveRDS(list(ctx = ctx, edgeID = ea$edgeID, segs = s, M = M), f)
      logf("match %s: %d classed segments on %d edges, %d edge matches, %.0f s", ctx, nrow(s), nrow(eg), length(M@x),
           proc.time()[["elapsed"]] - t0)
    }
    m <- readMatch(ctx)
    #the inventory: inside the areas (edges with metres in an area)
    edg <- inVal(readRDS(edgesFile(ctx)))
    inA <- m$edgeID %in% edg$edgeAoi$edgeID
    lenA <- tapply(edg$edgeAoi$len, edg$edgeAoi$edgeID, sum)[as.character(m$edgeID)]; lenA[is.na(lenA)] <- 0
    s <- m$segs
    onA <- if(ncol(m$M)) Matrix::colSums(m$M[inA, , drop = FALSE]) > 0 & !is.na(s$class) else logical(0)
    m$M <- m$M[, !is.na(s$class), drop = FALSE]
    row <- data.frame(ctx = ctx, segments = nrow(s), inAreas = sum(onA),
                      outings = length(unique(s$outing[onA])), users = length(unique(s$user[onA & !is.na(s$user)])),
                      anonymous = length(unique(s$trace[onA & is.na(s$user)])),
                      topUserShare = if(any(onA & !is.na(s$user))) max(table(s$user[onA])) / sum(onA) else NA_real_,
                      yearMin = if(any(onA)) min(s$year[onA]) else NA, yearMed = if(any(onA)) stats::median(s$year[onA]) else NA,
                      areaPathKm = sum(lenA) / 1000,
                      #segment passes per metre of the areas' paths
                      passesPerPath = if(ncol(m$M)) sum(as.vector(Matrix::rowSums(m$M)) * lenA) / max(sum(lenA), 1) else 0)
    for(g in OSM3) row[[paste0("outings_", g)]] <- length(unique(s$outing[onA & s$class == g]))
    inv[[ctx]] <- row
  }
  inv <- do.call(rbind, inv)
  #the selection (set in advance): >= 100 recreational outings in the areas, the 6 with
  #the most passes per path metre; fewer than 3 = stop
  inv$pass <- inv$outings >= 100
  inv$rank <- NA_integer_
  o <- order(-inv$passesPerPath); o <- o[inv$pass[o]]
  inv$rank[o] <- seq_along(o)
  inv$selected <- !is.na(inv$rank) & inv$rank <= 6
  utils::write.csv(inv, osmPath("inventory.csv"), row.names = FALSE)
  logf("inventory (outings = distinct recreational outings on the areas' edges; rule: >= 100, the 6 densest kept):")
  print(inv, digits = 3, row.names = FALSE)
  if(sum(inv$selected) < 3) logf("STOP: only %d area(s) pass the selection rule - OSM is too thin for a pooled fit", sum(inv$selected))
  invisible(inv)
}
selectedAreas <- function(){
  sel <- envList("VFT_OSM_SELECT", character(0))
  if(length(sel)){
    if(!all(sel %in% OSM_CTXS)) stop("VFT_OSM_SELECT: unknown area(s) ", paste(setdiff(sel, OSM_CTXS), collapse = ", "))
    return(sel)
  }
  inv <- utils::read.csv(osmPath("inventory.csv"))
  sel <- inv$ctx[inv$selected]
  if(length(sel) < 3) stop("fewer than 3 areas pass the selection rule")
  sel
}
#a stored match with the segments' current classes (NA = no longer counted)
readMatch <- function(ctx){
  m <- readRDS(osmPath("match", paste0(ctx, ".rds")))
  cl <- readRDS(osmPath("classes.rds"))$segs
  m$segs$class <- cl$class[match(m$segs$seg, cl$seg)]
  m
}

#the per-edge counts of a set of segments by group: per outing once, per user at most
#OSM_USER_CAP (an anonymous trace is its own user)
edgeCounts <- function(m, keepSeg = rep(TRUE, nrow(m$segs)), groups = OSM3){
  s <- m$segs
  usr <- ifelse(is.na(s$user), paste0("anon:", s$trace), paste0("u:", s$user))
  sapply(groups, function(g){
    k <- which(keepSeg & !is.na(s$class) & (if(identical(g, "all")) TRUE else s$class == g))
    if(!length(k)) return(numeric(nrow(m$M)))
    #edges x outings (any segment of the outing), then x users, capped
    tr <- factor(s$outing[k])
    T <- m$M[, k, drop = FALSE] %*% Matrix::sparseMatrix(i = seq_along(k), j = as.integer(tr), x = 1)
    T@x[] <- pmin(T@x, 1)
    uOf <- usr[k][match(levels(tr), s$outing[k])]
    uf <- factor(uOf)
    U <- T %*% Matrix::sparseMatrix(i = seq_along(uf), j = as.integer(uf), x = 1)
    U@x[] <- pmin(U@x, OSM_USER_CAP)
    as.vector(Matrix::rowSums(U))
  })
}

#### stage gate ####

stageGate <- function(){
  sel <- selectedAreas()
  ms <- lapply(stats::setNames(sel, sel), readMatch)
  ag <- lapply(stats::setNames(sel, sel), function(ctx){
    ea <- igraph::edge_attr(inVal(readRDS(netFile(ctx)))$net)
    stopifnot(identical(ea$edgeID, ms[[ctx]]$edgeID))
    cellAgg(ctx, ea)
  })
  groups <- c(OSM3, "all")
  set.seed(26)
  rel <- do.call(rbind, lapply(1:20, function(b){
    half <- lapply(sel, function(ctx){
      s <- ms[[ctx]]$segs
      usr <- ifelse(is.na(s$user), paste0("anon:", s$trace), paste0("u:", s$user))
      u <- unique(usr); h <- stats::setNames(sample(rep_len(1:2, length(u))), u)
      h[usr]
    }); names(half) <- sel
    tg <- function(k) do.call(rbind, lapply(sel, function(ctx)
      cellTargets(ag[[ctx]], edgeCounts(ms[[ctx]], half[[ctx]] == k, groups))))
    a <- tg(1); bb <- tg(2)
    data.frame(split = b, group = groups, spearman = vapply(groups, function(g) suppressWarnings(stats::cor(a[, g], bb[, g], method = "spearman")), 1))
  }))
  res <- stats::aggregate(spearman ~ group, rel, stats::median)
  full <- do.call(rbind, lapply(sel, function(ctx) cellTargets(ag[[ctx]], edgeCounts(ms[[ctx]], groups = groups))))
  res$cellsWithTraces <- vapply(res$group, function(g) mean(full[, g] > 0), 1)
  res$kept <- res$spearman >= 0.4
  res <- res[match(groups, res$group), ]
  utils::write.csv(res, osmPath("gate.csv"), row.names = FALSE)
  logf("gate over %s (%d cells): split-half Spearman, median of 20 splits (rule >= 0.4):", paste(sel, collapse = ", "), nrow(full))
  print(res, digits = 3, row.names = FALSE)
  cl <- readRDS(osmPath("classes.rds"))
  if(isTRUE(cl$agree < 0.75)) logf("gate: the speed classes agree with the keywords on %.0f %% only (< 75 %%): shared layer only", 100 * cl$agree)
  invisible(res)
}
#the groups fitted: those passing the gate (and the keyword check); none = the shared one
fittedGroups <- function(){
  ov <- envList("VFT_OSM_GROUPS", character(0))
  if(length(ov)){
    if(!all(ov %in% OSM3)) stop("VFT_OSM_GROUPS: groups are ", paste(OSM3, collapse = ", "))
    logf("fit: groups %s by VFT_OSM_GROUPS, whatever the gate says", paste(ov, collapse = ", "))
    return(ov)
  }
  g <- utils::read.csv(osmPath("gate.csv"))
  if(!isTRUE(g$kept[g$group == "all"])) stop("the gate failed even for all recreational traces: OSM is insufficient")
  if(isTRUE(readRDS(osmPath("classes.rds"))$agree < 0.75)) return(character(0))
  intersect(OSM3, g$group[g$kept])
}

#### stage extract ####

OSM_NR <- 20L
#tailor_duln.R's stageLayers / stageExtract read today's files through the global
#layersFile / extractFile, which tailor_duln_elements.R redefined: run them with today's
withToday <- function(fn) { environment(fn) <- list2env(list(layersFile = layersFileToday, extractFile = extractFileToday),
                                                        parent = globalenv()); fn }
nrFile <- function(ctx) osmPath("nr", paste0(ctx, ".rds"))
stageExtractOsm <- function(){
  sel <- selectedAreas()
  for(ctx in sel){
    inVal({
      withToday(stageLayers)(BOX_OF[[ctx]])
      stageLayers2(BOX_OF[[ctx]])
      withToday(stageExtract)(ctx)
      stageExtract2(ctx)
    })
  }
  #the stair measures: simTyped() tallies the metres on stairs per type
  oc <- CONTEXTS; CONTEXTS <<- sel; on.exit(CONTEXTS <<- oc, add = TRUE)
  inVal(stageStairs())
  CONTEXTS <<- oc
  #Nearby-Random per type (the offset), OSM_NR reps per area
  todo <- sel[!file.exists(vapply(sel, nrFile, ""))]
  if(length(todo)){
    g <- data.frame(ctx = rep(todo, each = OSM_NR), type = "nearbyRandom", rep = rep(seq_len(OSM_NR), length(todo)),
                    vdir = OSM_VAL, typeArea = FALSE, stringsAsFactors = FALSE)
    res <- runGrid(g, "nearbyRandom")
    for(ctx in todo) saveRDS(list(grid = g[g$ctx == ctx, ], res = res[g$ctx == ctx]), nrFile(ctx))
  }
}

#### stage fit ####

#the selected areas stacked: virtual cells 1..N; targets and Nearby-Random per group,
#the latter scaled to the area's mean target (the offset takes the area's level)
poolAreas <- function(groups){
  sel <- selectedAreas()
  parts <- lapply(sel, function(ctx){
    ex <- readRDS(extractFile(ctx))
    m <- readMatch(ctx)
    ea <- igraph::edge_attr(inVal(readRDS(netFile(ctx)))$net)
    ag <- cellAgg(ctx, ea)
    if(!identical(ag$cells, ex$cells)) stop(ctx, ": the cells differ from the extract's")
    y <- cellTargets(ag, edgeCounts(m, groups = groups))
    nr <- readRDS(nrFile(ctx))
    ok <- vapply(nr$res, function(r) identical(r$status, "done"), TRUE)
    if(!all(ok)) stop(ctx, ": ", sum(!ok), " Nearby-Random runs failed")
    nrT <- lapply(stats::setNames(TYPES7, TYPES7), function(k) typeBase(nr$res, ag$G, k)[ex$cells])
    nrG <- sapply(groups, function(g){
      ty <- if(identical(g, "all")) TYPES7 else names(OSM_OF)[OSM_OF == g]
      v <- Reduce(`+`, nrT[ty])
      v / mean(v) * max(mean(y[, g]), 1e-3 * mean(y))
    })
    list(ctx = ctx, ex = ex, y = y, nr = matrix(nrG, ncol = length(groups), dimnames = list(NULL, groups)))
  })
  n <- vapply(parts, function(p) length(p$ex$cells), 1L)
  ex <- list(cells = seq_len(sum(n)), X = do.call(rbind, lapply(parts, function(p) p$ex$X)),
             logHa = unlist(lapply(parts, function(p) p$ex$logHa)), xy = do.call(rbind, lapply(parts, function(p) p$ex$xy)),
             area = rep(sel, n))
  y <- do.call(rbind, lapply(parts, `[[`, "y")); nr <- do.call(rbind, lapply(parts, `[[`, "nr"))
  list(ex = ex, sty = list(base = lapply(stats::setNames(groups, groups), function(g) y[, g])),
       nr = lapply(stats::setNames(groups, groups), function(g) nr[, g]), sel = sel)
}

stageFitOsm <- function(){
  groups <- fittedGroups()
  fitGroups <- if(length(groups)) groups else "all"
  pl <- poolAreas(fitGroups)
  logf("fit: %d areas, %d cells, groups %s", length(pl$sel), length(pl$ex$cells), paste(fitGroups, collapse = ", "))
  #the types of a group that failed the gate take the shared weights (a zero difference)
  actOf <- OSM_OF; actOf[!actOf %in% fitGroups] <- "shared"
  fits <- list()
  for(signed in c(FALSE, TRUE)){
    d <- if(signed) file.path(OSM_FIT, "signed") else OSM_FIT
    fd <- file.path(d, "fit_types.rds"); key <- if(signed) "signed" else "free"
    if(file.exists(fd)){ fits[[key]] <- readRDS(fd); next }
    b <- if(signed) list(pos = SIGN_POS, neg = SIGN_NEG) else NULL
    all <- fitTypes(pl$ex, pl$sty, pl$nr, paste0("OSM traces, ", length(pl$sel), " areas", if(signed) ", signed" else ""),
                    types = fitGroups, target = "counts", bounds = b)
    all$dev$shared <- list(delta = stats::setNames(rep(0, length(all$ident)), all$ident), lambda = NA, cv = NA, cv0 = NA)
    all$actOf <- actOf; all$sameAs <- OSM_SAME
    #no evaluation site is in the fit: noGP is the same
    res <- list(all = all, noGP = all, nrMaps = pl$nr, areas = pl$sel, groups = fitGroups)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    saveRDS(res, fd)
    #apply_duln_variant.R copies this one beside fit_types.rds
    file.copy(file.path(TD, "types", "survey_types.csv"), file.path(d, "survey_types.csv"), overwrite = TRUE)
    fits[[key]] <- res
  }
  free <- fits$free$all; sgn <- fits$signed$all
  #the weights in DULN units (scaled on Glatt's edges as every variant: element geometry
  #only, no usage), against the xlsx, the survey fit (E2) and Strava (S5E)
  exG <- readRDS(extractFile("glatt"))
  tw <- typeWeights(free, exG); tws <- typeWeights(sgn, exG)
  ts <- typeWeights(readRDS(file.path(TD, "elements", "fit_types.rds"))$all, exG)
  tst <- typeWeights(readRDS(file.path(TD, "strava", "clean", "fit_types.rds"))$all, exG)
  els <- ELEMENTS_NEW()
  vs <- data.frame(element = els, xlsxEq_walkNat = unname(xlsxNew("walkNat")[els]), survey_E2 = unname(ts$shared[els]),
                   strava_S5E = unname(tst$shared[els]), osm_shared = unname(tw$shared[els]),
                   osm_shared_signed = unname(tws$shared[els]), identified = els %in% free$ident)
  for(g in fitGroups) if(g != "all") vs[[paste0("osm_", g)]] <- unname(tw$perType[[ACT_COL[[g]]]][els])
  utils::write.csv(vs, fitPath("osm_weights.csv"), row.names = FALSE)
  id <- vs$identified
  agree <- function(a, b) c(r = stats::cor(a[id], b[id]), sign = mean(sign(a[id]) == sign(b[id])))
  sm <- rbind(`OSM vs survey (E2)` = agree(vs$osm_shared, vs$survey_E2), `OSM vs Strava (S5E)` = agree(vs$osm_shared, vs$strava_S5E),
              `OSM vs xlsx` = agree(vs$osm_shared, vs$xlsxEq_walkNat), `Strava vs survey` = agree(vs$strava_S5E, vs$survey_E2))
  logf("shared weights over the %d elements the OSM fit identifies (Pearson r, share of equal signs):", sum(id))
  print(sm, digits = 3)
  print(vs[order(-abs(vs$osm_shared)), ], digits = 2, row.names = FALSE)
  #CV
  cvt <- rbind(data.frame(fit = "free", group = "shared", cv = min(free$cvShared)),
               data.frame(fit = "signed", group = "shared", cv = sgn$cvSharedBox),
               do.call(rbind, lapply(fitGroups, function(g) rbind(
                 data.frame(fit = "free", group = g, cv = free$dev[[g]]$cv),
                 data.frame(fit = "free", group = paste0(g, " (shared weights)"), cv = free$dev[[g]]$cv0)))))
  utils::write.csv(cvt, fitPath("osm_cv.csv"), row.names = FALSE)
  logf("cross-validated deviance (2 km blocks):"); print(cvt, digits = 4, row.names = FALSE)
  #leave one area out, all groups summed: on the held-out area, Nearby-Random x
  #exp(c X w) with c fitted there (one scale per weight set, the same freedom for each),
  #w the shared OSM fit without that area, the xlsx (Nature walkers), E2 and S5E; the
  #share of Nearby-Random's deviance each removes, and the Spearman of its fit
  W0 <- list(xlsx = xlsxNew("walkNat")[els], E2 = ts$shared[els], S5E = tst$shared[els])
  lo <- do.call(rbind, lapply(pl$sel, function(a){
    tr <- pl$ex$area != a; te <- which(!tr)
    sub <- list(cells = seq_len(sum(tr)), X = pl$ex$X[tr, , drop = FALSE], logHa = pl$ex$logHa[tr], xy = pl$ex$xy[tr, , drop = FALSE])
    sty <- list(base = lapply(pl$sty$base, function(v) v[tr])); nr <- lapply(pl$nr, function(v) v[tr])
    ft <- fitTypes(sub, sty, nr, paste("without", a), types = fitGroups, target = "counts")
    wO <- stats::setNames(numeric(length(els)), els); wO[ft$ident] <- ft$betaShared[ft$ident]
    W <- c(list(OSM = wO), W0)
    y <- Reduce(`+`, lapply(pl$sty$base, function(v) v[te])); n0 <- Reduce(`+`, lapply(pl$nr, function(v) v[te]))
    off <- log(pmax(n0, 0.5 * min(n0[n0 > 0])))
    m0 <- stats::glm(y ~ 1, offset = off, family = stats::quasipoisson())
    row <- data.frame(heldOut = a, cells = length(te), outingCells = sum(y > 0),
                      NR_spearman = stats::cor(y, stats::fitted(m0), method = "spearman"))
    for(k in names(W)){
      s <- drop(pl$ex$X[te, els, drop = FALSE] %*% W[[k]]); s <- (s - mean(s)) / stats::sd(s)
      m <- stats::glm(y ~ s, offset = off, family = stats::quasipoisson())
      row[[paste0(k, "_devGain")]] <- 1 - stats::deviance(m) / stats::deviance(m0)
      row[[paste0(k, "_spearman")]] <- stats::cor(y, stats::fitted(m), method = "spearman")
    }
    row
  }))
  utils::write.csv(lo, fitPath("osm_leave_one_area_out.csv"), row.names = FALSE)
  logf("leave one area out (all groups summed): deviance removed beyond Nearby-Random and Spearman, per weight set:")
  print(lo, digits = 3, row.names = FALSE)
  invisible(fits)
}

#### the variants (built by data-raw/apply_duln_variant.R) ####

BIKE_STAIRS <- list(bikeSport = c(stairs = -10), ebikeNat = c(stairs = -10))
TY_VARIANTS <- list(
  O3E_s0.5  = list(kind = "types", typeArea = FALSE, manual = BIKE_STAIRS, postNet = stairsPostNet),
  OSE_s0.5  = list(kind = "shared", typeArea = FALSE, postNet = stairsPostNet),
  O3Ec_s0.5 = list(kind = "types", typeArea = FALSE, fit = "signed", manual = BIKE_STAIRS, postNet = stairsPostNet))

#### run ####

if(!IS_OSM_WORKER){
  t00 <- proc.time()[["elapsed"]]
  logf("OSM traces: %d candidate areas (%s); stages %s", length(OSM_CTXS), paste(OSM_CTXS, collapse = ", "), paste(OSM_STAGES, collapse = ","))
  if("contexts" %in% OSM_STAGES) stageContexts()
  if("crawl" %in% OSM_STAGES) stageCrawl()
  if("traces" %in% OSM_STAGES) stageTraces()
  if("classify" %in% OSM_STAGES) stageClassify()
  if("match" %in% OSM_STAGES) stageMatch()
  if("gate" %in% OSM_STAGES) stageGate()
  if("extract" %in% OSM_STAGES) stageExtractOsm()
  if("fit" %in% OSM_STAGES) stageFitOsm()
  if("typed" %in% OSM_STAGES) stageTypedAct()
  if("score" %in% OSM_STAGES) stageScoreAct()
  logf("tailor_duln_osm: done in %.1f min, output in %s", (proc.time()[["elapsed"]] - t00) / 60, OSM)
}
