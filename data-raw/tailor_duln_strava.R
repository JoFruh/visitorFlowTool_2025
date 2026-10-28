#### DULN per STRAVA group (5) from the Strava global heatmap at Glatt ####
#
# data-raw/tailor_duln_activities.R fitted the DULN element weights per activity to the
# Glatt survey. This fits them to Strava's sport-type heatmap layers instead (the user's
# choice, 2026-10-21): no survey enters the fit, so all three surveys - Glatt's too -
# become independent tests. Five groups, one DULN layer each:
#   nature_walk   Hike               walkNat + dogNat
#   social_walk   Walk               walkSoc + dogProx
#   biker         Ride + GravelRide  bikeSport
#   nature_ebike  EBikeRide          ebikeNat
#   jogger        Run                jogger
# The dog types take their walker group's WHOLE layer (fit$sameAs); bikers and e-bikers
# stay apart, so Ride's pull to roads stays with the bikers. Only cells inside the
# areas are fitted, which drops most commuting.
#
# The heatmap is a relative colour scale (possibly normalised per tile), not counts. The
# target is therefore the normal score of a cell's rank, fitted by tailor_duln_types.R's
# fitTypes(target = "ranks"): a Gaussian ridge with log Nearby-Random per group as a
# free covariate. Ride + GravelRide is the mean of the two layers' normal scores, scored
# again (their counts are unknowable: equal weights).
#
# THE ELEMENTS (VFT_ST_ELEMENTS): "clean" (default) is data-raw/tailor_duln_elements.R's
# set - exclusive path classes width x surface against 2 m natural paths, forest reserve /
# forest / 50 m edge, summits + viewpoints, the large roads' noise - fitted unconstrained
# and held to the expected signs (as E2 / E2c); "today" is the 37 elements of the xlsx
# (the first Strava run, variants S5 / SS). The heat (tiles .. extract) is the same for
# both; the fits of the clean set go to <VFT_ST_OUT>/clean (and clean/signed).
#
# THE TILES come from the user's own browser session: log in at
# strava.com/maps/global-heatmap, pick sport type Walk and the grayscale colour, F12 >
# Network > filter png > right-click a tile > "Copy as cURL" (bash or cmd), and paste it
# into <VFT_ST_RAW>/session.txt. Its cookies are never printed or written elsewhere;
# delete the file after the download. Tiles are fetched one at a time (at most 1 per
# second) and cached, so a rerun resumes; 401/403 stops (the cookies expired: refresh the
# file). The tiles stay local - Strava's terms do not cover this use, which the user
# accepted; ask Strava before publishing anything built on them.
#
# Stages (VFT_ST_STAGES):
#   tiles    probe one tile per sport, then every z14 tile over Glatt's network
#   decode   the colour ramp -> heat 0-1 per pixel; one GeoTIFF (3857) per sport
#   check    georeferencing (heat against the network, shifted) and the seam test that
#            decides global ranks or ranks within each z14 tile (rule set in advance)
#   extract  heat per edge (3x3 focal max, mean along the line) and per comparison cell;
#            the group targets; Strava against the Glatt survey per cell (descriptive)
#   fit      fitTypes(target = "ranks") over the 5 groups; weights, CV, Strava vs survey
#   typed, score   tailor_duln_activities.R's, by the 5 groups, + each group's map
#            against its own Strava layer
#   figures  heat and DULN change per group, weights against the survey and the xlsx
# The variants (stairs x3 for bikers in all) are built on a validation folder's areas by
# data-raw/apply_duln_variant.R with VFT_AV_SCRIPT=tailor_duln_strava.R and
#   clean: S5E_s0.5 per group, SSE_s0.5 shared (VFT_AV_TYSUB=strava/clean), S5Ec_s0.5
#          per group, signed (strava/clean/signed);
#   today: S5_s0.5 per group, SS_s0.5 shared (VFT_ST_ELEMENTS=today, VFT_AV_TYSUB=strava).
#
#   Rscript data-raw/tailor_duln_strava.R
#
# VFT_ST_STAGES   which stages (default tiles, decode, check, extract, fit)
# VFT_ST_ELEMENTS clean (default) or today
# VFT_ST_OUT      output (default <duln_tailored>/strava; the clean fits in its clean/)
# VFT_ST_RAW      tiles, mosaics and the session file (default Local Data/strava_glatt)
# VFT_ST_SESSION  the session file (default <VFT_ST_RAW>/session.txt, or session.txt.txt)
# VFT_ACT_RUNS    the folders of the typed stage, name=path comma list (best=, s5=, ss=)

IS_ST_WORKER <- identical(Sys.getenv("VFT_TY_WORKER"), "1")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
ST_SCRIPT <- file.path(.repo, "data-raw", "tailor_duln_strava.R")
if(!IS_ST_WORKER){
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if(length(f) && grepl("tailor_duln_strava", f[1])) ST_SCRIPT <- normalizePath(f[1], winslash = "/")
}
.stStages <- Sys.getenv("VFT_ST_STAGES", "tiles,decode,check,extract,fit")
ST_ELEMENTS <- Sys.getenv("VFT_ST_ELEMENTS", "clean")
CLEAN <- identical(ST_ELEMENTS, "clean")
if(!CLEAN && !identical(ST_ELEMENTS, "today")) stop("VFT_ST_ELEMENTS is clean or today")
#the clean set: tailor_duln_elements.R swaps layersFile, extractFile, ELEMENTS_NEW and
#xlsxNew (it sources tailor_duln_activities.R). VFT_ST_CHAIN=none: a caller sourced the
#chain already (data-raw/tailor_duln_strava_areas.R, through tailor_duln_osm.R)
Sys.setenv(VFT_ACT_STAGES = "none", VFT_EL_STAGES = "none")
if(!identical(Sys.getenv("VFT_ST_CHAIN"), "none"))
  source(file.path(.repo, "data-raw", if(CLEAN) "tailor_duln_elements.R" else "tailor_duln_activities.R"))
TY_SCRIPT <- ST_SCRIPT

#the heat (tiles .. extract) in ST_BASE, the element set's fits in ST
ST_BASE <- Sys.getenv("VFT_ST_OUT", file.path(TD, "strava"))
ST <- if(CLEAN) file.path(ST_BASE, "clean") else ST_BASE
ST_STAGES <- strsplit(.stStages, ",", fixed = TRUE)[[1]]
ST_RAW <- Sys.getenv("VFT_ST_RAW", file.path(LOCAL, "strava_glatt"))
ST_SESSION <- Sys.getenv("VFT_ST_SESSION", "")
if(!nzchar(ST_SESSION)){
  cand <- file.path(ST_RAW, c("session.txt", "session.txt.txt"))
  ST_SESSION <- if(any(file.exists(cand))) cand[file.exists(cand)][1] else cand[1]
}
ST_Z <- 14
stPath <- function(...){ p <- file.path(ST, ...); dir.create(dirname(p[1]), recursive = TRUE, showWarnings = FALSE); p }
basePath <- function(...){ p <- file.path(ST_BASE, ...); dir.create(dirname(p[1]), recursive = TRUE, showWarnings = FALSE); p }
rawPath <- function(...){ p <- file.path(ST_RAW, ...); dir.create(dirname(p[1]), recursive = TRUE, showWarnings = FALSE); p }
#the activity stages (typed, score) write here
ACT <- ST

#the 5 groups (the user's, 2026-10-21)
STRAVA5 <- c("nature_walk", "social_walk", "biker", "nature_ebike", "jogger")
ST_LABEL <- c(nature_walk = "Nature walkers (Hike)", social_walk = "Social walkers (Walk)",
              biker = "Bikers (Ride + Gravel)", nature_ebike = "E-bikers (E-Bike)", jogger = "Joggers (Run)")
ST_SPORTS <- list(nature_walk = "Hike", social_walk = "Walk", biker = c("Ride", "GravelRide"),
                  nature_ebike = "EBikeRide", jogger = "Run")
SPORTS <- unique(unlist(ST_SPORTS))
ST_OF <- c(walkNat = "nature_walk", dogNat = "nature_walk", walkSoc = "social_walk", dogProx = "social_walk",
           bikeSport = "biker", ebikeNat = "nature_ebike", jogger = "jogger")
#the dog types take their walkers' whole layer
ST_SAME <- c(dogNat = "walkNat", dogProx = "walkSoc")
#tailor_duln_activities.R's typed and score stages, by the 5 groups
ACT4 <- STRAVA5; ACT_OF <- ST_OF; ACT_LABEL <- ST_LABEL
ACT_COL <- c(nature_walk = "walkNat", social_walk = "walkSoc", biker = "bikeSport", nature_ebike = "ebikeNat", jogger = "jogger")

#### stage tiles ####

#The session file: a tile request copied as cURL (bash or cmd). Returns the URL's parts
#and the headers to send; nothing of it is ever printed.
readSession <- function(){
  if(!file.exists(ST_SESSION)) stop("no session file ", ST_SESSION, " - see the header")
  s <- gsub("[\r^]", "", paste(readLines(ST_SESSION, warn = FALSE), collapse = " "))
  url <- regmatches(s, regexpr("https://[^\"' ]+\\.png[^\"' ]*", s))
  if(length(url) != 1) stop("the session file holds no tile URL")
  hd <- regmatches(s, gregexpr("(-H|--header) [\"'][^\"']+[\"']", s))[[1]]
  hd <- sub("[\"']$", "", sub("^(-H|--header) [\"']", "", hd))
  nm <- sub(":.*$", "", hd); val <- sub("^[^:]+:\\s*", "", hd)
  ck <- regmatches(s, regexpr("(-b|--cookie) [\"'][^\"']+", s))
  if(length(ck)){ nm <- c(nm, "Cookie"); val <- c(val, sub("^(-b|--cookie) [\"']", "", ck)) }
  keep <- tolower(nm) %in% c("cookie", "user-agent", "referer", "origin", "accept", "accept-language")
  h <- stats::setNames(val[keep], nm[keep])
  if(!any(tolower(names(h)) == "cookie")) stop("the session file holds no cookies")
  m <- regmatches(url, regexec("^(.*/)sport_([A-Za-z]+)(/[^/]+/)([0-9]+)/([0-9]+)/([0-9]+)(@2x)?(\\.png.*)$", url))[[1]]
  if(!length(m)) stop("the tile URL is not .../sport_<Sport>/<colour>/z/x/y.png")
  ex <- regmatches(s, regexpr("_strava_CloudFront-Expires=[0-9]+", s))
  expires <- if(length(ex)) as.POSIXct(as.numeric(sub(".*=", "", ex)) / 1000, origin = "1970-01-01") else NA
  list(pre = m[2], sport = m[3], colour = m[4], z = as.integer(m[5]), x = as.integer(m[6]), y = as.integer(m[7]),
       hi = m[8], post = m[9], headers = h, expires = expires)
}
tileUrl <- function(ss, sport, z, x, y) paste0(ss$pre, "sport_", sport, ss$colour, z, "/", x, "/", y, ss$hi, ss$post)
lon2x <- function(lon, z) floor((lon + 180) / 360 * 2^z)
lat2y <- function(lat, z){ r <- lat * pi / 180; floor((1 - log(tan(r) + 1 / cos(r)) / pi) / 2 * 2^z) }
#the tiles over a context's network (shapeLarger; Glatt's unless another is given)
tileRange <- function(z = ST_Z, ctx = "glatt"){
  bb <- sf::st_bbox(sf::st_transform(readRDS(aoiFile(ctx))$shapeLarger, 4326))
  list(x = lon2x(bb[["xmin"]], z):lon2x(bb[["xmax"]], z), y = lat2y(bb[["ymax"]], z):lat2y(bb[["ymin"]], z), z = z)
}
tileFile <- function(sport, z, x, y) file.path(ST_RAW, "tiles", sport, z, x, paste0(y, ".png"))
#a context's own file name: Glatt's as before, "<name>_<ctx>.<ext>" for another
ctxName <- function(name, ctx) if(identical(ctx, "glatt")) name else sub("(\\.[a-z]+)$", paste0("_", ctx, "\\1"), name)
isPng <- function(b) length(b) >= 8 && identical(b[1:8], as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a)))

#one request, at most one per second; the status and the bytes
.lastFetch <- new.env(); .lastFetch$t <- 0
fetchTile <- function(h, url){
  wait <- 1 - (proc.time()[["elapsed"]] - .lastFetch$t)
  if(wait > 0) Sys.sleep(wait)
  .lastFetch$t <- proc.time()[["elapsed"]]
  r <- tryCatch(curl::curl_fetch_memory(url, handle = h),
                error = function(e) list(status_code = -1L, content = raw(0), err = sub("\\?.*", "", conditionMessage(e))))
  list(status = r$status_code, body = r$content, err = r$err)
}
newHandle <- function(ss){
  h <- curl::new_handle()
  curl::handle_setheaders(h, .list = as.list(ss$headers))
  h
}

stageTiles <- function(ctx = "glatt"){
  tr <- tileRange(ctx = ctx)
  g <- expand.grid(y = tr$y, x = tr$x, sport = SPORTS, stringsAsFactors = FALSE)
  g$file <- tileFile(g$sport, tr$z, g$x, g$y)
  have <- function() file.exists(g$file) | file.exists(sub("\\.png$", ".none", g$file))
  if(all(have()) && !identical(Sys.getenv("VFT_ST_PROBE_ONLY"), "1")){
    logf("tiles %s: all %d present, none fetched", ctx, nrow(g)); return(invisible(g))
  }
  ss <- readSession()
  logf("session: sport_%s, colour %s, tile %d/%d/%d%s; cookies valid until %s", ss$sport, gsub("/", "", ss$colour),
       ss$z, ss$x, ss$y, ss$hi, format(ss$expires))
  if(!is.na(ss$expires) && ss$expires < Sys.time() + 600) stop("the session's cookies expire within 10 min - refresh the session file")
  h <- newHandle(ss)
  #the probe: the copied tile in every sport; each must be a PNG and differ from Walk's
  pr <- list(); body <- list()
  for(sp in SPORTS){
    f <- rawPath("probe", paste0(sp, ".png"))
    r <- fetchTile(h, tileUrl(ss, sp, ss$z, ss$x, ss$y))
    ok <- r$status == 200 && isPng(r$body)
    if(ok) writeBin(r$body, f)
    body[[sp]] <- r$body
    pr[[sp]] <- data.frame(sport = sp, status = r$status, bytes = length(r$body), png = isPng(r$body))
  }
  pr <- do.call(rbind, pr)
  pr$sameAsWalk <- vapply(SPORTS, function(sp) sp != "Walk" && identical(body[[sp]], body[["Walk"]]), TRUE)
  print(pr, row.names = FALSE)
  utils::write.csv(pr, basePath("probe.csv"), row.names = FALSE)
  if(any(pr$status %in% c(401, 403))) stop("the tiles refused the session (401/403): refresh the session file")
  if(!all(pr$status == 200 & pr$png) || any(pr$sameAsWalk))
    stop("the probe failed for ", paste(pr$sport[!(pr$status == 200 & pr$png) | pr$sameAsWalk], collapse = ", "),
         " - paste that layer's cURL into the session file")
  if(identical(Sys.getenv("VFT_ST_PROBE_ONLY"), "1")) return(invisible(pr))
  #every tile over the network
  todo <- which(!have())
  logf("tiles %s: z%d, %d x %d per sport, %d sports = %d tiles; %d to fetch (~%.0f min)", ctx, tr$z, length(tr$x), length(tr$y),
       length(SPORTS), nrow(g), length(todo), length(todo) / 60)
  t0 <- proc.time()[["elapsed"]]; nEmpty <- 0L
  for(j in seq_along(todo)){
    i <- todo[j]
    r <- fetchTile(h, tileUrl(ss, g$sport[i], tr$z, g$x[i], g$y[i]))
    if(r$status %in% c(401, 403)) stop(sprintf("tile %s %d/%d refused (%d) after %d tiles: the cookies expired - refresh the session file and rerun",
                                               g$sport[i], g$x[i], g$y[i], r$status, j - 1))
    if(r$status == 429) stop("Strava asks to slow down (429) - stopped; rerun later")
    if(r$status == -1L){
      Sys.sleep(5)
      r <- fetchTile(h, tileUrl(ss, g$sport[i], tr$z, g$x[i], g$y[i]))
      if(r$status == -1L) stop("network error: ", r$err)
    }
    dir.create(dirname(g$file[i]), recursive = TRUE, showWarnings = FALSE)
    if(r$status == 200 && isPng(r$body)) writeBin(r$body, g$file[i]) else if(r$status %in% c(204, 404)){
      file.create(sub("\\.png$", ".none", g$file[i])); nEmpty <- nEmpty + 1L
    } else stop(sprintf("tile %s %d/%d: status %d, %d bytes - stopped", g$sport[i], g$x[i], g$y[i], r$status, length(r$body)))
    if(j %% 100 == 0) logf("tiles: %d / %d, %.1f min", j, length(todo), (proc.time()[["elapsed"]] - t0) / 60)
  }
  ok <- have()
  logf("tiles %s: done, %d of %d present (%d without heat)", ctx, sum(ok), nrow(g), nEmpty)
  if(!all(ok)) stop("tiles missing")
  invisible(g)
}

#### stage decode: grey level -> heat, one mosaic per sport ####

#The grayscale tiles (1024 px at z14 @2x, ~1.6 m) are black where no one went and
#brighter with more activity: the grey level IS the heat. The mosaic keeps the max of
#each 2 x 2 block (~3.2 m), as an 8-bit GeoTIFF in 3857.
#kind "max" the 2 x 2 max (the peak next to a path), "mean" the 2 x 2 mean (keeps the
#spread across a path's width: data-raw/tailor_duln_strava_areas.R's width measure)
mosaicFile <- function(sp, ctx = "glatt", kind = "max"){
  nm <- paste0(sp, "_z", ST_Z, if(identical(kind, "mean")) "_mean", ".tif")
  if(identical(ctx, "glatt")) file.path(ST_RAW, "mosaic", nm) else file.path(ST_RAW, "mosaic", ctx, nm)
}
tileExt <- function(tr){
  W <- 20037508.342789244; s <- 2 * W / 2^tr$z
  terra::ext(-W + min(tr$x) * s, -W + (max(tr$x) + 1) * s, W - (max(tr$y) + 1) * s, W - min(tr$y) * s)
}
stageDecode <- function(ctx = "glatt", kind = "max"){
  tr <- tileRange(ctx = ctx)
  st <- list()
  for(sp in SPORTS){
    f <- mosaicFile(sp, ctx, kind)
    if(!file.exists(f)){
      t0 <- proc.time()[["elapsed"]]
      M <- NULL; h <- NA_integer_
      for(ix in seq_along(tr$x)) for(iy in seq_along(tr$y)){
        p <- tileFile(sp, tr$z, tr$x[ix], tr$y[iy])
        if(!file.exists(p)){
          if(!file.exists(sub("\\.png$", ".none", p))) stop("tile missing: ", sp, " ", tr$x[ix], "/", tr$y[iy])
          next
        }
        a <- png::readPNG(p)
        if(length(dim(a)) == 3){
          if(dim(a)[3] >= 3 && max(abs(a[, , 1] - a[, , 2]), abs(a[, , 2] - a[, , 3])) > 0)
            stop(sp, ": a tile is not grey - pick the grayscale colour")
          a <- if(dim(a)[3] %in% c(2, 4)) a[, , 1] * a[, , dim(a)[3]] else a[, , 1]
        }
        a <- round(a * 255)
        if(is.null(M)){
          h <- nrow(a); stopifnot(h == ncol(a), h %% 2 == 0)
          M <- matrix(0L, length(tr$y) * h / 2, length(tr$x) * h / 2)
        }
        o <- seq(1, h, by = 2); e <- o + 1L
        b <- if(identical(kind, "mean")) round((a[o, o] + a[e, o] + a[o, e] + a[e, e]) / 4) else pmax(a[o, o], a[e, o], a[o, e], a[e, e])
        M[(iy - 1) * h / 2 + seq_len(h / 2), (ix - 1) * h / 2 + seq_len(h / 2)] <- as.integer(b)
      }
      r <- terra::rast(M, extent = tileExt(tr), crs = "EPSG:3857")
      names(r) <- sp
      dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
      terra::writeRaster(r, f, overwrite = TRUE, datatype = "INT1U", gdal = c("COMPRESS=DEFLATE", "TILED=YES"))
      logf("decode %s %s (%s): %d x %d px of %.2f m, %.0f s", ctx, sp, kind, nrow(M), ncol(M), terra::res(r)[1], proc.time()[["elapsed"]] - t0)
    }
    v <- terra::values(terra::rast(f), mat = FALSE)
    nz <- v[v > 0]; top <- max(nz)
    st[[sp]] <- data.frame(sport = sp, cover = mean(v > 0), maxLevel = top, atMax = mean(nz >= top - 2),
                           q50 = stats::median(nz), ksUniform = unname(suppressWarnings(stats::ks.test(nz / top, "punif")$statistic)))
  }
  st <- do.call(rbind, st)
  utils::write.csv(st, basePath(ctxName(if(identical(kind, "mean")) "decode_mean.csv" else "decode.csv", ctx)), row.names = FALSE)
  logf("decode %s (%s): per sport, the share of pixels with heat, the top grey level, the share of heat pixels at it, and the distance of the heat from uniform:", ctx, kind)
  print(st, digits = 3, row.names = FALSE)
  invisible(st)
}

#### stage check: georeferencing, and per-tile normalisation ####

#points along the network's edges (3857), every ~5 m, with their edge
edgePoints <- function(net, n = 200000){
  g <- sf::st_transform(sf::st_zm(igraph::edge_attr(net)$SHAPE), 3857)
  xy <- sf::st_coordinates(sf::st_segmentize(g, 5))
  set.seed(5)
  xy[sort(sample.int(nrow(xy), min(n, nrow(xy)))), c("X", "Y")]
}
#Rules (set before any fit):
#  georeferencing - the mean heat at the network's points, shifted by -4..4 pixels in x
#    and y, must peak within 1 pixel of no shift (else stop);
#  seams - the median |difference| of neighbouring heat pixels (both > 0) across the z14
#    tile seams, against the same within tiles (the pixel pairs half a tile from the
#    seams): a ratio of at most 1.5 for every sport means one normalisation over the
#    area (global ranks); otherwise the heat is ranked within each z14 tile.
stageCheck <- function(ctx = "glatt"){
  f <- basePath(ctxName("check.rds", ctx))
  if(file.exists(f)) return(invisible(readRDS(f)))
  net <- readRDS(netFile(ctx))$net
  pts <- edgePoints(net)
  nTy <- length(tileRange(ctx = ctx)$y)
  geo <- list(); seam <- list()
  for(sp in SPORTS){
    r <- terra::rast(mosaicFile(sp, ctx)); px <- terra::res(r)[1]
    sh <- expand.grid(dx = -4:4, dy = -4:4)
    sh$heat <- vapply(seq_len(nrow(sh)), function(i){
      v <- terra::extract(r, cbind(pts[, "X"] + sh$dx[i] * px, pts[, "Y"] + sh$dy[i] * px))[[1]]
      mean(v, na.rm = TRUE)
    }, 1)
    b <- sh[which.max(sh$heat), ]
    geo[[sp]] <- data.frame(sport = sp, px_m = px, bestDx = b$dx, bestDy = b$dy, heatBest = b$heat,
                            heatNoShift = sh$heat[sh$dx == 0 & sh$dy == 0])
    M <- terra::as.matrix(r, wide = TRUE); ts <- nrow(M) / nTy
    #|v[, c] - v[, c + 1]| over the columns cc and |v[r, ] - v[r + 1, ]| over the rows rr,
    #where both pixels have heat
    pairDiff <- function(cc, rr){
      d1 <- unlist(lapply(cc, function(c){ a <- M[, c]; b <- M[, c + 1]; k <- a > 0 & b > 0; abs(a[k] - b[k]) }))
      d2 <- unlist(lapply(rr, function(r){ a <- M[r, ]; b <- M[r + 1, ]; k <- a > 0 & b > 0; abs(a[k] - b[k]) }))
      as.numeric(c(d1, d2))
    }
    #the seams (a tile's last column / row) and the pairs half a tile away from them
    ds <- pairDiff(seq(ts, ncol(M) - 1, by = ts), seq(ts, nrow(M) - 1, by = ts))
    di <- pairDiff(seq(ts / 2, ncol(M) - 1, by = ts), seq(ts / 2, nrow(M) - 1, by = ts))
    seam[[sp]] <- data.frame(sport = sp, nSeam = length(ds), nInside = length(di), medSeam = stats::median(ds),
                             medInside = stats::median(di), meanSeam = mean(ds), meanInside = mean(di))
  }
  geo <- do.call(rbind, geo); seam <- do.call(rbind, seam)
  seam$ratio <- seam$meanSeam / seam$meanInside
  print(geo, digits = 3, row.names = FALSE); print(seam, digits = 3, row.names = FALSE)
  if(any(abs(geo$bestDx) > 1 | abs(geo$bestDy) > 1)) stop(ctx, ": the heat is off the network by more than a pixel - check the georeferencing")
  scope <- if(all(seam$ratio <= 1.5)) "global" else "tile"
  logf("check %s: georeferencing within a pixel for every sport; seam/inside ratio %s -> ranks %s", ctx,
       paste(sprintf("%s %.2f", seam$sport, seam$ratio), collapse = ", "), if(scope == "global") "over the whole area" else "within each z14 tile")
  res <- list(geo = geo, seam = seam, scope = scope)
  utils::write.csv(merge(geo, seam), basePath(ctxName("check.csv", ctx)), row.names = FALSE)
  saveRDS(res, f)
  invisible(res)
}

#### stage extract: heat per edge and per cell; the group targets ####

#normal scores of v within each group of grp (ranks, ties averaged)
nscore <- function(v, grp){
  out <- v
  for(g in unique(grp)){ i <- which(grp == g); out[i] <- stats::qnorm((rank(v[i]) - 0.5) / length(i)) }
  out
}
#the 5 groups' targets on the given base cells: each sport's cell heat as normal scores
#(over the cells, or per z14 tile), Ride + GravelRide averaged and scored again
groupTarget <- function(se, cells){
  grp <- if(identical(se$scope, "tile")) se$tile[cells] else rep(1L, length(cells))
  sapply(STRAVA5, function(k){
    sp <- ST_SPORTS[[k]]
    z <- sapply(sp, function(s) nscore(se$Hc[[s]][cells], grp))
    if(length(sp) > 1) nscore(rowMeans(z), grp) else drop(z)
  })
}
#a folder's base cells on the extract's grid
onStravaGrid <- function(se, base, cells){
  grid <- terra::unwrap(se$grid)
  if(isTRUE(terra::compareGeom(base, grid, stopOnError = FALSE))) return(cells)
  terra::cellFromXY(grid, terra::xyFromCell(base, cells))
}

#the heat along each edge (edges x sports): the 3 x 3 max (~10 m: GPS tracks scatter
#round the path), its mean along the line (exact_extract on a 0.5 m buffer, as the elements)
edgeHeat <- function(ea, ctx = "glatt"){
  g <- sf::st_transform(sf::st_zm(ea$SHAPE), 3857)
  R <- terra::rast(lapply(SPORTS, function(sp) terra::rast(mosaicFile(sp, ctx))))
  Rf <- terra::focal(R, w = 3, fun = "max", na.rm = TRUE, filename = file.path(tempdir(), paste0("strava_focal_", ctx, ".tif")), overwrite = TRUE)
  names(Rf) <- SPORTS
  H <- as.matrix(exactextractr::exact_extract(Rf, sf::st_buffer(g, 0.5), "mean", progress = FALSE))
  colnames(H) <- SPORTS
  H[!is.finite(H)] <- 0
  H
}

stageExtractStrava <- function(){
  f <- basePath("extract_glatt.rds")
  if(file.exists(f)) return(invisible(readRDS(f)))
  t0 <- proc.time()[["elapsed"]]
  scope <- stageCheck()$scope
  net <- readRDS(netFile("glatt"))$net
  ea <- igraph::edge_attr(net)
  H <- edgeHeat(ea)
  logf("extract: heat on %d edges, %.0f s", nrow(H), proc.time()[["elapsed"]] - t0)
  #the cells: tailor_duln.R's stageExtract() mapping (length-weighted mean over the
  #cell's edges), on every base cell that holds an edge
  aoi <- readRDS(aoiFile("glatt")); sv <- readRDS(surveyFile("glatt")); edg <- readRDS(edgesFile("glatt"))
  G <- buildGrid("glatt", aoi, sv, edg)
  ec <- edg$edgeCells
  er <- match(ec$edgeID, ea$edgeID)
  baseCell <- if(terra::ncell(G$base) == terra::ncell(G$t100)) ec$cell else
    terra::cellFromXY(G$base, terra::xyFromCell(G$t100, ec$cell))
  len <- ea$SHAPE_Leng[er]
  keep <- !is.na(baseCell) & !is.na(er)
  S <- rowsum(H[er[keep], , drop = FALSE] * len[keep], baseCell[keep])
  W <- rowsum(len[keep], baseCell[keep])
  cl <- as.integer(rownames(S))
  Hc <- lapply(stats::setNames(SPORTS, SPORTS), function(sp){ v <- rep(NA_real_, terra::ncell(G$base)); v[cl] <- S[, sp] / W[, 1]; v })
  ex <- readRDS(extractFile("glatt"))
  if(anyNA(Hc[[1]][ex$cells])) stop("comparison cells without heat")
  #each base cell's z14 tile
  ll <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(as.data.frame(terra::xyFromCell(G$base, seq_len(terra::ncell(G$base)))),
                                                         coords = c("x", "y"), crs = terra::crs(G$base)), 4326))
  tile <- paste(lon2x(ll[, 1], ST_Z), lat2y(ll[, 2], ST_Z))
  se <- list(edgeID = ea$edgeID, H = H, Hc = Hc, tile = tile, scope = scope, grid = terra::wrap(terra::rast(G$base)))
  saveRDS(se, f)
  #what the heat looks like on the network: per sport, the edges at the top grey level,
  #and the long edges (>= 200 m) with the most heat, where they lie (LV95)
  mid <- sf::st_coordinates(sf::st_centroid(sf::st_transform(sf::st_zm(ea$SHAPE), 2056)))
  for(sp in c("Walk", "Hike", "Ride", "Run")){
    o <- order(-H[, sp], -ea$SHAPE_Leng); o <- o[ea$SHAPE_Leng[o] >= 200][1:8]
    logf("extract %s: %.1f %% of the edge length at >= 95 %% of the top heat; the hottest long edges at %s", sp,
         100 * sum(ea$SHAPE_Leng[H[, sp] >= 0.95 * max(H[, sp])]) / sum(ea$SHAPE_Leng),
         paste(sprintf("%.0f/%.0f", mid[o, 1] / 1000, mid[o, 2] / 1000), collapse = " "))
  }
  #descriptive only: each group's heat against the Glatt survey routes of its types,
  #the survey's total, and the group's Nearby-Random map (Spearman over the cells)
  sty <- stageSurveyTypes()
  cr <- readRDS(file.path(TD, "types", "capture_runs.rds"))
  Gs <- withOut(RS, buildGrid("glatt", readRDS(aoiFile("glatt")), readRDS(surveyFile("glatt")), readRDS(edgesFile("glatt"))))
  nr5 <- sumBy(lapply(stats::setNames(TYPES7, TYPES7), function(k) typeBase(cr$res[cr$grid$type == "nearbyRandom"], Gs, k)), ST_OF)
  sv5 <- sumBy(sty$base, ST_OF)
  tg <- groupTarget(se, ex$cells)
  tot <- Reduce(`+`, sty$base)[ex$cells]
  sp <- function(a, b) stats::cor(a, b, method = "spearman")
  ds <- do.call(rbind, lapply(STRAVA5, function(k) data.frame(group = k, label = ST_LABEL[[k]],
    routes = sum(sty$table$routes[sty$table$type %in% names(ST_OF)[ST_OF == k]]),
    vsOwnRoutes = sp(tg[, k], sv5[[k]][ex$cells]), vsAllRoutes = sp(tg[, k], tot),
    vsNearbyRandom = sp(tg[, k], nr5[[k]][ex$cells]), ownRoutesVsNR = sp(sv5[[k]][ex$cells], nr5[[k]][ex$cells]))))
  utils::write.csv(ds, basePath("strava_vs_survey_cells.csv"), row.names = FALSE)
  logf("extract: Strava against the Glatt survey per cell (Spearman over the %d comparison cells; descriptive, the fit never sees the survey):",
       length(ex$cells))
  print(ds, digits = 3, row.names = FALSE)
  logf("extract: done, %.0f s", proc.time()[["elapsed"]] - t0)
  invisible(se)
}

#### stage fit ####

#the survey fit on the same element set, and each type's layer from it: today's set the
#best model's (T_shared_s0.5, the delta rule), the clean set E2's (the 4 activities)
SURVEY_FIT <- file.path(TD, if(CLEAN) "elements" else "types", "fit_types.rds")
BIKE_STAIRS <- list(bikeSport = c(stairs = -10), ebikeNat = c(stairs = -10))
withStairs <- function(w, k){ m <- BIKE_STAIRS[[k]]; if(length(m)) w[names(m)] <- m; w }

stageFitStrava <- function(){
  se <- readRDS(basePath("extract_glatt.rds"))
  ex <- readRDS(extractFile("glatt"))
  cr <- readRDS(file.path(TD, "types", "capture_runs.rds"))
  G <- withOut(RS, buildGrid("glatt", readRDS(aoiFile("glatt")), readRDS(surveyFile("glatt")), readRDS(edgesFile("glatt"))))
  nrRuns <- cr$res[cr$grid$type == "nearbyRandom"]
  nr5 <- sumBy(lapply(stats::setNames(TYPES7, TYPES7), function(k) typeBase(nrRuns, G, k)), ST_OF)
  tg <- groupTarget(se, ex$cells)
  nb <- length(nr5[[1]])
  sty <- list(base = lapply(stats::setNames(STRAVA5, STRAVA5), function(k){ v <- numeric(nb); v[ex$cells] <- tg[, k]; v }))
  #unconstrained, and (clean set) held to tailor_duln_elements.R's expected signs
  fits <- list()
  for(signed in if(CLEAN) c(FALSE, TRUE) else FALSE){
    d <- if(signed) file.path(ST, "signed") else ST
    fd <- file.path(d, "fit_types.rds"); key <- if(signed) "signed" else "free"
    if(file.exists(fd)){ fits[[key]] <- readRDS(fd); next }
    b <- if(signed) list(pos = SIGN_POS, neg = SIGN_NEG) else NULL
    lab <- paste0("Strava groups, ", if(CLEAN) "clean elements" else "today's elements", if(signed) ", signed" else "")
    #Strava holds no survey: GlattPark's ground needs no hold-out, so noGP is the same fit
    all <- fitTypes(ex, sty, nr5, lab, types = STRAVA5, target = "ranks", bounds = b)
    all$actOf <- ST_OF; all$sameAs <- ST_SAME
    res <- list(all = all, noGP = all, nrMaps = nr5, scope = se$scope)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    saveRDS(res, fd)
    #apply_duln_variant.R copies this one beside fit_types.rds
    file.copy(file.path(TD, "types", "survey_types.csv"), file.path(d, "survey_types.csv"), overwrite = TRUE)
    fits[[key]] <- res
  }
  #the weights in DULN units: per type, the xlsx (clean: its equivalent), the survey fit's
  #layer, the Strava layers (shared by the delta rule; per group; per group signed)
  free <- fits$free$all; sgn <- fits$signed$all
  tw <- typeWeights(free, ex); tws <- if(CLEAN) typeWeights(sgn, ex) else NULL
  sf <- readRDS(SURVEY_FIT)$all; ts <- typeWeights(sf, ex)
  surveyLayer <- function(k) if(CLEAN) withStairs(S_TY * ts$perType[[k]], k) else xlsxNew(k) + S_TY * ts$shared - xlsxNew("walkNat")
  wt <- do.call(rbind, lapply(TYPES7, function(k){
    el <- names(xlsxNew(k))
    ss <- xlsxNew(k) + S_TY * tw$shared - xlsxNew("walkNat")
    data.frame(type = k, group = ST_OF[[k]], element = el, xlsx = unname(xlsxNew(k)[el]), survey = unname(surveyLayer(k)[el]),
               strava_shared = unname(ss[el]), strava_group = unname(withStairs(S_TY * tw$perType[[k]], k)[el]),
               strava_group_signed = if(CLEAN) unname(withStairs(S_TY * tws$perType[[k]], k)[el]) else NA_real_,
               identified = el %in% free$ident)
  }))
  utils::write.csv(wt, stPath("strava_weights.csv"), row.names = FALSE)
  #the fit: cross-validated MSE, own difference against the shared weights; the shared
  #fit with and without the elements; the slopes on log Nearby-Random
  cvOf <- function(a, lab) rbind(
    data.frame(fit = lab, group = "shared", lambda = a$lambdaShared, cvOwn = if(is.null(a$bounds)) min(a$cvShared) else a$cvSharedBox,
               cvShared = a$cvNone, gain = NA_real_, gammaNR = NA_real_),
    do.call(rbind, lapply(STRAVA5, function(k){ d <- a$dev[[k]]; own <- if(is.null(a$bounds)) d$cv else d$cvBox
      data.frame(fit = lab, group = k, lambda = d$lambda, cvOwn = own, cvShared = d$cv0, gain = NA_real_,
                 gammaNR = unname(a$gammaNR[[paste0("nr_", k)]])) })))
  cvt <- rbind(cvOf(free, "unconstrained"), if(CLEAN) cvOf(sgn, "signed"))
  cvt$gain <- 1 - cvt$cvOwn / cvt$cvShared
  utils::write.csv(cvt, stPath("strava_cv.csv"), row.names = FALSE)
  logf("cross-validated MSE (shared: with the elements vs none; groups: own difference vs the shared weights):")
  print(cvt, digits = 4, row.names = FALSE)
  #Strava's shared weights against the survey fit's and the xlsx's, element by element
  #(DULN units at strength 1), and each group's layer
  els <- ELEMENTS_NEW()
  vs <- data.frame(element = els, xlsx_walkNat = unname(xlsxNew("walkNat")[els]), survey_shared = unname(ts$shared[els]),
                   strava_shared = unname(tw$shared[els]), strava_shared_signed = if(CLEAN) unname(tws$shared[els]) else NA_real_,
                   identified_strava = els %in% free$ident, identified_survey = els %in% sf$ident)
  for(k in STRAVA5) vs[[paste0("strava_", k)]] <- unname(tw$perType[[ACT_COL[[k]]]][els])
  if(CLEAN) for(k in STRAVA5) vs[[paste0("signed_", k)]] <- unname(tws$perType[[ACT_COL[[k]]]][els])
  utils::write.csv(vs, stPath("strava_vs_survey.csv"), row.names = FALSE)
  both <- vs$identified_strava & vs$identified_survey
  agree <- function(a, b) c(r = stats::cor(a[both], b[both]), sign = mean(sign(a[both]) == sign(b[both])))
  sm <- rbind(`Strava vs survey` = agree(vs$strava_shared, vs$survey_shared), `Strava vs xlsx` = agree(vs$strava_shared, vs$xlsx_walkNat),
              `survey vs xlsx` = agree(vs$survey_shared, vs$xlsx_walkNat))
  if(CLEAN) sm <- rbind(sm, `Strava signed vs survey` = agree(vs$strava_shared_signed, vs$survey_shared))
  logf("shared weights over the %d elements both fits identify (Pearson r, share of equal signs):", sum(both))
  print(sm, digits = 3)
  cols <- c("element", "xlsx_walkNat", "survey_shared", "strava_shared", if(CLEAN) "strava_shared_signed", paste0("strava_", STRAVA5))
  print(vs[order(-abs(vs$strava_shared)), cols], digits = 2, row.names = FALSE)
  invisible(fits)
}

#### the variants (built by data-raw/apply_duln_variant.R) ####

TY_VARIANTS <- if(CLEAN) list(
  S5E_s0.5  = list(kind = "types", typeArea = FALSE, manual = BIKE_STAIRS, postNet = stairsPostNet),
  SSE_s0.5  = list(kind = "shared", typeArea = FALSE, postNet = stairsPostNet),
  S5Ec_s0.5 = list(kind = "types", typeArea = FALSE, fit = "signed", manual = BIKE_STAIRS, postNet = stairsPostNet)
) else list(
  S5_s0.5 = list(kind = "types", typeArea = FALSE, manual = BIKE_STAIRS, postNet = stairsPostNet),
  SS_s0.5 = list(kind = "shared", typeArea = FALSE, postNet = stairsPostNet))

#### stage score: + each group's map against its own Strava layer ####

stageScoreStrava <- function(){
  stageScoreAct()
  tr <- readRDS(actPath("typed_runs.rds"))
  R <- actRuns()
  g <- tr$grid; res <- tr$res
  se <- readRDS(basePath("extract_glatt.rds"))
  OUT0 <- R[["best"]]
  sv <- withOut(OUT0, readRDS(surveyFile("glatt")))
  G <- withOut(OUT0, buildGrid("glatt", readRDS(aoiFile("glatt")), sv, readRDS(edgesFile("glatt"))))
  #the folder's comparison cells (in the areas, no lakes) that hold heat
  cells <- setdiff(which(as.logical(G$mBase)), sv$lakeCells)
  sc <- onStravaGrid(se, G$base, cells)
  ok <- !is.na(sc) & !is.na(se$Hc[[1]][sc])
  cells <- cells[ok]; sc <- sc[ok]
  tg <- groupTarget(se, sc)
  B <- 1000; set.seed(1); idx <- matrix(sample.int(ACT_REPS, ACT_REPS * B, replace = TRUE), B)
  runsOf <- function(v){ i <- which(g$ctx == "glatt" & g$variant == v & g$type == "full"); rr <- res[i][order(g$rep[i])]
    if(!length(rr) || !all(vapply(rr, function(r) identical(r$status, "done"), logical(1)))) NULL else rr }
  scoreS <- function(rr, k){
    x <- numeric(terra::ncell(G$t100))
    for(r in rr) for(t in names(ST_OF)[ST_OF == k]){ b <- r$byType[[t]]; x[b$cells] <- x[b$cells] + b$counts }
    stats::cor(as.vector(G$P %*% x)[cells], tg[, k], method = "spearman")
  }
  out <- list(); rb <- runsOf("best")
  for(k in STRAVA5){
    bB <- vapply(seq_len(B), function(b) scoreS(rb[idx[b, ]], k), 1)
    for(v in unique(g$variant)){
      rr <- runsOf(v); if(is.null(rr)) next
      bV <- vapply(seq_len(B), function(b) scoreS(rr[idx[b, ]], k), 1)
      out[[length(out) + 1]] <- data.frame(group = k, label = ST_LABEL[[k]], variant = v, spearman = scoreS(rr, k),
        dBest = scoreS(rr, k) - scoreS(rb, k), lo = unname(stats::quantile(bV - bB, .025)), hi = unname(stats::quantile(bV - bB, .975)))
    }
  }
  out <- do.call(rbind, out)
  utils::write.csv(out, stPath("per_group_vs_strava.csv"), row.names = FALSE)
  logf("each group's simulated map at Glatt against its own Strava layer (Spearman over %d cells):", length(cells))
  print(out, digits = 3, row.names = FALSE)
}

#### stage figures ####

FIG_NEW <- Sys.getenv("VFT_ACT_FIGNEW", "s5")
basemapOf <- function(ctx, bb){
  tileF <- basePath("figures", paste0("basemap_", ctx, ".tif"))
  tryCatch({
    if(!file.exists(tileF))
      terra::writeRaster(maptiles::get_tiles(sf::st_as_sfc(bb), crop = TRUE, zoom = 13,
        provider = maptiles::create_provider(name = "swisstopo_grey", citation = "swisstopo",
          url = "https://wmts.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-grau/default/current/3857/{z}/{x}/{y}.jpeg")),
        tileF, overwrite = TRUE)
    terra::rast(tileF) * 0.3 + 178
  }, error = function(e){ logf("basemap failed: %s", conditionMessage(e)); NULL })
}
stageFiguresStrava <- function(){
  R <- actRuns()
  dir.create(stPath("figures"), showWarnings = FALSE)
  se <- readRDS(basePath("extract_glatt.rds"))
  pal <- grDevices::colorRampPalette(c("#2166ac", "#67a9cf", "#d1e5f0", "#f2f2f2", "#fddbc7", "#ef8a62", "#b2182b"))(101)
  heatPal <- grDevices::colorRampPalette(c("#f0f0f0", "#fdd49e", "#fc8d59", "#d7301f", "#7f0000"))(101)
  #1. each group's heat on Glatt's edges (normal scores over the edges, Ride + Gravel averaged)
  net <- readRDS(netFile("glatt"))$net
  g <- sf::st_transform(sf::st_zm(igraph::edge_attr(net)$SHAPE), 2056)
  bb <- sf::st_bbox(g); asp <- diff(bb[c(2, 4)]) / diff(bb[c(1, 3)])
  base <- basemapOf("glatt", bb)
  baseMap <- function() if(!is.null(base)) terra::plotRGB(base, axes = FALSE, mar = 0, smooth = TRUE) else plot(sf::st_as_sfc(bb), border = NA)
  one <- rep(1L, nrow(se$H))
  ez <- sapply(STRAVA5, function(k){ z <- sapply(ST_SPORTS[[k]], function(s) nscore(se$H[, s], one))
    if(length(ST_SPORTS[[k]]) > 1) nscore(rowMeans(z), one) else drop(z) })
  grDevices::png(stPath("figures", "heat_glatt.png"), width = 2400, height = round(1600 * asp), res = 170)
  graphics::par(mfrow = c(2, 3), mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), bg = "white")
  for(k in STRAVA5){
    ci <- round(100 * stats::pnorm(ez[, k])) + 1; o <- order(ez[, k])
    baseMap()
    plot(g[o], col = heatPal[ci[o]], lwd = 0.4 + 1.2 * stats::pnorm(ez[o, k]), add = TRUE)
    graphics::legend("topright", bg = grDevices::adjustcolor("white", 0.85), box.col = NA, cex = 0.8, inset = 0.01,
                     legend = c("top 10 %", "median", "bottom 10 %"), col = heatPal[c(91, 51, 11)], lwd = 4, title = ST_LABEL[[k]])
  }
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Strava global heatmap (z14), Glatt\nedge heat as a rank over the edges", cex = 1.2)
  grDevices::dev.off()
  #2. the change of edge DULN against the best model, per group, at every site
  cors <- list(); areaT <- list()
  for(ctx in CONTEXTS){
    nb <- withOut(R[["best"]], readRDS(netFile(ctx)))$net
    nn <- withOut(R[[FIG_NEW]], readRDS(netFile(ctx)))$net
    eb <- igraph::edge_attr(nb); en <- igraph::edge_attr(nn)
    vb <- igraph::vertex_attr(nb); vn <- igraph::vertex_attr(nn)
    stopifnot(identical(eb$edgeID, en$edgeID))
    for(k in TYPES7){
      col <- COL_OF[[k]]
      cors[[length(cors) + 1]] <- data.frame(ctx = ctx, type = k, group = ST_OF[[k]],
        rEdge = stats::cor(eb[[col]], en[[col]]), rNode = stats::cor(vb[[col]], vn[[col]], use = "complete.obs"),
        meanBest = mean(eb[[col]]), meanNew = mean(en[[col]]), sdBest = stats::sd(eb[[col]]), sdNew = stats::sd(en[[col]]))
    }
    ab <- withOut(R[["best"]], readRDS(aoiFile(ctx)))$polys; an <- withOut(R[[FIG_NEW]], readRDS(aoiFile(ctx)))$polys
    for(k in STRAVA5) areaT[[length(areaT) + 1]] <- data.frame(ctx = ctx, group = k, aoi = ab$AOI,
      ha = as.numeric(sf::st_area(ab)) / 1e4, best = ab$DULN, new = an[[paste0("DULN_", ACT_COL[[k]])]])
    gc <- sf::st_transform(sf::st_zm(eb$SHAPE), 2056)
    bc <- sf::st_bbox(gc); ac <- diff(bc[c(2, 4)]) / diff(bc[c(1, 3)])
    base <- basemapOf(ctx, bc)
    baseMap <- function() if(!is.null(base)) terra::plotRGB(base, axes = FALSE, mar = 0, smooth = TRUE) else plot(sf::st_as_sfc(bc), border = NA)
    grDevices::png(stPath("figures", sprintf("dDULN_%s.png", ctx)), width = 2400, height = round(1600 * ac), res = 170)
    graphics::par(mfrow = c(2, 3), mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), bg = "white")
    for(k in STRAVA5){
      col <- COL_OF[[ACT_COL[[k]]]]
      d <- en[[col]] - eb[[col]]
      lim <- max(5, round(stats::quantile(abs(d), .98)))
      ci <- pmin(pmax(round(50 + 50 * d / lim), 0), 100) + 1
      o <- order(abs(d))
      baseMap()
      plot(gc[o], col = pal[ci[o]], lwd = ifelse(abs(d[o]) > lim / 4, 1.3, 0.5), add = TRUE)
      graphics::legend("topright", bg = grDevices::adjustcolor("white", 0.85), box.col = NA, cex = 0.75, inset = 0.01,
                       legend = c(sprintf("+%g", lim), sprintf("+%g", lim / 2), "0", sprintf("-%g", lim / 2), sprintf("-%g", lim)),
                       col = pal[c(101, 76, 51, 26, 1)], lwd = 4, title = ST_LABEL[[k]])
    }
    graphics::plot.new()
    graphics::text(0.5, 0.5, sprintf("%s: edge DULN, Strava (%s)\nminus the best model (survey, shared)", ctx, FIG_NEW), cex = 1.2)
    grDevices::dev.off()
  }
  cors <- do.call(rbind, cors); utils::write.csv(cors, stPath("dulnBestVsNew.csv"), row.names = FALSE)
  areaT <- do.call(rbind, areaT); utils::write.csv(areaT, stPath("areaDulnBestVsNew.csv"), row.names = FALSE)
  print(cors, digits = 3, row.names = FALSE)
  #3. the shared weights: xlsx, survey, Strava (DULN units at strength 1)
  vs <- utils::read.csv(stPath("strava_vs_survey.csv"))
  vs <- vs[vs$identified_strava | vs$identified_survey, ]
  vs <- vs[order(vs$strava_shared), ]
  grDevices::png(stPath("figures", "weights.png"), width = 1500, height = 60 + 34 * nrow(vs), res = 150)
  graphics::par(mar = c(4, 11, 1, 1))
  lim <- range(c(vs$xlsx_walkNat, vs$survey_shared, vs$strava_shared), na.rm = TRUE)
  plot(NA, xlim = lim, ylim = c(0.5, nrow(vs) + 0.5), yaxt = "n", xlab = "weight (DULN units, strength 1)", ylab = "")
  graphics::axis(2, at = seq_len(nrow(vs)), labels = vs$element, las = 1, cex.axis = 0.8)
  graphics::abline(v = 0, col = "grey70"); graphics::abline(h = seq_len(nrow(vs)), col = "grey93")
  graphics::points(vs$xlsx_walkNat, seq_len(nrow(vs)), pch = 4, col = "grey40")
  graphics::points(vs$survey_shared, seq_len(nrow(vs)), pch = 16, col = "#2166ac")
  graphics::points(vs$strava_shared, seq_len(nrow(vs)), pch = 17, col = "#d7301f")
  graphics::legend("bottomright", legend = c("xlsx (Nature walkers)", "survey fit (shared)", "Strava fit (shared)"),
                   pch = c(4, 16, 17), col = c("grey40", "#2166ac", "#d7301f"), bg = "white", cex = 0.8)
  grDevices::dev.off()
  logf("figures in %s", stPath("figures"))
}

#### run ####

if(!IS_ST_WORKER){
  t00 <- proc.time()[["elapsed"]]
  if("tiles" %in% ST_STAGES) stageTiles()
  if("decode" %in% ST_STAGES) stageDecode()
  if("check" %in% ST_STAGES) stageCheck()
  if("extract" %in% ST_STAGES) stageExtractStrava()
  if("fit" %in% ST_STAGES) stageFitStrava()
  if("typed" %in% ST_STAGES) stageTypedAct()
  if("score" %in% ST_STAGES) stageScoreStrava()
  if("figures" %in% ST_STAGES) stageFiguresStrava()
  if(!identical(ST_STAGES, "none"))
    logf("tailor_duln_strava: done in %.1f min, output in %s", (proc.time()[["elapsed"]] - t00) / 60, ST)
}
