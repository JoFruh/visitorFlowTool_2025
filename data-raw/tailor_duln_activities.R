#### DULN per ACTIVITY (4) from the Glatt survey ####
#
# data-raw/tailor_duln_types.R fitted a DULN per agent TYPE (7). The Glatt survey has
# too few routes to tell them apart (25 Nature e-biker routes, ~50 per dog type), and
# the per-type layers did no better than the shared fit. This is the same process on
# the survey's 4 ACTIVITIES (the user's choice, 2026-10-21):
#   walking (walkNat, walkSoc)      dog walking (dogNat, dogProx)
#   jogging (jogger)                biking (bikeSport, ebikeNat; bike + E_bike)
# The engine keeps its 7 columns: both types of an activity take the activity's fitted
# weights on the identified elements, and their own xlsx weights elsewhere
# (tailor_duln_types.R's per-type rule), at strength 0.5.
#
# STAIRS FOR BIKERS (the user's rule): a stair counts 3 x its length in the bikers'
# routing - a bike weight becomes w * (1 + 2 * stair share of the edge) in all three
# weight columns of bikeSport and ebikeNat. The stair DULN weight of both bike types
# stays the xlsx's -10 (the best model's), so the junction choice keeps avoiding them.
# Stairs are TLM_STRASSE with KUNSTBAUTE 900 (swissTLM3D), matched to the edges by a
# 1 m buffer; overlaps under 2 m (crossings) do not count.
#
# Stages (VFT_ACT_STAGES):
#   survey   the 7 typed survey maps (types/survey_types.rds) summed per activity
#   stairs   the stair metres of every edge, per site
#   fit      tailor_duln_types.R's fitTypes() over the 4 activities, offsets the
#            7-type Nearby-Random captures summed (types/capture_runs.rds); all Glatt
#            cells, and without GlattPark's ground (GlattPark's weights)
#   typed    Full with the tracks per type on the validation folders (VFT_ACT_RUNS):
#            the best model, the activity variant (and its per-activity area choice),
#            the stairs-only control; Nearby-Random of the activity variant at Glatt
#   score    totals of the per-activity area choice; each activity's map at Glatt
#            against its own routes; the stair metres walked
#   figures  weights and maps for the results page
# The variants are built on a validation folder's areas by data-raw/apply_duln_variant.R
# (VFT_AV_SCRIPT=tailor_duln_activities.R) from TY_VARIANTS below:
#   A4_s0.5            the 4 activity layers, stairs -10 for bikers, stairs x3 routing
#   T_shared_stairs3   the best model's layers (T_shared_s0.5) with only stairs x3
#
#   Rscript data-raw/tailor_duln_activities.R
#
# VFT_ACT_STAGES   which stages (default survey, stairs, fit)
# VFT_ACT_OUT      output (default <duln_tailored>/activities)
# VFT_ACT_RUNS     the folders of the typed stage, name=path comma list
# VFT_ACT_REPS     reps of the typed stage (default 20)
# VFT_ACT_ALPHA    the route alpha of the typed stage (default 5, the best model's)

IS_ACT_WORKER <- identical(Sys.getenv("VFT_TY_WORKER"), "1")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
ACT_SCRIPT <- file.path(.repo, "data-raw", "tailor_duln_activities.R")
if(!IS_ACT_WORKER){
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if(length(f) && grepl("tailor_duln_activities", f[1])) ACT_SCRIPT <- normalizePath(f[1], winslash = "/")
}
.actStages <- Sys.getenv("VFT_ACT_STAGES", "survey,stairs,fit")
Sys.setenv(VFT_TY_STAGES = "none")
source(file.path(.repo, "data-raw", "tailor_duln_types.R"))
#the workers re-source THIS script (tailor_duln_types.R's runGrid())
TY_SCRIPT <- ACT_SCRIPT
#tailor_duln.R fixes route alpha 2; the best model runs at 5
CORR$alpha <- envNum("VFT_ACT_ALPHA", 5)

ACT  <- Sys.getenv("VFT_ACT_OUT", file.path(TD, "activities"))
ACT_STAGES <- strsplit(.actStages, ",", fixed = TRUE)[[1]]
ACT_REPS <- as.integer(envNum("VFT_ACT_REPS", 20))
ACT4   <- c("walking", "dog", "jogging", "biking")
ACT_LABEL <- c(walking = "Walking", dog = "Dog walking", jogging = "Jogging", biking = "Biking")
ACT_OF <- c(walkNat = "walking", walkSoc = "walking", dogNat = "dog", dogProx = "dog",
            jogger = "jogging", bikeSport = "biking", ebikeNat = "biking")
BIKE_TYPES   <- c("bikeSport", "ebikeNat")
STAIR_FACTOR <- 3
STAIR_BUF_M  <- 1
STAIR_MIN_M  <- 2
STAIR_GDB    <- file.path(LOCAL, "TLM3D", "SWISSTLM3D_CHLV95LN02.gdb")
actPath <- function(...){ p <- file.path(ACT, ...); dir.create(dirname(p[1]), recursive = TRUE, showWarnings = FALSE); p }
sumBy <- function(lst, of) lapply(stats::setNames(ACT4, ACT4), function(a) Reduce(`+`, lst[names(of)[of == a]]))

#### stage survey ####

stageSurveyAct <- function(){
  f <- actPath("survey_activities.rds")
  if(file.exists(f)) return(invisible(readRDS(f)))
  sty <- readRDS(file.path(TD, "types", "survey_types.rds"))
  tab7 <- sty$table
  tab <- do.call(rbind, lapply(ACT4, function(a){
    t <- tab7[tab7$type %in% names(ACT_OF)[ACT_OF == a], ]
    data.frame(activity = a, label = ACT_LABEL[[a]], types = paste(t$type, collapse = "+"), routes = sum(t$routes),
               respondents = sum(t$respondents), km = sum(t$km), cellVisits = sum(t$cellVisits))
  }))
  tab$routeShare <- tab$routes / sum(tab$routes)
  logf("the Glatt routes by activity:")
  print(tab, digits = 3, row.names = FALSE)
  res <- list(table = tab, base = sumBy(sty$base, ACT_OF), lakeCells = sty$lakeCells)
  stopifnot(isTRUE(all.equal(Reduce(`+`, res$base), Reduce(`+`, sty$base))))
  saveRDS(res, f)
  utils::write.csv(tab, actPath("survey_activities.csv"), row.names = FALSE)
  #apply_duln_variant.R copies this one beside fit_types.rds
  file.copy(file.path(TD, "types", "survey_types.csv"), actPath("survey_types.csv"), overwrite = TRUE)
  invisible(res)
}

#### stage stairs ####

#the stair metres on every edge of a site's network (edge IDs are 1..n)
STAIR_DIR <- Sys.getenv("VFT_ACT_STAIRS", file.path(TD, "activities"))
stairsFile <- function(ctx){ dir.create(STAIR_DIR, recursive = TRUE, showWarnings = FALSE); file.path(STAIR_DIR, paste0("stairs_", ctx, ".rds")) }
stageStairs <- function(){
  for(ctx in CONTEXTS){
    f <- stairsFile(ctx)
    if(file.exists(f)) next
    net <- readRDS(netFile(ctx))$net
    ea <- igraph::edge_attr(net)
    g <- sf::st_transform(sf::st_zm(ea$SHAPE), 2056)
    bb <- sf::st_bbox(g)
    st <- sf::st_read(STAIR_GDB, query = "SELECT * FROM TLM_STRASSE WHERE KUNSTBAUTE = 900",
                      wkt_filter = sf::st_as_text(sf::st_as_sfc(bb)), quiet = TRUE)
    st <- sf::st_zm(sf::st_geometry(st))
    sf::st_crs(st) <- 2056   #LV95 + LN02 heights: the plane is 2056
    buf <- sf::st_union(sf::st_buffer(st, STAIR_BUF_M))
    e <- sf::st_sf(edgeID = ea$edgeID, geometry = g)
    hit <- which(lengths(sf::st_intersects(e, buf)) > 0)
    stair <- numeric(nrow(e))
    if(length(hit)){
      ix <- suppressWarnings(sf::st_intersection(e[hit, ], buf))
      m <- tapply(as.numeric(sf::st_length(ix)), ix$edgeID, sum)
      stair[match(as.integer(names(m)), e$edgeID)] <- m
    }
    stair[stair < STAIR_MIN_M] <- 0
    len <- as.numeric(sf::st_length(g))
    d <- data.frame(edgeID = ea$edgeID, len = len, SHAPE_Leng = ea$SHAPE_Leng, stair = pmin(stair, len),
                    share = ifelse(len > 0, pmin(stair / len, 1), 0))
    saveRDS(d, f)
    logf("stairs %s: %d stair features (%.1f km) in the box; %d edges with stairs (%.1f km), %d of them >= 50 %% stairs",
         ctx, length(st), sum(as.numeric(sf::st_length(st))) / 1000, sum(d$stair > 0), sum(d$stair) / 1000, sum(d$share >= .5))
  }
}
.stairCache <- new.env()
stairsOf <- function(ctx){
  if(is.null(.stairCache[[ctx]])) .stairCache[[ctx]] <- readRDS(stairsFile(ctx))
  .stairCache[[ctx]]
}
#the bikers' weights on the stair part of an edge: STAIR_FACTOR x
stairsPostNet <- function(net, ctx){
  s <- stairsOf(ctx)
  ea <- igraph::edge_attr(net)
  k <- match(ea$edgeID, s$edgeID)
  if(anyNA(k)) stop(ctx, ": edges without a stair measure - rerun stage stairs")
  fac <- 1 + (STAIR_FACTOR - 1) * s$share[k]
  for(t in BIKE_TYPES) for(sfx in c("", "_attr", "_ATTR")){
    nm <- paste0("SHAPE_Leng_", t, sfx)
    if(is.null(ea[[nm]])) stop("no weight column ", nm)
    ea[[nm]] <- ea[[nm]] * fac
  }
  cls <- class(net)
  igraph::edge_attr(net) <- ea
  class(net) <- cls
  logf("stairs x%g %s: %d edges' bike weights raised, by %.1f km of stairs", STAIR_FACTOR, ctx, sum(fac > 1), sum(s$stair[k]) / 1000)
  net
}
#simTyped()'s stair metres walked (tailor_duln_types.R)
STAIR_LEN_OF <- function(ctx){
  s <- stairsOf(ctx)
  n <- max(s$edgeID)
  st <- numeric(n); ln <- numeric(n)
  st[s$edgeID] <- s$stair; ln[s$edgeID] <- s$SHAPE_Leng
  list(stair = st, len = ln)
}

#### the variants (built by data-raw/apply_duln_variant.R) ####

TY_VARIANTS <- list(
  A4_s0.5 = list(kind = "types", typeArea = FALSE,
                 manual = list(bikeSport = c(stairs = -10), ebikeNat = c(stairs = -10)),
                 postNet = stairsPostNet),
  T_shared_stairs3 = list(kind = "shared", typeArea = FALSE, postNet = stairsPostNet))

#### stage fit ####

stageFitAct <- function(){
  f <- actPath("fit_types.rds")
  if(file.exists(f)) return(invisible(readRDS(f)))
  sa <- stageSurveyAct()
  cr <- readRDS(file.path(TD, "types", "capture_runs.rds"))
  ex <- readRDS(extractFile("glatt"))
  G <- withOut(RS, buildGrid("glatt", readRDS(aoiFile("glatt")), readRDS(surveyFile("glatt")), readRDS(edgesFile("glatt"))))
  nrRuns <- cr$res[cr$grid$type == "nearbyRandom"]
  nr7 <- lapply(stats::setNames(TYPES7, TYPES7), function(k) typeBase(nrRuns, G, k))
  nr4 <- sumBy(nr7, ACT_OF)
  all  <- fitTypes(ex, sa, nr4, "activities, all Glatt cells", types = ACT4)
  noGP <- fitTypes(dropGround(ex, "glattPark"), sa, nr4, "activities, without GlattPark's ground", types = ACT4)
  all$actOf <- noGP$actOf <- ACT_OF
  res <- list(all = all, noGP = noGP, nrMaps = nr4)
  saveRDS(res, f)
  #the weights in DULN units (as the layers get them, before the manual stairs)
  ft7 <- readRDS(file.path(TD, "types", "fit_types.rds"))
  exG <- readRDS(extractFile("glatt"))
  wt <- do.call(rbind, lapply(c("all", "noGP"), function(fu){
    tw <- typeWeights(res[[fu]], exG)
    t7 <- typeWeights(ft7[[fu]], exG)
    do.call(rbind, lapply(TYPES7, function(k){
      el <- names(xlsxNew(k))
      best <- xlsxNew(k) + S_TY * t7$shared - xlsxNew("walkNat")
      new  <- S_TY * tw$perType[[k]]
      man  <- TY_VARIANTS$A4_s0.5$manual[[k]]
      newM <- new; if(length(man)) newM[names(man)] <- man
      data.frame(fit = fu, type = k, activity = ACT_OF[[k]], element = el, xlsx = unname(xlsxNew(k)),
                 best = unname(best[el]), new_fit = unname(new[el]), new = unname(newM[el]),
                 manual = el %in% names(man), identified = el %in% res[[fu]]$ident)
    }))
  }))
  utils::write.csv(wt, actPath("activity_weights.csv"), row.names = FALSE)
  cvt <- do.call(rbind, lapply(c("all", "noGP"), function(fu) do.call(rbind, lapply(ACT4, function(a){
    d <- res[[fu]]$dev[[a]]
    data.frame(fit = fu, activity = a, lambda = d$lambda, cvOwn = d$cv, cvShared = d$cv0, gain = 1 - d$cv / d$cv0)
  }))))
  utils::write.csv(cvt, actPath("activity_cv.csv"), row.names = FALSE)
  logf("cross-validated deviance, the activity's own difference against the shared weights:")
  print(cvt, digits = 4, row.names = FALSE)
  b <- wt[wt$fit == "all" & wt$element == "stairs", c("type", "xlsx", "best", "new_fit", "new")]
  logf("stairs, DULN units at s0.5 (new = after the manual weight):")
  print(b, digits = 3, row.names = FALSE)
  invisible(res)
}

#### stage typed: Full with the tracks per type ####

actRuns <- function(){
  x <- strsplit(strsplit(Sys.getenv("VFT_ACT_RUNS", ""), ",", fixed = TRUE)[[1]], "=", fixed = TRUE)
  if(!length(x)) stop("set VFT_ACT_RUNS (best=..., act4=..., stairs3=...)")
  stats::setNames(vapply(x, `[`, "", 2), vapply(x, `[`, "", 1))
}

stageTypedAct <- function(){
  f <- actPath("typed_runs.rds")
  R <- actRuns()
  #the folder whose per-type area choice and Nearby-Random are run too (VFT_ACT_TYPEAREA)
  TA <- Sys.getenv("VFT_ACT_TYPEAREA", "act4")
  g <- rbind(
    expand.grid(rep = seq_len(ACT_REPS), ctx = CONTEXTS, variant = names(R),
                type = "full", typeArea = FALSE, stringsAsFactors = FALSE),
    #the per-activity area choice and the per-activity Nearby-Random: that folder's
    if(TA %in% names(R)) expand.grid(rep = seq_len(ACT_REPS), ctx = CONTEXTS, variant = paste0(TA, "_typeArea"),
                type = "full", typeArea = TRUE, stringsAsFactors = FALSE),
    if(TA %in% names(R)) expand.grid(rep = seq_len(ACT_REPS), ctx = "glatt", variant = paste0(TA, "_nr"),
                type = "nearbyRandom", typeArea = FALSE, stringsAsFactors = FALSE))
  g$vdir <- unname(R[sub("_(typeArea|nr)$", "", g$variant)])
  g <- g[!is.na(g$vdir), ]
  g <- g[order(match(g$ctx, c("glatt", "glattPark", "wigger")), g$vdir, g$variant, g$rep), ]
  key <- function(d) paste(d$ctx, d$variant, d$rep)
  res <- vector("list", nrow(g)); todo <- seq_len(nrow(g))
  if(file.exists(f)){
    old <- readRDS(f); m <- match(key(g), key(old$grid))
    res[!is.na(m)] <- old$res[m[!is.na(m)]]
    todo <- which(!vapply(res, function(r) identical(r$status, "done"), logical(1)))
  }
  logf("typed: %d runs, %d to do", nrow(g), length(todo))
  if(length(todo)) res[todo] <- runGrid(g[todo, ], "typed")
  saveRDS(list(grid = g, res = res), f)
  #the runs without the per-activity area choice are the folders' stored runs
  same <- vapply(seq_len(nrow(g)), function(i){
    if(g$typeArea[i] || !identical(res[[i]]$status, "done")) return(NA)
    s <- withOut(g$vdir[i], runsFrom(g$ctx[i], g$type[i], g$rep[i]))[[1]]
    identical(as.integer(s$cells), as.integer(res[[i]]$cells)) && identical(as.integer(s$counts), as.integer(res[[i]]$counts))
  }, logical(1))
  logf("typed: the runs reproduce the folders' stored runs: %d of %d identical", sum(same, na.rm = TRUE), sum(!is.na(same)))
  if(any(!same, na.rm = TRUE)) stop("the typed runs differ from the stored ones")
  invisible(list(grid = g, res = res))
}

#### stage score ####

stageScoreAct <- function(){
  tr <- readRDS(actPath("typed_runs.rds"))
  R <- actRuns()
  g <- tr$grid; res <- tr$res
  runsOf <- function(ctx, v){ i <- which(g$ctx == ctx & g$variant == v); rr <- res[i][order(g$rep[i])]
    if(!length(rr) || !all(vapply(rr, function(r) identical(r$status, "done"), logical(1)))) NULL else rr }
  B <- 1000; set.seed(1); idx <- matrix(sample.int(ACT_REPS, ACT_REPS * B, replace = TRUE), B)
  ci <- function(d) if(all(is.na(d))) c(NA_real_, NA_real_) else unname(stats::quantile(d, c(.025, .975), na.rm = TRUE))
  #totals: the scorers of the activity folder (same areas and survey as the others)
  #(the folders share areas and survey: the best model's scores them all)
  scorers <- lapply(stats::setNames(CONTEXTS, CONTEXTS), function(ctx) withOut(R[["best"]], scorerFor(ctx)))
  VS <- unique(g$variant[g$type == "full"])
  sc <- function(ctx, rr) mean(scorers[[ctx]](rr))
  tot <- list()
  for(ctx in CONTEXTS){
    vs <- VS
    rr <- lapply(stats::setNames(vs, vs), function(v) runsOf(ctx, v))
    bt <- lapply(Filter(Negate(is.null), rr), function(x) vapply(seq_len(B), function(b) sc(ctx, x[idx[b, ]]), 1))
    for(v in names(bt)){
      d1 <- if(!is.null(bt$best)) bt[[v]] - bt$best else NA
      d2 <- if(!is.null(bt$act4)) bt[[v]] - bt$act4 else NA
      tot[[length(tot) + 1]] <- data.frame(ctx = ctx, variant = v, score = sc(ctx, rr[[v]]),
        dBest = sc(ctx, rr[[v]]) - sc(ctx, rr$best), dBest_lo = ci(d1)[1], dBest_hi = ci(d1)[2],
        dAct4 = if(is.null(rr$act4)) NA else sc(ctx, rr[[v]]) - sc(ctx, rr$act4), dAct4_lo = ci(d2)[1], dAct4_hi = ci(d2)[2])
    }
  }
  tot <- do.call(rbind, tot)
  utils::write.csv(tot, actPath("typed_totals.csv"), row.names = FALSE)
  options(width = 220)
  logf("totals, %d reps (mean Pearson 100 + 500 m; GlattPark 500 m):", ACT_REPS)
  print(tot, digits = 3, row.names = FALSE)

  #per activity at Glatt, against its own routes, on the activity folder's grid
  OUT0 <- R[["best"]]
  ln <- typedLines()
  sv <- withOut(OUT0, readRDS(surveyFile("glatt")))
  if(nrow(sv$L) != nrow(ln) || !identical(sv$resp, ln$resp)) stop("the folder's Glatt survey lines are not the typed lines")
  act <- unname(ACT_OF[ln$agentType])
  base <- lapply(stats::setNames(ACT4, ACT4), function(a) as.vector(Matrix::colSums(sv$L[act %in% a, , drop = FALSE])))
  stopifnot(isTRUE(all.equal(Reduce(`+`, base), sv$surveyBase)))
  G <- withOut(OUT0, buildGrid("glatt", readRDS(aoiFile("glatt")), sv, readRDS(edgesFile("glatt"))))
  cfg <- CTX$glatt; vn <- HEAD_VIEWS("glatt")
  surA <- lapply(base, function(b) lapply(stats::setNames(vn, vn), function(v) surveyView(G, G$views[[v]], b)))
  scoreA <- function(rr, a){
    x <- numeric(terra::ncell(G$t100))
    for(r in rr) for(k in names(ACT_OF)[ACT_OF == a]){ b <- r$byType[[k]]; x[b$cells] <- x[b$cells] + b$counts }
    mean(vapply(vn, function(v) stats::cor(trans(as.vector(G$views[[v]]$Msim %*% x), cfg$simTransform), surA[[a]][[v]]), 1))
  }
  pa <- list()
  vs <- unique(g$variant)
  for(a in ACT4){
    rb <- runsOf("glatt", "best")
    bB <- vapply(seq_len(B), function(b) scoreA(rb[idx[b, ]], a), 1)
    for(v in vs){
      rr <- runsOf("glatt", v); if(is.null(rr)) next
      bV <- vapply(seq_len(B), function(b) scoreA(rr[idx[b, ]], a), 1)
      pa[[length(pa) + 1]] <- data.frame(activity = a, label = ACT_LABEL[[a]], variant = v, score = scoreA(rr, a),
                                         dBest = scoreA(rr, a) - scoreA(rb, a), lo = ci(bV - bB)[1], hi = ci(bV - bB)[2])
    }
  }
  pa <- do.call(rbind, pa)
  utils::write.csv(pa, actPath("per_activity_glatt.csv"), row.names = FALSE)
  logf("per activity at Glatt (mean Pearson 100 + 500 m of the activity's map against its own routes):")
  print(pa, digits = 3, row.names = FALSE)

  #stair metres walked, per activity
  st <- list()
  for(ctx in CONTEXTS) for(v in VS){
    rr <- runsOf(ctx, v); if(is.null(rr) || is.null(rr[[1]]$stairs)) next
    S <- Reduce(`+`, lapply(rr, `[[`, "stairs"))
    for(a in ACT4){
      k <- names(ACT_OF)[ACT_OF == a]
      s <- rowSums(S[, k, drop = FALSE])
      st[[length(st) + 1]] <- data.frame(ctx = ctx, variant = v, activity = a, agents = s[["agents"]],
                                         stairM_perAgent = s[["stairM"]] / s[["agents"]],
                                         stairPerKm = 1000 * s[["stairM"]] / s[["trackM"]])
    }
  }
  st <- do.call(rbind, st)
  utils::write.csv(st, actPath("stairs_walked.csv"), row.names = FALSE)
  logf("stairs walked (m per agent; m per km of track):")
  print(st, digits = 3, row.names = FALSE)
}

#### stage figures: the edge DULN maps ####

#each activity's column: the type whose layer stands for it on the maps
ACT_COL <- c(walking = "walkNat", dog = "dogNat", jogging = "jogger", biking = "bikeSport")
#the folder (VFT_ACT_RUNS name) the maps compare with the best model
FIG_NEW <- Sys.getenv("VFT_ACT_FIGNEW", "act4")
stageFiguresAct <- function(){
  R <- actRuns()
  dir.create(actPath("figures"), showWarnings = FALSE)
  cors <- list(); areaT <- list()
  for(ctx in CONTEXTS){
    nb <- withOut(R[["best"]], readRDS(netFile(ctx)))$net
    na <- withOut(R[[FIG_NEW]], readRDS(netFile(ctx)))$net
    eb <- igraph::edge_attr(nb); e4 <- igraph::edge_attr(na)
    vb <- igraph::vertex_attr(nb); v4 <- igraph::vertex_attr(na)
    stopifnot(identical(eb$edgeID, e4$edgeID))
    for(k in TYPES7){
      col <- COL_OF[[k]]
      cors[[length(cors) + 1]] <- data.frame(ctx = ctx, type = k, activity = ACT_OF[[k]],
        rEdge = stats::cor(eb[[col]], e4[[col]]), rNode = stats::cor(vb[[col]], v4[[col]], use = "complete.obs"),
        meanBest = mean(eb[[col]]), meanNew = mean(e4[[col]]), sdBest = stats::sd(eb[[col]]), sdNew = stats::sd(e4[[col]]))
    }
    ab <- withOut(R[["best"]], readRDS(aoiFile(ctx)))$polys; a4 <- withOut(R[[FIG_NEW]], readRDS(aoiFile(ctx)))$polys
    for(a in ACT4) areaT[[length(areaT) + 1]] <- data.frame(ctx = ctx, activity = a, aoi = ab$AOI,
      ha = as.numeric(sf::st_area(ab)) / 1e4, best = ab$DULN, new = a4[[paste0("DULN_", ACT_COL[[a]])]])
    #the maps: new - best per activity, on the swisstopo grey map
    g <- sf::st_transform(sf::st_zm(eb$SHAPE), 2056)
    bb <- sf::st_bbox(g)
    tileF <- actPath("figures", paste0("basemap_", ctx, ".tif"))
    base <- tryCatch({
      if(!file.exists(tileF))
        terra::writeRaster(maptiles::get_tiles(sf::st_as_sfc(bb), crop = TRUE, zoom = 13,
          provider = maptiles::create_provider(name = "swisstopo_grey", citation = "swisstopo",
            url = "https://wmts.geo.admin.ch/1.0.0/ch.swisstopo.pixelkarte-grau/default/current/3857/{z}/{x}/{y}.jpeg")),
          tileF, overwrite = TRUE)
      #faded towards white, so the edges read over it
      terra::rast(tileF) * 0.3 + 178
    }, error = function(e){ logf("basemap failed: %s", conditionMessage(e)); NULL })
    pal <- grDevices::colorRampPalette(c("#2166ac", "#67a9cf", "#d1e5f0", "#f2f2f2", "#fddbc7", "#ef8a62", "#b2182b"))(101)
    asp <- diff(bb[c(2, 4)]) / diff(bb[c(1, 3)])
    baseMap <- function(){
      if(!is.null(base)) terra::plotRGB(base, axes = FALSE, mar = 0, smooth = TRUE)
      else plot(sf::st_as_sfc(bb), border = NA)
    }
    #the 4 activities side by side (2 x 2), one scale per panel
    grDevices::png(actPath("figures", sprintf("dDULN_%s.png", ctx)), width = 2000, height = round(2000 * asp), res = 170)
    graphics::par(mfrow = c(2, 2), mar = c(0, 0, 0, 0), oma = c(0, 0, 0, 0), bg = "white")
    for(a in ACT4){
      col <- COL_OF[[ACT_COL[[a]]]]
      d <- e4[[col]] - eb[[col]]
      lim <- max(5, round(stats::quantile(abs(d), .98)))
      ci <- pmin(pmax(round(50 + 50 * d / lim), 0), 100) + 1
      o <- order(abs(d))
      baseMap()
      plot(g[o], col = pal[ci[o]], lwd = ifelse(abs(d[o]) > lim / 4, 1.3, 0.5), add = TRUE)
      graphics::legend("topright", bg = grDevices::adjustcolor("white", 0.85), box.col = NA, cex = 0.75, inset = 0.01,
                       legend = c(sprintf("+%g", lim), sprintf("+%g", lim / 2), "0", sprintf("-%g", lim / 2), sprintf("-%g", lim)),
                       col = pal[c(101, 76, 51, 26, 1)], lwd = 4, title = ACT_LABEL[[a]])
    }
    grDevices::dev.off()
    #the stair edges (x3 for bikers)
    s <- stairsOf(ctx)
    k <- match(eb$edgeID, s$edgeID)
    grDevices::png(actPath("figures", sprintf("stairs_%s.png", ctx)), width = 1400, height = round(1400 * asp), res = 150)
    graphics::par(mar = c(0, 0, 0, 0), bg = "white")
    baseMap()
    plot(g, col = "#bdbdbd", lwd = 0.4, add = TRUE)
    on <- which(s$share[k] > 0)
    plot(g[on], col = "#b2182b", lwd = 2.2, add = TRUE)
    graphics::legend("bottomleft", bg = "white", box.col = NA, cex = 0.8, legend = c("stairs (x3 for bikers)", "other paths"),
                     col = c("#b2182b", "#bdbdbd"), lwd = c(3, 1))
    grDevices::dev.off()
  }
  cors <- do.call(rbind, cors); utils::write.csv(cors, actPath("dulnBestVsNew.csv"), row.names = FALSE)
  areaT <- do.call(rbind, areaT); utils::write.csv(areaT, actPath("areaDulnBestVsNew.csv"), row.names = FALSE)
  print(cors, digits = 3, row.names = FALSE)
  logf("figures in %s", actPath("figures"))
}

#### run ####

if(!IS_ACT_WORKER){
  t00 <- proc.time()[["elapsed"]]
  if("survey" %in% ACT_STAGES) stageSurveyAct()
  if("stairs" %in% ACT_STAGES) stageStairs()
  if("fit" %in% ACT_STAGES) stageFitAct()
  if("typed" %in% ACT_STAGES) stageTypedAct()
  if("score" %in% ACT_STAGES) stageScoreAct()
  if("figures" %in% ACT_STAGES) stageFiguresAct()
  if(!identical(ACT_STAGES, "none"))
    logf("tailor_duln_activities: done in %.1f min, output in %s", (proc.time()[["elapsed"]] - t00) / 60, ACT)
}
