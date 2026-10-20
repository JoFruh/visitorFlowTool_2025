#### Map or model? Where DULN fails to explain the surveys ####
#
# Full (DULN everywhere) does no better than Nearby (distance only) in the survey
# validation. This script asks why, without running the ABM, on the runs and inputs
# of data-raw/validate_usage_surveys.R (VFT_VAL_OUT, default the fixed manual-area
# final run):
#
#   A1 destination  An aggregate multinomial logit of the area choice, fitted to the
#                   survey's areas (Wigger, Glatt: each route's main area, the one it
#                   spends the most metres in; GlattPark: the heatmap mass per area,
#                   as quasi-counts). The agents are the engine's own: origins,
#                   outing durations, speeds and types captured from findClosestAOI_cpp()
#                   in a Full run. Distances are real path lengths (trueAreaDistances()).
#                   Nested models: distance only, + DULN, + area, and a saturated one
#                   with a constant per area, each with the reach (the share of the
#                   outing an area may be away) profiled.
#   A1b map gaps    The saturated constants against the area's DULN and against what
#                   DULN may lack (land cover, water close by, hiking trails...).
#   A2 movement     Inside the areas, cell by cell: survey usage against the
#                   simulation, Poisson with offset log(sim) and an area fixed
#                   effect, so the coefficients say what the survey has MORE of than
#                   the simulated walk - DULN, hiking trails, natural surface.
#
#   Rscript data-raw/diagnose_duln.R
#
# VFT_CAL_OUT      output root (default .../pathUsageValidation_2026_calibration);
#                  this writes <root>/diagnosis
# VFT_DIAG_SEEDS   populations captured per context (default 10 at Wigger, 5 elsewhere)

Sys.setenv(VFT_VAL_WORKER = "1")   #definitions only: no main(), no compile
LOCAL <- "C:/Users/frueh/Documents/Local Data"
if(!nzchar(Sys.getenv("VFT_VAL_OUT")))
  Sys.setenv(VFT_VAL_OUT = file.path(LOCAL, "pathUsageValidation_2026_final_manual_fixed"))
Sys.setenv(VFT_VAL_ENGINE = "corrected")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
source(file.path(.repo, "data-raw", "validate_usage_surveys.R"))
CAL  <- Sys.getenv("VFT_CAL_OUT", file.path(LOCAL, "pathUsageValidation_2026_calibration"))
DIAG <- file.path(CAL, "diagnosis")
dir.create(DIAG, recursive = TRUE, showWarnings = FALSE)
LC_DIR <- "C:/Users/frueh/Documents/Local Data/landcover"
REACH_GRID <- c(0.05, 0.10, 0.15, 0.20, 0.25, 0.33, 0.50, 0.75, 1, Inf)
#the quasi-counts GlattPark's heatmap mass is scaled to (it has no routes)
GLATTPARK_N <- 300

#### the agents, as the engine draws them ####

#One Full run per seed, stopped inside the area choice: findClosestAOI_cpp() sees
#every agent's start node, speed, outing duration and type, after the population,
#the parking share and the characteristics are drawn.
capturePop <- function(ctx, seeds){
  f <- file.path(DIAG, paste0("pop_", ctx, ".rds"))
  if(file.exists(f)) return(readRDS(f))
  sc <- scenarioFor(ctx, "full")
  out <- lapply(seeds, function(seed){
    got <- NULL
    nsSet("findClosestAOI_cpp", function(AOIList_o, AOI_v, adj_ptr, src_v, agentSpeeds,
                                         agentDurations, AOI_aois, AOI_dulns, agentTyps, AOI_areas){
      got <<- data.frame(src = as.integer(src_v), speed = agentSpeeds, dur = agentDurations,
                         typ = as.character(agentTyps), seed = seed)
      stop(structure(class = c("vftCaptured", "error", "condition"),
                     list(message = "captured", call = NULL)))
    })
    on.exit(nsSet("findClosestAOI_cpp", ORIG$findClosestAOI_cpp))
    set.seed(seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
    quiet <- file(nullfile(), open = "wt"); sink(quiet)
    tryCatch(ns$vftSimulateScenario(sc$net, sc$polys, sc$thr, parking = sc$parking,
                                    residentDivision = sc$residentDivision,
                                    progPrep = fakeProgress(), progSim = fakeProgress()),
             vftCaptured = function(e) NULL, finally = { sink(); close(quiet) })
    if(is.null(got)) stop(ctx, " seed ", seed, ": the area choice was never reached")
    got
  })
  pop <- do.call(rbind, out)
  D <- trueAreaDistances(ctx, sc$net)
  res <- list(pop = pop, D = D, nodeAOI = igraph::vertex_attr(sc$net, "AOI"))
  saveRDS(res, f)
  res
}

#### the areas and what is in them ####

areaTable <- function(ctx){
  aoi <- readRDS(aoiFile(ctx)); edg <- readRDS(edgesFile(ctx))
  p <- sf::st_drop_geometry(aoi$polys)
  #a letter can have several rows: area-weighted DULN, summed area
  duln <- tapply(p$DULN * p$area, p$AOI, sum) / tapply(p$area, p$AOI, sum)
  area <- tapply(p$area, p$AOI, sum)
  data.frame(AOI = names(duln), DULN = as.numeric(duln), area = as.numeric(area[names(duln)]))
}

#Land cover by random points (1 m ground and canopy), inside each clipped area and in
#a 200 m ring around it; path attributes of the edges inside.
areaCovariates <- function(ctx, net){
  f <- file.path(DIAG, paste0("areacov_", ctx, ".rds"))
  if(file.exists(f)) return(readRDS(f))
  aoi <- readRDS(aoiFile(ctx)); edg <- readRDS(edgesFile(ctx))
  ac <- clipAois(aoi)
  g  <- terra::rast(file.path(LC_DIR, "ground_CH_1m.tif"))
  cn <- terra::rast(file.path(LC_DIR, "canopy_CH_1m.tif"))
  set.seed(7)
  shares <- function(geom, n = 1500){
    pts <- sf::st_sample(geom, n)
    if(!length(pts)) return(c(grass = NA, bush = NA, artificial = NA, natural = NA, water = NA, tree = NA))
    v  <- terra::vect(pts)
    gv <- terra::extract(g, v, ID = FALSE)[[1]]
    cv <- terra::extract(cn, v, ID = FALSE)[[1]]
    c(grass = mean(gv %in% 1), bush = mean(gv %in% 2), artificial = mean(gv %in% 3),
      natural = mean(gv %in% 4), water = mean(gv %in% 5),
      tree = mean(cv %in% c(7, 10:13)))
  }
  inside <- t(vapply(seq_len(nrow(ac)), function(i) shares(sf::st_geometry(ac)[i]), numeric(6)))
  ring <- t(vapply(seq_len(nrow(ac)), function(i){
    gi <- sf::st_geometry(ac)[i]
    shares(sf::st_difference(sf::st_buffer(gi, 200), gi), 800)
  }, numeric(6)))
  colnames(ring) <- paste0("ring_", colnames(ring))
  ea <- igraph::edge_attr(net)
  ed <- edg$edgeAoi
  er <- match(ed$edgeID, ea$edgeID)
  ed$hiking  <- ea$WANDERWEGE[er] %in% 0
  ed$natural <- ea$hardNatur[er] %in% 2
  ed$narrow  <- ea$roadWidth[er] %in% 2
  ed$dulnE   <- ea$DULN_WALK_[er]
  path <- do.call(rbind, lapply(ac$AOI, function(a){
    d <- ed[ed$AOI == a, ]; w <- d$len
    data.frame(AOI = a, pathM_ha = sum(w) / ac$areaHa[ac$AOI == a],
               hiking = if(sum(w)) sum(w * d$hiking) / sum(w) else NA,
               naturalSurf = if(sum(w)) sum(w * d$natural) / sum(w) else NA,
               narrow = if(sum(w)) sum(w * d$narrow) / sum(w) else NA,
               dulnEdge = if(sum(w)) sum(w * d$dulnE) / sum(w) else NA)
  }))
  out <- cbind(data.frame(AOI = ac$AOI, areaHa = ac$areaHa), inside, ring)
  out <- merge(out, path, by = "AOI")
  saveRDS(out, f)
  out
}

#### what the survey chose ####

observedAreas <- function(ctx){
  aoi <- readRDS(aoiFile(ctx)); sv <- readRDS(surveyFile(ctx)); cfg <- CTX[[ctx]]
  ac <- clipAois(aoi)
  if(is.null(sv$LV)){
    G <- buildGrid(ctx, aoi, sv, readRDS(edgesFile(ctx)))
    mass <- vapply(G$aoiCells, function(cl) sum(sv$surveyBase[cl]), 1)
    q <- mass / sum(mass) * GLATTPARK_N
    return(list(primary = q, visit = q, nLines = NA_integer_, nNone = NA_integer_, quasi = TRUE))
  }
  lines <- readSurveyLines(cfg$survey)
  ints <- suppressWarnings(sf::st_intersection(
    sf::st_sf(line = seq_len(nrow(lines)), geometry = sf::st_geometry(lines)), ac[, "AOI"]))
  d <- data.frame(line = ints$line, AOI = ints$AOI, len = as.numeric(sf::st_length(ints)))
  d <- stats::aggregate(len ~ line + AOI, d, sum)
  d <- d[d$len >= VISIT_MIN_M, ]
  d <- d[order(d$line, -d$len), ]
  prim <- d[!duplicated(d$line), ]
  primary <- table(factor(prim$AOI, levels = ac$AOI))
  visit <- stats::setNames(as.numeric(Matrix::colSums(sv$LV)), sv$lvAoi)
  list(primary = stats::setNames(as.numeric(primary), names(primary)), visit = visit,
       nLines = nrow(lines), nNone = nrow(lines) - nrow(prim), quasi = FALSE)
}

#### the aggregate logit ####

design <- function(cap, at){
  A <- intersect(colnames(cap$D), at$AOI)
  pop <- cap$pop
  D <- cap$D[pop$src, A, drop = FALSE]
  t <- D / (pop$speed * 1000)            #hours
  T <- pop$dur / 60                      #hours
  x <- at[match(A, at$AOI), ]
  list(A = A, t = t, T = T, tT = t / T, duln = x$DULN, logArea = log(x$area / 1e4), n = nrow(t))
}

#What a reach fixes, whatever the coefficients: the areas in reach (the real distance,
#the 5-minute floor) and, for agents with none, the capped closest-area fallback of
#the corrected engine - uniform among the areas within FALLBACK_H of the closest, if
#that one is within CORR$fallbackCap of the outing.
reachPre <- function(des, reach){
  lim <- pmax(reach * des$T, FLOOR_H)
  ok  <- is.finite(des$t) & des$t <= lim
  has <- rowSums(ok) > 0
  Ffb <- NULL
  if(any(!has)){
    tt <- des$t[!has, , drop = FALSE]
    tt[!is.finite(tt)] <- Inf
    tmin <- matrixStats::rowMins(tt)
    capOk <- is.finite(tmin) & tmin <= CORR$fallbackCap * des$T[!has]
    Ffb <- ((tt <= tmin + FALLBACK_H) & capOk) * 1
  }
  list(reach = reach, ok = ok, has = has, Ffb = Ffb)
}

#P[agent, area] for one parameter vector
probMatrix <- function(des, b, pre, asc = NULL){
  n <- des$n; k <- length(des$A)
  h <- pre$has
  U <- b[["bT"]] * des$tT[h, , drop = FALSE] + b[["bD"]] * des$t[h, , drop = FALSE] +
    matrix(b[["bQ"]] * des$duln + b[["bA"]] * des$logArea + (if(is.null(asc)) 0 else asc),
           sum(h), k, byrow = TRUE)
  U[!pre$ok[h, , drop = FALSE]] <- -Inf
  E <- matrix(0, n, k)
  E[h, ] <- exp(U - matrixStats::rowMaxs(U))
  if(!is.null(pre$Ffb)) E[!h, ] <- pre$Ffb
  rs <- rowSums(E)
  P <- E / ifelse(rs > 0, rs, 1)
  list(P = P, home = mean(rs == 0))
}

aggLL <- function(Pbar, obs){
  q <- Pbar / sum(Pbar)
  q <- pmax(q, 1e-9); q <- q / sum(q)
  sum(obs * log(q))
}

#fit one model at one reach; `free` names the coefficients estimated, the rest are 0
fitModel <- function(des, obs, free, reach, sat = FALSE, multi = FALSE){
  k <- length(des$A)
  pre <- reachPre(des, reach)
  ref <- which.max(obs)
  np <- length(free) + if(sat) k - 1 else 0
  unpack <- function(th){
    b <- c(bT = 0, bD = 0, bQ = 0, bA = 0)
    b[free] <- th[seq_along(free)]
    asc <- NULL
    if(sat){ asc <- numeric(k); asc[-ref] <- th[length(free) + seq_len(k - 1)] }
    list(b = b, asc = asc)
  }
  nll <- function(th){
    u <- unpack(th)
    pm <- probMatrix(des, u$b, pre, u$asc)
    v <- -aggLL(colMeans(pm$P), obs)
    if(is.finite(v)) v else 1e10
  }
  #several starts: from a single one BFGS can run off along a flat ridge (Glatt's
  #area term went to 521, a deterministic pick of the largest area)
  mkStart <- function(vals){
    s <- numeric(np)
    for(nm in intersect(names(vals), free)) s[match(nm, free)] <- vals[[nm]]
    s
  }
  starts <- list(mkStart(list(bT = -5, bD = -2)), mkStart(list(bD = -5, bA = 1)),
                 mkStart(list(bD = -1, bA = 0.5, bQ = 0.1)), mkStart(list(bT = -2, bD = -10, bA = 1)))
  if(!multi) starts <- starts[1]
  o <- NULL
  for(s in starts){
    oi <- stats::optim(s, nll, method = "BFGS", control = list(maxit = 500, reltol = 1e-10))
    if(is.null(o) || oi$value < o$value) o <- oi
  }
  if(multi && np <= 6){
    oi <- stats::optim(o$par, nll, method = "Nelder-Mead", control = list(maxit = 2000, reltol = 1e-12))
    if(oi$value < o$value) o <- oi
  }
  u <- unpack(o$par)
  pm <- probMatrix(des, u$b, pre, u$asc)
  Pbar <- colMeans(pm$P)
  list(ll = -o$value, np = np, b = u$b, asc = u$asc, reach = reach, Pbar = Pbar,
       home = pm$home, conv = o$convergence,
       r_share = suppressWarnings(stats::cor(Pbar / sum(Pbar), obs / sum(obs))))
}

MODELS <- list(
  M0_distShare = list(free = "bT"),               #distance as a share of the outing
  M0_distAbs   = list(free = "bD"),               #distance in hours
  M0_both      = list(free = c("bT", "bD")),
  M1_duln      = list(free = c("bT", "bD", "bQ")),
  M1_area      = list(free = c("bT", "bD", "bA")),
  M1_dulnArea  = list(free = c("bT", "bD", "bQ", "bA")),
  #without the share-of-outing term: t and t/T trade off against each other with
  #opposite signs from site to site, which cannot transfer; the calibration starts here
  M1c_area     = list(free = c("bD", "bA"), multi = TRUE),
  M1c_dulnArea = list(free = c("bD", "bQ", "bA"), multi = TRUE),
  Msat         = list(free = c("bT", "bD"), sat = TRUE))

#the reference: what the simulated types chose (their `goals`), as shares
simGoalShares <- function(ctx, A){
  aoi <- readRDS(aoiFile(ctx))
  lapply(stats::setNames(c("full", "nearby", "random", "fullRandom"), c("Full", "Nearby", "Random", "Full-Random")), function(t){
    fs <- runFile(ctx, aoi$stamp, t, seq_len(40))
    fs <- fs[file.exists(fs)]
    g <- numeric(length(A)); names(g) <- A
    for(f in fs){
      x <- readRDS(f)$goals
      x <- x[names(x) %in% A]
      g[names(x)] <- g[names(x)] + as.numeric(x)
    }
    g
  })
}

destinationContext <- function(ctx){
  logf("A1 %s: capturing the population", ctx)
  nSeeds <- as.integer(envNum("VFT_DIAG_SEEDS", if(ctx == "wigger") 10 else 5))
  cap <- capturePop(ctx, BASE_SEED + seq_len(nSeeds))
  at  <- areaTable(ctx)
  des <- design(cap, at)
  ob  <- observedAreas(ctx)
  obs <- ob$primary[des$A]; obs[is.na(obs)] <- 0
  lost <- sum(ob$primary) - sum(obs)
  logf("A1 %s: %d agents x %d areas; %.0f routes with a main area (%.0f in areas without nodes, dropped)",
       ctx, des$n, length(des$A), sum(obs), lost)
  fits <- list()
  for(m in names(MODELS)){
    best <- NULL
    #the saturated model at the reach the DULN + area model chose: one constant per
    #area makes the profile slow, and the constants absorb most of what reach does
    grid <- if(isTRUE(MODELS[[m]]$sat)) fits$M1_dulnArea$reach else REACH_GRID
    for(r in grid){
      ft <- fitModel(des, obs, MODELS[[m]]$free, r, sat = isTRUE(MODELS[[m]]$sat),
                     multi = isTRUE(MODELS[[m]]$multi))
      if(is.null(best) || ft$ll > best$ll) best <- ft
    }
    fits[[m]] <- best
    logf("A1 %s %-13s LL %9.2f  reach %-5s  r(shares) %.3f  bT %7.2f bD %7.2f bQ %6.3f bA %6.3f  home %.3f",
         ctx, m, best$ll, format(best$reach), best$r_share, best$b[["bT"]], best$b[["bD"]],
         best$b[["bQ"]], best$b[["bA"]], best$home)
  }
  #the simulated types' own choices, scored the same way
  sg <- simGoalShares(ctx, des$A)
  simRows <- do.call(rbind, lapply(names(sg), function(t) data.frame(
    context = ctx, model = paste0("sim_", t), ll = aggLL(sg[[t]] + 1e-9, obs), np = NA, reach = NA,
    r_share = suppressWarnings(stats::cor(sg[[t]] / sum(sg[[t]]), obs / sum(obs))),
    bT = NA, bD = NA, bQ = NA, bA = NA, home = NA)))
  tab <- do.call(rbind, lapply(names(fits), function(m){ f <- fits[[m]]
    data.frame(context = ctx, model = m, ll = f$ll, np = f$np, reach = f$reach, r_share = f$r_share,
               bT = f$b[["bT"]], bD = f$b[["bD"]], bQ = f$b[["bQ"]], bA = f$b[["bA"]], home = f$home) }))
  tab <- rbind(tab, simRows)
  ll0 <- tab$ll[tab$model == "M0_both"]; llS <- tab$ll[tab$model == "Msat"]
  tab$gapClosed <- (tab$ll - ll0) / (llS - ll0)
  tab$nObs <- sum(obs); tab$quasi <- ob$quasi
  #LR test against distance only (not for GlattPark's quasi-counts)
  tab$p_vs_M0 <- ifelse(tab$np > 2 & !ob$quasi,
                        stats::pchisq(pmax(0, 2 * (tab$ll - ll0)), df = tab$np - 2, lower.tail = FALSE), NA)
  shares <- data.frame(context = ctx, AOI = des$A, observed = obs, obsShare = obs / sum(obs),
                       DULN = des$duln, areaHa = exp(des$logArea),
                       M0 = fits$M0_both$Pbar / sum(fits$M0_both$Pbar),
                       M1 = fits$M1_dulnArea$Pbar / sum(fits$M1_dulnArea$Pbar),
                       simFull = sg$Full / sum(sg$Full), simNearby = sg$Nearby / sum(sg$Nearby),
                       asc = fits$Msat$asc)
  list(tab = tab, shares = shares, des = des, fits = fits)
}

#### map gaps: what the per-area constants follow ####

mapGaps <- function(sh){
  cov <- do.call(rbind, lapply(unique(sh$context), function(ctx)
    cbind(context = ctx, areaCovariates(ctx, readRDS(netFile(ctx))$net))))
  d <- merge(sh, cov[, setdiff(names(cov), "areaHa")], by = c("context", "AOI"))
  #the constants are identified up to a constant per site: centre them, weighted by
  #what the survey saw, and keep areas with at least 3 routes (or quasi-counts)
  d <- d[d$observed >= 3, ]
  d$ascC <- d$asc - ave(d$asc, d$context, FUN = mean)
  vars <- c("DULN", "logArea", "dulnEdge", "grass", "bush", "artificial", "natural", "water",
            "tree", "ring_water", "ring_tree", "ring_artificial", "pathM_ha", "hiking",
            "naturalSurf", "narrow")
  d$logArea <- log(d$areaHa)
  uni <- do.call(rbind, lapply(vars, function(v){
    ok <- is.finite(d[[v]]) & is.finite(d$ascC)
    if(sum(ok) < 6 || stats::sd(d[[v]][ok]) == 0) return(NULL)
    #ascC is centred per site already, so no site term is needed
    m <- stats::lm(stats::as.formula(paste("ascC ~", v)), data = d[ok, ])
    cf <- summary(m)$coefficients
    #and beyond the area's size, the strongest single predictor
    pa <- if(v != "logArea"){
      m2 <- stats::lm(stats::as.formula(paste("ascC ~ logArea +", v)), data = d[ok, ])
      summary(m2)$coefficients[v, c(1, 4)]
    }else c(NA, NA)
    data.frame(var = v, n = sum(ok), rho = stats::cor(d[[v]][ok], d$ascC[ok], method = "spearman"),
               coef = cf[v, 1], p = cf[v, 4], R2 = summary(m)$r.squared,
               coef_givenArea = pa[1], p_givenArea = pa[2])
  }))
  list(data = d, uni = uni[order(uni$p), ])
}

#### A2: movement inside the areas ####

movementContext <- function(ctx){
  cfg <- CTX[[ctx]]
  aoi <- readRDS(aoiFile(ctx)); sv <- readRDS(surveyFile(ctx)); edg <- readRDS(edgesFile(ctx))
  net <- readRDS(netFile(ctx))$net
  G <- buildGrid(ctx, aoi, sv, edg)
  n100 <- terra::ncell(G$t100)
  simSum <- function(t){
    x <- numeric(n100)
    for(k in seq_len(N_REPS)){
      f <- runFile(ctx, aoi$stamp, t, k); if(!file.exists(f)) break
      r <- readRDS(f); x[r$cells] <- x[r$cells] + r$counts
    }
    as.vector(G$P %*% x)
  }
  sims <- lapply(c(nearbyRandom = "nearbyRandom", nearby = "nearby", full = "full"), simSum)
  #edge attributes onto the comparison grid (100 m cells, or 500 m at GlattPark)
  ea <- igraph::edge_attr(net)
  ec <- edg$edgeCells
  baseCell <- if(terra::ncell(G$base) == n100) ec$cell else
    terra::cellFromXY(G$base, terra::xyFromCell(G$t100, ec$cell))
  er  <- match(ec$edgeID, ea$edgeID)
  len <- ea$SHAPE_Leng[er]
  #a per-edge value, length-weighted over the edges of each cell
  agg <- function(v){
    v <- as.numeric(v)[er]
    s <- tapply(len * v, baseCell, sum); w <- tapply(len, baseCell, sum)
    out <- rep(NA_real_, terra::ncell(G$base)); out[as.integer(names(s))] <- s / w; out
  }
  mix <- c(walkNat = 0.232, walkSoc = 0.1667, dogNat = 0.1535, dogProx = 0.1124,
           ebikeNat = 0.089, bikeSport = 0.05937, jogger = 0.2328)
  dulnMix <- Reduce(`+`, Map(function(col, w) w * ea[[col]], DULN_BY_TYPE[names(mix)], mix))
  cellX <- data.frame(dulnMix = agg(dulnMix), hiking = agg(ea$WANDERWEGE %in% 0),
                      naturalSurf = agg(ea$hardNatur %in% 2), narrow = agg(ea$roadWidth %in% 2),
                      pathLen = { s <- tapply(len, baseCell, sum); o <- numeric(terra::ncell(G$base))
                        o[as.integer(names(s))] <- s; o })
  #what the engine's junction choice actually reads: the NODE DULN columns (the
  #per-type rasters sampled at the node, raw scale), same type mix, mean over the
  #nodes in the cell
  va <- igraph::vertex_attr(net)
  nodeMix <- Reduce(`+`, Map(function(col, w) w * va[[col]], DULN_BY_TYPE[names(mix)], mix))
  nc <- terra::cellFromXY(G$base, sf::sf_project("EPSG:4326", "EPSG:2056", cbind(va$X, va$Y)))
  okN <- !is.na(nc) & is.finite(nodeMix)
  sN <- tapply(nodeMix[okN], nc[okN], mean)
  cellX$dulnNode <- NA_real_
  cellX$dulnNode[as.integer(names(sN))] <- sN
  #the cells of the areas the compare stage's movement test uses
  if(!is.null(sv$LV)){
    nRespAoi <- vapply(seq_along(edg$aoiLetters), function(k) length(unique(sv$resp[sv$LV[, k] > 0])), 1L)
    qual <- edg$aoiLetters[nRespAoi >= MIN_RESP_AOI & lengths(G$aoiCells) >= MIN_AOI_CELLS]
  }else{
    massA <- vapply(G$aoiCells, function(cl) sum(sv$surveyBase[cl]), 1)
    qual <- names(G$aoiCells)[massA > 0 & lengths(G$aoiCells) >= MIN_AOI_CELLS]
  }
  rows <- do.call(rbind, lapply(qual, function(a){
    cl <- G$aoiCells[[a]]
    data.frame(AOI = a, cell = cl, survey = sv$surveyBase[cl],
               nr = sims$nearbyRandom[cl], nearby = sims$nearby[cl], full = sims$full[cl], cellX[cl, ])
  }))
  rows <- rows[is.finite(rows$dulnMix), ]
  z <- function(x) (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
  rows <- rows[is.finite(rows$dulnNode), ]
  for(v in c("dulnMix", "dulnNode", "hiking", "naturalSurf", "narrow")) rows[[paste0(v, "_z")]] <- z(rows[[v]])
  fitOne <- function(offCol, form){
    d <- rows[rows[[offCol]] > 0 & rows$survey >= 0, ]
    #only areas where the survey has usage: a fixed effect for an empty area is -Inf
    d <- d[ave(d$survey, d$AOI, FUN = sum) > 0, ]
    m <- stats::glm(stats::as.formula(paste("survey ~ factor(AOI) +", form)),
                    offset = log(d[[offCol]]), family = stats::quasipoisson(), data = d)
    cf <- summary(m)$coefficients
    keep <- grep("_z$", rownames(cf), value = TRUE)
    data.frame(context = ctx, offset = offCol, term = keep, coef = cf[keep, 1], se = cf[keep, 2],
               p = cf[keep, 4], nCells = nrow(d), nAreas = length(unique(d$AOI)))
  }
  forms <- c(duln = "dulnMix_z", all = "dulnMix_z + hiking_z + naturalSurf_z + narrow_z",
             node = "dulnNode_z", nodeAll = "dulnNode_z + hiking_z + naturalSurf_z + narrow_z",
             both = "dulnMix_z + dulnNode_z")
  out <- do.call(rbind, lapply(c("nr", "nearby", "full"), function(o)
    do.call(rbind, lapply(names(forms), function(fm) cbind(model = fm, fitOne(o, forms[[fm]]))))))
  out$offset <- c(nr = "Nearby-Random", nearby = "Nearby", full = "Full")[out$offset]
  list(tab = out, rows = cbind(context = ctx, rows))
}

#### run ####

ctxs <- CONTEXTS
A1 <- lapply(stats::setNames(ctxs, ctxs), destinationContext)
destTab <- do.call(rbind, lapply(A1, `[[`, "tab"))
shares  <- do.call(rbind, lapply(A1, `[[`, "shares"))
utils::write.csv(destTab, file.path(DIAG, "destination_models.csv"), row.names = FALSE)
utils::write.csv(shares, file.path(DIAG, "destination_shares.csv"), row.names = FALSE)
saveRDS(lapply(A1, `[[`, "fits"), file.path(DIAG, "destination_fits.rds"))

logf("A1b: map gaps")
mg <- mapGaps(shares)
utils::write.csv(mg$uni, file.path(DIAG, "mapgaps_constants.csv"), row.names = FALSE)
utils::write.csv(mg$data, file.path(DIAG, "mapgaps_areas.csv"), row.names = FALSE)
print(mg$uni, digits = 3, row.names = FALSE)

logf("A2: movement")
A2 <- lapply(stats::setNames(ctxs, ctxs), movementContext)
moveTab <- do.call(rbind, lapply(A2, `[[`, "tab"))
utils::write.csv(moveTab, file.path(DIAG, "movement_glm.csv"), row.names = FALSE)
utils::write.csv(do.call(rbind, lapply(A2, `[[`, "rows")), file.path(DIAG, "movement_cells.csv"),
                 row.names = FALSE)
options(width = 200)
print(destTab[, c("context", "model", "ll", "np", "reach", "r_share", "bT", "bD", "bQ", "bA",
                  "home", "gapClosed", "p_vs_M0")], digits = 3, row.names = FALSE)
print(moveTab, digits = 3, row.names = FALSE)
logf("diagnosis written to %s", DIAG)
