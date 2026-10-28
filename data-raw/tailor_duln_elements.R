#### DULN per activity on CLEAN elements: exclusive path, forest and summit classes ####
#
# The per-activity fit (data-raw/tailor_duln_activities.R) gave weights that cannot be
# defended - forests, small rivers and viewpoints negative, hard paths below large
# roads. Neither fitting on area-centred cells nor area intercepts changed that. The
# elements overlap: a path's width (narrowPaths, medPaths, medRoads, largeRoads) and
# its surface (naturePaths, hardPaths) are separate additive elements, forest is a
# 50/100/150 m ring sum with the reserves on top, and viewpoints and peaks share the
# hills. The user's element set (2026-10-21) makes them exclusive:
#   path class per edge, width x surface (the network's OBJEKTART / BELAGSART):
#     road6        6+ m roads (OBJEKTART 3-6, 8, 9, 20)
#     road4_hard   3-4 m roads (10-12), sealed;   road4_nat  natural
#     path2_hard   2 m paths (15, 18), sealed;    path2_nat  natural - THE REFERENCE
#     path1_hard   1 m paths (16, 17), sealed;    path1_nat  natural
#   largeRoadNear  the large roads' noise: min(largeRoads rings, 2) - 2 to 50 m, 1 to 100 m
#   forestReserve  in a cantonal or Pro Natura reserve
#   forestIn       in other forest (TLM3D land cover 12, 13)
#   forestEdge     no forest, no reserve, within 50 m of forest
#   summitsViews   max(viewpoints, peaks) rings
# replacing narrowPaths, medPaths, naturePaths, hardPaths, largeRoads, medRoads,
# forests, forestReserves, forestProNatura, viewpoints and peaks. Every other element,
# the lake band and the stairs rule for bikers stay.
# Edges take their path class from their own attributes (one-hot); nodes and areas
# from 10 m rasters of TLM_STRASSE by class, like the other path elements.
#
# Two fits, both 4 activities as tailor_duln_activities.R: unconstrained, and held to
# the expected signs (SIGN_POS >= 0, SIGN_NEG <= 0) - elements/ and elements/signed/.
#
# Stages (VFT_EL_STAGES):
#   layers   the new rasters on today's layer grid -> layers2_<box>.tif (all layers)
#   extract  the edges' elements + the new ones, the cells' means (as tailor_duln.R)
#   fit      both fits, all Glatt cells and without GlattPark's ground; tables
#   typed, score, figures   tailor_duln_activities.R's, into elements/
# The variants (E2_A4_s0.5 unconstrained, E2c_A4_s0.5 signed) are built on a
# validation folder's areas by data-raw/apply_duln_variant.R with
# VFT_AV_SCRIPT=tailor_duln_elements.R and VFT_AV_TYSUB=elements or elements/signed.
#
#   Rscript data-raw/tailor_duln_elements.R
#
# VFT_EL_STAGES   which stages (default layers, extract, fit)
# VFT_EL_OUT      output (default <duln_tailored>/elements)

IS_EL_WORKER <- identical(Sys.getenv("VFT_TY_WORKER"), "1")
.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
EL_SCRIPT <- file.path(.repo, "data-raw", "tailor_duln_elements.R")
if(!IS_EL_WORKER){
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if(length(f) && grepl("tailor_duln_elements", f[1])) EL_SCRIPT <- normalizePath(f[1], winslash = "/")
}
.elStages <- Sys.getenv("VFT_EL_STAGES", "layers,extract,fit")
Sys.setenv(VFT_ACT_STAGES = "none")
source(file.path(.repo, "data-raw", "tailor_duln_activities.R"))
TY_SCRIPT <- EL_SCRIPT

EL <- Sys.getenv("VFT_EL_OUT", file.path(TD, "elements"))
EL_STAGES <- strsplit(.elStages, ",", fixed = TRUE)[[1]]
elPath <- function(...){ p <- file.path(EL, ...); dir.create(dirname(p[1]), recursive = TRUE, showWarnings = FALSE); p }
#the activity stages (typed, score, figures) write here
ACT <- EL

PATH_CLASSES <- c("road6", "road4_hard", "road4_nat", "path2_hard", "path1_hard", "path1_nat")
PATH_REF <- "path2_nat"
NEW_RASTER <- c("largeRoadNear", "forestReserve", "forestIn", "forestEdge", "summitsViews")
DROP_EL <- c("narrowPaths", "medPaths", "naturePaths", "hardPaths", "largeRoads", "medRoads",
             "forests", "forestReserves", "forestProNatura", "viewpoints", "peaks")
SIGN_POS <- c("forestReserve", "forestIn", "forestEdge", "smallRivers", "lakeNear", "lakeBand", "summitsViews")
SIGN_NEG <- c("road6", "largeRoadNear", "urban", "highways")

#the path class of a TLM_STRASSE line / network edge; motorways (0-2) are the
#highways element, not a path class
pathClassOf <- function(oa, bel){
  oa <- as.integer(oa); nat <- as.integer(bel) %in% 200L
  out <- rep(NA_character_, length(oa))
  out[oa %in% c(3:6, 8, 9, 20)] <- "road6"
  i <- oa %in% c(10:12, 7, 13, 14, 19, 21); out[i] <- ifelse(nat[i], "road4_nat", "road4_hard")
  i <- oa %in% c(15, 18); out[i] <- ifelse(nat[i], "path2_nat", "path2_hard")
  i <- oa %in% c(16, 17); out[i] <- ifelse(nat[i], "path1_nat", "path1_hard")
  out
}

#### the element set, swapped in for tailor_duln_types.R's functions ####

layersFileToday  <- layersFile
extractFileToday <- extractFile
xlsxNewToday     <- xlsxNew
ELEMENTS_TODAY   <- ELEMENTS_NEW
layersFile  <- function(box) elPath("layers", paste0("layers2_", box, ".tif"))
extractFile <- function(ctx) elPath("extract", paste0(ctx, ".rds"))
ELEMENTS_NEW <- function() c(setdiff(ELEMENTS_TODAY(), DROP_EL), PATH_CLASSES, NEW_RASTER)
#a type's weights on the new set: kept elements as the xlsx has them; the new ones
#what today's layer gives them ("xlsx-equivalent"), path classes against the reference
xlsxNew <- function(k){
  v <- xlsxNewToday(k)
  w <- function(e) v[[e]]
  ref <- w("medPaths") + w("naturePaths")
  eq <- c(road6 = 3 * w("largeRoads") - 2 * w("largeRoads") - ref,   #on the road; largeRoadNear adds 2 x
          road4_hard = 2 * w("medRoads") - ref, road4_nat = 2 * w("medRoads") - ref,
          path2_hard = w("medPaths") + w("hardPaths") - ref,
          path1_hard = w("narrowPaths") + w("hardPaths") - ref, path1_nat = w("narrowPaths") + w("naturePaths") - ref,
          largeRoadNear = w("largeRoads"),
          forestReserve = 3 * w("forests") + w("forestReserves"), forestIn = 3 * w("forests"),
          forestEdge = 3 * w("forests"), summitsViews = (w("viewpoints") + w("peaks")) / 2)
  out <- c(v[setdiff(names(v), DROP_EL)], eq)
  out[ELEMENTS_NEW()]
}

#### stage layers ####

stageLayers2 <- function(box){
  f <- layersFile(box)
  if(file.exists(f)) return(invisible(NULL))
  t0 <- proc.time()[["elapsed"]]
  X <- terra::rast(layersFileToday(box))
  tm <- X[[1]]
  ev <- as.vector(terra::ext(X))
  wkt <- sf::st_as_text(sf::st_as_sfc(sf::st_bbox(c(xmin = ev[["xmin"]], ymin = ev[["ymin"]], xmax = ev[["xmax"]],
                                                    ymax = ev[["ymax"]]), crs = sf::st_crs(2056))))
  rd <- function(q){ x <- sf::st_read(STAIR_GDB, query = q, wkt_filter = wkt, quiet = TRUE)
    g <- sf::st_zm(sf::st_geometry(x)); sf::st_crs(g) <- 2056; sf::st_set_geometry(sf::st_drop_geometry(x), g) }
  #path classes: TLM_STRASSE without tunnels (as largeRoads)
  st <- rd("SELECT * FROM TLM_STRASSE")
  st <- st[!st$KUNSTBAUTE %in% 1000, ]
  cls <- pathClassOf(st$OBJEKTART, st$BELAGSART)
  v <- terra::vect(st)
  P <- lapply(c(PATH_CLASSES, PATH_REF), function(k){
    r <- if(any(cls %in% k)) terra::rasterize(v[which(cls %in% k)], tm, field = 1, background = 0, touches = TRUE) else tm * 0
    names(r) <- k; r
  })
  km <- tapply(as.numeric(sf::st_length(st)), addNA(factor(cls)), sum) / 1000
  logf("layers2 %s: path classes (km): %s", box, paste(sprintf("%s %.0f", names(km), km), collapse = ", "))
  #forest: land cover 12, 13; reserves from today's rasters
  fo <- rd("SELECT * FROM TLM_BODENBEDECKUNG WHERE OBJEKTART IN ('12', '13')")
  forest <- terra::rasterize(terra::vect(fo), tm, field = 1, background = 0) > 0
  reserve <- (X[["forestReserves"]] > 0) | (X[["forestProNatura"]] > 0)
  k <- terra::focalMat(tm, 50, "circle"); k[k > 0] <- 1; k[k == 0] <- NA
  near <- terra::focal(forest, k, fun = "max", na.rm = TRUE) > 0
  fR <- reserve * 1; names(fR) <- "forestReserve"
  fI <- (forest & !reserve) * 1; names(fI) <- "forestIn"
  fE <- (near & !forest & !reserve) * 1; names(fE) <- "forestEdge"
  sv <- max(X[["viewpoints"]], X[["peaks"]]); names(sv) <- "summitsViews"
  ln <- terra::clamp(X[["largeRoads"]], upper = 2, values = TRUE); names(ln) <- "largeRoadNear"
  ha <- function(r) sum(terra::values(r, mat = FALSE), na.rm = TRUE) * prod(terra::res(tm)) / 1e4
  logf("layers2 %s: forest reserve %.0f ha, other forest %.0f ha, forest edge (50 m) %.0f ha", box, ha(fR), ha(fI), ha(fE))
  terra::writeRaster(c(X, terra::rast(P), ln, fR, fI, fE, sv), f, overwrite = TRUE,
                     gdal = c("COMPRESS=DEFLATE", "TILED=YES", "BIGTIFF=YES"), datatype = "FLT4S")
  logf("layers2 %s: written, %.0f s", box, proc.time()[["elapsed"]] - t0)
}

#### stage extract ####

#the cells' elements: the length-weighted mean over the edges each holds, as
#tailor_duln.R's stageExtract() (the same cells: inside the areas and the perimeter,
#no lakes)
cellMeans <- function(ctx, E, ea){
  aoi <- readRDS(aoiFile(ctx)); sv <- readRDS(surveyFile(ctx)); edg <- readRDS(edgesFile(ctx))
  G <- buildGrid(ctx, aoi, sv, edg)
  ec <- edg$edgeCells
  er <- match(ec$edgeID, ea$edgeID)
  baseCell <- if(terra::ncell(G$base) == terra::ncell(G$t100)) ec$cell else
    terra::cellFromXY(G$base, terra::xyFromCell(G$t100, ec$cell))
  len <- ea$SHAPE_Leng[er]
  keep <- !is.na(baseCell) & !is.na(er)
  S <- rowsum(E[er[keep], , drop = FALSE] * len[keep], baseCell[keep])
  W <- rowsum(len[keep], baseCell[keep])
  Xc <- S / as.vector(W)
  cells <- as.integer(rownames(Xc))
  inMask <- G$mBase[cells] & !(cells %in% sv$lakeCells)
  list(X = Xc[inMask, , drop = FALSE], cells = cells[inMask])
}

stageExtract2 <- function(ctx){
  f <- extractFile(ctx)
  if(file.exists(f)) return(invisible(NULL))
  t0 <- proc.time()[["elapsed"]]
  ex <- readRDS(extractFileToday(ctx))
  net <- readRDS(netFile(ctx))$net
  ea <- igraph::edge_attr(net)
  stopifnot(identical(ea$edgeID, ex$edgeID))
  cls <- pathClassOf(ea$OBJEKTART, ea$BELAGSART)
  #edges of OBJEKTART outside pathClassOf (Glatt has none; e.g. 22, 23 at Monte Bre and
  #Dietschiberg) keep their area elements and get no path-class weight; logged below
  unk <- is.na(cls)
  P <- sapply(c(PATH_CLASSES, PATH_REF), function(k) as.numeric(cls == k))
  P[is.na(P)] <- 0
  stopifnot(all(rowSums(P) == !unk))
  g <- edgeGeom2056(net)
  Xn <- terra::rast(layersFile(BOX_OF[[ctx]]))[[NEW_RASTER]]
  En <- as.matrix(exactextractr::exact_extract(Xn, sf::st_buffer(g, 0.5), "mean", progress = FALSE))
  colnames(En) <- sub("^mean\\.", "", colnames(En))
  En[!is.finite(En)] <- 0
  E2 <- cbind(ex$E, P, En)
  #the aggregation reproduces today's cells first
  cm0 <- cellMeans(ctx, ex$E, ea)
  if(!identical(cm0$cells, ex$cells) || !isTRUE(all.equal(unname(cm0$X[, colnames(ex$X)]), unname(ex$X))))
    stop(ctx, ": the cell aggregation does not reproduce the stored extract")
  cm <- cellMeans(ctx, E2, ea)
  out <- ex; out$E <- E2; out$X <- cm$X
  saveRDS(out, f)
  km <- tapply(ea$SHAPE_Leng, cls, sum) / 1000
  logf("extract2 %s: %d edges, path classes (km) %s; cells reproduce today's extract; %.0f s", ctx, nrow(E2),
       paste(sprintf("%s %.0f", names(km), km), collapse = ", "), proc.time()[["elapsed"]] - t0)
  if(any(unk)) logf("extract2 %s: %d edges without a path class, OBJEKTART %s, %.2f km", ctx, sum(unk),
                    paste(sort(unique(ea$OBJEKTART[unk])), collapse = ", "), sum(ea$SHAPE_Leng[unk]) / 1000)
}

#### stage fit ####

elFitDir <- function(signed) if(signed) elPath("signed") else EL
stageFitEl <- function(){
  sa <- stageSurveyAct()
  nr4 <- readRDS(file.path(TD, "activities", "fit_types.rds"))$nrMaps
  ex <- readRDS(extractFile("glatt"))
  fits <- list()
  for(signed in c(FALSE, TRUE)){
    d <- elFitDir(signed); f <- file.path(d, "fit_types.rds")
    if(file.exists(f)){ fits[[as.character(signed)]] <- readRDS(f); next }
    b <- if(signed) list(pos = SIGN_POS, neg = SIGN_NEG) else NULL
    lab <- if(signed) "clean elements, signed" else "clean elements"
    all  <- fitTypes(ex, sa, nr4, paste0(lab, ", all Glatt cells"), types = ACT4, bounds = b)
    noGP <- fitTypes(dropGround(ex, "glattPark"), sa, nr4, paste0(lab, ", without GlattPark's ground"), types = ACT4, bounds = b)
    all$actOf <- noGP$actOf <- ACT_OF
    res <- list(all = all, noGP = noGP, nrMaps = nr4)
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    saveRDS(res, f)
    file.copy(file.path(TD, "types", "survey_types.csv"), file.path(d, "survey_types.csv"), overwrite = TRUE)
    fits[[as.character(signed)]] <- res
  }
  #the weights, DULN units at s0.5 (the layers'), against the act4 fit on today's elements
  act <- readRDS(file.path(TD, "activities", "fit_types.rds"))
  exT <- readRDS(extractFileToday("glatt"))
  wt <- do.call(rbind, lapply(c("all", "noGP"), function(fu){
    tu <- typeWeights(fits[["FALSE"]][[fu]], ex); ts <- typeWeights(fits[["TRUE"]][[fu]], ex)
    #act4 on today's set (today's functions)
    ta <- local({ xlsxNew <- xlsxNewToday; ELEMENTS_NEW <- ELEMENTS_TODAY
      environment(typeWeights) <- environment(); typeWeights(act[[fu]], exT) })
    do.call(rbind, lapply(TYPES7, function(k){
      el <- ELEMENTS_NEW()
      man <- TY_VARIANTS$E2_A4_s0.5$manual[[k]]
      u <- S_TY * tu$perType[[k]][el]; s <- S_TY * ts$perType[[k]][el]
      if(length(man)){ u[names(man)] <- man; s[names(man)] <- man }
      data.frame(fit = fu, type = k, activity = ACT_OF[[k]], element = el, xlsxEq = unname(xlsxNew(k)[el]),
                 act4 = unname((S_TY * ta$perType[[k]])[el]), new = unname(u), signed = unname(s),
                 manual = el %in% names(man), identified = el %in% fits[["FALSE"]][[fu]]$ident)
    }))
  }))
  utils::write.csv(wt, elPath("element_weights.csv"), row.names = FALSE)
  cvt <- do.call(rbind, lapply(c("all", "noGP"), function(fu) rbind(
    data.frame(fit = fu, set = "today's elements (act4)", group = "shared", cv = min(act[[fu]]$cvShared)),
    data.frame(fit = fu, set = "clean elements", group = "shared", cv = min(fits[["FALSE"]][[fu]]$cvShared)),
    data.frame(fit = fu, set = "clean elements, signed", group = "shared", cv = fits[["TRUE"]][[fu]]$cvSharedBox),
    do.call(rbind, lapply(ACT4, function(a) rbind(
      data.frame(fit = fu, set = "today's elements (act4)", group = a, cv = act[[fu]]$dev[[a]]$cv),
      data.frame(fit = fu, set = "clean elements", group = a, cv = fits[["FALSE"]][[fu]]$dev[[a]]$cv),
      data.frame(fit = fu, set = "clean elements, signed", group = a, cv = fits[["TRUE"]][[fu]]$dev[[a]]$cvBox)))))))
  utils::write.csv(cvt, elPath("element_cv.csv"), row.names = FALSE)
  #each element's marginal log(survey / Nearby-Random) at Glatt, all activities
  y <- Reduce(`+`, sa$base)[ex$cells]; nr <- Reduce(`+`, nr4)[ex$cells]; nr <- nr / sum(nr) * sum(y)
  mg <- do.call(rbind, lapply(ELEMENTS_NEW(), function(e){
    p <- ex$X[, e] > 0
    data.frame(element = e, shareCells = mean(p), logRatioWith = log(sum(y[p]) / sum(nr[p])),
               logRatioWithout = if(all(p)) NA else log(sum(y[!p]) / sum(nr[!p])))
  }))
  utils::write.csv(mg, elPath("element_marginal.csv"), row.names = FALSE)
  options(width = 200)
  logf("CV deviance (lower is better):"); print(cvt, digits = 4, row.names = FALSE)
  logf("weights of the Nature walkers (s0.5):")
  print(wt[wt$fit == "all" & wt$type == "walkNat", c("element", "xlsxEq", "act4", "new", "signed", "identified")], digits = 2, row.names = FALSE)
  logf("weights of the Sports bikers (s0.5):")
  print(wt[wt$fit == "all" & wt$type == "bikeSport", c("element", "xlsxEq", "act4", "new", "signed")], digits = 2, row.names = FALSE)
}

#### the variants ####

TY_VARIANTS <- list(
  E2_A4_s0.5 = list(kind = "types", typeArea = FALSE,
                    manual = list(bikeSport = c(stairs = -10), ebikeNat = c(stairs = -10)),
                    postNet = stairsPostNet),
  E2c_A4_s0.5 = list(kind = "types", typeArea = FALSE, fit = "signed",
                     manual = list(bikeSport = c(stairs = -10), ebikeNat = c(stairs = -10)),
                     postNet = stairsPostNet))

#### run ####

if(!IS_EL_WORKER){
  t00 <- proc.time()[["elapsed"]]
  if("layers" %in% EL_STAGES) for(box in unique(BOX_OF)) stageLayers2(box)
  if("extract" %in% EL_STAGES) for(ctx in CONTEXTS) stageExtract2(ctx)
  if("fit" %in% EL_STAGES) stageFitEl()
  if("typed" %in% EL_STAGES) stageTypedAct()
  if("score" %in% EL_STAGES) stageScoreAct()
  if("figures" %in% EL_STAGES) stageFiguresAct()
  if(!identical(EL_STAGES, "none"))
    logf("tailor_duln_elements: done in %.1f min, output in %s", (proc.time()[["elapsed"]] - t00) / 60, EL)
}
