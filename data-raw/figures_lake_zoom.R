#### The lake shores close up: survey against the simulations ####
#
# For the lake-emphasis test (data-raw/lake_emphasis_duln.R): per site, the 100 m maps
# of the survey and every type, cropped to the lakes of 10 ha or more, and the
# z-difference to the survey; plus, per lake, the share of the usage on its shore zone
# (cells within VFT_LZ_ZONE_M of the water) and the correlation inside that zone.
# Uses figures_misalignment.R's definitions (same values as the compare stage).
#
#   VFT_VAL_OUT=<pair folder> VFT_VAL_MODES=full,fullL2 VFT_VAL_CONTEXTS=glattPark,glatt \
#   Rscript data-raw/figures_lake_zoom.R
#
# VFT_LZ_ZONE_M  the shore zone's width (default 300, the Greifensee loop's reach)
# VFT_LZ_PAD_M   the map's margin round the lakes (default 800)

.repo <- Sys.getenv("VFT_VAL_REPO", "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025")
#figures_misalignment.R's definitions, without its main()
local({
  src <- readLines(file.path(.repo, "data-raw", "figures_misalignment.R"))
  src <- src[!grepl("^main\\(\\)\\s*$", src)]
  eval(parse(text = src), envir = globalenv())
})
ZONE <- envNum("VFT_LZ_ZONE_M", 300)
PAD  <- envNum("VFT_LZ_PAD_M", 800)

rows <- list()
for(ctx in CONTEXTS){
  D <- contextData(ctx)
  lk <- lakesOf(D$aoi)
  if(is.null(lk)) next
  lk <- sf::st_cast(sf::st_union(lk), "POLYGON")
  lk <- lk[as.numeric(sf::st_area(lk)) >= ns$VFT_AOI_LAKE_MIN_M2]
  if(!length(lk)) next
  ac <- D$G$ac
  ctxGeo <- list(cfg = D$cfg, ac = ac, perim = perimeter2056(D$aoi), lakes = lk, nReps = length(D$reps))
  V <- viewValues(D, "1_all")
  pts <- sf::st_as_sf(data.frame(x = V$x, y = V$y), coords = c("x", "y"), crs = 2056)
  dist <- apply(sf::st_distance(pts, lk), 1, min)
  #per lake (touching basins as one): shore share and the zone's r
  zones <- sf::st_cast(sf::st_union(sf::st_buffer(lk, ZONE)), "POLYGON")
  for(z in seq_along(zones)){
    inZ <- lengths(sf::st_intersects(pts, zones[z])) > 0
    if(sum(inZ) < 5) next
    ha <- sum(as.numeric(sf::st_area(lk[lengths(sf::st_intersects(lk, zones[z])) > 0]))) / 1e4
    shareOf <- function(x) sum(x[inZ]) / sum(x)
    rows[[length(rows) + 1]] <- data.frame(context = ctx, lake_ha = round(ha), cells = sum(inZ), type = "Survey",
                                           shore_share = shareOf(V$survey), r_in_zone = NA_real_)
    for(t in names(V$sim))
      rows[[length(rows) + 1]] <- data.frame(context = ctx, lake_ha = round(ha), cells = sum(inZ), type = TYPES[[t]]$label,
                                             shore_share = shareOf(V$sim[[t]]),
                                             r_in_zone = stats::cor(V$sim[[t]][inZ], V$survey[inZ]))
  }
  #the maps, cropped to the lake whose shore zone holds the most survey usage
  zMass <- vapply(seq_along(zones), function(z)
    sum(V$survey[lengths(sf::st_intersects(pts, zones[z])) > 0]), 1)
  bb <- sf::st_bbox(sf::st_buffer(zones[which.max(zMass)], PAD))
  near <- V$x >= bb[["xmin"]] & V$x <= bb[["xmax"]] & V$y >= bb[["ymin"]] & V$y <= bb[["ymax"]]
  crop <- coord_sf(datum = NA, crs = 2056, expand = FALSE, xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")])
  #std01 over the whole site, as the other maps
  panel <- function(v, title) suppressMessages(basePanel(data.frame(x = V$x[near], y = V$y[near], res = V$res, v = v[near]),
                                                         ctxGeo, title, labels = FALSE) + crop)
  top <- c(list(panel(std01(V$survey), "Survey") + seqScale()),
           lapply(names(V$sim), function(t) panel(std01(V$sim[[t]]), TYPES[[t]]$label) + seqScale()))
  lim <- max(unlist(lapply(V$d, function(d) stats::quantile(abs(d[near]), 0.99))), 1)
  bot <- c(list(ggplot() + theme_void()),
           lapply(names(V$d), function(t) panel(V$d[[t]], sprintf("%s minus survey, r = %.3f", TYPES[[t]]$label, V$r[[t]])) +
                    divScale(round(lim, 1), "z difference")))
  p1 <- ggpubr::ggarrange(plotlist = top, nrow = 1, common.legend = TRUE, legend = "bottom")
  p2 <- ggpubr::ggarrange(plotlist = bot, nrow = 1, common.legend = TRUE, legend = "bottom", legend.grob = ggpubr::get_legend(bot[[2]]))
  p <- ggpubr::ggarrange(p1, p2, ncol = 1)
  p <- ggpubr::annotate_figure(p, top = ggpubr::text_grob(sprintf("%s at %d m, round the lakes (dashed), %d replicates",
                                                                  D$cfg$label, V$scale_m, length(D$reps)), size = 9, color = COL$ink))
  n <- length(top)
  f <- outPath("figures", "lakes", sprintf("lake_zoom_%s.png", ctx))
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  ggsave(f, p, width = 6.5 * n, height = 15, units = "cm", dpi = 250, bg = "white")
  logf("lake zoom %s written", ctx)
}
tab <- do.call(rbind, rows)
utils::write.csv(tab, outPath("results", "lake_shore.csv"), row.names = FALSE)
options(width = 160); print(tab, digits = 3, row.names = FALSE)
