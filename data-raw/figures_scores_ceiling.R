#### Four agreement scores per simulation type, against the noise ceiling ####
#
# Reads the results/ of finished validate_usage_surveys.R runs (one per area set)
# and draws Pearson r, Spearman rho, AUC and contradicted presence for Full,
# Nearby, Nearby-Random and Full-Random, at 100 m and 500 m, with the noise
# ceiling as a dashed grey line:
#   Pearson, Spearman   r_max of ceiling.csv (Full): split-half reliabilities of
#                       survey and simulation, Spearman-Brown corrected
#   AUC, contradicted   the survey against itself, half the respondents against
#                       the other half (spatial_agreement.csv) - for contradicted
#                       presence it is the level a perfect model would still show
# GlattPark's survey is a 500 m heatmap without respondent tracks: no 100 m, no
# ceiling, no contradicted presence. Pearson and Spearman carry the paired
# bootstrap's 95 % interval; AUC and contradicted presence are point estimates over
# all replicates.
#
#   Rscript data-raw/figures_scores_ceiling.R
#
# VFT_SC_DIRS  label=path,label=path (default: automatic and manual final areas)
# VFT_SC_OUT   folder of the side-by-side figure (each set's own figure goes to its
#              figures/)

suppressPackageStartupMessages(library(ggplot2))
LOCAL <- "C:/Users/frueh/Documents/Local Data"
dirs <- local({
  v <- Sys.getenv("VFT_SC_DIRS", "")
  if(nzchar(v)){ v <- strsplit(v, ",", fixed = TRUE)[[1]]
    stats::setNames(sub("^[^=]*=", "", v), sub("=.*$", "", v)) }
  else c(`Automatic areas` = file.path(LOCAL, "pathUsageValidation_2026_final"),
         `Manual areas` = file.path(LOCAL, "pathUsageValidation_2026_final_manual"))
})
OUTDIR <- Sys.getenv("VFT_SC_OUT", file.path(LOCAL, "pathUsageValidation_2026_final_manual", "scores_ceiling"))
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

TYPES  <- c(Full = "Full\nsim.", Nearby = "Near-\nby", `Nearby-Random` = "Nearby-\nrandom",
            `Full-Random` = "Full\nrandom")
SCORES <- c(pearson = "Pearson r", spearman = "Spearman \u03c1", auc = "AUC",
            contra = "Contradicted presence\n(lower = better)")
#column order of the panels: site, then scale
PANELS <- c("glattPark|500" = "GlattPark\n500 m", "wigger|100" = "Wigger\n100 m",
            "wigger|500" = "Wigger\n500 m", "glatt|100" = "Glatt\n100 m", "glatt|500" = "Glatt\n500 m")
SET_COL   <- c("#2a78d6", "#eb6834")   #categorical slots 1, 2 (validated, light surface)
SET_SHAPE <- c(16, 17)
CEIL_COL  <- "grey55"

readSet <- function(lab, dir){
  rd  <- function(f) utils::read.csv(file.path(dir, "results", f))
  cor <- rd("correlations_long.csv")
  cor <- cor[cor$test == "map" & cor$cellset == "all" & grepl("_all$", cor$view) &
               cor$method %in% c("pearson", "spearman"), ]
  sa  <- rd("spatial_agreement.csv")
  ce  <- tryCatch(rd("ceiling.csv"), error = function(e) NULL)
  pts <- rbind(
    data.frame(context = cor$context, type = cor$type, scale_m = cor$scale_m, score = cor$method,
               value = cor$point, lo = cor$ci_lo, hi = cor$ci_hi),
    do.call(rbind, lapply(c("auc", "contra"), function(s)
      data.frame(context = sa$context, type = sa$type, scale_m = sa$scale_m, score = s,
                 value = sa[[s]], lo = NA_real_, hi = NA_real_))))
  #the views of the old scale and 100 m (GlattPark's view 1 is 500 m)
  key <- paste(pts$context, pts$scale_m, sep = "|")
  ref <- pts[pts$type == "Survey half vs half" & pts$score %in% c("auc", "contra") & key %in% names(PANELS), ]
  pts <- pts[pts$type %in% names(TYPES) & key %in% names(PANELS), ]
  ceil <- ref[c("context", "scale_m", "score", "value")]
  if(!is.null(ce)){
    ce <- ce[ce$type == "Full" & grepl("_all$", ce$view), ]
    #view "<f>_all": f x 100 m (only Wigger and Glatt have a ceiling, both on 100 m cells)
    ceil <- rbind(ceil, data.frame(context = ce$context, scale_m = 100 * as.numeric(sub("_all$", "", ce$view)),
                                   score = ce$method, value = ce$r_max))
    ceil <- ceil[paste(ceil$context, ceil$scale_m, sep = "|") %in% names(PANELS), ]
  }
  pts$set <- lab; ceil$set <- lab
  list(pts = pts, ceil = ceil)
}
sets <- lapply(names(dirs), function(l) readSet(l, dirs[[l]]))
pts  <- do.call(rbind, lapply(sets, `[[`, "pts"))
ceil <- do.call(rbind, lapply(sets, `[[`, "ceil"))
tidy <- function(d){
  d$panel <- factor(unname(PANELS[paste(d$context, d$scale_m, sep = "|")]), levels = PANELS)
  d$score <- factor(unname(SCORES[d$score]), levels = SCORES)
  d$set   <- factor(d$set, levels = names(dirs))
  if(!is.null(d$type)) d$type <- factor(unname(TYPES[d$type]), levels = TYPES)
  d
}
#the plotted values, with plain keys (the type and score labels carry line breaks)
o <- tidy(pts)
utils::write.csv(pts[order(o$set, o$score, o$panel, o$type),
                     c("set", "context", "scale_m", "score", "type", "value", "lo", "hi")],
                 file.path(OUTDIR, "scores_ceiling.csv"), row.names = FALSE)
o <- tidy(ceil)
utils::write.csv(ceil[order(o$set, o$score, o$panel), c("set", "context", "scale_m", "score", "value")],
                 file.path(OUTDIR, "scores_ceiling_reference.csv"), row.names = FALSE)
pts <- tidy(pts); ceil <- tidy(ceil)

#panels without a value (GlattPark: no respondent tracks)
gaps <- local({
  g <- expand.grid(panel = levels(pts$panel), score = levels(pts$score), stringsAsFactors = FALSE)
  have <- unique(paste(pts$panel[!is.na(pts$value)], pts$score[!is.na(pts$value)]))
  g <- g[!paste(g$panel, g$score) %in% have, ]
  g$panel <- factor(g$panel, levels = levels(pts$panel)); g$score <- factor(g$score, levels = levels(pts$score))
  g
})
#one y range per score row, shared by every figure, so the sets compare across files
yr <- do.call(rbind, lapply(levels(pts$score), function(s){
  v <- c(pts$value[pts$score == s], pts$lo[pts$score == s], pts$hi[pts$score == s], ceil$value[ceil$score == s])
  v <- v[is.finite(v)]
  #AUC from chance (0.5), the others from 0
  lo <- if(s == SCORES[["auc"]]) min(0.5, min(v)) else min(0, min(v))
  data.frame(score = factor(s, levels = levels(pts$score)), y = c(lo, min(1, max(v) * 1.05)))
}))

caption <- paste0(
  "Grey dashed: noise ceiling - Pearson/Spearman: r_max from split-half reliabilities of survey and simulation; ",
  "AUC/contradicted presence: the survey against itself (half vs half).\n",
  "GlattPark: 500 m heatmap survey without respondent tracks, so no ceiling and no contradicted presence.\n",
  "Bars: paired-bootstrap 95 % interval over replicates and respondents (Pearson, Spearman); AUC and contradicted ",
  "presence are point estimates. The AUC axis starts at chance (0.5).\n",
  "Contradicted presence at 500 m uses the busiest 100 m cell's outing share per block (a lower bound).")

base <- function(d, ce, nSets){
  w   <- if(nSets > 1) 0.5 else 0
  pd  <- position_dodge(width = w)
  #the ceiling as a dashed segment over each type's slot of its set, so with two sets
  #every set's points sit under their own ceiling
  ceT <- merge(ce, data.frame(type = factor(TYPES, levels = TYPES)), by = NULL)
  si  <- as.integer(ceT$set); slot <- if(nSets > 1) w / nSets else 0.8
  ceT$x <- as.integer(ceT$type) + if(nSets > 1) (si - (nSets + 1) / 2) * slot else 0
  d$x <- as.integer(d$type)
  gaps$label <- "n/a"
  ggplot(d, aes(x, value, colour = set, shape = set, group = set)) +
    geom_blank(data = yr, aes(x = 1, y = y), inherit.aes = FALSE) +
    geom_segment(data = ceT, aes(x = x - slot / 2, xend = x + slot / 2, y = value, yend = value),
                 inherit.aes = FALSE, colour = CEIL_COL, linetype = "22", linewidth = 0.6) +
    geom_linerange(aes(ymin = lo, ymax = hi), position = pd, linewidth = 0.5, na.rm = TRUE) +
    geom_point(position = pd, size = 2.2, na.rm = TRUE) +
    geom_text(data = gaps, aes(x = 2.5, y = -Inf, label = label), inherit.aes = FALSE,
              vjust = -1.2, size = 2.8, colour = "grey45") +
    facet_grid(score ~ panel, scales = "free_y", switch = "y") +
    scale_x_continuous(breaks = seq_along(TYPES), labels = unname(TYPES), expand = expansion(add = 0.5)) +
    scale_colour_manual(values = stats::setNames(SET_COL[seq_len(nSets)], levels(d$set)), name = NULL) +
    scale_shape_manual(values = stats::setNames(SET_SHAPE[seq_len(nSets)], levels(d$set)), name = NULL) +
    labs(x = NULL, y = NULL, caption = caption) +
    theme_minimal(base_size = 9) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
          panel.border = element_rect(fill = NA, colour = "grey80", linewidth = 0.3),
          strip.placement = "outside", strip.text = element_text(face = "bold", size = 8.5),
          strip.text.y.left = element_text(angle = 90),
          axis.text.x = element_text(size = 6.5, lineheight = 0.85), legend.position = "top",
          plot.caption = element_text(hjust = 0, size = 6.5, colour = "grey35", lineheight = 1.05),
          plot.title = element_text(face = "bold", size = 11))
}

#one figure per area set, into that set's figures/ (and the side-by-side folder)
for(s in names(dirs)){
  d <- droplevels(pts[pts$set == s, ]); d$set <- factor(d$set, levels = s)
  ce <- ceil[ceil$set == s, ]; ce$set <- factor(ce$set, levels = s)
  p <- base(d, ce, 1) + ggtitle(paste0("Survey agreement per simulation type - ", tolower(s))) +
    theme(legend.position = "none")
  slug <- gsub("[^a-z]+", "_", tolower(s))
  for(o in unique(c(file.path(dirs[[s]], "figures"), OUTDIR)))
    ggsave(file.path(o, paste0("fig_scores_ceiling_", slug, ".png")), p, width = 8.5, height = 7.6, dpi = 300, bg = "white")
}
if(length(dirs) > 1){
  p <- base(pts, ceil, length(dirs)) + ggtitle("Survey agreement per simulation type - area sets side by side")
  ggsave(file.path(OUTDIR, "fig_scores_ceiling_sets.png"), p, width = 9, height = 7.9, dpi = 300, bg = "white")
}
message("written to ", OUTDIR)
