#### The agreement scores of every simulation type, current engine vs recommended ####
#
# Reads results/spatial_agreement.csv of two runs of validate_usage_surveys.R (all
# five types, full tracks) and draws, per context, each score for each type and
# engine, with the survey's own half-vs-half level (or, for concentration, the
# survey's own value) as the reference.
#
#   Rscript data-raw/compare_types_scores.R
#
# VFT_TYPES_DIRS overrides the two folders as label=path,label=path.

suppressPackageStartupMessages(library(ggplot2))
LOCAL <- "C:/Users/frueh/Documents/Local Data"
dirs <- local({
  v <- Sys.getenv("VFT_TYPES_DIRS", "")
  if(nzchar(v)){ v <- strsplit(v, ",", fixed = TRUE)[[1]]
    stats::setNames(sub("^[^=]*=", "", v), sub("=.*$", "", v)) }
  else c(`current engine` = file.path(LOCAL, "pathUsageValidation_2026_types_engine"),
         `recommended (straight-on, alpha 5, capped real reach)` = file.path(LOCAL, "pathUsageValidation_2026_types_recommended"))
})
NOTE   <- Sys.getenv("VFT_TYPES_NOTE", "")
OUTDIR <- Sys.getenv("VFT_TYPES_OUT", file.path(LOCAL, "pathUsageValidation_2026_types_compare"))
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

d <- do.call(rbind, lapply(names(dirs), function(e){
  x <- utils::read.csv(file.path(dirs[[e]], "results", "spatial_agreement.csv"))
  x$engine <- e; x
}))
#the survey reference is the same in both runs
ref <- d[d$type == "Survey half vs half" & d$engine == names(dirs)[1], ]
d <- d[d$type != "Survey half vs half", ]
utils::write.csv(d, file.path(OUTDIR, "types_scores.csv"), row.names = FALSE)

TYPE_ORDER <- c("Full", "Nearby", "Nearby-Random", "Random", "Full-Random")
SCORES <- c(pearson = "Pearson r", hit = "hit\n(survey use covered)", auc = "AUC\n(used vs unused)",
            boyce = "Boyce index", contra = "contradicted presence\n(lower = better)",
            top5 = "busiest 5 % of cells\n(target: survey)")
LABEL <- c(glattPark = "GlattPark", wigger = "Wigger", glatt = "Glatt wider region")
ENG_COL <- stats::setNames(c("#9a9994", "#2a78d6"), names(dirs))

for(ctx in intersect(names(LABEL), unique(d$context))){
  x <- d[d$context == ctx, ]
  long <- do.call(rbind, lapply(names(SCORES), function(m) data.frame(
    type = x$type, engine = x$engine, scale = paste(x$scale_m, "m"), score = SCORES[[m]], value = x[[m]])))
  long <- long[is.finite(long$value), ]
  long$type <- factor(long$type, levels = rev(TYPE_ORDER))
  long$score <- factor(long$score, levels = SCORES)
  r <- ref[ref$context == ctx, ]
  refs <- do.call(rbind, lapply(names(SCORES), function(m){
    if(m == "top5"){
      s <- x[x$type == "Full" & x$engine == names(dirs)[1], ]
      return(data.frame(scale = paste(s$scale_m, "m"), score = SCORES[[m]], value = s$top5_survey))
    }
    if(!nrow(r)) return(NULL)
    data.frame(scale = paste(r$scale_m, "m"), score = SCORES[[m]], value = r[[m]])
  }))
  refs <- refs[is.finite(refs$value), ]
  refs$score <- factor(refs$score, levels = SCORES)
  p <- ggplot(long, aes(value, type)) +
    geom_vline(data = refs, aes(xintercept = value), colour = "#52514e", linetype = "22", linewidth = 0.35) +
    geom_point(aes(colour = engine), size = 2, alpha = 0.9) +
    scale_colour_manual(values = ENG_COL, name = NULL) +
    facet_grid(scale ~ score, scales = "free_x") +
    labs(x = NULL, y = NULL, title = sprintf("%s: agreement with the survey by simulation type%s", LABEL[[ctx]],
                                             if(nzchar(NOTE)) paste0(" (", NOTE, ")") else ""),
         caption = "Dashed: the survey against itself (half the respondents vs the other half); for concentration, the survey's own value.") +
    theme_bw(base_size = 8) +
    theme(legend.position = "bottom", strip.text = element_text(size = 7), panel.grid.minor = element_blank(),
          plot.title = element_text(size = 9.5), plot.caption = element_text(size = 6.5, colour = "#52514e"))
  ggsave(file.path(OUTDIR, sprintf("fig_types_scores_%s.png", ctx)), p, width = 26,
         height = 4 + 4 * length(unique(long$scale)), units = "cm", dpi = 300, bg = "white")
}
num <- vapply(d, is.numeric, TRUE); d[num] <- lapply(d[num], function(v) round(v, 3))
print(d[order(d$context, d$view, d$engine, match(d$type, TYPE_ORDER)),
        c("context", "view", "engine", "type", "pearson", "hit", "auc", "boyce", "contra", "top5", "top5_survey")],
      row.names = FALSE)
