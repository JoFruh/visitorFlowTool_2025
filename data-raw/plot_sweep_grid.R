#### The survey sweep's grid: rows = the DULN tailoring, columns = the survey site ####
#
# Each row is one pair folder of data-raw/validate_usage_surveys.R's compare stage (run with
# VFT_VAL_SCALES_M=100,200,300,400,500): a DULN tailored on one survey (or on the Strava heat),
# simulated at the three sites. A panel shows Pearson r (all cells) against the cell size for
# Full (by its area-choice calibration), Nearby, Nearby-Random and Full-Random, with
#   black, solid   a perfect simulation's r: sqrt of the survey's split-half reliability
#   black, dashed  the survey's half vs half r
# GlattPark's survey is a 500 m grid, so its column has that one cell size.
#
#   Rscript data-raw/plot_sweep_grid.R <out dir> "<row label>=<pair folder>[=<fitted site>]" ...
#
# <fitted site>: the context (glattPark, wigger, glatt) whose own survey the row's DULN was
# fitted on - its panel is marked "DULN fitted here".

args <- commandArgs(TRUE)
if(length(args) < 2) stop("plot_sweep_grid.R <out dir> <label>=<pair folder>[=<fitted site>] ...")
OUT <- args[1]
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
rows <- lapply(strsplit(args[-1], "=", fixed = TRUE), function(x) list(label = x[1], dir = x[2], fitted = if(length(x) > 2) x[3] else NA))
suppressPackageStartupMessages(library(ggplot2))

SITES <- c(glattPark = "GlattPark", wigger = "Wigger", glatt = "Glatt")
#the models, in a fixed order: colour and point shape follow the model, never its rank
MODELS <- c("Full (Glatt calibration)", "Full (GlattPark calibration)", "Nearby", "Nearby-Random", "Full-Random")
COLS   <- stats::setNames(c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4"), MODELS)
SHAPES <- stats::setNames(c(16, 17, 15, 18, 4), MODELS)
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e1e0d9"

rd <- function(r){
  m <- utils::read.csv(file.path(r$dir, "results", "correlations_long.csv"), check.names = FALSE)
  m <- m[m$test == "map" & m$cellset %in% "all" & m$method == "pearson" & grepl("^[0-9]+_all$", m$view), ]
  if(length(bad <- setdiff(unique(m$type), MODELS))) stop(r$dir, ": unexpected model(s) ", paste(bad, collapse = ", "))
  m$row <- r$label
  ce <- utils::read.csv(file.path(r$dir, "results", "ceiling.csv"), check.names = FALSE)
  ce <- ce[ce$method == "pearson", ]
  ce <- ce[!duplicated(ce[, c("context", "view")]), c("context", "view", "rho_survey")]
  ce <- merge(ce, unique(m[, c("context", "view", "scale_m")]), by = c("context", "view"))
  rho <- pmax(ce$rho_survey, 0)
  ce$r_ceiling <- sqrt(rho); ce$r_half <- rho / (2 - rho); ce$row <- r$label
  list(m = m, ce = ce)
}
got <- lapply(rows, rd)
M  <- do.call(rbind, lapply(got, `[[`, "m"))
CE <- do.call(rbind, lapply(got, `[[`, "ce"))
rowLv <- vapply(rows, `[[`, "", "label")
prep <- function(d){
  d$row  <- factor(d$row, levels = rowLv)
  d$site <- factor(SITES[d$context], levels = SITES)
  d
}
M <- prep(M); CE <- prep(CE)
M$type <- factor(M$type, levels = MODELS)
utils::write.csv(M[order(M$row, M$site, M$type, M$scale_m), c("row", "context", "type", "view", "scale_m", "point", "ci_lo", "ci_hi", "N")],
                 file.path(OUT, "sweep_long.csv"), row.names = FALSE)
utils::write.csv(CE[order(CE$row, CE$site, CE$scale_m), c("row", "context", "view", "scale_m", "rho_survey", "r_ceiling", "r_half")],
                 file.path(OUT, "sweep_ceiling.csv"), row.names = FALSE)

#the five head panels per row and model, and their mean
head5 <- list(GP500 = c("glattPark", 500), W100 = c("wigger", 100), W500 = c("wigger", 500), G100 = c("glatt", 100), G500 = c("glatt", 500))
tab <- do.call(rbind, lapply(split(M, list(M$row, M$type), drop = TRUE), function(d){
  v <- vapply(head5, function(h){ x <- d$point[d$context == h[1] & d$scale_m == as.numeric(h[2])]; if(length(x)) x[1] else NA_real_ }, 1)
  data.frame(row = d$row[1], model = d$type[1], t(round(v, 4)), mean = round(mean(v), 4))
}))
tab <- tab[order(tab$row, tab$model), ]
utils::write.csv(tab, file.path(OUT, "sweep_pearson.csv"), row.names = FALSE)
options(width = 200); print(tab, row.names = FALSE)

#GlattPark has one cell size: the models side by side round 500 m
k <- as.integer(M$type) - (length(MODELS) + 1) / 2
M$x <- ifelse(M$context == "glattPark", M$scale_m + 14 * k, M$scale_m)
seg <- CE[CE$context == "glattPark", ]
lin <- CE[CE$context != "glattPark", ]
note <- do.call(rbind, lapply(rows, function(r) if(is.na(r$fitted)) NULL else
  data.frame(row = r$label, context = r$fitted, label = "DULN fitted here")))
if(!is.null(note)) note <- prep(note)
ylim <- c(floor(min(M$ci_lo, CE$r_half, na.rm = TRUE) * 20) / 20, 1)
xb <- function(l) if(diff(l) < 150) 500 else seq(100, 500, 100)
REF <- c("Perfect simulation (sqrt of the survey's reliability)" = "solid", "Survey half vs half" = "dashed")

p <- ggplot(M, aes(x, point, colour = type, shape = type)) +
  geom_line(data = lin, aes(scale_m, r_ceiling, linetype = names(REF)[1]), colour = INK, linewidth = 0.5, inherit.aes = FALSE) +
  geom_line(data = lin, aes(scale_m, r_half, linetype = names(REF)[2]), colour = INK, linewidth = 0.5, inherit.aes = FALSE) +
  geom_segment(data = seg, aes(x = scale_m - 40, xend = scale_m + 40, y = r_ceiling, yend = r_ceiling, linetype = names(REF)[1]),
               colour = INK, linewidth = 0.5, inherit.aes = FALSE) +
  geom_segment(data = seg, aes(x = scale_m - 40, xend = scale_m + 40, y = r_half, yend = r_half, linetype = names(REF)[2]),
               colour = INK, linewidth = 0.5, inherit.aes = FALSE) +
  geom_line(data = M[M$context != "glattPark", ], linewidth = 0.7) +
  geom_linerange(aes(ymin = ci_lo, ymax = ci_hi), linewidth = 0.4, show.legend = FALSE) +
  geom_point(size = 1.9, stroke = 0.9) +
  facet_grid(row ~ site, scales = "free_x", labeller = labeller(row = label_wrap_gen(18))) +
  scale_x_continuous(breaks = xb, expand = expansion(mult = 0.08)) +
  scale_y_continuous(limits = ylim, breaks = seq(0, 1, 0.1), expand = expansion(mult = c(0.02, 0.02))) +
  scale_colour_manual(values = COLS, drop = FALSE) + scale_shape_manual(values = SHAPES, drop = FALSE) +
  scale_linetype_manual(values = REF, breaks = names(REF)) +
  labs(x = "Cell size (m)", y = "Pearson r, simulation vs survey (all cells)", colour = NULL, shape = NULL, linetype = NULL,
       caption = paste0("Rows: what the attractivity layer (DULN) was tailored on. Columns: the survey the simulation is tested against. ",
                        "Bars: bootstrap 95 % CI.\nGlattPark's survey is a 500 m grid (one cell size); its ground lies inside Glatt's. ",
                        "Every simulation draws the site's survey type mix, durations and respondent counts.")) +
  theme_bw(base_size = 9) +
  theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(0, 0, 0, 0), legend.key.width = unit(9, "mm"),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = GRID, linewidth = 0.3),
        strip.background = element_rect(fill = "#f0efec", colour = NA), strip.text = element_text(colour = INK, size = 8.5),
        axis.text = element_text(colour = INK2), axis.title = element_text(colour = INK),
        plot.caption = element_text(colour = INK2, hjust = 0, size = 7), panel.border = element_rect(colour = "#c3c2b7"))
if(!is.null(note))
  p <- p + geom_text(data = note, aes(x = -Inf, y = ylim[1], label = label), hjust = -0.08, vjust = -0.7, size = 2.5,
                     colour = INK2, fontface = "italic", inherit.aes = FALSE)
ggsave(file.path(OUT, "fig_sweep_grid.png"), p, width = 21, height = 6 + 5.2 * length(rows), units = "cm", dpi = 300, bg = "white")
cat("plot_sweep_grid: done,", OUT, "\n")
