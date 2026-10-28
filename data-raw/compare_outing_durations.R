# Where a simulated outing's time goes, against the surveys' outings (2026-10-23).
# Reads the per-agent outing logs of a validation folder's runs (VFT_VAL_OUTLOG=1 in
# data-raw/validate_usage_surveys.R) and each site's survey:
#   in-area minutes   Glatt: stated A4 (rum_time/durations_stated.rds, per type);
#                     GlattPark: Q9 (minutes in the recreation area)
#   whole-outing km   Glatt: the FilBleu routes (rum_time/routes.rds, home starts);
#                     Wigger: the drawn lines
# Writes to <folder>/outing/: outing_decomposition.csv (per site, run type and agent
# type), outing_vs_survey.csv (medians, ratio, KS D) and fig_outing_durations.png.
#
#   Rscript data-raw/compare_outing_durations.R <validation folder>

args <- commandArgs(TRUE)
D <- if(length(args)) args[1] else stop("the validation folder?")
L <- "C:/Users/frueh/Documents/Local Data"
RT <- file.path(L, "pathUsageValidation_2026_best_autoCut_rumT")
DATA <- file.path(L, "Shared visitorFlow Project Data")
OUTD <- file.path(D, "outing"); dir.create(OUTD, showWarnings = FALSE)
SPEED <- c(walkNat = 5, walkSoc = 3, dogNat = 3, dogProx = 2, ebikeNat = 20, bikeSport = 20, jogger = 7)
logf <- function(...) cat(format(Sys.time(), "%H:%M:%S "), sprintf(...), "\n", sep = "")

#### the simulated outings ####
files <- list.files(file.path(D, "runs"), pattern = "^rep_\\d+\\.rds$", recursive = TRUE, full.names = TRUE)
logs <- do.call(rbind, lapply(files, function(f){
  r <- readRDS(f)
  if(is.null(r$outingLog)) return(NULL)
  x <- r$outingLog
  #the folder's type, not r$type: a copied-in alias (fullND...) keeps its source's
  x$ctx <- r$ctx; x$run <- basename(dirname(f)); x$rep <- r$rep
  x$setting <- paste(c(if(is.null(r$outing)) "area" else r$outing,
                       if(is.null(r$returnRoute)) "shortest" else r$returnRoute,
                       if(is.null(r$legRound)) "minute" else r$legRound,
                       if(is.null(r$durSource)) "engine T" else basename(r$durSource$file),
                       if(is.null(r$duration)) NULL else paste0("T x", r$duration$factor)), collapse = " / ")
  x
}))
if(is.null(logs)) stop("no run in ", D, " carries an outing log")
logf("%d agents from %d runs (%s)", nrow(logs), length(unique(paste(logs$ctx, logs$run, logs$rep))),
     paste(unique(paste(logs$ctx, logs$run)), collapse = ", "))
x <- logs[!logs$noGoal, ]
x$overshoot <- x$turnAt - x$firstOver
x$wayKm   <- x$wayM / 1000
x$inKm    <- x$inM / 1000
x$totalKm <- x$wayKm + x$inKm
#in-area walking speed against the type's (the minute rounding of every leg)
x$inSpeedRatio <- ifelse(x$inArea > 0, (x$inKm / (x$inArea / 60)) / x$speed, NA)
x$timedHome <- !is.na(x$turnAt)

q <- function(v, p) stats::quantile(v, p, na.rm = TRUE, names = FALSE)
COLS <- c("timeExt", "approach", "firstOver", "overshoot", "inArea", "back", "end",
          "wayKm", "inKm", "totalKm", "inSpeedRatio")
decomp <- function(g){
  out <- data.frame(ctx = g$ctx[1], run = g$run[1], agentTyp = if(length(unique(g$agentTyp)) > 1) "all" else g$agentTyp[1],
                    setting = g$setting[1], agents = nrow(g),
                    timedHome = mean(g$timedHome), unfinished = mean(g$unfinished),
                    endOverT = mean(g$end > g$timeExt + 1e-9, na.rm = TRUE))
  for(k in COLS){
    out[[paste0(k, "_med")]] <- stats::median(g[[k]], na.rm = TRUE)
    out[[paste0(k, "_p25")]] <- q(g[[k]], .25)
    out[[paste0(k, "_p75")]] <- q(g[[k]], .75)
  }
  out$endMinusT_med <- stats::median(g$end - g$timeExt, na.rm = TRUE)
  out
}
grp <- split(x, list(x$ctx, x$run), drop = TRUE)
dec <- do.call(rbind, c(lapply(grp, decomp),
                        unlist(lapply(grp, function(g) lapply(split(g, g$agentTyp), decomp)), recursive = FALSE)))
rownames(dec) <- NULL
utils::write.csv(dec, file.path(OUTD, "outing_decomposition.csv"), row.names = FALSE)
show <- dec[dec$agentTyp == "all", c("ctx", "run", "setting", "agents", "timedHome", "timeExt_med", "approach_med",
                                     "overshoot_med", "inArea_med", "back_med", "end_med", "endMinusT_med",
                                     "wayKm_med", "inKm_med", "totalKm_med", "inSpeedRatio_med")]
logf("where the time goes (medians; minutes, km):")
print(format(show, digits = 3), row.names = FALSE)

#### the surveys ####
stated <- readRDS(file.path(RT, "rum_time/durations_stated.rds"))
routes <- readRDS(file.path(RT, "rum_time/routes.rds"))$routes
routes <- routes[routes$len_m > 0 & routes$access %in% "home, foot or bike", ]
gp <- haven::read_sav(file.path(DATA, "04_Glattpark_survey/MA_SB_Quellgebiet_Umkodiert.sav"))
q9 <- as.numeric(gp$Q9); q9 <- q9[is.finite(q9) & q9 > 0]
wl <- as.numeric(sf::st_length(sf::st_read(file.path(DATA, "Social Data/Wigger_data/Linien_GEOREF.gdb"), quiet = TRUE))) / 1000
wl <- wl[is.finite(wl) & wl > 0]
#the Glatt in-area reference pooled over types as the routes are (one draw per route)
set.seed(1)
glattStated <- unlist(lapply(names(stated$durs), function(k)
  sample(stated$durs[[k]], sum(routes$type == k), replace = TRUE)))
REF <- list(
  glatt     = list(min = glattStated, km = routes$len_m / 1000,
                   minLab = "stated time in the area (A4)", kmLab = "FilBleu routes"),
  glattPark = list(min = q9, km = NULL, minLab = "stated time in the area (Q9)", kmLab = NA),
  wigger    = list(min = NULL, km = wl, minLab = NA, kmLab = "drawn lines"))

ks <- function(a, b) unname(suppressWarnings(stats::ks.test(a, b)$statistic))
vs <- do.call(rbind, lapply(grp, function(g){
  ref <- REF[[g$ctx[1]]]
  rows <- list()
  if(!is.null(ref$min)){
    s <- g$inArea[is.finite(g$inArea)]
    rows[[1]] <- data.frame(measure = "minutes in the area", survey = ref$minLab,
                            sim_median = stats::median(s), survey_median = stats::median(ref$min),
                            ratio = stats::median(s) / stats::median(ref$min), ks_D = ks(s, ref$min))
  }
  if(!is.null(ref$km)){
    s <- g$totalKm
    rows[[length(rows) + 1]] <- data.frame(measure = "km, whole outing", survey = ref$kmLab,
                                           sim_median = stats::median(s), survey_median = stats::median(ref$km),
                                           ratio = stats::median(s) / stats::median(ref$km), ks_D = ks(s, ref$km))
  }
  cbind(data.frame(ctx = g$ctx[1], run = g$run[1], setting = g$setting[1]), do.call(rbind, rows))
}))
rownames(vs) <- NULL
utils::write.csv(vs, file.path(OUTD, "outing_vs_survey.csv"), row.names = FALSE)
logf("simulated vs surveyed outings (medians):")
print(format(vs, digits = 3), row.names = FALSE)

#### figure: ECDFs, survey vs the runs ####
INK <- "#1f1f1f"; GRID <- "#e4e3df"; MUTED <- "#6b6a64"
SERIES <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100")
runs <- unique(x$run)
png(file.path(OUTD, "fig_outing_durations.png"), width = 2400, height = 1500, res = 200)
op <- par(mfrow = c(2, 3), oma = c(0, 0, 2.2, 0), mar = c(4.2, 4.2, 2.6, 1), mgp = c(2.4, .7, 0), col.axis = MUTED, col.lab = INK,
          fg = MUTED, las = 1, bg = "white")
panel <- function(ctx, what){
  ref <- REF[[ctx]]
  sv <- if(what == "min") ref$min else ref$km
  sims <- lapply(stats::setNames(runs, runs), function(t){
    g <- x[x$ctx == ctx & x$run == t, ]
    if(!nrow(g)) return(NULL)
    v <- if(what == "min") g$inArea else g$totalKm
    v[is.finite(v)]
  })
  sims <- sims[!vapply(sims, is.null, TRUE)]
  allv <- c(sv, unlist(sims))
  if(!length(allv)){ plot.new(); return(invisible()) }
  xmax <- q(allv, .98)
  plot(NA, xlim = c(0, xmax), ylim = c(0, 1), xlab = if(what == "min") "minutes in the area" else "km, whole outing",
       ylab = "share of outings", main = "")
  title(main = paste0(ctx, if(what == "min") " - time in the area" else " - outing length"), adj = 0,
        col.main = INK, font.main = 1, cex.main = 1.1)
  abline(h = c(.25, .5, .75), col = GRID, lwd = 1)
  for(i in seq_along(sims)) lines(stats::ecdf(sims[[i]]), col = SERIES[match(names(sims)[i], runs)],
                                  lwd = 2, do.points = FALSE, verticals = TRUE)
  if(length(sv)) lines(stats::ecdf(sv), col = INK, lwd = 2.5, do.points = FALSE, verticals = TRUE)
  else mtext("no survey reference", side = 3, line = -1.5, col = MUTED, cex = .8)
}
for(ctx in c("glatt", "glattPark", "wigger")) panel(ctx, "min")
for(ctx in c("glatt", "glattPark", "wigger")) panel(ctx, "km")
par(op)
#one legend for all panels
par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot.new()
legend("top", legend = c("survey", paste("sim:", runs)), col = c(INK, SERIES[seq_along(runs)]),
       lwd = c(2.5, rep(2, length(runs))), horiz = TRUE, bty = "n", text.col = INK, cex = .9, inset = 0, xpd = NA)
dev.off()
logf("compare_outing_durations: done, %s", OUTD)
