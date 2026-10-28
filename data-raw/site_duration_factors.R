# One factor per survey site that scales the engine's outing durations to the level
# the site's respondents report (2026-10-23): the engine's 30-240 min classes keep
# their shape, only their level becomes the site's (VFT_VAL_DURATION, outing scope).
#   glatt      stated time in the area (A4, uniform within the class, one per typed
#              route) / the simulated agents' drawn duration - both time in the area
#   glattPark  stated time in the area (Q9) / the drawn duration
#   wigger     no stated time: the drawn lines' km / the simulated whole outing's km
#              (way there + inside + back), from a run on the engine's durations
# Means, not medians: the engine's median is always its 120-min class.
#
#   Rscript data-raw/site_duration_factors.R <folder with engine-duration runs + outing logs> <out csv>

args <- commandArgs(TRUE)
D   <- args[1]; OUTF <- args[2]
L   <- "C:/Users/frueh/Documents/Local Data"
RT  <- file.path(L, "pathUsageValidation_2026_best_autoCut_rumT")
DATA <- file.path(L, "Shared visitorFlow Project Data")

files <- list.files(file.path(D, "runs"), pattern = "^rep_\\d+\\.rds$", recursive = TRUE, full.names = TRUE)
logs <- do.call(rbind, lapply(files, function(f){
  r <- readRDS(f)
  if(is.null(r$outingLog) || !is.null(r$duration) || !is.null(r$durSource)) return(NULL)
  x <- r$outingLog; x$ctx <- r$ctx; x
}))
logs <- logs[!logs$noGoal, ]

stated <- readRDS(file.path(RT, "rum_time/durations_stated.rds"))
routes <- readRDS(file.path(RT, "rum_time/routes.rds"))$routes
routes <- routes[routes$len_m > 0 & routes$access %in% "home, foot or bike", ]
set.seed(1)
glattStated <- unlist(lapply(names(stated$durs), function(k)
  sample(stated$durs[[k]], sum(routes$type == k), replace = TRUE)))
gp <- haven::read_sav(file.path(DATA, "04_Glattpark_survey/MA_SB_Quellgebiet_Umkodiert.sav"))
q9 <- as.numeric(gp$Q9); q9 <- q9[is.finite(q9) & q9 > 0]
wl <- as.numeric(sf::st_length(sf::st_read(file.path(DATA, "Social Data/Wigger_data/Linien_GEOREF.gdb"), quiet = TRUE))) / 1000
wl <- wl[is.finite(wl) & wl > 0]

g  <- logs[logs$ctx == "glatt", ]; p <- logs[logs$ctx == "glattPark", ]; w <- logs[logs$ctx == "wigger", ]
res <- data.frame(
  ctx = c("glatt", "glattPark", "wigger"),
  measure = c("minutes in the area", "minutes in the area", "km, whole outing"),
  survey_mean = c(mean(glattStated), mean(q9), mean(wl)),
  sim_mean    = c(mean(g$timeExt), mean(p$timeExt), mean((w$wayM + w$inM) / 1000)),
  survey_median = c(stats::median(glattStated), stats::median(q9), stats::median(wl)),
  sim_median    = c(stats::median(g$timeExt), stats::median(p$timeExt), stats::median((w$wayM + w$inM) / 1000)))
res$factor <- round(res$survey_mean / res$sim_mean, 3)
print(format(res, digits = 3), row.names = FALSE)
utils::write.csv(res, OUTF, row.names = FALSE)
