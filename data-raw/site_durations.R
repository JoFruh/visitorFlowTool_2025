# Each survey site's own outing durations, per engine agent type, in the format
# VFT_VAL_DURSRC reads (list(durs = minutes per type, source =)). Written for the
# outing-duration screens (2026-10-23): the engine draws 30-240 min (median 120,
# R/determineAgentCharacteristics.R, from other surveys); the sites report less.
#
#   glatt      the Glatt survey's stated time in a recreation area (A4_duration), per
#              typed route, uniform within the class - data-raw/calibrate_aoi_rum_time.R
#              wrote it (rum_time/durations_stated.rds); copied as it is
#   glattPark  the GlattPark residents' survey, Q9 "Wie lange verweilen Sie im
#              Naherholungsgebiet" (minutes), per type through the Q1 activities
#              done "Meistens" or "Immer" (several per respondent: a respondent
#              counts for each); a type with
#              fewer than MIN_N respondents takes everyone's
#   wigger     the Wigger respondents' drawn lines (no time, no activity): the line's
#              length at the type's engine speed - a WHOLE-outing duration
#
# Also each site's agent type mix (typemix_site_<ctx>.rds, list(mix =, source =), for
# VFT_VAL_TYPEMIX), so the evaluation's agents are the survey's in kind as well:
#   glatt      the share of the typed FilBleu routes per type (rum_time/routes.rds)
#   glattPark  the Q1 activities done "Meistens" / "Immer", one count per respondent and
#              activity; walking, dog walking and biking split between their two engine
#              types as in Glatt's routes (GlattPark asks no motives)
#   wigger     Glatt's mix: the Wigger survey has no activities
#
#   Rscript data-raw/site_durations.R <out dir> <durations_stated.rds>

args <- commandArgs(TRUE)
OUT    <- if(length(args) >= 1) args[1] else stop("the output folder?")
STATED <- if(length(args) >= 2) args[2] else stop("rum_time/durations_stated.rds?")
DATA   <- "C:/Users/frueh/Documents/Local Data/Shared visitorFlow Project Data"
GP_SAV <- file.path(DATA, "04_Glattpark_survey/MA_SB_Quellgebiet_Umkodiert.sav")
W_GDB  <- file.path(DATA, "Social Data/Wigger_data/Linien_GEOREF.gdb")
MIN_N  <- 10
#the engine's types and speeds (km/h), as vftAgentTimeAndSpeed()
SPEED  <- c(walkNat = 5, walkSoc = 3, dogNat = 3, dogProx = 2, ebikeNat = 20, bikeSport = 20, jogger = 7)
TYPES  <- names(SPEED)
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
logf <- function(...) cat(format(Sys.time(), "%H:%M:%S "), sprintf(...), "\n", sep = "")
summ <- function(durs) do.call(rbind, lapply(names(durs), function(k) data.frame(
  type = k, n = length(durs[[k]]), median = stats::median(durs[[k]]), mean = mean(durs[[k]]),
  p25 = stats::quantile(durs[[k]], .25, names = FALSE), p75 = stats::quantile(durs[[k]], .75, names = FALSE))))
save1 <- function(ctx, durs, source, extra = list()){
  stopifnot(all(TYPES %in% names(durs)), all(vapply(durs[TYPES], function(d) length(d) > 0 && all(d > 0), TRUE)))
  f <- file.path(OUT, paste0("durations_site_", ctx, ".rds"))
  saveRDS(c(list(durs = durs[TYPES], source = source, made = format(Sys.time())), extra), f)
  s <- summ(durs[TYPES]); s$context <- ctx
  logf("%s: %s", ctx, source); print(format(s, digits = 3), row.names = FALSE)
  s
}

#### Glatt: the stated time in the area ####
st <- readRDS(STATED)
sG <- save1("glatt", st$durs, st$source)

#### GlattPark: Q9 minutes per activity ####
gp <- haven::read_sav(GP_SAV)
q9 <- as.numeric(gp$Q9)
#how often the respondent does it there, 1 "Nie" to 5 "Immer": counted for
#"Meistens" and "Immer"
act <- function(v) as.numeric(gp[[v]]) %in% c(4, 5)
#the engine's types each activity stands for
MAP <- list(walkNat = c("Q1_1_Umk", "Q1_5_Umk"), walkSoc = "Q1_1_Umk",
            dogNat = "Q1_2_Umk", dogProx = "Q1_2_Umk", jogger = "Q1_3_Umk",
            ebikeNat = "Q1_4_Umk", bikeSport = "Q1_4_Umk")
ok <- is.finite(q9) & q9 > 0
logf("glattPark: %d respondents, %d with Q9 (median %.0f min)", nrow(gp), sum(ok), stats::median(q9[ok]))
for(v in unique(unlist(MAP))) logf("  %s (%s): %d respondents", v, attr(gp[[v]], "label"), sum(act(v) & ok))
nGP <- c()
durGP <- lapply(stats::setNames(TYPES, TYPES), function(k){
  sel <- ok & Reduce(`|`, lapply(MAP[[k]], act))
  nGP[k] <<- sum(sel)
  if(sum(sel) >= MIN_N) q9[sel] else q9[ok]
})
logf("glattPark: respondents per type %s; below %d take everyone's", paste(names(nGP), nGP, sep = "=", collapse = " "), MIN_N)
sGP <- save1("glattPark", durGP, "GlattPark survey Q9 (minutes in the recreation area), per type through the Q1 activities",
             list(respondents = nGP))

#### Wigger: the lines at the type's speed ####
lines <- sf::st_read(W_GDB, quiet = TRUE)
m <- as.numeric(sf::st_length(lines))
m <- m[is.finite(m) & m > 0]
logf("wigger: %d lines, median %.2f km", length(m), stats::median(m) / 1000)
durW <- lapply(stats::setNames(TYPES, TYPES), function(k) m / (SPEED[[k]] * 1000 / 60))
sW <- save1("wigger", durW, "Wigger survey lines: length / the type's engine speed (the whole outing)",
            list(metres = m))
#the same, an open line counted twice: 90 % of the lines end >= 100 m from where they
#start and one end lies at the respondent's home, so an out-and-back walk was most
#likely drawn one way (2026-10-24)
g  <- sf::st_geometry(sf::st_transform(sf::st_zm(lines), 2056))
gap <- vapply(seq_along(g), function(i){ cc <- sf::st_coordinates(g[i]); sqrt(sum((cc[1, 1:2] - cc[nrow(cc), 1:2])^2)) }, 1)
m2 <- as.numeric(sf::st_length(lines)) * ifelse(gap >= 100, 2, 1)
m2 <- m2[is.finite(m2) & m2 > 0]
logf("wigger2: %.0f %% of the lines open (counted twice), median %.2f km", 100 * mean(gap >= 100), stats::median(m2) / 1000)
durW2 <- lapply(stats::setNames(TYPES, TYPES), function(k) m2 / (SPEED[[k]] * 1000 / 60))
sW2 <- save1("wigger2", durW2, "Wigger survey lines, open lines twice (out and back drawn one way): length / the type's engine speed (the whole outing)",
             list(metres = m2))

#### agent type mixes ####
routes <- readRDS(file.path(dirname(STATED), "routes.rds"))$routes
gl <- table(factor(routes$type, levels = TYPES)); gl <- gl / sum(gl)
within <- function(a, b) c(gl[[a]], gl[[b]]) / (gl[[a]] + gl[[b]])
cnt <- c(walk = sum(act("Q1_1_Umk") & ok) + sum(act("Q1_5_Umk") & ok), dog = sum(act("Q1_2_Umk") & ok),
         jog = sum(act("Q1_3_Umk") & ok), bike = sum(act("Q1_4_Umk") & ok))
gpMix <- c(walkNat = cnt[["walk"]] * within("walkNat", "walkSoc")[1], walkSoc = cnt[["walk"]] * within("walkNat", "walkSoc")[2],
           dogNat = cnt[["dog"]] * within("dogNat", "dogProx")[1], dogProx = cnt[["dog"]] * within("dogNat", "dogProx")[2],
           ebikeNat = cnt[["bike"]] * within("ebikeNat", "bikeSport")[1], bikeSport = cnt[["bike"]] * within("ebikeNat", "bikeSport")[2],
           jogger = cnt[["jog"]])
gpMix <- gpMix / sum(gpMix)
ENGINE <- c(walkNat = 0.232, walkSoc = 0.1667, dogNat = 0.1535, dogProx = 0.1124, ebikeNat = 0.089, bikeSport = 0.05937, jogger = 0.2328)
mixes <- list(glatt = list(mix = c(gl)[TYPES], source = "Glatt survey: share of the typed FilBleu routes per type"),
              glattPark = list(mix = gpMix[TYPES], source = "GlattPark survey Q1 activities (Meistens / Immer), split within walking, dog walking and biking as in Glatt's routes"),
              wigger = list(mix = c(gl)[TYPES], source = "Glatt's mix (the Wigger survey has no activities)"))
for(ctx in names(mixes)) saveRDS(c(mixes[[ctx]], list(made = format(Sys.time()))), file.path(OUT, paste0("typemix_site_", ctx, ".rds")))
mt <- data.frame(type = TYPES, engine = ENGINE[TYPES], glatt = mixes$glatt$mix, glattPark = mixes$glattPark$mix, wigger = mixes$wigger$mix)
logf("agent type mixes (shares):"); print(format(mt, digits = 2), row.names = FALSE)
utils::write.csv(mt, file.path(OUT, "typemix_site.csv"), row.names = FALSE)

utils::write.csv(rbind(sG, sGP, sW, sW2), file.path(OUT, "durations_site.csv"), row.names = FALSE)
logf("site_durations: done, %s", OUT)
