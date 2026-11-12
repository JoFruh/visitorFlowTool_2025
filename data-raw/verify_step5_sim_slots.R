## Verification for step 5's three simulation slots per card (normal, morning and
## afternoon heatwave): the slot helpers, the migration of saves from before the
## slots, and the icon strip's markup (R/heatwave_helpers.R).
##
## Run:  Rscript data-raw/verify_step5_sim_slots.R
## Pure: no app, no data.

suppressPackageStartupMessages({
  pkgload::load_all(".", export_all = TRUE, quiet = TRUE)
})

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-66s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

pu <- function(tag) structure(list(tag = tag), class = "fakeGraph")
rec <- function(bin) list(bin = bin, rev = 3L, base = TRUE, card = "versionBtn0",
                          params = VFT_HEATWAVE, summary = list())

#### 1. the slot helpers ####
sc <- list(network = "net")
ok("an empty scenario has no slot", !any(vftScenarioSlots(sc)))
ok("the slots are normal, morning, afternoon",
   identical(names(vftScenarioSlots(sc)), c("normal", "morning", "afternoon")))

sc <- vftScenarioSetSim(sc, "normal", list(pathUsage = pu("n"), dayPop = "dn", conflicts = NULL))
ok("normal writes the old fields", identical(sc$pathUsage, pu("n")) && identical(sc$dayPop, "dn"))
ok("...and no heatSims", is.null(sc$heatSims))

sc <- vftScenarioSetSim(sc, "afternoon", list(pathUsage = pu("a"), dayPop = "da",
                                              conflicts = NULL, heatwave = rec("afternoon")))
ok("afternoon goes into heatSims", identical(sc$heatSims$afternoon$pathUsage, pu("a")))
ok("...and leaves the normal slot alone", identical(sc$pathUsage, pu("n")))
ok("slots: normal + afternoon",
   identical(unname(vftScenarioSlots(sc)), c(TRUE, FALSE, TRUE)))
ok("vftScenarioSim(normal) has no heatwave record", is.null(vftScenarioSim(sc, "normal")$heatwave))
ok("vftScenarioSim(afternoon) carries its record",
   identical(vftScenarioSim(sc, "afternoon")$heatwave$bin, "afternoon"))
ok("vftScenarioSim(morning) is NULL", is.null(vftScenarioSim(sc, "morning")))

sc2 <- vftScenarioSetSim(sc, "normal", NULL)
ok("emptying normal keeps the heat slot",
   is.null(sc2$pathUsage) && !is.null(vftScenarioSim(sc2, "afternoon")))
sc3 <- vftScenarioSetSim(sc2, "afternoon", NULL)
ok("emptying the last heat slot drops heatSims", is.null(sc3$heatSims))

#conflicts are found in the slot, through the same reader newVersions uses
sim <- vftScenarioSim(sc, "afternoon")
sim$conflicts <- list(hotspots = data.frame(x = 1), smKey = list(a = 1))
sc4 <- vftScenarioSetSim(sc, "afternoon", sim)
ok("a slot's conflicts read back through vftStoredConflicts()",
   identical(vftStoredConflicts(vftScenarioSim(sc4, "afternoon"), list(a = 1)), data.frame(x = 1)))
ok("...and not as the normal slot's", is.null(vftStoredConflicts(sc4, list(a = 1))))

#### 2. the migration ####
old <- list(network = "net", pathUsage = pu("m"), dayPop = "dm", heatwave = rec("morning"))
mg  <- vftScenarioMigrate(old)
ok("legacy heatwave result moves to its bin", identical(mg$heatSims$morning$pathUsage, pu("m")))
ok("...the normal slot is emptied", is.null(mg$pathUsage) && is.null(mg$dayPop))
ok("...the old record is gone", is.null(mg$heatwave))
ok("...and the record travels with it", identical(mg$heatSims$morning$heatwave$rev, 3L))
ok("a normal scenario is unchanged", identical(vftScenarioMigrate(sc), sc))
ok("migration is idempotent", identical(vftScenarioMigrate(mg), mg))
dead <- list(network = "net", heatwave = rec("morning"))
ok("a record without a result is just dropped", identical(vftScenarioMigrate(dead), list(network = "net")))

#### 3. the icon strip ####
html <- function(...) gsub("&#39;", "'", as.character(step5SimIconsTag("versionBtn0", ...)), fixed = TRUE)
cls <- function(h, slot){
  m <- regmatches(h, regexpr(sprintf('class="[^"]*"[^>]*data-slot="%s"', slot), h))
  sub('class="([^"]*)".*', "\\1", m)
}
h <- html(vftScenarioSlots(list()))
ok("three icons", lengths(regmatches(h, gregexpr("vft5SimIcon[ \"]", h))) == 3)
ok("no result: all three vft5SimNone",
   all(grepl("vft5SimNone", c(cls(h, "normal"), cls(h, "morning"), cls(h, "afternoon")))))
ok("the suns are vft5SimHeat, the tick is not",
   grepl("vft5SimHeat", cls(h, "morning")) && !grepl("vft5SimHeat", cls(h, "normal")))

h <- html(vftScenarioSlots(sc), stale = c(morning = FALSE, afternoon = TRUE), shown = "afternoon")
ok("normal run: the tick is filled", !grepl("vft5SimNone", cls(h, "normal")) && grepl("#006268", h))
ok("stale afternoon result is vft5SimStale", grepl("vft5SimStale", cls(h, "afternoon")))
ok("the shown slot is vft5SimShown", grepl("vft5SimShown", cls(h, "afternoon")) &&
     !grepl("vft5SimShown", cls(h, "normal")))
ok("a missing slot is never stale", !grepl("vft5SimStale", cls(h, "morning")))
ok("clicks carry card and slot",
   grepl("{card: 'versionBtn0', slot: 'morning'", h, fixed = TRUE) &&
     grepl("event.stopPropagation()", h, fixed = TRUE))
h <- html(vftScenarioSlots(sc), ns = shiny::NS("step5"))
ok("the input is namespaced", grepl("'step5-simIconClick'", h, fixed = TRUE))

cat(sprintf("\n%d failure(s)\n", fails))
if (fails) quit(status = 1)
