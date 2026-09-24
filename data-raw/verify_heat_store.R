## Verification for the per-scenario heat map store and its card icons
## (heatStoreNew() and heatIconsTag() in R/heat_helpers.R).
##
## The store decides which heat icons a scenario card shows, so every rule the
## page relies on is checked here, without Shiny:
##   - one slot per time of day, replaced by a later computation
##   - a stroke makes every map of that scenario stale, but keeps it
##   - Reset revives only the maps computed on the unpainted scenario
##   - a job dispatched before a stroke comes back stale
##   - a changed study area never validates an old map
##
## Run:  Rscript data-raw/verify_heat_store.R
suppressPackageStartupMessages({library(shiny)})
R <- Sys.getenv("VFT_R", file.path(getwd(), "R"))
if(!dir.exists(R)) R <- "C:/Users/frueh/VScode_GitClones/visitorFlowTool_2025/R"
env <- new.env(parent = globalenv())
for (f in sort(list.files(R, pattern = "[.][Rr]$", full.names = TRUE))) {
  suppressWarnings(try(sys.source(f, envir = env), silent = TRUE))
}
attach(env, warn.conflicts = FALSE)

fails <- 0
ok <- function(what, cond, extra = "") {
  cat(sprintf("%-62s %s %s\n", what, if (isTRUE(cond)) "PASS" else "FAIL", extra))
  if (!isTRUE(cond)) fails <<- fails + 1
}

AOI <- "aoi-1"
## what computeHeat() stores: the revision and base flag captured at dispatch
job <- function(s, key, base, tag) list(heat = tag, proj = tag, rev = heatStoreRev(s, key),
                                         base = base, aoi = AOI)

cat("=== 1. slots per time of day ===\n")
s <- heatStoreNew()
ok("an unknown card has nothing", length(heatStoreValidBins(s, "versionBtn1", AOI)) == 0)
ok("...and revision 0", identical(heatStoreRev(s, "versionBtn1"), 0L))
heatStorePut(s, "versionBtn1", "afternoon", job(s, "versionBtn1", TRUE, "A"))
heatStorePut(s, "versionBtn1", "morning",   job(s, "versionBtn1", TRUE, "M"))
ok("two maps, listed in HEAT_BINS order",
   identical(heatStoreValidBins(s, "versionBtn1", AOI), c("morning", "afternoon")))
ok("get returns the map for its bin",
   identical(heatStoreGet(s, "versionBtn1", "morning", AOI)$heat, "M"))
ok("get of a bin never computed is NULL",
   is.null(heatStoreGet(s, "versionBtn1", "midday", AOI)))
ok("another card is untouched", length(heatStoreValidBins(s, "versionBtn2", AOI)) == 0)
ok("a different study area validates nothing",
   length(heatStoreValidBins(s, "versionBtn1", "aoi-2")) == 0)

cat("\n=== 2. a stroke makes them stale, not gone ===\n")
heatStoreBump(s, "versionBtn1")
ok("no valid bins after a stroke", length(heatStoreValidBins(s, "versionBtn1", AOI)) == 0)
ok("...but the maps are still kept", length(s[["versionBtn1"]]$maps) == 2)
ok("the other card still has none and is not bumped",
   identical(heatStoreRev(s, "versionBtn2"), 0L))

cat("\n=== 3. a map computed on paint; Reset brings back only unpainted ones ===\n")
heatStorePut(s, "versionBtn1", "midday", job(s, "versionBtn1", FALSE, "P"))
ok("the painted-state midday map is valid now",
   identical(heatStoreValidBins(s, "versionBtn1", AOI), "midday"))
heatStoreBump(s, "versionBtn1")
ok("another stroke: nothing valid", length(heatStoreValidBins(s, "versionBtn1", AOI)) == 0)
heatStoreReset(s, "versionBtn1")
ok("Reset revives morning and afternoon (computed unpainted)",
   identical(heatStoreValidBins(s, "versionBtn1", AOI), c("morning", "afternoon")))
ok("...and not midday (computed on paint Reset does not bring back)",
   is.null(heatStoreGet(s, "versionBtn1", "midday", AOI)))
heatStoreBump(s, "versionBtn1")
heatStoreReset(s, "versionBtn1")
ok("stroke then Reset again: same answer",
   identical(heatStoreValidBins(s, "versionBtn1", AOI), c("morning", "afternoon")))

cat("\n=== 4. a later computation replaces a stale slot ===\n")
heatStorePut(s, "versionBtn1", "midday", job(s, "versionBtn1", TRUE, "P2"))
ok("midday is valid again, with the new map",
   identical(heatStoreGet(s, "versionBtn1", "midday", AOI)$heat, "P2"))
ok("all three bins valid", identical(heatStoreValidBins(s, "versionBtn1", AOI), HEAT_BINS))

cat("\n=== 5. a stroke while the job runs ===\n")
e <- job(s, "versionBtn3", TRUE, "R")     # dispatched...
heatStoreBump(s, "versionBtn3")           # ...painted on meanwhile...
heatStorePut(s, "versionBtn3", "midday", e)  # ...and the result lands
ok("the result is kept but not valid", is.null(heatStoreGet(s, "versionBtn3", "midday", AOI)))
ok("...and Reset revives it, since it was dispatched unpainted",
   { heatStoreReset(s, "versionBtn3"); !is.null(heatStoreGet(s, "versionBtn3", "midday", AOI)) })

cat("\n=== 6. drop ===\n")
heatStoreDrop(s, "versionBtn3")
ok("dropping one card forgets it", is.null(s[["versionBtn3"]]))
ok("...and keeps the others", length(heatStoreValidBins(s, "versionBtn1", AOI)) == 3)
heatStoreDrop(s, "notThere")
ok("dropping an unknown key is harmless", length(ls(s)) == 1)
heatStoreDrop(s)
ok("dropping everything empties the store", length(ls(s)) == 0)
ok("a NULL card is a no-op everywhere",
   is.null(heatStorePut(s, NULL, "midday", e)) && is.null(heatStoreBump(s, NULL)) &&
     is.null(heatStoreGet(s, NULL, "midday", AOI)) && length(ls(s)) == 0)

cat("\n=== 7. the icon strip ===\n")
ns <- shiny::NS("newVersions")
t0 <- heatIconsTag("versionBtn2", character(0), ns = ns)
h0 <- as.character(t0)
ok("an empty strip is still emitted, keyed by card",
   grepl('class="vftHeatIcons"', h0) && grepl('data-card="versionBtn2"', h0) &&
     !grepl("vftHeatIcon\\b\"", h0) && !grepl("<button", h0))
h1 <- as.character(heatIconsTag("versionBtn2", c("morning", "afternoon"), shown = "afternoon", ns = ns))
ok("one button per bin", lengths(regmatches(h1, gregexpr("<button", h1))) == 2)
ok("only the shown bin is marked",
   lengths(regmatches(h1, gregexpr("vftHeatShown", h1))) == 1 &&
     grepl('vftHeatIcon vftHeatShown"[^>]*data-bin="afternoon"', h1))
## htmltools writes the attribute's quotes as &#39;, which the browser decodes
h1q <- gsub("&#39;", "'", h1, fixed = TRUE)
ok("clicks go to the one namespaced input",
   grepl("Shiny.setInputValue('newVersions-heatIconClick'", h1q, fixed = TRUE) &&
     grepl("card: 'versionBtn2', bin: 'morning'", h1q, fixed = TRUE))
ok("tooltips are the bin labels", grepl('title="Morgen"', h1) && grepl('title="Nachmittag"', h1))
svgs <- vapply(HEAT_BINS, heatBinIconSVG, character(1))
ok("three distinct icons, each in its own colour",
   length(unique(svgs)) == 3 &&
     all(mapply(function(s, b) grepl(HEAT_BIN_ICON_COLORS[[b]], s, fixed = TRUE), svgs, HEAT_BINS)))
ok("the sun rises east, peaks, sets west",
   grepl('cx="7" cy="13"', svgs[["morning"]]) && grepl('cx="12" cy="7"', svgs[["midday"]]) &&
     grepl('cx="17" cy="13"', svgs[["afternoon"]]))

cat(sprintf("\n%d check(s) failed\n", fails))
quit(status = if (fails == 0) 0 else 1)
