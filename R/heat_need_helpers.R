#### where does heat cost the most recreation? (step 5 + Hitzeminderung) ####
#
# A scenario card can hold a normal simulation and a heatwave simulation per
# time of day (R/heatwave_helpers.R). The places where usage DROPS most from the
# normal run to the heatwave run are the places people avoid in the heat - the
# candidates for shade, trees, water. "Bereiche mit Hitzeminderungsbedarf
# erkennen" circles them, on step 5 and on the Hitzeminderung page.
#
# The result is stored on the scenario, sc$heatNeed = list(hotspots, bins, key),
# and is DELIBERATELY not cleared by edits: it is guidance for painting, and
# painting is exactly what makes it stale. Staleness is shown (dashed circles,
# a refresh button), never acted on. See vftHeatNeedFresh().
#
# Pure functions, no Shiny state: data-raw/verify_heat_need.R drives them
# headless. NO `%in%` ON A SpatRaster (see R/conflict_helpers.R).

#' Colours of the circles, by when the drop matters
VFT_HEAT_NEED_COLORS <- c(morning = "#C99A00", afternoon = "#F07800", both = "#D62828")
#' What the modal offers: one time of day, or both
VFT_HEAT_NEED_CHOICES <- c("morning", "afternoon", "both")

#' The bins a modal choice stands for.
vftHeatNeedBins <- function(choice){
  if(identical(choice, "both")) VFT_HEATWAVE_BINS
  else if(isTRUE(choice %in% VFT_HEATWAVE_BINS)) choice
  else character(0)
}

#' A simulation's edges as sf: `edgeID` (when the graph has it) and `passage`,
#' flat - the same table step 5's getPassageTable() builds.
vftSimEdges <- function(pathUsage){
  if(is.null(pathUsage)) return(NULL)
  e <- sf::st_as_sf(dplyr::as_tibble(tidygraph::activate(pathUsage, "edges")))
  if(!is.null(sf::st_z_range(e)) || !is.null(sf::st_m_range(e)))
    e <- sf::st_zm(e, drop = TRUE, what = "ZM")
  e[, intersect(c("edgeID", "passage"), names(e))]
}

#' A simulation, as two plain numbers: its edge count and its summed passage.
#' What a stored search is checked against - survives the save file, where the
#' graph itself would have to be compared whole.
vftSimFingerprint <- function(pathUsage){
  if(is.null(pathUsage)) return(NULL)
  p <- tryCatch(igraph::edge_attr(pathUsage, "passage"), error = function(e) NULL)
  if(is.null(p)) return(NULL)
  c(length(p), round(sum(as.numeric(p), na.rm = TRUE), 6))
}

#' The key a search is stored with: the fingerprints of the simulations it
#' compared, and the heat-map revision each heatwave run was made on.
vftHeatNeedKey <- function(sc, bins){
  key <- list(normal = vftSimFingerprint(vftScenarioSim(sc, "normal")$pathUsage))
  for(b in bins){
    sim <- vftScenarioSim(sc, b)
    key[[b]] <- list(fp = vftSimFingerprint(sim$pathUsage), rev = sim$heatwave$rev)
  }
  key
}

#' Does the stored search still describe the scenario? FALSE once either
#' simulation it compared has been re-run or dropped, or once the heat map
#' under a heatwave run has been painted over since (`revNow` is the card's
#' heat-store revision now - heatStoreRev(), as hwStale() in step 5 reads it).
#' NA when nothing is stored.
vftHeatNeedFresh <- function(sc, revNow){
  rec <- sc$heatNeed
  if(!is.list(rec) || is.null(rec$hotspots)) return(NA)
  now <- vftHeatNeedKey(sc, rec$bins)
  if(is.null(now$normal) || !isTRUE(all.equal(now$normal, rec$key$normal))) return(FALSE)
  for(b in rec$bins){
    k <- now[[b]]
    if(is.null(k$fp) || !isTRUE(all.equal(k$fp, rec$key[[b]]$fp))) return(FALSE)
    if(!identical(k$rev, revNow)) return(FALSE)
  }
  TRUE
}

#' What a search for `bins` still needs on this scenario.
#'
#' @param hasMap function(bin) TRUE when a valid heat map exists for the bin.
#' @param revNow the card's heat-store revision now.
#' @return list(normal = TRUE when the normal run is missing, heatMaps = bins
#'   without a valid map, heatSims = bins whose heatwave run is missing, stale,
#'   or about to be made on a new map).
vftHeatNeedPlan <- function(sc, bins, hasMap, revNow){
  noMap <- bins[!vapply(bins, function(b) isTRUE(hasMap(b)), logical(1))]
  sims  <- bins[vapply(bins, function(b){
    sim <- vftScenarioSim(sc, b)
    is.null(sim) || !identical(sim$heatwave$rev, revNow) || b %in% noMap
  }, logical(1))]
  list(normal = is.null(vftScenarioSim(sc, "normal")), heatMaps = noMap, heatSims = sims)
}

#' Nothing left to run: the search itself is all that is needed.
vftHeatNeedPlanEmpty <- function(plan) !isTRUE(plan$aoi) && !plan$normal &&
  !length(plan$heatMaps) && !length(plan$heatSims)

#' Find the places where usage falls most from the normal run to the heatwave
#' run(s).
#'
#' Per time of day, an edge's loss is max(0, normal - heatwave) passage,
#' matched by `edgeID` (row order when there is none). The losses and the normal
#' usage are put on one EPSG:2056 grid of `res` metres, the heaviest edge per
#' cell. A cell qualifies for a time of day when its loss is at least `relFloor`
#' of its normal usage AND at least `absFloor` of the busiest edge's normal
#' usage: the ABM is stochastic, and two runs differ everywhere by a little -
#' the floors are what keep that noise from being circled. All parameters are
#' fixed in advance, none fitted.
#'
#' The circles are PLACES, not regions: compact enough to plant or shade. A
#' first version clustered every qualifying cell within reach of another, and on
#' a real heatwave run the cells chained into one circle over the whole study
#' area. So: the qualifying loss is summed over a `radius` window round every
#' cell, the strongest such peak becomes a circle round the qualifying cells
#' within `radius` of it, those cells are taken, no other centre may lie within
#' 1.5 `radius` of it, and the next peak is taken - up to `maxCircles`. A circle
#' over fewer than `minCells` cells (about 100 m of path at 20 m) is a spot, not
#' a place, and is dropped: one edge's noise passes the floors now and then, a
#' real heat effect never covers just one short stretch. With two times of day,
#' a circle is "both" when the weaker one's summed loss there is at least
#' `bothShare` of the stronger's, else the stronger's.
#'
#' @param normal the normal run's edges (vftSimEdges()).
#' @param heat a named list of heatwave runs' edges, names in VFT_HEATWAVE_BINS.
#' @return a data.frame, one row per circle, strongest first: lng, lat
#'   (EPSG:4326), radius_m, score (passages lost), lossPct (lost share of the
#'   normal usage in the circle, %), nCells, when ("morning", "afternoon",
#'   "both"), and one column of summed loss per time of day. Zero rows when
#'   nothing qualifies.
vftHeatNeedHotspots <- function(normal, heat, res = 20, relFloor = 0.2, absFloor = 0.05,
                                radius = 150, maxCircles = 5, bothShare = 1/3,
                                minCells = 6){
  heat <- heat[!vapply(heat, is.null, logical(1))]
  bins <- intersect(names(heat), VFT_HEATWAVE_BINS)
  none <- data.frame(lng = numeric(0), lat = numeric(0), radius_m = numeric(0),
                     score = numeric(0), lossPct = numeric(0), nCells = integer(0),
                     when = character(0), stringsAsFactors = FALSE)
  for(b in bins) none[[b]] <- numeric(0)
  if(is.null(normal) || !nrow(normal) || !length(bins)) return(none)

  flat <- function(e){
    if(!is.null(sf::st_z_range(e)) || !is.null(sf::st_m_range(e)))
      e <- sf::st_zm(e, drop = TRUE, what = "ZM")
    sf::st_transform(e, 2056)
  }
  normal <- flat(normal)
  idOf <- function(e) if("edgeID" %in% names(e)) e$edgeID else seq_len(nrow(e))
  idN  <- idOf(normal)
  pN   <- as.numeric(normal$passage)
  pN[!is.finite(pN)] <- 0
  maxN <- max(pN)
  if(!is.finite(maxN) || maxN <= 0) return(none)

  loss <- lapply(heat[bins], function(h){
    pH <- as.numeric(h$passage)[match(idN, idOf(h))]
    #an edge the heatwave run does not have is not a loss we can speak for
    pH[!is.finite(pH)] <- pN[!is.finite(pH)]
    pmax(0, pN - pH)
  })

  #one grid for everything, a cell's margin around the network
  bb <- sf::st_bbox(normal)
  grid <- terra::rast(xmin = bb[["xmin"]] - res, xmax = bb[["xmax"]] + res,
                      ymin = bb[["ymin"]] - res, ymax = bb[["ymax"]] + res,
                      resolution = res, crs = "EPSG:2056")
  burn <- function(v){
    keep <- v > 0
    if(!any(keep)) return(rep(NA_real_, terra::ncell(grid)))
    e <- sf::st_sf(v = v[keep], geometry = sf::st_geometry(normal)[keep])
    terra::values(terra::rasterize(terra::vect(e), grid, field = "v", fun = "max"), mat = FALSE)
  }
  nV <- burn(pN)
  nV[!is.finite(nV)] <- 0
  lV <- lapply(loss, burn)

  qual <- lapply(lV, function(v) is.finite(v) & v > 0 & v >= relFloor * nV & v >= absFloor * maxN)
  hot  <- which(Reduce(`|`, qual))
  if(!length(hot)) return(none)

  lossAt <- vapply(lV, function(v){ x <- v[hot]; x[!is.finite(x)] <- 0; x }, numeric(length(hot)))
  if(!is.matrix(lossAt)) lossAt <- matrix(lossAt, ncol = length(bins))
  colnames(lossAt) <- bins
  #a cell counts for a time of day only where it qualified for it
  for(b in bins) lossAt[!qual[[b]][hot], b] <- 0
  best <- apply(lossAt, 1, max)

  #the qualifying loss round every cell
  v <- numeric(terra::ncell(grid)); v[hot] <- best
  bestR <- grid; terra::values(bestR) <- v
  w <- terra::focalMat(grid, radius, "circle"); w[w > 0] <- 1
  around <- terra::values(terra::focal(bestR, w, fun = "sum", na.rm = TRUE), mat = FALSE)[hot]

  xy <- terra::xyFromCell(grid, hot)
  free <- rep(TRUE, length(hot))   #not yet in a circle
  open <- rep(TRUE, length(hot))   #may still be a centre
  picks <- list()
  while(length(picks) < maxCircles){
    cand <- which(free & open)
    if(!length(cand)) break
    k <- cand[which.max(around[cand])]
    d <- sqrt((xy[, 1] - xy[k, 1])^2 + (xy[, 2] - xy[k, 2])^2)
    mem <- which(free & d <= radius)
    free[mem] <- FALSE
    open[d < 1.5 * radius] <- FALSE
    if(length(mem) >= minCells) picks[[length(picks) + 1]] <- mem
  }
  if(!length(picks)) return(none)

  halfDiag <- sqrt(2) * res / 2
  circ <- t(vapply(picks, function(m){
    wt <- best[m]
    cx <- sum(xy[m, 1] * wt) / sum(wt); cy <- sum(xy[m, 2] * wt) / sum(wt)
    c(cx, cy, max(sqrt((xy[m, 1] - cx)^2 + (xy[m, 2] - cy)^2)) + halfDiag, sum(wt), length(m))
  }, numeric(5)))
  ll <- sf::st_coordinates(sf::st_transform(
    sf::st_as_sf(data.frame(x = circ[, 1], y = circ[, 2]), coords = c("x", "y"), crs = 2056), 4326))
  out <- data.frame(lng = ll[, "X"], lat = ll[, "Y"], radius_m = circ[, 3],
                    score = circ[, 4], nCells = as.integer(circ[, 5]))
  per <- vapply(picks, function(m) colSums(lossAt[m, , drop = FALSE]), numeric(length(bins)))
  per <- matrix(per, nrow = length(bins), dimnames = list(bins, NULL))
  usage <- vapply(picks, function(m) sum(nV[hot][m]), numeric(1))
  when <- if(length(bins) == 1) rep(bins, nrow(out)) else
    vapply(seq_len(nrow(out)), function(i){
      x <- per[, i]
      if(min(x) >= bothShare * max(x)) "both" else bins[which.max(x)]
    }, character(1))

  ord <- order(-out$score)
  ans <- data.frame(lng = out$lng, lat = out$lat, radius_m = out$radius_m,
                    score = out$score,
                    lossPct = ifelse(usage > 0, 100 * out$score / usage, 0),
                    nCells = out$nCells, when = when, stringsAsFactors = FALSE)
  for(b in bins) ans[[b]] <- per[b, ]
  ans <- ans[ord, , drop = FALSE]
  rownames(ans) <- NULL
  ans
}

#### the UI half, shared by the two pages ####

#' The words for a circle's time of day, in the session's language.
vftHeatNeedWhenLabel <- function(when, i18n){
  vapply(when, function(w){
    if(identical(w, "both")) vftTrText(i18n, "Morgen und Nachmittag")
    else vftTrText(i18n, HEAT_BIN_KEYS[[w]])
  }, character(1), USE.NAMES = FALSE)
}

#' The modal that asks which time of day to search: the choice, and under it
#' `ns("heatNeedPlan")` - the checklist of what will run, with the button that
#' starts it (vftHeatNeedPlanUI()), rendered by the page for the choice made.
vftHeatNeedModal <- function(ns, i18n, choice = "afternoon"){
  if(!isTRUE(choice %in% VFT_HEAT_NEED_CHOICES)) choice <- "afternoon"
  shiny::modalDialog(
    title = vftTrText(i18n, "Bereiche mit Hitzeminderungsbedarf erkennen"),
    shiny::tags$p(vftTrText(i18n, "Die normale Simulation wird mit der Hitzewellen-Simulation verglichen. Eingekreist werden die Orte, an denen die Wegnutzung bei Hitze am stärksten zurückgeht.")),
    shiny::radioButtons(ns("heatNeedChoice"), label = vftTrText(i18n, "Tageszeit"),
                        inline = TRUE, selected = choice,
                        choiceNames = list(vftTrText(i18n, "Morgen"), vftTrText(i18n, "Nachmittag"),
                                           vftTrText(i18n, "Beide")),
                        choiceValues = VFT_HEAT_NEED_CHOICES),
    shiny::uiOutput(ns("heatNeedPlan")),
    footer = shiny::modalButton(vftTrText(i18n, "Abbrechen")),
    easyClose = TRUE, size = "m")
}

#' The checklist under the modal's choice, and the button that starts the run.
#'
#' `here` is TRUE on the Hitzeminderung page, which can compute heat maps; on
#' step 5 a missing map means going there (the button says so). A plain button
#' with its own input, ns("heatNeedGo"), rather than an actionButton: this is
#' re-rendered on every change of the choice, and a re-rendered actionButton
#' comes back at 0.
vftHeatNeedPlanUI <- function(plan, bins, ns, i18n, here){
  row <- function(label, todo, what){
    shiny::tags$li(class = if(todo) "vftHnTodo" else "vftHnHave",
                   shiny::tags$span(class = "vftHnMark", if(todo) "▸" else "✓"),
                   shiny::tags$span(label), " – ",
                   shiny::tags$em(if(todo) what else vftTrText(i18n, "vorhanden")))
  }
  binName <- function(b) vftTrText(i18n, HEAT_BIN_KEYS[[b]])
  items <- list()
  #the Hitzeminderung door has none (plan$aoi, set by that page)
  if(isTRUE(plan$aoi))
    items <- list(row(vftTrText(i18n, "Zielgebiete"), TRUE,
                      vftTrText(i18n, "werden automatisch bestimmt")))
  items <- c(items, list(row(vftTrText(i18n, "Normale Simulation"), plan$normal,
                             vftTrText(i18n, "wird gestartet"))))
  for(b in bins){
    items <- c(items, list(
      row(sprintf(vftTrText(i18n, "Hitzekarte (%s)"), binName(b)), b %in% plan$heatMaps,
          vftTrText(i18n, if(here) "wird berechnet" else "wird auf der Seite Hitzeminderung berechnet")),
      row(sprintf(vftTrText(i18n, "Hitzewellen-Simulation (%s)"), binName(b)), b %in% plan$heatSims,
          vftTrText(i18n, "wird gestartet"))))
  }
  away <- !here && length(plan$heatMaps)
  go <- if(away) "Zur Hitzeminderung und starten"
        else if(vftHeatNeedPlanEmpty(plan)) "Erkennen" else "Starten"
  shiny::tagList(
    shiny::tags$ul(class = "vftHnPlan", items),
    shiny::tags$button(
      type = "button", class = "vftB vftConfirmBtn vftHnGo",
      onclick = sprintf("Shiny.setInputValue('%s', {n: Date.now()}, {priority: 'event'})",
                        ns("heatNeedGo")),
      vftTrText(i18n, go)))
}

#' The modal shown while a run is going: its stages, each ticked as it ends
#' (vftHeatNeedStage()), and a cancel that takes effect after the current one.
#' Not dismissable otherwise - the page must not change under the run.
vftHeatNeedRunModal <- function(stages, ns, i18n){
  shiny::modalDialog(
    title = vftTrText(i18n, "Erkennung läuft"),
    shiny::tags$ul(class = "vftHnPlan vftHnRun",
                   lapply(seq_along(stages), function(i)
                     shiny::tags$li(id = paste0("vftHnStage", i), class = "vftHnTodo",
                                    shiny::tags$span(class = "vftHnMark", "▸"),
                                    shiny::tags$span(stages[[i]])))),
    footer = shiny::tags$button(
      type = "button", class = "btn btn-default",
      onclick = sprintf("this.disabled = true; Shiny.setInputValue('%s', {n: Date.now()}, {priority: 'event'})",
                        ns("heatNeedCancel")),
      vftTrText(i18n, "Abbrechen")),
    easyClose = FALSE, size = "m")
}

#' Tick stage `i` of the run modal (and mark the next as running).
vftHeatNeedStage <- function(i){
  shinyjs::runjs(sprintf(paste0(
    "(function(){var a=document.getElementById('vftHnStage%d');",
    "if(a){a.className='vftHnHave';a.firstChild.textContent='\\u2713';}",
    "var b=document.getElementById('vftHnStage%d');if(b){b.className='vftHnActive';}})();"),
    i, i + 1))
}

#' The CSS of the two modals, the same on both pages.
VFT_HEAT_NEED_CSS <- "
.vftHnPlan{ list-style:none; margin:10px 0 14px; padding:0; }
.vftHnPlan li{ display:flex; align-items:baseline; gap:6px; padding:3px 0; }
.vftHnPlan .vftHnMark{ display:inline-block; width:14px; font-weight:700; }
.vftHnPlan li.vftHnHave{ color:#3b6e4f; }
.vftHnPlan li.vftHnTodo{ color:#6b4a00; }
.vftHnPlan li.vftHnActive{ color:#006268; font-weight:600; }
.vftHnPlan li.vftHnActive .vftHnMark{ animation:vftHnBlink 1s ease-in-out infinite alternate; }
@keyframes vftHnBlink{ from{ opacity:1; } to{ opacity:.25; } }
.vftHnGo{ width:100%; }
"

#' Take the circles off.
vftHeatNeedClear <- function(map){
  map |> leaflet::clearGroup("heatNeed") |> leaflet::removeControl("heatNeedLegend")
}

#' Draw a stored search: one circle per row, coloured by when the drop matters
#' (VFT_HEAT_NEED_COLORS), a permanent label at its top ("Nachmittag -38 %"),
#' and a legend. `stale` draws them dashed and says so in the legend.
#' `interactive = FALSE` on the Hitzeminderung page, where the circles must not
#' swallow the clicks and strokes the page is built on.
vftHeatNeedDraw <- function(map, h, i18n, stale = FALSE, interactive = TRUE, pane = "layer3"){
  if(is.null(h) || !nrow(h)) return(map)
  col <- unname(VFT_HEAT_NEED_COLORS[h$when])
  lbl <- sprintf("%s −%d %%", vftHeatNeedWhenLabel(h$when, i18n), as.integer(round(h$lossPct)))
  map <- leaflet::addCircles(map, lng = h$lng, lat = h$lat, radius = h$radius_m,
                             group = "heatNeed", color = col, weight = 3, opacity = 1,
                             dashArray = if(isTRUE(stale)) "8 6" else NULL,
                             fill = TRUE, fillColor = col, fillOpacity = 0.12,
                             options = leaflet::pathOptions(pane = pane, interactive = interactive))
  map <- leaflet::addLabelOnlyMarkers(map, lng = h$lng, lat = h$lat + h$radius_m / 110574,
                                      group = "heatNeed", label = lbl,
                                      options = leaflet::markerOptions(interactive = FALSE),
                                      labelOptions = leaflet::labelOptions(
                                        noHide = TRUE, direction = "top", offset = c(0, -2),
                                        style = list("font-weight" = "600", "font-size" = "12px")))
  whens <- intersect(names(VFT_HEAT_NEED_COLORS), unique(h$when))
  title <- vftTrText(i18n, "Hitzeminderungsbedarf")
  if(isTRUE(stale)) title <- paste0(title, " (", vftTrText(i18n, "vor den letzten Änderungen"), ")")
  leaflet::addLegend(map, position = "bottomleft", layerId = "heatNeedLegend",
                     colors = unname(VFT_HEAT_NEED_COLORS[whens]),
                     labels = vftHeatNeedWhenLabel(whens, i18n), title = title, opacity = 1)
}
