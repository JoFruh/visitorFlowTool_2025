## Build heat_coefficients.xlsx - the review workbook for the heat parameter
## tables. Reads the CSVs exactly as the app does, so the workbook can never
## drift from the machine-readable source of truth.
##
## Charts follow the dataviz skill: validated default categorical theme used in
## its documented fixed order (the validator is a node script and node is not
## installed here, so no palette is invented), every series direct-labelled so
## identity never rests on colour alone, recessive axes, 2px lines.

suppressPackageStartupMessages(library(openxlsx))

D <- Sys.getenv("VFT_TABLES",
  "C:/Users/frueh/OneDrive - Eidg. Forschungsanstalt WSL/Dokumente/visitorFlowTool_DATA/data/tables")

mat <- read.csv2(file.path(D, "heat_materials.csv"),  stringsAsFactors = FALSE)
dec <- read.csv2(file.path(D, "heat_decay.csv"),      stringsAsFactors = FALSE)
geo <- read.csv2(file.path(D, "heat_geometry.csv"),   stringsAsFactors = FALSE)
ref <- read.csv2(file.path(D, "heat_references.csv"), stringsAsFactors = FALSE)

## -- dataviz tokens ---------------------------------------------------------
SURFACE <- "#fcfcfb"; INK <- "#0b0b0b"; INK2 <- "#52514e"; MUTED <- "#898781"
## validated categorical theme, fixed order - never cycled, never re-hued
SERIES <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#e87ba4", "#008300",
            "#4a3aa7", "#e34948")

BINS <- c("morning", "midday", "afternoon")
GROUND_IDS <- c(1, 2, 3, 4, 5, 8)

grid_axes <- function(xlim, ylim, xat, xlab_txt, ylab_txt, xlabels = xat) {
  plot.new(); plot.window(xlim = xlim, ylim = ylim)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],
       col = SURFACE, border = NA)
  abline(h = pretty(ylim), col = "#ecebe8", lwd = 1)
  abline(h = 0, col = "#d8d7d3", lwd = 1)
  axis(1, at = xat, labels = xlabels, col = MUTED, col.axis = MUTED,
       cex.axis = 0.85, lwd = 1, tck = -0.02)
  axis(2, at = pretty(ylim), col = MUTED, col.axis = MUTED,
       cex.axis = 0.85, lwd = 1, tck = -0.02, las = 1)
  mtext(xlab_txt, 1, line = 2.3, col = INK2, cex = 0.9)
  mtext(ylab_txt, 2, line = 2.6, col = INK2, cex = 0.9)
}

## -- sheet 2: diurnal -------------------------------------------------------
## Job: change over time, comparison across materials -> overlapping lines.
plot_diurnal <- function() {
  s <- mat[mat$level == "ground" & mat$shaded == 0, ]
  cls <- GROUND_IDS
  nm <- sapply(cls, function(i) s$class_name[s$class_id == i][1])
  par(mar = c(4, 4.5, 3, 7.5), bg = SURFACE, family = "sans")
  ys <- range(s$pet_0m_K) + c(-0.6, 0.6)
  grid_axes(c(1, 3.02), ys, 1:3, "time of day", "dPET (K) vs unshaded grass at midday",
            xlabels = BINS)
  ends <- sapply(cls, function(i) s$pet_0m_K[s$class_id == i & s$time_bin == "afternoon"])
  ## de-collide the direct labels: nudge apart any that land within a line height
  lab_y <- ends; gap <- diff(ys) * 0.05
  for (i in order(lab_y)) {
    lower <- lab_y[lab_y < lab_y[i] + gap & seq_along(lab_y) != i]
    if (length(lower)) lab_y[i] <- max(max(lower) + gap, lab_y[i])
  }
  for (k in seq_along(cls)) {
    v <- sapply(BINS, function(b) s$pet_0m_K[s$class_id == cls[k] & s$time_bin == b])
    lines(1:3, v, col = SERIES[k], lwd = 2)
    points(1:3, v, col = SERIES[k], pch = 19, cex = 1.1)
    ## direct label: identity never rests on colour alone
    if (abs(lab_y[k] - v[3]) > 1e-9)
      segments(3.01, v[3], 3.05, lab_y[k], col = MUTED, lwd = 0.8, xpd = NA)
    text(3.06, lab_y[k], nm[k], col = INK2, cex = 0.8, adj = 0, xpd = NA)
  }
  title("Diurnal shape by material, in full sun", col.main = INK, cex.main = 1.05, adj = 0)
  mtext("asphalt climbs into the afternoon; water stays flat", 3, line = 0.1,
        col = MUTED, cex = 0.82, adj = 0)
}

## -- sheet 3: decay ---------------------------------------------------------
## Job: is each fitted half-distance plausible? -> small multiples, one panel
## per class. A single series per panel needs no legend, and an implausible
## half-distance is obvious against its own marked midpoint.
plot_decay <- function() {
  d <- dec[!is.na(dec$half_dist_m), ]
  op <- par(mfrow = c(2, 4), mar = c(3.4, 3.6, 2.6, 1), bg = SURFACE, family = "sans",
            oma = c(0, 0, 2.4, 0))
  on.exit(par(op))
  xs <- seq(0, 220, by = 2)
  for (i in seq_len(nrow(d))) {
    amp <- d$amp_edge_K[i]; hd <- d$half_dist_m[i]; mx <- d$max_extent_m[i]
    y <- ifelse(xs > mx, 0, amp * 2^(-xs / hd))
    yl <- range(c(0, amp)) * 1.25
    grid_axes(c(0, 220), yl, c(0, 50, 100, 150, 200), "distance (m)", "dPET (K)")
    lines(xs, y, col = SERIES[1], lwd = 2)
    ## half-distance marker and the max-extent cut-off
    segments(hd, 0, hd, amp / 2, col = MUTED, lwd = 1, lty = 3)
    points(hd, amp / 2, col = SERIES[1], pch = 19, cex = 1.1)
    abline(v = mx, col = "#d8d7d3", lwd = 1, lty = 2)
    resid <- amp * 2^(-mx / hd)
    text(mx - 3, amp * 0.06, sprintf("cut at %+.2f K", resid), col = MUTED,
         cex = 0.66, adj = 1, srt = 90)
    text(hd + 5, amp / 2, sprintf("half %gm", hd), col = INK2, cex = 0.78, adj = 0)
    title(d$class_name[i], col.main = INK, cex.main = 0.98, adj = 0)
    mtext(sprintf("edge %+.1f K, max %gm, min %g ha", amp, mx, d$min_patch_ha[i]),
          3, line = -0.1, col = MUTED, cex = 0.68, adj = 0)
  }
  mtext("Advective decay per class - dashed line is max_extent_m",
        3, outer = TRUE, line = 0.4, col = INK, cex = 1.0, adj = 0.01)
}

## -- shadow lengths, for the geometry sheet ---------------------------------
shadow_tbl <- function() {
  g <- function(p) geo$value[geo$parameter == p]
  hts <- geo[grepl("^height_", geo$parameter), ]
  out <- data.frame(class = sub("^height_", "", hts$parameter),
                    height_m = hts$value, stringsAsFactors = FALSE)
  for (b in BINS) {
    out[[paste0("shadow_", b, "_m")]] <-
      round(sapply(seq_len(nrow(out)), function(i)
        g(sprintf("shadow_length_%s_%s", out$class[i], b))), 1)
  }
  out$sun_elevation_deg <- NULL
  out
}

CONVERSION <- data.frame(
  step = c("surface -> radiant load", "radiant load -> PET", "climate transfer",
           "time binning", "reference point"),
  rule = c("dTglobe = 0.33 x dTsurface",
           "dPET = 0.5 x dTmrt",
           "magnitudes from Cfb/Dfb sources only; others contribute shape",
           "morning ~09:00, midday ~12:00, afternoon ~15:00 solar time",
           "unshaded short grass at midday = 0 K"),
  basis = c(
    "Measured directly by Speak et al. 2020 across five materials: dTglobe/dTsurface ranges 0.17 (grass) to 0.45 (bitumen), mean 0.33. Replaces the 0.4-0.5 rule of thumb assumed before the data were checked.",
    "Single linear conversion. PET responds to Tmrt with a slope well below 1 because air temperature, wind and humidity also enter the heat balance. Assumption, not a measurement.",
    "Speak et al. is Sydney (Cfa) and supplies ordering and ratios only. Schwaab, Meeussen, Hathway, Munich and Rahman/Zurich supply magnitudes.",
    "Asphalt peaks 1-2 h after solar noon, so the afternoon bin is placed after the peak, not at it.",
    "Best-measured reference surface and the natural neutral for a landscape tool; keeps higher = hotter so the existing diverging palette still reads correctly."),
  confidence = c("medium", "medium", "high", "medium", "high"),
  stringsAsFactors = FALSE)

LIMITS <- data.frame(
  limitation = c(
    "Not a validated model",
    "Two stacked conversions",
    "Buildings are weakly sourced",
    "Bush has no dedicated study",
    "Water is the weakest row",
    "Distance decay is outward-facing, the forest evidence is inward-facing",
    "min_patch_ha is contested by the Munich result",
    "No wind, no humidity, no season other than midsummer",
    "max_extent_m truncates rather than tapers"),
  detail = c(
    "These numbers replace invented constants with sourced ones. That is a strict improvement, not a validation. Nothing here has been compared against a measurement in the study area.",
    "Most dPET values pass through surface -> globe -> PET. Each step carries its own error and they multiply.",
    "Class 8 had no value at all before (NA). The value here is inferred from SOLWEIG's sunlit-facade finding, not measured. Low confidence.",
    "Class 2 is interpolated between grass and closed canopy. No temperate shrub-layer PET study was found.",
    "Hathway & Sharples measured cooling ABOVE the river and found it reduced in summer when water was warmer. Summer river cooling in temperate cities is small and bank-form dependent.",
    "Meeussen measures edge-to-INTERIOR gradients (how cooling builds up inside a forest), not how far cooling projects outward into open land. The half_dist_m for canopy_tree borrows from park-cooling studies instead.",
    "Munich found structural complexity can outweigh size, and that low-complexity parks were sometimes WARMER than their surroundings. A pure area threshold is therefore an approximation.",
    "All values are midsummer, low-wind, clear-sky. The geometry sheet fixes declination at mid-July.",
    "The decay curve is cut to zero at max_extent_m while still carrying 8-12 percent of its edge amplitude (canopy_tree: -0.12 K at 200 m; see the decay sheet, where each cut-off is annotated). Applied to a raster as written, that step will draw a visible RING ARTEFACT at exactly that radius around every patch. Phase 4 must taper the last stretch to zero, or set max_extent_m at 5+ half-distances, rather than truncating."),
  stringsAsFactors = FALSE)

## -- assemble ---------------------------------------------------------------
wb <- createWorkbook()
hdr <- createStyle(textDecoration = "bold", fgFill = "#f0efec", border = "bottom",
                   borderColour = MUTED, halign = "left", valign = "top")
wrap <- createStyle(wrapText = TRUE, valign = "top")

add <- function(name, df, widths = NULL, wrapcols = NULL) {
  addWorksheet(wb, name)
  writeData(wb, name, df, headerStyle = hdr)
  freezePane(wb, name, firstActiveRow = 2)
  setColWidths(wb, name, seq_along(df), widths %||% "auto")
  if (!is.null(wrapcols))
    addStyle(wb, name, wrap, rows = 2:(nrow(df) + 1), cols = wrapcols,
             gridExpand = TRUE, stack = TRUE)
}
`%||%` <- function(a, b) if (is.null(a)) b else a

add("materials", mat, widths = c(8,18,8,20,8,11,10,11,11,16,22,60), wrapcols = 12)

addWorksheet(wb, "diurnal")
writeData(wb, "diurnal", "Diurnal shape by material (sunlit ground classes)", startRow = 1)
piv <- reshape(mat[mat$level == "ground" & mat$shaded == 0,
                   c("class_name", "time_bin", "pet_0m_K")],
               idvar = "class_name", timevar = "time_bin", direction = "wide")
names(piv) <- sub("^pet_0m_K\\.", "", names(piv))
writeData(wb, "diurnal", piv[c("class_name", BINS)], startRow = 3, headerStyle = hdr)
png(p1 <- tempfile(fileext = ".png"), width = 1000, height = 620, res = 130, bg = SURFACE)
plot_diurnal(); dev.off()
insertImage(wb, "diurnal", p1, width = 7.7, height = 4.8, startRow = 12, startCol = 1)
setColWidths(wb, "diurnal", 1:4, c(22, 12, 12, 12))

addWorksheet(wb, "decay")
writeData(wb, "decay", dec, startRow = 1, headerStyle = hdr)
png(p2 <- tempfile(fileext = ".png"), width = 1500, height = 760, res = 130, bg = SURFACE)
plot_decay(); dev.off()
insertImage(wb, "decay", p2, width = 11.5, height = 5.8, startRow = 13, startCol = 1)
setColWidths(wb, "decay", seq_along(dec), "auto")

add("geometry", geo, widths = c(30, 12, 22, 22, 12, 52), wrapcols = 6)
addWorksheet(wb, "shadow_lengths")
writeData(wb, "shadow_lengths",
          "Shadow length = height / tan(sun elevation), 47 deg N, mid-July", startRow = 1)
writeData(wb, "shadow_lengths", shadow_tbl(), startRow = 3, headerStyle = hdr)
setColWidths(wb, "shadow_lengths", 1:5, c(22, 11, 18, 18, 18))

add("references", ref, widths = c(18, 62, 26, 26, 12, 22, 22, 20, 14, 14, 70),
    wrapcols = c(2, 11))
add("conversion", CONVERSION, widths = c(26, 40, 80, 12), wrapcols = 3)
add("limitations", LIMITS, widths = c(30, 95), wrapcols = 2)

out <- file.path(D, "heat_coefficients.xlsx")
saveWorkbook(wb, out, overwrite = TRUE)
cat("wrote", out, "\n")
cat("sheets:", paste(names(wb), collapse = ", "), "\n")
