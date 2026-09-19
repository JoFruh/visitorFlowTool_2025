## Verification for the heat coefficient tables (plan steps 1-8).
## Reads exactly as the app would: read.csv2(vftData("tables/...")).
## Run:  Rscript verify_heat_tables.R

D <- Sys.getenv("VFT_TABLES",
  "C:/Users/frueh/OneDrive - Eidg. Forschungsanstalt WSL/Dokumente/visitorFlowTool_DATA/data/tables")

fails <- 0L
ok <- function(label, cond, detail = "") {
  cond <- isTRUE(cond)
  if (!cond) fails <<- fails + 1L
  cat(sprintf("%-4s %s%s\n", if (cond) "PASS" else "FAIL", label,
              if (nzchar(detail)) paste0("  [", detail, "]") else ""))
}

mat <- read.csv2(file.path(D, "heat_materials.csv"), stringsAsFactors = FALSE)
dec <- read.csv2(file.path(D, "heat_decay.csv"),     stringsAsFactors = FALSE)
geo <- read.csv2(file.path(D, "heat_geometry.csv"),  stringsAsFactors = FALSE)
ref <- read.csv2(file.path(D, "heat_references.csv"), stringsAsFactors = FALSE)

cat("\n== 1. parse check: numeric columns must not come back character ==\n")
ok("materials pet_0m_K is numeric",  is.numeric(mat$pet_0m_K),  class(mat$pet_0m_K))
ok("materials tmrt_0m_K is numeric", is.numeric(mat$tmrt_0m_K), class(mat$tmrt_0m_K))
ok("decay amp_edge_K is numeric",    is.numeric(dec$amp_edge_K), class(dec$amp_edge_K))
ok("decay pet_150m_K is numeric",    is.numeric(dec$pet_150m_K), class(dec$pet_150m_K))
ok("geometry value is numeric",      is.numeric(geo$value),      class(geo$value))

cat("\n== 2. completeness ==\n")
GROUND_IDS <- c(1, 2, 3, 4, 5, 8); CANOPY_IDS <- c(6, 7, 9)
BINS <- c("morning", "midday", "afternoon")
g <- mat[mat$level == "ground", ]
ok("ground rows = 6 classes x 2 shade x 3 bins = 36", nrow(g) == 36, nrow(g))
ok("ground covers exactly the expected ids", setequal(unique(g$class_id), GROUND_IDS))
ok("every ground class has both shade states in all 3 bins",
   all(table(g$class_id, g$shaded) == 3))
cn <- mat[mat$level == "canopy", ]
ok("canopy rows = 3 classes x 3 bins = 9", nrow(cn) == 9, nrow(cn))
ok("canopy covers exactly the expected ids", setequal(unique(cn$class_id), CANOPY_IDS))
ok("all time bins spelled as expected", setequal(unique(mat$time_bin), BINS))
ok("no duplicate (class,shaded,bin) keys",
   !any(duplicated(mat[c("class_id", "shaded", "time_bin")])))
ok("decay has one row per paint class (9)",
   nrow(dec) == 9 && setequal(dec$class_id, c(GROUND_IDS, CANOPY_IDS)))

cat("\n== 3. join against PAINT_CATEGORIES ==\n")
PAINT <- data.frame(
  id = 1:9,
  name = c("grass","bush","artificial","natural","water",
           "canopy_artificial","canopy_tree","artificial_block","canopy_cleared"),
  stringsAsFactors = FALSE)
ok("no material class_id missing from PAINT_CATEGORIES",
   all(mat$class_id %in% PAINT$id))
ok("no PAINT_CATEGORIES id missing from the tables",
   all(PAINT$id %in% c(mat$class_id, dec$class_id)))
nm <- merge(unique(mat[c("class_id","class_name")]), PAINT, by.x = "class_id", by.y = "id")
ok("class_name agrees with PAINT_CATEGORIES$name", all(nm$class_name == nm$name),
   paste(nm$class_name[nm$class_name != nm$name], collapse = ","))

cat("\n== 4. derived decay columns agree with the stored parameters ==\n")
## Phase 4 applies the decay as a distance-weighted SUM over every qualifying
## source cell, so a sampled column is not a point on the radial curve - it is
## that curve integrated over the half-plane of source a straight edge presents.
## Normalising by the half-plane is what keeps amp_edge_K meaning what the
## literature measured. Kept identical to heat_decay_halfplane() in
## R/heat_helpers.R and decay_at() in build_heat_tables.py; if those three ever
## disagree, this is the check that says so.
weight <- function(r, half, mx){
  f0 <- 2^(-mx / half)
  ifelse(r > mx, 0, (2^(-r / half) - f0) / (1 - f0))
}
profile <- function(d, amp, half, mx, n = 20000){
  if (is.na(half) || is.na(mx) || d >= mx) return(0)
  h  <- mx / n;  r  <- (seq_len(n) - 0.5) * h
  den <- sum(weight(r, half, mx) * pi * r) * h
  h2 <- (mx - d) / n; r2 <- d + (seq_len(n) - 0.5) * h2
  num <- sum(weight(r2, half, mx) * 2 * r2 * acos(pmin(1, d / r2))) * h2
  amp * num / den
}
SAMP <- c(5, 25, 75, 150)
bad <- character(0)
for (i in seq_len(nrow(dec))) {
  if (is.na(dec$half_dist_m[i])) next
  for (d in SAMP) {
    want <- profile(d, dec$amp_edge_K[i], dec$half_dist_m[i], dec$max_extent_m[i])
    got <- dec[[sprintf("pet_%dm_K", d)]][i]
    if (abs(want - got) > 0.006)   # derived columns carry 2 decimals
      bad <- c(bad, sprintf("%s@%dm want %.3f got %.3f", dec$class_name[i], d, want, got))
  }
}
ok("all derived samples match amp/half/max", length(bad) == 0, paste(bad, collapse = "; "))

## the two properties the normalisation exists to guarantee
edge <- sapply(seq_len(nrow(dec)), function(i)
  profile(0, dec$amp_edge_K[i], dec$half_dist_m[i], dec$max_extent_m[i]))
endv <- sapply(seq_len(nrow(dec)), function(i)
  profile(dec$max_extent_m[i], dec$amp_edge_K[i], dec$half_dist_m[i], dec$max_extent_m[i]))
ok("the curve reaches exactly zero at max_extent_m (no ring artefact)",
   all(abs(endv) < 1e-12))
has <- !is.na(dec$half_dist_m)
ok("a straight edge of a large patch returns exactly amp_edge_K",
   all(abs(edge[has] - dec$amp_edge_K[has]) < 0.002),
   sprintf("max dev %.5f", max(abs(edge[has] - dec$amp_edge_K[has]))))

cat("\n== 5. ordering sanity (per time bin) ==\n")
pv <- function(nm, sh, b) mat$pet_0m_K[mat$class_name == nm & mat$shaded == sh & mat$time_bin == b]
for (b in BINS) {
  ok(sprintf("[%s] artificial > natural > grass (sunlit)", b),
     pv("artificial",0,b) > pv("natural",0,b) && pv("natural",0,b) > pv("grass",0,b))
  ok(sprintf("[%s] shaded grass cooler than sunlit grass", b), pv("grass",1,b) < pv("grass",0,b))
  gb <- g[g$time_bin == b, ]
  ok(sprintf("[%s] every shaded row below its sunlit twin", b),
     all(mapply(function(id) pv(PAINT$name[id], 1, b) < pv(PAINT$name[id], 0, b), GROUND_IDS)))
  sun <- diff(range(gb$pet_0m_K[gb$shaded == 0])); shd <- diff(range(gb$pet_0m_K[gb$shaded == 1]))
  ok(sprintf("[%s] shade collapses the material spread (%.1f -> %.1f K)", b, sun, shd), shd < sun)
}

cat("\n== 6. diurnal / thermal inertia ==\n")
ok("asphalt hotter in the afternoon than the morning",
   pv("artificial",0,"afternoon") > pv("artificial",0,"morning"))
gap <- function(b) pv("artificial",0,b) - pv("grass",0,b)
ok(sprintf("asphalt-grass gap widens into the afternoon (%.1f -> %.1f K)",
           gap("morning"), gap("afternoon")), gap("afternoon") > gap("morning"))
rng <- function(nm, sh) diff(range(mat$pet_0m_K[mat$class_name == nm & mat$shaded == sh]))
ok(sprintf("water varies least across bins of any sunlit ground class (%.1f K)", rng("water",0)),
   rng("water",0) <= min(sapply(PAINT$name[GROUND_IDS], rng, sh = 0)))

cat("\n== 7. half-distances inside the temperate band ==\n")
hd <- dec$half_dist_m[!is.na(dec$half_dist_m)]
ok("every half_dist_m within 3-150 m", all(hd >= 3 & hd <= 150), paste(range(hd), collapse = "-"))

cat("\n== 8. magnitude sanity ==\n")
ok(sprintf("|pet_150m_K| <= 1.0 for all classes (max %.2f)", max(abs(dec$pet_150m_K))),
   max(abs(dec$pet_150m_K)) <= 1.0)
ok(sprintf("|amp_edge_K| <= 2.0 for all classes (max %.2f)", max(abs(dec$amp_edge_K))),
   max(abs(dec$amp_edge_K)) <= 2.0)
ok("decay magnitude strictly falls with distance for every cool/warm source",
   all(apply(abs(dec[sprintf("pet_%dm_K", SAMP)]), 1, function(r) all(diff(r) <= 1e-9))))

cat("\n== 9. traceability ==\n")
ok("no material row lacks a reference", all(nzchar(mat$reference)))
ok("no decay row lacks a reference",    all(nzchar(dec$reference)))
ok("every reference key used is defined in heat_references.csv",
   all(unique(c(mat$reference, dec$reference, geo$reference)) %in% ref$reference),
   paste(setdiff(unique(c(mat$reference, dec$reference, geo$reference)), ref$reference), collapse = ","))
ok("every shape_only row is flagged as such",
   all(mat$climate_transfer[mat$reference == "Speak et al. 2020"] == "shape_only"))

cat("\n== 10. side-by-side with today's hardcoded constants ==\n")
HEAT_GROUND <- c("1"=0.3,"2"=0.2,"3"=0.7,"4"=0.5,"5"=0.0)
HEAT_CANOPY <- c("0"=0.0,"6"=-0.5,"7"=-0.8,"9"=0.0)
mid <- mat[mat$time_bin == "midday" & mat$shaded %in% c("0","NA"), ]
cmp <- data.frame(
  class = mid$class_name,
  old   = ifelse(mid$class_id %in% names(HEAT_GROUND),
                 HEAT_GROUND[as.character(mid$class_id)],
                 HEAT_CANOPY[as.character(mid$class_id)]),
  new_pet_K = mid$pet_0m_K, row.names = NULL)
cmp$old[is.na(cmp$old)] <- NA
print(cmp, row.names = FALSE)

cat(sprintf("\n%s  %d check(s) failed\n", if (fails == 0) "ALL CHECKS PASSED" else "FAILURES", fails))
quit(status = if (fails == 0) 0 else 1)
