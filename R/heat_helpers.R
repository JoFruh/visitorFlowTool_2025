#' Heat signature of a landscape, from the ground and canopy land cover.
#'
#' One raster answering "how hot does it feel here", derived from what the
#' surface is made of, what stands above it, what the sun is doing, and what the
#' neighbourhood is made of. It is computed from the *composite* - the surveyed
#' land cover with the user's paint laid over it - so that editing a design
#' changes the answer, which is the whole reason the heat-mitigation editor
#' exists.
#'
#' Phase 4 of the rework in HEAT_COEFFICIENTS.md. The model, per cell and per
#' time bin:
#'
#'   PET = local(material, shaded, bin)          #heat_materials.csv
#'       + svf(SVF)                              #enclosure, sunlit cells only
#'       + wall(sunlit facade nearby)            #SOLWEIG's city hot-spot
#'       + advective(distance to cool/warm patches)   #heat_decay.csv
#'
#' The output is **degrees Kelvin of PET relative to unshaded short grass at
#' solar midday**, which is the reference surface the tables are built on. Zero
#' therefore means "as comfortable as an open lawn at noon", positive is worse,
#' negative is better.
#'
#' WHAT CHANGED FROM THE PREVIOUS MODEL, and why each mattered:
#'
#'  1. **The cell's own material now enters directly.** The old model built heat
#'     purely from two focal means, so standing *under* a tree scored almost the
#'     same as standing 9 m away - it structurally could not express the largest
#'     and best-evidenced effect in the whole literature.
#'  2. **The output carries a unit.** The old weights were 0.1 and 0.01, which do
#'     not sum to 1, so the result landed in roughly +-0.09 of nothing in
#'     particular. These are kelvin.
#'  3. **Shade is cast, not dropped straight down** (Phase 2), and it selects a
#'     *row* of the material table rather than adding a constant - because
#'     shading collapses the differences between materials rather than shifting
#'     them all equally (12.3 K of spread sunlit, 4.6 K shaded).
#'  4. **Distance decay is exponential and per class**, replacing one shared
#'     box-car that weighted a cell at 1 m the same as one at 99 m. The
#'     characteristic length varies about 40x between materials, which a single
#'     kernel throws away.
#'  5. **Patch size is tested.** A 20 m2 lawn has no 100 m cooling effect; the
#'     park studies that produced these numbers measure hectares.
#'  6. **Time of day is a parameter**, because thermal inertia reorders the
#'     materials: asphalt peaks 1-2 h after solar noon while grass and water
#'     barely move.
#'
#' The numbers live in tables outside the repo (see HEAT_COEFFICIENTS.md), not
#' in this file, so retuning never means editing code.


# -------------------------------------------------------------- constants ---

#' Output resolution in metres.
#'
#' 5, not the 1 m of the land cover: heat is a smoothed field and does not need
#' that, and coarsening first is what keeps the distance transforms affordable.
#' It was 10 m before Phase 2, which put a whole shadow inside a single cell -
#' at 45.8 deg a 15 m tree casts 14.6 m, so 10 m could not see it move.
HEAT_RES <- 5

#' Fully opaque: the heat surface is the thing being read when it is on, and at
#' partial opacity the land cover beneath would tint it and misreport the value.
HEAT_OPACITY <- 1

#' Diverging, because the scale runs negative (shaded vegetation) as well as
#' positive (open hard surfacing), so a one-sided ramp would waste half its range
#' and hide the difference between cool and merely average.
HEAT_COLORS <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")

#' Which time bin the read-out shows when nothing has been chosen.
#'
#' Midday rather than afternoon, even though afternoon is hotter: midday is the
#' reference the tables' zero point is defined at, so it is the bin where the
#' scale reads most directly.
HEAT_BIN_DEFAULT <- "midday"

#' The two geometry terms, switchable because they are the weakest links.
#'
#' Both come from `heat_geometry.csv` rows marked `confidence = low`, and they
#' pull in opposite directions on purpose: enclosure removes sky (cooling a
#' sunlit cell by taking away diffuse radiation) while a sunlit facade adds
#' long-wave. There is a real double-counting risk between the SVF term and the
#' shaded material rows, which is why the SVF term is applied to sunlit cells
#' only - a shaded cell has already been priced by its own row. Set either to
#' FALSE to see the model without it.
HEAT_APPLY_SVF  <- TRUE
HEAT_APPLY_WALL <- TRUE


# ------------------------------------------------------------------ tables --

#' The material table, cached. Semicolon-separated *and* decimal-comma, so
#' read.csv2 - read.csv returns every number as a character string that becomes
#' NA on as.numeric(), silently emptying the model.
heatMaterials <- local({
  cache <- NULL
  function(refresh = FALSE){
    if(!is.null(cache) && !refresh) return(cache)
    f <- try(vftData("tables/heat_materials.csv"), silent = TRUE)
    if(inherits(f, "try-error") || !file.exists(f)){
      warning("heat_materials.csv not found; the heat model cannot run")
      return(NULL)
    }
    cache <<- utils::read.csv2(f, stringsAsFactors = FALSE)
    cache
  }
})

#' The distance-decay table, cached.
heatDecay <- local({
  cache <- NULL
  function(refresh = FALSE){
    if(!is.null(cache) && !refresh) return(cache)
    f <- try(vftData("tables/heat_decay.csv"), silent = TRUE)
    if(inherits(f, "try-error") || !file.exists(f)) return(NULL)
    cache <<- utils::read.csv2(f, stringsAsFactors = FALSE)
    cache
  }
})


# ---------------------------------------------------------------- pipeline --

#' Map class ids to values, with anything unlisted becoming `others`.
heat_subst <- function(r, ids, vals, others = NA){
  keep <- !is.na(ids) & !is.na(vals)
  if(!any(keep)) return(terra::ifel(is.na(r), others, others))
  terra::subst(r, from = as.integer(ids[keep]), to = as.numeric(vals[keep]),
               others = others)
}

#' The local radiative term: what this cell is made of, in the state it is in.
#'
#' `shade` is the 0/1 raster from Phase 2. It picks which *row* of the table
#' applies rather than adding a constant, which is the point - Speak et al.
#' measured that shading collapses the spread between materials instead of
#' shifting them all by the same amount.
#'
#' Ground classes with no row (0, "unclassified") become NA and drop out.
#' Canopy classes with no row contribute 0, not NA: open sky is a *known* state
#' meaning "nothing overhead", and as NA it would propagate through the addition
#' and empty out every unshaded cell on the map.
heatLocalTerm <- function(ground, canopy, shade, bin = HEAT_BIN_DEFAULT,
                          mat = heatMaterials()){
  if(is.null(mat)) return(NULL)
  m <- mat[mat$time_bin == bin, ]
  if(!nrow(m)) return(NULL)

  g   <- m[m$level == "ground", ]
  sun <- heat_subst(ground, g$class_id[g$shaded == 0], g$pet_0m_K[g$shaded == 0])
  shd <- heat_subst(ground, g$class_id[g$shaded == 1], g$pet_0m_K[g$shaded == 1])
  base <- terra::ifel(shade == 1, shd, sun)

  cn <- m[m$level == "canopy", ]
  extra <- heat_subst(canopy, cn$class_id, cn$pet_0m_K, others = 0)
  base + terra::ifel(is.na(extra), 0, extra)
}

#' Resolution in metres for the advective convolution.
#'
#' Coarser than HEAT_RES on purpose. Patches are identified at the full grid, so
#' nothing small is lost and no patch area is misjudged; only the convolution
#' runs coarse, on a *fractional cover* mask that preserves the total source
#' area. The field being convolved is smooth by construction - the shortest
#' half-distance in the table is 25 m - so this costs almost nothing and saves a
#' great deal: measured over central Sion, 10 m runs 4.6x faster than 5 m for a
#' maximum error of 0.076 K and a mean of 0.028 K, an order of magnitude below
#' the uncertainty on the parameters themselves.
HEAT_ADV_RES <- 10

#' The radial kernel shape: 1 at the source, 0 at `maxext`.
#'
#' A plain `2^(-d/half)` cut off at max_extent_m is still carrying 8-12 % of its
#' amplitude when it is cut, and a step like that produces a **visible ring** at
#' exactly that radius around every patch - limitation 9 in
#' HEAT_COEFFICIENTS.md. Subtracting the end value and renormalising removes the
#' step without touching the reviewed parameters: still 1 at the source, still
#' the fitted half-distance shape, but it lands on zero instead of falling off a
#' cliff.
heat_decay_weight <- function(d, half, maxext){
  f0 <- 2^(-maxext / half)
  w  <- (2^(-d / half) - f0) / (1 - f0)
  ifelse(d > maxext, 0, w)
}

#' The 2-D convolution kernel, and its sum over a half-plane.
#'
#' The half-plane sum is the normaliser, and choosing it is the whole design: it
#' is the configuration a cell sees at the straight edge of a very large patch,
#' which is exactly the configuration the literature measured `amp_edge_K` in.
#' Dividing by it means a long straight forest edge returns `amp_edge_K` on the
#' nose, and everything smaller returns proportionally less - so the parameter
#' keeps the meaning its name claims.
#'
#' The centre cell is zeroed: a source cell does not advect onto itself, and its
#' own state is already carried by the local term.
heat_decay_kernel <- function(half, maxext, res){
  rad <- ceiling(maxext / res)
  ij  <- expand.grid(i = -rad:rad, j = -rad:rad)
  d   <- sqrt(ij$i^2 + ij$j^2) * res
  m   <- matrix(heat_decay_weight(d, half, maxext), nrow = 2 * rad + 1)
  m[rad + 1, rad + 1] <- 0
  m
}

#' Convolve a raster with a radial kernel, by FFT.
#'
#' Replaces terra::focal(w = k, fun = "sum", na.rm = TRUE, fillvalue = 0) and
#' means exactly the same thing, to 7e-13. focal walks every kernel cell for
#' every raster cell - 41 x 41 weights over 311 x 411 cells is 215 M
#' multiplications for the tree layer alone - where an FFT does it in
#' n log n. Measured on that layer: 0.413 s -> 0.049 s, and 1.43 s -> 0.17 s
#' over all seven classes.
#'
#' CORRELATION AND CONVOLUTION ARE THE SAME THING HERE, and only because of what
#' the kernel is. An FFT computes a convolution, which is a correlation with the
#' kernel flipped in both axes; focal computes the correlation. heat_decay_kernel()
#' builds its weights from sqrt(i^2 + j^2), so it is symmetric under that flip and
#' the two agree. A kernel that ever stops being radially symmetric - a wind
#' direction, say - breaks this silently and in a way no check on a round kernel
#' can catch, so it would need rev() on both axes here.
#'
#' The padding is what keeps a circular convolution from wrapping: an image of
#' nr x nc and a kernel of 2*rad+1 needs at least nr + 2*rad rows, and
#' stats::nextn() rounds that up to a length with small prime factors. NA is 0,
#' which is what na.rm = TRUE meant, and the zeros past the edge are what
#' fillvalue = 0 meant.
heat_conv <- function(r, k){
  m <- terra::as.matrix(r, wide = TRUE)
  m[is.na(m)] <- 0
  nr <- nrow(m); nc <- ncol(m)
  rad <- (nrow(k) - 1L) %/% 2L
  nr2 <- stats::nextn(nr + 2L * rad); nc2 <- stats::nextn(nc + 2L * rad)

  P <- matrix(0, nr2, nc2); P[seq_len(nr), seq_len(nc)] <- m
  K <- matrix(0, nr2, nc2); K[seq_len(nrow(k)), seq_len(ncol(k))] <- k
  C <- Re(stats::fft(stats::fft(P) * stats::fft(K), inverse = TRUE)) / (nr2 * nc2)

  #t() before flattening, for the reason heatShadeRaster() spells out: terra
  #fills row by row, as.vector() on an R matrix walks column by column.
  terra::setValues(terra::rast(r),
                   as.vector(t(C[rad + seq_len(nr), rad + seq_len(nc), drop = FALSE])))
}

heat_kernel_halfplane <- function(m){
  rad <- (nrow(m) - 1) / 2
  sum(m[, seq_len(rad)])
}

#' The response at distance `d` from the straight edge of a very large patch.
#'
#' The analytic form of what the convolution does, for the derived columns of
#' `heat_decay.csv` and for verification. Integrating the radial kernel over the
#' half-plane beyond `d`:
#'
#'   numerator(d) = int_d^R w(r) * 2r*acos(d/r) dr
#'   denominator  = int_0^R w(r) * pi*r dr
#'
#' A Riemann sum rather than integrate(): acos(d/r) has an infinite derivative at
#' r = d, which adaptive quadrature handles badly, and the same fixed sum is used
#' by build_heat_tables.py so the two agree to the printed decimal.
heat_decay_halfplane <- function(d, amp, half, maxext, n = 20000){
  if(is.na(half) || is.na(maxext) || d >= maxext) return(0)
  h   <- maxext / n
  r   <- (seq_len(n) - 0.5) * h
  den <- sum(heat_decay_weight(r, half, maxext) * pi * r) * h
  h2  <- (maxext - d) / n
  r2  <- d + (seq_len(n) - 0.5) * h2
  num <- sum(heat_decay_weight(r2, half, maxext) * 2 * r2 *
               acos(pmin(1, d / r2))) * h2
  amp * num / den
}

#' The advective term: what the *neighbourhood* does to this cell.
#'
#' A physically different quantity from the local term and never summed with it
#' at the same scale - this is air that has been cooled or warmed elsewhere and
#' moved, which is an order of magnitude smaller than radiant load and reaches
#' much further. Bowler's temperate meta-analysis puts parks at 0.94 K, not the
#' 3+ K of tropical remote-sensing studies.
#'
#' A distance-weighted SUM over every qualifying source cell, not the distance to
#' the nearest one. That distinction is the whole point, and the first version of
#' this function got it wrong: with distance-to-nearest, a 0.25 ha copse and a
#' 64 ha forest produce **bit-identical** fields, because patch size enters only
#' as the binary `min_patch_ha` gate and is then discarded. A design tool must
#' not say that scattering a few small clumps is worth as much as building a
#' park, and that one did.
#'
#' Summing fixes it without any new parameter, because the size dependence falls
#' out of geometry - a bigger wood simply has more of itself inside the kernel:
#'
#'   0.25 ha  16 % of a large forest's effect
#'   1 ha     45 %
#'   4 ha     84 %
#'   >= 16 ha 100 %, saturating once the patch outgrows max_extent_m
#'
#' It also accumulates over *direction*, which distance-to-nearest cannot: a
#' clearing ringed by trees is cooler than a spot the same distance from a single
#' straight forest edge (-0.11 K against -0.03 K at 155 m).
#'
#' `min_patch_ha` is kept as a hard floor even though the sum would now handle
#' small patches gracefully, because the Munich study found low-complexity
#' patches below a few hectares produce no measurable cool island at all - and
#' occasionally the opposite.
heatAdvectiveTerm <- function(ground, canopy, dec = heatDecay(), res = NULL,
                              conv_res = HEAT_ADV_RES){
  if(is.null(dec)) return(NULL)
  if(is.null(res)) res <- terra::res(ground)[1]
  acc <- ground * 0

  for(i in seq_len(nrow(dec))){
    msk <- heat_source_mask(ground, canopy, dec[i, , drop = FALSE], res)
    if(is.null(msk)) next
    cv <- heat_adv_field(msk, dec[i, , drop = FALSE], res, conv_res)
    if(!is.null(cv)) acc <- acc + cv
  }
  acc
}

#' The cells of one class that qualify as an advective source, as a 0/1 raster.
#'
#' NULL when the class has no cell in the area, or none in a patch large enough
#' to clear `min_patch_ha`. Split out of heatAdvectiveTerm() so a cached run can
#' compare this against the mask it convolved last time: differencing *here*,
#' after the patch test rather than before it, is what makes an incremental
#' rebuild safe. `min_patch_ha` is a property of a whole connected patch, so a
#' single erased cell can drop a 300 m tree avenue below the floor and change
#' the field 256 m away; a diff taken on the raw class raster would miss that,
#' while a diff taken here shows every cell that stopped qualifying.
heat_source_mask <- function(ground, canopy, row, res = NULL){
  half <- row$half_dist_m[1]; mx <- row$max_extent_m[1]
  amp  <- row$amp_edge_K[1];  mp <- row$min_patch_ha[1]
  if(is.na(half) || is.na(mx) || is.na(amp) || amp == 0) return(NULL)
  if(is.null(res)) res <- terra::res(ground)[1]

  cid <- row$class_id[1]
  #row-major, which is terra's own order and what ccl_big_patches() expects.
  #NA is "not this class" here, the same as 0 - the perimeter mask and a cell of
  #some other material are both simply not a source.
  v <- terra::values(terra::ifel((ground == cid) | (canopy == cid), 1L, 0L),
                     mat = FALSE)
  v[is.na(v)] <- 0L
  if(!any(v == 1L)) return(NULL)

  #The patch-size test, at the FULL grid: coarsen first and a narrow avenue of
  #trees disappears, and every patch area is misjudged.
  #
  #ccl_big_patches() replaces patches() + freq() + subst() with one union-find
  #pass and answers the only question asked here - "is this cell in a patch big
  #enough to count". terra::patches() was 60 % of a cold heatRaster() and is
  #superlinear with area: 7.6 s of a 12.6 s run over 3.6 x 2.6 km, and 53 s of
  #73 s over 6.3 x 4.5 km. This is 21x and 49x faster respectively, and returns
  #a bit-identical mask - which check group 10 asserts against the old pipeline.
  mc  <- (if(is.na(mp)) 0 else mp) * 10000 / res^2
  big <- ccl_big_patches(as.integer(v), terra::nrow(ground),
                         terra::ncol(ground), mc)
  if(!any(big == 1L)) return(NULL)
  terra::setValues(terra::rast(ground), big)
}

#' Convolve one class's source mask into its share of the advective term.
#'
#' `win` computes only that extent, for a caller that knows the rest of the
#' field cannot have moved. The mask is still passed whole, because a cell
#' inside `win` draws on sources up to `max_extent_m` outside it - bounding the
#' *output* is safe, bounding the input is not.
heat_adv_field <- function(msk, row, res = NULL, conv_res = HEAT_ADV_RES,
                           win = NULL){
  if(is.null(msk)) return(NULL)
  if(is.null(res)) res <- terra::res(msk)[1]
  half <- row$half_dist_m[1]; mx <- row$max_extent_m[1]; amp <- row$amp_edge_K[1]
  fact <- max(1L, as.integer(round(conv_res / res)))

  if(fact > 1){
    #mean, not modal: fractional cover preserves the total source area, so a
    #thin line of trees still weighs what it is worth on the coarse grid
    mk <- terra::aggregate(msk, fact, fun = "mean", na.rm = TRUE)
    k  <- heat_decay_kernel(half, mx, conv_res)
    if(!is.null(win)) mk <- terra::crop(mk, heat_adv_pad(win, mx, mk))
    cv <- heat_conv(mk, k)
    cv <- terra::resample(cv, if(is.null(win)) msk else terra::crop(msk, win),
                          method = "bilinear")
  }else{
    k  <- heat_decay_kernel(half, mx, res)
    m2 <- if(is.null(win)) msk else terra::crop(msk, heat_adv_pad(win, mx, msk))
    cv <- heat_conv(m2, k)
    if(!is.null(win)) cv <- terra::crop(cv, win)
  }
  amp * cv / heat_kernel_halfplane(k)
}

#' `win` grown by one full reach and snapped to `r`'s grid.
#'
#' The kernel reaches `max_extent_m`, and terra's focal fills past the edge of
#' what it is given with `fillvalue = 0`. Handing it exactly `win` would
#' therefore treat every source just outside as absent and draw a cold ring
#' around the window - so it gets a margin of real data to read, and the result
#' is cropped back afterwards.
heat_adv_pad <- function(win, mx, r){
  p <- terra::ext(terra::xmin(win) - mx, terra::xmax(win) + mx,
                  terra::ymin(win) - mx, terra::ymax(win) + mx)
  terra::intersect(terra::align(p, r, snap = "out"), terra::ext(r))
}

#' The two geometry terms, converted from Tmrt into PET.
#'
#' `svf_coefficient` is quoted in K of Tmrt per unit sky view factor *in full
#' sun*, hence the restriction to sunlit cells; `wall_bonus_K` likewise. Both are
#' multiplied by `tmrt_to_pet_slope` to land in the same unit as everything else.
#' The SVF reference is 1 - an open field - so the term is zero or negative.
heatGeometryTerm <- function(shade, svf, wall, geom = heatGeometry(),
                             apply_svf = HEAT_APPLY_SVF,
                             apply_wall = HEAT_APPLY_WALL){
  if(is.null(geom)) return(NULL)
  slope <- unname(geom[["tmrt_to_pet_slope"]])
  out <- shade * 0

  if(apply_svf && !is.null(svf)){
    k <- unname(geom[["svf_coefficient"]])
    #only the diffuse share applies to a cell that still has its direct beam.
    #Without this factor the term charges a sunlit cell for shade it has not
    #received - the shaded rows of heat_materials.csv already carry the beam
    #blocking, because shade there selects a row rather than adding a constant.
    #Left out, the SVF term reached -8.1 K and became the largest single term in
    #a model where it is also the least trusted. See heat_geometry.csv.
    fr <- unname(geom[["svf_sunlit_fraction"]])
    if(is.na(fr)) fr <- 1
    if(!is.na(k) && !is.na(slope)){
      out <- out + terra::ifel(shade == 0, slope * k * fr * (svf - 1), 0)
    }
  }
  if(apply_wall && !is.null(wall)){
    b <- unname(geom[["wall_bonus_K"]])
    if(!is.na(b) && !is.na(slope)) out <- out + terra::ifel(wall == 1, slope * b, 0)
  }
  out
}

# ------------------------------------------------------- reuse between runs --

#' HEAT_OBSTRUCTION_IDS - the class ids that change the obstruction height field -
#' is defined in paintbrush_helpers.R, beside the palette column it is derived
#' from, and NOT here where it is read.
#'
#' Not a stylistic choice. A package's R files are sourced in alphabetical order,
#' so heat_helpers.R runs before paintbrush_helpers.R: a top-level
#' `HEAT_OBSTRUCTION_IDS <- as.integer(PAINT_HEIGHT_IDS)` in this file cannot see
#' the palette yet and the whole package fails to load. Everything else here
#' reaches the palette from INSIDE a function body, where the lookup happens at
#' call time and the order no longer matters.

#' The time-of-day choices for the Hitzeminderung control, labelled.
#'
#' Values are HEAT_BINS, which is what heatRaster() takes; labels are the German
#' keys translated into whatever language the Translator is currently set to.
#' Lives here rather than in the UI so the UI and the language observer that
#' refreshes it cannot drift apart - they are the same list or they are a bug.
#'
#' Through vftTrText(), because a select's labels must be plain character and
#' i18n$t() returns a tag once usei18n() has run. See vftTrText() for what that
#' costs and why the server has to update these on a language change.
heatBinChoices <- function(i18n = NULL){
  keys <- c(morning = "Morgen", midday = "Mittag", afternoon = "Nachmittag")
  stats::setNames(HEAT_BINS,
                  vapply(HEAT_BINS, function(b) vftTrText(i18n, keys[[b]]),
                         character(1)))
}

#' HEAT MAPS KEPT PER SCENARIO.
#'
#' A heat map costs seconds in a daemon, so every one computed on the
#' newVersions page is kept for the scenario it describes, one slot per time of
#' day, and the scenario's card shows an icon for each one that still applies.
#'
#' `store[[card]] = list(rev = <int>, maps = list(<bin> = entry))`, keyed by the
#' card's `inputId_select`: it survives a rename, where a position does not
#' survive a deletion. An entry is `list(heat, proj, rev, base, aoi)`:
#'
#'   - `rev`  the scenario's paint revision when the job was dispatched. Every
#'            stroke or import bumps the scenario's revision, so a map is valid
#'            only while the two agree. It is NOT removed when it goes stale.
#'   - `base` TRUE when the scenario was unpainted at dispatch. Reset returns a
#'            scenario to exactly that state, so heatStoreReset() revives these
#'            entries and only these - a map computed on painted cells describes
#'            a design Reset does not bring back.
#'   - `aoi`  paintBaselineKey() of the study area, so a changed perimeter never
#'            validates a map of the old one.
#'
#' A plain environment, outside `r` and outside the scenario list: it holds work
#' already done and never takes part in reactive invalidation, and the scenario
#' list is saved, mirrored and shipped to daemons, none of which a SpatRaster
#' survives. The revision is what decides staleness, not the presence of a map.
heatStoreNew <- function() new.env(parent = emptyenv())

heatStoreSlot <- function(s, key){
  x <- if(is.null(key)) NULL else s[[key]]
  if(is.null(x)) list(rev = 0L, maps = list()) else x
}

#' Keep `entry` for `key`'s `bin`, replacing whatever was there - stale or not.
heatStorePut <- function(s, key, bin, entry){
  if(is.null(key)) return(invisible(NULL))
  x <- heatStoreSlot(s, key)
  x$maps[[bin]] <- entry
  s[[key]] <- x
  invisible(NULL)
}

#' The scenario's current paint revision, which a job records at dispatch.
heatStoreRev <- function(s, key) heatStoreSlot(s, key)$rev

#' The scenario was painted on: every map it holds is now stale.
#'
#' Bumped whether or not anything is stored yet, so that a job already in
#' flight comes back stale rather than valid.
heatStoreBump <- function(s, key){
  if(is.null(key)) return(invisible(NULL))
  x <- heatStoreSlot(s, key)
  x$rev <- x$rev + 1L
  s[[key]] <- x
  invisible(NULL)
}

#' The scenario went back to the unpainted land cover: a new revision, in which
#' the maps that were computed unpainted are valid again.
heatStoreReset <- function(s, key){
  if(is.null(key)) return(invisible(NULL))
  heatStoreBump(s, key)
  x <- s[[key]]
  for(b in names(x$maps)){
    if(isTRUE(x$maps[[b]]$base)) x$maps[[b]]$rev <- x$rev
  }
  s[[key]] <- x
  invisible(NULL)
}

#' The map for `key` at `bin`, or NULL when there is none or it is stale.
heatStoreGet <- function(s, key, bin, aoi){
  if(is.null(key) || is.null(bin)) return(NULL)
  x <- heatStoreSlot(s, key)
  e <- x$maps[[bin]]
  if(is.null(e) || !identical(e$rev, x$rev) || !identical(e$aoi, aoi)) return(NULL)
  e
}

#' The time-of-day bins `key` holds a valid map for, in HEAT_BINS order.
heatStoreValidBins <- function(s, key, aoi){
  HEAT_BINS[vapply(HEAT_BINS, function(b) !is.null(heatStoreGet(s, key, b, aoi)),
                   logical(1))]
}

#' Forget `keys` altogether (a deleted card), or everything when `keys` is NULL.
heatStoreDrop <- function(s, keys = NULL){
  if(is.null(keys)) keys <- ls(s, all.names = TRUE)
  keys <- intersect(keys, ls(s, all.names = TRUE))
  if(length(keys)) rm(list = keys, envir = s)
  invisible(NULL)
}

#' Colours of the three time-of-day icons: the sun warms through the day.
HEAT_BIN_ICON_COLORS <- c(morning = "#FFF08A", midday = "#FFD000", afternoon = "#FF9500")

#' A sun over a horizon, placed where it stands at `bin`: low in the east for
#' morning, high for midday, low in the west for afternoon. Drawing rather than a
#' stock icon because morning and afternoon differ only in azimuth - which is
#' also all that tells the two bins apart in the model.
heatBinIconSVG <- function(bin){
  pos <- switch(bin, morning = c(7, 13), midday = c(12, 7), afternoon = c(17, 13))
  sprintf(paste0('<svg viewBox="0 0 24 24" width="20" height="20" aria-hidden="true">',
                 '<circle cx="%g" cy="%g" r="5.2" fill="%s" stroke="#8a6d00" stroke-width="1.2"/>',
                 '<line x1="1.5" y1="19" x2="22.5" y2="19" stroke="#555" stroke-width="2.2" stroke-linecap="round"/>',
                 '</svg>'),
          pos[1], pos[2], HEAT_BIN_ICON_COLORS[[bin]])
}

#' The strip of heat icons at the foot of a scenario card.
#'
#' One round button per bin in `bins`; `shown` is the bin on the map, if it is
#' this card's. All the clicks go to ONE input, `ns("heatIconClick")`, carrying
#' the card and the bin - a per-card input would bring back the trap where
#' removeUI() leaves a card's input value behind for the next visit's observer.
#' The strip is always emitted, empty or not, so the browser has something to
#' replace when a map arrives or goes stale.
heatIconsTag <- function(card, bins, shown = NULL, i18n = NULL, ns = identity){
  labels <- stats::setNames(names(heatBinChoices(i18n)), HEAT_BINS)
  input  <- ns("heatIconClick")
  shiny::tags$div(
    class = "vftHeatIcons", `data-card` = card,
    lapply(bins, function(b){
      shiny::tags$button(
        type  = "button",
        class = paste(c("vftHeatIcon", if(identical(b, shown)) "vftHeatShown"), collapse = " "),
        title = labels[[b]],
        `data-bin` = b,
        onclick = sprintf(paste0("event.stopPropagation(); Shiny.setInputValue('%s', ",
                                 "{card: '%s', bin: '%s', n: Date.now()}, {priority: 'event'});"),
                          input, card, b),
        shiny::HTML(heatBinIconSVG(b)))
    }))
}

#' A cache for repeated heatRaster() calls over one area.
#'
#' Hand the same environment back on every call and each term is recomputed only
#' when something it actually depends on has changed. Three facts make this
#' worth doing, all of them measured over a 1.8 x 1.3 km window at Sion:
#'
#'   - the advective term is 1.47 s of a 2.38 s read-out and is built class by
#'     class; repainting one material can change at most two of the seven layers
#'   - the advective term and the SVF do not depend on the time of day at all,
#'     so switching bins recomputes 1.93 s of work that cannot have changed
#'   - reading and cropping the national rasters is another 0.33 s, and depends
#'     on neither the edits nor the bin
#'
#' This is exact, not an approximation: a layer is either reused untouched or
#' rebuilt over the whole area. In particular it does *not* try to recompute a
#' window around the edit - that would have to reckon with `min_patch_ha` being
#' a property of a whole connected patch, where erasing one cell in a 300 m tree
#' avenue drops the entire line below the floor and changes cells 256 m away.
#'
#' One cache belongs to one area and one set of paintLandcoverSeed() options. A
#' different grid is detected and rebuilds everything; different `...` options on
#' the same grid are not, so use a fresh cache if those ever vary.
heatCacheNew <- function() new.env(parent = emptyenv())

#' How many sessions' heat caches one process keeps.
#'
#' A cache holds every computed layer as a value vector - about 80 MB over a
#' 3.6 x 2.6 km area, more over a larger one - so these cannot accumulate for
#' the life of a daemon. The cap is small because the thing being protected is a
#' single user's repaint loop, not a history.
HEAT_CACHE_SESSIONS <- 4L

#' The per-session heat caches of this process - see heatCacheFor() for why this
#' is a namespace object and not a binding in `.GlobalEnv`.
.vft_heatCaches <- new.env(parent = emptyenv())

#' One session's heat cache, inside whichever process is doing the work.
#'
#' heatRaster()'s cache is an environment, and an environment cannot cross the
#' mirai boundary - nor would we want it to. It holds every layer as a value
#' vector, and serialising 80 MB on the main thread at both ends of every call
#' is precisely the cost that moving this job off the thread exists to remove.
#' So the cache stays in the process that builds it, keyed by session, exactly
#' as the national raster caches in R/providers.R do.
#'
#' The consequence is worth stating plainly rather than discovering later: with
#' more than one daemon, a session's next heat job may land on a daemon that has
#' never seen this area, and that call rebuilds everything. That is slower than
#' a hit, but it is not wrong - heat_cache_state() sees an unknown grid and
#' rebuilds - and it is still off the thread every other user is waiting on. A
#' miss costs the user who asked for it; the synchronous version charged it to
#' everybody.
#'
#' THE REGISTRY LIVES IN THIS NAMESPACE, NOT IN `.GlobalEnv` - which is where it
#' was until 2026-09-24, and where it never survived a single job. mirai daemons
#' run with `cleanup = TRUE` by default (mirai 2.7.1), which removes every
#' binding a task adds to the global environment before the next task starts. So
#' the registry was created, filled and thrown away inside each heat job: every
#' job in production was a cold build, including a change of time of day, and
#' nothing failed, because a cold cache is a valid cache. Checked on a real
#' daemon: a task-created global binding is gone by the next task, while an
#' environment held elsewhere keeps what tasks put in it. A namespace object is
#' elsewhere; data-raw/verify_heat_async.R now times a warm repeat to prove it.
#'
#' What .GlobalEnv bought - surviving a load_all() in a dev session, see .vftP()
#' in R/perf_helpers.R - does not matter here: a lost heat cache is a cold build,
#' not a wrong answer. (Sourced by a verification script, this binding lands in
#' the global environment like everything else, which is why those scripts
#' still find it there.)
#'
#' `dir` SHARES A SESSION'S CACHE BETWEEN DAEMONS through disk. With two daemons
#' the miss above was not an edge case but a coin toss: a change of time of day,
#' which should cost 0.85 s off a warm cache, cost a 4.2 s cold build whenever it
#' landed on the other daemon. Every job now saves the session's cache to
#' `dir` (heat_cache_save(), called from heatRasterPacked()), and a process that
#' has no entry for the session - or holds one older than the file - restores
#' from it. The file is the latest state whoever wrote it; an entry in memory is
#' only this daemon's last word.
#'
#' Restoring is exact for the same reason the cache is: every entry is checked
#' against the stored class rasters it was built from (heat_cache_state()), so
#' a file from another area or an older design causes rebuilds, not wrong
#' answers. A file that will not read is a cold cache, never an error.
heatCacheFor <- function(key, dir = NULL){
  if(is.null(key) || !nzchar(key)) return(heatCacheNew())
  reg <- .vft_heatCaches

  if(!is.null(dir)){
    f  <- heatCacheFile(dir, key)
    mt <- suppressWarnings(as.numeric(file.info(f)$mtime))
    held <- reg[[key]]
    if(is.finite(mt) && (is.null(held) || !isTRUE(held$.mtime >= mt))){
      restored <- heat_cache_restore(f)
      if(!is.null(restored)) reg[[key]] <- restored
    }
  }
  if(is.null(reg[[key]])) reg[[key]] <- heatCacheNew()
  #touch order, so the eviction below drops the session that has been idle
  #longest rather than an arbitrary one
  reg$.order <- c(setdiff(reg$.order, key), key)
  keep <- utils::tail(reg$.order, HEAT_CACHE_SESSIONS)
  for(k in setdiff(reg$.order, keep)) rm(list = k, envir = reg)
  reg$.order <- keep
  reg[[key]]
}

#' Where a session's heat cache is kept on disk. The key is a session token, but
#' it becomes a file name, so anything that is not plainly safe is replaced.
heatCacheFile <- function(dir, key){
  file.path(dir, paste0("heat_", gsub("[^A-Za-z0-9_-]", "_", key), ".rds"))
}

#' Write a heat cache for another process to pick up.
#'
#' The cache is value vectors plus two template SpatRasters, and a SpatRaster is
#' an external pointer. The templates carry no values - they are geometry - so
#' they are saved as geometry (heat_tpl_pack()) rather than wrap()ped, which
#' would try to read values they do not have. Everything else goes as it is.
#' Uncompressed, because the file lives for one session on local disk and the
#' whole point is speed: 80 MB over 3.6 x 2.6 km (the layers plus the class
#' rasters and source masks they are checked against), written in 0.12 s and
#' read back in 0.16 s - against a 4.2 s cold build.
#'
#' Written under a temporary name and renamed into place, so a daemon reading
#' while another writes sees the old file or the new one, never half of one.
heat_cache_save <- function(cache, file){
  if(is.null(cache) || is.null(file)) return(invisible(FALSE))
  tryCatch({
    nms <- setdiff(ls(cache, all.names = TRUE), ".mtime")
    lst <- mget(nms, envir = cache)
    lst <- lapply(lst, function(x) if(inherits(x, "SpatRaster")) heat_tpl_pack(x) else x)
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    tmp <- paste0(file, ".", Sys.getpid(), ".tmp")
    saveRDS(lst, tmp, compress = FALSE)
    if(!file.rename(tmp, file)){
      unlink(file); if(!file.rename(tmp, file)){ unlink(tmp); return(invisible(FALSE)) }
    }
    cache$.mtime <- as.numeric(file.info(file)$mtime)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}

#' Read a heat cache heat_cache_save() wrote, or NULL if it cannot be read.
heat_cache_restore <- function(file){
  tryCatch({
    lst <- readRDS(file)
    if(!is.list(lst)) return(NULL)
    env <- heatCacheNew()
    for(n in names(lst)){
      x <- lst[[n]]
      assign(n, if(inherits(x, "vft_heat_tpl")) heat_tpl_unpack(x) else x, envir = env)
    }
    env$.mtime <- as.numeric(file.info(file)$mtime)
    env
  }, error = function(e) NULL)
}

#' A cache template reduced to its grid, and back. A layer with values would
#' lose them here, so one is refused rather than silently emptied.
heat_tpl_pack <- function(x){
  if(terra::hasValues(x)) stop("heat_tpl_pack: a template must not carry values")
  structure(list(ext = as.vector(terra::ext(x)), nrow = terra::nrow(x),
                 ncol = terra::ncol(x), crs = terra::crs(x)),
            class = "vft_heat_tpl")
}
heat_tpl_unpack <- function(p){
  terra::rast(terra::ext(p$ext), nrows = p$nrow, ncols = p$ncol, crs = p$crs)
}

#' heatRaster() across a process boundary: plain arguments in, a packed raster out.
#'
#' The entry point the worker runs. Everything terra touches is an external
#' pointer and cannot be serialised, so the painted rasters arrive wrapped and
#' the surface goes back wrapped - the same contract R/providers.R states at the
#' top of the file. `key` is the session token that selects the cache above; a
#' NULL key means "no cache", which is what a verification script wants.
#' `cacheDir` shares the cache between daemons through disk - see heatCacheFor().
heatRasterPacked <- function(aoi, groundEdits = NULL, canopyEdits = NULL,
                             bin = HEAT_BIN_DEFAULT, res = HEAT_RES,
                             key = NULL, progress = NULL, cacheDir = NULL, ...){
  unpack <- function(x) if(is.null(x)) NULL else terra::unwrap(x)
  cache <- heatCacheFor(key, cacheDir)
  out <- heatRaster(aoi, unpack(groundEdits), unpack(canopyEdits),
                    bin = bin, res = res, cache = cache,
                    progress = progress, ...)
  if(!is.null(key) && nzchar(key) && !is.null(cacheDir))
    heat_cache_save(cache, heatCacheFile(cacheDir, key))
  if(is.null(out)) NULL else terra::wrap(out)
}

#' The stage markers the progress bar is driven from, or a no-op.
#'
#' `progress` is a vftProgress() handle - the small $set/$inc/$close triple that
#' is safe to send to a worker, see R/async_helpers.R. It is optional and NULL
#' has to cost nothing: the verification scripts in data-raw and every direct
#' call to heatRaster() run without one.
#'
#' Total by construction, and deliberately so. This is called from inside the
#' model, and a progress bar is a display feature: a handle whose queue has gone
#' away, or a session that closed while the daemon was still working, must not be
#' able to take a computation down with it.
heat_ticker <- function(progress){
  noop <- function(value) invisible(NULL)
  if(is.null(progress)) return(noop)
  #pulled out rather than called through `progress$`, so that this works for
  #both shapes of bar the app has: the vftProgress() handle, which is a plain
  #list of closures, and a shiny::Progress or ipc::AsyncProgress, whose $set is
  #a method. A `$` on anything else raises here, once, instead of on every term.
  setter <- tryCatch(progress$set, error = function(e) NULL)
  if(!is.function(setter)) return(noop)
  function(value) tryCatch(setter(value = value), error = function(e) NULL)
}

#' What a repaint changed, and whether the geometry has to be redone.
#'
#' Returns `touched = NULL` to mean "assume everything", which is what a first
#' call or a change of grid gets. Otherwise `touched` is the set of class ids
#' involved on either side of every differing cell - both the id painted over
#' and the id painted - because a class layer changes when it loses cells just
#' as much as when it gains them.
heat_cache_state <- function(cache, ground, canopy){
  fresh <- list(touched = NULL, geom_dirty = TRUE)
  if(is.null(cache)) return(fresh)

  gv <- terra::values(ground, mat = FALSE)
  cv <- terra::values(canopy, mat = FALSE)
  sig <- c(dim(ground)[1:2], as.vector(terra::ext(ground)))

  if(is.null(cache$sig) || !isTRUE(all.equal(cache$sig, sig)) ||
     length(cache$gv) != length(gv)){
    cache$v   <- list()
    cache$sig <- sig
    cache$tpl <- terra::rast(ground)
    cache$gv  <- gv
    cache$cv  <- cv
    return(fresh)
  }

  #NA is a value here - it is the mask outside the perimeter - so a plain !=
  #would report every one of those cells as unchanged and also as changed
  diff <- function(a, b) which(xor(is.na(a), is.na(b)) |
                               (!is.na(a) & !is.na(b) & a != b))
  ig <- diff(cache$gv, gv)
  ic <- diff(cache$cv, cv)
  touched <- unique(c(cache$gv[ig], gv[ig], cache$cv[ic], cv[ic]))
  touched <- sort(touched[!is.na(touched)])

  cache$gv <- gv
  cache$cv <- cv
  geom_dirty <- any(touched %in% HEAT_OBSTRUCTION_IDS)

  #Every geometry layer for every bin goes, not just the one about to be
  #rebuilt. `touched` is measured against the previous call, so a stale entry
  #for a bin nobody is looking at right now is never seen as dirty again: plant
  #a tree while the afternoon is displayed and the midday shade in the cache
  #still has no tree in it, but the next switch back to midday reports no class
  #change and happily reuses it. That was a 5 K error on a real sequence, and it
  #is the reason these are purged by pattern rather than by key.
  #
  #The advective layers need no such sweep - they carry no second dimension. A
  #class layer is a function of that class's mask and nothing else, and it is
  #rebuilt in the same call that sees the mask change.
  if(geom_dirty && length(cache$v)){
    cache$v <- cache$v[!grepl("^(shade_|wall_|svf$)", names(cache$v))]
  }
  list(touched = touched, geom_dirty = geom_dirty)
}

#' Reuse a cached layer, or build and store it.
#'
#' Layers are held as bare value vectors against one template rather than as
#' SpatRasters. A SpatRaster terra decided to spill to a scratch file is a
#' dangling reference once that file is swept up, and the failure would surface
#' as a wrong map rather than an error.
heat_cached <- function(cache, key, dirty, build){
  if(is.null(cache) || is.null(cache$tpl)) return(build())
  if(!dirty && !is.null(cache$v[[key]])){
    return(terra::setValues(terra::rast(cache$tpl), cache$v[[key]]))
  }
  out <- build()
  cache$v[[key]] <- if(is.null(out)) NULL else terra::values(out, mat = FALSE)
  out
}

#' An edits raster reduced to what is needed to tell two of them apart.
heat_edit_snap <- function(e){
  if(is.null(e)) return(NULL)
  list(ext = as.vector(terra::ext(e)), dim = dim(e)[1:2],
       v = terra::values(e, mat = FALSE))
}

#' The extent a repaint changed, NULL for "nothing" and NA for "cannot tell".
#'
#' NA is returned whenever the two snapshots are not directly comparable - one
#' side absent, a different extent, a different grid. That is the conservative
#' answer and it costs a full rebuild, which is what the first call and a plan
#' import both get.
heat_edit_delta <- function(old, new){
  if(is.null(old) && is.null(new)) return(NULL)
  if(is.null(old) || is.null(new)) return(NA)

  xr <- function(s) (s$ext[2] - s$ext[1]) / s$dim[2]
  yr <- function(s) (s$ext[4] - s$ext[3]) / s$dim[1]
  if(!isTRUE(all.equal(xr(old), xr(new))) ||
     !isTRUE(all.equal(yr(old), yr(new)))) return(NA)

  #The two extents are usually NOT the same. A version's painted raster is the
  #bounding box of everything painted so far, so it grows the first time a
  #stroke lands outside it - which is most strokes early on. Comparing only
  #same-extent pairs would send all of those down the full-rebuild path, so the
  #two are laid on their common grid first. Both are on the global paint grid
  #(see PAINT_RES), so extending aligns them exactly.
  if(!isTRUE(all.equal(old$ext, new$ext)) || !identical(old$dim, new$dim)){
    a <- heat_edit_rast(old); b <- heat_edit_rast(new)
    u <- terra::union(terra::ext(a), terra::ext(b))
    a <- terra::extend(a, u); b <- terra::extend(b, u)
    if(!all(dim(a)[1:2] == dim(b)[1:2])) return(NA)
    old <- list(ext = as.vector(u), dim = dim(a)[1:2],
                v = terra::values(a, mat = FALSE))
    new <- list(ext = as.vector(u), dim = dim(b)[1:2],
                v = terra::values(b, mat = FALSE))
  }
  if(length(old$v) != length(new$v)) return(NA)

  d <- which(xor(is.na(old$v), is.na(new$v)) |
             (!is.na(old$v) & !is.na(new$v) & old$v != new$v))
  if(!length(d)) return(NULL)

  nc  <- new$dim[2]
  row <- ((d - 1L) %/% nc) + 1L
  col <- ((d - 1L) %%  nc) + 1L
  #rows run north to south, so row 1 is the top of the extent
  terra::ext(new$ext[1] + (min(col) - 1) * xr(new), new$ext[1] + max(col) * xr(new),
             new$ext[4] - max(row) * yr(new),       new$ext[4] - (min(row) - 1) * yr(new))
}

#' Rebuild an edits raster from the snapshot heat_edit_snap() kept of it.
heat_edit_rast <- function(s){
  r <- terra::rast(nrows = s$dim[1], ncols = s$dim[2],
                   xmin = s$ext[1], xmax = s$ext[2],
                   ymin = s$ext[3], ymax = s$ext[4], crs = "EPSG:2056")
  terra::setValues(r, s$v)
}

#' Coarsen a class-id raster to the heat grid, voting on the MATERIAL first.
#'
#' A plain modal vote over the raw ids loses a patch whose height the user split.
#' A 5 m cell holding 8 cells of tree@10 m, 7 of tree@25 m and 10 of grass is
#' 60 per cent tree, but the raw tally is {11:8, 13:7, 1:10} and grass wins it -
#' so a painted avenue comes out with no crown, no shade and no sky blocking, and
#' nothing anywhere says so.
#'
#' So it is voted twice. The material decides what the cell is (all five tree
#' steps counting as one tree), then the height follows only if the winning
#' variant belongs to the winning material. Where they disagree - a cell that is
#' mostly tree but whose single commonest id is grass - the material wins and the
#' height falls back to that material's default step, which is the surveyed
#' value and the same id the ramp's base carries.
#'
#' Both of heat_landcover()'s aggregate sites must use this, the full rebuild and
#' the windowed one. If only one does, a repaint near a window edge coarsens
#' differently from a full read of the same paint and the incremental cache stops
#' being exact - which is what verify_heat_model.R group 8 walks.
#'
#' Stays in terra on purpose. A one-pass C++ vote was tried (2026-09-24) and
#' lost, 0.56-0.69 s against 0.36-0.41 s over 3.6 x 2.6 km: copying 12.7 M cells
#' out of terra into R alone costs more than terra's whole two-stage vote.
heat_modal_class <- function(r, f){
  if(f <= 1) return(r)
  var <- terra::aggregate(r, fact = f, fun = "modal", na.rm = TRUE)
  if(!length(PAINT_VARIANT_IDS)) return(var)
  base <- terra::aggregate(paintBaseRaster(r), fact = f, fun = "modal", na.rm = TRUE)
  terra::ifel(paintBaseRaster(var) == base, var, base)
}

#' The class rasters a heat run works on: the national baseline at `res`, with
#' the version's paint laid over it.
#'
#' With a cache this rebuilds only the cells a repaint touched. Aggregation is
#' blockwise - a 5 m cell is the modal class of its own 25 one-metre cells and
#' of nothing else - so patching by the changed extent is exact rather than
#' approximate, provided the window is snapped out to the coarse grid first.
#'
#' Worth doing because this is the only part of heatRaster() still working at
#' 1 m: reading the two national rasters and laying the paint over them is
#' 0.48 s of a 0.74 s warm call over 1.8 x 1.3 km, and 1.4 s over 3.6 x 2.6 km.
#' Holding the 1 m seed instead would cost 150 MB on that larger area - 25x
#' every other cached layer put together - which is why this re-reads a small
#' window rather than keeping the big one.
heat_landcover <- function(aoi, ge, ce, res, cache, ...){
  full <- function(){
    seed <- paintLandcoverSeed(aoi, ...)
    if(is.null(seed)) return(NULL)
    g <- paintOverlayEdits(seed$ground, ge)
    c_ <- paintOverlayEdits(seed$canopy, ce)
    f <- res / terra::res(g)[1]
    g  <- heat_modal_class(g,  f)
    c_ <- heat_modal_class(c_, f)
    list(ground = g, canopy = c_)
  }
  if(is.null(cache)) return(full())

  akey <- paste(c(res, format(as.vector(sf::st_bbox(aoi)), digits = 12)),
                collapse = "|")
  sg <- heat_edit_snap(ge); sc <- heat_edit_snap(ce)

  store <- function(lc){
    if(is.null(lc)) return(lc)
    cache$akey  <- akey
    cache$lctpl <- terra::rast(lc$ground)
    cache$lcg   <- terra::values(lc$ground, mat = FALSE)
    cache$lcc   <- terra::values(lc$canopy, mat = FALSE)
    cache$eg    <- sg; cache$ec <- sc
    lc
  }
  if(!identical(cache$akey, akey) || is.null(cache$lctpl)) return(store(full()))

  #three answers, told apart by type rather than by value: NULL is "nothing
  #changed", a SpatExtent is "this much changed", anything else is the NA that
  #means "cannot tell". is.na() on a SpatExtent is an S4 warning, not a test.
  dg <- heat_edit_delta(cache$eg, sg)
  dc <- heat_edit_delta(cache$ec, sc)
  known <- function(d) is.null(d) || inherits(d, "SpatExtent")
  if(!known(dg) || !known(dc)) return(store(full()))

  rebuild <- function(){
    list(ground = terra::setValues(terra::rast(cache$lctpl), cache$lcg),
         canopy = terra::setValues(terra::rast(cache$lctpl), cache$lcc))
  }
  if(is.null(dg) && is.null(dc)) return(rebuild())

  win <- if(is.null(dg)) dc else if(is.null(dc)) dg else terra::union(dg, dc)
  #snap out to the coarse grid: a block half inside the window would otherwise
  #be recomputed from part of its cells and come back with the wrong mode
  win <- terra::align(win, cache$lctpl, snap = "out")
  win <- terra::intersect(win, terra::ext(cache$lctpl))
  if(is.null(win)) return(rebuild())

  seed <- paintLandcoverSeed(aoi, ..., win = win)
  if(is.null(seed)) return(store(full()))
  g <- paintOverlayEdits(seed$ground, if(is.null(ge)) NULL else terra::crop(ge, win))
  c_ <- paintOverlayEdits(seed$canopy, if(is.null(ce)) NULL else terra::crop(ce, win))
  f <- res / terra::res(g)[1]
  g  <- heat_modal_class(g,  f)
  c_ <- heat_modal_class(c_, f)
  idx <- terra::cells(cache$lctpl, terra::ext(g))
  if(length(idx) != terra::ncell(g)) return(store(full()))   #misaligned: refuse
  cache$lcg[idx] <- terra::values(g,  mat = FALSE)
  cache$lcc[idx] <- terra::values(c_, mat = FALSE)
  cache$eg <- sg; cache$ec <- sc
  rebuild()
}

#' The advective term, one cached layer per class.
#'
#' Same sum as heatAdvectiveTerm() over the whole table - it is called here once
#' per row instead of once for all of them, so that a row whose class was not
#' repainted can be skipped entirely.
heat_advective_cached <- function(ground, canopy, dec, res, cache, touched){
  if(is.null(dec)) return(NULL)
  acc <- NULL
  for(i in seq_len(nrow(dec))){
    cid <- dec$class_id[i]
    if(is.na(dec$half_dist_m[i]) || is.na(dec$amp_edge_K[i]) ||
       dec$amp_edge_K[i] == 0) next
    row <- dec[i, , drop = FALSE]
    key <- paste0("adv", cid)

    #a class nothing was painted over or into cannot have moved
    if(!is.null(touched) && !(cid %in% touched) && !is.null(cache) &&
       !is.null(cache$v[[key]])){
      lay <- terra::setValues(terra::rast(cache$tpl), cache$v[[key]])
      acc <- if(is.null(acc)) lay else acc + lay
      next
    }

    msk <- heat_source_mask(ground, canopy, row, res)
    mv  <- if(is.null(msk)) NULL else terra::values(msk, mat = FALSE) > 0
    win <- heat_mask_delta(cache, key, mv, msk, row$max_extent_m[1])

    if(is.null(msk)){
      if(!is.null(cache)) cache$v[[key]] <- NULL
      next
    }
    if(identical(win, "none") && !is.null(cache$v[[key]])){
      lay <- terra::setValues(terra::rast(cache$tpl), cache$v[[key]])
      acc <- if(is.null(acc)) lay else acc + lay
      next
    }

    if(inherits(win, "SpatExtent") && !is.null(cache$v[[key]])){
      #only the part of the field that could have moved
      part <- heat_adv_field(msk, row, res, HEAT_ADV_RES, win = win)
      lay  <- terra::setValues(terra::rast(cache$tpl), cache$v[[key]])
      idx  <- terra::cells(lay, terra::ext(part))
      if(length(idx) == terra::ncell(part)){
        vals <- cache$v[[key]]
        vals[idx] <- terra::values(part, mat = FALSE)
        cache$v[[key]] <- vals
        lay <- terra::setValues(terra::rast(cache$tpl), vals)
        acc <- if(is.null(acc)) lay else acc + lay
        next
      }
    }

    lay <- heat_adv_field(msk, row, res, HEAT_ADV_RES)
    if(!is.null(cache)) cache$v[[key]] <- terra::values(lay, mat = FALSE)
    acc <- if(is.null(acc)) lay else acc + lay
  }
  acc
}

#' Where one class's eligible-source mask changed, and how far that reaches.
#'
#' "none" when the mask is untouched, a SpatExtent covering everything the
#' change can reach, or NULL when there is nothing to compare against. The
#' extent is the changed cells grown by `max_extent_m`, which is the whole of
#' the kernel's reach - beyond it a changed source contributes exactly zero,
#' because the decay curve is renormalised to land on zero there rather than
#' being truncated while still carrying amplitude.
heat_mask_delta <- function(cache, key, mv, msk, mx){
  if(is.null(cache) || is.null(mv)) return(NULL)
  slot <- paste0("m_", key)
  old  <- cache[[slot]]
  cache[[slot]] <- mv
  if(is.null(old) || length(old) != length(mv)) return(NULL)
  d <- which(old != mv)
  if(!length(d)) return("none")

  nc  <- terra::ncol(msk)
  row <- ((d - 1L) %/% nc) + 1L
  col <- ((d - 1L) %%  nc) + 1L
  e   <- terra::ext(msk)
  xr  <- terra::xres(msk); yr <- terra::yres(msk)
  ch  <- terra::ext(terra::xmin(e) + (min(col) - 1) * xr,
                    terra::xmin(e) + max(col) * xr,
                    terra::ymax(e) - max(row) * yr,
                    terra::ymax(e) - (min(row) - 1) * yr)
  heat_adv_pad(ch, mx, msk)
}

#' The heat raster for an area.
#'
#' `aoi` is the step-1 perimeter; `groundEdits`/`canopyEdits` are a version's
#' painted rasters (either may be NULL). `bin` is one of HEAT_BINS. Returns NULL
#' when there is no land cover to work from, on the same terms as
#' paintLandcoverSeed().
#'
#' `cache` is an optional heatCacheNew() environment; pass the same one back on
#' every call over the same area and unchanged terms are reused. Leaving it NULL
#' computes everything every time, which is what the verification scripts do.
#'
#' One seed call covers both levels. Going through paintCompositeRaster() per
#' level would crop the two national rasters twice over - four file reads where
#' two will do - and the crop is the expensive part of this function.
heatRaster <- function(aoi, groundEdits = NULL, canopyEdits = NULL,
                       bin = HEAT_BIN_DEFAULT, res = HEAT_RES,
                       cache = NULL, progress = NULL, ...){
  vftTime("heat:heatRaster", {
  bin  <- match.arg(bin, HEAT_BINS)
  #WHERE THE BAR HAS GOT TO IS WHERE THE MODEL HAS GOT TO.
  #
  #Each tick() fires when that term is actually in hand, so the fraction is a
  #position in this pipeline and not a guess at a clock. The marks are spaced by
  #the share of a COLD run the terms up to that point cost, measured 2026-09-21
  #over 3.6 x 2.6 km at Sion (2.91 s, 621 x 821 at 5 m):
  #
  #   land cover  1.21 s  42 %   read, crop, paint and coarsen the two rasters
  #   shade       0.22 s   8 %
  #   local       0.08 s   3 %
  #   sky view    0.20 s   7 %
  #   wall        0.11 s   4 %
  #   geometry    0.06 s   2 %
  #   advective   0.99 s  34 %   the seven convolutions
  #
  #Reading the land cover and the advective field are the two halves of it, one
  #at each end, and everything the 2026-09-21 rewrite made fast is the thin part
  #in the middle - patches() and the horizon scan were 60 % and 13 % before it.
  #That shape is why the marks are not evenly spaced.
  #
  #They are anchors, not a rate. A WARM run skips whole terms from the cache and
  #the bar jumps past them, which is the truth about that run rather than a
  #fault in it: a change of time of day, the commonest recompute, keeps the land
  #cover, the sky view and the advective field and rebuilds only the shade and
  #the wall term, so it lands on 42 % at once, spends its half second between
  #there and 66 %, and then runs out.
  #
  #Nothing in the model reads these numbers - they are the spacing of the marks
  #and nothing else. Group 11 of data-raw/verify_heat_model.R pins the sequence,
  #and re-measuring is one run of the profile in that group's comment.
  tick <- heat_ticker(progress)
  tick(0.02)
  #The baseline is read, painted and coarsened here. Coarsening happens BEFORE
  #the geometry, not after: the shadow march, the horizon scan and three
  #distance transforms all run on this grid, and at 1 m over a 6 km AOI that is
  #36 M cells of work for a field that is smooth at 5 m anyway. `modal` and not
  #`mean`, because these are class ids. With a cache, only the cells a repaint
  #touched are re-read - see heat_landcover().
  lc <- heat_landcover(aoi, groundEdits, canopyEdits, res, cache, ...)
  if(is.null(lc)) return(NULL)
  ground <- lc$ground
  canopy <- lc$canopy
  tick(0.42)

  geom  <- heatGeometry()
  #what this repaint touched, and therefore what has to be rebuilt. With no
  #cache both come back "everything", which is the behaviour without one.
  #
  #On the RAW rasters, deliberately: the height variants are what tell a 10 m
  #tree from a 25 m one, and this is the only place that can notice the
  #difference. Normalise first and a change of height reads as no change at all.
  st    <- heat_cache_state(cache, ground, canopy)
  gd    <- st$geom_dirty

  #TWO VIEWS OF THE SAME PAINT, AND EACH TERM TAKES THE ONE IT MEANS.
  #
  #`ground`/`canopy` keep the height variants: the shadow march, the horizon
  #scan and the wall test all go through heatObstructionHeight(), which IS the
  #class-id-to-metres lookup, so taking the height away from them is taking the
  #feature away.
  #
  #`mg`/`mc` have the variants substituted back to their base material, and
  #every THERMAL lookup reads those. heat_materials.csv and heat_decay.csv are
  #keyed on the nine original classes - a 3 m crown and a 25 m crown are made of
  #the same thing - and an unlisted id there does not raise anything. It goes
  #quiet: heatLocalTerm() resolves ground classes with `others = NA`, so a block
  #variant would drop its cells out of the finished map, and canopy classes with
  #`others = 0`, so a tree variant would read as open sky under a 25 m tree.
  mg <- paintBaseRaster(ground)
  mc <- paintBaseRaster(canopy)

  #`touched` is measured on the raw ids, but the advective cache is keyed by the
  #base class of heat_decay.csv. Paint grass over a 10 m tree and touched is
  #{1, 11}: adv1 rebuilds, adv7 does not, and the tree patch that just shrank is
  #still in the cached field. Carrying both spellings is what closes that.
  touched <- st$touched
  if(!is.null(touched)) touched <- sort(unique(c(touched, paintBaseId(touched))))

  #shade and the wall term are per bin; the SVF is not - a horizon angle does
  #not care where the sun is - so it survives a change of time of day
  shade <- heat_cached(cache, paste0("shade_", bin), gd,
                       function() heatShadeRaster(ground, canopy, bin, geom))
  if(is.null(shade)) return(NULL)
  tick(0.50)

  #the local term reads every cell's own class, so any repaint at all moves it.
  #It is also the cheapest term in the model, so it is never cached.
  local <- heatLocalTerm(mg, mc, shade, bin)
  if(is.null(local)) return(NULL)
  tick(0.53)

  svf  <- if(HEAT_APPLY_SVF)
    heat_cached(cache, "svf", gd,
                function() heatSvfRaster(ground, canopy, geom = geom)) else NULL
  tick(0.60)
  wall <- if(HEAT_APPLY_WALL)
    heat_cached(cache, paste0("wall_", bin), gd,
                function() heatWallRaster(ground, canopy, bin, geom, shade)) else NULL
  tick(0.64)
  geo  <- heatGeometryTerm(shade, svf, wall, geom)
  tick(0.66)

  adv <- heat_advective_cached(mg, mc, heatDecay(), res, cache, touched)
  tick(0.99)

  out <- local
  if(!is.null(geo)) out <- out + geo
  if(!is.null(adv)) out <- out + adv
  names(out) <- "heat"
  out
  })
}

#' Leaflet palette for a heat raster, centred on zero.
#'
#' Symmetric about 0 so the midpoint of the ramp is the reference surface -
#' unshaded grass at midday - rather than the middle of whatever range this
#' particular area happens to span. Otherwise the same colour means different
#' things on different sites and two designs cannot be compared by eye.
heatPalette <- function(heat, colors = HEAT_COLORS){
  v <- terra::values(heat)
  v <- v[is.finite(v)]
  lim <- if(length(v)) max(abs(range(v))) else 1
  if(lim == 0) lim <- 1
  leaflet::colorNumeric(colors, domain = c(-lim, lim), na.color = "transparent")
}
