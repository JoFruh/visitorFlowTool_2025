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
  cell_m2 <- res^2
  fact <- max(1L, as.integer(round(conv_res / res)))

  for(i in seq_len(nrow(dec))){
    half <- dec$half_dist_m[i]; mx <- dec$max_extent_m[i]
    amp  <- dec$amp_edge_K[i];  mp <- dec$min_patch_ha[i]
    if(is.na(half) || is.na(mx) || is.na(amp) || amp == 0) next

    cid <- dec$class_id[i]
    src <- terra::ifel((ground == cid) | (canopy == cid), 1, NA)
    if(all(is.na(terra::values(src)))) next

    #the patch-size test, at the FULL grid: coarsen first and a narrow avenue of
    #trees disappears, and every patch area is misjudged
    pch <- terra::patches(src, directions = 8, zeroAsNA = TRUE)
    fr  <- terra::freq(pch)
    big <- fr$value[fr$count * cell_m2 >= (if(is.na(mp)) 0 else mp) * 10000]
    if(!length(big)) next
    msk <- terra::ifel(pch %in% big, 1, 0)
    msk <- terra::ifel(is.na(msk), 0, msk)

    if(fact > 1){
      #mean, not modal: fractional cover preserves the total source area, so a
      #thin line of trees still weighs what it is worth on the coarse grid
      mk <- terra::aggregate(msk, fact, fun = "mean", na.rm = TRUE)
      k  <- heat_decay_kernel(half, mx, conv_res)
      cv <- terra::focal(mk, w = k, fun = "sum", na.rm = TRUE, fillvalue = 0)
      cv <- terra::resample(cv, msk, method = "bilinear")
    }else{
      k  <- heat_decay_kernel(half, mx, res)
      cv <- terra::focal(msk, w = k, fun = "sum", na.rm = TRUE, fillvalue = 0)
    }
    acc <- acc + amp * cv / heat_kernel_halfplane(k)
  }
  acc
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

#' The heat raster for an area.
#'
#' `aoi` is the step-1 perimeter; `groundEdits`/`canopyEdits` are a version's
#' painted rasters (either may be NULL). `bin` is one of HEAT_BINS. Returns NULL
#' when there is no land cover to work from, on the same terms as
#' paintLandcoverSeed().
#'
#' One seed call covers both levels. Going through paintCompositeRaster() per
#' level would crop the two national rasters twice over - four file reads where
#' two will do - and the crop is the expensive part of this function.
heatRaster <- function(aoi, groundEdits = NULL, canopyEdits = NULL,
                       bin = HEAT_BIN_DEFAULT, res = HEAT_RES, ...){
  vftTime("heat:heatRaster", {
  bin  <- match.arg(bin, HEAT_BINS)
  seed <- paintLandcoverSeed(aoi, ...)
  if(is.null(seed)) return(NULL)

  ground <- paintOverlayEdits(seed$ground, groundEdits)
  canopy <- paintOverlayEdits(seed$canopy, canopyEdits)

  #coarsen BEFORE the geometry, not after: the shadow march, the horizon scan
  #and three distance transforms all run on this grid, and at 1 m over a 6 km
  #AOI that is 36 M cells of work for a field that is smooth at 5 m anyway.
  #`modal` and not `mean`, because these are class ids.
  fact <- res / terra::res(ground)[1]
  if(fact > 1){
    ground <- terra::aggregate(ground, fact = fact, fun = "modal", na.rm = TRUE)
    canopy <- terra::aggregate(canopy, fact = fact, fun = "modal", na.rm = TRUE)
  }

  geom  <- heatGeometry()
  shade <- heatShadeRaster(ground, canopy, bin, geom)
  if(is.null(shade)) return(NULL)

  local <- heatLocalTerm(ground, canopy, shade, bin)
  if(is.null(local)) return(NULL)

  svf  <- if(HEAT_APPLY_SVF)  heatSvfRaster(ground, canopy, geom = geom) else NULL
  wall <- if(HEAT_APPLY_WALL) heatWallRaster(ground, canopy, bin, geom, shade) else NULL
  geo  <- heatGeometryTerm(shade, svf, wall, geom)

  adv <- heatAdvectiveTerm(ground, canopy, res = res)

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
