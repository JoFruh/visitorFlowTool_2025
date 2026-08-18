#' Heat signature of a landscape, from the ground and canopy land cover.
#'
#' One raster answering "how hot is it here", derived from what the surface is
#' made of and what stands above it. It is computed from the *composite* - the
#' surveyed land cover with the user's paint laid over it - so that editing a
#' design changes the answer, which is the whole reason the heat-mitigation
#' editor exists.
#'
#' The model, per cell:
#'
#'   base = HEAT_GROUND[ground class] + HEAT_CANOPY[canopy class]
#'   heat = W_near * mean(base within R_near) + W_far * mean(base within R_far)
#'
#' Heat is a neighbourhood property, not a per-cell one: a car park is hot, and
#' it makes the meadow beside it hotter too. Two circular windows carry that -
#' a tight one for the immediate surroundings and a wide one for the district -
#' and the cell's own value sits inside both.
#'
#' Every number here is a first pass and expected to be tuned, which is why they
#' are all named constants rather than literals in the pipeline below.


# -------------------------------------------------------------- constants ---

#' Heat contribution of each ground material. Names are PAINT_CATEGORIES ids.
#'
#' Absent ids become NA (see `others = NA` in heat_subst), which is how
#' artificial_block (8) and unclassified (0) are excluded: a building's thermal
#' behaviour is not modelled yet, and 0 means the sources say nothing at all.
#' Both drop out of the window means rather than contributing an invented value.
HEAT_GROUND <- c("1" = 0.3,   # gras
                 "2" = 0.2,   # busch
                 "3" = 0.7,   # kuenstlich
                 "4" = 0.5,   # natuerlich
                 "5" = 0.0)   # wasser

#' Shade added by whatever stands above the ground.
#'
#' 0 is "open sky" and must be 0.0, not NA - it is a *known* state meaning "no
#' shade", unlike ground 0 which means "unknown". As NA it would propagate
#' through the addition below and empty out every unshaded cell in the map.
#'
#' A tree shades harder than an artificial canopy: foliage blocks more sun than
#' a deck and transpires as well.
HEAT_CANOPY <- c("0" =  0.0,  # open sky - no shade
                 "6" = -0.5,  # kuenstlich
                 "7" = -0.8)  # baum

#' Moving window radii in metres, and the weight each window's mean carries.
HEAT_RADII   <- c(near = 10,  far = 100)
HEAT_WEIGHTS <- c(near = 0.1, far = 0.01)

#' Output resolution in metres.
#'
#' The land cover is 1 m; heat is a smoothed field and does not need that, so
#' coarsening first is what keeps the two focal passes affordable. It is also
#' the one knob that matters for the near window: at 10 m a 10 m radius is a
#' 3x3 neighbourhood, so drop this to 5 if that proves too blunt.
HEAT_RES <- 10

#' Fully opaque: the heat surface is the thing being read when it is on, and at
#' partial opacity the land cover beneath would tint it and misreport the value.
HEAT_OPACITY <- 1

#' Diverging, because `base` runs negative (shaded vegetation) as well as
#' positive (open hard surfacing), so a one-sided ramp would waste half its
#' range and hide the difference between cool and merely average.
HEAT_COLORS <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")


# ---------------------------------------------------------------- pipeline --

#' Map class ids to their model values, with anything unlisted becoming NA.
heat_subst <- function(r, tbl){
  terra::subst(r, from = as.integer(names(tbl)), to = unname(tbl), others = NA)
}

#' Circular 0/1 mask of the given radius, in cells of `r`.
#'
#' A mask rather than focalMat()'s weights on purpose. Those weights are
#' normalised to sum to 1, which is only a mean if every cell in the window has
#' a value; here buildings and unclassified ground are deliberately NA, so the
#' divisor has to be "cells that actually had data" - which is what
#' `fun = "mean", na.rm = TRUE` over a 0/1 mask computes.
heat_window <- function(r, radius_m){
  m <- terra::focalMat(r, radius_m, type = "circle")
  m[m > 0] <- 1
  m
}

#' The heat raster for an area.
#'
#' `aoi` is the step-1 perimeter; `groundEdits`/`canopyEdits` are a version's
#' painted rasters (either may be NULL). Returns NULL when there is no land
#' cover to work from, on the same terms as paintLandcoverSeed().
#'
#' One seed call covers both levels. Going through paintCompositeRaster() per
#' level would crop the two national rasters twice over - four file reads where
#' two will do - and the crop is the expensive part of this function.
heatRaster <- function(aoi, groundEdits = NULL, canopyEdits = NULL,
                       res = HEAT_RES, radii = HEAT_RADII,
                       weights = HEAT_WEIGHTS, ...){
  seed <- paintLandcoverSeed(aoi, ...)
  if(is.null(seed)) return(NULL)

  ground <- paintOverlayEdits(seed$ground, groundEdits)
  canopy <- paintOverlayEdits(seed$canopy, canopyEdits)

  #NA propagates through the addition, and that is the mechanism by which
  #buildings and unclassified ground leave the model rather than skewing it
  base <- heat_subst(ground, HEAT_GROUND) + heat_subst(canopy, HEAT_CANOPY)

  fact <- res / terra::res(base)[1]
  if(fact > 1){
    base <- terra::aggregate(base, fact = fact, fun = "mean", na.rm = TRUE)
  }

  out <- NULL
  for(k in names(radii)){
    f <- terra::focal(base, w = heat_window(base, radii[[k]]),
                      fun = "mean", na.rm = TRUE)
    out <- if(is.null(out)) weights[[k]] * f else out + weights[[k]] * f
  }
  names(out) <- "heat"
  out
}

#' Leaflet palette for a heat raster, centred on zero.
#'
#' Symmetric about 0 so the midpoint of the ramp is "neither warming nor
#' cooling" rather than the middle of whatever range this particular area
#' happens to span - otherwise the same colour means different things on
#' different sites, and two designs cannot be compared by eye.
heatPalette <- function(heat, colors = HEAT_COLORS){
  v <- terra::values(heat)
  v <- v[is.finite(v)]
  lim <- if(length(v)) max(abs(range(v))) else 1
  if(lim == 0) lim <- 1
  leaflet::colorNumeric(colors, domain = c(-lim, lim), na.color = "transparent")
}
