#' Cast shadows for the heat model - Phase 2 of the rework in HEAT_COEFFICIENTS.md.
#'
#' Phase 1 produced the parameter tables; this file turns the geometry half of
#' them into an actual shadow raster. Until now "shaded" meant *a canopy stands
#' on this exact cell* - shade fell straight down, so a 15 m tree cooled its own
#' footprint and nothing else. That is wrong by the width of the shadow: at the
#' morning and afternoon sun elevation the tables fix (45.8 deg), a 15 m tree
#' casts 14.6 m, which at 5 m is three cells away from the trunk.
#'
#' The method is Ratti & Richens (2004), which is also what SOLWEIG uses: march
#' the obstruction-height raster toward the sun one cell at a time, lowering the
#' ray by `step * tan(elevation)` at each step, and keep the running maximum. A
#' cell is shaded when some obstruction along that ray still stands above the
#' ray when it arrives. It is a handful of whole-matrix shifts, no ray tracing
#' per cell, and it is exact for the height field it is given.
#'
#' WHAT IS DELIBERATELY NOT MODELLED. There is no height raster: every tree is
#' 15 m, every building 12 m, every artificial canopy 4 m, read from
#' heat_geometry.csv (see the "no height raster" decision in
#' HEAT_COEFFICIENTS.md). Real Swiss canopy is 15-25 m and real buildings
#' 12-15 m, so shadow *lengths* are right on average and wrong per object. The
#' tables are the place to retune that, not this file.
#'
#' Terrain is not in the height field either - only canopy and buildings. A
#' valley in its own mountain's shadow is invisible here, which matters in
#' exactly the alpine settings this tool is often pointed at. Phase 3's horizon
#' scan is where a DEM would enter.
#'
#' Sky view factor and the sunlit-wall term are Phase 3 and are not here.


# ------------------------------------------------------------- geometry ------

#' The geometry parameter table, as a named numeric vector.
#'
#' Read through vftData() like every other table the app reads, and with
#' read.csv2() because these files are semicolon-separated *and* decimal-comma -
#' read.csv() returns every value as a character string that silently becomes NA
#' on as.numeric(). Cached, because this is called once per time bin per render
#' and the file never changes within a session.
heatGeometry <- local({
  cache <- NULL
  function(refresh = FALSE){
    if(!is.null(cache) && !refresh) return(cache)
    f <- try(vftData("tables/heat_geometry.csv"), silent = TRUE)
    if(inherits(f, "try-error") || !file.exists(f)){
      warning("heat_geometry.csv not found; cast shadows are unavailable")
      return(NULL)
    }
    d <- utils::read.csv2(f, stringsAsFactors = FALSE)
    v <- stats::setNames(suppressWarnings(as.numeric(d$value)), d$parameter)
    cache <<- v
    v
  }
})

#' The three time bins the heat tables are built for.
HEAT_BINS <- c("morning", "midday", "afternoon")

#' Sun elevation and azimuth for a time bin, in degrees.
#'
#' These are *stored*, not computed: Phase 1 froze them for the reference hot
#' day (mid-July, 47 deg N) so that every number in the tables belongs to one
#' declared geometry. Morning and afternoon share an elevation of 45.8 deg and
#' differ only in azimuth - they are symmetric about solar noon - which is worth
#' knowing before reading a map, because it means the morning/afternoon heat
#' difference in the tables is thermal inertia and not sun angle.
heatSunPosition <- function(bin = "midday", geom = heatGeometry()){
  bin <- match.arg(bin, HEAT_BINS)
  if(is.null(geom)) return(NULL)
  el <- geom[[paste0("sun_elevation_", bin)]]
  az <- geom[[paste0("sun_azimuth_", bin)]]
  if(is.na(el) || is.na(az)) return(NULL)
  list(elevation = el, azimuth = az)
}

#' Representative obstruction height per class id, in metres.
#'
#' Keyed by the id in the raster the height is read from, which is why buildings
#' appear twice: artificial_block is `level = "both"` in PAINT_CATEGORIES and
#' generate_ground_canopy_CH.r burns it into the ground *and* the canopy raster,
#' so whichever layer is consulted gives the same 12 m.
heatHeights <- function(geom = heatGeometry()){
  if(is.null(geom)) return(NULL)
  c("6" = unname(geom[["height_canopy_artificial"]]),
    "7" = unname(geom[["height_canopy_tree"]]),
    "8" = unname(geom[["height_artificial_block"]]))
}


# --------------------------------------------------------- the height field --

#' Obstruction heights over an area, from the class rasters alone.
#'
#' Everything not listed in heatHeights() is 0 - open sky (0), cleared canopy
#' (9) and every ground material. NA becomes 0 as well: outside the study area
#' there is nothing to cast a shadow, and an NA here would poison the running
#' maximum and blank out the shadow raster wherever a ray crossed the edge.
heatObstructionHeight <- function(ground, canopy, geom = heatGeometry()){
  h <- heatHeights(geom)
  if(is.null(h)) return(NULL)
  ids <- as.integer(names(h))

  hc <- terra::subst(canopy, from = ids, to = unname(h), others = 0)
  #a building is in the ground raster too, and only there when a plan import has
  #cleared the canopy above it
  hg <- terra::subst(ground, from = 8L, to = unname(h[["8"]]), others = 0)
  out <- max(hc, hg, na.rm = TRUE)
  terra::ifel(is.na(out), 0, out)
}


# ------------------------------------------------------------- the shadow ----

#' Ratti & Richens shadow march over a height matrix.
#'
#' Returns a logical matrix: TRUE where an obstruction blocks the direct beam at
#' the cell's own surface. `res` is the cell size in metres, `elev`/`azim`
#' degrees, with azimuth clockwise from north and pointing *toward* the sun.
#'
#' "Its own surface", not "ground level", is the whole of the last comparison
#' below and it is not a detail. A cell that is itself an obstruction presents
#' its roof to the sun, so the ray only has to clear `H` there, not 0. Compared
#' against 0 instead, a flat plain of equal-height buildings shades 98 % of its
#' own roofs at midday and an isolated building shades 80 % of itself - every
#' roof reads as a cool surface because it picks the shaded row of the material
#' table. On ground cells H is 0 and the two forms are identical, which is why
#' this stayed invisible: the terms the tables were reviewed on never exercised
#' it. It also puts this module back in agreement with heat_svf_matrix(), which
#' measures horizon angles from the cell's own height for the same reason.
#'
#' The step is normalised so the longer of the two components is exactly one
#' cell. That keeps every step a whole number of cells - the shift is then plain
#' matrix indexing rather than an interpolation - while `step_m` records how far
#' the ray actually travelled, which is what the ray must be lowered by. Getting
#' that wrong is the classic bug in this algorithm: use `res` for a diagonal
#' azimuth and every shadow comes out a factor of sqrt(2) too long.
#'
#' k starts at 1, so a cell is never shaded by the obstruction standing on it.
#' That is deliberate and heatShadeRaster() relies on it - the canopy directly
#' overhead is a separate, certain thing, and keeping the two apart is what lets
#' the caller tell "under a tree" from "in a tree's shadow".
heat_shadow_march <- function(H, res, elev, azim){
  nr <- nrow(H); ncl <- ncol(H)
  out <- matrix(FALSE, nr, ncl)
  hmax <- max(H, na.rm = TRUE)
  if(!is.finite(hmax) || hmax <= 0) return(out)
  if(elev <= 0) return(!out)            #sun below the horizon: everything is shaded

  el <- elev * pi / 180
  az <- azim * pi / 180
  dx <- sin(az)                          #+ east
  dy <- cos(az)                          #+ north
  s  <- max(abs(dx), abs(dy))
  if(s == 0) return(out)
  dx <- dx / s; dy <- dy / s             #one component is now exactly +-1
  step_m <- res * sqrt(dx^2 + dy^2)
  rise   <- step_m * tan(el)
  if(rise <= 0) return(out)

  nsteps <- as.integer(ceiling(hmax / rise))
  if(nsteps < 1) return(out)
  #a ray cannot usefully travel further than the grid
  nsteps <- min(nsteps, max(nr, ncl))

  run <- matrix(0, nr, ncl)
  for(k in seq_len(nsteps)){
    ro <- as.integer(round(k * dy))      #rows are indexed southward, so this is
    co <- as.integer(round(k * dx))      #subtracted below
    sr <- seq_len(nr) - ro
    sc <- seq_len(ncl) + co
    kr <- which(sr >= 1 & sr <= nr)
    kc <- which(sc >= 1 & sc <= ncl)
    if(!length(kr) || !length(kc)) next
    #off-grid neighbours stay 0: nothing outside the window casts a shadow into it
    cand <- matrix(0, nr, ncl)
    cand[kr, kc] <- H[sr[kr], sc[kc]] - k * rise
    run <- pmax(run, cand)
  }
  run > H
}

#' Shade over an area for one time bin.
#'
#' Returns a 0/1 raster on `ground`'s grid, where 1 means the direct beam is
#' blocked - either because a canopy stands on the cell or because something
#' nearby casts its shadow onto it.
#'
#' `own_canopy` is what the model did before Phase 2 and is kept in the sum on
#' purpose rather than being replaced: a cell under a crown is shaded at every
#' sun angle, which the march cannot say because it deliberately starts one cell
#' out. Ground class 8 is *not* treated as its own shade - the ground under a
#' building is not a place to stand.
heatShadeRaster <- function(ground, canopy, bin = "midday", geom = heatGeometry()){
  sun <- heatSunPosition(bin, geom)
  if(is.null(sun)) return(NULL)
  H <- heatObstructionHeight(ground, canopy, geom)
  if(is.null(H)) return(NULL)

  m <- terra::as.matrix(H, wide = TRUE)
  m[is.na(m)] <- 0
  cast <- heat_shadow_march(m, terra::res(H)[1], sun$elevation, sun$azimuth)

  #t() before flattening, and it is not optional: terra fills a raster row by
  #row, while as.vector() on an R matrix walks it column by column. Without the
  #transpose the shade raster comes back transposed - which on a square window
  #still looks like a plausible map of shadows, and is silently wrong everywhere.
  out <- terra::setValues(terra::rast(H), as.integer(t(cast)))
  #NOT `canopy %in% c(6L, 7L)`: terra defines an S4 `%in%` for SpatRaster, but
  #this package reaches terra through `terra::` and imports nothing from it, so
  #inside the namespace `%in%` is base's - which calls match() on the raster
  #and dies with "'match' requires vector arguments". It works in any script
  #that has library(terra) on the search path, which is why it passed every
  #check and still killed the app.
  own <- terra::ifel(canopy == 6L | canopy == 7L, 1L, 0L)
  own <- terra::ifel(is.na(own), 0L, own)
  out <- terra::ifel((out + own) > 0, 1L, 0L)
  names(out) <- "shade"
  out
}
