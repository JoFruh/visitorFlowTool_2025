#' Sky view factor and the sunlit-wall term - Phase 3 of the rework in
#' HEAT_COEFFICIENTS.md.
#'
#' Phase 2 answered "is the direct beam blocked here". That is only half of the
#' radiant load. The other half is how much of the sky a cell can see, because a
#' cell that is shaded but hemmed in by warm walls is not the same place as a
#' cell that is shaded and open: the first cannot radiate its heat away, and it
#' receives long-wave from every surface around it. Sky view factor is the term
#' that separates them, and it is the one thing an additive shade constant can
#' never express.
#'
#' SVF here is the fraction of the sky hemisphere a cell can see, 1 = open field,
#' 0 = fully enclosed. It is computed by reusing the Phase 2 march over a ring of
#' azimuths to get a horizon angle per direction, then
#'
#'   SVF ~ 1 - mean over directions of sin^2(horizon angle)
#'
#' which is the standard uniform-sky integration used by SOLWEIG and by Lindberg
#' & Grimmond's SVF tool.
#'
#' WHERE THIS IS DELIBERATELY COARSER THAN SOLWEIG. SOLWEIG integrates 153
#' directions; this uses 16 by default. That is the main accuracy sacrifice of
#' the whole rework and it was chosen knowingly - the plan asked for "a simplified
#' faster version of SOLWEIG which is less accurate". 16 directions resolve a
#' street canyon's two walls and a courtyard's four; they do not resolve the gap
#' between two towers. The count is an argument, so raising it costs time and
#' nothing else.
#'
#' The wall term is cruder still, and openly so. SOLWEIG models wall temperature
#' with a step-heating scheme driven by how long each facade has been in sun;
#' here a cell simply gets a bonus when it is sunlit AND stands within
#' `wall_bonus_distance_m` of an obstruction that is itself sunlit on the side
#' facing the cell. That reproduces SOLWEIG's headline finding - the hottest
#' place in a city is in front of a sunlit facade - without any of its physics.


# ------------------------------------------------------------------- SVF -----

#' Horizon angle per direction, then SVF, over an obstruction-height matrix.
#'
#' The working implementation. Same arguments, same result, and the same
#' algorithm as heat_svf_matrix_r() below - which is kept, and checked against
#' this on every run, because a reference you can read is the only way to tell
#' later whether the fast path still means what it says.
#'
#' This is C++ (src/heat_cpp.cpp) because R could not be made to do it. The scan
#' is 16 azimuths x 12 steps = 192 passes over the whole grid, and at 1.4 M
#' cells that is 1.6 s of a 12.6 s cold heatRaster(), rising to 6.4 s over
#' 6.3 x 4.5 km. Rewriting the R version to write only into the valid sub-block
#' instead of allocating two full grids per step changed NOTHING measurable -
#' the arithmetic itself was the cost, not the allocation - which is what
#' settled the question. 15-20x faster, agreeing to 2e-16.
#'
#' The shadow march in R/shadow_helpers.R is deliberately NOT given the same
#' treatment: at the tables' sun elevations it runs 2 to 3 steps, not 192, and
#' costs 0.22 s where this cost 1.56 s.
heat_svf_matrix <- function(H, res, n_dir = 16, max_dist_m = NULL){
  nr <- nrow(H); ncl <- ncol(H)
  hmax <- suppressWarnings(max(H, na.rm = TRUE))
  if(!is.finite(hmax) || hmax <= 0) return(matrix(1, nr, ncl))
  if(is.null(max_dist_m)) max_dist_m <- 4 * hmax
  #t() on the way in and byrow on the way out: svf_horizon() works row-major,
  #which is terra's order, while an R matrix is column-major. heatSvfRaster()
  #below skips both by handing over terra's values untouched.
  matrix(svf_horizon(as.vector(t(H)), nr, ncl, res, as.integer(n_dir), max_dist_m),
         nrow = nr, byrow = TRUE)
}

#' Horizon angle per direction, then SVF, over an obstruction-height raster.
#'
#' `n_dir` azimuths evenly around the compass. For each, march outward exactly as
#' heat_shadow_march() does, but keep the largest *angle* subtended rather than
#' asking a yes/no question about one sun position. The two functions are kept
#' separate rather than generalised into one: the shadow march stops as soon as
#' the ray clears the tallest obstruction, while this one must run to the full
#' search radius in every direction, and fusing them would make both slower and
#' harder to read.
#'
#' `max_dist_m` bounds the search. Beyond a few times the tallest obstruction the
#' subtended angle is too small to matter, and an unbounded scan over a large AOI
#' is the one way this becomes expensive.
heat_svf_matrix_r <- function(H, res, n_dir = 16, max_dist_m = NULL){
  nr <- nrow(H); ncl <- ncol(H)
  hmax <- suppressWarnings(max(H, na.rm = TRUE))
  if(!is.finite(hmax) || hmax <= 0) return(matrix(1, nr, ncl))

  #4x the tallest obstruction: beyond that the horizon angle is under 15 deg and
  #contributes sin^2 < 0.07 to a mean that is already dominated by nearer objects
  if(is.null(max_dist_m)) max_dist_m <- 4 * hmax
  nsteps <- max(1L, as.integer(ceiling(max_dist_m / res)))
  nsteps <- min(nsteps, max(nr, ncl))

  acc <- matrix(0, nr, ncl)
  az <- seq(0, 2 * pi, length.out = n_dir + 1)[seq_len(n_dir)]
  for(a in az){
    dx <- sin(a); dy <- cos(a)
    s <- max(abs(dx), abs(dy))
    dx <- dx / s; dy <- dy / s
    step_m <- res * sqrt(dx^2 + dy^2)

    best <- matrix(0, nr, ncl)          #tangent of the largest angle so far
    for(k in seq_len(nsteps)){
      ro <- as.integer(round(k * dy)); co <- as.integer(round(k * dx))
      sr <- seq_len(nr) - ro; sc <- seq_len(ncl) + co
      kr <- which(sr >= 1 & sr <= nr); kc <- which(sc >= 1 & sc <= ncl)
      if(!length(kr) || !length(kc)) break
      #relative height matters, not absolute: a cell on a roof is not overshadowed
      #by its own building. The consequence is worth stating, because it looks
      #like a bug in any summary that does not exclude them - a cell that IS an
      #obstruction comes out at SVF ~ 1, since everything around it is lower.
      #That is correct (a roof does see the whole sky) and harmless for the heat
      #model, which reads ground-level cells; but average SVF over a town centre
      #without masking obstruction cells reports the rooftops, not the streets.
      cand <- matrix(0, nr, ncl)
      cand[kr, kc] <- (H[sr[kr], sc[kc]] - H[kr, kc]) / (k * step_m)
      best <- pmax(best, cand)
    }
    #tan -> sin^2 without a trig call: sin^2 = t^2 / (1 + t^2)
    t2 <- best^2
    acc <- acc + t2 / (1 + t2)
  }
  out <- 1 - acc / n_dir
  pmin(pmax(out, 0), 1)
}

#' Sky view factor over an area, as a raster in [0, 1].
heatSvfRaster <- function(ground, canopy, n_dir = 16, geom = heatGeometry()){
  H <- heatObstructionHeight(ground, canopy, geom)
  if(is.null(H)) return(NULL)
  #terra::values() is already row-major, which is exactly what svf_horizon()
  #wants, so this path never builds an R matrix and never transposes. NA is 0:
  #outside the study area there is nothing standing up to block the sky.
  v <- terra::values(H, mat = FALSE)
  v[is.na(v)] <- 0
  hmax <- if(length(v)) max(v) else 0
  sv <- if(!is.finite(hmax) || hmax <= 0) rep(1, length(v)) else
    svf_horizon(as.numeric(v), terra::nrow(H), terra::ncol(H),
                terra::res(H)[1], as.integer(n_dir), 4 * hmax)
  out <- terra::setValues(terra::rast(H), sv)
  names(out) <- "svf"
  out
}


# ------------------------------------------------------- sunlit wall term ----

#' Cells standing in front of a sunlit facade.
#'
#' Returns a 0/1 raster. A cell qualifies when all three hold:
#'
#'  1. it is itself in the sun (a shaded cell in front of a hot wall is still
#'     shaded, and the tables already price that);
#'  2. it has no obstruction of its own - a facade bonus on a building's own
#'     footprint is meaningless;
#'  3. within `wall_bonus_distance_m`, in the direction *toward* the sun's
#'     azimuth reflected about the cell, there stands an obstruction whose face
#'     turned toward the cell is lit.
#'
#' Condition 3 is the whole trick, and it is simpler than it sounds: a facade is
#' lit when the sun is on the same side of it as the cell. Marching *away* from
#' the sun from each cell and asking whether we run into an obstruction is
#' equivalent, because an obstruction found downsun of a sunlit cell must be
#' presenting its sunward face to that cell.
heatWallRaster <- function(ground, canopy, bin = "midday", geom = heatGeometry(),
                           shade = NULL){
  sun <- heatSunPosition(bin, geom)
  if(is.null(sun) || is.null(geom)) return(NULL)
  H <- heatObstructionHeight(ground, canopy, geom)
  if(is.null(H)) return(NULL)
  if(is.null(shade)) shade <- heatShadeRaster(ground, canopy, bin, geom)
  if(is.null(shade)) return(NULL)

  res <- terra::res(H)[1]
  dist_m <- unname(geom[["wall_bonus_distance_m"]])
  if(is.na(dist_m) || dist_m <= 0) return(terra::setValues(terra::rast(H), 0L))

  m <- terra::as.matrix(H, wide = TRUE)
  m[is.na(m)] <- 0
  nr <- nrow(m); ncl <- ncol(m)

  az <- sun$azimuth * pi / 180
  #away from the sun - the direction in which a wall shows us its lit face
  dx <- -sin(az); dy <- -cos(az)
  s <- max(abs(dx), abs(dy)); dx <- dx / s; dy <- dy / s
  step_m <- res * sqrt(dx^2 + dy^2)
  nsteps <- max(1L, as.integer(round(dist_m / step_m)))

  near <- matrix(FALSE, nr, ncl)
  for(k in seq_len(nsteps)){
    ro <- as.integer(round(k * dy)); co <- as.integer(round(k * dx))
    sr <- seq_len(nr) - ro; sc <- seq_len(ncl) + co
    kr <- which(sr >= 1 & sr <= nr); kc <- which(sc >= 1 & sc <= ncl)
    if(!length(kr) || !length(kc)) next
    hit <- matrix(FALSE, nr, ncl)
    #only a real wall counts, not a tree crown standing on stilts of air
    hit[kr, kc] <- m[sr[kr], sc[kc]] > m[kr, kc] + 1
    near <- near | hit
  }

  wall <- terra::setValues(terra::rast(H), as.integer(as.vector(t(near))))
  own  <- terra::ifel(H > 0, 1L, 0L)
  out  <- terra::ifel(wall == 1L & shade == 0L & own == 0L, 1L, 0L)
  names(out) <- "wall"
  out
}
