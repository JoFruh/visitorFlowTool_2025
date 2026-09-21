// Heat model inner loops.
//
// Two whole-grid scans that dominated heatRaster() and could not be made fast
// in R. Both are exact replacements, not approximations: the connected-component
// labeller returns a mask bit-identical to the terra::patches() pipeline it
// replaces, and the horizon scan agrees with the R version to 2e-16.
//
// Measured over a 3.6 x 2.6 km window at Sion (621 x 821 at HEAT_RES = 5 m),
// which is the area that took 12.6 s to compute cold:
//
//   patch masks, 7 classes   7.59 s -> 0.37 s   (21x; 49x over 6.3 x 4.5 km)
//   sky view factor          1.56 s -> 0.11 s   (15-20x)
//
// Both take and return grids in ROW-MAJOR order, which is terra's own layout -
// terra::values(r, mat = FALSE) hands one over directly and setValues() takes
// one back. Do NOT pass as.vector() of an R matrix, which walks column by
// column; that is the same transposition trap heatShadeRaster() documents.

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <cmath>

using namespace Rcpp;


// ----------------------------------------------------- connected components --

/*
 * ccl_big_patches:
 *
 * Two-pass union-find connected-component labelling with 8-connectivity,
 * keeping only components of at least `min_cells` cells. Replaces
 *
 *     pch <- terra::patches(src, directions = 8, zeroAsNA = TRUE)
 *     fr  <- terra::freq(pch)
 *     big <- fr$value[fr$count * res^2 >= min_patch_ha * 10000]
 *     msk <- terra::subst(pch, from = big, to = 1, others = 0)
 *
 * with one pass, and answers the only question heat_source_mask() actually
 * asks - "is this cell in a patch big enough to count" - without ever building
 * the intermediate label raster.
 *
 * terra::patches() is not merely slower, it is superlinear: 2.7x the cells cost
 * it 7x the time, which is what made a 6.3 km AOI take 73 s. This is O(n a(n)).
 *
 * `src` is 1 where the class is present and 0 or NA elsewhere. The first pass
 * labels each cell from its four already-visited 8-neighbours (W, NW, N, NE),
 * merging their labels when a cell joins two runs that were not yet known to be
 * one patch. The second pass counts each root and writes the 0/1 answer.
 */
// [[Rcpp::export]]
IntegerVector ccl_big_patches(IntegerVector src, int nrow, int ncol,
                              double min_cells) {
  const R_xlen_t n = (R_xlen_t)nrow * (R_xlen_t)ncol;
  if (src.size() != n) stop("ccl_big_patches: src length does not match nrow*ncol");

  std::vector<int> lab((size_t)n, 0);
  std::vector<int> parent;
  parent.push_back(0);                    // label 0 means "no patch"

  // path-halving find; union keeps the smaller label as the root so the first
  // label seen for a patch stays its representative
  auto find = [&parent](int x) {
    while (parent[(size_t)x] != x) {
      parent[(size_t)x] = parent[(size_t)parent[(size_t)x]];
      x = parent[(size_t)x];
    }
    return x;
  };
  auto merge = [&parent, &find](int a, int b) {
    a = find(a); b = find(b);
    if (a == b) return;
    if (a < b) parent[(size_t)b] = a; else parent[(size_t)a] = b;
  };

  // the four 8-neighbours that are already labelled when this cell is reached
  const int off[4][2] = {{0, -1}, {-1, -1}, {-1, 0}, {-1, 1}};

  for (int r = 0; r < nrow; ++r) {
    for (int c = 0; c < ncol; ++c) {
      const R_xlen_t i = (R_xlen_t)r * ncol + c;
      const int v = src[i];
      if (v == NA_INTEGER || v == 0) continue;

      int best = 0;
      for (int k = 0; k < 4; ++k) {
        const int rr = r + off[k][0], cc = c + off[k][1];
        if (rr < 0 || cc < 0 || cc >= ncol) continue;
        const int l = lab[(size_t)((R_xlen_t)rr * ncol + cc)];
        if (l == 0) continue;
        if (best == 0) best = l; else merge(best, l);
      }
      if (best == 0) {                    // a patch nobody has met yet
        best = (int)parent.size();
        parent.push_back(best);
      }
      lab[(size_t)i] = best;
    }
  }

  // count per root, in cells. double and not int because a single patch can
  // exceed 2^31 cells in principle and the comparison is against a double anyway
  std::vector<double> cnt(parent.size(), 0.0);
  for (R_xlen_t i = 0; i < n; ++i)
    if (lab[(size_t)i]) cnt[(size_t)find(lab[(size_t)i])] += 1.0;

  IntegerVector out(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    const int l = lab[(size_t)i];
    out[i] = (l && cnt[(size_t)find(l)] >= min_cells) ? 1 : 0;
  }
  return out;
}


// ------------------------------------------------------------ horizon / SVF --

/*
 * svf_horizon:
 *
 * The march in heat_svf_matrix(), one for one. `n_dir` azimuths evenly around
 * the compass; along each, step outward keeping the largest tangent of the
 * angle subtended by anything the ray passes, measured from the cell's OWN
 * height - a cell on a roof is not overshadowed by its own building. Then
 *
 *     SVF = 1 - mean over directions of sin^2(horizon), sin^2 = t^2/(1+t^2)
 *
 * The R version allocates two full grids per step and pmax()es the whole grid,
 * 16 x 12 = 192 times. Rewriting that in R to write only into the valid
 * sub-block changed nothing measurable - the arithmetic itself was the cost,
 * which is why this is in C++ and the shadow march, at 2-3 steps, is not.
 *
 * The inner loop is bounded to the rows and columns where the shifted read is
 * on the grid, so nothing off the edge contributes; that matches the R version,
 * where the out-of-range block of `cand` stayed 0.
 */
// [[Rcpp::export]]
NumericVector svf_horizon(NumericVector H, int nrow, int ncol, double res,
                          int n_dir, double max_dist_m) {
  const R_xlen_t n = (R_xlen_t)nrow * (R_xlen_t)ncol;
  if (H.size() != n) stop("svf_horizon: H length does not match nrow*ncol");
  if (n_dir < 1) stop("svf_horizon: n_dir must be at least 1");

  int nsteps = (int)std::ceil(max_dist_m / res);
  if (nsteps < 1) nsteps = 1;
  const int lim = nrow > ncol ? nrow : ncol;
  if (nsteps > lim) nsteps = lim;

  std::vector<double> acc((size_t)n, 0.0), best((size_t)n, 0.0);

  for (int d = 0; d < n_dir; ++d) {
    const double a = 2.0 * M_PI * (double)d / (double)n_dir;
    double dx = std::sin(a), dy = std::cos(a);
    const double s = std::max(std::fabs(dx), std::fabs(dy));
    if (s == 0.0) continue;
    dx /= s; dy /= s;                     // one component is now exactly +-1
    const double step_m = res * std::sqrt(dx * dx + dy * dy);

    std::fill(best.begin(), best.end(), 0.0);
    for (int k = 1; k <= nsteps; ++k) {
      const int ro = (int)std::lround((double)k * dy);
      const int co = (int)std::lround((double)k * dx);
      const double inv = 1.0 / ((double)k * step_m);

      // rows are indexed southward, so the source row is r - ro
      const int r0 = std::max(0, ro),  r1 = std::min(nrow, nrow + ro);
      const int c0 = std::max(0, -co), c1 = std::min(ncol, ncol - co);
      if (r1 <= r0 || c1 <= c0) break;

      for (int r = r0; r < r1; ++r) {
        const double* hs = &H[(R_xlen_t)(r - ro) * ncol];
        const double* hh = &H[(R_xlen_t)r * ncol];
        double* bb = &best[(size_t)((R_xlen_t)r * ncol)];
        for (int c = c0; c < c1; ++c) {
          const double t = (hs[c + co] - hh[c]) * inv;
          if (t > bb[c]) bb[c] = t;
        }
      }
    }
    for (R_xlen_t i = 0; i < n; ++i) {
      const double t2 = best[(size_t)i] * best[(size_t)i];
      acc[(size_t)i] += t2 / (1.0 + t2);
    }
  }

  NumericVector out(n);
  for (R_xlen_t i = 0; i < n; ++i) {
    const double v = 1.0 - acc[(size_t)i] / (double)n_dir;
    out[i] = v < 0.0 ? 0.0 : (v > 1.0 ? 1.0 : v);
  }
  return out;
}
