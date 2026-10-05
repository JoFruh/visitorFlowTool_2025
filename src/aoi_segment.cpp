// Areas of interest: the basins of the attractiveness raster.
//
// generateAoI2() used to make one area out of every connected run of cells
// above the threshold, so two forests joined by a thin attractive strip came
// out as a single destination. R/aoiSegment.R splits those runs into the
// high-value cores they grow from and merges the cores back only where a path
// crosses between them and the dip between them is shallow. This file is the
// part that cannot be done fast in R: one sorted sweep over every cell.
//
// The grid is ROW-MAJOR, terra's own layout - terra::values(r, mat = FALSE)
// hands one over directly. Do NOT pass as.vector() of an R matrix (the
// transposition trap heat_cpp.cpp and heatShadeRaster() document).

#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <cstdint>
#include <cmath>
#include <limits>

using namespace Rcpp;

/*
 * aoi_basins:
 *
 * A watershed of the attractiveness surface, restricted to cells >= `thresh`,
 * with rook (4-) connectivity - the adjacency terra::as.polygons(aggregate =
 * TRUE) dissolves by, so the basins tile exactly the areas the threshold alone
 * would have made.
 *
 * Cells are visited from the most attractive down. A cell none of whose four
 * neighbours has been visited yet is a local maximum and starts a basin; its
 * value is that basin's peak. Any other cell joins the basin of its most
 * attractive visited neighbour, i.e. it is grown from the core it climbs to.
 * Basins are never merged here. When a cell touches two basins, they are
 * recorded as neighbours, and the value at that FIRST contact is their saddle:
 * the sweep runs downhill, so the first contact is the highest pass between
 * them - how far a walker has to come down to get from one core to the other.
 *
 * Ties are visited in cell order, so a plateau is one basin, not one per cell.
 *
 * Each pair also gets the LENGTH of its border, as the number of cell edges
 * the two basins share: `nh` across a left/right step (an edge as long as a
 * cell is high), `nv` across an up/down step (as long as a cell is wide). A
 * label never changes once given, so every shared edge is counted exactly
 * once - when the second of its two cells is visited. A narrow neck shares a
 * few edges; two halves of one wide block share many.
 *
 * Each basin gets its CIRCUMFERENCE the same way: `ph`/`pv` count the cell edges
 * between it and anything else - another basin, land below the threshold, or
 * the grid's edge - holes included.
 *
 * Returns list(label = integer per cell, 0 below the threshold or NA;
 *              peak, ncell, ph, pv = per basin, 1-based;
 *              a, b, saddle, nh, nv = one row per neighbouring pair, a < b).
 */
// [[Rcpp::export]]
List aoi_basins(NumericVector v, int nrow, int ncol, double thresh) {
  const R_xlen_t n = (R_xlen_t)nrow * (R_xlen_t)ncol;
  if (v.size() != n) stop("aoi_basins: v length does not match nrow*ncol");

  std::vector<int> cells;
  cells.reserve((size_t)n);
  for (R_xlen_t i = 0; i < n; ++i)
    if (!ISNAN(v[i]) && v[i] >= thresh) cells.push_back((int)i);
  std::stable_sort(cells.begin(), cells.end(),
                   [&v](int x, int y) { return v[x] > v[y]; });

  std::vector<int> lab((size_t)n, 0);
  std::vector<double> peak;
  std::vector<int> ncell;
  struct Pair { double saddle; int nh, nv; };
  std::unordered_map<int64_t, Pair> pairs;     // (a, b) -> saddle, border
  std::vector<int64_t> order;                  // pairs in first-contact order

  for (int c : cells) {
    const int r = c / ncol, k = c % ncol;
    int nb[4], m = 0;
    bool horiz[4];                             // a left/right neighbour?
    if (r > 0)        { horiz[m] = false; nb[m++] = c - ncol; }
    if (r < nrow - 1) { horiz[m] = false; nb[m++] = c + ncol; }
    if (k > 0)        { horiz[m] = true;  nb[m++] = c - 1; }
    if (k < ncol - 1) { horiz[m] = true;  nb[m++] = c + 1; }

    int best = -1;
    for (int j = 0; j < m; ++j)
      if (lab[nb[j]] > 0 && (best < 0 || v[nb[j]] > v[best])) best = nb[j];

    if (best < 0) {
      peak.push_back(v[c]);
      ncell.push_back(1);
      lab[c] = (int)peak.size();
      continue;
    }
    lab[c] = lab[best];
    ncell[lab[c] - 1]++;

    for (int j = 0; j < m; ++j) {
      const int l = lab[nb[j]];
      if (l <= 0 || l == lab[c]) continue;
      const int a = std::min(l, lab[c]), b = std::max(l, lab[c]);
      const int64_t key = ((int64_t)a << 32) | (int64_t)b;
      auto it = pairs.emplace(key, Pair{v[c], 0, 0});
      if (it.second) order.push_back(key);
      if (horiz[j]) it.first->second.nh++; else it.first->second.nv++;
    }
  }

  //circumference per basin: every side of a labelled cell that faces a cell of
  //another label (0 included) or the grid's edge
  std::vector<int> ph(peak.size(), 0), pv(peak.size(), 0);
  for (int c : cells) {
    const int r = c / ncol, k = c % ncol, l = lab[c];
    if (k == 0        || lab[c - 1]    != l) ph[l - 1]++;
    if (k == ncol - 1 || lab[c + 1]    != l) ph[l - 1]++;
    if (r == 0        || lab[c - ncol] != l) pv[l - 1]++;
    if (r == nrow - 1 || lab[c + ncol] != l) pv[l - 1]++;
  }

  const size_t ne = order.size();
  IntegerVector ea(ne), eb(ne), enh(ne), env(ne);
  NumericVector es(ne);
  for (size_t i = 0; i < ne; ++i) {
    const Pair& p = pairs[order[i]];
    ea[i]  = (int)(order[i] >> 32);
    eb[i]  = (int)(order[i] & 0xffffffff);
    es[i]  = p.saddle;
    enh[i] = p.nh;
    env[i] = p.nv;
  }

  return List::create(_["label"]  = IntegerVector(lab.begin(), lab.end()),
                      _["peak"]   = NumericVector(peak.begin(), peak.end()),
                      _["ncell"]  = IntegerVector(ncell.begin(), ncell.end()),
                      _["ph"]     = IntegerVector(ph.begin(), ph.end()),
                      _["pv"]     = IntegerVector(pv.begin(), pv.end()),
                      _["a"] = ea, _["b"] = eb, _["saddle"] = es,
                      _["nh"] = enh, _["nv"] = env);
}

// 1-D squared distance transform of a sampled function (Felzenszwalb &
// Huttenlocher 2012): d[q] = min_p (f[p] + ((q - p) * s)^2), the lower envelope
// of parabolas rooted at every p. `s` is the sample spacing in metres.
static void aoi_edt1d(const std::vector<double>& f, std::vector<double>& d,
                      double s, int n) {
  const double INF = std::numeric_limits<double>::infinity();
  std::vector<int> v((size_t)n);
  std::vector<double> z((size_t)n + 1);
  int k = 0;
  v[0] = 0; z[0] = -INF; z[1] = INF;
  for (int q = 1; q < n; ++q) {
    //every f is finite and z[0] = -inf, so k never falls below 0
    double sq;
    for (;;) {
      const int p = v[k];
      sq = ((f[q] + (q * s) * (q * s)) - (f[p] + (p * s) * (p * s))) /
           (2.0 * s * (q - p));
      if (sq > z[k]) break;
      --k;
    }
    ++k; v[k] = q; z[k] = sq; z[k + 1] = INF;
  }
  k = 0;
  for (int q = 0; q < n; ++q) {
    while (z[k + 1] < q * s) ++k;
    const double t = (q - v[k]) * s;
    d[q] = t * t + f[v[k]];
  }
}

/*
 * aoi_edt:
 *
 * For R/aoiCut.R, the inverse split: how far, in metres, each cell of the
 * threshold mask lies from the nearest cell OUTSIDE it - below the threshold,
 * NA, or beyond the grid's edge. A body's interior peaks at the radius of the
 * largest circle that fits in it; a bridge between two bodies is a saddle at
 * half the bridge's width. aoi_basins() on this surface cuts at the bridges.
 *
 * Exact Euclidean (cell centre to cell centre), separable: columns first with
 * spacing `dy`, then rows with spacing `dx` - cells need not be square, and on
 * lon/lat they are not (~73 x 106 m). The grid is ROW-MAJOR, as aoi_basins().
 * Each line is padded with one outside cell at both ends, which is what makes
 * the grid's edge count as outside.
 *
 * Returns a numeric vector per cell: the distance inside, NA outside.
 */
// [[Rcpp::export]]
NumericVector aoi_edt(LogicalVector inside, int nrow, int ncol, double dx, double dy) {
  const R_xlen_t n = (R_xlen_t)nrow * (R_xlen_t)ncol;
  if (inside.size() != n) stop("aoi_edt: inside length does not match nrow*ncol");
  // "infinitely far" without inf: inf - inf in the envelope would be NaN
  const double BIG = 1e20;
  std::vector<double> g((size_t)n);

  std::vector<double> f((size_t)nrow + 2), d((size_t)nrow + 2);
  for (int k = 0; k < ncol; ++k) {
    f[0] = 0; f[nrow + 1] = 0;
    for (int r = 0; r < nrow; ++r) {
      const int x = inside[(R_xlen_t)r * ncol + k];
      f[r + 1] = (x == TRUE) ? BIG : 0;
    }
    aoi_edt1d(f, d, dy, nrow + 2);
    for (int r = 0; r < nrow; ++r) g[(size_t)r * ncol + k] = d[r + 1];
  }

  NumericVector out(n, NA_REAL);
  f.assign((size_t)ncol + 2, 0); d.assign((size_t)ncol + 2, 0);
  for (int r = 0; r < nrow; ++r) {
    f[0] = 0; f[ncol + 1] = 0;
    for (int k = 0; k < ncol; ++k) f[k + 1] = g[(size_t)r * ncol + k];
    aoi_edt1d(f, d, dx, ncol + 2);
    for (int k = 0; k < ncol; ++k) {
      const R_xlen_t c = (R_xlen_t)r * ncol + k;
      if (inside[c] == TRUE) out[c] = std::sqrt(d[k + 1]);
    }
  }
  return out;
}
