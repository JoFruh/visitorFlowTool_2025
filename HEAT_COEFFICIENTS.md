# Heat coefficients: where the numbers come from

The Hitzeminderung model rework, all four phases. This document explains the
parameter tables that replaced the hardcoded constants in
[R/heat_helpers.R](R/heat_helpers.R), and the model now built on them.

**Status: reviewable, not validated.** These numbers replace *invented* constants
with *sourced* ones, and the model that consumes them is checked against the
tables and against closed-form geometry. That is a strict improvement and nothing
more. **No value here has been compared against a measurement in the study
area.**

| Phase | What | Where |
| --- | --- | --- |
| 1 | literature parameter tables | `<dataDir>/tables/`, built by `data-raw/build_heat_tables.py` |
| 2 | cast shadows | [R/shadow_helpers.R](R/shadow_helpers.R) |
| 3 | sky view factor, sunlit wall | [R/svf_helpers.R](R/svf_helpers.R) |
| 4 | the model rewrite | [R/heat_helpers.R](R/heat_helpers.R) |

## Why this exists

`R/heat_helpers.R` says so itself, at lines 19-20:

> "Every number here is a first pass and expected to be tuned, which is why they
> are all named constants rather than literals in the pipeline below."

Nothing in the repo cited a source for `HEAT_GROUND`, `HEAT_CANOPY`, `HEAT_RADII`
or `HEAT_WEIGHTS`, including the choice of 10 m and 100 m as the two radii.

## The files

All four live in `<dataDir>/tables/`, resolved by `vftDataDir()`
([R/data_paths.R:60-90](R/data_paths.R#L60-L90)) — **outside the repo**, like
every other table the app reads.

| File | Contents |
| --- | --- |
| `heat_materials.csv` | 45 rows: class x shaded x time bin -> local dPET |
| `heat_decay.csv` | 9 rows: per-class advective decay parameters + derived samples |
| `heat_geometry.csv` | 26 rows: sun positions, obstruction heights, SVF/wall/conversion constants |
| `heat_references.csv` | 10 rows: full citations, climate zone, method, key values |
| `heat_coefficients.xlsx` | 8-sheet review workbook built from the four CSVs |

> **Decimal comma is mandatory.** The app reads tables with `read.csv2()`
> ([R/step2_server.R:293-297](R/step2_server.R#L293-L297)), which is `sep=";"`
> **and `dec=","`**. A file written with decimal points parses every numeric
> column as `character`.

Because the CSVs live outside the repo, the scripts that produce them are kept
**inside** it, in `data-raw/` alongside the existing standalone data-prep scripts:

```
data-raw/build_heat_tables.py   # the values live here - edit this, not the CSV
data-raw/verify_heat_tables.R   # 43 checks on the tables
data-raw/build_heat_xlsx.R      # the review workbook, built from the CSVs
data-raw/verify_shadows.R       # 24 checks on Phase 2
data-raw/verify_svf.R           # 23 checks on Phase 3
data-raw/verify_heat_model.R    # 43 checks on the assembled model
```

Each exits non-zero on failure. Run in that order. Generation is Python and verification is R **on purpose**, so
the check is independent of the toolchain that produced the files. Both take a
`VFT_TABLES` environment variable to override the output directory.

`build_heat_tables.py` is the editable source of truth for every number. Editing
a CSV by hand works but will be silently overwritten on the next regeneration,
and for derived decay columns verification step 4 will catch it.

## Choice 1 — the target variable is PET

Not land surface temperature. LST is the best-populated literature and gives the
largest material contrasts, but a hot surface you never touch overstates what a
visitor feels. PET is the human heat-balance measure defined in VDI 3787 Blatt 2,
which is the standard behind Swiss and German Klimaanalyse practice — the right
target for a visitor-flow tool, and the defensible one for a WSL product.

Consequence: **shade dominates and material matters less than an LST-based model
would suggest.** Measured directly by Speak et al.: sunlit *globe* temperatures
span 12.3 K across five materials, sunlit *surface* temperatures span far more.

## Choice 2 — temperate Central Europe only

Cooling magnitudes vary strongly with climate. Schwaab et al. measured urban
trees at 8-12 K below urban fabric in Central Europe but only 0-4 K in Southern
Europe, and treeless green space in Southern Europe is sometimes *warmer* than
urban fabric. Pooling those would be meaningless here.

Every row carries a `climate_transfer` column:

- `temperate` — the source is Cfb/Dfb and its magnitude is used directly.
- `shape_only` — the source is outside the zone. Its **ordering, ratios and curve
  shape** are used; its **magnitudes are not**.

Switzerland sits in Schwaab's "Alps/Mid-Europe" region, so that paper's Central
European figures apply directly rather than by analogy.

### A correction worth recording

Speak et al. 2020 is **Sydney, Australia** (Cfa), not Bolzano as the planning
notes had it, and it is in *Forests* 11:1141, not *Landscape & Urban Planning*.
It is the single best per-material shade dataset available, so it is retained —
but flagged `shape_only`, and no Sydney magnitude enters the tables.

## Choice 3 — the radiative and advective terms are separate tables

They are different physical quantities and must never be summed into one number.

- **Local (0 m), `heat_materials.csv`.** Radiant load on the body: shade and
  surface temperature. Large — tree shade moves PET by many K.
- **At distance, `heat_decay.csv`.** Advection of cooled air. An order of
  magnitude smaller. Meeussen measured a −2.8 ± 0.8 °C summer mean air offset
  inside European forests; Hathway & Sharples found river cooling above 1.5 °C in
  spring and *less in summer*.

The honest headline: **in temperate Europe the 150 m column is near zero for
almost every class.** The largest `pet_150m_K` in the table is 0.21 K.

## Choice 4 — decay is stored as parameters, not samples

Sampling an exponential at fixed distances throws information away. Stored per
class: `amp_edge_K`, `half_dist_m`, `max_extent_m`, `min_patch_ha`. The sampled
columns are **derived** and regenerated, never hand-edited; verification step 4
asserts they still agree. They are *not* simply `amp_edge_K · 2^(−d/half_dist_m)`
— see "the accumulation the first version missed" below for why a sampled column
is that curve integrated over a half-plane.

### Where the sample distances come from

Fixed 10 m / 100 m samples miss both ends: half-distances range from ~3 m (tree
shade) to ~130 m (park, forest), a factor of ~40. Each sampled distance resolves
a named mechanism:

| Distance | Mechanism | Temperate evidence |
| --- | --- | --- |
| 0 m | radiant / contact | the dominant term |
| 5 m | shadow penumbra, altered sky view factor | shade-edge measurements |
| 25 m | steepest advective gradient; temperate river limit | Hathway & Sharples |
| 75 m | forest-edge gradient; small-patch reach | Meeussen; 0.8-3.8 ha patches cool over 30-120 m |
| 150 m | at and past the fitted half-distance | half-range 112 m summer |

A 400 m sample was dropped: in temperate Europe it is ~0 for everything but large
forest and water. `max_extent_m` still records the outer limit, so nothing is
lost from the parameters.

Meeussen's own plots sat at 1.5 / 4.5 / 12.5 / 36.5 / 99.5 m — chosen on an
exponential curve for the same reason. That is independent support for log
spacing over round numbers.

## Choice 5 — shade is a per-material state, not an additive constant

The old model added a flat `-0.8` for tree canopy regardless of what lay beneath.
Speak et al. measured shade reduction *per surface* and found the ordering is
real but, more importantly, that **shade collapses the differences between
materials**:

| | sunlit spread | shaded spread |
| --- | --- | --- |
| Globe temperature (5 materials) | 12.3 K | 4.6 K |

Shaded asphalt and shaded grass end up close together. A single additive constant
cannot express that; a `(material, shaded)` row can. The same collapse appears in
the tables — verification step 5 asserts it holds in every time bin (midday:
10.5 K sunlit, 4.0 K shaded).

## Choice 6 — three time bins, because thermal inertia reorders the materials

Asphalt peaks 1-2 h after solar noon and holds heat; grass and moist soil peak
lower and earlier because energy goes to evapotranspiration; water barely moves.
So the asphalt-grass gap is modest in the morning and maximal mid-afternoon —
6.5 K vs 10.0 K in these tables. Verification step 6 asserts both the direction
and the widening.

Water is the most thermally stable surface in the scene by a wide margin, and
against a fixed anchor it stays nearly flat (0.3 K across all three bins). The
widening gap to asphalt is carried by asphalt rising, not by water falling.

### The morning/afternoon difference is thermal, not geometric

Computed rather than assumed: at 47°N in mid-July, solar times 09:00 and 15:00
are symmetric about solar noon, so the sun elevation is **identical** in both
(45.8°) and only the azimuth flips (109.4° vs 250.6°). Shadow *length* is the
same morning and afternoon; only its *direction* changes.

Everything that makes an afternoon hotter than a morning at the same sun angle is
therefore material thermal inertia — which is exactly what `heat_materials.csv`
carries.

## The conversion chain, and its weak link

Most `pet_0m_K` values pass through two conversions. Both are stated openly
because this is where the error lives.

| Step | Rule | Basis |
| --- | --- | --- |
| surface -> radiant load | `dTglobe = 0.33 x dTsurface` | **Measured**, Speak et al.: the ratio runs 0.17 (grass) to 0.45 (bitumen), mean 0.33 |
| radiant load -> PET | `dPET = 0.5 x dTmrt` | **Assumed.** PET responds to Tmrt with a slope below 1 because air temperature, wind and humidity also enter the heat balance |

The first rule replaced a planning assumption of 0.4-0.5 once the data were
actually checked — the measured mean is 0.33, and the spread across materials is
wide enough that a single ratio is itself an approximation.

## Sign convention

**Unshaded short grass at midday = 0 K.** Best-measured reference surface, the
natural neutral for a landscape tool, and it preserves "higher = hotter" so the
existing symmetric-about-zero palette
([R/heat_helpers.R:137-143](R/heat_helpers.R#L137-L143)) still reads correctly.

## What moves, against today's constants

Midday, sunlit, compared with the current `HEAT_GROUND`:

| class | old (unitless) | new dPET (K) |
| --- | --- | --- |
| grass | 0.3 | 0.0 |
| bush | 0.2 | −1.5 |
| artificial | 0.7 | 9.0 |
| natural | 0.5 | 4.0 |
| water | 0.0 | −1.0 |
| artificial_block | **NA** | 7.0 |

The old values were unitless and spanned 0.7; the new ones are in K of PET and
span 10.5. Two classes had no value at all before: ground 8 (buildings) and
ground 0, both of which fell out of the window means as `NA`. Ground 8 now has a
value. Ground 0 stays out, because "unclassified" genuinely means the sources say
nothing.

That last decision was reasonable only while ground 0 was rare, and it was not:
21–25 % of Switzerland carried no class, almost all of it farmland that
swissTLM3D omits by design and OpenStreetMap had not drawn. The fix belongs in
the land-cover build rather than in these tables, and is now there — step 9 of
[data-raw/generate_ground_canopy_CH.r](data-raw/generate_ground_canopy_CH.r)
backfills from ESA WorldCover. Ground 0 still means "nothing is known", but it
now applies to a residue rather than to a fifth of the country.

## What these tables cannot support

Also on the `limitations` sheet of the workbook.

1. **Not a validated model.** Sourced, not verified against local measurement.
2. **Two stacked conversions**, whose errors multiply.
3. **Buildings are weakly sourced.** Class 8 is inferred from SOLWEIG's
   sunlit-facade finding, not measured. Low confidence.
4. **Bush has no dedicated study** and is interpolated between grass and closed
   canopy.
5. **Water is the weakest row.** Summer river cooling in temperate cities is
   small and strongly bank-form dependent.
6. **The forest distance evidence points the wrong way.** Meeussen measures
   edge-to-*interior* gradients — how cooling builds up *inside* a forest — not
   how far it projects *outward* into open land. `half_dist_m` for `canopy_tree`
   borrows from park-cooling studies instead.
7. **`min_patch_ha` is contested.** The Munich study found structural complexity
   can outweigh size, and that low-complexity parks were sometimes *warmer* than
   their surroundings. A pure area threshold is an approximation.
8. **Midsummer, low wind, clear sky only.** No wind, no humidity, no other season.
9. **`max_extent_m` truncates rather than tapers.** The curve is cut to zero while
   still carrying 8-12 % of its edge amplitude (`canopy_tree`: −0.12 K at 200 m).
   Applied to a raster as written, that step will draw a **visible ring artefact**
   at exactly that radius around every patch. Phase 4 must taper the last stretch,
   or set `max_extent_m` at 5+ half-distances, rather than truncating.

## Phase 2 — cast shadows, built

[R/shadow_helpers.R](R/shadow_helpers.R). Ratti & Richens (2004), the same
marching algorithm SOLWEIG uses: step the obstruction-height field toward the
sun one cell at a time, lower the ray by `step · tan(elevation)` each step, keep
the running maximum. Obstruction heights are the fixed per-class values in
`heat_geometry.csv` (tree 15 m, building 12 m, artificial canopy 4 m), so this
needs **no height raster and no new data dependency**.

**What it changes.** Shade stops falling straight down. Over central Sion at
5 m, the shaded fraction goes from **10.8 %** — the canopy's own footprint, all
the previous model could express — to **43.5 % at midday and 53 % morning and
afternoon**. That is the single largest correction in the whole rework, and it
lands on the term Phase 1 showed matters most: shading collapses the spread
between materials from 12.3 K sunlit to 4.6 K shaded.

Verified against the geometry the tables already state — every shadow reaches
`height / tan(elevation)` to within one cell, points within 0.3° of directly
away from the sun, is shortest at midday, and is symmetric between morning and
afternoon. Flat ground casts nothing; a sun below the horizon shades everything.

Two limits are structural and deliberate:

- **No height raster**, so every tree is 15 m. Shadow *lengths* are right on
  average and wrong per object. Retune in `heat_geometry.csv`, not in the code.
- **No terrain.** The height field carries canopy and buildings only, so a valley
  in its own mountain's shadow is invisible — which matters in exactly the alpine
  settings this tool is often pointed at. A DEM would enter at Phase 3's horizon
  scan.

Nothing consumes the shade raster yet; that is Phase 4.

## Phase 3 — sky view factor and the sunlit wall, built

[R/svf_helpers.R](R/svf_helpers.R), verified by
[data-raw/verify_svf.R](data-raw/verify_svf.R).

**SVF** reuses the Phase 2 march over 16 azimuths to get a horizon angle per
direction, then `SVF ≈ 1 − mean(sin²(horizon))`. SOLWEIG integrates 153
directions; 16 is the main accuracy sacrifice of the whole rework, and it is an
argument, so raising it costs time and nothing else. Verified against closed form
on a circular wall of known height and radius, where `SVF = 1 / (1 + (h/d)²)`, to
within 0.03 across four geometries. Over central Sion, ground-level SVF runs
0.55 at 0–5 m from an obstruction, through 0.73, 0.86, 0.95, to 0.98 beyond 40 m.

One property surprises every reader of a summary table: **a cell that is itself
an obstruction comes out at SVF ≈ 1.** Horizon angles are measured relative to
the cell's own height, so a roof correctly sees the whole sky. It is harmless —
the model reads ground-level cells — but an unmasked average over a town centre
reports the rooftops rather than the streets, and the first draft of the
verification failed for exactly that reason.

**The wall term** is a proximity flag, not SOLWEIG's wall-temperature scheme: a
cell qualifies when it is sunlit, carries no obstruction of its own, and stands
within `wall_bonus_distance_m` of something taller in the downsun direction. That
reproduces SOLWEIG's headline finding — the hottest place in a city is in front
of a sunlit facade — without any of its physics. It covers 4.8–7.7 % of cells
depending on the bin, and the band correctly swaps sides of the buildings between
morning and afternoon.

### The correction these two forced

`svf_coefficient` is 20 K of Tmrt per unit SVF, and applying it as written **broke
the model**. Most of that 20 K is the direct beam being cut off — which the
shaded rows of `heat_materials.csv` already price, because shade there selects a
different row rather than adding a constant. Charging a sunlit cell the full
sensitivity bills it for shading it has not received. Measured over central Sion,
the SVF term alone then reached −8.1 K and had a mean of −1.32 K, making the
least-trusted parameter in the model its largest single term.

What a sunlit cell actually loses as its sky closes is the **diffuse** share of
the sky load, ~0.15–0.25 of global shortwave on a clear summer day. A new
parameter `svf_sunlit_fraction` = 0.20 carries that, leaving the reviewed
`svf_coefficient` untouched. The SVF term then has mean −0.26 K and a floor of
−1.62 K: a correction, which is what it should be. **This is the single most
uncertain number in the tables** — attack it first if the geometry looks wrong.

## Phase 4 — the model rewrite, built

[R/heat_helpers.R](R/heat_helpers.R), verified by
[data-raw/verify_heat_model.R](data-raw/verify_heat_model.R). Per cell, per bin:

```
PET = local(material, shaded, bin)     # heat_materials.csv
    + svf(SVF)                         # sunlit cells only
    + wall(sunlit facade within 5 m)
    + advective(distance to patches)   # heat_decay.csv
```

Output is **kelvin of PET relative to unshaded short grass at solar midday**.
All five structural defects are fixed:

1. **The cell's own material now enters directly** — it is the first term, not an
   artefact of two focal means.
2. **The output carries a unit.** Kelvin, not the old unitless ±0.09.
3. **Exponential decay, per class**, replacing one shared box-car that weighted a
   cell at 1 m the same as one at 99 m — and applied as an accumulating sum, so a
   large forest outweighs a small copse.
4. **Patch size is tested** with `terra::patches()` — a connected-component area
   computation, which no focal operation can express.
5. **Water is a cool source at distance**, no longer neutral.

Plus the time-of-day control beside `heatSwitch`, which drops the cached surface
on change exactly as a brush stroke does.

**The strongest evidence it is right:** on cells where nothing but the material
acts — sunlit, open sky, no wall, no advective source in range — the model
reproduces the table to the second decimal, in all three bins
(grass −1.00/0.00/+0.50, asphalt +5.49/+8.99/+10.49 against table values of
−1.00/0.00/+0.50 and +5.50/+9.00/+10.50).

The term budget over central Sion at midday, which is the right shape — the local
radiative term dominates, geometry corrects it, advection is a whisper:

| term | median | mean | min | max |
| --- | --- | --- | --- | --- |
| local | +0.00 | +3.00 | −4.00 | +9.00 |
| svf | −0.09 | −0.26 | −1.62 | 0.00 |
| wall | 0.00 | +0.15 | 0.00 | +2.00 |
| advective | −0.30 | −0.35 | −1.44 | +1.23 |
| **total** | **+0.85** | **+3.31** | **−5.20** | **+12.00** |

### Two things that look like bugs and are not

**The median is not monotonic across the day** (+0.6 morning, +0.3 midday, +1.1
afternoon) while the mean is (+1.28, +2.81, +3.15). The shaded share of the map
changes with sun elevation — 50 % morning, 40 % midday, 50 % afternoon — so the
median lands on a different material-and-state combination in each bin. At midday
the median cell is sunlit grass, which is the reference surface and therefore
exactly 0.00 by construction. Every individual material really is hotter later in
the day; the verification checks that cell by cell.

**The ring artefact is gone.** Limitation 9 warned that cutting
`amp · 2^(−d/half)` at `max_extent_m` leaves 8–12 % of the amplitude standing and
draws a visible ring around every patch. The kernel is now shifted and
renormalised, `(2^(−d/half) − f₀)/(1 − f₀)` with `f₀ = 2^(−max/half)`, so it
reaches the cut-off at exactly zero.

## The accumulation the first version missed

The first Phase 4 build computed the advective term from the **distance to the
nearest** qualifying source cell. That is wrong, and measurably so: a **0.25 ha
copse and a 64 ha forest produced bit-identical fields** — zero difference across
a 256-fold range of area. Patch size entered only as the binary `min_patch_ha`
gate and was then discarded, so every wood over 0.2 ha projected the cooling of a
large park. For a tool whose purpose is comparing designs, that is a bad failure:
it says scattering a few small clumps is worth as much as building a park.

The fix is to sum over **every** qualifying source cell, weighted by distance,
rather than looking only at the nearest. No new parameter is involved — the size
dependence falls out of geometry, because a bigger wood simply has more of itself
inside the kernel:

| wood | area | response 25 m from the edge | share of a large forest |
| --- | --- | --- | --- |
| 50 m | 0.25 ha | −0.152 K | 17 % |
| 100 m | 1 ha | −0.419 K | 46 % |
| 200 m | 4 ha | −0.769 K | 84 % |
| 400 m | 16 ha | −0.912 K | 100 % |
| 800 m | 64 ha | −0.912 K | 100 %, saturated |

Saturation at ~16 ha is correct rather than a defect: once a patch is wider than
`max_extent_m` in every direction, more of it lies beyond the kernel and cannot
contribute. It also accumulates over *direction*, which distance-to-nearest
cannot express at all — a clearing ringed by trees comes out at −0.11 K where a
spot the same distance from one straight forest edge gets −0.03 K.

**`amp_edge_K` keeps exactly the meaning the literature gave it** because of how
the sum is normalised: divide by the kernel's sum over a **half-plane**, which is
the configuration a cell sees at the long straight edge of a large wood — which
is the configuration the field studies measured. A straight edge therefore
returns `amp_edge_K` on the nose, and everything smaller returns less.

The consequence for the table is that a sampled column is no longer a point on
the radial curve. It is that curve integrated over the half-plane:

```
numerator(d) = ∫_d^R w(r) · 2r·acos(d/r) dr
denominator  = ∫_0^R w(r) · π·r dr
```

The same Riemann sum lives in R, Python and the verification. The model applies a
*discrete* convolution on a finite grid, which cannot match a continuous integral
exactly; the gap is bounded and printed rather than assumed small — worst case
0.060 K for `canopy_tree`, against parameters whose own confidence is "low" to
"medium".

**Cost.** The convolution runs on a 10 m grid while patches are identified at the
full 5 m grid, so no narrow avenue of trees is lost and no patch area is
misjudged; only the smooth field is coarsened, using a *fractional cover* mask
that preserves total source area. Measured over central Sion that is 4.6× faster
than convolving at 5 m for a maximum error of 0.076 K. A full three-bin read-out
stays at about 1.7 s for a 1.4 × 1.0 km area.

**What it changed in practice.** Central Sion at midday went from a mean of
+2.77 K to **+3.31 K**: the old formulation was systematically over-optimistic,
crediting every scattered clump with a full park's cooling.

### And it exposed something the old form was hiding

Summing accumulates *inside* a wood as well as outside it. The distance transform
excluded source cells, so a forest interior received exactly zero from the
advective term; the sum gives it the full plane of surrounding source instead of
the half-plane an edge sees, so the interior saturates at **2.06 × `amp_edge_K`**
(−2.47 K for `canopy_tree`). The transect that falls out:

| position | −200 m | −100 m | −25 m | edge | +25 m | +100 m | +200 m |
| --- | --- | --- | --- | --- | --- | --- | --- |
| K of PET | −2.47 | −2.29 | −1.63 | −1.27 | −0.91 | −0.21 | 0.00 |

The magnitude is defensible — Meeussen measured a −2.8 °C summer mean *air*
offset inside European forests, and PET responds more strongly than air
temperature, so −2.5 K PET is on the conservative side.

**The gradient is not.** This reaches within 5 % of the interior value only
115 m inside the edge, where Meeussen's measured edge influence is 12.5–36.5 m.
One length scale, `half_dist_m`, now governs both how far cooling projects
*outward* (park studies: 30–120 m) and how fast it builds up *inward*
(Meeussen: 12.5–36.5 m), and the literature says those are not the same number.
Limitation 6 already flagged that `half_dist_m` borrows from the park-cooling
direction; what is new is that the accumulating form makes the borrowed value
visible in the other direction too, where the distance transform simply returned
zero and hid it. Splitting the two would need an inward length scale that no
source in this set supports, so it is recorded rather than invented.

## What is still not built

- **No terrain anywhere.** The height field carries canopy and buildings only, so
  an alpine valley in its own mountain's shadow is invisible. This matters for
  where this tool actually gets pointed, and a DEM would enter at the horizon
  scan.
- **No wind, no humidity, no season but midsummer.**
- **Still not validated against local measurement.** Every number is sourced;
  none is verified against a thermometer in a Swiss town.
