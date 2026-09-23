#!/usr/bin/env python3
"""Build the three heat-coefficient CSVs for visitorFlowTool.

Writes semicolon-separated, decimal-comma CSVs (R read.csv2 convention) into the
app's external tables directory. Values and provenance are documented in
HEAT_COEFFICIENTS.md; every numeric row carries its own reference.

Generated here in Python on purpose: verification runs in R via read.csv2(), so
the check is independent of the toolchain that produced the files.
"""
import csv, math, os, sys

OUT = os.environ.get(
    "VFT_TABLES",
    "C:/Users/frueh/OneDrive - Eidg. Forschungsanstalt WSL/Dokumente/"
    "visitorFlowTool_DATA/data/tables",
)

# --------------------------------------------------------------- references --
# Short keys used in the `reference` column; full citations in references.csv.
R_SCHWAAB = "Schwaab et al. 2021"
R_SPEAK   = "Speak et al. 2020"
R_MEEUS   = "Meeussen et al. 2021"
R_HATH    = "Hathway & Sharples 2012"
R_MUNICH  = "Muenchen 36 parks 2025"
R_RAHMAN  = "Rahman et al. 2020"
R_ARMSON  = "Armson et al. 2012"
R_SOLWEIG = "Lindberg et al. SOLWEIG"
R_VDI     = "VDI 3787 Blatt 2"
R_ASPHALT = "Asphalt diurnal literature"

# Climate-transfer flag: does the source's *magnitude* transfer to Cfb/Dfb?
OK, SHAPE = "temperate", "shape_only"

# ----------------------------------------------------------- 1a. materials --
# dPET in K relative to UNSHADED SHORT GRASS AT MIDDAY = 0.
# Ordering and the sun->shade collapse come from Speak (globe temperatures,
# shape only, Sydney); temperate magnitudes anchored on Schwaab, Meeussen,
# Hathway, Munich. Diurnal shape from the asphalt thermal-inertia literature.
# cols: (morning, midday, afternoon)
GROUND = [
    # id name              label_de              sun                shade              ref            transfer conf   note
    (1, "grass",            "Gras",              (-1.0, 0.0, 0.5),  (-2.5,-3.0,-3.0), R_SCHWAAB, OK,    "high",  "reference surface; sun/midday defines the zero point"),
    (2, "bush",             "Busch",             (-1.0,-1.5,-1.5),  (-3.0,-3.5,-3.5), R_MEEUS,   OK,    "medium","interpolated between grass and closed canopy; no dedicated shrub study"),
    (3, "artificial",       "Kuenstlich",        ( 5.5, 9.0,10.5),  ( 1.0, 0.5, 1.5), R_SCHWAAB, OK,    "high",  "asphalt/impervious; afternoon peak from thermal inertia, 1-2 h after solar noon"),
    (4, "natural",          "Natuerlich",        ( 2.5, 4.0, 4.5),  (-1.5,-2.0,-2.0), R_SPEAK,   SHAPE, "medium","bare soil; strongly moisture dependent, dry soil behaves closer to class 3"),
    # Water is the most thermally stable surface in the scene by a wide margin
    # (highest heat capacity), so against a fixed grass/midday anchor it stays
    # nearly flat. The widening gap to asphalt over the day is carried by
    # asphalt rising, not by water falling.
    (5, "water",            "Wasser",            (-0.8,-1.0,-1.1),  (-1.8,-2.0,-2.1), R_HATH,    OK,    "low",   "open water: cool surface but high sky view factor, so modest in PET terms"),
    (8, "artificial_block", "Kuenstlicher Block",( 4.0, 7.0, 8.5),  ( 0.5, 0.0, 1.0), R_SOLWEIG, SHAPE, "low",   "buildings: was NA in HEAT_GROUND; sunlit facade raises Tmrt, see heat_geometry"),
]

# Canopy classes carry the *additional* effect beyond simply shading the ground
# (i.e. transpiration), not the shading itself - shading is the `shaded` flag.
CANOPY = [
    (6, "canopy_artificial", "Kuenstlich",        ( 0.0, 0.0, 0.0), R_RAHMAN, OK, "medium","deck/bridge: shades but does not transpire, so no term beyond the shade flag"),
    (7, "canopy_tree",       "Baum",              (-0.5,-1.0,-1.0), R_RAHMAN, OK, "medium","transpirative cooling on top of shade; Rahman notes radiation trapping can offset it"),
    (9, "canopy_cleared",    "Geraeumt",          ( 0.0, 0.0, 0.0), R_SCHWAAB, OK,"high",  "plan import removed the canopy: identical to open sky by construction"),
]

BINS = ["morning", "midday", "afternoon"]

def dec(x, nd=1):
    """Format for read.csv2: decimal comma, no thousands separator.

    Small negatives that round to zero are emitted as plain "0,00" rather than
    "-0,00". R parses either as zero, so this is cosmetic - but a minus sign in
    front of a zero in a reviewed table reads as a claim about direction that
    the number does not support.
    """
    s = ("%%.%df" % nd) % x
    if float(s) == 0.0:
        s = ("%%.%df" % nd) % 0.0
    return s.replace(".", ",")

def write_materials(path):
    rows = []
    for cid, name, de, sun, shade, ref, tr, conf, note in GROUND:
        for shaded, vals in ((0, sun), (1, shade)):
            for b, v in zip(BINS, vals):
                rows.append(dict(
                    class_id=cid, class_name=name, level="ground", label_de=de,
                    shaded=shaded, time_bin=b,
                    pet_0m_K=dec(v), tmrt_0m_K=dec(v * 2.0),
                    confidence=conf, climate_transfer=tr, reference=ref, note=note))
    for cid, name, de, vals, ref, tr, conf, note in CANOPY:
        for b, v in zip(BINS, vals):
            rows.append(dict(
                class_id=cid, class_name=name, level="canopy", label_de=de,
                shaded="NA", time_bin=b,
                pet_0m_K=dec(v), tmrt_0m_K=dec(v * 2.0),
                confidence=conf, climate_transfer=tr, reference=ref, note=note))
    cols = ["class_id","class_name","level","label_de","shaded","time_bin",
            "pet_0m_K","tmrt_0m_K","confidence","climate_transfer","reference","note"]
    dump(path, cols, rows)
    return rows

# --------------------------------------------------------------- 1b. decay --
# amp_edge_K  = dPET contributed at the patch edge
# half_dist_m = exponential half-range
# max_extent_m= beyond this the effect is treated as zero
# min_patch_ha= below this patch size the remote term does not apply at all
DECAY = [
    (1, "grass",            -0.3, 40,  120, 0.5, R_MUNICH,  OK,    "medium","open lawn: Munich found low-complexity parks can even be warmer than surroundings"),
    (2, "bush",             -0.4, 50,  150, 0.5, R_MUNICH,  OK,    "low",   "scaled between grass and canopy by structural complexity"),
    (3, "artificial",        0.6, 35,  100, 0.5, R_SCHWAAB, OK,    "low",   "warm-source term; far less studied than the cooling direction"),
    (4, "natural",           0.1, 30,   90, 0.5, R_SPEAK,   SHAPE, "low",   "near-neutral at distance; sign flips with soil moisture"),
    (5, "water",            -0.8, 25,   80, 0.2, R_HATH,    OK,    "medium","Sheffield: cooling concentrated near the bank and strongly modulated by urban form"),
    (6, "canopy_artificial", 0.0, "",   "",  "",  R_RAHMAN, OK,    "high",  "no advective term: a deck cools nothing downwind"),
    (7, "canopy_tree",      -1.2, 60,  200, 0.2, R_MEEUS,   OK,    "medium","strongest cool source; Meeussen edge influence 12.5-36.5 m, park studies 30-120 m"),
    (8, "artificial_block",  0.5, 30,   90, 0.5, R_SCHWAAB, OK,    "low",   "built mass as a warm source"),
    (9, "canopy_cleared",    0.0, "",   "",  "",  R_SCHWAAB, OK,   "high",  "open sky by construction"),
]
SAMPLES = [5, 25, 75, 150]

def kernel_weight(r, half, mx):
    """The radial kernel shape: 1 at the source, 0 at max_extent_m.

    Not a plain 2**(-r/half). That curve is still carrying 8-12 % of its
    amplitude at max_extent_m (canopy_tree: -0.12 K of -1.2 K at 200 m), and
    cutting it there draws a visible ring into the raster at exactly that radius
    around every patch - limitation 9 in HEAT_COEFFICIENTS.md.
    """
    if r > mx:
        return 0.0
    f0 = 2 ** (-mx / half)
    return (2 ** (-r / half) - f0) / (1 - f0)

def decay_at(d, amp, half, mx, n=20000):
    """PET contribution at distance d from the straight edge of a LARGE patch.

    Phase 4 applies the decay as a distance-weighted sum over every qualifying
    source cell, not as a function of the distance to the nearest one. That is
    what makes a big forest cool more than a small copse - with
    distance-to-nearest they came out bit-identical, which is the defect this
    replaced. The consequence for this table is that a sampled column is no
    longer a point on the radial curve; it is the curve integrated over the
    half-plane of source that a straight edge presents:

        numerator(d) = int_d^R w(r) * 2r*acos(d/r) dr
        denominator  = int_0^R w(r) * pi*r dr

    Normalising by the half-plane is what keeps amp_edge_K meaning what the
    literature measured: a long straight forest edge returns exactly amp_edge_K.

    A fixed Riemann sum rather than adaptive quadrature, because acos(d/r) has an
    infinite derivative at r = d. heat_decay_halfplane() in R/heat_helpers.R is
    the same sum with the same n, so the two agree to the printed decimal.
    """
    if half == "" or mx == "" or d >= mx:
        return 0.0
    h = mx / float(n)
    den = sum(kernel_weight((i + 0.5) * h, half, mx) * math.pi * ((i + 0.5) * h)
              for i in range(n)) * h
    h2 = (mx - d) / float(n)
    num = 0.0
    for i in range(n):
        r = d + (i + 0.5) * h2
        num += kernel_weight(r, half, mx) * 2.0 * r * math.acos(min(1.0, d / r))
    num *= h2
    return amp * num / den

def write_decay(path):
    rows = []
    for cid, name, amp, half, mx, mp, ref, tr, conf, note in DECAY:
        r = dict(class_id=cid, class_name=name, amp_edge_K=dec(amp),
                 half_dist_m=half, max_extent_m=mx, min_patch_ha=(dec(mp) if mp != "" else ""))
        for d in SAMPLES:
            v = decay_at(d, amp, half, mx)
            # two decimals: these are small numbers, and rounding to one would
            # put the derived column further from its own parameters than the
            # verification tolerance allows.
            r["pet_%dm_K" % d] = dec(v, 2)
        r.update(confidence=conf, climate_transfer=tr, reference=ref, note=note)
        rows.append(r)
    cols = (["class_id","class_name","amp_edge_K","half_dist_m","max_extent_m","min_patch_ha"]
            + ["pet_%dm_K" % d for d in SAMPLES]
            + ["confidence","climate_transfer","reference","note"])
    dump(path, cols, rows)
    return rows

# ------------------------------------------------------------ 1c. geometry --
def sun_position(lat_deg, decl_deg, hour_angle_deg):
    """Solar elevation and azimuth (deg, azimuth from north, clockwise)."""
    la, de, ha = map(math.radians, (lat_deg, decl_deg, hour_angle_deg))
    el = math.asin(math.sin(la) * math.sin(de) + math.cos(la) * math.cos(de) * math.cos(ha))
    az = math.atan2(-math.sin(ha) * math.cos(de),
                    math.cos(la) * math.sin(de) - math.sin(la) * math.cos(de) * math.cos(ha))
    return math.degrees(el), (math.degrees(az) + 360) % 360

LAT, DECL = 47.0, 21.5          # 47 deg N, mid-July declination
# Obstruction height per class id, and the ONLY place these numbers are written
# down. R reads them back through heatHeights(), which takes its id list from
# PAINT_CATEGORIES (R/paintbrush_helpers.R) and looks up one height_<name> row
# here per id - so this list and that table have to name the same thirteen
# classes, and verify_heat_model.R asserts they do.
#
# Since the height bar there are three RAMPS rather than three scalars: the user
# picks a step and the class id carries it. Each material keeps its original id
# as the default step, which is what the national land cover writes and what a
# version saved before the bar replays as - so ids 6, 7 and 8 are the surveyed
# heights and must stay in this list even though they now sit inside a ramp.
#
# 6 and 8 moved when the ramps were fixed: an artificial canopy was 4 m and is
# now 5 m, a block was 12 m and is now 10 m, both snapped onto the nearest step
# the bar offers so that no height exists anywhere except the ones a user can
# choose. That moves every surveyed building and bridge in the country by a
# couple of metres of shadow, which is why the stored Sion regression baseline
# has to be regenerated alongside this file.
#
# 19 was added later, when swissBUILDINGS3D gave every building in the country a
# real height: the median Swiss building measures 13.6 m, which is almost exactly
# the midpoint of the 10-25 m gap the ramp had, so a third of all buildings were
# taking the larger rounding error. It is a ramp STEP and not a new material -
# heat_materials.csv is untouched by it.
HEIGHTS = [
    ("canopy_tree_3",         10,  3.0),
    ("canopy_tree_10",        11, 10.0),
    ("canopy_tree",            7, 15.0),   # default step of the tree ramp
    ("canopy_tree_20",        12, 20.0),
    ("canopy_tree_25",        13, 25.0),
    ("canopy_artificial",      6,  5.0),   # default step, was 4.0
    ("canopy_artificial_10",  14, 10.0),
    ("canopy_artificial_15",  15, 15.0),
    ("artificial_block_5",    16,  5.0),
    ("artificial_block",       8, 10.0),   # default step, was 12.0
    ("artificial_block_15",   19, 15.0),
    ("artificial_block_25",   17, 25.0),
    ("artificial_block_50",   18, 50.0),
]

def write_geometry(path):
    rows = []
    def add(p, v, u, ref, conf, note):
        rows.append(dict(parameter=p, value=v, unit=u, reference=ref, confidence=conf, note=note))
    add("reference_latitude", dec(LAT), "deg N", R_VDI, "high", "Swiss plateau")
    add("reference_declination", dec(DECL), "deg", R_VDI, "high", "mid-July, the hot-day reference")
    for b, ha in zip(BINS, (-45.0, 0.0, 45.0)):
        el, az = sun_position(LAT, DECL, ha)
        add("sun_elevation_%s" % b, dec(el), "deg", R_VDI, "high",
            "solar time %02d:00, hour angle %+g deg; morning and afternoon are "
            "symmetric about solar noon so elevation is identical and only the "
            "azimuth flips" % (12 + ha / 15, ha))
        add("sun_azimuth_%s" % b, dec(az), "deg from N", R_VDI, "high", "clockwise from north")
    for name, cid, h in HEIGHTS:
        add("height_%s" % name, dec(h), "m", R_MEEUS, "medium",
            "class %d obstruction height; no height raster is read - the class id "
            "carries the height, see PAINT_CATEGORIES" % cid)
    add("svf_coefficient", dec(20.0), "K Tmrt per unit SVF", R_SOLWEIG, "low",
        "Tmrt sensitivity to sky view factor, beam blocking included; Phase 3")
    # Without this, Phase 4 double-counts shade. svf_coefficient is the FULL
    # sensitivity, and most of it is the direct beam being cut off - which the
    # shaded rows of heat_materials.csv already price, because shade there
    # selects a different row rather than adding a constant. Applying the full
    # 20 K/unit to a cell that still has its beam therefore charges it for
    # shading it has not received: measured over central Sion it dragged the
    # median down by 0.25 K and the floor to -9.3 K, making the least-trusted
    # parameter in the model the largest single term in it.
    #
    # What a sunlit cell actually loses as its sky view closes is the DIFFUSE
    # share of the sky load. On a clear summer day in temperate Europe diffuse is
    # ~0.15-0.25 of global shortwave; 0.20 is the clear-sky reference value. This
    # is the most uncertain number in the whole table - it is the one to attack
    # first if the geometry terms look wrong.
    add("svf_sunlit_fraction", dec(0.20, 2), "fraction", R_SOLWEIG, "low",
        "share of svf_coefficient that still applies when the direct beam is "
        "unobstructed; the rest is beam blocking, already priced by the shaded rows")
    add("wall_bonus_K", dec(4.0), "K Tmrt", R_SOLWEIG, "low",
        "in front of a sunlit facade; SOLWEIG reports the city maximum Tmrt there")
    add("wall_bonus_distance_m", 5, "m", R_SOLWEIG, "low", "distance over which the facade bonus applies")
    add("tmrt_to_pet_slope", dec(0.5), "K PET per K Tmrt", R_VDI, "medium",
        "single linear conversion; see HEAT_COEFFICIENTS.md conversion section")
    add("globe_to_surface_ratio", dec(0.33, 2), "ratio", R_SPEAK, "medium",
        "measured dTglobe/dTsurface, range 0.17-0.45 across five materials")
    for name, cid, h in HEIGHTS:
        for b, ha in zip(BINS, (-45.0, 0.0, 45.0)):
            el, _ = sun_position(LAT, DECL, ha)
            add("shadow_length_%s_%s" % (name, b), dec(h / math.tan(math.radians(el))), "m",
                R_SOLWEIG, "high", "derived: height / tan(sun elevation)")
    cols = ["parameter","value","unit","reference","confidence","note"]
    dump(path, cols, rows)
    return rows

# ------------------------------------------------------------- references ---
REFS = [
    (R_SCHWAAB, "Schwaab J, Meier R, Mussetti G, Seneviratne S, Buergi C, Davin EL (2021) The role of urban trees in reducing land surface temperatures in European cities. Nature Communications 12:6763",
     "10.1038/s41467-021-26768-w", "293 European cities", "Cfb/Dfb + Csa", "satellite LST (Landsat)", "LST", "summer (JJA)",
     "120285 scenes", OK, "Trees vs continuous urban fabric: -12 to -8 K in Central Europe (France, Alps/Mid-Europe, British Isles, Eastern Europe); -4 to 0 K Southern Europe. Treeless green space 2-4x less effective. Daytime overpass ~10:30 local, so these are late-morning values."),
    (R_SPEAK, "Speak A, Montagnani L, Wellstein C, Zerbe S (2020) Temperature Reduction in Urban Surface Materials through Tree Shading Depends on Surface Type Not Tree Species. Forests 11(11):1141",
     "10.3390/f11111141", "Greater Sydney, Australia", "Cfa", "field, FLIR C3 IR camera", "surface + globe temperature", "austral summer 2018-2019, noon-15:00",
     "471 trees, 13 species", SHAPE, "dTs: bark mulch -24.8+-7.1, bare soil -22.1+-5.6, bitumen -20.9+-5.8, grass -18.5+-4.8, concrete pavers -17.5+-6.0. dTglobe: mulch -10.9, bitumen -9.5, soil -7.2, pavers -4.3, grass -3.2. Sunlit globe spread 12.3 K vs shaded 4.6 K = the collapse. NOT temperate: magnitudes not transferred, ordering and ratios are."),
    (R_MEEUS, "Meeussen C, Govaert S, Vanneste T, et al. (2021) Microclimatic edge-to-interior gradients of European deciduous forests. Agricultural and Forest Meteorology 311:108699",
     "10.1016/j.agrformet.2021.108699", "European deciduous forests", "Cfb/Dfb", "field loggers, 45 transects", "air + soil temperature", "summer and winter 2018-2020",
     "225 plots", OK, "Summer mean air offset vs open reference -2.8+-0.8 C; summer maxima 8.3+-3.1 C cooler inside. Plots at 1.5/4.5/12.5/36.5/99.5 m. Depth of edge influence (air) between 12.5 and 36.5 m. Recommends >=12.5 m buffer."),
    (R_HATH, "Hathway EA, Sharples S (2012) The interaction of rivers and urban form in mitigating the Urban Heat Island effect: A UK case study. Building and Environment 58:14-22",
     "10.1016/j.buildenv.2012.06.013", "Sheffield, UK", "Cfb", "field survey", "air temperature", "spring and summer",
     "one river reach", OK, "Mean daytime cooling over 1.5 C above the river in spring, REDUCED in summer when river water was warmer. Bank urban form governs how far cooling is felt. The temperate corrective to 300-600 m river claims."),
    (R_MUNICH, "Seasonal and diurnal cooling potential of urban green spaces in Munich, Germany, moderated by size and vegetation complexity (2025) Urban Forestry & Urban Greening 128934",
     "10.1016/j.ufug.2025.128934", "Munich, Germany", "Cfb", "field, 36 parks + mobile laser scanning", "air temperature", "hot days",
     "36 parks, 0.25-375 ha", OK, "Cooling up to 3.3 C in structurally complex parks; low-complexity parks partially WARMER than surroundings. +23 percentage points canopy cover = about -1 C. Structure can compensate for size."),
    (R_RAHMAN, "Rahman MA, et al. (2020) Tree effects on urban microclimate: diurnal, seasonal and climatic temperature differences explained by separating radiation, evapotranspiration and roughness effects. Urban Forestry & Urban Greening 126970",
     "10.1016/j.ufug.2020.126970", "Zurich (+ Phoenix, Singapore, Melbourne)", "Cfb for Zurich", "mechanistic model", "air + surface temperature", "summer, diurnal cycle",
     "4 cities", OK, "Evapotranspiration of well-watered trees alone lowers 2 m air temperature by at most 3.1-5.8 C. A non-transpiring tree interacting with radiation can RAISE 2 m air temperature by 1.6-2.1 C, partially offsetting. Basis for the small canopy_tree term."),
    (R_ARMSON, "Armson D, Stringer P, Ennos AR (2012) The effect of tree shade and grass on surface and globe temperatures in an urban area. Urban Forestry & Urban Greening 11(3):245-255",
     "10.1016/j.ufug.2012.05.002", "Manchester, UK", "Cfb", "field", "surface + globe temperature", "June-July 2009, 2010",
     "plots + park", OK, "NOT OPEN ACCESS - values taken from the abstract only, not verified against the full text. Grass reduced maximum surface temperatures by up to 24 C, tree shade by up to 19 C; surface composition had little effect on globe temperature whereas shading reduced it by 5-7 C. Treat as medium confidence until the full text is checked."),
    (R_SOLWEIG, "Lindberg F, Holmer B, Thorsson S (2008) SOLWEIG 1.0 - modelling spatial variations of 3D radiant fluxes and mean radiant temperature in complex urban settings. Int J Biometeorol 52:697-713; Lindberg & Grimmond (2011) for SVF",
     "10.1007/s00484-008-0162-7", "Goteborg, Sweden", "Cfb", "model", "Tmrt", "-", "-", OK,
     "Source for the geometry parameters and for the finding that the highest urban Tmrt occurs in front of sunlit facades. Shadow casting after Ratti & Richens (2004)."),
    (R_VDI, "VDI 3787 Blatt 2: Umweltmeteorologie - Methoden zur human-biometeorologischen Bewertung von Klima und Lufthygiene fuer die Stadt- und Regionalplanung",
     "", "Germany (standard)", "-", "standard", "PET", "-", "-", OK,
     "Defines PET. The basis of Swiss and German Klimaanalyse practice, which is why PET rather than LST is the target variable."),
    (R_ASPHALT, "Asphalt pavement diurnal surface-temperature literature (multiple sources)",
     "", "various", "mixed", "field", "surface temperature", "summer", "-", SHAPE,
     "Asphalt surface temperature peaks 1-2 h after solar noon and cools faster than the air above it after about 18:00. Used for the SHAPE of the diurnal curve only, not for magnitudes."),
]

def write_references(path):
    cols = ["reference","citation","doi","location","koeppen","method","variable_measured",
            "season","sample","climate_transfer","key_values"]
    rows = [dict(zip(cols, r)) for r in REFS]
    dump(path, cols, rows)
    return rows

# -------------------------------------------------------------------- io ---
def dump(path, cols, rows):
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=cols, delimiter=";", lineterminator="\r\n")
        w.writeheader()
        for r in rows:
            w.writerow(r)
    print("wrote %-28s %3d rows x %2d cols" % (os.path.basename(path), len(rows), len(cols)))

if __name__ == "__main__":
    if not os.path.isdir(OUT):
        sys.exit("tables dir not found: %s" % OUT)
    write_materials(os.path.join(OUT, "heat_materials.csv"))
    write_decay(os.path.join(OUT, "heat_decay.csv"))
    write_geometry(os.path.join(OUT, "heat_geometry.csv"))
    write_references(os.path.join(OUT, "heat_references.csv"))
    print("\ntables dir: %s" % OUT)
