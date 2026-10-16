## Stage counts of .vftSimplifyCoverage() on the filBleu cut labels, to find
## which step turns 62 areas (GEOS 3.14) into 51 (GEOS 3.12). Run on both machines:
##   Rscript data-raw/fingerprint_aoi_polygons.R > poly_<machine>.txt
suppressPackageStartupMessages(library(sf))
terra::terraOptions(progress = 0)
ns <- asNamespace("visitorFlowTool")
p  <- function(label, ...) cat(sprintf("%-34s", label), ..., "\n")
thr <- 8
shape <- sf::st_transform(sf::st_read(ns$vftData("maps/filBleu_area_final.shp"), quiet = TRUE), 4326)
D <- terra::crop(terra::rast(ns$vftData("maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif")), sf::st_buffer(shape, 1000))
names(D) <- "walkNat"
buf   <- sf::st_union(sf::st_buffer(sf::st_geometry(sf::st_transform(shape, 2056)), 1000))
labels <- ns$vftCutAoI(D, thr, lakes = ns$.vftReadLakesWithin(buf))
p("GEOS / terra", sf::sf_extSoftVersion()[["GEOS"]], as.character(packageVersion("terra")))
p("labels", length(unique(stats::na.omit(terra::values(labels, mat = FALSE)))))

P <- terra::as.polygons(labels, aggregate = TRUE, na.rm = TRUE)
p("1 as.polygons", nrow(P))
P <- sf::st_set_crs(sf::st_set_crs(sf::st_geometry(sf::st_as_sf(P)), NA), sf::st_crs("EPSG:4326"))
P <- sf::st_transform(P, 2056)
p("  total ha", round(sum(as.numeric(sf::st_area(P))) / 1e4))
edges <- sf::st_line_merge(sf::st_union(sf::st_cast(sf::st_boundary(P), "MULTILINESTRING")))
p("2 edges (parts / length km)", length(sf::st_cast(edges, "LINESTRING", warn = FALSE)),
  round(sum(as.numeric(sf::st_length(edges))) / 1e3, 3))
edges <- sf::st_simplify(edges, dTolerance = 75, preserveTopology = TRUE)
p("3 simplified (parts / length km)", length(sf::st_cast(edges, "LINESTRING", warn = FALSE)),
  round(sum(as.numeric(sf::st_length(edges))) / 1e3, 3))
nF <- function(e) length(suppressWarnings(sf::st_collection_extract(sf::st_polygonize(e), "POLYGON")))
p("3b simple? / crossings", sf::st_is_simple(edges), "noded faces", nF(sf::st_union(edges)))
p("3c unsimplified faces", nF(sf::st_line_merge(sf::st_union(sf::st_cast(sf::st_boundary(P), "MULTILINESTRING")))))
for(tol in c(10, 25, 50)){
  e <- sf::st_simplify(sf::st_line_merge(sf::st_union(sf::st_cast(sf::st_boundary(P), "MULTILINESTRING"))), dTolerance = tol, preserveTopology = TRUE)
  p(sprintf("3d tolerance %g m faces", tol), nF(e), "noded", nF(sf::st_union(e)))
}
#the fix in R/aoiSegment.R, copied so it can be checked before a reinstall
splitMultiEdges <- function(edges){
  l  <- sf::st_cast(sf::st_cast(edges, "MULTILINESTRING"), "LINESTRING")
  ms <- lapply(l, unclass)
  pairKey <- function(m){
    k <- c(paste(m[1, 1], m[1, 2]), paste(m[nrow(m), 1], m[nrow(m), 2]))
    if(k[1] == k[2]) NA_character_ else paste(sort(k), collapse = "|")
  }
  keys  <- vapply(ms, pairKey, "")
  multi <- !is.na(keys) & keys %in% keys[duplicated(keys)]
  parts <- lapply(seq_along(ms), function(i){
    m <- ms[[i]]
    n <- nrow(m)
    if(!multi[i] || n < 3) return(list(m))
    k <- min(n - 1L, max(2L, (n + 1L) %/% 2L))
    list(m[seq_len(k), , drop = FALSE], m[k:n, , drop = FALSE])
  })
  sf::st_sfc(sf::st_multilinestring(unlist(parts, recursive = FALSE)),
             crs = sf::st_crs(edges))
}
e0 <- sf::st_line_merge(sf::st_union(sf::st_cast(sf::st_boundary(P), "MULTILINESTRING")))
p("3e multi-edges split, 75 m: faces", nF(sf::st_simplify(splitMultiEdges(e0), dTolerance = 75, preserveTopology = TRUE)))
faces <- suppressWarnings(sf::st_collection_extract(sf::st_polygonize(edges), "POLYGON"))
p("4 faces / ha", length(faces), round(sum(as.numeric(sf::st_area(faces))) / 1e4))
pts <- sf::st_transform(sf::st_point_on_surface(faces), sf::st_crs("EPSG:4326"))
lab <- terra::extract(labels, terra::vect(pts))[, 2]
p("5 faces labelled / unlabelled", sum(!is.na(lab)), sum(is.na(lab)))
keep <- !is.na(lab)
areas <- do.call(c, lapply(split(which(keep), lab[keep]), function(ix) sf::st_union(faces[ix])))
p("6 unioned per label", length(areas))
areas <- ns$.vftPolygonParts(areas)
a <- as.numeric(sf::st_area(areas)) / 1e4
p("7 polygon parts / under 10 ha", length(areas), sum(a < 10))
p("  under 25 ha", sum(a < 25), "total ha", round(sum(a)))
areas <- ns$.vftAbsorbFragments(areas, 100000)
a <- as.numeric(sf::st_area(areas)) / 1e4
p("8 after absorb / > 10 ha", length(areas), sum(a > 10))
p("  largest (ha)", paste(round(sort(a, decreasing = TRUE)[1:8]), collapse = " "))
g <- sf::st_transform(areas, 4326)
p("9 valid on the sphere (s2)", sum(sf::st_is_valid(g)), "of", length(g))
g <- ns$.vftS2Valid(g)
p("10 after .vftS2Valid", length(g), "largest ha",
  paste(round(sort(as.numeric(sf::st_area(sf::st_transform(g, 2056))) / 1e4, decreasing = TRUE)[1:6]), collapse = " "))
