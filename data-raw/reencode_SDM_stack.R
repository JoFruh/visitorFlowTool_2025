# Re-encode the SDM stack so step 2's species scan stops paying to decompress
# bytes it never looks at.
#
# Run once, by hand, against the data directory. It writes a new file next to
# the old one and never touches the original, so it is safe to re-run and easy
# to back out of: point R/step2_server.R back at the old name.
#
#
# WHY
#
# SDMapsCH_100m_binary_4326_COG.tif is 667 bands of 3999x1756, DEFLATE, stored
# as Float32 in 512x512 blocks. Step 2 crops it to a user-drawn perimeter, and
# profiling that job showed the read is essentially the whole cost - the crop
# was 1.2-5.8s while everything after it (global(), the subsets, wrap()) came to
# under 0.5s even for a canton-sized perimeter. Two properties of the file made
# that read far more expensive than the data warrants:
#
#   Float32 for 0/1 data. Every cell costs 4 bytes to inflate where 1 would do.
#
#   512x512 blocks. A block is the smallest unit GDAL can read, so a 2 km
#   perimeter - 15x15 pixels - still had to inflate a 512x512 block in each of
#   667 bands to look at 15x15 of the cells in it. That is why the old scan took
#   about as long for a 2 km perimeter as for a 10 km one: nearly all of the
#   work was blocks the crop then threw away, and that waste did not shrink as
#   the area did.
#
# Measured after the change, same perimeters, GDAL_NUM_THREADS=ALL_CPUS on both:
#
#     box     old (F32/512)   new (Byte/128)
#      2 km       1.14s           0.06s      19x
#     10 km       1.17s           0.11s      11x
#     30 km       2.01s           0.70s      2.9x
#     60 km       4.84s           3.05s      1.6x
#
# The gain narrows as the perimeter grows because a large perimeter genuinely
# does have to read most of the file. The small perimeters most users draw were
# the ones paying almost entirely for waste.
#
#
# WHY gdalwarp AND NOT gdal_translate
#
# This is the trap in this conversion, and it is silent. gdal_translate with
#
#     -ot Byte -a_nodata 255
#
# produces a file that looks right and is wrong: -a_nodata only writes the
# NoData *tag*, it does not remap the source's NoData cells to it. The source
# NoData is NaN, and casting NaN to Byte gives 0 - which is also the legitimate
# "species absent" value. So every cell outside the modelled area silently
# became a real 0.
#
# The species scan would not have caught it: sum(na.rm = TRUE) treats NA and 0
# alike, so the species list came out identical on every box tested. It would
# have surfaced later and looked unrelated - step 2 plots the sensitivity matrix
# with colNA = "black", so a perimeter overlapping the border would have drawn
# the outside-Switzerland part in the lowest sensitivity colour instead of black.
#
# gdalwarp with -srcnodata nan -dstnodata 255 does the remap, and - checked, not
# assumed, since gdalwarp has a reputation for dropping band metadata - it
# preserves the band descriptions, which are the species names the whole app
# keys on.
#
# 255 is the new NoData because Byte cannot hold NaN and 255 cannot collide with
# the 0/1 payload. Nothing downstream compares against the NoData number itself;
# terra turns it into NA on read either way.
#
#
# WHY A TILED GTiff AND NOT A COG
#
# The stack is read from local disk, never over HTTP, so the things that make a
# COG a COG - the overviews and the IFD ordering that let a client range-request
# them - buy nothing here. They do cost: with 667 bands the COG driver's
# overview pass ran for over ten minutes without finishing. A plain tiled GTiff
# with the same block size reads identically fast and writes in about a minute.
#
#
# WHY 128 AND NOT 64
#
# 128 was the knee in testing. Smaller blocks keep helping the smallest
# perimeters, but per-block overhead starts to tell on the large ones, which are
# the slowest cases and so the ones worth protecting. If typical perimeters ever
# get much smaller, re-measure 64 rather than assuming.

suppressMessages({
  library(sf)
  library(terra)
})

devtools::load_all(".", quiet = TRUE)   # for vftData()

src <- vftData("maps/species_new/SDM/SDMapsCH_100m_binary_4326_COG.tif")
dst <- vftData("maps/species_new/SDM/SDMapsCH_100m_binary_4326_Byte128.tif")

stopifnot(file.exists(src))
if(file.exists(dst)){
  stop("Destination already exists, refusing to overwrite:\n  ", dst,
       "\nDelete it first if you really mean to rebuild.")
}

message("Re-encoding ", basename(src), " -> ", basename(dst))

t0 <- Sys.time()
sf::gdal_utils(
  "warp",
  source      = src,
  destination = dst,
  options = c(
    "-ot", "Byte",
    "-srcnodata", "nan",
    "-dstnodata", "255",
    #Same grid in, same grid out - this is a re-encode, not a reprojection.
    #near is the only resampling that cannot invent a value that is neither
    #0 nor 1.
    "-r", "near",
    "-of", "GTiff",
    "-co", "COMPRESS=DEFLATE",
    "-co", "TILED=YES",
    "-co", "BLOCKXSIZE=128",
    "-co", "BLOCKYSIZE=128",
    "-co", "NUM_THREADS=ALL_CPUS",
    "-multi", "-wo", "NUM_THREADS=ALL_CPUS"
  )
)
message("Wrote in ", round(as.numeric(Sys.time() - t0, units = "secs"), 1), "s, ",
        round(file.size(dst) / 1e6, 1), " MB (was ",
        round(file.size(src) / 1e6, 1), " MB)")


## VERIFY ####
#The question is not whether the new file resembles the old one, it is whether
#step 2 would build the same species list and draw the same map from either. So
#check the two things step 2 actually derives - the per-layer sums inside a
#perimeter, and the NoData footprint - rather than the file as a whole. The
#NoData check is the one that catches the gdal_translate trap described above.

terra::setGDALconfig("GDAL_NUM_THREADS", "ALL_CPUS")

mkbox <- function(cx, cy, km){
  d <- km / 111 / 2
  sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(cbind(
    c(cx - d, cx + d, cx + d, cx - d, cx - d),
    c(cy - d, cy - d, cy + d, cy + d, cy - d)))), crs = 4326))
}

scan_one <- function(f, v){
  r  <- terra::rast(f)
  t0 <- Sys.time()
  cr <- terra::crop(r, v, mask = TRUE)
  s  <- terra::global(cr, fun = "sum", na.rm = TRUE)$sum
  names(s) <- names(cr)
  keep <- !is.na(s) & s > 0
  list(secs    = as.numeric(Sys.time() - t0, units = "secs"),
       kept    = names(s)[keep],
       cover   = s[keep],
       #the NoData footprint inside the perimeter, which is what colNA plots
       naCells = terra::global(is.na(cr[[1]]), fun = "sum")$sum)
}

ro <- terra::rast(src); rn <- terra::rast(dst)
stopifnot(identical(names(ro), names(rn)))          # species names survived
stopifnot(identical(dim(ro), dim(rn)))
stopifnot(isTRUE(all.equal(as.vector(terra::ext(ro)), as.vector(terra::ext(rn)))))
message("Band names, dimensions and extent match.")

#Several places, not one: a box in the middle of the country exercises none of
#the NoData handling, so the border boxes are the interesting ones.
locs <- list(c(8.24, 46.80), c(6.15, 46.20), c(9.85, 46.50), c(7.45, 47.55))
ok <- TRUE
for(loc in locs) for(km in c(2, 10, 30)){
  v   <- terra::vect(mkbox(loc[1], loc[2], km))
  old <- scan_one(src, v)
  new <- scan_one(dst, v)

  sameKept  <- identical(old$kept, new$kept)
  sameCover <- isTRUE(all.equal(old$cover, new$cover))
  sameNA    <- isTRUE(all.equal(old$naCells, new$naCells))
  ok <- ok && sameKept && sameCover && sameNA

  message(sprintf("  %5.2f,%5.2f %2d km: old %5.2fs -> new %5.2fs (%4.1fx), %3d species  %s",
                  loc[1], loc[2], km, old$secs, new$secs,
                  old$secs / max(new$secs, 1e-9), length(new$kept),
                  if(sameKept && sameCover && sameNA) "identical" else
                    sprintf("MISMATCH kept=%s cover=%s nodata=%s",
                            sameKept, sameCover, sameNA)))
}

if(!ok){
  stop("The re-encoded file does not reproduce the original. Do NOT point the ",
       "app at it.")
}
message("All boxes identical, NoData footprint included. ",
        "Safe to point R/step2_server.R at the new file.")
