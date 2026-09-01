#### Session state: one registry, one writer, one restore function ####
#
#Before this file the shape of a save lived in three places that had to be kept
#in step by hand - the assembly block in downloadSave's content function, the
#`if(exists("envBase_..."))` ladder in step 1's confirm observer, and the
#filename switch. Adding a key meant remembering all three, and one of them
#(the ladder) was quietly wrong: it tested exists() against the OBSERVER's
#frame, so a second load in the same session inherited the first file's value
#for every key the second file happened not to carry.
#
#Everything about the format is now in VFT_STATE_KEYS plus the four functions
#below, and vftApplyState() is driven by a named list rather than by whatever
#load() left lying in the caller's frame.


#### the full save file ####
#
#The 30 keys the downloadable .RData carries, in the order save() used to name
#them. Each key `k` travels in the file as `envBase_k`; that mapping is the
#whole format, and it is unchanged, so files written before this rewrite still
#load and files written after it are still readable by the old path.
#
#One key is not a plain copy of r[[k]]: `SM_pres` travels PACKED (see
#vftStateFromR/vftApplyState below), because a SpatRaster is an external
#pointer and does not survive save().
VFT_STATE_KEYS <- c(
  "step",
  "shape",
  "toSelectSpAfter",
  "SM_pres",
  "SMcolors",
  "network",
  "parking",
  "residential",
  "minThresh",
  "isSkip",
  "confirm",
  "finalPolygons",
  "networkList",
  "versionsUI",
  "triggerNewVersions_nr",
  "triggerstep6_nr",
  "pathUsage",
  "step6FirstRun",
  "newVersionsFirstRun",
  "groupSave_all",
  "groupSave_sens",
  "groupSave_type",
  "groupSave_class",
  "checkboxSave",
  "filterList",
  "weightInputs",
  "weightNames",
  "needHelp",
  "species",
  "minCutThresh"
)

#Keys a restore is allowed to write that the current writer does not produce.
#`dateTime` was written by an older build and the ladder still read it;
#`currentLang` is set from step 1's own return on the file path and from the
#snapshot on the browser path. Reading a key nothing writes costs nothing and
#keeps old files whole.
VFT_STATE_APPLY <- c(VFT_STATE_KEYS, "dateTime", "currentLang")


#### the browser snapshot ####
#
#The lightweight subset that goes into localStorage for crash recovery. The
#budget is the browser's, roughly 5 MB of string per origin, and the measured
#full state is 17.8-24.1 MB gzipped (see the compression table in
#vftStateWrite) - 4 to 20 times over. So this carries what the user CHOSE and
#what they DREW, and nothing that the app can compute again:
#
#  * geometry:  shape (the perimeter), finalPolygons (the Zielgebiete)
#  * step 2:    the species/group/filter selections and the weights
#  * step 3:    the threshold and the skip flag
#  * nav/misc:  which step, which language, whether the tutorial is wanted
#
#Deliberately OUT, and why:
#  SM_pres           ~4 MB packed on its own - most of the whole budget
#  network           the path network, several MB, and a provider derives it
#  networkList       one copy of the network PER SCENARIO; the dominant term
#  pathUsage         a whole graph with the simulation on it
#  parking, shp_PA   produced by the network preparation, not by the user
#  versionsUI        card metadata for scenarios that are not in the snapshot;
#                    carrying it would name versions that cannot be opened
#  residential, step6FirstRun, newVersionsFirstRun, the trigger counters
#                    all scenario-side bookkeeping that means nothing without
#                    networkList
#
#The consequence is deliberate and is the user's call (2026-09-01): a crash at
#step 5 restores the perimeter, the areas of interest and every setting, and
#vftRestoreStep() lands the user at the furthest step those keys can actually
#reach - it does not bring a finished simulation back. The disk-icon save is
#what preserves one of those.
VFT_SNAPSHOT_KEYS <- c(
  "shape",
  "finalPolygons",
  "species",
  "groupSave_all",
  "groupSave_sens",
  "groupSave_type",
  "groupSave_class",
  "checkboxSave",
  "filterList",
  "weightInputs",
  "weightNames",
  "toSelectSpAfter",
  "SMcolors",
  "minCutThresh",
  "minThresh",
  "isSkip",
  "step",
  "currentLang",
  "needHelp"
)
#Not `dateTime`, though it is in VFT_STATE_APPLY: nothing in the app has
#written r$dateTime for a long time, so it would travel as a NULL every time.
#It stays readable so an old file that carries one is not rejected.

#Above this many characters of base64 the snapshot is not offered to the
#browser. localStorage quotas are commonly 5 MB of UTF-16 string; this leaves
#room for the JSON envelope and for a quota that is smaller than advertised.
VFT_SNAPSHOT_MAX_CHARS <- 3.5 * 1024 * 1024


#' Collect session state out of the app-level reactiveValues
#'
#' @param r the app-level reactiveValues from app_server()
#' @param keys which of VFT_STATE_KEYS to collect
#' @return a named list, one element per key, NULLs preserved
#' @noRd
vftStateFromR <- function(r, keys = VFT_STATE_KEYS){

  vals <- stats::setNames(vector("list", length(keys)), keys)

  #`vals[k] <- list(x)`, not `vals[[k]] <- x`. Double-bracket assignment of NULL
  #DELETES the element, so every key the session has not set yet would simply
  #vanish from the list - and a key that is absent is a key vftApplyState()
  #leaves alone, which would make a restore inherit whatever the session
  #already held instead of the emptiness that was saved. Single-bracket
  #assignment of a one-element list stores the NULL.
  for(k in keys){
    if(identical(k, "SM_pres")) next
    vals[k] <- list(r[[k]])
  }

  #The sensitivity raster has to travel as a plain R object, because a
  #SpatRaster is an external pointer and does not survive save().
  #
  #This used to be terra::as.data.frame(xy = TRUE, na.rm = FALSE) - one row per
  #cell INCLUDING NAs, carrying two full-precision coordinate doubles per cell
  #that are entirely redundant for a regular grid, and dropping the CRS (hence
  #the explicit crs<- on the restore path). terra::wrap() holds the same
  #information as a PackedSpatRaster. Measured at 100 m resolution over a
  #100 km area (1M cells): 20.0 MB -> 4.0 MB retained per session and
  #0.11s -> 0.01s to build.
  #
  #The RAM is the point, not the time: this is cached for the life of the
  #session, on a host where daemons have already been OOM-killed.
  #
  #Cached because SM_pres only changes in step 2 (and on resume), so it is built
  #once and reused by every later save rather than recomputed on the main
  #thread each time.
  #
  #A save taken before step 2 has been confirmed has no sensitivity matrix, and
  #that is a legitimate file: vftApplyState() below tests for NULL and skips it.
  #The WRITE side has to test too - terra::wrap(NULL) does not return NULL, it
  #aborts ("unable to find an inherited method for 'wrap' for signature
  #x = \"NULL\""), out of a download handler, so the browser gets an error page
  #instead of the file. Reachable since the nav bar started offering step 3
  #straight after step 1: nothing between there and step 4 needs SM_pres, so
  #nothing stops a user routing around step 2.
  if("SM_pres" %in% keys){
    if(is.null(r$SM_pres_packed) && !is.null(r$SM_pres)){
      r$SM_pres_packed <- terra::wrap(r$SM_pres)
    }
    vals["SM_pres"] <- list(r$SM_pres_packed)
  }

  vals
}


#' Write collected state to a .RData file in the app's save format
#'
#' @param vals a named list from vftStateFromR()
#' @param file destination path
#' @noRd
vftStateWrite <- function(vals, file){

  e <- new.env(parent = emptyenv())
  for(k in names(vals)) assign(paste0("envBase_", k), vals[[k]], envir = e)

  #save() defaults to gzip at compression_level 6, which is pure main-thread
  #CPU over the whole session state -- the raster, the network, the polygons --
  #on the thread every other user is waiting on. Measured on a 91.6 MB
  #representative payload:
  #  level 6 (default) 4.94 s -> 17.8 MB
  #  level 3           2.03 s -> 20.2 MB
  #  level 1           0.87 s -> 24.1 MB
  #  none              0.11 s -> 91.6 MB
  #Level 1 is 5.7x less blocking for a 35% larger file, and the format is
  #unchanged so load() reads it exactly as before. Uncompressed would be faster
  #still but quadruples what the user has to download.
  save(list = ls(envir = e, all.names = TRUE), envir = e, file = file,
       compress = "gzip", compression_level = 1)
}


#' Read a save file into a named list
#'
#' Loads into a fresh empty environment rather than the caller's frame, which is
#' what stops a second load in one session inheriting the first file's values.
#'
#' @param path a .RData written by vftStateWrite() (or by any earlier build)
#' @return a named list keyed WITHOUT the envBase_ prefix
#' @noRd
vftStateRead <- function(path){

  e <- new.env(parent = emptyenv())
  load(path, envir = e)

  nms  <- ls(envir = e, all.names = TRUE)
  nms  <- nms[startsWith(nms, "envBase_")]
  keys <- substring(nms, nchar("envBase_") + 1L)

  vals <- stats::setNames(vector("list", length(keys)), keys)
  #single-bracket again, for the same reason as in vftStateFromR(): a saved NULL
  #has to come back as a present-but-NULL element, not as a missing key.
  for(i in seq_along(keys)) vals[keys[i]] <- list(get(nms[i], envir = e))

  #envBase_basemap is deliberately NOT read back. step1 never assigned it, so
  #every save file that carries it carries a NULL; the key is simply not in
  #VFT_STATE_APPLY, so vftApplyState() ignores it.
  vals
}


#' Write state into the app-level reactiveValues and resume where it reaches
#'
#' @param r the app-level reactiveValues
#' @param vals a named list from vftStateRead() or vftStateDecode()
#' @param session the shiny session
#' @param resume navigate to the restored step when TRUE
#' @noRd
vftApplyState <- function(r, vals, session, resume = TRUE){

  if(!is.list(vals) || is.null(names(vals))) return(invisible(FALSE))

  keys <- intersect(names(vals), VFT_STATE_APPLY)

  for(k in keys){
    #SM_pres is not a plain copy - it arrives packed and is rehydrated below.
    if(identical(k, "SM_pres")) next
    r[[k]] <- vals[[k]]
  }

  #keep step6FirstRun TRUE when versionsUI is empty. The original version is
  #generated only on the first run (otherwise versionsUI is used), so a state
  #with no cards has to look like a first run whatever the flag said.
  if(is.null(r$versionsUI)){
    r$step6FirstRun <- TRUE
  }

  #Load the saved sensitivity matrix. terra::rast() has methods for both shapes
  #this can arrive in - a PackedSpatRaster from a current save file, or the xy
  #data.frame older files carry - so both restore here without branching. Only
  #the data.frame form loses the CRS, hence the crs<- below; on a packed raster
  #it is a harmless no-op (it is already 4326).
  #
  #The guard used to be `length(envBase_SM_pres > 0)`, which compares the whole
  #object against 0 and takes the length of the RESULT. That is never 0 for a
  #non-empty data.frame, so it never actually guarded anything - and it ERRORS
  #outright on a PackedSpatRaster ("comparison (>) is possible only for atomic
  #and list types"), which would have made every new save file unloadable.
  if("SM_pres" %in% keys && !is.null(vals[["SM_pres"]])){
    r$SM_pres <- terra::rast(vals[["SM_pres"]])
    terra::crs(r$SM_pres) <- "epsg:4326"
    #Deliberately NOT reusing the loaded object as the save cache: an old file
    #hands back a 20 MB data.frame, and keeping it would both retain it for the
    #session and write the old fat form again at the next save. Leaving this
    #NULL costs one terra::wrap() (~0.01s) and means every file this session
    #writes is the compact form.
    r$SM_pres_packed <- NULL
  }

  if(!isTRUE(resume)) return(invisible(TRUE))

  #### resume ####
  #
  #This was five hand-written branches, one per step code, with no branch at all
  #for the last step and one (`r$step == 1`) that bumped a reactiveVal nothing
  #observed - so a save taken at step 1 restored its data and then sat on
  #whatever tab was already showing. All of it is two calls now, and neither of
  #them is a list of steps: vftRestoreStep() reads the registry, so a step added
  #or removed there needs nothing here.
  #
  #It answers a question the ladder never asked. The number in the file says
  #where the user WAS; whether that step can be entered is a question about what
  #else the file carried, and the registry already knows. A save that names step
  #5 but has no `species` in it used to be honoured: step 5 was built, read
  #NULL, and failed somewhere in the middle. It now resumes at the furthest step
  #that can actually run and says so.
  #
  #REACHABLE, not available, which is the capability this buys: a state carrying
  #nothing but `shape` is legal. It names step 2, step 2 needs only `shape`, and
  #the buffered perimeter, the attractiveness crop and the path network are
  #derived by the provider layer when a step that reads them is entered - not
  #rebuilt eagerly on the way in. vftGoToStep() holds the navigation until they
  #land, with the progress bar showing. This is also exactly what makes the
  #lightweight browser snapshot a legal restore.
  #
  #The two national COGs used to be opened and cropped right here, on the main
  #thread, for every restore - including restores to steps that read neither of
  #them. They are providers now.
  wanted <- vftStepForCode(r$step)
  resumeAt <- vftRestoreStep(r)

  if(!is.null(wanted) && !identical(wanted, resumeAt)){
    vftDbg(paste0("RESTORE: state names ", wanted, ", resuming at ", resumeAt,
                  " (missing: ",
                  paste(vftStepMissing(r, wanted), collapse = ", "), ")"))
    #Said out loud rather than logged only: landing somewhere other than where
    #the state was taken is confusing enough to be worth a line, and the
    #alternative the ladder took - honour the number and let the module fail -
    #is worse.
    try(shiny::showNotification(
      paste0("Die Datei reicht nur bis „", VFT_STEPS[[resumeAt]]$label,
             "“ - dort geht es weiter."),
      type = "warning", duration = 8, session = session), silent = TRUE)
  }

  vftGoToStep(r, resumeAt, session)

  invisible(TRUE)
}


#### the browser transport ####
#
#serialize() -> gzip -> base64 rather than a hand-written JSON schema. It is
#type-exact, so an sf polygon survives without a GeoJSON round trip and without
#a per-class encoder to keep in step with VFT_SNAPSHOT_KEYS, and base64enc is
#already an Import. The string is what goes into localStorage.

#' @noRd
vftStateEncode <- function(vals){
  raw <- serialize(vals, connection = NULL, version = 3L)
  base64enc::base64encode(memCompress(raw, type = "gzip"))
}

#' @noRd
vftStateDecode <- function(txt){

  if(!is.character(txt) || length(txt) != 1L || is.na(txt) || !nzchar(txt)){
    return(NULL)
  }

  out <- tryCatch(
    unserialize(memDecompress(base64enc::base64decode(txt), type = "gzip")),
    error = function(e){
      vftDbg(paste0("vftStateDecode failed: ", conditionMessage(e)))
      NULL
    })

  #Anything that is not a plain named list is rejected before a single value
  #reaches r. The string comes from this user's own browser and is the same
  #trust level as the .RData they upload by hand, but a corrupted or
  #half-written localStorage entry must fail as "no snapshot", not as an error
  #inside a restore.
  if(is.null(out) || !is.list(out) || is.null(names(out))) return(NULL)

  out
}
