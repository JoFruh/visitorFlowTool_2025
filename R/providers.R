#### Lazy data providers ####

# Step 1 used to build everything. Drawing a perimeter dispatched one future that
# read the national paths GDB, built the tbl_graph, cropped three national COGs
# and extracted eight raster bands onto every node - and then handed back a
# network whose fat node table is not read until step 4, a residents column not
# read until step 5, and two rasters of which exactly one is wanted in step 3.
# A user who drew an area and stopped paid for all of it.
#
# This file inverts that. A key is not computed because of where the user has
# been; it is computed because something is about to read it. The registry below
# says, per key, what it is derived FROM and how to derive it, and vftEnsure()
# walks that graph and dispatches only the part that is both missing and
# reachable.
#
# THE PROMISE PROBLEM IS SOLVED BY NEVER AWAITING. vftEnsure() dispatches and
# returns immediately; nothing blocks. The %...>% handler assigns r[[key]] <-
# value, and that assignment is a reactive write, so the one observe() in
# vftProviderServer() re-runs, sees the next layer of the graph become
# dispatchable, and sends it. The same write invalidates vftStepAvailable(),
# which is what lets a deferred navigation complete on its own. No module server
# ever awaits anything - modules keep receiving plain values, they are simply
# constructed later, which is what makes this shippable before Stage 5 converts
# them.
#
# Two constraints every provider respects:
#
#   1. terra objects are external pointers and cannot cross the mirai boundary.
#      terra::wrap() on return, terra::unwrap() in the handler.
#   2. The .GlobalEnv$.vft_* raster caches live in the DAEMON, not here. Each
#      daemon re-reads the national COGs once in its lifetime, which is correct:
#      hoisting the crop to the main thread would put it back on the thread every
#      other user is waiting on.

#' How long a provider may be considered in flight before the marker lapses.
#'
#' Same safety-valve reasoning as VFT_BUSY_MAX_S in R/async_helpers.R, and for
#' the same observed failure: a daemon can die holding a job, and the promise
#' then never settles either way. Without this the key would be wedged
#' "in flight" for the rest of the session and could never be asked for again -
#' which is worse than a duplicate dispatch, because the revival in
#' .vftEnsureDaemons() only happens ON the next dispatch.
VFT_PROVIDER_MAX_S <- 300

#' What can be derived, from what, and how.
#'
#' Keyed by PROVIDER name, not by key, because dispatch is not free and the
#' split of sf_to_tidygraph3() has to be grouped by consuming step rather than by
#' column: `network`, `DULN` and the node table are one job producing three keys,
#' because computing them separately would mean sending a 3 MB graph back into a
#' daemon to have columns added to it.
#'
#' Fields:
#'   keys    - the `r$` keys this provider fills. The FIRST provider in this list
#'             that lists a key is the one used for it, so cheap specific
#'             providers must come before broad ones: `DULN_all` alone is a crop,
#'             and step 3 must get that rather than the whole network job.
#'   needs   - other keys that must be ready before it can run.
#'   async   - TRUE goes through vftFuture() into a daemon; FALSE runs inline
#'             here and must therefore be cheap.
#'   label   - what the progress bar and the error notification call it.
#'   provide - function(vals, progress). `vals` is a named list of the `needs`,
#'             already isolated. Returns a named list of key -> value for a sync
#'             provider, or a PROMISE of one for an async provider.
VFT_PROVIDERS <- list(

  #The buffered perimeter every raster crop and the path query are cut against.
  #
  #step 1 computes this itself (it has the branch-specific shape handling and
  #the two branches disagree about CRS and normalisation), so on the forward path
  #this provider never runs. It exists for the RESTORE path: save files carry
  #envBase_shape and have never carried the buffer, so without it every restored
  #session would be unable to derive anything at all.
  #
  #Synchronous because it is two sf calls on a single polygon. Putting it in a
  #daemon would cost more in dispatch than it costs to do.
  shapeLarger = list(
    keys  = "shapeLarger",
    needs = "shape",
    async = FALSE,
    label = "Perimeter",
    provide = function(vals, progress){
      #EPSG:3857 to buffer, because the buffer is in metres. Left in 3857
      #afterwards: every consumer transforms it to 4326 itself, which is what
      #step 1's shapefile branch already produced and what sf_to_tidygraph3()
      #has always been handed.
      buf <- sf::st_buffer(sf::st_transform(vals$shape, "EPSG:3857"), dist = 1000)

      #st_as_sfc() is here to drop the attribute columns an `sf` carries, which
      #is the form step 1 hands over and the form every consumer expects. It has
      #no method for a bare `sfc` and ABORTS on one rather than passing it
      #through - and unlike step 1, which knows it built an `sf` two lines
      #earlier, this reads whatever a save file happens to hold. `r$shape` is an
      #sf today (it is `r$polygonsList`, or a shapefile read), so this branch is
      #not known to have fired; asking costs nothing and not asking would take
      #out the one provider the whole restore path is built on - with nothing
      #derivable afterwards, every restore would be stuck at step 2.
      if(!inherits(buf, "sfc")) buf <- sf::st_as_sfc(buf)
      list(shapeLarger = buf)
    }
  ),

  #The single-band attractiveness raster, cropped. This is ALL step 3 wants, and
  #it is the reason step 3 no longer waits for the path network: on its own the
  #crop is seconds, against ~30s for the network job it used to be bundled into.
  DULN_all = list(
    keys  = "DULN_all",
    needs = "shapeLarger",
    async = TRUE,
    label = "Attraktivitaetsdaten",
    provide = function(vals, progress){
      shapeLarger <- vals$shapeLarger
      p <- vftFuture({
        #the daemon's own cache - see the note at the top of this file
        if(!exists(".vft_DULN_all_full", envir = .GlobalEnv)){
          .GlobalEnv$.vft_DULN_all_full <-
            terra::rast(vftData("maps/DULN/DULN_nat_majMaxMeanAGGBlur.tif"))
        }
        out <- terra::crop(.GlobalEnv$.vft_DULN_all_full,
                           sf::st_transform(shapeLarger, "epsg:4326"))
        progress$close()
        terra::wrap(out)
      }, seed = TRUE, progress = progress)

      promises::then(p, function(packed){
        list(DULN_all = terra::unwrap(packed))
      })
    }
  ),

  #The seven-band per-agent attractiveness raster, cropped. Step 4 wants this and
  #DULN_all and NOTHING ELSE, and of these seven bands it reads exactly one:
  #generateAoI2() is handed DULN$walkNat (not the whole raster - wrapping seven
  #bands to send one across to the worker was six bands of pure transport), and
  #step 4's .vftDULNna() cuts the lakes out of the same band for the manual
  #editing handlers. The other six are here for sf_to_tidygraph3() and the
  #newVersions page, which do read them.
  #
  #Step 3 asks for this key too, at the top of its build observer in
  #app_server.R, even though step 3 itself never touches it - it is a PREFETCH.
  #Step 4 needs it, the crop takes seconds, and confirming step 3 used to sit
  #and wait for the whole thing with the user watching a step they had finished.
  #
  #It exists for exactly the reason DULN_all above does, one step later. `DULN`
  #used to be produced only by the `network` provider, so step 4 listing it in
  #its `needs` pulled the whole ~30s path-network job - which step 4 does not
  #read a single edge of. This is the same crop sf_to_tidygraph3() performs
  #internally, against the same buffered perimeter and with the same band names,
  #so the two are interchangeable and whichever runs first wins.
  DULN = list(
    keys  = "DULN",
    needs = "shapeLarger",
    async = TRUE,
    #not the same string as DULN_all's: step 4 asks for both at once and the two
    #progress bars sit side by side.
    label = "Attraktivitaetsdaten pro Aktivitaet",
    provide = function(vals, progress){
      shapeLarger <- vals$shapeLarger
      p <- vftFuture({
        #the daemon's own cache - see the note at the top of this file
        if(!exists(".vft_DULN_full", envir = .GlobalEnv)){
          .GlobalEnv$.vft_DULN_full <-
            terra::rast(vftData("maps/attr/allAttrs_COG_final.tif"))
        }
        out <- terra::crop(.GlobalEnv$.vft_DULN_full,
                           terra::project(terra::vect(shapeLarger), "EPSG:4326"))
        #the names every consumer indexes by - terra::subset(DULN, "walkNat") in
        #generateAoI2(), DULN$walkNat in step 4. Kept identical to
        #sf_to_tidygraph3()'s, which is the other place this crop is made.
        names(out) <- c("jog", "dogNat", "ebikeNat", "walkNat",
                        "dogProx", "walkSoc", "bikerSport")
        progress$close()
        terra::wrap(out)
      }, seed = TRUE, progress = progress)

      promises::then(p, function(packed){
        list(DULN = terra::unwrap(packed))
      })
    }
  ),

  #The path network and everything hung off its nodes. One job, four keys.
  #
  #NOTHING BEFORE A SIMULATION OR A NEW VERSION ASKS FOR THIS ANY MORE. No step's
  #`needs` names it: steps 1-4 work on the perimeter and the two rasters, and the
  #two places that genuinely read paths - the simulation launch in step 5 and the
  #newVersions page's edit contexts - ask for it through vftPrepareThen() in
  #R/prepare_network.R. Walking 1-2-3-4, adjusting a threshold, redrawing a
  #polygon and confirming again therefore costs no GDB read at all, and the one
  #read that does happen is cached in `r$network` for the rest of the session.
  #
  #`networkNodes` is not a key of its own - the columns arrive attached to
  #`network` - but it is named here so that vftEnsure() can be asked for it and
  #so the invalidation graph has something to hang step 5's `Residents`
  #dependency on. VFT_KEY_READY tests for the columns, which is what makes an old
  #save file carrying a fat envBase_network count as already done.
  #
  #DULN and DULN_all are listed too because sf_to_tidygraph3() has to crop them
  #anyway to extract them onto the nodes; returning them costs nothing. If step 3
  #or step 4 already derived them, this overwrites them with identical crops,
  #which is why the two cheap providers above must come FIRST in this list -
  #otherwise either step would pull the whole network job.
  network = list(
    keys  = c("network", "DULN", "DULN_all", "networkNodes"),
    needs = "shapeLarger",
    async = TRUE,
    label = "Wegnetz",
    provide = function(vals, progress){
      shapeLarger <- vals$shapeLarger
      p <- vftFuture({
        #spatial filter for the GDB read, as well-known text
        wkt <- sf::st_as_text(sf::st_transform(shapeLarger, "epsg:4326"))
        progress$inc(1/3)

        loadedPaths <- sf::st_read(vftData("maps/paths/paths_11_24_final_4.gdb"),
                                   query = 'SELECT * FROM "paths_11_24_final_4"',
                                   wkt_filter = wkt,
                                   promote_to_multi = FALSE, type = 2)
        progress$inc(1/3)

        res <- sf_to_tidygraph3(loadedPaths, shapeLarger, directed = FALSE)
        progress$inc(1/3)
        progress$close()
        res
      }, seed = TRUE, progress = progress)

      promises::then(p, function(res){
        list(network  = res[[1]][[1]],
             DULN     = terra::unwrap(res[[2]][[1]]),
             DULN_all = terra::unwrap(res[[3]][[1]]))
      })
    }
  )
)

#' Which provider fills a key, or NULL if nothing can derive it.
vftKeyProvider <- function(key){
  for(nm in names(VFT_PROVIDERS)){
    if(key %in% VFT_PROVIDERS[[nm]]$keys) return(nm)
  }
  NULL
}

#' Keys that are recorded in the graph but are not `r$` slots of their own.
#'
#' `networkNodes` is columns on `r$network`. It has to appear in the graph - step
#' 4 and step 5 genuinely depend on it and it genuinely has to be computed - but
#' there is nothing to assign and nothing to NULL.
VFT_PSEUDO_KEYS <- "networkNodes"

#' Could this key be produced, given what `r` currently holds?
#'
#' "Ready" and "derivable" are different questions and the nav bar asks the
#' second one: DULN_all is derivable from shape, so step 3 lights up before the
#' crop has been done and clicking it starts the crop; minThresh comes from the
#' step-3 slider and nothing can derive it, so step 4 stays dark until step 3 has
#' been through.
#'
#' Reads `r`, so calling this inside an observe takes a dependency on every key
#' along the chain it walks.
vftKeyDerivable <- function(r, key, .seen = character(0)){
  if(vftKeyReady(r, key)) return(TRUE)
  if(key %in% .seen) return(FALSE)          #a cycle cannot derive anything
  nm <- vftKeyProvider(key)
  if(is.null(nm)) return(FALSE)
  needs <- VFT_PROVIDERS[[nm]]$needs
  if(length(needs) == 0) return(TRUE)
  all(vapply(needs, function(n) vftKeyDerivable(r, n, c(.seen, key)), logical(1)))
}


#### What a change invalidates ####

# This is the graph the plan calls the biggest risk, and it is written down in
# full from day one rather than retrofitted. The reason is specific: go back to
# step 2, pick a different species set, come forward again, and step 5 will
# happily show a simulation computed against the sensitivity matrix that is no
# longer displayed anywhere. Nothing errors. The map just quietly means something
# else. Retrofitting these edges means shipping a period where that is possible,
# which is why they are here before backward navigation is switched on.
#
# Note this is a SUPERSET of VFT_PROVIDERS' `needs`: a provider only records what
# it can recompute, while invalidation has to record everything that would be
# WRONG - including keys no provider will ever produce, because a human produces
# them in a module.

#' Key -> the keys it was derived from. Read as "if any of these change, this is
#' stale".
VFT_DERIVED_FROM <- list(
  shapeLarger     = "shape",
  network         = "shapeLarger",
  DULN            = "shapeLarger",
  DULN_all        = "shapeLarger",
  networkNodes    = c("network", "DULN", "DULN_all"),

  #step 2: the species selection and everything cut from it
  species         = "shape",
  filterList      = "species",
  checkboxSave    = "species",
  groupSave_all   = "species",
  groupSave_sens  = "species",
  groupSave_type  = "species",
  groupSave_class = "species",
  weightInputs    = "species",
  weightNames     = "species",
  toSelectSpAfter = "species",
  SM_pres         = c("shape", "species", "weightInputs"),
  SMcolors        = "SM_pres",
  minCutThresh    = "SM_pres",
  #the packed copy the save path caches. An edge rather than a clean-up hook,
  #because it is not a dependent of anything SM_pres derives - it IS SM_pres, in
  #the only form save() can carry - and hooks only run for keys the closure
  #reaches, which never includes the key being invalidated FROM.
  SM_pres_packed  = "SM_pres",

  #step 3: the attractiveness threshold
  minThresh       = "DULN_all",
  isSkip          = "DULN_all",

  #step 4: the confirmed areas of interest and the network they were cut on
  finalPolygons   = c("shape", "DULN", "DULN_all", "minThresh"),
  parking         = c("shape", "finalPolygons"),
  networkList     = c("network", "networkNodes", "finalPolygons", "parking"),

  #step 5 and beyond.
  #
  #A simulation is a function of the NETWORK, and of nothing that step 2
  #produces. This file used to claim otherwise - "a simulation is a function of
  #the network AND of the sensitivity matrix" - and made `pathUsage` a dependent
  #of SM_pres, species and minCutThresh, so going back to step 2 threw away every
  #simulation and every saved version. That claim is not true of this code and
  #never was: `launchMultiSim()`, `launchSim_v2()`, `generatePopulation()`,
  #`subsetPopulation()`, `generateAoI2()`, `determineShortestPath()`,
  #`choosePath()` and `determineAgentCharacteristics()` contain no reference to
  #any of the three. Every consumer of the sensitivity matrix is a DISPLAY
  #consumer - a leaflet raster overlay (step5_server.R:702, :1494,
  #newVersions_server.R:3681), a terra::plot overlay (step5_server.R:1701), the
  #GeoTIFF export (:322) and the species caption beside it - and each of them
  #reads the CURRENT value. Change the species set and the overlay is redrawn;
  #there is no stale mixture to protect against, so there is nothing to discard.
  #
  #The edges that DO protect against a mixed result are the ones through
  #`networkList` and `finalPolygons`: going back to step 1, 3 or 4 still
  #invalidates the simulations, because those steps really do change what was
  #simulated.
  pathUsage       = "networkList",
  versionsUI      = c("networkList", "pathUsage"),
  #which scenario card step 5 and the newVersions page come back with selected,
  #held as a NAME - see vftVersionPosition() in R/modules.R. A name outliving its
  #version is harmless (it resolves to the first card), so this edge is hygiene
  #rather than a guard: a rebuilt version list has no business carrying the old
  #one's selection, and there is no label for it in VFT_KEY_LABEL because
  #"you will lose which card was highlighted" is not worth a line in the warning.
  selectedVersion = "versionsUI",
  shp_PA          = "shape"
)

#' Extra clean-up when a key is discarded.
#'
#' For the two cases where NULLing the key is not the whole story.
#'
#' The first is the partial invalidation the plan singles out: simulation results
#' live INSIDE r$networkList, one pathUsage per version, so discarding the
#' simulation must reach into the list rather than drop it - the networks and the
#' hand-drawn edits in it are still valid. The second restores a flag that
#' app_server reads to decide whether the original scenario has to be rebuilt.
#'
#' Anything that is simply "a second copy of a key" belongs in VFT_DERIVED_FROM
#' as an edge instead, not here: hooks run only for keys the dependency closure
#' reaches, and that never includes the key being invalidated FROM.
VFT_INVALIDATE_HOOKS <- list(
  pathUsage = function(r){
    nl <- shiny::isolate(r$networkList)
    if(is.null(nl) || !length(nl)) return(invisible(NULL))
    r$networkList <- lapply(nl, function(v){
      if(is.list(v)) v$pathUsage <- NULL
      v
    })
    invisible(NULL)
  },
  versionsUI = function(r){
    #app_server already holds the rule "no versionsUI means the original scenario
    #has to be generated again"; discarding versionsUI has to restore it or step
    #5 comes back with an empty version list and no way to refill it.
    r$step6FirstRun        <- TRUE
    r$newVersionsFirstRun  <- TRUE
    invisible(NULL)
  }
)

#' Everything downstream of `keys`, transitively, in dependency order.
#'
#' Does NOT include `keys` themselves: they are about to be recomputed by the
#' step the user is going back to, not discarded.
vftDependents <- function(keys){
  out  <- character(0)
  seen <- character(0)
  frontier <- keys
  while(length(frontier)){
    direct <- names(VFT_DERIVED_FROM)[vapply(
      VFT_DERIVED_FROM, function(parents) any(parents %in% frontier), logical(1))]
    direct <- setdiff(direct, c(seen, keys))
    if(!length(direct)) break
    out      <- c(out, direct)
    seen     <- c(seen, direct)
    frontier <- direct
  }
  out
}

#' The keys a step produces, i.e. what re-doing it would overwrite.
vftStepProduces <- function(step){
  names(VFT_KEY_SOURCE)[VFT_KEY_SOURCE == step]
}

#' Human names, for the "this will be discarded" modal.
VFT_KEY_LABEL <- c(
  network       = "Wegnetz",
  networkNodes  = "Wegnetz-Daten",
  DULN          = "Attraktivitaetsdaten",
  DULN_all      = "Attraktivitaetsdaten",
  species       = "Artenauswahl",
  SM_pres       = "Sensibilitaetsmatrix",
  SMcolors      = "Sensibilitaetsmatrix",
  minCutThresh  = "Sensibilitaetsschwelle",
  minThresh     = "Attraktivitaetsschwelle",
  finalPolygons = "Zielgebiete",
  parking       = "Parkplaetze",
  networkList   = "Szenarien",
  pathUsage     = "Simulationsergebnisse",
  versionsUI    = "gespeicherte Versionen"
)

#' Dependents an invalidation must SPARE.
#'
#' One exception, and it is not a special case so much as a missing edge. A
#' canvas - the scenario list the newVersions page runs on when it was entered
#' through the Hitzeminderung door, see vftIsCanvasList() in R/steps.R - holds a
#' heat design painted on the land cover inside the step-1 perimeter. It has no
#' network, no Zielgebiete and nothing simulated, and it is derived from the
#' PERIMETER and nothing else. `minThresh`, `finalPolygons` and the path network
#' therefore cannot make it stale.
#'
#' The graph says otherwise, because it describes the ordinary scenario list:
#' networkList <- finalPolygons <- minThresh. So confirming step 3 - which is
#' exactly what a user does when they take this page's own offer to go and draw
#' the Zielgebiete - discarded the painting on the way there, and step 4's
#' confirm discarded it again on the way back. That is the whole of what this
#' function exists to stop.
#'
#' `versionsUI` and `selectedVersion` go with it: the cards ARE the scenarios,
#' one card per entry by position, and keeping one list while clearing the other
#' leaves generateVersionButtons() resolving names against a list that is no
#' longer there.
#'
#' A change to `shape` is not spared: the paint is georeferenced to the outline
#' drawn in step 1, so a new outline really does invalidate it.
#'
#' @param dep the dependents this invalidation is working on, so the answer is a
#'   subset of them and the caller can `setdiff()` it straight out.
vftInvalidateKeep <- function(r, keys, dep){
  if(!length(dep) || "shape" %in% keys) return(character(0))
  if(!vftIsCanvasList(shiny::isolate(r$networkList))) return(character(0))
  intersect(dep, c("networkList", "versionsUI", "selectedVersion"))
}

#' What would actually be lost, as text, if `keys` were discarded.
#'
#' Only keys that are BOTH downstream and currently populated: a warning naming
#' results the user has not produced yet is a warning they learn to click
#' through. Empty means there is nothing to warn about and the caller should just
#' go.
vftInvalidationPreview <- function(r, keys){
  dep <- setdiff(vftDependents(keys), VFT_PSEUDO_KEYS)
  #the same rule vftInvalidate() applies below, asked here as well: a key that
  #survives the invalidation is not lost, and naming it would put "2
  #gespeicherte Versionen werden verworfen" in front of a user whose versions
  #step 4 is about to keep and fill in.
  dep <- setdiff(dep, vftInvalidateKeep(r, keys, dep))
  if(!length(dep)) return(character(0))

  live <- dep[vapply(dep, function(k) !is.null(shiny::isolate(r[[k]])), logical(1))]
  if(!length(live)) return(character(0))

  labs <- VFT_KEY_LABEL[live]
  labs <- labs[!is.na(labs)]
  if(!length(labs)) return(character(0))

  #versionsUI is a list of saved scenarios, and "3 gespeicherte Versionen" tells
  #the user far more about what they are about to lose than the bare noun.
  if("versionsUI" %in% live){
    n <- length(shiny::isolate(r$versionsUI))
    if(n > 0) labs[names(labs) == "versionsUI"] <-
      paste0(n, " gespeicherte Version", if(n == 1) "" else "en")
  }
  unique(unname(labs))
}

#' Discard `keys`' dependents, so nothing computed from a superseded input can be
#' displayed.
#'
#' Deliberately does NOT warn, ask, or navigate - the modal is the caller's job
#' (see vftConfirmInvalidation()), because "what is about to be lost" has to be
#' answerable BEFORE anything is thrown away.
#'
#' @return the keys that were cleared, invisibly.
vftInvalidate <- function(r, keys, session = shiny::getDefaultReactiveDomain()){
  dep <- vftDependents(keys)
  if(!length(dep)) return(invisible(character(0)))

  #a canvas is not downstream of the areas of interest whatever the graph says -
  #see vftInvalidateKeep(). Its HOOK is skipped with it, deliberately: the one on
  #versionsUI re-arms r$newVersionsFirstRun and r$step6FirstRun, which would
  #rebuild the version cards for a list that was never taken away.
  keep <- vftInvalidateKeep(r, keys, dep)
  dep  <- setdiff(dep, keep)
  if(length(keep))
    vftDbg(paste0("INVALIDATE ", paste(keys, collapse = ", "),
                  " -> kept (canvas) ", paste(keep, collapse = ", ")))
  if(!length(dep)) return(invisible(character(0)))

  for(k in dep){
    if(!(k %in% VFT_PSEUDO_KEYS)) r[[k]] <- NULL
    hook <- VFT_INVALIDATE_HOOKS[[k]]
    if(!is.null(hook)) try(hook(r), silent = TRUE)
  }

  #anything still queued for derivation is queued against the state we have just
  #torn down; and a provider that failed against the old inputs deserves another
  #chance against the new ones.
  .vftWantedClear(session)
  .vftFailedClear(session)
  #same reasoning for the continuations: a vftEnsureThen() waiter is holding a
  #position in a networkList that may not exist any more, so it is dropped -
  #through its onFail, because the caller disabled a button before asking.
  .vftThenClear(session)

  vftDbg(paste0("INVALIDATE ", paste(keys, collapse = ", "),
                " -> cleared ", paste(dep, collapse = ", ")))
  invisible(dep)
}



#### Creating new data is what discards derived data ####

# Until now the trigger was NAVIGATION: vftGoToStep() noticed a backward move,
# named what was downstream of the step being returned to, and threw it away on
# confirm. That is the wrong event, and it was wrong in both directions.
#
# Too eager: going back to LOOK at a step destroyed results the user had not
# decided to replace. Step 5 -> step 3 discarded every simulation before the
# user had touched the threshold slider.
#
# Not eager enough: the destruction that actually happens is a WRITE, and a
# write can be reached without a backward move. Step 5 -> step 2 is free (step 2
# is display-only downstream), but confirming step 2 walks FORWARD to step 3,
# and forward moves were never checked - so the user reached step 3, confirmed a
# new threshold, the areas of interest were regenerated with no warning at all,
# and the nav bar went on offering step 5 and its now-orphaned scenarios.
#
# So the event is the write. vftCommit() is the single door every step's confirm
# handler puts its results through: it compares them with what `r` already
# holds, invalidates only what was derived from the keys that ACTUALLY changed,
# and asks first when that would cost the user something. Re-confirming a step
# without changing anything is therefore free, and so is walking back through
# five steps to read them.

#' Which of `values` would actually change `r`.
#'
#' `identical()` rather than a per-key comparison: it is total, so a key can be
#' added to a confirm handler without teaching this function about it. Two
#' consequences worth knowing:
#'
#'   - a freshly computed object of the same content is NOT identical to the old
#'     one (terra pointers especially), so such a key counts as changed. That
#'     errs towards invalidating, which is the safe direction.
#'   - a slider that was moved and moved back IS identical, so returning to a
#'     step, looking, and confirming it again costs nothing.
#'
#' tryCatch because identical() on an external pointer whose object has been
#' released can raise; an error means "cannot prove it is the same", i.e. changed.
vftChangedKeys <- function(r, values){
  keys <- names(values)
  if(is.null(keys) || !length(keys)) return(character(0))
  keys[vapply(keys, function(k){
    old <- shiny::isolate(r[[k]])
    !isTRUE(tryCatch(identical(old, values[[k]]), error = function(e) FALSE))
  }, logical(1))]
}

#' Write a step's results into `r`, discarding what they supersede.
#'
#' @param values named list of `r` key -> new value. Everything the step
#'   produces, whether or not it changed; the comparison is done here.
#' @param step the step making the write, for the modal's wording only.
#' @param then run after the write has happened - the navigation, the autosave.
#'   NOT run if the user cancels, which is the point of it being a callback:
#'   cancelling has to leave the user on the step they are still working on.
#' @param onCancel run instead of `then` if the user cancels. Nothing is written
#'   and nothing is discarded; this is only for putting the step's own buttons
#'   back the way they were before its confirm handler disabled them.
#'
#' @return the keys that changed, invisibly; NULL if a decision is pending.
vftCommit <- function(r, values, session = shiny::getDefaultReactiveDomain(),
                      step = NULL, then = NULL, onCancel = NULL){
  #A key VFT_KEY_SOURCE attributes to this step but that is not in `values` is
  #either written somewhere else on purpose (step 1's providers derive four of
  #its six) or forgotten - and a forgotten one is a key that changes without
  #anything downstream noticing. Only a trace, because only the first case can
  #be told from the second by reading the call site.
  if(!is.null(step)){
    missed <- setdiff(vftStepProduces(step), names(values))
    if(length(missed))
      vftDbg(paste0("COMMIT ", step, ": not written here - ",
                    paste(missed, collapse = ", ")))
  }

  changed <- vftChangedKeys(r, values)

  if(!length(changed)){
    vftDbg("COMMIT: nothing changed, nothing discarded")
    if(is.function(then)) then()
    return(invisible(character(0)))
  }

  #Only ask when something the user can point at would be lost. A warning about
  #results they have not produced yet is a warning they learn to click through.
  atRisk <- vftInvalidationPreview(r, changed)
  if(length(atRisk) && !is.null(session)){
    vftAskCommit(r, values, changed, atRisk, session, step, then, onCancel)
    return(invisible(NULL))
  }

  vftApplyCommit(r, values, changed, session, then)
}

#' The write itself, once it is allowed to happen.
#'
#' Invalidate BEFORE writing: VFT_INVALIDATE_HOOKS reach sideways - the
#' `pathUsage` hook strips the simulation out of every element of r$networkList -
#' and step 4 commits a new networkList in the same call. Clearing first means
#' the hook works on the list it was meant for, and the new value lands on top.
#'
#' Every key in `values` is written, not only the changed ones: a key that did
#' not change may still have been cleared as a dependent of one that did (step
#' 4's `parking` is derived from its `finalPolygons`), and it has to come back.
vftApplyCommit <- function(r, values, changed,
                           session = shiny::getDefaultReactiveDomain(),
                           then = NULL){
  vftInvalidate(r, changed, session)
  for(k in names(values)) r[[k]] <- values[[k]]

  vftDbg(paste0("COMMIT ", paste(changed, collapse = ", ")))
  if(is.function(then)) then()
  invisible(changed)
}

#' Ask before a write that costs the user results.
#'
#' The pending decision is parked in `session$userData`, NOT closed over by a
#' fresh pair of observers per modal. Per-modal observers were the old shape of
#' this and they are a trap: a cancelled one stays armed, so the NEXT modal's OK
#' click runs the PREVIOUS modal's closure and writes values the user has since
#' abandoned. One pair of observers per session, one slot, and cancel empties it.
vftAskCommit <- function(r, values, changed, atRisk,
                         session = shiny::getDefaultReactiveDomain(),
                         step = NULL, then = NULL, onCancel = NULL){
  session$userData$vftCommitPending <-
    list(r = r, values = values, changed = changed, then = then, onCancel = onCancel)

  what <- if(!is.null(step) && !is.null(VFT_STEPS[[step]]))
            paste0(" in Schritt '", VFT_STEPS[[step]]$label, "'") else ""

  shiny::showModal(shiny::modalDialog(
    title = "Neue Daten uebernehmen?",
    shiny::tags$p(paste0(
      "Sie erstellen damit neue Daten", what,
      ". Die folgenden Ergebnisse bauen darauf auf und werden verworfen:")),
    shiny::tags$ul(lapply(atRisk, shiny::tags$li)),
    shiny::tags$p(paste0(
      "Abbrechen laesst alles unveraendert - fruehere Schritte koennen Sie ",
      "jederzeit ansehen, ohne etwas zu verlieren.")),
    footer = shiny::tagList(
      shiny::actionButton("vftInvalidateCancel", "Abbrechen"),
      shiny::actionButton("vftInvalidateOk", "Neu erstellen und verwerfen",
                          class = "btn-danger")
    ),
    easyClose = FALSE
  ), session = session)

  invisible(NULL)
}

#' Wire the two modal buttons. Called once from `app_server()`.
#'
#' The `v == 0` guard is what makes one pair of observers safe across many
#' modals: re-showing a modal re-renders the actionButton, and a re-rendered
#' actionButton reports 0 - a CHANGE, which observeEvent would otherwise treat
#' as a click on the button that has just appeared.
vftCommitServer <- function(r, session = shiny::getDefaultReactiveDomain()){
  take <- function(){
    p <- session$userData$vftCommitPending
    session$userData$vftCommitPending <- NULL
    p
  }

  shiny::observeEvent(session$input$vftInvalidateOk, {
    v <- session$input$vftInvalidateOk
    if(is.null(v) || v == 0) return(invisible(NULL))
    p <- take()
    shiny::removeModal(session = session)
    if(is.null(p)) return(invisible(NULL))
    vftApplyCommit(p$r, p$values, p$changed, session, p$then)
  }, ignoreInit = TRUE)

  shiny::observeEvent(session$input$vftInvalidateCancel, {
    v <- session$input$vftInvalidateCancel
    if(is.null(v) || v == 0) return(invisible(NULL))
    p <- take()
    shiny::removeModal(session = session)
    if(is.null(p)) return(invisible(NULL))
    vftDbg("COMMIT CANCELLED - nothing written, nothing discarded")
    if(is.function(p$onCancel)) try(p$onCancel(), silent = TRUE)
  }, ignoreInit = TRUE)

  invisible(NULL)
}

#### Per-session provider state ####

# Three things are tracked per session, and NONE of them lives in `r`:
#
#   wanted   - keys something has asked for, a reactiveVal so the observe wakes
#              on a new request
#   pending  - a navigation waiting for its data, likewise reactive
#   inflight / failed - plain environments, deliberately NOT reactive
#
# The last pair is the important distinction. Dispatch happens INSIDE the
# provider observe, so a reactive in-flight marker would invalidate the observe
# that just set it, and the observe would re-run and dispatch again. The plan
# writes this as r$.vftInflight[[key]]; it has to be non-reactive to work at all,
# and it is read only from inside that same observe, where a dependency would buy
# nothing.

.vftProviderStore <- function(session){
  if(is.null(session)) return(NULL)
  ud <- tryCatch(session$userData, error = function(e) NULL)
  if(is.null(ud)) return(NULL)
  if(is.null(ud$vftInflight)) ud$vftInflight <- new.env(parent = emptyenv())
  if(is.null(ud$vftFailed))   ud$vftFailed   <- new.env(parent = emptyenv())
  ud
}

.vftInflightSet <- function(session, name){
  ud <- .vftProviderStore(session)
  if(is.null(ud)) return(invisible(NULL))
  assign(name, Sys.time(), envir = ud$vftInflight)
  invisible(NULL)
}

.vftInflightClear <- function(session, name){
  ud <- .vftProviderStore(session)
  if(is.null(ud)) return(invisible(NULL))
  if(exists(name, envir = ud$vftInflight, inherits = FALSE))
    rm(list = name, envir = ud$vftInflight)
  invisible(NULL)
}

#' Is this provider already running? Lapses after VFT_PROVIDER_MAX_S - see the
#' note on that constant.
vftProviderInflight <- function(session, name){
  ud <- .vftProviderStore(session)
  if(is.null(ud)) return(FALSE)
  if(!exists(name, envir = ud$vftInflight, inherits = FALSE)) return(FALSE)
  since <- get(name, envir = ud$vftInflight, inherits = FALSE)
  if(as.numeric(difftime(Sys.time(), since, units = "secs")) >= VFT_PROVIDER_MAX_S){
    rm(list = name, envir = ud$vftInflight)
    return(FALSE)
  }
  TRUE
}

#' Record that a provider settled without producing what it promised.
#'
#' Without this the observe is a spin loop: the key is still missing, its needs
#' are still ready, nothing is in flight - so it dispatches again, forever, once
#' per reactive flush. One failed job would become an unbounded stream of them.
#' Cleared by vftInvalidate(), so changing the inputs is what earns a retry.
.vftFailedSet <- function(session, name){
  ud <- .vftProviderStore(session)
  if(is.null(ud)) return(invisible(NULL))
  assign(name, TRUE, envir = ud$vftFailed)
  invisible(NULL)
}

vftProviderFailed <- function(session, name){
  ud <- .vftProviderStore(session)
  if(is.null(ud)) return(FALSE)
  isTRUE(tryCatch(get(name, envir = ud$vftFailed, inherits = FALSE),
                  error = function(e) FALSE))
}

.vftFailedClear <- function(session){
  ud <- .vftProviderStore(session)
  if(is.null(ud)) return(invisible(NULL))
  rm(list = ls(ud$vftFailed, all.names = TRUE), envir = ud$vftFailed)
  invisible(NULL)
}

.vftWantedClear <- function(session){
  if(is.null(session)) return(invisible(NULL))
  wv <- tryCatch(session$userData$vftWanted, error = function(e) NULL)
  if(is.null(wv)) return(invisible(NULL))
  if(length(shiny::isolate(wv()))) wv(character(0))
  invisible(NULL)
}

#' Ask for some keys. Returns immediately, having blocked nothing.
#'
#' The work is not started here - it is started by the observe in
#' vftProviderServer(), which is also what starts the NEXT layer when this one
#' lands. Asking twice for the same key costs nothing.
vftEnsure <- function(r, keys, session = shiny::getDefaultReactiveDomain()){
  if(!length(keys)) return(invisible(FALSE))
  wv <- tryCatch(session$userData$vftWanted, error = function(e) NULL)
  if(is.null(wv)){
    vftDbg("vftEnsure(): no provider server on this session")
    return(invisible(FALSE))
  }
  cur <- shiny::isolate(wv())
  new <- union(cur, keys)
  if(!identical(new, cur)) wv(new)
  invisible(TRUE)
}

#' The app-level `r` the provider layer writes into.
#'
#' vftProviderServer() puts it here, beside vftWanted and vftPending, because
#' the two callers that need it are inside MODULES - step 5's simulation launch
#' and the newVersions page's context switch - and a module shadows `r` with its
#' own reactiveValues. `session$userData` is shared between a module session and
#' the root one, which makes this the same channel vftEnsure() already uses to
#' find the wanted-set.
#'
#' NULL when there is no provider server (tests, or a build without one), which
#' every caller has to handle: it is the same "no provider server on this
#' session" case vftEnsure() reports by returning FALSE.
vftAppReactives <- function(session = shiny::getDefaultReactiveDomain()){
  tryCatch(session$userData$vftAppR, error = function(e) NULL)
}

#' Ask for some keys and do something once they are there.
#'
#' vftEnsure() with a continuation, for the consumers that are not steps. A step
#' gets this for free - vftGoToStep() records a pending step and the provider
#' observe performs the navigation when the last key lands - and this is the same
#' mechanism for a caller that wants a VALUE rather than a page: the network for
#' a simulation, say, which no step's `needs` names any more.
#'
#' Nothing is awaited, in keeping with the rest of this file. When the keys are
#' already there `then()` runs in the SAME tick, which the newVersions page needs
#' (its caller bumps a render trigger and the map must not wait a flush);
#' otherwise it runs from the provider observe, on the flush the last key lands
#' in.
#'
#' `then` is called with no arguments - read the values off `r` yourself, so that
#' there is one answer to "where does this value live" rather than two.
#'
#' `onFail` runs instead of `then` if a provider along the chain gives up. It is
#' not optional in practice: every caller here disables a button before asking,
#' and without it a failed derivation leaves that button dead for the session.
#'
#' @return TRUE if the request was registered or satisfied, FALSE if there is no
#'   provider server to serve it.
vftEnsureThen <- function(keys, then, onFail = NULL,
                          session = shiny::getDefaultReactiveDomain()){
  r <- vftAppReactives(session)
  if(is.null(r)){
    vftDbg("vftEnsureThen(): no provider server on this session")
    return(invisible(FALSE))
  }

  if(all(vapply(keys, function(k) shiny::isolate(vftKeyReady(r, k)), logical(1)))){
    then()
    return(invisible(TRUE))
  }

  ud <- .vftProviderStore(session)
  if(is.null(ud)) return(invisible(FALSE))
  #`session` is recorded so the continuation can be run back under it. It is
  #registered from inside a MODULE and run from the provider observe, whose
  #domain is the APP session - and shinyjs::enable() and update*Input()
  #namespace against the domain, not against the session their caller can see.
  #The same trap, and the same fix, as vftModuleEnterFn() in R/modules.R.
  ud$vftThen <- c(ud$vftThen,
                  list(list(keys = keys, then = then, onFail = onFail,
                            domain = session)))

  vftDbg(paste0("ENSURE-THEN waiting on ", paste(keys, collapse = ", ")))
  vftEnsure(r, keys, session)
}

#' Run one waiter's callback under the domain it was registered from.
#'
#' isolate() for the second half of vftModuleEnterFn()'s reasoning: this runs
#' inside the provider observe(), which is not isolated, so a bare reactive read
#' in a continuation would make that observe depend on it and re-dispatch on
#' every change to it.
#'
#' Errors are caught - an exception here would take the provider observe down and
#' with it the whole session's navigation - but they are REPORTED, not swallowed:
#' the continuations are simulation launches and map redraws, and a silent
#' failure would look exactly like a hang.
.vftRunThenCallback <- function(fn, domain, what){
  if(!is.function(fn)) return(invisible(NULL))
  tryCatch(shiny::withReactiveDomain(domain, shiny::isolate(fn())),
           error = function(e)
             message("vftEnsureThen: ", what, " failed: ", conditionMessage(e)))
  invisible(NULL)
}

#' Run whatever vftEnsureThen() left waiting, for the keys that have arrived.
#'
#' Called from the provider observe, which is where readiness changes are seen.
#' A waiter whose keys can no longer be produced - a provider along the chain has
#' failed, or nothing derives them at all - is dropped rather than left pending,
#' because the caller is holding a disabled button on the strength of it.
#'
#' The list is taken and reset BEFORE anything is called: `then()` may register
#' another waiter (the second half of a two-stage prepare does exactly that), and
#' appending to a list this loop is iterating would lose it.
.vftRunThenWaiters <- function(r, session){
  ud <- .vftProviderStore(session)
  if(is.null(ud) || !length(ud$vftThen)) return(invisible(NULL))

  waiting <- ud$vftThen
  ud$vftThen <- NULL
  keep <- list()

  for(w in waiting){
    ready <- all(vapply(w$keys, function(k) vftKeyReady(r, k), logical(1)))
    if(ready){
      vftDbg(paste0("ENSURE-THEN ready: ", paste(w$keys, collapse = ", ")))
      .vftRunThenCallback(w$then, w$domain, paste(w$keys, collapse = ", "))
      next
    }
    stuck <- any(vapply(w$keys, function(k){
      nm <- vftKeyProvider(k)
      is.null(nm) || vftProviderFailed(session, nm)
    }, logical(1)))
    if(stuck){
      vftDbg(paste0("ENSURE-THEN gave up on ", paste(w$keys, collapse = ", ")))
      .vftRunThenCallback(w$onFail, w$domain, "onFail")
      next
    }
    keep <- c(keep, list(w))
  }

  ud$vftThen <- c(keep, ud$vftThen)
  invisible(NULL)
}

#' Drop every waiting continuation, running each one's onFail.
#'
#' vftInvalidate() calls this: a waiter registered against the state that has
#' just been torn down cannot be honoured, and silently forgetting it would leave
#' whatever button the caller disabled dead for the session.
.vftThenClear <- function(session){
  ud <- .vftProviderStore(session)
  if(is.null(ud) || !length(ud$vftThen)) return(invisible(NULL))
  waiting <- ud$vftThen
  ud$vftThen <- NULL
  for(w in waiting) .vftRunThenCallback(w$onFail, w$domain, "onFail")
  invisible(NULL)
}

#' Remember that a navigation is waiting for its data.
vftSetPendingStep <- function(session, step){
  pv <- tryCatch(session$userData$vftPending, error = function(e) NULL)
  if(is.null(pv)) return(invisible(FALSE))
  if(!identical(shiny::isolate(pv()), step)) pv(step)
  invisible(TRUE)
}

vftClearPendingStep <- function(session){
  pv <- tryCatch(session$userData$vftPending, error = function(e) NULL)
  if(is.null(pv)) return(invisible(FALSE))
  if(!is.null(shiny::isolate(pv()))) pv(NULL)
  invisible(TRUE)
}

#' Which providers can be started right now, for a set of wanted keys.
#'
#' Walks the closure needs-first, so a key three layers down contributes its
#' ancestors rather than itself. Reads readiness through vftKeyReady(), which is
#' what gives the calling observe its dependencies.
.vftRunnable <- function(r, keys, session){
  todo <- character(0)
  seen <- character(0)

  visit <- function(k){
    if(k %in% seen) return(invisible(NULL))
    seen <<- c(seen, k)
    if(vftKeyReady(r, k)) return(invisible(NULL))

    nm <- vftKeyProvider(k)
    if(is.null(nm)) return(invisible(NULL))            #a human has to make this one
    if(vftProviderFailed(session, nm)) return(invisible(NULL))

    prov <- VFT_PROVIDERS[[nm]]
    for(n in prov$needs) visit(n)

    ready <- !length(prov$needs) ||
      all(vapply(prov$needs, function(n) vftKeyReady(r, n), logical(1)))
    if(ready && !vftProviderInflight(session, nm)) todo <<- union(todo, nm)
    invisible(NULL)
  }

  for(k in keys) visit(k)
  todo
}

#' Write a provider's output into `r`.
.vftAssignProvided <- function(r, out){
  if(!is.list(out) || !length(out)) return(invisible(character(0)))
  nms <- names(out)
  nms <- nms[!is.na(nms) & nzchar(nms)]
  for(k in nms) r[[k]] <- out[[k]]
  invisible(nms)
}

#' Start one provider.
.vftDispatchProvider <- function(r, name, session){
  prov <- VFT_PROVIDERS[[name]]
  if(is.null(prov)) return(invisible(NULL))

  vals <- stats::setNames(
    lapply(prov$needs, function(k) shiny::isolate(r[[k]])), prov$needs)

  #--- synchronous: cheap enough that a dispatch would cost more than the work
  if(!isTRUE(prov$async)){
    out <- tryCatch(prov$provide(vals, NULL),
                    error = function(e){
                      message("vftProvider: ", prov$label, " failed: ",
                              conditionMessage(e))
                      NULL
                    })
    if(is.null(out)){
      .vftFailedSet(session, name)
    }else{
      .vftAssignProvided(r, out)
    }
    return(invisible(NULL))
  }

  #--- asynchronous
  vftDbg(paste0("PROVIDE ", name, " -> ", paste(prov$keys, collapse = ", ")))

  #vftProgress, not ipc::AsyncProgress: the latter drags the whole session into
  #the worker. See R/async_helpers.R.
  progress <- vftProgress(message = prov$label,
                          detail  = "Daten werden vorbereitet...",
                          queue   = ipc::shinyQueue(),
                          millis  = 1000)

  .vftInflightSet(session, name)

  p <- tryCatch(prov$provide(vals, progress), error = function(e) e)
  if(inherits(p, "condition")){
    .vftInflightClear(session, name)
    .vftFailedSet(session, name)
    vftClearPendingStep(session)
    vftAsyncError(progress, prov$label)(p)
    return(invisible(NULL))
  }

  p <- promises::then(p, function(out){
    try(progress$close(), silent = TRUE)
    .vftAssignProvided(r, out)
    NULL
  })

  p <- promises::catch(p, function(e){
    #a stranded pending navigation is worse than the failure itself: the user is
    #left on the previous step with no way to ask again.
    vftClearPendingStep(session)
    .vftFailedSet(session, name)
    vftAsyncError(progress, prov$label)(e)
    NULL
  })

  promises::finally(p, function(){
    .vftInflightClear(session, name)
    #a job that settled without filling its keys must not be dispatched again on
    #the next flush - see .vftFailedSet().
    #
    #isolate() is load-bearing: this runs on later()'s event loop, outside any
    #reactive consumer, where a bare r[[k]] read is the fatal "Can't access
    #reactive value" error rather than a stale one. Same trap as the nav bar's
    #seeding read in R/navigation.R.
    still <- shiny::isolate(
      prov$keys[!vapply(prov$keys, function(k) vftKeyReady(r, k), logical(1))])
    if(length(still)){
      .vftFailedSet(session, name)
      vftClearPendingStep(session)
    }
  })

  invisible(NULL)
}

#' The one observe that runs the whole provider layer.
#'
#' Called once from app_server(), after vftNavInit(). Everything the layer does
#' happens here:
#'
#'   * dispatch whatever is currently runnable for the wanted set,
#'   * run whatever vftEnsureThen() left waiting on keys that have now arrived,
#'   * and, when a deferred navigation's step has become available, perform it.
#'
#' It re-runs because .vftRunnable() reads the readiness of every key in the
#' closure, so the assignment a resolved provider makes into `r` is itself the
#' signal to start the next layer. There is no polling and no completion
#' bookkeeping beyond the in-flight markers.
vftProviderServer <- function(r, session = shiny::getDefaultReactiveDomain()){
  session$userData$vftWanted  <- shiny::reactiveVal(character(0))
  session$userData$vftPending <- shiny::reactiveVal(NULL)
  #the handle vftEnsureThen() reaches this `r` through, from inside a module that
  #has shadowed the name. See vftAppReactives().
  session$userData$vftAppR    <- r

  shiny::observe({
    wanted  <- session$userData$vftWanted()
    pending <- session$userData$vftPending()

    if(!is.null(pending)) wanted <- union(wanted, VFT_STEPS[[pending]]$needs)

    if(length(wanted)){
      for(nm in .vftRunnable(r, wanted, session)) .vftDispatchProvider(r, nm, session)

      #before the clear below, so that a waiter whose last key has just landed is
      #run on this flush rather than on a later one that may never come.
      .vftRunThenWaiters(r, session)

      #stop wanting things that have arrived, so that a later vftInvalidate()
      #does not find a stale request and immediately rebuild what it just threw
      #away.
      if(all(vapply(wanted, function(k) vftKeyReady(r, k), logical(1))))
        .vftWantedClear(session)
    }

    if(!is.null(pending) && vftStepAvailable(r, pending)){
      session$userData$vftPending(NULL)
      vftGoToStep(r, pending, session, check = FALSE)
    }
  })

  invisible(NULL)
}
