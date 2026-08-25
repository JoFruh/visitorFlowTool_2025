#### The step registry ####

# Until now nothing in the app knew what a step needs. `r$step` was written on
# every transition and read by exactly one thing - the save file's name - and
# the restore path's if/else ladder. There is no conditionalPanel anywhere in
# the repo and only two req() calls, both about file uploads, so a step whose
# inputs do not exist is reached the same way as one whose inputs do: it is
# built, its module reads NULL, and it fails somewhere in the middle.
#
# This file is the one place that says, per step, which `r$` keys have to exist
# before it can run. That is deliberately the ONLY form the answer takes: the
# `needs` are key names, so readiness is a NULL test against the live `r`, and
# there is no second copy of "how far the user has got" to keep in sync with the
# first. The restore path gets gating for free - whatever it rehydrates into
# `r$` is, by construction, ready.
#
# Stage 4 hangs lazy providers off exactly these names: a key becomes available
# either because something already put it in `r$`, or because a provider can
# derive it. vftKeyReady() below is the single place that distinction lands.

#' The steps, in the order the user walks them.
#'
#' `tab` is the tabPanel value in the hidden tabsetPanel built by `app_ui()`.
#'
#' `code` is what gets written to `r$step`, which is the number the save file
#' records and the restore path switches on. It is NOT the position in this
#' list: `newVersions` is a side trip off step 5 and deliberately leaves
#' `r$step` alone, so that a save taken there restores to step 5.
#'
#' `label` is the nav bar button text. Plain German rather than i18n$t(): the
#' bar is static markup built once per session (that is the point of it - see
#' vftStepNav() in R/app_ui.R), so it cannot follow the language selector
#' without becoming an output again, which is the cost this whole stage exists
#' to avoid.
#'
#' `needs` are `r$` key names. A step is available when every one of them is
#' ready; see vftKeyReady() for what "ready" means for the two keys where it is
#' not simply "not NULL".
VFT_STEPS <- list(
  step1       = list(tab = "tab_step1",       code = 1L,
                     label = "1 Gebiet",
                     needs = character(0)),
  step2       = list(tab = "tab_step2",       code = 2L,
                     label = "2 Sensibilität",
                     needs = "shape"),
  step3       = list(tab = "tab_step3",       code = 3L,
                     label = "3 Interessengebiete",
                     needs = c("shape", "DULN_all")),
  step4       = list(tab = "tab_step4",       code = 4L,
                     label = "4 Wegnetz",
                     needs = c("shape", "network", "networkNodes",
                               "DULN", "DULN_all", "minThresh")),
  step5       = list(tab = "tab_step5",       code = 5L,
                     label = "5 Simulation",
                     needs = c("networkList", "SM_pres", "SMcolors",
                               "shape", "species", "minCutThresh")),
  newVersions = list(tab = "tab_newVersions", code = NA_integer_,
                     label = "Neue Versionen",
                     needs = c("networkList", "SM_pres", "SMcolors",
                               "finalPolygons", "DULN", "shape")),
  finalStep   = list(tab = "tab_finalStep",   code = 6L,
                     label = "Resultate",
                     needs = c("networkList", "versionsUI",
                               "SM_pres", "shape", "finalPolygons"))
)

#' Which step produces each key.
#'
#' Only used to write the nav bar tooltips ("... benoetigt: 3 Interessengebiete"),
#' which is why it can be a flat static map: it answers "what would I have to go
#' and do first", not "where did this value come from this time".
VFT_KEY_SOURCE <- c(
  shape         = "step1",
  network       = "step1",
  networkNodes  = "step1",
  DULN          = "step1",
  DULN_all      = "step1",
  SM_pres       = "step2",
  SMcolors      = "step2",
  species       = "step2",
  minCutThresh  = "step2",
  minThresh     = "step3",
  networkList   = "step4",
  finalPolygons = "step4",
  versionsUI    = "step5"
)

#' Readiness tests for the keys where "not NULL" is the wrong question.
#'
#' Keys absent from this list use the default `!is.null(r[[key]])`.
#'
#' `networkNodes` is not an `r$` key at all today: the node table is built inside
#' sf_to_tidygraph3() and arrives already attached to `r$network`. Stage 4 splits
#' it out into a provider of its own, and old save files carry an
#' `envBase_network` with the columns baked in either way - so the honest test is
#' for the columns, not for a key, and it keeps working unchanged on both sides
#' of that split. igraph::vertex_attr_names() rather than
#' vftGraphTibble(r$network, "nodes") because this runs inside the nav bar's
#' observe on every change to `r` and only the names are wanted: the names call
#' allocates nothing, assembling the tibble walks every column.
#'
#' `minThresh` is the step-3 slider, and step 3 has a skip button that goes
#' straight to step 4 without ever setting it (the `step3return$isSkip()`
#' branches in app_server.R). A user who took that path HAS met step 4's
#' prerequisites; a NULL test would call them unmet and dark out the button they
#' just came through.
VFT_KEY_READY <- list(
  networkNodes = function(r){
    if(is.null(r$network)) return(FALSE)
    all(c("Residents", "DULN_WALK_") %in% igraph::vertex_attr_names(r$network))
  },
  minThresh = function(r){
    if(!is.null(r$minThresh)) return(TRUE)
    isTRUE(as.logical(r$isSkip))
  }
)

#' Is the step nav bar switched on?
#'
#' Off by default, following the VFT_GL / VFT_RPROF / VFT_DEBUG convention. With
#' it off, vftStepNav() renders nothing and vftNavBarServer() wires nothing, so
#' the app behaves exactly as it did before this stage: the only way between
#' steps stays the confirm buttons.
#'
#' `VFT_NAV=1` turns the whole bar on. Anything else non-"0" is read as a
#' comma-separated list of step names, which turns the bar on with only those
#' buttons live: `VFT_NAV=step1,step3`. That is the Stage 5 rollout switch -
#' re-entering a step rebuilds its module server until each one is converted to a
#' first-touch singleton, so steps get let in one at a time as they are converted
#' rather than all at once.
vftNavEnabled <- function(){
  !identical(Sys.getenv("VFT_NAV", "0"), "0")
}

#' Which steps the nav bar is allowed to navigate to.
#'
#' Availability (`vftStepAvailable()`) is about the data: has this step got its
#' inputs. This is about the rollout: is this step's module ready to be
#' re-entered at all. Both have to say yes.
vftNavSteps <- function(){
  raw <- Sys.getenv("VFT_NAV", "0")
  if(identical(raw, "0"))  return(character(0))
  if(identical(raw, "1"))  return(names(VFT_STEPS))
  steps <- trimws(strsplit(raw, ",", fixed = TRUE)[[1]])
  steps[steps %in% names(VFT_STEPS)]
}

#' Is this step one the nav bar may go to?
vftNavAllows <- function(step){
  step %in% vftNavSteps()
}

#' The input id of one nav bar button. Unnamespaced - the bar lives at app level.
vftNavInputId <- function(step){
  paste0("vftNav_", step)
}

#' Is one `r$` key ready to be read?
#'
#' Stage 4 extends this with "...or a provider can derive it, and that provider's
#' own needs are satisfiable". Everything that asks about readiness asks here, so
#' that is a change in one function rather than at every call site.
vftKeyReady <- function(r, key){
  test <- VFT_KEY_READY[[key]]
  if(is.null(test)) return(!is.null(r[[key]]))
  isTRUE(test(r))
}

#' The keys a step needs and does not have, in `needs` order.
vftStepMissing <- function(r, step){
  needs <- VFT_STEPS[[step]]$needs
  if(length(needs) == 0) return(character(0))
  needs[!vapply(needs, function(k) vftKeyReady(r, k), logical(1))]
}

#' Can this step be entered?
#'
#' Not a reactive itself, but it reads `r`, so calling it inside an observe()
#' takes a dependency on every key it touches - which is what makes the nav bar
#' light up on its own when a step's last missing input arrives.
vftStepAvailable <- function(r, step){
  length(vftStepMissing(r, step)) == 0
}

#' The steps that have to be done before this one, as labels.
#'
#' Static: derived from `needs` and VFT_KEY_SOURCE, not from the session state,
#' so the tooltip can be baked into the markup at UI build time and the bar stays
#' free of outputs.
vftStepPrereqLabels <- function(step){
  needs   <- VFT_STEPS[[step]]$needs
  if(length(needs) == 0) return(character(0))
  sources <- unique(VFT_KEY_SOURCE[needs])
  sources <- sources[!is.na(sources) & sources != step]
  if(length(sources) == 0) return(character(0))
  #back into registry order, so a tooltip reads "2 ..., 3 ..." not "3 ..., 2 ..."
  sources <- names(VFT_STEPS)[names(VFT_STEPS) %in% sources]
  vapply(sources, function(s) VFT_STEPS[[s]]$label, character(1), USE.NAMES = FALSE)
}

#' The tooltip for one nav bar button.
vftStepTooltip <- function(step){
  prereq <- vftStepPrereqLabels(step)
  if(length(prereq) == 0) return(VFT_STEPS[[step]]$label)
  paste0(VFT_STEPS[[step]]$label, " – benötigt: ",
         paste(prereq, collapse = ", "))
}
