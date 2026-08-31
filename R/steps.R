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
#' records and the restore path resolves back to a step name through
#' vftStepForCode(). It is NOT the position in this list: `newVersions` is a side
#' trip off step 5 and deliberately leaves `r$step` alone, so that a save taken
#' there restores to step 5 - and loses nothing by it, because what that page
#' produces is mirrored into `networkList` / `versionsUI` as it is made.
#'
#' newVersions is now the LAST step. `finalStep` - the "Resultate" page, module
#' lastStep_server - was removed on 2026-08-27: it had gone non-functional. Its
#' code 6 is not reissued; see vftStepForCode() for what a save carrying it does.
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
                     label = "1 Gebiet wählen",
                     needs = character(0)),
  step2       = list(tab = "tab_step2",       code = 2L,
                     label = "2 Sensibilität der Biodiversität",
                     needs = "shape"),
  step3       = list(tab = "tab_step3",       code = 3L,
                     label = "3 ZG definieren",
                     needs = c("shape", "DULN_all")),
  #`network` and `networkNodes` are NOT here, despite the label. Step 4 draws and
  #edits AOI polygons; it does not read a single node or edge to do it. The map
  #stopped drawing the paths as a backdrop (step4_server.R), and generateAoI2()
  #works entirely on the DULN rasters - it took `network` as an argument for
  #years and never touched it. Listing them meant that merely OPENING step 4
  #dispatched the ~30s GDB read and tbl_graph build, on every walk through the
  #app, for data nothing on the page would look at.
  #
  #The path network is loaded at the two places that read paths: the simulation
  #launch and the newVersions page's edit contexts, both through
  #vftPrepareThen() in R/prepare_network.R. It is cached in `r$network` from then
  #on, so it is loaded once per perimeter and never again.
  step4       = list(tab = "tab_step4",       code = 4L,
                     label = "4 ZG bearbeiten",
                     needs = c("shape", "DULN", "DULN_all", "minThresh")),
  #`minThresh` is new on both of these. The attractivity-weighted edge distances
  #used to be baked into the network by step 4's confirm; they are computed when
  #a simulation is first launched now (R/prepare_network.R), so both pages
  #genuinely read the threshold. VFT_KEY_READY$minThresh below already answers
  #TRUE on step 3's skip path, so adding it darkens nothing.
  #`SM_pres`, `SMcolors`, `species` and `minCutThresh` are NOT here, and that is
  #the whole of "step 2 is optional". Every one of them is step 2's, and step 2
  #is skippable by construction: step 3 needs only the perimeter and a crop
  #derived from it, so the bar offers step 3 the moment step 1 is confirmed and
  #nothing between there and here asks for a sensitivity matrix.
  #
  #Listing them made the walk 1 -> 3 -> 4 -> 5 a trap. Step 4's confirm moves
  #the user on with `check = FALSE`, so they ARRIVED at a step 5 that the nav
  #bar graded unreachable: highlighted as current, greyed, refusing every click
  #back into it, with "Neue Versionen" dark beside it for the same two keys.
  #
  #They are out because they are not what step 5 IS. A simulation is a function
  #of the network - see the long note on `pathUsage` in VFT_DERIVED_FROM - and
  #every consumer of the matrix on this page and the next is a display consumer
  #behind one checkbox. So the step opens, runs and simulates without them, and
  #the two controls that need them say so: the checkbox offers to go and make
  #one (step5_server.R's smAskCreate()), and newVersions already branches on
  #`is.null(SM_pres)` at each of its three draw sites.
  step5       = list(tab = "tab_step5",       code = 5L,
                     label = "5 Naherholung simulieren",
                     needs = c("networkList", "shape", "minThresh")),
  newVersions = list(tab = "tab_newVersions", code = NA_integer_,
                     label = "Szenarien erstellen",
                     needs = c("networkList", "finalPolygons", "DULN", "shape",
                               "minThresh"))
)

#' How the nav bar groups its buttons, left to right.
#'
#' Purely cosmetic - a thick white separator (see vftStepNav() in R/app_ui.R)
#' goes between these groups and nowhere else. Steps 3-5 and newVersions sit
#' together because they all work the same perimeter once it exists: everything
#' downstream of the outline that leads to, and comes out of, a simulation.
#'
#' newVersions used to be a group of its own, with a separator in front of it.
#' It is plugged into the right-hand end of the chevron chain instead - it is
#' the last thing a user does with a simulation, not a destination beside it.
#' Hitzeminderung is still separate, and still added by hand after the loop: it
#' is not a VFT_STEPS entry, so this registry has nothing to hold for it.
VFT_NAV_GROUPS <- list("step1", "step2",
                       c("step3", "step4", "step5", "newVersions"))

#### Folding the simulation half of the bar ####

# Seven destinations at once is the whole depth of the tool shown to someone who
# has not drawn anything yet. Four of the seven are one piece of work, and they
# are exactly the group above, so the bar ships with that group FOLDED behind a
# single button and offers four choices to start with:
#
#   Gebiet waehlen | Sensibilitaet der Biodiversitaet |
#   Naherholung simulieren | Hitzeminderung
#
# Clicking the folded button unfolds the chain and enters its first reachable
# member (step 3 in practice). It folds back only when step 1 commits a NEW
# outline - that write discards everything downstream, so the walk starts again.
#
# Nothing about this is an output. Both the folded button and the four chevrons
# are always in the markup; ONE class on #vftNav decides which of them is
# displayed. See vftStepNav() in R/app_ui.R and vftNavBarServer() in
# R/navigation.R, which is the only writer of that class.

#' The one group the bar can fold up.
#'
#' Compared with `identical()` against the entries of VFT_NAV_GROUPS, so the two
#' cannot drift: change the group there without changing this and the group is
#' simply never folded, rather than half-folded.
VFT_NAV_FOLD <- c("step3", "step4", "step5", "newVersions")

#' The input id of the button that stands in for the folded group.
VFT_NAV_FOLD_ID <- "vftNav_sim"

#' Whose label and translations that button borrows.
#'
#' step5's, i.e. "Naherholung simulieren" - the umbrella name for what the
#' whole group does, and a `:nav_step5:` row the translation CSVs already
#' carry, so folding the bar needs no new i18n string. The button is NOT step 5: clicking it enters
#' vftNavFoldTarget(), which is normally step 3.
VFT_NAV_FOLD_LABEL <- "step5"

#' Which hidden per-module input each step's banner controls actually are.
#'
#' The nav bar's language select / help / info controls are unnamespaced and
#' rendered once, at app level - see vftStepNav(). Each step's own server still
#' listens for its OWN namespaced languageSelect_N / helpButtonN / infoButtonN,
#' completely unchanged; this map is what lets vftNavBannerProxyServer() (in
#' R/navigation.R) find the right hidden proxy to drive for whichever step is
#' current. The suffixes themselves are not a pattern (newVersions is
#' `languageSelect_7` but `helpButton6` / `infoButton6`) because they were never
#' meant to be read as one - they are just what each step module happened to be
#' called historically.
VFT_BANNER_PROXY <- list(
  step1       = list(lang = "languageSelect_1", help = "helpButton1", info = "infoButton1"),
  step2       = list(lang = "languageSelect_2", help = "helpButton2", info = "infoButton2"),
  step3       = list(lang = "languageSelect_3", help = "helpButton3", info = "infoButton3"),
  step4       = list(lang = "languageSelect_4", help = "helpButton4", info = "infoButton4"),
  step5       = list(lang = "languageSelect_5", help = "helpButton5", info = "infoButton5"),
  newVersions = list(lang = "languageSelect_7", help = "helpButton6", info = "infoButton6")
)

#' Which step produces each key.
#'
#' Only used to write the nav bar tooltips ("... benoetigt: 3 Interessengebiete"),
#' which is why it can be a flat static map: it answers "what would I have to go
#' and do first", not "where did this value come from this time".
#' Also read backwards, by vftStepProduces() in R/providers.R. That reading used
#' to be load-bearing - a backward move discarded everything downstream of what
#' this map attributed to the step being returned to - and it is not any more:
#' the discard happens at the write, and vftCommit() is handed the actual new
#' values rather than inferring them from here. What the backward reading is
#' still for is keeping this map honest: vftCommit() traces any key attributed to
#' a step that the step's confirm handler did not write. `shapeLarger` is listed
#' for that second reading only - it is in no step's `needs`, because nothing but
#' a provider ever reads it.
VFT_KEY_SOURCE <- c(
  shape         = "step1",
  shapeLarger   = "step1",
  network       = "step1",
  networkNodes  = "step1",
  DULN          = "step1",
  DULN_all      = "step1",
  SM_pres       = "step2",
  SMcolors      = "step2",
  species       = "step2",
  minCutThresh  = "step2",
  #The rest of what step 2 writes when it confirms (app_server.R's step-2 confirm
  #handler sets every one of these). They are listed for the BACKWARD reading of
  #this map only - none of them is in any step's `needs`, so no tooltip changes.
  #
  #Without them they counted as dependents of `species` rather than as step 2's
  #own output, so going back to step 2 discarded the very selection the user was
  #going back to look at: leave again without re-confirming and the app - and the
  #next save file - had a NULL species selection while the screen showed one.
  filterList      = "step2",
  checkboxSave    = "step2",
  groupSave_all   = "step2",
  groupSave_sens  = "step2",
  groupSave_type  = "step2",
  groupSave_class = "step2",
  weightInputs    = "step2",
  weightNames     = "step2",
  toSelectSpAfter = "step2",
  minThresh     = "step3",
  isSkip        = "step3",
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
    #tryCatch because this runs inside the nav bar's observe, on every change to
    #`r`. igraph::vertex_attr_names() aborts outright on anything that is not a
    #graph, and an abort there does not fail the TEST - it kills the observe and
    #takes the whole session's navigation with it. A save file carrying something
    #unexpected under envBase_network should mean "not ready", not "no nav bar".
    isTRUE(tryCatch(
      all(c("Residents", "DULN_WALK_") %in% igraph::vertex_attr_names(r$network)),
      error = function(e) FALSE))
  },
  minThresh = function(r){
    if(!is.null(r$minThresh)) return(TRUE)
    isTRUE(as.logical(r$isSkip))
  },
  #' A list of scenarios, not a canvas.
  #'
  #' The Hitzeminderung door opens on the perimeter alone, and the newVersions
  #' page then seeds a baseline "Original" so the page has a card and the paint a
  #' place to be copied from - see enter() there. It is mirrored into
  #' `r$networkList` like any other scenario, which is what makes the painting
  #' survive a trip to another step and back, and it carries `heatOnly = TRUE` so
  #' that this test can tell it apart from the real thing.
  #'
  #' Without the distinction a heat-only session that then SKIPPED step 3 (which
  #' satisfies `minThresh` without producing anything) would light step 5 up:
  #' `networkList` present, `shape` present, `minThresh` answered - and step 5
  #' would go on to prepare a scenario against a NULL `finalPolygons`, which is
  #' an error inside a future rather than a message on screen. The two steps that
  #' name this key want scenarios; a canvas is not one.
  #Both halves: an ABSENT or empty list is not a canvas either, and it is not a
  #list of scenarios - it is nothing.
  networkList = function(r){
    nl <- r$networkList
    is.list(nl) && length(nl) > 0 && !vftIsCanvasList(nl)
  }
)

#' Is this scenario list a CANVAS - somewhere to paint - rather than scenarios?
#'
#' TRUE for the list the newVersions page runs on when it was entered through the
#' Hitzeminderung door: every entry tagged `heatOnly = TRUE`, no network, no areas
#' of interest, nothing simulated. FALSE for an empty or absent list, which is not
#' a canvas either - it is nothing.
#'
#' One function rather than two copies of the test, because two rules ask it and
#' they must never disagree: VFT_KEY_READY above (a canvas does not make step 5
#' reachable) and vftInvalidateKeep() in R/providers.R (a canvas is not discarded
#' by the steps that produce the path network and the Zielgebiete, because it was
#' not derived from either).
#'
#' `is.list()` per entry before `$`: `$` aborts on an atomic, and both callers run
#' inside observers where an abort takes the whole session's navigation with it
#' rather than answering "no".
vftIsCanvasList <- function(nl){
  if(!is.list(nl) || !length(nl)) return(FALSE)
  all(vapply(nl, function(v) is.list(v) && isTRUE(v$heatOnly), logical(1)))
}

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

#' Which steps have been CONVERTED to first-touch singletons.
#'
#' One name, three consequences, which is what makes the conversion safe to do
#' one module at a time:
#'
#'   1. `vftModuleOnce()` (R/modules.R) reuses the module built on the first
#'      visit instead of constructing another one beside it;
#'   2. `vftGoToStep()` calls that module's `enter()` closure on every return;
#'   3. the nav bar offers the step and vftGoToStep(check = TRUE) allows it.
#'
#' A step NOT listed here keeps the old behaviour exactly: its module server is
#' called again on every visit, so the nav bar refuses to send the user back to
#' it. That is not a restriction for its own sake - re-entering an unconverted
#' step builds a SECOND module beside the live one, and both then answer the same
#' confirm button. The older instance holds the plain values it captured at
#' construction, so it writes the network it froze BEFORE the user changed the
#' area of interest back into `r$`: observed live as two "Original" scenarios in
#' step 5, one simulated against an area that was no longer on screen.
#'
#' **As of 2026-08-26 the list is complete: every module is converted**, so
#' nothing takes the unconverted branch any more. The branch is kept rather than
#' deleted - it is what makes vftModuleOnce() safe to read, and removing a step
#' from this vector is still the way to isolate one if a conversion has to be
#' backed out.
#'
#' Conversion order was smallest first - step3, step4, step2, step5, step1,
#' newVersions, lastStep - so the mechanism was proved on 400 lines before it
#' was applied to 3900. lastStep is gone (2026-08-27, non-functional); the six
#' that are left are the six steps there are.
#'
#' step1 was taken out of turn because the user asked for the return, and it is
#' the odd one: its module is built at SESSION START by its own
#' `ignoreNULL = FALSE` observer rather than by a navigation, so it is the only
#' step whose module exists before its visit counter has ever moved. That is why
#' vftStepEntered() special-cases it - and why, before this, the nav bar greyed
#' step 1 out from the first flush onwards.
#'
#' Note this restricts the NAV BAR and the module cache only. The app's own
#' transitions (`vftGoToStep(check = FALSE)`) still reach any step, which is what
#' let step 5 and newVersions keep bouncing between each other while neither was
#' converted. Both ends of that bounce are singletons now, which closes the last
#' rebuild on the busiest path in the app.
VFT_REENTRANT_STEPS <- c("step1", "step2", "step3", "step4", "step5",
                         "newVersions")

#' May the nav bar return to this step after it has been built once?
vftStepReentrant <- function(step){
  step %in% VFT_REENTRANT_STEPS
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

#' Can this step be REACHED - now, or after something has been derived?
#'
#' The distinction Stage 4 introduces, and the one the nav bar asks about. A step
#' whose last missing input has a provider is reachable but not yet available:
#' the button is live, clicking it starts the derivation, and vftGoToStep() holds
#' the navigation until the value lands. A step missing something no provider can
#' make - `minThresh`, which is a slider a human has to move - is neither, and
#' its button stays dark with a tooltip naming the step that produces it.
#'
#' vftKeyDerivable() lives in R/providers.R, which is where the answer to "or can
#' it be derived" is defined; this is only the per-step form of it.
vftStepReachable <- function(r, step){
  missing <- vftStepMissing(r, step)
  if(length(missing) == 0) return(TRUE)
  all(vapply(missing, function(k) vftKeyDerivable(r, k), logical(1)))
}

#' The first member of the folded group this session could actually enter.
#'
#' Two answers in one: it is where the folded button navigates to, and - through
#' `is.null()` on the result - whether that button is enabled at all. One
#' function so those two can never disagree, the way vftIsCanvasList() is one
#' function for its two callers.
#'
#' In practice this is step 3 the moment the perimeter exists: step 3 needs
#' `shape` and `DULN_all`, and the second is derivable from the first, so
#' "reachable" here is exactly the user's "once step 1 is complete". Later
#' members are candidates only for a build whose VFT_NAV list leaves step 3 out.
#'
#' Reads `r`, so calling it inside an observe takes a dependency on those keys -
#' which is what makes the folded button light up on its own, the same way every
#' other button in the bar does.
vftNavFoldTarget <- function(r, steps = vftNavSteps()){
  cand <- VFT_NAV_FOLD[VFT_NAV_FOLD %in% steps]
  for(s in cand) if(vftStepReachable(r, s)) return(s)
  NULL
}

#### The Hitzeminderung door ####

# Hitzeminderung is not a step - it is a second door into the newVersions page
# that arrives with contextChoice preset to 4 (see vftStepNav() in R/app_ui.R and
# the vftNav_hitze observer in R/navigation.R). It used to mirror newVersions'
# own reachability, which is the wrong answer: newVersions `needs` the path
# network, the confirmed areas of interest and the step-3 threshold because its
# two EDIT contexts read them, and context 4 reads none of the three. It paints a
# land cover baseline derived from the step-1 perimeter and nothing else - which
# is why R/prepare_network.R deliberately exempts context 4 from the preparation
# in the first place.
#
# So the door opens on the perimeter alone. The page then runs in what
# newVersions_server.R calls heat-only mode: a baseline "Original" scenario and
# nothing else, so the user adds a version of their own to paint on (Original is
# read-only here exactly as it is in the two edit contexts), and those contexts
# offer to go and produce what they need rather than being drawn dark.

#' What the Hitzeminderung door needs, as opposed to what newVersions needs.
#'
#' Key names, read the same way `needs` is - through vftKeyReady() - so this
#' stays one list rather than a second copy of "has the user got a perimeter".
VFT_HITZE_NEEDS <- "shape"

#' Can the Hitzeminderung door be opened - now, or after something is derived?
#'
#' The vftStepReachable() test, against VFT_HITZE_NEEDS instead of a step's
#' `needs`. Reads `r`, so calling it inside an observe() takes a dependency on
#' `shape` and the button lights up on its own when step 1 confirms.
vftHitzeReachable <- function(r){
  missing <- VFT_HITZE_NEEDS[
    !vapply(VFT_HITZE_NEEDS, function(k) vftKeyReady(r, k), logical(1))]
  if(length(missing) == 0) return(TRUE)
  all(vapply(missing, function(k) vftKeyDerivable(r, k), logical(1)))
}

#' The steps that have to be done before the Hitzeminderung door opens.
#'
#' vftStepPrereqSteps() for VFT_HITZE_NEEDS, so the button's tooltip says
#' "benötigt: 1 Gebiet" rather than repeating newVersions' three prerequisites
#' under a label that does not need them.
vftHitzePrereqSteps <- function(){
  sources <- unique(VFT_KEY_SOURCE[VFT_HITZE_NEEDS])
  sources <- sources[!is.na(sources)]
  if(length(sources) == 0) return(character(0))
  names(VFT_STEPS)[names(VFT_STEPS) %in% sources]
}

#' How far through the walk a step sits.
#'
#' Only used to tell "going back" from "going on", which is the question that
#' decides whether a move can invalidate downstream results. newVersions shares
#' step 5's rank because it is a side trip off it in both directions, and moving
#' between the two must never count as going back.
VFT_STEP_RANK <- c(step1 = 1L, step2 = 2L, step3 = 3L, step4 = 4L,
                   step5 = 5L, newVersions = 5L)

#' Is `to` behind `from`?
vftStepIsBack <- function(from, to){
  if(length(from) != 1L || length(to) != 1L) return(FALSE)
  #[[ ]] on a name this vector does not carry is an error, not NA
  if(!(from %in% names(VFT_STEP_RANK)) || !(to %in% names(VFT_STEP_RANK)))
    return(FALSE)
  VFT_STEP_RANK[[to]] < VFT_STEP_RANK[[from]]
}

#### The restore path ####

# Stage 6. A save file records one number - `envBase_step`, which is whatever
# `code` the step the user was on carries - and restoring used to be an if/else
# ladder of five hand-written branches, one per number, with no branch at all for
# the last step and one branch (`r$step == 1`) that bumped a reactiveVal nothing
# observed. The two functions below replace the ladder: one turns the number back
# into a step name, the other decides whether that step can actually be entered.
#
# The second question is the one the ladder never asked. A save is a snapshot of
# `r`, and the registry already says, per step, which keys have to be in `r`
# before it can run - so "resume where you were" and "resume somewhere the module
# will not read NULL" are different answers, and only the second one is safe.
# Getting it wrong is not a caught error: the module is built, reads NULL, and
# fails somewhere in the middle.

#' The step a save file's `r$step` code means, or NULL if it means nothing.
#'
#' Codes are the `code` field of VFT_STEPS, NOT positions - see the note there.
#' newVersions has no code, so nothing ever resolves to it: a save taken on that
#' page carries step 5's code, which is what it should resume at anyway.
#'
#' A code ABOVE the highest one issued resolves to the last step rather than to
#' NULL. That is not an old-file concession - those are off the table - it is the
#' honest answer to "the user was as far on as it is possible to be": codes 6, 7
#' and 8 were the Resultate page and its variants, which no longer exist, and the
#' furthest a session can now get is step 5. Resolving to NULL instead would send
#' someone who had finished a simulation back to step 1.
vftStepForCode <- function(code){
  if(length(code) != 1L) return(NULL)
  code <- suppressWarnings(as.integer(code))
  if(is.na(code)) return(NULL)

  codes <- vapply(VFT_STEPS, function(s) s$code, integer(1))
  codes <- codes[!is.na(codes)]
  if(!length(codes)) return(NULL)

  hit <- names(codes)[codes == code]
  if(length(hit)) return(hit[[1]])
  if(code > max(codes)) return(names(codes)[[which.max(codes)]])
  NULL
}

#' Where a restored session should actually resume.
#'
#' The step the save NAMES, if its inputs are there or can be derived; otherwise
#' the furthest step before it that can be. Reachable rather than available on
#' purpose, and that is the whole of the new capability Stage 6 buys: a save
#' carrying nothing but `shape` names step 2, step 2 needs only `shape`, and
#' everything past it - the buffered perimeter, the attractiveness crop, the path
#' network - is derived by the provider layer when a step that reads it is
#' entered rather than rebuilt eagerly on the way in.
#'
#' Only steps with a `code` are candidates: newVersions is a side trip and is
#' reached from step 5, never restored into.
#'
#' Reads `r`. Call it from an isolated context (the restore handler is one).
vftRestoreStep <- function(r){
  walk <- names(VFT_STEPS)[
    !vapply(VFT_STEPS, function(s) is.na(s$code), logical(1))]
  if(!length(walk)) return(NULL)

  target <- vftStepForCode(shiny::isolate(r$step))
  if(is.null(target) || !(target %in% walk)) return(walk[[1]])

  #back down the walk from the named step until one can be entered. step 1 needs
  #nothing, so this always terminates with an answer.
  for(i in rev(seq_len(match(target, walk)))){
    if(vftStepReachable(r, walk[[i]])) return(walk[[i]])
  }
  walk[[1]]
}

#' The steps that have to be done before this one, as labels.
#'
#' Static: derived from `needs` and VFT_KEY_SOURCE, not from the session state,
#' so the tooltip can be baked into the markup at UI build time and the bar stays
#' free of outputs.
vftStepPrereqLabels <- function(step){
  vapply(vftStepPrereqSteps(step), function(s) VFT_STEPS[[s]]$label,
         character(1), USE.NAMES = FALSE)
}

#' The same answer as `vftStepPrereqLabels()`, as step NAMES rather than labels.
#'
#' Split out because the nav bar's tooltips are translated now (see
#' `vftStepNav()` in R/app_ui.R): it has to write the same sentence three times,
#' once per language, so it needs the step names to look each label up in the
#' language being written. Reading the German labels back out of VFT_STEPS -
#' which is all `vftStepPrereqLabels()` can give it - would give it only the one.
vftStepPrereqSteps <- function(step){
  needs   <- VFT_STEPS[[step]]$needs
  if(length(needs) == 0) return(character(0))
  sources <- unique(VFT_KEY_SOURCE[needs])
  sources <- sources[!is.na(sources) & sources != step]
  if(length(sources) == 0) return(character(0))
  #back into registry order, so a tooltip reads "2 ..., 3 ..." not "3 ..., 2 ..."
  names(VFT_STEPS)[names(VFT_STEPS) %in% sources]
}

#' The tooltip for one nav bar button.
vftStepTooltip <- function(step){
  prereq <- vftStepPrereqLabels(step)
  if(length(prereq) == 0) return(VFT_STEPS[[step]]$label)
  paste0(VFT_STEPS[[step]]$label, " – benötigt: ",
         paste(prereq, collapse = ", "))
}
