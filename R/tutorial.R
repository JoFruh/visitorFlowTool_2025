#### The guided tutorial ####
#
# One tour per step, played in the browser by inst/app/www/vft-tutorial.js - a
# port of FRISCH's first-use tutorial. A grey layer is laid over the page with
# windows cut into it over the control to use next, and a card beside them says
# what to do. The user does it for real, through the window, and the tour moves
# on when the page shows it was done. A hint that is only there to be read has a
# Next button instead.
#
# Everything about the tour itself - the hints, where their windows go, what
# counts as done - lives in the JS, because every one of those questions is
# about the page as drawn. What it needs from here is its texts in the language
# the user is in, and one R-side door: the help button's modal, which offers the
# tour of the step being shown.
#
# Two ways in:
#   * a bubble under the help button offers step 1's tour on the first visit.
#     Finishing, stopping or closing it is stored on the device
#     (localStorage `vft.tutorial.v1`) and it does not come back.
#   * the help button, on a step that has a tour, opens vftTutorialModal()
#     instead of that step's own help modal, and its button starts the tour from
#     that step's first hint.
#
# Tours CHAIN: step 1's last hint sends the user to the step they chose, and the
# JS starts that step's tour when the nav bar's ring lands there - if it has one.
# Adding a tour for another step is therefore a `TOURS.<key>` entry in the JS,
# its key here, and its texts; nothing about step 1's has to change.
#
# Depends on the nav bar (VFT_NAV): the help button, and the next-step modal
# step 1's tour walks through, only exist with it.

#' The tour keys that have a tour. Must match the keys of `TOURS` in
#' inst/app/www/vft-tutorial.js - data-raw/verify_tutorial.R checks they do.
#'
#' A key is the nav bar button the ring is on, minus its `vftNav_` prefix
#' (`vftTutorialKey()`): `step1`..`step5`, `newVersions`, or `hitze` for the
#' Hitzeminderung door, which is the newVersions tab on another context and so
#' needs a tour of its own.
#'
#' `toHitze` is no page's: it is the one hint step 5 plays when the user comes
#' back from the newVersions tour (the JS's `NEXT`), pointing on to
#' Hitzeminderung. It is listed so its text is sent; no ring key matches it, so
#' no help button offers it.
#'
#' `commit` and `saveLoad` are no page's either: one card each, played between
#' two tours. `commit` is the data-loss warning (vftAskCommit() in
#' R/providers.R) - its text also interrupts any hint while that modal is up.
#' `saveLoad` points at the nav bar's save and load buttons, once per device,
#' after the first tour finished past step 1's.
VFT_TUTORIAL_TOURS <- c("step1", "step2", "step3", "step4", "step5", "newVersions", "toHitze",
                        "hitze", "commit", "saveLoad")

#' How many hints of its own each tour has; its texts are `:tut_<key>_1:` ..
#' `_<n>:`. `hitze` also plays three of the scenarios page's hints, with their
#' texts, when there is no scenario to paint on (`borrowed()` in the JS).
VFT_TUTORIAL_HINTS <- c(step1 = 6L, step2 = 13L, step3 = 5L, step4 = 6L, step5 = 10L,
                        newVersions = 23L, toHitze = 1L, hitze = 26L, commit = 1L,
                        saveLoad = 1L)

#' The alternative texts of hints that change with the app - a hint's
#' `variant()` in the JS picks one as the hint starts, its `textWhen()` while
#' it is on. `"9b"` is hint 9's variant b, text `:tut_step5_9b:`; `"7b"` is
#' hint 7's text while the image's name modal is open; `"2b"` hint 2's when
#' the simulation it follows was launched in hint 1 rather than by the page.
#' (Hint 6 had one too, for an area without a sensitivity matrix; it is passed
#' over there now.) The scenarios page's `"21b"` is hint 21's text while the
#' rename modal is open. Step 2's `"6b"` is a hint of its own between 6 and 7
#' (`alt` in the JS), so the rows after it keep their numbers.
VFT_TUTORIAL_VARIANTS <- list(step5 = c("2b", "7b", "9b"), newVersions = "21b", step2 = "6b")

#' English stand-ins for a translation row that has not been added yet.
#'
#' `:token:` keys, not German literals, because the step texts are long HTML
#' and would make unwieldy keys - so a missing row would otherwise put
#' ":tut_step1_3:" on screen. English rather than German because English is
#' what the tour was written in. The CSVs carry all three languages.
#'
#' Step texts may use `<br>`, `<b>` (teal), `<em>` (bold, one size larger) and
#' `<em class=vftTutAoi>` (the same, in the areas of interest's green),
#' `<em class=vftTutAoiRed>` (the same, in step 3's map red),
#' `<em class=vftTutAoiFill>` (the same, in step 4's map fill green),
#' `<em class=vftTutSens>` (the same, orange: biodiversity sensitivity on step 2),
#' `<em class=vftTutUsage>` (the same, a busy path's blue: path usage),
#' `<em class=vftTutWarn>` (the same, the data-loss modal's red button),
#' `<em class=vftTutShape>` (the same, in the blue of a shape being drawn),
#' `<i class=vftTutPolyDraw></i>` (a shape being drawn, two lines high),
#' `<em class=vftTutBio>` (the same, in the biodiversity sensitivity's dark red),
#' `<em class=vftTutGrey>` (the same, dark grey: the reset button, the
#' Original scenario), `<em class=vftTutDelete>`, `vftTutParking`,
#' `vftTutResidence` (the scenarios page's red, blue and ochre buttons),
#' `<em class=vftTutNew>` (the seeded scenario's grey italic name),
#' `<em class=vftTutHeat>` (heat mitigation, dark red),
#' `<em class=vftTutGrass>`, `<em class=vftTutTree>` (a paint material, in its
#' paint's colour), `<em class=vftTutBlack>` (a tool's name, black),
#' `<em class=vftTutHot>` ("heat", orange), `<em class=vftTutArtificial>` /
#' `vftTutCanopyArt` (the plan import's two artificial materials),
#' `<span class=vftTutIconRow><i class=vftTutCut1></i><i class=vftTutCut2></i><i class=vftTutCut3></i></span>`
#' (step 4's cut, numbered: one vertex, the scissors, the area cut in two),
#' `<i class=vftTutScissors></i>` (step 4's scissors button) and
#' `<i class=vftTutNode></i>` / `<i class=vftTutNodeSel></i>` (a path network
#' node, and the selected one) - see
#' inst/app/www/vft-tutorial.css. `<b><em>` is teal and one size larger. The
#' class is left unquoted: the CSV reader drops a `"` inside a field, so a
#' quoted one would not match the fallback.
VFT_TUTORIAL_FALLBACK <- list(
  ":tut_step1_1:" = "Welcome to Visitor Flow Tool.<br>Here you can quickly and easily explore the impacts of planning on biodiversity and heat mitigation, <em>anywhere in Switzerland</em>.<br>Let's explore how!",
  ":tut_step1_2:" = "You can upload shapefiles or a .kml file to determine the full area you wish to work on.",
  ":tut_step1_3:" = "You can also draw your own <em class=vftTutShape>areas</em> directly on the map.<br>Place at least 3 points on the map, to determine an <em class=vftTutShape>area</em>.<br><i class=vftTutPolyDraw></i><br>As an example, draw an <em class=vftTutShape>area</em> around Birmensdorf.",
  ":tut_step1_4:" = "Now that we have an area, we can hit <b>Confirm</b> to finish this step.",
  ":tut_step1_5:" = "There are now various stages you can choose from, depending on your interests.<br>Choose the one you're interested in!",
  ":tut_step1_6:" = "The tutorial will continue to your chosen next step.<br>If you interrupt the tutorial, you can restart it at any step you wish.<br>Simply click the <b><em>help</em></b> button on that step!",
  ":tut_step2_1:"  = "You've selected the <b><em>Biodiversity Sensitivity</em></b> step.",
  ":tut_step2_2:"  = "You will notice other steps are available, allowing you to go back and forth as you wish.",
  ":tut_step2_3:"  = "Here you create a <em class=vftTutSens>Biodiversity Sensitivity</em> map.<br>You can quickly and easily combine species distributions relevant to your project.",
  ":tut_step2_4:"  = "On the right you can select species by group.<br>By default all species are selected.<br>Let's select <b><em>Amphibians</em></b>.",
  ":tut_step2_5:"  = "The distributions of all Amphibians are stacked to create a single sensitivity map.",
  ":tut_step2_6:"  = "On the left, individual species are shown.",
  ":tut_step2_6b:" = "The most widespread species in the area are closer to the top of the list.",
  ":tut_step2_7:"  = "Information is given for each species, such as its national priority, its Red List status and whether it is an Emerald species.<br>Its scientific name is a link to more information.",
  ":tut_step2_8:"  = "A species' weight can be increased, raising its importance.<br>Increase the Yellowbelly toad's weight to 3.",
  ":tut_step2_9:"  = "You can also set the weights automatically, let's base the weights on the Red List status.<br>Click the button.",
  ":tut_step2_10:" = "If the sensitivity map gets too crowded, you can raise the threshold to focus on the most sensitive areas.<br>Let's hide the bottom 25% of sensitive areas.",
  ":tut_step2_11:" = "We now have a Biodiversity Sensitivity map!<br>We can download it as a GeoTIFF here.",
  ":tut_step2_12:" = "Our <b><em>Biodiversity Sensitivity</em></b> map will be used in the next steps.<br>We can also confirm our map and head to the next step!",
  ":tut_step2_13:" = "We have come to the end of this step.<br>Choose your next step!",
  ":tut_step3_1:" = "Before we <b><em>Simulate Recreation</em></b>, we need to specify where recreationists go to recreate (<em class=vftTutAoiRed>Areas of Interest</em>). E.g. parks, forests, lakesides.",
  ":tut_step3_2:" = "In this sub-step, we quickly define <em class=vftTutAoiRed>Areas of Interest</em> (AoI) by sliding a bar.",
  ":tut_step3_3:" = "Slide the bar to the value of <b>8</b>.",
  ":tut_step3_4:" = "Now all areas with an attractivity value above 8 are shown in red.<br>They will become <em class=vftTutAoiRed>Areas of Interest</em> (AoIs).",
  ":tut_step3_5:" = "<b><em>Confirm</em></b> to go to the next sub-step and precisely edit the Areas of Interest.",
  ":tut_step4_1:" = "In this sub-step, we can further refine the <em class=vftTutAoiFill>Areas of Interest</em>.",
  ":tut_step4_2:" = "You can cut existing polygons by drawing a line across them:<br>Click two points onto the map, the second point will be a scissor.<span class=vftTutIconRow><i class=vftTutCut1></i><i class=vftTutCut2></i><i class=vftTutCut3></i></span>Clicking the scissor cuts along the red dotted line, cutting polygons.",
  ":tut_step4_3:" = "Add 3 points on the map, to start a <em class=vftTutShape>shape</em> (like in step 1).<br><i class=vftTutPolyDraw></i><br>Adding <em class=vftTutShape>shapes</em> over existing ones will combine them!<br>Create a new <em class=vftTutShape>shape</em> on the map.",
  ":tut_step4_4:" = "You can always <em class=vftTutGrey>Reset</em> back to the original shapes.<br>Let's reset to remove our modifications.",
  ":tut_step4_5:" = "You can also choose to correct automatically!<br>This will attempt to create realistic <em class=vftTutAoiFill>Areas of Interest</em>.<br>E.g. by making a lake's contour its own area.<br><b><em>Try it!</em></b>",
  ":tut_step4_6:" = "Let's <b><em>confirm</em></b> the corrected AoIs.",
  ":tut_step5_1:" = "In this step, we can simply launch a recreation simulation!",
  ":tut_step5_2:" = "On your arrival, the path network is loaded and the <b><em>Recreation Simulation</em></b> is run.",
  ":tut_step5_2b:" = "Path data is downloaded and the simulation is run.<br>This can take a bit of time.",
  ":tut_step5_3:" = "We now see the <em class=vftTutUsage>path usage</em>.<br>Wider and bluer paths have more usage.",
  ":tut_step5_4:" = "Many details can be shown.",
  ":tut_step5_5:" = "Let's show the simulated recreationists' (agents') starting points.",
  ":tut_step5_6:" = "You can show your previously generated <em class=vftTutBio>Biodiversity Sensitivity</em> Map here.",
  ":tut_step5_7:" = "You can create a PDF showing and detailing your map and choices.",
  ":tut_step5_7b:" = "Give a name to the image and confirm.",
  ":tut_step5_8:" = "Here,<br>you can create and select your <b><em>Scenarios</em></b> to simulate them.",
  ":tut_step5_9:" = "You only have the <em class=vftTutGrey>Original</em> scenario for now.<br>Click this button to create a new <b><em>Scenario</em></b>!",
  ":tut_step5_9b:" = "Select another scenario here.",
  ":tut_step5_10:" = "Now you can launch a new simulation!",
  ":tut_newVersions_1:"  = "Here we can create new <b><em>Scenarios</em></b>.<br>Which we can then use in the Simulations.",
  ":tut_newVersions_2:"  = "To change anything, we first need to create a new <b><em>Scenario</em></b>.<br>We cannot alter the <em class=vftTutGrey>Original</em> scenario.",
  ":tut_newVersions_3:"  = "Submit a name for the new scenario.",
  ":tut_newVersions_4:"  = "To change anything, we first need to select a new <b><em>Scenario</em></b>.<br>We cannot alter the <em class=vftTutGrey>Original</em> scenario.",
  ":tut_newVersions_5:"  = "We can change a variety of things through the different contexts shown here.",
  ":tut_newVersions_6:"  = "Let's first focus on <em class=vftTutGrey>Paths/Roads</em>.",
  ":tut_newVersions_7:"  = "Here, we see paths, roads, their qualities and crossroads <i class=vftTutNode></i>(nodes).",
  ":tut_newVersions_8:"  = "Click any <em>path</em> to either remove it, or change its qualities.",
  ":tut_newVersions_9:"  = "You can now see the path's qualities and change them.",
  ":tut_newVersions_10:" = "We can also <em class=vftTutDelete>delete</em> the path.<br>Click the button.",
  ":tut_newVersions_11:" = "You will notice the path is now gone.",
  ":tut_newVersions_12:" = "You can also select nodes <i class=vftTutNode></i>.<br>Select one by clicking on it.",
  ":tut_newVersions_13:" = "You can create a new path from the selected node <i class=vftTutNodeSel></i>,<br>either by clicking on the map to create a new attached node,<br>or by clicking another existing node to connect them.",
  ":tut_newVersions_14:" = "As you are creating a new path, you have to determine its qualities.<br>Submit the new path qualities.",
  ":tut_newVersions_15:" = "Your new path now exists!",
  ":tut_newVersions_16:" = "You can delete the selected node <i class=vftTutNodeSel></i> by clicking on it a second time,<br>all paths connected to the deleted node will also be deleted.<br>Delete a node.",
  ":tut_newVersions_17:" = "Now let's change the parking spaces and residences.<br>Click the new context.",
  ":tut_newVersions_18:" = "Create a new <em class=vftTutShape>shape</em> with at least 3 clicks on the map.",
  ":tut_newVersions_19:" = "Now choose if the area will be a <em class=vftTutResidence>residence</em>, or a <em class=vftTutParking>parking space</em>.",
  ":tut_newVersions_20:" = "All these changes are saved to the selected scenario.",
  ":tut_newVersions_21:" = "A <em class=vftTutNew>New</em> scenario was automatically created on your first visit, you can change the name by clicking on it.",
  ":tut_newVersions_21b:" = "Submit a name for the scenario.",
  ":tut_newVersions_22:" = "You cannot modify the <em class=vftTutGrey>Original</em> scenario.",
  ":tut_newVersions_23:" = "Once you've got your new scenarios, you can <b><em>confirm</em></b> them to return to the Simulate Recreation step.",
  ":tut_toHitze_1:" = "We can now explore the last context: <em class=vftTutHeat>Heat mitigation</em>.",
  ":tut_commit_1:" = "If you alter data from previous steps, linked data from later steps will be erased.<br>A <em class=vftTutWarn>warning</em> is given, confirm to continue.",
  ":tut_saveLoad_1:" = "You can <b><em>save</em></b> and <b><em>load</em></b> your work at any time here.",
  ":tut_hitze_1:"  = "This step allows you to observe the impact of landscape changes on <em class=vftTutHot>heat</em> and its consequences.",
  ":tut_hitze_2:"  = "Heat mitigation is another context for building scenarios, which can affect Outdoor Recreation.<br><br>However, it also works independently from the Recreation Simulation.",
  ":tut_hitze_3:"  = "Here we see the main landscape materials mapped.",
  ":tut_hitze_4:"  = "We can repaint these materials as we wish!<br>Choose the <em class=vftTutGrass>Grass</em> material from your palette.",
  ":tut_hitze_5:"  = "Now let's paint the new material on the map.<br>Zoom in to be more precise with your brush.",
  ":tut_hitze_6:"  = "You can paint the ground level, or the canopy (trees, roofs, buildings).<br>You can switch between the two here.",
  ":tut_hitze_7:"  = "Let's now choose to paint <em class=vftTutTree>trees</em>!",
  ":tut_hitze_8:"  = "The <em class=vftTutTree>tree</em> material can have different heights.<br>This will affect the projected shade.<br>Choose a height.",
  ":tut_hitze_9:"  = "Now paint some trees.",
  ":tut_hitze_10:" = "Careful! Planting trees on underground structures, such as parking spaces, is not recommended.<br><br>If you insist, you can ignore the element and continue painting.",
  ":tut_hitze_11:" = "After altering the map's materials, you can calculate the impact on <em class=vftTutHot>heat</em>.<br>Click here.",
  ":tut_hitze_12:" = "This can take a few seconds, as the interaction of different materials and shade, at different scales, are combined into a single map.",
  ":tut_hitze_13:" = "We now have a heat map for this scenario. It was calculated at noon.<br><br>We can also calculate it for the morning or the afternoon.",
  ":tut_hitze_14:" = "These heat maps are saved to the selected scenario.<br>You can show them again instantly by clicking the active icons.<br>Click the one for noon.",
  ":tut_hitze_15:" = "Let's now hide the heat map.<br>To be able to paint again.",
  ":tut_hitze_16:" = "The <em class=vftTutBlack>Eraser</em> will remove your modifications at precise areas of the map.<br><em class=vftTutBlack>Reset</em> will remove all your modifications on the selected scenario.",
  ":tut_hitze_17:" = "The last tool, <em class=vftTutBlack>Load Existing Plan</em>, will allow you to quickly change materials with an existing map/image!<br>For the sake of the tutorial, we will provide the map.",
  ":tut_hitze_18:" = "Align the uploaded map to your location and hit <b><em>Next</em></b>.<br>These will not necessarily align as the provided map may not be from this location.",
  ":tut_hitze_19:" = "Now you can assign a material to every color of the map.<br>Though most of these will have been done automatically.",
  ":tut_hitze_20:" = "But say the brown paths actually represent <em class=vftTutArtificial>Artificial</em> paths.<br>Change the material assigned.",
  ":tut_hitze_21:" = "You can see the end result here.",
  ":tut_hitze_22:" = "If specific color nuances are missing, you can add them.<br>Here, the darker grey area is an artificial roof.<br>Let's use <b><em>Pick colour</em></b> and click on it.",
  ":tut_hitze_23:" = "Now, assign <em class=vftTutCanopyArt>Artificial canopy</em> to it.",
  ":tut_hitze_24:" = "Now <b><em>Apply</em></b> your choices.",
  ":tut_hitze_25:" = "The uploaded map is now painted onto the map's ground and canopy layers!",
  ":tut_hitze_26:" = "Now <b><em>confirm</em></b> this new scenario.<br>The heat information can be used in a Recreation Simulation.",
  ":tut_next:"       = "Next",
  ":tut_stop:"       = "Stop tutorial",
  ":tut_offer:"      = "New here? Take a short guided tour.",
  ":tut_start:"      = "Start tutorial",
  ":tut_help_title:" = "Need help with this step?"
)

#' One tutorial text, in `lang` or else the session's current language.
#'
#' With `lang`, straight out of the Translator's table - NOT through `t()`.
#' After usei18n(), `t()` answers in the per-session language update_lang()
#' keeps, and on a language change that is set by the step's own observer, a
#' round trip through the browser AFTER `r$currentLang` has moved. Asked from
#' the r$currentLang observer, `t()` therefore gave the texts of the language
#' before: French after switching back to German. The table has no such timing.
#'
#' Without `lang`, through .vftT() (R/async_helpers.R), which handles the
#' usei18n() tag trap - that is the modal's case, shown well after any change.
#' Either way a key the CSVs do not carry comes back as itself, the `:...:`
#' shape tested here, the same test vftNextStepModal()'s lab() makes.
vftTutorialTr <- function(key, session = shiny::getDefaultReactiveDomain(), lang = NULL){
  out <- if(!is.null(lang)) vftTutorialLookup(key, lang, session)
         else tryCatch(.vftT(session)(key), error = function(e) key)
  if(length(out) != 1L || is.na(out) || !nzchar(out) || grepl("^:.*:$", out)){
    fb <- VFT_TUTORIAL_FALLBACK[[key]]
    return(if(is.null(fb)) key else fb)
  }
  as.character(out)
}

#' A key's row in the session Translator's table, for one language; NA when
#' there is no Translator, no such row or no such language.
vftTutorialLookup <- function(key, lang, session = shiny::getDefaultReactiveDomain()){
  tr <- tryCatch(session$userData$vftI18n, error = function(e) NULL)
  if(is.null(tr)) return(NA_character_)
  tb <- tryCatch(tr$get_translations(), error = function(e) NULL)
  if(is.null(tb) || !key %in% rownames(tb) || !lang %in% colnames(tb)) return(NA_character_)
  as.character(tb[key, lang])
}

#' Everything the browser needs to say, in `lang` (the current language when
#' NULL).
#'
#' `lang` lets the JS hold a hint back until the texts of a language just picked
#' have arrived, rather than showing the old ones for a moment.
vftTutorialTexts <- function(session = shiny::getDefaultReactiveDomain(),
                             lang = NULL){
  tr <- function(key) vftTutorialTr(key, session, lang)
  tours <- lapply(stats::setNames(VFT_TUTORIAL_TOURS, VFT_TUTORIAL_TOURS), function(k)
    vapply(sprintf(":tut_%s_%d:", k, seq_len(VFT_TUTORIAL_HINTS[[k]])),
           tr, character(1), USE.NAMES = FALSE))
  #an object per tour, {"9b": "..."}: as.list() keeps each an object in JSON
  alts <- lapply(stats::setNames(names(VFT_TUTORIAL_VARIANTS), names(VFT_TUTORIAL_VARIANTS)),
                 function(k) {
                   v <- VFT_TUTORIAL_VARIANTS[[k]]
                   as.list(stats::setNames(vapply(sprintf(":tut_%s_%s:", k, v), tr, character(1),
                                                  USE.NAMES = FALSE), v))
                 })
  list(lang  = if(is.null(lang)) "" else as.character(lang),
       next_ = tr(":tut_next:"),
       stop  = tr(":tut_stop:"),
       offer = tr(":tut_offer:"),
       start = tr(":tut_start:"),
       #a list of character vectors; each goes to the browser as an array, even
       #a tour of one hint (I() keeps jsonlite from unboxing it)
       tours = lapply(tours, I),
       alts  = alts)
}

#' Which tour the step being shown would play.
#'
#' The ring's button rather than `r$navStep`, because Hitzeminderung is the
#' newVersions tab on another context and needs its own tour - the same
#' distinction vftNavCurrentId() exists to make. Before the first navigation the
#' ring is not set yet, and the user is on step 1.
vftTutorialKey <- function(r){
  id <- shiny::isolate(vftNavCurrentId(r))
  if(is.null(id)) "step1" else sub("^vftNav_", "", id)
}

#' The help button's modal, on a step that has a tour.
#'
#' Teal, like the next-step modal it sits beside (VFT_NEXT_CSS in
#' R/navigation.R), and scoped to #shiny-modal the same way so it cannot reach
#' any other modal. Its button is plain markup calling vftTutorialStart(), which
#' closes this modal and starts the tour from `key`'s first hint - the tour runs
#' in the browser, so there is nothing for R to do on the click.
#'
#' Translated at show time through vftTutorialTr(), for the reason
#' vftNextStepModal() gives: a modal built per click is always in the language
#' the user is in now.
VFT_TUTORIAL_MODAL_CSS <- "
#shiny-modal .modal-content { background-color:#006268; border:none;
    border-radius:4px; box-shadow:0 10px 40px rgba(0,0,0,.35); }
#shiny-modal .modal-body { padding:34px 30px 34px; text-align:center; }
#shiny-modal .vft-tut-head { color:#ffffff; font-size:26px; font-weight:700;
    line-height:1.25; margin:0 0 24px;
    font-family:'FranklinGFB','FranklinGothic','Franklin Gothic Book',
                'Libre Franklin',Arial,sans-serif; }
"

vftTutorialModal <- function(session = shiny::getDefaultReactiveDomain(), key = "step1",
                             lang = NULL){
  shiny::showModal(shiny::modalDialog(
    footer = NULL, easyClose = TRUE,
    shiny::tags$style(shiny::HTML(VFT_TUTORIAL_MODAL_CSS)),
    shiny::tags$div(class = "vft-tut-head", vftTutorialTr(":tut_help_title:", session, lang)),
    shiny::tags$button(type = "button", class = "btn vftTutorialStartBtn",
                       onclick = sprintf("vftTutorialStart('%s')", key),
                       vftTutorialTr(":tut_start:", session, lang))
  ), session = session)
  invisible(NULL)
}

#' Keep the browser's tutorial texts in the current language.
#'
#' Sent once at session start - which is also what lets the offer bubble appear
#' - and again on every language change. `r$currentLang` is written by
#' vftLangServer() alongside the Translator's language, so by the time this
#' runs the Translator already answers in the new one.
#'
#' Does nothing without the nav bar: there is no help button to start a tour
#' from, and no next-step modal for step 1's tour to walk through.
vftTutorialServer <- function(r, session = shiny::getDefaultReactiveDomain()){
  if(!vftNavEnabled()) return(invisible(NULL))
  shiny::observeEvent(r$currentLang, {
    lang <- r$currentLang
    if(is.null(lang)) lang <- "de"
    session$sendCustomMessage("vft-tutorial-texts", vftTutorialTexts(session, lang))
  }, ignoreNULL = FALSE)
  invisible(NULL)
}
