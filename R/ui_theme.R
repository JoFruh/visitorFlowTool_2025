#### One look for every step ####

# The newVersions page was redesigned first (see the notes at the top of
# R/newVersions_ui.R) and this file is what the other steps took from it: the
# banner's teal as the one accent colour, a compact title just under the banner,
# white outlined secondary buttons, one filled teal button per page for the step
# it finishes, map layers as switch rows in small cards, and maps in a rounded
# frame. The pieces newVersions and step 5 share - the rail, the switches, the
# scenario column - moved here out of newVersions_ui.R, and the two pages lay
# out on the same `.vft-ws-*` classes.
#
# Emitted once, into <head>, by app_ui(). <head> and not the body on purpose:
# each module's own stylesheet (NV_CSS, STEP5_CSS) is a tags$head() too, and
# app_ui() comes first in the tree, so these rules are always EARLIER in the
# cascade than a page's own. That matters wherever the two meet at equal
# specificity - `.vftB{font:inherit}` here and `.vftMat{font-size:14px}` in
# NV_CSS, for one: in the other order the material labels lose their size.

VFT_TEAL      <- "#006268"
VFT_TEAL_TINT <- "#e3f0f0"

#' An 18px stroke icon, drawn in currentColor so it follows the text colour.
#' @noRd
vftIcon <- function(name, size = 18){
  if (name == "tools") return(vftToolsIcon(size))
  d <- switch(name,
    road     = '<path d="M5 20 9.5 4"/><path d="M19 20 14.5 4"/><path d="M12 5v2"/><path d="M12 11v2"/><path d="M12 17v2"/>',
    home     = '<path d="M3 11 12 4l9 7"/><path d="M5 10v10h14V10"/><path d="M10 20v-5h4v5"/>',
    heat     = '<path d="M14 14.76V4.5a2 2 0 0 0-4 0v10.26a4 4 0 1 0 4 0Z"/><path d="M12 10v6"/>',
    eraser   = '<path d="m7 21-4-4a2 2 0 0 1 0-2.8L13.2 4a2 2 0 0 1 2.8 0l5 5a2 2 0 0 1 0 2.8L12 21"/><path d="M22 21H7"/><path d="m5 11 9 9"/>',
    reset    = '<path d="M3 12a9 9 0 1 0 3-6.7L3 8"/><path d="M3 3v5h5"/>',
    upload   = '<path d="M12 15V3"/><path d="m7 8 5-5 5 5"/><path d="M5 15v4a2 2 0 0 0 2 2h10a2 2 0 0 0 2-2v-4"/>',
    up       = '<path d="m6 15 6-6 6 6"/>',
    download = '<path d="M12 3v12"/><path d="m7 10 5 5 5-5"/><path d="M5 21h14"/>',
    check    = '<path d="M20 6 9 17l-5-5"/>',
    skip     = '<path d="m5 4 10 8-10 8V4z"/><path d="M19 5v14"/>',
    search   = '<circle cx="11" cy="11" r="7"/><path d="m21 21-4.3-4.3"/>',
    bulb     = '<path d="M9 18h6"/><path d="M10 22h4"/><path d="M12 2a7 7 0 0 0-4 12.7c.6.5 1 1.3 1 2.1V17h6v-.2c0-.8.4-1.6 1-2.1A7 7 0 0 0 12 2z"/>',
    folder   = '<path d="M3 7a2 2 0 0 1 2-2h4l2 2h8a2 2 0 0 1 2 2v8a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2z"/>',
    draw     = '<path d="M4 18 7 6l11 2 2 10z"/><circle cx="4" cy="18" r="1.6"/><circle cx="7" cy="6" r="1.6"/><circle cx="18" cy="8" r="1.6"/><circle cx="20" cy="18" r="1.6"/>',
    image    = '<rect x="3" y="3" width="18" height="18" rx="2"/><circle cx="9" cy="9" r="2"/><path d="m21 15-5-5L5 21"/>',
    scissors = '<circle cx="6" cy="6" r="3"/><circle cx="6" cy="18" r="3"/><path d="M20 4 8.1 15.9"/><path d="M14.5 14.5 20 20"/><path d="M8.1 8.1 12 12"/>',
    plus     = '<path d="M12 5v14"/><path d="M5 12h14"/>',
    play     = '<path d="M7 4.5v15l12-7.5z"/>')
  shiny::HTML(sprintf(paste0('<svg width="%d" height="%d" viewBox="0 0 24 24" fill="none" stroke="currentColor" ',
                             'stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true">%s</svg>'),
                      size, size, d))
}

#' A FILLED hammer crossed over a double-ended wrench (step 5's 'Manage
#' scenarios' card), in currentColor. Its own 48-unit box, not vftIcon()'s
#' 24-unit stroke one. Both tools are drawn upright - head up, handle down -
#' and turned 45 degrees, the hammer's head to the top left, the wrench's to
#' the top right. The mask cuts the wrench's two jaws and, where the hammer
#' lies on it, a gap the hammer's outline plus 1.8 units wide: the tools stay
#' apart without painting the card's colour, so hover and disabled still
#' work. One mask id for every copy: they are identical, so whichever the
#' browser resolves draws the same.
#' @noRd
vftToolsIcon <- function(size = 18){
  hammer <- '<rect x="14" y="3" width="20" height="10" rx="2"/><rect x="21.5" y="11" width="5" height="33" rx="2.5"/>'
  shiny::HTML(paste0(
    '<svg width="', size, '" height="', size, '" viewBox="0 0 48 48" fill="currentColor" aria-hidden="true">',
    '<defs><mask id="vftToolsGap" maskUnits="userSpaceOnUse" x="-4" y="-4" width="56" height="56">',
    '<rect x="-4" y="-4" width="56" height="56" fill="white"/>',
    '<g transform="rotate(45 24 24)" fill="black">',
    '<path d="M21 -2H27V8A3 3 0 0 1 21 8Z"/><path d="M21 50H27V39A3 3 0 0 0 21 39Z"/></g>',
    '<g transform="rotate(-45 24 24)" fill="black" stroke="black" stroke-width="3.6" stroke-linejoin="round">',
    hammer, '</g></mask></defs>',
    '<g transform="translate(1.5 1.9)">',
    '<g mask="url(#vftToolsGap)"><g transform="rotate(45 24 24)">',
    '<circle cx="24" cy="10" r="8"/><circle cx="24" cy="37" r="8"/>',
    '<rect x="21.2" y="12" width="5.6" height="23" rx="2.8"/></g></g>',
    '<g transform="rotate(-45 24 24)">', hammer, '</g></g></svg>'))
}

#' The small key in front of a map layer: what the layer looks like ON the
#' map, so the colour lives here and the switch itself stays neutral.
#' @noRd
vftGlyph <- function(kind){
  g <- switch(kind,
    sm    = '<rect x="1" y="1" width="7.5" height="7.5" rx="1.5" fill="#fee08b"/><rect x="9.5" y="1" width="7.5" height="7.5" rx="1.5" fill="#fdae61"/><rect x="1" y="9.5" width="7.5" height="7.5" rx="1.5" fill="#f46d43"/><rect x="9.5" y="9.5" width="7.5" height="7.5" rx="1.5" fill="#d73027"/>',
    pa    = '<rect x="1.5" y="1.5" width="15" height="15" rx="3" fill="rgba(74,134,54,0.25)" stroke="#4a8636" stroke-width="1.5"/><path d="M4 12 L12 4 M7 15 L15 7" stroke="#4a8636" stroke-width="1.2"/>',
    aoi   = '<rect x="1.5" y="1.5" width="15" height="15" rx="3" fill="rgba(0,128,0,0.12)" stroke="green" stroke-width="1.8"/>',
    aoiIn = '<rect x="1.5" y="1.5" width="15" height="15" rx="3" fill="rgba(0,128,0,0.12)" stroke="green" stroke-width="1.8"/><path d="M4.5 12 C 7 12, 9 6, 13.5 6" fill="none" stroke="#182db5" stroke-width="2.4" stroke-linecap="round"/>',
    start = '<circle cx="5.5" cy="6" r="3" fill="none" stroke="red" stroke-width="1.6"/><circle cx="12.5" cy="7.5" r="3" fill="none" stroke="red" stroke-width="1.6"/><circle cx="8" cy="13" r="3" fill="none" stroke="red" stroke-width="1.6"/>',
    park  = '<rect x="1.5" y="1.5" width="15" height="15" rx="3" fill="rgba(70,130,180,0.3)" stroke="steelblue" stroke-width="1.6"/>',
    resid = '<rect x="1.5" y="1.5" width="15" height="15" rx="3" fill="rgba(138,114,43,0.3)" stroke="#8a722b" stroke-width="1.6"/>',
    conf  = '<circle cx="9" cy="9" r="7" fill="rgba(198,40,40,0.2)" stroke="#c62828" stroke-width="2"/>',
    orig  = '<circle cx="9" cy="9" r="7" fill="none" stroke="#6a1b9a" stroke-width="2" stroke-dasharray="3 2.5"/>',
    usage = '<path d="M1.5 13 C 6 13, 8 5, 16.5 5" fill="none" stroke="#182db5" stroke-width="4" stroke-linecap="round"/>',
    #four of the paint materials (grass, water, artificial, tree), as painted
    mat   = '<rect x="1" y="1" width="7.5" height="7.5" rx="1.5" fill="lightgreen"/><rect x="9.5" y="1" width="7.5" height="7.5" rx="1.5" fill="dodgerblue"/><rect x="1" y="9.5" width="7.5" height="7.5" rx="1.5" fill="grey"/><rect x="9.5" y="9.5" width="7.5" height="7.5" rx="1.5" fill="#006400"/>')
  shiny::HTML(paste0('<svg width="18" height="18" viewBox="0 0 18 18" aria-hidden="true">', g, '</svg>'))
}

#' A map layer switch: a plain checkbox inside a label, styled as a switch.
#'
#' Still an ordinary Shiny input - shiny's checkboxInputBinding binds any
#' input[type=checkbox] that has an id - so the server reads it and updates it
#' with updateCheckboxInput() exactly as it would a checkboxInput(), and
#' shinyjs::enable()/disable() on the id reach the checkbox itself.
#' `disabled` puts the attribute on the checkbox, where enable() takes it off -
#' shinyjs::disabled() around the row would mark the LABEL instead.
#' `checked` is the initial value, as checkboxInput()'s `value`.
#' @noRd
vftSwitchRow <- function(inputId, label, glyph = NULL, disabled = FALSE, checked = FALSE){
  shiny::tags$label(
    class = "vftRailRow",
    shiny::tags$input(id = inputId, type = "checkbox", class = "vftRailCheck",
                      disabled = if(isTRUE(disabled)) NA,
                      checked  = if(isTRUE(checked)) NA),
    if(!is.null(glyph)) shiny::span(class = "vftGlyph", vftGlyph(glyph)),
    shiny::span(class = "vftRailLabel", label),
    shiny::span(class = "vftSw"))
}

#' The same row for an overlay the SERVER switches: an actionButton, whose
#' on/off state the server shows with the vftConflictOn class. Disabled until
#' the server has something to show.
#' @noRd
vftToggleButton <- function(inputId, label, glyph){
  shinyjs::disabled(
    shiny::tags$button(
      id = inputId, type = "button", class = "vftB action-button vftRailRow",
      shiny::span(class = "vftGlyph", vftGlyph(glyph)),
      shiny::span(class = "vftRailLabel", label),
      shiny::span(class = "vftSw")))
}

#' A page head: the step's title, and under it whatever explains the step.
#' @noRd
vftHead <- function(title, ..., class = NULL){
  shiny::div(class = paste(c("vft-head", class), collapse = " "),
             shiny::h4(shiny::strong(title)), ...)
}

#' A line of explanation under a title: smaller and quieter than the title.
#' @noRd
vftSub <- function(...) shiny::p(class = "vft-sub", ...)

#' A tip under a title. It used to be a line of dark red bold text, which read
#' as an error; it is a teal note now, with a bulb in front.
#' @noRd
vftTip <- function(...) shiny::p(class = "vft-tip", vftIcon("bulb", 16), shiny::span(...))

#' A button label with an icon in front.
#' @noRd
vftBtnLabel <- function(icon, label) shiny::tagList(vftIcon(icon), shiny::span(label))

#' The shared stylesheet, in <head> - see the note at the top of this file.
#' @noRd
vftThemeCSS <- function() shiny::tags$head(shiny::tags$style(shiny::HTML(VFT_THEME_CSS)))

VFT_THEME_CSS <- "
:root{ --vft-teal:#006268; --vft-teal-dark:#004e53; --vft-teal-tint:#e3f0f0;
  --vft-line:#d6d9d8; --vft-ctl-line:#d3d7d5; --vft-ink:#1f2624; --vft-muted:#5b6462; }

/* ---- the contact line under the banner ------------------------------ */
/* A full-width row of its own whose text sits at the far left: it pushed every
   title 21px down, and it is also what made every step 17px taller than the
   window - --vft-fit in R/layout_helpers.R leaves room for the banner and not
   for this. Out of the flow, the text stays where it was, over the empty
   left end of each page's title row. */
.vft-nav-contact{ position:absolute; left:0; }

/* ---- the page head -------------------------------------------------- */
.vft-head{ display:flex; flex-direction:column; align-items:center; gap:4px;
  padding:6px 0 10px; text-align:center; }
/* .tab-content in front for weight: the short-screen rules in
   R/layout_helpers.R give every .tab-content h4 a margin, and they are in the
   body, i.e. later in the cascade than anything in <head> */
.tab-content .vft-head h4, .tab-content .vft-ws-side h4{ margin:0; line-height:1.1; }
.vft-sub{ margin:0; max-width:1000px; font-size:13px; line-height:1.4; color:#5b6462; }
.vft-tip{ display:inline-flex; align-items:center; gap:6px; margin:2px 0 0; padding:3px 10px;
  border-radius:8px; background:#e3f0f0; color:#006268; font-size:13px; font-weight:600;
  line-height:1.3; text-align:left; }
.vft-tip svg{ flex:0 0 auto; }
/* a tip with a button to its right (step 4's Automatic Cuts) */
.vft-tip-row{ display:flex; flex-wrap:wrap; align-items:center; justify-content:center; gap:10px; }
.vft-tip-row .vft-tip{ margin:0; }
.vft-tip-row .btn.vft-btn{ min-height:30px; padding:3px 12px; font-size:13px; }

/* ---- buttons -------------------------------------------------------- */
/* Two kinds on every page: the ONE filled teal button that finishes the step
   (vft-btn-primary), and white outlined ones for everything else - downloads,
   reset, skip (vft-btn). Both sit on a Bootstrap .btn, so every state
   Bootstrap colours - hover, focus, active, active+hover - is restated. */
.btn.vft-btn, .btn.vft-btn-primary{ display:inline-flex; align-items:center; justify-content:center;
  gap:6px; margin:0; white-space:normal; line-height:1.15; box-shadow:none; text-shadow:none;
  background-image:none; vertical-align:middle; }
.btn.vft-btn{ min-height:38px; padding:6px 14px; border:1px solid #d3d7d5; border-radius:8px;
  background:#ffffff; color:#1f2624; font-size:14px; font-weight:600; }
.btn.vft-btn:hover, .btn.vft-btn:focus, .btn.vft-btn:active, .btn.vft-btn:active:hover,
.btn.vft-btn:active:focus{ background:#ffffff; border-color:#9fb0ae; color:#1f2624; box-shadow:none; }
.btn.vft-btn:active{ background:#f1f4f4; }
/* a secondary-sized button filled teal (step 4's Automatic Cuts); the icon
   is drawn in currentColor, so it turns white with the text */
.btn.vft-btn.vft-btn-teal{ background:#006268; border-color:#006268; color:#ffffff; font-weight:700; }
.btn.vft-btn.vft-btn-teal:hover, .btn.vft-btn.vft-btn-teal:focus, .btn.vft-btn.vft-btn-teal:active,
.btn.vft-btn.vft-btn-teal:active:hover, .btn.vft-btn.vft-btn-teal:active:focus{
  background:#004e53; border-color:#004e53; color:#ffffff; }
.btn.vft-btn-primary, .btn-success, .btn-primary{ background:#006268; border-color:#006268; color:#ffffff; }
.btn.vft-btn-primary{ min-height:48px; padding:8px 26px; border:0; border-radius:10px;
  font-size:16px; font-weight:700; }
.btn.vft-btn-primary:hover, .btn.vft-btn-primary:focus, .btn.vft-btn-primary:active,
.btn.vft-btn-primary:active:hover, .btn.vft-btn-primary:active:focus,
.btn-success:hover, .btn-success:focus, .btn-success:active, .btn-success:active:hover,
.btn-primary:hover, .btn-primary:focus, .btn-primary:active, .btn-primary:active:hover{
  background:#004e53; border-color:#004e53; color:#ffffff; }
.btn.vft-btn:focus-visible, .btn.vft-btn-primary:focus-visible{ outline:2px solid #006268; outline-offset:2px; }
.btn.vft-btn[disabled], .btn.vft-btn-primary[disabled]{ opacity:.45; }
.btn.vft-btn svg, .btn.vft-btn-primary svg{ flex:0 0 auto; }

/* the action bar under a step's map: the secondary buttons first, the button
   that finishes the step last. The gap is on the buttons rather than the bar,
   so a bar whose buttons are all still hidden (step 1) takes no height. */
.vft-actions{ display:flex; flex-wrap:wrap; align-items:center; justify-content:center; column-gap:10px; }
.vft-actions > .btn, .vft-actions > label, .vft-actions > div{ margin-top:10px; }
/* a switch on its own, outside a rail card: step 4's cut mode */
.vftRailRow.vft-switch-pill{ width:auto; min-height:38px; border:1px solid #d3d7d5; background:#ffffff; font-weight:600; }
.vftRailRow.vft-switch-pill:hover{ border-color:#9fb0ae; background:#ffffff; }

/* ---- cards, section heads, map frames, form controls ---------------- */
.vft-card{ background:#ffffff; border:1px solid #d6d9d8; border-radius:10px; padding:10px 12px; }
.vft-sec-head{ margin:0 0 6px; font-size:11px; font-weight:700; letter-spacing:.08em;
  text-transform:uppercase; color:#5b6462; }
.vft-mapframe{ border:1px solid #d6d9d8; border-radius:10px; overflow:hidden; }
.vft-step4-frame{ border-radius:10px; overflow:hidden; }
.tab-content input[type=checkbox], .tab-content input[type=radio]{ accent-color:#006268; }

/* ---- plain buttons (not Bootstrap .btn) ----------------------------- */
.vftB{ font:inherit; margin:0; cursor:pointer; }
.vftB:disabled{ cursor:default; }
.vftB:focus-visible{ outline:2px solid #006268; outline-offset:2px; }

/* ---- the rail: every 'show X' is one switch row --------------------- */
.vftRailCard{ background:#ffffff; border:1px solid #d6d9d8; border-radius:10px;
  padding:10px 6px 6px; display:flex; flex-direction:column; gap:2px; }
.vftRailHead{ margin:0 8px 4px; font-size:11px; font-weight:700; letter-spacing:.08em;
  text-transform:uppercase; color:#5b6462; }
.vftRailRow{ position:relative; display:flex; align-items:center; gap:10px; width:100%;
  min-height:42px; margin:0; padding:4px 8px; border:0; border-radius:8px;
  background:transparent; color:#1f2624; font-size:14px; font-weight:400;
  line-height:1.15; text-align:left; cursor:pointer; }
.vftRailRow:hover{ background:#f1f4f4; }
.vftRailRow:disabled{ opacity:.45; background:transparent; }
.vftGlyph{ flex:0 0 18px; width:18px; height:18px; display:flex; }
.vftRailLabel{ flex:1 1 auto; min-width:0; }
input.vftRailCheck{ position:absolute; opacity:0; width:1px; height:1px; margin:0; pointer-events:none; }
/* a disabled checkbox is invisible, so what dims is the row it sits in */
.vftRailCheck:disabled ~ *{ opacity:.45; }
.vftRailRow:has(> .vftRailCheck:disabled){ cursor:default; background:transparent; }
/* greyed but LIVE: an overlay whose data another step produces. The click
   still toggles the checkbox, and the server answers it with an offer to go
   and produce it (newVersions' showSM / showAOI) - a [disabled] one could
   not say what is missing. */
.vftRailCheck.vftRailOff ~ *{ opacity:.45; }
.vftSw{ position:relative; flex:0 0 36px; width:36px; height:20px; border-radius:10px;
  background:#c6cbc9; transition:background .2s; }
.vftSw::after{ content:''; position:absolute; top:2px; left:2px; width:16px; height:16px;
  border-radius:50%; background:#ffffff; box-shadow:0 1px 2px rgba(0,0,0,.3); transition:left .2s; }
.vftRailCheck:checked ~ .vftSw,
.vftRailRow.vftConflictOn .vftSw{ background:#006268; }
.vftRailCheck:checked ~ .vftSw::after,
.vftRailRow.vftConflictOn .vftSw::after{ left:18px; }
.vftRailCheck:focus-visible ~ .vftSw{ outline:2px solid #006268; outline-offset:2px; }

/* ---- the workspace: head over rail + map, scenario column beside ----- */
/* newVersions and step 5. The body is a column: a top row (.vft-ws-top) that
   absorbs the slack, and under it whatever a page adds (newVersions' paint
   panel). In the top row the head sits over the rail and the map only
   (.vft-ws-main), and the scenario column runs up to the top of the page, its
   title level with the page's. The head's left padding is the rail plus the
   gap, so the title is centred over the MAP, which is within ~10px of the
   page's centre. The column heights are in R/layout_helpers.R. */
.vft-fit-page > .vft-ws-body{ padding-top:6px; }
.vft-ws-body > .vft-ws-top{ flex:1 1 auto; min-height:0; max-height:840px; display:flex; gap:16px; }
.vft-ws-main{ flex:1 1 auto; min-width:0; display:flex; flex-direction:column; }
.vft-ws-main > .vft-ws-head{ flex:0 0 auto; }
.vft-head.vft-ws-head{ padding:0 0 8px 216px; }
.vft-ws-maprow{ flex:1 1 auto; min-height:0; display:flex; gap:16px; }
.vft-ws-col.vft-ws-side{ max-height:none; }
.vft-ws-rail{ flex:0 0 200px; display:flex; flex-direction:column; gap:12px;
  min-height:0; overflow-y:auto; }
.vft-ws-mapslot{ flex:1 1 auto; min-width:0; min-height:200px; position:relative;
  border:1px solid #d6d9d8; border-radius:10px; overflow:hidden; }
.vft-ws-side{ flex:0 0 220px; width:220px; }
.vft-ws-side .vft-ws-list{ padding:0 10px; }

/* the scenario list: a 2-column grid of square cards, a tile that is not a scenario first */
.vftAddCard{ width:100%; aspect-ratio:1 / 1; padding:4px; border:2px dashed #aab4b2;
  border-radius:10px; background:#f7f9f9; color:#006268; display:flex; flex-direction:column;
  align-items:center; justify-content:center; gap:4px; font-size:13px; font-weight:600;
  line-height:1.15; }
.vftAddCard:hover{ border-color:#006268; background:#e3f0f0; }
.vftAddCard:disabled{ opacity:.45; background:#f7f9f9; border-color:#aab4b2; }
/* step 5's first tile opens newVersions, where the scenarios are made - it is
   not itself 'add a scenario', so it is a filled teal card and not the dashed
   '+' tile newVersions uses for that. Icon in the middle, label at the foot. */
.vftManageCard{ width:100%; aspect-ratio:1 / 1; padding:8px 4px 9px; border:0; border-radius:10px;
  background:#006268; color:#ffffff; display:flex; flex-direction:column; align-items:center;
  font-size:13px; font-weight:600; line-height:1.15; text-align:center; }
.vftManageCard > svg{ flex:1 1 auto; min-height:0; }
.vftManageCard:hover{ background:#004e53; }
.vftManageCard:disabled{ opacity:.45; background:#006268; }
.vftConfirmBtn{ display:inline-flex; align-items:center; justify-content:center; gap:8px;
  width:180px; height:60px; padding:0 6px; border:0; border-radius:10px; background:#006268;
  color:#ffffff; font-size:16px; font-weight:700; white-space:normal; line-height:1.15; }
.vftConfirmBtn svg{ flex:0 0 auto; }
.vftConfirmBtn:hover{ background:#004e53; }
.vftConfirmBtn:disabled{ opacity:.45; }
"
