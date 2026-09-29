#### newVersions UI - create and edit scenarios ####

#A material's swatch colour, straight from the palette. The buttons used to
#restate their hexes by hand in inline CSS and had to be edited together with
#PAINT_CATEGORIES; now there is one table.
nvPaintHex <- function(name) PAINT_CATEGORIES$hex[match(name, PAINT_CATEGORIES$name)]

#The scene beside the level switch: a tree, a bench, grass and a house. The
#ground group (grass, bench, the house's ground floor) and the canopy group (the
#crown, the roof) are dimmed and lit by CSS off the level checkbox - see
#.plvGround/.plvCanopy below - so the scene needs no server round trip.
nvLevelSceneSVG <- function(){
  shiny::HTML(paste0(
    '<svg viewBox="0 0 120 100" preserveAspectRatio="none" aria-hidden="true">',
    '<rect class="plvBand plvBandC" x="0" y="0" width="120" height="50"/>',
    '<rect class="plvBand plvBandG" x="0" y="50" width="120" height="50"/>',
    '<line x1="0" y1="50" x2="120" y2="50" stroke="#cfd4d2" stroke-width="1" stroke-dasharray="3 3"/>',
    '<rect x="24" y="44" width="6" height="44" rx="1.5" fill="#7a5230"/>',
    '<g class="plvGround">',
      '<rect x="0" y="88" width="120" height="12" fill="#86c46f"/>',
      '<path d="M4 88 l2.5 -6 l2.5 6 M16 88 l2 -5 l2 5 M36 88 l2.5 -6 l2.5 6 M66 88 l2 -5 l2 5 M112 88 l2.5 -6 l2.5 6" fill="none" stroke="#5d9e4a" stroke-width="1.4" stroke-linejoin="round"/>',
      '<rect x="44" y="71" width="2" height="10" fill="#6d4c41"/><rect x="58" y="71" width="2" height="10" fill="#6d4c41"/>',
      '<rect x="42" y="70" width="20" height="3" rx="1.5" fill="#8d6e63"/>',
      '<rect x="40" y="79" width="24" height="3.5" rx="1.5" fill="#8d6e63"/>',
      '<rect x="43" y="82" width="2.5" height="6" fill="#6d4c41"/><rect x="58.5" y="82" width="2.5" height="6" fill="#6d4c41"/>',
      '<rect x="74" y="57" width="38" height="31" fill="#efe2c6" stroke="#9c8a66" stroke-width="1"/>',
      '<rect x="88" y="71" width="9" height="17" fill="#8d6e63"/>',
      '<rect x="77" y="62" width="8" height="7" fill="#bcdcf0" stroke="#9c8a66" stroke-width="0.8"/>',
      '<rect x="101" y="62" width="8" height="7" fill="#bcdcf0" stroke="#9c8a66" stroke-width="0.8"/>',
    '</g>',
    '<g class="plvCanopy">',
      '<rect x="103" y="31" width="5" height="13" fill="#a4432e"/>',
      '<polygon points="69,58 93,28 117,58" fill="#c0533a"/>',
      '<circle cx="17" cy="36" r="10" fill="#3b8a3e"/><circle cx="37" cy="36" r="10" fill="#3b8a3e"/>',
      '<circle cx="27" cy="20" r="11" fill="#2f7d32"/><circle cx="27" cy="31" r="14" fill="#2f7d32"/>',
    '</g>',
    '</svg>'))
}

#One material. The swatch is the paint colour; the selected ring is teal and
#comes from the colorBtnSelected class setPaintColor() moves between buttons.
nvMaterialButton <- function(inputId, label, hex, selected = FALSE, extraClass = NULL, sub = NULL){
  shiny::tags$button(
    id = inputId, type = "button",
    class = paste("vftB action-button vftMat",
                  if(selected) "colorBtnSelected" else "colorBtnNotSelected", extraClass),
    shiny::span(class = "vftSwatch", style = paste0("background:", hex, ";")),
    shiny::span(label),
    sub)
}

nvDockHead <- function(label) shiny::div(class = "vftDockHead", label)

NV_CSS <- "
/* ---- head: the title centred over the centred context control ----- */
/* The head is the shared workspace head (R/ui_theme.R): over the rail and
   the map only, centred over the map, the scenario column's title level
   with it. */
.vft-nv-ctx .form-group{ margin:0; }

/* the context radios as one segmented control. The server renders them
   (contextChoice_ui) and may disable the Hitzeminderung choice; the disabled
   label then keeps the opacity that render's script gives it. */
#newVersions-contextChoice .shiny-options-group,
#newVersions-heatBin .shiny-options-group{ display:flex; background:#e9eded; }
#newVersions-contextChoice .shiny-options-group{ gap:3px; padding:3px; border-radius:8px; }
#newVersions-contextChoice .radio-inline,
#newVersions-heatBin .radio-inline{ position:relative; margin:0; padding:0; }
#newVersions-contextChoice .radio-inline + .radio-inline,
#newVersions-heatBin .radio-inline + .radio-inline{ margin-left:0; }
#newVersions-contextChoice input,
#newVersions-heatBin input{ position:absolute; opacity:0; width:1px; height:1px; margin:0; }
/* step5_ui.R puts `.radio-inline span{margin-left:10px}` in <head>, where it
   reaches every radio label in the app - here it pushed each label (and the
   translation span inside it) 10px right and clipped the long ones */
#newVersions-contextChoice .radio-inline span,
#newVersions-heatBin .radio-inline span{ margin-left:0; line-height:normal; }
#newVersions-contextChoice .radio-inline > span{ display:flex; align-items:center; gap:8px;
  height:22px; padding:0 16px; border-radius:6px; font-weight:600; color:#4f5856; cursor:pointer; }
#newVersions-contextChoice input:checked + span,
#newVersions-heatBin input:checked + span{ background:#ffffff; color:#006268;
  box-shadow:0 1px 3px rgba(0,0,0,.18); }
#newVersions-contextChoice input:focus-visible + span,
#newVersions-heatBin input:focus-visible + span{ outline:2px solid #006268; outline-offset:1px; }

/* ---- the body ------------------------------------------------------ */
/* Rail | map | scenario column, with the paint panel (Hitzeminderung only)
   under all three. The layout, the rail and its switch rows, the '+' tile and
   the confirm button are shared with step 5 and live in R/ui_theme.R; the
   column heights in R/layout_helpers.R. The paint panel takes its natural
   height, so showing it costs the map and the scenario list the same height
   and the bottom of the map and of the confirm button stay on one line. */

/* ---- the paint panel (Hitzeminderung) ------------------------------ */
/* One line down to a 1440px window. Wraps rather than overflows below that:
   the heat section drops to a second line instead of pushing the panel past
   the page edge. The 10px gap is what makes 1440 fit - at 14 it wrapped. */
.vftDock{ display:flex; flex-wrap:wrap; align-items:flex-start; gap:12px 10px;
  padding:10px 16px 14px; background:#ffffff; border:1px solid #d6d9d8; border-radius:10px; }
.vftDockSec{ display:flex; flex-direction:column; gap:8px; }
.vftDockHead{ height:14px; line-height:14px; font-size:11px; font-weight:700;
  letter-spacing:.08em; text-transform:uppercase; color:#5b6462; white-space:nowrap; }
.vftDockSep{ width:1px; align-self:stretch; background:#e4e7e6; }
/* the heat section sits under the scenario column: margin-left:auto pushes it
   to the right end, and its width is the column's 220px less the panel's
   16px right padding, so its left edge lines up with the column's. */
.vftDockHeat{ margin-left:auto; width:204px; }

/* LEVEL SWITCH (up = canopy, down = ground). The checkbox is a transparent
   overlay over the whole scene and track, so a click anywhere flips it, and it
   is still a plain Shiny input - no custom JS. `input.` and not a bare class:
   bootstrap sets input[type=checkbox]{margin:4px 0 0}, which outranks a lone
   class and moved the overlay 4px down - that used to be 4px of page, i.e. a
   scrollbar on the pane.
   The knob stops are the two material rows beside it: 44px each with a 12px
   gap, so the knob (42px, inside a 1px border) sits at 1 and 55. */
.paintLevelSwitch{ position:relative; display:flex; gap:8px; width:216px; height:100px;
  margin:0; font-weight:400; cursor:pointer; }
input.paintLevelCheckbox{ position:absolute; top:0; left:0; width:100%; height:100%;
  opacity:0; margin:0; cursor:pointer; z-index:2; }
.paintLevelScene{ display:block; flex:0 0 120px; height:100px; box-sizing:border-box;
  border:1px solid #dfe3e2; border-radius:10px; overflow:hidden; background:#fafbfb; }
.paintLevelScene svg{ display:block; width:100%; height:100%; }
.plvBand{ fill:transparent; transition:fill .25s; }
.plvGround, .plvCanopy{ transition:opacity .25s; }
.plvCanopy{ opacity:.25; }
.paintLevelCheckbox:checked ~ .paintLevelScene .plvCanopy{ opacity:1; }
.paintLevelCheckbox:checked ~ .paintLevelScene .plvGround{ opacity:.25; }
.paintLevelCheckbox:not(:checked) ~ .paintLevelScene .plvBandG,
.paintLevelCheckbox:checked ~ .paintLevelScene .plvBandC{ fill:#dcecec; }
.paintLevelTrack{ position:relative; flex:1 1 auto; height:100px; box-sizing:border-box;
  border:1px solid #d6dbda; border-radius:10px; background:#eef2f2; }
.paintLevelSlot{ position:absolute; left:0; right:0; height:49px; display:flex;
  align-items:center; justify-content:center; font-size:13px; font-weight:600; color:#6b7472; }
.plvSlotCanopy{ top:0; }
.plvSlotGround{ bottom:0; }
.paintLevelKnob{ position:absolute; left:4px; right:4px; top:55px; height:42px;
  border-radius:7px; background:#006268; color:#ffffff; display:flex; align-items:center;
  justify-content:center; gap:5px; font-size:14px; font-weight:700;
  box-shadow:0 2px 5px rgba(0,98,104,.35); transition:top .25s ease; }
.paintLevelKnob svg{ transform:rotate(180deg); transition:transform .25s; }
.paintLevelCheckbox:checked ~ .paintLevelTrack .paintLevelKnob{ top:1px; }
.paintLevelCheckbox:checked ~ .paintLevelTrack .paintLevelKnob svg{ transform:none; }
.knobLabelCanopy{ display:none; }
.paintLevelCheckbox:checked ~ .paintLevelTrack .knobLabelCanopy{ display:inline; }
.paintLevelCheckbox:checked ~ .paintLevelTrack .knobLabelGround{ display:none; }
.paintLevelCheckbox:focus-visible ~ .paintLevelTrack{ outline:2px solid #006268; outline-offset:2px; }
/* greyed with the materials whenever the brush is shut - on the original, and
   while heat is shown. The disabling is shinyjs::toggleState on the checkbox,
   but the checkbox is invisible, so what dims is what it sits on. */
.paintLevelCheckbox:disabled ~ span{ opacity:.35; }
.paintLevelCheckbox:disabled{ cursor:default; }

/* MATERIALS, laid out as COLUMNS rather than a canopy row over a ground row:
   the height bar has to be able to open between any two of them and span both
   levels, and a row can only take it at its own height. The block (both
   levels, full height) first, then two columns of canopy over ground, then
   the three ground-only buttons at the foot - so every canopy button is on
   the top row, packed left against the block, and every ground button on the
   bottom one, which is where the level switch's knob stops. Each item has an explicit `order` for the bar to slot
   in after (see .paintHeightAt_*). */
.vftMatWrap{ display:flex; align-items:flex-end; gap:6px; height:100px; }
.vftMatCol{ display:flex; flex-direction:column; gap:12px; }
.vftMatCol > .vftMat{ justify-content:flex-start; }
.vftOrd10{ order:10; } .vftOrd20{ order:20; } .vftOrd30{ order:30; }
.vftOrd40{ order:40; } .vftOrd50{ order:50; } .vftOrd60{ order:60; }
.vftMat{ display:flex; align-items:center; gap:6px; height:44px; padding:0 8px 0 7px;
  border:1px solid #d3d7d5; border-radius:8px; background:#ffffff; color:#1f2624;
  font-size:14px; font-weight:600; white-space:nowrap; transition:opacity .2s; }
.vftMat:hover{ border-color:#9fb0ae; }
.vftSwatch{ flex:0 0 16px; width:16px; height:16px; border-radius:4px;
  box-shadow:inset 0 0 0 1px rgba(0,0,0,.2); }
.vftMat.colorBtnSelected{ border-color:#006268; box-shadow:0 0 0 2px #006268; background:#e3f0f0; }
.vftMat.vftBlock{ flex-direction:column; justify-content:center; width:100px; height:100px;
  white-space:normal; line-height:1.1; text-align:center; gap:5px; }
.vftBlock .vftSwatch{ flex:0 0 24px; width:24px; height:24px; border-radius:6px; }
.vftBlockSub{ font-size:11px; font-weight:400; color:#5b6462; }
.vftDock .paintBtnDisabled,
.vftDock .vftMat:disabled,
.vftDock .vftTool:disabled,
.vftDock .paintHeightBtn:disabled{ opacity:.35; }

/* HEIGHT: one swatch per step of the armed material's ramp, tallest on top,
   and the swatch colour IS the height - see PAINT_CATEGORIES. It pops up
   directly RIGHT OF THE ARMED MATERIAL and pushes the buttons after it along:
   showHeightBar() in newVersions_server.R gives the bar the class of the armed
   ramp, whose order falls just after that material's column. CSS order rather
   than moving the node, so the swatches' input bindings are never touched.
   flex:1 on the swatches so a 5-step and a 3-step ramp both come to 100px. */
.paintHeightBar{ height:100px; width:56px; flex:0 0 56px; }
.paintHeightBar.paintHeightAt_artificial_block{ order:15; }
.paintHeightBar.paintHeightAt_canopy_artificial{ order:25; }
.paintHeightBar.paintHeightAt_canopy_tree{ order:35; }
.paintHeightGroup{ display:flex; flex-direction:column; height:100%; gap:3px; }
.paintHeightBtn{ flex:1 1 0; min-height:0; width:100%; padding:0; border:0; border-radius:5px;
  font-size:11px; font-weight:700; line-height:1; white-space:nowrap; }
.paintHeightBtn.colorBtnSelected{ box-shadow:0 0 0 2px #ffffff, 0 0 0 4px #006268; }

/* TOOLS: plan import across the top, eraser and reset under it. The import
   label is the long one ('Bestehenden Plan laden'), so it may wrap. */
.vftToolGrid{ display:grid; grid-template-columns:repeat(2, minmax(0, 1fr)); gap:12px 8px; width:196px; }
.vftTool{ display:flex; align-items:center; justify-content:center; gap:6px; height:44px;
  padding:0 6px; border:1px solid #d3d7d5; border-radius:8px; background:#ffffff;
  color:#1f2624; font-size:14px; font-weight:600; white-space:nowrap; }
.vftTool:hover{ border-color:#9fb0ae; }
.vftToolWide{ grid-column:1 / -1; white-space:normal; line-height:1.15; text-align:center; }
/* an icon is never what gives way when a translated label is long */
.vftB svg{ flex:0 0 auto; }
/* held down: the eraser while it is on, the heat button while a map is shown.
   .vftB for the weight: a bare class loses to .vftTool/.vftHeatBtn, which
   come later and set their own white background. */
.vftB.paintToolActive,
.vftB.paintToolActive:hover{ background:#006268; border-color:#006268; color:#ffffff; }

/* HEAT. One button that computes, shows and hides: 'Hitze berechnen' while the
   selected scenario has no map at this time of day, 'Hitze anzeigen' once it
   has one, 'Hitze ausblenden' while a map is up (held down, .paintToolActive),
   because that is what the next click does. All three labels are in the
   markup, so the client can translate them, and the server only moves classes
   (heatLabelSync(), showHeat()/hideHeat()). */
.vftHeatBtn{ position:relative; display:flex; align-items:center; justify-content:center;
  gap:8px; width:100%; height:44px; padding:0 30px; border:1px solid #006268;
  border-radius:8px; background:#ffffff; color:#006268; font-size:15px; font-weight:700;
  white-space:nowrap; }
.vftHeatLblShow{ display:none; }
.vftHeatHasMap .vftHeatLblShow{ display:inline; }
.vftHeatHasMap .vftHeatLblCalc{ display:none; }
.vftHeatLblHide{ display:none; }
.paintToolActive .vftHeatLblHide{ display:inline; }
.paintToolActive .vftHeatLblCalc, .paintToolActive .vftHeatLblShow{ display:none; }
/* WORK IN PROGRESS: the heat model runs in a daemon for seconds, so the button
   that started it says so - a ring turning at its right end. A pseudo-element,
   so it adds no box and the label cannot shift; pointer-events:none because the
   button is disabled for the duration (heatWorking()). Bootstrap-style dimming
   of a disabled button would read as 'off', so the dimming moves from the
   button to its contents and the ring stays at full strength. */
.paintToolBusy::after{ content:''; position:absolute; right:10px; top:50%; width:16px;
  height:16px; margin-top:-8px; box-sizing:border-box; border-radius:50%;
  border:3px solid transparent; border-top-color:currentColor;
  animation:paintToolSpin .8s linear infinite; pointer-events:none; }
.paintToolBusy:disabled{ opacity:1; }
.paintToolBusy:disabled > *{ opacity:.65; }
@keyframes paintToolSpin{ to{ transform:rotate(360deg); } }

/* TIME OF DAY: a radio group styled as a segmented control, each choice a
   sun icon over its label */
#newVersions-heatBin{ margin:0; width:100%; }
#newVersions-heatBin .shiny-options-group{ gap:2px; padding:3px 2px; border-radius:9px; }
#newVersions-heatBin .radio-inline{ flex:1 1 0; min-width:0; font-weight:600; }
#newVersions-heatBin .radio-inline > span{ display:flex; flex-direction:column; align-items:center;
  justify-content:center; gap:1px; height:42px; padding:0 1px; border-radius:7px;
  font-size:11px; letter-spacing:-0.01em; white-space:nowrap; color:#4f5856; cursor:pointer; }
#newVersions-heatBin svg{ width:18px; height:18px; }
#newVersions-heatBin input:disabled + span{ opacity:.5; cursor:default; }

/* HEAT MODE: while a heat map is on screen the brush is refused, so rather
   than a panel of greyed buttons the panel holds the heat controls alone,
   centred. The server sets the class with the brush gates (applyPaintGates()).
   The heat section is exactly as tall as the level switch and the materials
   (14px head + 8px gap + 100px), so the panel keeps its height and the map
   above it does not move. */
.vftDock.vftHeatMode{ justify-content:center; }
.vftDock.vftHeatMode > :not(.vftDockHeat){ display:none; }
.vftDock.vftHeatMode .vftDockHeat{ margin-left:0; }
.vftDock.vftHeatMode .vftDockHead{ text-align:center; }

/* A CLICK ON A DISABLED CONTROL is answered (see the vft-why-disabled script
   below and 'A CLICK ON A DISABLED CONTROL' in newVersions_server.R). A
   disabled element gets no click event, so these let the click through to
   whatever is under them, where the script picks it up. */
#newVersions-paintColorButtonsDiv button:disabled,
#newVersions-paintColorButtonsDiv input:disabled,
#newVersions-showConflicts:disabled,
#newVersions-showConflictsOrig:disabled,
#newVersions-showUsage:disabled,
#newVersions-newVersionsConfirmButton:disabled{ pointer-events:none; }

/* ---- the scenario column ------------------------------------------- */
/* The list is a 2-column grid of square cards: the dashed '+' tile first,
   then one .vftCardSlot per scenario. The cards are insertUI'd into
   #placeholder, which enter() removes and re-inserts on every visit, so
   #placeholder is display:contents and its cards become cells of the same
   grid as the tile in front of it. */
#topPlaceHolder_newVersion{ display:grid; grid-template-columns:repeat(2, minmax(0, 1fr));
  gap:10px; padding:10px 0; }
#placeholder{ display:contents; }
.vftCardSlot{ min-width:0; }
/* the delete X, in the card's top-right corner */
.vftCardDel{ position:absolute; top:4px; right:4px; z-index:3; width:22px; height:22px;
  padding:0; border:0; border-radius:50%; background:transparent; color:#6b7472;
  display:flex; align-items:center; justify-content:center; }
.vftCardDel:hover{ background:#f6e1df; color:#b3261e; }
.vftCardDel:disabled{ opacity:.35; background:transparent; }

.leaflet .legend{ font-size:15px; }
/* the path usage overlay is a picture over the network being edited: its WebGL
   canvas sits above the edges' canvas and would take every click meant for
   them, so its pane is click-through, all of it. (Pane 'usageLayer':
   Leaflet's createPane() drops 'Pane' from a name when it builds the class,
   so 'usagePane' would not match.) */
.leaflet-usageLayer-pane, .leaflet-usageLayer-pane *{ pointer-events:none !important; }
"

newVersions_ui <- function(id, i18n){
vftDbg("UI6")
      ns <- function(x) shiny::NS(id, x)

      #vft-fit-page: this step is a flex column exactly as tall as the pane, and
      #the body - vft-ws-body - fills it. The head sits inside the body, over
      #the map, and the map takes whatever it leaves. Nothing is reserved for
      #the head, because the context radio group in it is server-rendered
      #(contextChoice_ui) and so invisible to any static measurement: a
      #translation wrapping or a media query firing just moves the boundary.
      #See R/layout_helpers.R.
      shiny::fluidPage(class = "vft-fit-page",
        #activate translation for this ui
        shiny.i18n::usei18n(i18n),
        shinyjs::useShinyjs(),
        shiny::tags$head(shiny::tags$style(shiny::HTML(NV_CSS))),
              #the page banner (language select, title, logo, help/info) now
              #lives once in the nav bar - see vftStepNav() in R/app_ui.R.
              #These three inputs stay, just hidden: this step's server still
              #listens for its own languageSelect_7 / helpButton6 /
              #infoButton6 unchanged, and vftNavBannerProxyServer() drives
              #them from the nav bar's single visible control while this step
              #is current.
              shinyjs::hidden(
                shiny::selectInput(inputId = ns("languageSelect_7"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                                   selected = "de", width = 100 ),
                shiny::actionButton(inputId = ns("helpButton6"), label = ""),
                shiny::actionButton(inputId = ns("infoButton6"), label = "")
              ),

        #BODY ####
        shiny::div(class = "vft-ws-body",
          shiny::div(class = "vft-ws-top",

          #the head over the rail and the map; the scenario column beside
          #this runs up to the top of the page
          shiny::div(class = "vft-ws-main",

            #HEAD ####
            shiny::div(class = "vft-head vft-ws-head",
              shiny::h4(shiny::strong(i18n$t("Neue Szenarien erstellen"))),
              shiny::div(class = "vft-nv-ctx", shiny::uiOutput(outputId = ns("contextChoice_ui")))
            ),

           shiny::div(class = "vft-ws-maprow",

            #RAIL: everything that is shown on the map, one switch each ####
            shiny::div(class = "vft-ws-rail",
              shiny::div(class = "vftRailCard",
                shiny::div(class = "vftRailHead", i18n$t("Kartenebenen")),
                #the painted materials. Hitzeminderung only - the server shows
                #this row with the paint dock (paintContext in the render)
                shiny::div(id = ns("showMaterialsRow"), style = "display:none;",
                  vftSwitchRow(ns("showMaterials"), i18n$t("Materialien"), "mat", checked = TRUE)),
                vftSwitchRow(ns("showSM"),  i18n$t("Sensitivitäts-Matrix"), "sm"),
                vftSwitchRow(ns("showPA"),  i18n$t("Schutzgebiete"),        "pa"),
                vftSwitchRow(ns("showAOI"), i18n$t("Zielgebiete"),          "aoi")
              ),
              #CONFLICTS AND USAGE. The biodiversity-recreation conflicts step 5
              #found for the selected scenario, the Original's (searched for here
              #if step 5 never did - see "SHOW THE ORIGINAL'S CONFLICTS"), and
              #step 5's simulated path usage. Each is disabled until the server
              #has checked there is something to show - see the conflict and
              #usage observers in newVersions_server.R.
              shiny::div(class = "vftRailCard",
                shiny::div(class = "vftRailHead", i18n$t("Auswertung")),
                vftToggleButton(ns("showConflicts"),     i18n$t("Konflikte"),             "conf"),
                vftToggleButton(ns("showConflictsOrig"), i18n$t("Konflikte im Original"), "orig"),
                vftToggleButton(ns("showUsage"),         i18n$t("Wegnutzung"),            "usage")
              )
            ),

            #MAP ####
            shiny::div(class = "vft-ws-mapslot",
              shinycssloaders::withSpinner(leaflet::leafletOutput(ns("versionMap"), height = 600),
                                           type = 3, color = VFT_TEAL, color.background = "white")
            )
           )
          ),

            #SCENARIOS ####
            #the same height as the map beside it - the scenario list is what
            #flexes, so the confirm button at the foot of this column and the
            #bottom of the map end on the same line. See R/layout_helpers.R.
            shiny::div(class = "vft-ws-col vft-scencol vft-ws-side",
              shiny::fluidRow(shiny::column(12,
                shiny::h4(shiny::HTML(paste0(i18n$t("Erstellen/auswählen Sie"), "<br>", i18n$t("ein Szenario"))))
              )),
              shiny::div(style = "height: 6px"),

              #the row that absorbs this column's slack: the scenario list grows
              #and shrinks with the screen so that the confirm button under it
              #stays put. The box already scrolls, so what a short screen costs
              #is rows of the list, not the button.
              shiny::fluidRow(class = "vft-ws-listrow",
                shiny::column(12, class = "vft-ws-list vft-vlist",
                       style = "border: 1px solid #d6d9d8; border-radius: 10px; vertical-align:middle; width: 200px; overflow-y: auto;",
                  shinyjs::inlineCSS(list(.selected = "border-width: 3px; border-color: #006268")),
                  shinyjs::inlineCSS(".selected:focus {border-width: 3px; border-color: #006268; background-color: white} "),
                  shinyjs::inlineCSS(list(.notSelected = "border-width: 1px; border-color: #b9c1bf")),
                  shinyjs::inlineCSS(list(.original = "border-width: thick; border-color: grey")),
                  #The scenario cards' names: Bootstrap's .btn never wraps, so a
                  #long name ran out of the square. Wrap it (breaking a long word
                  #if it must), centre it both ways, and cut it off with an
                  #ellipsis after four lines rather than overflow the card.
                  #Scoped to the select buttons - the X in the corner is a button
                  #too. Square by aspect-ratio: the card is half the list's width.
                  shinyjs::inlineCSS(paste(
                    "#topPlaceHolder_newVersion .vftCard { position: relative; display: block; }",
                    "#topPlaceHolder_newVersion button[id*='versionBtn'] {",
                    "  display: flex; align-items: center; justify-content: center;",
                    "  width: 100%; aspect-ratio: 1 / 1; background: #ffffff;",
                    "  white-space: normal; overflow: hidden; padding: 6px 8px 18px; line-height: 1.2;",
                    "  border-radius: 10px;",
                    "}",
                    "#topPlaceHolder_newVersion button[id*='versionBtn'] .action-label {",
                    "  display: -webkit-box; -webkit-box-orient: vertical; -webkit-line-clamp: 4;",
                    "  overflow: hidden; max-width: 100%; overflow-wrap: anywhere; text-align: center;",
                    "}")),
                  #THE HEAT ICONS at a card's foot, one per time of day on
                  #every card (heatIconsTag() in R/heat_helpers.R): in colour
                  #where a stored map still applies, greyed where there is
                  #none yet - clicking a grey one computes it. Shown on the
                  #Hitzeminderung context only - the server puts .vftHeatCtx
                  #on the column there. .vftHeatStale is paintbrush.js
                  #greying them the moment a stroke starts, ahead of the
                  #flush that makes it true on the server. .vftHeatBusy is
                  #a job in flight (heatWorking()), which refuses clicks.
                  #The card of the map on screen goes red, over the teal
                  #of the selection, since that may be another card.
                  shinyjs::inlineCSS(paste(
                    ".vftHeatIcons { position: absolute; left: 4px; bottom: 4px; display: none; gap: 2px; z-index: 2; }",
                    "#topPlaceHolder_newVersion.vftHeatCtx .vftHeatIcons { display: flex; }",
                    ".vftHeatIcon { width: 24px; height: 24px; box-sizing: border-box; padding: 0;",
                    "  border-radius: 50%; border: 1px solid #888; background: #ffffff; opacity: 0.75;",
                    "  display: flex; align-items: center; justify-content: center; cursor: pointer; }",
                    ".vftHeatIcon:hover { opacity: 1; }",
                    ".vftHeatIcon.vftHeatNone, .vftHeatStale .vftHeatIcon { filter: grayscale(1); opacity: 0.4;",
                    "  border-style: dashed; }",
                    ".vftHeatIcon.vftHeatNone:hover, .vftHeatStale .vftHeatIcon:hover { opacity: 0.8; }",
                    "#topPlaceHolder_newVersion.vftHeatBusy .vftHeatIcon { pointer-events: none; }",
                    ".vftHeatIcon.vftHeatShown { opacity: 1; border: 3px solid red; }",
                    ".vftHeatIcons .vftHeatIcon.vftHeatPending { filter: none; opacity: 1; border: 2px solid #006268;",
                    "  animation: vftHeatPulse 0.9s ease-in-out infinite alternate; }",
                    "@keyframes vftHeatPulse { from { box-shadow: 0 0 0 0 rgba(0,98,104,0.5); }",
                    "  to { box-shadow: 0 0 0 4px rgba(0,98,104,0); } }",
                    ".vftCard button:disabled ~ .vftHeatIcons { pointer-events: none; opacity: 0.4; }",
                    "#topPlaceHolder_newVersion button.vftHeatCard { border-width: thick !important; border-color: red !important; }")),
                  #...and the server's replacement for one card's strip,
                  #as markup, so the strip has one generator and it is R's
                  shiny::tags$script(shiny::HTML(paste(
                    "if(!window.__vftHeatIcons){ window.__vftHeatIcons = true;",
                    "Shiny.addCustomMessageHandler('vft-heat-icons', function(m){",
                    "  var el = document.querySelector('.vftHeatIcons[data-card=\"' + m.card + '\"]');",
                    "  if(el) el.outerHTML = m.html;",
                    "}); }"))),

                  #NEW SCENARIO: the first tile of the grid, dashed, with the
                  #cards after it. Outside #placeholder, which enter() empties
                  #and re-inserts after it on every visit.
                  shiny::div(id = "topPlaceHolder_newVersion",
                      shiny::tags$button(
                        id = ns("addVersionButton"), type = "button",
                        class = "vftB action-button vftAddCard",
                        shiny::HTML(paste0('<svg width="24" height="24" viewBox="0 0 24 24" fill="none" ',
                                           'stroke="currentColor" stroke-width="2.4" stroke-linecap="round" ',
                                           'aria-hidden="true"><path d="M12 5v14"/><path d="M5 12h14"/></svg>')),
                        shiny::span(i18n$t("Neues Szenario"))),
                      shiny::div(id = "placeholder")
                  )
                )
              ),

              shiny::div(style = "height: 16px"),

              shiny::fluidRow(shiny::column(12,
                shiny::tags$button(id = ns("newVersionsConfirmButton"), type = "button",
                                   class = "vftB action-button vftConfirmBtn",
                                   i18n$t("Szenarien bestätigen"))
              ))
            )
          ),

          #PAINT PANEL (heat mitigation, context 4) ####
          #Hidden until the context 4 render shows it. Spans the whole width
          #under the map and the scenario column: level, materials, height and
          #tools on the left, the heat read-out under the scenario column.
          shiny::div(
            id = ns("paintColorButtonsDiv"), class = "vftDock", style = "display:none;",

            #LEVEL SWITCH (up = canopy, down = ground)
            shiny::div(class = "vftDockSec",
              nvDockHead(i18n$t("Ebene")),
              shiny::tags$label(
                class = "paintLevelSwitch",
                shiny::tags$input(id = ns("paintLevel"), type = "checkbox", class = "paintLevelCheckbox"),
                shiny::tags$span(class = "paintLevelScene", nvLevelSceneSVG()),
                shiny::tags$span(
                  class = "paintLevelTrack",
                  shiny::tags$span(class = "paintLevelSlot plvSlotCanopy", i18n$t("Krone")),
                  shiny::tags$span(class = "paintLevelSlot plvSlotGround", i18n$t("Boden")),
                  shiny::tags$span(
                    class = "paintLevelKnob",
                    vftIcon("up"),
                    shiny::tags$span(class = "knobLabelCanopy", i18n$t("Krone")),
                    shiny::tags$span(class = "knobLabelGround", i18n$t("Boden"))
                  )
                )
              )
            ),
            shiny::div(class = "vftDockSep"),

            #MATERIALS, as columns (see .vftMatWrap): the block, which belongs
            #to both levels, spans both rows and is never disabled by the
            #switch; then canopy over ground twice, so the two canopy buttons
            #follow the block on the top row; then three ground-only buttons.
            shiny::div(class = "vftDockSec",
              nvDockHead(i18n$t("Material")),
              shiny::div(class = "vftMatWrap",
                nvMaterialButton(ns("paintColor_block"), i18n$t("Kuenstlicher Block"), nvPaintHex("artificial_block"),
                                 extraClass = "vftBlock vftOrd10",
                                 sub = shiny::span(class = "vftBlockSub", i18n$t("Krone + Boden"))),
                shiny::div(class = "vftMatCol vftOrd20",
                  nvMaterialButton(ns("paintColor_canopyArtificial"), i18n$t("Kuenstlich"), nvPaintHex("canopy_artificial")),
                  nvMaterialButton(ns("paintColor_natural"),          i18n$t("Natuerlich"), nvPaintHex("natural"))
                ),
                shiny::div(class = "vftMatCol vftOrd30",
                  nvMaterialButton(ns("paintColor_canopyTree"), i18n$t("Baum"),   nvPaintHex("canopy_tree")),
                  nvMaterialButton(ns("paintColor_water"),      i18n$t("Wasser"), nvPaintHex("water"))
                ),
                nvMaterialButton(ns("paintColor_grass"),      i18n$t("Gras"),       nvPaintHex("grass"),
                                 selected = TRUE, extraClass = "vftOrd40"),
                nvMaterialButton(ns("paintColor_bush"),       i18n$t("Busch"),      nvPaintHex("bush"),       extraClass = "vftOrd50"),
                nvMaterialButton(ns("paintColor_artificial"), i18n$t("Kuenstlich"), nvPaintHex("artificial"), extraClass = "vftOrd60"),

                #HEIGHT BAR, placed just right of the armed material by CSS
                #order (.paintHeightAt_*). Every step of every ramp is built
                #HERE, statically, and shown or hidden with shinyjs - not
                #rendered per material with renderUI. That keeps the swatches'
                #input ids fixed: a re-rendered actionButton arrives with its
                #click count reset, and on this page a control that silently
                #loses its value is the failure mode that has bitten hardest
                #(see the scenario-card notes in newVersions_server.R). Built by
                #looping PAINT_CATEGORIES, so colour and metres both come from
                #the one table.
                shiny::div(
                  id = ns("paintHeightBar"), class = "paintHeightBar", style = "display:none;",
                  lapply(vftHeightRamps(), function(ramp){
                    shiny::div(
                      id = ns(paste0("paintHeightGroup_", ramp$name)),
                      class = "paintHeightGroup", style = "display:none;",
                      #tallest on top, so the bar reads like the thing it
                      #describes rather than like a table sorted by id
                      lapply(rev(seq_len(nrow(ramp$steps))), function(k){
                        st <- ramp$steps[k, ]
                        shiny::tags$button(
                          id = ns(paste0("paintHeight_", st$id)), type = "button",
                          class = "vftB action-button paintHeightBtn colorBtnNotSelected",
                          style = sprintf("background-color: %s; color: %s;", st$hex, st$fg),
                          sprintf("%g m", st$height))
                      })
                    )
                  })
                )
              )
            ),
            shiny::div(class = "vftDockSep"),

            #PLAN IMPORT + ERASER + RESET. Eraser and reset act on the paint
            #only; the land cover baseline underneath is never edited, so these
            #reveal it rather than erase it.
            shiny::div(class = "vftDockSec",
              nvDockHead(i18n$t("Werkzeuge")),
              shiny::div(class = "vftToolGrid",
                #PLAN IMPORT. Opens the browser's file picker; the file never
                #reaches R - planimport.js places, classifies and previews it,
                #and only the result is sent on Apply. A plain button rather than
                #an actionButton: R has nothing to do on the click. Gated with
                #the brush in applyPaintGates().
                shiny::tags$button(
                  id = ns("paintImport"), type = "button",
                  class = "vftB vftTool vftToolWide",
                  onclick = "document.getElementById('newVersions-planFile').click();",
                  vftIcon("upload"), shiny::span(i18n$t("Bestehenden Plan laden"))),
                shiny::tags$input(id = ns("planFile"), type = "file",
                                  accept = ".pdf,.png,.jpg,.jpeg,.tif,.tiff",
                                  style = "display:none;"),
                shiny::tags$button(id = ns("paintEraser"), type = "button",
                                   class = "vftB action-button vftTool",
                                   vftIcon("eraser"), shiny::span(i18n$t("Radierer"))),
                shiny::tags$button(id = ns("paintReset"), type = "button",
                                   class = "vftB action-button vftTool",
                                   vftIcon("reset"), shiny::span(i18n$t("Reset")))
              )
            ),

            #HEAT. Reads the design rather than editing it, so it sits apart
            #from the brush tools, at the far end under the scenario column. One
            #button computes and shows: it recomputes when the paint has changed
            #since the last read-out and reuses the stored surface when it has
            #not (see the heatSwitch observer). The recompute is tied to this
            #deliberate click and not to every stroke because it costs seconds,
            #and the brush is refused while heat is on, so what is on screen
            #always describes the design underneath it.
            shiny::div(class = "vftDockSec vftDockHeat",
              nvDockHead(i18n$t("Hitze")),
              shiny::tags$button(
                id = ns("heatSwitch"), type = "button",
                class = "vftB action-button vftHeatBtn",
                vftIcon("heat"),
                shiny::span(class = "vftHeatLblCalc", i18n$t("Hitze berechnen")),
                shiny::span(class = "vftHeatLblShow", i18n$t("Hitze anzeigen")),
                shiny::span(class = "vftHeatLblHide", i18n$t("Hitze ausblenden"))),
              #TIME OF DAY. The heat model is computed for one of three bins,
              #and the ranking of materials genuinely changes between them:
              #asphalt peaks 1-2 h after solar noon while grass and water barely
              #move. Morning and afternoon share a sun elevation (45.8 deg) and
              #differ only in azimuth - the heat difference between them is
              #thermal inertia, not sun angle. Each bin has its own stored map.
              #
              #Radio buttons rather than the select this used to be: a radio's
              #label can hold the <span> i18n$t() returns, so the client
              #translates it like any other label and the server no longer has
              #to re-send the choices on a language change.
              shiny::radioButtons(
                inputId = ns("heatBin"), label = NULL, inline = TRUE,
                choiceNames = lapply(HEAT_BINS, function(b)
                  shiny::tagList(shiny::HTML(heatBinIconSVG(b)), shiny::span(i18n$t(HEAT_BIN_KEYS[[b]])))),
                choiceValues = HEAT_BINS,
                selected = HEAT_BIN_DEFAULT)
            )
          ),
          #filled by planimport.js while a plan is being placed; the paint
          #panel above is hidden meanwhile
          shiny::div(id = ns("planImportPanel"), style = "display:none;")
        ),

        #A CLICK ON A DISABLED CONTROL. The browser sends no click to a
        #disabled element, so the CSS above lets it through to what is under
        #it and this listens for it on the document, in the capture phase:
        #  - a label whose own control is disabled (the context radios, the
        #    level switch, the time of day) - the label still gets the click;
        #  - otherwise any disabled button of this page's guarded set whose box
        #    holds the pointer - checked by geometry, because with
        #    pointer-events:none it is no longer the event's target.
        #The server is told which control it was and decides what, if
        #anything, the click gets. Only this page's controls (the ns prefix).
        shiny::tags$script(shiny::HTML(sprintf(
          "(function(){
             if(window.__vftWhyDisabled) return; window.__vftWhyDisabled = true;
             var NS = '%s';
             var GUARD = ['#' + NS + 'paintColorButtonsDiv button:disabled',
                          '#' + NS + 'showConflicts:disabled',
                          '#' + NS + 'showConflictsOrig:disabled',
                          '#' + NS + 'showUsage:disabled',
                          '#' + NS + 'newVersionsConfirmButton:disabled'].join(',');
             function report(id, value){
               if(!window.Shiny || !Shiny.setInputValue) return;
               Shiny.setInputValue(NS + 'disabledClick',
                                   {id: id, value: value == null ? null : String(value), t: Date.now()},
                                   {priority: 'event'});
             }
             document.addEventListener('click', function(e){
               var t = e.target;
               if(!t || !t.closest) return;
               var lab = t.closest('label');
               if(lab){
                 var inp = lab.querySelector('input:disabled');
                 var key = inp && (inp.id || inp.name) || '';
                 if(key.indexOf(NS) === 0){ report(key, inp.type === 'radio' ? inp.value : null); return; }
               }
               var x = e.clientX, y = e.clientY, els = document.querySelectorAll(GUARD);
               for(var i = 0; i < els.length; i++){
                 var b = els[i].getBoundingClientRect();
                 if(b.width > 0 && x >= b.left && x <= b.right && y >= b.top && y <= b.bottom){
                   report(els[i].id, null); return;
                 }
               }
             }, true);
           })();", ns("")))),

        shiny::tagList(
          shiny::tags$script(src = "www/paintbrush.js"),  # at the end of the UI, outside tags$head
          #after paintbrush.js, whose internal hooks (window.__vftPaintHooks) it uses
          shiny::tags$script(src = "www/planimport.js")
        )
      )
}
