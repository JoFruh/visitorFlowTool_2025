#### Step 5 UI - launch the simulation ####

#Step 5 is laid out like newVersions - the two are the same kind of page, a
#map with a scenario column beside it - on the shared `.vft-ws-*` workspace
#classes (R/ui_theme.R): the head over the rail and the map, the scenario
#column running up to the top with its title level with the page's. The
#downloads are an action bar under the map, like every other step's, and the
#launch button's foot is level with that bar.
#
#Everything here is scoped to this page. The rules that used to stand in this
#step's <head> - `.radio{margin-bottom:30px}`, `input[type='radio']{width:20px}`
#and `.radio span{margin-left:10px}` - were unscoped and reached every radio and
#checkbox in the app; newVersions had to override them one by one.
STEP5_CSS <- "
/* the agent type: the server renders it (agentCheckbox_ui), label included, and
   re-renders it disabled when there is nothing to show. Its label is the
   card's head; each choice is a row like the switch rows under it. */
.vft5-agent .form-group{ margin:0; }
.vft5-agent .control-label{ display:block; margin:0 8px 4px; font-size:11px; font-weight:700;
  letter-spacing:.08em; line-height:1.3; text-transform:uppercase; color:#5b6462; }
.vft5-agent .radio{ margin:0; }
.vft5-agent .radio label{ display:flex; align-items:center; gap:10px; min-height:28px; margin:0;
  padding:2px 8px; border-radius:8px; font-size:14px; line-height:1.15; }
.vft5-agent .radio label:hover{ background:#f1f4f4; }
.vft5-agent .radio input[type=radio]{ position:static; flex:0 0 auto; width:16px; height:16px; margin:0; }
.vft5-agent .radio label span{ margin:0; }
.vft5-agent .radio.disabled label, .vft5-agent input:disabled + span{ opacity:.45; }
.vft5-rail .vftRailRow{ min-height:32px; }
.vft5-rail .vftRailRow > svg{ flex:0 0 auto; color:#006268; }
/* the download bar under the map: centred under the map, not under the rail
   and the map together, so the rail's width is its left padding */
.vft5-actions{ flex:0 0 auto; padding-left:216px; }

/* the scenario list: a 2-column grid of square cards, the dashed '+' tile
   first. The cards are insertUI'd into #placeholder_step5 (updateVersions()
   in step5_server.R), each in a wrapper div with a 5px spacer either side, so
   #placeholder_step5 is display:contents, its wrappers are grid cells, and the
   spacers go. The card's 120px size is an inline style, hence !important. */
#topPlaceHolder{ display:grid; grid-template-columns:repeat(2, minmax(0, 1fr)); gap:10px; padding:10px 0; }
#placeholder_step5{ display:contents; }
#placeholder_step5 > div{ min-width:0; }
#placeholder_step5 > div > div{ display:none; }
#placeholder_step5 .btn{ position:relative; display:flex; align-items:center; justify-content:center;
  width:100% !important; height:auto !important; aspect-ratio:1 / 1; margin:0; padding:6px 8px 18px;
  background:#ffffff; border-style:solid; border-radius:10px; white-space:normal; overflow:hidden;
  line-height:1.2; }
#placeholder_step5 .btn .action-label{ display:-webkit-box; -webkit-box-orient:vertical;
  -webkit-line-clamp:4; overflow:hidden; max-width:100%; overflow-wrap:anywhere; text-align:center; }
/* selected before notSelected: the server adds notSelected to the card it
   leaves without taking selected off, so the later rule has to win */
#placeholder_step5 .btn.selected{ border-width:3px; border-color:#006268; }
#placeholder_step5 .btn.notSelected{ border-width:1px; border-color:#b9c1bf; }
/* has this scenario been simulated: an empty ring or a teal tick, bottom left.
   These were two PNGs used as the whole card's background. */
#placeholder_step5 .btn::after{ content:''; position:absolute; left:6px; bottom:6px; width:18px;
  height:18px; box-sizing:border-box; border-radius:50%; }
#placeholder_step5 .btn.noSim::after{ border:2px solid #9aa3a1; }
#placeholder_step5 .btn.withSim::after{ background:#006268 url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='white' stroke-width='3.5' stroke-linecap='round' stroke-linejoin='round'%3E%3Cpath d='M20 6 9 17l-5-5'/%3E%3C/svg%3E\") center / 12px no-repeat; }

.leaflet .legend{ text-align:left; font-size:15px; }
"

step5_ui <- function(id, i18n){
vftDbg("UI6")
      ns <- function(x) shiny::NS(id, x)

      #vft-fit-page: the page is a flex column exactly as tall as the pane and
      #the workspace body fills it - see R/layout_helpers.R.
      shiny::fluidPage(class = "vft-fit-page",
        #activate translation for this ui
        shiny.i18n::usei18n(i18n),
        shinyjs::useShinyjs(),
        shiny::tags$head(shiny::tags$style(shiny::HTML(STEP5_CSS))),
        #the page banner (language select, title, logo, help/info) now lives
        #once in the nav bar - see vftStepNav() in R/app_ui.R. These three
        #inputs stay, just hidden: this step's server still listens for its
        #own languageSelect_5 / helpButton5 / infoButton5 unchanged, and
        #vftNavBannerProxyServer() drives them from the nav bar's single
        #visible control while this step is current.
        shinyjs::hidden(
          shiny::selectInput(inputId = ns("languageSelect_5"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                             selected = i18n$get_translation_language(), width = 100 ),
          shiny::actionButton(inputId = ns("helpButton5"), label = ""),
          shiny::actionButton(inputId = ns("infoButton5"), label = "")
        ),

        #BODY ####
        shiny::div(class = "vft-ws-body",
          shiny::div(class = "vft-ws-top",

          shiny::div(class = "vft-ws-main",

            #HEAD ####
            vftHead(i18n$t("Simulation der Naherholung starten:"),
                    vftSub(i18n$t("Rechts: Erstellen Sie neue Szenarien von Infrastrukturkarten (Wege, Häuser etc.) und wählen Sie diese aus."),
                           shiny::tags$br(),
                           i18n$t("Nachdem eine Simulation gestartet wurde, können Sie die anzuzeigenden Informationen auswählen (Optionen auf der linken Seite).")),
                    class = "vft-ws-head"),

            shiny::div(class = "vft-ws-maprow",

              #RAIL: what the map shows ####
              #Every display control starts disabled; the map-present observer
              #in step5_server.R enables them together once there is a
              #simulation to show, and applyConflictState() the conflict search.
              shiny::div(class = "vft-ws-rail vft5-rail",
                shiny::div(class = "vftRailCard vft5-agent",
                  shiny::uiOutput(ns("agentCheckbox_ui"))
                ),
                shiny::div(class = "vftRailCard",
                  shiny::div(class = "vftRailHead", i18n$t("Kartenebenen")),
                  vftSwitchRow(ns("SMcheckbox"),          i18n$t("Sensitivitäts-Matrix"),   "sm", disabled = TRUE),
                  vftSwitchRow(ns("aoi"),                 i18n$t("Zielgebiete"),            "aoi", disabled = TRUE),
                  vftSwitchRow(ns("onlyAOIcheckbox"),     i18n$t("Innerhalb Zielgebiete"),  "aoiIn", disabled = TRUE),
                  vftSwitchRow(ns("startingCheckbox"),    i18n$t("Agenten Ausgangspunkte"), "start", disabled = TRUE),
                  vftSwitchRow(ns("PA_Checkbox"),         i18n$t("Schutzgebiete"),          "pa", disabled = TRUE),
                  vftSwitchRow(ns("ParkingCheckbox"),     i18n$t("Parking"),                "park", disabled = TRUE),
                  vftSwitchRow(ns("ResidentialCheckbox"), i18n$t("Neue Wohngebiete"),       "resid", disabled = TRUE)
                ),
                #a search, not a layer: one click finds the conflicts of the
                #selected scenario and draws them. Enabled by
                #applyConflictState() in step5_server.R only while the selected
                #scenario has a simulation AND a sensitivity matrix exists.
                shiny::div(class = "vftRailCard",
                  shiny::div(class = "vftRailHead", i18n$t("Auswertung")),
                  shinyjs::disabled(
                    shiny::tags$button(
                      id = ns("conflictButton"), type = "button", class = "vftB action-button vftRailRow",
                      shiny::span(class = "vftGlyph", vftGlyph("conf")),
                      shiny::span(class = "vftRailLabel", i18n$t("Biodiversitäts-Erholungs-Konflikt finden")),
                      vftIcon("search")))
                )
              ),

              #MAP ####
              #The map container is STATIC and lives here for the life of the
              #session. It used to be emitted from inside `mapArea_UI`, a
              #renderUI in step5_server.R, and that is what broke every
              #leafletProxy() call after a re-entry: leaflet finds a map by
              #`$(el).data("leaflet-map")` - jQuery data on the ELEMENT, attached
              #once inside renderValue() - so re-running the renderUI inserted a
              #fresh, unregistered <div id="step5-mapAreaLeaflet"> and every
              #proxy call after it logged "Couldn't find map with id
              #step5-mapAreaLeaflet" and did nothing. That is the whole of the
              #radio buttons and the version cards going dead on the way back in.
              #newVersions_ui.R has always had its map static, which is why that
              #page never showed the symptom.
              #
              #The "no simulation yet" placeholder is an OVERLAY on top of the
              #map rather than a replacement for it - deliberately not
              #shinyjs::hide() on the map itself, because a display:none leaflet
              #container has offsetWidth 0, and leaflet then defers its render to
              #a resize() callback that only Shiny's own visibility machinery
              #fires. z-index 1200 clears leaflet's own highest layer
              #(.leaflet-top/.leaflet-bottom at 1000).
              #
              #Frame, map and overlay agree on a size by construction: the frame
              #fills the map slot and the map and the overlay are both 100% of
              #it (R/layout_helpers.R). The 600 x 884 below are what the map was
              #before it filled the slot; the stylesheet overrides both.
              shiny::div(class = "vft-ws-mapslot",
                shiny::div(class = "vft-map5-frame",
                  leaflet::leafletOutput(ns("mapAreaLeaflet"), height = 600, width = 884),
                  shiny::div(
                    id = ns("mapPlaceholder"),
                    style = paste("position: absolute; top: 0; left: 0;",
                                  "width: 100%; height: 100%;",
                                  "z-index: 1200; background-color: #ffffff;"),
                    shiny::uiOutput(ns("mapArea_UI"))
                  )
                )
              )
            ),

            #DOWNLOADS, in the action bar under the map like every other
            #step's. The launch button - the one that finishes this step - is
            #at the foot of the scenario column, level with this bar.
            shiny::div(class = "vft-actions vft5-actions",
              shiny::actionButton(ns("SMbutton"), class = "vft-btn",
                                  label = vftBtnLabel("download", i18n$t("Download: Sensitivitäts-Matrix [.tif]"))),
              shinyjs::disabled(
                shiny::actionButton(ns("pathsDwnldButton"), class = "vft-btn",
                                    label = vftBtnLabel("download", i18n$t("Download: Wege und Zielgebiete [.gpkg]")))),
              shinyjs::disabled(
                shiny::actionButton(ns("imageButton"), class = "vft-btn",
                                    label = vftBtnLabel("image", i18n$t("Ein Bild erstellen"))))
            )
          ),

            #SCENARIOS ####
            #the same height as the map beside it - the scenario list is what
            #flexes, so the launch button at the foot of this column and the
            #bottom of the map end on the same line. See R/layout_helpers.R.
            shiny::div(class = "vft-ws-col vft-scencol vft-ws-side",
              shiny::fluidRow(shiny::column(12,
                shiny::h4(shiny::HTML(paste0(i18n$t("Erstellen/auswählen Sie"), "<br>", i18n$t("ein Szenario"))))
              )),
              shiny::div(style = "height: 6px"),

              shiny::fluidRow(class = "vft-ws-listrow",
                shiny::column(12, class = "vft-ws-list vft-vlist",
                       style = "border: 1px solid #d6d9d8; border-radius: 10px; vertical-align:middle; width: 200px; overflow-y: auto;",
                  #NEW SCENARIOS: the first tile of the grid, which opens the
                  #newVersions page. The cards follow it, insertUI'd into
                  ##placeholder_step5 - see updateVersions() in step5_server.R.
                  shiny::div(id = "topPlaceHolder",
                    shiny::tags$button(
                      id = ns("newVersionsButton"), type = "button",
                      class = "vftB action-button vftAddCard",
                      vftIcon("plus", 24),
                      shiny::span(i18n$t("Neues Szenario"))),
                    shiny::div(id = "placeholder_step5")
                  )
                )
              ),

              shiny::div(style = "height: 16px"),

              #launch new simulation
              shiny::fluidRow(shiny::column(12,
                shiny::tags$button(id = ns("launchSim"), type = "button",
                                   class = "vftB action-button vftConfirmBtn",
                                   shiny::HTML(paste0(i18n$t(":Simulation:"))))
              ))
            )
          )
        ),

        shiny::div(
          shinyjs::hidden( shiny::downloadButton(ns("downloadTIFF")) ),
          shinyjs::hidden( shiny::downloadButton(ns("downloadPaths")) ),
          shinyjs::hidden( shiny::downloadButton(ns("downloadSM")) )
        )
      )

}
