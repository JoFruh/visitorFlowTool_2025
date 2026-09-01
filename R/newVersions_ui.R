#### Step 1 UI - determine area ####
newVersions_ui <- function(id, i18n){
vftDbg("UI6")
      #vft-fit-page: this step is a flex column exactly as tall as the pane, and
      #the CONTENT row below - marked vft-nv-body - takes whatever the head
      #leaves. It used to reserve a flat 60px for that head instead, and that
      #was always wrong: the head is the two h4 lines PLUS the context radio
      #group, which is server-rendered (contextChoice_ui) and so was invisible
      #to the static measurement that set the 60. Measured, it is 89-126px
      #depending on which short-screen media tier is active, so the band was 29
      #to 66px too tall and the map and the confirm button hung below the
      #bottom of the screen. Nothing is reserved now, so no number can be
      #wrong: the radio row appearing, a translation wrapping to two lines or a
      #media query firing all just move the boundary. See R/layout_helpers.R.
      shiny::fluidPage(class = "vft-fit-page",
        #activate translation for this ui
        shiny.i18n::usei18n(i18n),
              #the page banner (language select, title, logo, help/info) now
              #lives once in the nav bar - see vftStepNav() in R/app_ui.R.
              #These three inputs stay, just hidden: this step's server still
              #listens for its own languageSelect_7 / helpButton6 /
              #infoButton6 unchanged, and vftNavBannerProxyServer() drives
              #them from the nav bar's single visible control while this step
              #is current.
              shinyjs::hidden(
                shiny::selectInput(inputId = shiny::NS(id, "languageSelect_7"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                                   selected = "de", width = 100 ),
                shiny::actionButton(inputId = shiny::NS(id, "helpButton6"), label = ""),
                shiny::actionButton(inputId = shiny::NS(id, "infoButton6"), label = "")
              ),
               shiny::fluidRow(
                 shiny::column(12, align = "center",
                               shiny::h4(strong(i18n$t("Schaffung neuer Szenarien für die Infrastruktur."))),
                               shiny::h4(i18n$t("Wählen Sie die Elemente, die Sie ändern möchten.")),
                               fluidRow(
                                 column(12, align = "center",


                                        shiny::uiOutput(outputId = NS(id, "contextChoice_ui"))
                                        # shiny::radioButtons(inputId = NS(id,"contextChoice"), label = NULL, inline = TRUE, choices = list(i18n$t("Infrastruktur") = 1,  i18n$t("Parken/Wohnen") = 3))#"Beschilderung/Attraktivität" = 2,

                                 )
                               ),
                 )
               ),


               #CONTENT
               #the row that absorbs this page's slack. It is NOT marked
               #vft-grow: that class carries generic rules for the columns
               #inside it (`.vft-grow > [class*="col-"] > *`), which would win
               #on specificity over `.vft-nv-col > *` and stretch every button
               #and caption in the sidebar. vft-nv-body sizes this row and
               #stops there; the two columns keep their own internal flex.
               shiny::fluidRow(class = "vft-nv-body",
                 shiny::column(1, align = "center",
                        shiny::fluidRow(

                          shiny::column(12, align = "center",
                                 shiny::h5(shiny::strong(i18n$t("Sensitivitäts-Matrix anzeigen")))
                          )

                        ),

                        shiny::fluidRow(shiny::h5()),

                        shiny::fluidRow(

                          shiny::column(12, align = "center",
                                 shinyWidgets::prettySwitch(shiny::NS(id, "showSM"), value = FALSE, label = NULL, width = "150px",
                                              bigger = TRUE, fill = TRUE, status = "success", inline = TRUE)
                          )),
                        shiny::fluidRow(

                          shiny::column(12, align = "center",
                                        shiny::h5(shiny::strong(i18n$t("Schutzgebiete anzeigen")))
                          )
                        ),
                        shiny::fluidRow(shiny::h5()),
                        shiny::fluidRow(
                          shiny::column(12, align = "center",
                                        shinyWidgets::prettySwitch(shiny::NS(id, "showPA"), value = FALSE, label = NULL, width = "150px",
                                                                   bigger = TRUE, fill = TRUE, status = "success", inline = TRUE)
                          )),

                        shiny::fluidRow(

                          shiny::column(12, align = "center",
                                        shiny::h5(shiny::strong(i18n$t("Zielgebiete anzeigen")))
                          )
                        ),
                        shiny::fluidRow(shiny::h5()),
                        shiny::fluidRow(
                          shiny::column(12, align = "center",
                                        shinyWidgets::prettySwitch(shiny::NS(id, "showAOI"), value = FALSE, label = NULL, width = "150px",
                                                                   bigger = TRUE, fill = TRUE, status = "success", inline = TRUE)
                          ))
                 ),


                 #MAIN MAP ####

                 #code to alter legends manually (for left align etc.)
                 #vft-nv-col: this column and the scenario sidebar are the same
                 #height and each flexes internally, so their bottoms line up -
                 #the map ends where the confirm button ends. The map is the
                 #slack-taker here (vft-nv-mapslot), which also means the
                 #paint-tool block below it, hidden until the heat-mitigation
                 #context shows it, comes out of the map rather than out of the
                 #bottom of the page. See R/layout_helpers.R.
                 shiny::column(9, align = "center", class = "vft-nv-col",
                               tags$head(
                                 tags$style(
                                   ".leaflet .legend {
                                            align: left;
                                            font-size: 15px;
                                            }

                                   /* vertical ground/canopy switch (context 4).
                                      Hand-built rather than a rotated prettySwitch: the knob has to
                                      travel the full height of both button rows and carry translatable
                                      text. Still a plain Shiny input - shiny's checkboxInputBinding
                                      binds any input[type=checkbox] with an id, so no custom JS.
                                      Track height 104px = 1px border + 4 + 45 + 4 + 45 + 4 + 1px
                                      border, i.e. the two 45px button rows plus their 10px gap, so
                                      the knob lands exactly on the row it activates. */
                                   .paintLevelSwitch {
                                            position: relative; display: inline-block;
                                            width: 130px; height: 104px; cursor: pointer; margin: 0;
                                            }
                                   /* `input.` and not a bare class: bootstrap sets
                                      `input[type=checkbox]{margin:4px 0 0}`, which is one
                                      specificity point above a lone class and so kept winning
                                      here. On an absolutely positioned overlay with top:0 that
                                      margin simply moves it, so this 104px box hung 4px below
                                      its own track - and being the lowest thing in the map
                                      column, those 4px were 4px of page, i.e. a scrollbar on
                                      the pane whenever the paint tools were on show. */
                                   input.paintLevelCheckbox {
                                            position: absolute; top: 0; left: 0;
                                            width: 100%; height: 100%;
                                            opacity: 0; margin: 0; cursor: pointer; z-index: 2;
                                            }
                                   .paintLevelTrack {
                                            position: absolute; top: 0; right: 0; bottom: 0; left: 0;
                                            background: #e0e0e0; border: 1px solid #bdbdbd;
                                            border-radius: 6px;
                                            }
                                   /* knob 10px narrower than the track and centred on it by
                                      construction (left 50% + translateX), so it stays centred
                                      whatever the track's border does to the padding box; same
                                      slightly rounded rectangle shape as the material buttons.
                                      Its two vertical stops are 4px from the top and 4px from the
                                      bottom of that 102px padding box: 4 and 4 + 45 + 4 = 53. */
                                   .paintLevelKnob {
                                            position: absolute;
                                            left: 50%; transform: translateX(-50%);
                                            width: calc(100% - 10px);
                                            top: 53px; height: 45px;
                                            border-radius: 4px; border: 1px solid #05714e;
                                            background: #069869; color: white;
                                            font-weight: bold; font-size: 12px;
                                            display: flex; align-items: center; justify-content: center;
                                            transition: top .2s ease;
                                            }
                                   .paintLevelCheckbox:checked ~ .paintLevelTrack .paintLevelKnob { top: 4px; }
                                   .knobLabelCanopy { display: none; }
                                   .paintLevelCheckbox:checked ~ .paintLevelTrack .knobLabelCanopy { display: inline; }
                                   .paintLevelCheckbox:checked ~ .paintLevelTrack .knobLabelGround { display: none; }
                                   /* Eraser and Reset. Circles, so they read as tools rather than as
                                      two more materials in the row of rectangular colour buttons.
                                      47px each + the 10px gap = 104px, matching the level switch
                                      beside them, so the group stays vertically aligned. */
                                   .paintToolBtn {
                                            width: 47px; height: 47px; padding: 0;
                                            border-radius: 50%; font-size: 10px; font-weight: bold;
                                            display: flex; align-items: center; justify-content: center;
                                            white-space: normal; line-height: 1.05;
                                            }
                                   /* the eraser is a toggle, so it needs a visibly held-down state */
                                   .paintToolActive {
                                            background-color: #069869 !important; color: white;
                                            border-color: #05714e !important;
                                            }"
                                 )

                                ),


                                 column(12, class = "vft-nv-mapslot",

                                        shinycssloaders::withSpinner(  leaflet::leafletOutput(shiny::NS(id, "versionMap"), height = 600), type = 3, color = "#069869", color.background = "white" )

                                 ),

                                 #PAINT COLOR BUTTONS (heat mitigation, context 4) ####
                                 #canopy row on top, ground row below, both right-aligned against the
                                 #vertical level switch on their right - the switch knob sits on the row
                                 #it activates
                                 shiny::fluidRow(
                                   shinyjs::useShinyjs(),
                                   shinyjs::inlineCSS(list(.colorBtnSelected = "border-width: thick; border-color: black")),
                                   shinyjs::inlineCSS(list(.colorBtnNotSelected = "border-width: thin; border-color: grey")),
                                   shinyjs::inlineCSS(list(.paintBtnDisabled = "opacity: 0.35;")),
                                   shiny::column(12, align = "center",
                                                 shiny::div(
                                                   id = NS(id, "paintColorButtonsDiv"),
                                                   style = "display:none;",
                                                   shiny::div(
                                                     style = "display:flex; align-items:center; justify-content:center; gap:15px; margin-top:10px;",
                                                     shiny::div(
                                                       style = "display:flex; flex-direction:column; align-items:flex-end; gap:10px;",
                                                       #CANOPY LEVEL
                                                       shiny::div(
                                                         style = "display:flex; gap:10px;",
                                                         shiny::actionButton(
                                                           inputId = shiny::NS(id, "paintColor_canopyArtificial"), label = i18n$t("Künstlich"),
                                                           class = "colorBtnNotSelected",
                                                           style = "background-color: #3f3f3f; color: white; width: 90px; height: 45px;"
                                                         ),
                                                         shiny::actionButton(
                                                           inputId = shiny::NS(id, "paintColor_canopyTree"), label = i18n$t("Baum"),
                                                           class = "colorBtnNotSelected",
                                                           style = "background-color: #14532d; color: white; width: 90px; height: 45px;"
                                                         )
                                                       ),
                                                       #GROUND LEVEL
                                                       shiny::div(
                                                         style = "display:flex; gap:10px;",
                                                         shiny::actionButton(
                                                           inputId = shiny::NS(id, "paintColor_grass"), label = i18n$t("Gras"),
                                                           class = "colorBtnSelected",
                                                           style = "background-color: lightgreen; width: 90px; height: 45px;"
                                                         ),
                                                         shiny::actionButton(
                                                           inputId = shiny::NS(id, "paintColor_bush"), label = i18n$t("Busch"),
                                                           class = "colorBtnNotSelected",
                                                           style = "background-color: #6aa84f; color: white; width: 90px; height: 45px;"
                                                         ),
                                                         shiny::actionButton(
                                                           inputId = shiny::NS(id, "paintColor_artificial"), label = i18n$t("Künstlich"),
                                                           class = "colorBtnNotSelected",
                                                           style = "background-color: grey; width: 90px; height: 45px;"
                                                         ),
                                                         shiny::actionButton(
                                                           inputId = shiny::NS(id, "paintColor_natural"), label = i18n$t("Natürlich"),
                                                           class = "colorBtnNotSelected",
                                                           style = "background-color: #a05a3c; color: white; width: 90px; height: 45px;"
                                                         ),
                                                         shiny::actionButton(
                                                           inputId = shiny::NS(id, "paintColor_water"), label = i18n$t("Wasser"),
                                                           class = "colorBtnNotSelected",
                                                           style = "background-color: dodgerblue; color: white; width: 90px; height: 45px;"
                                                         )
                                                       )
                                                     ),
                                                     #BOTH LEVELS AT ONCE - a solid block occupies the ground and everything
                                                     #above it, so it belongs to neither row and is never disabled by the level
                                                     #switch. Its height is the two rows plus the 10px gap between them, so it
                                                     #lines up with them exactly.
                                                     shiny::actionButton(
                                                       inputId = shiny::NS(id, "paintColor_block"), label = i18n$t("Artificial block"),
                                                       class = "colorBtnNotSelected",
                                                       #white-space/flex override Bootstrap's nowrap and top-aligned label, which
                                                       #a two-word caption in a 90px-wide, 100px-tall button would otherwise show up
                                                       style = paste("background-color: #1f1f1f; color: white; width: 90px; height: 100px;",
                                                                     "white-space: normal; display: flex; align-items: center;",
                                                                     "justify-content: center; text-align: center;")
                                                     ),
                                                     #LEVEL SWITCH (up = canopy, down = ground)
                                                     tags$label(
                                                       class = "paintLevelSwitch",
                                                       tags$input(id = shiny::NS(id, "paintLevel"), type = "checkbox",
                                                                  class = "paintLevelCheckbox"),
                                                       tags$span(
                                                         class = "paintLevelTrack",
                                                         tags$span(
                                                           class = "paintLevelKnob",
                                                           tags$span(class = "knobLabelCanopy", i18n$t("Krone")),
                                                           tags$span(class = "knobLabelGround",  i18n$t("Boden"))
                                                         )
                                                       )
                                                     ),
                                                     #ERASER + RESET. Both act on the paint only; the land cover
                                                     #baseline underneath is never edited, so these reveal it rather
                                                     #than erase it. Round, to read as tools rather than as two more
                                                     #materials in the row of rectangular colour buttons.
                                                     shiny::div(
                                                       style = "display:flex; flex-direction:column; gap:10px;",
                                                       shiny::actionButton(
                                                         inputId = shiny::NS(id, "paintEraser"), label = i18n$t("Eraser"),
                                                         class = "paintToolBtn colorBtnNotSelected",
                                                         style = "background-color: #ffffff;"
                                                       ),
                                                       shiny::actionButton(
                                                         inputId = shiny::NS(id, "paintReset"), label = i18n$t("Reset"),
                                                         class = "paintToolBtn colorBtnNotSelected",
                                                         style = "background-color: #ffffff;"
                                                       )
                                                     ),
                                                     #HEAT. Reads the design rather than editing it, so it sits in its
                                                     #own column apart from the brush tools. Two buttons keep this
                                                     #column the same 104px as the eraser column and the level switch.
                                                     #Refresh is separate because the heat model costs seconds over a
                                                     #large area: recomputing on every stroke would stall the shared
                                                     #R process for everyone, so the user decides when to pay it.
                                                     shiny::div(
                                                       style = "display:flex; flex-direction:column; gap:10px;",
                                                       shiny::actionButton(
                                                         inputId = shiny::NS(id, "heatSwitch"), label = i18n$t("Heat"),
                                                         class = "paintToolBtn colorBtnNotSelected",
                                                         style = "background-color: #ffffff;"
                                                       ),
                                                       shiny::actionButton(
                                                         inputId = shiny::NS(id, "heatRefresh"), label = i18n$t("Refresh"),
                                                         class = "paintToolBtn colorBtnNotSelected",
                                                         style = "background-color: #ffffff;"
                                                       )
                                                     )
                                                   )
                                                 )
                                   )
                                 )

                 ),
                #SIDE BAR
                #the scenario sidebar, same height as the map column beside it -
                #the scenario list is what flexes here, so the confirm button at
                #the foot of this column and the bottom of the map end on the
                #same line. See R/layout_helpers.R.
                shiny::column(2,align = "center", class = "vft-nv-col",

                              shiny::fluidRow(shiny::column(12,
                                                            shiny::h4(shiny::HTML(paste0(i18n$t("Erstellen/auswählen Sie"), "<br>", i18n$t("ein Szenario"))))
                              )
                              ),
                       shiny::fluidRow(
                         shinyjs::useShinyjs(),
                         #create new versions button
                         shiny::column(12,
                                shiny::actionButton( style = "background-color: #53bbb4; height: 50px; width: 50px; vertical-align: middle; font-size: 30px",
                                              inputId = shiny::NS(id, "addVersionButton"), label = shiny::strong("+")
                                )
                         )
                       ),
                       shiny::div( style = "height: 10px" ),

                       #the row that absorbs this column's slack: the scenario
                       #list grows and shrinks with the screen so that the
                       #confirm button under it stays put. The box already
                       #scrolls, so what a short screen costs is rows of the
                       #list, not the button.
                       shiny::fluidRow(class = "vft-nv-listrow", style= "padding-left: 25px",

                         #list of version boxes

                         shiny::column(12, align = "center", class = "vft-fit-vlist-nv",
                                       style='border: 1px solid black; vertical-align:middle; width: 200px; overflow-y: scroll;',

                                shinyjs::useShinyjs(),
                                shinyjs::inlineCSS(list(.selected = "border-width: thick; border-color: green")),
                                shinyjs::inlineCSS(".selected:focus {border-width: thick; border-color: green; background color: white} "),
                                shinyjs::inlineCSS(list(.notSelected = "border-width: thin; border-color: grey")),
                                shinyjs::inlineCSS(list(.original = "border-width: thick; border-color: grey")),



                                shiny::div(id = "topPlaceHolder_newVersion",
                                    shiny::div(id = "placeholder")
                                )

                         )
                       ),


                       shiny::div( style = "height: 25px" ),

                       shiny::fluidRow(
                         shinyjs::useShinyjs(),
                         #confirmation button
                         shiny::column(12,
                                shiny::actionButton(class = "btn-success", style = "height: 70px; width: 180px ;vertical-align: middle",
                                             inputId = shiny::NS(id, "newVersionsConfirmButton"), label = i18n$t("Szenarien bestätigen")
                                )
                         )
                       )
                       ),
                ),
        tagList(
          # ... your normal UI ...,
          tags$script(src = "www/paintbrush.js")  # at the end of the UI, outside tags$head
        )

              )


}
