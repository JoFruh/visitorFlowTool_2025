#### Step 1 UI - determine area ####
newVersions_ui <- function(id, i18n){
print("UI6")
      shiny::fluidPage(
        #activate translation for this ui
        shiny.i18n::usei18n(i18n),
              #TITLE


               shiny::fluidRow(style = "display: flex; align-items:center;background-color:#006268; height: 100px; color: #ffffff; ",
                 # shiny::column(4, align = "left",
                 #               shiny::titlePanel(shiny::h1("Besucherlenkungs-Tool: ", align = "left"))
                 # ),
                 # shiny::column(4, align = "center",
                 #               shiny::h2("Schritt 4+ (Neuen Versionen)")
                 # )

                 shiny::column(4, align = "left",  style = "font-family: 'franklin gothic'",
                               shiny::HTML("<title>Visitor Flow Tool</title>"),
                               shiny::div(style = "margin-top: 2px"),

                               shiny::selectInput(inputId = shiny::NS(id, "languageSelect_7"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                                                  selected = "de", width = 100 ),
                               shiny::div(style = "margin-top:-25px"),
                               shiny::h2(i18n$t("Besucherlenkungs-Tool: ") )
                 ),
                 shiny::column(4,align = "center",
                               # shiny::h1("Schritt 1")
                               shiny::uiOutput(NS(id,"bannerUI_7"))
                               # imageMap(NS(id, "banner"), 'www/stepNewVersions_wsl.png' , list() )


                 ),
                 shiny::column(4, align = "right",
                               shiny::column(10, align = "right",
                                             div(
                                               shiny::HTML("
                                          <img src ='www/BiodivCenterLogo_w.png' style = 'align: right; width: 200px; height:75%;object-fit:contain;'>
                                          ")
                                             )),
                               shiny::column(2, align = "right", style = "margin-top: 10px",
                                             shiny::actionButton(inputId = shiny::NS(id, "helpButton6"), label = "", style = "width: 30px; height: 30px;
background: url('helpIcon.png');  background-size: cover; background-position: center; border:none"),
                                             shiny::div(style = "margin-top:5px"),
                                             shiny::actionButton(inputId = shiny::NS(id, "infoButton6"), label = "", style = "width: 30px; height: 30px;
background: url('infoIcon.png');  background-size: cover; background-position: center; border: none")
                               )
                 )

               )
        ,
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
               shiny::fluidRow(
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
                 shiny::column(9, align = "center",
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
                                   .paintLevelCheckbox {
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


                                 column(12,

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
                                                     )
                                                   )
                                                 )
                                   )
                                 )

                 ),
                #SIDE BAR
                shiny::column(2,align = "center",

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

                       shiny::fluidRow(style= "padding-left: 25px",

                         #list of version boxes

                         shiny::column(12, align = "center", style='border: 1px solid black; vertical-align:middle; height:400px; width: 200px; overflow-y: scroll;',

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
