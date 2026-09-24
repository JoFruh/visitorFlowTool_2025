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
                                   /* The switch is greyed out with the material buttons whenever the brush is
                                      shut - on the original scenario, and while the heat read-out is on. The
                                      disabling itself is shinyjs::toggleState on the checkbox, but the checkbox
                                      is a transparent overlay, so dimming it would show nothing: what has to
                                      dim is the track and knob it sits on top of. */
                                   .paintLevelCheckbox:disabled ~ .paintLevelTrack { opacity: 0.35; }
                                   .paintLevelCheckbox:disabled { cursor: default; }
                                   /* HEIGHT BAR (context 4). One swatch per step of the armed
                                      material's height ramp, tallest on top, and the swatch colour IS
                                      the height - see PAINT_CATEGORIES in paintbrush_helpers.R. Shown
                                      only while a material that carries a height is armed: a tree, an
                                      artificial canopy or a block. Block included in Ground mode too,
                                      because it is level 'both' and the level switch never disables it.
                                      104px like everything else in this strip (the two 45px button rows
                                      plus their 10px gap). That number is not cosmetic: this row is the
                                      lowest thing in the map column, so a few px of overflow here are a
                                      few px of page and a scrollbar on the pane - the same trap the
                                      level switch's `input.` selector above exists to avoid. The
                                      swatches flex to fill it, so a 5-step ramp and a 3-step ramp are
                                      the same height with different sized swatches. */
                                   .paintHeightBar {
                                            display: flex; flex-direction: column;
                                            height: 104px; width: 56px; gap: 0;
                                            }
                                   .paintHeightGroup {
                                            display: flex; flex-direction: column;
                                            height: 100%; gap: 4px;
                                            }
                                   /* flex:1 with min-height:0 rather than a fixed px height: the three
                                      ramps have 5, 3 and 5 steps and all three have to come to 104px. */
                                   .paintHeightBtn {
                                            flex: 1 1 0; min-height: 0; width: 100%; padding: 0;
                                            font-size: 10px; font-weight: bold; line-height: 1;
                                            display: flex; align-items: center; justify-content: center;
                                            white-space: nowrap;
                                            }
                                   /* Eraser and Reset. Circles, so they read as tools rather than as
                                      two more materials in the row of rectangular colour buttons.
                                      47px each + the 10px gap = 104px, matching the level switch
                                      beside them, so the group stays vertically aligned. */
                                   .paintToolBtn {
                                            width: 47px; height: 47px; padding: 0;
                                            border-radius: 50%; font-size: 10px; font-weight: bold;
                                            display: flex; align-items: center; justify-content: center;
                                            white-space: normal; line-height: 1.05;
                                            /* these labels are translated, and the French ones are single
                                               words longer than a 47px circle (Reinitialiser). white-space
                                               only breaks at spaces, so without this such a word spills out
                                               of the button instead of wrapping inside it. */
                                            overflow-wrap: anywhere;
                                            }
                                   /* the eraser is a toggle, so it needs a visibly held-down state */
                                   .paintToolActive {
                                            background-color: #069869 !important; color: white;
                                            border-color: #05714e !important;
                                            }
                                   /* WORK IN PROGRESS. The heat model runs in a daemon for seconds, and
                                      until it answers the map shows either the previous surface or nothing
                                      at all, so the button that started it has to say that something is
                                      happening. A ring turning just outside the circle: it is drawn in a
                                      pseudo-element, so it adds no box of its own and the tool row cannot
                                      shift while it spins, and the label underneath stays readable.
                                      pointer-events:none because the button is disabled for the duration
                                      (heatWorking() in newVersions_server.R) and the ring must not become
                                      the one part of it that still takes a click.
                                      The border trick rather than an animated image: one colour stop on an
                                      otherwise transparent border is a circle with a gap in it, and
                                      rotating that is the whole animation. Nothing to load, and it follows
                                      the app green.
                                      Single quotes on the empty content: this whole block is one
                                      double-quoted R string. */
                                   .paintToolBusy { position: relative; }
                                   .paintToolBusy::after {
                                            content: ''; position: absolute;
                                            top: -4px; left: -4px; right: -4px; bottom: -4px;
                                            border-radius: 50%;
                                            border: 3px solid transparent;
                                            border-top-color: #069869;
                                            animation: paintToolSpin 0.8s linear infinite;
                                            pointer-events: none;
                                            }
                                   /* Bootstrap dims a disabled button, and the ring is the one thing on it
                                      that has to stay at full strength - dimmed, working reads as off. So
                                      the dimming moves from the button to its label, which is an element
                                      and not a bare text node because usei18n() wraps it in a span. */
                                   .paintToolBusy[disabled] { opacity: 1; }
                                   .paintToolBusy[disabled] > * { opacity: 0.65; }
                                   @keyframes paintToolSpin { to { transform: rotate(360deg); } }"
                                 )

                                ),


                                 column(12, class = "vft-nv-mapslot",

                                        shinycssloaders::withSpinner(  leaflet::leafletOutput(shiny::NS(id, "versionMap"), height = 600), type = 3, color = "#069869", color.background = "white" ),

                                        #CONFLICTS. Shows again the biodiversity-recreation
                                        #conflicts step 5 found for the selected scenario. On the
                                        #map, bottom-left, the one corner leaflet leaves free here
                                        #(zoom top-left, legends top-right, attribution
                                        #bottom-right). Outside the leafletOutput, not a leaflet
                                        #control, so it survives the map's re-renders and shinyjs
                                        #can grey it out. Disabled until the server has checked the
                                        #selected scenario - see the conflict observers in
                                        #newVersions_server.R. The column is Bootstrap's
                                        #position:relative, which is what `absolute` is against.
                                        shiny::tags$style(shiny::HTML(paste(
                                          ".vftConflictBtnWrap { position: absolute; left: 25px; bottom: 25px; z-index: 1000; }",
                                          "#newVersions-showConflicts { background-color: #ffffff; border: 2px solid #c62828; color: #c62828; font-weight: bold; box-shadow: 0 1px 4px rgba(0,0,0,0.3); }",
                                          "#newVersions-showConflicts.vftConflictOn { background-color: #c62828; color: #ffffff; }",
                                          "#newVersions-showConflictsOrig { margin-left: 8px; background-color: #ffffff; border: 2px dashed #6a1b9a; color: #6a1b9a; font-weight: bold; box-shadow: 0 1px 4px rgba(0,0,0,0.3); }",
                                          "#newVersions-showConflictsOrig.vftConflictOn { background-color: #6a1b9a; color: #ffffff; }"))),
                                        #The second button shows the Original's conflicts
                                        #whichever card is selected, and searches for them if
                                        #step 5 never did - see "SHOW THE ORIGINAL'S CONFLICTS".
                                        #Dashed purple on the map too, to read apart from the
                                        #selected scenario's red.
                                        shiny::div(class = "vftConflictBtnWrap",
                                                   shinyjs::disabled(
                                                     shiny::actionButton(shiny::NS(id, "showConflicts"),
                                                                         label = i18n$t("Konflikte anzeigen"))
                                                   ),
                                                   shinyjs::disabled(
                                                     shiny::actionButton(shiny::NS(id, "showConflictsOrig"),
                                                                         label = i18n$t("Konflikte im Original anzeigen"))
                                                   ))

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
                                                     #HEIGHT BAR. Leftmost, because the level switch has to stay
                                                     #immediately right of the two material rows - its knob slides
                                                     #onto the row it activates, so nothing may come between them.
                                                     #
                                                     #Every step of every ramp is built HERE, statically, and shown
                                                     #or hidden with shinyjs - not rendered per material with
                                                     #renderUI. That is how the rest of this palette works, and it
                                                     #keeps the swatches' input ids fixed: a re-rendered
                                                     #actionButton arrives with its click count reset, and on this
                                                     #page a control that silently loses its value is the failure
                                                     #mode that has bitten hardest (see the scenario-card notes in
                                                     #newVersions_server.R).
                                                     #
                                                     #Built by looping PAINT_CATEGORIES instead of writing one
                                                     #actionButton per step out, so colour and metres both come
                                                     #from the one table. The eight material buttons below restate
                                                     #their hexes by hand and are the cautionary example.
                                                     shiny::div(
                                                       id = NS(id, "paintHeightBar"),
                                                       class = "paintHeightBar",
                                                       style = "display:none;",
                                                       lapply(vftHeightRamps(), function(ramp){
                                                         shiny::div(
                                                           id = NS(id, paste0("paintHeightGroup_", ramp$name)),
                                                           class = "paintHeightGroup",
                                                           style = "display:none;",
                                                           #tallest on top, so the bar reads like the thing it
                                                           #describes rather than like a table sorted by id
                                                           lapply(rev(seq_len(nrow(ramp$steps))), function(k){
                                                             st <- ramp$steps[k, ]
                                                             shiny::actionButton(
                                                               inputId = shiny::NS(id, paste0("paintHeight_", st$id)),
                                                               label   = sprintf("%g m", st$height),
                                                               class   = "paintHeightBtn colorBtnNotSelected",
                                                               style   = sprintf("background-color: %s; color: %s;",
                                                                                 st$hex, st$fg)
                                                             )
                                                           })
                                                         )
                                                       })
                                                     ),
                                                     shiny::div(
                                                       style = "display:flex; flex-direction:column; align-items:flex-end; gap:10px;",
                                                       #CANOPY LEVEL
                                                       shiny::div(
                                                         style = "display:flex; gap:10px;",
                                                         shiny::actionButton(
                                                           inputId = shiny::NS(id, "paintColor_canopyArtificial"), label = i18n$t("Kuenstlich"),
                                                           class = "colorBtnNotSelected",
                                                           style = "background-color: #e0e0e0; color: black; width: 90px; height: 45px;"
                                                         ),
                                                         shiny::actionButton(
                                                           inputId = shiny::NS(id, "paintColor_canopyTree"), label = i18n$t("Baum"),
                                                           class = "colorBtnNotSelected",
                                                           style = "background-color: #004200; color: white; width: 90px; height: 45px;"
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
                                                           inputId = shiny::NS(id, "paintColor_artificial"), label = i18n$t("Kuenstlich"),
                                                           class = "colorBtnNotSelected",
                                                           style = "background-color: grey; width: 90px; height: 45px;"
                                                         ),
                                                         shiny::actionButton(
                                                           inputId = shiny::NS(id, "paintColor_natural"), label = i18n$t("Natuerlich"),
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
                                                       inputId = shiny::NS(id, "paintColor_block"), label = i18n$t("Kuenstlicher Block"),
                                                       class = "colorBtnNotSelected",
                                                       #white-space/flex override Bootstrap's nowrap and top-aligned label, which
                                                       #a two-word caption in a 90px-wide, 100px-tall button would otherwise show up
                                                       style = paste("background-color: #3d3d3d; color: white; width: 90px; height: 100px;",
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
                                                         inputId = shiny::NS(id, "paintEraser"), label = i18n$t("Radierer"),
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
                                                     #own column apart from the brush tools, and centres against them
                                                     #rather than filling their 104px - there is only one of it.
                                                     #Switching it on recomputes when the paint has changed since the
                                                     #last read-out and reuses the cached surface when it has not, so
                                                     #there is nothing left for a separate Refresh button to do. The
                                                     #heat model costs seconds over a large area and the R process is
                                                     #shared, which is why that recompute is tied to this deliberate
                                                     #click and not to every stroke - and why the brush is refused
                                                     #while heat is on (see the heat observers in newVersions_server.R),
                                                     #so what is on screen always describes the design underneath it.
                                                     shiny::div(
                                                       style = "display:flex; flex-direction:column; gap:10px;",
                                                       shiny::actionButton(
                                                         inputId = shiny::NS(id, "heatSwitch"), label = i18n$t("Hitze"),
                                                         class = "paintToolBtn colorBtnNotSelected",
                                                         style = "background-color: #ffffff;"
                                                       ),
                                                       #TIME OF DAY. The heat model is computed for one of three
                                                       #bins, and the ranking of materials genuinely changes between
                                                       #them: asphalt peaks 1-2 h after solar noon while grass and
                                                       #water barely move, so afternoon is not just "midday but
                                                       #more". Morning and afternoon share a sun elevation (45.8 deg)
                                                       #and differ only in azimuth - the heat difference between them
                                                       #is thermal inertia, not sun angle.
                                                       #
                                                       #Changing this drops the cached surface exactly as painting
                                                       #does, because a heat raster belongs to one time of day as
                                                       #much as it belongs to one design.
                                                       #
                                                       #The wrapper kills selectInput's bottom margin, which would
                                                       #otherwise push this column out of line with the brush tools
                                                       #beside it.
                                                       #vftTrText(), not i18n$t(): usei18n() above puts the
                                                       #Translator in client-side mode, where t() returns a
                                                       #<span> tag rather than a string. A select's option
                                                       #labels are plain text and cannot hold one - and three
                                                       #tags in a row become nine list elements, which is
                                                       #exactly the "'names' attribute [9] must be the same
                                                       #length as the vector [3]" this used to die with.
                                                       #
                                                       #Unwrapping means the client cannot swap these labels
                                                       #on a language change either, so the server updates them
                                                       #instead - see the heatBin block in the language
                                                       #observer in newVersions_server.R.
                                                       shiny::div(
                                                         style = "margin-bottom:-15px; width:104px;",
                                                         shiny::selectInput(
                                                           inputId = shiny::NS(id, "heatBin"), label = NULL,
                                                           choices = heatBinChoices(i18n),
                                                           selected = HEAT_BIN_DEFAULT, width = "104px"
                                                         )
                                                       ),
                                                       #PLAN IMPORT. Opens the browser's file picker; the file
                                                       #never reaches R - planimport.js places, classifies and
                                                       #previews it, and only the result is sent on Apply. A plain
                                                       #button rather than an actionButton: R has nothing to do
                                                       #on the click. Gated with the brush in applyPaintGates().
                                                       tags$button(
                                                         id = shiny::NS(id, "paintImport"), type = "button",
                                                         class = "btn btn-default paintToolBtn colorBtnNotSelected",
                                                         style = "background-color: #ffffff;",
                                                         onclick = "document.getElementById('newVersions-planFile').click();",
                                                         i18n$t("Plan laden")
                                                       ),
                                                       tags$input(id = shiny::NS(id, "planFile"), type = "file",
                                                                  accept = ".pdf,.png,.jpg,.jpeg,.tif,.tiff",
                                                                  style = "display:none;")
                                                     )
                                                   )
                                                 ),
                                                 #filled by planimport.js while a plan is being placed;
                                                 #the paint buttons above are hidden meanwhile
                                                 shiny::div(id = NS(id, "planImportPanel"), style = "display:none;")
                                   )
                                 )

                 ),
                #SIDE BAR
                #the scenario sidebar, same height as the map column beside it -
                #the scenario list is what flexes here, so the confirm button at
                #the foot of this column and the bottom of the map end on the
                #same line. See R/layout_helpers.R.
                shiny::column(2, class = "vft-nv-col vft-scencol",

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
                       shiny::fluidRow(class = "vft-nv-listrow",

                         #list of version boxes

                         shiny::column(12, class = "vft-fit-vlist-nv vft-vlist",
                                       style='border: 1px solid black; vertical-align:middle; width: 200px; overflow-y: scroll;',

                                shinyjs::useShinyjs(),
                                shinyjs::inlineCSS(list(.selected = "border-width: thick; border-color: green")),
                                shinyjs::inlineCSS(".selected:focus {border-width: thick; border-color: green; background color: white} "),
                                shinyjs::inlineCSS(list(.notSelected = "border-width: thin; border-color: grey")),
                                shinyjs::inlineCSS(list(.original = "border-width: thick; border-color: grey")),
                                #The scenario cards' names: Bootstrap's .btn never wraps, so a
                                #long name ran out of the 100px square. Wrap it (breaking a
                                #long word if it must), centre it both ways, and cut it off
                                #with an ellipsis after four lines rather than overflow the
                                #card. Scoped to the select buttons - the "X" beside each card
                                #is a button too.
                                shinyjs::inlineCSS(paste(
                                  "#topPlaceHolder_newVersion button[id*='versionBtn'] {",
                                  "  display: inline-flex; align-items: center; justify-content: center;",
                                  "  white-space: normal; overflow: hidden; padding: 4px; line-height: 1.2;",
                                  "}",
                                  "#topPlaceHolder_newVersion button[id*='versionBtn'] .action-label {",
                                  "  display: -webkit-box; -webkit-box-orient: vertical; -webkit-line-clamp: 4;",
                                  "  overflow: hidden; max-width: 100%; overflow-wrap: anywhere; text-align: center;",
                                  "}")),
                                #THE HEAT ICONS at a card's foot, one per stored heat map
                                #that still applies (heatIconsTag() in R/heat_helpers.R).
                                #Shown on the Hitzeminderung context only - the server
                                #puts .vftHeatCtx on the column there. .vftHeatStale is
                                #paintbrush.js hiding them the moment a stroke starts,
                                #ahead of the flush that makes it true on the server.
                                #The card of the map on screen goes red, over the green
                                #of the selection, since that may be another card.
                                shinyjs::inlineCSS(paste(
                                  ".vftCard { position: relative; display: inline-block; vertical-align: middle; }",
                                  ".vftHeatIcons { position: absolute; left: 4px; bottom: 4px; display: none; gap: 2px; z-index: 2; }",
                                  "#topPlaceHolder_newVersion.vftHeatCtx .vftHeatIcons { display: flex; }",
                                  "#topPlaceHolder_newVersion .vftHeatIcons.vftHeatStale { display: none; }",
                                  ".vftHeatIcon { width: 24px; height: 24px; box-sizing: border-box; padding: 0;",
                                  "  border-radius: 50%; border: 1px solid #888; background: #ffffff; opacity: 0.75;",
                                  "  display: flex; align-items: center; justify-content: center; cursor: pointer; }",
                                  ".vftHeatIcon:hover { opacity: 1; }",
                                  ".vftHeatIcon.vftHeatShown { opacity: 1; border: 3px solid red; }",
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
          tags$script(src = "www/paintbrush.js"),  # at the end of the UI, outside tags$head
          #after paintbrush.js, whose internal hooks (window.__vftPaintHooks) it uses
          tags$script(src = "www/planimport.js")
        )

              )


}
