#### Step 1 UI - determine area ####
step6_ui <- function(id, i18n){
print("UI6")
      shiny::fluidPage(
        #activate translation for this ui
        shiny.i18n::usei18n(i18n),
        shiny::fluidRow( style = "display: flex; align-items:center;background-color:#006268; height: 100px; color: #ffffff; ",
                         shiny::column(4, align = "left",  style = "font-family: 'franklin gothic'",
                                       shiny::HTML("<title>Visitor Flow Tool</title>"),
                                       shiny::div(style = "margin-top: 2px"),

                                       shiny::selectInput(inputId = shiny::NS(id, "languageSelect_6"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                                                          selected = i18n$get_translation_language(), width = 100 ),
                                       shiny::div(style = "margin-top:-25px"),
                                       shiny::h2(i18n$t("Besucherlenkungs-Tool: "))                         ),
                         shiny::column(4,align = "center",
                                       # shiny::h1("Schritt 1")

                                       shiny::uiOutput(NS(id,"bannerUI_6"))
                                         # imageMap(NS(id, "banner"), 'www/step5_wsl.png' , list(A = "0,0,0,100,70,100,70,0", B = "70,0,70,100,160,100,160,0", C = "160,0,160,100,260,100,260,0", D = "260,0,260,100,360,100,360,0") )


                         ),
                         shiny::column(4, align = "right",
                                       shiny::column(10, align = "right",
                                                     div(
                                                       shiny::HTML("
                                          <img src ='www/BiodivCenterLogo_w.png' style = 'align: right; width: 200px; height:75%;object-fit:contain;'>
                                          ")
                                                     )),
                                       shiny::column(2, align = "right", style = "margin-top: 10px",
                                                     shiny::actionButton(inputId = shiny::NS(id, "helpButton5"), label = "", style = "width: 30px; height: 30px;
background: url('helpIcon.png');  background-size: cover; background-position: center; border:none"),
                                                     shiny::div(style = "margin-top:5px"),
                                                     shiny::actionButton(inputId = shiny::NS(id, "infoButton5"), label = "", style = "width: 30px; height: 30px;
background: url('infoIcon.png');  background-size: cover; background-position: center; border: none")
                                       )
                         )
        ),
        shiny::fluidRow(column(12, align = "left", style = "display:inline-block;height:1px;color:#006268; font-family: 'franklin gothic';margin-top:-10px;margin-left:-13px ",
                               shiny::h5(i18n$t("app designer/contact: johan.frueh@wsl.ch"), href = "mailto:'johan.frueh@wsl.ch'")
        )),

        shiny::fluidRow(
          shiny::column(12, align = "center",
                 shiny::h3(strong(i18n$t("Simulation der Naherholung starten:")))
          )
        ),
        shiny::fluidRow(
          shiny::column(12, align = "center",
                 shiny::h5(i18n$t("Rechts: Erstellen Sie neue Szenarien von Infrastrukturkarten (Wege, Häuser etc.) und wählen Sie diese aus.")),
                 shiny::h5(i18n$t("Nachdem eine Simulation gestartet wurde, können Sie die anzuzeigenden Informationen auswählen (Optionen auf der linken Seite).")),

                 )
        ),

              shiny::fluidRow(
                shiny::tags$style("
      .radio { /* checkbox is a div class*/
        line-height: 20px;
        margin-bottom: 30px; /*set the margin, so boxes don't overlap*/
      }
      input[type='radio']{ /* style for checkboxes */
        width: 20px; /*Desired width*/
        height: 20px; /*Desired height*/
        line-height: 20px;
      }
      span {
          margin-left: 10px;  /*set the margin, so boxes don't overlap labels*/
          line-height: 20px;
      }
  "),

                shiny::column(3, style = "align: left; font-size: 15px;",

                              shinyjs::disabled(
                                shiny::tagList(
                                  shiny::uiOutput(shiny::NS(id, "agentCheckbox_ui"))

                                  ,
                                  shiny::h5( shiny::strong(i18n$t("Andere Informationen anzeigen"))
                                  ),
                                  shiny::checkboxInput(shiny::NS(id, "onlyAOIcheckbox"), i18n$t("Innerhalb Zielgebiete")
                                  ),
                                  shiny::checkboxInput(shiny::NS(id, "SMcheckbox"), i18n$t("Sensitivitäts-Matrix")
                                  ),
                                  shiny::checkboxInput(shiny::NS(id, "startingCheckbox"), i18n$t("Agenten Ausgangspunkte")
                                  ),
                                  shiny::checkboxInput(shiny::NS(id, "aoi"), i18n$t("Zielgebiete")
                                  ),
                                  shiny::checkboxInput(shiny::NS(id, "ParkingCheckbox"), i18n$t("Parking")
                                  ),
                                  shiny::checkboxInput(shiny::NS(id, "ResidentialCheckbox"), i18n$t("Neue Wohngebiete")
                                  ),
                                  shinyjs::useShinyjs()
                                  # shinyjs::disabled(shiny::checkboxInput(shiny::NS(id, "frictionCheckbox"), shiny::HTML(as.character(i18n$t(":Überlappung:")))))
                                )
                              ),
                              shiny::tags$script(
                          "
                          $('#step6-dayCheckbox .radio label span').map(function(choice){
                              this.innerHTML = $(this).text();

                          });
                          "
                       )

                       ),
                shiny::column(7, align = "center",
                              tags$head(
                                tags$style(
                                  ".leaflet .legend { text-align: left;font-size: 15px;}"
                                )),
                       # leaflet::leafletOutput(shiny::NS(id, "pathUsageMap"), height = 500px)
                       leaflet::leafletOutput(NS(id, "mapAreaLeaflet"), height = 0),
                       shiny::uiOutput(NS(id, "mapArea"), height = 500),
                       shiny::uiOutput(NS(id, "mapScript"), height = 0),


                       # download buttons ####
                       shiny::fluidRow(
                         #                 shiny::div(style = "height: 5px"),
                         shiny::column(12, align = "center",
                                       #button under versions box


                                         shiny::actionButton(shiny::NS(id, "SMbutton"), label = shiny::HTML(as.character(i18n$t("Download: Sensitivitäts-Matrix [.tif]"))), class = "btn-warning",
                                                             style = " align: center; vertical-align: middle")
                                       ,
                                       #button under versions box
                                       shinyjs::disabled({

                                         shiny::actionButton(shiny::NS(id, "pathsDwnldButton"), label = shiny::HTML(as.character(i18n$t("Download: Wege und Zielgebiete [.gpkg]"))), class = "btn-warning",
                                                             style = " align: center; vertical-align: middle")
                                       }),

                                       #button under versions box
                                       shinyjs::disabled({

                                         shiny::actionButton(shiny::NS(id, "imageButton"), label = shiny::HTML(as.character(i18n$t("Ein Bild erstellen"))), class = "btn-warning",
                                                             style = " align: center; vertical-align: middle")
                                       })
                         )
                       ),

                       ),
                # shiny::column(1,
                #
                #   shiny::fluidRow(
                #
                #     shiny::column(12, align = "center",
                #            shiny::h5(shiny::strong("Fokus auf Zielgebiete"))
                #            )
                #
                #     ),
                #
                #   shiny::fluidRow(shiny::h5()),
                #
                #   shiny::fluidRow(
                #
                #     # shiny::column(12, align = "center",
                #     #        shinyWidgets::prettySwitch(shiny::NS(id, "usageSwitch"), value = FALSE, label = NULL, width = "150px",
                #     #                     bigger = TRUE, fill = TRUE, status = "success", inline = TRUE)
                #     #        )
                #
                #   ),
                #   shiny::fluidRow(
                #
                #     # shiny::column(12, align = "center",
                #     #        shiny::h5(shiny::strong("Sensitivit\u00E4ts- Matrix anzeigen"))
                #     # )
                #
                #   ),
                #
                #   shiny::fluidRow(shiny::h5()),
                #
                #   shiny::fluidRow(
                #
                #     # shiny::column(12, align = "center",
                #     #        shinyWidgets::prettySwitch(shiny::NS(id, "SMswitch"), value = FALSE, label = NULL, width = "150px",
                #     #                     bigger = TRUE, fill = TRUE, status = "success", inline = TRUE)
                #     # )
                #
                #   )
                #
                #
                #
                #   ),
                shiny::column(2, style = "align:center",
                       shiny::fluidRow(
                         #create new versions button
                         shiny::column(12, style = "align: center",
                           shiny::actionButton( style = "background-color: #53bbb4; height: 30px; align: center; vertical-align: middle",
                             inputId = shiny::NS(id, "newVersionsButton"), label = shiny::strong(i18n$t("Neue Szenarien erstellen"))
                             )
                           )
                         ),

                  shiny::div( style = "height: 25px" ),

                  shiny::fluidRow(

                    #list of version boxes

                    shiny::column(12, style='border: 1px solid black; vertical-align:middle; align: center; height:400px; width: 200px; overflow-y: scroll;',

                           shinyjs::useShinyjs(),
                           shinyjs::inlineCSS(list(.selected = "border-width: thick; border-color: green")),
                           shinyjs::inlineCSS(".selected:focus {border-width: thick; border-color: green; background color: white} "),
                           shinyjs::inlineCSS(list(.notSelected = "border-width: thin; border-color: grey")),
                           shinyjs::inlineCSS(list(.original = "border-width: thick; border-color: grey")),
                           shinyjs::inlineCSS(list(.noSim = "background: url('www/noSim.png'); background-size: cover; background-position: center")),
                           shinyjs::inlineCSS(list(.withSim = "background: url('www/Sim.png'); background-size: cover; background-position: center")),



                           shiny::div(id = "topPlaceHolder",
                                      shiny::div(id = "placeholder_step6")
                            )

                           )
                    ),


                  shiny::div( style = "height: 5px" ),

                  # shiny::fluidRow(
                  #
                  # #confirmation button
                  # shiny::column(12, style = "align: center",
                  #         shiny::actionButton(class = "btn-success", style = "height: 70px; width: 190px ;align: center; vertical-align: middle; padding : 1px",
                  #           inputId = shiny::NS(id, "confirmButtonFinal"), label = shiny::HTML("Alle Simulationen abschlie&szlig;en <br/> und Ausgabe generieren")
                  #           )
                  #          )
                  #   ),

                  shiny::fluidRow(
                    shiny::column(width = 12, align = "center",
                      shiny::tagList(
                      #launch new simulation
                      shiny::actionButton(shiny::NS(id, "launchSim"), label = shiny::HTML(paste0(i18n$t(":Simulation:"))), class = "btn-success btn-lg", style = " align: center; vertical-align: middle; height: 75px; width: 200px; text-wrap:wrap; padding-top:5px", width = "100px"),
                      shiny::tags$script(
                        "
                          $('#step6-dayCheckbox .radio label span').map(function(choice){
                              this.innerHTML = $(this).text();

                          });
                          "
                      )
                      )


                    )

                  )
                ),



                ),
#

              shiny::fluidRow(
                shiny::column(12, align = "center", style = "display:table-cell; vertical-align: middle; ",
                       shinyjs::useShinyjs(),


                       shinyjs::hidden( shiny::downloadButton(NS(id, "downloadTIFF")) ),
                       shinyjs::hidden( shiny::downloadButton(NS(id, "downloadPaths")) ),
                       shinyjs::hidden( shiny::downloadButton(NS(id, "downloadSM")) )



                )
              )
              )

}
