#### Step 1 UI - determine area ####
step3_ui <- function(id, i18n){
vftDbg("UI4")
      shiny::fluidPage(
        #activate translation for this ui
        shiny.i18n::usei18n(i18n),
        shiny::fluidRow( style = "display: flex; align-items:center;background-color:#006268; height: 100px; color: #ffffff; ",
                         shiny::column(4, align = "left",  style = "font-family: 'franklin gothic'",
                                       shiny::HTML("<title>Visitor Flow Tool</title>"),
                                       shiny::div(style = "margin-top: 2px"),

                                       shiny::selectInput(inputId = shiny::NS(id, "languageSelect_3"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                                                          selected = i18n$get_translation_language(), width = 100 ),
                                       shiny::div(style = "margin-top:-25px"),
                                       shiny::h2(i18n$t("Besucherlenkungs-Tool: ")  )                       ),

                         shiny::column(4,align = "center",
                                       # shiny::h1("Schritt 1")
                                       shiny::uiOutput(NS(id,"bannerUI_3"))
                                         # imageMap(NS(id, "banner"), 'www/step3_wsl.png' , list(A = "0,0,0,100,70,100,70,0", B = "70,0,70,100,160,100,160,0") )



                         ),
                         shiny::column(4, align = "right",
                                       shiny::column(10, align = "right",
                                                     div(
                                                       shiny::HTML("
                                          <img src ='www/BiodivCenterLogo_w.png' style = 'align: right; width: 200px; height:75%;object-fit:contain;'>
                                          ")
                                                     )),
                                       shiny::column(2, align = "right", style = "margin-top: 10px",
                                                     shiny::actionButton(inputId = shiny::NS(id, "helpButton3"), label = "", style = "width: 30px; height: 30px;
background: url('helpIcon.png');  background-size: cover; background-position: center; border:none"),
                                                     shiny::div(style = "margin-top:5px"),
                                                     shiny::actionButton(inputId = shiny::NS(id, "infoButton3"), label = "", style = "width: 30px; height: 30px;
background: url('infoIcon.png');  background-size: cover; background-position: center; border: none")
                                       )
                         )
        ),
        shiny::fluidRow(column(12, align = "left", style = "display:inline-block;height:1px;color:#006268; font-family: 'franklin gothic';margin-top:-10px;margin-left:-13px ",
                               shiny::h5(i18n$t("app designer/contact: johan.frueh@wsl.ch"), href = "mailto:'johan.frueh@wsl.ch'")
        )),

        shiny::fluidRow(
          shiny::column(12, align = "center",
                 shiny::h3(strong(i18n$t("Bestimmen der Zielgebiete"))),
                 shiny::h4(i18n$t("Ein Zielgebiet ist ein Areal, das für Naherholungssuchende von Interesse sein kann.")),
                 shiny::h4(i18n$t("Für die Erholungssimulation müssen wir alle möglichen Zielgebiete definieren, aus denen die simulierten Besucher wählen können."))
          )
        ),
        shiny::fluidRow(
          shiny::column(12, align = "center",
                 shiny::h5(i18n$t("Bewegen Sie den Schieberegler, um den Umfang der Zielgebiete zu bestimmen. (Dies basiert auf einem 'Attraktivitätsmodell')")),
                 shiny::h5(""),
                 shiny::h5(i18n$t("Im nächsten Schritt haben Sie die Möglichkeit, Zielgebiete manuell zu korrigieren (hinzufügen/löschen/ausschneiden).")),
                 shiny::h5(style = "color:#8f0404;font-weight:bold", i18n$t("Tipp: Wählen Sie einen Schwellenwert, der die größten Bereiche erzeugt und diese gleichzeitig voneinander getrennt hält."))
          )
        ),

              shiny::fluidRow(
                shiny::column(4),
                shiny::column(4, align = "center",
                       shinyWidgets::chooseSliderSkin(skin = "Shiny", color = "#B06161"),
                       shinyWidgets::sliderTextInput(
                         inputId =shiny::NS(id, "AOISlider"),
                         label = i18n$t("Zielgebiete Schwelle"),
                         choices = as.character(round(seq(from = 20, to = 0, by = -0.1), 1)),
                         selected = 11)
                       # sliderInput(NS(id, "AOISlider"), label = "Determine AoI extent", min = 11, max = 0, value = 3, width = "100%")
                ),
                shiny::column(4,
          #                     shinyjs::disabled(
          #                       shiny::tagList(
          #                         shiny::checkboxInput(inputId = shiny::NS(id, "naturalAreasCheck"), label = shiny::HTML(as.character(i18n$t(":natGebiete:")))) ,
          #                         shiny::tags$script(
          #                           "
          # $('#step3-speciesCheckbox .checkbox label span').map(function(choice){
          #     this.innerHTML = $(this).text();
          #
          # });
          # "
          #                         )
          #                       )
          #                     )

                )
              ),
              shiny::fluidRow(
                shiny::column(4),
                shiny::column(4, align = "center",
                       shiny::plotOutput(shiny::NS(id, "AOIMap"), height = 400),
                ),
                shiny::column(4)
              ),
              shiny::div(style = "height: 10px"),
              shiny::fluidRow(
                shiny::column(12, align = "center", style = "display:table-cell; vertical-align: middle; ",
                       shiny::actionButton(shiny::NS(id, "confirmButton3"), label = i18n$t("Best\u00E4tigen"), class = "btn-success btn-lg"),
                       shiny::actionButton(shiny::NS(id, "skipButton"), label = i18n$t("Skip this step"), class = "btn-secondary")
                       )
              ),
              shiny::div(style = "height: 20px"),
              # ,
                # column(4, sliderInput(inputId = NS(id, "threshold"), label = "Threshold", min = 0.1, max = 1, value = 1))

)

}
