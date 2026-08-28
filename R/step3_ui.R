#### Step 1 UI - determine area ####
step3_ui <- function(id, i18n){
vftDbg("UI4")
      shiny::fluidPage(
        #activate translation for this ui
        shiny.i18n::usei18n(i18n),
        #the page banner (language select, title, logo, help/info) now lives
        #once in the nav bar - see vftStepNav() in R/app_ui.R. These three
        #inputs stay, just hidden: this step's server still listens for its
        #own languageSelect_3 / helpButton3 / infoButton3 unchanged, and
        #vftNavBannerProxyServer() drives them from the nav bar's single
        #visible control while this step is current.
        shinyjs::hidden(
          shiny::selectInput(inputId = shiny::NS(id, "languageSelect_3"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                             selected = i18n$get_translation_language(), width = 100 ),
          shiny::actionButton(inputId = shiny::NS(id, "helpButton3"), label = ""),
          shiny::actionButton(inputId = shiny::NS(id, "infoButton3"), label = "")
        ),

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
