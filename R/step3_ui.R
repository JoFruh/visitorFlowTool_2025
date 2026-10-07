#### Step 3 UI - define the areas of interest ####

#' The threshold slider's look. It runs from 0 on the left to 20 on the right,
#' and the cells above the threshold are the ones the map paints red
#' (`col = c("white", "red3")` in step3_server.R), so the line from the handle
#' to the maximum is red3 and the line up to the handle is the skin's plain
#' grey. `!important` and two classes, because chooseSliderSkin()'s teal is an
#' `!important` rule of one class; its value label goes the step's dark grey.
#' @noRd
VFT_AOI_SLIDER_CSS <- "
.vft-aoi-slider .irs-line { background: #cd0000; border-color: #cd0000; }
.vft-aoi-slider .irs-bar, .vft-aoi-slider .irs-bar-edge {
  background: linear-gradient(to bottom, #dedede -50%, #fff 150%) !important;
  background-color: #ededed !important;
  border: 1px solid #cccccc; border-right: none; }
.vft-aoi-slider .irs-single { background: #4a4f4e !important; }
"

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

        #what an area of interest is, then what to do here, then the tip - three
        #paragraphs that used to be six headings in three sizes
        vftHead(i18n$t("Bestimmen der Zielgebiete"),
                vftSub(i18n$t("Ein Zielgebiet ist ein Areal, das für Naherholungssuchende von Interesse sein kann."), " ",
                       i18n$t("Für die Erholungssimulation müssen wir alle möglichen Zielgebiete definieren, aus denen die simulierten Besucher wählen können.")),
                vftSub(i18n$t("Bewegen Sie den Schieberegler, um den Umfang der Zielgebiete zu bestimmen. (Dies basiert auf einem 'Attraktivitätsmodell')"), " ",
                       i18n$t("Im nächsten Schritt haben Sie die Möglichkeit, Zielgebiete manuell zu korrigieren (hinzufügen/löschen/ausschneiden).")),
                vftTip(i18n$t("Tipp: Wählen Sie einen Schwellenwert, der die größten Bereiche erzeugt und diese gleichzeitig voneinander getrennt hält."))),

              shiny::fluidRow(
                shiny::column(4),
                shiny::column(4, align = "center",
                       #the skin applies to every slider in the app - step 2's
                       #threshold slider takes the teal from here too
                       shinyWidgets::chooseSliderSkin(skin = "Shiny", color = VFT_TEAL),
                       #...but not this one: everything above the threshold is
                       #what the map shows in red, so the line from the handle
                       #to the maximum is that red and the rest stays plain
                       shiny::tags$style(shiny::HTML(VFT_AOI_SLIDER_CSS)),
                       shiny::div(class = "vft-aoi-slider",
                         shinyWidgets::sliderTextInput(
                           inputId =shiny::NS(id, "AOISlider"),
                           label = i18n$t("Zielgebiete Schwelle"),
                           choices = as.character(round(seq(from = 0, to = 20, by = 0.1), 1)),
                           selected = 11))
                ),
                shiny::column(4)
              ),
              shiny::fluidRow(
                shiny::column(4),
                shiny::column(4, align = "center",
                       shiny::div(class = "vft-mapframe",
                         shiny::plotOutput(shiny::NS(id, "AOIMap"), height = 400)
                       )
                ),
                shiny::column(4)
              ),

              #ACTIONS: skipping first, the button that finishes the step last
              shiny::div(class = "vft-actions",
                shiny::actionButton(shiny::NS(id, "skipButton"), class = "vft-btn",
                                    label = vftBtnLabel("skip", i18n$t("Skip this step"))),
                shiny::actionButton(shiny::NS(id, "confirmButton3"), class = "vft-btn-primary",
                                    label = vftBtnLabel("check", i18n$t("Bestätigen")))
              )
)

}
