#### Step 1 UI - determine area ####
step4_ui <- function(id, i18n){
vftDbg("UI5")
      #vft-fit-page + vft-grow on the map row below: the map takes whatever the
      #heading rows above and the confirm/reset/download row below do not use,
      #so it runs down to the buttons instead of stopping 200px short of them.
      #See R/layout_helpers.R.
      shiny::fluidPage(class = "vft-fit-page",
        #activate translation for this ui
        shiny.i18n::usei18n(i18n),
        shinyjs::useShinyjs(),

        #the page banner (language select, title, logo, help/info) now lives
        #once in the nav bar - see vftStepNav() in R/app_ui.R. These three
        #inputs stay, just hidden: this step's server still listens for its
        #own languageSelect_4 / helpButton4 / infoButton4 unchanged, and
        #vftNavBannerProxyServer() drives them from the nav bar's single
        #visible control while this step is current.
        shinyjs::hidden(
          shiny::selectInput(inputId = shiny::NS(id, "languageSelect_4"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                             selected = i18n$get_translation_language(), width = 100 ),
          shiny::actionButton(inputId = shiny::NS(id, "helpButton4"), label = ""),
          shiny::actionButton(inputId = shiny::NS(id, "infoButton4"), label = "")
        ),

        shiny::fluidRow(
          shiny::column(12, align = "center",
                 shiny::h3(strong(i18n$t("Zielgebiete manuell korrigieren:")))
          )
        ),
        shiny::fluidRow(
          shiny::column(4),
          shiny::column(4, align = "center",
                        shiny::h5(i18n$t("Klicken Sie auf ein Zielgebiet, um es zu entfernen.")),
                        shiny::h5(i18n$t("Klicken Sie mehrmals auf ein leeres Areal, um ein neues zu erstellen.")),
                        shiny::h5(style = "color:#8f0404;font-weight:bold", i18n$t("Tipp: Jede einzelne Fläche sollte ein spezifisches Erholungsziel darstellen."))
          ),
          shiny::column(4, align = "left",
                        shinyWidgets::materialSwitch(
                          inputId = shiny::NS(id, "cutButton"),
                          label = i18n$t("Polygonschnitt-Modus"),
                          value = FALSE,
                          status = "danger"
                        )
          ) ),

              shiny::fluidRow(class = "vft-grow",
                shiny::column(12, align = "center",
                              shinyjs::useShinyjs(),
                              shinyjs::inlineCSS(list(.cutModeOn = "border-color: red; border-style: solid; border-width:5px;")),
                              shinyjs::inlineCSS(list(.cutModeOff = "border-color: black; border-style: solid; border-width:0px;")),

                              #mapFrame carries the cut-mode border, so it is the
                              #box that has to be full height - the map fills it,
                              #and the 5px red border in cut mode comes off the
                              #map rather than making the page 10px taller.
                              shiny::div(id= "mapFrame", class = "cutModeOff vft-grow-fill",
                       shinycssloaders::withSpinner(  leaflet::leafletOutput(shiny::NS(id, "finalAOIMap"), height = 500), type = 3, color = "#069869", color.background = "white" )
                              )
                       )
              ),
              shiny::div(style = "height: 10px"),
              shiny::fluidRow(
                shiny::column(12, align = "center", style = "display:table-cell; vertical-align: middle; ",
                       shiny::actionButton(shiny::NS(id, "confirmButton4"), label = i18n$t("Bestätigen"), class = "btn-success btn-lg"),

                       shiny::actionButton(shiny::NS(id, "resetButton"), label = i18n$t("Reset"), class = "btn-warning btn-lg"),
                       shiny::actionButton(shiny::NS(id, "aoiButton"), label = i18n$t("Download: Zielgebiete [.gpkg]"), class = "btn-warning"),
                       )
              ),
              shiny::div(style = "height: 20px"),
        shiny::fluidRow(
          shiny::column(12, align = "center", style = "display:table-cell; vertical-align: middle; ",
                        shinyjs::useShinyjs(),

                        shinyjs::hidden( shiny::downloadButton(NS(id, "downloadAOI")) )



          )
        )


)

}
