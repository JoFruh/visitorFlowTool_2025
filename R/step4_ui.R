#### Step 4 UI - correct the areas of interest by hand ####
step4_ui <- function(id, i18n){
vftDbg("UI5")
      #vft-fit-page + vft-grow on the map row below: the map takes whatever the
      #head above and the action bar below do not use. See R/layout_helpers.R.
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

        vftHead(i18n$t("Zielgebiete manuell korrigieren:"),
                vftSub(i18n$t("Klicken Sie auf ein Zielgebiet, um es zu entfernen."), " ",
                       i18n$t("Klicken Sie mehrmals auf ein leeres Areal, um ein neues zu erstellen.")),
                #the tip, and to its right the button that cuts the areas into
                #destinations (VFT_AOI_SPLIT_METHOD) - arriving here gives the
                #plain threshold areas. Reset undoes it.
                shiny::div(class = "vft-tip-row",
                  vftTip(i18n$t("Tipp: Jede einzelne Fläche sollte ein spezifisches Erholungsziel darstellen.")),
                  shiny::actionButton(shiny::NS(id, "autoCutButton"), class = "vft-btn vft-btn-teal",
                                      label = vftBtnLabel("scissors", i18n$t("Automatische Schnitte"))))),

        shiny::fluidRow(class = "vft-grow",
          shiny::column(12, align = "center",
                        #mapFrame carries the border, so it is the box that
                        #has to be full height - the map fills it. (It used to
                        #turn red in the polygon cut mode; cutting is now the
                        #scissors button on a two-point line, drawn by
                        #inst/app/www/polydraw.js, and there is no mode.)
                        shiny::div(id= "mapFrame", class = "vft-grow-fill vft-step4-frame",
                                   style = "border: 1px solid #d6d9d8;",
                 shinycssloaders::withSpinner(  leaflet::leafletOutput(shiny::NS(id, "finalAOIMap"), height = 500), type = 3, color = VFT_TEAL, color.background = "white" )
                        )
          )
        ),

        #ACTIONS: the secondary buttons first, the button that finishes the
        #step last.
        shiny::div(class = "vft-actions",
          shiny::actionButton(shiny::NS(id, "resetButton"), class = "vft-btn",
                              label = vftBtnLabel("reset", i18n$t("Reset"))),
          shiny::actionButton(shiny::NS(id, "aoiButton"), class = "vft-btn",
                              label = vftBtnLabel("download", i18n$t("Download: Zielgebiete [.gpkg]"))),
          shiny::actionButton(shiny::NS(id, "confirmButton4"), class = "vft-btn-primary",
                              label = vftBtnLabel("check", i18n$t("Bestätigen")))
        ),

        shinyjs::hidden( shiny::downloadButton(shiny::NS(id, "downloadAOI")) )
)

}
