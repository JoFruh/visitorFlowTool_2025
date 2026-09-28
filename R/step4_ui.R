#### Step 4 UI - correct the target areas by hand ####
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
                vftTip(i18n$t("Tipp: Jede einzelne Fläche sollte ein spezifisches Erholungsziel darstellen."))),

        shiny::fluidRow(class = "vft-grow",
          shiny::column(12, align = "center",
                        #cut mode: a red frame round the map, so the mode is
                        #visible where the clicks go. Moved between the two
                        #classes by the cutButton observer in step4_server.R.
                        shinyjs::inlineCSS(list(.cutModeOn = "border: 3px solid #c62828;")),
                        shinyjs::inlineCSS(list(.cutModeOff = "border: 1px solid #d6d9d8;")),

                        #mapFrame carries the cut-mode border, so it is the
                        #box that has to be full height - the map fills it,
                        #and the border comes off the map rather than making
                        #the page taller in cut mode.
                        shiny::div(id= "mapFrame", class = "cutModeOff vft-grow-fill vft-step4-frame",
                 shinycssloaders::withSpinner(  leaflet::leafletOutput(shiny::NS(id, "finalAOIMap"), height = 500), type = 3, color = VFT_TEAL, color.background = "white" )
                        )
          )
        ),

        #ACTIONS: the mode switch and the secondary buttons first, the button
        #that finishes the step last. The switch is a plain checkbox styled as
        #one (vftRailRow, R/ui_theme.R): input$cutButton is TRUE/FALSE as it
        #was from the materialSwitch it replaces.
        shiny::div(class = "vft-actions",
          shiny::tags$label(
            class = "vftRailRow vft-switch-pill",
            shiny::tags$input(id = shiny::NS(id, "cutButton"), type = "checkbox", class = "vftRailCheck"),
            shiny::span(class = "vftGlyph", vftIcon("scissors")),
            shiny::span(class = "vftRailLabel", i18n$t("Polygonschnitt-Modus")),
            shiny::span(class = "vftSw")),
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
