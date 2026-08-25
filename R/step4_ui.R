#### Step 1 UI - determine area ####
step4_ui <- function(id, i18n){
vftDbg("UI5")
      shiny::fluidPage(
        #activate translation for this ui
        shiny.i18n::usei18n(i18n),
        shinyjs::useShinyjs(),

        shiny::fluidRow( style = "display: flex; align-items:center;background-color:#006268; height: 100px; color: #ffffff; ",
                         shiny::column(4, align = "left",  style = "font-family: 'franklin gothic'",
                                       shiny::HTML("<title>Visitor Flow Tool</title>"),
                                       shiny::div(style = "margin-top: 2px"),

                                       shiny::selectInput(inputId = shiny::NS(id, "languageSelect_4"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                                                          selected = i18n$get_translation_language(), width = 100 ),
                                       shiny::div(style = "margin-top:-25px"),
                                       shiny::h2(i18n$t("Besucherlenkungs-Tool: ")  )                       ),

                         shiny::column(4,align = "center",
                                       # shiny::h1("Schritt 1")
                                       vftBannerImg(id, "www/step4_wsl.png")


                         ),
                         shiny::column(4, align = "right",
                                       shiny::column(10, align = "right",
                                                     div(
                                                       shiny::HTML("
                                          <img src ='www/BiodivCenterLogo_w.png' style = 'align: right; width: 200px; height:75%;object-fit:contain;'>
                                          ")
                                                     )),
                                       shiny::column(2, align = "right", style = "margin-top: 10px",
                                                     shiny::actionButton(inputId = shiny::NS(id, "helpButton4"), label = "", style = "width: 30px; height: 30px;
background: url('helpIcon.png');  background-size: cover; background-position: center; border:none"),
                                                     shiny::div(style = "margin-top:5px"),
                                                     shiny::actionButton(inputId = shiny::NS(id, "infoButton4"), label = "", style = "width: 30px; height: 30px;
background: url('infoIcon.png');  background-size: cover; background-position: center; border: none")
                                       )
                         )
        ),
        shiny::fluidRow(column(12, align = "left", style = "display:inline-block;height:1px;color:#006268; font-family: 'franklin gothic';margin-top:-10px;margin-left:-13px ",
                               shiny::h5("app designer/contact: johan.frueh@wsl.ch", href = "mailto:'johan.frueh@wsl.ch'")
        )),

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

              shiny::fluidRow(
                shiny::column(12, align = "center",
                              shinyjs::useShinyjs(),
                              shinyjs::inlineCSS(list(.cutModeOn = "border-color: red; border-style: solid; border-width:5px;")),
                              shinyjs::inlineCSS(list(.cutModeOff = "border-color: black; border-style: solid; border-width:0px;")),

                              shiny::div(id= "mapFrame", class = "cutModeOff",
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
