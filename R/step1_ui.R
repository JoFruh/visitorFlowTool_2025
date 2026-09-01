#### Step 1 UI - determine area ####

step1_ui <- function(id, i18n){
languageTable <- read.csv2(vftData("tables/translation_de.csv"))
vftDbg("UI1")
            #vft-fit-page: this step is a flex column the height of the pane, and
            #the row marked vft-grow below - the map - takes whatever the rows
            #around it are not using. That is what lets the map run to the bottom
            #of the screen WHILE the confirm buttons are still hidden, and hand
            #the space back by itself the moment they are shown. See
            #R/layout_helpers.R.
            shiny::fluidPage(class = "vft-fit-page",
              #activate translation for this ui
              shiny.i18n::usei18n(i18n),
              #the page banner (language select, title, logo, help/info) now
              #lives once in the nav bar - see vftStepNav() in R/app_ui.R. These
              #three inputs stay, just hidden: this step's server still listens
              #for its own languageSelect_1 / helpButton1 / infoButton1
              #unchanged, and vftNavBannerProxyServer() drives them from the
              #nav bar's single visible control while this step is current.
              shinyjs::hidden(
                shiny::selectInput(inputId = shiny::NS(id, "languageSelect_1"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                                   selected = "de", width = 100 ),
                shiny::actionButton(inputId = shiny::NS(id, "helpButton1"), label = ""),
                shiny::actionButton(inputId = shiny::NS(id, "infoButton1"), label = "")
              ),
              #vft-step1-head on both rows between the banner and the map: the
              #title, the intake block and the 'load saved data' button were set
              #at the app's default heading sizes and took about 150px off the
              #one thing this step is for. R/layout_helpers.R sets them a couple
              #of steps smaller at every screen height, not just a short one.
              shiny::fluidRow(class = "vft-step1-head",
                shiny::column(12, align = "center",
                       shiny::h3(shiny::strong(i18n$t("Definieren den Bereich in der Schweiz.") ) )
                )
              ),
              shiny::fluidRow(class = "vft-step1-head",
                shiny::column(3, align = "center",
                shiny::h4(i18n$t("Eine Kontur einreichen:") ),
                         shiny::h6(shiny::HTML(paste0(i18n$t("eine"), "<strong> .kml</strong>", i18n$t("Datei"), "<br>", i18n$t("oder als"), "<strong> Shapefile</strong><br>", i18n$t("durch mehrerer Dateien"), "<br>(<strong>.shp, .dbf, .shx</strong> etc.)" )) ), #
                         shiny::fileInput(shiny::NS(id, "shp"), label =NULL, multiple
                                          = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj", ".kml"))
                         #empty space for confirm button

                ),
                shiny::column(1, align = "center",
                         shiny::h4(i18n$t("ODER") )
                         ),
                shiny::column(4, align = "center",
                         shiny::h4(i18n$t("Klicken Sie mehrmals auf die Karte, um einen Bereich direkt zu zeichnen!") ),
                         shiny::h5(shiny::HTML(paste0(i18n$t("(Verwenden Sie das"), shiny::strong(i18n$t("Mausrad")), i18n$t("zum"), shiny::strong(i18n$t("Zoomen")), ".)"))),
                         #written from ten places in step1_server via shinyjs::html().
                         #A plain div, so it is not a registered output and never enters
                         #the per-message manageHiddenOutputs() sweep.
                         shiny::tags$div(id = shiny::NS(id, "zoomText"))
              ),
              shiny::column(1, align = "center",
                            shiny::h4(i18n$t("ODER") )
              ),
              shiny::column(3, align = "center",
              shiny::actionButton(inputId = NS(id, "loadSavedData"), label = shiny::HTML(text = paste0(i18n$t("Laden Sie gespeicherte Daten"), "<br/>", i18n$t("aus dieser App")," <strong>(.RData)</strong>") ), class = "btn-lg", type = "default")
              )
              ),

              shiny::fluidRow(
                shiny::column(2),
                shiny::column(3, align = "center",
                       shinyjs::useShinyjs(),
                       shinyjs::hidden(
                         shiny::actionButton(shiny::NS(id, "confirmButton1"), label = i18n$t("Best\u00E4tigen Datei"), class = "btn-success btn-lg")
                       )),
                shiny::column(2, align = "center",
                              shinyjs::hidden(
                                shiny::actionButton(shiny::NS(id, "attrButton"), label = i18n$t("Download: Naherholungskarte [.tif]"), class = "btn-warning")
                              )
                ),
                shiny::column(3, align = "center",
                       shinyjs::hidden(
                         shiny::actionButton(shiny::NS(id, "confirmButton2"), label = i18n$t("Best\u00E4tigen Bereich"), class = "btn-success btn-lg")

                       )),
                shiny::column(2)

              ),

            # fluidRow(
            #
            #   column(12, align = "center",
            #          titlePanel(h1("Besucherlenkungs-Tool", align = "center")),
            #          div(style = "height:15px"),
            #          h1("Schritt 1:"),
            #          h2("Definieren des Gebietes in der Schweiz."),
            #          div(style = "height:50px"),
            #          h3("Koordinaten bestimmen:"),
            #          textInput(NS(id, "gps_tl"), "GPS-Koordinaten der linken oberen Ecke", value = "X.XXX..., X.XXX..."),
            #          textInput(NS(id, "gps_br"), "GPS-Koordinaten der rechten unteren Ecke", value = "X.XXX..., X.XXX..."),
            #
            #          h1("", align = "center"),
            #          div(style = "height:5px"),
            #          h1("ODER\r\r", align = "center"),
            #          h1("\r"),
            #          div(style = "height:5px"),
            #          h3("Eine Kontur einreichen:"),
            #          h5("entweder als eine einzige", strong(" .kml"), "Datei"),
            #          h5("oder als", strong(" Shapefile "), "durch Auswahl mehrerer Dateien"," (", strong(".shp, .dbf, .sbn, .sbx, .shx, .prj"),")"),
            #          fileInput(NS(id, "shp"), label =NULL, multiple = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj", ".kml")),
            #          #empty space for confirm button
            #          shinyjs::useShinyjs(),
            #          hidden(
            #          actionButton(NS(id, "confirmButton"), label = "Bestätigen", class = "btn-success")
            #          )
            #
            #   )
            # ),

            # shiny::fluidRow(
            #   shiny::column(12, align = "center",
            #          shiny::textOutput(shiny::NS(id, "errorText")),
            #          shiny::tags$head(shiny::tags$style(paste0("#",shiny::NS(id, "errorText") ,"{color: red;
            #                                  font-size: 20px;
            #                                  font-style: italic;
            #                                  }")
            #          )))
            #
            # ),
            #the grow row: the map takes every pixel the rows above and below it
            #leave, so it ends at the bottom of the screen whether or not the
            #confirm buttons are showing. height = 600 stays as what it used to
            #be on a tall monitor; the stylesheet overrides it.
            shiny::fluidRow(class = "vft-grow",
              shiny::column(12, align = "center",
                     leaflet::leafletOutput(shiny::NS(id, "areaSelectMap"), height = 600),
              )
            ),
            fluidRow(
              shiny::column(12,
                            shinyjs::useShinyjs(),

                            shinyjs::hidden( shiny::downloadButton(NS(id, "downloadAttr")) )
                            )
            )
            # fluidRow(
            #   column(6, align = "center",
            #          plotOutput(NS(id, "startMap_whole") )
            #   ),
            #   column(6, align = "center",
            #          plotOutput(NS(id, "startMap_zoom") )
            #   )
            # ),
            # fluidRow(
            #   column(12, align = "center",
            #          h6("47.37, 8.41"),
            #          h6("47.34, 8.46"))
            # )

    )

}
