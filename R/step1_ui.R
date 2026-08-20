#### Step 1 UI - determine area ####

step1_ui <- function(id, i18n){
languageTable <- read.csv2(vftData("tables/translation_de.csv"))
print("UI1")
            shiny::fluidPage(
              #activate translation for this ui
              shiny.i18n::usei18n(i18n),
              shiny::fluidRow( style = "display: flex; align-items:center;background-color:#006268; height: 100px; color: #ffffff; ",
                               shiny::column(4, align = "left",  style = "font-family: 'franklin gothic'",
                                             shiny::HTML("<title>Visitor Flow Tool</title>"),
                                             shiny::div(style = "margin-top: 2px"),

                                                           shiny::selectInput(inputId = shiny::NS(id, "languageSelect_1"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                                                                              selected = "de", width = 100 ),
                                                           shiny::div(style = "margin-top:-25px"),
                                                           shiny::h2(i18n$t("Besucherlenkungs-Tool: ") ),



                               ),

                shiny::column(4,align = "center",
                        shiny::uiOutput(NS(id,"bannerUI"))
                       # shiny::h1("Schritt 1")
                       # imageMap(NS(id, "banner"), "www/step1_wsl.png") , list() )


                ),
                shiny::column(4, align = "right",
                              shiny::column(10, align = "right",
                              div(
                              shiny::HTML("
                                          <img src ='www/BiodivCenterLogo_w.png' style = 'align: right; width: 200px; height:75%; object-fit:contain;'>
                                          ")
                              )),
                              shiny::column(2, align = "right", style = "margin-top: 10px",
                                            shiny::actionButton(inputId = shiny::NS(id, "helpButton1"), label = "", style = "width: 30px; height: 30px;
background: url('helpIcon.png');  background-size: cover; background-position: center; border:none"),
                                            shiny::div(style = "margin-top:5px"),
                                            shiny::actionButton(inputId = shiny::NS(id, "infoButton1"), label = "", style = "width: 30px; height: 30px;
background: url('infoIcon.png');  background-size: cover; background-position: center; border: none")
                              )
                              )


              ),
              shiny::fluidRow(column(12, align = "left", style = "display:inline-block;height:1px;color:#006268; font-family: 'franklin gothic';margin-top:-10px;margin-left:-13px ",
                              shiny::h5(i18n$t("app designer/kontakt: johan.frueh@wsl.ch"), href = "mailto:'johan.frueh@wsl.ch'")
              )),
              shiny::fluidRow(
                shiny::column(12, align = "center",
                       shiny::h3(shiny::strong(i18n$t("Definieren den Bereich in der Schweiz.") ) )
                )
              ),
              shiny::fluidRow(
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
                         shiny::htmlOutput(shiny::NS(id, "zoomText"))
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
            shiny::fluidRow(
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
