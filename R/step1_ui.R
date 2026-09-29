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
                shiny::actionButton(inputId = shiny::NS(id, "infoButton1"), label = ""),
                #the load card that stood beside the two cards below is now the
                #banner's "Sitzung laden" button (vftStepNav() in R/app_ui.R),
                #which clicks this one - so step1_server's loadSavedData
                #observer and its upload dialog are unchanged.
                shiny::actionButton(inputId = shiny::NS(id, "loadSavedData"), label = "")
              ),
              vftHead(i18n$t("Definieren den Bereich in der Schweiz.")),

              #THE TWO WAYS IN, as two centred cards with an 'ODER' between
              #them: upload an outline, or draw one on the map. (The third,
              #loading a saved session, is the banner's load button.)
              #Their text is set small (.vft-step1-* in R/layout_helpers.R) -
              #it took ~150px off the map at the app's default heading sizes.
              shiny::div(class = "vft-step1-options",
                shiny::div(class = "vft-card vft-step1-opt",
                  shiny::div(class = "vft-step1-opt-head", vftIcon("upload"), shiny::span(i18n$t("Eine Kontur einreichen:"))),
                  vftSub(shiny::HTML(paste0(i18n$t("eine"), " <strong>.kml</strong> ", i18n$t("Datei"), " ", i18n$t("oder als"), " <strong>Shapefile</strong> ", i18n$t("durch mehrerer Dateien"), " (<strong>.shp, .dbf, .shx</strong> etc.)"))),
                  shiny::fileInput(shiny::NS(id, "shp"), label =NULL, multiple
                                   = TRUE, accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj", ".kml"))
                ),
                shiny::div(class = "vft-step1-or", i18n$t("ODER")),
                shiny::div(class = "vft-card vft-step1-opt",
                  shiny::div(class = "vft-step1-opt-head", vftIcon("draw"), shiny::span(i18n$t("Zeichnen Sie ein Polygon auf der Karte."))),
                  vftSub(shiny::HTML(paste0(i18n$t("(Verwenden Sie das"), " ", shiny::strong(i18n$t("Mausrad")), " ", i18n$t("zum"), " ", shiny::strong(i18n$t("Zoomen")), ".)"))),
                  #written from ten places in step1_server via shinyjs::html().
                  #A plain div, so it is not a registered output and never enters
                  #the per-message manageHiddenOutputs() sweep.
                  shiny::tags$div(id = shiny::NS(id, "zoomText")),
                  #"too large for heat mitigation": never written, only shown
                  #and hidden by class - by step1_server for the outline in
                  #force, by polydraw.js for the ring being drawn. The rules
                  #are in R/layout_helpers.R.
                  shiny::tags$div(id = shiny::NS(id, "areaWarn"), class = "vft-area-warn",
                    shiny::tags$span(style = "color:#dd1717; font-size:medium; font-weight:bold;",
                      i18n$t("Warnung: Das gewählte Gebiet ist zu groß für die Hitzeminderungsplanung. Wenn Sie diese Funktion nutzen möchten, wählen Sie ein kleineres Gebiet.")))
                )
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
                        #ACTIONS, over the map : the download
            #first, the confirm last. All three start hidden and step1_server
            #shows them once there is an outline - the file's confirm for an
            #upload, the area's for a drawing - so until then the bar takes no
            #height at all (see .vft-actions in R/ui_theme.R).
            shiny::div(class = "vft-actions",
              shinyjs::hidden(
                shiny::actionButton(shiny::NS(id, "attrButton"), class = "vft-btn",
                                    label = vftBtnLabel("download", i18n$t("Download: Naherholungskarte [.tif]")))
              ),
              shinyjs::hidden(
                shiny::actionButton(shiny::NS(id, "confirmButton1"), class = "vft-btn-primary",
                                    label = vftBtnLabel("check", i18n$t("Bestätigen Datei")))
              ),
              shinyjs::hidden(
                shiny::actionButton(shiny::NS(id, "confirmButton2"), class = "vft-btn-primary",
                                    label = vftBtnLabel("check", i18n$t("Bestätigen Bereich")))
              )
            ),
            
            #the grow row: the map takes every pixel the rows above and below it
            #leave, so it ends at the bottom of the screen whether or not the
            #confirm buttons are showing. height = 600 stays as what it used to
            #be on a tall monitor; the stylesheet overrides it.
            shiny::fluidRow(class = "vft-grow",
              shiny::column(12, align = "center",
                     shiny::div(class = "vft-mapframe",
                       leaflet::leafletOutput(shiny::NS(id, "areaSelectMap"), height = 600)
                     )
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
