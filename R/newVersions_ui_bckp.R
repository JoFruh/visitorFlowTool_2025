#### Step 1 UI - determine area ####
newVersions_ui_bckp <- function(id, i18n){
print("UI6")
      shiny::fluidPage(
        #activate translation for this ui
        shiny.i18n::usei18n(i18n),
              #TITLE


               shiny::fluidRow(style = "display: flex; align-items:center;background-color:#006268; height: 100px; color: #ffffff; ",
                 # shiny::column(4, align = "left",
                 #               shiny::titlePanel(shiny::h1("Besucherlenkungs-Tool: ", align = "left"))
                 # ),
                 # shiny::column(4, align = "center",
                 #               shiny::h2("Schritt 4+ (Neuen Versionen)")
                 # )

                 shiny::column(4, align = "left",  style = "font-family: 'franklin gothic'",
                               shiny::HTML("<title>Visitor Flow Tool</title>"),
                               shiny::div(style = "margin-top: 2px"),

                               shiny::selectInput(inputId = shiny::NS(id, "languageSelect_7"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                                                  selected = "de", width = 100 ),
                               shiny::div(style = "margin-top:-25px"),
                               shiny::h2(i18n$t("Besucherlenkungs-Tool: ") )
                 ),
                 shiny::column(4,align = "center",
                               # shiny::h1("Schritt 1")
                               imageMap(NS(id, "banner"), 'www/stepNewVersions_wsl.png' , list() )


                 ),
                 shiny::column(4, align = "right",
                               shiny::column(10, align = "right",
                                             div(
                                               shiny::HTML("
                                          <img src ='www/BiodivCenterLogo_w.png' style = 'align: right; width: 200px; height:75%;object-fit:contain;'>
                                          ")
                                             )),
                               shiny::column(2, align = "right", style = "margin-top: 10px",
                                             shiny::actionButton(inputId = shiny::NS(id, "helpButton6"), label = "", style = "width: 30px; height: 30px;
background: url('helpIcon.png');  background-size: cover; background-position: center; border:none"),
                                             shiny::div(style = "margin-top:5px"),
                                             shiny::actionButton(inputId = shiny::NS(id, "infoButton6"), label = "", style = "width: 30px; height: 30px;
background: url('infoIcon.png');  background-size: cover; background-position: center; border: none")
                               )
                 )

               )
        ,
               shiny::fluidRow(
                 shiny::column(12, align = "center",
                               shiny::h4(strong(i18n$t("Schaffung neuer Szenarien für die Infrastruktur."))),
                               shiny::h4(i18n$t("Wählen Sie die Elemente, die Sie ändern möchten.")),
                               fluidRow(
                                 column(12, align = "center",


                                        shiny::uiOutput(outputId = NS(id, "contextChoice_ui"))
                                        # shiny::radioButtons(inputId = NS(id,"contextChoice"), label = NULL, inline = TRUE, choices = list(i18n$t("Infrastruktur") = 1,  i18n$t("Parken/Wohnen") = 3))#"Beschilderung/Attraktivität" = 2,

                                 )
                               ),
                 )
               ),


               #CONTENT
               shiny::fluidRow(
                 shiny::column(1, align = "center",
                        shiny::fluidRow(

                          shiny::column(12, align = "center",
                                 shiny::h5(shiny::strong(i18n$t("Sensitivitäts-Matrix anzeigen")))
                          )

                        ),

                        shiny::fluidRow(shiny::h5()),

                        shiny::fluidRow(

                          shiny::column(12, align = "center",
                                 shinyWidgets::prettySwitch(shiny::NS(id, "showSM"), value = FALSE, label = NULL, width = "150px",
                                              bigger = TRUE, fill = TRUE, status = "success", inline = TRUE)
                          ))
                        ),
                 #MAIN MAP ####

                 #code to alter legends manually (for left align etc.)
                 shiny::column(9, align = "center",
                               tags$head(
                                 tags$style(
                                   ".leaflet .legend {
                                            align: left;
                                            font-size: 15px;
                                            }"
                                 )),


                                 column(12,

                                        shinycssloaders::withSpinner(  leaflet::leafletOutput(shiny::NS(id, "versionMap"), height = 600), type = 3, color = "#069869", color.background = "white" )

                                 )

                 ),
                #SIDE BAR
                shiny::column(2,align = "center",

                              shiny::fluidRow(shiny::column(12,
                                                            shiny::h4(shiny::HTML(paste0(i18n$t("Erstellen/auswählen Sie"), "<br>", i18n$t("ein Szenario"))))
                              )
                              ),
                       shiny::fluidRow(
                         shinyjs::useShinyjs(),
                         #create new versions button
                         shiny::column(12,
                                shiny::actionButton( style = "background-color: #53bbb4; height: 50px; width: 50px; vertical-align: middle; font-size: 30px",
                                              inputId = shiny::NS(id, "addVersionButton"), label = shiny::strong("+")
                                )
                         )
                       ),
                       shiny::div( style = "height: 10px" ),

                       shiny::fluidRow(style= "padding-left: 25px",

                         #list of version boxes

                         shiny::column(12, align = "center", style='border: 1px solid black; vertical-align:middle; height:400px; width: 200px; overflow-y: scroll;',

                                shinyjs::useShinyjs(),
                                shinyjs::inlineCSS(list(.selected = "border-width: thick; border-color: green")),
                                shinyjs::inlineCSS(".selected:focus {border-width: thick; border-color: green; background color: white} "),
                                shinyjs::inlineCSS(list(.notSelected = "border-width: thin; border-color: grey")),
                                shinyjs::inlineCSS(list(.original = "border-width: thick; border-color: grey")),



                                shiny::div(id = "topPlaceHolder_newVersion",
                                    shiny::div(id = "placeholder")
                                )

                         )
                       ),


                       shiny::div( style = "height: 25px" ),

                       shiny::fluidRow(
                         shinyjs::useShinyjs(),
                         #confirmation button
                         shiny::column(12,
                                shiny::actionButton(class = "btn-success", style = "height: 70px; width: 180px ;vertical-align: middle",
                                             inputId = shiny::NS(id, "newVersionsConfirmButton"), label = i18n$t("Szenarien bestätigen")
                                )
                         )
                       )
                       )
                )


              )

}
