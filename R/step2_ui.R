#### Step 1 UI - determine area ####
step2_ui <- function(id, i18n){
  shiny::fluidPage(
    #activate translation for this ui
    shiny.i18n::usei18n(i18n),

    shiny::fluidRow( style = "display: flex; align-items:center;background-color:#006268; height: 100px; color: #ffffff; ",
                     shinyjs::useShinyjs(),
                     shiny::column(4, align = "left",  style = "font-family: 'franklin gothic'",
                                   shiny::HTML("<title>Visitor Flow Tool</title>"),
                                   shiny::div(style = "margin-top: 2px"),
                                   shiny::selectInput(inputId = shiny::NS(id, "languageSelect_2"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                                                      selected = i18n$get_translation_language(), width = 100 ),
                                   shiny::div(style = "margin-top:-25px"),
                                   shiny::h2(i18n$t("Besucherlenkungs-Tool: ") )                     ),

                     shiny::column(4,align = "center",
                                   # shiny::h1("Schritt 1")
                                   vftBannerImg(id, "www/step2_wsl.png")


                     ),
                     shiny::column(4, align = "right",
                                   shiny::column(10, align = "right",
                                                 div(
                                                   shiny::HTML("
                                          <img src ='www/BiodivCenterLogo_w.png' style = 'align: right; width: 200px; height:75%;object-fit:contain;'>
                                          ")
                                                 )),
                                   shiny::column(2, align = "right", style = "margin-top: 10px",
                                                 shiny::actionButton(inputId = shiny::NS(id, "helpButton2"), label = "", style = "width: 30px; height: 30px;
background: url('helpIcon.png');  background-size: cover; background-position: center; border:none"),
                                                 shiny::div(style = "margin-top:5px"),
                                                 shiny::actionButton(inputId = NS(id, "infoButton2"), label = "", style = "width: 30px; height: 30px;
background: url('infoIcon.png');  background-size: cover; background-position: center; border: none")
                                   )
                     )
    ),
    shiny::fluidRow(column(12, align = "left", style = "display:inline-block;height:1px;color:#006268; font-family: 'franklin gothic';margin-top:-10px;margin-left:-13px ",
                           shiny::h5(i18n$t("app designer/contact: johan.frueh@wsl.ch"), href = "mailto:'johan.frueh@wsl.ch'")
    )),
    shiny::fluidRow(
      shiny::column(12, align = "center",
      shiny::h3(shiny::strong(i18n$t("Bestimmen die Sensitivit\u00E4tsmatrix")))
      )
    ),
    shiny::fluidRow(
      shiny::column(5, align = "center",
             shiny::h4(i18n$t("Arten ausw\u00E4hlen/abw\u00E4gen") )
      ),
      shiny::column(2, align = "center",
             shiny::h4(i18n$t("UND/ODER"))
      ),
      shiny::column(5, align = "center",
             shiny::h4(i18n$t("Gruppen ausw\u00E4hlen"))
      )
    ),
    shiny::div(style = "height: 10px"),

    shiny::fluidRow(shiny::column(4, align = "center", style = "vertical-align:middle;",
                    shiny::fluidRow(

                             shiny::column(3,
                                    shiny::h5(shiny::strong(i18n$t("Filter: ")) )
                             ),
                             shiny::column(9,
                                    shiny::tags$style(type='text/css', ".selectize-input { font-size: 15px; line-height: 15px;} .selectize-dropdown { font-size: 13px; line-height: 13px; }"),
                                    shiny::uiOutput(shiny::NS(id, "filterList_ui") )
                             )

                    ),

                    shiny::h5(i18n$t("Verbreitetsten")),
                    shiny::h5("\u21D1"),

                    shiny::column(12,  align = "left", style = "vertical-align:middle; overflow-y: scroll; overflow-x:scroll; height: 350px",

                           # column(2,
                           #        uiOutput(outputId = NS(id, "speciesWeights") )
                           #        ),
                           shinyjs::inlineCSS(list(.checkbox = "width: 600px")),
                           shiny::uiOutput(shiny::NS(id, "speciesCheckbox"))
                    ),
                    shiny::h5("\u21D3"),
                    shiny::h5(i18n$t("Wenigsten verbreitet"))


             ),
                shiny::column(4, align = "center", style = " vertical-align: middle; height: 500px",
                              shiny::uiOutput(outputId = NS(id, "minCutoff_UI")),
                       shiny::sliderInput(shiny::NS(id, "minValThreshold"), label = i18n$t("Mindestschwellenwert für die Sensitivitätsmatrix [ % ]"), min = 0, max = 100, value = 0,
                                          ticks = FALSE),
                       shiny::plotOutput(shiny::NS(id, "SDMmap"))
                       ),
                shiny::column(1),
                shiny::column(3,  align = "left", style = " vertical-align:middle;  overflow-y: scroll; overflow-x:scroll ; height: 500px" ,
                       shiny::checkboxInput(shiny::NS(id, "groupCheckbox_all"), label = i18n$t("Alle") ),
                       shiny::uiOutput(shiny::NS(id, "groupCheckbox_sens")),
                       shiny::uiOutput(shiny::NS(id, "groupCheckbox_type")),
                       shiny::uiOutput(shiny::NS(id, "groupCheckbox_class")),


                       )
              ),
              shiny::fluidRow(
                shiny::column(4, align = "center",
                       shiny::actionButton(shiny::NS(id, "redListWeights"), label = i18n$t("Gewicht Status Rote Liste"), class = "btn-secondary"),
                       shiny::actionButton(shiny::NS(id, "priorityWeights"), label = i18n$t("Gewicht Priorit\u00E4t"), class = "btn-secondary"),
                       shiny::actionButton(shiny::NS(id, "resetWeights"), label = i18n$t("Gewichte zur\u00FCcksetzen"), class = "btn-secondary")
                       ),
                shiny::column(4, align = "center",
                       shiny::actionButton(shiny::NS(id, "confirmButton2"), label = i18n$t("Best\u00E4tigen"), class = "btn-success btn-lg"),

                       shiny::actionButton(shiny::NS(id, "SMbutton"), label = i18n$t("Download: Sensitivitäts-Matrix [.tif]"), class = "btn-warning")
                       # shiny::p("oder"),
                       # shiny::actionButton(shiny::NS(id, "selectSpAfter"), label = "Auswahl der Arten NACH der Erholungsmodellierung",
                       #              class = "btn-warning")
                       )
              ),
              shiny::fluidRow(
                shiny::column(12, align = "left",
                       #Legend
                       shiny::uiOutput(outputId = shiny::NS(id,"legend_ui") )

                       )
                # ,
                # column(4, sliderInput(inputId = NS(id, "threshold"), label = "Threshold", min = 0.1, max = 1, value = 1))
              ),
    shiny::fluidRow(
      shiny::column(12, align = "center", style = "display:table-cell; vertical-align: middle; ",
                    shinyjs::useShinyjs(),

                    shinyjs::hidden( shiny::downloadButton(NS(id, "downloadSM")) )



      )
    )

)

}
