#### Step 1 UI - determine area ####
step2_ui <- function(id, i18n){
  shiny::fluidPage(
    #activate translation for this ui
    shiny.i18n::usei18n(i18n),

    shinyjs::useShinyjs(),
    #the page banner (language select, title, logo, help/info) now lives once
    #in the nav bar - see vftStepNav() in R/app_ui.R. These three inputs stay,
    #just hidden: this step's server still listens for its own
    #languageSelect_2 / helpButton2 / infoButton2 unchanged, and
    #vftNavBannerProxyServer() drives them from the nav bar's single visible
    #control while this step is current.
    shinyjs::hidden(
      shiny::selectInput(inputId = shiny::NS(id, "languageSelect_2"), label = NULL, choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                         selected = i18n$get_translation_language(), width = 100 ),
      shiny::actionButton(inputId = shiny::NS(id, "helpButton2"), label = ""),
      shiny::actionButton(inputId = shiny::NS(id, "infoButton2"), label = "")
    ),
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

    #the three columns below are one visual band and share one height
    #(vft-step2-col, R/layout_helpers.R). Inside each, the tall scrolling part
    #flexes and everything else keeps its own size, so the species list, the
    #plot and the group list all end on the same line at any screen height.
    shiny::fluidRow(shiny::column(4, align = "center", class = "vft-step2-col",
                    style = "vertical-align:middle;",
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

                    #this is the part of the column that grows: it takes whatever
                    #the filter row, the two captions and the weight buttons
                    #below it leave, so the list is as long as the screen allows
                    #and no longer. It already scrolls, so a short screen costs
                    #rows of the list rather than the buttons underneath it.
                    #See .vft-step2-col in R/layout_helpers.R.
                    shiny::column(12,  align = "left", class = "vft-fit-species",
                                  style = "vertical-align:middle; overflow-y: scroll; overflow-x:scroll;",

                           # column(2,
                           #        uiOutput(outputId = NS(id, "speciesWeights") )
                           #        ),
                           shinyjs::inlineCSS(list(.checkbox = "width: 600px")),
                           shiny::uiOutput(shiny::NS(id, "speciesCheckbox"))
                    ),
                    shiny::h5("\u21D3"),
                    shiny::h5(i18n$t("Wenigsten verbreitet")),

                    #the three weight buttons live INSIDE this column now. They
                    #used to be a row of their own underneath, and that is what
                    #capped the plot in the next column: a Bootstrap row clears
                    #the tallest column of the row above it, so the plot could
                    #never run down past the species list to the buttons' level -
                    #making it taller only pushed the buttons further down. In
                    #the same column they are part of the same band, and the
                    #plot beside them now reaches their line. They are in the
                    #same place on screen as before: this is the column they
                    #already sat under.
                    shiny::div(class = "vft-step2-weights",
                       shiny::actionButton(shiny::NS(id, "redListWeights"), label = i18n$t("Gewicht Status Rote Liste"), class = "btn-secondary"),
                       shiny::actionButton(shiny::NS(id, "priorityWeights"), label = i18n$t("Gewicht Priorit\u00E4t"), class = "btn-secondary"),
                       shiny::actionButton(shiny::NS(id, "resetWeights"), label = i18n$t("Gewichte zur\u00FCcksetzen"), class = "btn-secondary")
                    )


             ),
                shiny::column(4, align = "center", class = "vft-step2-col",
                              style = " vertical-align: middle;",
                              shiny::uiOutput(outputId = NS(id, "minCutoff_UI")),
                       shiny::sliderInput(shiny::NS(id, "minValThreshold"), label = i18n$t("Mindestschwellenwert für die Sensitivitätsmatrix [ % ]"), min = 0, max = 100, value = 0,
                                          ticks = FALSE),
                       shiny::plotOutput(shiny::NS(id, "SDMmap"))
                       ),
                shiny::column(1),
                shiny::column(3,  align = "left", class = "vft-step2-col",
                              style = " vertical-align:middle;  overflow-y: scroll; overflow-x:scroll ;" ,
                       shiny::checkboxInput(shiny::NS(id, "groupCheckbox_all"), label = i18n$t("Alle") ),
                       shiny::uiOutput(shiny::NS(id, "groupCheckbox_sens")),
                       shiny::uiOutput(shiny::NS(id, "groupCheckbox_type")),
                       shiny::uiOutput(shiny::NS(id, "groupCheckbox_class")),


                       )
              ),
              #the confirm and download buttons share the legend's row rather
              #than having one of their own. That is a whole button row's worth
              #of height handed back to the band above - the weight buttons that
              #used to sit beside them are inside the species column now.
              #
              #The legend takes the LEFT THIRD only. That is what keeps the
              #button column the middle third, so the buttons stay centred on
              #the page - exactly where they sat when this row held the weight
              #buttons beside them. A wider legend column would be a wider
              #float, and the buttons would ride along to the right edge with
              #it. The legend is far wider than a third of the screen, so it
              #scrolls sideways inside its own box instead of claiming the
              #space - see .vft-step2-foot in R/layout_helpers.R.
              shiny::fluidRow(class = "vft-step2-foot",
                shiny::column(4, align = "left",
                       #Legend
                       shiny::uiOutput(outputId = shiny::NS(id,"legend_ui") )
                       ),
                shiny::column(4, align = "center",
                       shiny::actionButton(shiny::NS(id, "confirmButton2"), label = i18n$t("Best\u00E4tigen"), class = "btn-success btn-lg"),

                       shiny::actionButton(shiny::NS(id, "SMbutton"), label = i18n$t("Download: Sensitivitäts-Matrix [.tif]"), class = "btn-warning")
                       # shiny::p("oder"),
                       # shiny::actionButton(shiny::NS(id, "selectSpAfter"), label = "Auswahl der Arten NACH der Erholungsmodellierung",
                       #              class = "btn-warning")
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
