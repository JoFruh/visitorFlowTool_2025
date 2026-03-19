#### Step 1 UI - determine area ####
step2_ui <- function(id){
print("UI2")
      shiny::fluidPage(
               shiny::fluidRow(
                 shiny::column(12, align = "center",
                        shiny::titlePanel(shiny::h1("Besucherlenkungs-Tool", align = "center")),
                        shiny::div(style = "height:20px"),
                        shiny::h1("Schritt 2:"),
                        shiny::h2("Generieren ein Netzwerk f\u00FCr Agentenbasierte-Modellierung (ABM)."),
                        shiny::div(style = "height:50px")
                 ),
               ),

               shiny::fluidRow(
                 shiny::column(12, align = "center",
                        leaflet::leafletOutput(shiny::NS(id, "plotPaths"), height = 600),
                        shinyjs::useShinyjs(),
                        shinyjs::hidden(
                          shiny::actionButton(shiny::NS(id, "confirmButton2"), label = "Best\u00E4tigen")
                        ),
                        shiny::textOutput(shiny::NS(id, "errorText"))
                        )


              ),
              shiny::fluidRow(
                shiny::column(4),
                shiny::column(4, shiny::sliderInput(shiny::NS(id, "AOIthreshold"), label = "AoI Threshold", min = 0, max = 100, value = 70) ),
                shiny::column(4)
              )
)

}
