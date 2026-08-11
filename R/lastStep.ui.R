##### Last Step UI - save final images ####

lastStep_ui <- function(id){

  shiny::fluidPage(

    shiny::fluidRow( style = "display: flex; align-items:center;background-color:#006268; height: 100px; color: #ffffff; ",
                     shiny::column(4, align = "left",  style = "font-family: 'franklin gothic'",
                                   shiny::HTML("<title>Visitor Flow Tool</title>"),

                                   shiny::h2("Visitor Flow Tool: ")                         ),
                     shiny::column(4,align = "center",
                                   # shiny::h1("Schritt 1")
                                     imageMap(NS(id, "banner"), 'www/lastStep_wsl.png' , list(A = "0,0,0,100,70,100,70,0", B = "70,0,70,100,160,100,160,0", C = "160,0,160,100,260,100,260,0", D = "260,0,260,100,310,100,310,0", E = "310,0,310,100,360,100,360,0" ))


                     ),
                     shiny::column(4, align = "right",
                                   div(
                                     shiny::HTML("
                                          <img src ='www/BiodivCenterLogo_w.png' style = 'width: 50%; height:50%;object-fit:contain;'>
                                          ")
                                   )
                     )
    ),
    shiny::fluidRow(column(12, align = "left", style = "display:inline-block;height:1px;color:#006268; font-family: 'franklin gothic';margin-top:-10px;margin-left:-13px ",
                           shiny::h5("app designer/contact: johan.frueh@wsl.ch", href = "mailto:'johan.frueh@wsl.ch'")
    )),

    shiny::fluidRow(
      shiny::column(12, align = "center",
                    shiny::h3(strong("Informationen als fertige Bilder speichern:"))
      )
    ),
    shiny::fluidRow(
      shiny::column(12, align = "center",
                    shiny::h5(shiny::HTML("Wählen Sie die verschiedenen Informationen, die auf einer Karte angezeigt werden sollen, bevor Sie sie als .tiff-Bild (maximale Qualität, 300ppi) speichern.<br/>Sie können so viele verschiedene Bilder erstellen, wie Sie möchten.")),
      )
    ),

    shiny::fluidRow(
      shiny::tags$style("
      .radio { /* checkbox is a div class*/
        line-height: 20px;
        margin-bottom: 30px; /*set the margin, so boxes don't overlap*/
      }
      input[type='radio']{ /* style for checkboxes */
        width: 20px; /*Desired width*/
        height: 20px; /*Desired height*/
        line-height: 20px;
      }
      /* scoped to radio/checkbox labels only: a bare `span` rule here lands in <head>
         and would apply to every span in the whole app, not just this module */
      .radio span, .radio-inline span, .checkbox span, .checkbox-inline span {
          margin-left: 10px;  /*set the margin, so boxes don't overlap labels*/
          line-height: 20px;
      }
  "),

      shiny::column(3, style = "padding: 10px; align: left; font-size: 15px;",

                    shiny::tagList(
                      shiny::radioButtons(shiny::NS(id, "agentUseCheckbox"), "Agententyp",
                                          choices = c("alle" = "1", "Wanderer" = "2", "Radfahrer" = "3", "Hundespaziergänger" = "4", "Jogger" = "5")
                      ),
                      shiny::checkboxInput(shiny::NS(id, "onlyAOIcheckbox"), "Innerhalb Zielgebiete"),
                      shiny::checkboxInput(shiny::NS(id, "SMcheckbox"), "Sensitivitäts-Matrix",
                                           ),
                      shiny::checkboxInput(shiny::NS(id, "startingCheckbox"), "Agent starting points",
                      ),
                      shiny::checkboxInput(shiny::NS(id, "ParkingCheckbox"), "Parking",
                      ),
                      shiny::checkboxInput(shiny::NS(id, "ResidentialCheckbox"), "Bewohnen",
                      ),
                      shinyjs::useShinyjs(),
                      shinyjs::disabled(shiny::checkboxInput(shiny::NS(id, "frictionCheckbox"), shiny::HTML("Überlappung zwischen der Sensitivitätsmatrix<br>und der Erholungsnutzung ")))
                    ),
                    shiny::tags$script(
                      "
                          $('#step5-dayCheckbox .radio label span').map(function(choice){
                              this.innerHTML = $(this).text();

                          });
                          "
                    )

      ),
      shiny::column(6, align = "center",
                    tags$head(
                      tags$style(
                        ".leaflet .legend { text-align: left;font-size: 15px;}"
                      )),
                    # leaflet::leafletOutput(shiny::NS(id, "pathUsageMap"), height = 500px)
                    shiny::uiOutput(NS(id, "mapArea"), height = 500)
      ),
      shiny::column(1
      ),
      shiny::column(2,

                    shiny::fluidRow(

                      #list of version boxes

                      shiny::column(12, style='border: 1px solid black; vertical-align:middle; align: center; height:400px; width: 200px; overflow-y: scroll;',

                                    shinyjs::useShinyjs(),
                                    shinyjs::inlineCSS(list(.selected = "border-width: thick; border-color: green")),
                                    shinyjs::inlineCSS(".selected:focus {border-width: thick; border-color: green; background color: white} "),
                                    shinyjs::inlineCSS(list(.notSelected = "border-width: thin; border-color: grey")),
                                    shinyjs::inlineCSS(list(.original = "border-width: thick; border-color: grey")),
                                    shinyjs::inlineCSS(list(.noSim = "background: url('www/noSim.png'); background-size: cover; background-position: center")),
                                    shinyjs::inlineCSS(list(.withSim = "background: url('www/Sim.png'); background-size: cover; background-position: center")),



                                    shiny::div(id = "topPlaceHolder",
                                               shiny::div(id = "placeholder_lastStep")
                                    )

                      )
                    ),


                    shiny::div( style = "height: 5px" ),

                    # shiny::fluidRow(
                    #
                    # #confirmation button
                    # shiny::column(12, style = "align: center",
                    #         shiny::actionButton(class = "btn-success", style = "height: 70px; width: 190px ;align: center; vertical-align: middle; padding : 1px",
                    #           inputId = shiny::NS(id, "confirmButtonFinal"), label = shiny::HTML("Alle Simulationen abschlie&szlig;en <br/> und Ausgabe generieren")
                    #           )
                    #          )
                    #   ),

      )

    ),

    shiny::fluidRow(
      shiny::div(style = "height: 5px"),

      shiny::fluidRow(
        shiny::column(12, align = "center", style = "display:table-cell; vertical-align: middle; ",
                      shinyjs::useShinyjs(),

                      shiny::actionButton(shiny::NS(id, "launchSim"), label = shiny::HTML("Kartenkombination <br/> als Bild speichern "), class = "btn-success btn-lg"),

        )
      ),
      shiny::div(style = "height: 20px")

    )
  )

}
