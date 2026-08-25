app_ui <- function(){

  #prepare multilingual functions
  i18n <- shiny.i18n::Translator$new(translation_csvs_path = vftData("tables"), separator_csv = ";" )
  i18n$set_translation_language('de')

  shiny::fluidPage(
  #build tabs
  shinyjs::useShinyjs(),

  #Included once, at app level, for the whole session: it stops leaflet's
  #per-hover input messages at the client. See inst/app/www/vft-shim.js for what
  #it drops and why the list must stay at two suffixes. `www` is registered as a
  #resource path in .onLoad (R/zzz.R).
  shiny::tags$script(src = "www/vft-shim.js"),

  shinyjs::hidden( shiny::downloadButton("downloadSave") ),
  # shinyjs::hidden( shiny::downloadButton("downloadSaveRaster") ),

  shiny::tabsetPanel(id = "tabs", type = "hidden",

                     shiny::tabPanel( "tab_step1",

                                      step1_ui("step1", i18n = i18n),
                     ),

                     shiny::tabPanel( "tab_step2",

                                      step2_ui("step2", i18n = i18n)
                     ),
                     shiny::tabPanel( "tab_step3",

                                      step3_ui("step3", i18n = i18n)

                     ),
                     shiny::tabPanel( "tab_step4",

                                      step4_ui("step4", i18n = i18n)

                     ),
                     shiny::tabPanel( "tab_step5",

                                      step5_ui("step5", i18n = i18n)

                     ),
                     shiny::tabPanel("tab_newVersions",
                                     newVersions_ui("newVersions", i18n = i18n)
                     ),
                     shiny::tabPanel("tab_finalStep",

                                     lastStep_ui("finalStep"))
  )
)
}
