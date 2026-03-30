
# Define server logic
step4_server <- function(id, network, shape, confirm, i18n, currentLang,
                         needHelp = FALSE, DULN_all = NULL){

  #shape is the submitted shapefile, or shape produced by submitted coordinates
  shiny::moduleServer(id, function(input, output, session) {

    #render banner image from start
    if(currentLang == "de"){
      output$bannerUI_4 <- shiny::renderUI({
        imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step3_wsl.png"), list() )
        #replace /" with ', to avoid problems
        return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
      })
    }else if(currentLang == "fr"){
      output$bannerUI_4 <- shiny::renderUI({
        imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step3_wsl_fr.png"), list() )
        #replace /" with ', to avoid problems
        return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
      })
    }else if(currentLang == "en"){
      output$bannerUI_4 <- shiny::renderUI({
        imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step3_wsl_en.png"), list() )
        #replace /" with ', to avoid problems
        return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
      })
    }

    r <- shiny::reactiveValues()
    r$needHelp <- needHelp

    #handle language bar

    r$needHelp <- needHelp

    cat(file = stderr(), "TRUE_STEP_4_STARTED")

    r$currentLang <- currentLang

    shiny.i18n::update_lang(r$currentLang)
    shiny::updateSelectInput(inputId = "languageSelect_4", selected = currentLang)



#
#     if(r$needHelp == TRUE){
#
#       shinyjs::delay(1500,{
#
#         shiny::showModal(
#           shiny::modalDialog( footer = shiny::modalButton(label = i18n()$t("OK!") ),
#             h2(i18n()$t("Schnelles Festlegen von Zielgebiete für Naherholung")),
#             div(style = "text-align:center",
#                 img(src = "www/goToAOI.png", style = "display:inline;height:150px")
#             ),
#             h4(shiny::HTML(i18n()$t("Menschen suchen <b> bestimmte Gebiete</b> auf, um sich <b>zu erholen</b> (<b>Zielgebiete</b>)."))),
#             h4(shiny::HTML(i18n()$t("Um Naherholung zu simulieren, müssen wir zunächst <b>solche Zielgebiete bestimmen</b>"))),
#             h3(),
#             h4(shiny::HTML(i18n()$t("Hier können Sie diese Gebiete mithilfe der '<b>Attraktivität</b>' schnell bestimmen."))),
#
#             h5(shiny::HTML(i18n()$t("Sie können einen <b>Balken</b> verschieben,"))),
#             div(style = "text-align: center",
#                 shiny::img(src = "www/sliderBar.png", style = "display:inline;height:30px")
#             ),
#             div(style = "text-align:right",
#                 h5(shiny::HTML( i18n()$t("um eine <b>Attraktivitätsschwelle</b> zu wählen")))
#             ),
#             h2(),
#             h4(shiny::HTML(i18n()$t("Gebiete, deren Attraktivität <b>über oder gleich</b> dem gewählten Schwellenwert liegt, werden rot schattiert und sind <b>Teil eines Zielgebiets</b>"))),
#             h3(),
#             h4(shiny::HTML(i18n()$t("Im <b>nächsten Schritt</b> können Sie Zielgebiete <b>manuell korrigieren</b>.")))
#           )
#         )
#       } )
#     }

    #re-enable buttons (if disabled)
    shinyjs::enable("confirmButton4")
    shinyjs::enable("skipButton")

    #Prepare polygon Data
    AOI <- NULL

    #remove saved polygons if present

    if(!is.null(r$finalPolygons)){
      r$finalPolygons <- NULL
    }

    r$x <- NULL
    r$confirm <- NULL

    r$naturalAreas <- NULL

    r$isSkip <- 0

    plotUpdate <- reactiveVal(1)

    basemap <- maptiles::get_tiles(shape, provider = "OpenStreetMap")
    shape <- sf::st_transform(shape, 3857)
    shape_wgs <- sf::st_transform(shape, 4326)

    # r$DULN <- terra::rast("www/data/maps/attr/allAttrs_COG_final.tif" )
    # r$DULN <- terra::crop(r$DULN, shape_wgs)

    # r$DULN_all <- terra::aggregate(r$DULN$all, fact = 2)
    # # terra::plot(terra::focal(r$DULN_all, w=matrix(1, 5, 5), mean))
    # #Get a single layer of DULN
    #
    # #Blur it to make it a smoother selection
    # r$DULN_all <- terra::focal(r$DULN_all, w=matrix(1, 5, 5), mean)
    # DULN <- terra::project(DULN, "epsg:4326")
    pathMap <- network |> tidygraph::activate(edges) |> dplyr::as_tibble() |> sf::st_as_sf()
    pathMap <- sf::st_transform(pathMap, 4326)

    basemap <- terra::project(basemap, "epsg:4326")
#
#     basemap_col <- terra::colorize(basemap, to="col")

    # maxVal <- minmax(DULN)$minmax[2]

    #### UPDATE UI ####
    # output$bannerUI <- shiny::renderUI({
    #   if(r$isImported == FALSE){
    #     imageMap(NS(id, "banner"), 'www/step3.png' , list(A = "0,0,0,100,70,100,70,0", B = "70,0,70,100,160,100,160,0") )
    #
    #   }else{
    #     imageMap(NS(id, "banner"), 'www/step3.png' , list() )
    #
    #   }
    # })


    #### FUNCTIONS ####

    output$AOIMap <- shiny::renderPlot({
      cat(file = stderr(), "PLOT-1")

      plotUpdate()

      #if plotting attractivity
      # if(shiny::isolate(input$naturalAreasCheck == FALSE)){

        r$x <- as.numeric(input$AOISlider)
        AOIBreaks <- c(145, r$x, 0) #TODO: 11 for now, get maxVal automatically later
        # bb <- st_bbox(shape)

        vectShape <- terra::vect(shape_wgs)
        vectExt <- as.vector(terra::ext(terra::buffer(vectShape, 100)))
        cat(file = stderr(), paste0("basemap: ", basemap, "\n") )
        cat(file = stderr(), paste0("vectExt: ", vectExt, "\n") )
        # basemap <- get_map(location = bb, maptype = 'terrain', source = 'google')

        basemap <- terra::crop(basemap, vectExt)
        # terra::ext(DULN_all) <- vectExt

        terra::plotRGB(basemap, reset = FALSE)
        # cat(file = stderr(), "PLOT1")

        terra::plot(DULN_all, alpha = 0.3, col = c("white", "red3"),
                    legend = FALSE, breaks = AOIBreaks,  range = c(0,100), add = TRUE)
        terra::lines(vectShape, col = "black", lwd = 5)
        # cat(file = stderr(), "PLOT2")

      # }else{
      #   #otherwise, plotting natural areas
      #   vectShape <- terra::vect(shape_wgs)
      #   vectExt <- as.vector(terra::ext(terra::buffer(vectShape, 100)))
      #   cat(file = stderr(), paste0("basemap: ", basemap, "\n") )
      #   cat(file = stderr(), paste0("vectExt: ", vectExt, "\n") )
      #   # basemap <- get_map(location = bb, maptype = 'terrain', source = 'google')
      #
      #   basemap <- terra::crop(basemap, vectExt)
      #   # terra::ext(DULN_all) <- vectExt
      #
      #   terra::plotRGB(basemap, reset = FALSE)
      #
      #   plot(r$naturalAreas[[1]], add = TRUE, col = "green", border = "darkgreen", alpha = 0.5)
      #
      #
      #
      # }
    })

    # OBSERVERS ####
    #dismiss Modal
    obs_dimissModal <- shiny::observeEvent(input$dismissModal, {
      shiny::removeModal()
    })

    #observe info Button ####
    obs_info3 <- shiny::observeEvent(input$infoButton3, {
      shiny::showModal(
        shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                           h2(i18n()$t("Zusätzliche Informationen:") ),
                           h3(),
                           h3(i18n()$t("In diesem Schritt wird ein Attraktivitätsmodell verwendet, um die Gebiete zu bestimmen, in denen sich Erholungssuchende aufhalten könnten ('Zielgebiete').") ),
                           h3(),
                           h4(i18n()$t("Das Attraktivitätsmodell wurde durch das von Kienast et al. (2012) entwickelte Modell der Naherholungsattraktivität inspiriert.") ),
                           h3(),
                           h4(i18n()$t("Das Modell kombiniert mehrere natürliche und künstliche Landschaftseigenschaften (Wälder, Schutzgebiete, Seen, Flüsse, kulturelle Sehenswürdigkeiten, Wegqualität, Wegbreite usw.), um zu bestimmen, wie attraktiv ein Gebiet für Erholungssuchende sein kann.") ),
                           h3(),
                           h4(i18n()$t("Die verschiedenen Eigenschaften werden unterschiedlich gewichtet. Kienast et al. (2012) hatten beispielsweise festgestellt, dass Seen, Flüsse und Aussichtspunkte die höchste Gewichtung (Wichtigkeit) haben, Wälder die zweithöchste Gewichtung und dass die Höhe und die Vielfalt der Landschaftseigenschaften ebenfalls wichtig sind.") ),
                           h3(),
                           h3(i18n()$t("Die vollständigen Einzelheiten werden in Kürze veröffentlicht und hier verlinkt, sobald sie verfügbar sind.") )

        )
      )
    })

    #observe help ####
    obs_help3 <- shiny::observeEvent(input$helpButton3, {
      shiny::showModal(
                  shiny::modalDialog( footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                    h2(i18n()$t("Schnelles Festlegen von Zielgebiete für Naherholung")),
                    div(style = "text-align:center",
                        img(src = "www/goToAOI.png", style = "display:inline;height:150px")
                    ),
                    h4(shiny::HTML(i18n()$t("Menschen suchen <b> bestimmte Gebiete</b> auf, um sich <b>zu erholen</b> (<b>Zielgebiete</b>)."))),
                    h4(shiny::HTML(i18n()$t("Um Naherholung zu simulieren, müssen wir zunächst <b>solche Zielgebiete bestimmen</b>"))),
                    h3(),
                    h4(shiny::HTML(i18n()$t("Hier können Sie diese Zielgebiete <b>schnell bestimmen</b>."))),

                    h5(shiny::HTML(i18n()$t("Sie können dazu den <b>roten Balken</b> verkleinern oder vergrössern,"))),
                    div(style = "text-align: center",
                        shiny::img(src = "www/sliderBar.png", style = "display:inline;height:30px")
                    ),
                    div(style = "text-align:right",
                        h5(shiny::HTML( i18n()$t("um eine <b>Attraktivitätsschwelle</b> zu wählen")))
                    ),
                    h2(),
                    h4(shiny::HTML(i18n()$t("Gebiete, die <b>über</b> dem gewählten Schwellenwert liegent, werden rot schattiert und sind <b>Teil eines Zielgebiets</b>"))),
                    h3(),
                    h4(shiny::HTML(i18n()$t("Im <b>nächsten Schritt</b> können Sie Zielgebiete <b>manuell korrigieren</b>.")))
                  )
                )
    })

    #Language Change ####
    langChangeObs <- observeEvent(input$languageSelect_4, {
      print("CHANGE LANGUAGE")
      if(input$languageSelect_4 == "de"){
        # i18n$set_translation_language('de')
        shiny.i18n::update_lang("de")
        i18n()$set_translation_language("de")
        print("DE")
        output$bannerUI_4 <- shiny::renderUI({
          imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step3_wsl.png"), list() )
          #replace /" with ', to avoid problems
          return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
        })



      }else if(input$languageSelect_4 == "fr"){
        # i18n$set_translation_language('fr')
        shiny.i18n::update_lang("fr")
        i18n()$set_translation_language("fr")

        output$bannerUI_4 <- shiny::renderUI({
          imgMap <- imageMap(NS(id, "banner"), "www/step3_wsl_fr.png", list() )
          #replace /" with ', to avoid problems
          return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
        })


        print("FR")
      }else if(input$languageSelect_4 == "en"){
        # i18n$set_translation_language('en')
        shiny.i18n::update_lang("en")
        output$bannerUI_4 <- shiny::renderUI({
          imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step3_wsl.png"), list() )
          #replace /" with ', to avoid problems
          return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
        })


        print("EN")
      }else if(input$languageSelect_4 == "it"){
        # i18n$set_translation_language('it')
        shiny.i18n::update_lang("it")
        output$bannerUI_4 <- shiny::renderUI({
          imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step3_wsl.png"), list() )
          #replace /" with ', to avoid problems
          return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
        })


        print("IT")
      }

    }, ignoreInit = TRUE)

    #observe banner click (choosing to step back in history)
    obsBanner <- observeEvent(input$banner,  {
      print("MAPPED IMAGE CLICKED")
      #determine where to go back in history
      r$confirm <- input$banner

      #cleanup
      obsBanner$destroy()
      obsConfirm$destroy()
      obsSkip$destroy()


      return(list(minThresh = shiny::reactive(r$x), confirm = shiny::reactive({r$confirm}), skip = shiny::reactive({input$skipButton}), isSkip = shiny::reactive({r$isSkip}),
                  needHelp = shiny::reactive(r$needHelp), naturalAreas = shiny::reactive(NULL),
                  currentLang = shiny::reactive(i18n()$get_translation_language())) )

      #trigger return to past (return with specific confirm value?)
    }, ignoreInit = TRUE)

    obsConfirm <- shiny::observeEvent(input$confirmButton4, {

      #disable buttons temporarily
      shinyjs::disable("confirmButton4")
      shinyjs::disable("skipButton")

      r$confirm <- input$confirmButton4

      obsConfirm$destroy()
      obsSkip$destroy()
      #flush input value as well as destroying button
      # shinyjs::runjs("Shiny.setInputValue('confirmButton4', 0)")
      shinyjs::reset("step4-confirmButton4")
      # shinyjs::enable("confirmButton4")

      # if(input$naturalAreasCheck == FALSE){
      #   r$naturalAreas <- NULL
      # }

      #if the threshold is above max value of DULN_all
      #(There is no AoI)
      if(r$x > terra::minmax(DULN_all)[2]){
        #return imitating the skip button being pressed (skip = 1)
        r$isSkip <- 1
      }

      return(list(minThresh = shiny::reactive(r$x), confirm = shiny::reactive({r$confirm }), skip = shiny::reactive({input$skipButton}), isSkip = shiny::reactive({r$isSkip}),
                  needHelp = shiny::reactive(r$needHelp), naturalAreas = shiny::reactive(r$naturalAreas ),
                  currentLang = shiny::reactive(i18n()$get_translation_language())) )

    }, ignoreInit = TRUE)

    obsSkip <- shiny::observeEvent(input$skipButton, {
      shinyjs::disable("confirmButton4")
      shinyjs::disable("skipButton")

      r$confirm <- input$confirmButton4

      r$naturalAreas <- NULL

      r$isSkip <- 1

      return(list(minThresh = shiny::reactive(r$x), confirm = shiny::reactive({r$confirm }), skip = shiny::reactive({input$skipButton}), isSkip = shiny::reactive({r$isSkip}),
                  needHelp = shiny::reactive(r$needHelp), naturalAreas = shiny::reactive(r$naturalAreas)) )

    }, ignoreInit = TRUE)

    # #change DULN raster used, based on checkbox for natural areas
    # obsNaturalAreas <- shiny::observeEvent(input$naturalAreasCheck, {
    #   if(input$naturalAreasCheck == TRUE){
    #
    #
    #
    #     #deactivate slider
    #     shinyjs::disable(id = "AOISlider")
    #     #replace DULN with natural areas raster
    #
    #     #Get Natural areas data
    #
    #     #if it doesn't exist yet, retrieve it
    #     if(is.null(r$naturalAreas) ){
    #
    #       shinyjs::disable("confirmButton4")
    #
    #
    #
    #       progress <- ipc::AsyncProgress$new(message = "Loading Natural Area information...",
    #                                           detail = paste0("This should take less than ", 30, " seconds"),
    #                                           queue = ipc::shinyQueue(),
    #                                           millis = 1000)
    #       future::future({
    #
    #           #increment 1: loading paths
    #           progress$inc(1/3, detail = "Loading natural area info...")
    #           #LOAD / FILTER NATURAL AREAS ####
    #           wkt <- sf::st_as_text(sf::st_geometry(shape_wgs))
    #           #retrieve natural areas and crop
    #           natAreas <- sf::st_read("www/data/maps/naturalAreas/naturalAreas.shp",
    #                                  query = 'SELECT * FROM "naturalAreas"',
    #                                  wkt_filter = wkt
    #           )
    #
    #           progress$inc(2/3, detail = "Loading natural area info...")
    #
    #           #crop polygons to area
    #
    #           #get areas
    #           sf::sf_use_s2(FALSE)
    #           natAreas_cropped <- sf::st_crop(natAreas, shape_wgs )
    #           # natAreas_cropped_cast <- sf::st_cast(natAreas_cropped, "POLYGON", do_split = TRUE)
    #           natAreas_cropped_cast <- natAreas_cropped |> sf::st_cast("MULTIPOLYGON")  |> sf::st_cast("POLYGON")
    #           natAreas_cropped_cast$area <- sf::st_area(natAreas_cropped_cast)
    #
    #           sf::sf_use_s2(TRUE)
    #
    #           progress$inc(3/3, detail = "Loading natural area info...")
    #           #filter out smallest polygons
    #           natAreas_cropped_cast <- dplyr::filter(natAreas_cropped_cast, area > units::set_units(10000, "m^2") )
    #
    #         progress$close()
    #
    #         natAreas_cropped_cast
    #       }, seed = TRUE)%...>%(function(natAreas){
    #
    #         r$naturalAreas <- list(natAreas)
    #         shinyjs::enable(id = "confirmButton4")
    #
    #         plotUpdate(plotUpdate()+1)
    #
    #         return(r$naturalAreas)
    #
    #       })
    #
    #
    #     }else{
    #       plotUpdate(plotUpdate()+1)
    #     }
    #
    #   }else{
    #     #reactivate slider
    #     shinyjs::enable(id = "AOISlider")
    #     #replace natural areas raster with DULN
    #     plotUpdate(plotUpdate()+1)
    #   }
    # }, ignoreInit = TRUE)

    r$confirm <- input$confirmButton4
    return(list(minThresh = shiny::reactive(r$x), confirm = shiny::reactive({r$confirm }), skip = shiny::reactive({input$skipButton}), isSkip = shiny::reactive({r$isSkip}),
                needHelp = shiny::reactive(r$needHelp), naturalAreas = shiny::reactive(r$naturalAreas),
                currentLang = shiny::reactive(i18n()$get_translation_language())) )
  })
}
