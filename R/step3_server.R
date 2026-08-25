
# Define server logic
#
# CONVERTED TO A FIRST-TOUCH SINGLETON (Stage 5, first module). Two things follow
# from that and they are the whole shape of this file:
#
#   * `shape`, `currentLang`, `needHelp` and `DULN_all` are REACTIVES, not plain
#     values. A value captured at construction is frozen for the life of the
#     session, and this module is now built once - so a perimeter frozen on the
#     first visit would still be the one being drawn on after the user went back
#     and changed it.
#   * everything that has to happen per VISIT rather than per session is in the
#     enter() closure at the bottom, which vftGoToStep() calls on every return.
#     The body calls it once itself, so construction and re-entry do the same
#     work by the same code rather than by two copies of it.
#
# The observers no longer destroy themselves. They did that so a REBUILT module's
# handlers would not stack on top of the live ones; there is one instantiation
# now, and self-destruction would mean the step could be confirmed exactly once
# per session and then never left again.
step3_server <- function(id, shape, i18n, currentLang,
                         needHelp = shiny::reactive(FALSE),
                         DULN_all = shiny::reactive(NULL)){

  #count this instantiation. A module server should be created once per
  #session; this app re-calls it from an observeEvent on a trigger, so any
  #count above 1 means a duplicate set of observers and outputs is now live
  #alongside the previous one. See vftModuleInstance() in perf_helpers.R.
  vftModuleInstance("step3")

  #shape is the submitted shapefile, or shape produced by submitted coordinates
  shiny::moduleServer(id, function(input, output, session) {

    r <- shiny::reactiveValues()

    vftDbgCat("TRUE_STEP_4_STARTED")


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

    plotUpdate <- reactiveVal(1)

    #What the basemap was last fetched for. maptiles::get_tiles() is a main-thread
    #download-and-mosaic, so it must run when the perimeter CHANGES and not once
    #per visit - a user going back to step 3 to nudge the slider would otherwise
    #pay for a tile fetch to look at the same picture. Held outside `r` because
    #nothing should take a reactive dependency on "which shape did we cache".
    cache <- new.env(parent = emptyenv())
    cache$shape <- NULL

    # r$DULN <- terra::rast(vftData("maps/attr/allAttrs_COG_final.tif") )
    # r$DULN <- terra::crop(r$DULN, shape_wgs)

    # r$DULN_all <- terra::aggregate(r$DULN$all, fact = 2)
    # # terra::plot(terra::focal(r$DULN_all, w=matrix(1, 5, 5), mean))
    # #Get a single layer of DULN
    #
    # #Blur it to make it a smoother selection
    # r$DULN_all <- terra::focal(r$DULN_all, w=matrix(1, 5, 5), mean)
    # DULN <- terra::project(DULN, "epsg:4326")
    # Debounce the slider so renderPlot only fires 400ms after the user stops dragging
    debouncedSlider <- shiny::debounce(shiny::reactive(input$AOISlider), 400)

    #### FUNCTIONS ####

    output$AOIMap <- shiny::renderPlot({
      vftDbgCat("PLOT-1")

      plotUpdate()

        #the basemap and the perimeter come out of enter() now, through `r`, so
        #this re-renders by itself when the user goes back and changes the area
        shiny::req(r$basemapCropped, r$shapeWgs)
        attr <- DULN_all()
        shiny::req(attr)

        r$x <- as.numeric(debouncedSlider())
        AOIBreaks <- c(145, r$x, 0) #TODO: 11 for now, get maxVal automatically later

        vectShape <- terra::vect(r$shapeWgs)
        vftDbgCat(paste0("basemap: ", r$basemapCropped, "\n") )

        terra::plotRGB(r$basemapCropped, reset = FALSE)
        # cat(file = stderr(), "PLOT1")

        terra::plot(attr, alpha = 0.3, col = c("white", "red3"),
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
    langChangeObs <- observeEvent(input$languageSelect_3, {
      vftDbg("CHANGE LANGUAGE")
      if(input$languageSelect_3 == "de"){
        # i18n$set_translation_language('de')
        shiny.i18n::update_lang("de")
        i18n()$set_translation_language("de")
        vftDbg("DE")
        vftSetBanner(id, "www/step3_wsl.png")



      }else if(input$languageSelect_3 == "fr"){
        # i18n$set_translation_language('fr')
        shiny.i18n::update_lang("fr")
        i18n()$set_translation_language("fr")

        vftSetBanner(id, "www/step3_wsl_fr.png")


        vftDbg("FR")
      }else if(input$languageSelect_3 == "en"){
        # i18n$set_translation_language('en')
        shiny.i18n::update_lang("en")
        vftSetBanner(id, "www/step3_wsl.png")


        vftDbg("EN")
      }else if(input$languageSelect_3 == "it"){
        # i18n$set_translation_language('it')
        shiny.i18n::update_lang("it")
        vftSetBanner(id, "www/step3_wsl.png")


        vftDbg("IT")
      }

    }, ignoreInit = TRUE)

    #NOTE ON THE MISSING $destroy() CALLS AND THE MISSING return()s.
    #
    #Each of these three observers used to destroy itself and its siblings, and
    #then return a handle. Neither did what it looks like: the return value of an
    #observeEvent HANDLER goes nowhere - the module's handle is the one built at
    #the bottom of this function - and the self-destruction existed only because
    #a re-entered step used to build a SECOND set of these observers on top of
    #the live ones. There is one set now, for the life of the session, so
    #destroying it would mean step 3 could be confirmed once and then never left
    #again. The guard against a double confirm is the disable() below, which
    #enter() undoes on the way back in.
    obsBanner <- observeEvent(input$banner,  {
      vftDbg("MAPPED IMAGE CLICKED")
      #determine where to go back in history
      r$confirm <- input$banner
    }, ignoreInit = TRUE)

    obsConfirm <- shiny::observeEvent(input$confirmButton3, {
      #app_server flushes this input back to 0 on the way out (a runjs()
      #onInputChange), and that write is itself a change, so this observer fires
      #a second time with 0. It used to be invisible because the observer had
      #already destroyed itself by then.
      if(is.null(input$confirmButton3) || input$confirmButton3 == 0)
        return(invisible(NULL))

      #disable buttons temporarily
      shinyjs::disable("confirmButton3")
      shinyjs::disable("skipButton")

      r$confirm <- input$confirmButton3


      # if(input$naturalAreasCheck == FALSE){
      #   r$naturalAreas <- NULL
      # }

      #if the threshold is above max value of DULN_all
      #(There is no AoI)
      attr <- DULN_all()
      if(!is.null(attr) && !is.null(r$x) && r$x > terra::minmax(attr)[2]){
        #return imitating the skip button being pressed (skip = 1)
        r$isSkip <- 1
      }
    }, ignoreInit = TRUE)

    obsSkip <- shiny::observeEvent(input$skipButton, {
      if(is.null(input$skipButton) || input$skipButton == 0)
        return(invisible(NULL))

      shinyjs::disable("confirmButton3")
      shinyjs::disable("skipButton")

      r$confirm <- input$confirmButton3

      r$isSkip <- 1
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
    #       shinyjs::disable("confirmButton3")
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
    #           natAreas <- sf::st_read(vftData("maps/naturalAreas/naturalAreas.shp"),
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
    #         shinyjs::enable(id = "confirmButton3")
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

    #### enter(): everything that happens per VISIT rather than per session ####
    #
    # Called by vftGoToStep() on every return to this step, and once here at the
    # end of construction so the first visit and the fifth run the same code.
    #
    # This is the refactor the whole singleton depends on. Before it, "what has to
    # be true when the user arrives" was simply the module body, so the only way
    # to arrive was to run the body again - which is what built a second set of
    # observers every time.
    # vftModuleEnterFn() supplies the two properties this body must have and
    # neither of which is visible in it: the module's own session as the default
    # reactive domain (or every shinyjs:: and update*Input() call below silently
    # addresses an unnamespaced control that does not exist), and isolate() around
    # the whole body (or the observers enter() is called from - including the
    # provider observe(), which is not isolated - take a dependency on values
    # enter() itself assigns). See R/modules.R.
    enter <- vftModuleEnterFn(session, function(){
      lang <- currentLang()
      if(is.null(lang)) lang <- "de"

      #banner
      if(lang == "de"){
        vftSetBanner(id, "www/step3_wsl.png")
      }else if(lang == "fr"){
        vftSetBanner(id, "www/step3_wsl_fr.png")
      }else if(lang == "en"){
        vftSetBanner(id, "www/step3_wsl_en.png")
      }

      #language bar
      shiny.i18n::update_lang(lang)
      shiny::updateSelectInput(inputId = "languageSelect_3", selected = lang)
      r$currentLang <- lang
      r$needHelp    <- needHelp()

      #the buttons obsConfirm/obsSkip disabled on the way out
      shinyjs::enable("confirmButton3")
      shinyjs::enable("skipButton")

      #this step's own answer, discarded so a return starts from a clean slate
      #rather than from the threshold that is about to be replaced
      r$x       <- NULL
      r$confirm <- NULL
      r$isSkip  <- 0

      #The perimeter, and the basemap under it. Re-fetched only when the shape
      #has actually changed: get_tiles() is a main-thread download, and coming
      #back to move the slider must not pay for it. terra objects do not survive
      #identical() reliably, but an sf perimeter does - and it is the same R
      #object unless step 1 was re-confirmed.
      shp <- shape()
      if(!is.null(shp) && !identical(cache$shape, shp)){
        basemap  <- maptiles::get_tiles(shp, provider = "OpenStreetMap",
                                        cachedir = vft_tileCacheDir)
        basemap  <- terra::project(basemap, "epsg:4326")

        shapeWgs <- sf::st_transform(sf::st_transform(shp, 3857), 4326)
        vectExt  <- as.vector(terra::ext(terra::buffer(terra::vect(shapeWgs), 100)))

        cache$shape      <- shp
        r$shapeWgs       <- shapeWgs
        r$basemapCropped <- terra::crop(basemap, vectExt)
      }

      #the slider may not have moved, so nudge the plot explicitly
      plotUpdate(plotUpdate() + 1L)
      invisible(NULL)
    })

    enter()

    return(list(minThresh = shiny::reactive(r$x), confirm = shiny::reactive({r$confirm }), skip = shiny::reactive({input$skipButton}), isSkip = shiny::reactive({r$isSkip}),
                needHelp = shiny::reactive(r$needHelp),
                currentLang = shiny::reactive(i18n()$get_translation_language()),
                enter = enter) )
  })
}
