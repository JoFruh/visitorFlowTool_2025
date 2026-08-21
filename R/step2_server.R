
# Define server logic
#' @importFrom promises %...>%
#' @importFrom promises %...!%
#'
# df_spInfo_old = NULL,
step2_server <- function(id, fshape, confirm, i18n, currentLang, needHelp = TRUE,
                         filterList = NULL, checkboxSave = NULL){

  #count this instantiation. A module server should be created once per
  #session; this app re-calls it from an observeEvent on a trigger, so any
  #count above 1 means a duplicate set of observers and outputs is now live
  #alongside the previous one. See vftModuleInstance() in perf_helpers.R.
  vftModuleInstance("step2")

  #shape is the submitted shapefile, or shape produced by submitted coordinates
  shiny::moduleServer(id, function(input, output, session) {


    #render banner image from start
    if(currentLang == "de"){
      output$bannerUI_2 <- shiny::renderUI({
        imgMap <- imageMap(NS(id, "banner"), "www/step2_wsl.png", list() )
        #replace /" with ', to avoid problems
        return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
      })
    }else if(currentLang == "fr"){
      output$bannerUI_2 <- shiny::renderUI({
        imgMap <- imageMap(NS(id, "banner"), "www/step2_wsl_fr.png", list() )
        #replace /" with ', to avoid problems
        return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
      })
    }else if(currentLang == "en"){
      output$bannerUI_2 <- shiny::renderUI({
        imgMap <- imageMap(NS(id, "banner"), "www/step2_wsl_en.png", list() )
        #replace /" with ', to avoid problems
        return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
      })
    }



    #prepare promises and futures
    # future::plan(future::multicore, workers = 1)

    r <- shiny::reactiveValues()

    r$df_spInfo <- NULL #df_spInfo_old
    r$needHelp <- needHelp
    r$filterList <- filterList
    r$checkboxSave <- checkboxSave
    r$currentLang <- currentLang

    shiny.i18n::update_lang(r$currentLang)
    shiny::updateSelectInput(inputId = "languageSelect_2", selected = currentLang)


    #no need to save next to reactive values
    r$spChc <- NULL
    r$sdmLayers <- NULL
    r$keptSpecies <- NULL

    # r$sdmLayers

    #INITIALIZE VARIABLES ####
    SMUpdate <- shiny::reactiveVal(0)
    triggerUpdate <- shiny::reactiveVal()
    SM <- NULL
    r$SMcolors <- NULL
    r$SM_pres <- NULL
    SM_noPres <- NULL
    toSelectSpAfter <- FALSE
    shp <- sf::st_transform(fshape, 2056)
    shp_otherWGS <- sf::st_transform(fshape, 3395)
    shp_WGS84 <- sf::st_transform(fshape, 4326)
    shapeBB <- sf::st_bbox(shp)

    obsWeights <- NULL
    basemap <- maptiles::get_tiles(shp_WGS84, provider = "OpenStreetMap", cachedir = vft_tileCacheDir)
    basemap <- terra::crop(basemap, shp_WGS84)
    basemap <- terra::subset(basemap, 1)
    basemapWhite <- basemap
    basemapWhite[basemapWhite < 255] <- 255
    #convert basemap to have white as transparency
    alphaMap <- terra::app(basemap, function(x) (abs(x - 255)) / 255 )

    # basemap2 <- c(subset(basemap, 1:3), alphaMap)

    # names(basemap2) <- c("lyr.1", "lyr.2", "lyr.3", "lyr.4")

    # RGB(basemap2) <- 1:4

    r3 <- shiny::reactiveValues()
    r3$confirm <- 0


    #switches to turn on and off effects of checkboxes (allows to remove them without consequences)
    r3$ignoreGroupCheckboxEffect <- FALSE
    r3$ignoreAllCheckboxEffect <- FALSE
    r3$ignoreCheckboxEffect <- FALSE
    r3$ignoreNextUpdate <- FALSE
    r3$trigger_speciesRemoval <- 0
    # r3$trigger_speciesUpdate <- 0
    r3$trigger_applySavedGroupCheckboxes <- 0
    r3$trigger_reset <- 0
    r3$triggerSpChckUpdate <- 0

    r3$grpChkClass <- character(0)
    r3$grpChkType <- character(0)
    r3$grpChkSens <- character(0)

    r3$weightInputs <- NULL
    r3$weightNames <- NULL


    #INTERNAL FUNCTIONS ####

    buildHTMLList <- function(speciesList, speciesData, language){

      
      if(length(speciesList > 0)){

      # only go through species list if some species are available
      spChoice_names <- c()


      #cycle through all species
      for(sp in 1:length(speciesList)){

        # if(!speciesList[sp] %in% c("Castor fiber", "Corvus monedula") ){
          vftDbg(speciesList[sp])
          latinN <- speciesList[[sp]]


          #only 1 link for now (not language dependent)
          link <- speciesData[speciesData$latinN == latinN, "link"]

          #variable germanN used as contextual language vulgar name
          if(language == "de"){
            germanN <- speciesData[speciesData$latinN == latinN, "germanN"]
          }else if(language == "fr"){
            germanN <- speciesData[speciesData$latinN == latinN, "frenchN"]
          }else if(language == "en"){
            germanN <- speciesData[speciesData$latinN == latinN, "englishN"]
          }

          # make sure link ignored without an error if not available
          if (!is.null(link)) {
            if (length(link) > 0 ) {
              if(!is.na(link) & link != ""){
              textTag <- paste0("<p style = 'display: table-cell'><em>", shiny::a(paste0(latinN), href = link, target = "_blank"), "</em>")
            } else {
              textTag <- paste0("<p style = 'display: table-cell'><em>", latinN, "</em>")
            }
            }else{
              textTag <- paste0("<p style = 'display: table-cell'><em>", latinN, "</em>")
            }
          } else {
            textTag <- paste0("<p style = 'display: table-cell'><em>", latinN, "</em>")
          }


          #add vernacular name if present
          if (!is.null(link)) {
            if (length(link) > 0) {
              if (germanN != "" & !is.na(germanN)) {
                textTag <- paste0(textTag, "<br/>", germanN, "</p>")
              } else {
                textTag <- paste0(textTag, "</p>")
              }
            } else {
              textTag <- paste0(textTag, "</p>")
            }
          } else {
            textTag <- paste0(textTag, "</p>")
          }

          #columns 7:11 could also give images (priority, umbrella etc.)
          imageTags <- ""
          for(imageCol in c("responsibility", "threat", "emerald", "ch.priority")){
            img <- speciesData[speciesData$latinN == latinN, imageCol]

            #if an image is required
            if (!is.null(img)) {
              if (length(img) > 0) {
                if (!is.na(img) & img != "") {
                  # interpret international priority
                  if (imageCol == "responsibility") {
                    if (img %in% c(1, 2)) {
                      # very high / unique responsibility
                      imageTag <- paste0("<img src = 'www/HIR.png' style = 'display: inline-block;float: right; vertical-align: bottom; width: 35px; margin: 0 0 0 5px'>")
                    } else if (img %in% c(3)) {
                      # high responsibility
                      imageTag <- paste0("<img src = 'www/MIR.png' style = 'display: inline-block ;float: right; vertical-align: bottom; width: 35px; margin: 0 0 0 5px'>")
                    } else {
                      # if no data or 0
                      imageTag <- NULL
                    }
                  } else if (imageCol == "ch.priority") {
                    if (img %in% 1:4) {
                      imageTag <- paste0("<img src = 'www/", paste0("PR", img), ".png' style = 'display: inline-block ;float: right; vertical-align: bottom; width: 35px; margin: 0 0 0 5px'>")
                    } else {
                      imageTag <- NULL
                    }
                  } else {
                    # filename defined by img
                    imageTag <- paste0("<img src = 'www/", img, ".png' style = 'display: inline-block;float: right; vertical-align: bottom; width: 35px; margin: 0 0 0 5px'>")
                  }

                  imageTags <- paste0(imageTags, imageTag)
                }
              }
            }

            weightTag <- paste0("<div style = 'display: inline-block;float: right; vertical-align: bottom; width: 60px; margin: 0 0 0 5px' class='form-group shiny-input-container'><label class='control-label shiny-label-null' for='step2-weight_", sp , "' id= 'step2-weight_", sp ,"-label'>Weight</label><input id='step2-weight_", sp ,"' type='number' class='form-control' value='1' min = '1' max = '5'/></div>")

            finalTag <- paste0(textTag, weightTag, imageTags)

          }


          spChoice_names <- c(spChoice_names, finalTag)
        }
      # }

      spChoices <- sprintf("c%d",1:length(spChoice_names))
      names(spChoices) <- spChoice_names

      }else{
# If no species are available, create a dummy choice to avoid errors in the UI
  spChoices <- NULL
}

      return(spChoices)
    }

    # Cache species table — same file for every user, load only once per R session
    if (!exists(".vft_speciesData", envir = .GlobalEnv)) {
      .GlobalEnv$.vft_speciesData <- utils::read.csv2(vftData("tables/speciesInformation_SDMapsCH.csv"))
    }
    speciesData <- .GlobalEnv$.vft_speciesData



    # OLD METHOD ###
    ################
    #
    # #LOAD EVERYTHING
    # #sdmLayers and df_spInfo
    # shiny::observeEvent(NULL, {
    #   cat(file = stderr(), paste0("PROMISE ABOUT TO START" ) )
    #
    #   df_spInfo <- r$df_spInfo
    #
    #   progress <- ipc::AsyncProgress$new(message = i18n()$t(":aufbereitung:"),
    #                                      detail = paste0(i18n()$t("Dies sollte weniger als "), 30, i18n()$t(" Sekunden dauern")),
    #                                      queue = ipc::shinyQueue(),
    #                                      millis = 1000)
    #   future::future({
    #
    #
    #     cat(file = stderr(), paste0("PROMISE STARTED" ) )
    #
    #
    # #LOAD DATA (IF NOT ALREADY EXISTING)
    # #PREPARE SPECIES DATA AND MAPS####
    #
    # if( is.null(df_spInfo_old) ){
    #
    # # ABMprogress <- shiny::Progress$new()
    # # Make sure it closes when we exit this reactive, even if there's an error
    # # on.exit(ABMprogress$close())
    # progress$set(value = 0 )
    #
    #   #prepare data to keep information
    #   df_spInfo <- dplyr::tibble(species = character(), sdm = character(), sdm_cover = numeric() ) #, sdm_pres = list(), sdm_pres_cover = numeric()
    #
    #   #Prepare Raster Baseline (Need to remove first layer afterwards)
    #   sdmLayer <- terra::rast()
    #   sdmLayers_pres <- terra::rast()
    #   sdmLayers_noPres <- terra::rast()
    #
    #   #SWITCH TO USE OLD SDMs (more) or current SDMs
    #   # terra::add(sdmLayers) <- terra::rast(vftData("maps/species/SDM/proj_currentEM_Aira.caryophyllea_ensemble.tif"))
    #
    #   #OLDER WAY OF DOING IT (NOT COG)
    #   # terra::add(sdmLayers) <- terra::rast(vftData("maps/species/SDM/lowRes/Aphanes.australis_sdmLR.tif"))
    #
    #   #load COG
    #
    #   sdmLayer <- terra::rast(vftData("maps/species_new/SDM/allSDMs_binary_COG.tif"))
    #
    #     #TEMPORARY (remove lissotriton vulgaris double)
    #     sdmLayer <- sdmLayer[[-116]]
    #
    #     #SWITCH (activate) when using old or new SDMs
    #     # terra::crs(sdmLayers) <- "epsg:3395"
    #
    #     #SWITCH between old and recent SDMs
    #     # sdmLayers <- terra::crop(sdmLayers, terra::vect(shp_otherWGS), mask = TRUE)
    #     sdmLayer <- terra::crop(sdmLayer, terra::vect(shp_WGS84), mask = TRUE)
    #
    #     progress$set(value = 1/4)
    #
    #     # sdmLayers <-terra::project(sdmLayers, "epsg:4326")
    #     # layerNames <- c()
    #     # layerNames_pres <- c()
    #     # layerNames_noPres <- c()
    #     #
    #     # sdmLayers_pres <- terra::deepcopy( sdmLayers)
    #     # sdmLayers_noPres <- terra::deepcopy(sdmLayers)
    #     #
    #     # #create empty layer to add when maps is empty
    #     # empty_layer <- terra::rast(terra::ext(sdmLayers), resolution = terra::res(sdmLayers))
    #     # empty_layer <- terra::subst(empty_layer, NaN, 0)
    #
    #     #remove all empty ones
    #     sdmLayer <- sdmLayer[[terra::minmax(sdmLayer)[2,] != 0]]
    #
    #
    #
    #
    #   #load all sdm presences
    #   sdmPres <- sfarrow::read_sf_dataset( arrow::open_dataset(vftData("maps/species_new/presence/sfarrow/")))
    #   sdmPres <- sf::st_transform(sdmPres, "EPSG:4326")
    #   sdmPres <- sf::st_crop(sdmPres, shp_WGS84)
    #
    #   progress$set(value = 2/4)
    #
    #   #load all species SDM and presence files
    #   spNb <- 0
    #   for(sp in names(sdmLayer)){
    #
    #     # # if(sp == "Turdus torquatus"){browser()}
    #     #
    #     spNb <- spNb + 1
    #     progress$set( (value = 2/4) + ((spNb/length(names(sdmLayer)))/2 ) )
    #
    #     #only save sp reference (name)
    #     #use that to reference sdmLayer later (this way only sdmLayer needs to be wrapped)
    #     # sdm <- sdmLayer[[sp]]
    #     sdm <- sp
    #
    #     sdm_cover <- sum(terra::values(sdmLayer[[sp]], na.rm = TRUE))
    #
    #
    #     sdm_cover <- sum(terra::values(sdmLayer[[sp]], na.rm = TRUE))
    #
    #     # USING PRESENCE POLYGONS ####
    #
    #     # PRESENCE POLYGONS
    #     # This removes presences?
    #     #sdmPres
    #
    #     if(!is.null(sdmLayer[[sp]])){
    #
    #       sp_ <- sdmPres[[gsub("[.]", "_", sp)]]
    #       pres <- sdmPres[sdmPres$species == sp_]
    #
    #       if(length(sf::st_intersects(shp_WGS84, pres)[[1]]) > 0 |
    #          length(sf::st_contains(shp_WGS84, pres)[[1]]) > 0){
    #         #get intersection of presence and sdm (crop)
    #
    #         sdm_pres <- terra::mask(sdmLayer[[sp]], pres[1]) #only mask, extents remain same, NAs inserted
    #         sdm_noPres <- terra::mask(sdmLayer[[sp]], pres[1], inverse = TRUE) #only mask, extents remain same, NAs inserted
    #
    #         #ext(sdm_pres) <- ext(vect(shp_WGS84))
    #         #replace all NAs with 0
    #         sdm_pres[is.na(sdm_pres)] <- 0
    #         sdm_noPres[is.na(sdm_noPres)] <- 0
    #
    #         sdm_pres_cover <- sum(terra::values(sdm_pres, na.rm = TRUE))
    #
    #         #check resolution
    #
    #         #save sdm as layer
    #         #INSTEAD: replace existing sdmLayers
    #         if(!is.null(sdm_pres)){
    #           # terra::add(sdmLayers_pres) <- sdm_pres
    #           sdmLayer[[sp]] <- sdm_pres
    #         }else{
    #           # terra::add(sdmLayers_pres) <- empty_layer
    #           sdmLayer <- sdmLayer[[-sp]]
    #         }
    #
    #       }else{
    #         #if shape and presence polygons don't overlap or intersect
    #
    #         # terra::add(sdmLayers_noPres) <- sdm
    #         # terra::add(sdmLayers_pres) <- empty_layer
    #         sdmLayer <- sdmLayer[[-sp]]
    #
    #
    #         # layerNames_noPres <- c(layerNames_noPres, gsub(" ", "_", speciesData[sp, "latinN"]) )
    #         # layerNames_pres <- c(layerNames_pres, gsub(" ", "_", speciesData[sp, "latinN"]) )
    #
    #         sdm_pres_cover <- 0
    #         sdm_pres <- NULL
    #       }
    #
    #     }else{
    #       #if file doesn't exist
    #       print("ERROR: file does not exist")
    #
    #       sdmLayer <- sdmLayer[[-sp]]
    #
    #       sdm_pres <- NULL
    #       sdm_pres_cover <- 0
    #     }
    #
    #     #save information
    #
    #     df_spInfo <- dplyr::add_row(df_spInfo,
    #
    #                                 species = gsub("[.]", " ", sp),
    #                                 sdm = sdm,
    #                                 sdm_cover = sdm_cover#, sdm_pres_cover = sdm_pres_cover
    #     )
    #
    #   }
    #   progress$set(value = 4/4 )
    #
    #   #transform to df due to subsetting issues
    #   df_spInfo <- as.data.frame(df_spInfo)
    #
    #   #filter out species with no presence
    #   df_spInfo <- df_spInfo[df_spInfo$sdm_cover > 0,]
    #
    # }else{
    #
    #   #reload saved data ####
    #   print("RELOAD SAVED DATA")
    #   df_spInfo <- df_spInfo_old
    #
    # }
    #     allResults <- list(df_spInfo, terra::wrap(sdmLayer))
    #
    #     progress$close()
    #
    #     allResults
    #     }, seed = TRUE) %...>% (function(allResults){
    #
    #       cat(file = stderr(), paste0("PROMISE COMPLETED" ) )
    #
    #       # cat(file = stderr(), paste0("ALLRESULTS = ", allResults ) )
    #       r$df_spInfo <- allResults[[1]]
    #       r$sdmLayer <- terra::unwrap(allResults[[2]])
    #
    #       cat(file = stderr(), paste0("names(r$sdmLayer) = ", names(r$sdmLayer) ) )
    #
    #   #sort based on cover
    #   #order sdm_presence and order sdm, then concatenate them. (sdm_pres first, then sdm)
    #   spOrder <-order(r$df_spInfo$sdm_cover, decreasing = TRUE)
    #
    #   r$spChc <- r$df_spInfo[spOrder, "species"]
    #
    #   #create a table with ordered and subset species for current area
    #   r3$thisSpeciesData <- speciesData[match(r$spChc, speciesData$latinN),]
    #
    #   r3$speciesOrder <- r$spChc
    #
    #
    #   #try to update checkboxes
    #   #default: VU-CR red list with all species selected
    #   shinyjs::delay(600, shiny::updateSelectInput(inputId = "filterList", selected = "s8") )
    #   shinyjs::delay(1500, shiny::updateCheckboxInput(inputId = "groupCheckbox_all", value = TRUE) )
    #
    # })
    #
    # }, once = TRUE, ignoreInit = FALSE, ignoreNULL = FALSE, priority = 12)

### END OF OLD METHOD #####

    # NEW METHOD ##

    shiny::observeEvent(NULL, {

        vftDbgCat(paste0("PROMISE ABOUT TO START" ) )

        df_spInfo <- r$df_spInfo

        #vftProgress, not ipc::AsyncProgress: 78 MB of session state was crossing
        #into the worker from this site. See R/async_helpers.R.
        progress <- vftProgress(message = i18n()$t(":aufbereitung:"),
                                detail = paste0(i18n()$t("Dies sollte weniger als "), 30, i18n()$t(" Sekunden dauern")),
                                queue = ipc::shinyQueue(),
                                millis = 1000)

      vftFuture({

        # if(is.null(df_spInfo_old)){

        # ── Load COG and crop once ──────────────────────────────────────────────────
        #200species
        # sdmLayer <- terra::rast(vftData("maps/species_new/SDM/allSDMs_binary_COG.tif"))
        #600+ species (SDMaps_CH)
        sdmLayer <- terra::rast(vftData("maps/species_new/SDM/SDMapsCH_100m_binary_4326_COG.tif"))
        # sdmLayer <- sdmLayer[[-116]]                               # remove duplicate
        sdmLayer <- terra::crop(sdmLayer, terra::vect(shp_WGS84), mask = TRUE)

        progress$set(value = 1/4)

        # ── Drop empty layers in one vectorised call ─────────────────────────────────
        # minmax is already computed over all layers at once — no loop needed
        non_empty <- terra::minmax(sdmLayer)[2, ] != 0
        sdmLayer  <- sdmLayer[[non_empty]]

        progress$set(value = 2/4)

        # ── Compute per-layer sums in one vectorised call ────────────────────────────
        # global() is the idiomatic, fastest way to summarise all layers at once
        cover_vals <- terra::global(sdmLayer, fun = "sum", na.rm = TRUE)$sum
        names(cover_vals) <- names(sdmLayer)

        progress$set(value = 3/4)

        # ── Build df_spInfo without a loop ───────────────────────────────────────────
        df_spInfo <- data.frame(
          species   = gsub("[.]", " ", names(sdmLayer)),
          sdm       = names(sdmLayer),
          sdm_cover = cover_vals,
          row.names = NULL,
          stringsAsFactors = FALSE
        )

        # Drop species with zero modelled cover
        df_spInfo <- df_spInfo[df_spInfo$sdm_cover > 0, ]

        # Drop corresponding layers
        sdmLayer <- sdmLayer[[df_spInfo$sdm]]

        progress$set(value = 4/4)
        progress$close()

        list(df_spInfo, terra::wrap(sdmLayer))

      }, seed = TRUE) %...>% (function(allResults){

        vftDbgCat(paste0("PROMISE COMPLETED" ) )

              # cat(file = stderr(), paste0("ALLRESULTS = ", allResults ) )
              r$df_spInfo <- allResults[[1]]
              r$sdmLayer <- terra::unwrap(allResults[[2]])

              vftDbgCat(paste0("names(r$sdmLayer) = ", names(r$sdmLayer) ) )

          #sort based on cover
          #order sdm_presence and order sdm, then concatenate them. (sdm_pres first, then sdm)
          spOrder <-order(r$df_spInfo$sdm_cover, decreasing = TRUE)

          r$spChc <- r$df_spInfo[spOrder, "species"]

          #create a table with ordered and subset species for current area
          r3$thisSpeciesData <- speciesData[match(r$spChc, speciesData$latinN),]

          r3$speciesOrder <- r$spChc


          #try to update checkboxes
          #default: VU-CR red list with all species selected
          shinyjs::delay(600, shiny::updateSelectInput(inputId = "filterList", selected = "s8") )
          shinyjs::delay(1500, shiny::updateCheckboxInput(inputId = "groupCheckbox_all", value = TRUE) )


      })

    }, once = TRUE, ignoreInit = FALSE, ignoreNULL = FALSE, priority = 12)

    ## END OF NEW METHOD ###







      obsSpChoice <- shiny::observeEvent(r3$spChoices, {

        r3$spChoices_html <- buildHTMLList(r3$spChoices, speciesData, language = r$currentLang)

vftDbg("SPCHOICES EVENT")
        #LINKING GROUP AND SPECIES SELECTION ####
        #include sp in groupLinks
        #create groupChoices list which determines group checkboxes
        r3$groupChoices_class <- c()
        r3$groupChoices_sens <- c()
        r3$groupChoices_threat <- c()
        r3$groupChoices_priority <- c()
        r3$groupChoices_type <- c()


        #create group links which associates groupChoices with multiple species
        r3$groupLinks_class <- list()
        r3$groupLinks_sens <- list()
        r3$groupLinks_threat <- list()
        r3$groupLinks_priority <- list()
        r3$groupLinks_type <- list()
vftDbg("SPCHOICES")

vftDbgCat(paste0("r3$spChoices = ", r3$spChoices ) )

        if(length(r3$spChoices) > 0){

          #cycle through column of grouping characteristics
          for(col in c("group_de", "Aquatisch",      "Fliegen",        "Boden",          "Geraeusche",     "Naechtliche")){
            #for each, save grouping variable and associate to all species in spChoices with X
            groupCol_allSp <- r3$thisSpeciesData[, col]
            #use reordered and subset table (r3$thisSpeciesData)
            #
            groupCol <- groupCol_allSp[r3$thisSpeciesData$latinN %in% r3$spChoices]
            groupN <- names(r3$thisSpeciesData[col])

            #if not entirely made of either "" or NA
            if(!sum(groupCol %in% c("")) == length(groupCol) &
               !sum(groupCol %in% c(NA)) == length(groupCol)){

              #determine type of grouping and evaluate based on type (ex: activity = diurnal, nocturnal; group = Amphibia, mammal etc.; flying = X)
              if(groupN == "group_de"){

                #cycle through group types (mammal, bird etc..)
                for(groupType in unique(groupCol)){

                  groupSpecies <- NULL
                  groupSpecies <- r3$spChoices[groupCol == groupType]

                  r3$groupChoices_class <- c(r3$groupChoices_class, i18n()$t(groupType) )

                  #make groupLinks
                  #determine c1, c2 etc.. base on groupSpecies
                  groupNums <- which(r3$spChoices %in% groupSpecies)

                  groupNums <- sprintf("c%d", groupNums)

                  r3$groupLinks_class[[i18n()$t(groupType)]] <- groupNums

                }


              }else if(groupN %in% c("Fliegen", "Aquatisch",  "Naechtliche" )){

                groupSpecies <- NULL
                groupSpecies <- r3$spChoices[groupCol == "X"]


                #include in group choices
                r3$groupChoices_type <- c(r3$groupChoices_type, i18n()$t(groupN) )

                #make groupLinks
                #determine c1, c2 etc.. base on groupSpecies
                groupNums <- which(r3$spChoices %in% groupSpecies )

                groupNums <- sprintf("c%d", groupNums)
                r3$groupLinks_type[[i18n()$t(groupN)]] <- groupNums

              }else if(groupN %in% c("Boden", "Geraeusche")){

                groupSpecies <- NULL
                groupSpecies <- r3$spChoices[groupCol == "X"]

                #include in group choices
                r3$groupChoices_sens <- c(r3$groupChoices_sens, i18n()$t(groupN))

                #make groupLinks
                #determine c1, c2 etc.. base on groupSpecies
                groupNums <- which(r3$spChoices %in% groupSpecies )

                groupNums <- sprintf("c%d", groupNums)
                r3$groupLinks_sens[[i18n()$t(groupN)]] <- groupNums


                # TRANSLATE TO FRENCH IF FRENCH
                if(r$currentLang == "fr"){

                }


              }
            }
          }
        }else{
          #if spChoices is empty
          vftDbg("ERROR: No Potential Species Detected")
        }

      }, ignoreInit = FALSE)

      vftDbgCat(paste0("Species Selection Render" ) )

      #RENDER LEGEND ####
      output$legend_ui <- shiny::renderUI({
        shiny::h5(shiny::HTML(as.character(i18n()$t(":legend:"))))
      })

      # RENDER FILTER SELECT ####
      output$filterList_ui <- shiny::renderUI({
        shiny::selectInput(shiny::NS(id, "filterList"), choices =  stats::setNames(c("s1","s2", "s3", "s4", "s5","s6","s7", "s8", "s9"), c(i18n()$t("All") ,i18n()$t("CH Priorität 1") , i18n()$t("CH Priorität 1-2") , i18n()$t("CH Priorität 1-3") , i18n()$t("CH Priorität 1-4") ,
                                                                                                                                           i18n()$t("Rote Liste CR") , i18n()$t("Rote Liste EN-CR") , i18n()$t("Rote Liste VU-CR"), i18n()$t("Rote Liste NT-CR") ) ),
                           label = NULL,
                           width = "70%",
                           selected = "s9")
      })



    #SPECIES SELECTION RENDER####
    output$speciesCheckbox <- shiny::renderUI({

      #TODO: Check for empty list of species due to filter

      shiny::tagList(
        shiny::checkboxGroupInput(
          inputId = shiny::NS(id, "speciesCheckbox"),
          label = "",
          choices = r3$spChoices_html
        ),
        shiny::tags$script(
          "
          $('#step2-speciesCheckbox .checkbox label span').map(function(choice){
              this.innerHTML = $(this).text();

          });
          "
        )
      )
    })

    # output$speciesWeights <- renderUI({
    #   weightList <- list()
    #   id_nb = 0
    #   for(spRow in spChoices){
    #     id_nb = id_nb + 1
    #     weightList <- list(weightList,
    #
    #         numericInput(
    #           inputId = NS(id, paste0("speciesWeight_", id_nb) ), value = 1, min = 1, max = 5, label = NULL
    #         )
    #     )
    #   }
    #
    #   weightList
    #
    # })
      vftDbgCat(paste0("Group Selection Render" ) )

    #GROUP SELECTION RENDERS ####
      output$groupCheckbox_sens <- shiny::renderUI({
        shiny::checkboxGroupInput(
          inputId = shiny::NS(id, "groupCheckbox_sens"),
          label = i18n()$t("Empfindlichkeiten"),
          choices = r3$groupChoices_sens,
          selected = NULL
        )
      })
      output$groupCheckbox_type <- shiny::renderUI({
        shiny::checkboxGroupInput(
          inputId = shiny::NS(id, "groupCheckbox_type"),
          label = i18n()$t("Merkmale"),
          choices = r3$groupChoices_type,
          selected = NULL
        )
      })
    output$groupCheckbox_class <- shiny::renderUI({
      shiny::checkboxGroupInput(
        inputId = shiny::NS(id, "groupCheckbox_class"),
        label = i18n()$t("Klassifizierung"),
        choices = r3$groupChoices_class,
        selected = NULL
      )
    })

    vftDbgCat(paste0("Group Selection Mechanics" ) )

    #GROUP SELECTION MECHANICS####
    #for classification
    checkBoxGroupTriggers <- shiny::reactive({list(input$groupCheckbox_class,
                                            input$groupCheckbox_type,
                                            input$groupCheckbox_sens )})

    groupCheckboxHistory <- NULL
    removedCheck <- NULL

    #global variable to avoid cyclical updates
    ignoreNextUpdate <- FALSE

    # OBSERVERS ####
    #dismiss Modal
    obs_dimissModal <- shiny::observeEvent(input$dismissModal, {
      shiny::removeModal()
    })

    # observe help ####
    obs_help2 <- shiny::observeEvent(input$helpButton2, {
      shiny::showModal(
        shiny::modalDialog( footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                            h2(i18n()$t("Erstellen Sie eine Sensitivitätsmatrix.")),
                            shiny::img(src = "www/arrowLeft.png", style = "float:left;height:50px;margin-left:-70px"),h3(shiny::HTML(as.character( i18n()$t("Auf der <b>linken</b> Seite können Sie:")) ) ),
                            h4(shiny::HTML(as.character( i18n()$t("1) <b>Informationen</b> über die in diesem Gebiet vorkommenden Arten anzeigen, <br>2) <b>filtern</b>, um nur die Arten mit einer bestimmten Prioritätsstufe oder einem bestimmten Status auf der Roten Liste anzuzeigen, <br>3) die Arten unterschiedlich gewichten (Wichtigkeit), <br>4) Arten <b>einzeln</b> auswählen.")))),
                            h4(),
                            div(style = "white-space: nowrap",
                                h4(shiny::HTML(as.character( i18n()$t("Sie können die Gewichtung jeder<br>Art manuell ändern' <img src='www/weightWindow.png' style = 'display:inline;height:35px;'>."))))
                            ),
                            h4(shiny::HTML(as.character( i18n()$t("Oder automatisch auf der Grundlage der Priorität oder des Status auf der Roten Liste mit den Schaltflächen unten.")))),
                            div(style = "text-align:right",
                                h3(shiny::HTML(as.character( i18n()$t("Auf der <b>rechten</b> Seite können Sie:"))), shiny::img(src = "www/arrowRight.png", style = "float:right;height:50px;margin-right:-70px")),
                                h4(shiny::HTML(as.character( i18n()$t("1) <b>mehrere Arten auf einmal</b> auswählen, gruppiert nach Kategorie, Sensitivität oder Merkmalen.")))),
                            ),
                            h3(),
                            h3(shiny::HTML(as.character( i18n()$t("Wenn Sie mehrere Arten auswählen (mehrere Kästchen ankreuzen), werden die Verbreitungen der Arten kombiniert und angezeigt")))),
                            h3(),
                            h3(shiny::HTML(as.character( i18n()$t("Die endgültige Karte (die '<b>Sensitivitätsmatrix</b>') kann <b>später</b> bei der Simulation der Erholungsnutzung zur <b>Abschätzung</b> der ökologischen Wirkung verwendet werden!"))))
        )
      )
    })

    #observe info Button ####
    obs_info2 <- shiny::observeEvent(input$infoButton2, {
      shiny::showModal(
        shiny::modalDialog(footer = shiny::actionButton(inputId = shiny::NS(id, "dismissModal"), label = i18n()$t("OK!"), style = "background-color:#006268; color:#ffffff"  ),
                           h2(i18n()$t("Zusätzliche Informationen:") ),
                           h3(),
                           h3(i18n()$t("Für 200 gefährdete Arten in der Schweiz werden derzeit Verbreitungskarten berechnet.") ),
                           h3(),
                           h4(i18n()$t("Die Verbreitungsgebiete dieser Arten wurden anhand von InfoSpecies-Daten modelliert, die für die Forschung offen zugänglich sind, aus dem Zeitraum zwischen 1980 und 2022 stammen und auf eine Genauigkeit von mindestens 100 m begrenzt sind.") ),
                           h3(),
                           h4(i18n()$t("Zur Erstellung dieser Verteilungen wurden Random-Forest-Modelle mit „Pseudo-Abständen“ verwendet. Die „Pseudo-Abstände“ wurden nach dem Zufallsprinzip aus allen Beobachtungen der Familie der Art gezogen.") ),
                           h3(),
                           h4(i18n()$t("Obwohl die aktuellen Verbreitungskarten begrenzt sind, ist geplant, verbesserte Verbreitungskarten zahlreicher Arten zu integrieren, sobald diese in naher Zukunft über InfoSpecies verfügbar sind.") )


        )
      )
    })

    #Language Change ####
    langChangeObs <- observeEvent(input$languageSelect_2, {
      vftDbg("CHANGE LANGUAGE")
      vftDbg(input$languageSelect_2)
      if(input$languageSelect_2 == "de"){
        # i18n$set_translation_language('de')
        shiny.i18n::update_lang("de")
        i18n()$set_translation_language("de")
        r$currentLang <- "de"
        vftDbg("DE")
        output$bannerUI_2 <- shiny::renderUI({
          imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step2_wsl.png"), list() )
          #replace /" with ', to avoid problems
          return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
        })


        r3$spChoices_html <- buildHTMLList(r3$spChoices, speciesData, language = "de")

        # #update filter to trigger checkbox update
        # shiny::updateSelectizeInput(inputId = "filterList", selected = r$filterList)

        #SPECIES SELECTION RENDER####
        output$speciesCheckbox <- shiny::renderUI({

          shiny::tagList(

            #TODO: Check for empty list of species due to filter

            shiny::checkboxGroupInput(
              inputId = shiny::NS(id, "speciesCheckbox"),
              label = "",
              choices = r3$spChoices_html
            ),
            shiny::tags$script(
              "
          $('#step2-speciesCheckbox .checkbox label span').map(function(choice){
              this.innerHTML = $(this).text();

          });
          "
            )
          )
        })
#
#         r3$groupChoices_sens[r3$groupChoices_sens == "Sol"] <- "Boden"
#         r3$groupChoices_sens[r3$groupChoices_sens == "Bruit"] <- "Geraeusche"
#
#         r3$groupChoices_type[r3$groupChoices_type == "Aérien"] <- "Fliegen"
#         r3$groupChoices_type[r3$groupChoices_type == "Aquatique"] <- "Aquatisch"
#         r3$groupChoices_type[r3$groupChoices_type == "Nocturne"] <- "Naechtliche"
#
#
#         r3$groupChoices_class[r3$groupChoices_class == "Oiseaux"] <- "Vögel"
#         r3$groupChoices_class[r3$groupChoices_class == "Plantes"] <- "Gefässpflanzen"
#         r3$groupChoices_class[r3$groupChoices_class == "Amphibiens"] <- "Amphibien"
#         r3$groupChoices_class[r3$groupChoices_class == "Abeilles"] <- "Wildbienen"
#         r3$groupChoices_class[r3$groupChoices_class == "Reptiles"] <- "Reptilien"
#         r3$groupChoices_class[r3$groupChoices_class == "Coléoptères"] <- "Käfer"
#         r3$groupChoices_class[r3$groupChoices_class == "Papillons"] <- "Schmetterlinge"
#         r3$groupChoices_class[r3$groupChoices_class == "Ephémères"] <- "Eintagsfliegen"
#         r3$groupChoices_class[r3$groupChoices_class == "Sauterelles"] <- "Heuschrecken"
#         r3$groupChoices_class[r3$groupChoices_class == "Mammifères (chauve-souris exl.)"] <- "Säugetiere (ohne Fledermäuse)"
#         r3$groupChoices_class[r3$groupChoices_class == "Fourmis"] <- "Ameisen"
#         r3$groupChoices_class[r3$groupChoices_class == "Gros Champignons"] <- "Grosspilze"
#         r3$groupChoices_class[r3$groupChoices_class == "Champignons"] <- "Pilze"
#
#         # REVERT TO "ORIGINAL" (GERMAN)
#         #update group choices
#         output$groupCheckbox_sens <- shiny::renderUI({
#           shiny::checkboxGroupInput(
#             inputId = shiny::NS(id, "groupCheckbox_sens"),
#             label = "Empfindlichkeiten",
#             choices = r3$groupChoices_sens,
#             selected = NULL
#           )
#         })
#         output$groupCheckbox_type <- shiny::renderUI({
#           shiny::checkboxGroupInput(
#             inputId = shiny::NS(id, "groupCheckbox_type"),
#             label = "Merkmale",
#             choices = r3$groupChoices_type,
#             selected = NULL
#           )
#         })
#         output$groupCheckbox_class <- shiny::renderUI({
#           shiny::checkboxGroupInput(
#             inputId = shiny::NS(id, "groupCheckbox_class"),
#             label = "Klassifizierung",
#             choices = r3$groupChoices_class,
#             selected = NULL
#           )
#         })






      }else if(input$languageSelect_2 == "fr"){
        # i18n$set_translation_language('fr')
        shiny.i18n::update_lang("fr")
        i18n()$set_translation_language("fr")
        r$currentLang <- "fr"

        output$bannerUI_2 <- shiny::renderUI({
          imgMap <- imageMap(NS(id, "banner"), "www/step2_wsl_fr.png", list() )
          #replace /" with ', to avoid problems
          return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
        })

        # #trigger an update of the species choices checkbox
        # r3$triggerSpChckUpdate <- r3$triggerSpChckUpdate  + 1
        if(!is.null(r3$spChoices)){
          r3$spChoices_html <- buildHTMLList(r3$spChoices, speciesData, language = "fr")
        }

        # #update filter to trigger checkbox update
        # shiny::updateSelectizeInput(inputId = "filterList", selected = r$filterList)

        #SPECIES SELECTION RENDER####
        output$speciesCheckbox <- shiny::renderUI({

          shiny::tagList(

            #TODO: Check for empty list of species due to filter

            shiny::checkboxGroupInput(
              inputId = shiny::NS(id, "speciesCheckbox"),
              label = "",
              choices = r3$spChoices_html
            ),
            shiny::tags$script(
              "
          $('#step2-speciesCheckbox .checkbox label span').map(function(choice){
              this.innerHTML = $(this).text();

          });
          "
            )
          )
        })

#
#
#         #CHANGE TO FRENCH
#
#         r3$groupChoices_sens[r3$groupChoices_sens == "Boden"] <- "Sol"
#         r3$groupChoices_sens[r3$groupChoices_sens == "Geraeusche"] <- "Bruit"
#
#         r3$groupChoices_type[r3$groupChoices_type == "Fliegen"] <- "Aérien"
#         r3$groupChoices_type[r3$groupChoices_type == "Aquatisch"] <- "Aquatique"
#         r3$groupChoices_type[r3$groupChoices_type == "Naechtliche"] <- "Nocturne"
#
#
#         r3$groupChoices_class[r3$groupChoices_class == "Vögel"] <- "Oiseaux"
#         r3$groupChoices_class[r3$groupChoices_class == "Gefässpflanzen"] <- "Plantes"
#         r3$groupChoices_class[r3$groupChoices_class == "Amphibien"] <- "Amphibiens"
#         r3$groupChoices_class[r3$groupChoices_class == "Wildbienen"] <- "Abeilles"
#         r3$groupChoices_class[r3$groupChoices_class == "Reptilien"] <- "Reptiles"
#         r3$groupChoices_class[r3$groupChoices_class == "Käfer"] <- "Coléoptères"
#         r3$groupChoices_class[r3$groupChoices_class == "Schmetterlinge"] <- "Papillons"
#         r3$groupChoices_class[r3$groupChoices_class == "Eintagsfliegen"] <- "Ephémères"
#         r3$groupChoices_class[r3$groupChoices_class == "Heuschrecken"] <- "Sauterelles"
#         r3$groupChoices_class[r3$groupChoices_class == "Säugetiere (ohne Fledermäuse)"] <- "Mammifères (chauve-souris exl.)"
#         r3$groupChoices_class[r3$groupChoices_class == "Ameisen"] <- "Fourmis"
#         r3$groupChoices_class[r3$groupChoices_class == "Grosspilze"] <- "Gros Champignons"
#         r3$groupChoices_class[r3$groupChoices_class == "Pilze"] <- "Champignons"
#
#
#
#         #translate group choices
#         output$groupCheckbox_sens <- shiny::renderUI({
#           shiny::checkboxGroupInput(
#             inputId = shiny::NS(id, "groupCheckbox_sens"),
#             label = "Sensibilité",
#             choices = r3$groupChoices_sens,
#             selected = NULL
#           )
#         })
#         output$groupCheckbox_type <- shiny::renderUI({
#           shiny::checkboxGroupInput(
#             inputId = shiny::NS(id, "groupCheckbox_type"),
#             label = "Charactéristiques",
#             choices = r3$groupChoices_type,
#             selected = NULL
#           )
#         })
#         output$groupCheckbox_class <- shiny::renderUI({
#           shiny::checkboxGroupInput(
#             inputId = shiny::NS(id, "groupCheckbox_class"),
#             label = "Classification",
#             choices = r3$groupChoices_class,
#             selected = NULL
#           )
#         })
#







        vftDbg("FR")
      }else if(input$languageSelect_2 == "en"){
        shiny.i18n::update_lang("en")
        i18n()$set_translation_language("en")
        r$currentLang <- "en"

        output$bannerUI_2 <- shiny::renderUI({
          imgMap <- imageMap(NS(id, "banner"), "www/step2_wsl_en.png", list() )
          #replace /" with ', to avoid problems
          return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
        })


        # #trigger an update of the species choices checkbox
        # r3$triggerSpChckUpdate <- r3$triggerSpChckUpdate  + 1
        if(!is.null(r3$spChoices) ){
          r3$spChoices_html <- buildHTMLList(r3$spChoices, speciesData, language = "en")
        }

        # #update filter to trigger checkbox update
        # shiny::updateSelectizeInput(inputId = "filterList", selected = r$filterList)

        #SPECIES SELECTION RENDER####
        output$speciesCheckbox <- shiny::renderUI({

          shiny::tagList(

            #TODO: Check for empty list of species due to filter

            shiny::checkboxGroupInput(
              inputId = shiny::NS(id, "speciesCheckbox"),
              label = "",
              choices = r3$spChoices_html
            ),
            shiny::tags$script(
              "
          $('#step2-speciesCheckbox .checkbox label span').map(function(choice){
              this.innerHTML = $(this).text();

          });
          "
            )
          )
        })

#
#
#
#







        vftDbg("EN")
      }else if(input$languageSelect_2 == "it"){
        # i18n$set_translation_language('it')
        shiny.i18n::update_lang("it")
        output$bannerUI_2 <- shiny::renderUI({
          imgMap <- imageMap(NS(id, "banner"), i18n()$t("www/step2_wsl.png"), list() )
          #replace /" with ', to avoid problems
          return(shiny::tagList(shiny::HTML(gsub( "\"", "'",paste0(imgMap) ))  ) )
        })


        vftDbg("IT")
      }





    }, ignoreInit = TRUE)

    #observe banner click (choosing to step back in history)
    obsBanner <- observeEvent(input$banner,  {
      vftDbg("MAPPED IMAGE CLICKED")
      #determine where to go back in history
      r3$confirm <- input$banner
      # print(input$banner)
      # shinyjs::runjs("Shiny.onInputChange('step2-banner', 'O')")
      # print(input$banner)

      #cleanup
      obsPrWeights$destroy()
      obsRWeights$destroy()
      obsReset$destroy()
      obsFilter$destroy()
      obsSpCheck$destroy()
      obsGroupCheck$destroy()
      obsGrpTr$destroy()
      obsTrgReset$destroy()
      obsBanner$destroy()
      obsSpChoice$destroy()
      if(!is.null(obsWeights)){obsWeights$destroy()}
      obsSelectAfter$destroy()
      obsConfirm$destroy()

      return(list(SM_pres = shiny::reactive(r$SM_pres), SMcolors = shiny::reactive(r$SMcolors), toSelectSpAfter = shiny::reactive(toSelectSpAfter), confirm = shiny::reactive({r3$confirm}), needHelp = reactive({r$needHelp})),
             currentLang = shiny::reactive(i18n()$get_translation_language()))

      #trigger return to past (return with specific confirm value?)
    }, ignoreInit = TRUE)


    vftDbgCat(paste0("ObserveEvents" ) )


    obsTrgReset <- shiny::observeEvent(r3$trigger_reset, {
      vftDbg("TRIGGER RESET")
      r3$ignoreGroupCheckboxEffect <- FALSE
      r3$ignoreAllCheckboxEffect <- FALSE
      r3$ignoreCheckboxEffect <- FALSE


    },priority = 0, ignoreInit = TRUE)

    # observeEvent(r3$trigger_applySavedGroupCheckboxes, {
    #   print("REUPDATE CHECKBOX GROUPS")
    #   # then re-update groupCheckboxInput with checkmark (removing All checkbox would have erased groupCheckbox checkmark)
    #   updateCheckboxGroupInput(inputId = "groupCheckbox_class", select = r3$grpChkClass)
    #   updateCheckboxGroupInput(inputId = "groupCheckbox_type", select = r3$grpChkType)
    #   updateCheckboxGroupInput(inputId = "groupCheckbox_sens", select = r3$grpChkSens)
    # },ignoreInit = TRUE )

    # observeEvent(r3$trigger_speciesRemoval, {
    #   print("species REMOVAL triggered")
    #   print(input$speciesCheckbox)
    #   # updateCheckboxGroupInput(inputId ="speciesCheckbox", selected = character(0))
    #
    #   updateCheckboxGroupInput(inputId = "speciesCheckbox",
    #                            selected = character(0))
    #   #launch update now (should work better as perhaps updateCheckboxGroup now updates before species update)
    #   r3$trigger_speciesUpdate <- r3$trigger_speciesUpdate + 1
    #
    #
    # }, ignoreInit =  TRUE)



    # OBSERVER GROUP CHECKBOX####
    obsGrpTr <- shiny::observeEvent(checkBoxGroupTriggers(), {
vftDbg("GROUP TRIGGERS")
      vftDbgCat(paste0("r$checkboxSave = ", r$checkboxSave ) )

      #ignore this if there is stored checkbox info
      if(is.null(r$checkboxSave ) ){
        vftDbgCat(paste0("r3$ignoreGroupCheckboxEffect = ", r3$ignoreGroupCheckboxEffect ) )

        vftDbg("CHECKBOXGROUPTRIGGER EVENT")
        vftDbg(paste0("ignoreGroupCheckbox: ", r3$ignoreGroupCheckboxEffect) )
        if(r3$ignoreGroupCheckboxEffect == FALSE){
          vftDbgCat(paste0("r3$ignoreGroupCheckboxEffect = ", r3$ignoreGroupCheckboxEffect ) )

          #remove All checkbox if it is checked
          if(input$groupCheckbox_all == TRUE){

            #ignore checkbox_all effects
            r3$ignoreAllCheckboxEffect <- TRUE
            r3$ignoreGroupCheckboxEffect <- TRUE

            #reset histories
            spCheckHistory <<- NULL
            #reset all global variables linking group and species checkboxes
            groupCheckboxHistory <<- NULL
            removedCheck <<- NULL


            #first remove all Checkbox
            vftDbg("remove ALL group")
            shiny::updateCheckboxInput(inputId ="groupCheckbox_all", value = FALSE)

            #manually apply the group checkbox Link of selected group checkbox
            #because group checkbox effect was ignored
            shinyjs::delay(100, {
              vftDbg("MANUALLY UPDATE SPECIES")
              shiny::updateCheckboxGroupInput(inputId = "speciesCheckbox",
                                              selected = unique(c(unlist(r3$groupLinks_class[input$groupCheckbox_class]),
                                                                  unlist(r3$groupLinks_type[input$groupCheckbox_type]),
                                                                  unlist(r3$groupLinks_sens[input$groupCheckbox_sens])
                                              ))
              )}
            )

          }
          vftDbgCat(paste0("ignoreNextUpdate", ignoreNextUpdate ) )

          if(ignoreNextUpdate == TRUE){
            vftDbg("IGNORE UPDATE")
            #still save changes to groupCheckbox
            groupCheckboxHistory <<- c(input$groupCheckbox_class, input$groupCheckbox_type, input$groupCheckbox_sens )
            ignoreNextUpdate <<- FALSE

            #trigger observer to reset ignore-variables (with delay to execute after groupCheckbox trigger)
            vftDbg("DELAY 1")
            shinyjs::delay(100, {r3$trigger_reset <- r3$trigger_reset + 1})


          }else{


            vftDbg("DOING NEXT UPDATE")

            vftDbg("species UPDATE triggered")
            #do not ignore next update
            currentSelectedSp <- input$speciesCheckbox
            vftDbg(paste0("currentSelectedSpecies: ", currentSelectedSp))

            #determine which group checkbox changed
            if(!is.null(groupCheckboxHistory)){
              currentGroupCheckboxes <- c(input$groupCheckbox_class, input$groupCheckbox_type, input$groupCheckbox_sens )
              #was a checkbox added or removed?
              if(length(currentGroupCheckboxes) < length(groupCheckboxHistory)){
                #checkbox was removed
                #determine which one
                removed <- !(groupCheckboxHistory %in% currentGroupCheckboxes) #invert
                removedCheck <- groupCheckboxHistory[removed]

              }else{
                removedCheck <- NULL
              }


            }
            #determine in which groups there are checkboxes to make link with species checkboxes
            #determine selected species (combine all group checkboxes)
            selectedSpecies <- unique(c(unlist(r3$groupLinks_class[input$groupCheckbox_class]),
                                        unlist(r3$groupLinks_type[input$groupCheckbox_type]),
                                        unlist(r3$groupLinks_sens[input$groupCheckbox_sens])
            ))

            vftDbg(paste0("Selected Species:", selectedSpecies))

            vftDbg(paste0("groupLink:"))
            vftDbg(r3$groupLinks)

            #remove species related to removed group checkbox

            if(!is.null(removedCheck)){
              vftDbg("PARTX")
              if(removedCheck %in% names(r3$groupLinks_class)){
                currentSelectedSp <- currentSelectedSp[!(currentSelectedSp %in% unlist(r3$groupLinks_class[removedCheck]))]
              }else if(removedCheck %in% names(r3$groupLinks_type)){
                currentSelectedSp <- currentSelectedSp[!(currentSelectedSp %in% unlist(r3$groupLinks_type[removedCheck]))]
              }else if (removedCheck %in% names(r3$groupLinks_sens)){
                currentSelectedSp <- currentSelectedSp[!(currentSelectedSp %in% unlist(r3$groupLinks_sens[removedCheck]))]
              }
            }
            #combine remaining selected species with selected group checkboxes
            selectedSpecies <- unique(c(currentSelectedSp, selectedSpecies))
            #update checkboxes
            shiny::updateCheckboxGroupInput(inputId = "speciesCheckbox",
                                            selected = selectedSpecies)

            #force removal of selections by recreating checkboxes
            #if all checkbox groups are empty

            if(is.null(input$groupCheckbox_class)&
               is.null(input$groupCheckbox_type)&
               is.null(input$groupCheckbox_sens)){
              vftDbg("IFNULL")
              output$speciesCheckbox <- shiny::renderUI({
                shiny::tagList(

                  #TODO: Check for empty list of species due to filter

                  shiny::checkboxGroupInput(
                    inputId = shiny::NS(id, "speciesCheckbox"),
                    label="",
                    choices = r3$spChoices_html,
                    selected = NULL
                  ),
                  shiny::tags$script(
                    "
          $('#step2-speciesCheckbox .checkbox label span').map(function(choice){
              this.innerHTML = $(this).text();

          });
          "
                  )
                )
              })
            }

            #save current species selection
            groupCheckboxHistory <<- c(input$groupCheckbox_class, input$groupCheckbox_type, input$groupCheckbox_sens )

          }
          #trigger observer to reset ignore-variables (with delay to execute after groupCheckbox trigger)
          vftDbg("DELAY2")
          shinyjs::delay(100, {r3$trigger_reset <- r3$trigger_reset + 1})

        }
      }

    }, ignoreNULL = FALSE, ignoreInit = TRUE, priority = 1)

    # SPECIES CHECK BOX TRIGGER ####
    #determine which groupCheckboxes to remove if a species checkbox is unselected
    spCheckHistory <- NULL
    # r3 <- reactiveValues()
    r3$speciesWeightNames <- NULL
    r3$speciesWeightInputs <- NULL

    #OBSERVE ALL CHECKBOX SELECTION ####
    obsGroupCheck <- shiny::observeEvent(input$groupCheckbox_all, {
vftDbg("CHECKBOX ALL")
      vftDbg(paste0("ignoreAllCheckbox: ", r3$ignoreAllCheckboxEffect) )
      if(r3$ignoreAllCheckboxEffect == FALSE){
        if(input$groupCheckbox_all == TRUE){
          vftDbg("SELECT ALL")

          #ignore effects to be able to change checkboxes without triggering observer effects
          r3$ignoreGroupCheckboxEffect <- TRUE


          spCheckHistory <<- NULL
          #reset all global variables linking group and species checkboxes
          groupCheckboxHistory <<- NULL
          removedCheck <<- NULL

          shiny::updateCheckboxGroupInput(inputId ="groupCheckbox_class", selected = character(0))
          shiny::updateCheckboxGroupInput(inputId ="groupCheckbox_type", selected = character(0))
          shiny::updateCheckboxGroupInput(inputId ="groupCheckbox_sens", selected = character(0))

          #manually select all species
          vftDbg("select all species")

          #TODO: Check for empty list of species due to filter

          shiny::updateCheckboxGroupInput(inputId ="speciesCheckbox", selected = r3$spChoices_html)

          #trigger observer to reset ignore-variables (with delay to execute after groupCheckbox trigger)
          vftDbg("DELAY3")
          shinyjs::delay(100, {r3$trigger_reset <- r3$trigger_reset + 1})

        }else{
          vftDbg("UNSELECT ALL")

          spCheckHistory <<- NULL
          #reset all global variables linking group and species checkboxes
          groupCheckboxHistory <<- NULL
          removedCheck <<- NULL

          #avoid effects of unchecking groupCheckboxes (as species Checkbox will be all removed directly)
          r3$ignoreGroupCheckboxEffect <- TRUE

          #remove species checkboxes
          vftDbg("unselect all species")
          # updateCheckboxGroupInput(inputId ="speciesCheckbox", selected = character(0))

          #remove group checkboxes
          vftDbg("unselect group checkboxes")
          shiny::updateCheckboxGroupInput(inputId ="groupCheckbox_class", selected = character(0))
          shiny::updateCheckboxGroupInput(inputId ="groupCheckbox_type", selected = character(0))
          shiny::updateCheckboxGroupInput(inputId ="groupCheckbox_sens", selected = character(0))

          shiny::updateCheckboxGroupInput(inputId ="speciesCheckbox", selected = character(0))

          #trigger observer to reset ignore-variables (with delay to execute after groupCheckbox trigger)
          vftDbg("DELAY4")
          shinyjs::delay(100, {r3$trigger_reset <- r3$trigger_reset + 1})


        }
      }

    }, ignoreInit = TRUE, priority = 3)

    #observe whether checkbox or weights were altered (1 at end avoids error due to NULL triggers)
    obsSpCheck <- shiny::observeEvent(c(input$speciesCheckbox, triggerUpdate()), {
      if(r3$ignoreCheckboxEffect == FALSE){
      vftDbg("SPCHECKBOX")
      #LOAD EXISTING CHECKBOX DATA (IF EXISTING AND NO SPECIES SELECTED YET)
      if(!is.null(r$checkboxSave) & is.null(input$speciesCheckbox) ){
        vftDbg("LOADING EXISTING FILTER, CHECKBOXES AND WEIGHTS")
        r3$ignoreGroupCheckboxEffect <- TRUE
        r3$ignoreAllCheckboxEffect <- TRUE
        # r3$ignoreCheckboxEffect <- TRUE
        # ignoreNextUpdate <<- TRUE

        # shiny::isolate({
        #determine if All are selected (if not, only then check groups)
          shiny::updateSelectInput(inputId = "filterList", selected = r$filterList)

          #update rest on filter change

          # if(r$groupSave_all == TRUE){
          #   shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_all", selected = r$groupSave_all)
          # }else{
          #   shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_sens", selected = r$groupSave_sens)
          #   shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_type", selected = r$groupSave_type)
          #   shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_class", selected = r$groupSave_class)
          # }
          #
          # shiny::updateCheckboxGroupInput(inputId = "speciesCheckbox", selected = r$checkboxSave)
        #
        # })

        #function to load species weights into each weight input

        #avoid using saved version while working additionally on sensitive matrix


        # #update map
        # if(is.null(SMUpdate() ) ){
        #   SMUpdate(1)
        # }else{
        #   SMUpdate(SMUpdate() + 1)
        # }
        #reset ignores with delay
        vftDbg("DELAY5")
        shinyjs::delay(300, {r3$trigger_reset <- r3$trigger_reset + 1})

      }

        vftDbgCat(paste0("BLABLA" ) )


      if(!is.null(input$speciesCheckbox)){
vftDbg("BLABLA")
        #when there is at least one checkbox
        if(!is.null(spCheckHistory)){
          spRemovedCheck <- spCheckHistory[!(spCheckHistory %in% input$speciesCheckbox)]
          #if a checkbox was removed
          if(!is.null(spRemovedCheck) & length(spRemovedCheck > 0)){
            if(length(spRemovedCheck) == 1){
              #determine associated and checked groupCheckboxes
              #for class
              groups <- r3$groupLinks_class[input$groupCheckbox_class]
              #check that there's only one checkbox and it is linked to a checked groupCheckbox
              if(spRemovedCheck %in% unlist(groups)){
                #determine new selection by cycling through class elements
                toKeep <- c()
                for(i in 1:length(groups)){
                  if( !(spRemovedCheck %in% groups[[i]]) ){toKeep <- c(toKeep, i)}
                }
                groups <- groups[toKeep]
                #update without reactivity
                shiny::isolate(shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_class", selected = names(groups)))
                #avoid cyclical updates on checkboxes
                ignoreNextUpdate <<- TRUE
              }
              #for type
              groups <- r3$groupLinks_type[input$groupCheckbox_type]
              vftDbg("REMOVEDCHECK1")
              #check that there's only one checkbox and its linked to a checked groupCheckbox
              if(spRemovedCheck %in% unlist(groups)){
                #determine new selection by cycling through class elements
                toKeep <- c()
                for(i in 1:length(groups)){
                  if( !(spRemovedCheck %in% groups[[i]]) ){toKeep <- c(toKeep, i)}
                }
                groups <- groups[toKeep]
                shiny::isolate(shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_type", selected = names(groups)))
                #avoid cyclical updates on checkboxes
                ignoreNextUpdate <<- TRUE
              }
              #for sensitivity
              groups <- r3$groupLinks_sens[input$groupCheckbox_sens]
              vftDbg("REMOVEDCHECK2")
              #check that there's only one checkbox and its linked to a checked groupCheckbox
              if(spRemovedCheck %in% unlist(groups)){

                #determine new selection by cycling through class elements
                toKeep <- c()
                for(i in 1:length(groups)){
                  if( !(spRemovedCheck %in% groups[[i]]) ){toKeep <- c(toKeep, i)}
                }
                groups <- groups[toKeep]
                shiny::isolate(shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_sens", selected = names(groups)))
                #avoid cyclical updates on checkboxes
                ignoreNextUpdate <<- TRUE
              }

            }
          }
        }

        vftDbgCat(paste0("Create Sensitivity Matrix" ) )

        ## CREATE SENSITIVITY MATRIX ####
vftDbg("SENSITIVITYMATRIX")
        if( length(input$speciesCheckbox) > 0 ){

          vftDbgCat(paste0("speciescheckbox > 0" ) )

          #keep species from checkboxes
          keptSpNum <-input$speciesCheckbox # ex: c1, c2 etc..
          #remove "c"
          keptNb <- as.numeric( gsub("c", "", keptSpNum))

          vftDbgCat(paste0("keptNb =", keptNb ) )

          #make list of kept species' names
          r$keptSpecies <- r3$spChoices[keptNb]

          vftDbgCat(paste0("gsub() = ", gsub(" ", ".", r$keptSpecies) ) )
          vftDbgCat(paste0("names(r$sdmLayer) = ", names(r$sdmLayer) ) )

          #replace " " with "_" for layer names
          chosenLayers <- r$sdmLayer[[r$keptSpecies]]#gsub(" ", ".", )

          vftDbgCat(paste0("chosenLayers = ", chosenLayers ) )

          # chosenLayers_pres <- sdmLayers_pres[[gsub(" ", ".", keptSpecies)]]
          # chosenLayers_noPres <- sdmLayers_noPres[[gsub(" ", ".", keptSpecies)]]


          #get each species' weights into a reactive (triggers SM creation and plot if changed)
          r3$speciesWeights <- paste0("weight_", keptNb)

          vftDbgCat(paste0("STEP1" ) )


vftDbg("STEP1")
          #weight each layer by applying weights

          #first get all inputs (weight_X)
          weightInputs <- c()
          weightNames <- c()

          for(lyrNb in 1:terra::nlyr(chosenLayers)){
            weightInputs[lyrNb] <- input[[r3$speciesWeights[lyrNb]]]
            weightNames[lyrNb] <- r3$speciesWeights[lyrNb]

          }

          r3$weightInputs <- weightInputs
          r3$weightNames <- weightNames

          vftDbgCat(paste0("WEIGHTINPUTS" ) )

          vftDbg(paste0("weightInputs: ", r$weightInputs))
          #then do vectorised calculations
          #multiply by weight ( weight of 5 considers it as 5 species rather than 1)
          chosenLayers <- chosenLayers * weightInputs
          # chosenLayers_pres <- chosenLayers_pres * weightInputs
          # chosenLayers_noPres <- chosenLayers_noPres * weightInputs


vftDbg("STEP2")
          #combine rasters (addition and division by max => 0-1), generate sensitivity matrix (SM)
          #if multiple layers
          # if(terra::nlyr(chosenLayers)> 1 ){
          #
          #   #determine total SM by ADDING layer values between them
          #   #(offers a form of alpha diversity of critical species)
          #   SM <<- terra::app(chosenLayers, fun = function(x) sum(x))
          #
          # }else if (terra::nlyr(chosenLayers) == 1){
          #
          #   #for single layer
          #   SM <<- chosenLayers

          # }
vftDbg("CHOSEN")
          if(terra::nlyr(chosenLayers)> 1 ){

            #determine total SM by ADDING layer values between them
            #(offers a form of alpha diversity of critical species)
            r$SM_pres <- terra::app(chosenLayers, fun = function(x) sum(x))

          }else if (terra::nlyr(chosenLayers) == 1){

            #for single layer
            r$SM_pres <- chosenLayers

          }
# print("NLAYER")
#           if(terra::nlyr(chosenLayers_noPres)> 1 ){
#
#             #determine total SM by ADDING layer values between them
#             #(offers a form of alpha diversity of critical species)
#             SM_noPres <<- terra::app(chosenLayers_noPres, fun = function(x) sum(x))
#
#           }else if (terra::nlyr(chosenLayers_noPres) == 1){
#
#             #for single layer
#             SM_noPres <<- chosenLayers_noPres
#
#
#
#           }

        }
        vftDbgCat(paste0("UPDATE" ) )
        vftDbgCat(paste0("UPDATE" ) )
        vftDbgCat(paste0("UPDATE" ) )
        vftDbgCat(paste0("UPDATE" ) )
        vftDbgCat(paste0("UPDATE" ) )

        #update map
        SMUpdate(SMUpdate() + 1)

      }else{
        #input is null
        SMUpdate(0)
      }
      spCheckHistory <<- input$speciesCheckbox


      vftDbgCat(paste0("Species Weight" ) )



      vftDbg("SPECIESWEIGHT")
      if(length(r3$speciesWeights) > 0 ){
        #create a reactive list of active weight inputs
        reactiveWeights <- shiny::reactive({
          vftDbg("WEIGHTS REACTIVE")
          weightInputList <- list()
          for(i in 1:length(r3$speciesWeights)){
            weightInputList[[i]] <- input[[r3$speciesWeights[i] ]]
          }

          weightInputList

          })
        vftDbg("OBSER")

        vftDbgCat(paste0("Species Weight2" ) )

        #destroy past observer
        if(!is.null(obsWeights)){
          if(length(obsWeights) > 0){
            obsWeights$destroy()
          }
        }

        obsWeights <<- shiny::observeEvent(reactiveWeights(), {
          vftDbg("TRIGGER UPDATE WEIGHTS")
          if(is.null(triggerUpdate())){
            triggerUpdate(1)
          }else{
            triggerUpdate(triggerUpdate() + 1)

          }
        }, ignoreInit = TRUE)
      }

      vftDbgCat(paste0("Species Weight3" ) )


        ## CREATE WEIGHT OBSERVER ####
      # print("CREATE WEIGHT OBSERVER")
      # #remove previous observers (if they exist)
      # if(!is.null(r3$weightObsList)){
      #    if(length(r3$weightObsList) > 0){
      #   #cycle within to destroy all observers
      #   for(obs in r3$weightObsList){
      #     obs[[1]]$destroy()
      #   }
      #    }
      # }
      #   #reset observer list
      #   r3$weightObsList <- list()
      #   #create observers for all selected species
      #   for(i in 1:length(r3$speciesWeights)){
      #     tmp <- r3$speciesWeights[[i]]
      #     r3$weightObsList[[i]] <- list(
      #       observeEvent(input[[tmp]], {
      #         print("WEIGHT TEST")
      #       }, ignoreInit = TRUE)
      #     )
      #     print(paste0("obsEvent: ",tmp ))
      #
      #     }

      }
      1

    }, ignoreNULL = FALSE, ignoreInit = TRUE)



    # OBSERVE FILTER CHANGE ####
    #make reactive list of spOrder, that changes when filter changes
    obsFilter <- shiny::observeEvent(input$filterList, {

      vftDbgCat(paste0("r$groupSave_all = ", r$groupSave_all ) )

      #LOAD SAVED CHECKS WITH DELAY ####
      #if stored checkboxes exist
      if(!is.null(r$checkboxSave)){
        shinyjs::delay(300, {
          vftDbg("DELAY SAVED CHECKS")


          if(r$groupSave_all == TRUE){
            shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_all", selected = r$groupSave_all)
          }else{
            shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_sens", selected = r$groupSave_sens)
            shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_type", selected = r$groupSave_type)
            shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_class", selected = r$groupSave_class)
          }

          shiny::updateCheckboxGroupInput(inputId = "speciesCheckbox", selected = r$checkboxSave)

          shinyjs::delay(300, {
            vftDbg("Checkbox save zeroed")
            r$checkboxSave <- NULL
            #load weights into every weight field
            isolate({
              for(weightNo in 1:length(r$weightInputs) ){
                updateTextInput( inputId = r$weightNames[[weightNo]],
                                 value = r$weightInputs[[weightNo]]
                )
              }
            })
          })
          # r$checkboxSave <- NULL


        })
      }

      vftDbgCat(paste0("Change filter") )


      #global variable to avoid cyclical updates
      # ignoreNextUpdate <<- TRUE
      vftDbg("CHANGE FILTER")
      spCheckHistory <<- input$speciesCheckbox

      #update checkboxes

      shiny::updateCheckboxGroupInput(inputId = "speciesCheckbox",
                               selected = NULL)
      shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_class",
                               selected = NULL)
      shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_type",
                               selected = NULL)
      shiny::updateCheckboxGroupInput(inputId = "groupCheckbox_sens",
                               selected = NULL)
      shiny::updateCheckboxInput(inputId = "groupCheckbox_all",
                          value = FALSE)

      #global variable to avoid cyclical updates
      # ignoreNextUpdate <<- FALSE

      #
      #reset all global variables linking group and species checkboxes
      groupCheckboxHistory <<- NULL
      removedCheck <<- NULL

      #
      # #determine which groupCheckboxes to remove if a species checkbox is unselected
      #
      # # r3 <- reactiveValues()
      # r3$speciesWeights <- NULL
      # r3$speciesWeightInputs <- NULL
      #filter spChoices based on inputList
      vftDbgCat(paste0("input$filterList = ", input$filterList) )


      r3$spChoices <- switch(input$filterList,
                            "s1" = r$spChc,
                            "s2" = r$spChc[r$spChc %in% speciesData$latinN[speciesData$ch.priority %in% c("1")] ],
                            "s3" = r$spChc[r$spChc %in% speciesData$latinN[speciesData$ch.priority %in% c("1", "2")] ],
                            "s4" = r$spChc[r$spChc %in% speciesData$latinN[speciesData$ch.priority %in% c("1", "2", "3")] ],
                            "s5" = r$spChc[r$spChc %in% speciesData$latinN[speciesData$ch.priority %in% c("1", "2", "3", "4")] ],
                            "s6" = r$spChc[r$spChc %in% speciesData$latinN[speciesData$threat %in% c("CR")] ],
                            "s7" = r$spChc[r$spChc %in% speciesData$latinN[speciesData$threat %in% c("CR", "EN")] ],
                            "s8" = r$spChc[r$spChc %in% speciesData$latinN[speciesData$threat %in% c("CR", "EN", "VU")] ],
                            "s9" = r$spChc[r$spChc %in% speciesData$latinN[speciesData$threat %in% c("CR", "EN", "VU", "NT")] ],)

 



      }, ignoreInit = TRUE, priority = 10)

    # BUTTON OBSERVERS ####
    vftDbgCat(paste0("Button Observers" ) )

    obsReset <- shiny::observeEvent(input$resetWeights, {
      #go through all weights and reset to 1
      for(i in 1:length(r3$spChoices)){
        shiny::updateTextInput(
          inputId = paste0("weight_", i),
          value = 1
        )

      }
    })

    obsRWeights <- shiny::observeEvent(input$redListWeights, {
      #go through all weights and reset to 1
      for(i in 1:length(r3$spChoices)){
        #determine value based on Red List status (CRitical = 5, ENdanger = 4, VUlnerable = 3, etc.)
        threatLvl <- speciesData$threat[speciesData$latinN == r3$spChoices[i]]
        newValue = 1
        newValue <- switch(threatLvl,
                           CR = 5,
                           EN = 4,
                           VU = 3,
                           NT = 2,
                           LC = 1
        )
        shiny::updateTextInput(
          inputId = paste0("weight_", i),
          value = newValue
        )

      }
    })

    obsPrWeights <- shiny::observeEvent(input$priorityWeights, {
      #go through all weights and reset to 1
      for(i in 1:length(r3$spChoices)){
        #determine value based on Red List status (CRitical = 5, ENdanger = 4, VUlnerable = 3, etc.)
        prrLvl <- speciesData$ch.priority[speciesData$latinN == r3$spChoices[i]]
        newValue = 1
        newValue <- switch(prrLvl,
                           "1" = 5,
                           "2" = 4,
                           "3" = 3,
                           "4" = 2,
                           1
        )
        shiny::updateTextInput(
          inputId = paste0("weight_", i),
          value = newValue
        )

      }
    })

    # observe SM download click ####
    obsDownloadSM <- shiny::observeEvent(input$SMbutton, {
      shinyjs::click("downloadSM", asis = FALSE)

    }, ignoreInit = TRUE)


    #PREPARE SM DOWNLOAD ####
    output$downloadSM <- shiny::downloadHandler(
      filename = function(){

        if(r$currentLang %in% c("de", "en")){
          name <- "visitorFlow_SM.zip"
        }else if(r$currentLang == "fr"){
          name <- "visitorFlow_MdS.zip"
        }

        return(name)
      },
      content = function(file){

        # Create a dedicated temp folder with a clean name
        tmpDir <- tempfile(pattern = "SM_download")
        dir.create(tmpDir)

        # Define clean file names inside that folder
        tifFile  <- file.path(tmpDir, "SensitivityMatrix.tif")
        txtFile  <- file.path(tmpDir, "INFO_SM.txt")



        # tempTIF_SM <- tempfile(pattern = "SM_", fileext = ".tif")
        terra::writeRaster(r$SM_pres, filename = tifFile, filetype = "GTiff")

        #text info
        # tempTXT_info <- tempfile(pattern = "INFO_", fileext = ".txt")
        # fileConn<-file(tempTXT_info)
        writeLines(c("Information about the sensitivity matrix.",
                       "Values represent sensitivity (number of considered species present * weight given",
                       "ex: if a pixel has 3 species with weight of 1, and 3 species with weight of 2, it would have a sensitivity of 9",
                       "---",
                       ifelse(minCutThresh == 0,"", paste0("Important: This Sensitivity Matrix only shows the top ", minCutThresh, "% sensitivity values")),
                       "The species included are:",
                     paste(r3$spChoices)
        ), txtFile)
        # close(fileConn)

        # Zip using relative paths by setting wd to tmpDir
        oldWd <- setwd(tmpDir)
        on.exit(setwd(oldWd), add = TRUE)  # always restore wd

        #zip both
        utils::zip(file,files =  c("SensitivityMatrix.tif", "INFO_SM.txt"))


      }
    )
    #make handler work when invisible
    outputOptions(output, "downloadSM", suspendWhenHidden = FALSE)


    # RENDER MAP ####
    vftDbgCat(paste0("RenderMap" ) )

    output$SDMmap <- shiny::renderPlot({
      #calculate extent of legend based on plot extent
      plotExt <- terra::ext(terra::vect(shp_WGS84))
      left <- plotExt[1]
      right <- plotExt[2]
      bottom <- plotExt[3]
      top <- plotExt[4]
      legTop <- bottom - ((top-bottom) / 8)
      legBottom <- legTop - ((top-bottom) / 15)
      legExt <- c(left, right, legBottom, legTop)

      if(r$currentLang == "de"){
        terra::plot(basemapWhite, y = 1, type = "continuous", col = "#FFFFFF", ext = terra::ext(terra::vect(shp_WGS84)), range = c(0,12), legend = FALSE, box = FALSE, axes = FALSE,  mar = c(6.1, 0, 0, 0), plg = list(legend = "bottom", horiz = TRUE, title = "Sensitivität", title.adj = 0))
      }else if(r$currentLang == "fr"){
        terra::plot(basemapWhite, y = 1, type = "continuous", col = "#FFFFFF", ext = terra::ext(terra::vect(shp_WGS84)), range = c(0,12), legend = FALSE, box = FALSE, axes = FALSE,  mar = c(6.1, 0, 0, 0), plg = list(legend = "bottom", horiz = TRUE, title = "Sensibilité", title.adj = 0))
      }else if(r$currentLang == "en"){
        terra::plot(basemapWhite, y = 1, type = "continuous", col = "#FFFFFF", ext = terra::ext(terra::vect(shp_WGS84)), range = c(0,12), legend = FALSE, box = FALSE, axes = FALSE,  mar = c(6.1, 0, 0, 0), plg = list(legend = "bottom", horiz = TRUE, title = "Sensitivity", title.adj = 0))
      }
        vftDbg("SMUPDATE")
      if(SMUpdate() > 0){

#expose reactive to refreshing plot
       input$minValThreshold

shiny::isolate({
  #get max value of both SM_pres and SM_noPres
  r$maxVal <- terra::minmax(r$SM_pres)["max",]
  #make legend relative
  r$minVal <- input$minValThreshold / 100 * r$maxVal
  if (r$maxVal < 12) { r$maxVal <- 12 }

  #if no SM_pres, than just plot SM_noPres

#simply ignore noPres (simple solution for now)

          # if(!is.null(SM_noPres)){
          #   SMcolors <<- c("#FFFFFF00", grDevices::colorRampPalette(c("#FFD70080", "#FF8C0080", "#FF303080", "#9A32CD80", "#68228B80", "#3b035780"), alpha = TRUE)(maxVal))
          #
          #   # colorIntervals <- data.frame(from = 0:12, to = c(1:12, 1000), col = colors)
          #
          #   #plot map: range and colNA help set threshold
          #   terra::plot(x= SM_noPres, col  = SMcolors, box = FALSE, axes = FALSE, colNA = "#3b035780",  type = "continuous", range = c(0, maxVal), add = TRUE, mar = c(6.1, 0, 0, 0), plg = list(ext = legExt, loc = "bottom", title = "Artenzahl", pax = list(side = 1)))
          #   terra::lines(x = terra::vect(shp_WGS84), col = "black", lwd = 2)
          #
          # }
          # basemap <- basemaps::basemap_raster(ext = st_bbox(shp_WebMerc))

          

          if(!is.null(r$SM_pres)){
            r$SMcolors <- c("#FFFFFF00", grDevices::colorRampPalette(c("#FFD700", "red", "red4"), alpha = TRUE)(r$maxVal-r$minVal))

            # colorIntervals <- data.frame(from = 0:12, to = c(1:12, 1000), col = colors)

            #plot map: range and colNA help set threshold
            if(r$currentLang == "de"){
              terra::plot(x= r$SM_pres, box = FALSE, axes = FALSE, col  = r$SMcolors, colNA = "black",  type = "continuous", range = c(r$minVal, r$maxVal), fill_range = TRUE,
                          add = TRUE, mar = c(6.1, 0, 0, 0), plg = list(x = "bottom", horiz = TRUE, title = "Sensitivität", title.adj = -1, pax = list(side = 1)))
            }else if(r$currentLang == "fr"){
              terra::plot(x= r$SM_pres, box = FALSE, axes = FALSE, col  = r$SMcolors, colNA = "black",  type = "continuous", range = c(r$minVal, r$maxVal), fill_range = TRUE,
                          add = TRUE, mar = c(6.1, 0, 0, 0), plg = list(x = "bottom", horiz = TRUE, title = "Sensibilité", title.adj = -1, pax = list(side = 1)))
            }else if(r$currentLang == "en"){
              terra::plot(x= r$SM_pres, box = FALSE, axes = FALSE, col  = r$SMcolors, colNA = "black",  type = "continuous", range = c(r$minVal, r$maxVal), fill_range = TRUE,
                          add = TRUE, mar = c(6.1, 0, 0, 0), plg = list(x = "bottom", horiz = TRUE, title = "Sensitivity", title.adj = -1, pax = list(side = 1)))
            }
            terra::lines(x = terra::vect(shp_WGS84), col = "black", lwd = 2)

          }

          # add_legend(x = 0, y = -10, legend = 1:12, horiz = TRUE, title = "Artenzahl", title.adj = 0.5)

          # basemap <- basemaps::basemap_raster(ext = st_bbox(shp_WebMerc))
          terra::plot(basemap, 1, col = "#000000",legend = FALSE,  add = TRUE, alpha = alphaMap *0.7 )
        }) # end isolate

      }else if (SMUpdate() == 0){
        #plot empty area (correction: plot empty map)
        # plot.new()
        terra::plot(basemap, col = "#000000", box = FALSE, axes = FALSE, add = TRUE, alpha = alphaMap * 0.7, legend = FALSE)
        terra::lines(x = terra::vect(shp_WGS84), col = "black", lwd = 2)

      }
    })


    obsConfirm <- shiny::observeEvent(input$confirmButton2, {

      r3$ignoreCheckboxEffect <- FALSE

      vftDbg("CONFIRM STEP 3")
      r3$confirm <- input$confirmButton2
      #cleanup
      obsPrWeights$destroy()
      obsRWeights$destroy()
      obsReset$destroy()
      obsFilter$destroy()
      obsSpCheck$destroy()
      obsGroupCheck$destroy()
      obsGrpTr$destroy()
      obsTrgReset$destroy()
      obsBanner$destroy()
      obsSpChoice$destroy()
      if(!is.null(obsWeights)){obsWeights$destroy()}
      obsSelectAfter$destroy()
      obsConfirm$destroy()

      # shinyjs::reset("confirmButton2")
      # return(list(SM = reactive(SM), toSelectSpAfter = reactive(toSelectSpAfter), confirm = reactive({input$confirmButton2})) )

      # shinyjs::useShinyjs()

      #SAVE spKept, sdmLayers and Filter state
      r$checkboxSave <- input$speciesCheckbox
      r$groupSave_class <- input$groupCheckbox_class
      r$groupSave_sens <- input$groupCheckbox_sens
      r$groupSave_type <- input$groupCheckbox_type
      r$groupSave_all <- input$groupCheckbox_all
      r$weightInputs <- r3$weightInputs
      r$weightNames <- r3$weightNames

      #weightInputs saved on the fly


      # r$df_spInfo <- df_spInfo
      # r$spChc <- spChc
      r$filterList <- input$filterList
      # r$sdmLayers <- sdmLayers

      vftDbgCat("DEBUG1")

      #Finalize SMColors (make all values below Cutoff Threshold a minValue)
      r$SM_pres[r$SM_pres <= r$minVal] <- r$minVal


      # return(list(SM_pres = shiny::reactive(r$SM_pres), SMcolors = shiny::reactive(SMcolors), toSelectSpAfter = shiny::reactive(toSelectSpAfter), confirm = shiny::reactive({r3$confirm}), needHelp = reactive({r$needHelp}),
      #             groupSave_all = shiny::reactive(r$groupSave_all), groupSave_sens = shiny::reactive(r$groupSave_sens), groupSave_type = shiny::reactive(r$groupSave_type), groupSave_class = shiny::reactive(r$groupSave_class), checkboxSave = shiny::reactive(r$checkboxSave),
      #             filterList = shiny::reactive(r$filterList), weightInputs = shiny::reactive(r$weightInputs), weightNames = shiny::reactive(r$weightNames)) )
    return(list(SM_pres = shiny::reactive(r$SM_pres), SMcolors = shiny::reactive(r$SMcolors), toSelectSpAfter = shiny::reactive(toSelectSpAfter), confirm = shiny::reactive({r3$confirm}), needHelp = reactive({r$needHelp}),
                groupSave_all = shiny::reactive(r$groupSave_all), groupSave_sens = shiny::reactive(r$groupSave_sens), groupSave_type = shiny::reactive(r$groupSave_type), groupSave_class = shiny::reactive(r$groupSave_class), checkboxSave = shiny::reactive(r$checkboxSave),
                filterList = shiny::reactive(r$filterList), weightInputs = shiny::reactive(r$weightInputs), weightNames = shiny::reactive(r$weightNames), species = shiny::reactive(r$keptSpecies),
                currentLang = shiny::reactive(i18n()$get_translation_language()), minCutThresh = shiny::reactive(input$minValThreshold) ) )
    }, ignoreInit = TRUE)

    obsSelectAfter <- shiny::observeEvent(input$selectSpAfter, {

      toSelectSpAfter <<- TRUE

      SM <<- NULL

      r3$confirm <- input$selectSpAfter

      #cleanup
      obsPrWeights$destroy()
      obsRWeights$destroy()
      obsReset$destroy()
      obsFilter$destroy()
      obsSpCheck$destroy()
      obsGroupCheck$destroy()
      obsGrpTr$destroy()
      obsTrgReset$destroy()
      obsBanner$destroy()
      obsSpChoice$destroy()
      obsWeights$destroy()
      obsSelectAfter$destroy()
      obsConfirm$destroy()
      # return(list(SM = reactive(SM), toSelectSpAfter = reactive(toSelectSpAfter), confirm = reactive({input$selectSpAfter})) )

    })


    # output$SDMmap <- renderTmap({
    #
    # tmap_mode('view')
    #
    # tm_raster(col = "Band_1", alpha = 0.5)
    #
    # })
    #Manage SDM data (Make a sensitivity Matrix (SM))


    vftDbgCat("RETURNING...")
    return(list(SM_pres = shiny::reactive(r$SM_pres), SMcolors = shiny::reactive(r$SMcolors), toSelectSpAfter = shiny::reactive(toSelectSpAfter), confirm = shiny::reactive({r3$confirm}), needHelp = reactive({r$needHelp}),
                groupSave_all = shiny::reactive(r$groupSave_all), groupSave_sens = shiny::reactive(r$groupSave_sens), groupSave_type = shiny::reactive(r$groupSave_type), groupSave_class = shiny::reactive(r$groupSave_class), checkboxSave = shiny::reactive(r$checkboxSave),
                filterList = shiny::reactive(r$filterList), weightInputs = shiny::reactive(r$weightInputs), weightNames = shiny::reactive(r$weightNames), species = shiny::reactive(r$keptSpecies),
                currentLang = shiny::reactive(i18n()$get_translation_language()), minCutThresh = shiny::reactive(input$minValThreshold) ) )
  })
}
#
