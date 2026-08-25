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

  #the step nav bar, OUTSIDE the tabsetPanel so it is on screen whichever step
  #is showing. Renders nothing unless VFT_NAV=1. See vftStepNav() below.
  vftStepNav(),

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


#### The step nav bar ####

# There has never been a way to move between steps other than the confirm button
# at the bottom of the one you are on: no way back, no way to skip ahead, and no
# indication that a step you cannot use yet exists. The banner images were meant
# to be that bar - each carries five clickable areas mapped to the letters
# "A".."E" - but imageMap() early-returned a bare <img> before ever building the
# <map>, so those ~250 lines of back-navigation had never executed. Stage 1
# deleted imageMap(); this is what replaces it.
#
# The one hard constraint: it must NOT be a renderUI. This app registers 31
# distinct outputs and Shiny sweeps ALL of them, testing each for suspension, on
# every single input message batch - 11% of sampled time. Six of those outputs
# were banner renderUIs producing one static <img> each, and deleting them was
# the largest single item in Stage 1. Putting a seven-button bar back as an
# output would hand most of that saving straight back.
#
# So the markup is built once, here, with the buttons literally in the page.
# Nothing about it is reactive. The only things that change at runtime are the
# disabled attribute and one CSS class, and those go out as shinyjs messages
# from a single observe() in vftNavBarServer() - which sends only the ones that
# actually changed.

#' The step nav bar, or NULL when `VFT_NAV` is off.
#'
#' Buttons carry unnamespaced ids (`vftNav_step1`, ...) because the bar lives at
#' app level, outside the tabsetPanel and outside every module namespace.
#'
#' Everything except step 1 ships disabled: at session start step 1 is the only
#' step whose `needs` are met, and starting from the correct state means the user
#' never sees a frame of seven live buttons before the first flush corrects them.
#'
#' The tooltip is static too. `needs` and VFT_KEY_SOURCE are both compile-time
#' constants, so "which step would I have to do first" can be answered while the
#' page is being built rather than by a message per state change.
vftStepNav <- function(){
  if(!vftNavEnabled()) return(NULL)

  buttons <- lapply(names(VFT_STEPS), function(step){
    btn <- shiny::actionButton(
      inputId = vftNavInputId(step),
      label   = VFT_STEPS[[step]]$label,
      class   = "vft-nav-btn",
      title   = vftStepTooltip(step)
    )
    #step 1 is the only step whose needs are met at session start; a step this
    #build does not let the bar reach (VFT_NAV=step1,step3) ships disabled and
    #stays that way, since vftNavBarServer() gives it no observer to re-enable it.
    #
    #A plain `disabled` attribute, NOT shinyjs::disabled(). shinyjs::disabled()
    #marks the button with class `shinyjs-disabled` and disables it from JS ~10 ms
    #after load, and _setState() - what enable()/toggleState() run - only ever
    #touches the `disabled` attribute and the `disabled` class. It never removes
    #`shinyjs-disabled`. So the class outlives the state it described: style
    #against it and every button stays greyed out for the whole session however
    #many times the server enables it. (It also races: a button enabled inside
    #that 10 ms window gets disabled again by shinyjs init.) The attribute has
    #neither problem - jQuery removes it on enable, and it is true from parse.
    if(step == "step1" && vftNavAllows(step)) btn
    else shiny::tagAppendAttributes(btn, disabled = NA)
  })

  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      .vft-nav { display:flex; flex-wrap:wrap; gap:4px; padding:6px 0 10px 0; }
      .vft-nav .vft-nav-btn { border-radius:2px; font-size:13px; padding:4px 10px; }
      /* [disabled] is the one true state: set in the markup above, and added and
         removed by shinyjs::toggleState() at runtime. Do not add a class here. */
      .vft-nav .vft-nav-btn[disabled] { opacity:0.4; cursor:not-allowed; }
      .vft-nav .vft-nav-current { font-weight:700; border-bottom:3px solid #2c3e50; }
    ")),
    shiny::tags$div(id = "vftNav", class = "vft-nav", buttons)
  )
}
