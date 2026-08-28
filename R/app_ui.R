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
                     )
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
# the largest single item in Stage 1. Putting the bar back as an output would
# hand most of that saving straight back.
#
# So the markup is built once, here, with the buttons literally in the page.
# Nothing about it is reactive. The only things that change at runtime are the
# disabled attribute and one CSS class, and those go out as shinyjs messages
# from a single observe() in vftNavBarServer() - which sends only the ones that
# actually changed.

#' The step nav bar, or NULL when `VFT_NAV` is off.
#'
#' This is now ALSO the page banner: it used to be six copies of the same
#' teal strip (language select, title, logo, help/info) baked into every
#' step's own UI - see the note above `vftBannerImg()` in R/banner_helpers.R -
#' and the nav buttons sat in a separate bar below it. They are merged here,
#' with the buttons where the per-step banner image used to be, because a bar
#' that is rendered once at app level is exactly what a shared banner needs to
#' be: six copies of an identical strip were never buying anything.
#'
#' Buttons carry unnamespaced ids (`vftNav_step1`, ...) because the bar lives at
#' app level, outside the tabsetPanel and outside every module namespace. The
#' language select and help/info buttons are unnamespaced for the same reason
#' - `languageSelect`, `helpButton`, `infoButton` - and are NOT what the six
#' steps' own servers listen to. Each step still has its own namespaced
#' languageSelect_N / helpButtonN / infoButtonN, wired to that step's existing
#' (unchanged) logic; they are just hidden now instead of visible per step.
#' `vftNavBannerProxyServer()` in R/navigation.R forwards a click or a
#' selection on the one visible control to whichever step's hidden proxy
#' belongs to `r$navStep`, so nothing about the six steps' language/help/info
#' behaviour had to be touched to hoist the controls up here.
#'
#' Everything except step 1 ships disabled: at session start step 1 is the only
#' step whose `needs` are met, and starting from the correct state means the user
#' never sees a frame of live buttons before the first flush corrects them.
#'
#' The tooltip is static too. `needs` and VFT_KEY_SOURCE are both compile-time
#' constants, so "which step would I have to do first" can be answered while the
#' page is being built rather than by a message per state change.
#'
#' Button labels drop the leading digit VFT_STEPS carries ("1 Gebiet" ->
#' "Gebiet"): that digit is still useful elsewhere (tooltips, the "needs"
#' messages in steps.R), just not on the button itself.
#'
#' The four visual groups - Gebiet | Sensibilität | Interessengebiete,
#' Wegnetz, Simulation | Neue Versionen - are VFT_NAV_GROUPS below; a thick
#' white separator goes between groups, never inside one.
vftStepNav <- function(){
  if(!vftNavEnabled()) return(NULL)

  buttonFor <- function(step){
    btn <- shiny::actionButton(
      inputId = vftNavInputId(step),
      label   = sub("^\\d+\\s*", "", VFT_STEPS[[step]]$label),
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
  }

  groupDivs <- lapply(VFT_NAV_GROUPS, function(g){
    shiny::tags$div(class = "vft-nav-group", lapply(g, buttonFor))
  })
  center <- list()
  for(i in seq_along(groupDivs)){
    if(i > 1) center[[length(center) + 1L]] <- shiny::tags$div(class = "vft-nav-sep")
    center[[length(center) + 1L]] <- groupDivs[[i]]
  }

  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      #vftNav { display:flex; align-items:center; background-color:#006268;
                height:100px; color:#ffffff; font-family:'franklin gothic'; }
      #vftNav .vft-nav-left  { flex: 0 0 auto; padding-left:15px; }
      #vftNav .vft-nav-left h2 { margin-top:-15px; }
      #vftNav .vft-nav-center { flex: 1 1 auto; display:flex; align-items:center;
                                justify-content:center; }
      #vftNav .vft-nav-right { flex: 0 0 auto; padding-right:15px; display:flex;
                               align-items:center; gap:10px; }
      #vftNav .vft-nav-group { display:flex; gap:4px; }
      /* the vertical separator between groups - a bit of space either side, then
         a thick white bar, never placed between two buttons of the same group */
      #vftNav .vft-nav-sep { width:3px; align-self:center; height:32px;
                             background:#ffffff; margin:0 14px; border-radius:2px; }
      /* height:50px is 50% of the banner's 100px */
      #vftNav .vft-nav-btn { height:50px; background-color:#ffffff; color:#006268;
                             font-weight:700; border:none; border-radius:2px;
                             padding:0 14px; }
      /* [disabled] is the one true state: set in the markup above, and added and
         removed by shinyjs::toggleState() at runtime. Do not add a class here. */
      #vftNav .vft-nav-btn[disabled] { opacity:0.4; cursor:not-allowed; }
      /* current step: no underline - a thick white outline standing slightly
         proud of the button, via outline-offset rather than a border (a border
         would eat into the button's own layout box). */
      #vftNav .vft-nav-current { outline:3px solid #ffffff; outline-offset:3px; }
    ")),
    shiny::tags$div(id = "vftNav",
      shiny::tags$div(class = "vft-nav-left",
        shiny::HTML("<title>Visitor Flow Tool</title>"),
        shiny::selectInput(inputId = "languageSelect", label = NULL,
                           choices = c("Deutsch" = "de", "Français" = "fr", "English" = "en"),
                           selected = "de", width = 100),
        shiny::h2("Besucherlenkungs-Tool: ")
      ),
      shiny::tags$div(class = "vft-nav-center", center),
      shiny::tags$div(class = "vft-nav-right",
        shiny::HTML("
          <img src ='www/BiodivCenterLogo_w.png' style = 'width:200px; height:75%; object-fit:contain;'>
        "),
        shiny::tags$div(
          shiny::actionButton(inputId = "helpButton", label = "", style = "width: 30px; height: 30px;
background: url('helpIcon.png');  background-size: cover; background-position: center; border:none"),
          shiny::div(style = "margin-top:5px"),
          shiny::actionButton(inputId = "infoButton", label = "", style = "width: 30px; height: 30px;
background: url('infoIcon.png');  background-size: cover; background-position: center; border: none")
        )
      )
    ),
    shiny::tags$div(style = "display:inline-block;height:1px;color:#006268; font-family: 'franklin gothic';margin-top:4px;margin-left:15px",
      shiny::h5("app designer/kontakt: johan.frueh@wsl.ch")
    )
  )
}
