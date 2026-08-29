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

  #A queued progress bar goes red and shows the RUNNING job's percentage instead
  #of its own, so the user can see they are waiting and roughly for how long. The
  #class is toggled on one bar at a time rather than restyled globally, because a
  #session can have a red queued bar and a normal running bar on screen together.
  #`.shiny-progress-notification` is the notification style shiny renders by
  #default; the second selector covers style = "old", which uses `.bar`.
  #See .vftQueueWatch() in R/async_helpers.R for what sends the message.
  shiny::tags$style(shiny::HTML("
    .shiny-progress-notification.vft-queued .progress-bar,
    .shiny-progress.vft-queued .bar { background-color:#c0392b; }
  ")),
  shiny::tags$script(shiny::HTML("
    Shiny.addCustomMessageHandler('vft-progress-class', function(m){
      var el = document.getElementById('shiny-progress-' + m.id)
            || document.getElementById(m.id);
      if(el) el.classList[m.add ? 'add' : 'remove'](m.cls);
    });
  ")),

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
#' white separator goes between groups, never inside one. Hitzeminderung is a
#' fifth group, added by hand after the loop rather than through
#' VFT_NAV_GROUPS - it is not a VFT_STEPS entry (see the button's own comment
#' below), so it has nothing for that registry to hold.
vftStepNav <- function(){
  if(!vftNavEnabled()) return(NULL)

  buttonFor <- function(step, chevron = FALSE){
    lab <- sub("^\\d+\\s*", "", VFT_STEPS[[step]]$label)
    #a chevron button paints nothing itself: an empty span underneath it carries
    #the arrow shape, so that the shape and the outline drawn round it can live
    #on two different elements. See the .vft-nav-shape CSS below for why they
    #have to. Absolutely positioned, so it costs the label no layout.
    label <- if(chevron) shiny::tagList(shiny::tags$span(class = "vft-nav-shape"), lab) else lab
    btn <- shiny::actionButton(
      inputId = vftNavInputId(step),
      label   = label,
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

  #groups with more than one button (today just step3/step4/step5) get the
  #chevron treatment - plugged into each other in registry order, like
  #inst/app/www/step2_wsl.png. A single-button group stays a plain rectangle.
  groupDivs <- lapply(VFT_NAV_GROUPS, function(g){
    chevron <- length(g) > 1
    cls <- if(chevron) "vft-nav-group vft-nav-group--chevron" else "vft-nav-group"
    shiny::tags$div(class = cls, lapply(g, buttonFor, chevron = chevron))
  })
  center <- list()
  for(i in seq_along(groupDivs)){
    if(i > 1) center[[length(center) + 1L]] <- shiny::tags$div(class = "vft-nav-sep")
    center[[length(center) + 1L]] <- groupDivs[[i]]
  }

  #Hitzeminderung: not one of VFT_STEPS - it is a second door into newVersions
  #(same tab, same module) that arrives with contextChoice preset to 4 instead
  #of the default 1. See r$vftContextPreset in newVersions_server.R and the
  #vftNav_hitze observer in vftNavBarServer(). Its own group, its own
  #separator, so it reads as a 7th destination rather than a fourth member of
  #the "Neue Versionen" group.
  #same prerequisites as "Neue Versionen" - it is the same page - but its own
  #tooltip text rather than vftStepTooltip("newVersions"), which would say
  #"Neue Versionen – benötigt: ..." under a button labelled "Hitzeminderung".
  hitzePrereq <- vftStepPrereqLabels("newVersions")
  hitzeTooltip <- if(length(hitzePrereq) == 0) "Hitzeminderung" else
    paste0("Hitzeminderung – benötigt: ", paste(hitzePrereq, collapse = ", "))

  center[[length(center) + 1L]] <- shiny::tags$div(class = "vft-nav-sep")
  center[[length(center) + 1L]] <- shiny::tags$div(class = "vft-nav-group",
    shiny::tagAppendAttributes(
      shiny::actionButton(
        inputId = "vftNav_hitze",
        label   = "Hitzeminderung",
        class   = "vft-nav-btn",
        title   = hitzeTooltip
      ),
      disabled = NA
    )
  )

  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      /* 'franklin gothic' is not the name of any installed family - Windows
         ships 'Franklin Gothic Book' / 'Franklin Gothic Medium' and, on this
         machine, the face the loose match landed on is the family's ITALIC
         one. That is where the slanted button labels came from: not a
         font-style rule anywhere (there is none), but the only face the
         requested family could resolve to, which no font-style:normal can
         undo. Naming the real families, with a fallback chain, fixes it. */
      #vftNav { display:flex; align-items:center; background-color:#006268;
                height:100px; color:#ffffff;
                font-family:'Franklin Gothic Book','Franklin Gothic Medium',
                            'Libre Franklin','Arial Narrow',Arial,sans-serif; }
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
      /* height:50px is 50% of the banner's 100px. font-style:normal is belt
         and braces only - the italic labels came from the font-family above,
         not from any font-style rule. border is 2px transparent rather than
         `none` so [disabled] toggling a real border color does not change
         the button's box size. */
      #vftNav .vft-nav-btn { height:50px; background-color:#ffffff; color:#006268;
                             font-weight:700; font-style:normal;
                             border:2px solid transparent; border-radius:2px;
                             padding:0 14px; opacity:0.7; box-sizing:border-box; }
      /* [disabled] is the one true state: set in the markup above, and added and
         removed by shinyjs::toggleState() at runtime. Do not add a class here.
         Reachable steps (opacity 0.7 above) sit half way between this dark-teal
         unreachable look and the fully solid, fully opaque current step -
         Lagune's white/40%-opacity disabled state is gone, replaced with the
         Kontur treatment (outline, transparent fill) in the banner's own dark
         teal rather than white, so an unreachable step recedes into the banner
         instead of sitting on it as a faded white block. */
      #vftNav .vft-nav-btn[disabled] {
        background-color:transparent; color:#00474b; border-color:#00474b;
        opacity:1; cursor:not-allowed;
      }
      /* current step: no underline - a thick white outline standing slightly
         proud of the button, via outline-offset rather than a border (a border
         would eat into the button's own layout box). Full opacity - this is
         the one state .vft-nav-btn's 0.7 default is measured against. */
      #vftNav .vft-nav-current { outline:3.5px solid #ffffff; outline-offset:3.5px; opacity:1; }

      /* Interessengebiete | Wegnetz | Simulation, plugged into each other like
         inst/app/www/step2_wsl.png: each button is an arrow pointing right with
         a matching notch cut into its left edge, nested into the point of the
         one before it (negative margin). z-index falls left to right so each
         earlier tip shows through the next button's notch, the way the
         reference image's chevrons overlap. Everything else about these
         buttons - fill, text colour, opacity, the [disabled] look - is the
         plain .vft-nav-btn rule above; the shape is the only thing this group
         changes. */
      /* The button paints nothing: no background, no border, no clip-path. It is
         just the box and the label. Its .vft-nav-shape span - empty, absolutely
         positioned over that box, z-index:-1 so it lands above the button's own
         background and below the label - carries the arrow via clip-path on its
         ::before. That separation is the whole point; see the outline note below. */
      #vftNav .vft-nav-group--chevron { gap:0; }
      #vftNav .vft-nav-group--chevron .vft-nav-btn {
        background-color:transparent; border:none;
        margin-left:-15px; padding:0 22px 0 30px;
        position:relative;
      }
      #vftNav .vft-nav-shape { position:absolute; inset:0; z-index:-1;
                               pointer-events:none; }
      #vftNav .vft-nav-shape::before {
        content:''; position:absolute; inset:0; background-color:#ffffff;
        clip-path: polygon(0 0, calc(100% - 16px) 0, 100% 50%,
                            calc(100% - 16px) 100%, 0 100%, 16px 50%);
      }
      /* first: no notch to plug into anything before it */
      #vftNav .vft-nav-group--chevron .vft-nav-btn:first-child {
        margin-left:0; padding-left:18px;
      }
      #vftNav .vft-nav-group--chevron .vft-nav-btn:first-child .vft-nav-shape::before {
        clip-path: polygon(0 0, calc(100% - 16px) 0, 100% 50%,
                            calc(100% - 16px) 100%, 0 100%);
      }
      /* last (Simulation today): keeps the notch that plugs into the button
         before it, but drops the point - nothing plugs into ITS right side,
         so it has no reason to angle there. Flat-right padding matches the
         plain (non-chevron) buttons' 14px rather than the point's 22px. */
      #vftNav .vft-nav-group--chevron .vft-nav-btn:last-child { padding-right:14px; }
      #vftNav .vft-nav-group--chevron .vft-nav-btn:last-child .vft-nav-shape::before {
        clip-path: polygon(0 0, 100% 0, 100% 100%, 0 100%, 16px 50%);
      }
      #vftNav .vft-nav-group--chevron .vft-nav-btn:nth-child(1) { z-index:3; }
      #vftNav .vft-nav-group--chevron .vft-nav-btn:nth-child(2) { z-index:2; }
      #vftNav .vft-nav-group--chevron .vft-nav-btn:nth-child(3) { z-index:1; }

      /* ---- outlining the arrow ------------------------------------------
         Nothing painted outside a clip survives it. `border` and `outline` are
         geometry of the element's plain rectangular box, so on a chevron they
         hold on the flat top and bottom and vanish along the diagonals; and
         `filter: drop-shadow()` is applied BEFORE clipping, so putting one on
         the clipped element itself just gets the halo cut back off.
         The fix is the split above: clip-path sits on .vft-nav-shape::before,
         the filter sits on .vft-nav-shape, which has no clip of its own. A
         drop-shadow is computed from its subtree's rendered alpha - which by
         then is exactly the arrow - so the halo follows every edge, diagonals
         and point included, and nothing clips it away.
         One drop-shadow smears the shape in one direction; the chain unions
         them, so the outline is the shape grown by the Minkowski sum of the
         offsets. Four offset PAIRS at 45 deg to each other (x, y, and the two
         diagonals) sum to a near-circular octagon: with the diagonal pair at
         0.707 of the axis pair, the ring thickness varies under 10% with
         direction, and total thickness is 2.414x the axis offset. That is
         where these numbers come from - 1.24/0.88 gives a 3px ring, 0.83/0.59
         a 2px one. Do not just add more directions: every extra pair grows
         the ring, which is what made the first attempt at this look swollen.
         Chaining two such stacks nests two rings, so the current step gets
         white face -> 3px teal gap -> 3px white ring: the same reading as the
         plain buttons' `outline:3px #fff; outline-offset:3px`, drawn a way
         clip-path cannot eat. */
      #vftNav .vft-nav-group--chevron .vft-nav-btn[disabled] .vft-nav-shape::before {
        background-color:#006268;
      }
      #vftNav .vft-nav-group--chevron .vft-nav-btn[disabled] .vft-nav-shape {
        filter:
          drop-shadow(0.83px 0 0 #00474b)      drop-shadow(-0.83px 0 0 #00474b)
          drop-shadow(0 0.83px 0 #00474b)      drop-shadow(0 -0.83px 0 #00474b)
          drop-shadow(0.59px 0.59px 0 #00474b) drop-shadow(-0.59px -0.59px 0 #00474b)
          drop-shadow(0.59px -0.59px 0 #00474b) drop-shadow(-0.59px 0.59px 0 #00474b);
      }
      /* the ring stands proud of the button, so it would be overlapped by the
         neighbour plugged into it - the current step is lifted above both its
         siblings' z-index instead. .vft-nav-btn is in the selector only to
         match the nth-child rules' specificity; it comes later, so it wins. */
      #vftNav .vft-nav-group--chevron .vft-nav-btn.vft-nav-current {
        outline:none; z-index:5;
      }
      #vftNav .vft-nav-group--chevron .vft-nav-current .vft-nav-shape {
        filter:
          drop-shadow(1.24px 0 0 #006268)      drop-shadow(-1.24px 0 0 #006268)
          drop-shadow(0 1.24px 0 #006268)      drop-shadow(0 -1.24px 0 #006268)
          drop-shadow(0.88px 0.88px 0 #006268) drop-shadow(-0.88px -0.88px 0 #006268)
          drop-shadow(0.88px -0.88px 0 #006268) drop-shadow(-0.88px 0.88px 0 #006268)
          drop-shadow(1.24px 0 0 #ffffff)      drop-shadow(-1.24px 0 0 #ffffff)
          drop-shadow(0 1.24px 0 #ffffff)      drop-shadow(0 -1.24px 0 #ffffff)
          drop-shadow(0.88px 0.88px 0 #ffffff) drop-shadow(-0.88px -0.88px 0 #ffffff)
          drop-shadow(0.88px -0.88px 0 #ffffff) drop-shadow(-0.88px 0.88px 0 #ffffff);
      }
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
