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
  #
  #Handed the Translator so its labels, tooltips and title can carry all three
  #languages as data attributes - it is static markup, so it cannot re-render
  #when the language changes and swaps them on the client instead.
  vftStepNav(i18n),

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
#'
#' ---- fitting the buttons next to everything else ----------------------------
#'
#' Seven nav buttons (five steps, "Neue Versionen", "Hitzeminderung") plus the
#' title, the language selector, the logo and the two icon buttons did not fit
#' on a 1280- or 1440-wide monitor: the row overflowed and the logo was pushed
#' off the end. Nothing here is smaller because "smaller looks better" - every
#' size in the strip is now one of the eleven custom properties at the top of
#' the CSS below, each a `clamp(floor, ideal-in-vw, ceiling)`, so the whole
#' banner is ONE scale that tracks the window and "make it fit at 1280" is a
#' coefficient change in one place rather than an edit at twenty. At 2560 every
#' clamp is at its maximum and the bar is the size it has always been.
#'
#' Structural changes on top of the scale, in order of how much room they buy:
#' the language selector moved from the left (where it sat stacked above the
#' title, on a `margin-top:-15px` that existed only to undo the h2's own margin)
#' into the right-hand cluster with the rest of the chrome; the title is a plain
#' span with its own font size rather than an `<h2>`; the separators' fixed
#' 14px-a-side margins became the parent flex gap, so they scale too; and the
#' logo and the two icon buttons lost their inline pixel sizes.
#'
#' ---- and translating them ---------------------------------------------------
#'
#' The labels, the tooltips and the title are i18n now. They could NOT be
#' `i18n$t()` calls in this markup and stay translated, for exactly the reason
#' the bar is not an output: it is built once per session, so whatever language
#' it is built in is the language it would keep. So each label carries ALL THREE
#' translations, as `data-i18n-de` / `-fr` / `-en` on the label span and
#' `data-tip-*` on the button, and one small script at the bottom of this
#' function swaps `textContent` and `title` when `languageSelect` changes. It is
#' entirely client-side: no output, no custom message handler, not one extra
#' byte over the socket, and the swap is instant.
#'
#' The keys are `:nav_<step>:` (plus `:nav_hitze:`, `:nav_needs:`, `:nav_lang:`)
#' in the translation CSVs; the title reuses the existing
#' `Besucherlenkungs-Tool: ` row. A key the CSVs do not carry falls back to the
#' German literal already in VFT_STEPS, so adding a step does not require
#' touching the CSVs first - it just shows German until it does.
vftStepNav <- function(i18n = NULL){
  if(!vftNavEnabled()) return(NULL)

  langs <- c("de", "fr", "en")

  #every language of one key, with the German literal standing in wherever the
  #CSVs have no row (vftTrAll() hands back the key itself in that case).
  navTr <- function(key, fallback){
    out <- vftTrAll(i18n, key, langs)
    out[is.na(out) | out == key] <- fallback
    out
  }

  #Two forms of each step's name are needed. The button shows the label with its
  #leading digit stripped ("1 Gebiet" -> "Gebiet"), which is what the CSV rows
  #hold; the tooltip wants the digit back, because "benötigt: 2 Sensibilität"
  #tells the user which button to go and press and "benötigt: Sensibilität"
  #makes them look for it.
  shortLab <- lapply(stats::setNames(names(VFT_STEPS), names(VFT_STEPS)),
                     function(s) navTr(paste0(":nav_", s, ":"),
                                       sub("^\\d+\\s*", "", VFT_STEPS[[s]]$label)))
  #setNames, because paste() drops names and every one of these vectors is
  #indexed by language name from here on.
  fullLab <- lapply(names(shortLab), function(s){
    digit <- regmatches(VFT_STEPS[[s]]$label, regexpr("^\\d+", VFT_STEPS[[s]]$label))
    if(length(digit)) stats::setNames(paste(digit, shortLab[[s]]), langs) else shortLab[[s]]
  })
  names(fullLab) <- names(shortLab)

  needsWord <- navTr(":nav_needs:", "benötigt")

  #the translated form of vftStepTooltip(), written once per language. Same
  #sentence, same en dash, same registry ordering - vftStepPrereqSteps() is the
  #half of vftStepPrereqLabels() that answers in step names rather than German.
  tooltipFor <- function(step, label = fullLab[[step]]){
    prereq <- vftStepPrereqSteps(step)
    if(length(prereq) == 0) return(label)
    stats::setNames(vapply(langs, function(l){
      paste0(label[[l]], " – ", needsWord[[l]], ": ",
             paste(vapply(prereq, function(s) fullLab[[s]][[l]], character(1)),
                   collapse = ", "))
    }, character(1)), langs)
  }

  #`data-i18n-de` and friends. setNames on a list, because those names carry
  #dashes and so cannot be written as R argument names.
  withData <- function(tag, prefix, values){
    do.call(shiny::tagAppendAttributes,
            c(list(tag), stats::setNames(as.list(unname(values)),
                                         paste0(prefix, names(values)))))
  }

  buttonFor <- function(step, chevron = FALSE){
    lab <- withData(shiny::tags$span(class = "vft-nav-lab", shortLab[[step]][["de"]]),
                    "data-i18n-", shortLab[[step]])
    #a chevron button paints nothing itself: an empty span underneath it carries
    #the arrow shape, so that the shape and the outline drawn round it can live
    #on two different elements. See the .vft-nav-shape CSS below for why they
    #have to. Absolutely positioned, so it costs the label no layout.
    label <- if(chevron) shiny::tagList(shiny::tags$span(class = "vft-nav-shape"), lab) else lab
    tips <- tooltipFor(step)
    btn <- shiny::actionButton(
      inputId = vftNavInputId(step),
      label   = label,
      class   = "vft-nav-btn",
      title   = tips[["de"]]
    )
    btn <- withData(btn, "data-tip-", tips)
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
  #same prerequisites as "Neue Versionen" - it is the same page - so its tooltip
  #is built from newVersions' prereqs but under its OWN label: passing
  #tooltipFor("newVersions") straight through would say "Neue Versionen –
  #benötigt: ..." under a button labelled "Hitzeminderung".
  hitzeLab  <- navTr(":nav_hitze:", "Hitzeminderung")
  hitzeTips <- tooltipFor("newVersions", label = hitzeLab)

  center[[length(center) + 1L]] <- shiny::tags$div(class = "vft-nav-sep")
  center[[length(center) + 1L]] <- shiny::tags$div(class = "vft-nav-group",
    shiny::tagAppendAttributes(
      withData(
        shiny::actionButton(
          inputId = "vftNav_hitze",
          label   = withData(shiny::tags$span(class = "vft-nav-lab", hitzeLab[["de"]]),
                             "data-i18n-", hitzeLab),
          class   = "vft-nav-btn",
          title   = hitzeTips[["de"]]
        ),
        "data-tip-", hitzeTips),
      disabled = NA
    )
  )

  #the app title. Reuses the translation row the six step banner images already
  #used; the trailing ": " that row carries belonged to the old banner's layout
  #and is dropped here.
  titleTxt <- sub("[[:space:]:]+$", "",
                  navTr("Besucherlenkungs-Tool: ", "Besucherlenkungs-Tool"))
  langTip  <- navTr(":nav_lang:", "Sprache")

  shiny::tagList(
    shiny::tags$style(shiny::HTML("
      /* ---- one scale for the whole banner --------------------------------
         Every size below is one of these, and every one is
         clamp(floor, ideal, ceiling): the vw term tracks the monitor, the
         floor keeps a 1280-wide laptop readable, the ceiling stops a 4K panel
         inflating the strip. At 2560 all eleven sit at their maximum, which is
         the size this banner has always been.

         vw, not a container query unit: the banner is always exactly the page
         width so the two agree, and cqw would need `container-type` on
         #vftNav - size containment on an element the rest of the app's CSS
         does not expect it on - for no gain. */
      #vftNav {
        --nav-h:     clamp(70px,   5.20vw, 100px);
        --nav-font:  clamp(10.5px, 0.80vw,  15px);
        --nav-pad:   clamp(6px,    0.62vw,  14px);
        --nav-btn-h: clamp(34px,   2.55vw,  50px);
        --nav-notch: clamp(9px,    0.83vw,  16px);
        --nav-sep:   clamp(4px,    0.72vw,  14px);
        --nav-gap:   clamp(2px,    0.24vw,   4px);
        --nav-title: clamp(12.5px, 1.02vw,  22px);
        --nav-logo:  clamp(88px,   8.60vw, 190px);
        --nav-icon:  clamp(19px,   1.55vw,  30px);
        --nav-edge:  clamp(8px,    0.78vw,  15px);
      }

      /* 'franklin gothic' is not the name of any installed family - Windows
         ships 'Franklin Gothic Book' / 'Franklin Gothic Medium' and, on this
         machine, the face the loose match landed on is the family's ITALIC
         one. That is where the slanted button labels came from: not a
         font-style rule anywhere (there is none), but the only face the
         requested family could resolve to, which no font-style:normal can
         undo. Naming the real families, with a fallback chain, fixes it. */
      #vftNav { display:flex; align-items:center; background-color:#006268;
                height:var(--nav-h); color:#ffffff;
                gap:var(--nav-sep); padding:0 var(--nav-edge);
                box-sizing:border-box;
                font-family:'Franklin Gothic Book','Franklin Gothic Medium',
                            'Libre Franklin','Arial Narrow',Arial,sans-serif; }

      /* Shrink priority, and it matters: the title is the ONLY zone with a
         non-zero flex-shrink, so any pressure the clamp scale has not already
         absorbed lands on it and nowhere else. 'Besucherlenkungs-Tool' breaks
         at its own hyphen and 'Outil de Gestion de Visiteurs' at its spaces,
         so what an absurdly narrow window costs is two lines of title - never
         an ellipsised button label, never a logo pushed off the end.
         With `flex:1 1 auto` on the centre instead, shrink is distributed in
         proportion to each zone's base width, so the buttons - much the widest
         zone - would take about four fifths of it and ellipsise first. */
      #vftNav .vft-nav-left  { flex:0 1 auto; min-width:0; }
      #vftNav .vft-nav-title { font-size:var(--nav-title); font-weight:700;
                               line-height:1.1; margin:0;
                               overflow-wrap:break-word; }
      #vftNav .vft-nav-center { flex:1 0 auto; min-width:0; display:flex;
                                align-items:center; justify-content:center;
                                gap:var(--nav-sep); }
      #vftNav .vft-nav-right { flex:0 0 auto; display:flex; align-items:center;
                               gap:calc(var(--nav-sep) * 0.7); }
      #vftNav .vft-nav-group { display:flex; gap:var(--nav-gap); min-width:0; }
      /* the vertical separator between groups - a thick white bar, never placed
         between two buttons of the same group. Its old 14px-a-side margins are
         the parent's flex gap now, so they scale with everything else. */
      #vftNav .vft-nav-sep { width:3px; align-self:center; flex:0 0 auto;
                             height:calc(var(--nav-btn-h) * 0.64);
                             background:#ffffff; border-radius:2px; }
      /* font-style:normal is belt and braces only - the italic labels came from
         the font-family above, not from any font-style rule. border is 2px
         transparent rather than `none` so [disabled] toggling a real border
         colour does not change the button's box size.
         flex:0 1 auto + min-width:0 is the safety net under the clamp scale: if
         some future translation is longer than the scale allows for, that one
         label ellipsises - and its tooltip still names it in full - instead of
         the row overflowing and taking the logo with it. */
      #vftNav .vft-nav-btn { height:var(--nav-btn-h); background-color:#ffffff;
                             color:#006268; font-weight:700; font-style:normal;
                             font-size:var(--nav-font); line-height:1;
                             border:2px solid transparent; border-radius:2px;
                             padding:0 var(--nav-pad); opacity:0.7;
                             box-sizing:border-box; flex:0 1 auto; min-width:0;
                             display:flex; align-items:center; justify-content:center; }
      /* the button is a flex box (above) purely so the ellipsis has something to
         be measured against: shiny wraps an actionButton's label in its own
         `.action-label` span, and a block inside an inline span in a non-flex
         button has no width to overflow. Absolutely-positioned children are out
         of flow, so .vft-nav-shape is unaffected. */
      #vftNav .vft-nav-btn .action-label { min-width:0; overflow:hidden; }
      #vftNav .vft-nav-lab { display:block; white-space:nowrap; overflow:hidden;
                             text-overflow:ellipsis; }
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
         the one state .vft-nav-btn's 0.7 default is measured against. Ring and
         offset scale with the button, or at 1280 the offset alone would be a
         third of the gap between two groups. */
      #vftNav .vft-nav-current { outline:calc(var(--nav-btn-h) * 0.07) solid #ffffff;
                                 outline-offset:calc(var(--nav-btn-h) * 0.07);
                                 opacity:1; }

      /* ---- the right-hand cluster ---------------------------------------
         The language selector lives here now, not on the left. It used to sit
         stacked above the title inside .vft-nav-left, which is what the
         `margin-top:-15px` on that h2 was there to undo; moving it here puts
         all the chrome in one place and gives the title the whole left zone. */
      #vftNav .vft-nav-lang { width:calc(var(--nav-logo) * 0.62); min-width:62px; }
      #vftNav .vft-nav-lang .form-group { margin-bottom:0; }
      /* `.selectize-input` is what actually renders in the app - shiny turns the
         <select> into a selectize widget - and the other two cover the plain
         <select> that is left if selectize does not initialise. */
      #vftNav .vft-nav-lang .selectize-input,
      #vftNav .vft-nav-lang select,
      #vftNav .vft-nav-lang .form-control {
        min-height:0; height:calc(var(--nav-btn-h) * 0.62);
        line-height:calc(var(--nav-btn-h) * 0.62);
        padding:0 20px 0 8px; font-size:var(--nav-font);
        border-radius:2px; border:none;
      }
      #vftNav .vft-nav-lang .selectize-input:after { margin-top:-2px; }
      #vftNav .vft-nav-logo { width:var(--nav-logo); height:70%;
                              object-fit:contain; flex:0 0 auto; }
      /* the two icon buttons had their 30px squares written inline. They scale
         with the bar now, and stay STACKED because vertical space is what this
         banner has spare and horizontal space is exactly what it does not. */
      #vftNav .vft-nav-icons { display:flex; flex-direction:column;
                               gap:calc(var(--nav-icon) * 0.17); }
      /* :hover/:focus repeated because bootstrap's own .btn-default:hover sets a
         grey background, which would show as a box round the icon. */
      #vftNav .vft-nav-icon,
      #vftNav .vft-nav-icon:hover,
      #vftNav .vft-nav-icon:focus,
      #vftNav .vft-nav-icon:active { width:var(--nav-icon); height:var(--nav-icon);
                              padding:0; border:none; background-color:transparent;
                              box-shadow:none;
                              background-size:cover; background-position:center; }

      /* Interessengebiete | Wegnetz | Simulation, plugged into each other like
         inst/app/www/step2_wsl.png: each button is an arrow pointing right with
         a matching notch cut into its left edge, nested into the point of the
         one before it (negative margin). z-index falls left to right so each
         earlier tip shows through the next button's notch, the way the
         reference image's chevrons overlap. Everything else about these
         buttons - fill, text colour, opacity, the [disabled] look - is the
         plain .vft-nav-btn rule above; the shape is the only thing this group
         changes. Every padding and the overlap derive from --nav-notch, so the
         whole chevron geometry scales as one piece with the rest of the bar. */
      /* The button paints nothing: no background, no border, no clip-path. It is
         just the box and the label. Its .vft-nav-shape span - empty, absolutely
         positioned over that box, z-index:-1 so it lands above the button's own
         background and below the label - carries the arrow via clip-path on its
         ::before. That separation is the whole point; see the outline note below. */
      #vftNav .vft-nav-group--chevron { gap:0; }
      #vftNav .vft-nav-group--chevron .vft-nav-btn {
        background-color:transparent; border:none;
        margin-left:calc(var(--nav-notch) * -1 + 1px);
        padding:0 calc(var(--nav-notch) + var(--nav-pad) * 0.45)
                0 calc(var(--nav-notch) + var(--nav-pad));
        position:relative;
      }
      #vftNav .vft-nav-shape { position:absolute; inset:0; z-index:-1;
                               pointer-events:none; }
      #vftNav .vft-nav-shape::before {
        content:''; position:absolute; inset:0; background-color:#ffffff;
        clip-path: polygon(0 0, calc(100% - var(--nav-notch)) 0, 100% 50%,
                            calc(100% - var(--nav-notch)) 100%, 0 100%,
                            var(--nav-notch) 50%);
      }
      /* first: no notch to plug into anything before it */
      #vftNav .vft-nav-group--chevron .vft-nav-btn:first-child {
        margin-left:0;
        padding-left:calc(var(--nav-notch) * 0.2 + var(--nav-pad));
      }
      #vftNav .vft-nav-group--chevron .vft-nav-btn:first-child .vft-nav-shape::before {
        clip-path: polygon(0 0, calc(100% - var(--nav-notch)) 0, 100% 50%,
                            calc(100% - var(--nav-notch)) 100%, 0 100%);
      }
      /* last (Simulation today): keeps the notch that plugs into the button
         before it, but drops the point - nothing plugs into ITS right side,
         so it has no reason to angle there. Flat-right padding matches the
         plain (non-chevron) buttons' --nav-pad rather than the point's. */
      #vftNav .vft-nav-group--chevron .vft-nav-btn:last-child { padding-right:var(--nav-pad); }
      #vftNav .vft-nav-group--chevron .vft-nav-btn:last-child .vft-nav-shape::before {
        clip-path: polygon(0 0, 100% 0, 100% 100%, 0 100%, var(--nav-notch) 50%);
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
         plain buttons' outline + outline-offset, drawn a way clip-path cannot
         eat. These stay in fixed px while everything else scales: 2px is the
         thinnest a drop-shadow union reads cleanly at, so shrinking the ring
         with the bar would simply make it vanish at 1280. */
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
      /* `current` can land on a button that is MOMENTARILY [disabled] too - the
         nav bar's own observe() greys a step while the session is busy, and
         landing on the next step (e.g. step 4's confirm handing off to step 5)
         can do that in the very same tick the step becomes current, while an
         earlier step's provider is still resolving. Without this, the
         `[disabled] .vft-nav-shape` rule above and this current-ring rule tie in
         specificity (chevron + btn + one attr/class each) and the disabled,
         dark-teal ring silently wins - the step LOOKS unreached even though
         r$navStep says otherwise. These two rules add .vft-nav-current on top of
         [disabled]'s own selector so 'you are here' always outranks 'not
         reachable yet'. */
      #vftNav .vft-nav-group--chevron .vft-nav-btn[disabled].vft-nav-current .vft-nav-shape::before {
        background-color:#ffffff;
      }
      #vftNav .vft-nav-group--chevron .vft-nav-btn[disabled].vft-nav-current .vft-nav-shape {
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

      /* ---- below every desktop monitor ----------------------------------
         Not a design target: 1152 is under the narrowest desktop panel in use,
         and the clamp scale above is sized to fit German, French AND English
         at 1280. This is the safety net for a half-width window or an old 1024
         screen - the buttons take a row of their own rather than ellipsising,
         and the strip grows downwards instead of the logo being pushed off. */
      @media (max-width: 1152px){
        #vftNav { flex-wrap:wrap; height:auto;
                  padding-top:8px; padding-bottom:8px; row-gap:8px; }
        #vftNav .vft-nav-center { order:3; flex:1 0 100%; justify-content:flex-start; }
        #vftNav .vft-nav-left   { flex:1 1 auto; }
      }

      .vft-nav-contact { color:#006268; margin:4px 0 0 15px; font-size:12px;
                         font-family:'Franklin Gothic Book','Libre Franklin',
                                     Arial,sans-serif; }
    ")),
    #kept from the old markup: it is what names the browser tab. Outside the
    #flex row, so it cannot become a flex item of it.
    shiny::HTML("<title>Visitor Flow Tool</title>"),
    shiny::tags$div(id = "vftNav",
      shiny::tags$div(class = "vft-nav-left",
        withData(shiny::tags$div(class = "vft-nav-title", titleTxt[["de"]]),
                 "data-i18n-", titleTxt)
      ),
      shiny::tags$div(class = "vft-nav-center", center),
      shiny::tags$div(class = "vft-nav-right",
        withData(
          shiny::tags$div(class = "vft-nav-lang", title = langTip[["de"]],
            shiny::selectInput(inputId = "languageSelect", label = NULL,
                               choices = c("Deutsch" = "de", "Français" = "fr",
                                           "English" = "en"),
                               selected = "de", width = "100%")
          ),
          "data-tip-", langTip),
        shiny::tags$img(class = "vft-nav-logo", src = "www/BiodivCenterLogo_w.png"),
        shiny::tags$div(class = "vft-nav-icons",
          shiny::actionButton(inputId = "helpButton", label = "",
                              class = "vft-nav-icon",
                              style = "background-image:url('helpIcon.png');"),
          shiny::actionButton(inputId = "infoButton", label = "",
                              class = "vft-nav-icon",
                              style = "background-image:url('infoIcon.png');")
        )
      )
    ),
    #### swapping the language, entirely on the client ####
    #
    #Every label and tooltip in the bar carries all three languages as data
    #attributes (see the note above this function). This walks them when
    #`languageSelect` changes and writes textContent / title. `shiny:inputchanged`
    #covers both a click on the selector and an updateSelectInput() from the
    #server, and it is the only listener: no output, no custom message handler,
    #nothing over the socket for a language change that the app was not already
    #sending anyway.
    shiny::tags$script(shiny::HTML("
      (function(){
        function vftNavLang(lang){
          var nav = document.getElementById('vftNav');
          if(!nav || !lang) return;
          nav.querySelectorAll('[data-i18n-' + lang + ']').forEach(function(el){
            el.textContent = el.getAttribute('data-i18n-' + lang);
          });
          nav.querySelectorAll('[data-tip-' + lang + ']').forEach(function(el){
            el.setAttribute('title', el.getAttribute('data-tip-' + lang));
          });
          nav.setAttribute('lang', lang);
        }
        window.vftNavLang = vftNavLang;
        $(document).on('shiny:inputchanged', function(e){
          if(e.name === 'languageSelect') vftNavLang(e.value);
        });
      })();
    ")),
    shiny::tags$div(class = "vft-nav-contact",
                    "app designer/kontakt: johan.frueh@wsl.ch")
  )
}
