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
#' The three visual groups - Gebiet wählen | Sensibilität der Biodiversität |
#' ZG definieren, ZG bearbeiten, Naherholung simulieren, Szenarien erstellen -
#' are VFT_NAV_GROUPS below; a thick white
#' separator goes between groups, never inside one. Hitzeminderung is a fourth
#' group, added by hand after the loop rather than through VFT_NAV_GROUPS - it
#' is not a VFT_STEPS entry (see the button's own comment below), so it has
#' nothing for that registry to hold.
#'
#' ---- folding the third group ------------------------------------------------
#'
#' That third group ships FOLDED, behind one "Simulation" button, so the bar
#' offers four choices rather than seven to a user who has not drawn anything
#' yet. Both forms are always in the markup and one class on the bar chooses -
#' which is the only way to do it here, since the bar cannot re-render. See
#' VFT_NAV_FOLD in R/steps.R for the behaviour and vftNavBarServer() in
#' R/navigation.R for the single place that class is written.
#'
#' ---- fitting the buttons next to everything else ----------------------------
#'
#' Seven nav buttons (five steps, "Szenarien erstellen", "Hitzeminderung") plus
#' the
#' title, the language selector, the logo and the two icon buttons did not fit
#' on a 1280- or 1440-wide monitor: the row overflowed and the logo was pushed
#' off the end. Folding buys some of that back - four buttons and three
#' separators to start with, and one separator fewer than before when unfolded -
#' but the scale below is still what has to fit all seven, because unfolded is
#' where the user spends most of the walk. Nothing here is smaller because
#' "smaller looks better" - every
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
  #leading digit stripped ("3 ZG definieren" -> "ZG definieren"), which is what
  #the CSV rows hold; the tooltip wants the digit back, because "benötigt:
  #3 ZG definieren" tells the user which button to go and press and "benötigt:
  #ZG definieren" makes them look for it.
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
  #`prereq` is an override, for the one button whose prerequisites are not its
  #tab's: Hitzeminderung. See vftHitzePrereqSteps() in R/steps.R.
  tooltipFor <- function(step, label = fullLab[[step]],
                         prereq = vftStepPrereqSteps(step)){
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

  #### the button that stands in for the folded group ####
  #
  #Its OWN label and its OWN prerequisites, both as overrides, exactly like
  #Hitzeminderung below. It borrows step 5's name ("Naherholung simulieren")
  #because that is what the group as a whole is for - but NOT step 5's tooltip:
  #step 5 needs the confirmed network and the threshold, so tooltipFor("step5")
  #would name steps 3 and 4 on the one button whose job is to open them. It takes
  #step 3's prerequisites instead, which is the same test vftNavFoldTarget()
  #enables it by: "Naherholung simulieren - benoetigt: 1 Gebiet waehlen".
  foldLab  <- shortLab[[VFT_NAV_FOLD_LABEL]]
  foldTips <- tooltipFor(VFT_NAV_FOLD_LABEL, label = foldLab,
                         prereq = vftStepPrereqSteps(VFT_NAV_FOLD[[1]]))
  foldBtn <- shiny::tagAppendAttributes(
    withData(
      shiny::actionButton(
        inputId = VFT_NAV_FOLD_ID,
        label   = withData(shiny::tags$span(class = "vft-nav-lab", foldLab[["de"]]),
                           "data-i18n-", foldLab),
        class   = "vft-nav-btn",
        title   = foldTips[["de"]]
      ),
      "data-tip-", foldTips),
    disabled = NA
  )

  #groups with more than one button (today just VFT_NAV_FOLD) get the chevron
  #treatment - plugged into each other in registry order, like
  #inst/app/www/step2_wsl.png. A single-button group stays a plain rectangle.
  #
  #The foldable group emits TWO divs rather than one: the stand-in button, and
  #the chain it opens into. Both are always in the page, and the
  #`.vft-nav-folded` class on the bar picks which one is displayed - see the CSS
  #below.
  #
  #The fold button is deliberately NOT a child of the chevron div. That group's
  #whole shape is :first-child / :last-child / :nth-child rules, and those are
  #structural: a fifth child shifts every one of them even while it is
  #display:none.
  #
  #A tagList, not two entries in this list, because the separator loop below
  #interleaves one separator per ENTRY - and there is no separator inside a
  #group. tagList is flattened into .vft-nav-center, so the two divs are still
  #plain flex items of it and pick up its gap.
  #
  #Every member gets `chevron = TRUE` except VFT_NAV_FOLD_LABEL's own step
  #(step5, "Simulation") - it renders as a plain button, no .vft-nav-shape at
  #all, even though it sits inside the chevron DIV: see the CSS below for why it
  #still needs that container (the negative margins that pull its neighbours
  #onto it) while looking nothing like them.
  groupDivs <- lapply(VFT_NAV_GROUPS, function(g){
    chevron <- length(g) > 1
    cls <- if(chevron) "vft-nav-group vft-nav-group--chevron" else "vft-nav-group"
    btns <- lapply(g, function(s)
      buttonFor(s, chevron = chevron && !identical(s, VFT_NAV_FOLD_LABEL)))
    div <- shiny::tags$div(class = cls, btns)
    if(!identical(g, VFT_NAV_FOLD)) return(div)
    shiny::tagList(shiny::tags$div(class = "vft-nav-group vft-nav-fold", foldBtn),
                   div)
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
  #separator, so it reads as a destination of its own rather than a fifth link
  #in the simulation chain - which it emphatically is not: it opens on the
  #perimeter alone, while every member of that chain waits on something further
  #downstream. It is also why it stays visible while the chain is folded.
  #Its OWN label and its OWN prerequisites, both as overrides. Passing
  #tooltipFor("newVersions") straight through would say "Szenarien erstellen –
  #benötigt: ..." under a button labelled "Hitzeminderung" - and it would name
  #the three steps newVersions needs when this door needs only the perimeter
  #(VFT_HITZE_NEEDS in R/steps.R, which is the same test that enables it).
  hitzeLab  <- navTr(":nav_hitze:", "Hitzeminderung")
  hitzeTips <- tooltipFor("newVersions", label = hitzeLab,
                          prereq = vftHitzePrereqSteps())

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
        /* the four top-level buttons - Gebiet wählen, Sensibilität der
           Biodiversität, the folded Naherholung simulieren and Hitzeminderung -
           stand three quarters as tall as the
           banner, so the choices a user makes first read as the primary ones
           and the chain that opens behind Simulation reads as its detail.
           Derived from --nav-h rather than being an twelfth clamp of its own:
           '75% of the banner' is the rule, and it should stay true at every
           width the scale covers. */
        --nav-btn-main-h: calc(var(--nav-h) * 0.75);
        /* Simulation is the one link in the chain that is not a step on the way
           somewhere - it is what the other three lead to and come back from -
           so it stands at the TOP-LEVEL height even inside the chain, same as
           the folded button it replaces: unfolding grows three links either
           side of it rather than resizing it. It is plain rather than a
           chevron (see the CSS below), so unlike every other size in this
           bar its width does not have to mesh with anything - --nav-sim-pad is
           just generous horizontal padding, wide enough that it still reads as
           a rectangle standing behind the narrower buttons plugged into it. */
        --nav-sim-h:   var(--nav-btn-main-h);
        --nav-sim-pad: calc(var(--nav-pad) * 3);
        /* How wide a label is allowed to get before it wraps. The tall buttons
           carry names now ('Sensibilität der Biodiversität', 'Simulate
           Recreation') that no longer fit on one line at 1280 - so they get two,
           and this is what makes the break happen: a flex item sized to its
           content wraps nowhere, however long the string, because its
           max-content width IS the whole string. Capping it is the only thing
           that gives the text somewhere to break.

           Derived from --nav-font rather than being a clamp of its own, because
           what has to fit is a number of CHARACTERS, not a number of pixels: at
           ~0.5em per character in this bold Franklin face, 9.5em holds the
           longest line any of the seven labels breaks into in any of the three
           languages, at every width the scale covers. A label longer than that
           still ellipsises on the second line rather than widening the bar. */
        --nav-lab-w: calc(var(--nav-font) * 9.5);
        --nav-notch: clamp(9px,    0.83vw,  16px);

        --nav-sep:   clamp(4px,    0.72vw,  14px);
        --nav-gap:   clamp(2px,    0.24vw,   4px);
        --nav-title: clamp(12.5px, 1.02vw,  22px);
        --nav-logo:  clamp(88px,   8.60vw, 190px);
        --nav-icon:  clamp(19px,   1.55vw,  30px);
        --nav-edge:  clamp(8px,    0.78vw,  15px);
      }

      /* ---- why the labels kept coming out italic -------------------------
         Naming 'Franklin Gothic Book' did NOT fix it, and the reason is that
         the name Windows shows is not the name a browser matches.

         The Fonts control panel, and PowerPoint with it, lists a font by the
         name in the REGISTRY. A browser matches font-family against the name
         inside the font FILE. On this machine those disagree, because the
         Franklin faces installed here are not Microsoft's stock files:

           registry 'Franklin Gothic Book'    file FranklinGFB.TTF   family FranklinGFB
           registry 'Franklin Gothic Medium'  file FranklinGFM.TTF   family FranklinGFM
           registry 'Franklin Gothic Medium Italic'  framdit.ttf     family Franklin Gothic Medium

         So 'Franklin Gothic Book' matched nothing at all, and the stack fell
         through to 'Franklin Gothic Medium' - which the ITALIC file is the only
         face of. The browser matched that family, found one face, and used it.
         font-style:normal cannot undo that: there is no upright face in the
         family to fall back to, and Chrome will not un-slant one.

         Hence: the file-internal names first, so this machine gets the real
         Franklin Gothic Book; then the stock Windows name for a machine that
         has the stock file, where it does match. 'Franklin Gothic Medium' is
         deliberately NOT in the list any more - it is the trap, not a fallback.
         Verify a change here by RENDERING it, not by reading the font list:
         document.fonts.check() answers true for names that do not match. */
      #vftNav { display:flex; align-items:center; background-color:#006268;
                height:var(--nav-h); color:#ffffff;
                gap:var(--nav-sep); padding:0 var(--nav-edge);
                box-sizing:border-box;
                font-family:'FranklinGFB','FranklinGothic','Franklin Gothic Book',
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
         the parent's flex gap now, so they scale with everything else.
         Measured against the TOP-LEVEL button height, not the chain's: every
         separator borders a top-level group, and at 0.64 of the short chain
         height it read as a tick between two tall buttons rather than a
         divider. Same 0.64 proportion the bar has always had, against the
         height that is now beside it. */
      #vftNav .vft-nav-sep { width:3px; align-self:center; flex:0 0 auto;
                             height:calc(var(--nav-btn-main-h) * 0.64);
                             background:#ffffff; border-radius:2px; }
      /* font-style:normal is belt and braces only - the italic labels came from
         the font-family above, not from any font-style rule. border is 2px
         transparent rather than `none` so [disabled] toggling a real border
         colour does not change the button's box size.
         flex:0 1 auto + min-width:0 is the safety net under the clamp scale: if
         some future translation is longer than the scale allows for, that one
         label ellipsises - and its tooltip still names it in full - instead of
         the row overflowing and taking the logo with it.

         background-color is #B3D0D2, not white-at-0.7-opacity as it used to
         be. Those read the same on the plain banner - #B3D0D2 IS white at 0.7
         opacity over this exact teal, #006268, worked out once rather than
         composited by the browser every frame - but they stopped agreeing the
         moment Simulation went behind the chevron chain: `opacity` alpha-blends
         the WHOLE element, shape and all, against whatever is drawn under it,
         so a chevron button sitting over Simulation composited a second time on
         top of Simulation's already-blended fill and came out visibly lighter
         over the overlap than beside it. A flat, precomputed colour has nothing
         left to blend with - painted once, over anything. opacity:1 is the
         other half of that: nothing here uses translucency to mean 'reachable'
         any more, current and disabled included, so every state below is
         written as an explicit, opaque colour. */
      #vftNav .vft-nav-btn { height:var(--nav-btn-h); background-color:#b3d0d2;
                             color:#006268; font-weight:700; font-style:normal;
                             font-size:var(--nav-font); line-height:1;
                             border:2px solid transparent; border-radius:2px;
                             padding:0 var(--nav-pad); opacity:1;
                             box-sizing:border-box; flex:0 1 auto; min-width:0;
                             display:flex; align-items:center; justify-content:center; }
      /* the button is a flex box (above) purely so the ellipsis has something to
         be measured against: shiny wraps an actionButton's label in its own
         `.action-label` span, and a block inside an inline span in a non-flex
         button has no width to overflow. Absolutely-positioned children are out
         of flow, so .vft-nav-shape is unaffected. */
      /* the top-level buttons, i.e. everything that is not a link in the
         chain. Selected by what they are NOT, so the fold button and the two
         plain steps and Hitzeminderung are covered without naming four ids -
         and so a button added to either side later gets the right height by
         sitting in the right kind of group. */
      #vftNav .vft-nav-group:not(.vft-nav-group--chevron) .vft-nav-btn {
        height:var(--nav-btn-main-h);
        max-width:calc(var(--nav-lab-w) + var(--nav-pad) * 2);
      }
      #vftNav .vft-nav-btn .action-label { min-width:0; overflow:hidden; }
      /* ---- two lines up top, one in the chain ---------------------------
         The default is TWO lines, because the buttons that need them are the
         tall ones and 'tall' is the majority case here: the four top-level
         buttons plus Simulation, which stands at the top-level height inside
         the chain. -webkit-line-clamp rather than a plain height, so a label
         that would run to three lines is cut with an ellipsis on the second
         instead of pushing the banner taller - the same failure mode the old
         single-line rule had, one line further down.

         line-height is set here, not left at the button's own 1. It is 2, which
         is to say the blank band between the two lines is exactly as tall as a
         line of text: at 1 the ascenders and descenders of the two lines touch,
         and anything short of 2 still reads as one wrapped phrase rather than
         as two names stacked. Two lines therefore occupy 4em, which the
         shortest top-level button (--nav-btn-main-h, i.e. 0.75 x --nav-h)
         clears at every width the clamp scale covers.
         text-align:center because justify-content on the button only centres
         the label BOX; the second line inside it would otherwise sit left.

         `line-clamp` is written beside every `-webkit-line-clamp` for the
         unprefixed property that shipped in 2024; the prefixed one is what
         actually applies in this app's Chrome today. */
      #vftNav .vft-nav-lab { display:-webkit-box; -webkit-box-orient:vertical;
                             -webkit-line-clamp:2; line-clamp:2;
                             white-space:normal; overflow-wrap:break-word;
                             line-height:2; text-align:center;
                             overflow:hidden; text-overflow:ellipsis; }
      /* the chain is one line: a chevron is a fixed-height arrow with a point
         cut off each end, so a second line has nowhere to go in it - it would
         either overflow the shape or force every link in the chain taller.
         :not(:nth-last-child(2)) is the same selector the rest of this group
         uses to mean 'every link except Simulation' (see the tall-one block
         below); Simulation is not a chevron and keeps the two-line default. */
      #vftNav .vft-nav-group--chevron .vft-nav-btn:not(:nth-last-child(2)) .vft-nav-lab {
        display:block; white-space:nowrap;
        -webkit-line-clamp:none; line-clamp:none;
      }
      /* [disabled] is the one true state: set in the markup above, and added and
         removed by shinyjs::toggleState() at runtime. Do not add a class here.
         Reachable steps (the pale #b3d0d2 above) sit half way between this
         dark-teal unreachable look and the fully solid, fully opaque current
         step - Lagune's white/40%-opacity disabled state is gone, replaced with
         the Kontur treatment (outline, transparent fill) in the banner's own
         dark teal rather than white, so an unreachable step recedes into the
         banner instead of sitting on it as a faded white block. */
      #vftNav .vft-nav-btn[disabled] {
        background-color:transparent; color:#000000; border-color:#000000;
        opacity:1; cursor:not-allowed;
      }
      /* current step: no underline - a thick white outline standing slightly
         proud of the button, via outline-offset rather than a border (a border
         would eat into the button's own layout box). Solid white fill,
         restated explicitly here rather than inherited - the base rule's
         #b3d0d2 is the 'reachable but not here' colour, and the current step is
         the one thing in the bar that is neither reachable-pale nor
         unreachable-dark. Ring and offset scale with the button, or at 1280 the
         offset alone would be a third of the gap between two groups. */
      #vftNav .vft-nav-current { background-color:#ffffff;
                                 outline:calc(var(--nav-btn-h) * 0.07) solid #ffffff;
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

      /* ZG definieren | ZG bearbeiten | Naherholung simulieren | Szenarien
         erstellen, plugged into each other like
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
      /* ---- folded / unfolded --------------------------------------------
         Both the stand-in button and the chain it opens into are always in the
         page; this is the whole of the switch between them. One class on the
         bar, toggled by the single setFolded() in vftNavBarServer() - no
         output, no re-render, one shinyjs message when the state actually
         moves. The bar ships WITH the class (see #vftNav below), so the folded
         state is true from parse rather than from the first flush, for the same
         reason the buttons ship with a plain `disabled` attribute. */
      #vftNav.vft-nav-folded .vft-nav-group--chevron { display:none; }
      #vftNav:not(.vft-nav-folded) .vft-nav-fold     { display:none; }

      #vftNav .vft-nav-group--chevron { gap:0; align-items:center; }
      #vftNav .vft-nav-group--chevron .vft-nav-btn {
        background-color:transparent; border:none;
        margin-left:calc(var(--nav-notch) * -1 + 1px);
        padding:0 calc(var(--nav-notch) + var(--nav-pad) * 0.45)
                0 calc(var(--nav-notch) + var(--nav-pad));
        position:relative;
      }
      #vftNav .vft-nav-shape { position:absolute; inset:0; z-index:-1;
                               pointer-events:none; }
      /* #b3d0d2, matching the plain buttons' own reachable colour exactly (see
         the note on .vft-nav-btn above) - and, unlike the #ffffff this used to
         be, flat rather than something the browser blends per frame. That is
         what stops Simulation showing through in the overlap: this shape now
         paints the SAME solid colour whether it is sitting on the banner or on
         Simulation's own (also now flat #b3d0d2) fill, so there is nothing left
         underneath for it to combine with. The trade a flat colour makes is
         that the seam itself becomes invisible - two identical fills side by
         side read as one shape - which is what the outline further down this
         file (the 'outlining the arrow' section) exists to put back. */
      #vftNav .vft-nav-shape::before {
        content:''; position:absolute; inset:0; background-color:#b3d0d2;
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
      /* ---- the last link runs the other way -----------------------------
         ZG definieren > ZG bearbeiten > Naherholung simulieren < Szenarien
         erstellen.

         The first three chain forwards; Szenarien erstellen turns back and plugs
         into Simulation from the right, so its point is on its LEFT edge and
         points LEFT (apex at 0 50%, body starting at --nav-notch). Simulation
         itself is plain now (see below) and carries no matching notch - the
         point simply lies OVER it, on the same negative margin that pulls
         every other pair together, because Simulation is drawn behind it.

         Reading it as a flow: the first three are steps you walk on through,
         and Neue Versionen is where you come back to the simulation with a
         changed scenario rather than a further step away from it. */
      #vftNav .vft-nav-group--chevron .vft-nav-btn:last-child { padding-right:var(--nav-pad); }
      #vftNav .vft-nav-group--chevron .vft-nav-btn:last-child .vft-nav-shape::before {
        clip-path: polygon(var(--nav-notch) 0, 100% 0, 100% 100%,
                            var(--nav-notch) 100%, 0 50%);
      }
      /* ---- the tall one is plain, and sits behind its neighbours ---------
         Simulation - :nth-last-child(2), i.e. the one the reversed last link
         plugs into - is not a step on the way anywhere, so it does not carry a
         chevron shape at all: R builds it with `chevron = FALSE` (see
         vftStepNav() above), which means there is no .vft-nav-shape span to
         style here, only the plain .vft-nav-btn box.

         That box still sits inside the CHEVRON group, though, because it still
         wants the group's negative margins - the same `margin-left:
         calc(--nav-notch * -1 + 1px)` every other link gets from the base rule
         below is what pulls Wegnetz and Neue Versionen in close enough to
         overlap it. What this block undoes is everything about that base rule
         that made sense for a shape-carrying button and does not for a plain
         one: the transparent fill and the padding tapered for a point. Taller
         and wider than its neighbours (--nav-sim-h, --nav-sim-pad above), and
         LOWER z-index than both of them (the ladder below), so their points
         visibly lie on top of its edges rather than meeting a cut notch -
         'behind', literally, not just in reading order.

         Three states need their own line because the base rules they would
         otherwise inherit target a specificity this selector already exceeds
         (one more class than the group's blanket rule), which would otherwise
         make an unreachable Simulation read as reachable, and a current one
         lose its ring: */
      #vftNav .vft-nav-group--chevron .vft-nav-btn:nth-last-child(2) {
        height:var(--nav-sim-h);
        background-color:#b3d0d2; color:#006268; border:2px solid transparent;
        opacity:1;
        padding:0 var(--nav-sim-pad);
        max-width:calc(var(--nav-lab-w) + var(--nav-sim-pad) * 2);
        z-index:1;
      }
      #vftNav .vft-nav-group--chevron .vft-nav-btn:nth-last-child(2)[disabled] {
        background-color:transparent; color:#000000; border-color:#000000;
        opacity:1;
      }
      /* background-color restated for the same reason .vft-nav-current does
         above: the default rule just above this one is the pale reachable
         fill, and current is the one state that is neither that nor the dark
         unreachable one.

         z-index STAYS at 1 here - the one place in the bar where being the
         current step does not lift a button above its neighbours. Every other
         link is lifted (the .vft-nav-current rule further down sets z-index:5)
         because its ring stands proud of its box and would otherwise be
         overlapped by whatever plugs into it. Simulation is the opposite case:
         it is the thing the others plug INTO, and it is behind them by design,
         so lifting it would pull it over Wegnetz's point and Neue Versionen's,
         breaking the chain exactly when the user is standing in the middle of
         it. It has to be restated rather than left out - drop the declaration
         and the generic .vft-nav-btn.vft-nav-current rule below, which comes
         later in the file, wins on order and puts it back to 5. */
      #vftNav .vft-nav-group--chevron .vft-nav-btn:nth-last-child(2).vft-nav-current {
        background-color:#ffffff;
        outline:calc(var(--nav-sim-h) * 0.07) solid #ffffff;
        outline-offset:calc(var(--nav-sim-h) * 0.07);
        opacity:1; z-index:1;
      }
      /* falls left to right, so each earlier tip shows through the next
         button's notch - except Simulation (position 3), pushed BELOW both its
         neighbours instead of following the ladder, and Neue Versionen, lifted
         back above it: its point protrudes onto Simulation rather than the
         other way round, so it has to be the one drawn on top. */
      #vftNav .vft-nav-group--chevron .vft-nav-btn:nth-child(1) { z-index:4; }
      #vftNav .vft-nav-group--chevron .vft-nav-btn:nth-child(2) { z-index:3; }
      #vftNav .vft-nav-group--chevron .vft-nav-btn:nth-child(4) { z-index:3; }

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
         with the bar would simply make it vanish at 1280.

         A THIRD case uses this same trick now, for the ordinary reachable
         state neither disabled nor current touches. Flattening this shape's
         fill to #b3d0d2 (above) stopped Simulation bleeding through it, but it
         also means two flat fills of the same colour, one above the other,
         paint as a single unbroken shape - there is no longer anything at the
         seam for the eye to catch on. This ring is what puts a seam back: a
         single, thin #006268 outline - the banner's own colour - so wherever
         Wegnetz or Neue Versionen actually lies over Simulation, the boundary
         between them still reads, exactly the way it already did against the
         plain teal banner. Half the thickness of the disabled ring (0.59/0.42
         rather than 0.83/0.59) and one colour rather than two, because this is
         a seam to notice, not a state to announce. */
      #vftNav .vft-nav-group--chevron .vft-nav-shape {
        filter:
          drop-shadow(0.59px 0 0 #006268)      drop-shadow(-0.59px 0 0 #006268)
          drop-shadow(0 0.59px 0 #006268)      drop-shadow(0 -0.59px 0 #006268)
          drop-shadow(0.42px 0.42px 0 #006268) drop-shadow(-0.42px -0.42px 0 #006268)
          drop-shadow(0.42px -0.42px 0 #006268) drop-shadow(-0.42px 0.42px 0 #006268);
      }
      #vftNav .vft-nav-group--chevron .vft-nav-btn[disabled] .vft-nav-shape::before {
        background-color:#006268;
      }
      #vftNav .vft-nav-group--chevron .vft-nav-btn[disabled] .vft-nav-shape {
        filter:
          drop-shadow(0.83px 0 0 #000000)      drop-shadow(-0.83px 0 0 #000000)
          drop-shadow(0 0.83px 0 #000000)      drop-shadow(0 -0.83px 0 #000000)
          drop-shadow(0.59px 0.59px 0 #000000) drop-shadow(-0.59px -0.59px 0 #000000)
          drop-shadow(0.59px -0.59px 0 #000000) drop-shadow(-0.59px 0.59px 0 #000000);
      }
      /* the ring stands proud of the button, so it would be overlapped by the
         neighbour plugged into it - the current step is lifted above both its
         siblings' z-index instead. .vft-nav-btn is in the selector only to
         match the nth-child rules' specificity; it comes later, so it wins. */
      #vftNav .vft-nav-group--chevron .vft-nav-btn.vft-nav-current {
        outline:none; z-index:5;
      }
      /* the current step's face, solid white, the same as every plain button's.
         This used to need no rule at all: the shape was painted #ffffff and the
         BUTTON carried opacity, so 'current' was simply that opacity going to
         1 and the white arriving with it. The fill is a flat #b3d0d2 now (see
         the note on .vft-nav-shape::before above) and no opacity is left to
         lift, so the white has to be said. It names three classes, one fewer
         than the [disabled] fill above it, so a disabled button still paints
         dark even while r$navStep points at it - which is why the
         [disabled].vft-nav-current pair at the very bottom of this block has
         to say white a second time, for the tick where a step is both. */
      #vftNav .vft-nav-group--chevron .vft-nav-current .vft-nav-shape::before {
        background-color:#ffffff;
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

      /* FranklinGFM first here, not FranklinGFB. The file registered as
         'Franklin Gothic Book' carries a single face at weight 700, so asking
         for it at a normal weight - which this line does, being the only text
         in the banner that is not bold - renders bold anyway, there being no
         lighter face in the family to choose. FranklinGFM is the 400 one. */
      .vft-nav-contact { color:#006268; margin:4px 0 0 15px; font-size:12px;
                         font-family:'FranklinGFM','FranklinGothic','Franklin Gothic Book',
                                     'Libre Franklin',Arial,sans-serif; }
    ")),
    #kept from the old markup: it is what names the browser tab. Outside the
    #flex row, so it cannot become a flex item of it.
    shiny::HTML("<title>Visitor Flow Tool</title>"),
    #`vft-nav-folded` from parse: the bar opens showing four choices, not seven.
    #vftNavBarServer() is the only thing that ever removes it. See the fold CSS
    #above and VFT_NAV_FOLD in R/steps.R.
    shiny::tags$div(id = "vftNav", class = "vft-nav-folded",
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
