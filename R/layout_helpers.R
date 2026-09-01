#### Fitting a step to the height of the screen ####

# Every step was laid out for a tall monitor. The tall things in them are
# written as fixed pixels - `leafletOutput(height = 600)` in steps 1 and 5,
# `height = 500` in step 4, a 400px `plotOutput` in step 3, and the
# `height:350px` / `height:500px` / `height:400px` scroll boxes in step 2,
# step 5 and newVersions - so on a 768- or 900-tall laptop the confirm button,
# which is always the LAST thing in the page, falls below the fold. The user
# does not know the step is finished, because the only control that finishes it
# is off screen.
#
# There are two mechanisms here, and which one a step gets depends on whether
# anything above or below its map can APPEAR AND DISAPPEAR.
#
#   1. The fit-page (`.vft-fit-page` + `.vft-grow`). The step is a flex column
#      exactly as tall as the pane; everything keeps its natural height except
#      one marked row, which takes whatever is left. No arithmetic and nothing
#      to keep in sync with the markup - and, the reason it is here at all, the
#      layout re-solves itself whenever a sibling appears or disappears. Step 1's
#      confirm buttons start hidden, and newVersions' paint tools are
#      `display:none` until the heat-mitigation context shows them; under a
#      fixed reserve both of those cost dead space at the bottom of the screen
#      whenever they are not on show. Steps 1, 2, 4 and newVersions use this.
#
#   2. A clamp(floor, what is left, ceiling) height, the same idea the nav bar's
#      `--nav-*` scale already uses (see vftStepNav() in R/app_ui.R) - there the
#      vw term tracks the monitor's WIDTH, here the middle term tracks what is
#      left of its HEIGHT. Used where nothing toggles and the element is not the
#      last thing in its column: step 3's plot and step 5's map frame.
#
# The floors matter as much as the fill: below them an element stops being
# usable - a map you cannot navigate, a species list showing one row - and past
# that point the step's own pane scrolls instead of shrinking further.
#
# Why the pane scrolls and not the window: `.tab-content` is pinned to exactly
# the space under the nav bar and each `.tab-pane` scrolls inside it. That keeps
# the bar on screen at every window size - it is the only way back to an earlier
# step - and means "off the bottom" is always a short scroll inside the step
# rather than a page that has grown taller than the monitor.
#
# Resizing is handled for us: both leaflet and plotOutput re-render through
# their Shiny output binding's resize handler, which reads the element's actual
# offsetHeight. A flexed or clamped height is a real computed height, so both
# read it correctly, and the moment it changes - a window resize, or a row
# appearing - is a moment Shiny already resizes on.

#' One height scale for every step, as a `<style>` block.
#'
#' Included once from `app_ui()`. Nothing it emits is conditional on the step -
#' the same variables serve all six pages, and each page opts in by class.
#'
#' @return A `shiny::tags$style` tag.
#' @noRd
vftFitHeightCSS <- function(){

  # The nav bar is not always there: vftStepNav() returns NULL when VFT_NAV is
  # off, and then there is no #vftNav to leave room for. This is the ONLY
  # definition of the bar's height - vftStepNav()'s own `--nav-h` reads it from
  # here - so the two cannot drift apart.
  navH <- if(vftNavEnabled()) "clamp(70px, 5.20vw, 100px)" else "0px"

  shiny::tags$style(shiny::HTML(paste0("
    :root{
      --vft-nav-h: ", navH, ";

      /* What is left for a step once the bar has taken its share. The 4px is
         the rounding slack between that clamp and the browser's own layout -
         without it a sub-pixel remainder gives the BODY a scrollbar, which is
         the one scrollbar this file exists to remove. */
      --vft-fit: calc(100vh - var(--vft-nav-h) - 4px);
    }

    /* dvh, where it exists, is the viewport MINUS the browser chrome that
       collapses on scroll; vh counts that chrome as available and so hides a
       strip of whatever is last in the page. An @supports override rather than
       the primary value, so anything predating dvh still gets the vh line. */
    @supports (height: 100dvh){
      :root{ --vft-fit: calc(100dvh - var(--vft-nav-h) - 4px); }
    }

    /* ---- the pane is the viewport -------------------------------------- */
    .tab-content{
      height: var(--vft-fit);
    }
    .tab-content > .tab-pane{
      height: 100%;
      overflow-y: auto;
      /* x is hidden, not auto: several steps put a leaflet or a wide button row
         a pixel or two past the column edge, and an auto rule turns that into a
         permanent horizontal scrollbar that eats a row of height. */
      overflow-x: hidden;
    }

    /* ---- mechanism 1: the fit-page ------------------------------------- */
    /* `.vft-fit-page` goes on a step's fluidPage (it lands on the
       .container-fluid), `.vft-grow` on the one row that should absorb the
       slack. Only DIRECT children are pinned to their natural height, so a
       nested layout inside .vft-grow is left entirely alone. */
    .vft-fit-page{
      display: flex;
      flex-direction: column;
      height: 100%;
      min-height: 0;
    }
    .vft-fit-page > *{ flex: 0 0 auto; }
    .vft-fit-page > .vft-grow{
      flex: 1 1 auto;
      /* min-height:0 would let a map collapse to nothing on a very short
         window; this is the floor, and the pane scrolls once it is reached. */
      min-height: 240px;
      display: flex;
      flex-direction: column;
    }
    /* the chain from the grow row down to the widget: the Bootstrap column, any
       withSpinner() wrapper, then the output itself. Each has to be told to
       fill its parent or the height stops at the first one that does not. */
    .vft-grow > [class*=\"col-\"]{
      display: flex;
      flex-direction: column;
      min-height: 0;
      flex: 1 1 auto;
    }
    .vft-grow > [class*=\"col-\"] > *{ flex: 1 1 auto; min-height: 0; }
    /* an intermediate box inside the column that has to fill it too - step 4's
       #mapFrame, which carries the cut-mode border. border-box so that border
       comes off the map rather than making the page 10px taller when cut mode
       is switched on. */
    .vft-grow-fill{
      flex: 1 1 auto;
      min-height: 0;
      height: 100%;
      box-sizing: border-box;
    }
    .vft-grow .shiny-spinner-output-container{ height: 100%; }
    /* !important because leafletOutput() and plotOutput() write their height as
       an INLINE style, which beats any selector specificity a stylesheet can
       reach. The R calls keep their original numbers deliberately - they are
       still the right value on a tall screen, and they document what the
       element used to be. */
    .vft-grow .leaflet,
    .vft-grow .shiny-plot-output{ height: 100% !important; }

    /* ---- mechanism 2: the clamped elements ----------------------------- */
    :root{
      /* step 3 is the text-heaviest page in the app - a title, two lines of
         explanation, three of instruction and a tip - and its plot is the
         smallest to begin with, so it reserves the most. Measured furniture at
         1080p is 321px; 330 leaves a little slack for a longer translation. */
      --vft-h-step3: clamp(180px, calc(var(--vft-fit) - 330px), 400px);

      /* step 5's map frame. Measured furniture 152px - the title, two lines,
         and the row of three download buttons under the map. */
      --vft-h-step5: clamp(260px, calc(var(--vft-fit) - 175px), 600px);

      /* the scenario list beside step 5's map: the 'new scenarios' button sits
         above it and the launch button below, and both have to stay on screen
         with it. */
      --vft-h-step5-list: clamp(150px, calc(var(--vft-fit) - 330px), 400px);
    }

    #step3-AOIMap{ height: var(--vft-h-step3) !important; }

    /* step 5's map is the one with a frame around it, because its 'no
       simulation yet' placeholder is an OVERLAY on the live map rather than a
       replacement for it (step5_ui.R spells out why it must not be a hide()).
       Frame, map and overlay have to agree on a size or the white overlay stops
       covering the map, so all three take it from the frame. The 884px that was
       the frame's fixed width is now its max-width, so the frame is unchanged
       on a wide screen and stops overflowing its column on a narrow one.
       overflow:hidden is the backstop for the overlay's CONTENT: the
       'noch keine Simulation' image is a fixed-size PNG and used to spill out
       over the scenario sidebar to its right once the frame started shrinking. */
    .vft-map5-frame{
      position: relative;
      width: 100%;
      max-width: 884px;
      height: var(--vft-h-step5);
      overflow: hidden;
    }
    #step5-mapAreaLeaflet{
      height: 100% !important;
      width: 100% !important;
    }
    /* the placeholder image fills the frame and keeps its aspect ratio rather
       than being drawn at its natural 884x600. The uiOutput div between the
       overlay and the image needs the height too: a percentage height resolves
       against the PARENT's height, and an auto-height parent silently turns it
       back into the image's natural size. */
    #step5-mapArea_UI{ width: 100%; height: 100%; }
    #step5-mapPlaceholder img{
      width: 100%;
      height: 100%;
      object-fit: contain;
    }
    .vft-fit-vlist{ height: var(--vft-h-step5-list) !important; }

    /* ---- step 1: a shorter head, so the map starts higher --------------- */
    /* The rows between the banner and the map - the step title, the three-column
       intake block, the 'load saved data' button - were set at the app's default
       heading sizes and took ~150px of a screen whose whole point is the map
       under them. These are the same sizes the max-height queries at the foot of
       this file impose on a short screen, applied to this one block at every
       height. */
    .vft-step1-head h3{ font-size: 20px; margin: 6px 0; }
    .vft-step1-head h4{ font-size: 15px; margin: 4px 0; }
    .vft-step1-head h5{ font-size: 12px; margin: 3px 0; }
    .vft-step1-head h6{ font-size: 11px; margin: 2px 0; line-height: 1.35; }
    .vft-step1-head .form-group{ margin-bottom: 4px; }
    /* the fileInput's own progress bar is 20px of permanent white space under
       the control, and it only ever reads 'Upload complete'. */
    .vft-step1-head .progress{ height: 12px; margin-bottom: 0; }
    .vft-step1-head .btn-lg{ font-size: 14px; padding: 6px 12px; }

    /* ---- step 2: three columns of one height ---------------------------- */
    /* The species list, the plot and the group list are one visual band, so they
       share a height and each flexes internally: the two scroll boxes and the
       SDM plot absorb the slack, everything else in the column keeps its own
       size. The weight buttons moved INTO the first column (step2_ui.R) so that
       the plot beside them can run down to their level - a Bootstrap row cannot
       do that from a row below, because the next row clears the tallest column
       of the one above it. */
    .vft-step2-col{
      display: flex;
      flex-direction: column;
      min-height: 0;
      height: clamp(260px, calc(var(--vft-fit) - 200px), 620px);
    }
    .vft-step2-col > *{ flex: 0 0 auto; }
    /* the species scroll box and the SDM plot are the two that grow. */
    .vft-step2-col > .vft-fit-species{ flex: 1 1 auto; min-height: 90px; }
    .vft-step2-col > #step2-SDMmap{
      flex: 1 1 auto;
      min-height: 150px;
      height: auto !important;
    }
    /* These columns carry align=center, which the browser applies as
       text-align:-webkit-center - and that is what used to centre a shiny input
       container, because such a container has a definite width of its own
       (300px from shiny's stylesheet, or whatever width= the server passed).
       text-align cannot place a FLEX item, so once the column became a flex
       column those inputs dropped to the left edge. align-self puts them back.
       Only the inputs are listed: the plot and the scroll boxes must keep
       stretching to the full column width, so they are deliberately left out. */
    .vft-step2-col[align=\"center\"] > .shiny-input-container,
    .vft-step2-col[align=\"center\"] > .form-group,
    .vft-step2-col[align=\"center\"] > .shiny-html-output{
      align-self: center;
      max-width: 100%;
    }
    /* the weight buttons, at the foot of the species column. A flex row that
       wraps, so three btn-secondary labels in any of the three languages take
       one line on a wide screen and two on a narrow one instead of overflowing
       the column. */
    .vft-step2-weights{
      display: flex;
      flex-wrap: wrap;
      justify-content: center;
      gap: 4px;
      margin-top: 6px;
    }
    .vft-step2-weights .btn{ margin: 0; }
    /* the legend shares its row with the confirm and download buttons, so it
       must not push them off: it scrolls in place if a long species list makes
       it taller than the strip. */
    .vft-step2-foot{ margin-top: 6px; }
    /* The legend is four long lines of icon-plus-caption and is much wider than
       the third of the row it now sits in. It scrolls in BOTH directions inside
       its own box so it can neither push the button column sideways nor grow
       the row downwards: nowrap keeps it at four lines and sends the excess
       width to the horizontal scrollbar instead of wrapping into a tall block.
       The translation itself carries display:table-cell as an inline style,
       which sizes to its content and ignores the box it is in, so it is
       overridden here - an inline style needs !important to beat. */
    /* The band above stops growing at its 620px cap, so on a tall screen the
       slack past that point is free and the legend can use it to show all four
       lines without a vertical scrollbar. Below that the floor takes over and
       it scrolls, which costs lines of the legend rather than plot height. The
       745px subtrahend is the rest of the page at that cap (113px of furniture
       + 620px band + the 15px horizontal scrollbar, less a few px of margin),
       so this term grows exactly as fast as the slack does. The 80px floor is
       what the tightest case allows: between roughly 950 and 1000px of window
       the band is still growing 1:1 and the short-screen media rules below have
       not started shrinking the headings yet, so nothing else is giving ground. */
    .vft-step2-foot #step2-legend_ui{
      max-height: clamp(80px, calc(var(--vft-fit) - 745px), 130px);
      overflow: auto;
      white-space: nowrap;
    }
    .vft-step2-foot #step2-legend_ui p{
      display: block !important;
      margin: 0;
    }

    /* ---- newVersions: map column and sidebar end on the same line ------- */
    /* Both columns are flex, both are the same height, and in each one the
       element that should absorb the slack is the one that flexes - the map on
       the left, the scenario list on the right. That is what makes the bottom of
       the map and the bottom of the confirm button line up, and it keeps lining
       up when the paint-tool block under the map is shown: the map gives back
       exactly the height the tools take, rather than the page growing by it. */
    /* The band's height is not computed from a reserve any more. The page is a
       .vft-fit-page and this row is its grow child, so the row IS whatever the
       head leaves and the columns simply fill it. The reserve that used to be
       here was 60px against a head measured at 89-126px - the context radio
       group above the band is server-rendered, so it was missing from the
       static render the 60 was taken from - and the difference was the map and
       the confirm button hanging off the bottom of the screen.

       A separate class from .vft-grow on purpose: that one also carries rules
       for the columns INSIDE it, and `.vft-grow > [class*=\"col-\"] > *` would
       outrank `.vft-nv-col > *` below and stretch every button and caption in
       the sidebar. This sizes the row and nothing else.

       min-height is the floor the clamp used to give: below it the pane
       scrolls rather than the map shrinking further. */
    .vft-fit-page > .vft-nv-body{
      flex: 1 1 auto;
      min-height: 300px;
    }
    .vft-nv-col{
      display: flex;
      flex-direction: column;
      min-height: 0;
      /* 100% of that row, so the bottom of the map and the bottom of the
         confirm button land on the bottom of the pane. The 780px cap is the
         one the clamp had: past it a map is tall enough, and the slack on a
         very tall monitor is better left blank than spent on more map. */
      height: 100%;
      max-height: 780px;
    }
    .vft-nv-col > *{ flex: 0 0 auto; }
    .vft-nv-col > .vft-nv-mapslot{ flex: 1 1 auto; min-height: 200px; }
    .vft-nv-mapslot .shiny-spinner-output-container{ height: 100%; }
    #newVersions-versionMap{ height: 100% !important; }
    /* the sidebar's list sits one level down, inside its own fluidRow, so the
       ROW is the flex child and the box inside it fills the row. */
    .vft-nv-col > .vft-nv-listrow{
      flex: 1 1 auto;
      min-height: 120px;
      display: flex;
      min-width: 0;
    }
    .vft-nv-listrow > .vft-fit-vlist-nv{ height: 100% !important; }

    /* ---- and the text, on a short screen -------------------------------- */
    /* Shrinking the maps alone is not enough on steps 3, 4 and 5: their
       headings are six to eight stacked h3/h4/h5 lines, which is more vertical
       space than the map can give back. Scoped to `.tab-content` so the nav
       bar's own scale - which answers to WIDTH, and would fight this - is
       untouched.

       Bootstrap's h3/h4/h5 default to 24/18/14px with 10px/20px margins; these
       are two steps down from that in the same proportion, so a page reads as
       the same page set slightly tighter rather than as a different design. */
    @media (max-height: 860px){
      .tab-content h3{ font-size: 20px; margin-top: 8px; margin-bottom: 8px; }
      .tab-content h4{ font-size: 16px; margin-top: 6px; margin-bottom: 6px; }
      .tab-content h5{ font-size: 13px; margin-top: 5px; margin-bottom: 5px; }
      .tab-content h6{ font-size: 11px; margin-top: 4px; margin-bottom: 4px; }
      .tab-content .form-group{ margin-bottom: 8px; }
    }
    @media (max-height: 720px){
      .tab-content h3{ font-size: 17px; margin-top: 5px; margin-bottom: 5px; }
      .tab-content h4{ font-size: 14px; margin-top: 4px; margin-bottom: 4px; }
      .tab-content h5{ font-size: 12px; margin-top: 3px; margin-bottom: 3px; }
      .tab-content h6{ font-size: 10px; margin-top: 2px; margin-bottom: 2px; }
      .tab-content .form-group{ margin-bottom: 5px; }
      /* btn-lg is what every confirm button in the app is, and it is the one
         control that must never be the thing pushed off the bottom. */
      .tab-content .btn-lg{ font-size: 14px; padding: 6px 14px; }
      .tab-content .btn{ margin-bottom: 2px; }
    }
  ")))
}
