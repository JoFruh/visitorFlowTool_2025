#### The step banner ####

# Each step shows a static PNG strip at the top of its page, and swaps it for a
# translated copy when the user picks another language. That used to be an
# `output$bannerUI_N <- renderUI()` in six modules - 34 assignment sites for six
# outputs, all of them producing the same one `<img>` tag.
#
# Six outputs is not much until you look at what Shiny does with them.
# ShinySession$manageInputs() calls manageHiddenOutputs() on EVERY input message
# batch, with no argument, so it walks the whole registered-output list and runs
# the suspend test against each one. This app registers 31 distinct output names,
# so the six banners were paying ~19% of that sweep - for an image that changes
# at most a handful of times in a session.
#
# A plain <img> in the UI is not an output at all. It never enters the sweep, and
# the language swap becomes one attribute write on the client.
#
# The image the UI ships with is the German one, because 'de' is the app's
# starting language; every module also sets the banner explicitly when it is
# built, so a session that has already switched language gets the right image
# without waiting for the user to touch the selector again.

#' The banner `<img>`, for the UI side of a step module.
#'
#' @param id the module id, as passed to `stepN_ui()`.
#' @param src the initial image, relative to the registered `www` resource path.
vftBannerImg <- function(id, src){
  shiny::tags$img(id = shiny::NS(id, "banner_img"), height = 70, src = src)
}

#' Point a step's banner at another image, for the server side of a step module.
#'
#' Call it from the module body (to set the language the step was entered in) and
#' from the `languageSelect_N` observer (to follow a change). `src` is always one
#' of the literals in this package, so it needs no escaping.
#'
#' @param id the module id, as passed to `stepN_server()`.
#' @param src the image to show, relative to the registered `www` resource path.
vftSetBanner <- function(id, src){
  shinyjs::runjs(sprintf("$('#%s').attr('src', '%s');",
                         shiny::NS(id, "banner_img"), src))
}
