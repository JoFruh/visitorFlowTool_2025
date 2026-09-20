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


#### Translating static markup ####

# The nav bar (R/app_ui.R) is built once per session and is deliberately NOT an
# output - that is the whole point of it - so it cannot re-render when the user
# picks another language. What it does instead is carry ALL THREE translations
# of every label and tooltip in the markup, as `data-i18n-de/fr/en` and
# `data-tip-de/fr/en` attributes, and swap them on the client. One helper is
# needed for that: give me every language of one key at UI build time.

#' One i18n key as PLAIN TEXT in the Translator's current language.
#'
#' `i18n$t()` does not always return a string. Once `shiny.i18n::usei18n()` has
#' run - and every step module calls it at the top of its UI - the Translator
#' switches to client-side mode and `t()` returns a
#' `<span class="i18n" data-key="...">` tag instead, so the browser can swap the
#' text on a language change without a round trip. That tag is a three-element
#' list, and anywhere a bare string was expected it silently becomes three
#' values: `c(i18n$t(a), i18n$t(b), i18n$t(c))` is length 9, which is how a
#' selectInput's choices came to fail with "'names' attribute [9] must be the
#' same length as the vector [3]".
#'
#' The span already carries the right translation as its only child, so this
#' unwraps it rather than re-translating. Use it wherever the result has to be
#' character - a select's choice labels, a plot title, a tooltip attribute -
#' and plain `i18n$t()` wherever a tag is fine, which is most of the UI.
#'
#' The cost of unwrapping is that the client can no longer swap that text, so a
#' caller using this must refresh its own labels when the language changes.
vftTrText <- function(i18n, key){
  if(is.null(i18n)) return(key)
  out <- suppressWarnings(try(i18n$t(key), silent = TRUE))
  if(inherits(out, "try-error")) return(key)
  if(inherits(out, "shiny.tag")){
    txt <- tryCatch(paste0(as.character(out$children[[1]]), collapse = ""),
                    error = function(e) "")
    return(if(nzchar(txt)) txt else key)
  }
  if(length(out) == 1L && !is.na(out) && nzchar(as.character(out))){
    as.character(out)
  }else key
}

#' Every language of one i18n key, as a character vector named by language.
#'
#' `i18n$t()` translates into whatever language the Translator is currently set
#' to, so getting all three means setting it three times. The original language
#' is restored on the way out, including on error - `app_ui()` shares one
#' Translator with the six step modules, and leaving it on "en" would hand the
#' next `i18n$t()` in the UI build the wrong language.
#'
#' A key the CSVs do not carry comes back as the key itself (with a warning);
#' callers pass a `fallback` to `vftNavTr()` in R/app_ui.R for that case, so a
#' translation row that has not been added yet degrades to the German literal
#' rather than putting ":nav_step1:" on a button.
#'
#' @param i18n a shiny.i18n Translator, or NULL (tests, and any caller that has
#'   none) - in which case `key` is returned for every language.
#' @param key the translation key.
#' @param langs the languages to fetch, in the order the result should be named.
vftTrAll <- function(i18n, key, langs = c("de", "fr", "en")){
  if(is.null(i18n)) return(stats::setNames(rep(key, length(langs)), langs))

  keep <- i18n$get_translation_language()
  on.exit(i18n$set_translation_language(keep), add = TRUE)

  #through vftTrText(), because this is called from vftStepNav() in app_ui.R,
  #which today runs BEFORE the first step module calls usei18n() - but only
  #today. Called the other way round, every language came back as the German
  #key, since a tag fails the length-1 test below and falls through to `key`.
  #Nothing about the call site advertises that ordering, so it is removed as a
  #condition rather than documented as one.
  vapply(langs, function(l){
    i18n$set_translation_language(l)
    vftTrText(i18n, key)
  }, character(1))
}
