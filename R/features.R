#### Feature switches ####

# Extensions that ship in the code but are not offered to users yet. Each one is
# a plain constant: flip it here and restart the app.

#' Is the heat mitigation extension ("Hitzeminderung") offered?
#'
#' With FALSE, every way into newVersions' context 4 is closed but still shown,
#' greyed out, so the user can see the feature exists:
#'   - the nav bar's Hitzeminderung button (vftStepNav() in R/app_ui.R,
#'     vftNavBarServer() in R/navigation.R);
#'   - the Hitzeminderung choice of the "choose your next step" modal, which is
#'     dropped rather than greyed (VFT_NEXT_CHOICES in R/navigation.R);
#'   - the Hitzeminderung option of newVersions' context radios, and the page
#'     never opens on it by itself (newVersions_server.R);
#'   - step 1's "area too large for heat mitigation" warning, which would warn
#'     about a feature the user cannot reach.
#' A click on any of the greyed controls raises vftNotImplementedModal().
HEAT_MITIGATION <- FALSE

#' HEAT_MITIGATION, read at call time.
#'
#' A function rather than the constant at each call site, so that a test can
#' flip the switch on the loaded namespace and every reader follows.
vftHeatEnabled <- function() isTRUE(HEAT_MITIGATION)

#' "This functionality is not implemented yet."
#'
#' What a click on a control switched off above gets. The German literal is the
#' translation key, per .vftT() in R/async_helpers.R, so a CSV without the row
#' shows German rather than a bare key.
#'
#' @param session the app-level session or a module proxy - showModal() does
#'   not care which.
vftNotImplementedModal <- function(session = shiny::getDefaultReactiveDomain()){
  if(is.null(session)) return(invisible(NULL))
  tr <- .vftT(session)
  shiny::showModal(shiny::modalDialog(
    tr("Diese Funktion ist noch nicht implementiert."),
    footer = shiny::modalButton(tr("OK!")),
    easyClose = TRUE
  ), session = session)
  invisible(NULL)
}
