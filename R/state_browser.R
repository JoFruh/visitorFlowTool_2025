#### Crash recovery in the browser, and the explicit named save ####
#
#Two things live here, and they are deliberately NOT the same file:
#
#  * vftSnapshotWrite() pushes the LIGHTWEIGHT snapshot (VFT_SNAPSHOT_KEYS) into
#    the browser's localStorage at every step confirmation. Nobody asks for it
#    and nobody sees it; it exists so that a crash, a closed tab or a lost
#    connection does not cost the user their perimeter, their areas of interest
#    and their step-2 choices. It is device-local: it never reaches the server's
#    disk and never leaves the machine it was made on.
#
#  * vftStateServer()'s save modal writes the FULL state (VFT_STATE_KEYS) to a
#    file the user names and the browser downloads. That is the only way to
#    preserve a finished simulation, and it is the file step 1's existing
#    loader reads back.
#
#Until 2026-09-01 the app had neither: it fired shinyjs::click("downloadSave")
#at two step confirmations, so every user collected unrequested timestamped
#.RData files in their Downloads folder and crash recovery meant finding the
#right one among them. Both clicks are gone.


#' Push the lightweight snapshot to the browser
#'
#' Called from the `then =` of every vftCommit() in app_server(), which means it
#' runs only for writes that actually happened - a cancelled invalidation modal
#' never reaches `then`, so a snapshot is never taken of a state the user
#' declined to create.
#'
#' @param r the app-level reactiveValues
#' @param session the shiny session
#' @noRd
vftSnapshotWrite <- function(r, session){

  #Never take a confirm handler down with it. A snapshot is a convenience; a
  #failed one must cost the user nothing but a debug line.
  tryCatch({

    vals    <- vftStateFromR(r, VFT_SNAPSHOT_KEYS)
    payload <- vftStateEncode(vals)

    #finalPolygons are raster-derived, so their vertex count is bounded by the
    #resolution and the size of the area rather than by anything the user did.
    #They are the only member of the snapshot that can plausibly run away, so
    #they are the first thing dropped. Everything else together is a few KB.
    if(nchar(payload) > VFT_SNAPSHOT_MAX_CHARS){
      vftDbg(paste0("SNAPSHOT: ", round(nchar(payload) / 1024), " KB is over ",
                    "budget, retrying without finalPolygons"))
      vals[["finalPolygons"]] <- NULL
      payload <- vftStateEncode(vals)
    }

    if(nchar(payload) > VFT_SNAPSHOT_MAX_CHARS){
      vftDbg(paste0("SNAPSHOT: still ", round(nchar(payload) / 1024),
                    " KB, not stored"))
      return(invisible(FALSE))
    }

    step <- tryCatch(vftStepForCode(shiny::isolate(r$step)),
                     error = function(e) NULL)

    session$sendCustomMessage("vft-state-save", list(
      v       = 1L,
      ts      = format(Sys.time(), "%d.%m.%Y %H:%M"),
      step    = if(is.null(step)) "" else step,
      payload = payload
    ))

    vftDbg(paste0("SNAPSHOT: stored ", round(nchar(payload) / 1024), " KB at ",
                  if(is.null(step)) "?" else step))
    invisible(TRUE)

  }, error = function(e){
    vftDbg(paste0("SNAPSHOT failed: ", conditionMessage(e)))
    invisible(FALSE)
  })
}


#' Default name offered in the save dialog
#'
#' @noRd
vftSaveDefaultName <- function(r){

  dateTime <- gsub(":|-| ", "_", Sys.time())
  dateTime <- substr(dateTime, 1, nchar(dateTime) - 3)

  #The old filename switched on r$step, an INTEGER, so switch() picked by
  #position and its "2" =, "3" = names were decoration that disagreed with the
  #positions. Nothing has to guess any more: the registry maps the code to a
  #step, and the user can overwrite the whole name anyway.
  step <- tryCatch(vftStepForCode(shiny::isolate(r$step)),
                   error = function(e) NULL)

  paste0("visitorFlowSave",
         if(is.null(step)) "" else paste0("_", step),
         "_", dateTime)
}


#' Turn whatever the user typed into a safe .RData filename
#'
#' @noRd
vftSaveFileName <- function(name, r){

  nm <- tryCatch(as.character(name)[1], error = function(e) NA_character_)
  if(is.null(nm) || is.na(nm)) nm <- ""

  nm <- trimws(nm)
  nm <- sub("[.][Rr][Dd][Aa][Tt][Aa]$", "", nm)
  #Content-Disposition is a header: a newline or a quote in it is a header
  #injection, and a slash is a path. Anything that is not a letter, a digit,
  #a space, a dot, an underscore or a hyphen becomes an underscore.
  nm <- gsub("[^[:alnum:] ._-]", "_", nm)
  nm <- trimws(nm)

  if(!nzchar(nm)) nm <- vftSaveDefaultName(r)

  paste0(substr(nm, 1, 100), ".RData")
}


#' The save dialog and the crash-recovery prompt
#'
#' One pair of observers for the session, wired once from app_server().
#'
#' @noRd
vftStateServer <- function(r, input, session){

  #### the disk icon next to the title ####
  shiny::observeEvent(input$saveButton, {

    tr <- .vftT(session)

    shiny::showModal(shiny::modalDialog(
      title = tr("Sitzung speichern"),
      shiny::tags$p(tr("Die Datei enthält den vollständigen Stand dieser Sitzung.")),
      shiny::textInput("saveName", tr("Name der Datei"),
                       value = vftSaveDefaultName(r), width = "100%"),
      footer = shiny::tagList(
        #A real download button, not a hidden one clicked from R: the browser's
        #own save dialog is the only "choose a location" a web page can offer,
        #and it opens on a genuine user click. downloadSave's filename function
        #reads input$saveName at request time, so whatever is in the box when
        #this is pressed is the name the file gets.
        shiny::downloadButton("downloadSave", tr("Herunterladen")),
        shiny::modalButton(tr("Stornieren"))
      ),
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  #### crash recovery ####
  #
  #vft-state.js sends this once, on shiny:connected, and only when it actually
  #found something. `once` because a reconnect must not re-offer a restore the
  #user has already answered.
  shiny::observeEvent(input$vftStoredState, once = TRUE, {

    stored <- input$vftStoredState
    vals   <- vftStateDecode(tryCatch(stored$payload, error = function(e) NULL))

    if(is.null(vals)){
      vftDbg("RESTORE: stored snapshot unreadable, clearing it")
      session$sendCustomMessage("vft-state-clear", list())
      return(NULL)
    }

    #Hold it aside rather than closing over it: the two footer buttons are
    #rendered inside the modal and their observers are registered below, once
    #for the session.
    session$userData$vftStored <- vals

    tr    <- .vftT(session)
    step  <- tryCatch(as.character(stored$step), error = function(e) "")
    label <- if(nzchar(step) && !is.null(VFT_STEPS[[step]])) VFT_STEPS[[step]]$label else NULL

    shiny::showModal(shiny::modalDialog(
      title = tr("Frühere Sitzung gefunden"),
      shiny::tags$p(tr("Auf diesem Gerät wurde eine unterbrochene Sitzung gefunden.")),
      shiny::tags$p(shiny::tags$strong(
        paste0(if(is.null(label)) "" else paste0(label, " – "),
               tryCatch(as.character(stored$ts), error = function(e) "")))),
      #Said plainly, because it is the one thing about this that will surprise
      #someone: the snapshot carries choices and geometry, not the computed
      #layers, so the app resumes at the furthest step those choices reach.
      shiny::tags$p(tr("Ihre Auswahl und Ihre Gebiete werden wiederhergestellt. Berechnete Ergebnisse (Sensitivitätsmatrix, Wegenetz, Simulationen) müssen neu berechnet werden.")),
      footer = shiny::tagList(
        shiny::actionButton("vftStoredRestore", tr("Wiederherstellen"),
                            class = "btn-primary"),
        shiny::actionButton("vftStoredDiscard", tr("Verwerfen"))
      ),
      easyClose = FALSE
    ))
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  shiny::observeEvent(input$vftStoredRestore, {
    #A re-rendered actionButton reports 0, which observeEvent would otherwise
    #treat as a click - the same guard the commit observers carry.
    if(is.null(input$vftStoredRestore) || input$vftStoredRestore == 0) return(NULL)

    vals <- session$userData$vftStored
    shiny::removeModal()
    if(is.null(vals)) return(NULL)

    session$userData$vftStored <- NULL
    vftApplyState(r, vals, session)

    #Deliberately NOT cleared on restore: the snapshot is rewritten at the next
    #confirmation anyway, and until then it is still the best copy there is.
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$vftStoredDiscard, {
    if(is.null(input$vftStoredDiscard) || input$vftStoredDiscard == 0) return(NULL)

    session$userData$vftStored <- NULL
    session$sendCustomMessage("vft-state-clear", list())
    shiny::removeModal()
  }, ignoreInit = TRUE)

  invisible(NULL)
}
