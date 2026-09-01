#### The cross-session job queue ####
#
# With VFT_WORKERS=1 every heavy job in the app runs one at a time, and the
# waiting happens inside mirai's dispatcher where the app cannot see it: a second
# user clicking "launch simulation" gets a progress bar that opens at 0% and then
# does not move for as long as somebody else's job takes. Nothing tells them they
# are waiting rather than running. mirai::status()$awaiting gives a queue DEPTH
# and nothing else - not whose job is where, not how far the running one has got
# - so position has to be tracked here.
#
# One ticket per vftFuture() dispatch, in dispatch order. mirai's dispatcher is
# FIFO, so rank in this registry IS queue position: with n workers the first n
# live tickets are running and everything after them is waiting. Each progress
# bar publishes its own value back into its ticket, which is what lets a QUEUED
# session paint itself with the RUNNING job's percentage.
#
# Everything in here is best-effort and wrapped: this is a display feature, and a
# display feature must never be the reason a job fails to dispatch.

#' The shared queue record, in .GlobalEnv for the same reason as .vftPerfState.
#'
#' load_all() or a reinstall builds a fresh namespace, and a registry living in
#' that namespace would split the queue in two - which defeats the entire point,
#' since the whole feature rests on every session in the process counting against
#' one list. See .vftP() in R/perf_helpers.R for the longer version.
.vftQ <- function(){
  st <- .GlobalEnv$.vftQState
  if(is.null(st)){
    st <- new.env(parent = emptyenv())
    #monotonic dispatch counter; a ticket's seq is its place in the FIFO
    st$seq  <- 0L
    #id -> ticket list
    st$jobs <- new.env(parent = emptyenv())
    #job label -> the last few observed durations, in seconds, for the estimate
    st$hist <- new.env(parent = emptyenv())
    .GlobalEnv$.vftQState <- st
  }
  st
}

#' How many past runs of a label to keep. Enough for a median to be meaningful,
#' few enough that a machine that has got slower is reflected within a session.
VFT_QUEUE_HIST_N <- 10L

#' A ticket that is created but never dispatched is a progress bar somebody
#' opened and then abandoned. It must not sit in the queue counting against
#' everyone else's position, so it lapses.
VFT_QUEUE_NEW_TTL_S <- 30

#' And nothing at all survives this. A promise that never settles - the dead
#' daemon case .vftEnsureDaemons() exists for - would otherwise pin a queue
#' position for the life of the process. Deliberately far longer than any real
#' job, so it can only ever catch something already broken.
VFT_QUEUE_MAX_S <- 2 * 3600

#' A session identity that is always a character scalar.
#'
#' Only ever compared for equality, so a bar can say "your own job" instead of
#' "another user's" - it is never displayed and never leaves the process. NULL is
#' a real case, not defensive padding: a job dispatched outside a session has no
#' token, and NULL propagating into the comparison is an error rather than a
#' FALSE.
.vftQueueToken <- function(session){
  tok <- tryCatch(session$token, error = function(e) NULL)
  if(!is.character(tok) || length(tok) != 1L || is.na(tok)) NA_character_ else tok
}

#' Take a ticket. State "new" until vftFuture() actually dispatches it.
#'
#' @param label human-readable job name - the progress bar's `message`. Fixed at
#'   creation on purpose: the ABM rewrites its message mid-run, and the duration
#'   history is keyed by this, so a mutating label would fragment the history.
#' @param session used only for `$token`, so a bar can tell "my own job" from
#'   "somebody else's" without ever reading another session's data.
.vftQueueTicket <- function(label = NULL, session = NULL){
  tryCatch({
    q  <- .vftQ()
    id <- basename(tempfile("vftJob_"))
    ok <- is.character(label) && length(label) == 1L && !is.na(label) && nzchar(label)
    q$jobs[[id]] <- list(
      id        = id,
      label     = if(ok) label else "Hintergrundaufgabe",
      token     = .vftQueueToken(session),
      state     = "new",
      seq       = NA_integer_,
      createdAt = Sys.time(),
      startedAt = NULL,
      value     = NA_real_
    )
    id
  }, error = function(e) NULL)
}

#' Join the FIFO. Called from vftFuture() at the moment of dispatch, so the
#' sequence number reflects the order the users actually clicked in.
.vftQueueEnqueue <- function(id){
  if(is.null(id)) return(invisible(NULL))
  tryCatch({
    q  <- .vftQ()
    tk <- q$jobs[[id]]
    if(is.null(tk)) return(invisible(NULL))
    q$seq        <- q$seq + 1L
    tk$seq       <- q$seq
    tk$state     <- "queued"
    q$jobs[[id]] <- tk
    #recompute ranks now, so whatever is at the front is stamped as started even
    #if it has no progress bar and nobody is ticking on its behalf
    .vftQueueList()
  }, error = function(e) NULL)
  invisible(NULL)
}

#' Publish where a running job has got to, so queued sessions can display it.
.vftQueueReport <- function(id, value = NA_real_){
  if(is.null(id)) return(invisible(NULL))
  tryCatch({
    q  <- .vftQ()
    tk <- q$jobs[[id]]
    if(is.null(tk)) return(invisible(NULL))
    #a report is proof the job is executing, whatever the rank arithmetic says
    if(!identical(tk$state, "running")){
      tk$state <- "running"
      if(is.null(tk$startedAt)) tk$startedAt <- Sys.time()
    }
    if(length(value) == 1L && is.finite(value)) tk$value <- max(0, min(1, value))
    q$jobs[[id]] <- tk
  }, error = function(e) NULL)
  invisible(NULL)
}

#' Give up the position, and remember how long the job took.
#'
#' Called from vftFuture()'s finally(), NOT from the progress bar's close(): the
#' ticket's lifetime is the job's, and several call sites close their bar inside
#' the worker or in a then() before the promise has settled.
.vftQueueDone <- function(id){
  if(is.null(id)) return(invisible(NULL))
  tryCatch({
    q  <- .vftQ()
    tk <- q$jobs[[id]]
    if(is.null(tk)) return(invisible(NULL))
    if(!is.null(tk$startedAt)){
      d <- as.numeric(difftime(Sys.time(), tk$startedAt, units = "secs"))
      if(is.finite(d) && d > 0){
        h <- c(q$hist[[tk$label]], d)
        if(length(h) > VFT_QUEUE_HIST_N) h <- h[seq.int(length(h) - VFT_QUEUE_HIST_N + 1L, length(h))]
        q$hist[[tk$label]] <- h
      }
    }
    if(!is.null(q$jobs[[id]])) rm(list = id, envir = q$jobs)
    #the job behind this one has just reached the front; stamp its start
    .vftQueueList()
  }, error = function(e) NULL)
  invisible(NULL)
}

#' The live tickets in dispatch order, swept, with the front ones promoted.
#'
#' This is the one place that decides who is running: the first
#' .vftWorkerCount() dispatched tickets are, everything behind them is waiting.
#' Promotion happens here rather than in the caller so that a job with no
#' progress bar still gets a startedAt - which is what the duration history and
#' the wait estimate are built from.
.vftQueueList <- function(){
  tryCatch({
    q   <- .vftQ()
    ids <- ls(q$jobs, all.names = TRUE)
    if(!length(ids)) return(list())
    now  <- Sys.time()
    jobs <- lapply(ids, function(i) q$jobs[[i]])

    age  <- vapply(jobs, function(tk)
      as.numeric(difftime(now, tk$createdAt, units = "secs")), numeric(1))
    drop <- (vapply(jobs, function(tk) identical(tk$state, "new"), logical(1)) &
               age > VFT_QUEUE_NEW_TTL_S) | age > VFT_QUEUE_MAX_S
    if(any(drop)){
      rm(list = vapply(jobs[drop], function(tk) tk$id, character(1)), envir = q$jobs)
      jobs <- jobs[!drop]
    }

    #a ticket that has not been dispatched yet occupies no position
    jobs <- jobs[!vapply(jobs, function(tk) identical(tk$state, "new"), logical(1))]
    if(!length(jobs)) return(list())

    jobs <- jobs[order(vapply(jobs, function(tk) tk$seq, integer(1)))]

    n <- .vftWorkerCount()
    for(i in seq_along(jobs)){
      if(i <= n && identical(jobs[[i]]$state, "queued")){
        jobs[[i]]$state        <- "running"
        jobs[[i]]$startedAt    <- now
        q$jobs[[jobs[[i]]$id]] <- jobs[[i]]
      }
    }
    jobs
  }, error = function(e) list())
}

#' Typical duration of a label, or NA before anything of that kind has finished.
.vftQueueMedian <- function(label){
  h <- tryCatch(.vftQ()$hist[[label]], error = function(e) NULL)
  if(!length(h)) return(NA_real_)
  stats::median(h)
}

#' Seconds still to run on a job that has already started.
#'
#' Prefer its own reported percentage - that reflects THIS run on THIS machine -
#' and only fall back to the historical median while the percentage is too small
#' to extrapolate from. Below 5% the linear projection is wild: at 1% after two
#' seconds it predicts 198 more.
.vftQueueRemaining <- function(tk){
  st <- tk$startedAt
  if(is.null(st)) return(NA_real_)
  el <- as.numeric(difftime(Sys.time(), st, units = "secs"))
  v  <- tk$value
  if(length(v) == 1L && is.finite(v) && v >= 0.05) return(max(0, el * (1 - v) / v))
  m <- .vftQueueMedian(tk$label)
  if(is.finite(m)) return(max(0, m - el))
  NA_real_
}

#' How long until the ticket at position `k` starts: what is left of the jobs in
#' front of it. NA when nothing ahead can be estimated at all, which is the case
#' on a freshly restarted process before the running job has reported 5%.
#'
#' A job ahead with no history contributes nothing rather than blocking the whole
#' estimate, so this can under-report. That is the right way round: "at least
#' this long" is useful, and a missing number is better than a made-up one.
.vftQueueEta <- function(jobs, k){
  if(k <= 1L) return(NA_real_)
  tot <- 0; have <- FALSE
  for(i in seq_len(k - 1L)){
    tk <- jobs[[i]]
    s  <- if(identical(tk$state, "running")) .vftQueueRemaining(tk)
          else .vftQueueMedian(tk$label)
    if(length(s) == 1L && is.finite(s)){ tot <- tot + s; have <- TRUE }
  }
  if(have) tot else NA_real_
}

#' m:ss, for "running since".
.vftFmtElapsed <- function(secs){
  if(!length(secs) || !is.finite(secs)) return(NA_character_)
  secs <- max(0, round(secs))
  sprintf("%d:%02d", secs %/% 60, secs %% 60)
}

#' This session's translator, or identity when there is none.
#'
#' app_server() parks the shiny.i18n Translator on session$userData, which module
#' proxies share (the same property .vftBusyStore() relies on). That is what lets
#' code down here - providers.R and prepare_network.R have no i18n in scope -
#' translate without five new function signatures. The Translator is an R6 object
#' mutated in place by the language observers, so a language switch is picked up
#' by the next call with no extra wiring.
#'
#' Keys are German, matching the `or` column of the translation CSVs, so a
#' deployment whose CSVs have not been updated degrades to readable German rather
#' than to a bare key.
.vftT <- function(session = NULL){
  tr <- tryCatch(session$userData$vftI18n, error = function(e) NULL)
  if(is.null(tr)) return(function(x) x)
  function(x) tryCatch(tr$t(x), error = function(e) x)
}

#' Paint one queued progress bar with the running job's numbers.
#'
#' @param sp the real shiny::Progress - we are on the main thread, so drive it
#'   directly, exactly as the queue consumer below does
#' @param jobs the ordered live tickets from .vftQueueList()
#' @param k this bar's index in `jobs`
.vftQueuePaint <- function(sp, sess, pid, jobs, k, first = FALSE){
  tr    <- .vftT(sess)
  me    <- jobs[[k]]
  run   <- jobs[[1L]]
  total <- length(jobs)

  if(isTRUE(first))
    tryCatch(sess$sendCustomMessage("vft-progress-class",
                                   list(id = pid, cls = "vft-queued", add = TRUE)),
             error = function(e) NULL)

  msg <- tryCatch(sprintf(tr("In Warteschlange – Position %d von %d"), k, total),
                  error = function(e) "In Warteschlange")

  #Only the LABEL of the job in front is ever shown, never anything belonging to
  #the user running it. And a ticket with no session cannot be claimed as yours,
  #so NA reads as "someone else" - the safe way round, since the point of the
  #line is to stop a user blaming a colleague for their own queued job.
  mine <- !is.na(me$token) && identical(run$token, me$token)
  who  <- if(mine) tr("Ihr eigener Auftrag") else tr("Auftrag eines anderen Nutzers")
  det <- paste0(who, ": ", run$label)

  if(length(run$value) == 1L && is.finite(run$value))
    det <- paste0(det, " – ", round(run$value * 100), " %")

  el <- if(is.null(run$startedAt)) NA_character_ else
    .vftFmtElapsed(as.numeric(difftime(Sys.time(), run$startedAt, units = "secs")))
  if(!is.na(el)) det <- paste0(det, " (", sprintf(tr("seit %s"), el), ")")

  eta <- .vftQueueEta(jobs, k)
  if(length(eta) == 1L && is.finite(eta))
    det <- paste0(det, " – ",
                  sprintf(tr("noch ca. %d Min."), max(1L, as.integer(round(eta / 60)))))

  val <- if(length(run$value) == 1L && is.finite(run$value)) run$value else 0
  tryCatch(sp$set(value = val, message = msg, detail = det), error = function(e) NULL)
  invisible(NULL)
}

#' Watch this bar's place in the queue and paint it red until its turn comes.
#'
#' Returns two callbacks for vftProgress() to wire up: `handOver()`, called from
#' the queue consumer when the worker itself reports (which is proof the job is
#' executing), and `stop()`.
#'
#' The ticker is a self-rescheduling later::later(), not an observe(): it must run
#' from a promise callback and from later()'s own event loop, where there is no
#' reactive domain - the trap documented at R/providers.R#845. ipc's consumer does
#' exactly the same thing one layer down, and drives the same shiny::Progress from
#' it, so this is the established path rather than a new one.
.vftQueueWatch <- function(sp, tid, message0 = NULL, detail0 = NULL){
  noop <- list(handOver = function(closed = FALSE) invisible(NULL),
               stop     = function() invisible(NULL))
  if(is.null(sp) || is.null(tid)) return(noop)

  priv <- tryCatch(sp$.__enclos_env__$private, error = function(e) NULL)
  if(is.null(priv)) return(noop)
  sess <- priv$session; pid <- priv$id
  if(is.null(sess) || is.null(pid)) return(noop)

  st <- new.env(parent = emptyenv())
  st$stopped <- FALSE
  #have we ever taken the bar over? nothing is restored if we never touched it
  st$painted <- FALSE
  #has the worker reported? if so its caption wins and must not be overwritten
  st$handed  <- FALSE

  release <- function(){
    if(st$stopped) return(invisible(NULL))
    st$stopped <- TRUE
    if(st$painted){
      tryCatch(sess$sendCustomMessage("vft-progress-class",
                                     list(id = pid, cls = "vft-queued", add = FALSE)),
               error = function(e) NULL)
      #put the caller's own caption back, but only if nothing real has arrived to
      #overwrite it - otherwise we would undo the worker's first message.
      if(!st$handed)
        tryCatch(sp$set(value = 0, message = message0, detail = detail0),
                 error = function(e) NULL)
    }
    invisible(NULL)
  }

  tick <- function(){
    if(st$stopped) return(invisible(NULL))
    again <- tryCatch({
      tk <- .vftQ()$jobs[[tid]]
      if(is.null(tk)){
        FALSE                                   #finished, or swept
      }else if(identical(tk$state, "new")){
        TRUE                                    #dispatch has not happened yet
      }else{
        jobs <- .vftQueueList()
        k    <- which(vapply(jobs, function(j) identical(j$id, tid), logical(1)))
        if(!length(k) || k[1L] <= .vftWorkerCount()){
          FALSE                                 #our turn - hand the bar back
        }else{
          .vftQueuePaint(sp, sess, pid, jobs, k[1L], first = !st$painted)
          st$painted <- TRUE
          TRUE
        }
      }
    }, error = function(e) FALSE)

    if(isTRUE(again)) later::later(tick, 1) else release()
    invisible(NULL)
  }
  later::later(tick, 1)

  #A session that leaves mid-wait keeps its queue position - the job is still
  #running in a daemon and still delays everybody behind it - but there is no
  #longer a bar to paint, so stop ticking at it. ipc's shinyQueue registers its
  #own onEnded on this same session for the same reason.
  tryCatch(sess$onSessionEnded(function() release()), error = function(e) NULL)

  list(
    handOver = function(closed = FALSE){
      st$handed <- TRUE
      if(!isTRUE(closed)){
        v <- tryCatch({
          gv <- sp$getValue(); mn <- sp$getMin(); mx <- sp$getMax()
          if(is.null(gv) || !is.finite(mx - mn) || mx <= mn) NA_real_
          else (gv - mn) / (mx - mn)
        }, error = function(e) NA_real_)
        .vftQueueReport(tid, value = v)
      }
      release()
    },
    stop = release
  )
}


#' A progress bar a worker can drive without dragging the session with it.
#'
#' `ipc::AsyncProgress` is built to be passed into a future, but it cannot be
#' passed cheaply. Its private$progress is a `shiny::Progress`, which holds the
#' Shiny session, which holds every reactive value in it - so serialising the
#' progress bar serialises the user's whole accumulated state.
#'
#' Measured on 2026-08-21 with VFT_PAYLOAD=1, this was not a rounding error: the
#' progress object was 385 MB at step5_server.R#1165, 120 MB and 117 MB at the
#' two step-4 sites, 78 MB at step2, 25 MB at step1 - against 2.5-3.6 MB for the
#' `network` those futures exist to work on. The sizes climb with step number
#' because r$ accumulates as the user advances, which is the giveaway that it is
#' session state travelling and not anything the job needs. Confirmed directly:
#' putting 38 MB on a mock session grew AsyncProgress from 1.73 MB to 39.88 MB,
#' while the queue's producer stayed flat at 0.08 MB.
#'
#' The worker never needed any of it. AsyncProgress$set/inc/close only call
#' private$queue$producer$fireEval(); the `private$progress$set` inside that
#' block is a *quoted expression* sent over the queue and evaluated back on the
#' main thread. Only the producer has to cross the process boundary.
#'
#' So: the real progress bar stays here, a handler on this side drives it, and
#' the future gets a handle holding nothing but the producer and a signal name.
#' Same `$set()` / `$inc()` / `$close()` API, so call sites - including the ones
#' inside launchSim_v2.R and launchMultiSim.R that take `progress` as an
#' argument - are unchanged.
#'
#' Note the queue itself is NOT safe to send (1.7 MB even when empty, because the
#' consumer holds the progress reference); it is specifically `queue$producer`
#' that is small. Sending `progress$.__enclos_env__$private$queue` would undo
#' most of the gain.
#'
#' @param ... passed to ipc::AsyncProgress$new() (value, message, detail, ...)
#' @param millis how often the main thread drains the queue
#' @return a list with $set, $inc and $close, safe to capture in a future
vftProgress <- function(..., millis = 1000){
  progress <- ipc::AsyncProgress$new(..., millis = millis)

  #The underlying shiny::Progress. We are on the main thread in the handler, so
  #drive it directly rather than going back through AsyncProgress' own methods -
  #those re-post to the queue, costing an extra drain cycle (up to `millis`) of
  #lag on every update. Fall back to the public API if ipc ever changes shape.
  sp <- tryCatch(progress$.__enclos_env__$private$progress,
                 error = function(e) NULL)
  target <- if(is.null(sp)) progress else sp

  q      <- progress$.__enclos_env__$private$queue
  signal <- basename(tempfile("vftProgress_"))

  #### the queue display ####
  #Every bar takes a ticket here, one call before vftFuture() joins it to the
  #FIFO. It has to happen at THIS end and not at the dispatch, because the bar is
  #what has the session, the shiny::Progress id, and the label to show.
  dots  <- list(...)
  tid   <- .vftQueueTicket(label = dots$message,
                           session = tryCatch(sp$.__enclos_env__$private$session,
                                              error = function(e) NULL))
  watch <- .vftQueueWatch(sp, tid, message0 = dots$message, detail0 = dots$detail)

  #This closure captures `target` and therefore the session - which is fine and
  #intended: it lives in the consumer, on this side, and is never serialised.
  q$consumer$addHandler(function(sig, obj, e){
    #a progress bar must never be able to kill the consumer that drains the queue
    tryCatch(switch(obj$op,
                    set   = do.call(target$set, obj$args),
                    inc   = do.call(target$inc, obj$args),
                    close = target$close()),
             error = function(err) NULL)
    #a message from the worker is proof the job is executing: stop the queue
    #ticker before it can overwrite what the worker just wrote, and republish the
    #new value so sessions waiting behind this job can display it.
    tryCatch(watch$handOver(closed = identical(obj$op, "close")),
             error = function(err) NULL)
    NULL
  }, signal)

  handle <- .vftProgressHandle(q$producer, signal)

  #the ticket id, so vftFuture() can join THIS bar to the job it is about to
  #dispatch. A character scalar on the outside of the list: it adds nothing to
  #what crosses to the worker, so the "capture only the producer" rule above
  #still holds.
  attr(handle, "vftTicket") <- tid
  handle
}

#' The $set/$inc/$close triple a worker drives one bar with.
#'
#' Split out of vftProgress() so vftProgressPair() below can build a SECOND one
#' against the same queue without duplicating the part that has to be exactly
#' right.
#'
#' The closures' environment is built explicitly and holds ONLY the producer and
#' the signal name. Letting them capture the calling frame instead is precisely
#' the leak documented above - that frame holds the shiny::Progress, which holds
#' the session, which holds every reactive value in it. Parent is the package
#' namespace so the closures can still find what they need; namespaces serialise
#' by reference.
.vftProgressHandle <- function(producer, signal){
  e <- new.env(parent = asNamespace("visitorFlowTool"))
  e$.prod <- producer
  e$.sig  <- signal

  handle <- list(
    set = function(value = NULL, message = NULL, detail = NULL)
      .prod$fire(.sig, list(op = "set",
                            args = list(value = value, message = message,
                                        detail = detail))),
    inc = function(amount = 0.1, message = NULL, detail = NULL)
      .prod$fire(.sig, list(op = "inc",
                            args = list(amount = amount, message = message,
                                        detail = detail))),
    close = function()
      .prod$fire(.sig, list(op = "close", args = list()))
  )
  lapply(handle, function(f){ environment(f) <- e; f })
}

#' Two bars in sequence, driven by ONE job over one queue and one FIFO position.
#'
#' Step 5's launch is one piece of work with two halves - prepare the data, then
#' run the ABM - and it used to be two dispatches for no reason other than that
#' each half wanted a progress bar of its own. That cost a second place in the
#' worker queue (behind other users, not just behind yourself), a second
#' serialisation of the graph in each direction, and a second Shiny flush.
#'
#' A single dispatch cannot open a bar halfway through: the handle has to exist
#' before the expression is sent. So both handles are built here, and the second
#' bar's shiny::Progress is created LAZILY - by the main-thread handler, on the
#' first message the worker sends to it. Nothing is on screen for the second half
#' until the second half actually starts, which is the whole point.
#'
#' ONE TICKET for the pair, because the ticket belongs to the job and there is
#' one job. Bar 2 reports its own fraction against that ticket, so a session
#' queued behind this one keeps seeing a moving estimate once the long half has
#' taken over; without it the queue would sit at "preparation, 100 %" for the
#' entire ABM.
#'
#' @param ... passed to ipc::AsyncProgress$new() for the FIRST bar
#' @param message2,detail2 the second bar's caption, applied when it is created
#' @param millis how often the main thread drains the queue
#' @return list(prep = , sim = ), each a handle as vftProgress() returns, with the
#'   queue ticket as an attribute on the LIST so vftFuture() finds it there
vftProgressPair <- function(..., message2 = NULL, detail2 = NULL, millis = 1000){
  progress <- ipc::AsyncProgress$new(..., millis = millis)

  sp     <- tryCatch(progress$.__enclos_env__$private$progress,
                     error = function(e) NULL)
  target <- if(is.null(sp)) progress else sp
  sess   <- tryCatch(sp$.__enclos_env__$private$session, error = function(e) NULL)

  q    <- progress$.__enclos_env__$private$queue
  sig1 <- basename(tempfile("vftProgress_"))
  sig2 <- basename(tempfile("vftProgress2_"))

  dots  <- list(...)
  tid   <- .vftQueueTicket(label = dots$message, session = sess)
  watch <- .vftQueueWatch(sp, tid, message0 = dots$message, detail0 = dots$detail)

  #### bar 1: exactly what vftProgress() does ####
  q$consumer$addHandler(function(sig, obj, e){
    tryCatch(switch(obj$op,
                    set   = do.call(target$set, obj$args),
                    inc   = do.call(target$inc, obj$args),
                    close = target$close()),
             error = function(err) NULL)
    tryCatch(watch$handOver(closed = identical(obj$op, "close")),
             error = function(err) NULL)
    NULL
  }, sig1)

  #### bar 2: created by its first message, and never before ####
  #An environment rather than a local: the handler has to create the bar once and
  #then find it again on every message after that.
  st <- new.env(parent = emptyenv())
  st$bar  <- NULL
  st$done <- FALSE

  #A session that leaves mid-run must not leave a bar behind it, and closing a
  #Progress twice - or on a session that has gone - raises rather than no-ops.
  tryCatch(sess$onSessionEnded(function(){
    if(!is.null(st$bar)) try(st$bar$close(), silent = TRUE)
    st$bar <- NULL; st$done <- TRUE
  }), error = function(e) NULL)

  q$consumer$addHandler(function(sig, obj, e){
    tryCatch({
      if(st$done) return(NULL)

      #A close arriving on a bar that was never opened is the failure path -
      #vftAsyncError() closing both halves of a job that died during preparation.
      #Creating a bar in order to close it would flash one on screen for a phase
      #the job never reached.
      if(is.null(st$bar) && identical(obj$op, "close")){
        st$done <- TRUE
        return(NULL)
      }

      if(is.null(st$bar)){
        #the first message IS the hand-over: the first half is done with the
        #queue ticker, so let it go before painting anything of our own.
        tryCatch(watch$stop(), error = function(err) NULL)
        st$bar <- shiny::Progress$new(session = sess)
        st$bar$set(value = 0, message = message2, detail = detail2)
      }

      switch(obj$op,
             set   = do.call(st$bar$set, obj$args),
             inc   = do.call(st$bar$inc, obj$args),
             close = { st$bar$close(); st$bar <- NULL; st$done <- TRUE })
    }, error = function(err) NULL)

    #keep the FIFO's estimate honest for whoever is waiting behind this job
    tryCatch({
      v <- if(identical(obj$op, "set")) obj$args$value else NULL
      if(length(v) == 1L && is.finite(v)) .vftQueueReport(tid, value = v)
    }, error = function(err) NULL)
    NULL
  }, sig2)

  out <- list(prep = .vftProgressHandle(q$producer, sig1),
              sim  = .vftProgressHandle(q$producer, sig2))
  attr(out, "vftTicket") <- tid
  out
}

#' Where a dispatch came from, for the payload log.
#'
#' The expression carries its own source location when the package is installed
#' with keep_source = TRUE, which is how the line profiler is already being run.
#' Without srcrefs this returns NA rather than failing - the sizes are still the
#' useful part, they just cannot be attributed to a call site.
#' Reported line is the FIRST STATEMENT INSIDE the block, i.e. one line below the
#' vftFuture({ call itself. That is deliberate: "wholeSrcref" spans the entire
#' source file and reports line 1 for every site in it, which cannot tell step1's
#' two dispatches apart from each other. The per-statement srcref can.
.vftCallSite <- function(ex){
  tryCatch({
    sf  <- attr(ex, "srcfile")
    if(is.null(sf)) return(NA_character_)
    refs <- attr(ex, "srcref")
    line <- if(length(refs)) as.integer(refs[[1L]])[1L]
            else if(!is.null(attr(ex, "wholeSrcref"))) as.integer(attr(ex, "wholeSrcref"))[1L]
            else return(NA_character_)
    paste0(basename(sf$filename), "#", line)
  }, error = function(e) NA_character_)
}

#' Measure what a dispatch is about to ship to a worker. Diagnostic only.
#'
#' WARNING: this makes the app SLOWER while it is on. serializedSize() measures
#' an object by serialising it, so every global gets serialised once to weigh it
#' and again to actually send it - exactly the double work that setting
#' future.globals.maxSize = +Inf was introduced to remove. This exists to find
#' which captures are worth restructuring, then it goes back off. Never leave
#' VFT_PAYLOAD=1 set on the production host.
#'
#' Off unless VFT_PAYLOAD=1, and every failure is swallowed: a diagnostic must
#' never be the reason a user's job does not dispatch.
#' SUPERSEDED 2026-08-21 by .vftDispatchDiag(), and no longer called.
#'
#' It measures what future:::getGlobalsAndPackages() would capture, but the
#' transport now sends what globals::globalsOf() returns. Those are not
#' guaranteed to be the same set, so its 14.2 MB figure cannot be trusted as a
#' description of what actually crosses. Kept only because vftPayloadReport()
#' can still read historical logs.
.vftPayloadProbe <- function(ex, envir, elapsed){
  if(!identical(Sys.getenv("VFT_PAYLOAD", "0"), "1")) return(invisible(NULL))

  #force() here is load-bearing. `elapsed` arrives as the unevaluated promise
  #`as.numeric(Sys.time()) - t0`, and R evaluates a promise on FIRST USE - which
  #was inside the data.frame() below, after getGlobalsAndPackages() and every
  #serializedSize() call had already run. dispatch_s was therefore reporting the
  #dispatch PLUS this probe's own overhead, which is how the 2026-08-21 report
  #showed dispatch times RISING while the payload they measure fell 52x, and
  #disagreed with the line profile for the same line (13.4s vs 4.2s).
  #Read the clock before doing any work, not after.
  force(elapsed)
  probeStart <- as.numeric(Sys.time())

  tryCatch({
    gp <- future:::getGlobalsAndPackages(ex, envir = envir, globals = TRUE)
    g  <- gp$globals
    if(!length(g)) return(invisible(NULL))

    sizes <- vapply(g, function(x){
      tryCatch(as.numeric(parallelly::serializedSize(x)),
               error = function(e) NA_real_)
    }, numeric(1))

    dir <- Sys.getenv("VFT_PERF_DIR", "")
    if(!nzchar(dir)) dir <- file.path(tempdir(), "vft_perf")
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    f <- file.path(dir, paste0("vft_payload_", Sys.getpid(), "_",
                               format(Sys.Date(), "%Y%m%d"), ".csv"))

    utils::write.table(
      data.frame(time     = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                 site     = .vftCallSite(ex),
                 global   = names(g),
                 class    = vapply(g, function(x) class(x)[1L], character(1)),
                 bytes    = sizes,
                 dispatch_s = round(elapsed, 3),
                 #the probe's own cost, logged so it stays visible instead of
                 #silently inflating the number next to it
                 probe_s  = round(as.numeric(Sys.time()) - probeStart, 3),
                 stringsAsFactors = FALSE),
      file = f, sep = ",", row.names = FALSE,
      col.names = !file.exists(f), append = file.exists(f))
  }, error = function(e) invisible(NULL))
}

#' Rank what the async dispatches are shipping to workers.
#'
#' Reads the newest payload log and shows, per call site, the total bytes sent
#' and the biggest individual captures - the list to attack for Phase 3.
#' @param file a payload CSV; defaults to the newest in VFT_PERF_DIR
#' @param dir where to look when `file` is not given
vftPayloadReport <- function(file = NULL, dir = Sys.getenv("VFT_PERF_DIR", "")){
  if(!nzchar(dir)) dir <- file.path(tempdir(), "vft_perf")
  if(is.null(file)){
    fs <- list.files(dir, "^vft_payload_.*\\.csv$", full.names = TRUE)
    if(!length(fs)){
      message("No payload log in ", dir,
              " - run the app once with VFT_PAYLOAD=1 set before launching.")
      return(invisible(NULL))
    }
    file <- fs[which.max(file.mtime(fs))]
  }
  d <- utils::read.csv(file, stringsAsFactors = FALSE)
  if(!nrow(d)){ message("Payload log is empty."); return(invisible(NULL)) }

  d$MB <- d$bytes / 1024^2

  if(is.null(d$probe_s)) d$probe_s <- NA_real_

  cat("== PER DISPATCH SITE ==\n")
  cat("  (dispatch_s is the do.call alone; probe_s is this diagnostic's own cost,\n")
  cat("   which you are NOT paying with VFT_PAYLOAD unset)\n")
  bySite <- do.call(rbind, lapply(split(d, list(d$site, d$time), drop = TRUE),
                                  function(x) data.frame(site = x$site[1],
                                                         MB = sum(x$MB),
                                                         dispatch_s = x$dispatch_s[1],
                                                         probe_s = x$probe_s[1])))
  agg <- do.call(rbind, lapply(split(bySite, bySite$site), function(x)
    data.frame(site = x$site[1], n = nrow(x),
               MB_per_dispatch = round(mean(x$MB), 1),
               total_MB = round(sum(x$MB), 1),
               total_dispatch_s = round(sum(x$dispatch_s), 2),
               total_probe_s = round(sum(x$probe_s), 2))))
  print(agg[order(-agg$total_dispatch_s), ], row.names = FALSE)

  cat("\n== BIGGEST INDIVIDUAL CAPTURES ==\n")
  g <- do.call(rbind, lapply(split(d, list(d$site, d$global), drop = TRUE),
                             function(x) data.frame(site = x$site[1],
                                                    global = x$global[1],
                                                    class = x$class[1],
                                                    MB = round(mean(x$MB), 1),
                                                    n = nrow(x))))
  print(utils::head(g[order(-g$MB), ], 20), row.names = FALSE)
  invisible(d)
}

#' Find out why a slow dispatch was slow: size, or something that is not size.
#'
#' Always on, unlike VFT_PAYLOAD - but it only pays for itself when a dispatch is
#' ALREADY slow. Measuring a payload means serialising it, so measuring every
#' dispatch would reintroduce exactly the double-serialisation cost that setting
#' future.globals.maxSize = +Inf was meant to remove. A dispatch under the
#' threshold is not a problem and is never measured.
#'
#' This exists because on 2026-08-21 the send cost 21.3s that nothing accounts
#' for. The payload probe reported 14.2 MB, and no structure tested serialises
#' anywhere near slowly enough for that to cost 21s - the worst measured was
#' 0.01 s/MB for 300k tiny nested lists, and a 152 MB numeric matrix took 0.39s.
#' Ruled out by direct measurement: globals detection (0.32s in the same
#' profile), igraph and sf serialisation, socket backpressure (mirai::mirai() is
#' 0.00s with the pool saturated), and keep_source srcref inflation (13x on a
#' function, but only 0.219 MB in total).
#'
#' So either far more is crossing than the probe believed - it measures FUTURE's
#' globals, while the transport now sends the ones globals::globalsOf() returns,
#' which is a real discrepancy - or the send blocks for a reason that is not
#' serialisation at all. Logging bytes and serialisation seconds side by side,
#' per captured object, is what separates those two. Nothing measured so far
#' does, which is why this kept being guessed at instead of known.
#' @param ex the dispatched expression, for the call site
#' @param args exactly what was handed to mirai(), not a re-derivation of it
#' @param sendSecs how long mirai() itself took
#' @param globalsSecs how long globals detection took, for comparison

#' The configured worker count, parsed once and defended in one place.
#'
#' Lived inline in global.R, which meant the health check below could not agree
#' with it. VFT_WORKERS=1 is honoured deliberately: mirai::daemons(1) starts one
#' real daemon. Only unusable values fall back.
.vftWorkerCount <- function(){
  n <- suppressWarnings(as.integer(trimws(Sys.getenv("VFT_WORKERS", "2"))))
  if(is.na(n) || n < 1L) 2L else n
}

#' Start the daemon pool and warm every daemon, returning their pids.
#'
#' everywhere() rather than n separate mirai() calls: the dispatcher routes short
#' tasks to whichever daemon is idle, so n calls can all land on the same one and
#' leave the rest cold. .libPaths() is carried across explicitly because daemons
#' launch as bare Rscript and under Shiny Server frequently do not inherit it.
.vftStartDaemons <- function(n = .vftWorkerCount()){
  #record that a pool was started ON PURPOSE. .vftEnsureDaemons() must be able to
  #tell "the pool died" from "there was never meant to be a pool": in dev the app
  #deliberately runs with daemons(0), and reviving there would hand jobs to bare
  #Rscript daemons that cannot library() an uninstalled package.
  st <- .vftP(); st$daemonsWanted <- TRUE
  mirai::daemons(n)
  w <- mirai::everywhere({
    .libPaths(..vftLibs..)
    library(visitorFlowTool)
    Sys.getpid()
  }, ..vftLibs.. = .libPaths())
  mirai::call_mirai(w)
  unlist(lapply(w, function(x) if(is.numeric(x$data)) x$data else NA_integer_))
}

#' Restart the pool if every daemon has died.
#'
#' A daemon that dies -- crash, or the OOM killer on a 9.7 GiB host -- takes the
#' pool to connections = 0 while daemons() is still configured. In that state
#' mirai() keeps ACCEPTING work and queueing it, and nothing ever executes:
#' awaiting climbs, executing stays 0, and the job never resolves and never
#' errors. The user sees a spinner that runs forever. Reproduced by killing the
#' only daemon: a subsequent job was still unresolved after 10s with awaiting 1,
#' executing 0. With one worker a single death bricks the app silently.
#'
#' Only act on connections == 0, never on a merely reduced pool: at zero nothing
#' can be running, so restarting cannot cancel live work.
.vftEnsureDaemons <- function(){
  #never started on purpose -> dev, inline is the intended path, leave it alone
  if(!isTRUE(.vftP()$daemonsWanted)) return(invisible(0L))
  n <- .vftWorkerCount()
  conns <- tryCatch(mirai::status()$connections, error = function(e) NA_integer_)
  if(!length(conns) || is.na(conns) || conns > 0L) return(invisible(conns))
  message("vftFuture: all ", n, " daemon(s) are gone (connections = 0). ",
          "Jobs would queue forever without erroring; restarting the pool.")
  pids <- tryCatch(.vftStartDaemons(n), error = function(e){
    message("vftFuture: could not restart daemons: ", conditionMessage(e)); NA_integer_
  })
  message("vftFuture: daemon pool restarted; worker pids ",
          paste(pids, collapse = ", "))
  invisible(tryCatch(mirai::status()$connections, error = function(e) NA_integer_))
}

#' Per-job timeout in milliseconds, or NULL for none. OFF by default.
#'
#' This was defaulted on, at 1800s, to convert a silent hang into a visible
#' error. It caused a worse bug: mirai's .timeout starts when the job is QUEUED,
#' not when it starts running, so with one worker and three sessions the third
#' session's job sits behind two others with the clock already running and times
#' out having never executed. That cannot be fixed from here -- mirai gives no
#' way to separate queue time from run time -- so the default is off.
#'
#' The hang it guarded against is real (a dead daemon queues forever without
#' erroring) but .vftEnsureDaemons() addresses that directly by reviving the pool,
#' which is the right fix. Set VFT_TIMEOUT_S to a number of seconds to opt in.
.vftTimeoutMs <- function(){
  s <- suppressWarnings(as.numeric(trimws(Sys.getenv("VFT_TIMEOUT_S", "0"))))
  if(!isTRUE(is.finite(s)) || s <= 0) return(NULL)
  s * 1000
}

#' Send cumulative subsets of the args, to find where the cost appears.
#'
#' Established: each arg alone sends in 0.001-0.002s, all of them together take
#' as long as the real send (margs_s 6.955 against send_s 6.948), and the
#' expression is free (mexpr_s 0). So the cost is in some COMBINATION, and
#' singletons cannot show it. Thirteen synthetic reproductions failed -- arg
#' count, package functions among the args, and closures sharing one environment
#' are all instant on a spare machine -- so the bisect has to run against the
#' real objects, in the real process.
#'
#' Sends args[1], args[1:2], ... args[1:n] and times each. The step where the
#' time appears names the arg that is only expensive in company. Sorted by name
#' first so the order is stable between dispatches and the jump lands in the same
#' place every time, rather than moving with whatever order globalsOf returned.
.vftCumProbe <- function(args){
  if(!identical(Sys.getenv("VFT_MIRAI_PROBE", "0"), "1")) return(NULL)
  nms <- names(args)
  if(is.null(nms) || !length(nms)) return(NULL)
  keep <- !is.na(nms) & nzchar(nms)
  args <- args[keep]; nms <- nms[keep]
  o <- order(nms); args <- args[o]; nms <- nms[o]
  out <- rep(NA_real_, length(args))
  for(i in seq_along(args)){
    out[i] <- tryCatch({
      t <- as.numeric(Sys.time())
      p <- mirai::mirai(1L, .args = args[seq_len(i)])
      s <- as.numeric(Sys.time()) - t
      try(mirai::stop_mirai(p), silent = TRUE)
      s
    }, error = function(e) NA_real_)
  }
  stats::setNames(out, nms)
}

#' Split the real send into its two halves: the expression, and the args.
#'
#' The per-object probe came back with every capture at 0.001-0.002s while the
#' real send took 13s. The parts sum to ~0.01s; the whole is 13s. So it is not
#' any one object, and the probe never sent the one thing production does send
#' alongside them: the expression. Every probe so far used .expr = 1L.
#'
#'   expr_s slow, args_s ~0  -> it is the EXPRESSION. mirai(wrapped) carries the
#'                              bquote'd body, and with the package installed
#'                              keep.source = TRUE hangs srcrefs off it, each
#'                              pointing at a srcfile environment.
#'   args_s slow, expr_s ~0  -> it is the args TOGETHER, i.e. something shared
#'                              between them that sending one at a time cannot
#'                              show -- a common environment reached twice.
#'   both ~0                 -> the cost is in combining them, and the next cut
#'                              is cumulative subsets.
#'
#' Same opt-in as the per-object probe, same reason.
.vftSplitProbe <- function(wrapped, args){
  none <- c(expr = NA_real_, args = NA_real_)
  if(!identical(Sys.getenv("VFT_MIRAI_PROBE", "0"), "1")) return(none)
  one <- function(f){
    tryCatch({
      t <- as.numeric(Sys.time())
      p <- f()
      s <- as.numeric(Sys.time()) - t
      try(mirai::stop_mirai(p), silent = TRUE)
      s
    }, error = function(e) NA_real_)
  }
  c(expr = one(function() mirai::mirai(wrapped)),
    args = one(function() mirai::mirai(1L, .args = args)))
}

#' Time a mirai send of EACH captured object on its own, to name the culprit.
#'
#' The controls have narrowed this as far as aggregate numbers can. An empty send
#' is instant; a send of a raw vector of exactly the real message's length is
#' instant; R's own serialize() of the whole real message is instant (cold_s = 0);
#' /proc shows pure user CPU with majflt = 0, so no paging. Yet the real send
#' takes up to 14 seconds. It is therefore something specific IN the message, and
#' aggregates cannot say which thing.
#'
#' So send each captured object by itself and time it. Whatever is responsible
#' shows up as one row carrying the seconds, and it is named.
#'
#' Opt-in via VFT_MIRAI_PROBE=1, because if one object really does cost 14s then
#' probing it adds those seconds to the freeze. That is an acceptable price for
#' one diagnostic run and not something to inflict on every dispatch.
#'
#' Each probe carries `1L` as the expression, so only the captured object differs
#' from the empty ping that is already known to be instant.
.vftMiraiProbe <- function(args){
  if(!identical(Sys.getenv("VFT_MIRAI_PROBE", "0"), "1")) return(NULL)
  nms <- names(args)
  if(is.null(nms)) return(NULL)
  out <- rep(NA_real_, length(args))
  for(i in seq_along(args)){
    nm <- nms[i]
    #.args insists on non-empty names; skip anything unnamed rather than error
    if(is.na(nm) || !nzchar(nm)) next
    out[i] <- tryCatch({
      t <- as.numeric(Sys.time())
      p <- mirai::mirai(1L, .args = stats::setNames(list(args[[i]]), nm))
      s <- as.numeric(Sys.time()) - t
      try(mirai::stop_mirai(p), silent = TRUE)
      s
    }, error = function(e) NA_real_)
  }
  stats::setNames(out, nms)
}

.vftDispatchDiag <- function(ex, args, wrapped, sendSecs, globalsSecs, gcSecs,
                             mprobe = NULL,
                             sprobe = c(expr = NA_real_, args = NA_real_),
                             cprobe = NULL){
  #the cold/ping half of this trigger went with the probes it measured; the send
  #is the only half left that can be slow.
  slow <- is.finite(sendSecs) && sendSecs >= 1
  if(!slow) return(invisible(NULL))
  tryCatch({
    dir <- Sys.getenv("VFT_PERF_DIR", "")
    if(!nzchar(dir)) dir <- file.path(tempdir(), "vft_perf")
    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    st <- tryCatch(mirai::status()$mirai, error = function(e) NULL)
    nms <- names(args)
    if(is.null(nms)) nms <- rep("", length(args))
    rows <- lapply(seq_along(args), function(i){
      x  <- args[[i]]
      t  <- as.numeric(Sys.time())
      b  <- tryCatch(length(serialize(x, NULL)), error = function(e) NA_real_)
      data.frame(global = nms[i], class = class(x)[1], bytes = b,
                 ser_s = round(as.numeric(Sys.time()) - t, 3),
                 stringsAsFactors = FALSE)
    })
    #the expression itself was never measured before: it is what mirai sends
    #alongside .args, and it carries srcrefs when the package is installed with
    #keep_source = TRUE.
    tw <- as.numeric(Sys.time())
    wb <- tryCatch(length(serialize(wrapped, NULL)), error = function(e) NA_real_)
    rows[[length(rows) + 1L]] <- data.frame(
      global = "<expression>", class = "language", bytes = wb,
      ser_s = round(as.numeric(Sys.time()) - tw, 3), stringsAsFactors = FALSE)
    d <- do.call(rbind, rows)
    #and the whole message serialised in ONE pass, which is what mirai actually
    #does. Summing the parts can hide a cost that only appears together.
    tA <- as.numeric(Sys.time())
    ab <- tryCatch(length(serialize(c(list(wrapped), args), NULL)),
                   error = function(e) NA_real_)
    d$whole_MB    <- round(ab/1024^2, 2)
    d$whole_ser_s <- round(as.numeric(Sys.time()) - tA, 3)
    d$gc_s        <- round(gcSecs, 3)
    #per-object mirai send time, aligned to the same rows as ser_s
    d$cum_s       <- if(is.null(cprobe)) NA_real_ else
                       round(unname(cprobe[match(d$global, names(cprobe))]), 3)
    d$mexpr_s     <- round(unname(sprobe["expr"]), 3)
    d$margs_s     <- round(unname(sprobe["args"]), 3)
    d$mirai_s     <- if(is.null(mprobe)) NA_real_ else
                       round(unname(mprobe[match(d$global, names(mprobe))]), 3)
    d$time      <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    d$site      <- .vftCallSite(ex)
    d$send_s    <- round(sendSecs, 3)
    d$globals_s <- round(globalsSecs, 3)
    d$awaiting  <- if(is.null(st)) NA else as.integer(st[["awaiting"]])
    d$executing <- if(is.null(st)) NA else as.integer(st[["executing"]])
    f <- file.path(dir, paste0("vft_dispatch_", Sys.getpid(), "_",
                               format(Sys.Date(), "%Y%m%d"), ".csv"))
    utils::write.table(
      d[, c("time","site","global","class","bytes","ser_s",
            "send_s","globals_s","gc_s","whole_MB","whole_ser_s",
            "mirai_s","cum_s","mexpr_s","margs_s","awaiting","executing")],
      file = f, sep = ",", row.names = FALSE,
      col.names = !file.exists(f), append = file.exists(f))
  }, error = function(e) NULL)
  invisible(NULL)
}

#' Read the slow-dispatch log written by .vftDispatchDiag().
#'
#' The question this answers in one line: does the serialisation time of what was
#' sent ADD UP to how long the send took? If it does, the fix is to send less
#' (plan Phase 3). If it does not - small payload, fast to serialise, slow send -
#' then mirai is blocking for some other reason and shrinking captures would have
#' been wasted work.
#' @param file a dispatch CSV; defaults to the newest in VFT_PERF_DIR
#' @param dir where to look when `file` is not given
vftDispatchReport <- function(file = NULL, dir = Sys.getenv("VFT_PERF_DIR", "")){
  if(is.null(file)){
    if(!nzchar(dir)) dir <- file.path(tempdir(), "vft_perf")
    fs <- list.files(dir, pattern = "^vft_dispatch_.*csv$", full.names = TRUE)
    if(!length(fs)){
      message("no slow dispatches were logged (none took over 1s)")
      return(invisible(NULL))
    }
    file <- fs[which.max(file.mtime(fs))]
  }
  d <- utils::read.csv(file, stringsAsFactors = FALSE)
  d$MB <- d$bytes/1024^2
  message("log: ", file, "  (", nrow(d), " captures across slow dispatches)")

  #build the per-dispatch table column by column. rbind()ing one data.frame per
  #group looks tidier but yields a column that order() rejects as non-numeric.
  grp <- split(seq_len(nrow(d)), paste(d$time, d$site))
  num <- function(f) vapply(grp, function(i) as.numeric(f(d[i, , drop = FALSE])), 0)
  per <- data.frame(
    site       = vapply(grp, function(i) as.character(d$site[i][1]), ""),
    send_s     = num(function(g) g$send_s[1]),
    ping_s     = round(num(function(g) if(is.null(g$ping_s)) NA else g$ping_s[1]), 3),
    pingbig_s  = round(num(function(g) if(is.null(g$pingbig_s)) NA else g$pingbig_s[1]), 3),
    mexpr_s    = round(num(function(g) if(is.null(g$mexpr_s)) NA else g$mexpr_s[1]), 3),
    margs_s    = round(num(function(g) if(is.null(g$margs_s)) NA else g$margs_s[1]), 3),
    globals_s  = num(function(g) g$globals_s[1]),
    payload_MB = round(num(function(g) sum(g$MB, na.rm = TRUE)), 1),
    ser_s      = round(num(function(g) sum(g$ser_s, na.rm = TRUE)), 2),
    gc_s       = round(num(function(g) g$gc_s[1]), 2),
    whole_MB   = round(num(function(g) g$whole_MB[1]), 1),
    whole_s    = round(num(function(g) g$whole_ser_s[1]), 2),
    cold_s     = round(num(function(g) if(is.null(g$cold_tot_s)) NA else g$cold_tot_s[1]), 2),
    awaiting   = num(function(g) g$awaiting[1]),
    executing  = num(function(g) g$executing[1]),
    stringsAsFactors = FALSE)
  per$unexplained_s <- round(per$send_s - per$whole_s - per$gc_s, 2)
  #the freeze the user feels is now BOTH halves: the cold serialisation we do
  #up front plus whatever mirai spends after it.
  per$freeze_s <- round(per$send_s + ifelse(is.na(per$cold_s), 0, per$cold_s), 2)
  per <- per[order(-per$freeze_s), ]
  message("
== PER SLOW DISPATCH (send_s is the main-thread freeze) ==")
  print(per, row.names = FALSE)
  message("  freeze_s = cold_s + send_s, the whole main-thread cost.",
          " cold_s is the FIRST serialisation of the message, paid before mirai",
          " sees it; send_s is what mirai then took. Earlier builds measured",
          " serialisation only after the send, i.e. warm, which reads cheap.",
          " If cold_s now carries the seconds and send_s has collapsed, the cost",
          " is first-touch materialisation and the object is named under COLD",
          " FIRST-TOUCH below. If send_s still carries them, it is neither",
          " payload nor GC nor first-touch.")
  message("  ping_s is an EMPTY mirai send (one integer) and pingbig_s a send of a",
          " raw vector of exactly the real message's length -- both taken in the",
          " same second, same process, same load. ping_s ~0 with pingbig_s ~0 and",
          " send_s in seconds means the COST IS THE CONTENT, not the size: the",
          " bytes move fine, something in the message costs far more than its",
          " length. ping_s ~0 with pingbig_s ALSO slow means the cost is the SIZE",
          " -- allocating and moving that many bytes -- which on a memory-tight",
          " host points at allocation rather than serialisation.")
  message("  mexpr_s is mirai(wrapped) with NO args; margs_s is mirai(1L, .args=argl)",
          " with the trivial expression. The per-object probe put every capture at",
          " 0.001-0.002s while the whole send took 13s, so the parts do not sum to",
          " the whole and the expression was the one thing never sent alone.",
          " mexpr_s slow -> it is the EXPRESSION. margs_s slow -> it is the args",
          " TOGETHER, something shared between them that one-at-a-time cannot show.",
          " Both ~0 -> the cost is in combining them.")

  if("mirai_s" %in% names(d) && any(is.finite(d$mirai_s))){
    cols <- c("site","global","class","MB","mirai_s","cum_s","ser_s","cold_s")
    cols <- cols[cols %in% names(d)]
    ms <- d[is.finite(d$mirai_s), cols]
    if(nrow(ms)){
      message("
== PER-OBJECT MIRAI SEND (VFT_MIRAI_PROBE=1) ==")
      ms$MB <- round(ms$MB, 2)
      #ordered by cum_s where we have it, so the table reads as the bisect it is:
      #rows in the order they were added, and the step where the seconds appear.
      print(utils::head(if("cum_s" %in% names(ms) && any(is.finite(ms$cum_s)))
                          ms[order(ms$cum_s), ] else ms[order(-ms$mirai_s), ],
                        25), row.names = FALSE)
      message("  each object sent ALONE through mirai, expression 1L, so only the",
              " object differs from the empty ping that is known to be instant.",
              " A large mirai_s beside a near-zero ser_s is the answer: R can",
              " serialise that object in no time, and mirai cannot. That object",
              " is what to stop capturing -- pass what the worker needs instead",
              " of the thing that holds it.")
      message("  cum_s is the cumulative bisect: args sent as [1], [1:2], [1:3]",
              " ... in name order, so the row where cum_s jumps is the arg that",
              " is only expensive IN COMPANY. mirai_s is that same arg sent",
              " ALONE. An arg with mirai_s ~0 and a large cum_s is the answer:",
              " harmless by itself, expensive once combined with what came",
              " before it.")
    }
  }

  if("cold_s" %in% names(d)){
    cs <- d[is.finite(d$cold_s) & d$cold_s > 0.05,
            c("site","global","class","MB","cold_s","ser_s")]
    if(nrow(cs)){
      message("
== COLD FIRST-TOUCH (per object: cold_s = first serialisation, ser_s = warm) ==")
      cs$MB <- round(cs$MB, 2)
      print(utils::head(cs[order(-cs$cold_s), ], 15), row.names = FALSE)
      message("  a large cold_s beside a near-zero ser_s means the object was not",
              " really in memory until it was serialised.")
    }
  }

  message("
== BIGGEST INDIVIDUAL CAPTURES ==")
  top <- d[order(-d$bytes), c("site","global","class","MB","ser_s")]
  top$MB <- round(top$MB, 2)
  print(utils::head(top, 15), row.names = FALSE)
  invisible(list(per = per, captures = d))
}

#' Give a worker its own RNG stream, without depending on how the pool was built.
#'
#' mirai can seed its daemons via daemons(seed = ), but future.mirai creates the
#' daemons here, so that hook is not ours to set. Instead each dispatch takes the
#' next L'Ecuyer-CMRG substream from a single main-thread stream and hands it to
#' the worker - which is exactly what future(seed = TRUE) does, and keeps the
#' guarantee that two concurrent jobs cannot draw the same random numbers.
#'
#' RNGkind() is restored on exit: switching the generator is a global side effect
#' and the app's own main-thread randomness must not change because a job was
#' dispatched.
.vftNextSeed <- local({
  stream <- NULL
  function(){
    if(is.null(stream)){
      old <- RNGkind("L'Ecuyer-CMRG")
      on.exit(RNGkind(old[1]), add = TRUE)
      stream <<- get(".Random.seed", envir = globalenv())
    }
    stream <<- parallel::nextRNGStream(stream)
    stream
  }
})

#' Standard rejection handler for a vftFuture() chain
#'
#' Every one of the six async sites chained `%...>%` and none attached `%...!%`.
#' An unhandled promise rejection inside an observer does not merely fail that
#' observer - it propagates, and under `runApp()` in a non-interactive process it
#' halts R. On a single-process Shiny Server that means one user's failed job
#' takes down every other user's session. That is how a worker error at step 5
#' turned into "Execution halted".
#'
#' So: close the progress bar, log the message where the server log will keep it,
#' tell the user, and re-enable whatever control was disabled for the run - but
#' never re-raise.
#'
#' @param progress a vftProgress/AsyncProgress handle to close, or NULL
#' @param what human-readable name of the job, used in the message
#' @param enable optional shinyjs input id to re-enable, so a failed run does not
#'   leave the launch button dead until reload
#' @return a function(e) suitable for `%...!%`
#' @keywords internal
vftAsyncError <- function(progress = NULL, what = "background job", enable = NULL){
  force(progress); force(what); force(enable)
  function(e){
    msg <- tryCatch(conditionMessage(e), error = function(...) "unknown error")

    #each of these is best-effort: the handler's own failure must not become a
    #second unhandled rejection on top of the first
    #
    #A vftProgressPair() is two bars behind one handle, and BOTH have to go: a
    #job that dies during preparation would otherwise leave the ABM bar armed for
    #the rest of the session. Tested on $prep rather than on is.list(), because a
    #single vftProgress() handle is a list too - of $set/$inc/$close. Closing the
    #second half of a pair that never opened is safe: its handler recognises a
    #close on a bar that does not exist and declines to create one.
    for(..bar.. in if(is.list(progress) && !is.null(progress$prep))
                     list(progress$prep, progress$sim) else list(progress))
      try(if(!is.null(..bar..)) ..bar..$close(), silent = TRUE)
    #several sites disable a pair of buttons together, so accept a vector; one
    #id that no longer exists must not stop the others being re-enabled.
    for(..id.. in enable) try(shinyjs::enable(..id..), silent = TRUE)

    message("vftFuture: ", what, " failed: ", msg)
    try(shiny::showNotification(paste0(what, " failed: ", msg),
                                type = "error", duration = NULL), silent = TRUE)
    invisible(NULL)
  }
}

#' Dispatch async work without freezing every other user.
#'
#' `future::future()` blocks the calling thread when every worker is busy - it
#' waits, synchronously, for one to free up. In a Shiny app that thread is the
#' single main thread shared by all sessions, so one user starting a job while
#' the pool is full freezes *everybody*, including sessions doing nothing at all.
#' Measured directly: with 2 workers busy on 5s jobs, creating a third future
#' took 5.11s of blocked main thread, while the same three jobs dispatched
#' through mirai::mirai() took 0.00s each.
#'
#' This function used to solve that by gating on nbrOfFreeWorkers() and retrying
#' on later()'s event loop. That gate works - it was re-verified holding
#' correctly with the pool saturated - but it could not be the whole answer,
#' because it had to give up eventually: future counts a worker free only once
#' its future has been *collected*, so one uncollected future would otherwise pin
#' a slot for the life of the process and the app would silently stop doing async
#' work. The escape hatch that prevented that hang re-introduced the freeze. With
#' maxWaitSecs compressed 60s -> 3s against two 12s jobs, the timeout path was
#' reproduced blocking the event loop for 9.10s in a single turn.
#'
#' In the 2026-08-21 profile that showed as 33.0s of self time on the dispatch
#' line - 21.5% of the whole profile, 50.9s of logged stalls across 9 dispatches,
#' worst single stall 11.9s, and 152.7 of 300 user-seconds lost. It was the
#' largest single cost in the app, and it is why the freeze lands right after
#' Confirm in step 4: that is when a second and third session already have jobs
#' running and the pool is full.
#'
#' So the transport changes rather than the waiting strategy. mirai queues a job
#' without blocking the caller, so there is nothing to gate on and no deadline to
#' expire: the gate, the poll loop and the escape hatch are all gone, and with
#' them the uncollected-future hazard that forced the hatch to exist.
#'
#' future is still what sets the pool up in global.R, and future.mirai puts its
#' daemons on mirai's default profile - verified by dispatching a bare
#' mirai::mirai() after the plan and confirming it ran in a daemon process, not
#' here. So this reaches the same warmed workers without touching the plan.
#'
#' Globals are collected through the public `globals` API rather than future's
#' internal getGlobalsAndPackages(); the two were checked to return identical
#' names and packages for a step-5 shaped expression, in 0.02s. That cost is
#' negligible and, importantly, is NOT what the 33s was - detection and
#' serialisation together measured 0.05s. The blocking was the wait, not the work.
#'
#### How much async work a session has outstanding ####

# One counter per session, incremented when vftFuture() dispatches and
# decremented when the promise settles either way. It exists because the step nav
# bar made a new mistake cheap: leave a step while its job is in flight, come
# back, and the module is rebuilt and dispatches the SAME job again. With
# VFT_WORKERS=1 the second one queues behind the first, and a user clicking
# through four steps can have four heavy raster crops outstanding against one
# daemon before the first has finished.
#
# The counter is a reactiveVal so the bar can grey itself out while work is in
# flight, rather than only refusing the click after the fact. It is created
# lazily on first dispatch, so a session that never runs a job never has one, and
# nothing here is conditional on the nav bar being switched on.
#
# This is a stop-gap. Stage 4 replaces it with per-key in-flight markers
# (r$.vftInflight[[key]]) that also make the second dispatch unnecessary rather
# than merely unreachable, because the result is memoised into r$.

#' How long a session may be held "busy" before the guard lets go, in seconds.
#'
#' A safety valve, NOT a timeout: nothing is cancelled and no job is abandoned.
#' It bounds the damage of a promise that never settles - the dead-daemon case
#' .vftEnsureDaemons() exists for, where jobs queue forever without erroring.
#' Without it, one such job would disable the nav bar for the rest of the
#' session, and since the revival only happens on the NEXT dispatch, blocking
#' navigation would be exactly what stops that dispatch from ever being made.
VFT_BUSY_MAX_S <- 120

#' The userData environment holding this session's job counter, creating it on
#' first use. NULL when there is no session (tests, or a job dispatched outside
#' one) - every caller here treats that as "not busy".
#'
#' `session$userData` is shared with the root session by module proxies, so a job
#' dispatched from inside step2_server counts against the same session the nav
#' bar reads.
.vftBusyStore <- function(session){
  if(is.null(session)) return(NULL)
  ud <- tryCatch(session$userData, error = function(e) NULL)
  if(is.null(ud)) return(NULL)
  if(is.null(ud$vftBusy)){
    ud$vftBusy      <- shiny::reactiveVal(0L)
    ud$vftBusySince <- NULL
  }
  ud
}

#' Count one job out. Records when the session went from idle to busy, which is
#' what VFT_BUSY_MAX_S is measured from.
.vftJobStart <- function(session){
  ud <- .vftBusyStore(session)
  if(is.null(ud)) return(invisible(0L))
  n <- shiny::isolate(ud$vftBusy()) + 1L
  if(n == 1L) ud$vftBusySince <- Sys.time()
  ud$vftBusy(n)
  invisible(n)
}

#' Count one job back in, however it ended.
.vftJobEnd <- function(session){
  ud <- .vftBusyStore(session)
  if(is.null(ud)) return(invisible(0L))
  n <- max(0L, shiny::isolate(ud$vftBusy()) - 1L)
  if(n == 0L) ud$vftBusySince <- NULL
  ud$vftBusy(n)
  invisible(n)
}

#' Does this session have async work outstanding?
#'
#' A REACTIVE read: called inside an observe() it re-runs that observe when the
#' count changes, which is how the nav bar greys and un-greys itself. Call it
#' inside isolate() from anywhere that must not take that dependency.
vftSessionBusy <- function(session = shiny::getDefaultReactiveDomain()){
  #.vftBusyStore(), not a bare read of ud$vftBusy: the counter is created on
  #first use, and the nav bar's observe runs long BEFORE the first job is
  #dispatched. Reading a counter that does not exist yet returns FALSE without
  #registering a dependency on anything, so the observe would never re-run when
  #the first job started and the bar would not grey out until something else
  #happened to invalidate it. Creating it here makes the dependency exist from
  #the first read, whichever side gets there first.
  ud <- .vftBusyStore(session)
  if(is.null(ud)) return(FALSE)

  n <- ud$vftBusy()
  if(!length(n) || is.na(n) || n <= 0L) return(FALSE)

  since <- ud$vftBusySince
  if(is.null(since)) return(TRUE)
  as.numeric(difftime(Sys.time(), since, units = "secs")) < VFT_BUSY_MAX_S
}

#' Drop-in for future::future(): same expression, same `seed = TRUE`, and the
#' result chains with %...>% and %...!% exactly as before.
#' @param expr the expression to evaluate remotely, as for future::future()
#' @param ... accepted for call-site compatibility; `seed = TRUE` is honoured
#' @param envir environment the expression's globals are collected from
#' @param progress the vftProgress() handle belonging to this job, if it has one.
#'   Only used to join the bar to its queue ticket, so the bar can show where in
#'   the queue the job is; passing nothing costs the job nothing but its own
#'   display. See the queue section at the top of this file.
vftFuture <- function(expr, ..., envir = parent.frame(), progress = NULL){
  ex   <- substitute(expr)
  args <- list(...)
  #`envir` MUST be forced here. As a lazy default it would first evaluate at the
  #point of use, where parent.frame() is an unrelated frame, and the
  #expression's globals would be collected from the wrong environment.
  force(envir)
  useSeed <- isTRUE(args$seed)

  #captured HERE, not inside the promise body: the body runs immediately but the
  #settle handlers do not, and getDefaultReactiveDomain() is not reliable by then.
  jobSession <- shiny::getDefaultReactiveDomain()
  .vftJobStart(jobSession)

  #Join the FIFO HERE, before the promise body, so the sequence number is the
  #order the users clicked in rather than the order the promise bodies happened
  #to run. A job with no progress bar still takes a ticket: it still occupies the
  #daemon, so it still delays everybody behind it and must still be counted.
  qid <- attr(progress, "vftTicket")
  if(is.null(qid)) qid <- .vftQueueTicket(label = NULL, session = jobSession)
  .vftQueueEnqueue(qid)

  p <- promises::promise(function(resolve, reject){
    tryCatch({
      #dev / load_all: global.R installs a sequential plan and never starts
      #daemons. Run inline so developing against the package keeps working, and
      #so a globals problem cannot hide behind a backend difference.
      #a dead pool queues forever without erroring, so try to revive it before
      #deciding we have no daemons and falling back to the main thread
      .vftEnsureDaemons()
      conns <- tryCatch(mirai::status()$connections, error = function(e) 0L)
      if(!length(conns) || is.na(conns) || conns < 1L){
        #Label it. This branch runs the WHOLE job synchronously on the shared main
        #thread, freezing every other user for its full duration -- the single worst
        #thing the app can do. Unlabelled it is invisible: it produces no
        #async:send rows at all, so the dispatch report goes quiet exactly when
        #the problem is at its worst and the stall lands in "unattributed".
        st <- .vftP()
        if(!isTRUE(st$inlineWarned)) {
          st$inlineWarned <- TRUE
          message("vftFuture: NO mirai daemons -- running async jobs INLINE on the ",
                  "main thread. Every job now blocks every user for its full ",
                  "duration. Install visitorFlowTool on this host and restart.")
        }
        resolve(vftTime("async:INLINE", eval(ex, envir)))
        return(invisible(NULL))
      }

      #globals detection and the send are timed SEPARATELY. In the 2026-08-21
      #profile lumping them together hid which half to attack: detection was
      #0.32s while the send was 21.28s. Different problems, different fixes.
      tG   <- as.numeric(Sys.time())
      pkgs <- NULL
      gl   <- vftTime("async:globals", {
        g    <- globals::globalsOf(ex, envir = envir, mustExist = FALSE,
                                   recursive = TRUE)
        pkgs <- setdiff(globals::packagesOf(g), c("base", "methods"))
        globals::cleanup(g)
      })

      argl <- c(as.list(gl),
                list(..vftSeed.. = if(useSeed) .vftNextSeed() else NULL,
                     ..vftLibs.. = .libPaths()))

      #The SPILL that used to sit here is gone, and so are the cold/ping probes
      #below it. All three existed to explain why this dispatch blocked for
      #seconds on a 0.1 MB message, on the theory that it was the bytes. The
      #2026-08-25 profile answered it: of 525 samples taken with vftFuture on
      #the stack, 92% were in `mirai::mirai -> do_mirai -> request` - mirai's own
      #compiled transport - while saveRDS took 0.08s and serialize 0.18s across
      #the whole run. The cost was never the message we build, so removing bytes
      #from it could not have helped, and the disk round trip was pure overhead.
      #See vftSendStacks() in perf_helpers.R, which is what read this.
      #
      #the worker attaches the packages the expression needs, seeds itself from
      #the substream we were given, then runs the original expression spliced in
      #verbatim. Helper names are deliberately ugly so they cannot collide with a
      #captured global.
      wrapped <- bquote({
        #daemons are spawned as bare Rscript and do NOT necessarily inherit the
        #app process library path - under Shiny Server they frequently do not.
        #Give them ours, or every package function the expression calls comes
        #back as "could not find function" from deep inside the job.
        .libPaths(..vftLibs..)
        ..vftMissing.. <- character(0)
        for(..vftPkg.. in .(pkgs))
          if(!suppressWarnings(require(..vftPkg.., character.only = TRUE,
                                       quietly = TRUE)))
            ..vftMissing.. <- c(..vftMissing.., ..vftPkg..)
        #require() returns FALSE instead of erroring, and discarding that turns a
        #precise, fixable problem into a confusing one raised much later and
        #somewhere else. Name the package and where the worker actually looked.
        if(length(..vftMissing..))
          stop("async worker could not attach: ",
               paste(..vftMissing.., collapse = ", "),
               " -- worker .libPaths(): ",
               paste(.libPaths(), collapse = " ; "), call. = FALSE)
        if(!is.null(..vftSeed..))
          assign(".Random.seed", ..vftSeed.., envir = globalenv())
        .(ex)
      })

      #opt-in, all of them: VFT_MIRAI_PROBE=1
      mprobe <- .vftMiraiProbe(argl)
      #and the two halves of the real message: expression alone, args alone
      sprobe <- .vftSplitProbe(wrapped, argl)
      #and where in the arg set the cost appears
      cprobe <- .vftCumProbe(argl)

      tS <- as.numeric(Sys.time()); gc0 <- sum(gc.time()[1:2])
      m  <- vftTime("async:send", mirai::mirai(wrapped, .args = argl,
                                              .timeout = .vftTimeoutMs()))
      .vftDispatchDiag(ex, argl, wrapped, as.numeric(Sys.time()) - tS,
                       tS - tG, sum(gc.time()[1:2]) - gc0, mprobe = mprobe,
                       sprobe = sprobe, cprobe = cprobe)

      promises::then(promises::as.promise(m),
                     onFulfilled = resolve, onRejected = reject)
    }, error = function(e) reject(e))
  })

  #finally(), not then(): the count has to come back down on a FAILED job too,
  #and the "19 | Connection reset" that prompted this guard is a rejection. The
  #returned promise settles exactly as `p` does, so every existing %...>% /
  #%...!% call site is unaffected.
  #The ticket is released HERE and nowhere else. Several call sites close their
  #progress bar inside the worker or in a then() that runs before the promise has
  #settled, so the bar's lifetime is shorter than the job's - and it is the job
  #that holds the daemon and the queue position.
  promises::finally(p, function(){ .vftQueueDone(qid); .vftJobEnd(jobSession) })
}
