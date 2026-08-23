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

  #This closure captures `target` and therefore the session - which is fine and
  #intended: it lives in the consumer, on this side, and is never serialised.
  q$consumer$addHandler(function(sig, obj, e){
    #a progress bar must never be able to kill the consumer that drains the queue
    tryCatch(switch(obj$op,
                    set   = do.call(target$set, obj$args),
                    inc   = do.call(target$inc, obj$args),
                    close = target$close()),
             error = function(err) NULL)
    NULL
  }, signal)

  #The handle's closures get an environment holding ONLY the producer and the
  #signal name. Built explicitly instead of letting them capture this function's
  #frame, which contains `progress`, `target` and `q` - capturing that frame is
  #exactly the leak being fixed, just moved. Parent is the package namespace so
  #the closures can still find what they need; namespaces serialise by reference.
  e <- new.env(parent = asNamespace("visitorFlowTool"))
  e$.prod <- q$producer
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
#' Serialise the outgoing message cold, before mirai sees it
#'
#' Every serialisation figure recorded before this existed was taken AFTER the
#' send returned, which means it measured a second pass over objects mirai had
#' already forced and materialised. That is warm, and warm is cheap. Whatever
#' first-touch costs -- an ALTREP vector materialising, a file-backed
#' SpatRaster reading cells off disk, a promise forced through a long closure
#' chain -- was billed to the send and then invisible to the probe.
#'
#' This runs first, so it pays and reports that cost per object. Cheap when the
#' objects are already in memory (~0.02s on a 5 MB message), and when it is not
#' cheap that is precisely the finding.
#' @keywords internal
#' @noRd
#' Move the big captures out of the mirai message and onto disk
#'
#' Six reproductions failed to make mirai::mirai() slow -- real tbl_graph and
#' PackedSpatRaster objects, saturated pools, 16 MB payloads, large returns --
#' while the app blocks for seconds on the same call, scaling with the bytes
#' captured. This stops trying to explain that and simply removes the bytes:
#' every capture at or above VFT_SPILL_MB is written to a temp .rds and replaced
#' by its path. The worker reads it back (see the restore loop in the wrapped
#' expression) so the job is unchanged.
#'
#' Uncompressed on purpose: the point is to spend as little main-thread time as
#' possible, and compression would trade the bytes we are trying to avoid for
#' CPU we cannot spare.
#'
#' If the freeze follows the payload out of the message, the transport was the
#' cause. If it does not, the payload is definitively innocent and the cost is
#' somewhere neither timing nor size can see. Either outcome ends the guessing.
#' @keywords internal
#' @noRd
.vftSpillArgs <- function(args){
  out <- list(args = args, names = character(0), files = character(0))
  thr <- suppressWarnings(as.numeric(Sys.getenv("VFT_SPILL_MB", "0.5")))
  if(!isTRUE(is.finite(thr)) || thr <= 0) return(out)
  lim <- thr * 1024^2
  nms <- names(args)
  if(is.null(nms)) return(out)
  for(i in seq_along(args)){
    nm <- nms[i]
    #the seed is ours and tiny; never round-trip it through disk
    if(!nzchar(nm) || nm == "..vftSeed..") next
    v <- args[[i]]
    #a path is only cheaper than the object if the object is actually big, and
    #object.size is far cheaper than serialising to find out
    if(!isTRUE(utils::object.size(v) > lim)) next
    ok <- tryCatch({
      p <- tempfile(pattern = "vftspill_", fileext = ".rds")
      saveRDS(v, p, compress = FALSE)
      out$args[[i]]  <- p
      out$names      <- c(out$names, nm)
      out$files      <- c(out$files, p)
      TRUE
    }, error = function(e) FALSE)
    #a spill that fails must leave the original in place, not a broken path
    if(!ok) out$args[[i]] <- v
  }
  out
}

.vftColdProbe <- function(wrapped, args){
  tryCatch({
    nms <- names(args)
    if(is.null(nms)) nms <- rep("", length(args))
    s <- vapply(seq_along(args), function(i){
      t <- as.numeric(Sys.time())
      tryCatch(length(serialize(args[[i]], NULL)), error = function(e) NA_real_)
      as.numeric(Sys.time()) - t
    }, numeric(1))
    tw <- as.numeric(Sys.time())
    tryCatch(length(serialize(wrapped, NULL)), error = function(e) NA_real_)
    ws <- as.numeric(Sys.time()) - tw
    tA <- as.numeric(Sys.time())
    ab <- tryCatch(length(serialize(c(list(wrapped), args), NULL)),
                   error = function(e) NA_real_)
    list(per      = stats::setNames(c(s, ws), c(nms, "<expression>")),
         whole_s  = as.numeric(Sys.time()) - tA,
         whole_MB = ab/1024^2,
         total_s  = sum(s, na.rm = TRUE) + ws)
  }, error = function(e) NULL)
}

.vftDispatchDiag <- function(ex, args, wrapped, sendSecs, globalsSecs, gcSecs,
                             cold = NULL){
  #self-targeting, but it must trigger on EITHER half. If the cold probe
  #absorbs the first-touch cost then send_s goes small and a send-only trigger
  #would log nothing at all -- losing exactly the evidence we added it for.
  coldTot <- if(is.null(cold)) 0 else cold$total_s
  slow    <- (is.finite(sendSecs) && sendSecs >= 1) ||
             (is.finite(coldTot)  && coldTot  >= 1)
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
    #cold columns: what the first serialisation actually cost, per object and
    #for the whole message. These are the honest ones.
    d$cold_s      <- if(is.null(cold)) NA_real_ else
                       round(unname(cold$per[match(d$global, names(cold$per))]), 3)
    d$cold_tot_s  <- if(is.null(cold)) NA_real_ else round(cold$total_s, 3)
    d$cold_whl_s  <- if(is.null(cold)) NA_real_ else round(cold$whole_s, 3)
    d$cold_MB     <- if(is.null(cold)) NA_real_ else round(cold$whole_MB, 2)
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
            "cold_s","cold_tot_s","cold_whl_s","cold_MB","awaiting","executing")],
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
#' Drop-in for future::future(): same expression, same `seed = TRUE`, and the
#' result chains with %...>% and %...!% exactly as before.
#' @param expr the expression to evaluate remotely, as for future::future()
#' @param ... accepted for call-site compatibility; `seed = TRUE` is honoured
#' @param envir environment the expression's globals are collected from
vftFuture <- function(expr, ..., envir = parent.frame()){
  ex   <- substitute(expr)
  args <- list(...)
  #`envir` MUST be forced here. As a lazy default it would first evaluate at the
  #point of use, where parent.frame() is an unrelated frame, and the
  #expression's globals would be collected from the wrong environment.
  force(envir)
  useSeed <- isTRUE(args$seed)

  promises::promise(function(resolve, reject){
    tryCatch({
      #dev / load_all: global.R installs a sequential plan and never starts
      #daemons. Run inline so developing against the package keeps working, and
      #so a globals problem cannot hide behind a backend difference.
      conns <- tryCatch(mirai::status()$connections, error = function(e) 0L)
      if(!length(conns) || is.na(conns) || conns < 1L){
        resolve(eval(ex, envir))
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
                list(..vftSeed.. = if(useSeed) .vftNextSeed() else NULL))

      #SPILL. Six reproductions could not make mirai slow: a 9 MB tbl_graph
      #sends in 6 ms here, busy pool or idle, and 16 MB through a saturated pool
      #costs 13 ms. Yet in the app the same call blocks for seconds, and the
      #block scales with the bytes captured. Rather than keep hunting the
      #mechanism, take the bytes out of the message: anything large is written
      #to disk and replaced by its path, which the worker reads back. saveRDS of
      #6 MB is tens of milliseconds and is itself timed, so whatever happens
      #next is attributable. Set VFT_SPILL_MB=0 to switch this off.
      sp   <- vftTime("async:spill", .vftSpillArgs(argl))
      argl <- sp$args

      #the worker attaches the packages the expression needs, restores anything
      #that was spilled to disk, seeds itself from the substream we were given,
      #then runs the original expression spliced in verbatim. Helper names are
      #deliberately ugly so they cannot collide with a captured global.
      wrapped <- bquote({
        for(..vftPkg.. in .(pkgs))
          suppressWarnings(require(..vftPkg.., character.only = TRUE,
                                   quietly = TRUE))
        for(..vftSpill.. in .(sp$names))
          assign(..vftSpill.., readRDS(get(..vftSpill..)))
        if(!is.null(..vftSeed..))
          assign(".Random.seed", ..vftSeed.., envir = globalenv())
        .(ex)
      })

      #COLD probe. Everything .vftDispatchDiag measured before ran AFTER the
      #send, so it timed a second pass over objects mirai had already forced.
      #If the cost is first-touch (ALTREP materialisation, a file-backed
      #SpatRaster read from disk, a promise forced deep in a captured closure)
      #that measurement reads warm and lies. Serialise here, cold, and record
      #per-object times; then send. If send_s collapses while cold_s carries the
      #seconds, first-touch is the answer and the object name is in cold_s.
      cold <- .vftColdProbe(wrapped, argl)

      tS <- as.numeric(Sys.time()); gc0 <- sum(gc.time()[1:2])
      m  <- vftTime("async:send", mirai::mirai(wrapped, .args = argl))
      .vftDispatchDiag(ex, argl, wrapped, as.numeric(Sys.time()) - tS,
                       tS - tG, sum(gc.time()[1:2]) - gc0, cold)

      #the worker has read them by the time it answers; drop them either way
      #so a long session cannot silently fill the temp directory.
      done <- function(fn) function(x){ unlink(sp$files); fn(x) }
      promises::then(promises::as.promise(m),
                     onFulfilled = done(resolve), onRejected = done(reject))
    }, error = function(e) reject(e))
  })
}
