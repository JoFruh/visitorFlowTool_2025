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
    try(if(!is.null(progress)) progress$close(), silent = TRUE)
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
}
