#' Where the shared main thread goes, and for how long.
#'
#' This deployment is a single R process serving every user, so there is exactly
#' one main thread. Anything that runs on it - a render, an observer, a download
#' handler - freezes *all* connected sessions for its full duration. That is the
#' whole performance story, and until now nothing measured it.
#'
#' Two instruments, meant to be used together:
#'
#'   vftPerfInit()  starts a heartbeat that detects when the thread was blocked
#'   vftTime()      names the block that was running, so a stall gets attributed
#'
#' The heartbeat alone tells you *that* users were frozen for 4 seconds. Wrapping
#' candidate blocks in vftTime() tells you *which* code did it. Neither is a
#' sampling profiler: the goal is not to find the slow line inside a function,
#' it is to rank the functions by how much shared-thread time they cost under
#' real concurrency, so the per-step optimisation passes attack them in order.
#'
#' Cost is a timestamp and, on a threshold breach, one appended CSV line. It is
#' meant to be left on in production, which is the only place the interesting
#' contention happens. Set VFT_PERF=0 to disable.


# --------------------------------------------------------------- state ---

#' The single shared state record, held in .GlobalEnv rather than in this
#' namespace - and that placement is the whole point, not laziness.
#'
#' The heartbeat below reschedules itself through later(), so each pending
#' callback holds a closure over whatever copy of the package it was created
#' from. Reinstalling or load_all()-ing during a long-lived R session builds a
#' *fresh* namespace: a new environment, its "already started" flag back to
#' FALSE, its session counter back to 0. The new copy therefore starts a second
#' heartbeat while the previous one keeps ticking against its own stale record,
#' and both write to the same log - which is exactly how the first real baseline
#' came back with every stall recorded two or three times and a session count of
#' 0 during a three-session run.
#'
#' Keeping the record in .GlobalEnv gives every namespace copy the same object to
#' find, which is the same reasoning behind the .GlobalEnv$.vft_* raster cache.
#' The generation counter then guarantees only one live chain: init claims the
#' next generation, and any tick holding an older one retires itself.
.vftP <- function(){
  st <- .GlobalEnv$.vftPerfState
  if(is.null(st)){
    st <- new.env(parent = emptyenv())
    st$enabled    <- FALSE
    st$file       <- NULL
    st$sessions   <- 0L
    st$generation <- 0L

    #labels of vftTime() blocks currently on the stack, innermost last
    st$stack      <- character(0)

    #the most recently *finished* labelled block: its name, when it ended, and
    #how long it ran. Needed because of an ordering problem inherent to the
    #heartbeat: a later() callback cannot run while the thread is blocked, so by
    #the time the stall is observed the block that caused it has already
    #returned and popped itself off the stack. Remembering the last completion
    #lets the tick look backwards and attribute the stall.
    #
    #lastDur is what keeps that attribution honest. "A labelled block ended
    #during the window" is far too weak a test on its own: a 50ms labelled block
    #that happens to finish just before 7 seconds of unlabelled work would take
    #the blame for all 7 seconds. The first clean baseline did exactly that -
    #two 7s stalls pinned on app:downloadSave while the log contained no `block`
    #row at all, which is the contradiction that gave the false positive away.
    st$lastLabel  <- NA_character_
    st$lastEnd    <- 0
    st$lastDur    <- 0
    st$rprof      <- NULL

    #debug tracing off unless asked for; read once rather than per vftDbg() call
    st$debug      <- identical(Sys.getenv("VFT_DEBUG", "0"), "1")

    #heartbeat period and the delay above it that counts as a stall, in seconds.
    #0.25s ticks bound the attribution window tightly without being a load in
    #themselves; 0.5s is above ordinary scheduling jitter but well below what a
    #user perceives as the app hanging.
    st$interval   <- 0.25
    st$threshold  <- 0.5

    .GlobalEnv$.vftPerfState <- st
  }
  st
}


# ----------------------------------------------------------------- log ---

#' One CSV line per event. Opened in append mode each time on purpose.
#'
#' Holding a connection open across the life of the process would risk losing a
#' buffered tail if the process is killed - which, when investigating a hang, is
#' exactly how the process tends to end. The write is a few dozen bytes and only
#' happens on a threshold breach, so the cost of reopening is irrelevant next to
#' the stalls being recorded.
vftPerfLog <- function(kind, label, ms){
  p <- .vftP()
  if(!p$enabled) return(invisible(NULL))
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ",",
                 kind, ",",
                 #a label with a comma would shift every later column
                 gsub(",", ";", label, fixed = TRUE), ",",
                 round(ms), ",",
                 p$sessions, "\n")
  #instrumentation must never be able to take the app down with it: an
  #unwritable log directory is a reason to lose measurements, not sessions
  try(cat(line, file = p$file, append = TRUE), silent = TRUE)
  invisible(NULL)
}


# ----------------------------------------------------------- heartbeat ---

#' Schedule the next tick and record how late the last one was.
#'
#' later() callbacks run only when R returns to its event loop, so a tick that
#' asked for 0.25s and arrives 4s later is direct evidence that the main thread
#' was busy for the missing 3.75s. That is the measurement - there is no way to
#' observe it from inside the blocking code itself, because the blocking code is
#' precisely what is not yielding.
.vftPerfTick <- function(expected, gen){
  p <- .vftP()

  #a chain from a superseded package load retires here rather than doubling
  #every row for the rest of the session
  if(!identical(gen, p$generation)) return(invisible(NULL))

  now  <- as.numeric(Sys.time())
  late <- now - expected

  if(late > p$threshold){
    #two tests, both required. The block must have finished inside the window we
    #lost - a completion older than that belongs to some earlier, already
    #reported stall - and it must account for at least half of that window, or
    #it is a bystander being blamed for whatever really ran. Anything failing
    #either test is honestly reported as unattributed, which is a signal to go
    #and label the code that actually ran, not noise to be tidied away.
    attributable <- p$lastEnd > (now - late - p$interval) &&
                    p$lastDur >= 0.5 * late
    label <- if(attributable) p$lastLabel else "unattributed"
    vftPerfLog("stall", label, late * 1000)
  }

  nxt <- as.numeric(Sys.time()) + p$interval
  later::later(function() .vftPerfTick(nxt, gen), p$interval)
  invisible(NULL)
}


# ---------------------------------------------------------------- api ---

#' Start collecting. Call once per process, from global.R.
#'
#' `dir` defaults to VFT_PERF_DIR, then to a stable directory under tempdir().
#' The resolved path is messaged so it lands in the Shiny Server log - on a
#' server you administer only through the app directory, that message is
#' realistically how you will find the file.
#'
#' Note the tempdir() default is per-process and is deleted when that R session
#' exits, taking the log with it. Set VFT_PERF_DIR to somewhere durable for any
#' run whose results you intend to keep.
vftPerfInit <- function(dir = Sys.getenv("VFT_PERF_DIR", ""),
                        interval = 0.25, threshold = 0.5){

  if(identical(Sys.getenv("VFT_PERF", "1"), "0")){
    message("vftPerf: disabled (VFT_PERF=0)")
    return(invisible(FALSE))
  }

  p <- .vftP()

  if(!nzchar(dir)) dir <- file.path(tempdir(), "vft_perf")
  ok <- dir.create(dir, showWarnings = FALSE, recursive = TRUE) || dir.exists(dir)
  if(!ok){
    message("vftPerf: cannot create ", dir, " - profiling off")
    return(invisible(FALSE))
  }

  #the pid keeps a restarted process from appending to its predecessor's log,
  #where the two would be indistinguishable but the session counts would not line up
  p$file      <- file.path(dir, paste0("vft_perf_", Sys.getpid(), "_",
                                       format(Sys.Date(), "%Y%m%d"), ".csv"))
  p$interval  <- interval
  p$threshold <- threshold
  p$enabled   <- TRUE

  #claim the next generation: any heartbeat still pending from an earlier load
  #of this package sees a generation it does not hold and stops on its next tick
  p$generation <- p$generation + 1L
  gen <- p$generation

  #a reload must not inherit a session count from the previous namespace, and
  #cannot trust the old one either - the sessions it counted are gone with it
  p$sessions <- 0L

  if(!file.exists(p$file)){
    try(cat("time,kind,label,ms,sessions\n", file = p$file), silent = TRUE)
  }

  .vftPerfTick(as.numeric(Sys.time()) + interval, gen)

  message("vftPerf: logging main-thread stalls > ", threshold * 1000, "ms to ", p$file)
  invisible(TRUE)
}

#' Run `expr`, timing it and naming it while it runs.
#'
#' Wrap the body of any render or observer under suspicion:
#'
#'   output$map <- leaflet::renderLeaflet(vftTime("step5:map", { ... }))
#'
#' Returns exactly what `expr` returns and never alters its behaviour, so it is
#' safe to leave in place permanently on the paths that turn out to matter. A
#' `return()` inside the block returns from the *enclosing function*, as it would
#' without the wrapper, and the timing is still recorded on the way out.
#' Nesting is fine - the stack keeps the innermost label, which is the specific
#' one, and each level reports its own duration.
vftTime <- function(label, expr){
  p <- .vftP()
  if(!p$enabled) return(expr)

  start <- as.numeric(Sys.time())
  p$stack <- c(p$stack, label)

  #on.exit so an error inside expr still pops the stack and records the time;
  #without it one failed render would mis-label every stall thereafter
  on.exit({
    end <- as.numeric(Sys.time())
    n   <- length(p$stack)
    if(n > 0) p$stack <- p$stack[-n]
    p$lastLabel <- label
    p$lastEnd   <- end
    p$lastDur   <- end - start
    if((end - start) > p$threshold){
      vftPerfLog("block", label, (end - start) * 1000)
    }
  }, add = TRUE)

  expr
}

#' Track how many sessions share the process, so stalls can be read against load.
#'
#' A 3s block with one user connected is a slow app; the same block with five
#' connected is 15 user-seconds of freeze. Ranking the fixes needs that second
#' number, which means every logged row has to carry the concurrency it happened
#' under.
vftPerfSessionStart <- function(session){
  p <- .vftP()
  if(!p$enabled) return(invisible(NULL))
  p$sessions <- p$sessions + 1L
  vftPerfLog("session", "open", 0)

  session$onSessionEnded(function(){
    p$sessions <- max(0L, p$sessions - 1L)
    vftPerfLog("session", "close", 0)
  })
  invisible(NULL)
}

# -------------------------------------------------------- debug output ---

#' Debug tracing that costs nothing when it is switched off.
#'
#' The app carried ~390 print() and cat(file = stderr(), ...) calls on live code
#' paths. Every one of them formats its argument and writes to the console -
#' which on Shiny Server means writing to the log - on the single thread every
#' user shares. The 2026-08-20 profile put print.default and its paste0 calls at
#' roughly 3.2s of main-thread self time, and some of the arguments are far from
#' free to format: print(r$networkList) renders an entire list of igraph objects.
#'
#' Routing them through here rather than deleting them keeps the tracing
#' available - set VFT_DEBUG=1 and it all comes back - while costing a function
#' call and an environment lookup when off. Critically, R's lazy argument
#' evaluation means a switched-off vftDbg(expensiveThing()) never evaluates its
#' argument at all, so the saving is the formatting *and* whatever producing the
#' value would have cost.
vftDbg <- function(...){
  if(isTRUE(.vftP()$debug)) print(...)
  invisible(NULL)
}

#' As vftDbg(), for the cat(file = stderr(), ...) tracing style.
#'
#' Argument order differs from the calls it replaces - `file` moves to the end -
#' which is why the rewrite drops the explicit `file = stderr()` at each site.
vftDbgCat <- function(...){
  if(isTRUE(.vftP()$debug)) cat(..., file = stderr())
  invisible(NULL)
}


# -------------------------------------------------------------- Rprof ---

#' Sample the call stack while the main thread is blocked.
#'
#' The heartbeat and vftTime() between them can say "the thread was frozen for
#' 12 seconds" but not what ran, and they never will: the heartbeat only gets to
#' execute once the thread yields, and vftTime() only knows about code somebody
#' already thought to label. When the first clean baseline came back 54% blocked
#' with 96% of it unattributed, hand-labelling more call sites would have been
#' guesswork about which of ~200 renders and observers to wrap.
#'
#' Rprof has no such blind spot. It samples the whole call stack on a timer
#' *inside* the running code, so the functions eating the main thread show up
#' whether or not anyone anticipated them. It profiles this process only, which
#' is exactly right here - the mirai daemons are not the problem being measured.
#'
#' Off by default: a sample every 20ms is a few percent of overhead and the file
#' grows around 20 MB an hour, which is fine for a measuring run and wrong as a
#' permanent production setting. Enable with VFT_RPROF=1 for the run, then read
#' it back with vftRprofReport().
vftRprofStart <- function(dir = Sys.getenv("VFT_PERF_DIR", ""), interval = 0.02){
  if(!identical(Sys.getenv("VFT_RPROF", "0"), "1")) return(invisible(FALSE))

  if(!nzchar(dir)) dir <- file.path(tempdir(), "vft_perf")
  ok <- dir.create(dir, showWarnings = FALSE, recursive = TRUE) || dir.exists(dir)
  if(!ok) return(invisible(FALSE))

  f <- file.path(dir, paste0("vft_rprof_", Sys.getpid(), "_",
                             format(Sys.Date(), "%Y%m%d"), ".out"))
  p <- .vftP()
  p$rprof <- f

  #line.profiling needs srcrefs, which an installed package only carries if it
  #was built with them; harmless to ask for either way, and function-level
  #attribution is already enough to pick the next thing to fix
  utils::Rprof(f, interval = interval, line.profiling = TRUE,
               memory.profiling = FALSE)
  message("vftPerf: Rprof sampling every ", interval * 1000, "ms to ", f)
  invisible(TRUE)
}

#' Stop sampling. Rprof buffers, so the file is only complete after this.
vftRprofStop <- function(){
  utils::Rprof(NULL)
  invisible(.vftP()$rprof)
}

#' Rank what actually held the main thread, worst first.
#'
#' Returns both views, because on their own each one misleads:
#'
#'   $self   time spent *in* a function, excluding what it called. This is
#'           almost always a primitive - `%*%`, terra's C entry points, sf's
#'           GEOS calls - and tells you the kind of work, not the place to fix.
#'   $total  time spent in a function *including* everything beneath it. This is
#'           where the app's own renders, observers and helpers appear, and is
#'           the view that answers "which of my code is blocking everyone".
#'
#' Read $total first to find the responsible app function, then $self to see
#' what it is spending its time on.
vftRprofReport <- function(file = .vftP()$rprof, top = 25){
  if(is.null(file) || !file.exists(file)){
    message("no Rprof output; run with VFT_RPROF=1 and call vftRprofStop() first")
    return(invisible(NULL))
  }
  s <- summaryRprof(file)
  cat("total sampled time (s):", s$sampling.time, "\n")
  list(self  = utils::head(s$by.self[order(-s$by.self$self.time), ], top),
       total = utils::head(s$by.total[order(-s$by.total$total.time), ], top))
}


#' Read the log back as a ranked table - what to fix, worst first.
#'
#' `user_seconds` is the ranking column rather than raw duration: it weights each
#' stall by how many sessions were actually frozen by it, which is the quantity
#' this whole effort is trying to reduce. A slow block nobody contends with is
#' not the problem to solve first.
vftPerfSummary <- function(file = .vftP()$file){
  if(is.null(file) || !file.exists(file)) return(NULL)
  d <- utils::read.csv(file, stringsAsFactors = FALSE)
  d <- d[d$kind %in% c("stall", "block"), ]
  if(!nrow(d)) return(d)

  #sessions can be 0 for work that runs between connections; such a stall froze
  #nobody, but it still took real time, so floor the weight at 1 rather than 0
  d$user_seconds <- (d$ms / 1000) * pmax(1L, d$sessions)

  agg <- stats::aggregate(cbind(ms, user_seconds) ~ kind + label, data = d,
                          FUN = function(x) c(n = length(x), total = sum(x),
                                              max = max(x)))
  out <- data.frame(kind  = agg$kind,
                    label = agg$label,
                    n     = agg$ms[, "n"],
                    total_s = round(agg$ms[, "total"] / 1000, 1),
                    max_s   = round(agg$ms[, "max"] / 1000, 1),
                    user_seconds = round(agg$user_seconds[, "total"], 1),
                    stringsAsFactors = FALSE)
  out[order(-out$user_seconds), ]
}
