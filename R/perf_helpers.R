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

#' Process-wide, deliberately: the thing being measured is process-wide.
#'
#' A per-session store would be the wrong shape - a stall caused by one user's
#' render is experienced by every other session, and the whole point is to count
#' it once against the code that caused it, not once per victim.
.vftPerf <- new.env(parent = emptyenv())

.vftPerf$enabled   <- FALSE
.vftPerf$file      <- NULL
.vftPerf$started   <- FALSE
.vftPerf$sessions  <- 0L

#' Labels of vftTime() blocks currently on the stack, innermost last.
.vftPerf$stack     <- character(0)

#' The most recently *finished* labelled block, and when it ended.
#'
#' Needed because of an ordering problem inherent to the heartbeat: a later()
#' callback cannot run while the thread is blocked, so by the time the stall is
#' observed the block that caused it has already returned and popped itself off
#' the stack. Remembering the last completion lets the tick look backwards and
#' attribute the stall, provided that completion falls inside the stall window.
.vftPerf$lastLabel <- NA_character_
.vftPerf$lastEnd   <- 0

#' Heartbeat period and the delay above it that counts as a stall, in seconds.
#'
#' 0.25s ticks are frequent enough to bound the attribution window tightly
#' without being a load in themselves. The 0.5s threshold is above ordinary
#' scheduling jitter but well below what a user perceives as the app hanging.
.vftPerf$interval  <- 0.25
.vftPerf$threshold <- 0.5


# ----------------------------------------------------------------- log ---

#' One CSV line per event. Opened in append mode each time on purpose.
#'
#' Holding a connection open across the life of the process would risk losing a
#' buffered tail if the process is killed - which, when investigating a hang, is
#' exactly how the process tends to end. The write is a few dozen bytes and only
#' happens on a threshold breach, so the cost of reopening is irrelevant next to
#' the stalls being recorded.
vftPerfLog <- function(kind, label, ms){
  if(!.vftPerf$enabled) return(invisible(NULL))
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ",",
                 kind, ",",
                 #a label with a comma would shift every later column
                 gsub(",", ";", label, fixed = TRUE), ",",
                 round(ms), ",",
                 .vftPerf$sessions, "\n")
  #instrumentation must never be able to take the app down with it: an
  #unwritable log directory is a reason to lose measurements, not sessions
  try(cat(line, file = .vftPerf$file, append = TRUE), silent = TRUE)
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
.vftPerfTick <- function(expected){
  now  <- as.numeric(Sys.time())
  late <- now - expected

  if(late > .vftPerf$threshold){
    #attribute only if a labelled block finished during the window we lost; a
    #completion older than that belongs to some earlier, already-reported stall
    label <- if(.vftPerf$lastEnd > (now - late - .vftPerf$interval)){
      .vftPerf$lastLabel
    }else{
      "unattributed"
    }
    vftPerfLog("stall", label, late * 1000)
  }

  nxt <- as.numeric(Sys.time()) + .vftPerf$interval
  later::later(function() .vftPerfTick(nxt), .vftPerf$interval)
  invisible(NULL)
}


# ---------------------------------------------------------------- api ---

#' Start collecting. Call once per process, from global.R.
#'
#' `dir` defaults to VFT_PERF_DIR, then to a stable directory under tempdir().
#' The resolved path is messaged so it lands in the Shiny Server log - on a
#' server you administer only through the app directory, that message is
#' realistically how you will find the file.
vftPerfInit <- function(dir = Sys.getenv("VFT_PERF_DIR", ""),
                        interval = 0.25, threshold = 0.5){

  if(identical(Sys.getenv("VFT_PERF", "1"), "0")){
    message("vftPerf: disabled (VFT_PERF=0)")
    return(invisible(FALSE))
  }
  #global.R runs once per process, but a dev load_all() cycle can re-source it;
  #a second heartbeat would double every tick and corrupt the attribution
  if(.vftPerf$started) return(invisible(TRUE))

  if(!nzchar(dir)) dir <- file.path(tempdir(), "vft_perf")
  ok <- dir.create(dir, showWarnings = FALSE, recursive = TRUE) || dir.exists(dir)
  if(!ok){
    message("vftPerf: cannot create ", dir, " - profiling off")
    return(invisible(FALSE))
  }

  #the pid keeps a restarted process from appending to its predecessor's log,
  #where the two would be indistinguishable but the session counts would not line up
  .vftPerf$file      <- file.path(dir, paste0("vft_perf_", Sys.getpid(), "_",
                                              format(Sys.Date(), "%Y%m%d"), ".csv"))
  .vftPerf$interval  <- interval
  .vftPerf$threshold <- threshold
  .vftPerf$enabled   <- TRUE

  if(!file.exists(.vftPerf$file)){
    try(cat("time,kind,label,ms,sessions\n", file = .vftPerf$file), silent = TRUE)
  }

  .vftPerf$started <- TRUE
  .vftPerfTick(as.numeric(Sys.time()) + interval)

  message("vftPerf: logging main-thread stalls > ", threshold * 1000, "ms to ",
          .vftPerf$file)
  invisible(TRUE)
}

#' Run `expr`, timing it and naming it while it runs.
#'
#' Wrap the body of any render or observer under suspicion:
#'
#'   output$map <- leaflet::renderLeaflet(vftTime("step5:map", { ... }))
#'
#' Returns exactly what `expr` returns and never alters its behaviour, so it is
#' safe to leave in place permanently on the paths that turn out to matter.
#' Nesting is fine - the stack keeps the innermost label, which is the specific
#' one, and each level reports its own duration.
vftTime <- function(label, expr){
  if(!.vftPerf$enabled) return(expr)

  start <- as.numeric(Sys.time())
  .vftPerf$stack <- c(.vftPerf$stack, label)

  #on.exit so an error inside expr still pops the stack and records the time;
  #without it one failed render would mis-label every stall thereafter
  on.exit({
    end <- as.numeric(Sys.time())
    n   <- length(.vftPerf$stack)
    if(n > 0) .vftPerf$stack <- .vftPerf$stack[-n]
    .vftPerf$lastLabel <- label
    .vftPerf$lastEnd   <- end
    if((end - start) > .vftPerf$threshold){
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
  if(!.vftPerf$enabled) return(invisible(NULL))
  .vftPerf$sessions <- .vftPerf$sessions + 1L
  vftPerfLog("session", "open", 0)

  session$onSessionEnded(function(){
    .vftPerf$sessions <- max(0L, .vftPerf$sessions - 1L)
    vftPerfLog("session", "close", 0)
  })
  invisible(NULL)
}

#' Read the log back as a ranked table - what to fix, worst first.
#'
#' `user_seconds` is the ranking column rather than raw duration: it weights each
#' stall by how many sessions were actually frozen by it, which is the quantity
#' this whole effort is trying to reduce. A slow block nobody contends with is
#' not the problem to solve first.
vftPerfSummary <- function(file = .vftPerf$file){
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
