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

    #latches TRUE at the first session and never resets: it separates "the app
    #was booting" from "a user was waiting". Not the same as sessions > 0, which
    #drops back to 0 between connections - work done in a gap is still a stall,
    #it just froze nobody, whereas work done before the first connection ever is
    #startup. See .vftPerfTick().
    st$everSession <- FALSE

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
    #cumulative process GC seconds at the last tick, so each stall can report
    #how much of itself was garbage collection
    st$lastGc     <- sum(gc.time()[1:2])
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
vftPerfLog <- function(kind, label, ms, gcMs = NA_real_){
  p <- .vftP()
  if(!p$enabled) return(invisible(NULL))
  line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), ",",
                 kind, ",",
                 #a label with a comma would shift every later column
                 gsub(",", ";", label, fixed = TRUE), ",",
                 round(ms), ",",
                 p$sessions, ",",
                 #how much of this stall the garbage collector accounts for.
                 #GC runs at arbitrary allocation points, so its cost lands
                 #wherever the app happened to be - which is exactly what the
                 #"unattributed" bucket is made of. gc.time() is free to read
                 #and never triggers a collection, unlike gc() itself.
                 ifelse(is.na(gcMs), "", round(gcMs)), "\n")
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

    #Startup is logged as its own kind, not as a stall. The heartbeat starts near
    #the top of global.R, so everything after it - loading packages, the ~7.4s
    #protected-areas warm, tmap_mode, warming the daemons - is genuinely a blocked
    #main thread, and genuinely blocks nobody: there is no session yet. Counting
    #it made "49% of wall clock blocked" on a short single-user run, where over a
    #third of the freeze was the app booting. That number is the one being used to
    #judge whether any of this work helped, so it has to measure only time a user
    #could have been waiting.
    #
    #Kept in the log rather than dropped: a startup that grows is worth seeing, it
    #just is not concurrency cost. vftReport() prints it on its own line.
    #`sessions > 0` is checked as well as the latch, not instead of it. The latch
    #is what makes a freeze in a gap between connections still count as a stall;
    #the session count is what stops any path that registers a session without
    #going through vftPerfSessionStart() from being misfiled as startup. Only a
    #process that has never had a session, and has none now, is booting.
    #GC is charged to whichever line happened to allocate, so a stall that is
    #mostly GC is not really "caused" by the code it gets blamed on. Reading
    #gc.time() costs nothing and never triggers a collection.
    gcNow    <- sum(gc.time()[1:2])
    gcMs     <- (gcNow - p$lastGc) * 1000
    p$lastGc <- gcNow
    vftPerfLog(if(isTRUE(p$everSession) || p$sessions > 0L) "stall" else "startup",
               label, late * 1000, gcMs)
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
    try(cat("time,kind,label,ms,sessions,gc_ms\n", file = p$file), silent = TRUE)
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

#' Time a whole render, including the widget serialisation vftTime() cannot see.
#'
#' `vftTime()` wraps an expression, so putting it inside renderLeaflet({...})
#' measures only the code that builds the map object. For htmlwidgets that is
#' often the smaller half: turning the widget into JSON for the browser happens
#' afterwards, inside the function renderLeaflet() *returns*, when Shiny calls
#' it. That cost shows up in a profile as NA#htmlwidgets / NA#shinyRenderWidget
#' with no line attribution, and is invisible to a label placed inside the block.
#'
#' Worse, where a map is built eagerly and the render body is just `map` - as at
#' step5_server.R#686 - a label inside the block would measure nothing at all
#' while the real cost sits just outside it.
#'
#' This wraps the render function itself, so the timing spans everything Shiny
#' does to produce the output. Attributes are copied because Shiny inspects them:
#' render functions carry a class and metadata (output type, async support) and a
#' bare closure would lose them and change how the output is treated.
#'
#' Usage: output$map <- vftTimeRender("step5:map", leaflet::renderLeaflet({...}))
vftTimeRender <- function(label, renderFunc){
  force(label)
  force(renderFunc)
  p <- .vftP()
  if(!p$enabled) return(renderFunc)

  wrapped <- function(...) vftTime(label, renderFunc(...))
  #preserve class/metadata so Shiny still recognises this as a render function
  attributes(wrapped) <- attributes(renderFunc)
  wrapped
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
  #from here on a freeze has somebody to freeze, so ticks count as stalls rather
  #than startup - see .vftPerfTick()
  p$everSession <- TRUE
  p$sessions <- p$sessions + 1L
  vftPerfLog("session", "open", 0)

  session$onSessionEnded(function(){
    p$sessions <- max(0L, p$sessions - 1L)
    vftPerfLog("session", "close", 0)
  })
  invisible(NULL)
}

# ---------------------------------------------------- module instances ---

#' Count how many times a module server has been instantiated in one session.
#'
#' A Shiny module server is meant to be called once per session. This app calls
#' each step's server *inside* an observeEvent on a trigger reactive, and the
#' triggers are deliberately re-fired - `triggerStep4(triggerStep4() + 1)` is the
#' navigation idiom - so moving through the app re-runs stepN_server() and builds
#' a complete fresh set of observers and output bindings each time. Nothing
#' destroys the previous set: dropping the reference to an observer does not stop
#' it, so every old copy keeps firing on every matching event forever.
#'
#' If that is happening, it is the dominant cost in this app and it explains the
#' profile: work spread thinly across <Anonymous>/func/flushReact with no single
#' function to blame, output bookkeeping that grows, and a session that gets
#' slower the longer it runs. This counter turns that from a theory into a number
#' in the log - any count above 1 is a duplicate that should not exist.
#'
#' Emitted as its own row kind so it can be read alongside the stalls: the
#' interesting question is whether stall size tracks instance count.
#' The count MUST be per session, not per process.
#'
#' Module instantiation is a per-session event: five browser sessions each
#' legitimately build their own step1. A process-wide counter therefore reports
#' "step1 instantiated 5 times" for a perfectly healthy app, which is precisely
#' the false positive the first version of this function produced - three
#' sessions, three instantiations, flagged as duplicates when nothing was wrong.
#'
#' Keeping the tally in session$userData makes the number mean what the label
#' claims: how many times *this one session* built the module. Any value above 1
#' is then a genuine duplicate. getDefaultReactiveDomain() gives the module's
#' session proxy, whose userData is shared with the root session, so all of a
#' session's modules count against the same record.
vftModuleInstance <- function(name){
  p <- .vftP()
  if(!p$enabled) return(invisible(0L))

  sess <- shiny::getDefaultReactiveDomain()
  #outside a session (tests, direct calls) fall back to the process record
  store <- if(!is.null(sess)) sess$userData else p
  if(is.null(store$.vftModules)) store$.vftModules <- list()

  n <- (store$.vftModules[[name]] %||% 0L) + 1L
  store$.vftModules[[name]] <- n
  vftPerfLog("module", name, n)
  invisible(n)
}

`%||%` <- function(a, b) if(is.null(a)) b else a


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
#' Returns three views, because on their own each one misleads:
#'
#'   $lines  time attributed to a specific `file.R#123`. THE useful one - it
#'           names the exact line of the app's own code that held the thread.
#'           Empty unless the package was installed with source refs kept; see
#'           below, because without them everything collapses into <Anonymous>.
#'   $self   time spent *in* a function, excluding what it called. Almost always
#'           a primitive - `%*%`, terra's C entry points, sf's GEOS calls - and
#'           tells you the kind of work, not the place to fix.
#'   $total  time spent in a function *including* everything beneath it. Useful
#'           when $lines is unavailable, but this app's observers and renders are
#'           anonymous closures, so they show up as <Anonymous>/func/FUN rather
#'           than by name - which is why the first two profiles could not say
#'           what was blocking.
#'
#' To get $lines populated, the package must be installed with:
#'
#'   devtools::install(keep_source = TRUE)
#'
#' NOT via Sys.setenv(R_KEEP_PKG_SOURCE = "yes"), which looks like it should work
#' and does nothing: devtools::install() computes keep_source from its own
#' argument (defaulting to getOption("keep.source.pkgs")) and builds in a callr
#' subprocess that does not inherit the variable anyway. Verified by testing both.
#'
#' R drops source references from installed packages by default
#' (keep.source.pkgs = FALSE), and Rprof cannot report a line it has no record
#' of. This is the single setting that turns "27 unattributed stalls" into a
#' ranked list of file:line.
#' What is actually on the stack while a dispatch is frozen
#'
#' Four rounds of custom probes each ruled one thing out and named no cause, and
#' spilling the payload to disk left async:send untouched at 6s - so the cost is
#' neither the bytes nor anything a timer wrapped around the call can see. This
#' stops hypothesising and reads the sampled call stacks directly.
#'
#' summaryRprof() aggregates away exactly the detail that matters here, so this
#' parses the raw .out instead: every sample is a stack, innermost frame first.
#' Of the samples taken while vftFuture was on the stack, it reports which frame
#' was actually executing.
#'
#' Read it like this: if the innermost frame is mirai itself, the time is inside
#' the transport's compiled code and no R-level change will move it. If it is
#' something else, that name is the answer four rounds of timers could not reach.
#' @param file raw Rprof output; defaults to this run's file
#' @param frame stack frame marking a dispatch
#' @keywords internal
vftSendStacks <- function(file = .vftP()$rprof, frame = "vftFuture"){
  if(is.null(file) || !nzchar(file) || !file.exists(file)){
    message("no Rprof output; start the app with VFT_RPROF=1, then vftRprofStop()")
    return(invisible(NULL))
  }
  L <- readLines(file, warn = FALSE)
  int <- 0.02
  h <- grep("sample.interval", L, fixed = TRUE)
  if(length(h)){
    m <- regmatches(L[h[1]], regexpr("[0-9]+", L[h[1]]))
    if(length(m)) int <- as.numeric(m)/1e6
  }
  keep <- !startsWith(L, "#File") & !grepl("sample.interval", L, fixed = TRUE) &
          !grepl("line profiling", L, fixed = TRUE) & nzchar(L)
  L <- L[keep]
  if(!length(L)){ message("Rprof file has no samples"); return(invisible(NULL)) }

  #each sample is a stack of quoted frame names, innermost first
  qpat   <- '"[^"]+"'
  stacks <- lapply(L, function(x)
    gsub('"', '', regmatches(x, gregexpr(qpat, x))[[1]], fixed = TRUE))
  #the innermost line ref, when the profile was taken with line.profiling
  lref <- vapply(L, function(x){
    m <- regmatches(x, regexpr("[0-9]+#[0-9]+", x)); if(length(m)) m else ""
  }, "", USE.NAMES = FALSE)

  hit <- vapply(stacks, function(s) frame %in% s, logical(1))
  message("log: ", file)
  message("samples: ", length(stacks), " total, ", sum(hit), " with ", frame,
          " on the stack (", round(sum(hit)*int, 1), " s of ",
          round(length(stacks)*int, 1), " s profiled)")
  if(!sum(hit)){
    message("none - either no dispatch ran while profiling was on, or ", frame,
            " never appears in the stack")
    return(invisible(NULL))
  }

  tab <- function(v, n){
    t <- sort(table(v), decreasing = TRUE)
    k <- seq_len(min(n, length(t)))
    data.frame(name = names(t)[k], samples = as.integer(t)[k],
               seconds = round(as.integer(t)[k]*int, 3),
               pct = round(100*as.integer(t)[k]/sum(hit)), stringsAsFactors = FALSE)
  }

  d <- tab(vapply(stacks[hit], function(s) s[1], ""), 20)
  names(d)[1] <- "innermost_frame"
  message("
== WHAT WAS EXECUTING DURING A DISPATCH ==")
  print(d, row.names = FALSE)

  p <- tab(vapply(stacks[hit], function(s)
             paste(rev(utils::head(s, 3)), collapse = " -> "), ""), 10)
  names(p)[1] <- "path"
  message("
== CALL PATHS (outer -> innermost, 3 frames) ==")
  print(p, row.names = FALSE)

  ln <- lref[hit]; ln <- ln[nzchar(ln)]
  if(length(ln)){
    q <- tab(ln, 10); names(q)[1] <- "file#line"
    message("
== INNERMOST SOURCE LINES ==")
    print(q, row.names = FALSE)
  }
  invisible(list(inner = d, paths = p))
}

vftRprofReport <- function(file = .vftP()$rprof, top = 25){
  if(is.null(file) || !file.exists(file)){
    message("no Rprof output; run with VFT_RPROF=1 and call vftRprofStop() first")
    return(invisible(NULL))
  }
  s <- summaryRprof(file)
  cat("total sampled time (s):", s$sampling.time, "\n")

  byLine <- tryCatch(summaryRprof(file, lines = "show")$by.line,
                     error = function(e) NULL)
  #samples taken in code with no source reference are bucketed under
  #"<no location>". A table containing only that is not line data, it is the
  #symptom of a package installed without keep.source - report it as such rather
  #than handing back a one-row table that says nothing.
  #How much time has NO line attribution at all. This used to be dropped
  #silently, which made $lines look like a complete account of the run when it
  #was not: a profile whose top lines sum to ~10s, next to a stall log showing
  #85s of blocking, sends you hunting through the 10s. Report the gap instead -
  #a large <no location> figure means the blocking is in compiled or
  #srcref-less code (sf/terra/leaflet internals, or the package installed
  #without keep_source) and that $self / $total are the tables to read, not
  #$lines.
  noLoc <- 0
  if(!is.null(byLine) && nrow(byLine)){
    hit <- rownames(byLine) == "<no location>"
    if(any(hit)) noLoc <- sum(byLine$self.time[hit])
    byLine <- byLine[!hit, , drop = FALSE]
  }
  accounted <- if(!is.null(byLine) && nrow(byLine)) sum(byLine$self.time) else 0
  cat(sprintf("with line attribution: %.1f s | <no location>: %.1f s (%.0f%% of sampled)\n",
              accounted, noLoc,
              if(s$sampling.time > 0) 100 * noLoc / s$sampling.time else 0))
  if(noLoc > accounted){
    cat("  -> most of the time has no line info: read $self and $total, not $lines\n")
  }

  if(is.null(byLine) || !nrow(byLine)){
    cat("\nNO LINE DATA. Reinstall with source refs to get file:line attribution:\n")
    cat("  devtools::install(keep_source = TRUE)\n")
    byLine <- NULL
  }else{
    byLine <- utils::head(byLine[order(-byLine$self.time), , drop = FALSE], top)
  }

  list(lines = byLine,
       self  = utils::head(s$by.self[order(-s$by.self$self.time), ], top),
       total = utils::head(s$by.total[order(-s$by.total$total.time), ], top))
}


#' Everything the log has to say about a run, in one call.
#'
#' Finds the newest log in VFT_PERF_DIR by default, so there is no filename to
#' look up. Prints the module instance counts first because that is currently the
#' open question: a module server should be created once per session, and any
#' count above 1 means a duplicate set of observers and outputs is live.
vftReport <- function(file = NULL, dir = Sys.getenv("VFT_PERF_DIR", "")){
  if(is.null(file)){
    if(!nzchar(dir)) dir <- file.path(tempdir(), "vft_perf")
    fs <- list.files(dir, pattern = "^vft_perf_.*[.]csv$", full.names = TRUE)
    if(!length(fs)){
      message("no vft_perf_*.csv found in ", dir,
              " - set VFT_PERF_DIR before launching the app")
      return(invisible(NULL))
    }
    file <- fs[which.max(file.mtime(fs))]
  }
  d <- utils::read.csv(file, stringsAsFactors = FALSE)
  cat("log:", file, "\n")
  cat("rows:", nrow(d), "| max concurrent sessions:", max(d$sessions), "\n\n")

  cat("== MODULE INSTANCES (per session; >1 means duplicates are live) ==\n")
  m <- d[d$kind == "module", ]
  if(!nrow(m)){
    cat("  none recorded - the app never reached a step module,\n")
    cat("  or the installed package predates the counter (reinstall).\n\n")
  }else{
    #the count is already per session, so the max across rows is the worst any
    #single session reached. Total rows are shown too: with N sessions, N rows
    #all reading 1 is healthy, and is NOT the same thing as one session reaching N.
    peak <- tapply(m$ms, m$label, max)
    rows <- table(m$label)
    for(nm in names(sort(peak, decreasing = TRUE))){
      cat(sprintf("  %-12s worst session built it %2d time(s)   (%d instantiation(s) across all sessions)%s\n",
                  nm, peak[[nm]], rows[[nm]],
                  if(peak[[nm]] > 1) "   <-- DUPLICATE" else ""))
    }
    cat("\n")
  }

  #stall rows only. A labelled block that freezes the thread emits BOTH a `block`
  #row (how long the function ran) and a `stall` row (how long the loop lost),
  #so summing both double counts the same freeze. The stall rows are the measured
  #blocking; block rows exist to attribute it, and appear in the ranking below.
  #startup is reported, but never counted as blocking - nobody was connected
  boot <- d[d$kind == "startup", ]
  if(nrow(boot)){
    cat("== STARTUP (before any session; blocks nobody) ==\n")
    cat(sprintf("  %.1f s across %d tick(s), longest %.1f s\n\n",
                sum(boot$ms)/1000, nrow(boot), max(boot$ms)/1000))
  }

  s <- d[d$kind == "stall", ]
  if(nrow(s)){
    #Wall clock runs from the FIRST SESSION OPENING, not from process start.
    #Measuring from row 1 charges the app for however long it sat idle booting,
    #which on a short run swamps the percentage being read.
    opens <- d[d$kind == "session" & d$label == "open", ]
    t0 <- as.POSIXct(if(nrow(opens)) opens$time[1] else d$time[1])
    t1 <- as.POSIXct(d$time[nrow(d)])
    wall <- max(as.numeric(difftime(t1, t0, units = "secs")), 1e-9)
    cat("== MAIN-THREAD BLOCKING (from first session onward) ==\n")
    cat(sprintf("  wall clock       : %.0f s\n", wall))
    cat(sprintf("  blocked          : %.1f s (%.0f%% of wall clock)\n",
                sum(s$ms)/1000, 100*sum(s$ms)/1000/wall))
    cat(sprintf("  longest stall    : %.1f s\n", max(s$ms)/1000))
    cat(sprintf("  user-seconds lost: %.0f\n\n",
                sum(s$ms/1000 * pmax(1L, s$sessions))))
    #GC is reported separately because it is not a bug in any one function. It
    #runs at arbitrary allocation points, so its cost is charged to whatever
    #line happened to allocate - which is precisely how a large "unattributed"
    #bucket forms. If most of the freeze is GC, labelling more code will not
    #shrink it and the fix is to stop retaining memory, not to move work.
    if("gc_ms" %in% names(d)){
      g <- suppressWarnings(as.numeric(d$gc_ms))
      st <- d$kind == "stall"
      gcTot <- sum(g[st], na.rm = TRUE)/1000
      if(gcTot > 0){
        blocked <- sum(d$ms[st], na.rm = TRUE)/1000
        cat("== GARBAGE COLLECTION ==
")
        cat(sprintf("  GC during stalls : %.1f s (%.0f%% of all blocking)
",
                    gcTot, 100 * gcTot/max(blocked, 1e-9)))
        un <- st & d$label == "unattributed"
        if(any(un)){
          cat(sprintf("  of unattributed  : %.1f s of %.1f s (%.0f%%)
",
                      sum(g[un], na.rm = TRUE)/1000, sum(d$ms[un], na.rm = TRUE)/1000,
                      100 * sum(g[un], na.rm = TRUE)/max(sum(d$ms[un], na.rm = TRUE), 1e-9)))
        }
        worst <- which.max(ifelse(st, g, NA))
        if(length(worst) == 1 && is.finite(g[worst]))
          cat(sprintf("  worst single tick: %.1f s of GC inside a %.1f s stall (%s)
",
                      g[worst]/1000, d$ms[worst]/1000, d$label[worst]))
        cat("
")
      }
    }

    cat("== WORST OFFENDERS ==\n")
    print(utils::head(vftPerfSummary(file), 10))
  }
  invisible(d)
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
