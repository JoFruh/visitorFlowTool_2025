# visitorFlowTool: modular steps, lazy data, and per-flush overhead

Repo: `c:\Users\frueh\VScode_GitClones\visitorFlowTool_2025` (R package; server in `R/`,
UI in `R/app_ui.R` + `R/step*_ui.R`, Shiny entry in `inst/app/`). Branch `visitorFlowTool_2026`.

---

## Context

This is a Shiny app run as **a single R process serving every user**, so anything on the main
thread freezes all connected sessions for its full duration. That is the whole performance
story, and a long optimisation campaign has already removed the large spatial costs (the path
network now draws through WebGL; the protected-areas layer is pre-simplified at startup; the
autosave fires at 3 checkpoints instead of 8).

A line-level profile taken 2026-08-25 (`VFT_RPROF=1`, package installed with
`keep_source = TRUE`, read back with `visitorFlowTool:::vftRprofReport()`) shows what is left.
Total sampled 164.06 s, of which:

| Frame | total | pct | What it is |
|---|---|---|---|
| `self$manageHiddenOutputs` | 18.42 s | 11.23% | Shiny sweeping its registered-output list |
| `private$shouldSuspend` | 14.58 s | 8.89% | the per-output test inside that sweep |
| `map$get` / `map$as_list` | 9.58 s | 5.8% | the same sweep's internal lookups |
| `[.data.frame` | 28.80 s | 17.55% | app-level data frame subsetting |
| `CPL_geos_op` | 15.10 s | 9.20% | **startup only** — the pre-warmed PA simplify, blocks nobody |
| `request` (mirai transport) | 9.64 s | 5.88% | compiled; not reachable from R, closed |

So the app is now bound by **Shiny's own flush loop**, not by spatial work. That is Goal B.

Separately, the app's structure forces all data preparation to the front. Step 1 builds the
entire path network — including per-node population and seven attractiveness bands that are
not read until steps 4 and 5 — before the user has done anything but draw a perimeter. There
is no way to skip ahead, no way to go back, and no gating: a user can reach a step whose
inputs do not exist. That is Goal A.

**Intended outcome:** each step prepares only what it needs, when it needs it; users can move
freely between steps; steps whose prerequisites are unmet are visibly disabled; and Shiny's
per-flush overhead drops.

---

## Decisions taken

| Question | Decision |
|---|---|
| Backward navigation invalidating later results | **Auto-invalidate, with a cancellable warning modal.** Never display a result computed from mixed inputs. |
| Navigation UI | **New static nav bar.** Delete `imageMap()` and the 6 banner `renderUI` outputs. |
| Module lifecycle | **Singleton per session, converted one module at a time**, smallest first. |
| First shippable increment | **Stage 0 + Stage 1** (dead code, re-entry bugs, flush overhead). |

---

## Verified facts this plan depends on

Established by reading the code; re-verify before relying on any of them.

**Output/flush mechanics.** `manageHiddenOutputs()` is called from `ShinySession$manageInputs()`
on **every input message batch**, with no argument, so it sweeps **all** registered outputs.
There are 118 `output$x <-` assignment sites but only **31 distinct output names** —
`defineOutput()` destroys the prior observer when a name is re-assigned, so outputs do not leak.
**Per-sweep cost is bounded at 31; the 11% is driven at least as much by the number of sweeps
(message rate) as by output count.** Both levers matter. There is no supported hook or throttle
on `manageHiddenOutputs`; do not patch it — the app depends on `suspendWhenHidden` because all
7 module UIs sit in the DOM simultaneously.

**Hover messages force sweeps.** `leaflet.js`'s `mouseHandler` binds `mouseover` and `mouseout`
on every marker and shape, and stamps each event with `".nonce": Math.random()` so Shiny cannot
dedupe it. Every hover is one guaranteed input message and therefore one guaranteed full sweep.
**Nothing in this repo reads `_mouseover` or `_mouseout`** (verified: zero hits across `R/` and
`inst/`). `_click`, `_bounds`, `_center`, `_zoom` *are* read and must be left alone.

**Module lifecycle.** All 7 module servers are called from `R/app_server.R` inside
`observeEvent(triggerStepN(), ...)`. Navigation is "bump the reactiveVal counter", so re-entering
a step **re-calls its server and builds a fresh observer set**. Only hand-maintained lists of
named observers are destroyed, and they are inconsistent — `step5_server.R:2212-2222` destroys 11,
`step5_server.R:1036-1042` (the banner exit) destroys 2, so that path leaks 9 live observers per
visit. `vftModuleInstance()` (`R/perf_helpers.R:370`) exists to count this and logs a per-session
tally.

**UI.** One `shiny::tabsetPanel(id = "tabs", type = "hidden")` at `R/app_ui.R:14-46` holds all 7
module UIs, all present from session start. Navigation is `updateTabsetPanel(inputId = "tabs", ...)`,
only from `app_server.R`.

**Backward navigation is fully written and 100% dead.** Every step maps `input$banner` to
confirm-codes `"A"`..`"E"`, handled in `app_server.R` (~250 lines at :455, :546, :566, :687, :704,
:720, :832, :849, :865, :881, :1021-1087). It can never fire: `imageMap()` in
`inst/app/global.R:261-277` early-returns a bare `<img>` before building the `<map><area>`
elements, and every live call site passes `opts = list()`.

**No gating anywhere.** Zero `conditionalPanel` in the repo. `r$step` is recorded but never read
to gate. Only 2 `req()` calls, both about file uploads.

**Skip-ahead already exists, once.** The restore-from-`.RData` path (`app_server.R:272-371`)
rehydrates ~30 `envBase_*` into `r$` then jumps straight into a step via `triggerStepN(1)`
(:360-370). This proves modules can be cold-started from `r$` alone. Two gaps: the `r$step == 1`
branch calls `triggerStep1(1)` which **nothing observes**, and there is **no `r$step == 6` branch**,
so a save taken after the simulation restores to a blank screen.

**Everything crosses through `r$`** (`shiny::reactiveValues()`, `app_server.R:14`). Modules receive
**isolated plain values**, not reactives — which is what makes both the lazy and the singleton
refactors feasible.

### Data dependency gaps — the lazy-loading target

`sf_to_tidygraph3()` (`R/sf_to_tidygraph3.R:8`), called from step 1, does five unrelated jobs:

| Lines | Job | First genuinely consumed |
|---|---|---|
| :15-57, :350 | build the tbl_graph | **step 4** (`step4_server.R:133`, :147) |
| :230-239 | crop the 7-band `DULN` raster | **step 4** (`step4_server.R:1029-1056`) |
| :242-245 | crop `DULN_all` | step 3 (`step3_server.R:151`) |
| :249-257, :329-336 | `terra::extract` 7 attractiveness bands onto every node | **step 4** |
| :261-281 | crop residential raster, distribute population per node (`Residents`) | **step 5** (`generatePopulation.R:29,39`) |

Step 2 needs **only the perimeter** (`fshape`, used at `step2_server.R:73-79`); it never touches
`r$network`, `r$DULN` or `r$DULN_all`. Step 3 receives `network` and derives `pathMap`
(`step3_server.R:120-121`) which is **never used**.

### Dead plumbing to delete

`basemap`/`basemap_bw` (never assigned — `step1_server.R:128-129` — yet harvested, saved, and
forwarded to step5/lastStep); `naturalAreas` (always NULL); `networkPts` (`R/generateAoI2.R:24`,
assigned never used); `SM_noPres` (`step5_server.R:4`, never passed); `r$SMColors` and `r$spChc`
(`app_server.R:999-1000`, misspelled → silently NULL); `R/newVersions_server_bckp.R` +
`R/newVersions_ui_bckp.R` (~3.4k lines, no `Collate:` in DESCRIPTION so they are parsed at build).

### Re-entry hazards to fix

- **`once = TRUE` without `ignoreInit = TRUE`** at `app_server.R:372, 597, 616, 739, 1103`
  (the observers opened at :508 step3, :648 step4, :1006 finalStep). These fire immediately on
  creation with a stale button value, auto-advancing the app on re-entry. Lines 473, 804, 898,
  981 correctly pass `ignoreInit = TRUE`.
- **Out-of-scope reads.** `step1return` is frame-local to the `restartSteps()` handler, yet
  `app_server.R:386` reads `step1return$confirm()`. Survives only because `confirm` is a dead
  unforced parameter in every module. Same class: `app_server.R:1021-1102` tests
  `step5return$confirm()` where it means `finalStepReturn$confirm()`, and `:1099` bumps
  `triggerStep4(triggerStep5() + 1)`.

---

## Stage 0 — Dead code and latent bugs (no behaviour change)

Do this first so later stages diff against a clean baseline.

- Delete `R/newVersions_server_bckp.R`, `R/newVersions_ui_bckp.R`, `R/generateAoI2_backp.Rbckp`.
- Remove `basemap`/`basemap_bw`, `naturalAreas`, `networkPts`, `SM_noPres`, `r$SMColors`,
  `r$spChc`, and step 3's `network` parameter with its dead `pathMap`.
  **Keep reading `envBase_basemap` on the restore path** for old save files, then discard it.
- Delete the `if(step == 2/3/4/5/62)` dev shortcut blocks (`app_server.R:1110-1245`) — they
  `load()` files not in the repo behind a hardcoded `step <- 1`.
- Add `ignoreInit = TRUE` to the five bare `once = TRUE` observers.
- Fix the `finalStepReturn` / `triggerStep4(triggerStep5()+1)` bugs at `app_server.R:1021-1102`.

**Verify:** app boots; step 1 → 5 → finalStep completes; a `.RData` is written at each of the
three checkpoints; an **old** save file still restores; `vftReport()` shows no new stall labels.

---

## Stage 1 — Goal B: flush overhead (ships independently)

| # | Change | Effect | Risk |
|---|---|---|---|
| 1 | **Delete the 6 banner `renderUI`s.** Replace each `uiOutput(NS(id,"bannerUI_N"))` in the 6 `*_ui.R` files with `tags$img(id = NS(id,"banner_img"), height = 70, src = ...)`; swap `src` from the existing `languageSelect_N` observers via `shinyjs::runjs`. Deletes `imageMap()` entirely. | −6 of 31 outputs (**~19% off every sweep**), −34 assignment sites | **None.** `imageMap()` already returns exactly this `<img>`. |
| 2 | **Delete outputs with no DOM node.** `step1-errorText` (its `textOutput` is commented out at `step1_ui.R:134` — convert the two sites at `step1_server.R:1237,1336` to `showNotification(type="error")` so the message is still seen); `newVersions-inputList` (`newVersions_server.R:3652`, a `renderPrint(reactiveValuesToList(input))` debug dump that depends on *every* input); the `renderUI({NULL})` at `step5_server.R:44` on a `renderLeaflet`-bound id. | −2 outputs (**~7%**), removes a type mismatch | None. Fix `lastStep_server.R:239`/`:296` (`renderPlot` then `renderUI` on `mapArea`) the same way. |
| 3 | **Drop `_mouseover`/`_mouseout` at the client.** New `inst/app/www/vft-shim.js` included once from `app_ui()` (resource path already registered — `R/zzz.R:38`; include idiom at `newVersions_ui.R:381`). ~10 lines wrapping `Shiny.setInputValue` to return early for exactly those two suffixes. | Attacks **sweep count** — the only lever with an unbounded ceiling | Low-medium. Global monkey-patch; keep the suffix list to exactly two and comment why at the shim. |
| 4 | **Collapse `app_server.R:19-23`** — five `shinyjs::runjs()` calls into one. | 5 input batches → 1, per session start | None. |
| 5 | **`zoomText` → `shinyjs::html()`.** 10 assignment sites (`step1_server.R:548-744`) for one output. | −1 output, −10 sites | Low. |

**Verify:** re-run the same walkthrough under `VFT_RPROF=1` and compare the
`manageHiddenOutputs` / `shouldSuspend` rows against the 18.42 s / 14.58 s baseline. Items 1+2+5
should move them by roughly their output share; item 3 by whatever fraction of sweeps were
hover-driven — which settles "sweeps vs outputs" at zero instrumentation cost.

---

## Stage 2 — One navigation entry point

New file `R/navigation.R` with `vftGoToStep(r, step, session)` as **the only thing that changes
tabs**. It replaces the 8 trigger `reactiveVal`s (`app_server.R:26-33`), the ~20
`if(is.null(triggerStepN())) ... else triggerStepN(+1)` blocks, and the five near-identical
`"A"`..`"E"` back-navigation blocks.

Pure refactor. It removes the out-of-scope `step1return`/`step5return` reads by construction,
because nothing outside a step's own observer needs another step's return handle any more.

**Verify:** identical forward walkthrough. Back navigation is still unreachable (banner still
dead), so nothing user-visible can regress.

---

## Stage 3 — Step registry, gating, nav bar

New file `R/steps.R`:

```r
VFT_STEPS <- list(
  step1       = list(tab = "tab_step1",       needs = character(0)),
  step2       = list(tab = "tab_step2",       needs = "shape"),
  step3       = list(tab = "tab_step3",       needs = c("shape", "DULN_all")),
  step4       = list(tab = "tab_step4",       needs = c("shape","network","networkNodes",
                                                        "DULN","DULN_all","minThresh")),
  step5       = list(tab = "tab_step5",       needs = c("networkList","SM_pres","SMcolors",
                                                        "shape","species","minCutThresh")),
  newVersions = list(tab = "tab_newVersions", needs = c("networkList","SM_pres","SMcolors",
                                                        "finalPolygons","DULN","shape")),
  finalStep   = list(tab = "tab_finalStep",   needs = c("networkList","versionsUI",
                                                        "SM_pres","shape","finalPolygons"))
)
```

`needs` are `r$` key names, so readiness is `!is.null(r[[key]])` — no parallel state to keep in
sync, and the restore path automatically marks everything it rehydrates as ready.

`vftStepAvailable(r, step)` is a reactive: a step is available if every `need` is present **or**
has a provider (Stage 4) whose own transitive needs are satisfiable. That distinction matters —
`DULN_all` is derivable from `shape`, so step 3 is enabled and entering it triggers the
derivation; `minThresh` comes from the step-3 slider and is not derivable, so step 4 stays
disabled with a tooltip naming step 3.

**Nav bar:** `vftStepNav()` in `R/app_ui.R`, rendered once at app level **outside** the
tabsetPanel — 7 plain `actionButton`s with unnamespaced ids. Static markup, so it is **not an
output** and never enters the sweep. State from one `observe()` doing
`shinyjs::toggleState(id, condition = vftStepAvailable(r, s))` plus a CSS class for "current".

`shinyjs::disable()` is cosmetic — **`vftGoToStep()` must re-check availability server-side and
no-op**, since a user can fire the input from the console.

Ship behind `VFT_NAV=1`, mirroring the existing `VFT_GL` / `VFT_RPROF` / `VFT_DEBUG` convention,
enabling steps one at a time as Stage 5 converts each module.

**Verify:** `VFT_NAV=0` changes nothing. `VFT_NAV=1`: from cold, only step 1 is enabled; after
step 1 confirms, step 2 lights up; step 4 stays dark until step 3 sets `minThresh`.

---

## Stage 4 — Lazy data providers

New file `R/providers.R`. Generalises the memoise-into-`r$` idiom that already exists as
`getPassageTable()` / `getStartingPoints()` (`step5_server.R:474-505`).

```r
VFT_PROVIDERS <- list(
  <key> = list(needs   = character(),      # other provider keys
               async   = TRUE/FALSE,       # TRUE => runs through vftFuture()
               ready   = function(r) ...,  # default: !is.null(r[[key]])
               provide = function(r) ...)  # value, or a promise of one
)
```

**The promise problem is solved by never awaiting.** `vftEnsure(r, keys, session)` walks the
dependency closure, dispatches each missing async provider through `vftFuture()` **once**
(guarded by an in-flight marker `r$.vftInflight[[key]]` so double-navigation cannot double-dispatch),
and **returns immediately, blocking nothing**. The `%...>%` handler assigns `r[[key]] <- value`
and clears the marker; that assignment invalidates `vftStepAvailable()`, which un-gates the step.
Progress via the existing `vftProgress()`, errors via the existing `vftAsyncError()`
(both in `R/async_helpers.R`). **No module server ever awaits a promise** — modules keep receiving
plain values, they are simply constructed later. This is what makes Stage 4 shippable before
Stage 5.

**The split of `sf_to_tidygraph3()`.** Group by consuming step, not by column — dispatch is not
free, so one future per step boundary:

| Provider | Source today | Needs | First consumed |
|---|---|---|---|
| `shape` | step1 draw/upload | — | step 2 |
| `DULN_all` | `sf_to_tidygraph3.R:242-245` | `shape` | step 3 |
| `network` | :15-57, :350 | `shape` | step 4 |
| `DULN` | :230-239 | `shape` | step 4 |
| `networkNodes` | :249-257, :261-281, :329-336 | `network`, `DULN`, `DULN_all` | step 4 + step 5 |

Result: **step 1 prepares only the perimeter.** Step 2 needs nothing new. Step 3 dispatches the
`DULN_all` crop alone. Step 4 dispatches `network` + `DULN` + `networkNodes` as one future.

**Two constraints providers must respect:**

1. **`terra` objects cannot cross the mirai boundary** (external pointers). `terra::wrap()` on
   return, `terra::unwrap()` in the handler — the existing pattern at `sf_to_tidygraph3.R:360-361`
   and `step1_server.R:1205-1206`.
2. **The `.GlobalEnv$.vft_*` caches live in the daemon, not the main process.** Each daemon
   re-reads the national COGs once in its lifetime. That is correct — do **not** hoist the crop
   to the main thread.

**`ready` is not always a NULL check.** Old save files carry `envBase_network` with the fat node
table already attached, so `networkNodes` must test for columns:

```r
ready = function(r) !is.null(r$network) &&
        all(c("Residents", "DULN_WALK_") %in% names(vftGraphTibble(r$network, "nodes")))
```

(`vftGraphTibble()` exists at `R/graph_helpers.R:37`.)

**Invalidation — this is where the "auto-invalidate + warn" decision lands.**
`vftInvalidate(r, key)` NULLs the key and transitively every key whose `needs` contain it.
The graph **must include the cross-step edges from day one**: `SM_pres` → `pathUsage`,
`versionsUI`, `networkList[[i]]$pathUsage`. Without them, going back to step 2, re-confirming,
and returning to step 5 shows a simulation computed against a sensitivity matrix that is no
longer displayed — a silent correctness bug producing plausible-looking wrong maps.

The warning modal belongs in `vftGoToStep()` / the confirm handlers, **not** in `vftInvalidate()`:
compute the transitive closure first, and if it is non-empty and any of those keys is populated,
show a modal naming what will be discarded ("Re-confirming step 2 will discard the simulation
results and 3 saved versions") with Cancel as the default action. Only on confirm call
`vftInvalidate()`.

**Verify:** with `VFT_DEBUG=1`, walk step 1 → 2 and confirm **no raster read occurs**; walk to
step 3 and confirm exactly one `async:send` for `DULN_all`; walk to step 4 and confirm one for
the network group. `vftReport()` should show the step-1 stall gone and a smaller one at step 3/4.
Then load an **old** `.RData` at each of `r$step` 2..5 and confirm no provider re-runs.

---

## Stage 5 — Module lifecycle: first-touch singleton

Build each module server the first time its step is entered, then reuse. **Not** eager at session
start: output registration happens at construction, so eagerly building all 7 modules takes the
sweep set from ~6 to ~31 from t=0 — a 5× regression on the exact metric of Goal B — and would run
`maptiles::get_tiles()` (`step2_server.R:79`) and `vftProtectedAreas()` (`step5_server.R:75`) on
the main thread at session start.

**Convert one module per increment, smallest first:** step3 (435 lines) → step4 → step2 → step5 →
newVersions → lastStep → step1. Keep `VFT_NAV` gating each step until its module is converted.

Four things the singleton breaks, and the fix for each:

1. **Modules capture plain values at construction** — `step2_server(fshape = r$shape)` freezes the
   perimeter. Pass reactives (`shiny::reactive(r$shape)`) or pass `r` itself.
2. **Body side effects that must re-run per visit** — the welcome modal
   (`step1_server.R:30-46`), `shiny.i18n::update_lang` + `updateSelectInput`
   (`step5_server.R:39-40`), `shinyjs::reset` of confirm buttons, map re-initialisation. Extract
   into an `enter()` closure returned by each module: `stepN_server()` returns
   `list(..., enter = function() ...)`, called by `vftGoToStep()`. **This is the most important
   refactor for correctness and it is small per module.**
3. **All `once = TRUE` observers must go** — with one instantiation a `once = TRUE` confirm
   observer fires once and the step can never be left again. This also permanently disposes of
   the Stage 0 `ignoreInit` hazard.
4. **The `$destroy()` lists become dead** (`step5_server.R:1036-1042`, :2212-2233) — delete them
   with the leak they were half-patching. Keep the `removeUI`/`insertUI` of `placeholder_step5`
   and drive it from `enter()`.

**Verify:** `vftModuleInstance()` already logs a per-session tally. After this stage, walking
1→5→1→5→newVersions→5 must leave **every module at exactly 1**. That is the acceptance test and
it needs no new instrumentation.

---

## Stage 6 — Restore path and save compatibility

- `app_server.R:360-361`: the `r$step == 1` branch calls `triggerStep1(1)` which nothing observes —
  correct for free under `vftGoToStep()`.
- Add the missing `r$step == 6` branch routing to `finalStep`.
- Replace the `if/else` ladder with `vftGoToStep(r, names(VFT_STEPS)[r$step])`.
- Keep `terra::rast()` reading both save forms (`app_server.R:334-357`) exactly as is.
- A save containing only `shape` becomes a legal state: resume at step 2 and lazily rebuild
  everything else. New capability, not a compatibility break.

---

## Risks

1. **Stale downstream state is the biggest one** — addressed by the invalidation graph in Stage 4.
   Design the cross-step edges in from the start; retrofitting them means shipping a period where
   backward navigation silently produces wrong maps.
2. **Splitting `sf_to_tidygraph3` changes when node columns exist.** Consumers address them by
   name via `igraph::V(network)$...` (`generatePopulation.R:29-40`). Safe **provided**
   `r$networkList <- list(list(network = r$network, ...))` (`app_server.R:670`) is built only
   after `networkNodes` has run — otherwise step 5 gets a network without `Residents` and
   `sample()` fails on a zero-length `prob`.
3. **Enabling backward navigation exposes ~250 lines that have never executed.** Budget for more
   bugs of the `triggerStep4(triggerStep5()+1)` kind. This is why the nav bar ships behind `VFT_NAV`.
4. **The `setInputValue` shim is a global monkey-patch.** A future feature wanting hover will fail
   mysteriously. Two suffixes only, commented at the shim.
5. **Removing `basemap` touches the save format** — write path drops it, read path must keep
   tolerating it.
6. **Stage 5's conversion is wide** — step2 is 2120 lines, step5 2289, newVersions 3908. One
   module per increment; use `vftModuleInstance() == 1` as the gate for flipping `VFT_NAV` on for
   that step.

---

## Verification

**Standing constraints.** No new instrumentation — the existing machinery (`vftReport()`,
`vftRprofReport()`, `vftSendStacks()` in `R/perf_helpers.R`) is sufficient; make steps more
efficient or eliminate code. The server must run the **installed** package, never
`pkgload::load_all()` — `inst/app/global.R:171-214` detects this and otherwise runs every async
job inline on the shared main thread, which makes all of the above look fine in dev and freeze
production. Check the startup line every time: it prints main pid, worker pids, `VFT_WORKERS`,
resolved workers, and `mirai connections`.

**Per stage:** the "Verify" paragraph in each section above.

**End to end, after any stage:**

```bash
devtools::install(keep_source = TRUE)     # keep_source is required for $lines
VFT_RPROF=1 VFT_RPROF_SECS=1200 VFT_PERF_DIR=/home/frueh/vft_perf   # then launch
```

Drive step 1 → 5, toggle every checkbox on the step-5 map, let sampling auto-stop, then:

```r
visitorFlowTool:::vftReport()                  # stalls, blocking, module instance counts
visitorFlowTool:::vftRprofReport()$lines       # named lines
visitorFlowTool:::vftRprofReport()$self        # where the unattributed time is
```

**Baseline to beat** (2026-08-25, single session, local, step 1→5): 790 s wall clock, 104.9 s
blocked, 13 stalls, longest 26.3 s; `manageHiddenOutputs` 18.42 s, `shouldSuspend` 14.58 s,
`[.data.frame` 28.80 s total.

**Do not set `VFT_MIRAI_PROBE=1`** — it enables extra per-argument mirai sends that compete with
real jobs for the daemon pool and was measured making the app substantially slower.
