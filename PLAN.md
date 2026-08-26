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
| Backward navigation invalidating later results | **It does not.** Navigation reads; it never destroys. What destroys derived results is the confirm handler that writes new ones — auto-invalidate at that write, with a cancellable warning modal, and only for the keys whose value actually changed (corrected 2026-08-26; see "The trigger is the write"). |
| Navigation UI | **New static nav bar.** Delete `imageMap()` and the 6 banner `renderUI` outputs. |
| Module lifecycle | **Singleton per session, converted one module at a time**, smallest first. |
| First shippable increment | **Stage 0 + Stage 1** (dead code, re-entry bugs, flush overhead). |
| Old save file compatibility | **Not a constraint** (2026-08-26). Saving is being rewritten in its own session, so no remaining stage has to keep reading files an earlier build wrote. Do not design around the old shapes. One exception that only looks like one: `networkNodes`' column test serves *current* files too — see Stage 6. |

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
  ~~**Keep reading `envBase_basemap` on the restore path** for old save files, then discard it.~~
  **As built it is not read at all** — step 1 never assigned it, so every file carrying it carries a
  NULL and `load()` ignores the extra object. Old files stopped being a constraint on 2026-08-26
  anyway; see the decisions table.
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

**`ready` is not always a NULL check.** Save files carry `envBase_network` with the fat node table
already attached — old ones *and* the ones this build writes — so `networkNodes` must test for
columns. This is the one place that looks like an old-file concession and is not; it is why no
restore ever rebuilds the network:

```r
ready = function(r) !is.null(r$network) &&
        all(c("Residents", "DULN_WALK_") %in% names(vftGraphTibble(r$network, "nodes")))
```

(`vftGraphTibble()` exists at `R/graph_helpers.R:37`.)

**Invalidation — this is where the "auto-invalidate + warn" decision lands.**
`vftInvalidate(r, key)` NULLs the key and transitively every key whose `needs` contain it.
The graph **must include the cross-step edges from day one**: `finalPolygons` → `networkList` →
`pathUsage` → `versionsUI`, and `networkList[[i]]$pathUsage` reached through the hook. Without
them, going back to step 3 or 4, re-confirming, and returning to step 5 shows a simulation
computed against a network that is no longer displayed — a silent correctness bug producing
plausible-looking wrong maps.

> **Corrected 2026-08-26.** This paragraph originally named `SM_pres` → `pathUsage` as *the*
> cross-step edge, and Stage 4 shipped it. It is wrong, and going back to step 2 therefore threw
> away every simulation and every saved version. **Step 2 is independent of steps 3–6.** The
> simulation engine — `launchMultiSim()`, `launchSim_v2()`, `generatePopulation()`,
> `subsetPopulation()`, `generateAoI2()`, `determineShortestPath()`, `choosePath()`,
> `determineAgentCharacteristics()` — contains no reference to `SM_pres`, `SMcolors`, `species` or
> `minCutThresh`. Every consumer of them is a *display* consumer (leaflet raster overlays, a
> `terra::plot` overlay, the GeoTIFF export and its species caption) and each reads the current
> value, so changing the species set redraws the overlay and mixes nothing. The edges are removed;
> see the note on `VFT_DERIVED_FROM$pathUsage` in `R/providers.R` and the suite
> `stage5_step2_independent.R`, which holds both halves of the line: step 2 discards nothing, and
> steps 1, 3 and 4 still discard everything downstream.

The warning modal belongs in the confirm handlers, **not** in `vftInvalidate()`:
compute the transitive closure first, and if it is non-empty and any of those keys is populated,
show a modal naming what will be discarded ("Re-confirming step 2 will discard the simulation
results and 3 saved versions") with Cancel as the default action. Only on confirm call
`vftInvalidate()`.

#### The trigger is the write, not the navigation (corrected 2026-08-26)

Stage 4 shipped this in `vftGoToStep()`: a backward move named everything downstream of the step
being returned to and discarded it on confirm. That is the wrong event, and it was wrong twice
over.

*Too eager.* Going back to **look** destroyed results the user had not decided to replace. Step 5
→ step 3 threw away every simulation before the threshold slider had been touched.

*Not eager enough.* The destruction that actually happens is a write, and a write is reachable
without a backward move. Step 5 → step 2 is free (step 2 is display-only downstream), but
confirming step 2 walks **forward** to step 3, and forward moves were never checked — so the user
arrived at step 3, confirmed a new threshold, and the Zielgebiete were regenerated with no warning
at all while the nav bar went on offering step 5 and its now-orphaned scenarios. That is the bug
the user reported, and no amount of tuning the backward-navigation rule would have caught it.

So the event is the write. `vftCommit(r, values, session, step, then, onCancel)` in
`R/providers.R` is the single door every confirm handler puts its results through:

1. `vftChangedKeys()` compares each value with what `r` already holds. **Re-confirming a step
   without changing anything costs nothing** — no modal, no invalidation, just the move.
2. Only the keys that actually changed seed `vftInvalidationPreview()`. If something populated is
   downstream of them, `vftAskCommit()` raises the modal and **nothing is written yet**.
3. On OK, `vftApplyCommit()` invalidates first (so `VFT_INVALIDATE_HOOKS`, which reach sideways
   into `r$networkList`, work on the old list) and then writes every value, including the
   unchanged ones — a key that did not change may still have been cleared as a dependent of one
   that did.
4. On Cancel, nothing is written, nothing is discarded, and the step's `onCancel` puts its own
   buttons back.

The pending decision is parked in `session$userData$vftCommitPending` and answered by **one** pair
of observers registered once per session (`vftCommitServer()`, called from `app_server()`). A
fresh pair per modal is a trap: a cancelled one stays armed and the *next* modal's OK runs the
*previous* modal's closure, writing values the user has abandoned. `commit_on_create.R` holds that
case explicitly.

Two supporting changes were needed for "entering a step destroys nothing" to be true in practice:

* `app_server()` now passes `finalPolygons = shiny::reactive(r$finalPolygons)` to `step4_server()`.
  It used to leave the argument at its `NULL` default, so step 4's `enter()` reset its polygons to
  NULL on every visit and `.vftStep4Launch()` generated a fresh set — merely *looking* at step 4
  replaced the Zielgebiete. They are regenerated only when something upstream invalidated them,
  which leaves the key NULL again. `.vftStep4Launch()` additionally keeps an unconfirmed working
  set when `cache$aoiKey` (threshold, skip, perimeter) still matches, so a user who draws three
  areas, walks back to re-read step 3 and returns finds their three areas.
* Step 4's confirm no longer destroys its five map/confirm observers. That was there because
  confirming meant leaving for good and a *rebuilt* module would stack a second set of handlers;
  `enter()` has torn them down on every visit since Stage 5. Left in, it would have made Cancel
  useless — the user would land back on a frozen map they could neither edit nor confirm again.

#### Two collisions the singleton exposed (2026-08-26)

Both are the same shape: a variable that carried two meanings at once, harmless while the module
was destroyed on the way out, and permanent once it is not. Neither is visible in the dependency
graph, and neither would be found by re-reading `vftCommit()`.

**A module's results have to be published as they are produced.** Step 5 writes a simulation to
`r$networkList[[i]]$pathUsage` in its own `reactiveValues`, and that reached the app's `r` only
through the handlers on two buttons — "Neue Versionen" and confirm. Leaving by the nav bar goes
through neither, so `r` never heard about it and `enter()` overwrote the module's list from `r` on
the way back in. Run a simulation, step out, step back: gone. (The confirm door has never worked
either — step 5 has no live `confirmButton5` observer — so only the newVersions round trip ever
saved anything.) `vftMirror(r, key, get)` in `R/modules.R` publishes each output as it changes,
with two guards: an empty result is not a result, and an unchanged one is not republished —
`enter()` writes `r`'s own value into the module on every visit, and echoing it back would
invalidate every reader of the key for nothing. **When newVersions is converted it needs the same
treatment**, and so does anything else that produces results the user can lose.

**`r$checkboxSave` in step 2 meant both "a saved selection is waiting to be restored" and "here is
what the user just confirmed".** `obsGrpTr` is muted while the first is true, because a restore
drives the checkboxes itself; `obsConfirm` set it to satisfy the second. So after one confirm the
group boxes were muted for the rest of the session: come back to step 2 and every group box
selects nothing, except "Alle", whose observer carries no such guard. The confirm handler writes
`r3$checkboxOut` now, and the flag means only what it says. Suite: `step2_group_reentry.R` —
runtime for both halves (the mute works when armed; the group boxes work when it is not), plus a
source check that `r$checkboxSave` is written in exactly two places, because a harness without the
SDM stack cannot tick a species without killing the session.

**Verify:** with `VFT_DEBUG=1`, walk step 1 → 2 and confirm **no raster read occurs**; walk to
step 3 and confirm exactly one `async:send` for `DULN_all`; walk to step 4 and confirm one for
the network group. `vftReport()` should show the step-1 stall gone and a smaller one at step 3/4.
Then load a `.RData` at each of `r$step` 2..5 and confirm no provider re-runs. (This used to say an
**old** file specifically; as of 2026-08-26 old files are not a constraint, so use one this build
wrote.)

### As built (2026-08-25) — five deviations, all deliberate

1. **`shapeLarger` is a fifth provider.** Every crop and the path query are cut against the 1 km
   buffered perimeter, not `shape`, and step 1's two confirm branches normalise the drawn and the
   uploaded shape differently before buffering. So step 1 keeps producing it (two `sf` calls, on
   the main thread, on one polygon) and a **synchronous** provider derives it from `shape` alone
   for the restore path, which no save file has ever carried it on.
2. **`vftStepAvailable()` was not widened; `vftStepReachable()` was added beside it.** Modules
   still capture plain values at construction, so entering a step whose data is merely *derivable*
   would build it against NULLs. Reachable gates the **button**; available gates the **entry**.
   The gap between them is handled by deferral: `vftGoToStep()` calls `vftEnsure()`, records
   `session$userData$vftPending`, and returns without changing tab; the provider observe performs
   the navigation when the last key lands. This applies to **every** caller, not just checked ones
   — step 3's confirm button reaches step 4 with `check = FALSE` and has the same wait to do.
3. **The in-flight markers are not in `r`.** Dispatch happens inside the provider observe, so a
   reactive marker would invalidate the observe that just set it and dispatch again. They are a
   plain environment in `session$userData`, read only from inside that observe.
4. **The Stage 3 busy guard was NOT deleted.** Per-key markers stop a *provider* being dispatched
   twice; they say nothing about the futures step 2, 4 and 5 dispatch from their own module
   bodies. With the daemon death of 2026-08-25 still unexplained, a cheap generic guard was kept
   rather than removed on schedule. Delete it in Stage 5, once module bodies stop re-running.
5. **A failed provider is recorded and not retried.** Without it the observe is a spin loop: the
   key is still missing, its needs are still ready, nothing is in flight — so it dispatches again
   every flush, forever. `vftInvalidate()` clears the failures, so changing the inputs earns the
   retry.

The invalidation graph is complete and correct but **dormant**: it fires on backward navigation,
which `VFT_REENTRANT_STEPS` still blocks. That is the intended order — the edges exist before the
navigation that needs them, rather than after.

> **Superseded 2026-08-26.** The graph is no longer fired by navigation at all; it is fired by
> `vftCommit()` from each step's confirm handler. See "The trigger is the write, not the
> navigation" above. The edges themselves are unchanged.

---

## Stage 5 — Module lifecycle: first-touch singleton

Build each module server the first time its step is entered, then reuse. **Not** eager at session
start: output registration happens at construction, so eagerly building all 7 modules takes the
sweep set from ~6 to ~31 from t=0 — a 5× regression on the exact metric of Goal B — and would run
`maptiles::get_tiles()` (`step2_server.R:79`) and `vftProtectedAreas()` (`step5_server.R:75`) on
the main thread at session start.

**Convert one module per increment, smallest first:** step3 (435 lines) → step4 → step2 → step5 →
newVersions → lastStep → step1. Keep `VFT_NAV` gating each step until its module is converted.
*(step1 was taken out of this order on 2026-08-26, on request — see below. **All seven are done as
of 2026-08-26**, so `VFT_NAV=1` no longer gates anything back.)*

Four things the singleton breaks, and the fix for each:

1. **Modules capture plain values at construction** — `step2_server(fshape = r$shape)` freezes the
   perimeter. Pass reactives (`shiny::reactive(r$shape)`) or pass `r` itself.
2. **Body side effects that must re-run per visit** — `shiny.i18n::update_lang` + `updateSelectInput`
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

### As built (2026-08-25/26) — **complete: all 7 modules converted**

**The mechanism is `R/modules.R`** and it is done: `vftModuleOnce()`, `vftModuleHandle()`,
`vftModuleEnter()`, plus one call in `vftGoToStep()` right after the visit counter is bumped.

**`VFT_REENTRANT_STEPS` is the single switch** and means three things at once — the module is
reused rather than rebuilt, `vftGoToStep()` calls its `enter()`, and the nav bar offers it. Adding
a name to that vector *was* the act of converting a module. As of 2026-08-26 the vector holds all
seven, so nothing takes the unconverted branch any more; it is kept because it is still the way to
back a single conversion out.

| module | lines | state |
|---|---|---|
| step3 | 400 | **converted** — inputs are reactives, `enter()` re-runs banner/language/buttons/state and re-fetches the basemap only when the perimeter changed |
| step4 | 1305 | **converted** — `enter()` snapshots the reactives into locals of the same names, so the body is untouched; also destroys the previous visit's five map-interaction observers |
| step2 | 2205 | **converted** — the perimeter, its three projections, the basemap and the species scan are one snapshot `enter()` refills only when the shape changed; the twelve-observer teardown lists are gone |
| step5 | 2384 | **converted** — snapshot inputs like step 4; `enter()` also empties `#placeholder_step5` and destroys the previous visit's per-version observers before re-inserting the cards |
| step1 | 1217 | **converted** — out of turn, because the user asked for the return. `enter()` puts the perimeter in force back on the map, clears the drawing state and the step's own answer; the welcome modal stays where it was, in the body, and so greets nobody twice |
| newVersions | 3953 | **converted** — snapshot inputs; `enter()` clears `#placeholder` and the per-card observers, re-asks `isFirstRun`, and re-reads the two lists step 5 published. The fifteen-observer teardown in the confirm handler is gone with its `once = TRUE`, and `vftMirror()` now publishes its results |
| lastStep | 486 | **converted** — the smallest and the last. Snapshot inputs; `enter()` clears `#placeholder_lastStep` and the per-card observers and bumps a new `r$mapRedraw`. No `vftMirror()`: it produces nothing the app reads back |

**Four things learned converting the first two:**

0. **`enter()` must run with the MODULE's session as the default reactive domain.** Found in the
   live app: returning to step 3 left "Bestätigen" disabled. `enter()` ran and
   `shinyjs::enable("confirmButton3")` ran, but shinyjs and `update*Input()` namespace against
   `getDefaultReactiveDomain()` — not the session in lexical scope — and shinyjs prefixes an id
   only when that domain `inherits(., "session_proxy")`. During `vftGoToStep()` the domain is the
   *app* session, which does not, so the message addressed a control called `confirmButton3` that
   does not exist. No error, no warning. Every `update*Input()` in an `enter()` was missing the
   same way. `enter()` is therefore built by **`vftModuleEnterFn(session, function(){ ... })`**,
   which supplies this and (1) together so the five modules still to convert cannot omit either.

1. **`enter()` must be wrapped in `isolate()` as a whole.** It is called from `vftGoToStep()`,
   which is called from observers — including the provider `observe()`, which is *not* isolated.
   A bare `network()` read there makes that observe depend on the network, and it also *assigns*
   the network, so step 4 would re-enter itself forever.
2. **Snapshot, don't reactive-ise, a large module.** step 4 reads its nine inputs in ~90 places
   inside nested closures. `enter()` refills locals of the same names with `<<-`, so the 1300
   lines below are unchanged and still see plain values — which is what they want: a visit works
   against a fixed network, and only *between* visits may it change.
3. **Observers trapped in a helper's frame leak on re-entry.** step 4's banner and confirm
   observers lived in `plotMap()`'s frame and were unreachable from outside it — fine when every
   visit got a fresh frame, a leak now. They are on `r` with the other three, and `enter()`
   destroys all five before `plotMap()` makes new ones. **Leaving a step by the nav bar does not
   go through the confirm or banner handler**, so nothing else would have.

A `once = TRUE` observer created *inside* `enter()` as a deferred one-shot (step 4's AoI launch)
is correct and stays. The flag only had to go from the app-level observers watching a module's
return handle for the life of the session.

### Live findings, 2026-08-25/26 — fixed since the table above

Two defects the real app surfaced that no test here had caught, and one decision.

**`enter()` was running in the wrong reactive domain.** Returning to step 3 left "Bestätigen"
disabled. `enter()` ran, `shinyjs::enable("confirmButton3")` ran, nothing errored — but shinyjs
and `update*Input()` namespace against `getDefaultReactiveDomain()`, not the session in lexical
scope, and shinyjs prefixes an id **only** when that domain `inherits(., "session_proxy")`. Inside
`vftGoToStep()` the domain is the *app* session, which does not, so the message addressed a
control named `confirmButton3` that does not exist. Silent no-op. Every `update*Input()` in an
`enter()` was missing the same way, and step 4's `confirmButton4`/`resetButton` had it waiting.
Fixed by `vftModuleEnterFn(session, function(){ ... })` in `R/modules.R`, which supplies the module
domain **and** the `isolate()` together — **every remaining conversion must build its `enter()`
with it.** Regression test `stage5_enterdomain.R` asserts the id *on the wire*, and is verified to
fail when the `withReactiveDomain()` is removed.

**The step-4 autosave died with `terra::wrap(NULL)`.** `wrap()` aborts on NULL rather than
returning it, inside a download handler, so the browser got an error page instead of the file. The
real fault was an asymmetry: `downloadSave`'s read path has always accepted a save file with no
sensitivity matrix and skipped it; the write path aborted rather than producing one. Guarded at
`app_server.R:157`. `r$SM_pres` can legitimately be NULL there by two routes both opened by Stages
3–4 — a user routing around step 2 entirely (the nav bar offers step 3 as soon as step 1 is
confirmed, and neither step 3 nor step 4 needs `SM_pres`), or going back to step 1, which discards
it as a dependent of `shape`. Going back to step **2** does not: `vftDependents()` excludes its own
seed keys, so only the packed copy goes. Test: `stage5_savepath.R`.

**The banner is being retired — do not gate it.** `vftGoBack()` calls
`vftGoToStep(check = FALSE)`, so a banner letter bypasses the re-entry block the nav bar enforces:
"A" rebuilds step 1 and clears the sensitivity matrix with it. ("B" no longer rebuilds step 2 as of
2026-08-26 — it re-enters the singleton, which is right by accident rather than by design.) Decided
2026-08-26 that the nav bar replaces the banner outright, so this is left alone deliberately and
noted at `vftGoBack()` in `R/navigation.R`. Spend no effort on banner paths in the remaining
conversions.

### step 2, converted 2026-08-26 — what was specific to it

Everything below is in `R/step2_server.R` and is the third worked example of the standard four
steps. Three things were not in step 3 or step 4:

1. **The species scan had to move into `enter()` too.** It was a bare
   `observeEvent(NULL, ..., once = TRUE, priority = 12)` in the body — exactly right for a module
   built once per *visit*, and wrong for one built once per *session*: a user who went back to
   step 1, redrew the area and returned would have been choosing species for the area they had just
   replaced. It is `loadSpeciesData()` now, called from the changed-perimeter branch of `enter()`,
   and still a deferred one-shot observer so it runs after the body has registered its outputs.
2. **A re-scan has to apply the filter itself.** `r3$spChoices` is written in exactly one place —
   `obsFilter`, on a *change* to `input$filterList` — and the scan's completion nudges that input to
   `"s8"`. On the second scan the select is *already* `"s8"`, so nothing changes, `obsFilter` never
   runs and the species list stays the previous perimeter's. The `switch` is now
   `filterSpChoices()` and both callers use it, so the two paths cannot drift.
3. **The map is drawn from plain locals**, so a return visit has nothing reactive to re-render it.
   `mapRedraw` — a `reactiveVal` bumped by `enter()` and read at the top of `output$SDMmap` — is the
   nudge, with a `req()` beside it for the window before the first `enter()`.

The six save-state arguments (`checkboxSave`, the three `groupSave`s, the two weight lists) are read
**once, at construction, and deliberately not refreshed by `enter()`**. They exist to restore a
confirmed selection into empty checkboxes; on a return visit the checkboxes still hold that
selection live, and re-arming them would let the delayed restore block hijack the user's next filter
change and put the old selection back. The restore-from-file path populates `r` inside step 1's
confirm handler, before step 2 has ever been built, so it still gets them.

Also: `app_server` resets `step2-confirmButton2` on the way out, which re-fires `obsConfirm` with 0
now that the observer no longer destroys itself — hence the zero guard, the same one step 3 needed.
And the three teardown lists were not even identical to each other: `obsSelectAfter`'s
`obsWeights$destroy()` had no `is.null()` guard and errored whenever no species had ever been
weighted.

### step 5, converted 2026-08-26 — what was specific to it

Snapshot inputs through `.rx`, exactly as step 4. What is step 5's own is that its per-visit work
is the largest in the app, and one part of it is destructive:

* **The version cards.** One `insertUI()` into `#placeholder_step5` per entry in `versionsUI`, each
  with an observer of its own appended to `r$obsEventSelList`. Re-entering without clearing first
  shows every version twice and runs one card's handler twice per click — the step-5 shape of
  step 4's "one click, two polygons". `enter()` destroys the list and empties the placeholder
  before `generateVersionImages()` re-inserts. The clear used to be in the *go to new versions*
  handler: on the way out, and `once = TRUE`, so it worked for exactly one round trip and not at
  all for the nav bar.
* **`r$networkList` and `r$versionsUI` are refreshed by `enter()`**, which is the entire point of
  the newVersions side trip: that page writes both back into the app's `r` and step 5 has to pick
  them up. Under the old rebuild this happened by construction; under a singleton it has to be
  written down.
* **`isFirstRun_stp6` is re-asked on every entry**, so the "Original" scenario is created from
  `enter()` too. `app_server` clears `r$step6FirstRun` **outside** the `vftModuleOnce()` block —
  inside it, the flag would be cleared once and `vftInvalidate()`'s re-arm (when the saved versions
  are discarded) would never be seen. The order is "enter() asks, then app_server clears", because
  `vftGoToStep()` calls `enter()` directly while the counter write that re-runs that observer is
  deferred to the flush.
* `vftProtectedAreas()` is cached against the perimeter, like step 3's tiles and step 4's lakes.

Note `step5_server` never had a live `confirmButton5` observer — the final-confirm block is
commented out at what is now `step5_server.R:2193`. `r$confirm` is only ever a banner letter or 0,
so `app_server`'s step5 → finalStep branch has always been unreachable. Left alone: it is Stage 6's
question, and the nav bar reaches `finalStep` anyway.

### step 1, converted 2026-08-26 — what was specific to it

Asked for directly: *"Make it possible to also go back to step 1. Like other steps now, going back
to step 1 should not change anything, and the current shape outline should be shown. Only
finalizing a new outline in step 1 should replace the old one."* Taken out of the conversion order
for that reason. Four things are step 1's own:

* **It is the only module built at session start.** Its observer is `ignoreNULL = FALSE`, so it
  fires before any counter moves — "first touch" for step 1 is the page opening, not a navigation.
  Consequences: the module store is not empty at t=0 (it holds `step1` and nothing else, which is
  still the whole of "not eager"), `vftStepEntered()` keeps its special case, and the nav bar's
  step-1 button — which had been dark since the first flush, because step 1 was *entered* and
  unconverted — lights up.
* **Its `confirm` is a constant, not a click count.** Every other step answers with a rising button
  count, so each confirmation invalidates by itself. Step 1 answers `1` (or `-1` for a loaded save
  file), and `reactiveValues` dedupe: writing 1 over a 1 wakes nobody, and the second confirmation
  of the session would do nothing at all. Cleared in two places, and both are needed —
  `enter()` for "left and came back", `reportConfirm()` for "cancelled the discard modal and
  pressed again without leaving". The `once = TRUE` on app_server's confirm observer had to go with
  the rebuild it existed for.
* **The map has to draw the outline that is in force.** The render asked `r1$shape`, the *uploaded
  file* — a perimeter the user drew lives in `r1$finalShape` and never in `r1$shape` — so a return
  visit after a drawing showed an empty map of Switzerland with the area silently still in effect.
  It reads `finalShape` first now, fully isolated: the output is suspended while the tab is hidden
  and re-executes when it is shown, which is what redraws it, and `enter()` also pushes it through
  `leafletProxy` so it is there without waiting for a client round trip.
* **"Only finalising a new outline replaces the old one" is a flag, not a comparison.**
  `r1$isNewShape` is set at the three places an outline can be made (upload, coordinates, a polygon
  closed on the map) and cleared by `enter()`. Without it the drawing branch re-combines and
  re-buffers the perimeter on the way out, producing a `shapeLarger` equal to the stored one but
  not `identical()` to it — so every re-confirmation of an unchanged area would have offered to
  throw the whole walk away. With it, `reconfirmUnchanged()` hands the same two objects straight
  back and `vftCommit()` finds nothing changed.

Test: `step1_return.R` (44 assertions), which drives the real `step1_server` under `testServer` —
including drawing a triangle through `areaSelectMap_click` and closing it on the first vertex, which
is the whole "a new outline DOES replace" path — plus a control on the dedupe itself.

### newVersions, converted 2026-08-26 — what was specific to it

The biggest module in the app (3900 lines) and the other end of the busiest re-entry path: it is a
side trip off step 5 in both directions, so every round trip used to call this server again. The
standard four steps applied unchanged — snapshot inputs through `.rx`, an `enter()` built with
`vftModuleEnterFn()`, no more `$destroy()` lists or app-level `once = TRUE`, name added to
`VFT_REENTRANT_STEPS`. Five things are its own:

* **The teardown was in the confirm handler, on the way OUT, behind `once = TRUE`.** Fifteen named
  observers plus `removeObservers(r$appendedObservers)` plus the placeholder reset — all of it
  fired when the user pressed "Bestätigen", so it worked for exactly one round trip back to step 5
  and not at all for the nav bar. Under a singleton it would have been actively harmful: destroying
  `obsConfirm` and its fourteen siblings means the page can be confirmed once per session and then
  never left again. It is `enter()`'s job now, on the way IN, which covers every way of arriving.
* **`vftMirror()` was needed here for the same reason as step 5.** A new version reached the app's
  `r` only through the confirm handler; leaving by the nav bar went through neither, and then
  `enter()` refreshed the module from an `r` that had never heard of it. `networkList` and
  `versionsUI` are mirrored. **The explicit writes in app_server's confirm handler stay** and are
  not redundant: the mirror is an `observe()` and lands at the next flush, while the confirm handler
  calls `vftGoToStep(r, "step5")` — and therefore step 5's `enter()`, which reads `r$networkList` —
  in the same tick. `vftMirror()`'s `identical()` guard makes the second write free.
* **Both render counters had to be seeded in the body.** `output$versionMap` depends on nothing but
  `r$updateRender` and `r$updateNetworkPlot()`; the network itself is a plain local. `enter()`
  therefore has to bump them, and a counter that starts NULL never changes: `NULL + 1` is
  `numeric(0)` and `numeric(0) + 1` is `numeric(0)` again, so the map would have redrawn on the
  first visit and never after.
* **`isFirstRun` is re-asked per visit** — `applyFirstRun()` — because `vftInvalidate()` re-arms
  `r$newVersionsFirstRun` whenever the saved versions are discarded. `app_server` clears the flag
  from **outside** the `vftModuleOnce()` block, exactly as step 5 does with `r$step6FirstRun`.
  `r$versionBtn_nb` is deliberately reset only on a first run: it has to keep climbing, or a new
  version claims an `inputId` a live card already owns.
* **`output$contextChoice_ui` now reads `r$currentLang`** rather than the snapshot. It is the one
  output in the module whose content depends on the language, and with a singleton nothing else
  would ever re-execute it.

`removeObservers()` also gained a `try()` and a NULL guard: a version's removal observer is
`once = TRUE`, so a user who deleted a version leaves an already-destroyed observer in the list, and
an abort inside `enter()` would drop them on a page whose cards were never rebuilt.

Tests: `stage5_newversions.R` (37 assertions — formals, the AST walk over the 3900-line body, the
teardown, the call site, the mirror) and `stage5_newversions_live.R` (13, runtime). The live one's
sharp probe is **the removal button, not the select button**: the select handler is guarded by
`if(r$lastSelectedButton != inputId_select)`, so the first of N stacked copies sets that variable
and the rest return — it does not detect stacking at all. The removal handler has no such guard, so
N copies produce N `removeUI()` calls off one click. Verified: with the teardown taken out of
`enter()`, it reports 2 and then errors out of `r$networkList[[integer(0)]] <- NULL`.

### lastStep, converted 2026-08-26 — what was specific to it

The smallest module and the last one; with it Stage 5 is complete. Standard four steps, plus:

* **It needs no `vftMirror()`.** It produces nothing the app reads back — its only return is
  `confirm`, which carries a banner letter, and its map is display-only. Worth stating, because
  "no mirror" and "mirror forgotten" look identical in a diff.
* **`output$mapArea` needed an explicit nudge.** It is a `renderUI` that reads `r$networkList` and
  `r$selectedNetwork_position` through `isolate()` and takes its geometry from plain locals, so
  nothing in it invalidates on a return. New `r$mapRedraw`, seeded in the body (same `NULL + 1`
  trap as above) and bumped by `enter()`.
* **Its banner handler carried five `$destroy()` calls, four of them naming objects that do not
  exist in this file** — `obsConfirm`, `r$obsMapClick`, `r$obsMarkerClick`, `r$obsErase`. That is
  not dead code, it is unrunnable code: the first of them aborts the observer, and an error in an
  observer takes the session with it. It went unnoticed because the banner has never been clickable
  (`imageMap()` early-returns a bare `<img>`). Removed with the rest of the teardown; the runtime
  test now clicks the banner and asserts the letter comes back.
* **Its UI shipped a duplicate DOM id.** `lastStep.ui.R` and `step5_ui.R` both had a
  `div(id = "topPlaceHolder")`, and all seven module UIs sit in the DOM at once — so
  `insertUI(selector = "#topPlaceHolder")` resolves to step 5's, which comes first. Harmless while
  nobody re-inserted anything; the moment `enter()` started rebuilding the cards it would have put
  them in step 5's tab. lastStep's is `#topPlaceHolder_lastStep` now.

Tests: `stage5_laststep.R` (32) and `stage5_laststep_live.R` (7). The live one deliberately does
**not** click a version card: that handler reads `input$usageSwitch` and then pushes
`r$result$pathUsage` through tidygraph and sf, so with no simulation behind it the observer errors —
and an observer error kills the mock session, which makes every later assertion meaningless rather
than failing honestly. Building a real `pathUsage` graph there would be testing tidygraph. The
stacked-observer behaviour of this identical mechanism is covered at runtime by
`stage5_step5_live.R` and `stage5_newversions_live.R`; what is left for this file is the teardown,
which the insert/remove counts measure directly.

### Next session — start here

**Stage 5 is finished. All seven modules are first-touch singletons.**

**State.** Stage 4 is committed as `3ecb590`, the first Stage 5 increment as `2e288dc`, the step 2 /
step 5 conversions with the move of invalidation onto the write as `784bb5a`, and the step 1
conversion as `768a266`. **The newVersions and lastStep conversions are uncommitted.** 658
assertions green across 27 suites in the session scratchpad (the 28th,
`stage3_nocontext_unfixed.R`, is the negative control and must fail); the package installs clean to
a temp library and `testServer(app_server)` boots with `step1` — and only `step1` — in the module
store.

**Scope note (2026-08-26):** old save file compatibility is **off the table** for everything that
remains — the user is rewriting saving in a separate session. See the decisions table and Stage 6.
Do not add branches to read what an earlier build wrote, and do not weigh a design against it.

**Not yet live-tested: newVersions and lastStep.** Everything else on the list below has been
reported working by the user. The two new conversions need the same treatment in the real app:

- *newVersions:* 5 → Neue Versionen → 5 → Neue Versionen shows each version **once** and one click
  on a card selects it once; a version added on the newVersions page survives leaving by the **nav
  bar** rather than by "Bestätigen"; the X button on a version removes exactly one card; the page
  can be confirmed more than once per session (it could not, before — the confirm handler destroyed
  its own observer).
- *lastStep:* reach Resultate, go back to step 5, run another simulation, come back — the version
  cards are there once each and the map shows the *new* list rather than the one frozen on the
  first visit.

**Live checks — all reported working by the user, 2026-08-26.** Kept as the regression checklist
for this machinery, not as outstanding work:

- *the commit change* (`commit_on_create.R` covers the layer, not the modules): 5 → 2 → confirm → 3
  leaves the scenarios alone and leaves step 5 offered; confirming a **new** threshold at step 3
  raises the modal, and Cancel leaves step 3 usable (both buttons live) with everything intact;
  5 → 4 → confirm asks before replacing the scenario list, and Cancel there leaves the map
  editable; entering step 4 without confirming shows the Zielgebiete that are already there rather
  than generating new ones.
- *the two singleton collisions:* run a simulation in step 5, leave by the nav bar, come back — the
  result and the version cards are still there, and there is still exactly one "Original"; and in
  step 2, confirm, come back, tick a group box other than "Alle" — species are selected.
- *step 5's re-entry:* 5 → Neue Versionen → 5 shows each version **once**, one click on a card
  selects it once, a version created on the newVersions page appears on the return, and the
  simulation can still be launched after two round trips.
- *step 1's return:* confirm an area, walk on, take the nav bar back to step 1 — the map shows the
  area in force (drawn areas included, which was the broken case) and the welcome dialog does not
  reappear; pressing "Bestätigen" through goes to step 2 with **no** discard modal and nothing
  lost; drawing a new area and confirming it **does** raise the modal naming everything downstream.

**The runtime suites are the ones that earn their keep.** `stage5_step5_live.R`,
`stage5_newversions_live.R` and `stage5_laststep_live.R` drive the real module servers under
`testServer`, trace `insertUI`/`removeUI`/`addClass`, and are each **verified to fail** when the
teardown is removed from `enter()`. `step1_return.R` does the same for step 1, drawing a polygon
through the map's click inputs. Everything else in the scratchpad is source or AST inspection.

**The standard four steps per module**, for the record and for any module added later: reactive (or
snapshot) inputs → an `enter()` built with `vftModuleEnterFn()` → remove the `$destroy()` lists and
the app-level `once = TRUE` → add the name to `VFT_REENTRANT_STEPS`. For a module with many read
sites, snapshot rather than reactive-ise: step 4 keeps ~90 reads unchanged by shadowing the
reactives with locals of the same names that `enter()` refills with `<<-`. Two traps worth
restating because both were hit again in these last two conversions: an `enter()` that INCREMENTS a
counter needs that counter seeded in the body (`NULL + 1` is `numeric(0)`, and it stays
`numeric(0)`), and a render that reads everything through `isolate()` or through plain locals needs
an explicit reactive nudge or a return visit shows the previous visit's picture.

**Acceptance test, unchanged and still requiring the live app:** `vftPerfInit()` is called from
`inst/app/global.R`, not `app_server()`, so a bare `testServer(app_server)` has
`session$userData$.vftModules` NULL and proves nothing about the tally. Walk
1→5→1→5→newVersions→5 and read `vftReport()`: **every** module must be exactly 1 — there are no
exempt ones left.

**Next is Stage 6, the restore path**, below. Two things Stage 5 leaves on its doorstep: step 5 has
no live `confirmButton5` observer (the block is commented out at `step5_server.R:2193`), so
`app_server`'s step5 → finalStep branch has never been reachable and the nav bar is the only way to
Resultate; and the `r$step == 6` branch of the restore ladder still does not exist.

---

## Stage 6 — Restore path

**Old save files are no longer a constraint (decided 2026-08-26, by the user).** Saving is being
rewritten in its own session, so nothing from here on has to stay loadable by, or produce files
loadable by, an earlier build. Do **not** spend design on the old shapes; do not add branches for
them; and where an existing branch exists only to tolerate one, it may go.

What that unlocks, concretely — none of it required, all of it now allowed:

- `terra::rast()` accepting both the packed raster and the old xy `data.frame` form
  (`app_server.R:334-357`) no longer has to be kept. It costs nothing to leave, so leave it until
  the save rewrite decides the format.
- The `envBase_basemap` tolerance is already gone: step 1 never assigned it, current files no
  longer write it, and `load()` ignores the extra object either way.
- **`networkNodes`' column test is NOT an old-file concession** — do not delete it with the rest.
  Current save files also carry `envBase_network` with the node table already attached, which is
  exactly why no restore rebuilds the network. See the note under Stage 4.

The restore path work itself is unchanged:

- `app_server.R:360-361`: the `r$step == 1` branch calls `triggerStep1(1)` which nothing observes —
  correct for free under `vftGoToStep()`.
- Add the missing `r$step == 6` branch routing to `finalStep`.
- Replace the `if/else` ladder with `vftGoToStep(r, names(VFT_STEPS)[r$step])`.
- A save containing only `shape` becomes a legal state: resume at step 2 and lazily rebuild
  everything else. New capability — and no longer one that has to be reconciled with anything.

One thing the save rewrite will want to know: `r$pathUsage` is now mirrored out of step 5 as it is
produced (`vftMirror()`), so a save taken at step 5 carries the simulation. The slot and the
restore read were always there; what changed is that they are now populated.

---

## Risks

1. **Stale downstream state is the biggest one** — addressed by the invalidation graph in Stage 4.
   Design the cross-step edges in from the start; retrofitting them means shipping a period where
   backward navigation silently produces wrong maps. **The mirror risk is real too, and it is the
   one that actually bit (2026-08-26):** an edge that is not true throws the user's work away.
   Write an edge only where the downstream value was *computed from* the upstream one — being
   *drawn on top of* it is not a dependency. Check the consumer: if every reader is a plot, an
   overlay or a caption, there is no edge. **And getting the edges right is only half of it:** the
   same day showed that firing a correct graph at the wrong *moment* is its own bug in both
   directions. Fire it when new data is created, never when a step is merely reached.
2. **Splitting `sf_to_tidygraph3` changes when node columns exist.** Consumers address them by
   name via `igraph::V(network)$...` (`generatePopulation.R:29-40`). Safe **provided**
   `r$networkList <- list(list(network = r$network, ...))` (`app_server.R:670`) is built only
   after `networkNodes` has run — otherwise step 5 gets a network without `Residents` and
   `sample()` fails on a zero-length `prob`.
3. **Enabling backward navigation exposes ~250 lines that have never executed.** Budget for more
   bugs of the `triggerStep4(triggerStep5()+1)` kind. This is why the nav bar ships behind `VFT_NAV`.
4. **The `setInputValue` shim is a global monkey-patch.** A future feature wanting hover will fail
   mysteriously. Two suffixes only, commented at the shim.
5. ~~**Removing `basemap` touches the save format** — write path drops it, read path must keep
   tolerating it.~~ **Retired 2026-08-26:** old save files are no longer a constraint, and this one
   was never real anyway — `load()` tolerates the extra object. See Stage 6.
6. ~~**Stage 5's conversion is wide** — step2 is 2120 lines, step5 2289, newVersions 3908. One
   module per increment; use `vftModuleInstance() == 1` as the gate for flipping `VFT_NAV` on for
   that step.~~ **Closed 2026-08-26:** all seven modules are converted. The one-per-increment
   discipline held and the gate did its job; `vftModuleInstance() == 1` is now the acceptance test
   for the stage rather than a per-step gate.

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
