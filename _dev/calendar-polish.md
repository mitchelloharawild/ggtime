# Review: `coord_calendar()` implementation

Scope: `R/coord-calendar.R` (2455 lines; 1152 code / 1188 comment), plus the
parts of `R/coord-loop.R`, `R/loop-cut.R` and `R/utils.R` it drives. Reviewed
against the working tree (all 59 `test-coord-calendar.R` tests passing).

Every performance number below was measured on this tree; every defect was
reproduced. Scripts are throwaway, but the commands are given so each can be
re-run.

## Summary

The architecture is sound and the hard parts are right. Cutting the axis at
every granule boundary at once (`calendar_pieces()`) and folding each piece onto
its own row's origin is the correct core idea, and pushing the layout into the
*data* rather than into a grid of panels is what makes the draw cost independent
of calendar size — a property the test suite actually pins down
("the drawn background does not grow with the grid").

What needs work is mostly below that line:

* **One real bug** (sub-2-second calendars silently collapse) and **one design
  question that behaves like a bug** (flipped calendars read backwards).
* **A performance profile that is almost entirely per-call mixtime overhead**,
  not algorithmic — which changes what is worth optimising.
* **Five separate places that each re-derive the flip transposition**, which is
  what hides the flipped-orientation problem.
* **Theme surface that is registered but never read.**

---

## 1. Defects

### 1.1 `time_tol()` is ~1.8 seconds on every modern time axis

`R/coord-calendar.R:1303`

```r
time_tol <- function(x) 1e-9 * max(abs(x), 1)
```

The relative epsilon is scaled to the *magnitude of the epoch*, not to the
spacing of the cuts. On a 2026 `POSIXct` axis this is `1e-9 * 1.8e9 ≈ 1.79 s`;
on a `Date` axis it works out to almost exactly the same figure (`1e-9 * 20684`
days = 1.79 s), because `86400 × 20684 ≈ 1.8e9`.

`calendar_pieces()` (`:1439`) uses it to merge cuts:

```r
inner <- inner[c(TRUE, diff(inner) > tol)]
```

So **any calendar whose rows are shorter than ~1.8 s has every row cut silently
merged away**:

```r
t <- mixtime::datetime(seq(as.POSIXct("2015-01-01", tz="UTC"), by="1 sec", length.out=120))
ggplot(data.frame(t, y=rnorm(120)), aes(t, y)) + geom_point() +
  coord_calendar(cells = NULL, rows = mixtime::seconds(1L),
                 blocks = NULL, panes = NULL, cols = mixtime::minutes(1L))
#> n_col: 2  n_row: 2      (expected 2 x 60)
#> pieces: 2               (expected 120)
```

No error, no warning — the plot is simply wrong. 10-second rows work; 1-second
rows do not.

The tolerance is also far looser than it needs to be: one double ULP at 1.8e9 is
`4e-7`, so `1e-9` relative is ~4500 ULPs of slack.

**Fix.** Scale the tolerance to the *cut spacing*, which is what is actually
being compared, rather than to the epoch:

```r
time_tol <- function(x, spacing = NULL) {
  ulp <- .Machine$double.eps * max(abs(x), 1)
  if (is.null(spacing)) return(1e-9 * max(abs(x), 1))
  max(ulp * 8, min(spacing) * 1e-6)
}
```

Simplest correct version: at each call site pass the smallest granule spacing in
play and use a small fraction of it. `calendar_pieces()` has `diff(inner)` to
hand; `calendar_close()` has the cut vector; `calendar_place_instances()` has
`ends - starts`. Failing that, dropping the constant to `1e-12` moves the cliff
from ~1.8 s to ~1.8 ms, which at least puts it below anything a calendar
plausibly draws — but it only moves the cliff, it does not remove it.

Add a regression test at second resolution; nothing currently exercises a
granule finer than a day.

### 1.2 Flipped calendars read backwards

`R/coord-calendar.R:986` (`tile_viewport()`), `:1029` (`tile_positions()`)

The flip is implemented as a pure transpose — swap `x`/`y`, swap
`width`/`height`. A transpose is a reflection about the main diagonal, so it
reverses *both* reading directions:

```
time = "x":  2015-01-01 x=0.134 y=0.917   2015-01-08 x=0.134 y=0.750
             2015-02-01 x=0.612 y=0.917
             -> columns left to right, rows top to bottom          (correct)

time = "y":  2015-01-01 x=0.917 y=0.134   2015-01-08 x=0.750 y=0.134
             2015-02-01 x=0.917 y=0.612
             -> weeks right to LEFT, months bottom to TOP
```

The first week of a `time = "y"` calendar is drawn at the *right* edge and
successive weeks march leftward; January sits below February. This is baked into
the test suite — "the calendar grid is arranged the same way flipped"
(`test-coord-calendar.R:459`) asserts it explicitly, via
`length(layout$row$y) - findInterval(pos$x, rev(layout$row$y)) + 1L`.

I can't tell from the code whether this was chosen or fallen into; there is no
comment either way, and the docs don't show a flipped example. If it was
chosen, say so in `calendar_layout()`'s roxygen, because the next reader will
assume it's a bug. If it wasn't, the transpose needs to be a reflection *and* a
swap in one place (see §3.1) — `x = 1 - (y + height)` on the flipped axis.

### 1.3 `ggtime.calendar.*.background` is registered but never read

`register_calendar_theme_elements()` (`:1756`) registers `line`, `background`
and `text` for all five granules — 15 elements, plus 5 `element_blank()`
defaults for the backgrounds. But `calendar_element()` is only ever called for
`"line"` and `"text"`:

```
:2116  calendar_element(ctx$theme, "cell", "line")
:2251  calendar_element(ctx$theme, g, "line")
:2287  calendar_element(ctx$theme, "col", "line")
:2322  calendar_element(ctx$theme, granule, "text")
```

So `theme(ggtime.calendar.cell.background = element_rect(fill = "grey90"))` is
accepted and silently does nothing. Confirmed by rendering. The one test that
touches it (`:997`) only asserts it calculates to `NULL`.

The comment justifies this as uniformity ("so that styling a granule is the same
operation whichever granule it is"), which is a fair goal — but a registered
element that no drawing code reads is a promise the package doesn't keep, and
it's the kind of thing that generates bug reports. Either draw them (a
`rectGrob` per granule instance is nearly free given the tiling machinery
already in place, and `calendar_place_instances()` already computes the
`start`/`end` extents needed) or don't register them.

### 1.4 `strip_zero_grobs()` leaves `childrenOrder` stale

`R/coord-calendar.R:1005`

```r
grob$children <- lapply(grob$children[keep], strip_zero_grobs)
```

`gTree` carries a parallel `childrenOrder`, which is not updated:

```
childrenOrder before: r1 NULL r2
children after      : r1 r2
childrenOrder after : r1 NULL r2      # still names the dropped grob
```

It draws correctly today only because `grid`'s `drawGTree` looks up
`children[[nm]]`, gets `NULL` for the stale name, and draws nothing. But
`grid.ls()`, `getGrob()`, `editGrob()` and `setChildren()` all read
`childrenOrder`, so the grob is internally inconsistent.

**Fix.** One line — `grid::setChildren(grob, do.call(gList, kept))` keeps both
fields in step.

### 1.5 Per-panel mutation of granule state, with no protection

`compute_cuts()` (`:537`–`:585`) writes `self$cells`, `self$rows`,
`self$blocks`, `self$panes`, `self$cols`, `self$pane_default` on every panel.

This is safe *today*: every read (`:594`, `:604`, `:616`, `:617`, `:633`, `:662`,
`:688`, `:693`, `:721`) happens later in the same `setup_panel_params()` call.
But note the contrast — `.grid` was given a whole fresh environment per build,
with a long comment explaining why coord-level state leaks between builds, and
then six fields right next to it are mutated on the coord directly.

The exposed one is `loop_granule()` (`:633`), which returns `self$rows`. It is a
`CoordLoop` hook, so it *looks* callable from anywhere, and it is only correct
because `cyclical_scales()` happens to be called after `compute_cuts()` within
one method. With `facet_wrap(scales = "free_x")` over axes on different
calendars, the coord is left holding whichever panel was resolved last.

**Fix.** Return the resolved granules from `compute_cuts()` as part of the `cuts`
list and thread them through, rather than parking them on `self`. Then
`loop_granule()` takes the cuts as an argument, and the coord holds only input.

### 1.6 Caches with no invalidation key

Three caches key on "is it `NULL` yet":

* `grid$layout` in `grid_layout()` (`:813`)
* `piece_row_cache$rows` in `piece_rows()` (`:770`)
* `grid$pane_rows` growth via `add_pane_rows()` (`:748`)

All three are correct only because of a call-ordering invariant: every panel has
been through `panel_cuts()` before anything reads them. That invariant is stated
in comments but not asserted anywhere. `piece_rows()` in particular caches a
value derived from `pane_rows`, which is still growing during
`setup_panel_params()` — if any future change calls `piece_rows()` from inside
`panel_cuts()`, the cache silently freezes a half-grown layout.

**Fix.** Cheap insurance: stamp the cache with `length(grid$pane_rows)` and
`sum(grid$pane_rows)`, and recompute on mismatch. Two extra lines, removes the
whole class of failure.

### 1.7 Exact class-vector dispatch in the fast tiling path

`flatten_drawn_grobs()` (`:1078`) and `tiled_grob_geometry()` (`:1198`) dispatch
on exact class vectors:

```r
!identical(class(grob), c("gTree", "grob", "gDesc"))
identical(class(leaf), c("rect", "grob", "gDesc"))
identical(class(leaf), c("polyline", "grob", "gDesc"))
```

If `grid` or ggplot2 ever adds a class, or ggplot2 changes what `render_bg()`
emits, this falls through to `repeat_grob_in_grid()` — correct output, but the
draw cost silently goes from O(1) grobs to O(n_row × n_col). That is precisely
the property the design exists to guarantee.

The good news: it *is* guarded. `"the drawn background does not grow with the
grid"` would fail. But it only tests `theme_grey()`. I checked `theme_bw()`,
`theme_linedraw()`, `theme_dark()`, `theme_minimal()`, `panel.ontop = TRUE` and a
bordered `panel.background` — all still take the fast path, so widening that test
to a couple of themes is cheap and worth doing.

Consider also `inherits(leaf, "rect") && !inherits(leaf, "gTree")` rather than
`identical(class(...))`, which survives a subclass without giving up the
guarantee.

---

## 2. Performance

### 2.1 The cost is per-call mixtime overhead, not algorithmic

This is the finding that should reshape any optimisation work.
`loop_cuts_by_duration()` costs the same whether it cuts 9 boundaries or 1827:

| granule | over 5 years | over 1 week |
|---|---|---|
| days | 28.1 ms | 25.1 ms |
| weeks | 29.0 ms | 28.4 ms |
| months | 32.9 ms | 23.0 ms |
| quarters | 36.3 ms | 29.3 ms |

Breaking down one call on a 9-cut vector:

| step | cost |
|---|---|
| `granule_seq_by()` | 0.04 ms |
| `time_floor()` (scalar) | 3.38 ms |
| `time_ceiling()` (scalar) | 3.54 ms |
| `seq(from, to, by = step)` | 7.38 ms |
| `time_floor()` (snap, 9 cuts) | 3.02 ms |
| `fill_skipped_cuts()` | 6.04 ms |
| **total** | **25.1 ms** |

Every one of those is fixed S7/vecvec dispatch overhead in mixtime. Vector length
is irrelevant.

`coord_calendar()` makes **5–6 such calls per panel** — `cols`, `rows`,
`blocks`, `panes`, cell gridlines, cell labels — so ~150 ms of a ~400 ms build
for a 5-year daily calendar is fixed per-call overhead. Optimising the calendar's
own arithmetic will not move this; **reducing the number of calls, and pushing
the per-call overhead upstream to mixtime, is the whole game.**

### 2.2 Skip snapping on axes that cannot drift — ~36% of every cut, free

`snap_cuts_to_granule()` and `fill_skipped_cuts()` together are 9.06 ms of the
25.1 ms above. They exist for one reason: daylight-saving drift on a zoned axis.
On any axis without a timezone they are provably no-ops, and I confirmed it:

| axis | days | weeks | months | quarters |
|---|---|---|---|---|
| `Date` | no-op | no-op | no-op | no-op |
| `POSIXct` UTC | no-op | no-op | no-op | no-op |
| `POSIXct` Australia/Melbourne | **works** | **works** | no-op | no-op |

A guard in `loop_cuts_by_duration()` — skip snap/fill when the cuts are already
exactly evenly spaced, or when the time type carries no `tzone` — removes ~36%
of every cutting call with no behaviour change on zoned axes. That's roughly 13%
of total build time for a typical `Date` calendar, for a handful of lines.

Even-spacing is the safer test of the two: it needs no knowledge of mixtime's
internals, and it also covers zoned axes over ranges that happen to contain no
transition.

> **Post-review correction (implemented 2026-08-19).** The even-spacing test
> above is *unsound* and was not used. `seq()` steps a zoned axis by a constant
> 86400 s / 604800 s, so Melbourne daily and weekly cuts are exactly evenly
> spaced *while* every cut past a DST change sits an hour off its boundary --
> an even-spacing guard would silently kill the correction the existing DST
> tests pin down. `cuts_can_drift()` in `R/loop-cut.R` instead tests the thing
> drift actually is: a change in UTC offset across the cuts (via
> `mixtime::tz_name()`, then `as.POSIXlt()$gmtoff`), plus a guard for the
> unrelated job snapping also does (`at[n] >= to`, closing a range the cuts
> stop short of). Anything unknown answers "can drift", preserving current
> behaviour. A third guard (`at[1] == from`) was carried at first, purely
> because mixtime's `seq()` did not start where it was told on a zoned weekly
> axis; that was removed and reported upstream, and the underlying bug is now
> fixed in mixtime (`mixtime/_dev/seq-bug.md`). Measured: -50% on
> `Date`/`POSIXct`, -35% on mixtime axes, 0-3% *cost* on zoned axes; -31%
> end-to-end on a five-year `Date` calendar build.

### 2.3 `cells` is cut twice per panel

`panel_cuts()` cuts `cells` at `:691` via `calendar_cell_breaks()` (over one
row's window) and again at `:709` via `calendar_granule_tables()` (over the whole
calendar span, when `label_cells` is set — which is the default).

Both floor from the same instant: `row_window[1]` and `cell_span[1]` are both
`row_grid[1]`. So the row-window cuts are exactly the head of the span cuts, and
I verified they agree. One of the two calls (~25 ms, ~6% of build) is removable
by deriving `cell_breaks` from the span cut when the span cut exists, and only
cutting the row window when it doesn't.

Worth doing for consistency as much as speed. Right now the two cuts are
*independently* derived, and `fill_skipped_cuts()` takes its nominal step from
`stats::median(diff(cuts))` **of whatever vector it is handed**. A short vector
and a long vector can therefore, in principle, disagree about where a
DST-skipped boundary belongs — which would put the cell gridlines and the cell
labels out of step on a zoned axis. Deriving one from the other makes that
impossible by construction.

Minor, same theme: `blocks` and `panes` are both cut over `span` (`:616`,
`:617`). When they resolve to the same granule that's two identical 25 ms calls.

### 2.4 Not problems (checked, so nobody re-checks them)

* **Facetting does not re-cut.** ggplot2 4.x deduplicates
  `setup_panel_params()` across panels with identical scales: a 6-facet
  fixed-scale calendar makes the same 5 `loop_cuts_by_duration()` calls as a
  1-panel one. No memoisation needed.
* **The geometry tiling path works.** It fires under every stock theme I tried,
  and collapses the whole grid to 2 grobs.
* **`calendar_row_rules()`'s `n_row × n_col` matrix** is not a hotspot at any
  realistic calendar size.

---

## 3. Duplication and complexity

### 3.1 The flip transposition is re-derived in five places

This is the highest-value structural cleanup, and §1.2 is a direct consequence
of it.

| site | what it does with `flip` |
|---|---|
| `tile_viewport()` `:986` | swaps `x`/`y`, `width`/`height` |
| `tile_positions()` `:1029` | swaps the same four, as numerics |
| `calendar_rule_grob()` `:2145` | `args <- list(x = args$y, y = args$x)` |
| `calendar_label_grob()` `:2355` | swaps `x`/`y` *and* `hjust`/`vjust` |
| `render_axis_v()` / `render_axis_h()` `:898`, `:926` | picks `time_dim` vs its complement |
| `arrange_loops()` `:831` | picks `ggplot_global$x_aes` vs `y_aes` |

Six independent encodings of one idea. `calendar_render_context()` already exists
to carry `flip`/`time_dim` together — extend it to carry the *transposition
itself*:

```r
# in calendar_render_context()
transpose = if (coord$is_flipped) {
  function(along, across) list(x = across, y = along)
} else {
  function(along, across) list(x = along, y = across)
}
```

Then each site calls `ctx$transpose()` and the reflection question from §1.2 is
decided once, in one function, rather than five times by accident.

### 3.2 `render_axis_v()` and `render_axis_h()` are the same function

`:898`–`:958`. They differ in exactly four things: `gtable_col` vs `gtable_row`,
`width`/`heights` vs `height`/`widths`, `"y_axis"` vs `"x_axis"`, and
`reverse = TRUE` vs `FALSE`, plus which dimension of the layout they read.

```r
calendar_axis_gtable <- function(axis_grobs, dim, reverse, vertical) {
  if (length(dim[[1]]) < 2L) return(axis_grobs)
  track <- calendar_dim_tracks(dim, reverse)
  lapply(axis_grobs, function(grob) {
    grobs <- calendar_track_grobs(grob, track$tile)
    sizes <- unit(track$sizes, "npc")
    if (vertical) {
      gtable_col("y_axis", grobs, width = grobWidth(grob), heights = sizes)
    } else {
      gtable_row("x_axis", grobs, height = grobHeight(grob), widths = sizes)
    }
  })
}
```

Both methods become three lines. ~30 lines and one duplicated `if` removed.

### 3.3 Five near-identical granule-resolution blocks

`compute_cuts()` `:537`–`:585` is five 6-line `eval_granule()` calls plus a
sixth to resolve the pane default. Table-driven:

```r
granules <- list(
  cells  = list(quo = self$cells_quo),
  rows   = list(quo = self$rows_quo, missing = self$rows_missing,
                unit = "week",    fallback = quo(day(7L))),
  blocks = list(quo = self$blocks_quo),
  panes  = list(quo = self$panes_quo, default = quo(month(1L))),
  cols   = list(quo = self$cols_quo, missing = self$cols_missing,
                unit = "quarter", fallback = quo(month(3L)))
)
```

with one loop over it. Cuts ~50 lines to ~15, and — more usefully — puts the
three different defaulting policies (none / `missing()` fallback / resolve-then-
compare) side by side, where the asymmetry the long comment at `:276`–`:308`
spends 30 lines explaining becomes visible at a glance.

### 3.4 Three instance-table builders that produce the same frame

`calendar_col_instances()` `:1633`, `calendar_place_instances()` `:1662`,
`calendar_group_instances()` `:1707` all return
`data_frame(time, col, piece, start, end)`. Two of the three set
`start = 0, end = 1` and differ only in how they choose the representative piece
and the time to label it with:

* `col`: `piece = match(seq_len(n_col), pieces$col)`, time from `col_times`
* `group`: `piece = which(starts)`, time from the midpoint of the group's cuts

One helper — `calendar_row_spanning_instances(piece, time, pieces)` — covers
both, leaving `calendar_place_instances()` (the only one that genuinely computes
extents) standing alone. ~20 lines out, one concept in place of three.

### 3.5 The two largest methods should be free functions

`R/loop-cut.R`'s header states the principle:

> Keeping it free of ggproto also keeps it directly unit testable.

`compute_cuts()` (118 lines) and `panel_cuts()` (102 lines) are the two biggest
things in the file and both violate it. Both are pure functions of their
arguments plus the resolved granules — once §1.5 stops parking those on `self`,
both extract cleanly:

```r
calendar_compute_cuts(time_range, granules, trans)
calendar_panel_cuts(cut_params, cuts, granules, trans, spacing)
```

They can then be tested without building a plot, which is currently the only way
to reach either.

### 3.6 `register_calendar_theme_elements()` is three parallel loops

`:1756`–`:1830` builds `element_tree`, then `defaults` for backgrounds, then
special-cases four line elements, then loops again over `justification` and
`sizes` lists that are keyed by granule. Five separate granule-keyed structures
for what is one table:

| granule | line | text size | text just |
|---|---|---|---|
| cell | `rel(0.5)` | `rel(1)` | `c(1, 1)` |
| row | inherit | `rel(0.8)` | `c(0, 0)` |
| block | `rel(2)` | `rel(0.8)` | `c(1, 0)` |
| pane | blank | `rel(0.8)` | `c(0, 1)` |
| col | blank | `rel(0.8)` | `c(0.5, 0)` |

Write that as one literal and generate from it. The current form makes it hard
to see that `row` is the only granule with no explicit line default — which is
exactly the sort of thing a reviewer of the granule hierarchy wants to see.

### 3.7 Calendar-only helpers live in `utils.R`

`calendar_tiles()` (`R/utils.R:433`) — carrying the `calendar_` prefix —
`rep_len_last()` (`:410`), `pad_zeros()` (`:422`) and `run_lengths()` (`:404`)
are used *only* by `coord-calendar.R`. `calendar_tiles()` in particular is the
other half of `calendar_layout()` and reads oddly split from it. Move them into
the "grid geometry" section.

### 3.8 Comment style

The comment-to-code ratio (1.03) is *lower* than `coord-loop.R` (1.64) and
`geom-time-line.R` (2.19), so density is house style and I'd leave it alone. The
prose is genuinely good — `calendar_row_cuts()`'s February-2015 example and
`cut_connected()`'s ribbon explanation are the kind of comment that saves an
afternoon.

One specific category is worth pruning, though: comments that argue with a
previous version of the code rather than explain the current one.

* `:1947` — "…rather than `vctrs::vec_slice()`, which was mixed with plain `[`
  in this same expression for no functional reason."
* `:813` — "…`render_bg()`, `render_fg()`, both axis renderers and
  `arrange_loops()` (once per layer) all used to recompute it independently."
* `:1305` — "each used to derive it separately as an equal `1 / n` division,
  which only held while the grid was seamless."
* `:276`–`:308` — 33 lines arguing why a sentinel for `panes` would be
  redundant, in the function that doesn't have one.

These are changelog entries. They belong in `NEWS.md` or the commit message; in
the source they're weight a reader has to carry past. That's ~60 lines.

---

## 4. Suggested order

Roughly by value per unit of risk:

1. **§1.1 `time_tol()`** — a live silent-wrong-output bug. Fix and add a
   second-resolution test.
2. **§2.2 skip snap/fill when cuts can't drift** — ~13% off every build, few
   lines, easily tested against the zoned/unzoned table above.
3. **§1.4 `setChildren()`**, **§1.6 cache stamps**, **§1.7 widen the tiling
   test** — one-liners, remove whole classes of latent failure.
4. **§3.1 concentrate the flip** — then decide §1.2 deliberately, in one place,
   and either fix the orientation or document it.
5. **§1.3 backgrounds** — draw them or drop them; either way stop registering
   something inert.
6. **§3.2, §3.3, §3.4, §3.6** — mechanical deduplication, ~120 lines out, all
   covered by existing tests.
7. **§1.5 + §3.5 together** — stop mutating `self`, then extract the two big
   methods as free functions. Largest change, best done last, and it makes
   everything above easier to test.

Items 1–3 are strictly additive and could go in as one commit. Item 7 is the
only one that touches the `CoordLoop` hook contract.
