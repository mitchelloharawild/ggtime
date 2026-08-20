# Plan: arrangement and display of `coord_calendar()`

## Problem

`coord_calendar()` currently tiles the panel into an edge-to-edge grid of equal
fractions. Nothing separates the granules, so the output reads as one
undifferentiated block:

- Rows butt directly against each other — a line leaving one row visually
  merges into the next.
- Columns butt directly against each other — you cannot see where January ends
  and February begins.
- `cell` gridlines reuse `panel.grid.minor.x` and vanish against the panel fill.
- `block` gridlines reuse `panel.grid.major.y` with `linewidth` multiplied by 2
  (`R/coord-calendar.R:583`) — invisible in practice, and absent entirely when
  `cols` is set to the same granule.
- Repeated per-column axes collide (`MonTueWedThu…`).

## Settled design

| Decision | Outcome |
|---|---|
| Granule hierarchy | `cell` < `rows` < `block` < `pane` < `cols` |
| `block` | Connected rows, marked by a **heavy rule** |
| `pane` | Groups of rows separated by a **gap** (new granule) |
| `cell`, `rows` | Rules within a column — hairline and thin |
| `cols` | Gap between columns, plus optional border |
| Styling knobs | Registered theme elements, ggplot2 4.0 ink/paper aware |
| Structural knobs | `coord_calendar()` arguments |
| Labels | Per-granule format strings, all five granules; `cell` + `block` on by default |
| Axes | Unchanged — one axis per column, always |

Theme options apply **1:1 to the granules**: every granule gets the same set of
`line` / `background` / `text` / `spacing` elements, whether or not it uses all
of them.

## Constraint that shapes everything

**Spacing must be relative, not absolute.** `Coord$transform()` runs during
gtable construction, before the panel's physical size is resolved, so a
`unit(5.5, "pt")` gap cannot be converted to the `[0, 1]` panel coordinates that
`arrange_loops()` works in. Facets get absolute `panel.spacing` because each
facet is its own gtable cell; a calendar is one panel by design ("the layout is
applied to the data rather than to the drawing").

So the spacing elements hold a **fraction of a tile** (`rel(0.25)`), not a
`unit()`. Do not inherit them from the `spacing` root element, which is
absolute.

**Spacing cannot live in the theme at all** (verified against ggplot2 4.0.3).
The same constraint runs further than first assumed: `ggplot_gtable()` computes
`theme` *after* `by_layer(\(l, d) l$draw_geom(d, layout))`, and neither
`Coord$setup_panel_params()` nor `Coord$transform()` is passed a theme --
`Layout$setup_panel_params()` calls `coord$setup_panel_params(scale_x, scale_y,
params)` and nothing else. A gap moves data, so a theme element could never
feed it; drawing and data would desync. Spacing is therefore a **structural
knob**, taken as `coord_calendar(pane_spacing =, col_spacing =)`, consistent
with the settled design's own split (structural knobs are coord arguments).
`rel()` is accepted there as well as a bare number, since it reads naturally
for a fraction of a tile.

Rejected alternatives:
- Gapping only the drawn background/rules while data stays seamless — data
  overflows into the gaps.
- `Coord$draw_panel()` — it wraps an already-merged panel grob and cannot
  re-slice drawn geometry back into per-tile viewports.
- Rebuilding the calendar as a gtable of real panels — abandons the
  draw-once design and the coord's whole approach.

## Work items

### 1. One source of grid geometry (prerequisite)

Tile geometry is currently computed independently in three places, and the
existing `TODO`s at `R/coord-calendar.R:363` and `:439` already flag it:

- `arrange_loops()` — `x / n_col + (col - 1) / n_col`
- `repeat_grob_in_grid()` — one viewport per tile
- `render_axis_h()` / `render_axis_v()` — `gtable_row`/`gtable_col` at equal widths

All three assume equal fractions. Gaps break that assumption, so factor out a
single `calendar_layout()` returning per-tile origin and extent in npc:

```r
calendar_layout(n_row, n_col, row_pane, spacing)
#> list(
#>   col = list(x = <n_col>, width = <n_col>),
#>   row = list(y = <n_row>, height = <n_row>)
#> )
```

`row_pane` is the pane index of each row, so pane gaps land between the right
rows. Convert all four call sites to consume it. Ship this refactor on its own,
with existing snapshots unchanged (zero spacing ⇒ current geometry), before
touching appearance.

### 2. `pane` granule

Add `pane` alongside the existing four in `coord_calendar()`, defaulting to
`NULL` (no gapping) so current behaviour is preserved until a user opts in.
`pane` is structural like `rows`/`cols` in that it moves geometry, but derived
like `block` in that it groups existing rows rather than cutting new ones —
compute it in `setup_panel_params()` the same way `block_cuts` is computed, via
`loop_index()` over `row_cuts`, and feed the result to `calendar_layout()` as
`row_pane`.

`pane` must be coarser than `rows` and no coarser than `cols`; validate and
error clearly.

### 3. Rule hierarchy and gaps

With `calendar_layout()` in place:

- `cell` — hairline rule, drawn once per row window and tiled (unchanged
  mechanism, new element).
- `rows` — thin rule between rows within a pane.
- `block` — heavy rule where a row starts a new block. Keep the existing
  "same row in every column" shortcut in `calendar_add_block_grid()`, but drop
  the `linewidth * 2` hack in favour of the element's own default.
- `pane` — gap.
- `cols` — gap, plus an optional border rect per column.

Suppress a rule wherever a gap already separates the same boundary, so a block
rule is not drawn against a pane gap.

### 4. Theme elements

Register in `.onLoad()` (`R/zzz.R`) via `register_theme_elements()` with an
`element_tree` of `el_def()`s. Verified to work and to inherit correctly under
ggplot2 4.0.

For each granule `g` in `cell`, `row`, `block`, `pane`, `col`:

| Element | Class | Inherits |
|---|---|---|
| `ggtime.calendar.<g>.line` | `element_line` | `panel.grid` |
| `ggtime.calendar.<g>.background` | `element_rect` | `panel.background` |
| `ggtime.calendar.<g>.text` | `element_text` | `text` |

(No `spacing` element -- see the constraint above. Gaps are
`coord_calendar()` arguments.)

Inheriting the line and rect elements from `panel.grid`/`panel.background` is
what keeps ggplot2 4.0's ink/paper/accent theming working: `theme_grey()` derives
`panel.grid`'s colour from `paper`, so `theme_grey(ink = "white", paper =
"black")` recolours the calendar for free. **Do not hardcode colours** —
set only `linewidth` and `linetype` in the defaults and let colour inherit.
Where an accent is genuinely wanted, take it from the theme's `element_geom`
(`calc_element("geom", theme)@accent`) rather than a literal.

`text` elements carry the label justification, so a day-of-month label in the
cell corner is `element_text(hjust = 0, vjust = 1)`.

### 5. Granule instance tables (prerequisite for labels)

`repeat_grob_in_grid()` replicates one identical grob, which is fine for rules
but not for labels — every cell shows different text. Labels need each granule
instance placed individually.

Build a table of instances per granule, over the whole time range rather than by
arithmetic on a single window:

```r
calendar_granule_table(time_range, granule, cuts)
#> data.frame(time = <native>, col = <int>, row = <int>, pos = <npc within row>)
```

Assign `col` and `row` with `loop_index()` against `col_cuts`/`row_cuts`, as
`transform()` already does for data. Deriving each cell's time by adding
durations to a window start instead would go through irregular-duration
arithmetic (leap days, DST) and drift.

Keep the **native** cuts in `panel_params` alongside the numeric ones —
`setup_panel_params()` currently discards them at
`R/coord-calendar.R:244-251`, and labels cannot be formatted without them.

The same tables can then drive the cell gridlines, replacing
`calendar_cell_breaks()`.

### 6. Labels

Add `cell_labels`, `row_labels`, `block_labels`, `pane_labels`, `col_labels`,
taking mixtime format strings as `scale_x_mixtime()`'s `time_labels` does, or a
function of the native times. Format with `format(x, format = <string>)`, as
`R/scale-time.R:240`.

Defaults — verified against mixtime 0.2.0.9000:

```r
cell_labels  = "{cyc(day, month)}"                            # "07"
block_labels = "{cyc(month, year, label = TRUE, abbreviate = TRUE)}"  # "Apr"
row_labels   = NULL
pane_labels  = NULL
col_labels   = NULL
```

Two things to get right:

- **Bare granule tokens are not supported.** `"{day}"` errors; it must be
  `"{cyc(day, month)}"` or `"{lin(day)}"`. Any documented example must use the
  `cyc()`/`lin()` form. Week labels need the ISO calendar
  (`cal_isoweek$week`), as the existing roxygen examples do — plain `{cyc(week,
  year)}` errors.
- **Plain `Date`/`POSIXct` axes need wrapping.** `coord_calendar()` supports
  them (the test suite uses `Date` throughout), and a mixtime format string will
  not apply directly. Wrap with `mixtime::date()`/`mixtime::datetime()` before
  formatting — confirmed to work. Do not silently fall back to no labels, since
  `cell` and `block` labels are on by default and `Date` axes would lose them.

Labels are decoration, so they must go through `as_decoration()` and skip
cutting, exactly as gridlines and axis keys do.

Turning `cell`/`block` labels on by default is a visible behaviour change for
existing plots — call it out in `NEWS.md`.

### 7. Axes

No behaviour change (one axis set per column). But `render_axis_h()`/
`render_axis_v()` must consume `calendar_layout()` rather than computing `1 / n`
widths, or the axes will drift out of alignment once gaps exist.

Done: both consume `grid_layout()` via `calendar_dim_tracks()`, which turns each
gap into a filler track of its own. Covered by "repeated axes get one track per
row and column" (columns), "repeated axes leave a track for a pane gap" (rows)
and "flipped calendars gap and rule the same boundaries" (transposed).

## Suggested order

1. ~~`calendar_layout()` refactor — no visual change, snapshots unchanged.~~ Done.
2. ~~Theme elements registered; existing `cell`/`block` gridlines moved onto them,
   dropping the `linewidth * 2` hack.~~ Done.
3. ~~`pane` granule and gaps; `cols` gap.~~ Done.
4. ~~Rule hierarchy and gap/rule suppression.~~ Done.
5. ~~Granule instance tables.~~ Done.
6. ~~Labels.~~ Done.
7. ~~Axes.~~ Done (fell out of step 1).

Notes from 5–7:

- `panes` shipped defaulting to `mixtime::months(1L)`, against both this plan
  and the `NEWS.md` entry written for it, and a default coarser than a `cols`
  of weeks tripped its own validation. It now defaults to `NULL` as intended,
  which is also what the six tests failing at the start of step 5 expected.
- An instance is kept for a column only where `loop_index()` puts its time in
  that column, i.e. exactly where `transform()` would draw data of that time.
  A column inherits the widest column's row grid, so without this a narrow
  column labels trailing cells whose data is drawn in the next column along.
  "granule instances land where the data they name does" is the regression net.
- `block`/`pane` instances are placed at the row that starts the group but
  named by the time in the *middle* of it. Every column shares one row grid,
  folded by a numeric offset, so a group's first row can start a day or two
  before or after the boundary it was cut at -- naming a pane by its first row
  labelled Q2's third month "May", and naming it by the offset cut time gave
  the same answer.
- Labels are drawn in `render_fg()`, so `render_bg()` stays rules-only and the
  tests counting tiled background children are unaffected.
- `calendar_cell_breaks()` still drives the cell gridlines. Moving them onto
  the cell instance table would draw one rule per cell rather than one tiled
  set, which is more grobs for the same picture.

Notes from 1–4:

- Each granule's rules are drawn as a grob named after its element
  (`ggtime.calendar.block.line`), the way ggplot2 names the panel's own
  gridlines. The tests find them by name rather than by counting children.
- `element_line()` built outside a complete theme defaults to
  `inherit.blank = FALSE`, so registered defaults must pass
  `inherit.blank = TRUE` or `panel.grid = element_blank()` stops blanking the
  calendar's rules. `theme_test()` (what vdiffr renders with) blanks
  `panel.grid.minor` but not `panel.grid`, which is why moving `cell` onto
  `panel.grid` made it visible in the snapshots.
- `calc_element()` on a bare `theme_grey()` ignores registered defaults;
  they are merged in by `plot_theme()`. Tests that care about the defaults
  have to go through a plot.
- The `.background` elements are registered and blank by default, but nothing
  draws them yet.

1–4 deliver the separation the layout actually lacks; 5–6 are the annotation
stretch goal and depend only on 1.

## Testing

- Keep the existing structural tests (`.n_row`/`.n_col` derivation, no leakage
  between builds, decoration not folded into one row) passing untouched through
  step 1 — they are the regression net for the refactor.
- New unit tests: `calendar_layout()` tile origins sum correctly with and
  without spacing; `pane` grouping indices; granule tables give the right
  time per (row, col, cell); labels format for both mixtime and `Date` axes;
  granule validation errors.
- New `vdiffr` snapshots: pane gaps, column gaps, full rule hierarchy, cell +
  block labels, and one flipped (`time = "y"`) case — `is_flipped` threads
  through every one of these code paths and is easy to break.
- Check a dark theme (`theme_grey(ink = "white", paper = "black")`) renders the
  calendar correctly, to confirm nothing hardcodes a colour.

## 8. Granule hierarchy: rows cut by the granules above them

Steps 1–7 grouped `block` and `pane` over rows that had already been cut by
`row` alone, so a row starting in January and ending in February belonged
whole to whichever pane it *started* in: February's first days finished off
January's last row, and the gap between the panes fell after them. `col` was
the only granule that actually cut a row short.

The hierarchy is now enforced for all of them — `col` > `pane` > `block` >
`row` > `cell` — by cutting the axis at every boundary at once rather than in
two passes:

- `calendar_pieces()` cuts `[col start, calendar_close())` at the union of the
  `col`, `pane`, `block` and `row` cuts. Each piece is therefore one row of one
  column, and carries the `row`-grid start it is a part of as its fold origin —
  so a piece cut short still draws its days under their own weekdays, with the
  rest of the row left blank.
- `transform()` makes a single `cut_connected()`/`cut_pointwise()` call against
  those pieces, instead of one pass for `col` and another for `row`. A piece
  index is all `.col`/`.loop` need, via lookup.
- Rows are counted *within a pane of a column*, and the grid gives every pane
  as many rows as the column that needs most (`.pane_rows`, `$add_pane_rows()`,
  `$piece_rows()`). Panes therefore line up across columns, which is what makes
  one gap separate the same two months everywhere; previously `.row_pane` was
  taken from a single panel's shared row grid and its gaps fell mid-month in
  every column but the first.
- Granule instances carry a `piece` rather than a `row`, since a piece's row
  is only known once every panel has been set up. `render_fg()`/`render_bg()`
  resolve it through `$piece_rows()`.
- Block rules are per column, since a block can start at a different row in
  each; pane gaps are shared, since panes are aligned.

Consequences worth remembering:

- `block` is no longer purely a display setting: it groups rows, so its rule
  lands exactly on the block boundary. It is deliberately left unvalidated
  (unlike `pane`) — a `block` equal to or finer than `row` still cuts sanely,
  and the existing "labels are formatted" test relies on `block = row`.
- `calendar_close()` stops the last row at the next boundary of any granule
  rather than at the end of the row grid, so the overhang past the data never
  reaches into a column or pane the calendar has no room for.
- The last column cut is never an inner cut: with `col` it is `close`, and
  without it, it is the end of the data rather than a boundary at all.
- Instances are kept where they *overlap* the calendar rather than where they
  start inside it, because a mixtime date axis puts its column boundaries
  mid-day (`align_discrete`), so the first cell of a column opens before it.

### Cutting on boundaries, not by duration

Reported while reviewing the above: on the zoned pedestrian axis the day
labels drifted a day after each daylight saving change (1 May 2015 drawn under
Saturday, two labels crowding 30 April), and weekly rows of that axis started
on a Thursday.

The cause was in `loop_cuts_by_duration()` rather than the calendar:
`seq()` steps by a fixed duration from the floored start, and a day is not
86400 seconds long across a daylight saving change, so every cut past one sat
at 23:00 the evening before — and was named for that evening.
`snap_cuts_to_granule()` now floors each stepped cut back onto the boundary it
stands for, and steps on where snapping leaves the range unclosed. It is taken
only when it leaves the cuts ordered, so a granule that does not floor cleanly
against an axis keeps the old drifting cuts rather than gaining unusable ones.

That also fixes the mis-floored quarters the `col` default carried a comment
about: `mixtime::months(3L)` and `mixtime::quarters(1L)` now cut the same
columns on a `Date`, `POSIXct` or zoned mixtime axis.

Residual, and left alone: a week containing a change really is 169 hours long,
and the row window takes the widest row, so cells in the other weeks of that
calendar sit up to an hour (0.6% of a row) off their gridline.

Also from the review: a `pane` label with no gap above it (the first pane of
the grid, or `pane_spacing = 0`) falls back into the row it opens. It is
justified within the whole row there, not within the pane's own first cell,
which would sit it on top of that cell's own label.
