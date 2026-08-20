# Mixed-timezone `geom_time_line()`: the `cyc(day, month)` bug

Plotting combined multi-timezone data (e.g. London + Melbourne bind_rows()-ed
into one column, per [dev/tz.R](tz.R) and [dev/dst-geom.R](dst-geom.R)) is an
intended `ggtime` feature. It used to break in one specific, narrow way:

```r
library(ggplot2); library(dplyr); library(mixtime)

df <- bind_rows(
  London = tibble(time = datetime(as.POSIXct("2026-03-22", tz = "Europe/London") + (0:479) * 3600), value = 1:480),
  Melbourne = tibble(time = datetime(as.POSIXct("2026-03-22", tz = "Australia/Melbourne") + (0:479) * 3600), value = 1:480),
  .id = "city"
)

ggplot(df, aes(time, value, colour = city)) +
  geom_time_line() +
  scale_x_mixtime(
    time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA),
    time_breaks = mixtime::days(2L),
    time_labels = "{cyc(day, month)} {cyc(hour, day)}:00"
  )
#> Error: `object` must be an <S7_object>, not a <integer>
```

Fixed 2026-07-31 in `mixtime` (`R/chronon_common.R`). This note is the trail
that got there, kept for when something in this area breaks again.

## Symptom

- Only reproduces when **all** of these are true at once:
  1. the x column is built from more than one `datetime()`/vecvec piece
     (i.e. went through `bind_rows()`/`c()` on differing-timezone `mixtime`
     columns -- confirmed it doesn't even need genuinely different IANA
     zones, two same-zone chunks from different calendar dates reproduce it
     too, since GMT and BST are different UTC offsets);
  2. the scale has an explicit `time_breaks`;
  3. `time_labels` uses a cyclical granule **more than one level coarser**
     than the chronon (`cyc(day, month)` on an hourly chronon fails;
     `cyc(hour, day)`, one level up, never does).
- The error surfaces late, inside axis guide construction
  (`Guide$extract_key()` → `scale$get_labels()` → the `time_labels` closure →
  `format()`), long after `ggplot_build()`'s data stage has already
  succeeded -- so the rendered geometry (including the dashed DST-transition
  segments) was never wrong, only the axis labels.
- Two distinct error strings show up depending on exactly how the broken
  value is reached: `` `object` must be an <S7_object>, not a <integer> ``
  (from S7's `` `@.S7_object` ``/`check_is_S7()`, hit when `cyc()`'s glue
  parsing tries to use a granule object that isn't there) and
  `object 'month' not found` (when the missing granule is looked up as a bare
  symbol in the calendar data-mask instead).

## What it isn't

Ruled out empirically, each contradicted by a clean side-by-side test:

- **Not the inserted `NA` breaks** -- formatting the exact captured break
  vector with and without its `NA` entries failed identically.
- **Not the `tz = NA` wrapper class** -- `mixtime`'s naive-timezone sentinel
  (`'mt_naive' chr NA` vs a plain `chr NA`) differed between a working and a
  failing chronon at one point, but constructing a chronon with the wrapped
  sentinel directly (no combining involved) worked fine. Red herring.
- **Not session-level/global cache corruption** -- running the failing
  pipeline and then immediately re-running a standalone success case in the
  same R session didn't make the second one fail.
- **Not really about "two different timezones"** -- `bind_rows()`-ing two
  chunks that are *both* `Europe/London` (different calendar dates, so GMT
  vs BST) reproduces it identically. It's about the column having more than
  one underlying piece, not about the zones differing.
- **Not bare vs wrapped `mixtime`** -- reconstructing the broken value via
  `mixtime::mixtime()` (which always wraps in the outer vecvec container)
  instead of leaving it as a bare `mt_linear` made no difference either way.

## Root cause

`ScaleContinuousMixtime`'s scale transform
(`ggtime::transform_mixtime()`, `R/transform.R`) captures a `ptype` from the
*first* `mixtime` column it sees, via `vecvec::unvecvec(x)` followed by
`vctrs::vec_ptype(x)`. Every later restoration of bare numeric breaks back
into a `mixtime` (`to_mixtime()`'s `vctrs::vec_restore(vec_data(x), ptype)`,
and `time_breaks()`'s own `vctrs::vec_restore(breaks, x)`) rebuilds off that
captured `ptype`.

For **combined** data, `ptype`'s `chronon` attribute is itself the result of
merging the two pieces' (identical!) chronons through vctrs's ptype2
machinery. That merge goes through `mixtime`'s
`chronon_common_impl()` (`R/chronon_common.R`), which only special-cased
being handed a *single* chronon (`vec_size(chronons) == 1L`) -- being handed
the *same* chronon twice (once per bind_rows() piece) fell through to the
general case: a graph search for the greatest-lower-bound chronon
(`S7_graph_glb()` over `chronon_cardinality_graph()`), followed by
`granule_inherit_shared_props()` reconstructing an instance from the bare
class the graph returned.

That reconstructed chronon is `identical()` to the original on class, `n`
and `tz` -- confirmed directly, byte for byte -- but resolves a **different,
truncated calendar**. Minimal proof:

```r
chr <- mixtime::cal_gregorian$hour(1L, tz = NA)
piece1 <- mixtime::mixtime(1:5, chronon = chr, discrete = FALSE)@x[[1]]
piece2 <- mixtime::mixtime(6:10, chronon = chr, discrete = FALSE)@x[[1]]
identical(attr(piece1, "chronon"), attr(piece2, "chronon"))  # TRUE -- same chronon

combined_ptype <- vctrs::vec_ptype2(piece1, piece2)
names(mixtime::time_calendar(attr(combined_ptype, "chronon")))
#> "day" "ampm" "hour" "minute" "second" "millisecond"
# vs. the original chronon's own calendar:
names(mixtime::time_calendar(chr))
#> "year" "quarter" "month" "day" "ampm" "hour" "minute" "second" "millisecond"
```

`year`/`quarter`/`month` are gone from the merged chronon's calendar --
exactly why `cyc(day, month)` (needs `month`) fails while `cyc(hour, day)`
(only needs `day`, still present) doesn't. `time_calendar()` for an
`mt_unit` resolves via `attr(S7::S7_class(x), "cal")$calendar`
(`R/01_calendar.R`) -- a lookup keyed off the *class* the reconstructed
instance ends up with, not off `n`/`tz` equality, so `identical()` on the
usual visible attributes doesn't catch the difference.

## Fix

`chronon_common_impl()` (`mixtime`, `R/chronon_common.R`) now deduplicates
the input chronons up front, generalising the size-1 short-circuit that was
already there instead of adding a separate one:

```r
chronons <- unique(chronons)
if (vec_size(chronons) == 1L) return(chronons[[1L]])
```

`unique()` on a list of chronons works because they're ordinary R objects
under `identical()`-based comparison -- no custom equality method needed.
Deduplicating first, rather than checking "are all of these identical to
the first", also means N-way combinations with some but not all duplicates
(e.g. three pieces, two sharing a chronon) still shrink the search space
before the graph runs, rather than only ever short-circuiting the
all-identical case. Genuinely mixed-granularity combination (e.g. day +
month, still > 1 element after dedup) goes through the graph search exactly
as before.

No change was needed in `ggtime` itself -- `R/transform.R`'s
`vctrs::vec_restore()` calls work correctly once the `ptype` they restore
from carries an uncorrupted chronon. (A `ggtime`-side workaround was tried
first -- rebuilding via `mixtime::mixtime()` instead of `vec_restore()` in
`to_mixtime()` -- but that reconstructs from the *same* corrupted
`attr(ptype, "chronon")`, so it didn't help; the bug has to be fixed where
the chronon actually gets corrupted.)

## Verification

- `dev/example-london-gaza.R` builds the repro above as a single
  `bind_rows()`-ed column, one `geom_time_line()` layer, `time_breaks` +
  `cyc(day, month)` labels -- previously required two separate
  `geom_time_line(data = ...)` layers (one per city) to dodge the bug.
- `mixtime`: `devtools::test()` -- same single pre-existing failure before
  and after this change (`test-timezones.R:224`, an unrelated `mt_naive` vs
  `character` tz-wrapper mismatch left over from the same-day "Fix tz length
  for POSIXt tz_offset" commit -- not touched here).
- `ggtime`: `devtools::test()` -- passes unchanged.

## If this breaks again

Re-run the "minimal proof" snippet above first -- it isolates the bug
without ggplot2, a scale, or `geom_time_line()` in the loop at all. If
`vec_ptype2()` on two identical chronons drops calendar granules again,
the regression is in `chronon_common_impl()`'s short-circuit (or something
that bypasses it), not in `ggtime`.
