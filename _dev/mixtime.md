# mixtime notes from ggtime

Defects and rough edges in `mixtime` hit while working on `ggtime`, each reduced
to a `mixtime` only reproduction and re-run against the working tree. Where
`ggtime` works around one, the workaround is named so it can be removed when the
bug goes.

---

## 1. Multi-week chronons cannot be converted

A chronon of more than one week errors on conversion, though `seq()` steps by
the same granule quite happily.

```r
library(mixtime)
mixtime(date("2020-01-06"), discrete = TRUE, chronon = cal_isoweek$week(2L))
#> Error: Converting from multi-week chronons to years is not yet supported

mixtime(date("2020-01-06"), discrete = TRUE, chronon = cal_isoweek$week(1L))
#> 2020 W02
seq(vecvec::unvecvec(date("2020-01-06")), vecvec::unvecvec(date("2020-03-06")),
    by = cal_isoweek$week(2L))
#> 2020-01-06, 2020-01-20, 2020-02-03, ...
```

The error is raised when the result is formatted, so the vector is built and
only naming it fails.

**Where it shows in ggtime.** `scale_x_mixtime(time_breaks = mixtime::weeks(2L))`
errors when its labels are formatted, on data of any granularity. The breaks
themselves are generated fine.

---

## 2. `chronon_common()` returns a granule with a truncated calendar

The granule `chronon_common()` returns is rebuilt by its own search rather than
taken from its inputs, and carries only the part of the calendar that search
walked through. It is of the same class as the input granule but is not
interchangeable with it.

```r
library(mixtime)
common <- chronon_common(c(
  linear_time(0L, chronon = cal_gregorian$second(1L)),
  linear_time(0L, chronon = cal_gregorian$day(1L))
))
class(common)[1]  #> "mixtime::tu_second", as expected

names(time_calendar(linear_time(0L, chronon = common)))
#> day, ampm, hour, minute, second, millisecond, microsecond, nanosecond

names(time_calendar(linear_time(0L, chronon = cal_gregorian$second(1L))))
#> year, quarter, month, day, ampm, hour, minute, second, ...
```

The common chronon reaches no granule coarser than the one its search started
at, so `year`, `quarter` and `month` are missing from a calendar that should
have them.

**Where it shows in ggtime.** Using the returned granule as a scale's chronon
made `time_labels = "{cyc(day, month)}"` fail with "`object` must be an
<S7_object>, not a <integer>", because `month` was not on the calendar the
format string was resolved against. No current ggtime code path hits this.

---

## 3. `mixtime()` silently misreads a bare `<mt_time>`

Not a `mixtime` bug — `mixtime()` is documented to take data, and a bare
`<mt_time>` is not a `<mixtime>` — but it is a sharp edge worth blunting,
because the failure is silent and granularity dependent.

`is.numeric()` is `TRUE` for a bare `<mt_time>`, so `mixtime()` takes one for
raw numbers and offsets it by the target chronon's epoch:

```r
library(mixtime)
bare <- vecvec::unvecvec(year(2020L))     # <mt_linear>, holding 50
chronon_epoch(cal_gregorian$year(1L))     #> 1970

mixtime(bare, chronon = cal_gregorian$year(1L))              #> 51    (wrong)
mixtime(new_mixtime(bare), chronon = cal_gregorian$year(1L)) #> 2020  (right)
```

Only chronons with a non-zero epoch are affected — `day`, `month`, `second` and
`quarter` are all zero — so a conversion can be silently wrong on a yearly axis
while every other granularity is fine.

**Where it showed in ggtime.** Every `mixtime()` call was passing the bare
vector it had just unwrapped, so yearly data was drawn at -1920 and labelled 51,
and `time_breaks = mixtime::years(1L)` labelled its breaks 1855, 1856. Fixed by
`wrap_mixtime()` (`R/utils.R`), which re-wraps a bare `<mt_time>` with
`new_mixtime()` before handing it over.

**Possible mixtime change.** `mixtime()` could refuse a bare `<mt_time>`, or
treat one as time rather than as numbers — the `is.numeric()` branch is only
meant for genuinely bare numbers.

---

## Behaviour relied on (not bugs, but worth not losing)

* **A naive granule does not strip a zoned chronon.** Converting zoned data onto
  a naive granule (`cal_gregorian$day(1L)`, which names no time zone) keeps the
  data's zone rather than dropping to UTC:

  ```r
  dt <- datetime(as.POSIXct("2020-01-06", tz = "Australia/Melbourne"))
  attr(vecvec::unvecvec(mixtime(dt, chronon = cal_gregorian$day(1L), discrete = FALSE)), "chronon")
  #> <mixtime::tu_day> @ tz: "Australia/Melbourne"
  ```

  `ggtime` converts breaks onto granules taken from durations, which are always
  naive, and depends on this to keep a zoned axis's breaks where they belong.

* **`chronon_cardinality()` needs an `at` for variable pairs** (days in a month),
  while **`chronon_divmod()` does not** — it traverses only fixed cardinalities.
  Worth remembering if ggtime ever needs to order two granules again; nothing
  does at present.
