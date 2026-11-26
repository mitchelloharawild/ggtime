# ggtime 1.2.0

## New features

* Added `stat_summary_time()`, a variation of `ggplot2::stat_summary()` which
  summarises values within time granules. Implicit missing values are filled
  with `NA` so that incomplete granules aren't mistaken for complete ones.

* Added `guide_axis_mixtime()`, the new default axis guide for `scale_x_mixtime()`.
  Discrete time points span a granule rather than sit at an instant, so the axis
  now places ticks at the granule boundaries with the label between them, much
  like a discrete axis in {ggplot2}. Every granule is ticked when only some are
  labelled. Continuous time and durations are unaffected,
  and `guide = guide_axis()` restores the previous appearance.

* The default breaks of discrete time are labelled by the granule they step by
  when this is simpler (e.g. `2020` and `2022` rather than `2020 Jan` and
  `2022 Jan`).

* `scale_x_mixtime(time_chronon = )` and `scale_y_mixtime(time_chronon = )` now
  accept units of the plotted data's calendar, such as `hour(1L, tz = NA)` rather
  than `mixtime::cal_gregorian$hour(1L, tz = NA)`.

## Bug fixes

* `coord_loop()` now sizes its loops after time is transformed, so loops warped
  to the same length with `transform_warp()` fill the panel (and complete the
  circle of `coord_radial()`).

* `coord_loop(loops = )` now labels `scale_x_mixtime()` axes with the time since
  the start of the loop, or the proportion of the loop if it is warped with
  `transform_warp()`. The axis was previously labelled with the time of the first
  loop.

* `gg_lag()`, `gg_arma()`, `gg_acf()` and friends, and `autoplot()` of
  multiple variables no longer use the retired `tidyr::gather()`, which
  stripped the vecvec base vector of mixtime columns ("`x` is not a vecvec
  base vector"). They now use `tidyr::pivot_longer()`.

* `geom_time_line()` now draws correct slopes when an observation is at the
  exact instant of a time zone transition, or when `align_discrete` places it
  across one.

* `geom_time_line()` now identifies missing and duplicated time values
  separately for each granularity, and no longer errors when granularities
  without a fixed ratio (e.g. months and days) are used together. All discrete
  mixtime granularities are regular, including those finer than a day.

# ggtime 1.1.1

## Improvements

* The default breaks of mixtime scales now follow the calendar structure of
  the time being plotted, stepping by how its chronon nests within coarser
  granules (e.g. every 3 or 6 months within a year, every 7 days within a week)
  and aligning time points to them (each January, each Monday), rather than
  round numbers of the chronon (#37). Durations keep their unit in breaks and
  labels.

* `gg_season()` places its labels outside the seasonal lines, slightly separated
  from them.

* `autoplot()` of ACF/PACF/CCF results labels lags with plain numbers, as their
  unit is already in the axis title (#37).

## Bug fixes

* Time point `breaks` coarser than the data (e.g. `yearmonth()` breaks on a
  daily axis) are now placed at the start of each time point, for compatibility
  with mixtime refusing temporally indeterminate conversions of discrete time.

* `gg_season()` and `gg_subseries()` evaluate `y` before converting the index,
  so `y` can use the original index.

* `coord_loop()` no longer warns about integer overflow when comparing the
  cardinality of looped time.

# ggtime 1.1.0

## New features

* `geom_candlestick()`/`stat_ohlc()`: summarises the values within each
  time granule by their first (`open`), largest (`high`), smallest (`low`), and
  last (`close`) value, drawn as a candlestick chart. Rising and falling
  candles are coloured with `scale_colour_candlestick()` and
  `scale_fill_candlestick()`.

* `coord_calendar()` gains `cell_ratio`, which fixes the aspect ratio of a
  calendar cell as `coord_fixed()` fixes that of the data units of the axes.
  `cell_ratio = 1` gives the square cells of a printed calendar (#33).

## Improvements

* The plot helpers (`gg_season()`, `gg_subseries()`, `gg_lag()`,
  `gg_tsdisplay()`, and `autoplot()` of ACF/PACF/CCF results) are now all
  built from mixtime. Legacy `Date`, `POSIXct`, and tsibble
  `yearweek()`/`yearmonth()`/`yearquarter()` indices are converted to mixtime
  for plotting, so existing code continues to work. Labels for cyclical displays
  of time (e.g. time-of-day) have been improved as a result.

* `gg_season()` is now drawn with `coord_loop()`, keeping time continuous
  rather than offsetting it into a single season. The lines connecting
  consecutive seasons (e.g. December to January) are now drawn, split across
  the left and right margins of the plot.

* The `period` of `gg_season()`, `gg_subseries()`, and `gg_lag()` can now be
  a mixtime duration (e.g. `mixtime::weeks(2L)`) or a string of a whole number
  of a single unit (e.g. `"year"` or `"2 weeks"`), and is no longer converted
  to a lubridate period.

* `gg_subseries()` now facets by the smallest seasonal period of the data by
  default (e.g. a week for daily data, a day for hourly data).

* `gg_lag()` now only colours by season for periods with a fixed number of
  observations, and gives an informative error if the requested `period`
  doesn't (e.g. days in a month).

* `autoplot()` of ACF/PACF/CCF results now labels lags as durations of time
  (e.g. "12 months") rather than a count of observations.

* Looping `coord_radial()` with `coord_loop()` no longer labels the end of the
  loop with its start (e.g. "Jan/Jan").

* `gg_season()` and related plots now give a clear error for zero-row data
  instead of failing with "object 'found_format' not found".

* `gg_season()` now includes lines connecting observations across seasonalities
  (e.g. from December of one year to January of the next).

* ggtime no longer imports lubridate.

# ggtime 1.0.0

This release introduces the grammar of temporal graphics: a set of time-aware
geoms, scales, and coordinate systems that extend ggplot2 to work with mixtime
time vectors. These grammar elements sit alongside the plot helper functions 
from previous releases, and are what the helpers are now built from.

## New features

* `geom_time_line()`: a time-aware extension of `geom_line()` that keeps a
  line's slope an accurate reflection of the rate of change, even across
  timezone changes, gaps, and duplicated time points.
* `scale_x_mixtime()`/`scale_y_mixtime()`: the position scale behind every
  mixtime time axis, applied automatically whenever a `mixtime` vector is
  mapped to a plot. It maps time points of different granularities onto one
  shared axis, and takes calendrical durations for breaks (`time_breaks`)
  and calendar-aware format strings for labels (`time_labels`).
* `scale_colour_mixtime()`/`scale_fill_mixtime()`/`scale_alpha_mixtime()`/
  `scale_size_mixtime()`/`scale_linewidth_mixtime()`: mixtime-aware
  equivalents for other aesthetics.
* `coord_loop()`: loops the time axis around a calendrical period, so a
  continuous time axis can be compared cyclically instead of being
  discretised into a seasonal factor.
* `coord_calendar()`: arranges time into a calendar-like grid of rows and
  columns, useful for visualising events over short intervals within a long
  time span, such as holidays.
* `aes_nudge()`: specifying alignment of discrete time poisitions on continuous
  time plot axis with sensible defaults.
* `transform_warp()`: a scales transform for warping time.

# ggtime 0.2.0

This release completes the migration of graphics functions from {feasts} and
{fabletools}, including a deprecation process for a gradual migration to 
{ggtime}. If you previously used time-series graphics from {feasts} or {fable},
you should now include `library(ggtime)` in your script to avoid deprecation
notices. Existing code will continue to work with a deprecation warning.

The following plot helper functions from {fabletools} are included in this release:

* `autoplot(<fbl_ts>)`: Forecast plots to show forecast intervals with historical data.
* `autolayer(<fbl_ts>)`: Forecast layers for fable objects.
* `autoplot(<dcmp_ts>)`: Decomposition plots to show components of a dable object.
* `fortify(<fbl_ts>)`: Fortify method for converting fable objects into basic data frames.

# ggtime 0.1.0

Initial release including all plot helper functions from across the tidy time
series analysis packages. The following plot helper functions are included in
this release:

* `autoplot(<tbl_ts>)`: Time plots to show overall patterns in tsibble objects.
* `autolayer(<tbl_ts>)`: Time plot layers for tsibble objects.
* `gg_season()`: Seasonal plots to show the shape of seasonal patterns.
* `gg_subseries()`: Seasonal sub-series plots to show seasonal changes over time.
* `gg_lag()`: Lag plots to show relationships between now and the past.
* `gg_irf()`: Impulse response function plots to be used with `IRF()` results.
* `gg_arma()`: Plot the characteristic ARMA roots.
* `gg_tsdisplay()`: An ensemble graphic useful in exploring time series data.
* `gg_tsresiduals()`: An ensemble graphic useful in diagnosing model residuals.
