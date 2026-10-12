# ggtime (development version)

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
