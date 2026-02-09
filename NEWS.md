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
