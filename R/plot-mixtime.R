# Helpers for plotting tsibbles with a mixtime index (and/or a mixtime granule
# interval). These operate on mixtime granules and vectors directly, rather than
# going through lubridate.

is_granule <- function(x) {
  S7::S7_inherits(x, mixtime::mt_unit)
}

# Granules for the calendar units that can be named in a `period` string.
period_granules <- function() {
  list(
    year = mixtime::cal_gregorian$year(1L),
    quarter = mixtime::cal_gregorian$quarter(1L),
    month = mixtime::cal_gregorian$month(1L),
    week = mixtime::cal_isoweek$week(1L),
    day = mixtime::cal_gregorian$day(1L),
    hour = mixtime::cal_gregorian$hour(1L),
    minute = mixtime::cal_gregorian$minute(1L),
    second = mixtime::cal_gregorian$second(1L)
  )
}

scale_granule <- function(granule, n) {
  n <- granule@n * n
  granule@n <- if (is.integer(granule@n)) as.integer(n) else n
  granule
}

# The time zone of a tsibble's interval is not carried by the named period
# granules, so carry it across from the data.
as_period_granule <- function(period, interval, arg = "period") {
  if (is_granule(period)) {
    return(period)
  }
  if (mixtime::is_mixtime(period) && mixtime::time_is_duration(period)) {
    duration <- vecvec::unvecvec(period)
    return(scale_granule(duration@chronon, as.numeric(vctrs::vec_data(duration))))
  }
  if (is.numeric(period)) {
    # Multiples of the data's interval
    return(scale_granule(interval, period))
  }
  if (is.character(period)) {
    period <- lubridate::as.period(period)
  }
  if (lubridate::is.period(period)) {
    fields <- c(
      year = period@year,
      month = period@month,
      day = period@day,
      hour = period@hour,
      minute = period@minute,
      second = period@.Data
    )
    fields <- fields[fields != 0]
    granules <- period_granules()
    if (length(fields) == 1) {
      unit <- names(fields)
      # Whole weeks are specified in days by lubridate
      if (unit == "day" && fields %% 7 == 0) {
        return(scale_granule(granules$week, fields / 7))
      }
      return(scale_granule(granules[[unit]], unname(fields)))
    }
  }
  abort(
    paste0(
      "`",
      arg,
      "` must be a number of observations, a period such as \"year\" or ",
      "\"2 weeks\", or a mixtime granule or duration of a single unit."
    )
  )
}

# Number of observations (of the interval) in one period, or `NA` when that
# isn't constant (e.g. weeks in a year)
observations_per_period <- function(interval, period) {
  tryCatch(
    mixtime::chronon_cardinality(interval, period),
    error = function(e) NA_real_
  )
}

# Approximate number of observations per period for calendar units of
# varying length (e.g. days in a year), used only to choose default periods.
nominal_observations_per_period <- function(interval, period) {
  seconds <- c(
    second = 1,
    minute = 60,
    hour = 3600,
    day = 86400,
    week = 604800,
    month = 2629746,
    quarter = 7889238,
    year = 31556952
  )
  nominal <- function(x) seconds[[granule_unit(x)]] * x@n
  nominal(period) / nominal(interval)
}

# The default seasonal period for a granule interval: the largest (or
# smallest) common calendar period containing more than one observation (cf.
# `fabletools::get_frequencies(.auto = )`).
default_period_granule <- function(interval, auto = c("largest", "smallest")) {
  auto <- match.arg(auto)
  units <- c("year", "month", "week", "day", "hour", "minute")
  if (auto == "smallest") {
    # Months vary in length, so aren't a natural smallest period
    units <- rev(setdiff(units, "month"))
  }
  for (granule in period_granules()[units]) {
    n <- observations_per_period(interval, granule)
    if (is.na(n)) {
      n <- nominal_observations_per_period(interval, granule)
    }
    if (isTRUE(n > 1)) {
      return(granule)
    }
  }
  NULL
}

# The label of a season, named for the period granule it spans ("2020",
# "2020 Q1", "2020-01-06").
season_labels <- function(start, period) {
  format(mixtime::mixtime(start, chronon = period), attr = FALSE)
}

# Positions each time within its season, as a linear time on a common origin
# so that seasons overlay one another. Labels for the positions are given by
# `season_position_labels()`.
season_position <- function(x, period, start = mixtime::time_floor(x, period)) {
  origin <- min(start, na.rm = TRUE)
  origin + (x - start)
}

season_position_labels <- function(period) {
  force(period)
  function(x) time_labels_at(x, cycle = period)
}

# A factor of each time's position within its season ("Jan", "Feb", ...),
# in seasonal order.
season_position_factor <- function(x, period) {
  position <- season_position(x, period)
  levels <- sort(unique(position))
  factor(
    time_labels_at(position, cycle = period),
    levels = unique(time_labels_at(levels, cycle = period))
  )
}

# A season identifier factor, ordered chronologically.
season_id <- function(x, period, start = mixtime::time_floor(x, period)) {
  levels <- sort(unique(start))
  factor(
    season_labels(start, period),
    levels = unique(season_labels(levels, period))
  )
}

# Resolve a `period` argument for a granule interval into a granule. `NULL`
# defaults to the `auto` seasonal period (and stays `NULL` for other arguments
# such as `facet_period`).
season_period <- function(
  period,
  interval,
  arg = "period",
  auto = "largest"
) {
  if (is.null(period)) {
    if (arg != "period") {
      return(NULL)
    }
    period <- default_period_granule(interval, auto)
    if (is.null(period)) {
      abort("Could not find a seasonal period to use for this time series.")
    }
    return(period)
  }
  period <- as_period_granule(period, interval, arg)
  n <- observations_per_period(interval, period)
  if (isTRUE(n <= 1)) {
    abort("The data must contain at least one observation per seasonal period.")
  }
  period
}

# Frequencies for legacy time classes
legacy_default_period <- function(data) {
  interval <- interval(data)
  if (is_granule(interval)) {
    return(interval_to_period(season_period(NULL, interval)))
  }
  names(get_frequencies(NULL, data, .auto = "largest"))
}

# Whether a granule interval has any common seasonal period (cf.
# `any(fabletools::get_frequencies(.auto = "all") > 1)`)
interval_is_seasonal <- function(interval) {
  !is.null(default_period_granule(interval))
}

# Format an interval (a mixtime granule or list of granules, or a legacy
# tsibble interval) in tsibble's compact style, e.g. "1M" or "1h 30m".
format_interval <- function(x) {
  if (is_granule(x) || (is.list(x) && !inherits(x, "interval"))) {
    tsibble::format_interval(x)
  } else {
    format(x)
  }
}

# Convert the lags of an ACF/PACF/CCF (a `cf_lag` holding a count of lags and
# the interval they're a multiple of) to a mixtime duration, e.g. 12 lags of a
# monthly interval become "12 months". Lags without a regular interval (an
# irregular series is lagged by observation) are returned as plain numbers.
lag_to_duration <- function(lag, interval = attr(lag, "interval")) {
  lags <- as.numeric(vctrs::vec_data(lag))
  if (is.list(interval) && !inherits(interval, "interval")) {
    # A composite interval has no single unit to count lags in
    interval <- if (length(interval) == 1) interval[[1]] else NULL
  } else if (inherits(interval, "interval")) {
    # Legacy tsibble interval
    interval <- tryCatch(
      as_period_granule(interval_to_period(interval), NULL),
      error = function(e) NULL
    )
  }
  if (!is_granule(interval) || is.na(interval@n)) {
    return(lags)
  }
  mixtime::duration(lags, interval)
}
