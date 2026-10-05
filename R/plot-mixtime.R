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

# The default seasonal period for a granule interval: the largest common
# calendar period containing more than one observation (cf.
# `fabletools::get_frequencies(.auto = "largest")`).
default_period_granule <- function(interval) {
  for (granule in period_granules()[c(
    "year",
    "month",
    "week",
    "day",
    "hour",
    "minute"
  )]) {
    n <- observations_per_period(interval, granule)
    if (isTRUE(n > 1)) {
      return(granule)
    }
  }
  abort("Could not find a seasonal period to use for this time series.")
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

# A season identifier factor, ordered chronologically.
season_id <- function(x, period, start = mixtime::time_floor(x, period)) {
  levels <- sort(unique(start))
  factor(
    season_labels(start, period),
    levels = unique(season_labels(levels, period))
  )
}

# Resolve a `period` argument for a mixtime index, defaulting `NULL` to the
# default seasonal period (or `NULL` when `default = FALSE`).
season_period <- function(period, interval, arg = "period") {
  if (is.null(period)) {
    return(if (arg == "period") default_period_granule(interval) else NULL)
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
    return(interval_to_period(default_period_granule(interval)))
  }
  names(get_frequencies(NULL, data, .auto = "largest"))
}
