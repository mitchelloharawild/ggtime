lag_tsbl <- function(index) {
  tsibble::tsibble(t = index, y = sin(seq_along(index)), index = t)
}

monthly_dates <- seq(as.Date("2018-01-01"), by = "month", length.out = 72)
quarterly_dates <- seq(as.Date("2018-01-01"), by = "quarter", length.out = 24)
weekly_dates <- seq(as.Date("2018-01-01"), by = "week", length.out = 156)
daily_dates <- seq(as.Date("2020-01-06"), by = "day", length.out = 140)
yearly_dates <- seq(as.Date("1950-01-01"), by = "year", length.out = 51)
hourly_times <- seq(
  as.POSIXct("2020-01-06", tz = "UTC"),
  by = "hour",
  length.out = 21 * 24
)

expect_lag_builds <- function(data, period = NULL) {
  p <- gg_lag(data, y, period = period)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplotGrob(p))
  invisible(p)
}

test_that("gg_lag() supports mixtime indices", {
  p <- expect_lag_builds(lag_tsbl(mixtime::yearmonth(monthly_dates)))
  expect_equal(levels(p$data$season), month.abb)

  p <- expect_lag_builds(lag_tsbl(mixtime::yearquarter(quarterly_dates)))
  expect_equal(levels(p$data$season), paste0("Q", 1:4))

  expect_lag_builds(lag_tsbl(mixtime::yearweek(weekly_dates)))

  p <- expect_lag_builds(lag_tsbl(mixtime::as_mixtime(daily_dates)))
  expect_equal(
    levels(p$data$season),
    c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  )

  p <- expect_lag_builds(lag_tsbl(mixtime::as_mixtime(hourly_times)))
  expect_length(levels(p$data$season), 24)

  # Non-seasonal data isn't coloured by season
  p <- expect_lag_builds(lag_tsbl(mixtime::year(yearly_dates)))
  expect_null(p$mapping$colour)
})

test_that("gg_lag() accepts a period for mixtime indices", {
  monthly <- lag_tsbl(mixtime::yearmonth(monthly_dates))
  for (period in list("1y", "year", mixtime::years(1L))) {
    p <- expect_lag_builds(monthly, period)
    expect_equal(levels(p$data$season), month.abb)
  }
  p <- expect_lag_builds(monthly, 12)
  expect_length(levels(p$data$season), 12)

  p <- expect_lag_builds(lag_tsbl(mixtime::as_mixtime(daily_dates)), "week")
  expect_length(levels(p$data$season), 7)
  p <- expect_lag_builds(lag_tsbl(mixtime::as_mixtime(hourly_times)), 24)
  expect_length(levels(p$data$season), 24)

  expect_error(gg_lag(monthly, y, 1), "at least one observation")
})

test_that("gg_lag() supports legacy indices", {
  p <- expect_lag_builds(lag_tsbl(tsibble::yearmonth(monthly_dates)))
  expect_equal(levels(p$data$season), month.abb)
  p <- expect_lag_builds(lag_tsbl(tsibble::yearquarter(quarterly_dates)))
  expect_equal(levels(p$data$season), paste0("Q", 1:4))
  expect_lag_builds(lag_tsbl(tsibble::yearweek(weekly_dates)))
  p <- expect_lag_builds(lag_tsbl(daily_dates))
  expect_length(levels(p$data$season), 7)
  p <- expect_lag_builds(lag_tsbl(hourly_times))
  expect_length(levels(p$data$season), 24)

  # Non-seasonal data isn't coloured by season
  p <- expect_lag_builds(lag_tsbl(1950:2000))
  expect_null(p$mapping$colour)
})

test_that("gg_lag() requires a fixed number of observations per period", {
  # Weeks per year vary, so weekly data isn't coloured by default
  p <- expect_lag_builds(lag_tsbl(mixtime::yearweek(weekly_dates)))
  expect_null(p$mapping$colour)
  p <- expect_lag_builds(lag_tsbl(tsibble::yearweek(weekly_dates)))
  expect_null(p$mapping$colour)

  expect_error(
    gg_lag(lag_tsbl(mixtime::yearweek(weekly_dates)), y, "year"),
    "fixed number of observations"
  )
  expect_error(
    gg_lag(lag_tsbl(daily_dates), y, "month"),
    "fixed number of observations"
  )
  expect_error(
    gg_lag(lag_tsbl(mixtime::as_mixtime(daily_dates)), y, "year"),
    "fixed number of observations"
  )
})

test_that("gg_lag() doesn't colour series without a calendar interval", {
  p <- expect_lag_builds(lag_tsbl(seq(1, 100)))
  expect_null(p$mapping$colour)

  irregular <- tsibble::tsibble(
    t = daily_dates[c(1, 3, 4, 8, 9, 15)],
    y = 1:6,
    index = t,
    regular = FALSE
  )
  p <- expect_lag_builds(irregular)
  expect_null(p$mapping$colour)
})

expect_tsdisplay_season <- function(data) {
  p <- gg_tsdisplay(data, y)
  expect_s3_class(p, "gg_tsensemble")
  # Seasonal data shows a season plot
  expect_s3_class(p[[3]]$coordinates, "CoordLoop")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_no_error(print(p))
  invisible(p)
}

test_that("gg_tsdisplay() supports mixtime and legacy indices", {
  skip_if_not_installed("feasts")
  expect_tsdisplay_season(lag_tsbl(mixtime::yearmonth(monthly_dates)))
  expect_tsdisplay_season(lag_tsbl(mixtime::yearquarter(quarterly_dates)))
  expect_tsdisplay_season(lag_tsbl(mixtime::yearweek(weekly_dates)))
  expect_tsdisplay_season(lag_tsbl(mixtime::as_mixtime(daily_dates)))
  expect_tsdisplay_season(lag_tsbl(mixtime::as_mixtime(hourly_times)))

  expect_tsdisplay_season(lag_tsbl(tsibble::yearmonth(monthly_dates)))
  expect_tsdisplay_season(lag_tsbl(tsibble::yearquarter(quarterly_dates)))
  expect_tsdisplay_season(lag_tsbl(tsibble::yearweek(weekly_dates)))
  expect_tsdisplay_season(lag_tsbl(daily_dates))
  expect_tsdisplay_season(lag_tsbl(hourly_times))

  # Non-seasonal data shows a spectrum rather than a season plot
  p <- gg_tsdisplay(lag_tsbl(mixtime::year(yearly_dates)), y)
  expect_false(inherits(p[[3]]$coordinates, "CoordLoop"))
  p <- gg_tsdisplay(lag_tsbl(1950:2000), y)
  expect_false(inherits(p[[3]]$coordinates, "CoordLoop"))
  p <- gg_tsdisplay(lag_tsbl(seq(1, 100)), y)
  expect_false(inherits(p[[3]]$coordinates, "CoordLoop"))
})
