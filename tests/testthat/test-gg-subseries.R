subseries_tsbl <- function(index) {
  tsibble::tsibble(t = index, y = seq_along(index), index = t)
}

monthly_dates <- seq(as.Date("2018-01-01"), by = "month", length.out = 72)
quarterly_dates <- seq(as.Date("2018-01-01"), by = "quarter", length.out = 24)
weekly_dates <- seq(as.Date("2018-01-01"), by = "week", length.out = 156)
daily_dates <- seq(as.Date("2020-01-06"), by = "day", length.out = 140)
hourly_times <- seq(
  as.POSIXct("2020-01-06", tz = "UTC"),
  by = "hour",
  length.out = 21 * 24
)

expect_subseries_builds <- function(data, period = NULL) {
  p <- gg_subseries(data, y, period = period)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplotGrob(p))
  invisible(p)
}

subseries_axis_labels <- function(p) {
  x <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$x
  labels <- as.character(x$get_labels())
  labels[!is.na(labels)]
}

test_that("gg_subseries() supports mixtime indices", {
  p <- expect_subseries_builds(
    subseries_tsbl(mixtime::yearmonth(monthly_dates))
  )
  expect_equal(levels(p$data$id), month.abb)

  p <- expect_subseries_builds(
    subseries_tsbl(mixtime::yearquarter(quarterly_dates))
  )
  expect_equal(levels(p$data$id), paste0("Q", 1:4))

  expect_subseries_builds(subseries_tsbl(mixtime::yearweek(weekly_dates)))

  p <- expect_subseries_builds(
    subseries_tsbl(mixtime::as_mixtime(daily_dates)),
    "week"
  )
  expect_length(levels(p$data$id), 7)

  p <- expect_subseries_builds(
    subseries_tsbl(mixtime::as_mixtime(hourly_times)),
    "day"
  )
  expect_length(levels(p$data$id), 24)
})

test_that("gg_subseries() supports legacy indices", {
  expect_subseries_builds(subseries_tsbl(tsibble::yearmonth(monthly_dates)))
  expect_subseries_builds(
    subseries_tsbl(tsibble::yearquarter(quarterly_dates))
  )
  expect_subseries_builds(subseries_tsbl(tsibble::yearweek(weekly_dates)))
  expect_subseries_builds(subseries_tsbl(daily_dates), "week")
  expect_subseries_builds(subseries_tsbl(hourly_times), "day")
})

test_that("gg_subseries() labels legacy indices with times", {
  p <- expect_subseries_builds(
    subseries_tsbl(tsibble::yearmonth(monthly_dates))
  )
  expect_equal(levels(p$data$id), month.abb)
  expect_equal(
    subseries_axis_labels(p),
    c("2018", "2020", "2022")
  )

  p <- expect_subseries_builds(
    subseries_tsbl(tsibble::yearweek(weekly_dates))
  )
  expect_equal(
    subseries_axis_labels(p),
    c("2018", "2019", "2020")
  )
})

test_that("gg_subseries() defaults to the smallest seasonal period", {
  p <- expect_subseries_builds(subseries_tsbl(daily_dates))
  expect_length(levels(p$data$id), 7)

  p <- expect_subseries_builds(subseries_tsbl(hourly_times))
  expect_length(levels(p$data$id), 24)

  p <- expect_subseries_builds(
    subseries_tsbl(mixtime::as_mixtime(hourly_times))
  )
  expect_length(levels(p$data$id), 24)
})

test_that("gg_subseries() breaks the axis every few seasons", {
  p <- expect_subseries_builds(
    subseries_tsbl(mixtime::as_mixtime(hourly_times)),
    "day"
  )
  expect_lte(length(subseries_axis_labels(p)), 3)
  expect_equal(subseries_break_step(3), 1)
  expect_equal(subseries_break_step(21), 10)
  expect_equal(subseries_break_step(37), 20)
})

test_that("gg_subseries() errors for series without a seasonal period", {
  expect_error(
    gg_subseries(subseries_tsbl(1950:2000), y),
    "Could not find a seasonal period"
  )
})

test_that("gg_subseries() and gg_season() evaluate y with the original index", {
  skip_if_not_installed("lubridate")
  data <- subseries_tsbl(tsibble::yearmonth(monthly_dates))

  p <- gg_subseries(data, y / lubridate::days_in_month(t))
  expect_no_error(ggplot2::ggplotGrob(p))
  expect_equal(p$data$.y, data$y / lubridate::days_in_month(data$t))
  expect_equal(p$labels$y, "y/lubridate::days_in_month(t)")

  p <- gg_season(data, y / lubridate::days_in_month(t))
  expect_no_error(ggplot2::ggplotGrob(p))
  expect_equal(p$labels$y, "y/lubridate::days_in_month(t)")
})
