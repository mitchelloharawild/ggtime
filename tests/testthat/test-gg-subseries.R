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
  expect_subseries_builds(subseries_tsbl(1950:2000))
})
