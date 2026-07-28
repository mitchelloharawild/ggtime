scale_labels <- function(p, aes = "x") {
  params <- ggplot_build(p)$layout$panel_params[[1]][[aes]]
  labels <- params$get_labels()
  labels[!is.na(labels)]
}

test_that("durations are labelled as durations", {
  df <- data.frame(x = mixtime::days(1:10), y = 1:10)
  p <- ggplot(df, aes(x, y)) + geom_line()

  # Not "1970-01-03", which measures the duration from the epoch as if it were
  # a time point.
  expect_match(scale_labels(p), "days$")
})

test_that("mixed granularity durations share the finest chronon", {
  df <- data.frame(
    x = c(mixtime::days(1:5), mixtime::hours(c(140, 160))),
    y = 1:7
  )
  p <- ggplot(df, aes(x, y)) + geom_point()

  expect_match(scale_labels(p), "hours$")
})

test_that("durations can't be scaled alongside other modes of time", {
  df <- data.frame(
    x = c(mixtime::days(1:3), mixtime::date("2021-01-01")),
    y = 1:4
  )
  p <- ggplot(df, aes(x, y)) + geom_point()

  expect_error(ggplot_build(p), "alongside other modes of time")
})

test_that("time points are unaffected by duration handling", {
  df <- data.frame(x = mixtime::yearmonth(600:611), y = 1:12)
  p <- ggplot(df, aes(x, y)) + geom_line()

  expect_match(scale_labels(p), "^20[0-9]{2} [A-Z][a-z]{2}")
})

test_that("break widths must be a single duration", {
  # `fullseq()` steps by the width, and `seq()` would silently use only the
  # first of several.
  expect_error(
    scale_x_mixtime(time_breaks = c(mixtime::months(1L), mixtime::years(1L))),
    "`time_breaks` must be a single duration"
  )
  expect_error(
    scale_x_mixtime(time_minor_breaks = mixtime::months(1:2)),
    "`time_minor_breaks` must be a single duration"
  )
  expect_no_error(scale_x_mixtime(time_breaks = mixtime::months(1L)))

  # A time point says when, not how long.
  expect_error(
    scale_x_mixtime(time_breaks = mixtime::yearmonth(600L)),
    "`time_breaks` must be a duration, not a time point"
  )
})

test_that("break widths reject strings", {
  # Parsing a string like "1 year" into a duration would need mixtime's own
  # (unexported) parser. For now, only durations and granules are accepted.
  expect_error(
    scale_x_mixtime(time_breaks = "1 year"),
    "`time_breaks` must be a duration or a granule"
  )
})

test_that("break widths can be given as a granule directly", {
  df <- data.frame(x = mixtime::yearmonth(600:635), y = 1:36)
  p <- ggplot(df, aes(x, y)) + geom_line()

  # `mixtime::years()` and friends keep the duration inside a vecvec wrapper,
  # which `seq()` won't step by. Reducing it to the granule it steps by gives
  # the same breaks as passing that granule directly.
  expect_equal(
    scale_labels(p + scale_x_mixtime(time_breaks = mixtime::years(1L))),
    scale_labels(p + scale_x_mixtime(time_breaks = mixtime::cal_gregorian$year(1L)))
  )
  # A duration of more than one unit steps by all of it, rather than by one.
  expect_equal(
    scale_labels(p + scale_x_mixtime(time_breaks = mixtime::years(2L))),
    scale_labels(p + scale_x_mixtime(time_breaks = mixtime::cal_gregorian$year(2L)))
  )
})

test_that("time_breaks steps through the limits by a duration", {
  # `breaks_time_seq()` walks the limits with mixtime's calendrical arithmetic,
  # since `scales::fullseq()` has no method for the `<mt_time>` that a mixtime
  # scale's limits arrive as.
  df <- data.frame(x = mixtime::date("2020-01-06") + 0:60, y = 1:61)
  p <- ggplot(df, aes(x, y)) +
    geom_line() +
    scale_x_mixtime(time_breaks = mixtime::weeks(1L))

  breaks <- ggplot_build(p)$layout$panel_params[[1]]$x$get_breaks()
  breaks <- breaks[!is.na(breaks)]
  expect_gt(length(breaks), 1L)

  # Weekly breaks land on Mondays rather than at a fixed stride from the limits.
  expect_equal(unique(diff(breaks)), 7)
  expect_equal(
    unique(weekdays(as.Date(breaks, origin = "1970-01-01"))),
    weekdays(as.Date("2020-01-06"))
  )
})

test_that("time_labels formats breaks with a mixtime format string", {
  # The weekday of a Gregorian date is a day within an ISO week, so labelling it
  # crosses calendars -- which is the point of mixtime's format strings.
  weekday <- "{cyc(day, cal_isoweek$week, label = TRUE, abbreviate = TRUE)}"
  df <- data.frame(x = mixtime::date("2020-01-06") + 0:6, y = 1:7)
  p <- ggplot(df, aes(x, y)) +
    geom_line() +
    scale_x_mixtime(
      time_breaks = mixtime::days(1L),
      time_labels = weekday,
      align_discrete = 0
    )

  expect_setequal(
    setdiff(scale_labels(p), "NA"),
    weekdays(as.Date("2020-01-06") + 0:6, TRUE)
  )
})

test_that("default labels drop the within-chronon position when breaks are whole", {
  # Daily breaks land at midnight, so every break's within-day position is
  # 0.0% -- format that as a plain date rather than "2024-01-08 0.0%".
  df <- data.frame(x = mixtime::date("2024-01-01") + 0:20, y = 1:21)
  p <- ggplot(df, aes(x, y)) + geom_line() + scale_x_mixtime()

  labels <- setdiff(scale_labels(p), "NA")
  expect_true(length(labels) > 0)
  expect_false(any(grepl("%", labels, fixed = TRUE)))
  expect_match(labels, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
})

test_that("fractional within-chronon positions are still shown", {
  # A warp can place breaks partway through a chronon; that fractional
  # position should still be reported rather than always dropped.
  month_starts <- mixtime::yearmonth("2020-12-01") + 0:5
  df <- data.frame(x = mixtime::date("2021-01-01") + 0:89, y = 1:90)
  p <- ggplot(df, aes(x, y)) +
    geom_line() +
    scale_x_mixtime(
      breaks = mixtime::date("2021-01-15") + c(0, 30, 60),
      transform = transform_warp(month_starts + 0)
    )

  labels <- setdiff(scale_labels(p), "NA")
  expect_true(length(labels) > 0)
  expect_true(all(grepl("%", labels, fixed = TRUE)))
})

test_that("time_labels formats timezone-aware breaks", {
  tz <- "Australia/Melbourne"
  df <- data.frame(
    x = mixtime::datetime(seq(
      as.POSIXct("2020-01-06 00:00:00", tz = tz),
      by = "1 day",
      length.out = 7
    )),
    y = 1:7
  )
  p <- ggplot(df, aes(x, y)) +
    geom_line() +
    scale_x_mixtime(
      time_breaks = mixtime::days(1L),
      time_labels = "{cyc(day, month)}",
      align_discrete = 0
    )

  expect_setequal(setdiff(scale_labels(p), "NA"), sprintf("%02d", 6:12))
})
