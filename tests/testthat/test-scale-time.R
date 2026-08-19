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
    scale_labels(
      p + scale_x_mixtime(time_breaks = mixtime::cal_gregorian$year(1L))
    )
  )
  # A duration of more than one unit steps by all of it, rather than by one.
  expect_equal(
    scale_labels(p + scale_x_mixtime(time_breaks = mixtime::years(2L))),
    scale_labels(
      p + scale_x_mixtime(time_breaks = mixtime::cal_gregorian$year(2L))
    )
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

test_that("default labels leave the axis's timezone out", {
  # Every break of an axis shares its timezone, so naming it on each label
  # repeats what the axis as a whole says once.
  tz <- "Australia/Melbourne"
  df <- data.frame(
    x = mixtime::datetime(seq(
      as.POSIXct("2020-01-06 00:00:00", tz = tz),
      by = "1 hour",
      length.out = 72
    )),
    y = 1:72
  )
  p <- ggplot(df, aes(x, y)) + geom_line()

  # Breaks labelled at their own (second) chronon, ...
  expect_match(
    scale_labels(p),
    "^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$"
  )
  # ... and breaks labelled at the granule they step by.
  expect_match(
    scale_labels(p + scale_x_mixtime(time_breaks = mixtime::days(1L))),
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
  )

  # `time_labels` can still ask for it.
  expect_match(
    setdiff(
      scale_labels(
        p + scale_x_mixtime(time_labels = "{cyc(day, month)} {tz(.time)}")
      ),
      "NA"
    ),
    "^[0-9]{2} AEDT$"
  )
})

test_that("time_breaks labels name the granule the breaks step by", {
  df <- data.frame(
    x = mixtime::datetime("2015-01-01 00:00:00") + 3600 * (0:200),
    y = 1:201
  )
  p <- ggplot(df, aes(x, y)) + geom_line()

  # A break stepped by a granule lands at the start of one of that granule's
  # instances, so the label names the instance ("2015-01-01") rather than the
  # instant it sits at in the scale's own chronon ("2015-01-01 00:00:00").
  expect_match(
    scale_labels(p + scale_x_mixtime(time_breaks = mixtime::days(1L))),
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
  )
  expect_match(
    scale_labels(p + scale_x_mixtime(time_breaks = mixtime::months(1L))),
    "^[0-9]{4} [A-Z][a-z]{2}$"
  )
  # Breaks the scale did not step by a granule are labelled as before.
  expect_match(scale_labels(p), "^[0-9]{4}-[0-9]{2}-[0-9]{2} ")

  # Labels the user asked for are not overridden.
  expect_equal(
    setdiff(
      scale_labels(
        p +
          scale_x_mixtime(
            time_breaks = mixtime::days(1L),
            time_labels = "{cyc(day, month)}"
          )
      ),
      "NA"
    ),
    sprintf("%02d", 1:9)
  )
})

test_that("break granules finer than the chronon fall within it", {
  # Daily breaks on monthly data ask for breaks the data's own chronon cannot
  # land on: casting them onto it would put every break within a month at that
  # month, giving a year's worth of breaks but only twelve positions to draw
  # them at, each labelled with the same month over and over. Placing them
  # continuously puts each break at its position *within* the month instead.
  df <- data.frame(x = mixtime::yearmonth(600:611), y = 1:12)
  p <- ggplot(df, aes(x, y)) +
    geom_line() +
    scale_x_mixtime(time_breaks = mixtime::days(1L))

  labels <- scale_labels(p)
  expect_match(labels, "^20[0-9]{2}-[0-9]{2}-[0-9]{2}$")
  expect_equal(anyDuplicated(labels), 0L)
  # A year of monthly points, broken a day apart: as many labels as the panel
  # has days, rather than as many as it has months.
  expect_gt(length(labels), 300)

  # The breaks are a day apart on a scale still counted in months: the first
  # day of a 31 day January is a thirty-first of the way through it.
  breaks <- ggplot_build(p)$layout$panel_params[[1]]$x$get_breaks()
  breaks <- breaks[!is.na(breaks)]
  expect_equal(min(diff(breaks)), 1 / 31)

  # Which is the point of placing them this way: the data does not move for
  # them, so the months stay evenly spaced however the axis is broken.
  positions <- function(p) ggplot_build(p)$data[[1]]$x
  expect_equal(positions(p), positions(ggplot(df, aes(x, y)) + geom_line()))
  expect_equal(unique(diff(positions(p))), 1)

  # `time_minor_breaks` places its breaks the same way.
  p_minor <- ggplot(df, aes(x, y)) +
    geom_line() +
    scale_x_mixtime(time_minor_breaks = mixtime::days(1L))
  minor <- ggplot_build(p_minor)$layout$panel_params[[1]]$x$get_breaks_minor()
  expect_equal(anyDuplicated(minor), 0L)

  # A coarser granule than the data's needs no such placing: its breaks land
  # on whole months (600 is 2020 Jan).
  p_coarse <- ggplot(df, aes(x, y)) +
    geom_line() +
    scale_x_mixtime(time_breaks = mixtime::years(1L))
  breaks <- ggplot_build(p_coarse)$layout$panel_params[[1]]$x$get_breaks()
  expect_equal(breaks[!is.na(breaks)], c(600, 612))
  expect_equal(scale_labels(p_coarse), c("2020", "2021"))
})

test_that("chronons measured from a non-zero epoch keep their own time", {
  # `mixtime::mixtime()` reads a bare `<mt_time>` as raw numbers to be measured
  # from the target chronon's epoch rather than as time to convert onto it, and
  # a year chronon's epoch is 1970 -- so year 2020 (stored as 50) was drawn at
  # -1920 and labelled 51. Chronons whose epoch is zero (day, month, second)
  # were unharmed, which is why only yearly axes showed it.
  df <- data.frame(x = mixtime::year(2020:2025), y = 1:6)
  p <- ggplot(df, aes(x, y)) + geom_line()

  # 2020 is 50 years after the epoch, drawn mid-year by `align_discrete`.
  expect_equal(ggplot_build(p)$data[[1]]$x, 50:55 + 0.5)
  expect_match(scale_labels(p), "^20[0-9]{2}$")

  # Quarters are measured from a zero epoch, and were right all along.
  df <- data.frame(x = mixtime::yearquarter(200:207), y = 1:8)
  p <- ggplot(df, aes(x, y)) + geom_line()
  expect_equal(ggplot_build(p)$data[[1]]$x, 200:207 + 0.5)
})
