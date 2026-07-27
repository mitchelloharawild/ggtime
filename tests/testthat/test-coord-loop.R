test_that("ljust works", {
  skip_if_no_r42_graphics()

  df <- data.frame(
    year = c(1846, 1847, 1848, 1849, 1850, 1851, 1852, 1853, 1854, 1855, 1856, 1857),
    lynx = c(45150, 49150, 39520, 21230, 8420, 5560, 5080, 10170, 19600, 32910, 34380, 29590),
    peak = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  )

  p <- df |>
    ggplot(aes(x = year, y = lynx)) +
    geom_line(alpha = 0.25) +
    geom_point(aes(color = ordered(year)), size = 3, data = df[df$peak, ]) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 1))

  vdiffr::expect_doppelganger("loop + ljust = 0",
    p + coord_loop(loop = df$year[df$peak], ljust = 0, expand = c(TRUE, FALSE)),
    writer = write_svg_r42
  )

  vdiffr::expect_doppelganger("loop + ljust = 0.5",
    p + coord_loop(loop = df$year[df$peak], ljust = 0.5, expand = c(TRUE, FALSE)),
    writer = write_svg_r42
  )

  vdiffr::expect_doppelganger("loop + ljust = 1",
    p + coord_loop(loop = df$year[df$peak], ljust = 1, expand = c(TRUE, FALSE)),
    writer = write_svg_r42
  )

  x <- 0:93
  p <- tibble(
    time = as.Date("2025-04-01") + x,
    sin = sin(x/pi/31*20) + x/100,
    month = format(time, "%m")
  ) |>
    ggplot(aes(x = time, y = sin,
      color = month,
      group = NA)) +
    geom_path() +
    geom_point(size = 2) +
    geom_vline(xintercept = as.Date("2025-04-01"))

  vdiffr::expect_doppelganger("time_loop + ljust = 0",
    p + coord_loop(time_loop = "1 month", ljust = 0, expand = c(TRUE, FALSE)),
    writer = write_svg_r42
  )

  vdiffr::expect_doppelganger("time_loop + ljust = 0.5",
    p + coord_loop(time_loop = "1 month", ljust = 0.5, expand = c(TRUE, FALSE)),
    writer = write_svg_r42
  )

  vdiffr::expect_doppelganger("time_loop + ljust = 1",
    p + coord_loop(time_loop = "1 month", ljust = 1, expand = c(TRUE, FALSE)),
    writer = write_svg_r42
  )
})

test_that("coord_loop works with coord_radial", {
  skip_if_not_installed("vdiffr")

  x <- 0:93
  df <- tibble(
    time = as.Date("2025-04-01") + x,
    sin = sin(x/pi/31*20) + x/100,
    g = rep(c("a","b"), 47),
    month = format(time, "%m")
  )
  p <- df |>
    ggplot(aes(x = time, y = sin, color = month, group = NA)) +
    geom_path() +
    geom_point(size = 2) +
    geom_vline(xintercept = as.Date("2025-04-01"))

  vdiffr::expect_doppelganger("radial + ljust = 0.5",
    p + coord_loop(time_loop = "1 month", expand = c(TRUE, FALSE), coord = coord_radial())
  )
})

test_that("clip_loops works", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    year = c(1847, 1848, 1849, 1850, 1851, 1852, 1853, 1854, 1855, 1856),
    lynx = c(49150, 39520, 21230, 8420, 5560, 5080, 10170, 19600, 32910, 34380),
    peak = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )

  p <- df |>
    ggplot(aes(x = year, y = lynx)) +
    geom_line(alpha = 0.25) +
    geom_point(aes(color = ordered(year)), size = 3, data = df[df$peak, ]) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 1))

  vdiffr::expect_doppelganger("time_loops + clip_loops = off",
    p + coord_loop(time_loops = 9, expand = c(TRUE, FALSE), clip_loops = "off")
  )
})

test_that("looped axes are labelled cyclically", {
  loop_labels <- function(p, aes = "x") {
    params <- ggplot_build(p)$layout$panel_params[[1]][[aes]]
    labels <- params$get_labels()
    labels[!is.na(labels)]
  }

  df <- tibble::tibble(
    time = mixtime::yearmonth(36L + 0:71),
    value = as.numeric(USAccDeaths)
  )
  p <- ggplot(df, aes(x = time, y = value)) + geom_line()

  # Monthly data looped over years shows months of the year, so the year in
  # which each month fell is no longer meaningful.
  expect_equal(
    loop_labels(p + coord_loop(time_loops = mixtime::years(1L))),
    c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Jan")
  )
  expect_equal(
    loop_labels(
      p + coord_loop(time_loops = mixtime::years(1L), coord = coord_radial()),
      "theta"
    ),
    c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Jan")
  )
  expect_equal(
    loop_labels(p + coord_calendar(time_rows = mixtime::years(1L))),
    c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Jan")
  )

  # Without a loop, time is linear and labelled as points in time.
  expect_match(loop_labels(p + coord_loop()), "^19[0-9]{2} [A-Z][a-z]{2}")

  # Labels the user asked for are not overridden.
  expect_equal(
    loop_labels(
      p +
        coord_loop(time_loops = mixtime::years(1L)) +
        scale_x_mixtime(time_labels = "{lin(year)}-{cyc(month, year)}")
    ),
    c("1973-01", "1973-03", "1973-05", "1973-07", "1973-09", "1973-11", "1974-01")
  )
})

test_that("time_loops can be given as a granule directly", {
  df <- tibble::tibble(
    time = mixtime::yearmonth(36L + 0:71),
    value = as.numeric(USAccDeaths)
  )
  p <- ggplot(df, aes(x = time, y = value)) + geom_line()

  # `mixtime::years()` and friends return a `<mixtime>`, which keeps the
  # duration inside its vecvec wrapper. Stepping the time axis reads the
  # duration's chronon directly, so the wrapper has to come off first. Passing
  # that same granule directly should skip the reduction and give identical
  # results.
  expect_equal(
    ggplot_build(p + coord_loop(time_loops = mixtime::years(1L)))$data,
    ggplot_build(p + coord_loop(time_loops = mixtime::cal_gregorian$year(1L)))$data
  )
  # A duration of more than one unit steps by all of it, rather than by one.
  expect_equal(
    ggplot_build(p + coord_loop(time_loops = mixtime::years(2L)))$data,
    ggplot_build(p + coord_loop(time_loops = mixtime::cal_gregorian$year(2L)))$data
  )
  expect_equal(
    ggplot_build(p + coord_calendar(time_rows = mixtime::years(1L)))$data,
    ggplot_build(
      p + coord_calendar(time_rows = mixtime::cal_gregorian$year(1L))
    )$data
  )
})

test_that("time_loops must be a single duration", {
  # Several loop widths would be ambiguous, and both `unvecvec()` and `seq()`
  # would silently use only the first.
  expect_error(
    coord_loop(time_loops = c(mixtime::years(1L), mixtime::days(2L))),
    "`time_loops` must be a single duration"
  )
  expect_error(
    coord_loop(time_loops = mixtime::years(1:2)),
    "`time_loops` must be a single duration"
  )
  # `coord_calendar()` names the argument the user passed.
  expect_error(
    coord_calendar(time_rows = c(mixtime::years(1L), mixtime::days(2L))),
    "`time_rows` must be a single duration"
  )
  # No looping is not a duration to check.
  expect_no_error(coord_loop(time_loops = NULL))
  expect_no_error(coord_loop())

  # A time point says when, not how long.
  expect_error(
    coord_loop(time_loops = mixtime::yearmonth(600L)),
    "`time_loops` must be a duration, not a time point"
  )
  expect_error(
    coord_calendar(time_rows = mixtime::yearmonth(600L)),
    "`time_rows` must be a duration, not a time point"
  )
})

test_that("time_loops rejects strings", {
  # Parsing a string like "1 year" into a duration would need mixtime's own
  # (unexported) parser. For now, only durations and granules are accepted.
  expect_error(
    coord_loop(time_loops = "1 year"),
    "`time_loops` must be a duration or a granule"
  )
  expect_error(
    coord_calendar(time_rows = "1 week"),
    "`time_rows` must be a duration or a granule"
  )
})

test_that("the cycle of a looped axis follows the data's chronon", {
  loop_labels <- function(p) {
    params <- ggplot_build(p)$layout$panel_params[[1]]$x
    labels <- params$get_labels()
    labels[!is.na(labels)]
  }

  daily <- tibble::tibble(
    time = mixtime::date("2015-01-01") + 0:27,
    value = seq_len(28)
  )
  p <- ggplot(daily, aes(x = time, y = value)) + geom_line()

  # Daily data looped over weeks shows days of the week.
  expect_equal(
    loop_labels(p + coord_loop(time_loops = mixtime::weeks(1L))),
    c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon")
  )
  # The same data looped over months shows days of the month.
  expect_match(
    loop_labels(p + coord_loop(time_loops = mixtime::months(1L))),
    "^D[0-9]{2}"
  )

  quarterly <- tibble::tibble(
    time = mixtime::yearquarter(180L + 0:15),
    value = seq_len(16)
  )
  expect_equal(
    loop_labels(
      ggplot(quarterly, aes(x = time, y = value)) +
        geom_line() +
        coord_loop(time_loops = mixtime::years(1L))
    ),
    c("Q1", "Q2", "Q3", "Q4", "Q1")
  )
})
