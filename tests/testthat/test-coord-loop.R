lynx_data <- function() {
  data.frame(
    year = 1846:1857,
    lynx = c(
      45150,
      49150,
      39520,
      21230,
      8420,
      5560,
      5080,
      10170,
      19600,
      32910,
      34380,
      29590
    ),
    peak = c(
      FALSE,
      TRUE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      FALSE,
      TRUE,
      FALSE
    )
  )
}

seasonal_data <- function() {
  x <- 0:93
  data.frame(
    time = as.Date("2025-04-01") + x,
    sin = sin(x / pi / 31 * 20) + x / 100,
    month = format(as.Date("2025-04-01") + x, "%m")
  )
}

test_that("looping over explicit loop points works", {
  skip_if_no_r42_graphics()

  df <- lynx_data()
  p <- ggplot(df, aes(x = year, y = lynx)) +
    geom_line(alpha = 0.25) +
    geom_point(aes(color = ordered(year)), size = 3, data = df[df$peak, ]) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 1))

  vdiffr::expect_doppelganger(
    "loops",
    p + coord_loop(loops = df$year[df$peak], expand = TRUE),
    writer = write_svg_r42
  )
})

test_that("looping over a duration works", {
  skip_if_no_r42_graphics()

  p <- ggplot(
    seasonal_data(),
    aes(x = time, y = sin, color = month, group = NA)
  ) +
    geom_path() +
    geom_point(size = 2)

  vdiffr::expect_doppelganger(
    "time_loops",
    p + coord_loop(time_loops = mixtime::months(1L), expand = TRUE),
    writer = write_svg_r42
  )
})

test_that("annotations spanning the panel are drawn", {
  # Panel spanning annotations use `Inf`, which must resolve against the loop
  # window rather than the uncut range or they are shifted out of view (#11).
  skip_if_no_r42_graphics()

  p <- ggplot(seasonal_data(), aes(x = time, y = sin, group = NA)) +
    geom_path() +
    geom_hline(yintercept = 0.5, colour = "red") +
    geom_vline(xintercept = as.Date("2025-04-10"), colour = "blue") +
    annotate("segment", x = -Inf, xend = Inf, y = 1, yend = 1)

  vdiffr::expect_doppelganger(
    "annotations span the panel",
    p + coord_loop(time_loops = mixtime::months(1L), expand = TRUE),
    writer = write_svg_r42
  )
})

test_that("geometries crossing a loop boundary are cut into pieces", {
  skip_if_no_r42_graphics()

  df <- seasonal_data()
  rects <- data.frame(
    xmin = as.Date(c("2025-04-20", "2025-06-25")),
    xmax = as.Date(c("2025-05-10", "2025-07-05")),
    ymin = -Inf,
    ymax = Inf
  )

  p <- ggplot(df, aes(x = time)) +
    geom_rect(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      data = rects,
      inherit.aes = FALSE,
      fill = "orange",
      alpha = 0.5
    ) +
    geom_ribbon(
      aes(ymin = sin - 0.3, ymax = sin + 0.3),
      fill = "steelblue",
      alpha = 0.5,
      colour = "navy"
    ) +
    geom_line(aes(y = sin))

  vdiffr::expect_doppelganger(
    "rects and ribbons across boundaries",
    p + coord_loop(time_loops = mixtime::months(1L), expand = TRUE),
    writer = write_svg_r42
  )
})

test_that("coord_loop works with coord_radial", {
  skip_if_no_r42_graphics()

  p <- ggplot(
    seasonal_data(),
    aes(x = time, y = sin, color = month, group = NA)
  ) +
    geom_path() +
    geom_point(size = 2)

  vdiffr::expect_doppelganger(
    "radial",
    p +
      coord_loop(
        time_loops = mixtime::months(1L),
        expand = TRUE,
        coord = coord_radial()
      ),
    writer = write_svg_r42
  )
})

test_that("coord_loop rejects unsupported coords", {
  expect_error(coord_loop(coord = coord_transform()), "does not support")
  expect_error(coord_loop(time = "z"), "requires .*time")
  expect_error(
    coord_loop(coord = coord_radial(theta = "y"), time = "x"),
    "angular axis"
  )
})

test_that("looped axes are labelled cyclically", {
  loop_labels <- function(p, aes = "x") {
    params <- ggplot_build(p)$layout$panel_params[[1]][[aes]]
    labels <- params$get_labels()
    labels[!is.na(labels)]
  }

  df <- data.frame(
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
  # A calendar breaks its axis at every one of the row's own cells rather than
  # wherever pretty breaks fall, so each cell of the row is named. A yearly
  # row's default daily cells are far past `calendar_max_cell_breaks`, so
  # `cells` is dropped and the axis falls back to the scale's own breaks --
  # the same ones `coord_loop()` shows above.
  expect_equal(
    loop_labels(p + coord_calendar(rows = mixtime::years(1L), cols = NULL)),
    c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Jan")
  )
  # Cells the axis can be cut by are named as themselves.
  expect_equal(
    loop_labels(
      p +
        coord_calendar(
          cells = mixtime::months(1L),
          rows = mixtime::years(1L),
          cols = NULL
        )
    ),
    c(month.abb, "Jan")
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
    c(
      "1973-01",
      "1973-03",
      "1973-05",
      "1973-07",
      "1973-09",
      "1973-11",
      "1974-01"
    )
  )
})

test_that("time_loops can be given as a granule directly", {
  df <- data.frame(
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
    ggplot_build(
      p + coord_loop(time_loops = mixtime::cal_gregorian$year(1L))
    )$data
  )
  # A duration of more than one unit steps by all of it, rather than by one.
  expect_equal(
    ggplot_build(p + coord_loop(time_loops = mixtime::years(2L)))$data,
    ggplot_build(
      p + coord_loop(time_loops = mixtime::cal_gregorian$year(2L))
    )$data
  )
  expect_equal(
    ggplot_build(
      p + coord_calendar(rows = mixtime::years(1L), cols = NULL)
    )$data,
    ggplot_build(
      p + coord_calendar(rows = mixtime::cal_gregorian$year(1L), cols = NULL)
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
  # `coord_calendar()`'s granule arguments (`rows` among them) are captured
  # unevaluated and only resolved once the axis's calendar is known (see
  # `eval_granule()`), so this now surfaces at build time rather than at
  # construction, naming the argument the user passed just the same.
  df <- data.frame(time = as.Date("2020-01-01") + 0:9, value = 0:9)
  p <- ggplot(df, aes(x = time, y = value)) + geom_line()
  expect_error(
    ggplot_build(
      p + coord_calendar(rows = c(mixtime::years(1L), mixtime::days(2L)))
    ),
    "`rows` must be a single duration"
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
    ggplot_build(p + coord_calendar(rows = mixtime::yearmonth(600L))),
    "`rows` must be a duration, not a time point"
  )
})

test_that("time_loops rejects strings", {
  # Parsing a string like "1 year" into a duration would need mixtime's own
  # (unexported) parser. For now, only durations and granules are accepted.
  expect_error(
    coord_loop(time_loops = "1 year"),
    "`time_loops` must be a duration or a granule"
  )
  # See "time_loops must be a single duration" above: resolved (and so
  # validated) at build time.
  df <- data.frame(time = as.Date("2020-01-01") + 0:9, value = 0:9)
  p <- ggplot(df, aes(x = time, y = value)) + geom_line()
  expect_error(
    ggplot_build(p + coord_calendar(rows = "1 week")),
    "`rows` must be a duration or a granule"
  )
})

test_that("the cycle of a looped axis follows the data's chronon", {
  loop_labels <- function(p) {
    params <- ggplot_build(p)$layout$panel_params[[1]]$x
    labels <- params$get_labels()
    labels[!is.na(labels)]
  }

  daily <- data.frame(
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

  quarterly <- data.frame(
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

test_that("looping does not depend on clipping path support", {
  # Cutting the data means everything already lies inside the loop window, so
  # no clipping paths (and therefore no device or R version requirement) are
  # involved.
  uad <- tsibble::as_tsibble(USAccDeaths)
  uad$index <- as.Date(uad$index)
  p <- ggplot(uad, aes(x = index, y = value)) +
    geom_line() +
    coord_loop(time_loops = mixtime::years(1L))

  file <- tempfile(fileext = ".png")
  on.exit(unlink(file), add = TRUE)
  expect_no_error({
    grDevices::png(file, width = 400, height = 300)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(p)
  })
})

test_that("self$limits is restored even if the parent setup_panel_params() errors", {
  # As with `CoordCalendar`, `CoordLoop$setup_panel_params()` temporarily
  # overwrites `self$limits` with the loop window and must restore it even if
  # the parent call errors, or the coord is left permanently holding the loop
  # window as its user limits.
  df <- data.frame(
    time = as.Date("2020-01-01") + 0:365,
    value = seq_len(366)
  )
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_point() +
    coord_loop(time_loops = mixtime::months(1L))
  built <- ggplot_build(p)
  layout <- built$layout
  scale_x <- layout$panel_scales_x[[1]]
  scale_y <- layout$panel_scales_y[[1]]
  coord <- layout$coord
  params <- layout$coord_params

  # `ggplot_build()` above already exercised `setup_panel_params()` once;
  # reset to a known baseline before poisoning the scale.
  coord$limits <- list(x = NULL, y = NULL)
  before <- coord$limits

  # `setup_panel_params()` calls the parent's `setup_panel_params()` twice:
  # once for the uncut range, once (with `self$limits` overwritten) for the
  # loop window. Erroring only on the second call exercises the restore
  # without preventing the method from getting that far.
  orig_get_limits <- scale_x$get_limits
  calls <- 0L
  scale_x$get_limits <- function() {
    calls <<- calls + 1L
    if (calls == 2L) {
      stop("forced error for testing")
    }
    orig_get_limits()
  }

  expect_error(
    coord$setup_panel_params(scale_x, scale_y, params),
    "forced error for testing"
  )
  expect_identical(coord$limits, before)
})
