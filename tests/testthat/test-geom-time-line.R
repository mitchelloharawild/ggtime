# The instants the geom resolves transitions against, recovered from the
# positions a locally-placed scale placed -- mirrors the inversion done inline
# in `resolve_time_transitions()`. `align_discrete = 0` positions each time
# point at the start of its granule, so the positions map back to the
# instants exactly. Only meaningful for a locally-placed scale
# (`scale_places_local_time()`) -- a ggplot2 datetime scale or an absolute-time
# mixtime scale never reach this inversion at all (see "transitions are not
# resolved..." below), so there is nothing to recover instants from in those
# cases.
built_instants <- function(p) {
  b <- ggplot_build(p)
  data <- b$data[[1]]
  scale <- b$layout$panel_scales_x[[1]]

  local_time <- as_mt_concrete(scale$get_transformation()$inverse(data$x))
  offset <- as_mt_concrete(mixtime::duration(
    data$xtimeoffset,
    chronon = attr(local_time, "chronon"),
    discrete = FALSE
  ))
  seconds_since_epoch(local_time - offset, tz = NA)
}

melbourne <- function(n = 12) {
  as.POSIXct("2023-04-02", tz = "Australia/Melbourne") + seq_len(n) * 3600
}

test_that("local positions can be read back as instants", {
  time <- melbourne()
  df <- data.frame(value = seq_along(time))
  df$time <- time

  # Local time: positions are wall clock readings, which repeat an hour over
  # the daylight saving transition and so are not the instants themselves.
  local <- ggplot(df, aes(time, value)) +
    geom_time_line() +
    scale_x_mixtime(
      time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA),
      align_discrete = 0
    )
  expect_equal(built_instants(local), as.numeric(time))
})

test_that("the scale converts offsets without them reaching the axis", {
  time <- melbourne()
  df <- data.frame(value = seq_along(time))
  df$time <- time

  built <- function(chronon) {
    ggplot_build(
      ggplot(df, aes(time, value)) +
        geom_time_line() +
        scale_x_mixtime(time_chronon = chronon, align_discrete = 0)
    )
  }

  # Converted onto the scale's chronon: 11 and 10 hours, not 39600 and 36000
  # seconds, and the same offset again as a fraction of a day.
  hourly <- built(mixtime::cal_gregorian$hour(1L, tz = NA))
  expect_equal(unique(hourly$data[[1]]$xtimeoffset), c(11, 10))
  daily <- built(mixtime::cal_gregorian$day(1L, tz = NA))
  expect_equal(unique(daily$data[[1]]$xtimeoffset), c(11, 10) / 24)

  # An offset is not a position: it must not train the axis, which would drag
  # the range towards the epoch, nor be censored against the axis limits.
  expect_equal(
    hourly$layout$panel_scales_x[[1]]$get_limits(),
    range(hourly$data[[1]]$x)
  )
})

test_that("offsets given at several granularities are each converted", {
  mel <- "Australia/Melbourne"
  hours <- mixtime::mixtime(
    as.POSIXct("2023-04-01 20:00", tz = mel) + 0:5 * 3600,
    chronon = mixtime::cal_gregorian$hour(1L, tz = mel)
  )
  days <- mixtime::mixtime(
    as.POSIXct("2023-04-03 00:00", tz = mel) + 0:2 * 86400,
    chronon = mixtime::cal_gregorian$day(1L, tz = mel)
  )
  df <- data.frame(value = 1:9)
  df$time <- c(hours, days)

  # Time at two granularities has its offsets stated at both: 11 hours before
  # the transition, and 10/24ths of a day after it.
  p <- ggplot(df, aes(time, value)) +
    geom_time_line() +
    scale_x_mixtime(
      time_chronon = mixtime::cal_gregorian$hour(1L, tz = mel),
      align_discrete = 0
    )
  built <- ggplot_build(p)
  expect_equal(built$data[[1]]$xtimeoffset, c(rep(11, 6), rep(10, 3)))
})

test_that("a bare number is not accepted as an offset", {
  time <- melbourne()
  df <- data.frame(value = seq_along(time), offset = 45)
  df$time <- time

  plot <- function(data) {
    ggplot(data, aes(time, value, xtimeoffset = offset)) +
      geom_time_line() +
      scale_x_mixtime(
        time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA),
        align_discrete = 0
      )
  }
  expect_error(ggplot_build(plot(df)), "must be a duration")

  # The same offset as a duration is converted onto the scale's chronon.
  df$offset <- mixtime::seconds(rep(45L, nrow(df)))
  built <- ggplot_build(plot(df))
  expect_equal(unique(built$data[[1]]$xtimeoffset), 45 / 3600)
})

test_that("a transitions table says what its columns are", {
  at <- as.POSIXct("2023-06-01 14:30", tz = "UTC")
  expect_error(
    geom_time_line(
      transitions = data.frame(time = at, offset_before = 0, offset_after = 45)
    ),
    "must be a duration"
  )
  expect_error(
    geom_time_line(
      transitions = data.frame(
        time = as.numeric(at),
        offset_before = mixtime::seconds(0L),
        offset_after = mixtime::seconds(45L)
      )
    ),
    "must be a time point"
  )
  expect_error(
    geom_time_line(transitions = data.frame(time = at)),
    "must have .*offset_before.* and .*offset_after.* columns"
  )
  expect_error(geom_time_line(transitions = 1:3), "must be a .*data.frame")

  # The unit travels with the offset, so the same jump can be said either way.
  df <- data.frame(value = 1:12)
  df$time <- as.POSIXct("2023-06-01 09:00", tz = "UTC") + 0:11 * 3600
  jump <- function(offset) {
    p <- ggplot(df, aes(time, value)) +
      geom_time_line(
        transitions = data.frame(
          time = as.POSIXct("2023-06-01 14:30", tz = "UTC"),
          offset_before = mixtime::seconds(0L),
          offset_after = offset
        )
      ) +
      scale_x_mixtime(
        time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA),
        align_discrete = 0
      )
    grob <- layer_grob(p)[[1]]
    dashed <- which(grob$gp$lty == 2)
    expect_length(dashed, 1)
    as.numeric(grob$x1)[dashed] - as.numeric(grob$x0)[dashed]
  }
  expect_equal(jump(mixtime::seconds(2700L)), jump(mixtime::minutes(45L)))
})

test_that("a timezone transition is drawn as a dashed jump in local time", {
  time <- melbourne()
  df <- data.frame(value = seq_along(time))
  df$time <- time

  linetypes <- function(chronon) {
    p <- ggplot(df, aes(time, value)) +
      geom_time_line() +
      scale_x_mixtime(time_chronon = chronon, align_discrete = 0)
    layer_grob(p)[[1]]$gp$lty
  }

  # One dashed segment, joining the two rows inserted at the transition.
  local <- linetypes(mixtime::cal_gregorian$hour(1L, tz = NA))
  expect_equal(sum(local == 2), 1)

  # Absolute time applies no offset, so there is nothing for a transition to
  # jump over -- no rows are inserted at all.
  absolute <- linetypes(mixtime::cal_gregorian$hour(1L, tz = "UTC"))
  expect_equal(sum(absolute == 2), 0)

  # Nothing to jump over.
  quiet <- as.POSIXct("2023-03-11", tz = "Australia/Melbourne") + 0:11 * 3600
  df$time <- quiet
  expect_equal(
    sum(linetypes(mixtime::cal_gregorian$hour(1L, tz = NA)) == 2),
    0
  )
})

test_that("a timezone transition is drawn as a dashed jump on the y axis too", {
  time <- melbourne()
  df <- data.frame(value = seq_along(time))
  df$time <- time
  naive <- mixtime::cal_gregorian$hour(1L, tz = NA)

  # There is no exported `scale_y_mixtime()`, so the underlying scale
  # constructor is used directly to put a naive-chronon mixtime scale on `y`.
  scale_y <- function() {
    mixtime_scale(
      aesthetics = "y",
      palette = identity,
      time_chronon = naive,
      align_discrete = 0
    )
  }

  # Same transition, time mapped to `y` instead of `x`.
  p <- ggplot(df, aes(value, time)) +
    geom_time_line() +
    scale_y()
  expect_equal(sum(layer_grob(p)[[1]]$gp$lty == 2), 1)

  # Nothing to jump over.
  quiet <- as.POSIXct("2023-03-11", tz = "Australia/Melbourne") + 0:11 * 3600
  df$time <- quiet
  p_quiet <- ggplot(df, aes(value, time)) +
    geom_time_line() +
    scale_y()
  expect_equal(sum(layer_grob(p_quiet)[[1]]$gp$lty == 2), 0)
})

test_that("transitions on x and y are both resolved, without interfering", {
  time <- melbourne()
  df <- data.frame(
    x_time = time,
    y_time = time + 60
  )
  naive <- mixtime::cal_gregorian$hour(1L, tz = NA)

  p <- ggplot(df, aes(x_time, y_time)) +
    geom_time_line() +
    scale_x_mixtime(time_chronon = naive, align_discrete = 0) +
    mixtime_scale(
      aesthetics = "y",
      palette = identity,
      time_chronon = naive,
      align_discrete = 0
    )
  lty <- layer_grob(p)[[1]]$gp$lty
  # One dashed segment for the x transition, one for the y transition.
  expect_equal(sum(lty == 2), 2)
})

test_that("transition_aesthetics overrides the transition segment's style", {
  time <- melbourne()
  df <- data.frame(value = seq_along(time))
  df$time <- time
  naive <- mixtime::cal_gregorian$hour(1L, tz = NA)

  plot <- function(transition_aesthetics = list(linetype = 2)) {
    ggplot(df, aes(time, value)) +
      geom_time_line(transition_aesthetics = transition_aesthetics) +
      scale_x_mixtime(time_chronon = naive, align_discrete = 0)
  }

  # Default: the transition segment is dashed and otherwise styled like the
  # rest of the (solid, black) line.
  default_grob <- layer_grob(plot())[[1]]
  dashed <- which(default_grob$gp$lty == 2)
  expect_length(dashed, 1)
  expect_equal(default_grob$gp$col[dashed], default_grob$gp$col[-dashed][1])

  # `list()` disables the distinct style entirely: no segment is dashed, even
  # though the rows are still inserted (so the slope stays correct).
  no_style <- layer_grob(plot(list()))[[1]]
  expect_equal(sum(no_style$gp$lty == 2), 0)

  # Other aesthetics can be overridden too, and combined with `linetype`.
  styled <- layer_grob(plot(list(colour = "red", linetype = 3)))[[1]]
  transition <- which(styled$gp$lty == 3)
  expect_length(transition, 1)
  expect_false(styled$gp$col[transition] %in% styled$gp$col[-transition])

  # Unknown or malformed aesthetics are rejected up front.
  expect_error(
    geom_time_line(transition_aesthetics = list(shape = 1)),
    "unknown aesthetic"
  )
  expect_error(
    geom_time_line(transition_aesthetics = list(linetype = 1:2)),
    "single value"
  )
  expect_error(
    geom_time_line(transition_aesthetics = 1:3),
    "must be a .*list"
  )
})

test_that("transitions are not resolved when the scale has no naive chronon", {
  time <- melbourne()
  df <- data.frame(value = seq_along(time))
  df$time <- time

  # ggplot2's own datetime scale (no `scale_x_mixtime()` at all): positions
  # are the instant itself, so there is nothing for a transition to jump over.
  p <- ggplot(df, aes(time, value)) + geom_time_line()
  expect_equal(sum(layer_grob(p)[[1]]$gp$lty == 2), 0)
})
