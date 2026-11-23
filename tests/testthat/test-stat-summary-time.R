summary_time_data <- function(df, ..., scale = NULL) {
  p <- ggplot(df, aes(time, value)) + stat_summary_time(...) + scale
  ggplot_build(p)$data[[1]]
}

daily <- function(n = 60) {
  data.frame(
    time = mixtime::date(as.Date("2024-01-01") + seq_len(n) - 1),
    value = as.numeric(seq_len(n))
  )
}
gregorian <- mixtime::cal_gregorian

test_that("values are summarised within granules", {
  out <- summary_time_data(daily(), granule = month(1L), fun = mean, geom = "point")
  expect_equal(out$y, c(mean(1:31), mean(32:60)))
  expect_equal(out$n, c(31, 29))
  expect_true(all(out$complete))
  expect_equal(out$end - out$start, c(31, 29))
  expect_equal(out$x, (out$start + out$end) / 2)

  dur <- summary_time_data(
    daily(), granule = mixtime::months(1L), fun = mean, geom = "point"
  )
  expect_equal(dur$y, out$y)
})

test_that("implicit missing values are filled before summarising", {
  df <- daily()[-5, ]
  out <- summary_time_data(df, granule = month(1L), fun = sum, geom = "point")
  expect_equal(out$y, c(NA, sum(32:60)))
  expect_equal(out$complete, c(FALSE, TRUE))

  len <- summary_time_data(df, granule = month(1L), fun = length, geom = "point")
  expect_equal(len$y, c(31, 29))

  for (args in list(list(na.rm = TRUE), list(fun.args = list(na.rm = TRUE)))) {
    out <- rlang::inject(summary_time_data(
      df, granule = gregorian$month(1L),
      fun = sum, geom = "point", !!!args
    ))
    expect_equal(out$y, c(sum(1:31) - 5, sum(32:60)))
    expect_equal(out$complete, c(FALSE, TRUE))
  }
})

test_that("explicit missing values are equivalent to implicit ones", {
  df <- daily()
  df$value[5] <- NA
  expect_no_warning(
    out <- summary_time_data(df, granule = month(1L), fun = sum, geom = "point")
  )
  expect_equal(out$y, c(NA, sum(32:60)))
  expect_equal(out$n, c(30, 29))
  expect_equal(out$complete, c(FALSE, TRUE))
})

test_that("unobserved granules are missing", {
  df <- daily(91)
  df <- df[-(32:60), ]
  out <- summary_time_data(df, granule = month(1L), fun = mean, geom = "line")
  expect_equal(is.na(out$y), c(FALSE, TRUE, FALSE))
  out <- summary_time_data(
    df, granule = month(1L), fun = mean, geom = "line", na.rm = TRUE
  )
  expect_equal(nrow(out), 2)
})

test_that("duplicates don't compensate for gaps", {
  df <- daily(31)[c(1:30, 30), ]
  suppressWarnings(out <- summary_time_data(
    df, granule = month(1L), fun = mean, geom = "point"
  ))
  expect_true(is.na(out$y))
})

test_that("the default is a line of means", {
  expect_message(
    out <- summary_time_data(daily()[-5, ], granule = month(1L)),
    "mean"
  )
  expect_equal(out$y, c(NA, mean(32:60)))
  expect_s3_class(stat_summary_time(granule = month(1L))$geom, "GeomLine")
})

test_that("completeness is checked within groups", {
  df <- rbind(cbind(daily(31), g = "a"), cbind(daily(31)[-5, ], g = "b"))
  p <- ggplot(df, aes(time, value, colour = g)) +
    stat_summary_time(granule = month(1L), fun = mean, geom = "point")
  out <- ggplot_build(p)$data[[1]]
  expect_equal(out$complete, c(TRUE, FALSE))
  expect_length(unique(out$colour), 2)
})

test_that("chronon defaults to the interval of POSIXct and Date", {
  t <- as.POSIXct("2024-01-01", tz = "UTC") + 0:47 * 3600
  out <- summary_time_data(
    data.frame(time = t, value = 1), granule = day(1L), fun = sum, geom = "point"
  )
  expect_equal(out$y, c(24, 24))
  out <- summary_time_data(
    data.frame(time = t[-3], value = 1), granule = day(1L), fun = sum,
    geom = "point"
  )
  expect_equal(out$y, c(NA, 24))

  out <- summary_time_data(
    data.frame(time = as.Date("2024-01-01") + 0:59, value = 1),
    granule = month(1L), fun = sum, geom = "point"
  )
  expect_equal(out$y, c(31, 29))
})

test_that("chronon of mixtime is used, and can be specified", {
  df <- data.frame(
    time = mixtime::datetime(as.POSIXct("2024-01-01", tz = "UTC") + 0:47 * 3600),
    value = 1
  )
  expect_warning(
    out <- summary_time_data(df, granule = day(1L), fun = sum, geom = "point"),
    "no complete granules"
  )
  expect_true(all(is.na(out$y)))
  out <- summary_time_data(
    df, granule = day(1L), chronon = hour(1L), fun = sum, geom = "point"
  )
  expect_equal(out$y, c(24, 24))
})

test_that("completeness uses the data's chronon, not the scale's", {
  df <- data.frame(time = mixtime::yearmonth("2023 Jan") + 0:23, value = 1)
  out <- summary_time_data(
    df, granule = year(1L), fun = sum, geom = "point",
    scale = scale_x_mixtime(time_chronon = gregorian$day(1L))
  )
  expect_equal(out$y, c(12, 12))
})

test_that("granules of other calendars, and time on y", {
  out <- summary_time_data(
    daily(14), granule = mixtime::cal_isoweek$week(1L), fun = sum, geom = "point"
  )
  expect_equal(out$y, c(sum(1:7), sum(8:14)))

  p <- ggplot(daily(), aes(value, time)) +
    stat_summary_time(granule = month(1L), fun = mean, geom = "point")
  out <- ggplot_build(p)$data[[1]]
  expect_equal(out$x, c(16, 46))
})

test_that("time zones are respected", {
  tz <- "Australia/Melbourne"
  df <- data.frame(
    time = as.POSIXct("2024-06-01", tz = tz) + 0:47 * 3600, value = 1
  )
  out <- summary_time_data(df, granule = day(1L), fun = sum, geom = "point")
  expect_equal(out$y, c(24, 24))
  df$time <- mixtime::datetime(df$time)
  out <- summary_time_data(
    df, granule = day(1L), chronon = hour(1L), fun = sum, geom = "point"
  )
  expect_equal(out$y, c(24, 24))
})

test_that("days with daylight saving transitions can be complete", {
  skip("time_is_complete_at() expects 24 hours in every day")
  tz <- "Australia/Melbourne"
  # 23 hours
  df <- data.frame(
    time = as.POSIXct("2024-10-06", tz = tz) + 0:22 * 3600, value = 1
  )
  out <- summary_time_data(df, granule = day(1L), fun = sum, geom = "point")
  expect_equal(out$y, 23)
  # 24 of 25 hours
  df <- data.frame(
    time = (as.POSIXct("2024-04-07", tz = tz) + 0:24 * 3600)[-5], value = 1
  )
  out <- summary_time_data(df, granule = day(1L), fun = sum, geom = "point")
  expect_equal(out$y, NA_real_)
})

test_that("unusable granules are reported", {
  expect_error(stat_summary_time(fun = mean), "must be supplied")
  df <- data.frame(time = mixtime::yearmonth("2023 Jan") + 0:23, value = 1)
  expect_warning(
    ggplot_build(
      ggplot(df, aes(time, value)) +
        stat_summary_time(granule = day(1L), fun = sum)
    ),
    "Computation failed"
  )
})

test_that("display alignment doesn't change granule membership", {
  for (align in c(0, 0.5, 1)) {
    out <- summary_time_data(
      daily(), granule = month(1L), fun = sum, geom = "point",
      scale = scale_x_mixtime(align_discrete = align)
    )
    expect_equal(out$y, c(sum(1:31), sum(32:60)))
    expect_equal(out$n, c(31, 29))
  }
})

test_that("tsibble indices can be summarised", {
  date <- as.Date("2023-01-01")
  for (time in list(
    tsibble::yearmonth(date) + 0:23,
    tsibble::yearquarter(date) + 0:7
  )) {
    out <- summary_time_data(
      data.frame(time = time, value = 1),
      granule = year(1L), fun = sum, geom = "point"
    )
    expect_equal(out$y, rep(length(time) / 2, 2))
  }
})
