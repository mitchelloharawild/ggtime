tsbl_co2 <- as_tsibble(co2)
tsbl_ped <- dplyr::filter(
  tsibble::pedestrian,
  Sensor == "Southern Cross Station",
  yearmonth(Date) == yearmonth("2015 Jan")
)

test_that("gg_season() plots", {
  p <- gg_season(tsbl_co2, value)

  expect_equal(
    ggplot2::layer_data(p)$y,
    tsbl_co2$value
  )
  expect_equal(
    as.numeric(table(ggplot2::layer_data(p)$colour)),
    rep(12, 39)
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )

  p <- gg_season(tsbl_ped, Count, "day")

  expect_equal(
    ggplot2::layer_data(p)$y,
    tsbl_ped$Count
  )
  expect_equal(
    as.numeric(table(ggplot2::layer_data(p)$colour)),
    rep(24, 31)
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "Date_Time", y = "Count")
  )
})


test_that("gg_season() plot labels", {
  p <- gg_season(tsbl_co2, value, labels = "both")

  expect_equal(
    ggplot2::layer_data(p)$y,
    tsbl_co2$value
  )
  expect_equal(
    as.numeric(table(ggplot2::layer_data(p)$colour)),
    rep(12, 39)
  )
  expect_equal(
    c(ggplot2::layer_data(p, 2)$label, ggplot2::layer_data(p, 3)$label),
    ordered(rep(1959:1997, 2))
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
})

test_that("gg_season() facets", {
  p <- gg_season(tsbl_co2, value, facet_period = "10 year", labels = "both")

  # expect_equal(
  #   ggplot2::layer_data(p)$y,
  #   tsbl_co2$value
  # )
  expect_equal(
    as.numeric(table(ggplot2::layer_data(p)$colour)),
    rep(12, 39)
  )
  expect_equal(
    c(ggplot2::layer_data(p, 2)$label, ggplot2::layer_data(p, 3)$label),
    ordered(rep(1959:1997, 2))
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )
})

test_that("gg_subseries() plots", {
  p <- gg_subseries(tsbl_co2, value)

  expect_equal(
    ggplot2::layer_data(p)$y,
    tsbl_co2$value[order((seq_along(tsbl_co2$value) - 1) %% 12)]
  )
  expect_equal(
    as.numeric(table(ggplot2::layer_data(p)$PANEL)),
    rep(39, 12)
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )

  p <- gg_subseries(tsbl_ped, Count, "day")

  expect_equal(
    ggplot2::layer_data(p)$y,
    tsbl_ped$Count[order((seq_along(tsbl_ped$Count) - 1) %% 24)]
  )
  expect_equal(
    as.numeric(table(ggplot2::layer_data(p)$PANEL)),
    rep(31, 24)
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "Date_Time", y = "Count")
  )
})

test_that("gg_lag() plots", {
  p <- gg_lag(tsbl_co2, value)

  expect_equal(
    ggplot2::layer_data(p, 2)$x,
    do.call(
      c,
      map(seq_len(9), function(i) {
        tsbl_co2$value[seq_len(length(tsbl_co2$value) - i)]
      })
    )
  )
  expect_equal(
    as.numeric(table(ggplot2::layer_data(p, 2)$PANEL)),
    length(tsbl_co2$value) - seq_len(9)
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "lag(value, n)", y = "value")
  )

  p <- gg_lag(tsbl_co2, value, lags = c(1, 4, 9))

  expect_equal(
    ggplot2::layer_data(p, 2)$x,
    do.call(
      c,
      map(c(1, 4, 9), function(i) {
        tsbl_co2$value[seq_len(length(tsbl_co2$value) - i)]
      })
    )
  )
  expect_equal(
    as.numeric(table(ggplot2::layer_data(p, 2)$PANEL)),
    length(tsbl_co2$value) - c(1, 4, 9)
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "lag(value, n)", y = "value")
  )
})

test_that("gg_tsdisplay() plots", {
  p <- gg_tsdisplay(tsbl_co2, value)

  expect_s3_class(
    p,
    "gg_tsensemble"
  )

  expect_equal(
    ggplot2::layer_data(p[[1]], 1)$y,
    tsbl_co2$value
  )

  p_built <- ggplot2::ggplot_build(p[[1]])

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )

  p <- p + ggplot2::labs(x = "x", y = "y", title = "title")

  p_built <- ggplot2::ggplot_build(p[[1]])

  expect_equal(
    p_built$plot$labels[c("x", "y", "title")],
    list(x = "x", y = "y", title = "title")
  )

  p_built <- ggplot2::ggplot_build(p[[2]])

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "lag [1M]", y = "acf")
  )

  p_built <- ggplot2::ggplot_build(p[[3]])

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "index", y = "value")
  )

  p <- gg_tsdisplay(tsbl_co2, value, plot_type = "histogram")

  p_built <- ggplot2::ggplot_build(p[[3]])

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "value", y = "count"),
    ignore_attr = TRUE
  )

  p <- gg_tsdisplay(tsbl_co2, value, plot_type = "scatter")

  expect_equal(
    ggplot2::layer_data(p[[3]], 1)$y,
    tsbl_co2$value
  )

  p_built <- ggplot2::ggplot_build(p[[3]])

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = expression(Y[t - 1]), y = expression(Y[t]))
  )

  p <- gg_tsdisplay(tsbl_co2, value, plot_type = "spectrum")

  p_built <- ggplot2::ggplot_build(p[[3]])

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "frequency", y = "spectrum")
  )
})

test_that("gg_arma() plots", {
  skip_if_not_installed("fable")
  mdl <- tsbl_co2 %>%
    fabletools::model(fable::ARIMA(value ~ 0 + pdq(1, 1, 1) + PDQ(1, 1, 2)))

  p <- gg_arma(mdl)
  smmry <- fabletools::glance(mdl)
  ar_roots <- smmry$ar_roots[[1]]
  ma_roots <- smmry$ma_roots[[1]]

  expect_equal(
    ggplot2::layer_data(p, 4)$y,
    c(Im(1 / ar_roots), Im(1 / ma_roots))
  )
  expect_equal(
    ggplot2::layer_data(p, 4)$x,
    c(Re(1 / ar_roots), Re(1 / ma_roots))
  )
  expect_equal(
    ggplot2::layer_data(p, 4)$PANEL,
    factor(c(rep_along(ar_roots, 1), rep_along(ma_roots, 2)))
  )

  p_built <- ggplot2::ggplot_build(p)

  expect_equal(
    p_built$plot$labels[c("x", "y")],
    list(x = "Re(1/root)", y = "Im(1/root)")
  )
})

test_that("interval_to_period() converts mixtime granules", {
  cal <- mixtime::cal_gregorian
  expect_equal(
    interval_to_period(cal$month(1L)),
    lubridate::period(1, units = "month")
  )
  expect_equal(
    interval_to_period(cal$quarter(1L)),
    lubridate::period(3, units = "month")
  )
  expect_equal(
    interval_to_period(mixtime::cal_isoweek$week(2L)),
    lubridate::period(2, units = "week")
  )
  expect_equal(
    interval_to_period(cal$hour(1L)),
    lubridate::period(1, units = "hour")
  )
  # Composite intervals are a list of granules
  expect_equal(
    interval_to_period(list(cal$hour(1L), cal$minute(30L))),
    lubridate::hours(1) + lubridate::minutes(30)
  )
})

test_that("time_origin() does not silently treat mixtime as numeric", {
  expect_error(
    time_origin(mixtime::yearmonth("2020 Jan")),
    "mixtime"
  )
  expect_equal(time_origin(1:3), 0)
})

mixtime_tsbl <- function(index) {
  tsibble::tsibble(t = index, y = seq_along(index), index = t)
}

skip_if_no_mixtime_tsibble <- function() {
  skip_if_not(
    is_granule(tsibble::interval(
      mixtime_tsbl(mixtime::yearmonth(as.Date("2020-01-01") + c(0, 31)))
    )),
    "tsibble doesn't support mixtime indices"
  )
}

test_that("gg_season() and gg_subseries() support mixtime indices", {
  skip_if_no_mixtime_tsibble()
  monthly <- mixtime_tsbl(
    mixtime::yearmonth(seq(as.Date("2018-01-01"), by = "month", length.out = 36))
  )

  p <- gg_season(monthly, y)
  d <- ggplot2::layer_data(p)
  expect_equal(d$y, monthly$y)
  expect_equal(as.numeric(table(d$group)), rep(12, 3))
  expect_no_error(ggplot2::ggplot_build(gg_season(monthly, y, polar = TRUE)))
  expect_no_error(ggplot2::ggplot_build(gg_season(monthly, y, "2 years")))
  expect_no_error(ggplot2::ggplot_build(gg_season(monthly, y, 12)))
  expect_no_error(ggplot2::ggplot_build(
    gg_season(monthly, y, mixtime::years(1L))
  ))
  expect_error(gg_season(monthly, y, 1), "at least one observation")

  p <- gg_subseries(monthly, y)
  expect_equal(levels(p$data$id), month.abb)
  expect_no_error(ggplot2::ggplot_build(p))

  daily <- mixtime_tsbl(
    mixtime::date(seq(as.Date("2020-01-06"), by = "day", length.out = 28))
  )
  d <- ggplot2::layer_data(gg_season(daily, y, "week"))
  expect_equal(as.numeric(table(d$group)), rep(7, 4))
  expect_no_error(ggplot2::ggplot_build(gg_subseries(daily, y, "week")))
})

test_that("autoplot.tbl_ts() labels the axis with the compact interval", {
  skip_if_no_mixtime_tsibble()
  monthly <- mixtime_tsbl(
    mixtime::yearmonth(seq(as.Date("2018-01-01"), by = "month", length.out = 24))
  )
  expect_equal(format_interval(tsibble::interval(monthly)), "1M")
  expect_equal(ggplot2::autoplot(monthly, y)$labels$x, "t [1M]")
})

test_that("gg_lag() supports mixtime indices", {
  skip_if_no_mixtime_tsibble()
  monthly <- mixtime_tsbl(
    mixtime::yearmonth(seq(as.Date("2018-01-01"), by = "month", length.out = 36))
  )
  p <- gg_lag(monthly, y)
  expect_equal(levels(p$data$season), month.abb)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_no_error(ggplot2::ggplot_build(gg_lag(monthly, y, "year")))

  daily <- mixtime_tsbl(
    mixtime::date(seq(as.Date("2020-01-06"), by = "day", length.out = 28))
  )
  expect_equal(
    levels(gg_lag(daily, y)$data$season),
    c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  )

  hourly <- mixtime_tsbl(
    mixtime::datetime(
      as.POSIXct("2020-01-01", tz = "UTC") + 3600 * 0:71
    )
  )
  expect_length(levels(gg_lag(hourly, y)$data$season), 24)
})

test_that("granule intervals have default seasonal periods", {
  skip_if_no_mixtime_tsibble()
  cal <- mixtime::cal_gregorian
  expect_equal(default_period_granule(cal$month(1L)), cal$year(1L))
  expect_equal(default_period_granule(cal$day(1L)), cal$year(1L))
  expect_equal(
    default_period_granule(cal$day(1L), "smallest"),
    mixtime::cal_isoweek$week(1L)
  )
  expect_equal(default_period_granule(cal$hour(1L), "smallest"), cal$day(1L))
  expect_null(default_period_granule(cal$year(1L)))
  expect_true(interval_is_seasonal(cal$month(1L)))
  expect_false(interval_is_seasonal(cal$year(1L)))
})

test_that("lag_to_duration() converts lags to mixtime durations", {
  cal <- mixtime::cal_gregorian
  lag <- function(x, interval) {
    vctrs::new_vctr(x, interval = interval, class = "cf_lag")
  }

  expect_equal(
    lag_to_duration(lag(0:3, cal$month(1L))),
    mixtime::duration(c(0, 1, 2, 3), cal$month(1L))
  )
  # Lags are counts of the interval, whatever its size
  expect_equal(
    format(lag_to_duration(lag(2, cal$hour(3L)))),
    format(mixtime::duration(2, cal$hour(3L)))
  )
  expect_equal(
    lag_to_duration(lag(1:2, list(cal$day(1L)))),
    mixtime::duration(c(1, 2), cal$day(1L))
  )
  # Without a single regular unit, lags stay as counts
  expect_equal(lag_to_duration(lag(1:2, NULL)), c(1, 2))
  expect_equal(lag_to_duration(lag(1:2, list(cal$hour(1L), cal$minute(1L)))), c(1, 2))
})

test_that("autoplot.tbl_cf() plots lags as mixtime durations", {
  skip_if_no_mixtime_tsibble()
  skip_if_not_installed("feasts")
  loadNamespace("feasts")
  cf <- tibble::tibble(
    lag = vctrs::new_vctr(
      0:12,
      interval = mixtime::cal_gregorian$month(1L),
      class = "cf_lag"
    ),
    acf = cos(0:12)
  )
  cf <- tsibble::new_tsibble(
    tsibble::as_tsibble(cf, index = "lag"),
    num_obs = tibble::tibble(.len = 100),
    class = "tbl_cf"
  )
  p <- ggplot2::autoplot(cf)
  expect_true(mixtime::is_mixtime(p$data$lag))
  expect_equal(p$labels$x, "lag [1M]")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("lag_to_duration() passes mixtime durations through", {
  d <- mixtime::duration(0:3, mixtime::cal_gregorian$month(1L))
  expect_identical(lag_to_duration(d), d)
  expect_equal(
    lag_to_duration(0:3, mixtime::cal_gregorian$month(1L)),
    mixtime::duration(0:3, mixtime::cal_gregorian$month(1L))
  )
})

test_that("autoplot.tbl_cf() accepts a mixtime duration lag index", {
  skip_if_no_mixtime_tsibble()
  cf <- tibble::tibble(
    lag = mixtime::duration(0:12, mixtime::cal_gregorian$month(1L)),
    acf = cos(0:12)
  )
  cf <- tryCatch(
    tsibble::new_tsibble(
      tsibble::as_tsibble(cf, index = "lag"),
      num_obs = tibble::tibble(.len = 100),
      class = "tbl_cf"
    ),
    error = function(e) skip("tsibble can't index by duration")
  )
  p <- ggplot2::autoplot(cf)
  expect_equal(p$labels$x, "lag [1M]")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ACF-based plots support mixtime indices", {
  skip_if_no_mixtime_tsibble()
  skip_if_not_installed("feasts")
  skip_if_not_installed("fable")
  set.seed(1)
  indices <- list(
    monthly = list(
      index = mixtime::yearmonth(
        seq(as.Date("2015-01-01"), by = "month", length.out = 60)
      ),
      label = "lag [1M]"
    ),
    daily = list(
      index = mixtime::date(
        seq(as.Date("2020-01-06"), by = "day", length.out = 120)
      ),
      label = "lag [1D]"
    ),
    hourly = list(
      index = mixtime::datetime(
        as.POSIXct("2020-01-01", tz = "UTC") + 3600 * 0:119
      ),
      label = "lag [3600s]"
    )
  )
  for (x in indices) {
    data <- mixtime_tsbl(x$index)
    data$y <- rnorm(nrow(data))
    acf <- tryCatch(
      feasts::ACF(data, y),
      error = function(e) skip("feasts doesn't support mixtime intervals")
    )
    p <- ggplot2::autoplot(acf)
    expect_equal(p$labels$x, x$label)
    expect_no_error(ggplot2::ggplot_build(p))

    expect_no_error(ggplot2::ggplot_build(gg_lag(data, y)))
    expect_no_error(gg_tsdisplay(data, y))

    mbl <- fabletools::model(data, fable::NAIVE(y))
    expect_no_error(gg_tsresiduals(mbl))
  }
})

test_that("gg_season() errors clearly on zero-row data", {
  empty <- dplyr::filter(tsbl_co2, index > max(index))
  expect_error(
    gg_season(empty, value),
    "at least one observation"
  )
})
