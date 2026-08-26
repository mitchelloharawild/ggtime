# Hourly prices over `days` days, starting on a granule boundary so that the
# expected bins are easy to state.
hourly <- function(days = 4, tz = "UTC") {
  n <- days * 24
  data.frame(
    time = mixtime::datetime(
      as.POSIXct("2024-01-01 00:00:00", tz = tz) + (seq_len(n) - 1) * 3600
    ),
    price = seq_len(n)
  )
}

# Already summarised daily prices, of the shape `stat_ohlc()`
# re-summarises onto a coarser granule.
daily_ohlc <- function(days = 40) {
  data.frame(
    time = mixtime::date(as.Date("2024-01-01") + seq_len(days) - 1),
    open = seq_len(days),
    close = seq_len(days) + 1,
    low = seq_len(days) - 2,
    high = seq_len(days) + 3
  )
}

layer_data1 <- function(p) ggplot_build(p)$data[[1]]

test_that("open, high, low and close summarise each granule instance", {
  df <- hourly(3)
  data <- layer_data1(
    ggplot(df, aes(time, price)) +
      geom_candlestick(granule = mixtime::days(1L))
  )

  expect_equal(nrow(data), 3)
  expect_equal(data$n, rep(24L, 3))
  # `price` increases by one an hour, so each day opens on its first hour and
  # closes on its last, which are also its low and its high.
  expect_equal(data$open, c(1, 25, 49))
  expect_equal(data$close, c(24, 48, 72))
  expect_equal(data$low, data$open)
  expect_equal(data$high, data$close)
  # Mapped onto boxplot's positional aesthetics, so that the position scales
  # know how to work with them.
  expect_equal(data$lower, data$open)
  expect_equal(data$upper, data$close)
  expect_equal(data$ymin, data$low)
  expect_equal(data$ymax, data$high)
})

test_that("values are summarised in time order, not row order", {
  df <- hourly(1)
  shuffled <- df[sample(nrow(df)), ]

  data <- layer_data1(
    ggplot(shuffled, aes(time, price)) +
      geom_candlestick(granule = mixtime::days(1L))
  )
  expect_equal(data$open, 1)
  expect_equal(data$close, 24)
})

test_that("candlesticks are cut on the granule's own boundaries", {
  # Deliberately not starting on a boundary: the first candlestick covers the
  # whole of the day the data starts partway through, rather than a day
  # measured from the first observation.
  df <- hourly(3)
  df$time <- df$time + mixtime::hours(6L)

  data <- layer_data1(
    ggplot(df, aes(time, price)) +
      geom_candlestick(granule = mixtime::days(1L))
  )
  day <- 86400
  starts <- as.numeric(as.POSIXct(
    c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04"),
    tz = "UTC"
  ))
  expect_equal(data$x, starts + day / 2)
  expect_equal(data$width, rep(day * 0.9, 4))
})

test_that("instances of uneven length are drawn at uneven widths", {
  data <- layer_data1(
    ggplot(
      daily_ohlc(90),
      aes(
        time,
        lower = open,
        upper = close,
        ymin = low,
        ymax = high
      )
    ) +
      geom_candlestick(granule = mixtime::months(1L))
  )

  # January, February and March of a leap year.
  expect_equal(data$width / 0.9, c(31, 29, 31))
})

test_that("`width` sizes the body against its granule instance", {
  full <- layer_data1(
    ggplot(hourly(3), aes(time, price)) +
      geom_candlestick(granule = mixtime::days(1L), width = 1)
  )
  # Neighbouring bodies touch at a width of 1.
  expect_equal(full$xmax[-3], full$xmin[-1])
  expect_equal(full$xmax - full$xmin, rep(86400, 3))
})

test_that("already summarised values re-summarise onto a coarser granule", {
  df <- daily_ohlc(31)
  data <- layer_data1(
    ggplot(
      df,
      aes(
        time,
        lower = open,
        upper = close,
        ymin = low,
        ymax = high
      )
    ) +
      geom_candlestick(granule = mixtime::months(1L))
  )

  expect_equal(nrow(data), 1)
  expect_equal(data$open, df$open[1])
  expect_equal(data$close, df$close[31])
  expect_equal(data$low, min(df$low))
  expect_equal(data$high, max(df$high))
})

test_that("`direction` records how each candlestick closed", {
  df <- data.frame(
    time = mixtime::date(as.Date("2024-01-01") + rep(0:2, each = 2)),
    price = c(1, 2, 2, 1, 3, 3)
  )
  data <- layer_data1(
    ggplot(df, aes(time, price)) +
      geom_candlestick(granule = mixtime::days(1L))
  )

  expect_s3_class(data$direction, "ordered")
  expect_equal(
    as.character(data$direction),
    c("rising", "falling", "unchanged")
  )
  # Every level is present whether or not the data uses it, so that a
  # diverging scale places "unchanged" at its midpoint.
  expect_equal(levels(data$direction), c("falling", "unchanged", "rising"))
  only_rising <- data.frame(
    time = mixtime::date(as.Date("2024-01-01") + rep(0:1, each = 2)),
    price = c(1, 2, 3, 4)
  )
  data <- layer_data1(
    ggplot(only_rising, aes(time, price)) +
      geom_candlestick(granule = mixtime::days(1L))
  )
  expect_equal(as.character(data$direction), c("rising", "rising"))
  expect_equal(levels(data$direction), c("falling", "unchanged", "rising"))
})

test_that("`direction` colours fill and colour red/green by default, without an explicit scale", {
  df <- data.frame(
    time = mixtime::date(as.Date("2024-01-01") + rep(0:2, each = 2)),
    price = c(1, 2, 2, 1, 3, 3)
  )
  data <- layer_data1(
    ggplot(df, aes(time, price)) +
      geom_candlestick(granule = mixtime::days(1L))
  )

  expect_s3_class(data$direction, "ggtime_candlestick_direction")
  expect_equal(
    as.character(data$direction),
    c("rising", "falling", "unchanged")
  )
  expect_equal(
    data$fill,
    c(rising = "#388E3C", falling = "#D32F2F", unchanged = "grey70"),
    ignore_attr = TRUE
  )
  # `colour` defaults to the same mapping, so the wick and body border match
  # the body fill.
  expect_equal(data$colour, data$fill)
})

test_that("`change` records close minus open", {
  df <- data.frame(
    time = mixtime::date(as.Date("2024-01-01") + rep(0:2, each = 2)),
    price = c(1, 2, 2, 1, 3, 3)
  )
  data <- layer_data1(
    ggplot(df, aes(time, price)) +
      geom_candlestick(granule = mixtime::days(1L))
  )

  expect_equal(data$change, data$close - data$open)
  expect_equal(data$change, c(1, -1, 0))
})

test_that("orientation follows whichever axis measures time", {
  df <- hourly(2)

  expect_false(layer_data1(
    ggplot(df, aes(time, price)) +
      geom_candlestick(granule = mixtime::days(1L))
  )$flipped_aes[1])

  flipped <- layer_data1(
    ggplot(df, aes(price, time)) +
      geom_candlestick(granule = mixtime::days(1L))
  )
  expect_true(flipped$flipped_aes[1])
  expect_equal(flipped$xlower, c(1, 25))
  expect_equal(flipped$xupper, c(24, 48))

  # An explicit `orientation` wins over the time-typing.
  expect_true(layer_data1(
    ggplot(df, aes(price, time)) +
      geom_candlestick(granule = mixtime::days(1L), orientation = "y")
  )$flipped_aes[1])
})

test_that("values are summarised separately for each group", {
  df <- rbind(
    transform(hourly(2), series = "a"),
    transform(hourly(2), price = price + 100, series = "b")
  )
  data <- layer_data1(
    ggplot(df, aes(time, price, group = series)) +
      geom_candlestick(granule = mixtime::days(1L))
  )

  expect_equal(nrow(data), 4)
  expect_equal(sort(data$open), c(1, 25, 101, 125))
})

test_that("the value axis transformation reaches every summary statistic", {
  # Pins the choice of `lower`/`upper`/`ymin`/`ymax` over names of our own:
  # a position scale only transforms (and only trains its limits on) the
  # aesthetics it claims, and `ggplot_global$y_aes` spells the `y` member of
  # the `lower`/`upper` pair without its prefix. An invented `ylower` would
  # arrive at the geom holding raw values, drawn against transformed
  # positions.
  df <- hourly(2)
  data <- layer_data1(
    ggplot(df, aes(time, price)) +
      geom_candlestick(granule = mixtime::days(1L)) +
      scale_y_log10()
  )

  expect_equal(data$lower, log10(c(1, 25)))
  expect_equal(data$upper, log10(c(24, 48)))
  expect_equal(data$ymin, data$lower)
  expect_equal(data$ymax, data$upper)
})

test_that("`granule` resolves bare granules against the axis's calendar", {
  df <- hourly(1)
  expect_equal(
    layer_data1(
      ggplot(df, aes(time, price)) + geom_candlestick(granule = hour(6L))
    )$n,
    rep(6L, 4)
  )
  # The Gregorian calendar the axis uses has no `week` granule.
  expect_warning(
    ggplot_build(
      ggplot(df, aes(time, price)) + geom_candlestick(granule = week(1L))
    ),
    "no .week. granule"
  )
})

test_that("a layer drawn with `stat = 'identity'` sizes bodies by resolution", {
  df <- daily_ohlc(5)
  data <- layer_data1(
    ggplot(
      df,
      aes(
        time,
        lower = open,
        upper = close,
        ymin = low,
        ymax = high
      )
    ) +
      geom_candlestick(stat = "identity")
  )

  expect_equal(nrow(data), 5)
  expect_equal(data$xmax - data$xmin, rep(0.9, 5))
  expect_equal(data$lower, df$open)
})

test_that("`granule` must be supplied, and must suit the axis", {
  df <- hourly(1)
  expect_error(
    ggplot_build(ggplot(df, aes(time, price)) + geom_candlestick()),
    "must be supplied"
  )
  expect_warning(
    ggplot_build(
      ggplot(data.frame(x = 1:10, y = 1:10), aes(x, y)) +
        geom_candlestick(granule = mixtime::days(1L))
    ),
    "could not be cut against this axis"
  )
})

test_that("values to summarise must be mapped", {
  df <- hourly(1)
  expect_warning(
    ggplot_build(
      ggplot(df, aes(time)) + geom_candlestick(granule = mixtime::days(1L))
    ),
    "needs values to summarise"
  )
})

test_that("candlesticks draw a high-low line and an open-close body", {
  grob <- layer_grob(
    ggplot(hourly(3), aes(time, price)) +
      geom_candlestick(granule = mixtime::days(1L))
  )[[1]]

  expect_length(grob$children, 2)
  segments <- grob$children[[1]]
  rects <- grob$children[[2]]
  expect_s3_class(segments, "segments")
  expect_s3_class(rects, "rect")
  expect_length(rects$x, 3)
})
