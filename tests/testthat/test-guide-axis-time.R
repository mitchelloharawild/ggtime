axis_key <- function(p, aes = "x") {
  params <- ggplot_build(p)$layout$panel_params[[1]]
  guide <- params$guides$get_params(aes)
  guide$key
}

test_that("discrete time is bracketed by ticks with centered labels", {
  df <- tibble::tibble(y = 1:4)
  df$x <- mixtime::yearquarter(200:203)
  p <- ggplot(df, aes(x, y)) +
    geom_col() +
    scale_x_mixtime(breaks = mixtime::yearquarter(200:203))

  key <- axis_key(p)
  labels <- vctrs::vec_slice(key, key$.type == "major")
  ticks <- vctrs::vec_slice(key, key$.type == "boundary")

  expect_equal(labels$.label, c("2020 Q1", "2020 Q2", "2020 Q3", "2020 Q4"))

  # Contiguous granules share a boundary, so 4 granules need 5 ticks.
  expect_equal(nrow(ticks), 5L)
  expect_equal(diff(ticks$x), rep(diff(ticks$x)[1], 4))

  # Each label sits midway between the boundaries of its granule.
  expect_equal(labels$x, (ticks$x[-5] + ticks$x[-1]) / 2)
})

axis_ticks <- function(p) {
  key <- axis_key(p)
  key$.value[key$.type == "boundary"]
}
axis_labels <- function(p) {
  key <- axis_key(p)
  key$.label[key$.type == "major"]
}

test_that("every granule is ticked when breaks skip granules", {
  df <- tibble::tibble(y = 1:12)
  df$x <- mixtime::yearmonth(600:611)
  p <- ggplot(df, aes(x, y)) +
    geom_line() +
    scale_x_mixtime(breaks = mixtime::yearmonth(seq(600, 610, by = 5)))

  expect_equal(axis_ticks(p), 600:612)
  key <- axis_key(p)
  expect_equal(key$.value[key$.type == "major"], c(600.5, 605.5, 610.5))
})

test_that("time_breaks of many granules tick each granule", {
  df <- tibble::tibble(y = 1:60)
  df$x <- mixtime::yearmonth(600:659)
  p <- ggplot(df, aes(x, y)) +
    geom_line() +
    scale_x_mixtime(time_breaks = mixtime::years(2L))

  expect_equal(axis_ticks(p), seq(600, 660, by = 12))
  expect_equal(axis_labels(p), c("2020", "2022", "2024"))
})

test_that("default breaks are labelled by the granule they step by", {
  df <- tibble::tibble(y = 1:120)
  df$x <- mixtime::yearmonth(600:719)
  p <- ggplot(df, aes(x, y)) + geom_line()
  expect_equal(axis_ticks(p), seq(600, 720, by = 12))
  expect_equal(axis_labels(p), c("2020", "2022", "2024", "2026", "2028"))
  # The middle of each year
  expect_equal(diff(axis_key(p)$.value[1:2]), 24)
  expect_equal(axis_key(p)$.value[1], 606)

  # Explicit breaks are labelled as given
  expect_equal(
    axis_labels(p + scale_x_mixtime(breaks = mixtime::yearmonth(c(600, 624)))),
    c("2020 Jan", "2022 Jan")
  )
})

test_that("granules are only promoted to simpler names", {
  month <- vecvec::unvecvec(
    mixtime::mixtime(
      mixtime::yearmonth(c(600, 606, 612)),
      discrete = FALSE
    )
  )
  # Each starts a quarter, but "2020 Q1" is not a simpler "2020 Jan"
  expect_null(promote_granule(month))
  expect_equal(
    time_labels_at(month[1], promote_granule(month[c(1, 3)])),
    "2020"
  )
  # The step isn't known from a single break
  expect_null(promote_granule(month[1]))
})

test_that("too many granules are thinned", {
  df <- tibble::tibble(y = 1:120)
  df$x <- mixtime::yearmonth(600:719)
  p <- ggplot(df, aes(x, y)) + geom_line()
  # Quarters fit, and each label starts one
  expect_equal(
    axis_ticks(p + scale_x_mixtime(breaks = mixtime::yearmonth(c(600, 660)))),
    # Including those within the axis expansion
    seq(597, 723, by = 3)
  )
  # No coarser granule starts with February
  expect_equal(
    axis_ticks(p + scale_x_mixtime(breaks = mixtime::yearmonth(c(601, 661)))),
    c(601, 602, 661, 662)
  )
})

test_that("continuous time is left to the standard axis", {
  df <- tibble::tibble(
    # Adding a double makes these instants rather than whole seconds.
    x = as.POSIXct("2020-01-01", tz = "UTC") + (0:4) * 3600,
    y = 1:5
  )
  p <- ggplot(df, aes(x, y)) + geom_line()

  key <- axis_key(p)
  expect_false("boundary" %in% (key$.type %||% "major"))
})

test_that("guide_axis() opts out of granule brackets", {
  df <- tibble::tibble(y = 1:4)
  df$x <- mixtime::yearquarter(200:203)
  p <- ggplot(df, aes(x, y)) +
    geom_col() +
    scale_x_mixtime(
      breaks = mixtime::yearquarter(200:203),
      guide = guide_axis()
    )

  key <- axis_key(p)
  expect_false("boundary" %in% (key$.type %||% "major"))
  # Labels stay at the break which starts the granule.
  expect_equal(key$.value, 200:203)
})

test_that("granules label the granule rather than a position within it", {
  df <- tibble::tibble(y = 1:12)
  df$x <- mixtime::yearmonth(600:611)
  p <- ggplot(df, aes(x, y)) + geom_line()

  # Not "2020 Jan 0.0%", which names an instant at the start of the granule.
  labels <- axis_key(p)$.label
  expect_true(all(is.na(labels) | grepl("^20[0-9]{2} [A-Z][a-z]{2}$", labels)))
})

test_that("time_breaks bracket the granule their label names", {
  df <- tibble::tibble(y = 1:24)
  df$x <- mixtime::yearmonth(600:623)
  for (align in list(aes_nudge(), 0, 1)) {
    p <- ggplot(df, aes(x, y)) +
      geom_line() +
      scale_x_mixtime(
        time_breaks = mixtime::quarters(1L),
        align_discrete = align,
        limits = mixtime::yearmonth(c(600, 612))
      )

    key <- axis_key(p)
    ticks <- vctrs::vec_slice(key, key$.type == "boundary")
    labels <- vctrs::vec_slice(key, key$.type == "major")
    expect_equal(diff(ticks$.value), rep(3, nrow(ticks) - 1L))
    expect_equal(ticks$.value %% 3, rep(0, nrow(ticks)))
    q1 <- labels$.value[labels$.label == "2020 Q2"]
    expect_equal(q1, 604.5)
  }
})

test_that("durations are lengths of time without granules to bracket", {
  df <- tibble::tibble(x = mixtime::months(0:11), y = 1:12)
  key <- axis_key(ggplot(df, aes(x, y)) + geom_line())
  expect_false("boundary" %in% (key$.type %||% "major"))
})

test_that("angular axes are drawn like guide_axis_theta()", {
  df <- tibble::tibble(y = 1:12)
  df$x <- mixtime::yearmonth(600:611)
  p <- ggplot(df, aes(x, y)) + geom_line() + coord_radial()
  theta <- function(p) axis_key(p, "theta")

  expect_no_warning(key <- theta(p))
  expected <- theta(p + guides(theta = guide_axis_theta()))
  expect_equal(key$.value, expected$.value)
  expect_equal(key$.label, expected$.label)
  expect_no_error(ggplotGrob(p))
})
