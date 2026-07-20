scale_labels <- function(p, aes = "x") {
  params <- ggplot_build(p)$layout$panel_params[[1]][[aes]]
  labels <- params$get_labels()
  labels[!is.na(labels)]
}

test_that("durations are labelled as durations", {
  df <- tibble::tibble(x = mixtime::days(1:10), y = 1:10)
  p <- ggplot(df, aes(x, y)) + geom_line()

  # Not "1970-01-03", which measures the duration from the epoch as if it were
  # a time point.
  expect_match(scale_labels(p), "days$")
})

test_that("mixed granularity durations share the finest chronon", {
  df <- tibble::tibble(
    x = c(mixtime::days(1:5), mixtime::hours(c(140, 160))),
    y = 1:7
  )
  p <- ggplot(df, aes(x, y)) + geom_point()

  expect_match(scale_labels(p), "hours$")
})

test_that("durations can't be scaled alongside other modes of time", {
  df <- tibble::tibble(
    x = c(mixtime::days(1:3), mixtime::date("2021-01-01")),
    y = 1:4
  )
  p <- ggplot(df, aes(x, y)) + geom_point()

  expect_error(ggplot_build(p), "alongside other modes of time")
})

test_that("time points are unaffected by duration handling", {
  df <- tibble::tibble(x = mixtime::yearmonth(600:611), y = 1:12)
  p <- ggplot(df, aes(x, y)) + geom_line()

  expect_match(scale_labels(p), "^20[0-9]{2} [A-Z][a-z]{2}")
})
