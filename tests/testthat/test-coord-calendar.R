calendar_data <- function() {
  x <- 0:41
  data.frame(
    time = as.Date("2025-04-07") + x,
    value = sin(x / 7 * 2 * pi) + x / 40
  )
}

test_that("rows are laid out from cut loops", {
  skip_if_no_r42_graphics()

  p <- ggplot(calendar_data(), aes(x = time, y = value)) +
    geom_line() +
    scale_x_date(date_breaks = "1 day", date_labels = "%a")

  vdiffr::expect_doppelganger(
    "weekly rows",
    p + coord_calendar(time_rows = mixtime::weeks(1L)),
    writer = write_svg_r42
  )
})

test_that("gridlines and axes are not folded into a single row", {
  # Panel decoration describes the row window rather than data within it, so it
  # is replicated across rows as grobs. Applying the row placement to it would
  # squeeze the vertical gridlines and axis labels into the top row.
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    coord_calendar(time_rows = mixtime::weeks(1L))

  built <- ggplot_build(p)
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  bg <- coord$render_bg(params, theme_grey())

  # Each row's grill is drawn in its own viewport, and within that viewport the
  # x gridlines must still span the full height.
  grill <- bg$children[[1]]$children[[1]]
  vgrid <- grill$children[[grep("panel.grid.major.x", names(grill$children))]]
  expect_equal(range(as.numeric(vgrid$y)), c(0, 1))
})

test_that("row layout stacks loops without overlapping", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    coord_calendar(time_rows = mixtime::weeks(1L))

  built <- ggplot_build(p)
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]

  # Six weekly cuts over six weeks of data.
  n_row <- coord$.n_row
  expect_gt(n_row, 1L)

  transformed <- coord$transform(built$data[[1]], params)
  # Every point lies within its own row's horizontal band.
  expect_true(all(transformed$y >= 0 & transformed$y <= 1))
  expect_lt(diff(range(transformed$y)), 1)
})

test_that("the derived row count does not leak between builds", {
  # The coord object outlives a single build, so deriving the row count by
  # accumulating a maximum must be reset each time or a reused coord keeps the
  # largest row count it has ever seen.
  df <- calendar_data()
  coord <- coord_calendar(time_rows = mixtime::weeks(1L))

  invisible(ggplot_build(
    ggplot(df, aes(x = time, y = value)) + geom_line() + coord
  ))
  many <- coord$.n_row

  invisible(ggplot_build(
    ggplot(df[1:5, ], aes(x = time, y = value)) + geom_line() + coord
  ))
  expect_lt(coord$.n_row, many)
})

test_that("coord_calendar rejects unsupported arguments", {
  expect_error(coord_calendar(cols = 1:3), "not yet supported")
  expect_error(coord_calendar(time_cols = "1 week"), "not yet supported")
})

test_that("coord_calendar is unsupported for non-cartesian coords", {
  p <- ggplot(calendar_data(), aes(x = time, y = value)) +
    geom_line() +
    coord_calendar(time_rows = mixtime::weeks(1L), coord = coord_radial())

  expect_error(ggplot_build(p), "does not support")
})
