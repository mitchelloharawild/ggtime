calendar_data <- function() {
  x <- 0:41
  data.frame(
    time = as.Date("2025-04-07") + x,
    value = sin(x / 7 * 2 * pi) + x / 40
  )
}

#' A completed theme, as the render methods are handed one at draw time
#'
#' Registered element defaults -- the calendar's own among them -- are merged
#' into a theme only when it is completed, so a bare `theme_grey()` is not what
#' a plot actually renders with: the granule fills would inherit
#' `panel.background` rather than defaulting to blank, and the rules' `rel()`
#' linewidths would be read as absolute ones.
#' @noRd
calendar_theme <- function(theme = ggplot2::theme_grey()) {
  ggplot2::complete_theme(theme)
}

#' Every granule grob drawn anywhere in a rendered layer, by name
#'
#' A label is a `titleGrob`, i.e. a named `gTree` with the text as its child, so
#' a grob's own name is checked before descending into it.
#' @noRd
calendar_grobs <- function(grob) {
  found <- if (grepl("^ggtime\\.calendar\\.", grob$name %||% "")) {
    stats::setNames(list(grob), grob$name)
  } else {
    list()
  }
  if (inherits(grob, "gTree")) {
    found <- c(
      found,
      # Unnamed, or `unlist()` would prefix each grob's name with its parent's.
      unlist(unname(lapply(grob$children, calendar_grobs)), recursive = FALSE)
    )
  }
  found %||% list()
}

calendar_rule_names <- function(grob) {
  names(calendar_grobs(grob)) %||% character()
}

#' The panel's own gridlines actually drawn in a tree, by element name
#'
#' A blanked element still leaves a named `zeroGrob()` behind, so drawing
#' nothing has to be told apart from drawing a line; and `ggname()` suffixes an
#' element's name with the grob's own, which is trimmed back off.
#' @noRd
panel_grid_names <- function(grob) {
  if (inherits(grob, "zeroGrob")) {
    return(character())
  }
  drawn <- c(
    grob$name %||% character(),
    unlist(lapply(grob$children %||% list(), panel_grid_names)) %||% character()
  )
  unique(sub("\\.\\..*$", "", grep("^panel\\.grid\\.", drawn, value = TRUE)))
}

#' The labels drawn for one granule, in the order they were placed
#' @noRd
calendar_labels <- function(grob, granule) {
  drawn <- calendar_grobs(grob)[[paste0("ggtime.calendar.", granule, ".text")]]
  if (is.null(drawn)) {
    return(NULL)
  }
  drawn$children[[1]]$label
}

test_that("rows are laid out from cut loops", {
  skip_if_no_r42_graphics()

  p <- ggplot(calendar_data(), aes(x = time, y = value)) +
    geom_line() +
    scale_x_date(date_breaks = "1 day", date_labels = "%a")

  vdiffr::expect_doppelganger(
    "weekly rows",
    p + coord_calendar(rows = mixtime::weeks(1L), cols = NULL),
    writer = write_svg_r42
  )
})

test_that("columns are laid out from cut loops", {
  skip_if_no_r42_graphics()

  p <- ggplot(calendar_data(), aes(x = time, y = value)) +
    geom_line() +
    scale_x_date(date_breaks = "1 day", date_labels = "%a")

  vdiffr::expect_doppelganger(
    "monthly columns",
    p + coord_calendar(rows = mixtime::weeks(1L), cols = mixtime::months(1L)),
    writer = write_svg_r42
  )
})

test_that("panes and columns are separated by gaps", {
  skip_if_no_r42_graphics()

  x <- 0:167
  df <- data.frame(
    time = as.Date("2025-01-06") + x,
    value = sin(x / 7 * 2 * pi) + x / 160
  )
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    scale_x_date(date_breaks = "1 day", date_labels = "%a")

  vdiffr::expect_doppelganger(
    "pane and column gaps",
    p +
      coord_calendar(
        rows = mixtime::weeks(1L),
        panes = mixtime::months(1L),
        cols = mixtime::quarters(1L)
      ),
    writer = write_svg_r42
  )

  # `is_flipped` threads through the layout, the rules and the axes, so it gets
  # a case of its own.
  vdiffr::expect_doppelganger(
    "flipped pane and column gaps",
    ggplot(df, aes(y = time, x = value)) +
      geom_path() +
      scale_y_date(date_breaks = "1 day", date_labels = "%a") +
      coord_calendar(
        time = "y",
        rows = mixtime::weeks(1L),
        panes = mixtime::months(1L),
        cols = mixtime::quarters(1L)
      ),
    writer = write_svg_r42
  )
})

test_that("granules are labelled within the calendar", {
  skip_if_no_r42_graphics()

  x <- 0:167
  df <- data.frame(
    time = as.Date("2025-01-06") + x,
    value = sin(x / 7 * 2 * pi) + x / 160
  )
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    scale_x_date(date_breaks = "1 day", date_labels = "%a")

  # Cell labels are on by default; block and row labels opt in, and each lands
  # in a corner of its own rather than on top of the others. `pane` is off so
  # that the monthly boundary is ruled as a block rather than gapped as a pane.
  vdiffr::expect_doppelganger(
    "granule labels",
    p +
      coord_calendar(
        rows = mixtime::weeks(1L),
        blocks = mixtime::months(1L),
        panes = NULL,
        cols = mixtime::quarters(1L),
        label_blocks = "{cyc(month, year, label = TRUE, abbreviate = TRUE)}",
        label_rows = "{cyc(cal_isoweek$week, year)}"
      ),
    writer = write_svg_r42
  )
})

test_that("gridlines and axes are not folded into a single row", {
  # Panel decoration describes the row window rather than data within it, so it
  # is repeated across rows. Applying the row placement to it would squeeze the
  # vertical gridlines and axis labels into the top row.
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    coord_calendar(rows = mixtime::weeks(1L), cols = NULL)

  built <- ggplot_build(p)
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  bg <- coord$render_bg(params, calendar_theme())

  # The cell gridlines are one grob covering the whole grid rather than one
  # per tile, so a tile's worth of them is a block of segments within it: each
  # block must still span its own row from top to bottom.
  vgrid <- calendar_grobs(bg)[["ggtime.calendar.cell.line"]]
  n_break <- length(params$cell_breaks)
  expect_gt(n_break, 0L)
  # One column per tile, tiles ordered row-major.
  y <- matrix(as.numeric(vgrid$y), nrow = 2L * n_break)
  expect_equal(ncol(y), coord$.grid$n_row * coord$.grid$n_col)

  layout <- coord$grid_layout(params)
  tile_row <- rep(seq_len(coord$.grid$n_row), each = coord$.grid$n_col)
  expect_equal(apply(y, 2L, min), layout$row$y[tile_row])
  expect_equal(
    apply(y, 2L, max),
    (layout$row$y + layout$row$height)[tile_row]
  )
})

test_that("the panel's own grid is dropped", {
  # A calendar repeats the panel's decoration in every cell, so the panel's own
  # grid is dropped entirely: the time axis is ruled by `cell` instead, and
  # the calendar's rows are what rule the other way.
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    coord_calendar(rows = mixtime::weeks(1L), cols = NULL)

  built <- ggplot_build(p)
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]

  grid <- panel_grid_names(coord$render_bg(params, calendar_theme()))
  expect_equal(grid, character())

  # Flipped, the axes swap, and neither brings its gridlines back.
  flipped <- ggplot_build(
    ggplot(df, aes(y = time, x = value)) +
      geom_line() +
      coord_calendar(rows = mixtime::weeks(1L), cols = NULL, time = "y")
  )
  grid <- panel_grid_names(flipped$plot$coordinates$render_bg(
    flipped$layout$panel_params[[1]],
    calendar_theme()
  ))
  expect_equal(grid, character())
})

test_that("a theme cannot reinstate the gridlines a calendar drops", {
  # The panel's grid is blanked at each specific element name, so setting the
  # more specific element does not slip past the parent it inherits from.
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    coord_calendar(rows = mixtime::weeks(1L), cols = NULL) +
    theme(
      panel.grid.minor.y = element_line(colour = "red"),
      panel.grid.major.x = element_line(colour = "red"),
      panel.grid.major.y = element_line(colour = "red")
    )

  built <- ggplot_build(p)
  bg <- built$plot$coordinates$render_bg(
    built$layout$panel_params[[1]],
    ggplot2:::plot_theme(p)
  )
  expect_equal(panel_grid_names(bg), character())
})

test_that("row layout stacks loops without overlapping", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    coord_calendar(rows = mixtime::weeks(1L), cols = NULL)

  built <- ggplot_build(p)
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]

  # Six weekly cuts over six weeks of data, and a single column.
  expect_gt(coord$.grid$n_row, 1L)
  expect_equal(coord$.grid$n_col, 1L)

  transformed <- coord$transform(built$data[[1]], params)
  # Every point lies within its own row's horizontal band.
  expect_true(all(transformed$y >= 0 & transformed$y <= 1))
  expect_lt(diff(range(transformed$y)), 1)
})

test_that("column layout stacks loops side by side without overlapping", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    coord_calendar(rows = NULL, cols = mixtime::weeks(1L))

  built <- ggplot_build(p)
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]

  # Six weekly columns over six weeks of data, and a single row per column.
  expect_gt(coord$.grid$n_col, 1L)
  expect_equal(coord$.grid$n_row, 1L)

  transformed <- coord$transform(built$data[[1]], params)
  # Every point lies within its own column's vertical band.
  expect_true(all(transformed$x >= 0 & transformed$x <= 1))
  expect_lt(diff(range(transformed$x)), 1)
})

#' Where each of a day's points lands in the calendar grid
#'
#' The whole point of a calendar layout is that a value can be read off the
#' grid, so the grid position of every day is derived the way a reader derives
#' it: from the npc coordinates `transform()` produces and the tile geometry
#' the panel is drawn with, rather than from the coord's internal cuts.
#' @param dates One point per date, in order.
#' @param ... Passed to `coord_calendar()`.
#' @returns A data frame of `date` and its 1-based `col`, `row` and `cell`
#'   (the position within the row, counted in `cells` per row).
#' @noRd
calendar_places <- function(dates, ..., cells = 7L) {
  df <- data.frame(time = dates, value = 1)
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) + geom_point() + coord_calendar(...)
  )
  coord <- built$plot$coordinates
  panel_params <- built$layout$panel_params[[1]]
  pos <- coord$transform(
    built$data[[1]][, c("x", "y")],
    panel_params
  )

  layout <- coord$grid_layout(panel_params)
  col <- findInterval(pos$x, layout$col$x)
  within <- (pos$x - layout$col$x[col]) / layout$col$width[col]
  data.frame(
    date = dates,
    col = col,
    # Rows are indexed from the top, against the npc `y` they are placed in.
    row = length(layout$row$y) - findInterval(pos$y, rev(layout$row$y)) + 1L,
    cell = floor(within * cells + 1e-9) + 1L
  )
}

test_that("a day is placed in the calendar cell a printed calendar puts it in", {
  # A quarter of daily data, laid out as ISO weeks within months: the layout
  # every calendar plot is read as, and the one to check against a calendar.
  dates <- seq(as.Date("2015-01-01"), as.Date("2015-03-31"), by = "day")
  places <- calendar_places(
    dates,
    rows = mixtime::weeks(1L),
    cols = mixtime::months(1L),
    panes = NULL
  )

  # A cell of the grid is a weekday, in every column alike -- the axis is drawn
  # once beneath the whole calendar, so a column that shifted the weekdays
  # along would be read off wrongly. `weeks()` is ISO, so Monday opens a row.
  expect_equal(places$cell, as.integer(format(dates, "%u")))
  # A day is in the column of its own month, and in the week of the month that
  # a calendar prints it in, counting the week the 1st falls in as the first.
  expect_equal(places$col, as.integer(format(dates, "%m")))
  first <- as.Date(format(dates, "%Y-%m-01"))
  weeks_in <- as.integer(dates - first + (as.integer(format(first, "%u")) - 1L))
  expect_equal(places$row, weeks_in %/% 7L + 1L)

  # March 2015 runs Sunday to Tuesday, so it needs six rows where January and
  # February need five; the shared grid takes the largest.
  expect_equal(max(places$row), 6L)
})

test_that("a row holds only the days of one column and pane", {
  # The granule hierarchy is `col` over `pane` over `block` over `row`: a week
  # running from one month into the next belongs to two panes, so it is cut in
  # two and each half is a row of its own pane rather than one row of both.
  dates <- seq(as.Date("2015-01-01"), as.Date("2015-06-30"), by = "day")
  places <- calendar_places(
    dates,
    rows = mixtime::weeks(1L),
    panes = mixtime::months(1L),
    cols = mixtime::quarters(1L)
  )

  # No row of the grid mixes the days of two months.
  months <- split(format(places$date, "%Y-%m"), places[c("col", "row")])
  expect_true(all(lengths(lapply(months, unique)) <= 1L))
  # A day is still under its own weekday, cut row or not.
  expect_equal(places$cell, as.integer(format(dates, "%u")))
  # A day is in the column of its own quarter.
  expect_equal(
    places$col,
    (as.integer(format(dates, "%m")) - 1L) %/% 3L + 1L
  )

  # The day a month opens on starts a row, and the day before it closes the one
  # above -- unless the month opens a column, which starts the grid again.
  opens <- which(format(places$date, "%d") == "01")[-1L]
  stays <- places$col[opens] == places$col[opens - 1L]
  expect_equal(places$row[opens[stays]], places$row[opens[stays] - 1L] + 1L)
  expect_true(all(places$row[opens[!stays]] == 1L))
})

test_that("panes are aligned across the calendar's columns", {
  dates <- seq(as.Date("2015-01-01"), as.Date("2015-06-30"), by = "day")
  built <- ggplot_build(
    ggplot(data.frame(time = dates, value = 1), aes(x = time, y = value)) +
      geom_point() +
      coord_calendar(
        rows = mixtime::weeks(1L),
        panes = mixtime::months(1L),
        cols = mixtime::quarters(1L)
      )
  )
  coord <- built$plot$coordinates

  # A pane is as tall as the most rows any column puts in it, so that one gap
  # separates the same two months in every column. January, February, April and
  # May need five rows each once split; March and June need six and five, so
  # the third pane takes six.
  expect_equal(coord$.grid$pane_rows, c(5L, 5L, 6L))
  expect_equal(coord$.grid$n_row, 16L)
  expect_equal(coord$.grid$row_pane, rep(1:3, c(5L, 5L, 6L)))
})

test_that("calendar_pieces cuts a row at every coarser boundary", {
  # Rows of 3 over columns of 10 and panes of 5, so that neither the column nor
  # the pane boundaries fall on a row boundary; the grid closes at 21, past the
  # last column, as cutting a range always does.
  col_cuts <- c(0, 10, 20)
  row_cuts <- seq(0, 21, by = 3)
  pane_cuts <- seq(0, 20, by = 5)
  # The row grid rounds out to 21, but the calendar closes with its columns:
  # the rest of that row is time no column of it has room for.
  close <- calendar_close(col_cuts, row_cuts, pane_cuts, NULL, has_col = TRUE)
  expect_equal(close, 20)

  pieces <- calendar_pieces(
    col_cuts = col_cuts,
    row_cuts = row_cuts,
    pane_cuts = pane_cuts,
    block_cuts = NULL,
    close = close
  )

  # A piece per row of each column, cut wherever a column or pane starts.
  expect_equal(pieces$cuts, c(0, 3, 5, 6, 9, 10, 12, 15, 18, 20))
  # Each is folded onto the row it is a part of, not onto its own start, so a
  # piece cut short still measures where in the row it sits.
  expect_equal(pieces$origins, c(0, 3, 3, 6, 9, 9, 12, 15, 18))
  expect_equal(pieces$col, c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L))
  # Panes and rows are counted within the column, so the grid can align the
  # second pane of every column at the same row.
  expect_equal(pieces$pane, c(1L, 1L, 2L, 2L, 2L, 1L, 1L, 2L, 2L))
  expect_equal(pieces$pane_row, c(1L, 2L, 1L, 2L, 3L, 1L, 2L, 1L, 2L))
  expect_equal(
    pieces$pane_start,
    c(TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)
  )
  expect_null(pieces$block_start)

  # Two rows in the first pane of either column and three in the second, so the
  # grid is five rows deep and each column's second pane starts at the third.
  expect_equal(calendar_pane_rows(pieces), c(2L, 3L))
  expect_equal(
    calendar_piece_rows(pieces, c(2L, 3L)),
    c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L)
  )
})

test_that("cuts a second apart survive on a modern time axis", {
  # The tolerance cuts are compared with has to scale with how far apart they
  # are, not with how far the epoch is from zero: scaled to the epoch, it is
  # ~1.8 seconds on any present-day `POSIXct` axis, and every row of a calendar
  # with rows shorter than that is silently merged away.
  epoch <- as.numeric(as.POSIXct("2015-01-01", tz = "UTC"))
  col_cuts <- epoch + c(0, 60, 120)
  row_cuts <- epoch + seq(0, 120, by = 1)

  close <- calendar_close(col_cuts, row_cuts, NULL, NULL, has_col = TRUE)
  expect_equal(close, epoch + 120)

  pieces <- calendar_pieces(
    col_cuts = col_cuts,
    row_cuts = row_cuts,
    pane_cuts = NULL,
    block_cuts = NULL,
    close = close
  )
  # A one-second row of each of the two one-minute columns.
  expect_equal(length(pieces$cuts) - 1L, 120L)
  expect_equal(calendar_pane_rows(pieces), 60L)
  expect_equal(pieces$col, rep(1:2, each = 60L))

  # The other half of the contract: a boundary reached by flooring at two
  # different granules can differ in the last bit or two (1e-6 here is a few
  # ULPs at this epoch), and those two cuts must still count as one rather
  # than opening a row of nothing.
  nudged <- calendar_pieces(
    col_cuts = col_cuts,
    row_cuts = row_cuts,
    pane_cuts = NULL,
    block_cuts = col_cuts + 1e-6,
    close = close
  )
  expect_equal(nudged$cuts, pieces$cuts)
})

test_that("a calendar of one-second rows is laid out row by row", {
  # End to end, through the granules rather than the numeric cuts: nothing else
  # in the suite exercises a granule finer than a day.
  time <- mixtime::datetime(
    seq(as.POSIXct("2015-01-01", tz = "UTC"), by = "1 sec", length.out = 120)
  )
  built <- ggplot_build(
    ggplot(data.frame(time = time, value = seq_len(120)), aes(time, value)) +
      geom_point() +
      coord_calendar(
        cells = NULL,
        rows = mixtime::seconds(1L),
        blocks = NULL,
        panes = NULL,
        cols = mixtime::minutes(1L)
      )
  )
  params <- built$layout$panel_params[[1]]
  expect_equal(params$grid$n_col, 2L)
  expect_equal(params$grid$n_row, 60L)
  expect_equal(length(params$pieces$cuts) - 1L, 120L)
})

test_that("calendar_close() falls back to `end` when no cut reaches it", {
  # Every current caller guarantees a cut at or past `end`, but if `row_cuts`,
  # `pane_cuts` and `block_cuts` are all absent and `has_col = FALSE` drops
  # `col_cuts` too, the filtered vector is empty. `min()` on that would
  # silently return `Inf`; the guard should return `end` instead.
  col_cuts <- c(0, 10, 20)
  close <- calendar_close(
    col_cuts,
    row_cuts = NULL,
    pane_cuts = NULL,
    block_cuts = NULL,
    has_col = FALSE
  )
  expect_equal(close, 20)
  expect_false(is.infinite(close))
})

#' Where each of a day's points lands in a flipped calendar's grid
#'
#' `calendar_places()` for `time = "y"`. A flipped calendar transposes the
#' grid *and* reflects each of its dimensions, so that it still reads the way
#' a calendar reads: rows run left to right along `x`, and columns run top to
#' bottom down `y`, with time still running up the panel within a column.
#' Each dimension therefore sits at `1 - (pos + extent)` on the axis it now
#' runs along, which is all this has to undo -- there is no reversed index
#' left over.
#' @inheritParams calendar_places
#' @noRd
calendar_places_flipped <- function(dates, ..., cells = 7L) {
  df <- data.frame(time = dates, value = 1)
  built <- ggplot_build(
    ggplot(df, aes(y = time, x = value)) +
      geom_point() +
      coord_calendar(time = "y", ...)
  )
  coord <- built$plot$coordinates
  panel_params <- built$layout$panel_params[[1]]
  pos <- coord$transform(
    built$data[[1]][, c("x", "y")],
    panel_params
  )

  layout <- coord$grid_layout(panel_params)
  # The left edge of each row, and the bottom edge of each column.
  row_x <- 1 - (layout$row$y + layout$row$height)
  col_y <- 1 - (layout$col$x + layout$col$width)
  # Columns are indexed from the top, so they are looked up by how far down
  # the panel a point sits.
  col <- findInterval(1 - pos$y, layout$col$x)
  within <- (pos$y - col_y[col]) / layout$col$width[col]
  data.frame(
    date = dates,
    col = col,
    row = findInterval(pos$x, row_x),
    cell = floor(within * cells + 1e-9) + 1L
  )
}

test_that("the calendar grid is arranged the same way flipped", {
  dates <- seq(as.Date("2015-01-01"), as.Date("2015-03-31"), by = "day")
  args <- list(
    rows = mixtime::weeks(1L),
    cols = mixtime::months(1L),
    panes = NULL
  )

  expect_equal(
    inject(calendar_places_flipped(dates, !!!args)),
    inject(calendar_places(dates, !!!args))
  )
})

test_that("a flipped calendar reads left to right and top to bottom", {
  # A transpose on its own is a reflection about the main diagonal, which
  # reverses *both* reading directions: the first week would be drawn at the
  # right edge with later weeks marching leftwards, and January would sit
  # below February. Pinned here on the raw npc positions, so that the grid
  # cannot quietly turn back to front again.
  dates <- as.Date(c(
    "2015-01-01",
    "2015-01-02",
    "2015-01-08",
    "2015-02-01",
    "2015-02-08"
  ))
  built <- ggplot_build(
    ggplot(data.frame(time = dates, value = 1), aes(y = time, x = value)) +
      geom_point() +
      coord_calendar(
        time = "y",
        rows = mixtime::weeks(1L),
        cols = mixtime::months(1L),
        panes = NULL
      )
  )
  pos <- built$plot$coordinates$transform(
    built$data[[1]][, c("x", "y")],
    built$layout$panel_params[[1]]
  )
  names(pos) <- c("x", "y")
  rownames(pos) <- format(dates)

  # Rows run left to right: a later week of the same month is further right.
  expect_lt(pos["2015-01-01", "x"], pos["2015-01-08", "x"])
  expect_lt(pos["2015-02-01", "x"], pos["2015-02-08", "x"])
  # The first row of the grid starts at the left edge rather than the right.
  expect_lt(pos["2015-01-01", "x"], 0.5)

  # Columns run top to bottom: January sits above February.
  expect_gt(pos["2015-01-01", "y"], pos["2015-02-01", "y"])
  expect_gt(pos["2015-01-08", "y"], pos["2015-02-08", "y"])
  # The first column of the grid starts at the top edge rather than the
  # bottom, and time still runs up the panel within a row of it.
  expect_gt(pos["2015-01-01", "y"], 0.5)
  expect_gt(pos["2015-01-02", "y"], pos["2015-01-01", "y"])
})

test_that("the derived row count does not leak between builds", {
  # The coord object outlives a single build, so deriving the row count by
  # accumulating a maximum must be reset each time or a reused coord keeps
  # the largest row count it has ever seen. `cols = NULL` keeps a single
  # column spanning the data's own range, so shrinking the data directly
  # shrinks the row count (unlike with `cols` set, where a column's width --
  # and so the row count -- comes from the `cols` granule, not the data).
  df <- calendar_data()
  coord <- coord_calendar(rows = mixtime::weeks(1L), cols = NULL)

  invisible(ggplot_build(
    ggplot(df, aes(x = time, y = value)) + geom_line() + coord
  ))
  many_row <- coord$.grid$n_row

  invisible(ggplot_build(
    ggplot(df[1:5, ], aes(x = time, y = value)) + geom_line() + coord
  ))
  expect_lt(coord$.grid$n_row, many_row)
})

test_that("the derived column count does not leak between builds", {
  df <- calendar_data()
  coord <- coord_calendar(rows = NULL, cols = mixtime::weeks(1L))

  invisible(ggplot_build(
    ggplot(df, aes(x = time, y = value)) + geom_line() + coord
  ))
  many_col <- coord$.grid$n_col

  invisible(ggplot_build(
    ggplot(df[1:5, ], aes(x = time, y = value)) + geom_line() + coord
  ))
  expect_lt(coord$.grid$n_col, many_col)
})

#' All grobs of a given grid class found anywhere in a grob tree
#' @noRd
grobs_of_class <- function(grob, class) {
  found <- if (inherits(grob, class)) list(grob) else list()
  if (inherits(grob, "gTree")) {
    found <- c(
      found,
      unlist(
        lapply(grob$children, grobs_of_class, class = class),
        recursive = FALSE
      )
    )
  }
  found
}

test_that("blanked panel grid elements are not tiled as zeroGrobs", {
  # The panel's own grid is blanked (see "the panel's own grid is dropped"
  # above), which leaves a zeroGrob per blanked element in the parent's
  # render_bg() output. Tiling strips those, rather than carrying dead grobs
  # across the grid for them only to be walked and skipped at draw time.
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    coord_calendar(rows = mixtime::weeks(1L), cols = NULL)

  built <- ggplot_build(p)
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]

  bg <- coord$render_bg(params, calendar_theme())
  expect_equal(length(grobs_of_class(bg, "zeroGrob")), 0L)
})

test_that("a build is not corrupted by drawing after a later build reused its coord", {
  # The coord object outlives a single build and can be shared by more than
  # one plot (a template, a `patchwork`, a `ggplot_build()` result drawn
  # later). The layout state a build derives is read back only at *draw*
  # time (`ggplot_gtable()`), so building a second, smaller plot against the
  # same coord -- even though its own draw never happens -- must not corrupt
  # the first plot's draw.
  make_data <- function(n) {
    data.frame(
      time = as.Date("2020-01-01") + seq_len(n) - 1L,
      value = seq_len(n)
    )
  }
  coord <- coord_calendar(rows = mixtime::weeks(1L), cols = NULL)
  p_big <- ggplot(make_data(200), aes(x = time, y = value)) +
    geom_point() +
    coord
  p_small <- ggplot(make_data(10), aes(x = time, y = value)) +
    geom_point() +
    coord

  built_big <- ggplot_build(p_big)
  invisible(ggplot_build(p_small))

  gt <- ggplot_gtable(built_big)
  panel <- gt$grobs[[which(gt$layout$name == "panel")[1]]]
  points <- grobs_of_class(panel, "points")
  expect_length(points, 1L)

  x <- grid::convertX(points[[1]]$x, "npc", valueOnly = TRUE)
  y <- grid::convertY(points[[1]]$y, "npc", valueOnly = TRUE)
  expect_true(all(is.finite(x)))
  expect_true(all(is.finite(y)))
})

test_that("self$limits is restored even if the parent setup_panel_params() errors", {
  # `setup_panel_params()` temporarily overwrites `self$limits` with the row
  # window so the parent's expansion/breaks/limits logic applies to what is
  # actually drawn, then restores it. That restore has to happen even if the
  # parent call errors, or the coord is left permanently holding the row
  # window as its user limits, corrupting every later build.
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_point() +
    coord_calendar(rows = mixtime::weeks(1L), cols = NULL)
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
  # row window. Erroring only on the second call exercises the restore
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

test_that("cell and block granules can be disabled", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    coord_calendar(
      rows = mixtime::weeks(1L),
      cols = NULL,
      cells = NULL,
      blocks = NULL
    )

  built <- ggplot_build(p)
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]

  expect_null(params$cell_breaks)
  expect_null(params$block_cuts)

  # No cell/block rules drawn, but rows are still separated from each other.
  rules <- calendar_rule_names(coord$render_bg(params, calendar_theme()))
  expect_false(any(grepl("cell|block", rules)))
  expect_true("ggtime.calendar.row.line" %in% rules)
})

test_that("cell and block granules add extra gridline layers", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    coord_calendar(
      rows = mixtime::weeks(1L),
      cols = NULL,
      cells = mixtime::days(1L),
      blocks = mixtime::months(1L),
      # A default monthly pane would gap this boundary instead of ruling it.
      panes = NULL
    )

  built <- ggplot_build(p)
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]

  expect_false(is.null(params$cell_breaks))
  expect_false(is.null(params$block_cuts))

  # The block rule is layered on top of (rather than tiled into) the row x
  # column grid, so it is a sibling of the tiled grid as a whole rather than
  # part of a tile. Data spans April into May, so there is a block boundary to
  # draw.
  bg <- coord$render_bg(params, calendar_theme())
  expect_true("ggtime.calendar.block.line" %in% calendar_rule_names(bg))

  # The grid itself is drawn as geometry rather than replicated grobs: one
  # rect describing every tile's background, and one polyline describing every
  # tile's cell rules.
  n_tile <- coord$.grid$n_row * coord$.grid$n_col
  rects <- grobs_of_class(bg, "rect")
  expect_length(rects, 1L)
  expect_length(as.numeric(rects[[1]]$x), n_tile)

  expect_equal(sum(calendar_rule_names(bg) == "ggtime.calendar.cell.line"), 1L)
  cell_rules <- calendar_grobs(bg)[["ggtime.calendar.cell.line"]]
  expect_equal(
    length(cell_rules$id.lengths),
    n_tile * length(params$cell_breaks)
  )
})

test_that("the drawn background does not grow with the grid", {
  # A calendar's decoration is the same handful of shapes in every tile, so it
  # is drawn as geometry (one rect and one polyline over the whole grid)
  # rather than replicated per tile. The number of grobs the background costs
  # is therefore a small constant, whether the grid holds six tiles or a
  # couple of hundred -- which is what makes the draw cost independent of the
  # size of the calendar.
  calendar <- function(df, ...) {
    built <- ggplot_build(
      ggplot(df, aes(x = time, y = value)) + geom_line() + coord_calendar(...)
    )
    list(
      coord = built$plot$coordinates,
      params = built$layout$panel_params[[1]]
    )
  }
  background <- function(plot, theme) {
    bg <- plot$coord$render_bg(plot$params, calendar_theme(theme))
    list(
      tiles = plot$coord$.grid$n_row * plot$coord$.grid$n_col,
      # Every grob inherits "grob", so this counts the whole tree.
      grobs = length(grobs_of_class(bg, "grob")),
      rects = length(grobs_of_class(bg, "rect")),
      cell_rules = sum(calendar_rule_names(bg) == "ggtime.calendar.cell.line")
    )
  }

  small_plot <- calendar(
    calendar_data(),
    rows = mixtime::weeks(1L),
    cols = NULL
  )
  big_df <- data.frame(
    time = seq(as.Date("2022-01-01"), as.Date("2024-12-31"), by = "day")
  )
  big_df$value <- seq_len(nrow(big_df))
  big_plot <- calendar(
    big_df,
    cells = mixtime::days(1L),
    rows = mixtime::days(7L),
    panes = mixtime::months(1L),
    cols = mixtime::quarters(1L)
  )

  # The fast path recognises the shapes a theme's panel decoration is made of,
  # so it is checked against more than the default theme: a theme that draws a
  # border, one that draws the panel over the data, and one that draws almost
  # nothing must all keep the constant cost, not quietly fall back to a grob
  # per tile.
  themes <- list(
    grey = theme_grey(),
    bw = theme_bw(),
    linedraw = theme_linedraw(),
    dark = theme_dark(),
    minimal = theme_minimal(),
    ontop = theme_grey() + theme(panel.ontop = TRUE),
    bordered = theme_grey() +
      theme(panel.background = element_rect(fill = NA, colour = "black")),
    # A granule fill is one vectorised rect however many instances it has, so
    # it must not push the panel's own decoration off the geometry path.
    filled = theme_grey() +
      theme(ggtime.calendar.cell.background = element_rect(fill = "grey90"))
  )

  for (name in names(themes)) {
    small <- background(small_plot, themes[[name]])
    big <- background(big_plot, themes[[name]])

    # The grids are of very different sizes ...
    expect_gt(big$tiles, 20 * small$tiles)
    # ... but cost the same, constant, handful of grobs to draw.
    expect_equal(big$rects, small$rects, label = paste0(name, ": rects"))
    expect_equal(
      big$cell_rules,
      small$cell_rules,
      label = paste0(name, ": cell rules")
    )
    expect_lt(big$grobs, 2 * small$grobs, label = paste0(name, ": grobs"))
    expect_lt(big$grobs, 20L, label = paste0(name, ": grobs"))
  }
})

test_that("decoration that cannot be drawn as geometry is copied per tile", {
  # Tiling by geometry only knows the shapes a calendar itself draws; a
  # wrapped coord's decoration could be anything, and anything else still has
  # to be drawn -- as a copy of the grob in every tile's own viewport.
  df <- calendar_data()
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(rows = mixtime::weeks(1L), cols = NULL)
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  ctx <- calendar_render_context(coord, params, calendar_theme())

  tiled <- tile_grob_in_grid(circleGrob(), ctx)
  expect_length(grobs_of_class(tiled, "circle"), coord$.grid$n_row)

  # One tile's viewport each, in the tile's own place, rather than all of them
  # drawn over each other.
  layout <- coord$grid_layout(params)
  expect_equal(
    vapply(tiled$children, function(g) as.numeric(g$vp$y), numeric(1L)),
    layout$row$y,
    ignore_attr = TRUE
  )
})

test_that("a block rule replaces the row rule at the boundary it falls on", {
  df <- calendar_data()
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(
        rows = mixtime::weeks(1L),
        blocks = mixtime::months(1L),
        # A default monthly pane would gap this boundary instead of ruling it.
        panes = NULL,
        cols = NULL
      )
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  bg <- coord$render_bg(params, calendar_theme())

  rules <- calendar_grobs(bg)
  row_rule <- rules[["ggtime.calendar.row.line"]]
  block_rule <- rules[["ggtime.calendar.block.line"]]

  # Five interior boundaries between six rows, each ruled exactly once.
  expect_equal(
    length(row_rule$id.lengths) + length(block_rule$id.lengths),
    coord$.grid$n_row - 1L
  )
  expect_equal(length(block_rule$id.lengths), 1L)
  # The two never coincide.
  expect_false(any(as.numeric(block_rule$y) %in% as.numeric(row_rule$y)))
})

test_that("pane gaps replace the rules they separate", {
  df <- data.frame(
    time = as.Date("2025-01-06") + 0:167,
    value = seq_len(168L)
  )
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(
        rows = mixtime::weeks(1L),
        panes = mixtime::months(1L),
        blocks = mixtime::months(1L),
        cols = mixtime::quarters(1L)
      )
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]

  expect_false(is.null(coord$.grid$row_pane))
  expect_equal(length(coord$.grid$row_pane), coord$.grid$n_row)

  layout <- coord$grid_layout(params)
  gaps <- layout$row$y[-coord$.grid$n_row] -
    (layout$row$y[-1] + layout$row$height[-1])
  expect_equal(sum(gaps > 1e-9), sum(diff(coord$.grid$row_pane) != 0L))

  # `block` and `pane` are the same granule here, so every block boundary is
  # already separated by a gap and no block rule is left to draw.
  rules <- calendar_rule_names(coord$render_bg(params, calendar_theme()))
  expect_false("ggtime.calendar.block.line" %in% rules)
})

test_that("rules stop at a column gap", {
  df <- data.frame(time = as.Date("2025-01-06") + 0:167, value = seq_len(168L))
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(rows = mixtime::weeks(1L), cols = mixtime::months(1L))
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  bg <- coord$render_bg(params, calendar_theme())
  layout <- coord$grid_layout(params)

  row_rule <- calendar_grobs(bg)[["ggtime.calendar.row.line"]]
  # One rule per row boundary per column, each spanning its own column only.
  expect_equal(
    length(row_rule$id.lengths),
    (coord$.grid$n_row - 1L) * coord$.grid$n_col
  )
  expect_setequal(
    unique(as.numeric(row_rule$x)),
    c(layout$col$x, layout$col$x + layout$col$width)
  )
})

test_that("coord_calendar defaults produce a sensible layout without arguments", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) + geom_line() + coord_calendar()

  built <- ggplot_build(p)
  coord <- built$plot$coordinates
  expect_gt(coord$.grid$n_row, 1L)

  params <- built$layout$panel_params[[1]]
  transformed <- coord$transform(built$data[[1]], params)
  expect_true(all(transformed$x >= 0 & transformed$x <= 1))
  expect_true(all(transformed$y >= 0 & transformed$y <= 1))
})

test_that("calendar_layout tiles the panel seamlessly without spacing", {
  layout <- calendar_layout(n_row = 4L, n_col = 3L)

  expect_equal(layout$col$width, rep(1 / 3, 3))
  expect_equal(layout$col$x, c(0, 1 / 3, 2 / 3))
  expect_equal(layout$row$height, rep(1 / 4, 4))
  # Rows are indexed from the top, so the first row is the highest up.
  expect_equal(layout$row$y, c(3 / 4, 2 / 4, 1 / 4, 0))

  # Tiles meet edge to edge and fill the panel.
  expect_equal(layout$col$x + layout$col$width, c(1 / 3, 2 / 3, 1))
  expect_equal(max(layout$row$y + layout$row$height), 1)
})

test_that("calendar_layout spaces tiles by a fraction of a tile", {
  # Two columns with a quarter-tile gap: 2 tiles + 0.25 of a tile spans 1.
  layout <- calendar_layout(1L, 2L, spacing = list(col = 0.25))
  expect_equal(layout$col$width, rep(1 / 2.25, 2))
  expect_equal(layout$col$x, c(0, 1.25 / 2.25))
  expect_equal(max(layout$col$x + layout$col$width), 1)

  # Pane gaps land only between rows of different panes, so four rows in two
  # panes take a single gap.
  layout <- calendar_layout(
    4L,
    1L,
    row_pane = c(1L, 1L, 2L, 2L),
    spacing = list(pane = 0.5)
  )
  expect_equal(layout$row$height, rep(1 / 4.5, 4))
  expect_equal(diff(rev(layout$row$y)), c(1, 1.5, 1) / 4.5)
  expect_equal(max(layout$row$y + layout$row$height), 1)
  expect_equal(min(layout$row$y), 0)

  # Spacing with every row in its own pane gaps every row.
  layout <- calendar_layout(3L, 1L, row_pane = 1:3, spacing = list(pane = 0.5))
  expect_equal(layout$row$height, rep(1 / 4, 3))
})

test_that("calendar_dim_tracks turns gaps into empty tracks", {
  layout <- calendar_layout(1L, 3L)
  # A seamless layout is all tile and no filler, so the axis gtable is
  # unchanged from the equal `1 / n` division it used to compute itself.
  track <- calendar_dim_tracks(layout$col, reverse = FALSE)
  expect_equal(track$sizes, rep(1 / 3, 3))
  expect_true(all(track$tile))

  track <- calendar_dim_tracks(
    calendar_layout(1L, 2L, spacing = list(col = 0.25))$col,
    reverse = FALSE
  )
  expect_equal(track$tile, c(TRUE, FALSE, TRUE))
  expect_equal(track$sizes, c(1, 0.25, 1) / 2.25)
  expect_equal(sum(track$sizes), 1)

  # Reversed, tracks are ordered downwards from the top of the panel, so the
  # first row of the calendar comes first.
  track <- calendar_dim_tracks(
    calendar_layout(3L, 1L, row_pane = c(1L, 1L, 2L), list(pane = 1))$row,
    reverse = TRUE
  )
  expect_equal(track$tile, c(TRUE, TRUE, FALSE, TRUE))
  expect_equal(track$sizes, c(1, 1, 1, 1) / 4)
})

test_that("repeated axes get one track per row and column", {
  df <- calendar_data()
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(rows = mixtime::weeks(1L), cols = mixtime::months(1L))
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]

  # Columns are gapped by default, so the axis gtable holds a filler track
  # between each pair of label sets to keep them aligned with their column.
  h <- coord$render_axis_h(params, calendar_theme())[[1]]
  expect_equal(length(h$widths), 2L * coord$.grid$n_col - 1L)
  expect_equal(
    as.numeric(h$widths)[c(TRUE, FALSE)],
    coord$grid_layout(params)$col$width
  )
  # Rows are not gapped without `pane`, so their axes still tile end to end.
  v <- coord$render_axis_v(params, calendar_theme())[[1]]
  expect_equal(length(v$heights), coord$.grid$n_row)
})

test_that("granule elements inherit the theme's own panel styling", {
  # `plot_theme()` rather than `theme_grey()` directly: registered element
  # defaults are only merged in when the plot's theme is completed.
  p <- ggplot(calendar_data(), aes(x = time, y = value)) +
    geom_line() +
    coord_calendar()
  theme <- ggplot2:::plot_theme(p)

  grid <- calc_element("panel.grid", theme)
  cell <- calendar_element(theme, "cell", "line")
  block <- calendar_element(theme, "block", "line")

  # No colour of our own: the rules follow the theme's gridlines, so ink/paper
  # theming carries over.
  expect_equal(cell@colour, grid@colour)
  expect_equal(block@colour, grid@colour)
  # A hairline cell rule and a heavy block rule, either side of the theme's.
  expect_lt(cell@linewidth, grid@linewidth)
  expect_gt(block@linewidth, grid@linewidth)

  dark <- ggplot2:::plot_theme(p + theme_grey(ink = "white", paper = "black"))
  expect_equal(
    calendar_element(dark, "block", "line")@colour,
    calc_element("panel.grid", dark)@colour
  )

  # Granule fills are opt-in rather than inherited over the panel.
  expect_null(calendar_element(theme, "cell", "background"))
})

test_that("granule elements can be blanked and overridden", {
  p <- ggplot(calendar_data(), aes(x = time, y = value)) +
    geom_line() +
    coord_calendar()

  blanked <- ggplot2:::plot_theme(p + theme(panel.grid = element_blank()))
  expect_null(calendar_element(blanked, "cell", "line"))
  expect_null(calendar_element(blanked, "block", "line"))

  styled <- ggplot2:::plot_theme(
    p + theme(ggtime.calendar.cell.line = element_line(colour = "red"))
  )
  expect_equal(calendar_element(styled, "cell", "line")@colour, "red")
})

test_that("blank granule elements drop their gridlines", {
  built <- ggplot_build(
    ggplot(calendar_data(), aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(rows = mixtime::weeks(1L), cols = NULL)
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]

  theme <- calendar_theme(
    theme_grey() +
      theme(
        ggtime.calendar.cell.line = element_blank(),
        ggtime.calendar.block.line = element_blank(),
        ggtime.calendar.pane.line = element_blank(),
        ggtime.calendar.row.line = element_blank()
      )
  )
  expect_equal(calendar_rule_names(coord$render_bg(params, theme)), character())
})

#' A theme setting one granule's background, completed as a plot's would be
#' @noRd
granule_fill_theme <- function(granule, fill = "grey90") {
  element <- stats::setNames(
    list(if (is.null(fill)) element_blank() else element_rect(fill = fill)),
    calendar_element_name(granule, "background")
  )
  calendar_theme(theme_grey() + inject(theme(!!!element)))
}

#' Every rect one granule's background draws, as plain npc numbers
#' @noRd
granule_background <- function(bg, granule) {
  rect <- calendar_grobs(bg)[[calendar_element_name(granule, "background")]]
  if (is.null(rect)) {
    return(NULL)
  }
  data.frame(
    x = as.numeric(rect$x),
    y = as.numeric(rect$y),
    width = as.numeric(rect$width),
    height = as.numeric(rect$height)
  )
}

#' The name of every grob of a tree, in the order it is drawn
#' @noRd
draw_order <- function(grob) {
  if (inherits(grob, "gTree")) {
    return(unlist(lapply(grob$children, draw_order)) %||% character())
  }
  grob$name %||% ""
}

#' A calendar exercising all five granules at once
#' @noRd
every_granule_calendar <- function(...) {
  df <- data.frame(time = as.Date("2025-01-06") + 0:363)
  df$value <- seq_len(nrow(df))
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(
        cells = mixtime::days(1L),
        rows = mixtime::weeks(1L),
        blocks = mixtime::months(1L),
        panes = mixtime::months(1L),
        cols = mixtime::quarters(1L),
        ...
      )
  )
  list(
    coord = built$plot$coordinates,
    params = built$layout$panel_params[[1]]
  )
}

test_that("every granule's background is drawn behind its own rules", {
  plot <- every_granule_calendar()
  plain <- plot$coord$render_bg(plot$params, calendar_theme())

  # Blank by default: nothing drawn, and no grob left behind to walk past.
  expect_false(any(grepl("\\.background$", calendar_rule_names(plain))))
  expect_length(grobs_of_class(plain, "rect"), 1L)

  for (granule in calendar_granules) {
    name <- calendar_element_name(granule, "background")
    bg <- plot$coord$render_bg(plot$params, granule_fill_theme(granule))

    drawn <- calendar_grobs(bg)[[name]]
    expect_s3_class(drawn, "rect")
    expect_equal(drawn$gp$fill, "grey90", label = granule)
    # One vectorised rect for the granule as a whole, however many instances
    # it has -- a fill costs a grob, not a grob per instance.
    expect_length(grobs_of_class(bg, "rect"), 2L)

    order <- draw_order(bg)
    at <- which(order == name)
    expect_length(at, 1L)
    # Over the panel's own background ...
    expect_gt(at, max(grep("^panel\\.background", order)))
    # ... and under every rule the calendar draws, its own included. The
    # labels are drawn later still, in `render_fg()`, over the data.
    expect_lt(at, min(grep("^ggtime\\.calendar\\..*\\.line$", order)))

    blanked <- plot$coord$render_bg(
      plot$params,
      granule_fill_theme(granule, fill = NULL)
    )
    expect_false(name %in% calendar_rule_names(blanked))
  }
})

test_that("a granule background covers the instances it fills", {
  # `row`, `block` and `col` are unlabelled here, so their instance tables are
  # not built when the panel is set up; a fill is the other thing that needs
  # them, and it is only known about once the theme is in hand.
  plot <- every_granule_calendar()
  expect_null(plot$params$granule_instances$col)
  layout <- plot$coord$grid_layout(plot$params)

  # A column's fill spans the whole of the first row it heads.
  col <- granule_background(
    plot$coord$render_bg(plot$params, granule_fill_theme("col")),
    "col"
  )
  expect_equal(nrow(col), length(layout$col$x))
  expect_equal(col$x, layout$col$x)
  expect_equal(col$width, layout$col$width)
  expect_equal(col$y, rep(layout$row$y[1], nrow(col)))
  expect_equal(col$height, rep(layout$row$height[1], nrow(col)))

  # A pane's fill covers every row the pane holds, one rect per row, rather
  # than only the row it begins at -- see "a group's background covers every
  # row it spans" below.
  pane <- granule_background(
    plot$coord$render_bg(plot$params, granule_fill_theme("pane")),
    "pane"
  )
  expect_gt(nrow(pane), nrow(plot$params$granule_instances$pane))
  expect_setequal(pane$width, layout$col$width)
  expect_setequal(pane$height, layout$row$height)

  # A cell's fill meets its neighbours exactly at the cell rules, so the two
  # cannot drift apart.
  filled <- plot$coord$render_bg(plot$params, granule_fill_theme("cell"))
  cell <- granule_background(filled, "cell")
  rules <- calendar_grobs(filled)[["ggtime.calendar.cell.line"]]
  expect_true(all(
    round(unique(as.numeric(rules$x)), 9) %in%
      round(unique(c(cell$x, cell$x + cell$width)), 9)
  ))
  # Every fill is inside the panel, and none is empty.
  expect_true(all(cell$x >= -1e-9 & cell$x + cell$width <= 1 + 1e-9))
  expect_true(all(cell$width > 0 & cell$height > 0))
})

test_that("a group's background covers every row it spans", {
  # `block` and `pane` hold whole rows, and a fill of one covers all of them
  # rather than just the row the group begins at (the row that carries its
  # rule and its label). Every row of this calendar belongs to a pane and to a
  # block, so either fill covers exactly the tiles the calendar has pieces
  # for -- 78 of them, against the 15 groups the two granules cut.
  plot <- every_granule_calendar()
  params <- plot$params
  layout <- plot$coord$grid_layout(params)
  rows <- plot$coord$piece_rows(params)
  cols <- params$pieces$col
  tile <- function(x, y) paste(round(x, 9), round(y, 9))

  for (granule in c("pane", "block")) {
    bg <- granule_background(
      plot$coord$render_bg(params, granule_fill_theme(granule)),
      granule
    )
    groups <- calendar_instances(params, granule)
    expect_gt(nrow(bg), nrow(groups))
    expect_equal(nrow(bg), length(cols), label = granule)
    expect_setequal(
      tile(bg$x, bg$y),
      tile(layout$col$x[cols], layout$row$y[rows])
    )
    # One rect the size of its own tile per row covered, rather than one box
    # drawn from the group's first row to its last.
    expect_setequal(bg$width, layout$col$width)
    expect_setequal(bg$height, layout$row$height)

    # January is a pane (and a block) of five rows: all five are filled.
    first <- groups$piece[1]
    expect_equal(
      sum(tile(bg$x, bg$y) %in% tile(layout$col$x[1], layout$row$y[1:5])),
      5L
    )
    expect_equal(rows[first], 1L)
  }

  # The instance table itself still holds one instance per group, placed at
  # the group's first row -- so one label per group, where the extent above
  # would otherwise have multiplied them.
  panes <- params$granule_instances$pane
  expect_equal(nrow(panes), sum(params$pieces$pane_start))
  expect_length(
    calendar_labels(plot$coord$render_fg(params, calendar_theme()), "pane"),
    nrow(panes)
  )
})

test_that("a group's fill covers its rows while its rule keeps to the first", {
  # Blocks without panes, so that the block boundary is ruled rather than
  # gapped (see "a block rule replaces the row rule at the boundary it falls
  # on"), and the rule can be compared against the fill it now sits inside.
  built <- ggplot_build(
    ggplot(calendar_data(), aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(
        rows = mixtime::weeks(1L),
        blocks = mixtime::months(1L),
        panes = NULL,
        cols = NULL,
        label_blocks = function(x) rep("", length(x))
      )
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  layout <- coord$grid_layout(params)
  rows <- coord$piece_rows(params)
  first <- which(params$pieces$block_start)

  bg <- coord$render_bg(params, granule_fill_theme("block"))
  fill <- granule_background(bg, "block")
  # Two blocks (April and May) over six rows, and every row is filled.
  expect_equal(length(first), 2L)
  expect_equal(nrow(fill), length(rows))
  expect_setequal(fill$y, layout$row$y)

  # The rule still marks only where the second block starts: one rule, at the
  # boundary above that block's first row, rather than one per row filled.
  rule <- calendar_grobs(coord$render_bg(params, calendar_theme()))[[
    "ggtime.calendar.block.line"
  ]]
  expect_equal(
    unique(as.numeric(rule$y)),
    layout$row$y[rows[first[2]] - 1L]
  )
  # And one label per block, not one per row.
  expect_length(
    calendar_labels(coord$render_fg(params, calendar_theme()), "block"),
    length(first)
  )
})

test_that("a group's background stops at a column boundary", {
  # `calendar_pieces()` starts a new group at every column boundary, so a
  # group is always within a single column however coarse its granule is: a
  # quarterly block over monthly columns is cut into one group per column, and
  # no rect of its fill spans two of them.
  df <- data.frame(time = as.Date("2025-01-01") + 0:363)
  df$value <- seq_len(nrow(df))
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(
        rows = mixtime::weeks(1L),
        blocks = mixtime::quarters(1L),
        panes = NULL,
        cols = mixtime::months(1L)
      )
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  layout <- coord$grid_layout(params)

  expect_equal(sum(params$pieces$block_start), length(layout$col$x))
  fill <- granule_background(
    coord$render_bg(params, granule_fill_theme("block")),
    "block"
  )
  expect_equal(nrow(fill), length(params$pieces$col))
  expect_setequal(fill$x, layout$col$x)
  expect_setequal(fill$width, layout$col$width)
})

test_that("a group's background leaves the gaps within it clear", {
  # A quarterly block holds three monthly panes, so the rows it covers are
  # separated by the gaps between those panes -- and a column with fewer rows
  # than the pane it shares with another leaves the rest of that pane empty.
  # One rect per row covered keeps both clear; one box from the group's first
  # row to its last would paint over them.
  df <- data.frame(time = as.Date("2025-01-01") + 0:729)
  df$value <- seq_len(nrow(df))
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(
        rows = mixtime::weeks(1L),
        blocks = mixtime::quarters(1L),
        panes = mixtime::months(1L),
        cols = mixtime::years(1L)
      )
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  layout <- coord$grid_layout(params)
  rows <- coord$piece_rows(params)
  cols <- params$pieces$col
  tile <- function(x, y) paste(round(x, 9), round(y, 9))

  # The grid gaps rows within a block, and the second column is a row short of
  # the first, so the two cases are both in play here.
  gaps <- layout$row$y[-length(layout$row$y)] -
    (layout$row$y[-1] + layout$row$height[-1])
  expect_true(any(gaps > 1e-9))
  expect_lt(length(cols), 2L * length(layout$row$y))

  fill <- granule_background(
    coord$render_bg(params, granule_fill_theme("block")),
    "block"
  )
  expect_equal(nrow(fill), length(cols))
  expect_setequal(
    tile(fill$x, fill$y),
    tile(layout$col$x[cols], layout$row$y[rows])
  )
  # No rect is taller than one row, so nothing is painted over a gap or over
  # the row the shorter column never filled.
  expect_setequal(fill$height, layout$row$height)
})

test_that("granule backgrounds reflect with the grid when flipped", {
  # A flipped calendar reflects the grid of tiles but not the inside of one
  # (see `calendar_transposition()`), so a fill covering part of a row's
  # window has to follow the tile's interior rather than being reversed with
  # the grid. With a single column the two are told apart cleanly: a tile's
  # own `x` becomes `y` unchanged, while the grid's rows are reflected.
  df <- calendar_data()
  fill <- function(time_aes, granule) {
    mapping <- if (time_aes == "x") {
      aes(x = time, y = value)
    } else {
      aes(x = value, y = time)
    }
    built <- ggplot_build(
      ggplot(df, mapping) +
        geom_line() +
        coord_calendar(
          rows = mixtime::weeks(1L),
          cols = NULL,
          time = time_aes
        )
    )
    coord <- built$plot$coordinates
    params <- built$layout$panel_params[[1]]
    granule_background(
      coord$render_bg(params, granule_fill_theme(granule)),
      granule
    )
  }

  for (granule in c("cell", "row")) {
    flat <- fill("x", granule)
    flipped <- fill("y", granule)
    expect_equal(nrow(flipped), nrow(flat), label = granule)
    # Rows are reflected onto `x`, so the first row is at the left edge ...
    expect_equal(flipped$x, 1 - (flat$y + flat$height), label = granule)
    expect_equal(flipped$width, flat$height, label = granule)
    # ... while time still runs up the panel within a row, so a position
    # inside the row's window carries over unchanged.
    expect_equal(flipped$y, flat$x, label = granule)
    expect_equal(flipped$height, flat$width, label = granule)
  }
})

test_that("cell breaks fall strictly within a row", {
  built <- ggplot_build(
    ggplot(calendar_data(), aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(rows = mixtime::weeks(1L), cols = NULL)
  )
  breaks <- built$layout$panel_params[[1]]$cell_breaks

  # Six interior boundaries between the seven days of a week; the window's own
  # edges belong to the row, not to a cell.
  expect_equal(breaks, seq_len(6L) / 7)
})

test_that("coord_calendar rejects non-duration granules", {
  # `cols`/`blocks` are captured unevaluated and only resolved once the
  # axis's calendar is known (see `eval_granule()`), so the error now
  # surfaces at build time rather than at construction.
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) + geom_line()

  expect_error(ggplot_build(p + coord_calendar(cols = 1:3)), "duration")
  expect_error(ggplot_build(p + coord_calendar(blocks = "1 month")), "duration")
})

#' A `cal_sym454` time vector, for exercising calendar-relative granule
#' resolution against something other than the Gregorian default.
#' @noRd
sym454_data <- function() {
  x <- 0:41
  data.frame(
    time = mixtime::linear_time(
      as.Date("2025-04-07") + x,
      chronon = mixtime::cal_sym454$day(1L),
      calendar = mixtime::cal_sym454
    ),
    value = sin(x / 7 * 2 * pi) + x / 40
  )
}

#' The granules a built calendar resolved for one of its panels
#'
#' The resolved granules are per-panel state rather than input, so they travel
#' with the panel's cuts (see `calendar_compute_cuts()`) rather than being
#' parked on the coord. This resolves them exactly as
#' `CoordCalendar$compute_cuts()` does -- from the coord's own granule
#' arguments and the calendar of the axis the build actually trained.
#' @noRd
built_granules <- function(built, panel = 1L) {
  coord <- built$plot$coordinates
  scales <- if (identical(coord$time, "x")) {
    built$layout$panel_scales_x
  } else {
    built$layout$panel_scales_y
  }
  calendar_resolve_granules(
    coord$granule_specs(),
    time_scale_calendar(scales[[panel]])
  )
}

test_that("a bare granule token resolves against the axis's own calendar", {
  # A plain `Date`/`POSIXct` axis (no `scale_x_mixtime()`) resolves bare
  # tokens against the Gregorian calendar, exactly as the durations they
  # replace as defaults did.
  built <- ggplot_build(
    ggplot(calendar_data(), aes(x = time, y = value)) +
      geom_line() +
      coord_calendar()
  )
  granules <- built_granules(built)
  expect_equal(granules$cells, mixtime::cal_gregorian$day(1L))
  expect_equal(granules$rows, mixtime::cal_gregorian$day(7L))
  expect_equal(granules$panes, mixtime::cal_gregorian$month(1L))
  expect_equal(granules$cols, mixtime::cal_gregorian$quarter(1L))

  # A `scale_x_mixtime()` axis using a different calendar resolves the same
  # tokens against *that* calendar instead -- `panes`/`cols` here are the
  # `symmetry454` month and quarter, not the Gregorian ones. Suppresses an
  # unrelated warning from folding this short a span of data onto
  # `symmetry454` months, from mixtime's own cycle arithmetic rather than
  # anything under test here.
  built_sym454 <- suppressWarnings(ggplot_build(
    ggplot(sym454_data(), aes(x = time, y = value)) +
      geom_line() +
      scale_x_mixtime() +
      coord_calendar(cols = NULL)
  ))
  sym454 <- built_granules(built_sym454)
  expect_s3_class(sym454$panes, "mixtime::tu_sym454_month")
  expect_false(identical(sym454$panes, granules$panes))
})

test_that("a namespace-qualified granule or duration bypasses the calendar mask", {
  # `week` is not a Gregorian granule (see the next test), but an explicit
  # `cal_isoweek$week(1L)` -- or a plain duration -- is unaffected by the
  # calendar the axis itself uses, and means exactly what it says.
  built <- ggplot_build(
    ggplot(calendar_data(), aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(
        rows = mixtime::days(1L),
        panes = mixtime::cal_isoweek$week(1L),
        cols = NULL
      )
  )
  expect_equal(built_granules(built)$panes, mixtime::cal_isoweek$week(1L))
})

test_that("a defaulted rows/cols falls back when the calendar lacks the granule", {
  # `cal_sym454` has no `quarter` granule, unlike Gregorian -- left at its
  # default, `cols` (`quarter(1L)`) quietly falls back to the duration it
  # names (`month(3L)`) instead of erroring, the same way a defaulted `panes`
  # gives way rather than erroring when it doesn't fit (see
  # `CoordCalendar$granule_specs()`).
  built <- suppressWarnings(ggplot_build(
    ggplot(sym454_data(), aes(x = time, y = value)) +
      geom_line() +
      scale_x_mixtime() +
      coord_calendar()
  ))
  granules <- built_granules(built)
  expect_equal(granules$cols, mixtime::cal_sym454$month(3L))
  # `week` *is* one of `cal_sym454`'s own granules, so the default `rows`
  # resolves directly to it rather than falling back to `day(7L)`.
  expect_equal(granules$rows, mixtime::cal_sym454$week(1L))
})

test_that("a granule token missing from the calendar errors with a hint", {
  # The fallback above only applies to a defaulted `rows`/`cols` -- a granule
  # the user asked for by name still errors when their axis's calendar
  # cannot resolve it, exactly as any other unresolvable granule does.
  # `cal_gregorian` has no `week` granule, and `cal_sym454` (see
  # `sym454_data()`) has no `quarter` granule.
  expect_error(
    ggplot_build(
      ggplot(calendar_data(), aes(x = time, y = value)) +
        geom_line() +
        coord_calendar(rows = week(1L))
    ),
    "no.*week.*granule"
  )
  expect_error(
    suppressWarnings(ggplot_build(
      ggplot(sym454_data(), aes(x = time, y = value)) +
        geom_line() +
        scale_x_mixtime() +
        coord_calendar(cols = quarter(1L))
    )),
    "no.*quarter.*granule"
  )
})

test_that("coord_calendar rejects spacing that is not a fraction", {
  expect_error(coord_calendar(col_spacing = -1), "non-negative")
  expect_error(coord_calendar(pane_spacing = c(1, 2)), "single")
  expect_error(coord_calendar(col_spacing = "wide"), "non-negative")
  # `rel()` reads naturally for a fraction of a tile, so it is accepted.
  expect_equal(coord_calendar(col_spacing = rel(0.25))$col_spacing, 0.25)
})

test_that("cell_ratio fixes the panel's aspect so a cell has that shape", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) + geom_line()

  square <- ggplot_build(p + coord_calendar(cell_ratio = 1, cols = NULL))
  coord <- square$plot$coordinates
  params <- square$layout$panel_params[[1]]
  layout <- coord$grid_layout(params)

  # Daily cells of a weekly row: a cell is a seventh of the panel's width and
  # a row its whole height, so the panel must be as many times as tall as
  # there are rows, over the seven cells across it.
  cell_width <- layout$col$width[1L] / 7
  cell_height <- layout$row$height[1L]
  aspect <- coord$aspect(params)
  expect_equal(aspect, cell_width / cell_height)
  # The cell the panel is shaped around is square: its height, as a fraction
  # of the panel's height, times the panel's own aspect ratio.
  expect_equal(cell_height * aspect, cell_width)

  # The ratio is the cell's height over its width, so it scales the panel.
  tall <- ggplot_build(p + coord_calendar(cell_ratio = 2, cols = NULL))
  expect_equal(
    tall$plot$coordinates$aspect(tall$layout$panel_params[[1]]),
    2 * aspect
  )
})

test_that("cell_ratio leaves the panel free unless it is set", {
  df <- calendar_data()
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) + geom_line() + coord_calendar()
  )
  expect_null(
    built$plot$coordinates$aspect(built$layout$panel_params[[1]])
  )
})

test_that("a flipped calendar's cell_ratio still measures height over width", {
  df <- calendar_data()
  upright <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(cell_ratio = 1, cols = NULL)
  )
  flipped <- ggplot_build(
    ggplot(df, aes(y = time, x = value)) +
      geom_line() +
      coord_calendar(time = "y", cell_ratio = 1, cols = NULL)
  )

  # Time runs down the panel rather than across it, so the cell that was
  # `1 / 7` of the panel wide is now that fraction of it tall.
  expect_equal(
    flipped$plot$coordinates$aspect(flipped$layout$panel_params[[1]]),
    1 / upright$plot$coordinates$aspect(upright$layout$panel_params[[1]])
  )
})

test_that("cell_ratio shapes a whole row when there are no cells", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) + geom_line()
  built <- ggplot_build(p + coord_calendar(cells = NULL, cell_ratio = 1))
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  layout <- coord$grid_layout(params)

  expect_null(params$cell_breaks)
  expect_equal(
    coord$aspect(params),
    layout$col$width[1L] / layout$row$height[1L]
  )
})

test_that("a base coord's own ratio is scaled into the calendar's grid", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) + geom_line()
  built <- ggplot_build(
    p + coord_calendar(cols = NULL, coord = ggplot2::coord_fixed(2))
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  layout <- coord$grid_layout(params)

  # `coord_fixed()` shapes one row's window, which is a tile of the grid
  # rather than the whole panel.
  tile <- diff(params$y.range) / diff(params$x.range) * 2
  expect_equal(
    coord$aspect(params),
    tile * layout$col$width[1L] / layout$row$height[1L]
  )

  # Both would fix the panel's one aspect ratio.
  expect_error(
    coord_calendar(cell_ratio = 1, coord = ggplot2::coord_fixed()),
    "cannot both be set"
  )
})

test_that("coord_calendar rejects a cell_ratio that is not a positive number", {
  expect_error(coord_calendar(cell_ratio = 0), "positive number")
  expect_error(coord_calendar(cell_ratio = -1), "positive number")
  expect_error(coord_calendar(cell_ratio = "square"), "positive number")
  expect_error(coord_calendar(cell_ratio = c(1, 2)), "single")
  expect_equal(coord_calendar(cell_ratio = rel(2))$cell_ratio, 2)
  expect_null(coord_calendar()$cell_ratio)
})

test_that("a cell's width is the typical one, not the average", {
  expect_equal(calendar_cell_frac(NULL), 1)
  expect_equal(calendar_cell_frac(numeric()), 1)
  expect_equal(calendar_cell_frac(c(1, 2, 3) / 4), 1 / 4)
  # A row the cells do not divide evenly closes with a sliver of a cell (the
  # week of a daylight saving change), which must not shrink every other cell.
  expect_equal(calendar_cell_frac(c(0.2, 0.4, 0.6, 0.8, 0.99)), 0.2)
})

test_that("panes must sit between rows and cols in coarseness", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) + geom_line()

  expect_error(
    ggplot_build(
      p + coord_calendar(rows = mixtime::weeks(1L), panes = mixtime::days(1L))
    ),
    "coarser than"
  )
  expect_error(
    ggplot_build(
      p +
        coord_calendar(
          rows = mixtime::days(1L),
          panes = mixtime::years(1L),
          cols = mixtime::months(1L)
        )
    ),
    "must not be coarser"
  )
  # Equal to `cols` is allowed, since a pane may span a whole column.
  expect_no_error(ggplot_build(
    p +
      coord_calendar(
        rows = mixtime::weeks(1L),
        panes = mixtime::months(1L),
        cols = mixtime::months(1L)
      )
  ))
})

test_that("a defaulted panes value gives way where it does not fit", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) + geom_line()

  pane_cuts_of <- function(plot) {
    ggplot_build(plot)$layout$panel_params[[1]]$pane_cuts
  }

  # The monthly default is dropped rather than erroring, both where `rows` is
  # no finer than a month and where `cols` is finer than one.
  expect_null(pane_cuts_of(p + coord_calendar(rows = mixtime::months(1L))))
  expect_null(pane_cuts_of(
    p + coord_calendar(rows = mixtime::days(1L), cols = mixtime::weeks(1L))
  ))
  # Where it does fit, the default panes the calendar by month.
  expect_false(is.null(pane_cuts_of(
    p + coord_calendar(rows = mixtime::weeks(1L))
  )))
})

test_that("an explicit panes matching the default behaves like the default", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) + geom_line()

  pane_cuts_of <- function(plot) {
    ggplot_build(plot)$layout$panel_params[[1]]$pane_cuts
  }

  # Passing the default value explicitly used to hit the error path
  # (`pane_default <- missing(panes)` cannot tell this from a genuine
  # request), where leaving `panes` unset quietly drops it. Both must now
  # behave identically: no error, and no panes drawn.
  expect_no_error(ggplot_build(
    p +
      coord_calendar(panes = mixtime::months(1L), rows = mixtime::months(1L))
  ))
  expect_null(pane_cuts_of(
    p + coord_calendar(panes = mixtime::months(1L), rows = mixtime::months(1L))
  ))
  expect_identical(
    pane_cuts_of(
      p +
        coord_calendar(panes = mixtime::months(1L), rows = mixtime::months(1L))
    ),
    pane_cuts_of(p + coord_calendar(rows = mixtime::months(1L)))
  )

  # A wrapper (or `do.call()`) forwarding the default value loses `missing()`
  # information; it must give way the same as leaving `panes` unset, not
  # error.
  wrapped <- do.call(
    coord_calendar,
    list(panes = mixtime::months(1L), rows = mixtime::months(1L))
  )
  expect_no_error(ggplot_build(p + wrapped))
  expect_null(
    ggplot_build(p + wrapped)$layout$panel_params[[1]]$pane_cuts
  )

  # A genuinely explicit, incompatible `panes` must still error -- the
  # resolved-value comparison must not accidentally treat every `panes` as
  # defaulted.
  expect_error(
    ggplot_build(
      p + coord_calendar(rows = mixtime::weeks(1L), panes = mixtime::days(1L))
    ),
    "coarser than"
  )
})

test_that("a granule that fails to cut warns and is dropped, rather than vanishing silently", {
  # `mixtime::loc_altitude()` is not a time unit at all, so cutting any time
  # range at it always fails -- standing in for a granule the underlying
  # cutting machinery cannot make sense of against a particular axis (mixtime
  # gives that failure no condition class of its own to catch selectively,
  # see `calendar_axis_cuts()`).
  range <- mixtime::yearmonth(c(600L, 611L))

  expect_warning(
    result <- calendar_axis_cuts(
      range,
      mixtime::loc_altitude(1L),
      name = "cells"
    ),
    "`cells`.*dropped"
  )
  expect_null(result)

  # `fallback` is returned either way -- `time_range` itself for `cols`,
  # `NULL` for `panes`/`blocks`/`cells`.
  expect_warning(
    result <- calendar_axis_cuts(
      range,
      mixtime::loc_altitude(1L),
      fallback = range,
      name = "cols"
    ),
    "`cols`"
  )
  expect_identical(result, range)

  # A `NULL` granule is a deliberate "not set", not a cutting failure, so it
  # is dropped silently.
  expect_no_warning(calendar_axis_cuts(range, NULL))
})

test_that("flipped calendars gap and rule the same boundaries", {
  df <- data.frame(time = as.Date("2025-01-06") + 0:167, value = seq_len(168L))
  built <- ggplot_build(
    ggplot(df, aes(y = time, x = value)) +
      geom_path() +
      coord_calendar(
        time = "y",
        rows = mixtime::weeks(1L),
        panes = mixtime::months(1L),
        cols = mixtime::quarters(1L)
      )
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  bg <- coord$render_bg(params, calendar_theme())
  layout <- coord$grid_layout(params)

  # Flipped, rows run horizontally: their rules are vertical, so it is `x`
  # that sits on the row boundaries and `y` that spans a column.
  # Both dimensions are reflected as well as swapped, so that the grid reads
  # the same way round as an unflipped one (see `calendar_transposition()`).
  row_rule <- calendar_grobs(bg)[["ggtime.calendar.row.line"]]
  expect_true(all(as.numeric(row_rule$x) %in% (1 - layout$row$y)))
  expect_setequal(
    unique(as.numeric(row_rule$y)),
    1 - c(layout$col$x, layout$col$x + layout$col$width)
  )

  # The axis of a flipped calendar repeats down the columns, gaps included.
  v <- coord$render_axis_v(params, calendar_theme())[[1]]
  expect_equal(length(v$heights), 2L * coord$.grid$n_col - 1L)
})

test_that("granule instances land where the data they name does", {
  # The point of the instance tables: a label is placed by the same fold and
  # rescale the data goes through, so a cell's label sits exactly where an
  # observation at that time is drawn.
  df <- data.frame(time = as.Date("2025-01-01") + 0:180, value = 1)
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_point() +
      coord_calendar(rows = mixtime::weeks(1L), cols = mixtime::months(1L))
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  layout <- coord$grid_layout(params)

  cells <- params$granule_instances$cell
  drawn <- coord$transform(
    data.frame(x = as.numeric(cells$time), y = 1),
    params
  )
  expect_equal(
    drawn$x,
    layout$col$x[cells$col] + layout$col$width[cells$col] * cells$start
  )
})

test_that("granule instances describe each column's own times", {
  df <- data.frame(time = as.Date("2025-01-01") + 0:180, value = 1)
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_point() +
      coord_calendar(
        rows = mixtime::weeks(1L),
        blocks = mixtime::months(1L),
        cols = mixtime::months(1L),
        # `row`/`block`/`col` are unlabelled by default, which skips building
        # their instance tables entirely; label them (the text is irrelevant
        # here) so this test can inspect the tables below.
        label_rows = function(x) rep("", length(x)),
        label_blocks = function(x) rep("", length(x)),
        label_cols = function(x) rep("", length(x))
      )
  )
  params <- built$layout$panel_params[[1]]

  rows <- params$granule_instances$row
  # Rows tile their column end to end. The first picks up where the column
  # itself starts -- a Saturday, part way through the week February opens in --
  # and the rest are whole weeks of the shared grid.
  feb <- vctrs::vec_slice(rows, rows$col == 2L)
  expect_equal(feb$time[1], as.Date("2025-02-01"))
  expect_equal(unique(diff(as.numeric(feb$time[-1]))), 7)
  expect_equal(feb$start, c(5 / 7, 0, 0, 0, 0))
  expect_equal(feb$end, c(1, 1, 1, 1, 5 / 7))

  # A column is named by its own start rather than by the shared row grid it
  # inherits, which begins at the start of the week the column falls in.
  cols <- params$granule_instances$col
  expect_equal(
    cols$time,
    seq(as.Date("2025-01-01"), as.Date("2025-06-01"), by = "1 month")
  )
  # One block per column here, named from within the month it groups even
  # though its first row starts a day or two either side of the boundary.
  blocks <- params$granule_instances$block
  expect_equal(nrow(blocks), nrow(cols))
  expect_equal(
    format(blocks$time, "%m"),
    format(cols$time, "%m")
  )
})

test_that("a granule grouping rows is labelled against the row it starts", {
  # A group's label spans the whole of the row it opens rather than the part of
  # it the group's own first day takes up, so that it is placed the same way
  # whichever weekday the group happens to begin on. Placed at the pane's own
  # start instead, a month opening on a Sunday would have its label pushed to
  # the far right of the row by the six days belonging to the month before it.
  df <- data.frame(time = as.Date("2025-01-01") + 0:180, value = 1)
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_point() +
      coord_calendar(
        rows = mixtime::weeks(1L),
        blocks = mixtime::months(1L),
        panes = mixtime::months(1L),
        cols = mixtime::quarters(1L),
        # `block` is unlabelled by default, which skips its instance table;
        # label it (the text is irrelevant here) to inspect it below.
        label_blocks = function(x) rep("", length(x))
      )
  )
  params <- built$layout$panel_params[[1]]

  panes <- params$granule_instances$pane
  # Six panes, opening on weekdays as varied as a Wednesday (1 January) and a
  # Saturday (1 March), all spanning the whole of their row.
  expect_equal(nrow(panes), 6L)
  expect_equal(panes$start, rep(0, 6L))
  expect_equal(panes$end, rep(1, 6L))
  expect_equal(
    params$granule_instances$block[c("start", "end")],
    panes[c("start", "end")]
  )

  # Which lands every label the same distance into the column it opens -- the
  # element's own margin, `hjust = 0` justifying it against the column's left
  # edge -- rather than however far into the row the month happens to begin.
  coord <- built$plot$coordinates
  label <- calendar_grobs(
    coord$render_fg(params, ggplot2:::plot_theme(built$plot))
  )[["ggtime.calendar.pane.text"]]$children[[1]]

  # The label's `x` mixes npc with the margin's absolute units, so it takes a
  # device to resolve; a null one is enough to convert against.
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  at <- grid::convertX(label$x, "npc", valueOnly = TRUE)
  layout <- coord$grid_layout(params)
  expect_equal(
    at - layout$col$x[panes$col],
    rep(at[1], nrow(panes))
  )
})

test_that("labels are formatted for both mixtime and Date axes", {
  labels_of <- function(p) {
    built <- ggplot_build(p)
    calendar_labels(
      built$plot$coordinates$render_fg(
        built$layout$panel_params[[1]],
        ggplot2:::plot_theme(p)
      ),
      "cell"
    )
  }

  date <- data.frame(time = as.Date("2025-04-07") + 0:13, value = 1)
  p <- ggplot(date, aes(x = time, y = value)) +
    geom_point() +
    coord_calendar(rows = mixtime::weeks(1L), cols = NULL)
  # A `Date` axis is wrapped before formatting, rather than losing its labels.
  expect_equal(labels_of(p), sprintf("%02d", 7:20))

  mixed <- data.frame(time = mixtime::date(date$time), value = 1)
  p <- ggplot(mixed, aes(x = time, y = value)) +
    geom_point() +
    coord_calendar(rows = mixtime::weeks(1L), cols = NULL)
  expect_equal(labels_of(p), sprintf("%02d", 7:20))

  # Blocks are labelled by the month they group, not the day they start on.
  p <- ggplot(date, aes(x = time, y = value)) +
    geom_point() +
    coord_calendar(
      rows = mixtime::weeks(1L),
      blocks = mixtime::weeks(1L),
      cols = NULL,
      label_cells = NULL,
      label_blocks = "{cyc(month, year, label = TRUE, abbreviate = TRUE)}"
    )
  built <- ggplot_build(p)
  fg <- built$plot$coordinates$render_fg(
    built$layout$panel_params[[1]],
    ggplot2:::plot_theme(p)
  )
  expect_null(calendar_labels(fg, "cell"))
  expect_equal(calendar_labels(fg, "block"), c("Apr", "Apr"))
})

test_that("granule tables are only built for granules that are labelled", {
  # `cell` and `pane` are labelled by default, `row`/`block`/`col` are not --
  # so only the labelled two should have an instance table to show for it; an
  # unlabelled granule has nothing to spend that work on (`3.3`).
  df <- data.frame(time = as.Date("2025-01-01") + 0:365, value = 1)
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) + geom_line() + coord_calendar()
  )
  params <- built$layout$panel_params[[1]]

  expect_false(is.null(params$granule_instances$cell))
  expect_false(is.null(params$granule_instances$pane))
  expect_null(params$granule_instances$row)
  expect_null(params$granule_instances$block)
  expect_null(params$granule_instances$col)

  # Skipping the unlabelled tables must not have skipped drawing the ones
  # that are still labelled.
  coord <- built$plot$coordinates
  fg <- coord$render_fg(params, ggplot2:::plot_theme(built$plot))
  expect_false(is.null(calendar_labels(fg, "cell")))
  expect_false(is.null(calendar_labels(fg, "pane")))
})

test_that("disabling cell labels does not disable cell gridlines", {
  # The cell granule table (labels) and `cell_breaks` (gridlines) both come
  # from `self$cells`, but are built separately -- turning off `label_cells`
  # must skip only the (expensive) label table, not the gridlines it shares
  # a granule with (the bug this guards against is `1.2`).
  df <- data.frame(time = as.Date("2025-01-01") + 0:365, value = 1)
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(label_cells = NULL)
  )
  params <- built$layout$panel_params[[1]]
  coord <- built$plot$coordinates

  expect_null(params$granule_instances$cell)
  expect_false(is.null(params$cell_breaks))

  bg <- coord$render_bg(params, calendar_theme())
  expect_true("ggtime.calendar.cell.line" %in% calendar_rule_names(bg))
})

test_that("labels can be given as a function of the times", {
  df <- data.frame(time = as.Date("2025-04-07") + 0:6, value = 1)
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_point() +
    coord_calendar(
      rows = mixtime::weeks(1L),
      cols = NULL,
      label_cells = function(x) format(x, "%a")
    )
  built <- ggplot_build(p)
  expect_equal(
    calendar_labels(
      built$plot$coordinates$render_fg(
        built$layout$panel_params[[1]],
        ggplot2:::plot_theme(p)
      ),
      "cell"
    ),
    format(df$time, "%a")
  )
})

test_that("labels are blanked with their granule's text element", {
  df <- calendar_data()
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_line() +
    coord_calendar(rows = mixtime::weeks(1L), cols = NULL)
  built <- ggplot_build(p)
  params <- built$layout$panel_params[[1]]
  coord <- built$plot$coordinates

  theme <- ggplot2:::plot_theme(
    p +
      theme(
        ggtime.calendar.cell.text = element_blank(),
        ggtime.calendar.pane.text = element_blank()
      )
  )
  expect_equal(calendar_rule_names(coord$render_fg(params, theme)), character())

  # The labels follow the theme's own text colour, so ink/paper carries over.
  dark <- ggplot2:::plot_theme(p + theme_grey(ink = "white", paper = "black"))
  expect_equal(
    calendar_element(dark, "cell", "text")@colour,
    calc_element("text", dark)@colour
  )
})

test_that("coord_calendar rejects labels that are neither format nor function", {
  expect_error(coord_calendar(label_cells = 1:3), "format string")
  expect_error(coord_calendar(label_blocks = c("a", "b")), "format string")
})

test_that("a label that cannot format the axis errors informatively", {
  df <- data.frame(time = as.Date("2025-04-07") + 0:13, value = 1)
  p <- ggplot(df, aes(x = time, y = value)) +
    geom_point() +
    coord_calendar(
      rows = mixtime::weeks(1L),
      cols = NULL,
      label_cells = "{cyc(not_a_granule, month)}"
    )
  built <- ggplot_build(p)
  expect_error(
    built$plot$coordinates$render_fg(
      built$layout$panel_params[[1]],
      ggplot2:::plot_theme(p)
    ),
    "label_cells"
  )
})

test_that("repeated axes leave a track for a pane gap", {
  # Item 7's guarantee: the axes are built from the same layout as the grid,
  # so a gap between rows shifts the axis labels with it.
  df <- data.frame(time = as.Date("2025-01-06") + 0:167, value = seq_len(168L))
  built <- ggplot_build(
    ggplot(df, aes(x = time, y = value)) +
      geom_line() +
      coord_calendar(
        rows = mixtime::weeks(1L),
        panes = mixtime::months(1L),
        cols = mixtime::quarters(1L)
      )
  )
  coord <- built$plot$coordinates
  params <- built$layout$panel_params[[1]]
  layout <- coord$grid_layout(params)

  v <- coord$render_axis_v(params, calendar_theme())[[1]]
  # One track per row, plus one for each gap between panes.
  gaps <- sum(diff(coord$.grid$row_pane) != 0L)
  expect_equal(length(v$heights), coord$.grid$n_row + gaps)
  expect_equal(sum(as.numeric(v$heights)), 1)
  # The label sets themselves are as tall as the rows they belong to, and the
  # rest of the axis is the filler holding the gaps open.
  expect_equal(
    sum(abs(as.numeric(v$heights) - layout$row$height[1]) < 1e-9),
    coord$.grid$n_row
  )
})

test_that("coord_calendar is unsupported for non-cartesian coords", {
  # Checked at construction (like `coord_loop()`'s own coord support check),
  # not deferred until `ggplot_build()`.
  expect_error(
    coord_calendar(rows = mixtime::weeks(1L), coord = coord_radial()),
    "does not support"
  )
})

#' The granule specs of a plain weekly/monthly calendar
#'
#' `CoordCalendar$granule_specs()` in miniature, written out rather than taken
#' from a coord so that the free functions below can be exercised without one.
#' @noRd
test_granule_specs <- function(
  cells = quo(mixtime::days(1L)),
  rows = quo(week(1L)),
  blocks = quo(NULL),
  panes = quo(NULL),
  cols = quo(mixtime::months(1L))
) {
  list(
    cells = list(quo = cells),
    rows = list(
      quo = rows,
      is_default = TRUE,
      unit = "week",
      fallback = quo(day(7L))
    ),
    blocks = list(quo = blocks),
    panes = list(quo = panes, default = quo(month(1L))),
    cols = list(quo = cols)
  )
}

#' A grid environment, as `CoordCalendar$setup_params()` allocates one
#' @noRd
test_grid <- function() {
  new_environment(list(
    pane_rows = NULL,
    n_row = 1L,
    n_col = 1L,
    row_pane = NULL,
    layout = NULL
  ))
}

test_that("granules resolve against the calendar they are handed", {
  # `calendar_resolve_granules()` is a function of its arguments, so the same
  # specs resolved against two calendars give two independent answers --
  # neither of which is remembered anywhere between the calls.
  specs <- test_granule_specs(panes = quo(month(1L)))

  greg <- calendar_resolve_granules(specs, mixtime::cal_gregorian)
  sym454 <- calendar_resolve_granules(specs, mixtime::cal_sym454)

  # `cal_gregorian` has no `week` of its own, so the defaulted `rows` falls
  # back to a seven-day duration; `cal_sym454` defines both `week` and `month`
  # itself.
  expect_equal(greg$rows, mixtime::cal_gregorian$day(7L))
  expect_equal(sym454$rows, mixtime::cal_sym454$week(1L))
  expect_equal(greg$panes, mixtime::cal_gregorian$month(1L))
  expect_s3_class(sym454$panes, "mixtime::tu_sym454_month")
  expect_false(identical(greg$panes, sym454$panes))
  expect_false(identical(greg$rows, sym454$rows))
  # Resolving again gives the first answer back, unaffected by the second.
  expect_equal(calendar_resolve_granules(specs, mixtime::cal_gregorian), greg)

  # `panes` is compared to its own default once resolved, and the comparison
  # is made against the same calendar the granule was resolved with.
  expect_true(greg$pane_default)
  expect_true(sym454$pane_default)
  expect_false(
    calendar_resolve_granules(
      test_granule_specs(panes = quo(mixtime::days(7L))),
      mixtime::cal_gregorian
    )$pane_default
  )

  # A granule that resolves to nothing is recorded as `NULL` rather than
  # dropped from the list, so a reader cannot mistake "no blocks" for "not
  # resolved yet".
  expect_true("blocks" %in% names(greg))
  expect_null(greg$blocks)
})

test_that("the axis is cut into a calendar without building a plot", {
  # `calendar_compute_cuts()` is the whole of `CoordCalendar$compute_cuts()`
  # bar the coord's own input, and needs nothing but a time range and the
  # granules to cut it by.
  granules <- calendar_resolve_granules(
    test_granule_specs(panes = quo(month(1L))),
    mixtime::cal_gregorian
  )
  cuts <- calendar_compute_cuts(
    as.Date(c("2025-01-01", "2025-03-01")),
    granules
  )

  # Monthly columns, cut out past both ends of the range as every granule is
  # (see `loop_cuts_by_duration()`), so January and February each get one and
  # March opens a third.
  expect_equal(
    cuts$col_cuts,
    as.Date(c("2025-01-01", "2025-02-01", "2025-03-01", "2025-04-01"))
  )
  # One row grid across the whole axis, stepping a week at a time, so a
  # weekday is the same position in every column.
  expect_true(all(diff(as.numeric(cuts$row_grid)) == 7))
  expect_lte(cuts$row_grid[1], cuts$col_cuts[1])
  # The window the time axis is drawn against is one row long.
  expect_equal(cuts$row_window, cuts$row_grid[1] + c(0, 7))
  # The span reaches everything any granule of the calendar covers.
  expect_gte(cuts$span[2], cuts$col_cuts[length(cuts$col_cuts)])
  # `blocks` was `NULL`, `panes` a month.
  expect_null(cuts$block_cuts)
  expect_equal(cuts$pane_cuts[1], as.Date("2025-01-01"))
  # The granules travel with the cuts, which is what keeps them off the coord.
  expect_identical(cuts$granules, granules)
})

test_that("a panel's cuts are attached without building a plot", {
  # `calendar_panel_cuts()` is the whole of `CoordCalendar$panel_cuts()` bar
  # the coord's own input: given a panel's cuts it fills in the numeric cuts,
  # the pieces, the cell gridlines and the labelled granules' instance tables,
  # and grows the grid it is handed.
  trans <- scales::transform_date()
  granules <- calendar_resolve_granules(
    test_granule_specs(),
    mixtime::cal_gregorian
  )
  cuts <- calendar_compute_cuts(
    as.Date(c("2025-01-01", "2025-03-01")),
    granules
  )
  window <- as.numeric(trans$transform(cuts$row_window))
  grid <- test_grid()

  params <- calendar_panel_cuts(
    list(x = list(rescale = function(v) (v - window[1]) / diff(window))),
    cuts,
    trans,
    time_scale = "x",
    label_formats = list(
      cell = "{cyc(day, month)}",
      row = NULL,
      block = NULL,
      pane = NULL,
      col = NULL
    ),
    grid = grid
  )

  # The native cuts, reduced to the transformed space cutting happens in.
  expect_equal(params$col_cuts, as.numeric(trans$transform(cuts$col_cuts)))
  expect_equal(params$row_cuts, as.numeric(trans$transform(cuts$row_grid)))
  expect_null(params$block_cuts)
  expect_null(params$pane_cuts)

  # A piece per row of each column, in column order. A row is cut short
  # wherever a month boundary falls inside it, and every piece runs forwards.

  expect_setequal(unique(params$pieces$col), 1:3)
  expect_true(all(diff(params$pieces$cuts) > 0))

  # The grid handed in is grown to fit the panel, by reference.
  expect_equal(grid$n_col, 3L)
  expect_equal(grid$n_row, sum(grid$pane_rows))
  expect_identical(params$grid, grid)

  # Six cell boundaries within a seven-day row, as the row's own edges belong
  # to the row rather than to a cell.
  expect_equal(params$cell_breaks, seq_len(6L) / 7)

  # An instance table for the one labelled granule, and none for the rest --
  # those are built on demand (see `calendar_instances()`).
  expect_false(is.null(params$granule_instances$cell))
  expect_null(params$granule_instances$row)
  expect_null(params$granule_instances$col)
  # One instance per day of the calendar, in order. Cells falling outside the
  # calendar are dropped, so it opens at the first column rather than at the
  # row grid the cells were cut from.
  expect_true(all(diff(as.numeric(params$granule_instances$cell$time)) == 1))
  expect_equal(params$granule_instances$cell$time[1], cuts$col_cuts[1])
})

test_that("each panel's cyclical scale labels its own granule", {
  # `loop_granule()` is a `CoordLoop` hook, and used to answer from the coord
  # itself -- correct only because `compute_cuts()` happened to have run for
  # the same panel moments earlier. One coord sets up every panel of a build,
  # so answering from the coord means the last panel resolved wins; the
  # granules travel with each panel's own cuts instead.
  #
  # Driven through the hooks directly rather than through a facetted build:
  # every panel of a build shares one `time_chronon` (set once on the plot's
  # own scale by `ScaleContinuousMixtime$transform_df()`, before the panel
  # scales are cloned from it), so even `facet_wrap(scales = "free_x")` over
  # data on two calendars gives every panel the same calendar to resolve
  # against. The leak is therefore latent rather than reachable from a plot
  # today -- and this is what would make it reachable.
  coord <- coord_calendar(cols = NULL)
  trans <- scales::transform_date()
  uncut <- list(
    x = list(limits = as.numeric(as.Date(c("2025-01-01", "2025-04-01"))))
  )
  # Two panels whose axes use different calendars, so their `rows` granules
  # differ: `cal_gregorian` has no `week` of its own and falls back to a
  # seven-day duration, while `cal_sym454` defines one.
  gregorian <- ggplot2::scale_x_date()
  sym454 <- scale_x_mixtime()
  sym454$time_chronon <- mixtime::cal_sym454$day(1L)

  cuts <- lapply(
    list(gregorian, sym454),
    function(scale) coord$compute_cuts(uncut, trans, scale, NULL)
  )

  expect_equal(cuts[[1]]$granules$rows, mixtime::cal_gregorian$day(7L))
  expect_equal(cuts[[2]]$granules$rows, mixtime::cal_sym454$week(1L))

  # Asked after both panels have been cut, each still answers with its own.
  expect_equal(coord$loop_granule(cuts[[1]]), cuts[[1]]$granules$rows)
  expect_equal(coord$loop_granule(cuts[[2]]), cuts[[2]]$granules$rows)
  expect_false(identical(
    coord$loop_granule(cuts[[1]]),
    coord$loop_granule(cuts[[2]])
  ))

  # And so does the scale each panel is labelled with: a Gregorian axis falls
  # back to a seven-day duration, whose positions are numbered, while
  # `cal_sym454`'s own week names its days.
  days <- as.Date("2025-01-01") + 0:6
  labels <- lapply(
    cuts,
    function(cut) coord$cyclical_scales(gregorian, NULL, cut)$x$labels(days)
  )
  expect_equal(
    labels[[1]],
    time_labels_at(days, cycle = cuts[[1]]$granules$rows)
  )
  expect_equal(
    labels[[2]],
    time_labels_at(days, cycle = cuts[[2]]$granules$rows)
  )
  expect_false(identical(labels[[1]], labels[[2]]))
})

test_that("the time axis is broken at every cell of a row", {
  axis_labels <- function(p) {
    labels <- ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
    labels[!is.na(labels)]
  }

  # Hourly data, so the axis's own chronon is far finer than the calendar's
  # cells: pretty breaks land wherever they like, and would be labelled by the
  # second of the week they fall on.
  hourly <- data.frame(
    time = mixtime::datetime("2015-01-01 00:00:00") + 3600 * (0:200),
    value = seq_len(201)
  )
  p <- ggplot(hourly, aes(x = time, y = value)) + geom_line()

  # A break at every cell of the row, named as a position in the row's cycle.
  expect_equal(
    axis_labels(p + coord_calendar(rows = mixtime::weeks(1L), cols = NULL)),
    c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", "Mon")
  )
  # The cells are what is broken at, not the days in particular: a six hour
  # cell is one of the four the day holds, numbered within it.
  expect_equal(
    axis_labels(
      p +
        coord_calendar(
          rows = mixtime::days(1L),
          cells = mixtime::hours(6L),
          cols = NULL
        )
    ),
    c("h00", "h01", "h02", "h03", "h00")
  )
  # Without cells there is nothing to break at, so the scale's own breaks are
  # used (and labelled cyclically, as they were before).
  expect_lt(
    length(axis_labels(
      p + coord_calendar(rows = mixtime::weeks(1L), cells = NULL, cols = NULL)
    )),
    8L
  )
})

test_that("the calendar's breaks give way to the scale's own", {
  axis_labels <- function(p) {
    labels <- ggplot_build(p)$layout$panel_params[[1]]$x$get_labels()
    labels[!is.na(labels)]
  }

  daily <- data.frame(
    time = mixtime::date("2015-01-01") + 0:400,
    value = seq_len(401)
  )
  p <- ggplot(daily, aes(x = time, y = value)) + geom_line()
  weekly <- coord_calendar(rows = mixtime::weeks(1L), cols = NULL)

  # Breaks the user asked for, whether as values or as a granule of their own.
  expect_equal(
    axis_labels(
      p + weekly + scale_x_mixtime(time_breaks = mixtime::days(2L))
    ),
    c("Fri", "Sun", "Tue", "Thu")
  )
  expect_length(
    axis_labels(
      p + weekly + scale_x_mixtime(breaks = mixtime::date("2015-01-01") + 0:1)
    ),
    2L
  )
  # And labels the user asked for, at the calendar's own breaks.
  expect_equal(
    setdiff(
      axis_labels(
        p + weekly + scale_x_mixtime(time_labels = "{cyc(day, month)}")
      ),
      "NA"
    ),
    sprintf("%02d", c(29:31, 1:5))
  )

  # A row holding more cells than an axis can name keeps the scale's breaks: a
  # year of daily cells is 365 of them.
  expect_lt(
    length(axis_labels(
      p + coord_calendar(rows = mixtime::years(1L), cols = NULL)
    )),
    calendar_max_cell_breaks
  )

  # An axis with no cycle to name positions in is linear time, whose labels are
  # dates rather than positions in a row, so it keeps the scale's breaks too.
  expect_match(
    axis_labels(p + coord_calendar(rows = NULL, cols = mixtime::months(1L))),
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"
  )
})
