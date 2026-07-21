# Cut points are 0, 10, 20, 30, giving three loops of width 10.
cuts <- c(0, 10, 20, 30)

test_that("loop_index() assigns loops and clamps out of range values", {
  expect_equal(loop_index(c(0, 5, 10, 19, 20, 29), cuts), c(1, 1, 2, 2, 3, 3))

  # Values outside the cut range fall in the first/last loop rather than
  # producing an index of 0 or 4.
  expect_equal(loop_index(c(-100, -Inf, 30, 100, Inf), cuts), c(1, 1, 3, 3, 3))
  expect_equal(loop_index(NA_real_, cuts), NA_integer_)
})

test_that("fold_time() translates each loop onto the first", {
  expect_equal(fold_time(c(5, 15, 25), c(1, 2, 3), cuts), c(5, 5, 5))
  expect_equal(fold_time(c(0, 10, 20), c(1, 2, 3), cuts), c(0, 0, 0))
})

test_that("cut_pointwise() folds without adding or removing rows", {
  data <- data.frame(x = c(5, 15, 25), y = 1:3, group = 1L)
  cut <- cut_pointwise(data, "x", cuts)

  expect_equal(nrow(cut), 3L)
  expect_equal(cut$x, c(5, 5, 5))
  expect_equal(cut$y, 1:3)
  expect_equal(cut$.loop, c(1, 2, 3))
})

test_that("cut_pointwise() handles Inf and NA", {
  data <- data.frame(x = c(-Inf, 15, Inf, NA))
  cut <- cut_pointwise(data, "x", cuts)

  expect_equal(nrow(cut), 4L)
  expect_equal(cut$x, c(-Inf, 5, Inf, NA))
  # NA time still needs a usable loop for layouts that key off it.
  expect_equal(cut$.loop, c(1, 2, 3, 1))
})

test_that("cut_pointwise() folds every positional aesthetic on the time axis", {
  data <- data.frame(x = 15, xmin = 12, xmax = 25, y = 1)
  cut <- cut_pointwise(data, "x", cuts)

  expect_equal(cut$x, 5)
  expect_equal(cut$xmin, 2)
  expect_equal(cut$xmax, 5)
  # The non-time axis is untouched.
  expect_equal(cut$y, 1)
})

test_that("cut_connected() splits a path crossing one boundary", {
  data <- data.frame(x = c(5, 15), y = c(1, 3), group = 1L)
  cut <- cut_connected(data, "x", cuts)

  expect_equal(nrow(cut), 4L)
  # The boundary vertex is interpolated exactly, and duplicated: one closing the
  # first piece at the end of the window, one opening the second at its start.
  expect_equal(cut$x, c(5, 10, 0, 5))
  expect_equal(cut$y, c(1, 2, 2, 3))
  expect_equal(cut$.loop, c(1, 1, 2, 2))
  expect_equal(length(unique(cut$group)), 2L)
})

test_that("cut_connected() leaves a path within one loop folded but intact", {
  data <- data.frame(x = c(22, 25, 28), y = 1:3, group = 1L)
  cut <- cut_connected(data, "x", cuts)

  expect_equal(nrow(cut), 3L)
  expect_equal(cut$x, c(2, 5, 8))
  expect_equal(cut$y, 1:3)
  expect_equal(length(unique(cut$group)), 1L)
})

test_that("cut_connected() splits a segment spanning several loops at once", {
  # Only two vertices, but three loops: both intermediate boundaries must be
  # inserted rather than just the first.
  data <- data.frame(x = c(2, 28), y = c(0, 26), group = 1L)
  cut <- cut_connected(data, "x", cuts)

  expect_equal(cut$x, c(2, 10, 0, 10, 0, 8))
  expect_equal(cut$y, c(0, 8, 8, 18, 18, 26))
  expect_equal(cut$.loop, c(1, 1, 2, 2, 3, 3))
  expect_equal(length(unique(cut$group)), 3L)
})

test_that("cut_connected() gives ribbon upper and lower edges matching keys", {
  # GeomRibbon munches the upper edge left to right and the lower edge right to
  # left, as two separate paths keyed by `id`, then reassembles them with
  # polygonGrob(). Piece k of each edge must therefore get the same key.
  upper <- data.frame(x = c(2, 28), y = c(5, 6), id = 1)
  lower <- data.frame(x = c(28, 2), y = c(1, 0), id = 1)

  cut_upper <- cut_connected(upper, "x", cuts)
  cut_lower <- cut_connected(lower, "x", cuts)

  expect_equal(sort(unique(cut_upper$id)), sort(unique(cut_lower$id)))
  expect_equal(length(unique(cut_upper$id)), 3L)

  # Each loop gets one piece from each edge, so each id makes a closed ring.
  expect_equal(unname(table(cut_upper$id)), unname(table(cut_lower$id)))
})

test_that("cut_connected() keys ribbon ids clear of GeomRibbon's id offset", {
  # GeomRibbon offsets the lower edge's ids by max(ids) before drawing the
  # outline. Contiguous ids would make lower piece k collide with upper piece
  # k + max(ids) and draw a spurious line between them.
  upper <- cut_connected(data.frame(x = c(2, 28), y = 5, id = 1), "x", cuts)
  lower <- cut_connected(data.frame(x = c(28, 2), y = 0, id = 1), "x", cuts)

  expect_false(any(upper$id %in% (lower$id + 1)))
})

test_that("cut_connected() turns a rect ring spanning 3 loops into 3 rings", {
  # A rect arrives as a ring: top edge left to right, bottom edge right to left.
  data <- data.frame(
    x = c(2, 28, 28, 2),
    y = c(5, 5, 0, 0),
    group = 1L
  )
  cut <- cut_connected(data, "x", cuts)

  expect_equal(nrow(cut), 12L)
  expect_equal(length(unique(cut$group)), 3L)

  # GeomPolygon sorts by group before drawing, so check the rings that produces.
  rings <- split(cut[c("x", "y")], cut$group)
  expect_equal(
    lengths(lapply(rings, rownames)),
    c(4L, 4L, 4L),
    ignore_attr = TRUE
  )
  expect_equal(rings[[1]]$x, c(2, 10, 10, 2))
  expect_equal(rings[[2]]$x, c(0, 10, 10, 0))
  expect_equal(rings[[3]]$x, c(0, 8, 8, 0))
  expect_equal(rings[[1]]$y, c(5, 5, 0, 0))
  expect_equal(rings[[3]]$y, c(5, 5, 0, 0))
})

test_that("cut_connected() does not join separate paths", {
  data <- data.frame(x = c(5, 8, 25, 28), y = 1:4, group = c(1L, 1L, 2L, 2L))
  cut <- cut_connected(data, "x", cuts)

  # Nothing crosses a boundary within a group, so no vertices are inserted.
  expect_equal(nrow(cut), 4L)
  expect_equal(cut$x, c(5, 8, 5, 8))
  expect_equal(length(unique(cut$group)), 2L)
})

test_that("cut_connected() handles irregular cut spacing", {
  # Months, the case that broke the previous implementation's fixed-granularity
  # assumption.
  months <- as.numeric(seq(
    as.Date("2020-01-01"),
    as.Date("2020-05-01"),
    by = "1 month"
  ))
  data <- data.frame(
    x = as.numeric(as.Date(c("2020-01-15", "2020-04-15"))),
    y = c(0, 3),
    group = 1L
  )
  cut <- cut_connected(data, "x", months)

  expect_equal(cut$.loop, c(1, 1, 2, 2, 3, 3, 4, 4))
  # Every folded value sits within the first month's window.
  expect_true(all(cut$x >= months[1] & cut$x <= months[2]))
  # Boundary vertices land exactly on the month starts, not on a fixed stride.
  expect_equal(cut$x[2], months[2])
  expect_equal(cut$x[3], months[1])
})

test_that("cut_connected() copes with empty and single row input", {
  empty <- cut_connected(
    data.frame(x = numeric(), y = numeric(), group = integer()),
    "x",
    cuts
  )
  expect_equal(nrow(empty), 0L)

  one <- cut_connected(data.frame(x = 25, y = 1, group = 1L), "x", cuts)
  expect_equal(nrow(one), 1L)
  expect_equal(one$x, 5)
})

test_that("cut_connected() propagates NA without inventing vertices", {
  data <- data.frame(x = c(5, NA, 25), y = 1:3, group = 1L)
  cut <- cut_connected(data, "x", cuts)

  expect_equal(nrow(cut), 3L)
  expect_true(is.na(cut$x[2]))
})

test_that("cut_connected() works on the y axis", {
  data <- data.frame(x = c(1, 3), y = c(5, 15), group = 1L)
  cut <- cut_connected(data, "y", cuts)

  expect_equal(cut$y, c(5, 10, 0, 5))
  expect_equal(cut$x, c(1, 2, 2, 3))
})

test_that("loop_cuts() does not add an empty trailing loop", {
  # `time_ceiling()` already rounds past the end of the data, so closing the
  # last loop must not extend beyond it: an extra cut would add a loop holding
  # nothing, which `coord_calendar()` lays out as an empty row.
  df <- data.frame(
    time = seq(as.Date("1973-01-01"), as.Date("1978-12-01"), by = "1 month"),
    value = 1
  )
  built <- ggplot_build(
    ggplot(df, aes(time, value)) +
      geom_line() +
      coord_loop(time_loops = "1 year")
  )
  cuts <- built$layout$panel_params[[1]]$time_cuts

  # Six years of data means six loops, so seven cuts.
  expect_equal(length(cuts) - 1L, 6L)
  expect_equal(cuts[length(cuts)], as.Date("1979-01-01"))
})

test_that("loop_cuts() closes an explicit final loop wide enough for its data", {
  # The last loop point needs an end. It must cover data extending past it,
  # rather than being closed a fixed unit later and folding that data out of
  # the drawn window.
  df <- data.frame(
    time = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "1 day"),
    value = 1
  )
  loops <- as.Date(c("2020-01-01", "2020-04-01", "2020-07-01"))
  built <- ggplot_build(
    ggplot(df, aes(time, value)) + geom_line() + coord_loop(loops = loops)
  )
  cuts <- built$layout$panel_params[[1]]$time_cuts

  expect_equal(length(cuts) - 1L, 3L)
  expect_gte(cuts[length(cuts)], max(df$time))
})

test_that("rekey_loops() rejects ids that would silently mis-draw", {
  # `polygonGrob()` coerces `id` to integer, turning anything past integer range
  # into NA with only a warning, so this has to be caught rather than drawn.
  too_many <- ceiling(.Machine$integer.max / loop_id_stride) + 1
  expect_error(
    rekey_loops(data.frame(id = too_many), loop = 1L, n_loops = 1L),
    "Too many pieces"
  )
  expect_error(
    rekey_loops(data.frame(id = loop_id_stride), loop = 1L, n_loops = 1L),
    "Too many groups"
  )
})
