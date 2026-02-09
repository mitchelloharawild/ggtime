test_that("ljust works", {
  skip_if_no_r42_graphics()

  df <- data.frame(
    year = c(1846, 1847, 1848, 1849, 1850, 1851, 1852, 1853, 1854, 1855, 1856, 1857),
    lynx = c(45150, 49150, 39520, 21230, 8420, 5560, 5080, 10170, 19600, 32910, 34380, 29590),
    peak = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  )

  p <- df |>
    ggplot(aes(x = year, y = lynx)) +
    geom_line(alpha = 0.25) +
    geom_point(aes(color = ordered(year)), size = 3, data = df[df$peak, ]) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 1))

  vdiffr::expect_doppelganger("loop + ljust = 0",
    p + coord_loop(loop = df$year[df$peak], ljust = 0, expand = c(TRUE, FALSE)),
    writer = write_svg_r42
  )

  vdiffr::expect_doppelganger("loop + ljust = 0.5",
    p + coord_loop(loop = df$year[df$peak], ljust = 0.5, expand = c(TRUE, FALSE)),
    writer = write_svg_r42
  )

  vdiffr::expect_doppelganger("loop + ljust = 1",
    p + coord_loop(loop = df$year[df$peak], ljust = 1, expand = c(TRUE, FALSE)),
    writer = write_svg_r42
  )

  x <- 0:93
  p <- tibble(
    time = as.Date("2025-04-01") + x,
    sin = sin(x/pi/31*20) + x/100,
    month = format(time, "%m")
  ) |>
    ggplot(aes(x = time, y = sin,
      color = month,
      group = NA)) +
    geom_path() +
    geom_point(size = 2) +
    geom_vline(xintercept = as.Date("2025-04-01"))

  vdiffr::expect_doppelganger("time_loop + ljust = 0",
    p + coord_loop(time_loop = "1 month", ljust = 0, expand = c(TRUE, FALSE)),
    writer = write_svg_r42
  )

  vdiffr::expect_doppelganger("time_loop + ljust = 0.5",
    p + coord_loop(time_loop = "1 month", ljust = 0.5, expand = c(TRUE, FALSE)),
    writer = write_svg_r42
  )

  vdiffr::expect_doppelganger("time_loop + ljust = 1",
    p + coord_loop(time_loop = "1 month", ljust = 1, expand = c(TRUE, FALSE)),
    writer = write_svg_r42
  )
})

test_that("coord_loop works with coord_radial", {
  skip_if_not_installed("vdiffr")

  x <- 0:93
  df <- tibble(
    time = as.Date("2025-04-01") + x,
    sin = sin(x/pi/31*20) + x/100,
    g = rep(c("a","b"), 47),
    month = format(time, "%m")
  )
  p <- df |>
    ggplot(aes(x = time, y = sin, color = month, group = NA)) +
    geom_path() +
    geom_point(size = 2) +
    geom_vline(xintercept = as.Date("2025-04-01"))

  vdiffr::expect_doppelganger("radial + ljust = 0.5",
    p + coord_loop(time_loop = "1 month", expand = c(TRUE, FALSE), coord = coord_radial())
  )
})

test_that("clip_loops works", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    year = c(1847, 1848, 1849, 1850, 1851, 1852, 1853, 1854, 1855, 1856),
    lynx = c(49150, 39520, 21230, 8420, 5560, 5080, 10170, 19600, 32910, 34380),
    peak = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )

  p <- df |>
    ggplot(aes(x = year, y = lynx)) +
    geom_line(alpha = 0.25) +
    geom_point(aes(color = ordered(year)), size = 3, data = df[df$peak, ]) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 1))

  vdiffr::expect_doppelganger("time_loops + clip_loops = off",
    p + coord_loop(time_loops = 9, expand = c(TRUE, FALSE), clip_loops = "off")
  )
})
