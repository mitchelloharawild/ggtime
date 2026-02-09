test_that("inf works", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    year = c(1845, 1846, 1847, 1848, 1849, 1850, 1851,
      1852, 1853, 1854, 1855, 1856, 1857, 1858, 1859, 1860, 1861, 1862,
      1863, 1864, 1865, 1866, 1867, 1868, 1869, 1870, 1871, 1872, 1873,
      1874, 1875, 1876, 1877),
    lynx = c(30090, 45150, 49150, 39520,
      21230, 8420, 5560, 5080, 10170, 19600, 32910, 34380, 29590, 21300,
      13690, 7650, 4080, 4090, 14330, 38220, 60780, 70770, 72770, 42680,
      16390, 9830, 5800, 5260, 18910, 30950, 31180, 46340, 45770),
    peak = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE,
      FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  )

  p <- df |>
    ggplot(aes(x = year, y = lynx)) +
    geom_line(alpha = 0.25) +
    geom_hline(yintercept = 40000, color = "red") +
    geom_vline(xintercept = 1848) +
    annotate("segment", x = -Inf, xend = Inf, y = 30000, yend = 30000) +
    scale_x_continuous(breaks = seq(min(df$year), max(df$year), by = 1))

  vdiffr::expect_doppelganger("inf segment and hline",
    p + coord_calendar(rows = df$year[df$peak], expand = FALSE, clip_rows = FALSE)
  )
})
