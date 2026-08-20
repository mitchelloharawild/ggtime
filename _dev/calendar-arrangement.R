# Print a coord_calendar() layout as an ASCII calendar.
#
# The quickest way to see whether the coordinate space arranges days the way a
# printed calendar does: one point per day, then read back where each one
# landed from the npc coordinates `transform()` produced and the tile geometry
# the panel is drawn with. The regression test in
# tests/testthat/test-coord-calendar.R ("a day is placed in the calendar cell a
# printed calendar puts it in") asserts the same thing.

library(ggplot2)
library(mixtime)
pkgload::load_all()

#' Where each date lands in the calendar grid
#' @param dates One point per date.
#' @param cells Positions per row, i.e. days per week.
#' @param ... Passed to `coord_calendar()`.
calendar_places <- function(dates, ..., cells = 7L) {
  built <- ggplot_build(
    ggplot(data.frame(time = dates, value = 1), aes(x = time, y = value)) +
      geom_point() +
      coord_calendar(...)
  )
  coord <- built$plot$coordinates
  pos <- coord$transform(
    built$data[[1]][, c("x", "y")],
    built$layout$panel_params[[1]]
  )

  layout <- coord$grid_layout()
  col <- findInterval(pos$x, layout$col$x)
  within <- (pos$x - layout$col$x[col]) / layout$col$width[col]
  data.frame(
    date = dates,
    col = col,
    row = length(layout$row$y) - findInterval(pos$y, rev(layout$row$y)) + 1L,
    cell = floor(within * cells + 1e-9) + 1L
  )
}

#' Draw one block per column, a day per cell
draw_calendar <- function(places, labels = format(places$date, "%d")) {
  for (column in sort(unique(places$col))) {
    in_col <- places$col == column
    grid <- matrix("  ", nrow = max(places$row), ncol = max(places$cell))
    grid[cbind(places$row[in_col], places$cell[in_col])] <- labels[in_col]
    cat("column", column, "\n")
    cat(paste0(" ", apply(grid, 1, paste, collapse = " ")), sep = "\n")
    cat("\n")
  }
}

dates <- seq(as.Date("2015-01-01"), as.Date("2015-03-31"), by = "day")
places <- calendar_places(
  dates,
  row = mixtime::weeks(1L),
  col = mixtime::months(1L),
  pane = NULL
)
draw_calendar(places)

# Each position within a row is one weekday, the same one in every column.
table(places$cell, weekdays(places$date, abbreviate = TRUE))
