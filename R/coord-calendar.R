#' Calendar coordinates
#'
#' The calendar coordinate system arranges time series data into a calendar-like
#' layout, making it easier to see fine-grained temporal patterns over a long
#' time span. It has similar semantics as the looped coordinate system
#' ([coord_loop()]), however instead of overlaying looped data the calendar
#' coordinate space arranges each loop into rows and columns like a calendar.
#'
#' @inheritParams coord_loop
#'
#' @param rows Layout the time scale into calendar rows, one of:
#'   - `NULL` or `waiver()` for no rows (the default)
#'   - A `mixtime` vector giving time points at which the `time` axis should layout into rows
#'   - A function that takes the limits as input and returns row layout points as output
#' @param time_rows A duration giving the distance between calendar rows like
#' "1 weeks", or "1 month". If both `rows` and `time_rows` are
#' specified, `time_rows` wins.
#' @param cols,time_cols Not yet supported.
# @param cols Layout the time scale into calendar columns, one of:
#   - `NULL` or `waiver()` for no columns (the default)
#   - A `mixtime` vector giving time points at which the `time` axis should layout into columns
#   - A function that takes the limits as input and returns column layout points as output
# @param time_cols A duration giving the distance between calendar columns like
# "1 weeks", or "1 month". If both `cols` and `time_cols` are
# specified, `time_cols` wins.
#'
#' @details
#' This coordinate system is particularly useful for visualizing long time spans
#' with events that occur over short intervals (such as holidays).
#'
#' It works by:
#'
#' \enumerate{
#'   \item Dividing the time axis into segments based on the specified row (and column) periods
#'   \item Folding the time values of every segment into the first segment's window
#'   \item Cutting geometries that cross a segment boundary, and placing each
#'     piece into its own calendar row
#' }
#'
#' As for [coord_loop()], the layout is applied to the data rather than to the
#' drawing, so the panel contents are drawn only once regardless of how many
#' rows the calendar has.
#'
#' @section Practical usage:
#'
#' The calendar coordinate system arranges a cartesian coordinate system into a
#' dense calendar-like layout. Calendar layouts are particularly useful for
#' identifying specific dates or events that occur over short intervals in long
#' series. For example, the daily pedestrian counts at Melbourne's Birrarung
#' Marr park is nearby to several major sporting venues, and the calendar layout
#' makes obvious the spikes in pedestrian activity that occur during annual
#' sporting events (such as the Australian Open tennis tournament). Calendar
#' layouts are also useful to identify the effect of holidays, especially when
#' their dates change each year (such as Easter).
#'
#' Similarly to [coord_loop()], the calendar coordinate system draws geometries
#' that cross the boundaries of calendar rows or columns. The justification of
#' these geometries can be controlled with the `align_discrete` parameter of
#' [scale_x_mixtime()] as described in [coord_loop()].
#'
#' The calendar coordinate system works well in conjunction with facetting to
#' give more space between months and/or years of the calendar. When facetting,
#' using `scales = "free_x"` is recommended to make each facet only include
#' time periods appropriate for that panel.
#'
#' @examples
#' library(ggplot2)
#' library(mixtime)
#'
#' # Hourly pedestrian counts in Melbourne, as mixtime time points.
#' pedestrian <- tsibble::pedestrian |>
#'   dplyr::mutate(Time = datetime(Date_Time))
#'
#' # A weekly calendar arrangement of pedestrian counts in Melbourne
#' # Notice the periods of high activity days for the Birrarung Marr sensor
#' # during the Australian Open tennis tournament in late January.
#' pedestrian |>
#'   dplyr::filter(Time < datetime("2015-02-01 00:00:00")) |>
#'   ggplot(aes(x = Time, y = Count, color = Sensor)) +
#'   geom_line() +
#'   coord_calendar(time_rows = "1 week") +
#'   scale_x_mixtime(
#'     time_breaks = "1 day",
#'     time_labels = "{cyc(day, cal_isoweek$week, label = TRUE, abbreviate = TRUE)}"
#'   ) +
#'   theme(legend.position = "bottom")
#'
#' # Monthly facets can be used to create a complete calendar for 2015.
#' pedestrian |>
#'   dplyr::filter(year(Time) == year(2015)) |>
#'   ggplot(aes(x = Time, y = Count, color = Sensor)) +
#'   geom_line() +
#'   coord_calendar(time_rows = "1 week") +
#'   facet_wrap(vars(yearmonth(Time)), ncol = 4, scales = "free_x") +
#'   scale_x_mixtime(
#'     time_breaks = "1 day",
#'     time_labels = "{cyc(day, cal_isoweek$week, label = TRUE, abbreviate = TRUE)}"
#'   ) +
#'   theme(
#'     legend.position = "bottom",
#'     axis.text.y = element_blank(), axis.ticks.y = element_blank()
#'   )
#'
#' @importFrom gtable gtable_col gtable_row
#' @export
coord_calendar <- function(
  rows = waiver(),
  time_rows = waiver(),
  cols = waiver(),
  time_cols = waiver(),
  time = "x",
  xlim = NULL,
  ylim = NULL,
  expand = FALSE,
  default = FALSE,
  clip = "on",
  coord = coord_cartesian()
) {
  if (!is_waiver(cols) || !is_waiver(time_cols)) {
    cli::cli_abort(
      "{.arg cols} and {.arg time_cols} are not yet supported by {.fn coord_calendar}."
    )
  }

  # A calendar is a loop whose pieces are stacked into rows rather than
  # superimposed, so all of the cutting and folding is inherited from
  # `coord_loop()` and only the layout of the pieces is overridden.
  base_coord <- coord_loop(
    loops = rows,
    time_loops = time_rows,
    time = time,
    xlim = xlim,
    ylim = ylim,
    expand = expand,
    default = default,
    clip = clip,
    coord = coord
  )

  ggplot2::ggproto(NULL, CoordCalendar(base_coord))
}

#' @rdname ggplot2-ggproto
#' @keywords internal
CoordCalendar <- function(coord) {
  force(coord)
  ggplot2::ggproto(
    "CoordCalendar",
    coord,

    # The row count actually used. Derived across panels, so it is state rather
    # than input, and is reset at the start of every build by `setup_params()`.
    .n_row = 1L,

    # `setup_params()` runs once per build, before any panel is set up. Resetting
    # here is what keeps the `max()` below from accumulating across builds: the
    # coord object outlives a single build, so a plot reusing a coord that had
    # previously drawn more rows would otherwise keep the larger row count.
    setup_params = function(self, data) {
      self$.n_row <- 1L
      ggproto_parent(coord, self)$setup_params(data)
    },

    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
      if (!inherits(self, "CoordCartesian")) {
        cls <- setdiff(class(self), c("CoordCalendar", "CoordLoop"))[1L]
        cli::cli_abort("{.fn coord_calendar} does not support {.cls {cls}}.")
      }

      params <- ggproto_parent(coord, self)$setup_panel_params(
        scale_x,
        scale_y,
        params
      )

      if (is.null(self$n_row)) {
        # One row per cut loop, taking the maximum across panels so that facets
        # share a layout and a calendar row is the same height in every facet.
        self$.n_row <- max(self$.n_row, length(params$time_cuts) - 1L)
      }

      params
    },

    # Loop `k` becomes calendar row `k`, counting from the top of the panel.
    # `CoordLoop$transform()` has already cut, folded and rescaled the data, so
    # placing a piece into its row is just an affine squeeze along the
    # non-time axis.
    arrange_loops = function(self, data, panel_params) {
      row <- data$.loop
      data$.loop <- NULL
      n_row <- self$.n_row
      if (is.null(row) || n_row == 1L) {
        return(data)
      }

      offset <- (n_row - row) / n_row
      stacked <- if (self$is_flipped) {
        ggplot_global$x_aes
      } else {
        ggplot_global$y_aes
      }
      for (col in intersect(stacked, names(data))) {
        data[[col]] <- data[[col]] / n_row + offset
      }
      data
    },

    # `CoordLoop` already renders decoration for a single loop window; a
    # calendar just repeats that window down the panel.
    render_fg = function(self, panel_params, theme) {
      fg <- ggproto_parent(coord, self)$render_fg(panel_params, theme)
      repeat_grob_in_rows(fg, self$.n_row, self$is_flipped)
    },

    render_bg = function(self, panel_params, theme) {
      bg <- ggproto_parent(coord, self)$render_bg(panel_params, theme)
      repeat_grob_in_rows(bg, self$.n_row, self$is_flipped)
    },

    render_axis_v = function(self, panel_params, theme) {
      axis_grobs <- ggproto_parent(coord, self)$render_axis_v(
        panel_params,
        theme
      )
      if (!self$is_flipped) {
        # TODO: factor out (see note in repeat_grob_in_rows)
        height <- 1 / self$.n_row
        axis_grobs <- lapply(axis_grobs, function(grob) {
          gtable_col(
            "y_axis",
            replicate(self$.n_row, grob, simplify = FALSE),
            width = grobWidth(grob),
            heights = unit(rep(height, self$.n_row), "npc")
          )
        })
      }
      axis_grobs
    },

    render_axis_h = function(self, panel_params, theme) {
      axis_grobs <- ggproto_parent(coord, self)$render_axis_h(
        panel_params,
        theme
      )
      if (self$is_flipped) {
        # TODO: factor out (see note in repeat_grob_in_rows)
        width <- 1 / self$.n_row
        axis_grobs <- lapply(axis_grobs, function(grob) {
          gtable_row(
            "x_axis",
            replicate(self$.n_row, grob, simplify = FALSE),
            height = grobHeight(grob),
            widths = unit(rep(width, self$.n_row), "npc")
          )
        })
      }
      axis_grobs
    }
  )
}

#' Flip the x and y axes of a grid function
#' @param f a \pkg{grid} function, like [viewport()] or [rectGrob()].
#' @param is_flipped should it be flipped?
#' @returns function with the same parameters as `f` but with positional axis
#' arguments (`x`, `y`, `width`, `height`, ...) swapped.
#' @noRd
flip_grid_fun <- function(f, is_flipped) {
  if (!is_flipped) {
    return(f)
  }
  new_f <- function(x, y, width, height, just, hjust, vjust, ...) {
    f(
      x = y,
      y = x,
      width = height,
      height = width,
      just = rev(just),
      hjust = vjust,
      vjust = hjust,
      ...
    )
  }
  formals(new_f)[c("x", "y", "width", "height", "hjust", "vjust")] <-
    formals(f)[c("y", "x", "height", "width", "vjust", "hjust")]
  new_f
}

#' Replicate a grob in rows
#' @param grob a grob
#' @param n_row number of rows
#' @param is_flipped if `TRUE`, repeat in columns
#' @noRd
repeat_grob_in_rows <- function(grob, n_row, is_flipped) {
  # TODO: should be able to factor out a common implementation between this,
  # render_axis_h, and render_axis_v (gtable was being finicky for me about z
  # orders so I left this for now)
  height <- 1 / n_row
  .viewport <- flip_grid_fun(viewport, is_flipped)
  ys <- 1 - seq_len(n_row) / n_row
  height <- unit(1 / n_row, "npc")
  grobs <- lapply(ys, function(y) {
    grobTree(
      grob,
      vp = .viewport(
        x = unit(0, "npc"),
        y = unit(y, "npc"),
        just = c(0, 0),
        height = height
      )
    )
  })
  inject(grobTree(!!!grobs))
}
