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
#' @param clip_rows Should the drawing of each loop of the timescale be clipped to
#'   the breaks defined by `time_rows` and `ljust`?
#'   A setting of `"on"` (the default) means yes, and a setting of `"off"` means no.
#'
#' @details
#' This coordinate system is particularly useful for visualizing long time spans
#' with events that occur over short intervals (such as holidays).
#'
#' It works by:
#'
#' \enumerate{
#'   \item Dividing the time axis into segments based on the specified row (and column) periods
#'   \item Translating each panel into the rows and columns of a calendar layout
#' }
#'
#' The coordinate system requires R version 4.2.0 or higher due to its use of
#' usage of clipping paths.
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
#' these geometries can be controlled with the `ljust` parameter, as described
#' in [coord_loop()].
#'
#' The calendar coordinate system works well in conjunction with facetting to
#' give more space between months and/or years of the calendar. When facetting,
#' using `scales = "free_x"` is recommended to make each facet only include
#' time periods appropriate for that panel.
#'
#' @examples
#' library(ggplot2)
#'
#' # A weekly calendar arrangement of pedestrian counts in Melbourne
#' # Notice the periods of high activity days for the Birrarung Marr sensor
#' # during the Australian Open tennis tournament in late January.
#' tsibble::pedestrian |>
#'   dplyr::filter(Date < "2015-02-01") |>
#'   ggplot(aes(x = Date_Time, y = Count, color = Sensor)) +
#'   geom_line() +
#'   coord_calendar(time_rows = "1 week") +
#'   scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
#'   theme(legend.position = "bottom")
#'
#' # Monthly facets can be used to create a complete calendar for 2015.
#' tsibble::pedestrian |>
#'   dplyr::filter(lubridate::year(Date) == 2015) |>
#'   ggplot(aes(x = Date_Time, y = Count, color = Sensor)) +
#'   geom_line() +
#'   coord_calendar(time_rows = "1 week") +
#'   facet_wrap(
#'     vars(lubridate::month(Date, label = TRUE)),
#'     ncol = 4, scales = "free_x"
#'   ) +
#'   scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
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
  ljust = 0.5,
  xlim = NULL,
  ylim = NULL,
  expand = FALSE,
  default = FALSE,
  clip = "on",
  clip_rows = "on",
  coord = coord_cartesian()
) {
  if (!is_waiver(cols) || !is_waiver(time_cols)) {
    stop(
      "`cols` and `time_cols` are not currently supported in coord_calendar()"
    )
  }

  # TODO: maybe don't want an inheritance relationship here
  # (would need to factor out common setup_panel_params / draw_panel code)
  base_coord <- coord_loop(
    loops = rows,
    time_loops = time_rows,
    time = time,
    ljust = ljust,
    xlim = xlim,
    ylim = ylim,
    expand = expand,
    default = default,
    clip = clip,
    clip_loops = clip_rows
  )

  ggplot2::ggproto(
    NULL,
    CoordCalendar(base_coord)
  )
}

#' @rdname ggplot2-ggproto
#' @keywords internal
CoordCalendar <- function(coord) {
  force(coord)
  ggplot2::ggproto(
    "CoordCalendar",
    coord,

    n_row = 1L,

    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
      if (!inherits(self, "CoordCartesian")) {
        cls <- setdiff(class(self), c("CoordCalendar", "CoordLoop"))[1L]
        stop("coord_calendar(coord = <", cls, ">) is not supported.")
      }

      params <- ggproto_parent(coord, self)$setup_panel_params(
        scale_x,
        scale_y,
        params
      )

      # Determine row for each cut region
      # TODO: Currently this just lays everything out in a column, but n_row
      # and/or time_rows presumably should come from somewhere that knows stuff
      # about the layout of the calendar.
      n_row_local <- length(params$time_cuts) - 1L
      self$n_row <- max(self$n_row, n_row_local)
      params$time_rows <- seq_len(n_row_local)

      params
    },

    render_fg = function(self, panel_params, theme) {
      fg <- ggproto_parent(coord, self)$render_fg(panel_params, theme)
      repeat_grob_in_rows(fg, self$n_row, self$is_flipped)
    },

    render_bg = function(self, panel_params, theme) {
      bg <- ggproto_parent(coord, self)$render_bg(panel_params, theme)
      repeat_grob_in_rows(bg, self$n_row, self$is_flipped)
    },

    render_axis_v = function(self, panel_params, theme) {
      axis_grobs <- ggproto_parent(coord, self)$render_axis_v(
        panel_params,
        theme
      )
      if (!self$is_flipped) {
        # TODO: factor out (see note in repeat_grob_in_rows)
        height <- 1 / self$n_row
        axis_grobs <- lapply(axis_grobs, function(grob) {
          gtable_col(
            "y_axis",
            replicate(self$n_row, grob, simplify = FALSE),
            width = grobWidth(grob),
            heights = unit(rep(height, self$n_row), "npc")
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
        width <- 1 / self$n_row
        axis_grobs <- lapply(axis_grobs, function(grob) {
          gtable_row(
            "x_axis",
            replicate(self$n_row, grob, simplify = FALSE),
            height = grobHeight(grob),
            widths = unit(rep(width, self$n_row), "npc")
          )
        })
      }
      axis_grobs
    }
  )
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
