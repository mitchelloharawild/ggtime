#' Looped coordinates
#'
#' The looped coordinate system loops the cartesian coordinate system around
#' specific loop points. This is particularly useful for visualising seasonal
#' patterns that repeat over calendar periods, since the shape of seasonal
#' patterns can be more easily seen when superimposed on top of each other.
#'
#' @param loops Loop the time scale around a calendrical granularity, one of:
#'   - `NULL` or `waiver()` for no looping (the default)
#'   - A `mixtime` vector giving time points at which the `time` axis should loop
#'   - A function that takes the limits as input and returns loop points as output
#' @param time_loops A duration giving the distance between temporal
#' loops like "2 weeks", or "10 years". If both `loops` and `time_loops` are
#' specified, `time_loops` wins.
#' @param time A string specifying which aesthetic contains the time variable that
#'   should be looped over. Default is `"x"`.
#' @param ljust Loop justification, a number between 0 and 1
#'   indicating where the lines between looped positions are drawn
#'   (0 indicates left, 1 indicates right, 0.5 indicates center).
#' @param xlim,ylim Limits for the x and y axes. `NULL` means use the default limits.
#' @param expand Logical indicating whether to expand the coordinate limits.
#'   Default is `FALSE`.
#' @param default Logical indicating whether this is the default coordinate system.
#'   Default is `FALSE`.
#' @param clip Should drawing be clipped to the extent of the plot panel?
#'   A setting of `"on"` (the default) means yes, and a setting of `"off"` means no.
#' @param clip_loops Should the drawing of each loop of the timescale be clipped to
#'   the breaks defined by `time_loops` and `ljust`?
#'   A setting of `"on"` (the default) means yes, and a setting of `"off"` means no.
#' @param coord The underlying coordinate system to use. Default is `coord_cartesian()`.
#'
#' @details
#' This coordinate system is particularly useful for visualizing seasonal or
#' cyclic patterns in time series data. It works by:
#'
#' \enumerate{
#'   \item Dividing the time axis into segments based on the specified loop period
#'   \item Translating each segment to overlay on the first segment
#'   \item Creating a visualization where multiple time periods are superimposed
#' }
#'
#' The coordinate system requires R version 4.2.0 or higher due to its use of
#' usage of clipping paths.
#'
#' @section Practical usage:
#'
#' The looped coordinate system reveals patterns that repeat over regular time
#' periods, such as annual seasonality in monthly data, or weekly patterns in
#' daily data. It allows the `[x/y]` time aesthetic to be specified
#' continuously, and loops the time axis around specified time intervals. This
#' allows time within seasonal periods to be compared directly, and highlights
#' the shape of seasonal patterns. This is commonly used in time series analysis
#' to identify the peaks and troughs of seasonal patterns.
#'
#' A key advantage of time being specified continuously is that the connection
#' between the end of one seasonal period and the start of the next is
#' preserved. This is otherwise lost when time is discretised into ordered
#' factors (e.g. months of the year, or days of week). This allows lines and
#' other geometries to be drawn across seasonal boundaries, such as a line that
#' connects December to January when plotting annual seasonality.
#' The justification parameter `ljust` controls the side of the panel where
#' these connections are made, left justificiation (`ljust = 0`) gives space
#' for inter-seasonal time on the left of the panel, right justification
#' (`ljust = 1`) uses the right side of the panel, and center justification
#' (`ljust = 0.5`, the default) uses equal spacing on both ends of the season.
#'
#' @section Why not use seasonal factors?:
#'
#' Using factors to represent seasonal periods is common, but prone to errors
#' and is very limiting. Suppose you want to visualize weekly seasonality in
#' daily data. You could convert the date into a day of week factor (e.g. with
#' `lubridate::wday(date, label = TRUE)`), but this loses information about the
#' year and week of the observation. In order to correctly draw lines connecting
#' each day of the week (avoiding sawtooth patterns), you would additionally
#' need to group by year and week to separately identify each line segment. The
#' aesthetic mapping for plotting this pattern would look something like:
#'
#' ```
#' aes(
#'   x = lubridate::wday(date, label = TRUE),
#'   group = interaction(lubridate::year(date), lubridate::week(date)),
#'   y = value
#' )
#' ```
#'
#' These operations are error-prone, cumbersome, and are complicated to update
#' to show different seasonal patterns. For example, if you wanted to instead
#' show the annual seasonal pattern, both the `x` and `group` aesthetics would
#' need to be changed (to day of year and year respectively). Any errors in this
#' process would produce sawtooth patterns or other artifacts in the plot.
#'
#' Another common error in discretizing time into seasonal factors is
#' incorrect ordering of the factor levels. For example, if you instead used
#' `strftime(date, "%a")` to get the day of week, the levels would be sorted
#' alphabetically rather than in time order ("Fri", "Mon", "Sat", ...). No-one
#' wants to Monday to follow Friday!
#'
#' Discretizing time into seasonal factors also prevents plotting the seasonal
#' pattern across multiple granularities. For example when visualizing weekly
#' seasonality across data at daily and hourly frequencies, both day of week
#' and hour of week are needed. Since these factors have different levels, they
#' cannot be plotted on the same axis. In contrast, it is possible to plot both
#' daily and hourly data on the same axis using [scale_x_mixtime()], which can
#' then be looped over weekly periods with `coord_loop(time_loops = "1 week")`.
#'
#' Another subtle issue of using factors instead of continuous time is that
#' spacing between time points is regularized. For example, when plotting the
#' annual seasonal pattern with months as a factor, each month is given equal
#' width on the x-axis despite the fact that months have different lengths.
#'
#' @return A `Coord` ggproto object that can be added to a ggplot.
#'
#' @examples
#' library(ggplot2)
#' library(ggtime)
#'
#' # Basic usage with US accidental deaths data
#' uad <- tsibble::as_tsibble(USAccDeaths)
#' # Requires mixtime, POSIXct, or Date time types
#' uad$index <- as.Date(uad$index)
#'
#' p <- ggplot(uad, aes(x = index, y = value)) +
#'   geom_line()
#'
#' # Original plot
#' p
#'
#' # With yearly looping to show seasonal patterns
#' p + coord_loop(time_loop = "1 year")
#'
#' @export
coord_loop <- function(
  loops = waiver(),
  time_loops = waiver(),
  time = "x",
  ljust = 0.5,
  xlim = NULL,
  ylim = NULL,
  expand = FALSE,
  default = FALSE,
  clip = "on",
  clip_loops = "on",
  coord = coord_cartesian()
) {
  specialize_coord_loop(ggplot2::ggproto(
    NULL,
    CoordLoop(coord),
    loops = loops,
    time_loops = time_loops,
    time = time,
    ljust = ljust,
    limits = list(x = xlim, y = ylim),
    expand = expand,
    default = default,
    clip = clip,
    clip_loops = clip_loops
  ))
}

#' @rdname ggplot2-ggproto
#' @keywords internal
CoordLoop <- function(coord) {
  force(coord)
  ggplot2::ggproto(
    "CoordLoop",
    coord,

    # The name of the scale representing time within `panel_params`.
    # Usually but not always equal to `time`. Specializations of `CoordLoop`
    # must set this appropriately (in [specialize_coord_loop()]).
    time_scale = NULL,

    # Number of rows in the layout (e.g. for `coord_calendar()`).
    n_row = 1L,

    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
      self$is_clipped <- isTRUE(self$clip_loops %in% c("on", TRUE)) # could have stricter validation
      self$is_flipped <- isTRUE(self$time == "y")

      # We need to adjust the panel parameters so that the scale is zoomed in
      # on the first region (which we will translate all other regions onto in draw_panel).

      # Calculate the panel parmeters as normal (without cutting)
      # Need to do this so that user-defined limits, scale limits, expand, etc
      # are all appropriately taken into account
      uncut_params <- ggproto_parent(coord, self)$setup_panel_params(
        scale_x,
        scale_y,
        params
      )

      # Determine the cutpoints where we will loop
      if (is_waiver(self$loops)) {
        time_cuts <- cut_axis_time_loop(
          uncut_params,
          self$time_scale,
          self$time_loops,
          self$ljust
        )
      } else {
        time_cuts <- sort(unique(self$loops))
        time_cuts <- c(
          time_cuts - self$ljust,
          time_cuts[length(time_cuts)] + (1 - self$ljust)
        )
      }

      # Recalculate the panel parameters zoomed in on the first region.
      # Doing it this way should apply expand settings, etc, again.
      # (comment out this line to disable zooming for debugging)
      old_limits <- self$limits
      self$limits[[self$time_scale]] <- c(
        # Restart at the first time point
        time_cuts[1],
        # End at the longest time point in the loop
        time_cuts[1] + max(diff(time_cuts))
      )
      cut_params <- ggproto_parent(coord, self)$setup_panel_params(
        scale_x,
        scale_y,
        params
      )
      self$limits <- old_limits

      cut_params$time_cuts <- time_cuts
      cut_params$time_rows <- rep.int(1L, length(cut_params$time_cuts) - 1)
      cut_params$uncut <- uncut_params
      cut_params
    },

    range = function(self, panel_params) {
      # range needs to consider both the cut and the uncut scale so that (e.g.)
      # the position of infinities is correct
      Map(
        range,
        ggproto_parent(coord, self)$range(panel_params),
        ggproto_parent(coord, self)$range(panel_params$uncut)
      )
    }
  )
}

# specialization ----------------------------------------------------------

#' Specialize the implementation of coord_loop depending on the base coord
#'
#' [coord_loop()] wraps a base coord such as [coord_cartesian()] or
#' [coord_radial()]. This function is called by [CoordLoop()] to specialize an
#' instance for its underlying base coord by overriding methods needed to support
#' that base coord.
#' @param coord A [`ggproto`] object of class `CoordLoop`, which will inherit
#' from some other coord (as passed to `CoordLoop(coord = ...)`.
#' @param ... unused.
#' @details
#' Implement this method on a coord's class to provide support for that coord in
#' [coord_loop()]. Should return an object that inherits from the input `coord`.
#'
#' Specializations *must* implement:
#'
#' - `coord$time_scale`: The name of the time scale (e.g. `"x"`, `"y"`, ...):
#'   corresponds to the element of `panel_params` holding the `Scale` that
#'   handles time.
#'
#' Specializations *may need to* implement:
#'
#' - `coord$limits`: If the positional scales for this coord are not `x` and `y`
#'   (so `coord$time_scale` is not `"x"` or `"y"`), you may need to adjust
#'   `limits` to map limits from `xlim` and `ylim` onto the corresponding scales.
#' - `coord$transform()` and/or `coord$draw_panel()`: To transform coordinates
#'   into looped positions and/or transform grobs to create looping.
#'
#' We use a separate specialization function rather than making `CoordLoop()`
#' generic so that the default method of this generic can be an error
#' (representing an attempt to use an unsupported coord type).
#' @returns A [`ggproto`] object that inherits from `coord`. Raises an error
#' if no parent classes of `coord` are supported by [coord_loop()].
#' @noRd
specialize_coord_loop <- function(coord, ...) {
  UseMethod("specialize_coord_loop")
}

#' @export
specialize_coord_loop.default <- function(coord, ...) {
  cls <- setdiff(class(coord), "CoordLoop")[1L]
  stop("coord_loop(coord = <", cls, ">) is not supported.")
}

#' @export
specialize_coord_loop.CoordCartesian <- function(coord, ...) {
  force(coord)

  if (!isTRUE(coord$time %in% c("x", "y"))) {
    stop("coord_loop(coord = <CoordCartesian>, time = ...) requires time %in% c('x', 'y').")
  }

  ggplot2::ggproto(
    "CoordLoopCartesian",
    coord,

    time_scale = coord$time,

    transform = function(self, data, panel_params) {
      reverse <- panel_params$reverse %||% "none"
      x <- panel_params$x[[switch(reverse, xy = , x = "reverse", "rescale")]]
      y <- panel_params$y[[switch(reverse, xy = , y = "reverse", "rescale")]]
      data <- transform_position(data, x, y)
      # need to use the full range for squish_infinite otherwise segments with
      # infinite endpoints are not repeated over rows in coord_calendar
      range <- transform_position(self$backtransform_range(panel_params), x, y)
      transform_position(
        data,
        function(x) scales::squish_infinite(x, range$x),
        function(y) scales::squish_infinite(y, range$y)
      )
    },

    draw_panel = function(self, panel, params, theme) {
      check_can_clip("coord_loop(clip = 'on')", self$is_clipped)

      # Get cutpoints along the axis for dividing the panel grob into regions
      cuts <- params[[self$time_scale]]$rescale(params$time_cuts)

      translated_panels <- translate_and_superimpose_grobs(
        panel,
        cuts,
        params$time_rows,
        self$n_row,
        self$is_flipped,
        self$is_clipped
      )

      ggproto_parent(coord, self)$draw_panel(translated_panels, params, theme)
    }
  )
}

#' @export
specialize_coord_loop.CoordRadial <- function(coord, ...) {
  force(coord)

  if (!isTRUE(coord$time == coord$theta)) {
    stop("coord_loop(coord = <CoordRadial>, time = ...) requires time == coord$theta.")
  }

  ggplot2::ggproto(
    "CoordLoopRadial",
    coord,

    time_scale = "theta",
    limits = list(
      theta = coord$limits[[coord$theta]] %||% coord$super()$limits$theta,
      r = coord$limits[[coord$r]] %||% coord$super()$limits$r
    ),

    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
      params <- ggproto_parent(coord, self)$setup_panel_params(
        scale_x,
        scale_y,
        params
      )

      # construct a piecewise linear transformation that re-aligns the start of
      # each cut to the origin
      time_trans <- params$theta$get_transformation()$transform
      cuts <- time_trans(params$time_cuts) + self$ljust
      widths <- diff(cuts)
      origin <- cuts[1]
      rest <- cuts[-1]
      range <- diff(params$theta.range)
      prev_upper_limits <- origin + (seq_along(rest) - 1) * range + widths
      next_lower_limits <- origin + seq_along(rest) * range

      # NOTE: This is a hack that should be better. Basically the problem is
      # that we're trying to move points at the upper limit of a cut region to
      # the beginning of the next region, but to draw lines in between we need
      # to also move some munched points (from coord_munch()) that are between
      # the upper limit and the upper limit - eps where eps < 1 granularity
      # (cannot be == 1 or we'll move points that are before the upper limit).
      # Without something like this you just get an ugly straight line from
      # the end of one loop to the beginning of the next (instead of a curve).
      # A better solution would be if coord_munch were generic and we could
      # implement a method for it to insert the needed points to make a curve.
      #
      # The upshot is this currently only works properly (ish) if the input
      # scale transforms to a numeric where the smallest granularity is 1.
      eps <- 0.99

      time_unaligned <- c(
        origin - range,
        origin,
        vctrs::vec_interleave(rest - eps, rest),
        rest[length(rest)] + range
      )
      time_aligned <- c(
        origin - range,
        origin,
        vctrs::vec_interleave(prev_upper_limits - eps, next_lower_limits),
        next_lower_limits[length(next_lower_limits)] + range
      )
      params$align <- stats::approxfun(
        time_unaligned,
        time_aligned,
        ties = "ordered",
        rule = 2
      )

      # since we can't do proper clipping, the hackish solution I've come up
      # with is to make data in the gaps between loops transparent --- so we
      # need a function to identify if a value is in the gap
      in_gap_indicator <- stats::stepfun(
        vctrs::vec_interleave(
          prev_upper_limits - self$ljust * eps,
          next_lower_limits - self$ljust * eps
        ),
        c(0, rep(c(1, 0), length(rest))),
        ties = "min"
      )
      params$in_gap <- function(x) {
        in_gap_indicator(x) %in% 1  # NA => FALSE
      }

      params
    },

    transform =  function(self, data, panel_params) {
      # align the starting point of each loop
      data[[self$theta]] <- panel_params$align(data[[self$theta]])

      # emulate clipping --- this is a hack but I can't see a way to do proper clipping
      if (self$is_clipped) {
        in_gap <- panel_params$in_gap(data[[self$theta]])
        if (any(in_gap)) {
          data[in_gap, "colour"] = "transparent"
          data[in_gap, "fill"] = "transparent"
          data[in_gap, "alpha"] = 0
        }
      }

      ggproto_parent(coord, self)$transform(data, panel_params)
    }
  )
}


# helpers -----------------------------------------------------------------

#' Check that clipping grobs are supported
#' @param context string giving the context (e.g. function, arguments, etc)
#' to use in message in case of failure
#' @param check if not `TRUE` no check is made
#' @returns `invisible(NULL)` if clipping is supported or raises an error if not.
check_can_clip <- function(context, check) {
  if (check && !ggplot2::check_device("clippingPaths")) {
    stop(context, " requires R v4.2.0 or higher.")
  }
}

#' Translate and superimpose grobs at specified cutpoints along x (or y) axis
#' @param grobs list of grobs
#' @param cuts x (or y if `is_flipped`) positions to cut along
#' @param rows vector with length = `length(cuts) - 1` giving
#' destination row id of each corresponding cut region (starting from 1).
#' @param n_row maximum row in the layout
#' @param is_flipped are the axes flipped? (so `cuts` are `y` positions).
#' @param is_clipped should output regions be clipped?
#' @noRd
translate_and_superimpose_grobs <- function(
  grobs,
  cuts,
  rows,
  n_row,
  is_flipped = FALSE,
  is_clipped = FALSE
) {
  origin <- cuts[[1]]
  xs <- cuts[-length(cuts)]
  widths <- diff(cuts)

  ys <- 1 - rows / n_row
  height <- 1 / n_row

  # Translate and superimpose the panel grob on itself repeatedly.
  # I attempted to use defineGrob() + useGrob() here to improve efficiency
  # (since theoretically it allows efficient repitition of a single grob
  # drawn off-screen), I couldn't figure out how to make the grob defined
  # by defineGrob() not be clipped by the first region it would theoretically
  # have been drawn into given where it is in the grob tree, which meant
  # it was always getting clipped. So I stuck to just repeatedly re-drawing
  # the same grob.
  .viewport <- flip_grid_fun(viewport, is_flipped)
  .rectGrob <- flip_grid_fun(rectGrob, is_flipped)
  grob <- inject(grobTree(!!!grobs))
  translated_grobs <- .mapply(
    function(x, y, width) {
      grobTree(
        grob,
        vp = .viewport(
          x = unit(origin - x, "npc"),
          y = unit(y, "npc"),
          height = unit(height, "npc"),
          just = c(0, 0),
          clip = if (is_clipped) {
            .rectGrob(
              unit(x, "npc"),
              y = unit(0, "npc"),
              width = unit(width, "npc"),
              height = unit(1, "npc"),
              just = c(0, 0)
            )
          } else {
            "inherit"
          }
        )
      )
    },
    list(xs, ys, widths),
    NULL
  )

  # # Uncomment for debug info --- region boundaries and centers
  # centers <- rowMeans(embed(cuts, 2))
  # translated_grobs <- c(
  #   translated_grobs,
  #   list(
  #     rectGrob(x = unit(centers, "npc"), y = unit(rep(0.5, length(centers)), "npc"), width = unit(widths, "npc"), gp = gpar(col = "red", fill = NA)),
  #     pointsGrob(x = unit(centers, "npc"), y = unit(rep(0.5, length(centers)), "npc"), gp = gpar(col = "red"))
  #   )
  # )

  translated_grobs
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

#' Get cutpoints along a positional axis for creating a time loop
#' @param panel_params Panel params, e.g. as returned by `Coord$setup_panel_params()`
#' and passed to `Coord$draw_panel(params = ...)`
#' @param axis Axis to cut (`"x"` or `"y"`).
#' @param by Duration to cut by
#' @param ljust Loop justification, a number between 0 and 1
#' @returns vector of time cutpoints
#' @noRd
cut_axis_time_loop <- function(panel_params, axis, by, ljust) {
  trans <- panel_params[[axis]]$get_transformation()
  range <- panel_params[[axis]]$limits
  time_range <- trans$inverse(range)
  if (is.character(by)) {
    time_range[1] <- lubridate::floor_date(time_range[1], by, week_start = 1L)
    time_range[2] <- lubridate::ceiling_date(time_range[2], by, week_start = 1L)
  }
  time_cuts <- unique(c(
    seq(time_range[1], time_range[2] + 1, by = by),
    time_range[2] + 1
  )) - ljust
  time_cuts
}
