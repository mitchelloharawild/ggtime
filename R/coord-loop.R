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
#' @param xlim,ylim Limits for the x and y axes. `NULL` means use the default limits.
#' @param expand Logical indicating whether to expand the coordinate limits.
#'   Default is `FALSE`.
#' @param default Logical indicating whether this is the default coordinate system.
#'   Default is `FALSE`.
#' @param clip Should drawing be clipped to the extent of the plot panel?
#'   A setting of `"on"` (the default) means yes, and a setting of `"off"` means no.
#' @param coord The underlying coordinate system to use. Default is `coord_cartesian()`.
#'
#' @details
#' This coordinate system is particularly useful for visualizing seasonal or
#' cyclic patterns in time series data. It works by:
#'
#' \enumerate{
#'   \item Dividing the time axis into loops based on the specified loop period
#'   \item Folding the time values of every loop into the first loop's window
#'   \item Cutting geometries that cross a loop boundary into one piece per loop
#' }
#'
#' Since the looping is applied to the data rather than to the drawing, the
#' panel is drawn only once regardless of how many loops are shown. The cost of
#' the plot is therefore independent of the number of loops.
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
#' The justification of looping can be controlled using the `align_discrete` option
#' of [scale_x_mixtime()], where values from 0 to 1 specify the alignment.
#' Left alignment (`align_discrete = 0`) places inter-seasonal connections on the
#' left of the panel, right alignment (`align_discrete = 1`) uses the right side,
#' and center alignment (`align_discrete = 0.5`, the default) uses equal spacing
#' on both ends of the season.
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
#' @section Known limitations:
#'
#' Geometries are cut into loops by splitting the paths and rings that make them
#' up, which requires those shapes to be monotone along the time axis. This
#' works works for lines, paths, ribbons, areas, rects, tiles, bars, columns and
#' segments. A non-monotone concave polygon that crosses a loop boundary is not
#' cut correctly.
#'
#' @return A `Coord` ggproto object that can be added to a ggplot.
#'
#' @examples
#' library(ggplot2)
#' library(ggtime)
#' library(mixtime)
#'
#' # Basic usage with US accidental deaths data
#' uad <- tsibble::as_tsibble(USAccDeaths)
#' # Requires mixtime, POSIXct, or Date time types
#' uad$index <- yearmonth(uad$index)
#'
#' p <- ggplot(uad, aes(x = index, y = value)) +
#'   geom_line()
#'
#' # Original plot
#' p
#'
#' # With yearly looping to show seasonal patterns
#' p + coord_loop(time_loops = "1 year")
#'
#' @export
coord_loop <- function(
  loops = waiver(),
  time_loops = waiver(),
  time = "x",
  xlim = NULL,
  ylim = NULL,
  expand = FALSE,
  default = FALSE,
  clip = "on",
  coord = coord_cartesian()
) {
  specialize_coord_loop(ggplot2::ggproto(
    NULL,
    CoordLoop(coord),
    loops = loops,
    time_loops = time_loops,
    time = time,
    is_flipped = isTRUE(time == "y"),
    limits = list(x = xlim, y = ylim),
    expand = expand,
    default = default,
    clip = clip
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

    # Is the coord being wrapped linear? Used to decide whether munching needs
    # to do any real work, see `distance()` below.
    parent_is_linear = coord$is_linear(),

    # Set by `distance()` and consumed by `transform()` to detect connected data.
    munch_connected = FALSE,

    # When TRUE, cutting and layout are both skipped. Panel decoration
    # (gridlines, axis keys) is already expressed in the coordinates of the loop
    # window, so it must be passed straight through -- see `as_decoration()`.
    is_decoration = FALSE,

    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
      # Calculate the panel parameters as normal (without looping) so that
      # user-defined limits, scale limits, expand, etc are all taken into
      # account when working out where to cut.
      uncut_params <- ggproto_parent(coord, self)$setup_panel_params(
        scale_x,
        scale_y,
        params
      )

      time_cuts <- loop_cuts(
        uncut_params,
        self$time_scale,
        self$loops,
        self$time_loops
      )

      # Recalculate the panel parameters zoomed in on the first loop, so that
      # expansion, breaks and user limits are all applied to the window that is
      # actually drawn.
      old_limits <- self$limits
      self$limits[[self$time_scale]] <- c(
        # Restart at the first time point
        time_cuts[1],
        # End at the longest loop in the window
        time_cuts[1] + max(diff(time_cuts))
      )
      cut_params <- ggproto_parent(coord, self)$setup_panel_params(
        scale_x,
        scale_y,
        params
      )
      self$limits <- old_limits

      cut_params$time_cuts <- time_cuts
      # Cutting happens in transformed data space, which is what `transform()`
      # and `distance()` are handed.
      cut_params$loop_cuts <- as.numeric(
        cut_params[[self$time_scale]]$get_transformation()$transform(time_cuts)
      )
      cut_params
    },

    # Reporting as non-linear is what gives us the "is this geometry connected?"
    # signal: `coord_munch()` (called only by connected geoms) calls
    # `distance()` immediately before `transform()`, whereas pointwise geoms
    # reach `transform()` directly. It also gets us `Inf` resolved against the
    # backtransformed range for free, and makes rects and segments arrive as
    # rings and paths so that one cutting code path covers them all.
    is_linear = function() FALSE,

    distance = function(self, x, y, panel_params) {
      self$munch_connected <- TRUE

      if (self$parent_is_linear) {
        # Make munching a no-op: a distance of 0 gives `extra = 1`, and
        # interpolating a segment into one piece returns it unchanged.
        return(rep(0, max(length(x) - 1L, 0L)))
      }

      # The parent genuinely needs to munch (e.g. `coord_radial()`). Measure the
      # unfolded distance: a segment that spans several loops is cut into one
      # piece per loop, and each of those pieces needs its own vertices. Folding
      # first would report a segment spanning a whole loop as having travelled
      # nowhere, and it would be drawn as a straight chord instead of an arc.
      ggproto_parent(coord, self)$distance(x, y, panel_params)
    },

    transform = function(self, data, panel_params) {
      connected <- isTRUE(self$munch_connected)
      # Reset defensively on every call, so a `transform()` reached without a
      # preceding `distance()` is never mistaken for connected data.
      self$munch_connected <- FALSE

      if (isTRUE(self$is_decoration)) {
        return(ggproto_parent(coord, self)$transform(data, panel_params))
      }

      cut <- if (connected) cut_connected else cut_pointwise
      data <- cut(data, self$time, panel_params$loop_cuts)
      data <- ggproto_parent(coord, self)$transform(data, panel_params)
      self$arrange_loops(data, panel_params)
    },

    # Panel decoration is derived from the panel params, whose limits are the
    # loop window, so it already lives in the folded coordinate space. Cutting
    # it would fold breaks in the window's overhang (a break sitting exactly on
    # the next loop's start) back to the start of the panel.
    train_panel_guides = function(self, panel_params, layers, params = list()) {
      as_decoration(
        self,
        ggproto_parent(coord, self)$train_panel_guides(
          panel_params,
          layers,
          params
        )
      )
    },

    render_bg = function(self, panel_params, theme) {
      as_decoration(
        self,
        ggproto_parent(coord, self)$render_bg(panel_params, theme)
      )
    },

    render_fg = function(self, panel_params, theme) {
      as_decoration(
        self,
        ggproto_parent(coord, self)$render_fg(panel_params, theme)
      )
    },

    # Hook for laying the cut loops out within the panel. `coord_loop()`
    # superimposes them, so there is nothing to do beyond dropping the loop
    # index that `cut_*()` attached. `coord_calendar()` overrides this to stack
    # the loops into rows.
    arrange_loops = function(self, data, panel_params) {
      data$.loop <- NULL
      data
    }
  )
}

# specialization ----------------------------------------------------------

#' Specialize the implementation of coord_loop depending on the base coord
#'
#' [coord_loop()] wraps a base coord such as [coord_cartesian()] or
#' [coord_radial()]. This function is called by `CoordLoop()` to specialize an
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
#'
#' Note that the cutting and folding of data is handled generically by
#' `CoordLoop`, in terms of `coord$time` (the data aesthetic) and
#' `coord$time_scale` (the panel param). Specializations should not need to
#' override `transform()` or `draw_panel()`.
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
  cli::cli_abort(c(
    "{.fn coord_loop} does not support {.cls {cls}}.",
    i = "Supported coords are {.fn coord_cartesian} and {.fn coord_radial}."
  ))
}

#' Evaluate an expression that draws panel decoration
#'
#' Gridlines and axis keys are transformed through `Coord$transform()` just like
#' layer data is, but they describe the loop *window* rather than data within it,
#' and are already expressed in its coordinates. They must therefore be passed
#' through untouched: cutting them would fold a break sitting on the next loop's
#' start back to the start of the panel, and a layout that places each loop
#' separately (such as `coord_calendar()`'s rows) would squeeze them all into the
#' first row instead of replicating them across every row.
#'
#' ggplot2 only routes panel decoration through `render_bg()`, `render_fg()` and
#' `train_panel_guides()`, so wrapping those three covers it.
#' @param coord A `CoordLoop` ggproto object.
#' @param expr Expression to evaluate, lazily.
#' @noRd
as_decoration <- function(coord, expr) {
  old <- coord$is_decoration
  coord$is_decoration <- TRUE
  on.exit(coord$is_decoration <- old, add = TRUE)
  expr
}

#' @export
specialize_coord_loop.CoordCartesian <- function(coord, ...) {
  force(coord)

  if (!isTRUE(coord$time %in% c("x", "y"))) {
    cli::cli_abort(c(
      "{.fn coord_loop} requires {.arg time} to be {.val x} or {.val y}.",
      x = "{.arg time} is {.val {coord$time}}."
    ))
  }

  ggplot2::ggproto(
    "CoordLoopCartesian",
    coord,

    time_scale = coord$time
  )
}

#' @export
specialize_coord_loop.CoordRadial <- function(coord, ...) {
  force(coord)

  if (!isTRUE(coord$time == coord$theta)) {
    cli::cli_abort(c(
      "{.fn coord_loop} requires {.arg time} to be the angular axis of \\
       {.fn coord_radial}.",
      x = "{.arg time} is {.val {coord$time}}, but {.arg theta} is \\
           {.val {coord$theta}}."
    ))
  }

  ggplot2::ggproto(
    "CoordLoopRadial",
    coord,

    time_scale = "theta",
    limits = list(
      theta = coord$limits[[coord$theta]] %||% coord$super()$limits$theta,
      r = coord$limits[[coord$r]] %||% coord$super()$limits$r
    )
  )
}
