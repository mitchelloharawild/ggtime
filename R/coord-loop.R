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
#' loops, such as `mixtime::weeks(2L)` or `mixtime::years(10L)`. If both
#' `loops` and `time_loops` are specified, `time_loops` wins.
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
#'
#' Looping arranges time cyclically, so the time axis describes a position
#' within the loop rather than the passage of time. The axis is labelled to
#' match: monthly data looped over years is labelled with months of the year
#' ("Jan", "Feb", ...), and daily data looped over weeks with days of the week
#' ("Mon", "Tue", ...). Which labels are appropriate depends on both the chronon
#' of the data and the loop's cycle, and is determined by the calendar being
#' used. Labels given with the `labels` or `time_labels` options of
#' [scale_x_mixtime()] are used unchanged, since they say how the user wants
#' time written.
#'
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
#' then be looped over weekly periods with
#' `coord_loop(time_loops = mixtime::weeks(1L))`.
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
#' uad$index <- mixtime::yearmonth(uad$index)
#'
#' p <- ggplot(uad, aes(x = index, y = value)) +
#'   geom_line()
#'
#' # Original plot
#' p
#'
#' # With yearly looping to show seasonal patterns
#' p + coord_loop(time_loops = mixtime::years(1L))
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
  time_loops <- duration_as_granule(time_loops)

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

#' @noRd
CoordLoop <- function(coord) {
  force(coord)
  ggplot2::ggproto(
    "CoordLoop",
    coord,

    # Name of the scale holding time within `panel_params`; set by
    # `specialize_coord_loop()`.
    time_scale = NULL,

    # Is the wrapped coord linear? Decides whether munching does real work
    # in `distance()`.
    parent_is_linear = coord$is_linear(),

    # Set by `distance()`, read by `transform()` to detect connected data.
    munch_connected = FALSE,

    # When TRUE, skips cutting and layout: panel decoration is already in
    # the loop window's coordinates and must pass through untouched (see
    # `as_decoration()`).
    is_decoration = FALSE,

    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
      # Un-looped params first, so limits/expand are computed from the true
      # range before working out where to cut.
      uncut_params <- ggproto_parent(coord, self)$setup_panel_params(
        scale_x,
        scale_y,
        params
      )
      trans <- uncut_params[[self$time_scale]]$get_transformation()
      cuts <- self$compute_cuts(uncut_params, trans, scale_x, scale_y)

      # Labels a looped axis as a position within the loop, not a point in
      # time.
      scales <- self$cyclical_scales(scale_x, scale_y, cuts)

      # Zoomed into the loop window, so expansion, breaks and user limits
      # apply to what is actually drawn.
      old_limits <- self$limits
      on.exit(self$limits <- old_limits, add = TRUE)
      self$limits[[self$time_scale]] <- self$window(cuts)
      cut_params <- ggproto_parent(coord, self)$setup_panel_params(
        scales$x,
        scales$y,
        params
      )

      self$panel_cuts(cut_params, cuts, trans)
    },

    # Hook: cutpoints for cutting and folding the time axis, from the
    # un-looped params and the time scale's transformation. Returns a
    # vector of loop points here; `loop_granule()`, `window()` and
    # `panel_cuts()` below all read from it, and it's the only thing
    # carried from here to those hooks (a subclass needing richer
    # structure, like `CoordCalendar`, overrides all four together).
    #
    # `scale_x`/`scale_y` are unused here, but needed by `CoordCalendar`'s
    # override to resolve a bare granule expression against the time
    # scale's own calendar.
    compute_cuts = function(self, uncut_params, trans, scale_x, scale_y) {
      loop_cuts(uncut_params, self$time_scale, self$loops, self$time_loops)
    },

    # Hook: the granule the time axis cycles over, for `cyclical_scales()`
    # below. Loops over `self$time_loops` directly, ignoring `cuts`.
    loop_granule = function(self, cuts) self$time_loops,

    # Hook: the granule the time axis is divided into, stepping breaks and
    # naming labels unless the scale has breaks of its own. `NULL` here,
    # since a loop has no divisions of its own to break at.
    break_granule = function(self, cuts) NULL,

    # The time scale, re-labelled to describe a position within the loop
    # rather than a point in time.
    cyclical_scales = function(self, scale_x, scale_y, cuts) {
      cycle <- self$loop_granule(cuts)
      divide <- self$break_granule(cuts)
      if (identical(self$time, "x")) {
        list(x = as_cyclical_scale(scale_x, cycle, divide), y = scale_y)
      } else {
        list(x = scale_x, y = as_cyclical_scale(scale_y, cycle, divide))
      }
    },

    # Hook: the window to zoom `self$limits[[self$time_scale]]` into (see
    # `setup_panel_params()`): the first time point to the end of the
    # longest loop.
    window = function(self, cuts) {
      c(cuts[1], cuts[1] + max(diff(cuts)))
    },

    # Hook: attaches the cut-specific fields to the panel params.
    panel_cuts = function(self, cut_params, cuts, trans) {
      cut_params$time_cuts <- cuts
      # Transformed, since cutting happens in transformed data space.
      cut_params$loop_cuts <- as.numeric(trans$transform(cuts))
      cut_params
    },

    # Reporting non-linear routes connected geoms through `distance()`
    # before `transform()` (the "is this connected?" signal), resolves
    # `Inf` against the backtransformed range, and turns rects/segments
    # into rings/paths so one cutting path covers everything.
    is_linear = function() FALSE,

    distance = function(self, x, y, panel_params) {
      self$munch_connected <- TRUE

      if (self$parent_is_linear) {
        # A distance of 0 gives `extra = 1`: munching becomes a no-op.
        return(rep(0, max(length(x) - 1L, 0L)))
      }

      # Measures the unfolded distance, so a segment spanning several loops
      # still gets vertices per piece rather than reading as travelling
      # nowhere.
      ggproto_parent(coord, self)$distance(x, y, panel_params)
    },

    transform = function(self, data, panel_params) {
      connected <- isTRUE(self$munch_connected)
      # Reset so a call without a preceding `distance()` isn't mistaken for
      # connected data.
      self$munch_connected <- FALSE

      if (isTRUE(self$is_decoration)) {
        return(ggproto_parent(coord, self)$transform(data, panel_params))
      }

      data <- self$cut_data(data, panel_params, connected)
      data <- ggproto_parent(coord, self)$transform(data, panel_params)
      self$arrange_loops(data, panel_params)
    },

    # Hook: cuts and folds `data`'s time aesthetic by this panel's cuts,
    # ready for the plain coord's `transform()` to rescale into `[0, 1]`.
    # `connected` (munched line/path-like data) is cut without introducing
    # duplicate vertices at existing breaks.
    cut_data = function(self, data, panel_params, connected) {
      cut <- if (connected) cut_connected else cut_pointwise
      cut(data, self$time, panel_params$loop_cuts)
    },

    # Already lives in folded coordinate space (the panel params' limits
    # are the loop window), so passed through as decoration rather than
    # cut.
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

    # Hook for laying cut loops out within the panel. Superimposed here, so
    # just drops the loop index `cut_*()` attached.
    arrange_loops = function(self, data, panel_params) {
      data$.loop <- NULL
      data
    }
  )
}

# cyclical labels ---------------------------------------------------------

#' Label a time scale by position within the loop, broken at its divisions
#'
#' A looped axis measures a position within the cycle rather than the
#' passage of time, so it's labelled that way ("Jan" rather than "1973
#' Jan"), using mixtime's own formatting for the chronon and cycle. A coord
#' that divides the axis into granules of its own (`coord_calendar()`'s
#' `cells`) also gets a break at every division, named at that division's
#' own granularity.
#'
#' Both are defaults: `breaks`/`time_breaks` and `labels`/`time_labels` are
#' left unchanged where the scale already sets them.
#' @param scale The positional `Scale` handling the time aesthetic.
#' @param time_loops The loop duration given to [coord_loop()], as reduced by
#'   `duration_as_granule()`.
#' @param divisions The granule the coord divides the axis into, from
#'   `CoordLoop$break_granule()`, or `NULL` if it has no divisions of its own.
#' @returns `scale` broken and labelled by the loop, or unchanged where it has
#'   nothing to add.
#' @noRd
as_cyclical_scale <- function(scale, time_loops, divisions = NULL) {
  cycle <- loop_cycle(time_loops)

  # A scale stepping its own breaks by a granule takes precedence over the
  # coord's divisions.
  granule <- scale$time_breaks
  if (is_waiver(granule)) {
    granule <- NULL
  }
  # Only worth defaulting on a cyclical axis; without a cycle the labels
  # would be full dates, unreadable at every division.
  divide <- !is.null(divisions) &&
    !is.null(cycle) &&
    is.null(granule) &&
    is_waiver(scale$breaks)
  if (divide) {
    granule <- divisions
  }

  # Nothing to say about either where the breaks fall or how they are named.
  if (!divide && (is.null(cycle) || !is_waiver(scale$labels))) {
    return(scale)
  }

  fields <- list()
  if (divide) {
    fields$breaks <- breaks_time_granule(divisions)
  }
  if (is_waiver(scale$labels)) {
    fields$labels <- function(x) {
      time_labels_at(x, chronon = granule, cycle = cycle)
    }
  }

  # A child, not a mutation: the view scale formats labels at draw time,
  # long after `setup_panel_params()` returns.
  inject(ggplot2::ggproto(NULL, scale, !!!fields))
}

#' Breaks at every boundary of a granule
#'
#' Steps the axis by a granule using the coord's own cutting
#' (`loop_cuts_by_duration()`), so breaks land exactly where the axis is
#' divided. Also handles plain `Date`/`POSIXct` axes, unlike
#' `breaks_time_seq()`.
#' @param granule The granule to break at, as reduced by
#'   `duration_as_granule()`.
#' @returns A function of the scale's limits, returning breaks in the scale's
#'   own time type.
#' @noRd
breaks_time_granule <- function(granule) {
  force(granule)
  function(limits) {
    # Dropped rather than raised: the coord already checked this cuts
    # against the axis before handing it over.
    tryCatch(
      loop_cuts_by_duration(limits, granule),
      error = function(cnd) NULL
    )
  }
}

#' The cycle that a looped time axis repeats over
#' @inheritParams as_cyclical_scale
#' @returns A time granule describing the cycle, or `NULL` if the axis has no
#'   cycle of a known length (e.g. `loops` given as time points, whose
#'   spacing is a number of chronons rather than a granule).
#' @noRd
loop_cycle <- function(time_loops) {
  if (S7::S7_inherits(time_loops, mixtime::mt_unit)) time_loops else NULL
}

# specialization ----------------------------------------------------------

#' Specialize the implementation of coord_loop depending on the base coord
#'
#' Called by `CoordLoop()` to specialize an instance for its wrapped base
#' coord (e.g. [coord_cartesian()], [coord_radial()]), overriding whatever
#' methods that coord needs.
#' @param coord A [`ggproto`] object of class `CoordLoop`, inheriting from
#'   the base coord passed to `CoordLoop(coord = ...)`.
#' @param ... unused.
#' @details
#' A specialization must implement:
#'
#' - `coord$time_scale`: the name of the `panel_params` element holding the
#'   `Scale` that handles time (e.g. `"x"`, `"y"`).
#'
#' A specialization may need to implement:
#'
#' - `coord$limits`: if the positional scales aren't `x`/`y`, map `xlim`/
#'   `ylim` onto the corresponding scales.
#'
#' Cutting and folding is handled generically by `CoordLoop`, so
#' specializations shouldn't need to override `transform()` or
#' `draw_panel()`.
#' @returns A [`ggproto`] object that inherits from `coord`. Errors if no
#'   parent class of `coord` is supported by [coord_loop()].
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
#' Gridlines and axis keys describe the loop window, not data within it, and
#' are already in its coordinates, so they must pass through `transform()`
#' untouched rather than being cut and laid out like data. Wraps
#' `render_bg()`, `render_fg()` and `train_panel_guides()`, the only places
#' ggplot2 routes panel decoration through.
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
