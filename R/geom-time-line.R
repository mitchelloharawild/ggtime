#' Line geometry with temporal semantics
#'
#' @description
#' `geom_time_line()` connects observations in order of the time variable, similar to
#' [ggplot2::geom_line()], but with special handling for time zones, gaps and
#' duplicated values.
#'
#' The geometry helps to visualise time with changing time offsets provided by the
#' `[x/y]timeoffset` aesthetics. Changes in time offsets are drawn using dashed lines,
#' which are most commonly used for timezone changes and daylight savings time transitions.
#' Timezone offsets are automatically used when times from the `mixtime` package are
#' plotted in local time (see the `time_chronon`
#' argument of [scale_x_mixtime()]).
#'
#' This geometry also respects implicit missing values in regular time series, and will
#' not connect temporal observations separated by gaps.
#'
#' The [ggplot2::group] aesthetic determines which cases are connected together.
#'
#' @aesthetics GeomTimeLine
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_line
#' @param transitions A `data.frame` of known time offset transitions, shaped
#'  like [mixtime::tz_transitions()]'s own output (`time` a time point, and
#'  `offset_before`/`offset_after` [mixtime::duration()]s), with an optional
#'  `id` column to scope rows to a specific series (matched against the
#'  `xtimeid` aesthetic; applied to every series when omitted). Defaults to
#'  `waiver()`, which automatically calls [mixtime::tz_transitions()] for
#'  every timezone present in the data. See the "Time transitions" section
#'  below.
#' @param transition_aesthetics A named `list` of aesthetics for the segment
#'  drawn for time `transitions`, specifying how it is styled differently to the
#'  rest of the line. Defaults to `list(linetype = 2)`, drawing transitions
#'  with dashed lines. Valid aesthetics are `colour`/`color`, `linewidth`,
#'  `linetype` and `alpha`.
#' @param orientation Which positional axis (`"x"` or `"y"`) carries the time
#'  variable that observations are connected in order of. The default (`NA`)
#'  determines this automatically: whichever of `x`/`y` is time-valued (a
#'  `mixtime` or `POSIXct`), preferring `x` if both (or neither) are.
#'
#' @seealso
#'  [scale_mixtime] for defining local and absolute time using `time_chronon`.
#'
#'  [ggplot2::geom_line()]/[ggplot2::geom_path()] for standard line/path geoms in ggplot2.
#'
#' @section Practical usage:
#'
#' The `geom_time_line()` geometry extends [ggplot2::geom_line()] with time
#' semantics that ensure the line's slope accurately reflects rates of change in
#' the measurements over time.
#'
#'
#' Most notably, `geom_time_line()` works closely with the time scale
#' ([scale_x_mixtime()]) to correctly display time in local
#' and absolute time formats. Local time shows time as experienced in a
#' timezone, it is the time on clocks in that timezone. Absolute time shows time
#' as a continuous timeline without timezone adjustments. Which of these is shown
#' is controlled by the scale's `time_chronon`: a chronon with a timezone aligns
#' time points by the instant they occurred (absolute time), while a chronon with
#' `tz = NA` combines time points by their local wall-clock reading (local time).
#' The default is absolute time, labelled in the timezone of the data, or in UTC
#' if the data has multiple timezones.
#'
#'
#' When time series are visualised in local time, timezone offset changes (e.g.
#' due to daylight saving time) cause 'jumps' in time which are indicated with
#' dashed lines. This preserves the integrity of the line's slope across these
#' transitions. Another benefit of visualising time series in local time is to
#' compare time series across different timezones, as the time axis is better
#' aligned with human behaviour in their local timezone (e.g. working hours,
#' sleep patterns, etc). Plotting time series in *absolute time* shows the exact
#' contemporaneous timing of events across multiple timezones, which is useful
#' when resources or patterns are shared across timezones (e.g. international
#' markets, server load balancing, etc).
#'
#'
#' This geometry also maintains semantically valid slopes when time values are
#' missing (either implicitly or explicitly), or duplicated. Implicit missing
#' values in regular time series are semantically equivalent to explicit missing
#' values. Since the slope between unknown values is also unknown,
#' `geom_time_line()` will not draw lines connecting missing values of either
#' type. Since duplicated time values are not semantically valid in
#' regular time series, `geom_time_line()` will issue a warning (or an error if
#' systematic duplicates are detected). When drawing a line between duplicated
#' time points, the correct slopes are drawn by connecting all lines that lead
#' to and from the duplicated time points (rather than drawing sawtooth lines).
#'
#' Further details about each specific capability are described in the following
#' sections.
#'
#' @section Time transitions:
#'
#' When time is displayed locally, daylight savings transitions introduce
#' discontinuities in the local timeline when the clock jumps forwards or
#' backwards. `geom_time_line()` draws these jumps as dashed segments, to
#' preserve the integrity of the line's slope across the transition. When
#' the time scale is set to use local time (see the `time_chronon` argument
#' of [scale_x_mixtime()]), the default behaviour (`transitions = waiver()`)
#' sources daylight savings transitions automatically with
#' [mixtime::tz_transitions()].
#'
#' The appearance of transition segments is controlled with
#' `transition_aesthetics`, a named list of aesthetic overrides (`colour`,
#' `linewidth`, `linetype` and/or `alpha`). The default is a dashed line.
#'
#' Offset changes aren't always timezone related. A sensor may be periodically
#' synchronized to adjust for clock drift, or the `transitions` may reflect an
#' individual's personal travel through time zones. The local time should be
#' mapped to the `[x/y]` positional aesthetics, with the offset from absolute
#' time mapped to `[x/y]timeoffset` (a [mixtime::duration()]). The `transitions`
#' argument then specifies the instants at which the offset changes, and the
#' offset before and after each transition. This is specified as a `data.frame`
#' shaped like [mixtime::tz_transitions()], with an optional `id` column to
#' scope rows to a specific series (matched against the `[x/y]timeid`
#' aesthetic).
#'
#' @section Missing time values:
#'
#' Explicit missing values are where an `NA` value is included in the data, but
#' for regular time series it is also possible to identify implicit missing time
#' values. Unlike [ggplot2::geom_line()], `geom_time_line()` will also not connect
#' points separated by implicit missing values, creating gaps in the line (just
#' like when an explicit missing value is present in [ggplot2::geom_line()]).
#' The granularity of discrete `mixtime` time points determines what counts as
#' the next observation: monthly time points are expected every month, and
#' hourly time points every hour. Gaps are not detected for continuous time, or
#' for `POSIXct`, since these don't identify an interval between observations.
#'
#' If a group contains time points of several granularities (e.g. annual and
#' monthly), each granularity is drawn as a separate line with its own missing
#' and duplicated time values.
#'
#' @section Duplicated time values:
#'
#' If there are duplicated time values of the same granularity within a group,
#' `geom_time_line()` will issue a warning. An error will be raised if these
#' duplications are systematic across the geometry, specifically if more than
#' 50% of a granularity's time points contain the same number of duplicates. Systematic duplicates typically indicate a need to
#' use grouping aesthetics ([ggplot2::group], or [ggplot2::colour]) to
#' draw separate lines for each time series. Rather than plotting an erroneous
#' 'sawtooth' line which misrepresents the rate of change, the geometry will
#' draw all lines that connect to and from each of the duplicated time values.
#'
#' @examples
#'
#' library(ggplot2)
#'
#'
#' # Basic time line plot of a random walk (no timezone changes)
#' df_ts <- data.frame(
#'   time = as.POSIXct("2023-03-11", tz = "Australia/Melbourne") + 0:11 * 3600,
#'   value = cumsum(rnorm(12, 2))
#' )
#' ggplot(df_ts, aes(time, value)) +
#'   geom_time_line()
#'
#' # Random walk with a backward timezone change (DST ends)
#' df_tz_back <- data.frame(
#'   time = as.POSIXct("2023-04-02", tz = "Australia/Melbourne") + 0:11 * 3600,
#'   value = cumsum(rnorm(12, 2))
#' )
#' # Naive/local time (`tz = NA`) shows the DST transition as a dashed jump
#' ggplot(df_tz_back, aes(time, value)) +
#'   geom_time_line() +
#'   scale_x_mixtime(time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA))
#' # Absolute time aligns to a single fixed timezone, removing the jump
#' ggplot(df_tz_back, aes(time, value)) +
#'   geom_time_line() +
#'   scale_x_mixtime(time_chronon = mixtime::cal_gregorian$hour(1L, tz = "UTC"))
#'
#' # Random walk with a forward timezone change (DST starts)
#' df_tz_forward <- data.frame(
#'   time = as.POSIXct("2023-10-01", tz = "Australia/Melbourne") + 0:11 * 3600,
#'   value = cumsum(rnorm(12, 2))
#' )
#' ggplot(df_tz_forward, aes(time, value)) +
#'   geom_time_line() +
#'   scale_x_mixtime(time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA))
#' ggplot(df_tz_forward, aes(time, value)) +
#'   geom_time_line() +
#'   scale_x_mixtime(time_chronon = mixtime::cal_gregorian$hour(1L, tz = "UTC"))
#'
#' # Implicit missing values leave a gap in the line
#' df_monthly <- data.frame(
#'   month = mixtime::yearmonth(as.Date("2023-01-01")) + 0:9,
#'   value = cumsum(rnorm(10, 2))
#' )
#' ggplot(df_monthly[-c(4, 7), ], aes(month, value)) +
#'   geom_time_line()
#'
#' # Duplicate time values fan out rather than drawing a sawtooth line
#' df_duplicated <- rbind(df_monthly, df_monthly[c(5, 8), ])
#' dup_rows <- c(5, 8, 11, 12) # both entries of each duplicated month
#' df_duplicated[dup_rows, "value"] <- df_duplicated[dup_rows, "value"] +
#'   rnorm(4, sd = 5)
#' ggplot(df_duplicated, aes(month, value)) +
#'   geom_time_line()
#'
#' @export
geom_time_line <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE,
  transitions = waiver(),
  transition_aesthetics = list(linetype = 2),
  ...
) {
  l <- layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      transitions = check_transitions(transitions),
      transition_aesthetics = check_transition_aesthetics(
        transition_aesthetics
      ),
      orientation = orientation,
      ...
    )
  )
  time_line_layer(l)
}

# Is `x` a time-valued vector that can identify an instant?
is_time_typed <- function(x) is_mixtime(x) || inherits(x, "POSIXct")

#' Which axis to connect observations in order of
#'
#' Mirrors [ggplot2::has_flipped_aes()], but decides which of `x`/`y` to sort
#' by using time-typing rather than discreteness: `geom_line()`'s own
#' continuous-vs-discrete heuristic is meaningless here, since
#' `geom_time_line()` accepts time on either axis and the other axis is
#' typically continuous too (so both would look identical to it). `x` is
#' preferred when both, or neither, axis is time-valued -- the latter matching
#' `geom_line()`'s own default.
#'
#' @param data The layer's computed data, with a `[x/y]timegranule` column for
#'   each time-valued axis (see `time_line_layer()`).
#' @param params The layer's params, consulted for an explicit `orientation`
#'   override.
#' @returns `TRUE` to sort by `y` (only `y` is time-valued), `FALSE` to sort
#'   by `x` (the default).
#' @noRd
time_line_flipped_aes <- function(data, params) {
  if (!is.null(params$orientation) && !is.na(params$orientation)) {
    return(params$orientation == "y")
  }
  !is.null(data$ytimegranule) && is.null(data$xtimegranule)
}

#' Check a user-supplied `transitions` table
#'
#' Checked when the layer is built rather than when it is drawn, so the error
#' points at the `geom_time_line()` call that supplied the table.
#'
#' @param transitions The `transitions` argument.
#' @param call The calling environment, for error messages.
#' @returns `transitions` unchanged, or an error.
#' @noRd
check_transitions <- function(transitions, call = caller_env()) {
  if (is_waiver(transitions) || is.null(transitions)) {
    return(transitions)
  }
  if (!is.data.frame(transitions)) {
    cli::cli_abort(
      "{.arg transitions} must be a {.cls data.frame}, not \\
       {.obj_type_friendly {transitions}}.",
      call = call
    )
  }

  offsets <- c("offset_before", "offset_after")
  missing <- setdiff(c("time", offsets), names(transitions))
  if (length(missing) > 0) {
    cli::cli_abort(
      c(
        "{.arg transitions} must have {.field {missing}} column{?s}.",
        i = "It is shaped like {.fn mixtime::tz_transitions}'s own output."
      ),
      call = call
    )
  }

  if (!is_time_typed(transitions$time)) {
    cli::cli_abort(
      c(
        "{.code transitions$time} must be a time point, not \\
         {.obj_type_friendly {transitions$time}}.",
        i = "A transition happens at an instant, so it is given as a time
             rather than as a count of seconds.",
        i = "Use a {.cls mixtime} or {.cls POSIXct}, such as \\
             {.code as.POSIXct('2023-06-01 12:00', tz = 'UTC')}."
      ),
      call = call
    )
  }

  for (col in offsets) {
    x <- transitions[[col]]
    if (!is_mixtime(x) || !all(mixtime::time_is_duration(x))) {
      cli::cli_abort(
        c(
          "{.code transitions${col}} must be a duration, not \\
           {.obj_type_friendly {x}}.",
          i = "An offset is a length of time, so it carries the unit it is
               measured in, just like the {.field {'[x/y]timeoffset'}}
               aesthetics.",
          i = "Use a {.pkg mixtime} duration, such as \\
               {.code mixtime::seconds(45)}."
        ),
        call = call
      )
    }
  }

  transitions
}

#' Check a user-supplied `transition_aesthetics` list
#'
#' Checked when the layer is built rather than when it is drawn, so the error
#' points at the `geom_time_line()` call that supplied the list.
#'
#' @param transition_aesthetics The `transition_aesthetics` argument.
#' @param call The calling environment, for error messages.
#' @returns `transition_aesthetics` unchanged (with `color` spellings
#'   standardised to `colour`), or an error.
#' @noRd
check_transition_aesthetics <- function(
  transition_aesthetics,
  call = caller_env()
) {
  if (is.null(transition_aesthetics)) {
    return(list())
  }
  if (!is.list(transition_aesthetics)) {
    cli::cli_abort(
      "{.arg transition_aesthetics} must be a {.cls list}, not \\
       {.obj_type_friendly {transition_aesthetics}}.",
      call = call
    )
  }

  names(transition_aesthetics) <- sub(
    "color",
    "colour",
    names(transition_aesthetics),
    fixed = TRUE
  )

  # These are the only aesthetics `GeomTimeLine` draws that a transition
  # segment isn't already using for its own position or identity (`x`, `y`,
  # `group`, ...), so they are the only ones it makes sense to override here.
  allowed <- c("colour", "linewidth", "linetype", "alpha")
  unknown <- setdiff(names(transition_aesthetics), allowed)
  if (length(unknown) > 0) {
    cli::cli_abort(
      c(
        "{.arg transition_aesthetics} has unknown aesthetic{?s} \\
         {.field {unknown}}.",
        i = "Only {.field {allowed}} can be overridden for a transition."
      ),
      call = call
    )
  }
  if (any(lengths(transition_aesthetics) != 1)) {
    cli::cli_abort(
      "Each element of {.arg transition_aesthetics} must be a single value.",
      call = call
    )
  }

  transition_aesthetics
}

#' Identify the granules of a layer's time points
#'
#' Scales place all time points on a common chronon, so the granule which each
#' position came from is needed to restore it. The granules are stored by the
#' scale, and each row's granule is identified with an integer code in the
#' internal `[x/y]timegranule` column. This isn't an aesthetic, since a time
#' point's granule can't vary independently of the time itself.
#'
#' The column exists if the axis is time-valued, and its codes are `NA` if the
#' scale can't restore the time.
#'
#' These methods need the scales, which geoms, stats and positions either can't
#' access or may be replaced by the user. `Layer` isn't exported by ggplot2, so
#' the layer itself is extended.
#'
#' @param layer A layer.
#' @returns `layer`, with `[x/y]timegranule` and `time_cluster` columns in its
#'   data.
#' @noRd
time_line_layer <- function(layer) {
  # `ggproto()` finds its parent lazily, so it must not be reassigned.
  force(layer)
  stat <- layer$stat
  ggproto(
    NULL,
    layer,
    # Stats which summarise over time can't keep these, which isn't a problem
    # the user can act on, so ggplot2 shouldn't warn that they were dropped.
    stat = ggproto(
      NULL,
      stat,
      dropped_aes = c(
        stat$dropped_aes,
        GeomTimeLine$optional_aes,
        "xtimegranule",
        "ytimegranule"
      )
    ),
    compute_aesthetics = function(self, data, plot) {
      data <- ggproto_parent(layer, self)$compute_aesthetics(data, plot)
      for (axis in intersect(c("x", "y"), names(data))) {
        x <- data[[axis]]
        scale <- plot@scales$get_scales(axis)
        granule <- paste0(axis, "timegranule")
        if (
          inherits(scale, "ScaleContinuousMixtime") && !is_bare_numeric(x)
        ) {
          data[[granule]] <- scale$register_granules(x)
        } else if (is_time_typed(x) || inherits(x, "Date")) {
          data[[granule]] <- rep(NA_integer_, nrow(data))
        }
      }
      data
    },
    compute_position = function(self, data, layout) {
      axis <- if (isTRUE(data$flipped_aes[1])) "y" else "x"
      scales <- layout[[paste0("panel_scales_", axis)]]
      data <- time_clusters(data, axis, scales[[1]])
      ggproto_parent(layer, self)$compute_position(data, layout)
    }
  )
}

#' @keywords internal
GeomTimeLine <- ggproto(
  "GeomTimeLine",
  GeomPath,
  optional_aes = c(
    "xtimeoffset",
    "ytimeoffset",
    "xtimeid",
    "ytimeid"
  ),
  extra_params = c(GeomPath$extra_params, "orientation"),
  setup_params = function(data, params) {
    params$flipped_aes <- time_line_flipped_aes(data, params)
    params
  },
  setup_data = function(data, params) {
    # Connects observations in order of the time variable (like `geom_line()`
    # orders by `x`), not row order (like the `GeomPath` this otherwise is).
    data$flipped_aes <- params$flipped_aes
    time <- data[[if (params$flipped_aes) "y" else "x"]]
    data[order(data$PANEL, data$group, time), , drop = FALSE]
  },
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    arrow = NULL,
    arrow.fill = NULL,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    na.rm = FALSE,
    transitions = waiver(),
    transition_aesthetics = list(linetype = 2)
  ) {
    if (!anyDuplicated(data$group)) {
      cli::cli_inform(c(
        "{.fn geom_time_line}: Each group consists of only one observation.",
        i = "Do you need to adjust the {.field group} aesthetic?"
      ))
    }

    # Not found if the geom is used by another layer, e.g. `stat_summary()`
    if (!is.null(data$time_cluster)) {
      data <- time_paths(data)
    }

    # Insert transition/jumps at known time offset transitions for dashed
    # lines, styled with `transition_aesthetics` as they are inserted.
    data <- do.call(
      rbind,
      lapply(split(data, data$group), function(group_data) {
        group_data <- resolve_time_transitions(
          group_data,
          transitions = transitions,
          scale = panel_params$x$scale,
          axis = "x",
          aesthetics = transition_aesthetics
        )
        resolve_time_transitions(
          group_data,
          transitions = transitions,
          scale = panel_params$y$scale,
          axis = "y",
          aesthetics = transition_aesthetics
        )
      })
    )

    munched <- ggplot2::coord_munch(coord, data, panel_params)

    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) {
      return(ggplot2::zeroGrob())
    }

    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <- c(group_diff, TRUE)

    munched$fill <- arrow.fill %||% munched$colour

    # arrow <- ggplot2::repair_segment_arrow(arrow, munched$group)

    grid::segmentsGrob(
      munched$x[!end],
      munched$y[!end],
      munched$x[!start],
      munched$y[!start],
      default.units = "native",
      arrow = arrow,
      gp = gg_par(
        col = alpha(munched$colour, munched$alpha)[!end],
        fill = alpha(munched$fill, munched$alpha)[!end],
        lwd = munched$linewidth[!end],
        lty = munched$linetype[!end],
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      )
    )
  },
)

#' Sort data by time and identify clusters of duplicated time values
#'
#' Positions are restored to the granule that they came from, since they can
#' collide for mixed granularities (the middle of a month and of a day within
#' it) and for local time (either side of a backward transition).
#'
#' Duplicates and gaps are specific to each granularity within a group, so time
#' points of different granularities are never connected.
#'
#' @param data The layer's data.
#' @param axis The position aesthetic (`"x"` or `"y"`) to connect in order of.
#' @param scale The position scale of `axis`.
#' @returns `data` in time order, with a `time_cluster` id shared by duplicated
#'   time values. Ids are consecutive only if the clusters should be connected,
#'   so an id is skipped at each gap and between series.
#' @noRd
time_clusters <- function(data, axis, scale) {
  n <- nrow(data)
  if (n == 0) {
    return(data)
  }
  offset <- data[[paste0(axis, "timeoffset")]]
  granule <- data[[paste0(axis, "timegranule")]] %||% rep(NA_integer_, n)

  # Time in its own granule, or its position if that isn't known
  time <- as.numeric(data[[axis]])
  series <- rep(0L, n)
  regular <- rep(FALSE, n)
  for (code in unique(granule[!is.na(granule)])) {
    i <- which(granule == code)
    time[i] <- scale$restore_time(time[i], code, offset[i])
    series[i] <- scale$time_granules[[code]]$series
    regular[i] <- scale$time_granules[[code]]$regular
  }
  # Discrete time is whole granules, except where shifted by user offsets
  index <- ifelse(regular, round(time), time)

  ord <- order(data$PANEL, data$group, series, time)
  data <- data[ord, , drop = FALSE]
  i <- seq_len(n)[-1]
  now <- ord[i]
  prev <- ord[i - 1]

  new_series <- data$group[i] != data$group[i - 1] |
    data$PANEL[i] != data$PANEL[i - 1] |
    series[now] != series[prev]
  # `%in% TRUE` as missing times are neither duplicated nor separated by a gap
  same <- (index[now] == index[prev]) %in% TRUE & !new_series
  # Equal times at different offsets are not duplicates
  if (!is.null(offset)) {
    same <- same &
      vctrs::vec_equal(
        vctrs::vec_slice(offset, now),
        vctrs::vec_slice(offset, prev),
        na_equal = TRUE
      )
  }
  gap <- (regular[now] & regular[prev] & index[now] - index[prev] > 1) %in% TRUE
  data$time_cluster <- cumsum(
    c(1, ifelse(same, 0, 1 + (gap | new_series)))
  )

  sizes <- table(data$time_cluster)
  duplicates <- sizes[sizes > 1]
  if (length(duplicates) == 0) {
    return(data)
  }
  # Most time points of a granularity sharing the same number of duplicates
  # suggests a forgotten grouping variable, rather than incidental duplicates.
  cluster_series <- series[ord][!duplicated(data$time_cluster)]
  systematic <- vapply(split(as.integer(sizes), cluster_series), function(n) {
    any(n > 1) && max(table(n[n > 1])) / length(n) > 0.5
  }, logical(1))
  if (any(systematic)) {
    cli::cli_abort(c(
      "{.fn geom_time_line}: Time values are systematically duplicated.",
      i = "Do you need to use a grouping aesthetic ({.field group} or \\
           {.field colour}) to draw a separate line for each time series?"
    ))
  }
  cli::cli_warn(c(
    "{.fn geom_time_line}: Duplicated time values were found within a group.",
    i = "Drawing all lines that connect to and from the duplicated values."
  ))
  data
}

#' Split groups into the paths they should be drawn as
#'
#' Paths are broken at gaps, and around duplicated time values. All combinations
#' of rows are connected to and from duplicates, each as their own two-row path.
#'
#' @param data One panel's data, from `time_clusters()`.
#' @returns `data` with its rows arranged into paths identified by `group`.
#' @noRd
time_paths <- function(data) {
  rows <- split(seq_len(nrow(data)), data$time_cluster)
  unique <- lengths(rows) == 1
  i <- seq_along(rows)[-1]
  linked <- diff(as.numeric(names(rows))) == 1
  continues <- linked & unique[i] & unique[i - 1]

  path <- cumsum(c(TRUE, !continues))
  fans <- lapply(i[linked & !continues], function(j) {
    t(expand.grid(rows[[j - 1]], rows[[j]]))
  })
  fans <- as.integer(unlist(fans))

  out <- data[c(unlist(rows[unique]), fans), , drop = FALSE]
  out$group <- c(path[unique], max(path) + rep(seq_len(length(fans) / 2), each = 2))
  out
}

#' Insert rows at known time offset transitions
#'
#' Uses the `transitions` data frame to insert two rows at each transition
#' instant, one on either side, to draw a 'jump' segment between them.
#'
#' @param data One group's post-scale data, with `x` and `y`, and optionally
#'   `[x/y]timeoffset`, `[x/y]timeid` and `[x/y]timegranule` columns.
#' @param transitions Either `waiver()` (the default), which automatically
#'   looks up transitions with [mixtime::tz_transitions()], or a data frame of
#'   equivalent shape (with an optional `id` column that is matched against
#'   `[axis]timeid`).
#' @param scale The position scale in use for `axis`.
#' @param axis The position aesthetic (`"x"` or `"y"`) that is time-aware to
#'   resolve transitions.
#' @param aesthetics A named `list` of aesthetic overrides (see
#'   `geom_time_line()`'s own argument of the same name), applied directly to
#'   the first (dashed-segment-start) row of each inserted pair.
#' @returns `data`, with two rows inserted and styled with
#'   `aesthetics` per in-range transition.
#' @noRd
resolve_time_transitions <- function(
  data,
  transitions,
  scale,
  axis,
  aesthetics
) {
  pos_col <- axis
  other_col <- if (axis == "x") "y" else "x"
  id_col <- paste0(axis, "timeid")
  offset_col <- paste0(axis, "timeoffset")

  # Return early if the scale is not time-aware, or the data has no timezone
  if (is.null(scale) || !scale_places_local_time(scale)) {
    return(data)
  }
  # The timezone of each time point's granule is its default identifier
  if (is.null(data[[id_col]])) {
    granule <- data[[paste0(axis, "timegranule")]]
    if (is.null(granule)) {
      return(data)
    }
    tzs <- vapply(scale$time_granules, function(g) g$tz, character(1))
    data[[id_col]] <- tzs[granule]
  }
  tzdb <- is_waiver(transitions)
  # Without offsets (dropped by a stat), local time can't identify instants
  if (tzdb && (all(is.na(data[[id_col]])) || is.null(data[[offset_col]]))) {
    return(data)
  }

  # Everything below compares against `transitions$time`, which is an instant,
  # so the local positions have to be read back as instants too.
  instant <- rep(NA_real_, nrow(data))
  known <- !is.na(data[[pos_col]])
  # Rounded so that an observation at a transition is exactly at its instant,
  # despite the precision lost by coarse chronons.
  instant[known] <- round(
    seconds_since_epoch(
      scale$restore_instant(data[[pos_col]][known], data[[offset_col]][known]),
      tz = "UTC"
    ),
    4
  )
  if (all(is.na(instant))) {
    return(data)
  }

  # `waiver()`: lookup DST transitions with mixtime::tz_transitions()
  if (tzdb) {
    # Get unique timezones for transitions table
    ids <- unique(data[[id_col]][!is.na(data[[id_col]])])
    transitions <- do.call(
      rbind,
      lapply(ids, function(id) {
        rng <- range(instant[data[[id_col]] %in% id], na.rm = TRUE)
        tr <- mixtime::tz_transitions(
          as.POSIXct(rng[1], origin = "1970-01-01", tz = id),
          # One second later, as `tz_transitions()` excludes its `end`
          as.POSIXct(rng[2] + 1, origin = "1970-01-01", tz = id)
        )
        tr$id <- rep(id, nrow(tr))
        tr
      })
    )
  }

  if (is.null(transitions) || nrow(transitions) == 0) {
    return(data)
  }
  # Collapsed to their concrete `<mt_time>` subtype:
  # * `<mt_linear>` for time positions
  # * `<mt_duration>` for the offsets
  #
  # This allows use of the mixtime's `<mt_linear> + <mt_duration>` arithmetic
  transitions$time <- as_mt_concrete(transitions$time)
  transitions$offset_before <- as_mt_concrete(transitions$offset_before)
  transitions$offset_after <- as_mt_concrete(transitions$offset_after)
  transitions$time_num <- seconds_since_epoch(transitions$time, tz = "UTC")

  # A no-op when `tzdb`: those rows were already built from exactly the ids in
  # `data[[id_col]]` above, so only a user-supplied table needs filtering here.
  if (!tzdb && !is.null(transitions$id)) {
    transitions <- transitions[
      transitions$id %in% unique(data[[id_col]]),
      ,
      drop = FALSE
    ]
  }
  # An observation at the instant of a transition is already at the new offset,
  # so a transition at the last observation is needed to reach it, but one at
  # the first observation isn't.
  rng <- range(instant, na.rm = TRUE)
  transitions <- transitions[
    transitions$time_num > rng[1] & transitions$time_num <= rng[2],
    ,
    drop = FALSE
  ]
  if (nrow(transitions) == 0) {
    return(data)
  }
  transitions <- transitions[order(transitions$time_num), , drop = FALSE]

  # Constant (non-position) aesthetics for the inserted rows are copied from
  # an existing row in the group (e.g. colour, group, PANEL).
  template <- data[1, , drop = FALSE]
  known <- !is.na(instant)

  new_rows <- lapply(seq_len(nrow(transitions)), function(i) {
    tr <- transitions[i, ]

    if (tzdb) {
      second <- mixtime::mt_duration(1L, chronon = attr(tr$time, "chronon"))
      tz_ptype <- mixtime::mt_linear(
        integer(),
        chronon = mixtime::cal_time_civil$second(1L, tz = tr$id)
      )
      before_time <- vctrs::vec_cast(tr$time - second, tz_ptype)
      after_time <- vctrs::vec_cast(tr$time + second, tz_ptype)
    } else {
      # Only this branch consults the offsets: with no timezone to look the
      # local reading up from, it is the instant shifted by the offset itself.
      before_time <- tr$time + tr$offset_before
      after_time <- tr$time + tr$offset_after
    }

    # `[axis]timeoffset` is deliberately left as the template's: the offsets
    # either side of the transition are already spent, in
    # `before_time`/`after_time` above. Writing `tr`'s seconds back into the column would
    # mix units, since by this point the column holds either the scale's
    # chronon or a duration.
    pair <- template[c(1, 1), , drop = FALSE]
    pair[[pos_col]] <- scale$transform(c(before_time, after_time))
    # Ties can occur here when the other axis has already inserted its own
    # straddling pair nearby: both of its rows share one `other_col` value
    # (interpolated at a single instant), so if that value also lands as this
    # axis's grid point, `approx()` sees a duplicate. Averaging it away is
    # harmless -- both duplicate rows agree on the value already.
    # Missing values are kept, as nothing is drawn to or from them.
    pair[[other_col]] <- suppressWarnings(
      stats::approx(
        instant[known],
        data[[other_col]][known],
        xout = tr$time_num,
        na.rm = FALSE
      )$y
    )
    # Only the first (before) row is styled: segments are drawn from each
    # row to the next (see `draw_panel()`), so styling it is what makes the
    # segment spanning the jump -- from `before_time` to `after_time` --
    # dashed, without touching the segment that follows.
    for (aes in names(aesthetics)) {
      pair[[aes]][1] <- aesthetics[[aes]]
    }
    pair
  })

  # `order()` is stable, so ties in the instants are left in insertion order.
  # This keeps a pair's `before` row ahead of its `after` row, and both ahead of
  # an observation at the instant of the transition (which is already at the new
  # offset).
  out <- rbind(do.call(rbind, new_rows), data)
  out[order(c(rep(transitions$time_num, each = 2), instant)), , drop = FALSE]
}

#' Does `scale` place time points by local (wall-clock) reading?
#'
#' A dashed jump is only ever shown for local-time scales
#'
#' @param scale The position scale in use for this panel.
#' @returns `TRUE` if `scale` is a mixtime scale whose common chronon is
#'   naive, `FALSE` otherwise.
#' @noRd
scale_places_local_time <- function(scale) {
  chronon <- scale$time_chronon
  !is.null(chronon) && !is_waiver(chronon) && is.na(mixtime::tz_name(chronon))
}

# Epoch seconds of a time point, read in `tz` (`NA` for the naive/wall clock
# reading). Note this is a conversion between chronons rather than arithmetic
# on the underlying numbers, so it holds for chronons of uneven length.
# Discrete time is read at its start, since it can't be directly converted to a
# finer chronon.
seconds_since_epoch <- function(time, tz) {
  if (is.integer(vctrs::vec_data(time))) {
    time <- mixtime::mt_linear(
      as.numeric(vctrs::vec_data(time)),
      chronon = attr(time, "chronon")
    )
  }
  as.numeric(mixtime::mixtime(
    time,
    chronon = mixtime::cal_time_civil$second(1L, tz = tz),
    discrete = FALSE
  ))
}

# Collapses `x` (a bare `POSIXct`, or a `mixtime` that may still be the
# general mixed-granularity wrapper) down to its singular `<mt_time>` vector.
as_mt_concrete <- function(x) {
  if (S7::S7_inherits(x, mixtime::mt_time)) {
    return(x)
  }
  if (!is_mixtime(x)) {
    x <- mixtime::mixtime(x)
  }
  vecvec::unvecvec(x)
}
