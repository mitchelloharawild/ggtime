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
#' plotted in local time, which is the scale's default behaviour (see the `time_chronon`
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
#' and absolute time formats. Local time (the scale's default whenever all
#' time points share a timezone) shows time as experienced in that timezone, it
#' is the time on clocks in that timezone. Absolute time shows time as a
#' continuous timeline without timezone adjustments. Which of these is shown is
#' controlled by the scale's `time_chronon`: a chronon with `tz = NA` combines
#' time points by their local wall-clock reading (local time), while a chronon
#' with a fixed timezone (such as UTC, the default when a common chronon must
#' be identified across timezones) aligns them by the instant they occurred
#' (absolute time).
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
#' values, and `geom_time_line()` since the slope between unkown values is also
#' unknown, `geom_time_line()` will not draw lines connecting missing values of
#' either type. Since duplicated time values are not semantically valid in
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
#'
#' @section Duplicated time values:
#'
#' If there are duplicated time values within a group, `geom_time_line()` will
#' issue a warning. An error will be raised if these duplications are systematic
#' across the geometry, specifically if more than 50% of time points contain the
#' same number of duplicates. Systematic duplicates typically indicate a need to
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
# #' # Implicit missing values (WIP)
# #' df_missing <- df_ts[-c(4, 7, 8), ]
# #' ggplot(df_missing, aes(time, value)) +
# #'   geom_time_line()
# #'
# #' # Duplicate time values (WIP)
# #' df_duplicated <- rbind(df_ts, df_ts[c(5, 9), ])
# #' df_duplicated[12:13, "value"] <- df_duplicated[12:13, "value"] + 5
# #' ggplot(df_duplicated, aes(time, value)) +
# #'   geom_time_line()
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
  # Marks the layer for `ggplot_add.ggtime_time_line_layer()` below, which
  # defaults `[x/y]timeoffset`/`[x/y]timeid` once the layer is added to a
  # plot. Class is prepended, not replacing the layer's own classes, so
  # everything else about how a layer is added is untouched.
  class(l) <- c("ggtime_time_line_layer", class(l))
  l
}

# Is `x` a time-valued vector that carries an offset/identifier (a timezone or
# similar), and so is eligible for `[x/y]timeoffset`/`[x/y]timeid` defaults?
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
#' @param data The layer's computed data, with `[x/y]timetyped` columns (see
#'   `inject_time_aes()`).
#' @param params The layer's params, consulted for an explicit `orientation`
#'   override.
#' @returns `TRUE` to sort by `y` (only `y` is time-valued), `FALSE` to sort
#'   by `x` (the default).
#' @noRd
time_line_flipped_aes <- function(data, params) {
  if (!is.null(params$orientation) && !is.na(params$orientation)) {
    return(params$orientation == "y")
  }
  x_time <- isTRUE(data$xtimetyped[1])
  y_time <- isTRUE(data$ytimetyped[1])
  y_time && !x_time
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

#' The offset applied to a time point when it is drawn in local time
#'
#' A duration, not a bare number: the offset is derived here, from the original
#' data, but is subtracted much later from positions the scale has since placed
#' on a *common* chronon -- one which isn't known at this point, and need not be
#' the chronon of any of the inputs. A duration carries its own unit, so the
#' scale can convert it onto that chronon once it knows it (see
#' `transform_time_offset()`), rather than the two ends having to agree a unit
#' up front.
#'
#' [mixtime::tz_offset()] states the offset in the chronon of the data it was
#' asked about, and gives naive time (which has no timezone to be offset from)
#' an offset of zero, so nothing more is needed here.
#' @noRd
default_timeoffset <- function(x) {
  # 0, not NA: a non-time-typed `y` (the common case -- most plots don't have
  # a time-valued y) must default to a no-op offset, not `NA`, since it is
  # still subtracted even when nothing about it ever triggers a transition.
  if (!is_time_typed(x)) {
    return(mixtime::seconds(0L))
  }
  mixtime::tz_offset(x)
}

default_timeid <- function(x) {
  if (is_time_typed(x)) mixtime::tz_name(x) else NA_character_
}

#' Default `[x/y]timeoffset`/`[x/y]timeid`/`[x/y]timetyped` from the `x`/`y`
#' mapping
#'
#' Reuses whatever expression is mapped to `aesthetic` (`x` or `y`) to fill in
#' the offset/identifier/typed aesthetics, unless already mapped. This has to
#' run on the *mapping*, not on data computed later in the layer pipeline,
#' since scale transformation discards the original time-typed value (and, for
#' combined multi-timezone `mixtime` data, its per-point timezone) before any
#' `Geom`/`Stat`/`Position` method sees it.
#'
#' `[x/y]timetyped` in particular exists only so `GeomTimeLine$setup_data` can
#' tell which axis to connect observations in order of: `[x/y]timeid` can't be
#' reused for this, since it is `NA` both when an axis isn't time-valued at all
#' and when it is a naive `mixtime` with no timezone of its own (`tz_name()`
#' is `NA` either way), and only the former should mean "not the time axis".
#'
#' @param mapping The layer's own mapping, to fill in and return.
#' @param peek The complete mapping including inheritence from the base plot
#'   aesthetic `mapping`.
#' @param aesthetic The position aesthetic (`"x"` or `"y"`) to derive from.
#'
#' @noRd
inject_time_aes <- function(mapping, peek, aesthetic) {
  expr <- peek[[aesthetic]]
  if (is.null(expr) || !is_quosure(expr)) {
    return(mapping)
  }

  offset_aes <- paste0(aesthetic, "timeoffset")
  id_aes <- paste0(aesthetic, "timeid")
  typed_aes <- paste0(aesthetic, "timetyped")

  aes_env <- env(
    quo_get_env(expr),
    default_timeoffset = default_timeoffset,
    default_timeid = default_timeid,
    is_time_typed = is_time_typed
  )

  if (is.null(peek[[offset_aes]])) {
    mapping[[offset_aes]] <- new_quosure(
      expr(default_timeoffset(!!quo_get_expr(expr))),
      env = aes_env
    )
  }
  if (is.null(peek[[id_aes]])) {
    mapping[[id_aes]] <- new_quosure(
      expr(default_timeid(!!quo_get_expr(expr))),
      env = aes_env
    )
  }
  if (is.null(peek[[typed_aes]])) {
    mapping[[typed_aes]] <- new_quosure(
      expr(is_time_typed(!!quo_get_expr(expr))),
      env = aes_env
    )
  }
  mapping
}

# Inject the `[x/y]timeoffset`/`[x/y]timeid` default aesthetics in a way that
# accesses the semantically-rich original time-typed input data, and passes
# time duration offsets through `ScaleMixtime` to be converted onto the scale's
# chosen common chronon.
#' @exportS3Method ggplot2::ggplot_add ggtime_time_line_layer
ggplot_add.ggtime_time_line_layer <- function(object, plot, ...) {
  mapping <- object$mapping %||% aes()
  inherited <- if (isTRUE(object$inherit.aes)) plot$mapping else aes()
  peek <- c(mapping, inherited[setdiff(names(inherited), names(mapping))])

  mapping <- inject_time_aes(mapping, peek, "x")
  mapping <- inject_time_aes(mapping, peek, "y")
  object$mapping <- mapping

  NextMethod()
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
    # orders by `x`), not row order (like the `GeomPath` this otherwise is):
    # flipping onto the time axis, sorting, then flipping back reuses
    # `geom_line()`'s own trick to sort by whichever axis (`x` or `y`) needs
    # it, without duplicating its `draw_panel()`.
    data$flipped_aes <- params$flipped_aes
    data <- ggplot2::flip_data(data, params$flipped_aes)
    data <- data[order(data$PANEL, data$group, data$x), ]
    ggplot2::flip_data(data, params$flipped_aes)
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
    # TODO
    # * Add gaps for implicit missing values
    # * Uniqueness: add message for duplicate, warning for sawtoothing, error for systematic duplicates
    # * Multiple lines for duplicated time values

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

    if (!anyDuplicated(data$group)) {
      cli::cli_inform(c(
        "{.fn {class(self[1])}}: Each group consists of only one observation.",
        i = "Do you need to adjust the {.field group} aesthetic?"
      ))
    }

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

#' Insert rows at known time offset transitions
#'
#' Uses the `transitions` data frame to insert two rows at each transition
#' instant, one on either side, to draw a 'jump' segment between them.
#'
#' @param data One group's post-scale data, with `x`, `y`, `[x/y]timeoffset` and
#'   `[x/y]timeid` columns.
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
  if (is.null(scale) || is.null(data[[id_col]]) || all(is.na(data[[id_col]]))) {
    return(data)
  }
  if (!scale_places_local_time(scale)) {
    return(data)
  }

  # Everything below compares against `transitions$time`, which is an instant,
  # so the plotted positions have to be read back as instants too: `pos_col`
  # is a local wall clock reading, sitting `offset_col` away from the instant
  # itself. Undoing the placement by asking the scale to invert it -- rather
  # than subtracting the offset directly -- keeps this correct however the
  # positions were arrived at: the inverse also undoes any further `transform`
  # (e.g. [transform_warp()], under which positions aren't even linear in
  # time, and the offset would be meaningless in the warped space). The
  # offset is rebuilt into a duration on the chronon the inverted position
  # carries (the scale's own, which it already converted the offset onto --
  # see `transform_time_offset()`), so the subtraction is native `mt_linear`
  # - `mt_duration` arithmetic rather than raw subtraction on the chronon's
  # bare integer, and only the result is converted to seconds -- a conversion
  # between chronons, so it stays right for chronons of uneven length.
  local_time <- as_mt_concrete(scale$get_transformation()$inverse(data[[
    pos_col
  ]]))
  offset <- as_mt_concrete(mixtime::duration(
    data[[offset_col]] %||% 0,
    chronon = attr(local_time, "chronon"),
    discrete = FALSE
  ))
  data$time_instant <- seconds_since_epoch(local_time - offset, tz = NA)
  if (all(is.na(data$time_instant))) {
    data$time_instant <- NULL
    return(data)
  }

  # `waiver()`: lookup DST transitions with mixtime::tz_transitions()
  tzdb <- is_waiver(transitions)
  if (tzdb) {
    # Get unique timezones for transitions table
    ids <- unique(data[[id_col]][!is.na(data[[id_col]])])
    transitions <- do.call(
      rbind,
      lapply(ids, function(id) {
        rng <- range(data$time_instant[data[[id_col]] == id], na.rm = TRUE)
        tr <- mixtime::tz_transitions(
          as.POSIXct(rng[1], origin = "1970-01-01", tz = id),
          as.POSIXct(rng[2], origin = "1970-01-01", tz = id)
        )
        tr$id <- rep(id, nrow(tr))
        tr
      })
    )
  }

  if (is.null(transitions) || nrow(transitions) == 0) {
    data$time_instant <- NULL
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
  rng <- range(data$time_instant, na.rm = TRUE)
  transitions <- transitions[
    transitions$time_num > rng[1] & transitions$time_num < rng[2],
    ,
    drop = FALSE
  ]
  if (nrow(transitions) == 0) {
    data$time_instant <- NULL
    return(data)
  }
  transitions <- transitions[order(transitions$time_num), , drop = FALSE]

  # Constant (non-position) aesthetics for the inserted rows are copied from
  # an existing row in the group (e.g. colour, group, PANEL).
  template <- data[1, , drop = FALSE]

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
    # `before_time`/`after_time` above, and `time_instant` below records
    # where these rows sit. Writing `tr`'s seconds back into the column would
    # mix units, since by this point the column holds either the scale's
    # chronon or a duration.
    pair <- template[c(1, 1), , drop = FALSE]
    pair[[pos_col]] <- scale$transform(c(before_time, after_time))
    # Ties can occur here when the other axis has already inserted its own
    # straddling pair nearby: both of its rows share one `other_col` value
    # (interpolated at a single instant), so if that value also lands as this
    # axis's grid point, `approx()` sees a duplicate. Averaging it away is
    # harmless -- both duplicate rows agree on the value already.
    pair[[other_col]] <- suppressWarnings(
      stats::approx(
        data$time_instant,
        data[[other_col]],
        xout = tr$time_num
      )$y
    )
    pair$time_instant <- tr$time_num
    # Only the first (before) row is styled: segments are drawn from each
    # row to the next (see `draw_panel()`), so styling it is what makes the
    # segment spanning the jump -- from `before_time` to `after_time` --
    # dashed, without touching the segment that follows.
    for (aes in names(aesthetics)) {
      pair[[aes]][1] <- aesthetics[[aes]]
    }
    pair
  })

  # `order()` uses a stable sort for the short numeric `time_instant` here, so
  # ties (a pair's `before`/`after` rows share one `tr$time_num`) are left in
  # insertion order, keeping `before` ahead of `after`.
  out <- rbind(data, do.call(rbind, new_rows))
  out <- out[order(out$time_instant), , drop = FALSE]
  out$time_instant <- NULL
  out
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
seconds_since_epoch <- function(time, tz) {
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
