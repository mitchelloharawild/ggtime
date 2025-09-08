#' Position scales for mixtime data
#'
#' These are the default scales for mixtime vectors, responsible for mapping
#' time points to aesthetics along with identifying break points and labels for
#' the axes and guides. To override the scales behaviour manually, use
#' `scale_*_mixtime`. The primary purpose of these scales is to scale time
#' points across multiple granularities onto a common time scale. This is
#' achieved by identifying and coercing all time points to the finest chronon
#' that all time points can be represented in. This common time chronon is
#' automatically identified, but can be manually specified using the
#' `common_time`.
#'
#' @inheritParams ggplot2::scale_x_date
#' @param time_breaks A duration giving the distance between breaks like
#' "2 weeks", or "10 years". If both `breaks` and `time_breaks` are specified,
#' `time_breaks` wins.
#' @param time_minor_breaks A duration giving the distance between minor breaks like
#' "2 weeks", or "10 years". If both `minor_breaks` and `time_minor_breaks` are
#' specified, `time_minor_breaks` wins.
#' @param common_time Acts like a vctrs `ptype` defining the common chronon to
#' use for mixed granularity. The default automatically selects it.
#' @param time_align A number between 0 and 1 defining how to align coarser
#' granularities onto the common time scale. 0 means start alignment,
#' 1 means end alignment, and 0.5 means center alignment (the default).
#' @param time_labels Uses strftime strings or similar to format the labels from
#' the time points.
#' @param warps Warp the time scale to have a consistent length, one of:
#'   - `NULL` or `waiver()` for no warping (the default)
#'   - A `mixtime` vector giving positions of warping points
#'   - A function that takes the limits as input and returns warping points as
#'     output
#' @param time_warps A duration giving the distance between temporal warping
#' like "2 weeks", or "10 years". If both `warps` and `time_warps` are
#' specified, `time_warps` wins.
#'
#' @section Practical usage:
#'
#' When using `mixtime` vectors to represent time variables in ggplot2, these
#' scales are automatically applied. In most cases, the default behaviour will
#' be sufficient for scaling time points into plot aesthetics. These scales can
#' be used to manually adjust the scaling behaviour, such as adjusting the
#' breaks and labels or using a different common time scale.
#'
#' Similarly to the temporal scales in ggplot2 ([ggplot2::scale_x_date()] and
#' [ggplot2::scale_x_datetime()]), these scales can adjust the breaks and labels
#' using duration-based intervals and strftime-like formatting. These time aware
#' options are prefixed with `time_` (e.g. `time_breaks` and `time_labels`),
#' and take precedence over the non-time aware options (e.g. `breaks` and
#' `labels`). The scale's breaks can be specified with `mixtime::duration``
#' objects (e.g. `time_breaks = mixtime::months(1L)``), or with strings that can
#' be parsed into durations (e.g. `time_breaks = "1 month"`). Labels for time
#' points in Gregorian calendars can be specified using [base::strftime()]
#' formats (e.g. `time_labels = "%b %Y"`` for "Jan 2020"). Concise strings for
#' non-Gregorian calendars are not yet supported, but can be created using
#' custom label functions (e.g. `labels = function(x) { ... }`).
#'
#' A core feature of these scales is the ability to handle time from multiple
#' timezones, granularities, and calendars. This is achieved by mapping all time
#' points to a common time scale, which is automatically identifying the finest
#' compatible chronon that can represent the input data. This allows time points
#' across different granularities (e.g. [base::POSIXt], [base::Date], and
#' [mixtime::yearmonth]) to be plotted together on a common time scale. In this
#' case the finest chronon is 1 second (from [base::POSIXt]), so all time points
#' are mapped to a 1 second chronon for plotting. Mapping day and month chronons
#' to seconds introduces indeterminancy - which second should be used to
#' represent a day or month? This is resolved using the `time_align` argument,
#' which defaults to center alignment. This means that a day is mapped to noon,
#' and a month is mapped to the middle of the month.
#'
#' Another time specific feature of these scales is temporal warping. This
#' adjusts the mapping of time points to plot aesthetics to have a consistent
#' length between specified time points. This is most useful in comparing the
#' shapes of cycles with irregular durations, including:
#' * astronomical cycles (e.g. the rising and setting of the sun)
#' * economic cycles (e.g. growth and recession phases of economies)
#' * predator-prey cycles (e.g. population dynamics of interacting species)
#' * biological cycles (e.g. hormonal cycles)
#' * calendar cycles (e.g. days in each month)
#'
#' Further details about time specific scale options are described in the
#' following sections.
#'
#' @section Granularity alignment:
#'
#' Visualising mixed granularity time data introduces indeterminacy in the
#' mapping of less precise time points onto a common time scale. For example,
#' plotting monthly and daily data together raises the question of where to
#' place the monthly points relative to the daily points. By default, mixtime
#' uses center alignment, mapping the monthly points to the middle of the
#' month. This is controlled using the `time_align` argument, which accepts a
#' value between 0 (start alignment) and 1 (end alignment) and defaults to 0.5.
#'
#' The common time scale that defines how all granularities are mapped is
#' automatically identified based on the input data. This is achieved by finding
#' the finest chronon that all time points can be represented in. For example,
#' if the data contains both monthly and daily time points, the common time scale
#' will be daily, with the monthly points aligned according to the `time_align`
#' argument. If multiple time zones are present, the common time zone will
#' default to UTC. The common time scale can be manually specified using the
#' `common_time` argument, which accepts a `mixtime::time_unit`.
#'
#' @section Temporal warping:
#'
#' Time scales can be warped to have a consistent length between specified time
#' points. This is useful when visually exploring cyclical patterns where each
#' cycle has varying length. By warping the time scale, the shape of each cycle
#' can be more easily compared. Temporal warping is controlled using the `warps`
#' argument, which accepts a `mixtime` vector defining the positions of the
#' warping points. Calendar-based warping points can be conveniently specified
#' using the `time_warps` argument, which accepts a duration like "1 month".
#'
#' @examples
#'
#' ## The common timezone for mixed timezone data is UTC.
#' #df_tz_mixed <- data.frame(
#' #  time = mixtime::mixtime(
#' #    as.POSIXct("2023-10-01", tz = "Australia/Melbourne") + 0:23 * 3600,
#' #    as.POSIXct("2023-10-01", tz = "America/New_York") + 0:23 * 3600
#' #  ),
#' #  value = c(cumsum(rnorm(12, 2)), cumsum(rnorm(12, -2)))
#' #)
#' #ggplot(df_tz_mixed, aes(time, value)) +
#' #  geom_time_line()
#
#' ## Alternative breaks and labels apply to the common time scale.
#' #ggplot(df_tz_mixed, aes(time, value)) +
#' #  geom_time_line() +
#' #  scale_x_mixtime(time_breaks = "6 hours", time_labels = "%H:%M")
#
#' ## Plotting monthly and daily data together will align months to the middle day of the month.
#' #df_chronon_mixed <- data.frame(
#' #  time = c(
#' #    mixtime::yearmonth("2023-01") + 0:11,
#' #    as.Date("2023-01-01") + 0:364
#' #  ),
#' #  value = c(cumsum(rnorm(12, 30)), cumsum(rnorm(365, 1)))
#' #)
#
#' ## Warping the x-axis
#' #ggplot(df_chronon_mixed, aes(time, value)) +
#' #  geom_time_line(time_warps = "1 month")
#'
#'
#' @export
#' @rdname scale_mixtime
scale_x_mixtime <- function(
  name = waiver(),
  breaks = waiver(),
  time_breaks = waiver(),
  minor_breaks = waiver(),
  time_minor_breaks = waiver(),
  labels = waiver(),
  time_labels = waiver(),
  common_time = waiver(),
  time_align = 0.5,
  warps = waiver(),
  time_warps = waiver(),
  limits = NULL,
  expand = waiver(),
  oob = scales::censor,
  guide = waiver(),
  position = "bottom",
  sec.axis = waiver()
) {
  sc <- mixtime_scale(
    aesthetics = ggplot_global$x_aes,
    transform = "mixtime",
    name = name,
    palette = identity,
    breaks = breaks,
    time_breaks = time_breaks,
    minor_breaks = minor_breaks,
    time_minor_breaks = time_minor_breaks,
    labels = labels,
    time_labels = time_labels,
    warps = warps,
    time_warps = time_warps,
    timezone = NULL,
    guide = guide,
    limits = limits,
    expand = expand,
    oob = oob,
    position = position
  )
  debug(sc$get_labels)

  set_sec_axis(sec.axis, sc)
}


# scale_y_time <- function(name = waiver(),
#                          breaks = waiver(),
#                          cal_breaks = waiver(),
#                          labels = waiver(),
#                          cal_labels = waiver(),
#                          minor_breaks = waiver(),
#                          cal_minor_breaks = waiver(),
#                          timezone = NULL,
#                          limits = NULL,
#                          expand = waiver(),
#                          oob = scales::censor,
#                          guide = waiver(),
#                          position = "left",
#                          sec.axis = waiver()) {
#
#   sc <- datetime_scale(
#     c("y", "ymin", "ymax", "yend", "yintercept",
#       "ymin_final", "ymax_final", "lower", "middle", "upper", "y0"),
#     "time",
#     name = name,
#     palette = identity,
#     breaks = breaks,
#     cal_breaks = cal_breaks,
#     labels = labels,
#     cal_labels = cal_labels,
#     minor_breaks = minor_breaks,
#     cal_minor_breaks = cal_minor_breaks,
#     timezone = timezone,
#     guide = guide,
#     limits = limits,
#     expand = expand,
#     oob = oob,
#     position = position
#   )
#
#   set_sec_axis(sec.axis, sc)
# }

#' @importFrom scales breaks_width
#' @keywords internal
mixtime_scale <- function(
  aesthetics,
  transform,
  trans = deprecated(),
  palette,
  breaks = scales::breaks_pretty(),
  time_breaks = waiver(),
  minor_breaks = waiver(),
  time_minor_breaks = waiver(),
  labels = waiver(),
  time_labels = waiver(),
  warps = waiver(),
  time_warps = waiver(),
  timezone = NULL,
  guide = "legend",
  call = caller_call(),
  ...
) {
  call <- call %||% current_call()

  # Backward compatibility
  if (is.character(breaks)) {
    breaks <- breaks_width(breaks)
  }
  if (is.character(minor_breaks)) {
    minor_breaks <- breaks_width(minor_breaks)
  }

  if (!is_waiver(time_breaks)) {
    # TODO: Validate input as <duration>
    breaks <- breaks_width(time_breaks)
  }
  if (!is_waiver(time_minor_breaks)) {
    # TODO: Validate input as <duration>
    minor_breaks <- breaks_width(time_minor_breaks)
  }
  if (!is_waiver(time_labels)) {
    # TODO: Validate input as <duration>
    labels <- function(self, x) {
      scales::label_date(time_labels, timezone)(x)
    }
  }
  if (!is_waiver(time_warps)) {
    warps <- breaks_width(time_warps)
  }

  # x/y position aesthetics should use ScaleContinuousMixtime; others use ScaleContinuous
  if (all(aesthetics %in% c(ggplot_global$x_aes, ggplot_global$y_aes))) {
    scale_class <- ScaleContinuousMixtime
  } else {
    scale_class <- ScaleContinuous
  }

  sc <- ggplot2::continuous_scale(
    aesthetics,
    palette = palette,
    breaks = breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    guide = guide,
    transform = transform_mixtime(),
    trans = trans,
    call = call,
    ...,
    super = ggproto(NULL, scale_class, warps = warps)
  )

  # Range is hard-coded and not inherited by `super` in
  # `ggplot2::continuous_scale`, replace it.
  sc$range <- MixtimeRange$new()
  sc
}

#' @export
scale_type.mixtime <- function(x) c("mixtime", "continuous")

#' @keywords internal
ScaleContinuousMixtime <- ggproto(
  "ScaleContinuousMixtime",
  ScaleContinuous,
  secondary.axis = waiver(),
  timezone = NULL,
  range = MixtimeRange$new(),
  clone = function(self) {
    new <- ggproto(NULL, self)
    new$range <- MixtimeRange$new()
    new
  },
  # Redefine ScaleContinuous$train for (possible) self$range scoping issue
  train = function(self, x) {
    if (length(x) == 0) {
      return()
    }
    self$range$train(x, call = self$call)
  },
  transform_df = function(self, df) {
    # HACK
    # Add offsets for PositionTime[Civil/Absolute] here as "after_stat" since
    # Position$default_aes = aes(xoffset = stage(after_stat = f(x))) isn't
    # currently working in ggplot2
    missing_aes <- setdiff(names(PositionTimeCivil$default_aes), names(df))

    # Add gap filling for implicit missing values
    # DESIGN: should this be in position? Position may be too late to have access to enough data.
    # df <- as_tibble(tsibble::fill_gaps(as_tsibble(
    #   df,
    #   index = x,
    #   key = c(PANEL, group)
    # )))

    df <- ggproto_parent(ScaleContinuous, self)$transform_df(df)

    # Match missing_aes offset positions to transformed scales
    missing_aes_i <- match(missing_aes, paste0(names(df), "offset"))
    missing_aes_i <- missing_aes_i[!is.na(missing_aes_i)]

    df[missing_aes[missing_aes_i]] <- lapply(df[missing_aes_i], tz_offset)

    df
  },
  transform = function(self, x) {
    # Store common time type for default backtransformation, labels, and more.
    if (length(attr(x, "v")) != 1L) {
      cli::cli_abort(
        "{.field mixtime} scales currently work with single-type vectors only."
      )
    }
    # TODO: make this optionally user-specified
    self$ptype <- attributes(attr(x, "v")[[1L]])

    # Possibly redefine self$trans with new info from `x`

    if (is_bare_numeric(x)) {
      cli::cli_abort(
        c(
          "A {.cls numeric} value was passed to a {.field mixtime} scale.",
          i = "Please use the mixtime package to create time values."
        ),
        call = self$call
      )
    }

    ggproto_parent(ScaleContinuous, self)$transform(x)
  },
  map = function(self, x, limits = self$get_limits()) {
    # TODO: Check functionality of self$oob
    # self$oob(x, limits)

    as.numeric(self$warp_time(x))
  },
  warp_time = function(self, x, unwarp = FALSE) {
    if (!is_waiver(self$warps)) {
      warps <- vctrs::vec_data(vecvec::unvecvec(self$warps))
      if (inherits(x, "mixtime")) {
        # Obtain chronons from mixtime vector
        x <- vctrs::vec_data(vecvec::unvecvec(x))

        # Unsafe code:
        # Only apply warping if mixtime is provided, otherwise it is a pre-warped position
        x <- stats::approx(warps, seq_along(warps), xout = x)$y
      } else if (unwarp) {
        x <- stats::approx(seq_along(warps), warps, xout = x, rule = 2L)$y
      }
    }
    x
  },
  get_breaks = function(self, limits = self$get_limits()) {
    # if (!is_waiver(self$warps)) {
    #   warps <- vctrs::vec_data(vecvec::unvecvec(self$warps))
    #   limits <- stats::approx(
    #     seq_along(warps),
    #     warps,
    #     xout = limits,
    #     rule = 2L
    #   )$y
    #   attributes(limits) <- attributes(attr(self$warps, "v")[[1L]])
    # }

    breaks <- ggproto_parent(ScaleContinuous, self)$get_breaks(limits)
    self$warp_time(breaks)
  },

  get_labels = function(self, breaks = self$get_breaks()) {
    # Convert back to original time points for labelling
    breaks <- self$warp_time(breaks, unwarp = TRUE)

    ggproto_parent(ScaleContinuous, self)$get_labels(breaks)
  }
)

# Inversion requires recollection of offset and regularity
# Warping between specific time points numeric 0-n for n warp points, decimals indicate time between warp points
transform_mixtime <- function() {
  # TODO: replace common_attr with ptype provided by scale
  common_attr <- NULL

  # To original granularity
  to_mixtime <- function(x) {
    if (inherits(x, "mixtime")) {
      return(x)
    }

    # Less than perfect ('hack')
    #
    # The default coord limits are
    # `transformation$transform(transformation$inverse(c(NA_real_, NA_real_)))`,
    # which then also requires `Scale$transform$transform` to return atomic
    # numeric values :(
    #
    # A non-mixtime object is required here in order to have `ifelse()` work
    # correctly in `ggplot2:::expand_limits_continuous_trans()`
    #
    # This is also required for other aspects of ggplot2, such as for `pretty()`
    # to work in computing the default breaks.
    attributes(x) <- common_attr
    return(x)
  }
  # To common granularity (possibly with alignment?)
  # If local time is set, then an offset argument should be passed into the geom?

  # For aligning, find the range of times with a duration-based floor/ceiling.
  from_mixtime <- function(x) {
    if (!inherits(x, "mixtime")) {
      return(x)
    }
    if (length(attr(x, "v")) != 1L) {
      cli::cli_abort(
        "{.fun transform_mixtime} currently works with single granularity vectors only."
      )
    }
    common_attr <<- attributes(attr(x, "v")[[1L]])
    return(x)

    # structure(as.numeric(vecvec::unvecvec(x)), names = names(x), x = x)
  }
  scales::new_transform(
    "mixtime",
    transform = "from_mixtime",
    inverse = "to_mixtime",
    breaks = scales::breaks_pretty() #,
    #domain = to_mixtime(c(-Inf, Inf))
  )
}


gmt_offset <- function(x) {
  tryCatch(
    as.POSIXlt(x)$gmtoff,
    error = function(e) rep(0, length(x))
  )
}
tz_offset <- function(x) {
  if (!inherits(x, "mixtime")) {
    cli::cli_warn(
      "Missing timezone offset could not be calculated in the scale."
    )
  }
  attr(x, "v") <- lapply(attr(x, "v"), gmt_offset)
  as.numeric(vecvec::unvecvec(x))
}
