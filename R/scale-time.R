#' Position scales for mixtime data
#'
#' These are the default scales for mixtime vectors, which are usually used for
#' a plot automatically. To override the scales behaviour manually, use
#' `scale_*_mixtime`.
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

    if (inherits(x, "mixtime")) {
      # Obtain chronons from mixtime vector
      x <- vctrs::vec_data(vecvec::unvecvec(x))
    }

    if (!is_waiver(self$warps)) {
      warps <- vctrs::vec_data(vecvec::unvecvec(self$warps))
      # TODO: warping should be have warp points before and after limits
      x <- approx(warps, seq_along(warps), xout = x, rule = 2)$y
    }

    as.numeric(x)
  },
  break_info = function(self, range = NULL) {
    breaks <- ggproto_parent(ScaleContinuous, self)$break_info(range)
    if (!(is_waiver(self$secondary.axis) || self$secondary.axis$empty())) {
      self$secondary.axis$init(self)
      breaks <- c(breaks, self$secondary.axis$break_info(breaks$range, self))
    }
    breaks
  },
  sec_name = function(self) {
    if (is_waiver(self$secondary.axis)) {
      waiver()
    } else {
      self$secondary.axis$name
    }
  },
  # make_sec_title = function(self, title) {
  #   browser()
  #   if (!is_waiver(self$secondary.axis)) {
  #     self$secondary.axis$make_title(title)
  #   } else {
  #     ggproto_parent(ScaleContinuous, self)$make_sec_title(title)
  #   }
  # }
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

    attributes(x) <- common_attr

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
