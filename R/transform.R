# Transformation between `mixtime` mixed-granularity vectors and the continuous
# single-granularity time points needed for plotting.
#
# The transformation collapses a `mixtime` (a `vecvec` of potentially several
# temporal granularities) into a singular granularity `mt_time` vector.
#
# It is deliberately *not* a bare double, so that downstream transforms are
# time-aware, so that transforms requiring time (e.g. warping) can use time
# units, and transforms which are meaningless for time (`log10()`, `sqrt()`)
# produce an error.
transform_mixtime <- function(ptype = NULL) {
  force(ptype)

  # To original granularity
  to_mixtime <- function(x) {
    if (is.null(ptype)) {
      # No forward transformation has been performed yet, so there is nothing to
      # restore to. This happens when `scales::transform_compose()` probes the
      # domain of a composed transform.
      return(x)
    }

    # Restore the `mt_time` structure (and thus the chronon) from the ptype.
    x <- vctrs::vec_restore(vctrs::vec_data(x), ptype)

    if (!is.null(cycle <- attr(ptype, "cycle"))) {
      # Offset timezone for labelling purposes
      x <- x - tz_offset(x, tz_name(cycle))
    }
    x
  }

  from_mixtime <- function(x) {
    if (!S7::S7_inherits(x, mixtime::class_mixtime)) {
      return(x)
    }
    if (is.null(ptype)) {
      # TODO - better method for obtaining the ptype?
      ptype <<- x@x[[1L]][0L]
    }

    if (!is.null(cycle <- attr(ptype, "cycle"))) {
      # Set cyclical time points to be relative to the start of the cycle, so
      # they are plotted cyclically irrespective of linear time point.
      x <- x - mixtime::time_floor(x, cycle)
    }

    # Collapse to a singular time type, keeping the time attributes.
    vecvec::unvecvec(x)
  }

  scales::new_transform(
    "mixtime",
    transform = "from_mixtime",
    inverse = "to_mixtime",
    breaks = scales::breaks_pretty()
  )
}

#' Compose a transformation with the mixtime transformation
#'
#' Time transformations must run after time points have been mapped onto a
#' common time scale, so `transform_mixtime()` is always applied first. This
#' wraps [scales::transform_compose()] to keep behaviour which it discards:
#'
#' * `format` is taken from `transform_mixtime()`, since breaks are chosen (and
#'   therefore labelled) in mixtime space.
#' * `breaks` likewise come from `transform_mixtime()`.
#'
#' @param transform A transformation to apply after `transform_mixtime()`,
#'   given as a `<transform>` object or a name accepted by
#'   [scales::as.transform()]. `NULL` or `waiver()` applies no further
#'   transformation.
#' @param ptype Passed through to `transform_mixtime()`.
#'
#' @noRd
compose_time_transform <- function(transform = NULL, ptype = NULL) {
  time_transform <- transform_mixtime(ptype)

  if (is.null(transform) || is_waiver(transform)) {
    return(time_transform)
  }

  transform <- scales::as.transform(transform)
  if (identical(transform$name, "identity")) {
    return(time_transform)
  }

  composed <- scales::transform_compose(time_transform, transform)

  # Breaks are chosen in mixtime space, and so must also be formatted there.
  composed$breaks <- time_transform$breaks
  composed$format <- time_transform$format
  composed
}

#' Warp a scale so that intervals between fixed points are equally spaced
#'
#' Warping gives each interval between successive warp points the same width,
#' however much of the scale it actually covers. Warp point *i* is placed at
#' position *i*, and values in between are placed by linear interpolation with
#' [stats::approx()]: a value one third of the way between two warp points is
#' drawn one third of the way between their positions. Every interval is
#' therefore exactly one unit wide, so wide intervals are compressed and narrow
#' ones stretched.
#'
#' Values outside the range of `warps` cannot be placed, and become `NA`. Because
#' panels are drawn with their range expanded beyond the data, `warps` should
#' extend past the data on both sides rather than merely cover it.
#'
#' `warps` should be of a type compatible with the data being warped: a time
#' vector to warp time, or a `numeric` vector to warp a numeric scale.
#'
#' @section Warping time series:
#'
#' Warping is particularly useful for time series, where the intervals of a
#' granularity are often unequal: calendar months span 28 to 31 days, so a daily
#' series drawn on a linear axis gives February less width than March. Warping at
#' month boundaries removes that unevenness, making months comparable at a glance
#' and putting each month's gridlines at a regular spacing.
#'
#' Warp points need not share the data's granularity: they are converted to the
#' data's chronon before being compared with it, so monthly warp points can place
#' daily observations.
#'
#' Warping does change the granularity of the scale to that of `warps`, with time
#' points becoming continuous positions *within* that chronon rather than whole
#' units of it. Breaks and labels follow suit, so a monthly warp labels its axis
#' in months: a day in mid January is month `612.5`, which mixtime prints as
#' `2021 Jan 50.0%`. Fractions track the real calendar, so `613.5` is the midpoint
#' of 28 day February and `614.5` the midpoint of 31 day March.
#'
#' @param warps A sorted vector of at least two points, giving the fixed points
#'   between which the scale is stretched or compressed. Should match the data
#'   being warped: a `mixtime` (or `Date`/`POSIXt`) vector for a time scale, or a
#'   `numeric` vector otherwise.
#'
#' @returns A `<transform>` object, suitable for the `transform` argument of
#'   [ggplot2::scale_x_continuous()] or [scale_x_mixtime()].
#'
#' @examples
#' library(ggplot2)
#'
#' # Warp points need not be evenly spaced. A straight line makes the effect
#' # obvious: it kinks at x = 50, where the intervals change from 25 wide to 50
#' # wide, halving the slope from there on.
#' ggplot(data.frame(x = 10:90, y = 10:90), aes(x, y)) +
#'   geom_line() +
#'   scale_x_continuous(transform = transform_warp(c(0, 25, 50, 100)))
#'
#' # Daily pedestrian counts for the first quarter of 2021: busy on weekdays,
#' # much quieter at the weekend, drifting upwards over the quarter.
#' pedestrians <- tibble::tibble(
#'   date = mixtime::date("2021-01-01") + 0:89,
#'   count = round(
#'     ifelse(seq_along(date) %% 7 %in% c(2, 3), 4500, 12000) +
#'       cumsum(rnorm(length(date), 15, 150)) +
#'       rnorm(length(date), 0, 700)
#'   )
#' )
#'
#' # Warp points extend a month either side of the data, because panels are drawn
#' # with their range expanded beyond it. They are monthly while the data is
#' # daily, which is fine: warp points are converted to the data's granularity.
#' month_starts <- mixtime::yearmonth("2020-12-01") + 0:5
#'
#' # Without warping, the weekly cycle is evenly spaced but the month gridlines
#' # are not: 28 day February is drawn narrower than its 31 day neighbours.
#' ggplot(pedestrians, aes(date, count)) +
#'   geom_line() +
#'   scale_x_mixtime(breaks = month_starts + 0)
#'
#' # Warping at the start of each month evens out the gridlines, but the length
#' # of each day is adjusted: the 28 days of February and 31 days of January and
#' # March are stretched or compressed to the same width over the month.
#' ggplot(pedestrians, aes(date, count)) +
#'   geom_line() +
#'   scale_x_mixtime(
#'     breaks = month_starts,
#'     # + 0 indicates the start of each month (continuous time model)
#'     transform = transform_warp(month_starts + 0)
#'   )
#'
#' @export
transform_warp <- function(warps) {
  # Time warp points may be given at a different granularity to the data, and
  # are converted once the data arrives. Anything else warps on its values as-is.
  warp_is_time <- is_mixtime(warps) || inherits(warps, "mt_time")

  warp_points <- if (is_mixtime(warps)) {
    # Time warp points set the granularity of the warped scale, so there must
    # be a single granularity to convert the data onto.
    if (length(warps@x) > 1L) {
      cli::cli_abort(
        c(
          "{.arg warps} must be a single granularity, not {length(warps@x)}.",
          i = "Warping places each interval between successive points at the \\
               same width, which has no meaning across granularities."
        )
      )
    }
    vecvec::unvecvec(warps)
  } else {
    warps
  }

  # Un-warping reports positions in the warp points' own type.
  warp_ptype <- vctrs::vec_ptype(warp_points)

  if (vctrs::vec_size(warp_points) < 2L) {
    cli::cli_abort(
      "{.arg warps} must contain at least 2 points, not \\
       {vctrs::vec_size(warp_points)}."
    )
  }
  warp_values <- vctrs::vec_data(warp_points)
  if (anyNA(warp_values)) {
    cli::cli_abort("{.arg warps} must not contain missing values.")
  }
  if (is.unsorted(warp_values, strictly = TRUE)) {
    cli::cli_abort(
      c(
        "{.arg warps} must be strictly increasing.",
        i = "Duplicated or unordered warp points make the transformation ambiguous."
      )
    )
  }
  warp_index <- seq_along(warp_values) - 1

  # Lets the inverse tell warp points too narrow for the data apart from warp
  # points which cover the data but not the expanded panel range.
  warps_cover_data <- TRUE

  warp_transform <- function(x) {
    # Non-time warp points are already in the data's units.
    warp_at <- warp_values

    if (warp_is_time) {
      # Reduce mixed granularity mixtime to a single granularity mt_time vector
      if (is_mixtime(x)) {
        x <- vecvec::unvecvec(x)
      }

      if (!any(is.finite(vctrs::vec_data(x)))) {
        # `scales` probes the domain with an unclassed `c(-Inf, Inf)`. Warping
        # is monotonic increasing, so each infinity is its own image.
        return(vctrs::vec_data(x))
      }

      # Only `<mt_time>` carries the granularity to convert warp points onto.
      if (!inherits(x, "mt_time")) {
        cli::cli_abort(
          c(
            "Can't warp {.obj_type_friendly {x}} against time {.arg warps}.",
            i = "Use {.fn scale_x_mixtime} so that time points are mapped \\
                 onto a common granularity before warping.",
            i = "Time warping must be applied before any transformation \\
                 which discards time information."
          )
        )
      }

      # So that (say) monthly warp points can position daily observations.
      warp_at <- mixtime:::chronon_convert(warp_points, attr(x, "chronon"))
    }

    values <- vctrs::vec_data(x)
    out <- interpolate_warp(values, warp_at, warp_index)
    if (anyNA(out[is.finite(values)])) {
      warps_cover_data <<- FALSE
    }
    out
  }

  warp_inverse <- function(x) {
    # Un-warping works in the warp points' units rather than the data's: the
    # inverse cannot see the data, and remembering the granularity from the last
    # forward call would make the result order-dependent.
    values <- interpolate_warp(vctrs::vec_data(x), warp_index, warp_values)

    if (anyNA(values[is.finite(vctrs::vec_data(x))])) {
      # Returning `NA` would strip the panel of all breaks and labels, so an
      # un-warp outside the warp points is worth stopping for.
      bounds <- format(warp_points[c(1L, length(warp_values))])
      cli::cli_abort(
        c(
          "Can't un-warp positions outside the range of {.arg warps} \\
           ({bounds[1]} to {bounds[2]}).",
          if (warps_cover_data) {
            c(
              i = "{.arg warps} covers the data, but plots are drawn with the \\
                   panel range expanded beyond it, so {.arg warps} must extend \\
                   past the data on both sides.",
              i = "Add warp points either side of the data, or remove the \\
                   expansion with {.code expand = expansion(0)}."
            )
          } else {
            c(
              i = "{.arg warps} does not cover the range of the data. Extend it \\
                   to span every value being plotted."
            )
          }
        )
      )
    }

    vctrs::vec_restore(values, warp_ptype)
  }

  scales::new_transform(
    "warp",
    transform = "warp_transform",
    inverse = "warp_inverse",
    breaks = scales::breaks_pretty(),
    # A warp is only defined between its outermost points. Declaring that lets
    # `scales::transform_compose()` clamp to it instead of probing with a value
    # the warp cannot position.
    #
    # A time warp cannot state its domain (it varies with data granularity).
    domain = if (warp_is_time) c(-Inf, Inf) else range(warp_values)
  )
}

# Linear interpolation between two monotonic sequences, used for both directions
# of `transform_warp()`. Non-finite values pass through untouched, since warping
# is monotonic increasing.
interpolate_warp <- function(x, from, to) {
  out <- vctrs::vec_data(x)
  finite <- is.finite(out)
  if (!any(finite)) {
    return(out)
  }

  # `rule = 1` yields `NA` outside the warp points, the intended out-of-range
  # behaviour. Not reported here: breaks are routinely evaluated just beyond the
  # data, where censoring is normal and silent. The inverse reports the case
  # that matters.
  out[finite] <- stats::approx(from, to, xout = out[finite])$y
  out
}
