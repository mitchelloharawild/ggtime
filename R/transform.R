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
