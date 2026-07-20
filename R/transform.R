#' The mixtime scale transformation
#'
#' Maps `mixtime` vectors onto the continuous positions ggplot2 draws with, in
#' three steps:
#'
#' 1. collapse the mixed-granularity `mixtime` (a `vecvec` of potentially
#'    several temporal granularities) onto a common single-granularity
#'    `<mt_time>`;
#' 2. apply `transform` to those time points;
#' 3. hand back plain numbers.
#'
#' Step 2 deliberately sees an `<mt_time>` rather than a bare double, so that
#' time-aware transformations (e.g. [transform_warp()]) can work in time units,
#' and transformations which are meaningless for time (`log10()`, `sqrt()`)
#' error rather than quietly producing nonsense. Step 3 is equally deliberate:
#' ggplot2 compares transformed values against numeric limits (`censor()`,
#' `oob_squish()`), which errors on an `<mt_time>`. The time metadata therefore
#' exists only *between* those steps, and never reaches the scale.
#'
#' @param transform A transformation to apply to the time points, given as a
#'   `<transform>` object or a name accepted by [scales::as.transform()].
#'   `NULL` or `waiver()` applies no further transformation.
#' @param ptype The common time type to map onto. Defaults to `NULL`, taking it
#'   from the first `mixtime` transformed.
#'
#' @noRd
transform_mixtime <- function(transform = NULL, ptype = NULL) {
  force(ptype)

  if (is_waiver(transform)) {
    transform <- NULL
  }
  if (!is.null(transform)) {
    transform <- scales::as.transform(transform)
    if (identical(transform$name, "identity")) {
      transform <- NULL
    }
  }

  from_mixtime <- function(x) {
    if (is_mixtime(x)) {
      # Collapse to a singular time type, keeping the time attributes. The first
      # mixtime seen is the data, and so defines the common scale.
      x <- vecvec::unvecvec(x)
      if (is.null(ptype)) {
        ptype <<- vctrs::vec_ptype(x)
      }
    }

    # What is left may still not be on the common time scale: ggplot2 hands
    # user-supplied `breaks` straight to the transformation, rather than through
    # the scale's own `transform()` method.
    if (is.null(ptype)) {
      # No forward transformation yet, so there is nothing to convert onto.
      return(x)
    }
    if (!inherits(x, "mt_time")) {
      # Bare numbers are already positions in the common granularity, and need
      # only their class back.
      return(vctrs::vec_restore(vctrs::vec_data(x), ptype))
    }

    # An `<mt_time>` carries a `chronon` which may be coarser than the data's.
    # e.g. monthly breaks on a daily series - convert to the ptype granularity.
    target <- attr(ptype, "chronon")
    if (identical(attr(x, "chronon"), target)) {
      return(x)
    }
    # `chronon_convert()` returns a bare numeric, so the time attributes are
    # restored from `ptype` - which already carries the target chronon, and
    # keeps a duration a duration rather than making it a time point.
    vctrs::vec_restore(mixtime:::chronon_convert(x, target), ptype)
  }

  to_mixtime <- function(x) {
    if (inherits(x, "mt_time")) {
      # Already carries its granularity, no need to convert to common ptype.
      return(x)
    }
    if (is.null(ptype)) {
      # No forward transformation yet, so there is nothing to restore to.
      return(x)
    }
    # Restores the time attributes lost by a temporally unaware transformation.
    vctrs::vec_restore(vctrs::vec_data(x), ptype)
  }

  # Breaks are chosen in mixtime space, so `breaks_pretty()` returns bare
  # numbers. Restoring them to the limits' time type keeps the granularity
  # attached, so it survives the trip back through `transform` on its way to
  # becoming a position.
  time_breaks <- function(x, n = 5) {
    breaks <- scales::breaks_pretty()(x, n)
    if (inherits(x, "mt_time")) {
      breaks <- vctrs::vec_restore(breaks, x)
    }
    breaks
  }

  scales::new_transform(
    name = if (is.null(transform)) {
      "mixtime"
    } else {
      paste0("mixtime(", transform$name, ")")
    },
    transform = function(x) {
      x <- from_mixtime(x)
      if (!is.null(transform)) {
        x <- transform$transform(x)
      }
      vctrs::vec_data(x)
    },
    inverse = function(x) {
      # Forced on arrival so that everything downstream reads `ptype` after it
      # has been set.
      force(x)

      if (!is.null(transform)) {
        x <- transform$inverse(x)
      }
      to_mixtime(x)
    },
    breaks = time_breaks,
    # Much like `scales::transform_compose()`
    domain = transform$domain %||% c(-Inf, Inf)
  )
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
