#' Axis guide for discrete time
#'
#' A discrete time point is a *granule* rather than an instant: `2020 Jan` names
#' the whole of January, not one moment within it. `guide_axis_mixtime()` draws
#' the axis accordingly, placing ticks where each granule starts and ends and
#' the label naming it between them. This gives discrete time the same
#' appearance that [ggplot2::guide_axis()] gives a discrete scale, where a label
#' names a band of the axis rather than a position along it.
#'
#' This is the default guide for [scale_x_mixtime()], and applies only when the
#' data occupies whole granules. Continuous time points sit at an instant and
#' have no granule to bracket, so the guide then behaves exactly like
#' [ggplot2::guide_axis()].
#'
#' Every granule in the panel has its boundaries ticked, whether or not it is
#' labelled, so labels which skip granules (such as every 6 months) still show
#' the width of what they name. Where this would be too many ticks to tell apart,
#' only granules of a coarser unit, or those with labels, are ticked.
#'
#' The default breaks are labelled by the coarsest granule which simplifies them,
#' so 10 years of monthly data has years labelled (and ticked) rather than months.
#'
#' Angular axes of [ggplot2::coord_radial()] are drawn as by
#' [ggplot2::guide_axis_theta()], with a tick at each break.
#'
#' @inheritParams ggplot2::guide_axis
#'
#' @returns A `<Guide>` object, suitable for the `guide` argument of
#'   [scale_x_mixtime()] or [ggplot2::guides()].
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(y = c(3, 5, 4, 6))
#' df$x <- mixtime::yearquarter(200:203)
#'
#' # Each quarter is bracketed by ticks, with its label in between
#' ggplot(df, aes(x, y)) +
#'   geom_col() +
#'   scale_x_mixtime(breaks = mixtime::yearquarter(200:203))
#'
#' # Ticks at the break points instead, as a non-temporal axis would draw them
#' ggplot(df, aes(x, y)) +
#'   geom_col() +
#'   scale_x_mixtime(
#'     breaks = mixtime::yearquarter(200:203),
#'     guide = guide_axis()
#'   )
#'
#' @export
guide_axis_mixtime <- function(
  title = waiver(),
  theme = NULL,
  check.overlap = FALSE,
  angle = waiver(),
  n.dodge = 1,
  minor.ticks = FALSE,
  cap = "none",
  order = 0,
  position = waiver()
) {
  new_guide(
    title = title,
    theme = theme,
    check.overlap = check.overlap,
    angle = angle,
    n.dodge = n.dodge,
    minor.ticks = minor.ticks,
    cap = cap,
    order = order,
    position = position,
    available_aes = c("x", "y", "r", "theta"),
    name = "axis",
    super = GuideAxisMixtime
  )
}

#' @keywords internal
GuideAxisMixtime <- ggproto(
  "GuideAxisMixtime",
  GuideAxis,

  extract_key = function(scale, aesthetic, minor.ticks = FALSE, ...) {
    key <- GuideAxis$extract_key(
      scale,
      aesthetic,
      minor.ticks = minor.ticks,
      ...
    )
    if (is.null(key) || nrow(key) == 0L || is_theta_axis(list(...))) {
      return(key)
    }

    # Only a discrete time model has granules to bracket. Anything else - a
    # continuous time scale, or a plain scale this guide has been handed
    # directly - is left to the usual axis.
    time_scale <- scale$scale %||% scale
    if (!isTRUE(time_scale$time_discrete)) {
      return(key)
    }

    key$.type <- key$.type %||% "major"
    major <- vctrs::vec_slice(key, key$.type == "major")
    if (nrow(major) == 0L) {
      return(key)
    }

    # A break falls within the granule that its label names, which starts and
    # ends at whole units of that granule. These are measured through the
    # transformation rather than by adding a fixed width, so the bracket follows
    # the granule as it is drawn - under `transform_warp()`, granules are not all
    # the same width.
    transform <- scale$get_transformation()
    x <- transform$inverse(major$.value)
    # All breaks, as the labels were found from them (see `$get_labels()`).
    unit <- label_unit(time_scale, scale$get_breaks(), x)
    granules <- granule_bounds(x, unit)
    start <- as.numeric(transform$transform(granules$start))
    end <- as.numeric(transform$transform(granules$end))

    # The panel is drawn only over `range`, so a boundary or label beyond it
    # would be drawn outside the panel rather than clipped to it.
    range <- scale$continuous_range %||% scale$get_limits()
    bounds <- granule_ticks(range, transform, unit, start, end)
    bounds <- bounds[bounds >= range[1] & bounds <= range[2]]

    # The label names the granule as a whole, so it belongs between the
    # boundaries rather than at the break which starts it.
    middle <- (start + end) / 2
    visible <- middle >= range[1] & middle <= range[2]
    major <- vctrs::vec_slice(major, visible)
    major$.value <- middle[visible]
    major[[aesthetic]] <- scale$map(major$.value)

    # No `.label`, so that binding fills it with the missing value of whatever
    # type the labels are.
    boundary <- vctrs::data_frame(
      .value = bounds,
      .type = "boundary"
    )
    boundary[[aesthetic]] <- scale$map(bounds)

    vctrs::vec_rbind(
      major,
      boundary,
      vctrs::vec_slice(key, key$.type != "major")
    )
  },

  # Angular axes are drawn around the panel by ggplot2's own guide, which would
  # otherwise be chosen by `coord_radial()` if this guide weren't the default.
  transform = function(self, params, coord, panel_params) {
    if (is_theta_axis(params)) {
      return(GuideAxisTheta$transform(params, coord, panel_params))
    }
    ggproto_parent(GuideAxis, self)$transform(params, coord, panel_params)
  },

  draw = function(
    self,
    theme,
    position = NULL,
    direction = NULL,
    params = self$params
  ) {
    if (is_theta_axis(params)) {
      return(GuideAxisTheta$draw(theme, position, direction, params))
    }
    ggproto_parent(GuideAxis, self)$draw(theme, position, direction, params)
  },

  build_ticks = function(key, elements, params, position = params$opposite) {
    if ("boundary" %in% key$.type) {
      # Granule boundaries stand in for the usual break ticks, which would fall
      # under the middle of a label rather than at the edge of what it names.
      key <- vctrs::vec_slice(key, key$.type != "major")
      key$.type[key$.type == "boundary"] <- "major"
    }
    GuideAxis$build_ticks(key, elements, params, position)
  }
)

is_theta_axis <- function(params) {
  isTRUE(params$position %in% c("theta", "theta.sec"))
}

#' The unit of time that the labels of breaks name
#'
#' Breaks from `time_breaks` step by its granule, but each names only the first
#' unit of the step.
#'
#' @param scale The `ScaleContinuousMixtime` that the breaks belong to.
#' @param breaks All of the scale's breaks, on the transformed scale.
#' @param x The breaks as a continuous `<mt_time>` of the common chronon.
#' @noRd
label_unit <- function(scale, breaks, x) {
  unit <- scale$label_granule(breaks)
  if (is.null(unit)) {
    return(attr(x, "chronon"))
  }
  unit@n <- 1L
  unit
}

#' The bounds of the granules that time points fall in
#'
#' @param x A continuous `<mt_time>`.
#' @param unit The granule to find the bounds of.
#' @returns A list of the `start` and `end` of each granule, like `x`.
#' @noRd
granule_bounds <- function(x, unit) {
  chronon <- attr(x, "chronon")
  position <- time_convert(x, unit)
  # As for the labels, a time is named by the granule it falls in. Rounding
  # absorbs error in the conversions.
  start <- floor(round(vctrs::vec_data(position), 6))
  list(
    start = time_convert(vctrs::vec_restore(start, position), chronon),
    end = time_convert(vctrs::vec_restore(start + 1, position), chronon)
  )
}

#' The most granules of an axis to tick, beyond which the ticks of a typical
#' plot are too close together to tell apart.
#' @noRd
axis_max_granules <- 60L

#' Positions of the granule boundaries along an axis
#'
#' Every boundary of `unit` is ticked unless there would be too many of them, in
#' which case those of the finest coarser granule which fits are used. Labels
#' must each lie in a ticked granule that starts with theirs, and so if there
#' isn't one then only the labelled granules are ticked.
#'
#' @param range The range of the axis, on the transformed scale.
#' @param transform The scale's transformation.
#' @param unit The granule that labels name.
#' @param start,end The bounds of the labelled granules, on the transformed
#'   scale.
#' @returns Sorted boundaries on the transformed scale, covering `range`.
#' @noRd
granule_ticks <- function(range, transform, unit, start, end) {
  labelled <- unique(sort(c(start, end)))
  limits <- transform$inverse(range)
  # The limits are outside of what some transformations can invert.
  if (!all(is.finite(vctrs::vec_data(limits)))) {
    return(labelled)
  }
  chronon <- attr(limits, "chronon")

  for (granule in chronon_nesting(unit)$granule) {
    position <- time_convert(limits, granule)
    from <- floor(min(vctrs::vec_data(position)))
    to <- ceiling(max(vctrs::vec_data(position)))
    if (to - from > axis_max_granules) {
      next
    }
    ticks <- as.numeric(seq(from, to))
    ticks <- time_convert(vctrs::vec_restore(ticks, position), chronon)
    ticks <- as.numeric(transform$transform(ticks))
    ticks <- ticks[is.finite(ticks)]
    starts <- vapply(
      start,
      function(at) any(abs(ticks - at) < 1e-6 * max(1, abs(at))),
      logical(1L)
    )
    if (all(starts[start >= range[1] & start <= range[2]])) {
      return(ticks)
    }
  }
  labelled
}
