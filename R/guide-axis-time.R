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
#' Granule boundaries are shared: a granule ends where the next one begins, so
#' contiguous breaks produce a single row of ticks with one extra tick closing
#' the final granule. Breaks which skip granules cannot share boundaries, and
#' are instead bracketed by a tick on either side of each labelled granule.
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
    granules <- granule_bounds(transform$inverse(major$.value), time_scale)
    start <- as.numeric(transform$transform(granules$start))
    end <- as.numeric(transform$transform(granules$end))

    # Contiguous granules share boundaries, so a full row of granules needs only
    # one extra tick to close the last of them.
    bounds <- unique(sort(c(start, end)))

    # The panel is drawn only over `range`, so a boundary or label beyond it
    # would be drawn outside the panel rather than clipped to it.
    range <- scale$continuous_range %||% scale$get_limits()
    bounds <- bounds[bounds >= range[1] & bounds <= range[2]]

    # The label names the granule as a whole, so it belongs between the
    # boundaries rather than at the break which starts it.
    middle <- (start + end) / 2
    visible <- middle >= range[1] & middle <= range[2]
    major <- vctrs::vec_slice(major, visible)
    major$.value <- middle[visible]
    major[[aesthetic]] <- scale$map(major$.value)

    boundary <- vctrs::data_frame(
      .value = bounds,
      .label = NA_character_,
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

#' The bounds of the granules that breaks fall in
#'
#' Breaks from `time_breaks` are labelled at (and positioned within) a unit of
#' its granule, which may be coarser than the scale's common chronon. All other
#' breaks are labelled at the common chronon.
#'
#' @param x Break positions, as a continuous `<mt_time>` of the common chronon.
#' @param scale The `ScaleContinuousMixtime` that the breaks belong to.
#' @returns A list of the `start` and `end` of each granule, like `x`.
#' @noRd
granule_bounds <- function(x, scale) {
  chronon <- attr(x, "chronon")
  unit <- scale$time_breaks
  if (is_waiver(unit) || is.null(unit)) {
    start <- floor(vctrs::vec_data(x))
    return(list(
      start = vctrs::vec_restore(start, x),
      end = vctrs::vec_restore(start + 1, x)
    ))
  }

  # Breaks step by `unit`, but each names only the first unit of the step.
  unit@n <- 1L
  convert <- function(x, chronon) {
    vecvec::unvecvec(
      mixtime::mixtime(wrap_mixtime(x), chronon = chronon, discrete = FALSE)
    )
  }
  position <- convert(x, unit)
  # As for the labels, a break names the granule it falls in. Rounding absorbs
  # error in the conversions.
  start <- floor(round(vctrs::vec_data(position), 6))
  list(
    start = convert(vctrs::vec_restore(start, position), chronon),
    end = convert(vctrs::vec_restore(start + 1, position), chronon)
  )
}
