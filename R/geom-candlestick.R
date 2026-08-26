#' Candlestick or OHLC chart layers
#'
#' @description
#'
#' A candlestick summarises the values observed within one instance of a time
#' granule with four statistics known as 'OHLC': the first value (`open`), the
#' largest (`high`), the smallest (`low`), and the last (`close`). These
#' summary statistics for each granule are drawn as a vertical line spanning
#' from `low` to `high`, overlaid with a rectangle spanning `open` to `close`.
#' The colour and fill of the geometry indicate the change's direction, by
#' default rising values use green while falling values use red.
#'
#' Candlesticks are cut on the `granule`'s boundaries, and the width of the
#' candlestick reflects the length of time in that granule. Granules of uneven
#' length (e.g. month granules on a day chronon scale via
#' `geom_candlestick(granule = month(1L)) + scale_x_mixtime(time_chronon = day(1L))`)
#' are therefore drawn at uneven widths, each filling its own instance.
#'
#' @aesthetics GeomCandlestick
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_boxplot
#' @param granule The time granule to summarise within, given as:
#'
#'   * a duration, e.g. `mixtime::days(1L)`
#'   * a time granule, e.g. `mixtime::cal_gregorian$month(1L)`
#'   * a bare expression, e.g. `month(1L)`, naming a granule of whichever
#'     calendar the time axis uses (see the "Time granules" section below).
#'
#'   There is no default: a candlestick is defined by the granule it
#'   summarises, and the granule that suits a series is a property of the
#'   analysis rather than of the data.
#' @param width The width of each candlestick's body, as a proportion of the
#'   granule instance it summarises. Defaults to `0.9`, leaving a small gap
#'   between neighbouring candlesticks. A `width` of `1` makes each body span
#'   its whole granule instance, so that neighbours touch.
#' @param lineend Line end style for the high-low line (round, butt, square).
#' @param linejoin Line join style for the open-close body (round, mitre,
#'   bevel).
#'
#' @section Positional aesthetics:
#'
#' Each of the four summary statistics is carried by an axis-specific pair of
#' the positional aesthetics [ggplot2::geom_boxplot()] uses: `[x/y]lower`,
#' `[x/y]upper`, `[x/y]min` and `[x/y]max`. Being ggplot2's own position
#' aesthetics, the position scales transform and train them, and
#' [ggplot2::flip_data()] pairs the two members up, so only the member on the
#' axis holding the values is ever mapped:
#'
#' | Statistic | Values on `y` (time on `x`) | Values on `x` (time on `y`) |
#' | --------- | --------------------------- | --------------------------- |
#' | `open`    | `lower`                     | `xlower`                    |
#' | `high`    | `ymax`                      | `xmax`                      |
#' | `low`     | `ymin`                      | `xmin`                      |
#' | `close`   | `upper`                     | `xupper`                    |
#'
#' ggplot2 spells the `y` member of the `lower`/`upper` pair without its `y`
#' prefix (`lower`, not `ylower`), as [ggplot2::geom_boxplot()] does.
#'
#' `stat_ohlc()` computes all four from the `y` aesthetic (the values to
#' summarise), so they are only mapped directly to draw values that are already
#' summarised. If these OHLC values are provided as plot aesthetics and
#' `stat_ohlc()` is used with a coarser granule, they will be incorporated into
#' the new summary at the coarser granularity (e.g. daily open-high-low-close
#' prices can be drawn as weekly or monthly candlesticks)
#'
#' @section Computed variables:
#'
#' These are calculated by `stat_ohlc()`, and available to
#' [ggplot2::after_stat()]:
#'
#' \describe{
#'   \item{`open`, `high`, `low`, `close`}{The four summary statistics, also
#'     placed on the positional aesthetics above.}
#'   \item{`direction`}{Whether the candlestick closed above (`"rising"`),
#'     below (`"falling"`), or level with (`"unchanged"`) where it opened.
#'     Mapped to `fill` and `colour` by default.}
#'   \item{`change`}{The `close` minus the `open`, i.e. `direction` as a
#'     signed magnitude rather than a three-level factor. Useful for a
#'     continuous colour or fill scale, e.g.
#'     `aes(fill = after_stat(change))`, in place of the default discrete
#'     `direction` mapping.}
#'   \item{`n`}{The number of observations summarised.}
#'   \item{`width`}{The width of the candlestick's body, in data units.}
#' }
#'
#' @examples
#' library(ggplot2)
#'
#' prices <- data.frame(
#'   time = mixtime::datetime(
#'     as.POSIXct("2024-01-01", tz = "UTC") + (0:719) * 3600
#'   ),
#'   price = 100 + cumsum(rnorm(720, 0, 0.5))
#' )
#'
#' # Summarise hourly prices into daily candlesticks, coloured red/green by
#' # `direction` by default
#' ggplot(prices, aes(time, price)) +
#'   geom_candlestick(granule = mixtime::days(1L))
#'
#' # `granule` accepts a bare granule of the axis's own calendar
#' ggplot(prices, aes(time, price)) +
#'   geom_candlestick(granule = hour(6L))
#'
#' # Any other discrete diverging scale overrides the red/green default;
#' # `colour` needs its own scale too, or it stays red/green
#' ggplot(prices, aes(time, price)) +
#'   geom_candlestick(granule = mixtime::days(1L)) +
#'   scale_fill_brewer(type = "div", palette = "RdBu", drop = FALSE) +
#'   scale_colour_brewer(type = "div", palette = "RdBu", drop = FALSE)
#'
#' # Use after_stat(change) with a diverging colour scale for extent of change
#' ggplot(prices, aes(time, price)) +
#'   geom_candlestick(
#'     aes(fill = after_stat(change), colour = after_stat(change)),
#'     granule = mixtime::days(1L)
#'   ) +
#'   scale_fill_gradient2(low = "red", mid = "grey90", high = "green") +
#'   scale_colour_gradient2(low = "red", mid = "grey90", high = "green")
#'
#' # Already summarised prices can be plotted with stat = "identity"
#' set.seed(0)
#' close <- 100 + cumsum(rnorm(90))
#' ohlc <- data.frame(
#'   time = mixtime::date(as.Date("2024-01-01") + 0:89),
#'   open = c(100, close[-length(close)]),
#'   close = close
#' )
#' ohlc$high <- pmax(ohlc$open, ohlc$close) + runif(90, 0, 2)
#' ohlc$low <- pmin(ohlc$open, ohlc$close) - runif(90, 0, 2)
#' ohlc$direction <- ifelse(sign(ohlc$close - ohlc$open) > 0, "rising", "falling")
#' ggplot(
#'   ohlc,
#'   aes(
#'     x = time, fill = direction, colour = direction,
#'     lower = open, upper = close,
#'     ymin = low, ymax = high
#'   )
#' ) +
#'   geom_candlestick(stat = "identity")
#'
#' # Using stat = "ohlc" (the default) with complete OHLC data will
#' # compute the OHLC prices at coarser granules
#' ggplot(
#'   ohlc,
#'   aes(
#'     x = time, lower = open, upper = close,
#'     ymin = low, ymax = high
#'   )
#' ) +
#'   geom_candlestick(granule = mixtime::cal_isoweek$week(1L))
#'
#' @export
geom_candlestick <- function(
  mapping = NULL,
  data = NULL,
  stat = "ohlc",
  position = "identity",
  ...,
  granule,
  width = 0.9,
  lineend = "butt",
  linejoin = "mitre",
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {
  params <- list2(
    width = width,
    lineend = lineend,
    linejoin = linejoin,
    na.rm = na.rm,
    orientation = orientation,
    ...
  )
  # Captured unevaluated, since a bare granule (`month(1L)`) means nothing
  # until the axis's calendar is known (see `stat_ohlc()`). Omitted
  # from the params entirely when not supplied, so that `stat = "identity"`
  # doesn't warn about a parameter its statistic has no use for.
  granule <- enquo(granule)
  if (!quo_is_missing(granule)) {
    params$granule <- granule
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCandlestick,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @rdname geom_candlestick
#' @export
stat_ohlc <- function(
  mapping = NULL,
  data = NULL,
  geom = "candlestick",
  position = "identity",
  ...,
  granule,
  width = 0.9,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {
  params <- list2(
    width = width,
    na.rm = na.rm,
    orientation = orientation,
    ...
  )
  granule <- enquo(granule)
  if (!quo_is_missing(granule)) {
    params$granule <- granule
  }

  layer(
    data = data,
    mapping = mapping,
    stat = StatOhlc,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = params
  )
}

#' @keywords internal
StatOhlc <- ggproto(
  "StatOhlc",
  Stat,
  required_aes = "x|y",
  # `lower`/`upper`/`ymin`/`ymax` are consumed when already-summarised data is
  # re-summarised onto a coarser granule, and recomputed on the way out.
  non_missing_aes = c("x", "y", "lower", "upper", "ymin", "ymax"),
  # Either orientation's time and value aesthetics: the pair that survives is
  # rebuilt by `compute_panel()`, and the pair that doesn't is genuinely gone.
  dropped_aes = c("x", "y"),
  default_aes = aes(
    colour = after_stat(direction),
    fill = after_stat(direction)
  ),
  extra_params = c("na.rm", "orientation"),
  setup_params = function(data, params) {
    if (is.null(params$granule)) {
      cli::cli_abort(c(
        "{.arg granule} must be supplied.",
        i = "A candlestick summarises the values within one instance of a time
             granule, so the granule must be coarser than the chronon the data
             is measured at.",
        i = "For example, {.code granule = mixtime::days(1L)} draws one
             candlestick per day."
      ))
    }
    params
  },
  compute_panel = function(
    self,
    data,
    scales,
    granule = NULL,
    width = 0.9,
    na.rm = FALSE,
    orientation = NA
  ) {
    flipped_aes <- candlestick_flipped_aes(scales, orientation)
    data <- flip_data(data, flipped_aes)
    # Both the time axis and the calendar its granules resolve against come
    # from whichever scale carries time, which flipping has just made `x`.
    scale <- if (flipped_aes) scales$y else scales$x
    granule <- eval_granule(
      granule,
      time_scale_calendar(scale),
      arg = "granule"
    )

    summarised <- all(c("lower", "upper", "ymin", "ymax") %in% names(data))
    if (!summarised && is.null(data$y)) {
      # Named in the orientation the user mapped, not the one flipping has
      # just put the values on.
      value_aes <- if (flipped_aes) "x" else "y"
      cli::cli_abort(c(
        "{.fn stat_ohlc} needs values to summarise.",
        i = "Map the values to summarise to the {.field {value_aes}}
             aesthetic.",
        i = "Already summarised values are mapped to {.field lower},
             {.field upper}, {.field ymin} and {.field ymax}; see the
             {.emph Summary statistics} section of {.fn ggtime::geom_candlestick}."
      ))
    }
    if (nrow(data) == 0L) {
      return(flip_data(data, flipped_aes))
    }
    if (!summarised) {
      data$lower <- data$upper <- data$ymin <- data$ymax <- data$y
    }

    bins <- candlestick_bins(data$x, scale, granule)

    # Sorted so that "first" and "last" within a bin mean first and last in
    # time, rather than in row order. `order()` is stable, so observations
    # sharing a time keep the order they arrived in.
    ord <- order(data$group, bins$bin, data$x)
    data <- vctrs::vec_slice(data, ord)
    bin <- bins$bin[ord]

    groups <- vctrs::vec_group_loc(vctrs::data_frame(
      group = data$group,
      bin = bin
    ))
    loc <- groups$loc
    first <- vapply(loc, function(i) i[[1L]], integer(1L))
    last <- vapply(loc, function(i) i[[length(i)]], integer(1L))

    open <- data$lower[first]
    close <- data$upper[last]
    # No `na.rm` of its own: `Stat$compute_layer()` has already dropped every
    # row missing one of `non_missing_aes`, so a bin holds only finite values,
    # and `vec_group_loc()` never produces an empty one.
    high <- vapply(loc, function(i) max(data$ymax[i]), numeric(1L))
    low <- vapply(loc, function(i) min(data$ymin[i]), numeric(1L))

    # The first row of each bin carries every column the summary doesn't
    # recompute: `PANEL`, `group`, and any aesthetic constant within a group.
    out <- vctrs::vec_slice(data, first)
    out$y <- NULL

    at <- bins$at
    start <- at[groups$key$bin]
    end <- at[groups$key$bin + 1L]
    out$x <- (start + end) / 2
    out$width <- (end - start) * width
    out$xmin <- out$x - out$width / 2
    out$xmax <- out$x + out$width / 2

    out$lower <- out$open <- open
    out$upper <- out$close <- close
    out$ymin <- out$low <- low
    out$ymax <- out$high <- high
    out$change <- close - open
    # An ordered factor of `candlestick_directions`, so that a diverging scale
    # places "unchanged" at its midpoint and the two directions at its
    # extremes, whether or not the data uses all three.
    direction <- candlestick_directions[sign(out$change) + 2L]
    out$direction <- new_candlestick(
      factor(direction, levels = candlestick_directions, ordered = TRUE)
    )
    out$n <- lengths(loc)
    out$flipped_aes <- flipped_aes

    flip_data(out, flipped_aes)
  }
)

#' Legend key for candlesticks
#'
#' A body spanning the middle of the key, with the high-low line running the
#' full height of it either side.
#' @inheritParams ggplot2::draw_key_rect
#' @returns A grob.
#' @export
#' @keywords internal
draw_key_candlestick <- function(data, params, size) {
  colour <- data$colour %||% "grey20"
  alpha <- data$alpha %||% NA
  linewidth <- data$linewidth %||% 0.5
  linetype <- data$linetype %||% 1

  grid::grobTree(
    grid::linesGrob(
      x = c(0.5, 0.5),
      y = c(0.05, 0.95),
      gp = gg_par(col = alpha(colour, alpha), lwd = linewidth, lty = linetype)
    ),
    grid::rectGrob(
      x = 0.5,
      y = 0.5,
      width = 0.6,
      height = 0.5,
      gp = gg_par(
        col = alpha(colour, alpha),
        fill = alpha(data$fill %||% NA, alpha),
        lwd = linewidth,
        lty = linetype
      )
    )
  )
}

#' @keywords internal
GeomCandlestick <- ggproto(
  "GeomCandlestick",
  Geom,
  required_aes = c(
    "x|y",
    "lower|xlower",
    "upper|xupper",
    "ymin|xmin",
    "ymax|xmax"
  ),
  default_aes = aes(
    colour = from_theme(ink),
    fill = from_theme(paper),
    linewidth = from_theme(borderwidth),
    linetype = from_theme(bordertype),
    alpha = NA
  ),
  extra_params = c("na.rm", "orientation", "width"),
  setup_params = function(data, params) {
    # Only one of `x`/`y` is mapped when the layer draws already summarised
    # values with `stat = "identity"`, and it is the one carrying time.
    params$flipped_aes <- has_flipped_aes(
      data,
      params,
      main_is_orthogonal = FALSE
    )
    params
  },
  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- flip_data(data, params$flipped_aes)
    # `stat_ohlc()` sizes each body against the granule instance it
    # summarises, so `xmin`/`xmax` arrive already set. Without it (a layer
    # drawn with `stat = "identity"`) there is no granule to measure against,
    # so `width` falls back to a proportion of the axis's own resolution, as
    # every other width-taking geometry does.
    if (is.null(data$xmin) || is.null(data$xmax)) {
      width <- data$width %||%
        (resolution(data$x, FALSE, TRUE) * (params$width %||% 0.9))
      data$xmin <- data$x - width / 2
      data$xmax <- data$x + width / 2
    }
    flip_data(data, params$flipped_aes)
  },
  draw_key = draw_key_candlestick,
  draw_panel = function(
    self,
    data,
    panel_params,
    coord,
    lineend = "butt",
    linejoin = "mitre",
    width = 0.9,
    flipped_aes = FALSE
  ) {
    data <- flip_data(data, flipped_aes)
    if (nrow(data) == 0L) {
      return(zeroGrob())
    }

    common <- data[c(
      "colour",
      "linewidth",
      "linetype",
      "alpha",
      "group",
      "PANEL"
    )]

    # Spans the whole of `low` to `high`, rather than only the parts either
    # side of the body, so that a hollow body draws over it rather than
    # meeting it.
    wick <- vctrs::vec_cbind(
      vctrs::data_frame(
        x = data$x,
        xend = data$x,
        y = data$ymin,
        yend = data$ymax
      ),
      common
    )
    # `lower` is the opening value and `upper` the closing one, so either may
    # be the larger; the rectangle takes them in drawing order instead.
    body <- vctrs::vec_cbind(
      vctrs::data_frame(
        xmin = data$xmin,
        xmax = data$xmax,
        ymin = pmin(data$lower, data$upper),
        ymax = pmax(data$lower, data$upper),
        fill = data$fill
      ),
      common
    )

    grob <- grid::grobTree(
      GeomSegment$draw_panel(
        flip_data(wick, flipped_aes),
        panel_params,
        coord,
        lineend = lineend
      ),
      GeomRect$draw_panel(
        flip_data(body, flipped_aes),
        panel_params,
        coord,
        lineend = lineend,
        linejoin = linejoin
      )
    )
    grob$name <- grid::grobName(grob, "geom_candlestick")
    grob
  },
  rename_size = FALSE
)


# helpers -----------------------------------------------------------------

#' Which axis carries time?
#'
#' Decided by time-typing rather than by `ggplot2::has_flipped_aes()`'s
#' discreteness heuristic, matching the rule `geom_time_line()` uses: a
#' candlestick's value axis is continuous just like its time axis, so
#' discreteness cannot tell them apart. `x` wins when both, or neither, axis
#' is time-valued.
#' @param scales The panel's scales, as `Stat$compute_panel()` receives them.
#' @param orientation An explicit `orientation` parameter, or `NA` to decide
#'   from the scales.
#' @returns `TRUE` when time is on `y`, `FALSE` when it is on `x`.
#' @noRd
candlestick_flipped_aes <- function(scales, orientation = NA) {
  if (!is.null(orientation) && !is.na(orientation)) {
    return(identical(orientation, "y"))
  }
  is_time_scale(scales$y) && !is_time_scale(scales$x)
}

#' Does this scale measure time?
#' @param scale A position scale, or `NULL`.
#' @noRd
is_time_scale <- function(scale) {
  inherits(
    scale,
    c(
      "ScaleContinuousMixtime",
      "ScaleContinuousDate",
      "ScaleContinuousDatetime"
    )
  )
}

#' Assign each time position to a granule instance
#'
#' Cuts the axis at the granule's own boundaries (`loop_cuts_by_duration()`,
#' shared with `coord_loop()`/`coord_calendar()`), rather than stepping a
#' fixed length of time from the start of the data, so that each bin covers
#' exactly one instance of the granule even where instances differ in length.
#' @param x Time positions, in transformed data space.
#' @param scale The scale that measures time.
#' @param granule The resolved granule, from `eval_granule()`.
#' @param call The call to report an unusable axis against.
#' @returns A list of `at` (the bin boundaries, in transformed data space; bin
#'   `k` spans `at[k]` to `at[k + 1]`) and `bin` (each position's bin).
#' @noRd
candlestick_bins <- function(x, scale, granule, call = caller_env()) {
  trans <- scale$get_transformation()
  limits <- scale$get_limits()
  cuts <- try_fetch(
    loop_cuts_by_duration(trans$inverse(limits), granule),
    error = function(cnd) {
      cli::cli_abort(
        c(
          "{.arg granule} could not be cut against this axis.",
          i = "A candlestick summarises the values within a granule of time,
               so the axis it is drawn against must measure time."
        ),
        parent = cnd,
        call = call
      )
    }
  )
  at <- as.numeric(trans$transform(cuts))
  list(at = at, bin = loop_index(x, at))
}

# Levels of the `direction` computed variable, in the order a diverging scale
# should lay them out. All three are always present, so that the midpoint of
# such a scale lands on "unchanged" whether or not the data has any.
candlestick_directions <- c("falling", "unchanged", "rising")

#' A shim class for `direction`, existing only so that ggplot2's scale
#' dispatch (`scale_type()`, below) gives it the red/green default scale
#' below instead of the viridis-based `scale_[fill/colour]_ordinal()` a plain
#' ordered factor gets. The ordered factor underneath is untouched, so
#' `direction` still sorts and prints as one.
#' @param x An ordered factor of `candlestick_directions`.
#' @noRd
new_candlestick <- function(x) {
  class(x) <- c("ggtime_candlestick", class(x))
  x
}

#' @importFrom ggplot2 scale_type
#' @export
scale_type.ggtime_candlestick <- function(x) {
  c("candlestick", "ordinal", "discrete")
}

#' Default red/green scales for candlestick direction
#'
#' @description
#' [geom_candlestick()]'s default `fill` and `colour` scales for `direction`
#' computed in [stat_ohlc()]. This default works only when the defaults are
#' used alongside the `direction` statistic computed by [stat_ohlc()], so
#' candlesticks are red (falling) or green (rising) and can be overridden
#' with an explicit colour/fill scale palette.
#'
#' @param falling,unchanged,rising Colours for each level of `direction`.
#' @param ... Passed on to [ggplot2::scale_fill_manual()] /
#'   [ggplot2::scale_colour_manual()].
#'
#' @returns A `ggproto` scale object.
#' @seealso The *Rising and falling candlesticks* section of
#'   [geom_candlestick()].
#' @name scale_candlestick
#' @rdname scale_candlestick
#' @export
scale_fill_candlestick <- function(
  ...,
  falling = "#D32F2F",
  unchanged = "grey70",
  rising = "#388E3C"
) {
  ggplot2::scale_fill_manual(
    values = c(falling = falling, unchanged = unchanged, rising = rising),
    ...
  )
}

#' @rdname scale_candlestick
#' @export
scale_colour_candlestick <- function(
  ...,
  falling = "#D32F2F",
  unchanged = "grey70",
  rising = "#388E3C"
) {
  ggplot2::scale_colour_manual(
    values = c(falling = falling, unchanged = unchanged, rising = rising),
    ...
  )
}
