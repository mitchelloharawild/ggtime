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
#' `time_chronon` argument.
#'
#' @inheritParams ggplot2::scale_x_date
#' @param time_breaks A duration giving the distance between breaks, such as
#' `mixtime::weeks(2L)` or `mixtime::years(10L)`. If both `breaks` and
#' `time_breaks` are specified, `time_breaks` wins. Each break is positioned
#' within its granule by `align_discrete`, as data of that granule would be
#' (e.g. monthly breaks fall mid-month, on monthly observations).
#' @param time_minor_breaks A duration giving the distance between minor
#' breaks, such as `mixtime::weeks(2L)` or `mixtime::years(10L)`. If both
#' `minor_breaks` and `time_minor_breaks` are specified, `time_minor_breaks`
#' wins.
#' @param time_chronon A time granule that defines the common chronon to use for
#' mixed granularity (e.g. `mixtime::tu_day(1L)`). The default automatically
#' selects it as the finest chronon that all time points can be represented in.
#' @param align_discrete Either a single number between 0 and 1, or a
#'   `aes_nudge()` object, defining how to align coarser granularities
#'   onto the common time scale.
#'
#'   If a single number is supplied, it is used for all positional aesthetics:
#'   0 means start alignment, 1 means end alignment, and 0.5 means center
#'   alignment (the default).
#'
#'   To specify different offsets for different positional aesthetics (e.g.
#'   `x`, `xmin`, `xend`, `y`, `ymin`, ...), pass a `aes_nudge()` call,
#'   for example:
#'
#'   `align_discrete = aes_nudge(center = 0.5, left = 0.25, right = 0.75)``
#'
#'   The `center`, `left`, and `right` arguments apply to the semantically
#'   equivalent positional aesthetics (e.g. `left` applies to `xstart`, `xmin`,
#'   and `xlower`).
#' @param time_labels A mixtime format string to format the labels, as described
#' in `vignette("time-format-strings", package = "mixtime")`.
#' @param transform A transformation applied to the time scale, after time
#' points have been mapped onto the common time scale. Given as either a
#' `<transform>` object or the name of one. Defaults to `"identity"`, applying
#' no further transformation.
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
#' using duration-based intervals and time formatting. These time aware
#' options are prefixed with `time_` (e.g. `time_breaks` and `time_labels`),
#' and take precedence over the non-time aware options (e.g. `breaks` and
#' `labels`). The scale's breaks are specified with [mixtime::duration()]
#' objects (e.g. `time_breaks = mixtime::months(1L)`) or a time granule.
#'
#' Labels are specified with mixtime format strings, which describe a time point
#' as glue-style `{}` placeholders holding the granules to show. Since the
#' granules come from a calendar, this works across calendars rather than only
#' Gregorian ones: `time_labels = "{cyc(month, year, label = TRUE, abbreviate =
#' TRUE)} {lin(year)}"` gives "Jan 2020". See
#' `vignette("time-format-strings", package = "mixtime")` for the full syntax.
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
#' represent a day or month? This is resolved using the `align_discrete` argument,
#' which defaults to center alignment. This means that a day is mapped to noon,
#' and a month is mapped to the middle of the month.
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
#' month. This is controlled using the `align_discrete` argument, which accepts a
#' value between 0 (start alignment) and 1 (end alignment) and defaults to 0.5.
#'
#' The common time scale that defines how all granularities are mapped is
#' automatically identified based on the input data. This is achieved by finding
#' the finest chronon that all time points can be represented in. For example,
#' if the data contains both monthly and daily time points, the common time scale
#' will be daily, with the monthly points aligned according to the `align_discrete`
#' argument. If multiple time zones are present, the common time zone will
#' default to UTC. The common time scale can be manually specified using the
#' `time_chronon` argument, which accepts a `mixtime::time_unit`.
#'
#' @examples
#' library(ggplot2)
#' library(dplyr)
#' uad_month <- tibble(
#'   time = mixtime::yearmonth("1973 Jan") + 0:71,
#'   value = USAccDeaths
#' )
#' uad_year <- uad_month %>%
#'   group_by(time = mixtime::year(time)) %>%
#'   summarise(value = mean(value), .groups = "drop")
#'
#' bind_rows(
#'   month = uad_month,
#'   year = uad_year,
#'   .id = "grain"
#' ) %>%
#'   ggplot(aes(time, value, color = grain)) +
#'   geom_line() +
#'   scale_x_mixtime()
#'
#' @name scale_mixtime
NULL

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
  time_chronon = waiver(),
  align_discrete = aes_nudge(),
  transform = "identity",
  limits = NULL,
  expand = waiver(),
  oob = scales::censor,
  guide = waiver(),
  position = "bottom",
  sec.axis = waiver()
) {
  sc <- mixtime_scale(
    aesthetics = ggplot_global$x_aes,
    name = name,
    palette = identity,
    breaks = breaks,
    time_breaks = time_breaks,
    minor_breaks = minor_breaks,
    time_minor_breaks = time_minor_breaks,
    labels = labels,
    time_labels = time_labels,
    time_chronon = time_chronon,
    align_discrete = align_discrete,
    transform = transform,
    guide = guide,
    limits = limits,
    expand = expand,
    oob = oob,
    position = position
  )
  set_sec_axis(sec.axis, sc)
}

#' @export
#' @rdname scale_mixtime
scale_y_mixtime <- function(
  name = waiver(),
  breaks = waiver(),
  time_breaks = waiver(),
  minor_breaks = waiver(),
  time_minor_breaks = waiver(),
  labels = waiver(),
  time_labels = waiver(),
  time_chronon = waiver(),
  align_discrete = aes_nudge(),
  transform = "identity",
  limits = NULL,
  expand = waiver(),
  oob = scales::censor,
  guide = waiver(),
  position = "left",
  sec.axis = waiver()
) {
  sc <- mixtime_scale(
    aesthetics = ggplot_global$y_aes,
    name = name,
    palette = identity,
    breaks = breaks,
    time_breaks = time_breaks,
    minor_breaks = minor_breaks,
    time_minor_breaks = time_minor_breaks,
    labels = labels,
    time_labels = time_labels,
    time_chronon = time_chronon,
    align_discrete = align_discrete,
    transform = transform,
    guide = guide,
    limits = limits,
    expand = expand,
    oob = oob,
    position = position
  )
  set_sec_axis(sec.axis, sc)
}

#' Non-positional scales for mixtime data
#'
#' Beyond `x` and `y`, mixtime vectors can also be mapped to `colour`,
#' `fill`, `alpha`, `size`, and `linewidth` (e.g. to shade points or lines by
#' when they occurred). These are the default scales used for mixtime vectors
#' mapped to those aesthetics, and are automatically applied. To override the
#' scale's behaviour manually, use `scale_*_mixtime`.
#'
#' Unlike [scale_x_mixtime()] and [scale_y_mixtime()], these scales don't draw
#' a common time axis shared by multiple layers, so there is only a single
#' aesthetic column to map. The common chronon (and, for `colour`/`fill`, the
#' colour gradient) is still resolved the same way as the position scales,
#' with mixed granularities coerced onto the finest chronon they can all be
#' represented in and `time_breaks`/`time_labels` accepted for time-aware
#' breaks and labelling.
#'
#' @inheritParams scale_mixtime
#' @inheritParams ggplot2::scale_colour_gradient
#' @param aesthetics The names of the aesthetics that this scale should be
#' applied to, allowing this to be used with a different aesthetic (e.g. a
#' custom aesthetic tied to `colour` or `fill` by
#' [ggplot2::aes()]/[ggplot2::register_theme_elements()]).
#' @param range Output range for `size`, `alpha`, and `linewidth`, given as a
#' numeric vector of length 2. If `NULL`, the aesthetic's default range is
#' used.
#'
#' @examples
#' library(ggplot2)
#' df <- data.frame(
#'   time = mixtime::yearmonth(600:611),
#'   value = as.numeric(USAccDeaths[1:12])
#' )
#'
#' ggplot(df, aes(value, 1, colour = time)) +
#'   geom_point(size = 5) +
#'   scale_colour_mixtime()
#'
#' @name scale_mixtime_continuous
NULL

#' @export
#' @rdname scale_mixtime_continuous
scale_colour_mixtime <- function(
  name = waiver(),
  ...,
  low = "#132B43",
  high = "#56B1F7",
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "colour"
) {
  mixtime_scale(
    aesthetics = aesthetics,
    name = name,
    palette = scales::pal_seq_gradient(low, high, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_mixtime_continuous
scale_color_mixtime <- scale_colour_mixtime

#' @export
#' @rdname scale_mixtime_continuous
scale_fill_mixtime <- function(
  name = waiver(),
  ...,
  low = "#132B43",
  high = "#56B1F7",
  space = "Lab",
  na.value = "grey50",
  guide = "colourbar",
  aesthetics = "fill"
) {
  mixtime_scale(
    aesthetics = aesthetics,
    name = name,
    palette = scales::pal_seq_gradient(low, high, space),
    na.value = na.value,
    guide = guide,
    ...
  )
}

#' @export
#' @rdname scale_mixtime_continuous
scale_alpha_mixtime <- function(
  name = waiver(),
  ...,
  range = NULL,
  aesthetics = "alpha"
) {
  palette <- if (!is.null(range)) scales::pal_rescale(range) else NULL
  mixtime_scale(
    aesthetics = aesthetics,
    name = name,
    palette = palette,
    fallback.palette = scales::pal_rescale(c(0.1, 1)),
    ...
  )
}

#' @export
#' @rdname scale_mixtime_continuous
scale_size_mixtime <- function(
  name = waiver(),
  ...,
  range = NULL,
  aesthetics = "size"
) {
  palette <- if (!is.null(range)) scales::pal_area(range) else NULL
  mixtime_scale(
    aesthetics = aesthetics,
    name = name,
    palette = palette,
    fallback.palette = scales::pal_area(),
    ...
  )
}

#' @export
#' @rdname scale_mixtime_continuous
scale_linewidth_mixtime <- function(
  name = waiver(),
  ...,
  range = NULL,
  aesthetics = "linewidth"
) {
  palette <- if (!is.null(range)) scales::pal_rescale(range) else NULL
  mixtime_scale(
    aesthetics = aesthetics,
    name = name,
    palette = palette,
    fallback.palette = scales::pal_rescale(c(1, 6)),
    ...
  )
}


#' Step through a time range in whole `size` units
#'
#' Limits of a mixtime scale are an `<mt_time>` rather than a `Date` or
#' `POSIXct`, so the sequence has to be built with mixtime's own calendrical
#' arithmetic. Without this method, [scales::fullseq()] errors with "no
#' applicable method for 'fullseq'".
#'
#' Rounding outwards to whole `size` units matches the `Date` and `POSIXct`
#' methods, and keeps breaks on calendar boundaries (a "1 day" break lands at
#' midnight) rather than wherever the panel's expansion happens to fall.
#' @param range The range to cover, as an `<mt_time>`.
#' @param size The step size, as a duration or a time granule.
#' @param ... Ignored, for compatibility with other [scales::fullseq()] methods.
#' @noRd
#' @exportS3Method scales::fullseq mixtime::mt_time
`fullseq.mixtime::mt_time` <- function(range, size, ...) {
  size <- duration_as_granule(size)

  # One unit at a time, to reduce risk surface for unsupported conversion to
  # the scale's common chronon.
  unit <- size
  unit@n <- 1L
  # A duration range stays a duration, rather than being measured from the
  # epoch as a time point.
  convert <- if (S7::S7_inherits(range, mixtime::mt_duration)) {
    mixtime::duration
  } else {
    mixtime::mixtime
  }
  range <- vecvec::unvecvec(
    convert(wrap_mixtime(range), chronon = unit, discrete = FALSE)
  )

  seq(
    mixtime::time_floor(range[1], size),
    mixtime::time_ceiling(range[2], size),
    by = size
  )
}

#' Equally spaced breaks along a time scale
#'
#' The mixtime equivalent of [scales::breaks_width()]: a function factory
#' returning a break function that walks the scale's limits in steps of `width`
#' via [scales::fullseq()].
#'
#' Each break is a time of `width`'s granule, so it is positioned within that
#' granule by `align` just as the scale positions discrete time (see
#' `align_discrete`), keeping breaks on the data rather than between it.
#' @param width The break width, as a duration or a time granule.
#' @param align The position of each break within its granule, from 0 (its
#'   start) to 1 (its end).
#' @noRd
breaks_time_seq <- function(width, align = 0) {
  force(width)
  function(x) {
    breaks <- scales::fullseq(x, width)
    # A duration is a length rather than a time point, and has no granule to
    # be positioned within: the scale doesn't align duration data either.
    if (S7::S7_inherits(breaks, mixtime::mt_duration)) {
      return(breaks)
    }
    # `fullseq()` steps in single units of `width`'s granule, so this is
    # a position within the unit each break starts.
    breaks + align
  }
}

#' The coarser granules a chronon nests within
#'
#' A chronon nests a fixed number of times within some coarser granules (3
#' months to a quarter, 12 to a year; 7 days to a week), and these define the
#' natural break steps along a time scale. Coarser granules are searched for
#' among those of mixtime's calendars, as a chronon's own calendar may not hold
#' them all (weeks are not in the Gregorian calendar of a `date()`).
#'
#' @param chronon The chronon to nest, as a time granule.
#' @returns A list of `multiple`s (the number of chronons in each granule,
#'   starting with `1` for the chronon itself) and their `granule`s, ordered
#'   from finest to coarsest. Each multiple divides the next, so that every
#'   coarser granule is made up of whole finer ones.
#' @noRd
chronon_nesting <- function(chronon) {
  # Searching the calendars is slow (each granule without a fixed path errors),
  # and the breaks of every panel and axis ask again.
  key <- rlang::hash(chronon)
  if (!is.null(nested <- chronon_nesting_cache[[key]])) {
    return(nested)
  }

  calendars <- list(
    mixtime::time_calendar(chronon),
    mixtime::cal_gregorian,
    mixtime::cal_isoweek,
    mixtime::cal_time_civil
  )
  granules <- unlist(
    lapply(calendars, function(cal) lapply(names(cal), function(u) cal[[u]](1L))),
    recursive = FALSE
  )

  multiples <- vapply(
    granules,
    function(granule) {
      # Granules without a fixed path from the chronon (days within a month)
      # error, and are no natural step.
      tryCatch(
        as.numeric(mixtime::chronon_cardinality_fixed(chronon, granule)),
        error = function(e) NA_real_
      )
    },
    numeric(1L)
  )
  keep <- !is.na(multiples) & multiples > 1 & multiples == round(multiples)
  keep <- keep & !duplicated(multiples)
  ord <- order(multiples[keep])
  multiples <- multiples[keep][ord]
  granules <- granules[keep][ord]

  # Granules which don't nest within one another (if any) can't share an
  # alignment, so only the finest of them is kept.
  nested <- list(multiple = 1, granule = list(chronon))
  for (i in seq_along(multiples)) {
    if (multiples[i] %% nested$multiple[length(nested$multiple)] == 0) {
      nested$multiple <- c(nested$multiple, multiples[i])
      nested$granule <- c(nested$granule, granules[i])
    }
  }

  # Calendars' granules are time zone naive, but a day starts at midnight in
  # the chronon's own time zone.
  if (S7::S7_inherits(chronon, mixtime::mt_tz_unit)) {
    nested$granule <- lapply(nested$granule, function(granule) {
      if (S7::S7_inherits(granule, mixtime::mt_tz_unit)) {
        granule@tz <- chronon@tz
      }
      granule
    })
  }

  chronon_nesting_cache[[key]] <- nested
  nested
}

chronon_nesting_cache <- new.env(parent = emptyenv())

#' Breaks along a time scale which follow its calendar structure
#'
#' The mixtime equivalent of [scales::breaks_pretty()]. Rather than stepping by
#' round numbers of the scale's chronon (20 months), breaks step by the
#' divisors of how the chronon nests within coarser granules (1, 3 or 6 months
#' within a year), or round multiples of the coarsest of them (1, 2, 5 or 10
#' years). Time points are aligned to the coarser granule, so that 3 monthly
#' breaks fall on each quarter, and lengths of time count from zero.
#'
#' Chronons which nest in no coarser granule (years) use
#' [scales::breaks_pretty()].
#' @param n The desired number of breaks.
#' @noRd
breaks_mixtime <- function(n = 5) {
  n_default <- n
  function(x, n = n_default) {
    pretty <- function() {
      breaks <- scales::breaks_pretty()(x, n)
      if (S7::S7_inherits(x, mixtime::mt_time)) {
        breaks <- vctrs::vec_restore(breaks, x)
      }
      breaks
    }
    if (!S7::S7_inherits(x, mixtime::mt_time)) {
      return(pretty())
    }
    rng <- range(vctrs::vec_data(x), na.rm = TRUE, finite = TRUE)
    span <- diff(rng)
    nesting <- chronon_nesting(attr(x, "chronon"))
    multiple <- nesting$multiple
    # Fewer than two whole chronons is better broken into fractions of one.
    if (length(multiple) < 2L || !is.finite(span) || span < 2) {
      return(pretty())
    }

    # Divisors of each granule within the next, then round multiples of the
    # coarsest granule.
    top <- multiple[length(multiple)]
    steps <- unlist(Map(
      function(fine, coarse) {
        ratio <- coarse %/% fine
        fine * which(ratio %% seq_len(ratio - 1) == 0)
      },
      multiple[-length(multiple)],
      multiple[-1L]
    ))
    magnitude <- 10^seq(0, max(0, ceiling(log10(span / top))))
    steps <- c(steps, top * as.vector(outer(c(1, 2, 5), magnitude)))
    # Ties go to the coarser step.
    steps <- rev(steps)
    step <- steps[which.min(abs(span / steps - n))]

    # Lengths of time and cyclical time count from zero, but time points are
    # counted from the start of the granule the steps divide.
    origin <- 0
    if (S7::S7_inherits(x, mixtime::mt_linear)) {
      if (step <= top) {
        granule <- nesting$granule[[which(multiple >= step & multiple %% step == 0)[1L]]]
      } else {
        granule <- nesting$granule[[length(multiple)]]
        granule@n <- granule@n * step / top
      }
      start <- mixtime::time_floor(wrap_mixtime(vctrs::vec_restore(rng[1], x)), granule)
      origin <- vctrs::vec_data(vecvec::unvecvec(start))
    }

    breaks <- origin +
      step * seq(floor((rng[1] - origin) / step), ceiling((rng[2] - origin) / step))
    vctrs::vec_restore(breaks, x)
  }
}

#' Label time values by the granule they fall in
#'
#' Breaks are continuous positions on a time scale, but a label naming the
#' granule a break falls in ("Jan", "2015-01-01") is more use than one
#' measuring how far through the scale's own chronon it sits ("Jan 0.0%",
#' "2015-01-01 00:00:00"), so they are labelled discretely, at the granularity
#' the breaks were stepped by rather than the data's.
#'
#' The chronon's attribute suffix (the time zone or coordinates) are dropped.
#'
#' @param x Time values, as handed to a scale's `labels` function.
#' @param chronon The granule to label at, or `NULL` for `x`'s own.
#' @param cycle The cycle to label within, from `loop_cycle()`, or `NULL` for
#'   linear labels.
#' @returns A character vector of labels.
#' @noRd
time_labels_at <- function(x, chronon = NULL, cycle = NULL) {
  if (is_mixtime(x)) {
    x <- vecvec::unvecvec(x)
  }

  labels <- rep(NA_character_, vctrs::vec_size(x))
  # Breaks falling outside the panel are censored to `NA`, and name no granule.
  finite <- is.finite(vctrs::vec_data(x))
  if (!any(finite)) {
    return(labels)
  }

  labels[finite] <- format(
    time_discrete_at(x[finite], chronon = chronon, cycle = cycle),
    attr = FALSE
  )
  labels
}

#' Discretise time values to a granule
#'
#' Time points become the granule they fall in, while durations stay durations
#' counted in whole granules: converting a duration with `mixtime::mixtime()`
#' would measure it from the epoch, labelling "4 quarters" as "1971 Q1".
#'
#' @inheritParams time_labels_at
#' @returns A discrete `mixtime`.
#' @noRd
time_discrete_at <- function(x, chronon = NULL, cycle = NULL) {
  if (is_mixtime(x)) {
    x <- vecvec::unvecvec(x)
  }
  args <- list(wrap_mixtime(x), discrete = TRUE)
  if (!is.null(chronon)) {
    args$chronon <- chronon
  }
  if (S7::S7_inherits(x, mixtime::mt_duration)) {
    return(inject(mixtime::duration(!!!args)))
  }
  if (!is.null(cycle)) {
    args$cycle <- cycle
  }
  inject(mixtime::mixtime(!!!args))
}

#' @keywords internal
mixtime_scale <- function(
  aesthetics,
  palette,
  breaks = breaks_mixtime(),
  time_breaks = waiver(),
  minor_breaks = waiver(),
  time_minor_breaks = waiver(),
  labels = waiver(),
  time_labels = waiver(),
  time_chronon = waiver(),
  align_discrete = aes_nudge(),
  transform = "identity",
  guide = waiver(),
  call = caller_call(),
  ...
) {
  call <- call %||% current_call()

  if (!is_waiver(time_breaks)) {
    time_breaks <- duration_as_granule(time_breaks, call = call)
    # Breaks are aligned like the scale's own discrete time points.
    align <- align_discrete
    if (is.function(align)) {
      align <- align(aesthetics[1])
    }
    breaks <- breaks_time_seq(time_breaks, align)
  }
  if (!is_waiver(time_minor_breaks)) {
    time_minor_breaks <- duration_as_granule(time_minor_breaks, call = call)
    minor_breaks <- breaks_time_seq(time_minor_breaks)
  }
  if (!is_waiver(time_labels)) {
    labels <- function(self, x) {
      granule <- self$time_breaks
      if (!is_waiver(granule) && !is.null(granule)) {
        x <- time_discrete_at(x, chronon = granule)
      }
      format(x, format = time_labels)
    }
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
    transform = transform_mixtime(transform),
    call = call,
    ...,
    super = ggproto(
      NULL,
      scale_class,
      time_chronon = time_chronon,
      align_discrete = align_discrete,
      time_breaks = time_breaks,
    )
  )

  # Range is hard-coded and not inherited by `super` in
  # `ggplot2::continuous_scale`, replace it.
  # sc$range <- MixtimeRange$new()
  sc
}

#' Aesthetics carrying a length of time rather than a point in time
#'
#' `geom_time_line()`'s `[x/y]timeoffset` aesthetics measure how far the time
#' being drawn sits from the instant it occurred. They are durations, not
#' positions, but they are subtracted from positions, so they have to be
#' expressed in the same chronon as the positions set by this scale.
#'
#' They are deliberately *not* listed in the scale's `aesthetics` to avoid
#' training the axis range with mixed modes of time.
#'
#' @param aesthetics The scale's own `aesthetics`, to take the axis from.
#' @returns The offset aesthetic belonging to the same axis as `aesthetics`.
#' @noRd
time_offset_aes <- function(aesthetics) {
  if ("x" %in% aesthetics) "xtimeoffset" else "ytimeoffset"
}

#' Convert a time offset onto the scale's common chronon
#'
#' Not routed through `Scale$transform()`: an offset is a length of time, so it
#' takes neither the `align_discrete()` nudge nor the scale's `transform`.
#'
#' @param x The offset aesthetic's column, which must be a duration.
#' @param chronon The scale's common chronon, to convert onto.
#' @param aes The offset aesthetic's name, for error messages.
#' @param call The scale's call, for error messages.
#' @returns `x` as a bare number of `chronon` units.
#' @noRd
transform_time_offset <- function(x, chronon, aes, call = NULL) {
  if (!is_mixtime(x) || !all(mixtime::time_is_duration(x))) {
    cli::cli_abort(
      c(
        "The {.field {aes}} aesthetic must be a duration, not \\
         {.obj_type_friendly {x}}.",
        i = "A time offset is a length of time, which the scale converts onto
             the chronon it draws time on. A bare number has no unit to convert
             from.",
        i = "Use a {.pkg mixtime} duration, such as {.code mixtime::seconds(45)}."
      ),
      call = call
    )
  }
  # A granule at a time, as `$transform()` converts the positions: time given
  # at several granularities has its offsets stated at several granularities
  # too, and each granule is converted from its own.
  x@x <- lapply(x@x, function(v) {
    mixtime::duration(v, chronon = chronon, discrete = FALSE)@x[[1L]]
  })
  as.numeric(x)
}

#' @keywords internal
ScaleContinuousMixtime <- ggproto(
  "ScaleContinuousMixtime",
  ScaleContinuous,
  secondary.axis = waiver(),

  # The granule the breaks step by, allows control by coord_calendar's cells
  time_breaks = waiver(),

  get_labels = function(self, breaks = self$get_breaks()) {
    granule <- self$time_breaks
    if (is_waiver(granule) || is.null(granule) || !is_waiver(self$labels)) {
      return(ggproto_parent(ScaleContinuous, self)$get_labels(breaks))
    }
    # A child rather than a mutation of `self`: `get_labels()` may be called
    # more than once, and from a view scale holding the scale itself.
    labelled <- ggproto(
      NULL,
      self,
      labels = function(x) time_labels_at(x, chronon = granule)
    )
    ggproto_parent(ScaleContinuous, labelled)$get_labels(breaks)
  },
  # range = MixtimeRange$new(),
  # clone = function(self) {
  #   new <- ggproto(NULL, self)
  #   new$range <- MixtimeRange$new()
  #   new
  # },
  transform_df = function(self, df) {
    # Mostly ggplot2::Scale$transform_df, it additionally:
    # * computes the appropriate common time scale for mixed granularities
    # * passes in the aesthetic name for aes_nudge alignments
    # * converts time offsets onto the common time scale
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is_waiver(df)) {
      return()
    }
    aesthetics <- intersect(self$aesthetics, names(df))
    if (length(aesthetics) == 0) {
      return()
    }

    # Store common time type for default backtransformation, labels, and more.
    # Maybe other attributes are needed (e.g. cycle for cyclical time)
    if (is_waiver(self$time_chronon)) {
      self$time_chronon <- mixtime::chronon_glb(do.call(c, df[aesthetics]))
    }

    # TODO - Consider applying the aes_nudge here, and calling ggplot2::Scale$transform_df.
    out <- .mapply(
      self$transform,
      list(df[aesthetics], aesthetics),
      MoreArgs = NULL
    )
    names(out) <- aesthetics

    # Convert time offset aesthetics to the common time_chronon
    offsets <- intersect(time_offset_aes(self$aesthetics), names(df))
    for (aes in offsets) {
      out[[aes]] <- transform_time_offset(
        df[[aes]],
        self$time_chronon,
        aes,
        self$call
      )
    }
    df <- out

    # Add gap filling for implicit missing values
    # DESIGN: should this be in position? Position may be too late to have access to enough data.
    # df <- as_tibble(tsibble::fill_gaps(as_tsibble(
    #   df,
    #   index = x,
    #   key = c(PANEL, group)
    # )))

    df
  },
  transform = function(self, x, aes = NULL) {
    if (is_bare_numeric(x)) {
      cli::cli_abort(
        c(
          "A {.cls numeric} value was passed to a {.field mixtime} scale.",
          i = "Please use the {.pkg mixtime} package to create time values."
        ),
        call = self$call
      )
    }

    # Quick fix for Date/POSIXt types calling mixtime scales
    if (!is_mixtime(x)) {
      x <- mixtime::mixtime(x)
    }

    if (any(mixtime::time_is_cyclical(x))) {
      # For now, focus on linear arrangements of time.
      cli::cli_abort(
        c(
          "Cyclical time is not currently supported by {.pkg mixtime} scales.",
          i = "Cyclical time points cannot yet be placed relative to the start
               of their cycle."
        ),
        call = self$call
      )
    }

    is_duration <- vapply(
      x@x,
      S7::S7_inherits,
      logical(1L),
      mixtime::mt_duration
    )
    if (any(is_duration) && !all(is_duration)) {
      cli::cli_abort(
        c(
          "Can't scale durations alongside other modes of time.",
          i = "The duration mode of time measures a length of time, which has
               no position on a scale of time points."
        ),
        call = self$call
      )
    }

    align_nudge <- self$align_discrete
    # Aesthetic specific nudges from aes_nudge()
    if (is.function(align_nudge)) {
      align_nudge <- align_nudge(aes %||% "center")
    }

    x@x <- lapply(x@x, function(v) {
      if (S7::S7_inherits(v, mixtime::mt_duration)) {
        # The duration mode of time is an exact length rather than a point
        # within a granule, so it needs no `align_discrete` alignment. It must
        # also stay in the duration mode: measuring it from the epoch would
        # make "2 days" an absolute time point ("1970-01-03").
        return(mixtime::duration(
          v,
          chronon = self$time_chronon,
          discrete = FALSE
        )@x[[1L]])
      }

      # Use `align_discrete` to position discrete time models on continuous scales
      if (is.integer(v)) {
        v <- v + align_nudge
      }

      v <- mixtime::mixtime(
        wrap_mixtime(v),
        chronon = self$time_chronon,
        discrete = FALSE
      )
      v@x[[1L]]
    })

    ggproto_parent(ScaleContinuous, self)$transform(x)
  },

  map = function(self, x, limits = self$get_limits()) {
    if (inherits(x, "mixtime")) {
      x <- vecvec::unvecvec(x)
    }
    # as.numeric() -- extract the numerical representation.
    # This is where the mixed granularities should be mapped to a common scale.
    # `limits` is already in transformed space, so `oob` compares like with like.
    scaled <- as.numeric(self$oob(vctrs::vec_data(x), limits))
    if (!anyNA(scaled)) {
      return(scaled)
    }
    vctrs::vec_assign(scaled, is.na(scaled), self$na.value)
  }
)

#' Aesthetic specific alignment of discrete time
#'
#' Positioning discrete time points (e.g. months) on a continuous time scale
#' (e.g. days) is indeterminate - which day should represent a month? This is
#' resolved by aligning each time point within its granularity, where 0 is
#' start alignment, 1 is end alignment, and 0.5 is center alignment.
#'
#' Different positional aesthetics often require different alignments. A ribbon
#' spanning a month should start at the beginning of the month and end at the
#' end of it, while a line should pass through its center. `aes_nudge()`
#' specifies these alignments per aesthetic, and is passed to the
#' `align_discrete` argument of [scale_x_mixtime()].
#'
#' @param center,left,right Alignment applied to centered (e.g. `x`, `xend`),
#'   lower (e.g. `xmin`, `xlower`), and upper (e.g. `xmax`, `xupper`)
#'   positional aesthetics respectively. Setting these changes the default for
#'   all semantically equivalent aesthetics below.
#' @param x,xmin,xmax,xend,xintercept,xmin_final,xmax_final,xlower,xmiddle,xupper,x0
#'   Alignment for individual `x` aesthetics.
#' @param y,ymin,ymax,yend,yintercept,ymin_final,ymax_final,ylower,ymiddle,yupper,y0
#'   Alignment for individual `y` aesthetics.
#'
#' @returns A function that takes an aesthetic name and returns its alignment,
#'   suitable for the `align_discrete` argument of [scale_x_mixtime()].
#'
#' @examples
#' # Center aligned points, with intervals spanning the full granularity
#' aes_nudge(center = 0.5, left = 0, right = 1)
#'
#' # Align all time points to the start of their granularity
#' aes_nudge(center = 0, left = 0, right = 0)
#'
#' @export
aes_nudge <- function(
  center = 0.5,
  left = 0,
  right = 1,
  x = center,
  xmin = left,
  xmax = right,
  xend = center,
  xintercept = center,
  xmin_final = left,
  xmax_final = right,
  xlower = left,
  xmiddle = center,
  xupper = right,
  x0 = center,
  y = center,
  ymin = left,
  ymax = right,
  yend = center,
  yintercept = center,
  ymin_final = left,
  ymax_final = right,
  ylower = left,
  ymiddle = center,
  yupper = right,
  y0 = center
) {
  offsets <- as.list(environment())

  function(aesthetic) {
    offsets[[aesthetic]] %||% center
  }
}
