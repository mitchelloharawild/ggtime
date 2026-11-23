#' Summarise values within time granules
#'
#' @description
#' `stat_summary_time()` is a temporal variation of [ggplot2::stat_summary()].
#' Rather than summarising the values at each unique time point, it summarises
#' the values within each instance of a time `granule` (e.g. each month).
#'
#' A summary of a granule that is only partly observed is often misleading, such
#' as the total of a month which has only just started. The times within each
#' granule are checked with [mixtime::time_is_complete_at()], and implicit
#' missing values are filled with `NA` before the summary is computed. With
#' `na.rm = FALSE` (the default), functions like [mean()] and [sum()] then give
#' `NA` for incomplete granules, exactly as they do for explicit missing values.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::stat_summary
#' @param granule The time granule to summarise within, given as:
#'
#'   * a duration, e.g. `mixtime::months(1L)`
#'   * a time granule, e.g. `mixtime::cal_gregorian$month(1L)`
#'   * a bare expression, e.g. `month(1L)`, naming a granule of whichever
#'     calendar the time axis uses.
#' @param fun.data A function that is given the values within each granule and
#'   should return a data frame with variables `ymin`, `y`, and `ymax`. If no
#'   summary functions are supplied, the values are averaged with [mean()].
#' @param chronon The time granule at which values are expected to be observed,
#'   specified like `granule`. The default is the chronon of a mixtime vector,
#'   and the interval (greatest common divisor) found by
#'   [tsibble::interval_pull()] for other time classes, such as `POSIXct`.
#' @param width The computed width, as a proportion of the granule instance.
#' @param na.rm If `FALSE`, the default, missing values (both explicit, and
#'   those implied by gaps in time) are given to the summary functions. If
#'   `TRUE`, missing values are removed before the values are summarised, and
#'   granules without any values are not drawn.
#' @param orientation Which positional axis (`"x"` or `"y"`) carries the time.
#'   The default (`NA`) determines this from the scales.
#'
#' @section Aesthetics:
#'
#' `stat_summary_time()` requires the `x` and `y` aesthetics, one of which is
#' time. Other aesthetics are those of the `geom`, and are retained if they are
#' constant within each granule.
#'
#' @section Missing values:
#'
#' Within each group, every granule instance between its first and last
#' observations is summarised. A granule without observations is summarised from
#' a single `NA`, and so breaks a line rather than being drawn through.
#'
#' Values outside the scale's limits are also missing. Use the limits of
#' [ggplot2::coord_cartesian()] to zoom without affecting the summaries.
#'
#' Some summary functions always ignore missing values, including
#' [ggplot2::mean_se()]. Use the computed variable
#' `complete` to identify incomplete granules in this case.
#'
#' @section Computed variables:
#'
#' These are available to [ggplot2::after_stat()], alongside the results of the
#' summary functions:
#'
#' \describe{
#'   \item{`n`}{The number of non-missing values in the granule.}
#'   \item{`complete`}{Whether the granule has no implicit or explicit missing
#'     values.}
#'   \item{`start`, `end`}{The boundaries of the granule, in data units.}
#'   \item{`width`}{The `width` proportion of the granule, in data units.}
#' }
#'
#' @seealso [ggplot2::stat_summary()], and [stat_ohlc()] for open, high, low and
#'   close summaries.
#'
#' @examples
#' library(ggplot2)
#'
#' daily <- data.frame(
#'   date = mixtime::date(as.Date("2024-01-01") + 0:99),
#'   value = cumsum(rnorm(100))
#' )
#'
#' # Monthly totals, where the last month is incomplete and so is missing
#' ggplot(daily, aes(date, value)) +
#'   stat_summary_time(granule = month(1L), fun = sum)
#'
#' # Monthly means are on the same scale as the data
#' ggplot(daily, aes(date, value)) +
#'   geom_line(alpha = 0.3) +
#'   stat_summary_time(granule = month(1L), fun = mean, geom = "point")
#'
#' # Summarise the available values, and highlight incomplete months
#' ggplot(daily, aes(date, value)) +
#'   stat_summary_time(
#'     aes(fill = after_stat(complete)),
#'     granule = month(1L), fun = mean, geom = "col", na.rm = TRUE
#'   )
#'
#' # Hourly observations stored at a 1 second chronon
#' hourly <- data.frame(
#'   time = mixtime::datetime(
#'     as.POSIXct("2024-01-01", tz = "UTC") + 0:95 * 3600
#'   ),
#'   value = rnorm(96)
#' )
#' ggplot(hourly, aes(time, value)) +
#'   stat_summary_time(
#'     granule = day(1L), chronon = hour(1L), geom = "pointrange",
#'     fun = median, fun.min = min, fun.max = max
#'   )
#'
#' @export
stat_summary_time <- function(
  mapping = NULL,
  data = NULL,
  geom = "line",
  position = "identity",
  ...,
  granule,
  fun.data = NULL,
  fun = NULL,
  fun.max = NULL,
  fun.min = NULL,
  fun.args = list(),
  chronon = NULL,
  width = 0.9,
  na.rm = FALSE,
  orientation = NA,
  show.legend = NA,
  inherit.aes = TRUE
) {
  granule <- enquo(granule)
  if (quo_is_missing(granule)) {
    cli::cli_abort(c(
      "{.arg granule} must be supplied.",
      i = "For example, {.code granule = mixtime::months(1L)} computes one
           summary per month."
    ))
  }

  l <- layer(
    data = data,
    mapping = mapping,
    stat = StatSummaryTime,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      # Captured unevaluated, until the axis's calendar is known
      granule = granule,
      chronon = enquo(chronon),
      fun.data = fun.data,
      fun = fun,
      fun.max = fun.max,
      fun.min = fun.min,
      fun.args = fun.args,
      width = width,
      na.rm = na.rm,
      orientation = orientation,
      ...
    )
  )

  # Scales replace time with positions on their common chronon, from which the
  # completeness of the original granularity can't be determined. Keep a copy of
  # the original times, in columns that no scale applies to.
  ggproto(
    NULL,
    l,
    compute_aesthetics = function(self, data, plot) {
      out <- ggproto_parent(l, self)$compute_aesthetics(data, plot)
      out$.xtime <- out$x
      out$.ytime <- out$y
      out
    }
  )
}

#' @keywords internal
StatSummaryTime <- ggproto(
  "StatSummaryTime",
  Stat,
  # Not `required_aes`, since `Stat$compute_layer()` removes rows missing any of
  # them. The missing values are needed by the summary.
  optional_aes = c("x", "y"),
  dropped_aes = c(".xtime", ".ytime"),
  extra_params = c(
    "na.rm", "orientation", "fun.data", "fun.max", "fun.min", "fun.args"
  ),
  setup_params = function(data, params) {
    if (is.null(data$x) || is.null(data$y)) {
      cli::cli_abort(
        "{.fn stat_summary_time} requires the {.field x} and {.field y}
         aesthetics."
      )
    }
    params$fun <- make_summary_time_fun(
      params$fun.data,
      params[["fun"]],
      params$fun.max,
      params$fun.min,
      params$fun.args
    )
    # As for a tsibble's index, the interval is common to all series.
    params$interval <- list(
      x = time_interval(data$.xtime),
      y = time_interval(data$.ytime)
    )
    params
  },
  compute_panel = function(
    self,
    data,
    scales,
    granule = NULL,
    chronon = NULL,
    interval = NULL,
    fun = NULL,
    width = 0.9,
    na.rm = FALSE,
    orientation = NA
  ) {
    flipped_aes <- time_flipped_aes(scales, orientation)
    axis <- if (flipped_aes) "y" else "x"
    time <- if (flipped_aes) data$.ytime else data$.xtime
    data$.xtime <- data$.ytime <- NULL
    data <- flip_data(data, flipped_aes)
    scale <- scales[[axis]]
    calendar <- time_scale_calendar(scale)
    granule <- eval_granule(granule, calendar, arg = "granule")
    chronon <- eval_granule(chronon, calendar, arg = "chronon") %||%
      interval[[axis]]

    # Values without a (displayed) time don't belong to any granule
    keep <- is.finite(data$x)
    data <- vctrs::vec_slice(data, keep)
    time <- vctrs::vec_slice(time, keep)
    if (nrow(data) == 0L) {
      return(data.frame())
    }

    time <- as_summary_time(time, chronon, scale)
    check_summary_granule(time, granule)
    # Times belong to the granule they occur in, regardless of where the scale's
    # `align_discrete` displays them (e.g. the end of a day is in the next day).
    x <- data$x
    if (inherits(scale, "ScaleContinuousMixtime")) {
      centred <- ggproto(NULL, scale, align_discrete = 0.5)
      x <- centred$transform(time)
      if (is_mixtime(x)) {
        x <- vecvec::unvecvec(x)
      }
      x <- as.numeric(vctrs::vec_data(x))
    }
    bins <- granule_bins(x, scale, granule)

    out <- lapply(split(seq_len(nrow(data)), data$group), function(i) {
      summarise_time_group(
        vctrs::vec_slice(data, i),
        time = vctrs::vec_slice(time, i),
        bin = bins$bin[i],
        granule = granule,
        fun = fun,
        na.rm = na.rm
      )
    })
    out <- vctrs::vec_rbind(!!!unname(out))
    if (nrow(out) == 0L) {
      return(data.frame())
    }

    if (!na.rm && !any(out$complete)) {
      cli::cli_warn(c(
        "{.fn stat_summary_time} found no complete granules.",
        i = "Values are expected at every
             {.val {format_granule(mixtime::chronon_glb(time))}}. If they are
             observed less often, specify how often with {.arg chronon}.",
        i = "Use {.code na.rm = TRUE} to summarise incomplete granules."
      ))
    }

    out$start <- bins$at[out$.bin]
    out$end <- bins$at[out$.bin + 1L]
    out$x <- (out$start + out$end) / 2
    out$width <- (out$end - out$start) * width
    out$.bin <- NULL
    out$flipped_aes <- flipped_aes
    flip_data(out, flipped_aes)
  }
)

#' Summarise the granules spanned by a group
#' @param data The group's data, with time on `x`.
#' @param time The discrete times of `data`, from `as_summary_time()`.
#' @param bin The granule instance of each row of `data`.
#' @param granule The granule being summarised.
#' @param fun The summary function, from `make_summary_time_fun()`.
#' @param na.rm Should missing values be removed?
#' @returns A data frame with a row per granule instance, indexed by `.bin`.
#' @noRd
summarise_time_group <- function(data, time, bin, granule, fun, na.rm) {
  time_complete <- mixtime::time_is_complete_at(time, granule)

  rows <- lapply(seq(min(bin), max(bin)), function(b) {
    i <- which(bin == b)
    y <- data$y[i]
    observed <- sum(!is.na(y))
    complete <- length(i) > 0L && all(time_complete[i])

    if (na.rm) {
      if (observed == 0L) {
        return(NULL)
      }
      y <- y[!is.na(y)]
    } else if (!complete) {
      n_fill <- 1L
      if (length(i) > 0L) {
        expected <- mixtime::chronon_cardinality(
          mixtime::chronon_glb(time),
          granule,
          at = vctrs::vec_slice(time, i[[1L]])
        )
        n_fill <- max(1, ceiling(expected) - vctrs::vec_unique_count(time[i]))
      }
      y <- c(y, rep(NA_real_, n_fill))
    }

    # Retain the aesthetics that are constant within the granule
    from <- if (length(i) > 0L) i else seq_len(nrow(data))
    row <- vctrs::vec_slice(data, from[[1L]])
    row[c("x", "y", "ymin", "ymax", "xmin", "xmax")] <- NULL
    for (col in names(row)) {
      if (vctrs::vec_unique_count(data[[col]][from]) > 1L) {
        row[[col]] <- vctrs::vec_init(row[[col]], 1L)
      }
    }

    summary <- as.data.frame(fun(y))
    out <- vctrs::vec_cbind(
      row[rep(1L, nrow(summary)), setdiff(names(row), names(summary)),
        drop = FALSE],
      summary
    )
    out$n <- rep(observed, nrow(out))
    out$complete <- rep(complete && observed == length(i), nrow(out))
    out$.bin <- rep(b, nrow(out))
    out
  })
  vctrs::vec_rbind(!!!rows)
}

#' Combine summary functions, as `ggplot2::stat_summary()` does
#' @returns A function of a vector of values, returning a data frame.
#' @noRd
make_summary_time_fun <- function(fun.data, fun, fun.max, fun.min, fun.args) {
  as_fun <- function(f) {
    if (is.character(f)) match.fun(f) else as_function(f)
  }
  if (!is.null(fun.data)) {
    fun.data <- as_fun(fun.data)
    return(function(y) inject(fun.data(y, !!!fun.args)))
  }
  if (is.null(fun) && is.null(fun.max) && is.null(fun.min)) {
    cli::cli_inform("No summary function supplied, defaulting to {.fn mean}")
    fun <- mean
  }
  call_fun <- function(f, y) {
    if (is.null(f)) {
      return(NA_real_)
    }
    f <- as_fun(f)
    inject(f(y, !!!fun.args))
  }
  function(y) {
    data.frame(
      ymin = call_fun(fun.min, y),
      y = call_fun(fun, y),
      ymax = call_fun(fun.max, y)
    )
  }
}

#' The interval at which times are observed
#'
#' @param x A vector of times (or anything else, which has no interval).
#' @returns A granule, or `NULL` if `x` has no interval other than its own
#'   chronon.
#' @noRd
time_interval <- function(x) {
  if (is_mixtime(x) || !inherits(x, c("Date", "POSIXct"))) {
    return(NULL)
  }
  interval_as_granule(tsibble::interval_pull(x[!is.na(x)]))
}

#' Discrete times at the chronon they are observed at
#' @param x The original times.
#' @param chronon The chronon to use, or `NULL` for that of `x`.
#' @param scale The scale that measures time, which determines the time zone.
#' @noRd
as_summary_time <- function(x, chronon, scale, call = caller_env()) {
  x <- if (inherits(x, c("yearweek", "yearmonth", "yearquarter"))) {
    as_mixtime_index(x)
  } else {
    mixtime::mixtime(x)
  }
  if (length(x@x) > 1L) {
    cli::cli_abort(
      c(
        "{.fn stat_summary_time} can't check the completeness of mixed
         granularity time.",
        i = "Specify a common {.arg chronon}, or use a layer for each
             {.fn mixtime::time_chronon}."
      ),
      call = call
    )
  }
  chronon <- chronon %||% mixtime::chronon_glb(x)
  # Granule instances are those of the axis, which may be in another time zone
  own_tz <- mixtime::chronon_glb(x)@tz
  scale_tz <- if (inherits(scale, "ScaleContinuousMixtime")) {
    scale$time_chronon@tz
  }
  if (!is_naive_tz(own_tz)) {
    chronon@tz <- if (is_naive_tz(scale_tz)) own_tz else scale_tz
  }
  mixtime::mixtime(x, chronon = chronon, discrete = TRUE)
}

is_naive_tz <- function(tz) length(tz) == 0L || is.na(tz)

check_summary_granule <- function(time, granule, call = caller_env()) {
  if (!all(mixtime::time_is_determinate_at(time, granule))) {
    from <- format_granule(mixtime::chronon_glb(time))
    to <- format_granule(granule)
    cli::cli_abort(
      c(
        "Can't summarise time observed every {.val {from}} within
         {.val {to}} granules.",
        i = "{.arg granule} must be made up of whole {.arg chronon}s."
      ),
      call = call
    )
  }
}

format_granule <- function(x) paste(x@n, granule_unit(x))
