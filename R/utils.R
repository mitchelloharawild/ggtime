convert_time <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  }

  if (all(is.na(x))) {
    return(x)
  }

  if (max(x) > 1e5) {
    structure(x, class = c("POSIXct", "POSIXt"))
  } else {
    structure(x, class = "Date")
  }
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

unnest_tbl <- function(.data, tbl_col, .sep = NULL) {
  row_indices <- rep.int(
    seq_len(NROW(.data)),
    map_int(.data[[tbl_col[[1]]]], NROW)
  )

  nested_cols <- map(tbl_col, function(x) {
    lst_col <- .data[[x]]
    if (is.data.frame(lst_col[[1]])) {
      lst_col <- map(lst_col, as_tibble)
      vctrs::vec_rbind(!!!lst_col)
    } else {
      unlist(lst_col)
    }
  })

  if (!is.null(.sep)) {
    nested_cols <- map2(
      nested_cols,
      tbl_col,
      function(x, nm) set_names(x, paste(nm, colnames(x), sep = .sep))
    )
  }

  is_df <- map_lgl(nested_cols, is.data.frame)
  vctrs::vec_cbind(
    .data[row_indices, setdiff(names(.data), tbl_col), drop = FALSE], # Parent cols
    !!!set_names(nested_cols[!is_df], tbl_col[!is_df]), # Nested cols
    !!!nested_cols[is_df] # Nested df
  )
}

check_gaps <- function(x) {
  if (any(has_gaps(x)$.gaps)) {
    abort(sprintf(
      "%s contains implicit gaps in time. You should check your data and convert implicit gaps into explicit missing values using `tsibble::fill_gaps()` if required.",
      deparse(substitute(x))
    ))
  }
}

#' Reduce a duration argument to the granule it steps by
#'
#' Durations are used to specify the length of time between breaks or loops,
#' and must be a single duration. This function converts durations to granules,
#' and produces a helpful error message if this is not possible (e.g. inputs are
#' not a single duration.)
#'
#' @param x The duration to reduce: a `<mixtime>` duration, a time granule, or
#'   a waiver/`NULL` for no duration.
#' @param arg The name of the argument being checked, for the error message.
#' @param call The environment or call to report the error from.
#' @returns A time granule if `x` is a duration, and `x` unchanged otherwise.
#' @noRd
duration_as_granule <- function(x, arg = caller_arg(x), call = caller_env()) {
  # A granule (e.g. `mixtime::cal_gregorian$year(1L)`) is a scalar rather than a
  # vector, and is already what this returns -- except for its `@n`, coerced
  # to a double here to match the duration-conversion path below (whose
  # `granule@n * as.numeric(x)` is always a double). `@n`'s S7 type
  # (`class_numeric`) allows either storage mode, and a bare granule
  # constructor (`cal_gregorian$month(1L)`) builds an integer one; left alone,
  # two granules of equal value but different storage mode would not be
  # `identical()` (which is how `calendar_resolve_granules()` detects a
  # defaulted `coord_calendar()` granule), only because of how each happened
  # to be constructed.
  if (is_waiver(x) || is.null(x)) {
    return(x)
  }
  if (S7::S7_inherits(x, mixtime::mt_unit)) {
    x@n <- as.numeric(x@n)
    return(x)
  }

  if (!is_mixtime(x) && !S7::S7_inherits(x, mixtime::mt_duration)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a duration or a granule, not {.obj_type_friendly {x}}.",
        i = "A duration measures a length of time, such as {.code mixtime::days(1L)}."
      ),
      call = call
    )
  }

  size <- vctrs::vec_size(x)
  if (size != 1L) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single duration, not {size}.",
        i = "A duration gives one step, which is used for every interval."
      ),
      call = call
    )
  }

  if (!all(mixtime::time_is_duration(x))) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a duration, not a time point.",
        i = "A duration measures a length of time, such as
             {.code mixtime::years(1L)} or {.val 1 year}."
      ),
      call = call
    )
  }

  # `mixtime::years(2L)` steps by 2 years, which is a 2 year granule. This is
  # the conversion mixtime's own `seq()` method makes.
  if (is_mixtime(x)) {
    x <- vecvec::unvecvec(x)
  }
  granule <- attr(x, "chronon")
  granule@n <- granule@n * as.numeric(x)
  granule
}

#' Evaluate a granule expression against a calendar
#'
#' `coord_calendar()`'s granule arguments (`cells`, `rows`, `blocks`, `panes`,
#' `cols`) accept a bare granule expression such as `month(1L)`, which only
#' means something once resolved against a calendar: `month(1L)` should be
#' `cal_gregorian$month(1L)` on a Gregorian time axis and
#' `cal_sym454$month(1L)` on a symmetry454 one. `coord_calendar()` can't
#' resolve this at construction time -- no axis exists yet -- so the
#' expression is captured unevaluated (`rlang::enquo()`, see
#' `coord_calendar()`) and resolved here once the axis's calendar is known
#' (`calendar_resolve_granules()`, against the calendar `time_scale_calendar()`
#' reads off the panel's time scale).
#'
#' The masking itself -- evaluating the expression with the calendar's own
#' list of granule constructors (e.g. `list(year = ..., month = ..., day =
#' ...)`, see `mixtime::new_calendar()`) as an `eval_tidy()` data mask, so a
#' bare token resolves to that calendar's own unit -- is copied from
#' `mixtime::linear_time()` (mixtime/R/linear_time.R), which resolves the
#' same kind of expression (e.g. `linear_time(x, chronon = month(1L))`) the
#' same way, including the "could not find function" hint below. A
#' namespace-qualified call (`mixtime::days(1L)`) or an already-resolved
#' granule passes straight through the mask unaffected.
#' @param quo A quosure of the user's granule expression, from
#'   `rlang::enquo()`/`rlang::quo()`.
#' @param calendar The `mt_calendar` to resolve bare granule names against,
#'   as `time_scale_calendar()` gives it.
#' @inheritParams duration_as_granule
#' @returns A time granule, as `duration_as_granule()` reduces the evaluated
#'   expression to (or `NULL`/`waiver()` unchanged).
#' @noRd
eval_granule <- function(quo, calendar, arg = "x", call = caller_env()) {
  x <- tryCatch(
    eval_tidy(quo, data = calendar, env = emptyenv()),
    error = function(e) {
      # Special hint for the common case of a granule (typically `week`) not
      # defined by this calendar, adapted from the same check in
      # `mixtime::linear_time()`.
      unit <- sub(
        '^could not find function "(.*)"$',
        "\\1",
        conditionMessage(e)
      )
      if (!identical(unit, conditionMessage(e))) {
        cli::cli_abort(
          c(
            conditionMessage(e),
            i = "This calendar has no {.val {unit}} granule.",
            i = "Granule tokens are resolved against the calendar of the time
                 axis's data; supply the granule explicitly (e.g.
                 {.code mixtime::cal_isoweek${unit}(1L)}), or use a duration
                 instead."
          ),
          call = call
        )
      }
      cli::cli_abort(conditionMessage(e), call = call)
    }
  )
  duration_as_granule(x, arg = arg, call = call)
}

#' Evaluate a defaulted granule expression, falling back if unresolvable
#'
#' `coord_calendar()`'s `rows`/`cols` default to a bare granule token
#' (`week(1L)`/`quarter(1L)`) that names a *concept* rather than a specific
#' calendar's granule -- but unlike `cells`/`blocks`/`panes`, which every
#' calendar in the Granule hierarchy either supports outright or can ignore
#' (`NULL`), `week`/`quarter` are granules some calendars simply don't have
#' (`cal_gregorian` has no `week`, `cal_sym454` has no `quarter`). Left alone,
#' `eval_granule()` would error on those the same way it does for a granule
#' the user explicitly asked for by a name their axis's calendar doesn't
#' define -- appropriate for the latter, but not for a default nobody typed.
#'
#' This only swaps in `fallback` when both hold: the argument was left at its
#' default (`is_default`, from `missing()` at construction -- see
#' `coord_calendar()`) *and* the calendar genuinely has no granule named
#' `unit`. A `rows`/`cols` the caller wrote out themselves -- even if it
#' resolves to the same granule the default would have -- always goes through
#' `quo` unchanged, and still gets `eval_granule()`'s ordinary "no such
#' granule" error if its calendar can't resolve it. This is the one place
#' `rows`/`cols` need the `missing()` sentinel that `panes`'s own defaulting
#' (see `CoordCalendar$granule_specs()`) does without: `panes` decides after
#' resolving successfully, by comparing values, while here the default
#' expression itself may not be resolvable at all, so there is nothing to
#' compare.
#' @param quo A quosure of the user's granule expression, from
#'   `rlang::enquo()`/`rlang::quo()`.
#' @param is_default Was `quo` left at its default (`missing()` at
#'   construction), rather than supplied by the caller?
#' @param unit The bare granule name the default names (e.g. `"week"`),
#'   checked against `names(calendar)`.
#' @param fallback A quosure to resolve instead when `is_default` is `TRUE`
#'   and `calendar` has no `unit` granule (e.g. `quo(day(7L))`).
#' @inheritParams eval_granule
#' @returns As `eval_granule()`.
#' @noRd
eval_granule_default <- function(
  quo,
  is_default,
  unit,
  fallback,
  calendar,
  arg = "x",
  call = caller_env()
) {
  if (is_default && !(unit %in% names(calendar))) {
    quo <- fallback
  }
  eval_granule(quo, calendar, arg = arg, call = call)
}

interval_to_period <- function(interval) {
  with(
    interval,
    lubridate::years(year) +
      lubridate::period(3 * quarter + month, units = "month") +
      lubridate::weeks(week) +
      lubridate::days(day) +
      lubridate::hours(hour) +
      lubridate::minutes(minute) +
      lubridate::seconds(second) +
      lubridate::milliseconds(millisecond) +
      lubridate::microseconds(microsecond) +
      lubridate::nanoseconds(nanosecond)
  )
}

round_period <- function(period) {
  if (!lubridate::is.period(period)) {
    return(period)
  }
  if (!is.null(attr(period, "second"))) {
    attr(period, "minute") <- attr(period, "minute") %||%
      0 +
      attr(period, "second") %/% 60
    attr(period, "second") <- attr(period, "second") %% 60
  }

  if (!is.null(attr(period, "minute"))) {
    attr(period, "hour") <- attr(period, "hour") %||%
      0 +
      attr(period, "minute") %/% 60
    attr(period, "minute") <- attr(period, "minute") %% 60
  }

  if (!is.null(attr(period, "hour"))) {
    attr(period, "day") <- attr(period, "day") + attr(period, "hour") %/% 24
    attr(period, "hour") <- attr(period, "hour") %% 24
  }

  if (!is.null(attr(period, "month"))) {
    attr(period, "year") <- attr(period, "year") + attr(period, "month") %/% 12
    attr(period, "month") <- attr(period, "month") %% 12
  }
  period
}

floor_tsibble_date <- function(x, unit, ...) {
  UseMethod("floor_tsibble_date")
}
#' @export
floor_tsibble_date.default <- function(x, unit, ...) {
  unit <- round_period(unit)
  if (unit == lubridate::weeks(1)) {
    unit <- "week"
  }
  lubridate::floor_date(x, unit, week_start = 1)
}
#' @export
floor_tsibble_date.numeric <- function(x, unit, ...) {
  unit <- round_period(unit)
  unit <- if (unit@year != 0) unit@year else unit@.Data
  minx <- min(x)
  (x - minx) %/% unit * unit + minx
}
#' @export
floor_tsibble_date.yearquarter <- function(x, unit, ...) {
  yearquarter(lubridate::floor_date(as_date(x), round_period(unit), ...))
}
#' @export
floor_tsibble_date.yearmonth <- function(x, unit, ...) {
  yearmonth(lubridate::floor_date(as_date(x), round_period(unit), ...))
}
#' @export
floor_tsibble_date.yearweek <- function(x, unit, ...) {
  unit <- round_period(unit)
  if ((unit@year > 0) && (unit@day > 0)) {
    abort("Specify a period of either years or weeks to plot, not both.")
  }
  x <- lubridate::as_date(x)
  mth <- lubridate::month(x)
  wk <- as.numeric(strftime(x, "%V"))
  year <- lubridate::year(x) - (mth == 1 & wk == 53) + (mth == 12 & wk == 1)

  if (unit@year > 0) {
    year <- year - (year - 1970) %% unit@year
    wk <- "01"
    x <- paste0(year, " W", wk)
  } else if (unit@day > 0) {
    unit <- unit@day
    if (unit %% 7 > 0) {
      warn(
        "A period with fractional weeks has been specified, rounding to the nearest week"
      )
      unit <- round(unit / 7) * 7
    }
    x <- as.numeric(as_date(x)) + 3
    x <- structure((x %/% unit) * unit, class = "Date") - 3
  }
  yearweek(x)
}

time_origin <- function(x) {
  # Set origin at 1973-01-01 for weekday starting on Monday
  origin <- structure(94694400, class = c("POSIXct", "POSIXt"), tzone = "UTC")

  if (inherits(x, "yearweek")) {
    tsibble::yearweek(origin)
  } else if (inherits(x, "yearmonth")) {
    tsibble::yearmonth(origin)
  } else if (inherits(x, "yearquarter")) {
    tsibble::yearquarter(origin)
  } else if (is.numeric(x)) {
    0
  } else if (inherits(x, "Date")) {
    as.Date(origin)
  } else {
    origin
  }
}

#' @importFrom lubridate years year month as_date
time_offset_origin <- function(x, period, origin = time_origin(x)) {
  x_start <- floor_tsibble_date(x, period)

  if (inherits(x, "yearweek")) {
    tsibble::yearweek(as_date(origin) + (x - x_start))
  } else if (inherits(x, "yearmonth")) {
    tsibble::yearmonth(
      as_date(origin) +
        years(year(x) - year(x_start)) +
        months(month(x) - month(x_start))
    )
  } else if (inherits(x, "yearquarter")) {
    tsibble::yearquarter(
      as_date(origin) +
        years(year(x) - year(x_start)) +
        months(month(x) - month(x_start))
    )
  } else {
    origin + (x - x_start)
  }
}

within_bounds <- function(x, lim) {
  if (!inherits(lim, class(x))) {
    lim <- vctrs::vec_cast(lim, x)
  }
  x[x >= lim[1] & x <= lim[2]]
}

lag <- function(x, n) {
  if (n == 0) {
    return(x)
  }
  xlen <- length(x)
  n <- pmin(n, xlen)
  out <- c(rep(NA, n), x[seq_len(xlen - n)])
  out
}

ggtime_migrate_deprecate <- function(cl, pkg, version) {
  # Skip if ggtime is explicitly attached
  if ("package:ggtime" %in% search()) {
    return(NULL)
  }

  # Skip if ggtime is explicitly called with :: or :::
  if (is.call(cl[[1L]])) {
    if (identical(sym("ggtime"), cl[[1L]][[2L]])) {
      return(NULL)
    } else {
      cl[[1L]] <- cl[[1L]][[length(cl[[1L]])]]
    }
  }

  # Raise deprecation notice
  fn <- deparse(cl[[1L]])
  lifecycle::deprecate_soft(
    when = version,
    what = paste0(pkg, "::", fn, "()"),
    with = paste0("ggtime::", fn, "()"),
    details = "Graphics functions have been moved to the {ggtime} package. Please use `library(ggtime)` instead.",
    env = caller_env(),
    user_env = caller_env(2)
  )
}

#' Wrap a bare time vector for `mixtime::mixtime()`
#'
#' Going from time vectors to mixtime is necessary for [mixtime::mixtime()] to
#' correctly convert it to other chronons.
#'
#' @param x A time vector.
#' @returns `x`, as a `<mixtime>` if it was a bare `<mt_time>`.
#' @noRd
wrap_mixtime <- function(x) {
  if (S7::S7_inherits(x, mixtime::mt_time)) {
    return(mixtime::new_mixtime(x))
  }
  x
}
