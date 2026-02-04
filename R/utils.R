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

feasts_deprecate <- function(cl) {
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
    when = "0.4.2",
    what = paste0("feasts::", fn, "()"),
    with = paste0("ggtime::", fn, "()"),
    details = "Graphics functions have been moved to the {ggtime} package. Please use `library(ggtime)` instead.",
    env = caller_env(),
    user_env = caller_env(2)
  )
}
