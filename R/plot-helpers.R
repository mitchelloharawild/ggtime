globalVariables("all_of")

guess_plot_var <- function(x, y) {
  if (quo_is_null(enquo(y))) {
    mv <- measured_vars(x)
    pos <- which(vapply(x[mv], is.numeric, logical(1L)))
    if (is_empty(pos)) {
      abort(
        "Could not automatically identify an appropriate plot variable, please specify the variable to plot."
      )
    }
    inform(sprintf(
      "Plot variable not specified, automatically selected `y = %s`",
      mv[pos[1]]
    ))
    sym(mv[pos[1]])
  } else {
    get_expr(enexpr(y))
  }
}

#' Seasonal plot
#'
#' Produces a time series seasonal plot. A seasonal plot is similar to a regular
#' time series plot, except the x-axis shows data from within each season. This
#' plot type allows the underlying seasonal pattern to be seen more clearly,
#' and is especially useful in identifying years in which the pattern changes.
#'
#' The seasonal plot is drawn with [coord_loop()], which loops the continuous
#' time axis around each seasonal period. Since time remains continuous, the
#' line connecting one season to the next (e.g. December to January) is drawn
#' too, split between the right and left margins of the plot.
#'
#' @param data A tidy time series object (tsibble)
#' @param y The variable to plot (a bare expression). If NULL, it will
#' automatically selected from the data.
#' @param period The seasonal period to display. If NULL (default),
#' the largest frequency in the data is used. If numeric, it represents
#' the frequency times the interval between observations. If a string, it is a
#' whole number of a single unit (e.g., "year", "2 weeks", "1y" for 1 year,
#' "3m" for 3 months, "1d" for 1 day, "1h" for 1 hour, "1min" for 1 minute,
#' "1s" for 1 second). A mixtime duration (e.g. `mixtime::weeks(2L)`) or
#' granule can also be used.
#' Note that the data must have at least one observation per seasonal period,
#' and the period cannot be smaller than the observation interval.
#' @param facet_period A secondary seasonal period to facet by
#' (typically smaller than period).
#' @param max_col The maximum number of colours to display on the plot. If the
#' number of seasonal periods in the data is larger than `max_col`, the plot
#' will not include a colour. Use `max_col = 0` to never colour the lines, or Inf
#' to always colour the lines. If labels are used, then max_col will be ignored.
#' @param max_col_discrete The maximum number of colours to show using a discrete colour scale.
#' @param pal A colour palette to be used.
#' @param polar If TRUE, the season plot will be shown on polar coordinates
#' (looping [ggplot2::coord_radial()]).
#' @param labels Position of the labels for seasonal period identifier.
#' @param labels_repel If TRUE, the seasonal period identifying labels will be repelled with the ggrepel package.
#' @param labels_left_nudge,labels_right_nudge Allows seasonal period identifying labels to be nudged to the left or right from their default position.
#' @param ... Additional arguments passed to geom_line()
#'
#' @return A ggplot object showing a seasonal plot of a time series.
#'
#' @references
#' Hyndman and Athanasopoulos (2019) Forecasting: principles and practice,
#'  3rd edition, OTexts: Melbourne, Australia. https://OTexts.com/fpp3/
#'
#' @examplesIf requireNamespace("tsibbledata", quietly = TRUE)
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_retail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   gg_season(Turnover)
#'
#' @importFrom ggplot2 ggplot aes geom_line
#' @importFrom dplyr group_by summarise mutate filter
#' @export
gg_season <- function(
  data,
  y = NULL,
  period = NULL,
  facet_period = NULL,
  max_col = Inf,
  max_col_discrete = 7,
  pal = scales::hue_pal()(9),
  polar = FALSE,
  labels = c("none", "left", "right", "both"),
  labels_repel = FALSE,
  labels_left_nudge = 0,
  labels_right_nudge = 0,
  ...
) {
  # Guide users from {feasts} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "feasts", "0.4.2")

  y <- guess_plot_var(data, !!enquo(y))

  labels <- match.arg(labels)
  check_gaps(data)
  if (NROW(data) == 0) {
    abort(
      "The data must contain at least one observation to identify seasonal periods."
    )
  }
  idx <- index_var(data)
  n_key <- n_keys(data)
  keys <- key(data)
  ts_interval <- interval_as_granule(interval(data))

  period <- season_period(period, ts_interval)
  facet_period <- season_period(facet_period, ts_interval, "facet_period")

  data <- as_tibble(data)
  y_lab <- as_label(y)
  data <- eval_plot_var(data, y)
  y <- sym(".y")
  # Time stays continuous, and is looped over the period by `coord_loop()`
  data[[idx]] <- as_mixtime_index(data[[idx]])
  data[["id"]] <- season_id(data[[idx]], period)
  data[["facet_id"]] <- if (is.null(facet_period)) {
    FALSE
  } else {
    season_id(data[[idx]], facet_period)
  }

  num_ids <- length(unique(data[["id"]]))

  # A single line per series, so the connection between seasons is drawn
  # (and cut by `coord_loop()` into the panel's margins)
  mapping <- aes(
    x = !!sym(idx),
    y = !!y,
    colour = unclass(!!sym("id"))
  )

  if (num_ids > max_col) {
    mapping$colour <- NULL
  }

  p <- ggplot(data, mapping) +
    geom_line(...) +
    scale_x_mixtime() +
    ggplot2::labs(y = y_lab, colour = NULL)

  if (num_ids <= max_col) {
    breaks <- if (num_ids <= max_col_discrete) {
      seq_len(num_ids)
    } else {
      function(x) scales::oob_discard(scales::extended_breaks()(x), x)
    }
    p <- p +
      ggplot2::scale_color_gradientn(
        colours = pal,
        breaks = breaks,
        labels = function(idx) levels(data$id)[idx]
      )
  }

  if (num_ids <= max_col_discrete) {
    p <- p + ggplot2::guides(colour = ggplot2::guide_legend())
  }

  if (!is.null(facet_period)) {
    p <- p +
      facet_grid(
        rows = vars(!!!keys),
        cols = vars(!!sym("facet_id")),
        scales = ifelse(n_key > 1, "free", "free_x")
      )
  } else if (n_key > 1) {
    p <- p +
      facet_grid(
        rows = vars(!!!lapply(keys, function(x) expr(format(!!x)))),
        scales = "free_y"
      )
  }

  p <- p +
    if (polar) {
      coord_loop(time_loops = period, coord = ggplot2::coord_radial())
    } else {
      # Loops fill the time axis exactly, but the value axis is expanded as
      # usual (top, right, bottom, left).
      loop <- coord_loop(
        time_loops = period,
        expand = c(TRUE, FALSE, TRUE, FALSE)
      )
      if (labels == "none") {
        loop
      } else {
        # Room for labels outside the loop window, sized in absolute units
        # to fit the widest label however wide the panel is drawn. (A new
        # name, since ggproto looks up the parent `loop` by name later.)
        ggplot2::ggproto(
          NULL,
          loop,
          time_padding = season_label_padding(
            levels(data$id),
            left = labels %in% c("left", "both"),
            right = labels %in% c("right", "both")
          )
        )
      }
    }

  if (labels != "none") {
    labeller <- if (labels_repel) {
      rlang::check_installed("ggrepel")
      function(...) {
        ggrepel::geom_text_repel(..., direction = "y", segment.colour = NA)
      }
    } else {
      ggplot2::geom_text
    }
    # Rows of each season's first and last observations, ordered and grouped
    # once for all seasons: grouped dplyr verbs dispatch mixtime's S7 methods
    # (and format times for `%in%`) once per season, which is very slow.
    season <- vctrs::vec_group_id(
      data[c(vapply(keys, as_string, character(1L)), "facet_id", "id")]
    )
    season_order <- order(season, xtfrm(data[[idx]]))
    season_rows <- function(from_last) {
      season_order[!duplicated(season[season_order], fromLast = from_last)]
    }

    # Labels are placed at the season's boundaries rather than its first and
    # last observations, keeping them clear of the lines connecting seasons
    # (drawn up to the loop's edges). `+ 0` gives the continuous start of
    # the season's first time point. A space on the side facing the lines
    # separates them a little (ggplot2 has no absolute nudge for text).
    if (labels %in% c("left", "both")) {
      labels_left <- data[season_rows(FALSE), ]
      labels_left[[idx]] <- mixtime::time_floor(labels_left[[idx]], period) + 0

      p <- p +
        labeller(
          aes(label = paste0(!!sym("id"), " ")),
          data = labels_left,
          hjust = "outward",
          nudge_x = labels_left_nudge
        )
    }
    if (labels %in% c("right", "both")) {
      labels_right <- data[season_rows(TRUE), ]
      # Just before the next season starts, so it isn't folded into the start
      # of the loop.
      labels_right[[idx]] <- mixtime::time_ceiling(labels_right[[idx]], period) +
        0 -
        1e-3

      p <- p +
        labeller(
          aes(label = paste0(" ", !!sym("id"))),
          data = labels_right,
          hjust = "outward",
          nudge_x = labels_right_nudge
        )
    }

    p <- p + ggplot2::guides(colour = "none")
  }

  p
}

# The space `gg_season()` reserves either side of the loop for its labels, as
# a function of the theme (giving the labels' font), measured when drawn.
season_label_padding <- function(labels, left, right) {
  function(theme) {
    geom <- ggplot2::calc_element("geom", theme)
    text <- grid::textGrob(
      paste0(labels, " "),
      gp = grid::gpar(
        fontsize = geom@fontsize * ggplot2::.pt,
        fontfamily = geom@family
      )
    )
    # A little extra room keeps the labels off the panel's edge.
    width <- grid::grobWidth(text) + grid::unit(0.5, "lines")
    none <- grid::unit(0, "pt")
    grid::unit.c(if (left) width else none, if (right) width else none)
  }
}

#' Seasonal subseries plots
#'
#' A seasonal subseries plot facets the time series by each season in the
#' seasonal period. These facets form smaller time series plots consisting of
#' data only from that season. If you had several years of monthly data, the
#' resulting plot would show a separate time series plot for each month. The
#' first subseries plot would consist of only data from January. This case is
#' given as an example below.
#'
#' The horizontal lines are used to represent the mean of each facet, allowing
#' easy identification of seasonal differences between seasons. This plot is
#' particularly useful in identifying changes in the seasonal pattern over time.
#'
#' similar to a seasonal plot ([`gg_season()`]), and
#'
#' @inheritParams gg_season
#' @param period The seasonal period to facet by. If NULL (default), the
#' smallest common seasonal period of the data is used (e.g. a week for daily
#' data, a day for hourly data). Otherwise, a string or duration as described
#' in [gg_season()].
#'
#' @return A ggplot object showing a seasonal subseries plot of a time series.
#'
#' @references
#' Hyndman and Athanasopoulos (2019) Forecasting: principles and practice,
#'  3rd edition, OTexts: Melbourne, Australia. https://OTexts.com/fpp3/
#'
#' @examplesIf requireNamespace("tsibbledata", quietly = TRUE)
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_retail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   gg_subseries(Turnover)
#'
#' @importFrom ggplot2 facet_grid
#' @export
gg_subseries <- function(data, y = NULL, period = NULL, ...) {
  # Guide users from {feasts} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "feasts", "0.4.2")

  y <- guess_plot_var(data, !!enquo(y))
  keys <- key(data)
  check_gaps(data)
  idx <- index_var(data)
  ts_interval <- interval_as_granule(interval(data))

  # Each facet is a season position, so a smaller period gives fewer facets
  # with more observations in each
  period <- season_period(period, ts_interval, auto = "smallest")

  data <- as_tibble(data)
  y_lab <- as_label(y)
  data <- eval_plot_var(data, y)
  y <- sym(".y")
  data[[idx]] <- as_mixtime_index(data[[idx]])
  data <- data %>%
    mutate(
      id = season_position_factor(!!sym(idx), period),
      .yint = !!y
    ) %>%
    group_by(!!sym("id"), !!!keys) %>%
    mutate(.yint = mean(!!sym(".yint"), na.rm = TRUE))

  # Break at the start of every few seasons, so each facet has only a handful
  n_seasons <- length(unique(mixtime::time_floor(data[[idx]], period)))
  break_step <- scale_granule(period, subseries_break_step(n_seasons))

  ggplot(data, aes(x = !!sym(idx), y = !!y)) +
    geom_line(...) +
    ggplot2::labs(y = y_lab) +
    facet_grid(
      rows = vars(!!!lapply(keys, function(x) expr(format(!!x)))),
      cols = vars(!!sym("id")),
      scales = "free_y"
    ) +
    ggplot2::geom_hline(aes(yintercept = !!sym(".yint")), colour = "blue") +
    scale_x_mixtime(
      time_breaks = break_step,
      labels = function(x) time_labels_at(x, chronon = period)
    ) +
    ggplot2::theme(axis.text.x.bottom = ggplot2::element_text(angle = 90))
}

# Evaluate the plot variable into `.y` while the index is still in its
# original class, so expressions of it (e.g. `days_in_month(Month)`) work
# before the index is converted with `as_mixtime_index()`.
eval_plot_var <- function(data, y) {
  data[[".y"]] <- eval_tidy(y, data)
  data
}

# The number of seasons between subseries axis breaks: a 1-2-5 step giving at
# most `max_breaks` breaks per facet.
subseries_break_step <- function(n_seasons, max_breaks = 3) {
  steps <- c(1, 2, 5) * rep(10^(0:5), each = 3)
  steps[which(n_seasons / steps <= max_breaks)[1]]
}


#' Lag plots
#'
#' A lag plot shows the time series against lags of itself. It is often coloured
#' the seasonal period to identify how each season correlates with others.
#'
#' @inheritParams gg_season
#' @param lags A vector of lags to display as facets.
#' @param geom The geometry used to display the data.
#' @param arrow Arrow specification to show the direction in the lag path. If
#' TRUE, an appropriate default arrow will be used. Alternatively, a user
#' controllable arrow created with [`grid::arrow()`] can be used.
#' @param ... Additional arguments passed to the geom.
#'
#' @return A ggplot object showing a lag plot of a time series.
#'
#' @examplesIf requireNamespace("tsibbledata", quietly = TRUE)
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_retail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   gg_lag(Turnover)
#'
#' @importFrom ggplot2 ggplot aes geom_path geom_abline facet_wrap
#' @importFrom tidyr gather
#' @export
gg_lag <- function(
  data,
  y = NULL,
  period = NULL,
  lags = 1:9,
  geom = c("path", "point"),
  arrow = FALSE,
  ...
) {
  # Guide users from {feasts} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "feasts", "0.4.2")

  if (isTRUE(arrow)) {
    arrow <- grid::arrow(length = grid::unit(0.05, "npc"))
  } else if (isFALSE(arrow)) {
    arrow <- NULL
  }

  y <- guess_plot_var(data, !!enquo(y))
  geom <- match.arg(geom)
  lag_geom <- switch(geom, path = geom_path, point = function(..., arrow) {
    geom_point(...)
  })

  if (n_keys(data) > 1) {
    abort(
      "The data provided to contains more than one time series. Please filter a single time series to use `gg_lag()`"
    )
  }

  idx <- index_var(data)
  ts_interval <- interval_as_granule(interval(data))
  # Seasons are coloured only for periods with a fixed number of observations,
  # so that each lag is the same seasonal offset throughout the series
  period <- if (is.null(period)) {
    default_period_granule(ts_interval, "smallest", fixed = TRUE)
  } else {
    season_period(period, ts_interval)
  }
  if (is.null(period)) {
    # Non-seasonal data (e.g. yearly or weekly) isn't coloured by season
    frequency <- 1
  } else {
    frequency <- observations_per_period(ts_interval, period)
    if (is.na(frequency)) {
      abort(
        "The seasonal period must contain a fixed number of observations (e.g. not days in a month)."
      )
    }
  }

  lag_exprs <- map(lags, function(lag) expr(lag(!!y, !!lag))) %>%
    set_names(paste0(".lag_", lags))

  data <- data %>%
    as_tibble %>%
    mutate(
      season = if (!is.null(period)) {
        season_position_factor(as_mixtime_index(!!sym(idx)), period)
      },
      !!!lag_exprs
    )

  num_na <- eval_tidy(expr(sum(is.na(!!y))), data = data)
  if (num_na > 0) {
    warn(sprintf("Removed %i rows containing missing values (gg_lag).", num_na))
  }

  data <- data %>%
    gather(".lag", ".value", !!names(lag_exprs)) %>%
    mutate(
      .lag = factor(
        !!sym(".lag"),
        levels = names(lag_exprs),
        labels = paste("lag", lags)
      )
    ) %>%
    filter(!is.na(!!sym(".value")) & !is.na(!!y))

  mapping <- aes(x = !!sym(".value"), y = !!y)
  if (!isTRUE(frequency <= 1)) {
    mapping$colour <- sym("season")
  }

  data %>%
    ggplot(mapping) +
    geom_abline(colour = "gray", linetype = "dashed") +
    lag_geom(..., arrow = arrow) +
    facet_wrap(~.lag) +
    ggplot2::theme(aspect.ratio = 1) +
    xlab(paste0("lag(", as_string(y), ", n)"))
}

#' Ensemble of time series displays
#'
#' Plots a time series along with its ACF along with an customisable third
#' graphic of either a PACF, histogram, lagged scatterplot or spectral density.
#'
#' @param plot_type type of plot to include in lower right corner. By default
#' (`"auto"`) a season plot will be shown for seasonal data, a spectrum plot
#' will be shown for non-seasonal data without missing values, and a PACF will
#' be shown otherwise.
#' @inheritParams gg_season
#' @inheritParams feasts::ACF
#'
#' @return A list of ggplot objects showing useful plots of a time series.
#'
#' @author Rob J Hyndman & Mitchell O'Hara-Wild
#'
#' @seealso \code{\link[stats]{plot.ts}}, [feasts::ACF()],
#' \code{\link[stats]{spec.ar}}
#'
#' @references Hyndman and Athanasopoulos (2019) \emph{Forecasting: principles
#' and practice}, 3rd edition, OTexts: Melbourne, Australia.
#' \url{https://OTexts.com/fpp3/}
#'
#' @examplesIf requireNamespace("tsibbledata", quietly = TRUE)
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_retail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   gg_tsdisplay(Turnover)
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_histogram ylim
#' @importFrom stats na.exclude complete.cases
#' @export
gg_tsdisplay <- function(
  data,
  y = NULL,
  plot_type = c(
    "auto",
    "partial",
    "season",
    "histogram",
    "scatter",
    "spectrum"
  ),
  lag_max = NULL
) {
  # Guide users from {feasts} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "feasts", "0.4.2")

  if (n_keys(data) > 1) {
    abort(
      "The data provided to contains more than one time series. Please filter a single time series to use `gg_tsdisplay()`"
    )
  }
  rlang::check_installed("grid")
  rlang::check_installed("feasts")

  y <- guess_plot_var(data, !!enquo(y))

  plot_type <- match.arg(plot_type)
  if (plot_type == "auto") {
    if (!interval_is_seasonal(interval_as_granule(interval(data)))) {
      plot_type <- if (any(is.na(eval_tidy(y, data = data)))) {
        "partial"
      } else {
        "spectrum"
      }
    } else {
      plot_type <- "season"
    }
  }

  # Set up grid for plots
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 2)))

  p1 <- ggplot(data, aes(x = !!index(data), y = !!y)) +
    geom_line() +
    geom_point()

  p2 <- autoplot(feasts::ACF(data, !!y, lag_max = lag_max))

  if (plot_type == "partial") {
    p3 <- autoplot(feasts::PACF(data, !!y, lag_max = lag_max))

    # Match y-axis range across ACF and PACF
    p2_yrange <- ggplot2::layer_scales(p2)$y$range$range
    p3_yrange <- ggplot2::layer_scales(p3)$y$range$range
    yrange <- range(c(p2_yrange, p3_yrange))
    p2 <- p2 + ylim(yrange)
    p3 <- p3 + ylim(yrange)
  } else if (plot_type == "season") {
    p3 <- gg_season(data, !!y)
  } else if (plot_type == "histogram") {
    p3 <- ggplot(data, aes(x = !!y)) +
      geom_histogram(
        bins = min(500, grDevices::nclass.FD(na.exclude(data[[as_name(y)]])))
      ) +
      ggplot2::geom_rug()
  } else if (plot_type == "scatter") {
    p3 <- data %>%
      mutate(!!paste0(as_name(y), "_lag") := lag(!!y, 1)) %>%
      ggplot(aes(y = !!y, x = !!sym(paste0(as_name(y), "_lag")))) +
      geom_point() +
      xlab(expression(Y[t - 1])) +
      ylab(expression(Y[t]))
  } else if (plot_type == "spectrum") {
    spec <- safely(stats::spec.ar)(eval_tidy(y, data), plot = FALSE)

    p3 <- if (is.null(spec[["result"]])) {
      if (spec$error$message == "missing values in object") {
        warn(
          "Spectrum plot could not be shown as the data contains missing values. Consider using a different `plot_type`."
        )
      } else {
        warn(sprintf(
          "Spectrum plot could not be shown as an error occurred: %s",
          spec$error$message
        ))
      }
      ggplot() + ggplot2::labs(x = "frequency", y = "spectrum")
    } else {
      tibble(
        spectrum = drop(spec[["result"]]$spec),
        frequency = spec[["result"]]$freq
      ) %>%
        ggplot(aes(x = !!sym("frequency"), y = !!sym("spectrum"))) +
        geom_line() +
        ggplot2::scale_y_log10()
    }
  }

  structure(list(p1, p2, p3), class = c("gg_tsensemble", "gg"))
}

#' Ensemble of time series residual diagnostic plots
#'
#' Plots the residuals using a time series plot, ACF and histogram.
#'
#' @param data A mable containing one model with residuals.
#' @param ... Additional arguments passed to [`gg_tsdisplay()`].
#' @inheritParams fabletools::residuals.mdl_ts
#' @inheritParams gg_tsdisplay
#'
#'
#' @return A list of ggplot objects showing a useful plots of a time series model's residuals.
#'
#' @seealso [`gg_tsdisplay()`]
#'
#' @references Hyndman and Athanasopoulos (2019) \emph{Forecasting: principles
#' and practice}, 3rd edition, OTexts: Melbourne, Australia.
#' \url{https://OTexts.com/fpp3/}
#'
#' @examplesIf requireNamespace("fable", quietly = TRUE) && requireNamespace("tsibbledata", quietly = TRUE)
#' library(fable)
#'
#' tsibbledata::aus_production %>%
#'   model(ETS(Beer)) %>%
#'   gg_tsresiduals()
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_histogram ylim
#' @importFrom stats na.exclude complete.cases
#' @export
gg_tsresiduals <- function(
  data,
  type = "innovation",
  plot_type = "histogram",
  ...
) {
  # Guide users from {feasts} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "feasts", "0.4.2")

  if (!fabletools::is_mable(data)) {
    abort(
      "gg_tsresiduals() must be used with a mable containing only one model."
    )
  }

  data <- stats::residuals(data, type = type)
  if (n_keys(data) > 1) {
    abort(
      "gg_tsresiduals() must be used with a mable containing only one model."
    )
  }

  out <- gg_tsdisplay(data, !!sym(".resid"), plot_type = plot_type, ...)
  out[[1]] <- out[[1]] +
    ggplot2::ylab(sub(
      "([[:alpha:]])(.+)",
      "\\U\\1\\L\\2 residuals",
      type,
      perl = TRUE
    ))
  out
}

#' @export
`+.gg_tsensemble` <- function(e1, e2) {
  e1[[1]] <- e1[[1]] + e2
  e1
}

# Registered as the `chooseOpsMethod()` method for `gg_tsensemble` by
# `.onLoad()`, since the generic only exists from R 4.3.0.
# Named with an underscore rather than a dot to avoid roxygen S3 method noise.
chooseOpsMethod_gg_tsensemble <- function(x, y, mx, my, cl, reverse) {
  # Always use the gg_tsensemble method
  TRUE
}

#' @export
print.gg_tsensemble <- function(x, ...) {
  x <- lapply(x, ggplot2::ggplotGrob)

  gt <- gtable::gtable(
    name = "tsensemble",
    heights = grid::unit(rep(1, 2), "null"),
    widths = grid::unit(rep(1, 2), "null")
  )
  gt <- gtable::gtable_add_grob(
    gt,
    x,
    t = c(1, 2, 2),
    b = c(1, 2, 2),
    l = c(1, 1, 2),
    r = c(2, 1, 2),
    z = seq_along(x),
    clip = "off"
  )
  grid.draw(gt)
}

#' @importFrom grid grid.draw
#' @method grid.draw gg_tsensemble
#' @export
grid.draw.gg_tsensemble <- function(x, recording = TRUE) {
  print(x)
}

#' Plot characteristic ARMA roots
#'
#' Produces a plot of the inverse AR and MA roots of an ARIMA model.
#' Inverse roots outside the unit circle are shown in red.
#'
#' Only models which compute ARMA roots can be visualised with this function.
#' That is to say, the `glance()` of the model contains `ar_roots` and `ma_roots`.
#'
#' @param data A mable containing models with AR and/or MA roots.
#'
#' @return A ggplot object the characteristic roots from ARMA components.
#'
#' @examplesIf requireNamespace("fable", quietly = TRUE) && requireNamespace("tsibbledata", quietly = TRUE)
#' library(fable)
#' library(tsibble)
#' library(dplyr)
#'
#' tsibbledata::aus_retail %>%
#'   filter(
#'     State == "Victoria",
#'     Industry == "Cafes, restaurants and catering services"
#'   ) %>%
#'   model(ARIMA(Turnover ~ pdq(0,1,1) + PDQ(0,1,1))) %>%
#'   gg_arma()
#' @export
gg_arma <- function(data) {
  # Guide users from {feasts} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "feasts", "0.4.2")

  if (!fabletools::is_mable(data)) {
    abort(
      "gg_arma() must be used with a mable containing models that compute ARMA roots"
    )
  }

  fcts <- c(key(data), sym(".model"))

  data <- data %>%
    fabletools::glance() %>%
    gather("type", "root", !!sym("ar_roots"), !!sym("ma_roots")) %>%
    unnest_tbl("root") %>%
    filter(!is.na(!!sym("root"))) %>%
    mutate(
      type = factor(
        !!sym("type"),
        levels = c("ar_roots", "ma_roots"),
        labels = c("AR roots", "MA roots")
      ),
      UnitCircle = factor(
        abs(1 / !!sym("root")) > 1,
        levels = c(TRUE, FALSE),
        labels = c("Outside", "Within")
      )
    )

  ggplot(
    data,
    aes(
      x = Re(1 / !!sym("root")),
      y = Im(1 / !!sym("root")),
      colour = !!sym("UnitCircle")
    )
  ) +
    ggplot2::annotate(
      "path",
      x = cos(seq(0, 2 * pi, length.out = 100)),
      y = sin(seq(0, 2 * pi, length.out = 100))
    ) +
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_hline(yintercept = 0) +
    geom_point() +
    ggplot2::coord_fixed(ratio = 1) +
    facet_grid(vars(!!!fcts), vars(!!sym("type")))
}

#' Plot impulse response functions
#'
#' Produces a plot of impulse responses from an impulse response function.
#'
#' @param data A tsibble with impulse responses
#' @param y The impulse response variables to plot (defaults to all measured variables).
#'
#' @return A ggplot object of the impulse responses.
#'
#' @export
gg_irf <- function(data, y = all_of(measured_vars(data))) {
  # Guide users from {feasts} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "feasts", "0.4.2")

  kv <- key_vars(data)
  if (is_empty(kv)) {
    kv <- NULL
  }
  data <- tidyr::pivot_longer(
    data,
    {{ y }},
    names_to = ".variable",
    values_to = ".response"
  )
  ggplot(data) +
    geom_line(ggplot2::aes_string(x = index_var(data), y = ".response")) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(vars(!!!syms(kv)), vars(!!sym(".variable"))) +
    ggplot2::labs(y = "Impulse response", x = NULL)
}

#' Auto- and Cross- Covariance and -Correlation plots
#'
#' Produces an appropriate plot for the result of  [`feasts::ACF()`], [`feasts::PACF()`], or [`feasts::CCF()`].
#'
#' @param object A tbl_cf object (the result [`feasts::ACF()`], [`feasts::PACF()`], or [`feasts::CCF()`]).
#' @param level The level of confidence for the blue dashed lines.
#' @param ... Unused.
#'
#' @return A ggplot object showing the correlations.
#'
#' @importFrom ggplot2 ggplot geom_hline xlab ylab ggtitle vars
#' @importFrom stats qnorm
#' @export
autoplot.tbl_cf <- function(object, level = 95, ...) {
  # Guide users from {feasts} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "feasts", "0.4.2")

  cf_type <- colnames(object)[colnames(object) %in% c("acf", "pacf", "ccf")]
  plot_aes <- ggplot2::aes(
    x = !!sym("lag"),
    y = !!sym(cf_type)
  )
  interval <- interval(object)
  num_obs <- object %@% "num_obs"
  keys <- key(object)
  n_key <- n_keys(object)

  # Plot lags as mixtime durations (e.g. "12 months") rather than a count.
  # Lags may be a `cf_lag`, plain numbers, or already mixtime durations.
  object <- as_tibble(object)
  object[["lag"]] <- lag_to_duration(object[["lag"]], interval)

  if (length(level) > 1) {
    abort(
      "Only one confidence interval is currently supported for this autoplot."
    )
  }

  p <- ggplot(object, plot_aes) +
    geom_segment(aes(xend = !!sym("lag"), yend = 0)) +
    geom_hline(yintercept = 0) +
    xlab(paste0("lag [", format_interval(interval), "]"))

  # An explicit scale, as mixtime's default scale lookup requires ggtime to be
  # attached rather than only loaded (e.g. when called via `feasts::`). Lags
  # are labelled as plain counts, since their unit is in the axis title.
  if (mixtime::is_mixtime(object[["lag"]])) {
    p <- p +
      scale_x_mixtime(
        labels = function(x) as.character(vctrs::vec_data(x))
      )
  }

  if (!is.null(level)) {
    conf_int <- num_obs %>%
      mutate(
        upper = qnorm((1 + (level / 100)) / 2) / sqrt(!!sym(".len")),
        lower = !!parse_expr("-upper")
      ) %>%
      gather("type", ".ci", !!!syms(c("upper", "lower")))
    p <- p +
      geom_hline(
        aes(yintercept = !!sym(".ci"), group = !!sym("type")),
        data = conf_int,
        colour = "blue",
        linetype = "dashed"
      )
  }

  if (n_key > 1) {
    p <- p + facet_grid(rows = vars(!!!keys))
  }

  p
}

#' @importFrom ggplot2 scale_type
#' @export
scale_type.cf_lag <- function(x) c("cf_lag", "continuous")

#' lagged datetime scales
#' This set of scales defines new scales for lagged time structures.
#'
#' @param ... Further arguments to be passed on to scale_x_continuous()
#'
#' @return A ggproto object inheriting from `Scale`
#'
#' @keywords internal
#' @name scale_cf_lag
#' @rdname scale_cf_lag
#' @importFrom fabletools get_frequencies
#' @export
scale_x_cf_lag <- function(...) {
  scale <- ggplot2::ggproto(
    "ScaleContinuousLag",
    ggplot2::scale_x_continuous(...),
    train = function(self, x) {
      if (length(x) == 0) {
        return()
      }
      self$range$train(vctrs::vec_data(x))
      if (!is.null(gap <- attr(x, "interval"))) {
        self$interval <- gap
        freq <- get_frequencies(NULL, gap, .auto = "all")
        self$frequency <- min(freq[freq > 3] %0% NA)
      }
    },
    get_breaks = function(self, limits = self$get_limits()) {
      if (inherits(self$breaks, "waiver") && !is.na(self$frequency)) {
        freq <- self$frequency
        lags <- ceiling(limits[1]):floor(limits[2])
        # nlags <- length(lags)
        # np <- nlags / freq
        self$breaks <- lags[lags %% (freq / 2) == 0]
      }
      ggplot2::ggproto_parent(ggplot2::ScaleContinuous, self)$get_breaks()
    },
    get_labels = function(self, breaks = self$get_breaks()) {
      # if(inherits(self$labels, "waiver")){
      #   interval_type <- map_lgl(self$interval, ~.x!=0)
      #   self$labels <- breaks
      #   #suffix <- toupper(names(self$interval)[interval_type])
      #   #self$labels <- paste(breaks*as.numeric(self$interval[interval_type]), paste0(suffix, ifelse(breaks!=0, "S", "")))
      # }
      ggplot2::ggproto_parent(ggplot2::ScaleContinuous, self)$get_labels()
    }
  )
  scale
}
