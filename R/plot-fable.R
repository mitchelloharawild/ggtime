#' @importFrom ggplot2 fortify
#' @export
fortify.fbl_ts <- function(model, data = NULL, level = c(80, 95), ...) {
  return(as_tibble(model))
}

#' Plot a set of forecasts
#'
#' Produces a forecast plot from a fable. As the original data is not included
#' in the fable object, it will need to be specified via the `data` argument.
#' The `data` argument can be used to specify a shorter period of data, which is
#' useful to focus on the more recent observations.
#'
#' @param object A fable.
#' @param data A tsibble with the same key structure as the fable.
#' @param level The confidence level(s) for the plotted intervals.
#' @param show_gap Setting this to `FALSE` will connect the most recent value in `data` with the forecasts.
#' @param ... Further arguments passed used to specify fixed aesthetics for the forecasts such as `colour = "red"` or `linewidth = 3`.
#' @param point_forecast The point forecast measure to be displayed in the plot.
#'
#' @examplesIf requireNamespace("fable", quietly = TRUE) && requireNamespace("tsibbledata", quietly = TRUE)
#' library(fable)
#' library(tsibbledata)
#'
#' fc <- aus_production %>%
#'   model(ets = ETS(log(Beer) ~ error("M") + trend("Ad") + season("A"))) %>%
#'   forecast(h = "3 years")
#'
#' fc %>%
#'   autoplot(aus_production)
#'
#' @importFrom ggplot2 facet_wrap
#' @export
autoplot.fbl_ts <- function(
  object,
  data = NULL,
  level = c(80, 95),
  show_gap = TRUE,
  ...
) {
  # Guide users from {fabletools} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "fabletools", "0.6.0")

  fc_resp <- fabletools::response_vars(object)
  fc_key <- setdiff(key_vars(object), ".model")
  common_models <- duplicated(
    key_data(object)[[".model"]] %||% rep(TRUE, NROW(key_data(object)))
  )

  aes_y <- if (length(fc_resp) > 1) {
    sym("value")
  } else {
    sym(fc_resp)
  }

  # Structure input data to match fable
  if (!is.null(data)) {
    data <- as_tsibble(data)
    if (!identical(fc_key, key_vars(data))) {
      abort(
        "Provided data contains a different key structure to the forecasts."
      )
    }

    if (!is_empty(key(data))) {
      data <- dplyr::semi_join(data, object, by = key_vars(data))
    }

    if (length(fc_resp) > 1) {
      data <- gather(
        data,
        ".response",
        "value",
        !!!syms(fc_resp),
        factor_key = TRUE
      )
    }

    data <- data %>%
      dplyr::mutate_if(~ inherits(., "agg_vec"), compose(trimws, format))
  }

  # Compute facets, if any
  facet_vars <- if (length(fc_resp) > 1) ".response" else NULL
  if (any(common_models)) {
    facet_vars <- c(facet_vars, fc_key)
  }

  # Change colours to be more appropriate for later facets
  fc_layer <- build_fbl_layer(
    object,
    data = data,
    level = level,
    show_gap = show_gap,
    facet_vars = facet_vars,
    ...
  )

  # Add forecasts on base plot
  p <- ggplot(data, aes(x = !!index(object))) +
    fc_layer

  # Add historical data
  if (!is.null(data)) {
    hist_aes <- aes(y = !!aes_y)
    if (length(key_vars(data)) > 0) {
      hist_aes[["group"]] <- expr(interaction(!!!syms(key_vars(data))))
    }
    p <- p + geom_line(hist_aes)
  }

  # Add facets
  if (!is_empty(facet_vars)) {
    p <- p +
      facet_wrap(
        vars(!!!syms(facet_vars)),
        ncol = length(fc_resp),
        scales = "free_y"
      )
  }
  if (length(fc_resp) > 1) {
    p <- p + ggplot2::ylab(NULL)
  }
  p
}

#' @rdname autoplot.fbl_ts
#' @examplesIf requireNamespace("tsibbledata", quietly = TRUE)
#' aus_production %>%
#'   autoplot(Beer) +
#'   autolayer(fc)
#'
#' @export
autolayer.fbl_ts <- function(
  object,
  data = NULL,
  level = c(80, 95),
  point_forecast = list(mean = mean),
  show_gap = TRUE,
  ...
) {
  # Guide users from {fabletools} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "fabletools", "0.6.0")

  build_fbl_layer(
    object = object,
    data = data,
    level = level,
    point_forecast = point_forecast,
    show_gap = show_gap,
    ...
  )
}

build_fbl_layer <- function(
  object,
  data = NULL,
  level = c(80, 95),
  colour = NULL,
  color = NULL,
  fill = NULL,
  point_forecast = list(mean = mean),
  show_gap = TRUE,
  linetype = 1,
  ...,
  facet_vars = NULL
) {
  rlang::check_installed("distributional")
  rlang::check_installed("ggdist")
  mdl_key <- object %@% "model_cn"
  fc_key <- setdiff(key_vars(object), mdl_key)
  key_data <- key_data(object)
  key_vars <- key_vars(object)
  resp_var <- fabletools::response_vars(object)
  dist_var <- fabletools::distribution_var(object)
  idx <- index(object)
  common_models <- duplicated(
    key_data[[mdl_key]] %||% rep(TRUE, NROW(key_data(object)))
  )
  colour <- colour %||% color %||% fill %||% "#446ffc"
  without <- function(x, el) x[setdiff(names(x), el)]

  if (isFALSE(level)) {
    warn(
      "Plot argument `level` should be a numeric vector of levels to display. Setting `level = NULL` will remove the intervals from the plot."
    )
    level <- NULL
  }

  if (!show_gap && is.null(data)) {
    warn(
      "Could not connect forecasts to last observation as `data` was not provided. Setting `show_gap = FALSE`."
    )
  }
  if (!show_gap) {
    gap <- key_data
    gap[ncol(gap)] <- NULL
    last_obs <- filter(group_by_key(data), !!idx == max(!!idx))
    if (length(key_vars(last_obs)) == 0) {
      gap[names(last_obs)] <- last_obs
    } else {
      gap <- dplyr::left_join(gap, last_obs, by = key_vars(last_obs))
    }
    if (length(resp_var) > 1) {
      abort(
        "`show_gap = FALSE` is not yet supported for multivariate forecasts."
      )
    }
    gap[[dist_var]] <- distributional::dist_degenerate(gap[[resp_var]])
    dimnames(gap[[dist_var]]) <- resp_var
    gap <- fabletools::as_fable(
      gap,
      index = !!idx,
      key = key_vars(object),
      response = resp_var,
      distribution = dist_var
    )
    object <- dplyr::bind_rows(gap, object)
  }

  if (length(resp_var) > 1) {
    resp <- sym("value")
    grp <- syms(".response")
  } else {
    resp <- sym(resp_var)
    grp <- NULL
  }

  mapping <- aes(
    x = !!idx,
  )

  useful_keys <- fc_key[map_lgl(key_data[fc_key], function(x) {
    sum(!duplicated(x)) > 1
  })]
  col <- c(
    if (any(common_models)) useful_keys else NULL,
    if (sum(!common_models) > 1) ".model" else NULL
  )
  col <- setdiff(col, facet_vars)
  if (!is_empty(col)) {
    col_nm <- paste0(col, collapse = "/")
    col <- if (length(col) == 1) {
      sym(col)
    } else {
      expr(interaction(!!!syms(col), sep = "/"))
    }
  } else {
    col <- NULL
  }

  grp <- c(grp, syms(key_vars(object)))
  if (length(grp) > 0) {
    mapping$group <- expr(interaction(
      !!!map(grp, function(x) expr(format(!!x))),
      sep = "/"
    ))
  }

  # Single time point forecasts
  kd <- key_data(object)
  single_row <- lapply(split(kd$.rows, lengths(kd$.rows) == 1), unlist)

  out <- list()
  object <- object %>%
    dplyr::mutate_if(~ inherits(., "agg_vec"), compose(trimws, format))

  # Adapted from ggdist:::draw_key_lineribbon
  draw_key_ribbon <- function(self, data, params, size) {
    data$alpha <- data$alpha %||% NA
    if (
      is.null(data[["fill"]]) &&
        (!is.null(data[["fill_ramp"]]) || !all(is.na(data[["alpha"]])))
    ) {
      data$fill = "gray65"
    }
    if (
      is.null(data[["colour"]]) &&
        (!is.null(data[["colour_ramp"]]) || !all(is.na(data[["alpha"]])))
    ) {
      data$colour = "black"
    }
    # Apply ramped fill
    if (!is.null(data[["fill_ramp"]])) {
      if (utils::packageVersion("ggdist") > "3.3.1") {
        data$fill <- get(
          "ramp_colours",
          asNamespace("ggdist"),
          mode = "function"
        )(data$fill, data$fill_ramp)
      } else {
        data$fill <- mapply(
          function(color, amount) {
            (scales::seq_gradient_pal(
              attr(amount, "from") %||% "white",
              color
            ))(amount %||% NA)
          },
          data$fill,
          data$fill_ramp
        )
      }
    }
    # Apply ramped colour
    if (!is.null(data[["colour_ramp"]])) {
      if (utils::packageVersion("ggdist") > "3.3.1") {
        data$colour <- get(
          "ramp_colours",
          asNamespace("ggdist"),
          mode = "function"
        )(data$colour, data$colour_ramp)
      } else {
        data$colour <- mapply(
          function(color, amount) {
            (scales::seq_gradient_pal(
              attr(amount, "from") %||% "white",
              color
            ))(amount %||% NA)
          },
          data$colour,
          data$colour_ramp
        )
      }
    }
    ggplot2::draw_key_rect(data, params, size)
  }

  # Add forecast interval ribbons to plot
  if (!is.null(level)) {
    intvl_mapping <- mapping
    # intvl_mapping$dist <- sym(dist_var)
    intvl_mapping$ymin <- sym(".lower")
    intvl_mapping$ymax <- sym(".upper")
    intvl_mapping$fill_ramp <- intvl_mapping$colour_ramp <- sym(".width")
    intvl_mapping$fill <- intvl_mapping$colour <- col

    qi_marginal <- function(x, .width = 0.95, na.rm = FALSE) {
      if (!na.rm && anyNA(x)) {
        return(matrix(c(NA_real_, NA_real_), ncol = 2))
      }

      if (utils::packageVersion("distributional") > "0.4.0") {
        do.call(
          rbind,
          lapply(
            stats::quantile(
              x,
              (1 + c(-1, 1) * .width) / 2,
              type = "marginal",
              na.rm = na.rm
            ),
            t
          )
        )
      } else {
        do.call(
          rbind,
          lapply(
            stats::quantile(x, (1 + c(-1, 1) * .width) / 2, na.rm = na.rm),
            t
          )
        )
      }
    }

    dist_qi_frame <- function(data, level) {
      data <- ggdist::point_interval(
        as_tibble(data),
        !!sym(dist_var),
        .interval = qi_marginal,
        .width = level / 100
      )
      names(data)[match(".index", names(data))] <- ".response"
      data
    }

    if (!is.null(col)) {
      if (length(single_row[["FALSE"]]) > 0) {
        out[[length(out) + 1L]] <- ggdist::geom_lineribbon(
          without(intvl_mapping, "colour_ramp"),
          data = dist_qi_frame(object[single_row[["FALSE"]], ], level),
          ...,
          inherit.aes = FALSE,
          key_glyph = draw_key_ribbon
        )
      }
      if (length(single_row[["TRUE"]]) > 0) {
        out[[length(out) + 1L]] <- ggdist::geom_interval(
          intvl_mapping,
          data = dist_qi_frame(object[single_row[["TRUE"]], ], level),
          orientation = "vertical",
          ...,
          inherit.aes = FALSE,
          key_glyph = draw_key_ribbon
        )
      }
      out[[length(out) + 1L]] <- ggplot2::labs(fill = col_nm)
    } else {
      if (length(single_row[["FALSE"]]) > 0) {
        out[[length(out) + 1L]] <- ggdist::geom_lineribbon(
          without(intvl_mapping, "colour_ramp"),
          data = dist_qi_frame(object[single_row[["FALSE"]], ], level),
          fill = colour,
          ...,
          inherit.aes = FALSE,
          key_glyph = draw_key_ribbon
        )
      }
      if (length(single_row[["TRUE"]]) > 0) {
        out[[length(out) + 1L]] <- ggdist::geom_interval(
          intvl_mapping,
          data = dist_qi_frame(object[single_row[["TRUE"]], ], level),
          colour = colour,
          orientation = "vertical",
          ...,
          inherit.aes = FALSE,
          key_glyph = draw_key_ribbon
        )
      }
    }

    # Add scale for confidence level ramp
    if (length(level) > 6) {
      level_breaks <- ggplot2::waiver()
      level_guide <- ggdist::guide_rampbar()
    } else {
      level_breaks <- level / 100
      level_guide <- "legend"
    }
    if (length(single_row[["FALSE"]]) > 0) {
      out[[length(out) + 1L]] <- ggdist::scale_fill_ramp_continuous(
        name = "level",
        from = "white",
        breaks = level_breaks,
        limits = function(l) range(l),
        range = c(0.7, 0.3),
        labels = function(x) scales::percent(as.numeric(x)),
        guide = level_guide
      )
    }
    if (length(single_row[["TRUE"]]) > 0) {
      out[[length(out) + 1L]] <- ggdist::scale_colour_ramp_continuous(
        name = "level",
        from = "white",
        breaks = level_breaks,
        limits = function(l) range(l),
        range = c(0.7, 0.3),
        labels = function(x) scales::percent(as.numeric(x)),
        guide = level_guide
      )
    }
  }

  # Calculate point forecasts
  object <- as_tibble(object)
  object[names(point_forecast)] <- map(
    point_forecast,
    function(f, ...) f(...),
    object[[dist_var]]
  )

  unpack_data <- function(x) {
    x <- tidyr::pivot_longer(
      x[-match(dist_var, names(x))],
      names(point_forecast),
      names_to = "Point forecast",
      values_to = dist_var
    )
    if (length(resp_var) > 1) {
      x[[dist_var]] <- as_tibble(x[[dist_var]])
      x <- x[setdiff(names(x), resp_var)] %>%
        tidyr::unpack(!!dist_var) %>%
        tidyr::pivot_longer(
          names(x[[dist_var]]),
          names_to = ".response",
          values_to = dist_var
        )
    }
    x
  }

  # Add point forecasts to plot
  mapping$y <- sym(dist_var)
  if (length(point_forecast) > 1) {
    mapping$linetype <- mapping$shape <- sym("Point forecast")
    grp <- c(grp, mapping$linetype)
    mapping$group <- expr(interaction(
      !!!map(grp, function(x) expr(format(!!x))),
      sep = "/"
    ))
  }
  object <- as_tibble(object)
  if (!is.null(col)) {
    mapping$colour <- col

    if (length(single_row[["FALSE"]]) > 0) {
      out[[length(out) + 1L]] <- geom_line(
        mapping = without(mapping, "shape"),
        data = unpack_data(object[single_row[["FALSE"]], ]),
        ...,
        inherit.aes = FALSE
      ) #, key_glyph = ggplot2::draw_key_timeseries)
    }
    if (length(single_row[["TRUE"]]) > 0) {
      out[[length(out) + 1L]] <- ggplot2::geom_point(
        mapping = without(mapping, "linetype"),
        data = unpack_data(object[single_row[["TRUE"]], ]),
        size = 3,
        ...,
        inherit.aes = FALSE
      )
    }
    out[[length(out) + 1L]] <- ggplot2::labs(colour = col_nm)
  } else {
    if (length(single_row[["FALSE"]]) > 0) {
      out[[length(out) + 1L]] <- geom_line(
        mapping = without(mapping, "shape"),
        data = unpack_data(object[single_row[["FALSE"]], ]),
        color = colour,
        ...,
        inherit.aes = FALSE
      ) #, key_glyph = ggplot2::draw_key_timeseries)
    }
    if (length(single_row[["TRUE"]]) > 0) {
      out[[length(out) + 1L]] <- ggplot2::geom_point(
        mapping = without(mapping, "linetype"),
        data = unpack_data(object[single_row[["TRUE"]], ]),
        color = colour,
        size = 3,
        ...,
        inherit.aes = FALSE
      )
    }
  }

  out
}
