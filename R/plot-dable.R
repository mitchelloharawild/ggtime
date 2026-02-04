#' Decomposition plots
#'
#' Produces a faceted plot of the components used to build the response
#' variable of the dable. Useful for visualising how the components contribute
#' in a decomposition or model.
#'
#' @param object A dable.
#' @param .vars The column of the dable used to plot. By default, this will be the response variable of the decomposition.
#' @param scale_bars If `TRUE`, each facet will include a scale bar which represents the same units across each facet.
#' @param level If the decomposition contains distributions, which levels should be used to display intervals?
#' @inheritParams autoplot.tbl_ts
#'
#' @return A ggplot object showing a set of time plots of the decomposition.
#'
#' @examplesIf requireNamespace("feasts", quietly = TRUE) && requireNamespace("tsibbledata", quietly = TRUE)
#' library(fabletools)
#' tsibbledata::aus_production %>%
#'   model(feasts::STL(Beer)) %>%
#'   components() %>%
#'   autoplot()
#'
#' @importFrom ggplot2 ggplot geom_line geom_rect facet_grid vars ylab labs
#' @export
autoplot.dcmp_ts <- function(
  object,
  .vars = NULL,
  scale_bars = TRUE,
  level = c(80, 95),
  ...
) {
  # Guide users from {fabletools} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "fabletools")

  method <- object %@% "method"
  idx <- index(object)
  keys <- key(object)
  n_keys <- n_keys(object)

  .vars <- enquo(.vars)
  if (quo_is_null(.vars)) {
    .vars <- sym(fabletools::response_vars(object))
  }
  dcmp_str <- dcmp <- (object %@% "aliases")[[expr_name(get_expr(.vars))]]
  if (!is.null(dcmp_str)) {
    dcmp_str <- expr_text(dcmp_str)
  }
  object <- dplyr::transmute(
    as_tsibble(object),
    !!.vars,
    !!!syms(all.vars(dcmp))
  )
  object <- tidyr::pivot_longer(
    object,
    measured_vars(object),
    values_to = ".val",
    names_to = ".var",
    names_transform = list(.var = ~ factor(., levels = unique(.)))
  )

  if (has_dist <- inherits(object[[".val"]], "distribution")) {
    rlang::check_installed("distributional")
    rlang::check_installed("ggdist")
    interval_data <- as_tibble(object)
    interval_data[paste0(level, "%")] <- lapply(
      level,
      distributional::hilo,
      x = interval_data[[".val"]]
    )
    interval_data <- tidyr::pivot_longer(
      interval_data,
      paste0(level, "%"),
      names_to = NULL,
      values_to = "hilo"
    )
    intvl_aes <- aes(
      x = !!idx,
      dist = !!sym(".val"),
      fill_ramp = after_stat(level)
    )
    line_aes <- aes(x = !!idx, y = mean(!!sym(".val")))
    if (n_keys > 1) {
      line_aes$colour <- intvl_aes$fill <- intvl_aes$group <- expr(interaction(
        !!!keys,
        sep = "/"
      ))
    }
    dcmp_geom <- list(
      if (n_keys > 1) {
        ggdist::stat_ribbon(intvl_aes, .width = level / 100, ...)
      } else {
        ggdist::stat_ribbon(
          intvl_aes,
          fill = "gray65",
          .width = level / 100,
          ...
        )
      },
      ggdist::scale_fill_ramp_discrete(
        from = "white",
        range = c(0.3, 0.7),
        labels = function(x) scales::percent(as.numeric(x))
      ),
      geom_line(line_aes, ...)
    )
  } else {
    line_aes <- aes(x = !!idx, y = !!sym(".val"))
    if (n_keys > 1) {
      line_aes$colour <- expr(interaction(!!!keys, sep = "/"))
    }
    dcmp_geom <- geom_line(line_aes, ...)
  }

  p <- object %>%
    ggplot() +
    dcmp_geom +
    facet_grid(vars(!!sym(".var")), scales = "free_y") +
    ylab(NULL) +
    labs(
      title = paste(method %||% "A", "decomposition"),
      subtitle = paste(
        c(expr_text(get_expr(.vars)), dcmp_str),
        collapse = " = "
      )
    )

  # Rangebars
  if (scale_bars) {
    xranges <- range(object[[expr_name(idx)]])
    barwidth <- pmax(1, round((1 / 64) * diff(as.double(xranges))))

    # Avoid issues with visible bindings
    ymin <- ymax <- center <- diff <- NULL

    min_fn <- if (has_dist) {
      function(x, ...) min(stats::quantile(x, (100 - max(level)) / 200), ...)
    } else {
      min
    }
    max_fn <- if (has_dist) {
      function(x, ...) max(stats::quantile(x, (100 + max(level)) / 200), ...)
    } else {
      max
    }

    range_data <- as_tibble(object) %>%
      group_by(!!sym(".var")) %>%
      summarise(
        ymin = min_fn(!!sym(".val"), na.rm = TRUE),
        ymax = max_fn(!!sym(".val"), na.rm = TRUE)
      ) %>%
      mutate(
        center = (ymin + ymax) / 2,
        diff = min(ymax - ymin),
        xmin = xranges[1] - barwidth * 2,
        xmax = xranges[1] - barwidth,
        ymin = center - diff / 2,
        ymax = center + diff / 2
      )

    p <- p +
      geom_rect(
        data = range_data,
        aes(
          ymin = !!sym("ymin"),
          ymax = !!sym("ymax"),
          xmin = !!sym("xmin"),
          xmax = !!sym("xmax")
        ),
        fill = "gray75",
        colour = "black",
        linewidth = 1 / 3
      )
  }

  if (n_keys > 1) {
    colour_title <- paste0(map_chr(keys, expr_name), collapse = "/")
    p <- p + labs(colour = colour_title)
    if (has_dist) {
      p <- p + labs(fill = colour_title)
    }
  }

  p
}
