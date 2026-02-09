#' Plot time series from a tsibble
#'
#' Produces a time series plot of one or more variables from a tsibble. If the
#' tsibble contains a multiple keys, separate time series will be identified by
#' colour.
#'
#' @param object A tsibble.
#' @param .vars A bare expression containing data you wish to plot. Multiple variables can be plotted using [`ggplot2::vars()`].
#' @param ... Further arguments passed to [`ggplot2::geom_line()`], which can be used to specify fixed aesthetics such as `colour = "red"` or `size = 3`.
#'
#' @return A ggplot object showing a time plot of a time series.
#'
#' @examplesIf requireNamespace("tsibbledata", quietly = TRUE)
#' tsibbledata::gafa_stock %>%
#'  autoplot(vars(Close, log(Close)))
#'
#' @importFrom ggplot2 ggplot aes geom_line guides guide_legend xlab
#' @export
autoplot.tbl_ts <- function(object, .vars = NULL, ...) {
  # Guide users from {fabletools} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "fabletools", "0.6.0")

  quo_vars <- enquo(.vars)

  kv <- key_vars(object)
  nk <- n_keys(object)

  if (quo_is_null(quo_vars)) {
    mv <- measured_vars(object)
    pos <- which(vapply(object[mv], is.numeric, logical(1L)))
    if (is_empty(pos)) {
      abort(
        "Could not automatically identify an appropriate plot variable, please specify the variable to plot."
      )
    }
    inform(sprintf(
      "Plot variable not specified, automatically selected `.vars = %s`",
      mv[pos[1]]
    ))
    y <- sym(mv[pos[1]])
    .vars <- as_quosures(list(y), env = empty_env())
  } else if (possibly(compose(is_quosures, eval_tidy), FALSE)(.vars)) {
    .vars <- eval_tidy(.vars)
    object <- gather(
      mutate(object, !!!.vars),
      ".response",
      "value",
      !!!map(.vars, quo_name),
      factor_key = TRUE
    )
    y <- sym("value")
  } else {
    y <- quo_vars
    .vars <- list(y)
  }

  aes_spec <- list(x = index(object), y = y)

  if (nk > 1) {
    object <- dplyr::mutate_if(
      object,
      ~ inherits(., "agg_vec"),
      compose(trimws, format)
    )
    aes_spec["colour"] <- list(expr(interaction(!!!syms(kv), sep = "/")))
  }

  p <- ggplot(object, eval_tidy(expr(aes(!!!aes_spec)))) +
    geom_line(...) +
    xlab(paste0(index_var(object), " [", format(interval(object)), "]"))

  if (nk > 1) {
    p <- p +
      guides(colour = guide_legend(paste0(kv, collapse = "/")))
  }

  if (length(.vars) > 1) {
    p <- p +
      facet_wrap(
        vars(!!sym(".response")),
        scales = "free_y",
        ncol = length(.vars)
      ) +
      ggplot2::ylab(NULL)
  }

  p
}

#' @rdname autoplot.tbl_ts
#' @importFrom ggplot2 ggplot aes geom_line guides guide_legend xlab
#' @export
autolayer.tbl_ts <- function(object, .vars = NULL, ...) {
  # Guide users from {fabletools} to {ggtime}
  ggtime_migrate_deprecate(match.call(), "fabletools", "0.6.0")

  quo_vars <- enquo(.vars)
  kv <- key_vars(object)
  nk <- n_keys(object)

  if (quo_is_null(quo_vars)) {
    mv <- measured_vars(object)
    pos <- which(vapply(object[mv], is.numeric, logical(1L)))
    if (is_empty(pos)) {
      abort(
        "Could not automatically identify an appropriate plot variable, please specify the variable to plot."
      )
    }
    inform(sprintf(
      "Plot variable not specified, automatically selected `.vars = %s`",
      mv[pos[1]]
    ))
    y <- sym(mv[pos[1]])
    .vars <- as_quosures(list(y), env = empty_env())
  } else if (possibly(compose(is_quosures, eval_tidy), FALSE)(.vars)) {
    .vars <- eval_tidy(.vars)
    object <- gather(
      mutate(object, !!!.vars),
      ".response",
      "value",
      !!!map(.vars, quo_name),
      factor_key = TRUE
    )
    y <- sym("value")
  } else {
    y <- quo_vars
    .vars <- list(y)
  }

  aes_spec <- list(x = index(object), y = y)

  if (nk > 1) {
    aes_spec["colour"] <- list(expr(interaction(!!!syms(kv), sep = "/")))
  }
  if (n_keys(object) > 1) {
    aes_spec["group"] <- list(expr(interaction(
      !!!syms(key_vars(object)),
      sep = "/"
    )))
  }

  geom_line(
    eval_tidy(expr(aes(!!!aes_spec))),
    data = object,
    ...,
    inherit.aes = FALSE
  )
}
