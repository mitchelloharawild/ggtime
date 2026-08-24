# `day()`/`week()`/`quarter()` name granules in quosures, resolved per-panel
# by `calendar_resolve_granules()`. Declared to satisfy `R CMD check`.
utils::globalVariables(c("day", "week", "quarter"))

#' Calendar coordinates
#'
#' Arranges time series data into a calendar-like layout of rows and columns.
#' Data is cut into loops as in [coord_loop()], with each loop becoming its
#' own row and column of a grid rather than being overlaid.
#'
#' @inheritParams coord_loop
#'
#' @param cells Size of a calendar cell (see the Granule hierarchy section);
#'   governs the cell labels and the only gridline drawn along the time axis.
#'   * `NULL`: no cells, no gridlines, no cell labels
#'   * a granule or duration, e.g. `mixtime::days(1L)`
#'
#'   Defaults to `day(1L)`.
#' @param rows Size of a calendar row: `coord_loop()`'s `time_loops` under a
#'   calendar-specific name.
#'   * `NULL`: a single row spanning the whole column
#'   * a granule or duration, e.g. `mixtime::days(7L)`
#'
#'   Defaults to `week(1L)`, or `day(7L)` if the calendar has no `week`.
#' @param blocks Size of a calendar block: rows are grouped and marked with a
#'   thicker gridline where each block starts.
#'   * `NULL` (default): no blocks
#'   * a granule or duration, e.g. `mixtime::months(1L)`
#' @param panes Size of a calendar pane: rows are grouped and set apart by a
#'   gap rather than a rule. Must be coarser than `rows`, no coarser than
#'   `cols`.
#'   * `NULL`: no panes
#'   * a granule or duration, e.g. `mixtime::months(1L)`
#'
#'   Defaults to `month(1L)`, silently dropped if it doesn't fit.
#' @param cols Size of a calendar column, arranged left to right with no
#'   wrapping.
#'   * `NULL`: a single column spanning the whole time range
#'   * a granule or duration, e.g. `mixtime::quarters(1L)`
#'
#'   Defaults to `quarter(1L)`, or `month(3L)` if the calendar has no
#'   `quarter`.
#' @param pane_spacing,col_spacing Gap between panes of rows / between
#'   columns, as a fraction of one row's height / one column's width.
#' @param label_cells,label_rows,label_blocks,label_panes,label_cols How to
#'   label each instance of a granule:
#'   * a `mixtime` format string, as `time_labels` of [scale_x_mixtime()],
#'     e.g. `"{cyc(day, month)}"` (a bare `"{day}"` is not valid)
#'   * a function of the granule's times, returning a character vector
#'   * `NULL` (default except for `label_cells`/`label_panes`): no labels
#'
#'   Named by the time each instance starts, except a block or pane (which
#'   spans several rows), named by the time in the middle of the group.
#'
#' @details
#' Useful for visualizing long time spans with events over short intervals,
#' such as holidays. Cuts the time axis at every calendar boundary at once,
#' folds each piece into its row's window, and offsets it into its cell of
#' the grid. As with [coord_loop()], geometries crossing a boundary are cut,
#' justified per `align_discrete` (see [scale_x_mixtime()]).
#'
#' @section Granule arguments:
#'
#' `cells`/`rows`/`blocks`/`panes`/`cols` each accept:
#' * `NULL`
#' * a duration, e.g. `mixtime::days(1L)`
#' * a time granule, e.g. `mixtime::cal_gregorian$month(1L)`
#' * a bare expression, e.g. `day(1L)` or `month(1L)`, naming a granule of
#'   whichever calendar the time axis resolves to (from the scale's common
#'   chronon, or the Gregorian calendar for a plain `Date`/`POSIXct` axis)
#'
#' @section Granule hierarchy:
#'
#' Granules sit in a strict hierarchy: `col`, `pane`, `block`, `row`, `cell`.
#' A granule never straddles a boundary of anything above it; a row cut
#' short by a coarser boundary is left blank for the rest of its width, as
#' on a printed calendar.
#'
#' @section Breaks and labels:
#'
#' The time axis is broken at every `cells` boundary and labelled as a
#' position within the `rows` cycle (e.g. "Mon", "Tue", ...), unless
#' `breaks`/`labels`/`time_breaks`/`time_labels` are set explicitly (see
#' [scale_x_mixtime()]). Falls back to the scale's own breaks when `cells`
#' or `rows` is `NULL`, `cells` can't cut the axis, or a row holds too many
#' cells to name individually.
#'
#' @section Theming:
#'
#' Each granule has its own theme elements, `ggtime.calendar.<granule>.line`,
#' `.background` and `.text` (`<granule>` = `cell`, `row`, `block`, `pane` or
#' `col`), inheriting from `panel.grid`, `panel.background` and `text`. The
#' panel's own grid is not drawn; the `cell` granule rules the time axis
#' instead.
#'
#' ```r
#' + theme(
#'   ggtime.calendar.block.line = element_line(linewidth = 1, linetype = "22"),
#'   ggtime.calendar.cell.line = element_blank()
#' )
#' ```
#'
#' A granule's labels are justified within the granule they belong to, each
#' in a different default corner so several can be labelled at once without
#' colliding.
#'
#' @examples
#' library(ggplot2)
#' library(mixtime)
#'
#' # Hourly pedestrian counts in Melbourne, as mixtime time points.
#' pedestrian <- dplyr::mutate(tsibble::pedestrian, Time = datetime(Date_Time))
#'
#' # A weekly calendar arrangement of pedestrian counts, showing the high
#' # activity at Birrarung Marr during the Australian Open in late January.
#' pedestrian_jan <- dplyr::filter(pedestrian, Time < datetime("2015-02-01 00:00:00"))
#' ggplot(pedestrian_jan, aes(x = Time, y = Count, color = Sensor)) +
#'   geom_line() +
#'   coord_calendar(rows = mixtime::weeks(1L), cols = NULL) +
#'   scale_x_mixtime(
#'     time_breaks = mixtime::days(1L),
#'     time_labels = "{cyc(day, cal_isoweek$week, label = TRUE, abbreviate = TRUE)}"
#'   ) +
#'   theme(legend.position = "bottom")
#'
#' # A full year, with monthly rows
#' pedestrian_2015 <- dplyr::filter(
#'   pedestrian,
#'   mixtime::year(Time) == mixtime::year(2015), Sensor != "Birrarung Marr"
#' )
#' ggplot(pedestrian_2015, aes(x = Time, y = Count, color = Sensor)) +
#'   geom_line() +
#'   coord_calendar(rows = month(1L), cols = NULL) +
#'   scale_x_mixtime(
#'     time_breaks = mixtime::days(1L),
#'     time_labels = "{cyc(day, cal_isoweek$week, label = TRUE, abbreviate = TRUE)}"
#'   ) +
#'   theme(
#'     legend.position = "bottom",
#'     axis.text.y = element_blank(), axis.ticks.y = element_blank()
#'   )
#'
#' @importFrom gtable gtable_col gtable_row
#' @export
coord_calendar <- function(
  cells = day(1L),
  rows = week(1L),
  blocks = NULL,
  panes = month(1L),
  cols = quarter(1L),
  pane_spacing = 0.25,
  col_spacing = 0.1,
  label_cells = "{cyc(day, month)}",
  label_rows = NULL,
  label_blocks = NULL,
  label_panes = "{cyc(month, year, label = TRUE, abbreviate = TRUE)}",
  label_cols = NULL,
  time = "x",
  xlim = NULL,
  ylim = NULL,
  expand = FALSE,
  default = FALSE,
  clip = "on",
  coord = coord_cartesian()
) {
  # Granule args may be bare expressions (like the defaults above), resolved
  # once per panel against the axis's calendar (see `eval_granule()`).
  #
  # `rows`/`cols` need a `missing()` sentinel captured before evaluation: a
  # defaulted value falls back to a duration if unresolvable, but an
  # explicit one errors instead (see `eval_granule_default()` and
  # `CoordCalendar$granule_specs()`).
  rows_missing <- missing(rows)
  cols_missing <- missing(cols)

  cells_quo <- enquo(cells)
  rows_quo <- enquo(rows)
  blocks_quo <- enquo(blocks)
  panes_quo <- enquo(panes)
  cols_quo <- enquo(cols)

  pane_spacing <- check_spacing(pane_spacing)
  col_spacing <- check_spacing(col_spacing)
  label_formats <- list(
    cell = check_labels(label_cells),
    row = check_labels(label_rows),
    block = check_labels(label_blocks),
    pane = check_labels(label_panes),
    col = check_labels(label_cols)
  )

  # Shared `coord_loop()` machinery that `CoordCalendar` plugs its own
  # row/column cutting into via `CoordLoop`'s hooks.
  loop_coord <- specialize_coord_loop(ggplot2::ggproto(
    NULL,
    CoordLoop(coord),
    time = time,
    is_flipped = isTRUE(time == "y"),
    limits = list(x = xlim, y = ylim),
    expand = expand,
    default = default,
    clip = clip
  ))

  # Only a cartesian base coord supports the npc-space row/column grid.
  if (!inherits(loop_coord, "CoordCartesian")) {
    cls <- setdiff(class(loop_coord), "CoordLoop")[1L]
    cli::cli_abort("{.fn coord_calendar} does not support {.cls {cls}}.")
  }

  ggplot2::ggproto(
    NULL,
    CoordCalendar(loop_coord),
    cells_quo = cells_quo,
    rows_quo = rows_quo,
    rows_missing = rows_missing,
    blocks_quo = blocks_quo,
    panes_quo = panes_quo,
    cols_quo = cols_quo,
    cols_missing = cols_missing,
    pane_spacing = pane_spacing,
    col_spacing = col_spacing,
    label_formats = label_formats,
    # Captured now: needed by `check_pane_granule()` deep inside
    # `ggplot_build()`, once this call's own frame is gone.
    call = current_call()
  )
}

#' Check a granule's label format
#' @param x The user's label argument.
#' @returns `x` unchanged, or `NULL` for no labels.
#' @noRd
check_labels <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (is.null(x) || is_waiver(x)) {
    return(NULL)
  }
  if (is.function(x) || (is.character(x) && length(x) == 1L && !is.na(x))) {
    return(x)
  }
  cli::cli_abort(
    c(
      "{.arg {arg}} must be a single format string or a function.",
      i = "A format string names a granule within a cycle, such as
           {.str {'{cyc(day, month)}'}}."
    ),
    call = call
  )
}

#' Check a spacing fraction
#' @param x The user's spacing argument.
#' @returns `x` as a plain number.
#' @noRd
check_spacing <- function(x, arg = caller_arg(x), call = caller_env()) {
  # `rel()` also reads naturally for a fraction of a tile.
  x <- unclass(x)
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || x < 0 || !is.finite(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a single non-negative number.",
        i = "It gives a gap as a fraction of one row's height or column's
             width, such as {.code 0.25}."
      ),
      call = call
    )
  }
  x
}

#' The most cells a row may hold and still be broken at every one of them
#'
#' The days of a month: the coarsest cycle a calendar plausibly divides into
#' cells. See `CoordCalendar$break_granule()`.
#' @noRd
calendar_max_cell_breaks <- 31L

#' @noRd
CoordCalendar <- function(coord) {
  force(coord)
  ggplot2::ggproto(
    "CoordCalendar",
    coord,

    # Granule args from `coord_calendar()`, as unresolved quosures (see
    # `eval_granule()`).
    cells_quo = NULL,
    rows_quo = NULL,
    blocks_quo = NULL,
    panes_quo = NULL,
    cols_quo = NULL,

    # Were `rows`/`cols` left at their default? Read by `granule_specs()` to
    # fall back to a duration instead of erroring when unresolvable.
    rows_missing = FALSE,
    cols_missing = FALSE,

    # Gaps set by `coord_calendar()`.
    pane_spacing = 0,
    col_spacing = 0,

    # Label format per granule; `NULL` means no label.
    label_formats = list(),

    # Grid shape, derived across panels. Storage is a fresh environment made
    # by `setup_params()` per build; `.grid` is the transient handle
    # `setup_panel_params()` writes through.
    #
    # `pane_rows`: rows given to each pane, the tallest instance across
    # columns/panels. `n_row`/`row_pane` derive from it.
    .grid = NULL,

    # Fresh `.grid` per build, since a coord can be reused across builds.
    # Panel params keep a reference to it, so draw-time code always reads
    # this build's own grid.
    setup_params = function(self, data) {
      self$.grid <- new_environment(list(
        pane_rows = NULL,
        n_row = 1L,
        n_col = 1L,
        row_pane = NULL,
        # Cached by `grid_layout()`; fixed once every panel has been
        # through `panel_cuts()`.
        layout = NULL
      ))
      ggproto_parent(coord, self)$setup_params(data)
    },

    # Hook: cuts `col`/`row`/`pane`/`block` at once instead of one
    # dimension. Returns a list of cuts (see `calendar_compute_cuts()`),
    # carrying the resolved granules for `loop_granule()`, `window()` and
    # `panel_cuts()` below.
    compute_cuts = function(self, uncut_params, trans, scale_x, scale_y) {
      scale <- if (identical(self$time, "x")) scale_x else scale_y
      calendar_compute_cuts(
        # Uses the scale's own (possibly `xlim`/`ylim`-limited) limits for
        # the full time range.
        time_range = trans$inverse(uncut_params[[self$time_scale]]$limits),
        granules = calendar_resolve_granules(
          self$granule_specs(),
          time_scale_calendar(scale),
          call = self$call
        )
      )
    },

    # Granule specs for `calendar_resolve_granules()`: quosure plus
    # defaulting policy.
    #
    # * `cells`/`blocks`: resolved as written, no fallback.
    # * `rows`/`cols`: a defaulted value falls back to a duration instead of
    #   erroring when unresolvable (see `eval_granule_default()`).
    # * `panes`: "is this the default?" is decided after resolving, by
    #   comparing it to the default resolved against the same calendar.
    granule_specs = function(self) {
      list(
        cells = list(quo = self$cells_quo),
        rows = list(
          quo = self$rows_quo,
          is_default = self$rows_missing,
          unit = "week",
          fallback = quo(day(7L))
        ),
        blocks = list(quo = self$blocks_quo),
        panes = list(quo = self$panes_quo, default = quo(month(1L))),
        cols = list(
          quo = self$cols_quo,
          is_default = self$cols_missing,
          unit = "quarter",
          fallback = quo(month(3L))
        )
      )
    },

    # Hook: labels the time axis as a position within the `rows` cycle
    # (e.g. a weekday), read from `cuts` for this panel's own row granule.
    loop_granule = function(self, cuts) cuts$granules$rows,

    # Hook: breaks the axis at every `cell` boundary, matching the `cell`
    # gridlines (`calendar_cell_grid()`). Returns `NULL` (scale's own
    # breaks) when `cells` can't cut the axis or a row holds more cells
    # than `calendar_max_cell_breaks`.
    break_granule = function(self, cuts) {
      cell <- cuts$granules$cells
      if (is.null(cell)) {
        return(NULL)
      }
      # `calendar_axis_cuts()` already warns about an uncuttable `cells`.
      breaks <- tryCatch(
        loop_cuts_by_duration(cuts$row_window, cell),
        error = function(cnd) NULL
      )
      if (is.null(breaks)) {
        return(NULL)
      }
      # `loop_cuts_by_duration()` closes with a cut at or past the window's
      # end. The cells of the row are the ones opening within it.
      at <- as.numeric(breaks)
      end <- as.numeric(cuts$row_window[2])
      n_cells <- sum(at < end - time_tol(c(at, end), diff(at)))
      if (n_cells > calendar_max_cell_breaks) {
        return(NULL)
      }
      cell
    },

    # Hook: zooms `self$limits[[self$time_scale]]` into one row's own
    # window, so expansion, breaks and user limits are all applied to the
    # window that is actually drawn.
    window = function(self, cuts) cuts$row_window,

    # Hook: attaches the cut-specific fields to the panel params. The work
    # is done by `calendar_panel_cuts()`; this supplies the coord's own
    # input and this build's grid environment for it to grow.
    panel_cuts = function(self, cut_params, cuts, trans) {
      calendar_panel_cuts(
        cut_params,
        cuts,
        trans,
        time_scale = self$time_scale,
        label_formats = self$label_formats,
        grid = self$.grid,
        call = self$call
      )
    },

    # The calendar row each of a panel's pieces is drawn in. Resolved here
    # rather than at setup, since a piece's row depends on how tall the
    # panes ahead of it ended up, known only once every panel is set up.
    # Cached on `panel_params$piece_row_cache`, stamped with the shape of
    # `pane_rows` (`pane_rows_stamp()`).
    piece_rows = function(self, panel_params) {
      cache <- panel_params$piece_row_cache
      stamp <- pane_rows_stamp(panel_params$grid)
      if (is.null(cache$rows) || !identical(cache$stamp, stamp)) {
        cache$rows <- calendar_piece_rows(
          panel_params$pieces,
          panel_params$grid$pane_rows
        )
        cache$stamp <- stamp
      }
      cache$rows
    },

    # Hook: cuts and folds the time axis at every calendar boundary at once
    # (see `calendar_pieces()`). The piece a value lands in gives its column
    # and row of the grid; `CoordLoop$transform()` then rescales it into
    # `[0, 1]`, and `arrange_loops()` places it into its tile.
    cut_data = function(self, data, panel_params, connected) {
      cut <- if (connected) cut_connected else cut_pointwise
      pieces <- panel_params$pieces
      data <- cut(data, self$time, pieces$cuts, pieces$origins)
      piece <- data$.loop
      data$.col <- pieces$col[piece]
      data$.loop <- self$piece_rows(panel_params)[piece]
      data
    },

    # The tile geometry of the calendar grid; see `calendar_layout()`. Data
    # placement, decoration and the repeated axes all read it. Cached on
    # `grid$layout`, stamped with the shape of `pane_rows`
    # (`pane_rows_stamp()`).
    grid_layout = function(self, panel_params) {
      grid <- panel_params$grid
      stamp <- pane_rows_stamp(grid)
      if (is.null(grid$layout) || !identical(grid$layout_stamp, stamp)) {
        grid$layout <- calendar_layout(
          grid$n_row,
          grid$n_col,
          grid$row_pane,
          list(pane = self$pane_spacing, col = self$col_spacing)
        )
        grid$layout_stamp <- stamp
      }
      grid$layout
    },

    # `x`/`y` are `[0, 1]` positions within the row's window by this point.
    # Placing a piece is an affine squeeze into its tile: the column index
    # scales and offsets the time aes, the row index the perpendicular aes.
    # `calendar_transposition()` maps the layout onto the physical axes.
    arrange_loops = function(self, data, panel_params) {
      col <- data$.col
      row <- data$.loop
      data$.col <- NULL
      data$.loop <- NULL
      n_row <- panel_params$grid$n_row
      n_col <- panel_params$grid$n_col
      if (n_row == 1L && n_col == 1L) {
        return(data)
      }
      layout <- self$grid_layout(panel_params)
      transpose <- calendar_transposition(self$is_flipped)

      if (n_col > 1L && !is.null(col)) {
        track <- transpose$track(layout$col)
        for (time_aes in intersect(loop_position_aes(self$time), names(data))) {
          data[[time_aes]] <- data[[time_aes]] *
            track$extent[col] +
            track$pos[col]
        }
      }
      if (n_row > 1L && !is.null(row)) {
        track <- transpose$track(layout$row)
        for (value_aes in intersect(transpose$across_aes, names(data))) {
          data[[value_aes]] <- data[[value_aes]] *
            track$extent[row] +
            track$pos[row]
        }
      }
      data
    },

    # Tiles `CoordLoop`'s single-row decoration across the grid. Granule
    # labels are placed individually instead, since each instance shows a
    # different time; drawn in the foreground so they stay readable.
    render_fg = function(self, panel_params, theme) {
      fg <- ggproto_parent(coord, self)$render_fg(panel_params, theme)
      ctx <- calendar_render_context(self, panel_params, theme)
      grid <- tile_grob_in_grid(fg, ctx)
      labels <- calendar_label_grobs(
        panel_params$granule_instances,
        self$label_formats,
        ctx
      )
      if (length(labels) == 0L) {
        return(grid)
      }
      inject(grobTree(grid, !!!labels))
    },

    # Layered outwards: panel background, granule fills, `cell` rules,
    # row/column rules. Puts a granule fill over the background but under
    # the cell rules it divides.
    render_bg = function(self, panel_params, theme) {
      bg <- ggproto_parent(coord, self)$render_bg(
        panel_params,
        calendar_panel_grid_theme(theme)
      )
      ctx <- calendar_render_context(self, panel_params, theme)
      cells <- calendar_cell_grid(panel_params$cell_breaks, ctx)
      layers <- c(
        list(tile_grob_in_grid(bg, ctx)),
        calendar_granule_backgrounds(panel_params, ctx),
        if (!is.null(cells)) list(tile_grob_in_grid(cells, ctx))
      )
      grid <- if (length(layers) == 1L) {
        layers[[1L]]
      } else {
        inject(grobTree(!!!layers))
      }
      calendar_add_rules(grid, panel_params, ctx)
    },

    # Axes repeat along whichever dimension runs across them: the value
    # axis per row and the time axis per column, or the reverse when
    # flipped. See `calendar_transposition()`.
    render_axis_v = function(self, panel_params, theme) {
      axis_grobs <- ggproto_parent(coord, self)$render_axis_v(
        panel_params,
        theme
      )
      ctx <- calendar_render_context(self, panel_params, theme)
      calendar_axis_gtable(axis_grobs, ctx, ctx$transpose$axis_v, TRUE)
    },

    render_axis_h = function(self, panel_params, theme) {
      axis_grobs <- ggproto_parent(coord, self)$render_axis_h(
        panel_params,
        theme
      )
      ctx <- calendar_render_context(self, panel_params, theme)
      calendar_axis_gtable(axis_grobs, ctx, ctx$transpose$axis_h, FALSE)
    }
  )
}

# panel setup -----------------------------------------------------------

#' The calendar a time scale's granule expressions resolve against
#'
#' From the mixtime scale's `time_chronon`, mapped to a calendar by
#' `mixtime::time_calendar()`; the Gregorian calendar for a plain
#' `Date`/`POSIXct` axis.
#' @param scale The panel's time scale.
#' @returns An `mt_calendar`.
#' @noRd
time_scale_calendar <- function(scale) {
  chronon <- if (inherits(scale, "ScaleContinuousMixtime")) {
    scale$time_chronon
  } else {
    NULL
  }
  if (!is.null(chronon) && !is_waiver(chronon)) {
    mixtime::time_calendar(chronon)
  } else {
    mixtime::cal_gregorian
  }
}

#' Resolve a calendar's granule arguments against one calendar
#' @param specs One entry per granule argument, from
#'   `CoordCalendar$granule_specs()`: `quo` plus a defaulting policy.
#' @param calendar The `mt_calendar` to resolve against, from
#'   `time_scale_calendar()`.
#' @param call The call to report an unresolvable granule against.
#' @returns A list with one resolved granule per entry of `specs` (`NULL` if
#'   dropped), plus `pane_default`: was `panes` left at its default, or set
#'   explicitly to a granule resolving the same as it?
#' @noRd
calendar_resolve_granules <- function(specs, calendar, call = caller_env()) {
  granules <- list(pane_default = FALSE)
  for (arg in names(specs)) {
    spec <- specs[[arg]]
    resolved <- if (is.null(spec$unit)) {
      eval_granule(spec$quo, calendar, arg = arg, call = call)
    } else {
      eval_granule_default(
        spec$quo,
        spec$is_default,
        spec$unit,
        spec$fallback,
        calendar,
        arg = arg,
        call = call
      )
    }
    # `granules[[arg]] <- NULL` would drop the entry, not record `NULL`.
    granules[arg] <- list(resolved)
    if (!is.null(spec$default)) {
      granules$pane_default <- identical(
        resolved,
        eval_granule(spec$default, calendar, arg = arg, call = call)
      )
    }
  }
  granules
}

#' Cut the time axis at every calendar boundary
#'
#' The cuts a panel is laid out from. Called from
#' `CoordCalendar$compute_cuts()`, in native time.
#' @param time_range The panel's full time range, in the scale's own time
#'   type.
#' @param granules The resolved granules, from `calendar_resolve_granules()`.
#' @returns A list of:
#'   * `granules`: as passed in.
#'   * `col_cuts`: where each column starts, in native time.
#'   * `row_grid`: the row-aligned grid the rows are measured on.
#'   * `row_window`: the window one row covers, which the time axis is drawn
#'     against (see `CoordCalendar$window()`).
#'   * `span`: everything any granule of the calendar reaches.
#'   * `block_cuts`, `pane_cuts`: where each block and pane of rows starts,
#'     over `span`, or `NULL` for a calendar with no blocks/panes.
#' @noRd
calendar_compute_cuts <- function(time_range, granules) {
  col_cuts <- calendar_axis_cuts(
    time_range,
    granules$cols,
    fallback = time_range,
    name = "cols"
  )
  # One row grid, aligned to `rows` boundaries across the whole axis, so a
  # position within a row means the same thing in every column. Rows are
  # cut short from it in `calendar_panel_cuts()` wherever a coarser granule
  # falls inside one.
  row_grid <- calendar_row_cuts(col_cuts, granules$rows)
  # `max(diff(row_grid))` drops the time class to a plain number, but `+`
  # onto the still-classed `row_grid[1]` restores it.
  row_window <- c(row_grid[1], row_grid[1] + max(diff(row_grid)))
  # `blocks`/`panes` cut over the whole calendar, like `rows`/`cols`, since
  # a row cannot straddle either.
  span <- calendar_span(col_cuts, row_grid)

  list(
    granules = granules,
    col_cuts = col_cuts,
    row_grid = row_grid,
    row_window = row_window,
    span = span,
    block_cuts = calendar_axis_cuts(span, granules$blocks, name = "blocks"),
    pane_cuts = calendar_axis_cuts(span, granules$panes, name = "panes")
  )
}

#' Attach a panel's cuts to its params
#'
#' Builds the bulk of what a calendar panel needs: the numeric cuts, the
#' pane-fit check, the pieces the grid is built from, the cell gridlines, the
#' shared grid state, and every granule's instance table. Called from
#' `CoordCalendar$panel_cuts()`.
#' @param cut_params The panel params being built.
#' @param cuts This panel's cuts, from `calendar_compute_cuts()`.
#' @param trans The time scale's transformation, from `get_transformation()`.
#' @param time_scale `coord$time_scale`, naming the scale within `cut_params`
#'   that measures time.
#' @param label_formats `coord$label_formats`, saying which granules need an
#'   instance table built eagerly.
#' @param grid The build's grid environment (`coord$.grid`), grown here to fit
#'   this panel.
#' @param call The call to report a `panes` granule that does not fit against.
#' @returns `cut_params`, with the calendar's own fields attached.
#' @noRd
calendar_panel_cuts <- function(
  cut_params,
  cuts,
  trans,
  time_scale,
  label_formats,
  grid,
  call = caller_env()
) {
  # This panel's own resolved granules, which travel with its cuts rather
  # than on the coord. Not to be confused with `cut_params$granule_instances`
  # below, which is one instance table per granule.
  granules <- cuts$granules

  # Cutting happens in transformed data space, which is what `transform()`
  # receives.
  as_num <- function(x) {
    if (is.null(x)) NULL else as.numeric(trans$transform(x))
  }
  cut_params$col_cuts <- as_num(cuts$col_cuts)
  cut_params$row_cuts <- as_num(cuts$row_grid)
  cut_params$block_cuts <- as_num(cuts$block_cuts)
  cut_params$pane_cuts <- as_num(cuts$pane_cuts)

  if (!is.null(cut_params$pane_cuts)) {
    fits <- check_pane_granule(
      cut_params$pane_cuts,
      cut_params$row_cuts,
      cut_params$col_cuts,
      !is.null(granules$cols),
      is_default = granules$pane_default,
      call = call
    )
    # A defaulted `panes` that does not fit is dropped.
    if (!fits) {
      cut_params$pane_cuts <- NULL
    }
  }

  # Every row of every column, cut wherever a `cols`, `panes`, `blocks` or
  # `rows` boundary falls. What `cut_data()` folds data by.
  cut_params$pieces <- calendar_pieces(
    col_cuts = cut_params$col_cuts,
    row_cuts = cut_params$row_cuts,
    pane_cuts = cut_params$pane_cuts,
    block_cuts = cut_params$block_cuts,
    close = calendar_close(
      cut_params$col_cuts,
      cut_params$row_cuts,
      cut_params$pane_cuts,
      cut_params$block_cuts,
      has_col = !is.null(granules$cols)
    )
  )
  cut_params$cell_breaks <- calendar_cell_breaks(
    cuts$row_window,
    granules$cells,
    trans
  )

  # Grow the shared grid to fit this panel, so facets share a layout.
  grid$n_col <- max(grid$n_col, length(cuts$col_cuts) - 1L)
  calendar_add_pane_rows(grid, calendar_pane_rows(cut_params$pieces))

  # What every granule's instance table is built from.
  cut_params$granule_source <- calendar_granule_source(
    cut_params,
    time_scale,
    # Cells are cut from where the row grid starts, not the first column:
    # keeps a cell's origin aligned with the row it subdivides, even under
    # daylight saving drift.
    cell_span = c(cuts$row_grid[1], cuts$span[2]),
    col_times = cuts$col_cuts,
    cell = granules$cells,
    trans = trans
  )
  # Unlabelled granules build their instance table lazily in
  # `calendar_instances()`; `cell` is the most expensive to build.
  labelled <- calendar_granules[
    !vapply(label_formats[calendar_granules], is.null, logical(1))
  ]
  cut_params$granule_instances <- calendar_granule_tables(
    cut_params,
    labelled
  )
  cut_params$granule_cache <- new_environment()

  # Reference (not copy), so draw-time code sees this build's own grid.
  cut_params$grid <- grid

  # Per-panel cache for `piece_rows()`, since a piece's row also depends on
  # this panel's own `pieces`.
  cut_params$piece_row_cache <- new_environment()

  cut_params
}

#' Grow the shared grid's pane row counts to fit one panel
#' @param grid This build's grid environment (`CoordCalendar$.grid`),
#'   mutated in place.
#' @param pane_rows The row count of each of this panel's panes, from
#'   `calendar_pane_rows()`.
#' @noRd
calendar_add_pane_rows <- function(grid, pane_rows) {
  n <- max(length(pane_rows), length(grid$pane_rows))
  grid$pane_rows <- pmax(
    pad_zeros(grid$pane_rows, n),
    pad_zeros(pane_rows, n)
  )
  grid$n_row <- sum(grid$pane_rows)
  grid$row_pane <- rep(seq_along(grid$pane_rows), grid$pane_rows)
  invisible(grid$pane_rows)
}

#' A calendar's render-time context
#'
#' Bundles the flip transposition, the grid layout and the theme for the
#' drawing functions below.
#' @param coord The `CoordCalendar`.
#' @param panel_params Panel params, set up by `CoordCalendar$panel_cuts()`.
#' @param theme The plot's theme.
#' @returns A list with `layout` (from `coord$grid_layout()`), `piece_rows`
#'   (from `coord$piece_rows()`), `theme` and `transpose` (from
#'   `calendar_transposition()`).
#' @noRd
calendar_render_context <- function(coord, panel_params, theme) {
  list(
    layout = coord$grid_layout(panel_params),
    piece_rows = coord$piece_rows(panel_params),
    theme = theme,
    transpose = calendar_transposition(coord$is_flipped)
  )
}

#' How a calendar maps onto the physical axes
#'
#' `calendar_layout()` measures the grid in one orientation only (columns
#' along time, rows across it, top to bottom); this maps that onto the
#' panel's actual `x`/`y`, differently for `time = "y"` than `time = "x"`.
#'
#' A flipped calendar reflects each grid dimension (`1 - (pos + extent)`) so
#' rows still read left to right and columns top to bottom, but a tile's own
#' contents (data, background, `cell` rules) keep the direction of the axis
#' they're drawn against, so the grid of tiles is reflected while a tile's
#' interior is only swapped, hence the two mappings below.
#'
#' @param flip `coord$is_flipped`: does time run up the panel rather than
#'   across it?
#' @returns A list of:
#'   * `track()`: where one dimension of `calendar_layout()` sits on the
#'     physical axis it runs along, as `pos`/`extent`.
#'   * `box()`: a whole tile, from its along/across position and extent, as
#'     physical `x`/`y`/`width`/`height`.
#'   * `at()`: a point a given fraction of the way into such a tile, as
#'     physical `x`/`y`; the fraction is *not* reflected (see above).
#'   * `span()`: a box covering part of such a tile in the time direction and
#'     the whole of it in the other, as physical `x`/`y`/`width`/`height`; as
#'     with `at()`, the part is measured inside the tile and so is not
#'     reflected.
#'   * `point()`: a point of the panel's own grid geometry, as physical
#'     `x`/`y`; i.e. `box()` with no extent.
#'   * `local()`: a point inside a single tile, in that tile's own `[0, 1]`
#'     coordinates, swapped but never reflected.
#'   * `just()`, `margin()`: an `element_text()`'s justification and margin,
#'     transposed the same way `local()` transposes a position.
#'   * `across_aes`: the aesthetics running across the time direction, which
#'     the row of the grid offsets.
#'   * `axis_h`, `axis_v`: which dimension of the layout the horizontal and
#'     vertical axes repeat along, and whether that dimension's tracks run
#'     against the direction npc measures in (see `calendar_dim_tracks()`).
#' @noRd
calendar_transposition <- function(flip) {
  # Measures a position from the other end of the panel. `unit`s reflect
  # against `unit(1, "npc")` so `tile_viewport()` can hand them straight in.
  reflect <- function(pos, extent) {
    if (is.unit(pos) || is.unit(extent)) {
      unit(1, "npc") - pos - extent
    } else {
      1 - pos - extent
    }
  }

  track <- if (flip) {
    function(dim) list(pos = reflect(dim[[1]], dim[[2]]), extent = dim[[2]])
  } else {
    function(dim) list(pos = dim[[1]], extent = dim[[2]])
  }

  box <- if (flip) {
    function(along, across, along_extent, across_extent) {
      list(
        x = reflect(across, across_extent),
        y = reflect(along, along_extent),
        width = across_extent,
        height = along_extent
      )
    }
  } else {
    function(along, across, along_extent, across_extent) {
      list(
        x = along,
        y = across,
        width = along_extent,
        height = across_extent
      )
    }
  }

  at <- if (flip) {
    function(along, across, along_extent, across_extent, along_at, across_at) {
      tile <- box(along, across, along_extent, across_extent)
      list(
        x = tile$x + tile$width * across_at,
        y = tile$y + tile$height * along_at
      )
    }
  } else {
    function(along, across, along_extent, across_extent, along_at, across_at) {
      tile <- box(along, across, along_extent, across_extent)
      list(
        x = tile$x + tile$width * along_at,
        y = tile$y + tile$height * across_at
      )
    }
  }

  # Like `at()`, but taking an extent along the time direction rather than a
  # single point: the tile is placed (and so reflected) first, then the span
  # is offset into it, which keeps the span running the same way as the
  # tile's own interior rather than reversed with the grid.
  span <- if (flip) {
    function(
      along,
      across,
      along_extent,
      across_extent,
      along_at,
      along_span
    ) {
      tile <- box(along, across, along_extent, across_extent)
      list(
        x = tile$x,
        y = tile$y + tile$height * along_at,
        width = tile$width,
        height = tile$height * along_span
      )
    }
  } else {
    function(
      along,
      across,
      along_extent,
      across_extent,
      along_at,
      along_span
    ) {
      tile <- box(along, across, along_extent, across_extent)
      list(
        x = tile$x + tile$width * along_at,
        y = tile$y,
        width = tile$width * along_span,
        height = tile$height
      )
    }
  }

  list(
    track = track,
    box = box,
    at = at,
    span = span,
    point = function(x, y) box(x, y, 0, 0)[c("x", "y")],
    local = if (flip) {
      function(x, y) list(x = y, y = x)
    } else {
      function(x, y) list(x = x, y = y)
    },
    just = if (flip) {
      function(hjust, vjust) list(hjust = vjust, vjust = hjust)
    } else {
      function(hjust, vjust) list(hjust = hjust, vjust = vjust)
    },
    # `c(top, right, bottom, left)`; swapping axes swaps top/right and
    # bottom/left.
    margin = if (flip) {
      function(margin) margin[c(2L, 1L, 4L, 3L)]
    } else {
      function(margin) margin
    },
    across_aes = if (flip) ggplot_global$x_aes else ggplot_global$y_aes,
    # gtable rows build downwards (against npc `y`), columns rightwards
    # (with it).
    axis_h = list(dim = if (flip) "row" else "col", reverse = flip),
    axis_v = list(dim = if (flip) "col" else "row", reverse = !flip)
  )
}

#' A tile's viewport within the panel
#' @param x,y,width,height The tile's position and extent, as `unit()`
#'   objects in the grid's own (unflipped) orientation.
#' @param transpose The flip transposition, from `calendar_transposition()`.
#' @noRd
tile_viewport <- function(x, y, width, height, transpose) {
  tile <- transpose$box(x, y, width, height)
  viewport(
    x = tile$x,
    y = tile$y,
    width = tile$width,
    height = tile$height,
    just = c(0, 0)
  )
}

#' Drop `zeroGrob`s from a grob tree
#'
#' A blanked theme element leaves a named `zeroGrob()` behind, possibly
#' nested. Recurses into every `gTree`.
#' @param grob a grob
#' @noRd
strip_zero_grobs <- function(grob) {
  if (!inherits(grob, "gTree")) {
    return(grob)
  }
  keep <- !vapply(grob$children, inherits, logical(1), what = "zeroGrob")
  kept <- lapply(grob$children[keep], strip_zero_grobs)
  # `setChildren()` keeps `childrenOrder` in step with `children`.
  setChildren(grob, inject(gList(!!!unname(kept))))
}

#' Where every tile of the grid sits, as plain numbers
#'
#' As npc numerics rather than units, for the vectorised drawing path
#' (`tiled_grob_geometry()`), which places its geometry directly in the
#' panel's own coordinates. Tiles are listed row-major, matching
#' `repeat_grob_in_grid()`.
#' @param ctx Render context, from `calendar_render_context()`; only `layout`
#'   and `transpose` are used.
#' @returns A list of npc `x`, `y`, `width` and `height`, each with one value
#'   per tile, already transposed onto the physical axes when flipped.
#' @noRd
tile_positions <- function(ctx) {
  layout <- ctx$layout
  n_row <- length(layout$row$y)
  n_col <- length(layout$col$x)
  row <- rep(seq_len(n_row), each = n_col)
  col <- rep(seq_len(n_col), times = n_row)

  ctx$transpose$box(
    along = layout$col$x[col],
    across = layout$row$y[row],
    along_extent = layout$col$width[col],
    across_extent = layout$row$height[row]
  )
}

#' A unit's npc values, or `NULL` if it has none
#'
#' Only npc values scale meaningfully by a tile's extent; anything else is
#' left to the replicating path to resolve inside a viewport.
#' @param u a unit, or anything else
#' @noRd
npc_values <- function(u) {
  if (!is.unit(u) || !all(unitType(u) == "npc")) {
    return(NULL)
  }
  as.numeric(u)
}

#' Does a grob build what it draws at draw time?
#'
#' The vectorised tiling path reads and rebuilds a grob's slots, so it can
#' only handle grobs whose slots *are* what gets drawn. A `makeContent()`/
#' `makeContext()` method generates content when drawn instead.
#' @param grob a grob
#' @noRd
draws_own_content <- function(grob) {
  # The base grob classes have no such methods.
  classes <- setdiff(class(grob), c("gTree", "grob", "gDesc"))
  for (class in classes) {
    for (generic in c("makeContent", "makeContext")) {
      method <- utils::getS3method(generic, class, optional = TRUE)
      if (!is.null(method)) {
        return(TRUE)
      }
    }
  }
  FALSE
}

#' Flatten a grob tree down to the leaves that actually draw
#'
#' The vectorised path draws the leaves directly into the panel, so any
#' structure between them and the panel must change nothing: a plain
#' `gTree` with no viewport, no `gp` and no children viewports is descended
#' into; a `zeroGrob()` draws nothing and is dropped. Anything else, such as
#' a viewport to push, graphical parameters to inherit, or a `gTree`
#' subclass that builds its own content at draw time, is refused, so the
#' caller falls back to replication rather than silently dropping it.
#' @param grob a grob
#' @returns A flat list of leaf grobs in drawing order, or `NULL` if the tree
#'   holds anything the vectorised path cannot reproduce.
#' @noRd
flatten_drawn_grobs <- function(grob) {
  if (inherits(grob, "zeroGrob")) {
    return(list())
  }
  if (!inherits(grob, "gTree") || draws_own_content(grob)) {
    return(if (is.null(grob$vp)) list(grob) else NULL)
  }
  if (!is.null(grob$vp) || !is.null(grob$gp) || !is.null(grob$childrenvp)) {
    return(NULL)
  }
  leaves <- list()
  for (child in grob$children) {
    child_leaves <- flatten_drawn_grobs(child)
    if (is.null(child_leaves)) {
      return(NULL)
    }
    leaves <- c(leaves, child_leaves)
  }
  leaves
}

#' Repeat one rect into every tile
#'
#' One vectorised `rectGrob()`, one `x`/`y`/`width`/`height` value per tile
#' and a single shared `gp`.
#' @param grob The rect to repeat.
#' @param tiles Tile positions, from `tile_positions()`.
#' @returns A `rectGrob()`, or `NULL` if the rect cannot be placed this way.
#' @noRd
tile_rect_grob <- function(grob, tiles) {
  x <- npc_values(grob$x)
  y <- npc_values(grob$y)
  width <- npc_values(grob$width)
  height <- npc_values(grob$height)
  # One rect per tile: a rect already describing several instances isn't a
  # shape the calendar draws.
  if (any(lengths(list(x, y, width, height)) != 1L)) {
    return(NULL)
  }
  rectGrob(
    x = unit(tiles$x + x * tiles$width, "npc"),
    y = unit(tiles$y + y * tiles$height, "npc"),
    width = unit(width * tiles$width, "npc"),
    height = unit(height * tiles$height, "npc"),
    just = grob$just,
    hjust = grob$hjust,
    vjust = grob$vjust,
    gp = grob$gp,
    name = grob$name
  )
}

#' Repeat one polyline into every tile
#'
#' Tiles the cell rules' single vectorised polyline (`calendar_rule_grob()`)
#' with the same `id.lengths` trick one level up.
#' @param grob The polyline to repeat.
#' @param tiles Tile positions, from `tile_positions()`.
#' @returns A `polylineGrob()`, or `NULL` if the polyline cannot be placed
#'   this way.
#' @noRd
tile_polyline_grob <- function(grob, tiles) {
  # Neither an `id` (per-point) nor an `arrow` (per-line) is a shape the
  # calendar draws.
  if (!is.null(grob$id) || !is.null(grob$arrow)) {
    return(NULL)
  }
  x <- npc_values(grob$x)
  y <- npc_values(grob$y)
  if (is.null(x) || is.null(y) || length(x) != length(y)) {
    return(NULL)
  }
  n <- length(x)
  n_tile <- length(tiles$x)
  polylineGrob(
    x = unit(
      rep(tiles$x, each = n) +
        rep(x, times = n_tile) * rep(tiles$width, each = n),
      "npc"
    ),
    y = unit(
      rep(tiles$y, each = n) +
        rep(y, times = n_tile) * rep(tiles$height, each = n),
      "npc"
    ),
    id.lengths = rep(grob$id.lengths %||% n, times = n_tile),
    gp = grob$gp,
    name = grob$name
  )
}

#' Draw a grob's geometry across the grid, rather than replicating the grob
#'
#' Collapses what would be `n_row * n_col` grobs (each in its own viewport)
#' into one grob per shape, keeping the draw cost constant in grid size. A
#' leaf whose `gp` varies across its own instances is refused, since
#' repeating those instances would slide the parameters out of step.
#' @inheritParams tile_grob_in_grid
#' @returns A grob drawing `grob` in every tile, or `NULL` if it holds
#'   anything this cannot reproduce, in which case the caller replicates it
#'   the slow way instead.
#' @noRd
tiled_grob_geometry <- function(grob, ctx) {
  leaves <- flatten_drawn_grobs(grob)
  if (is.null(leaves)) {
    return(NULL)
  }
  # Nothing to draw is the same nothing in every tile.
  if (length(leaves) == 0L) {
    return(ggplot2::zeroGrob())
  }

  tiles <- tile_positions(ctx)
  tiled <- vector("list", length(leaves))
  for (i in seq_along(leaves)) {
    leaf <- leaves[[i]]
    drawn <- if (any(lengths(leaf$gp) > 1L)) {
      NULL
    } else if (draws_own_content(leaf)) {
      NULL
    } else if (inherits(leaf, "rect") && !inherits(leaf, "gTree")) {
      tile_rect_grob(leaf, tiles)
    } else if (inherits(leaf, "polyline") && !inherits(leaf, "gTree")) {
      tile_polyline_grob(leaf, tiles)
    }
    if (is.null(drawn)) {
      return(NULL)
    }
    tiled[[i]] <- drawn
  }
  inject(grobTree(!!!tiled))
}

#' Draw a grob in every tile of the grid
#'
#' Tiles by geometry where possible (`tiled_grob_geometry()`); otherwise
#' falls back to replicating per tile (`repeat_grob_in_grid()`).
#' @param grob The grob to tile, describing one tile in its own `[0, 1]`
#'   coordinates.
#' @param ctx Render context, from `calendar_render_context()`; only `layout`
#'   and `transpose` are used.
#' @noRd
tile_grob_in_grid <- function(grob, ctx) {
  tiled_grob_geometry(grob, ctx) %||% repeat_grob_in_grid(grob, ctx)
}

#' Replicate a grob into a row x column grid
#'
#' Every tile gets its own copy of `grob` inside its own viewport. Blanked
#' `panel.grid.*` elements are stripped first (`strip_zero_grobs()`) so only
#' children that actually draw something are replicated.
#' @inheritParams tile_grob_in_grid
#' @noRd
repeat_grob_in_grid <- function(grob, ctx) {
  grob <- strip_zero_grobs(grob)

  layout <- ctx$layout
  n_row <- length(layout$row$y)
  n_col <- length(layout$col$x)

  grobs <- vector("list", n_row * n_col)
  i <- 0L
  for (r in seq_len(n_row)) {
    for (c in seq_len(n_col)) {
      i <- i + 1L
      grobs[[i]] <- grobTree(
        grob,
        vp = tile_viewport(
          x = unit(layout$col$x[c], "npc"),
          y = unit(layout$row$y[r], "npc"),
          width = unit(layout$col$width[c], "npc"),
          height = unit(layout$row$height[r], "npc"),
          transpose = ctx$transpose
        )
      )
    }
  }
  inject(grobTree(!!!grobs))
}

# grid geometry ---------------------------------------------------------

#' Tolerance for comparing npc values
#' @noRd
NPC_TOL <- 1e-9

#' Tolerance for comparing time values
#'
#' Native time can be as large as a `POSIXct`'s ~1.8e9, well past where
#' `NPC_TOL` would resolve, so the tolerance is scaled instead: a millionth
#' of the finest cut spacing in play, floored at a few ULPs.
#' @param x The time value(s) the tolerance is scaled against for the ULP
#'   floor.
#' @param spacing Spacing(s) of the cuts being compared: differences between
#'   neighbouring boundaries of each granule, not between different
#'   granules. Zero, negative and non-finite entries are dropped; `NULL` or
#'   nothing left falls back to the ULP floor.
#' @noRd
time_tol <- function(x, spacing = NULL) {
  ulp <- 8 * .Machine$double.eps * max(abs(x), 1)
  spacing <- spacing[is.finite(spacing) & spacing > 0]
  if (!length(spacing)) {
    return(ulp)
  }
  max(ulp, min(spacing) * 1e-6)
}

#' Spacing of each of a set of cut vectors
#'
#' The `spacing` `time_tol()` wants: neighbour differences *within* each
#' granule's own cuts, keeping the vectors apart so that a row cut landing on
#' a column cut does not report a spacing of nearly zero. `NULL` granules
#' contribute nothing.
#' @param ... Numeric cut vectors, any of which may be `NULL`.
#' @noRd
cut_spacings <- function(...) {
  unlist(
    lapply(list(...), function(cuts) {
      if (length(cuts) > 1L) diff(cuts) else NULL
    }),
    use.names = FALSE
  )
}

#' Tile geometry of the calendar grid
#'
#' The single source of truth for where each calendar tile sits: read by
#' data placement, panel decoration and the repeated axes.
#'
#' Positions are npc within the panel, always in unflipped orientation (rows
#' stacked vertically, columns side by side); `calendar_transposition()`
#' maps them onto the physical axes.
#'
#' @param n_row,n_col The row and column counts, `panel_params$grid$n_row`/
#'   `panel_params$grid$n_col`.
#' @param row_pane Pane index of each row, as `loop_index()` gives it, or
#'   `NULL` for a single pane. Rows in adjacent panes are separated by a gap.
#' @param spacing Gap sizes as a named list, each entry a fraction of one
#'   tile's own extent rather than an absolute `unit()`: `pane` between panes
#'   of rows, `col` between columns. A missing entry is no gap, which gives
#'   back the seamless edge-to-edge grid.
#' @returns A list with `col` (`x`, the left edge of each column, and `width`)
#'   and `row` (`y`, the bottom edge of each row, and `height`). Rows are
#'   indexed from the top, as everywhere else in the calendar, so `row$y`
#'   decreases.
#' @noRd
calendar_layout <- function(n_row, n_col, row_pane = NULL, spacing = list()) {
  col_gaps <- rep(as.numeric(spacing$col %||% 0), max(n_col - 1L, 0L))
  row_gaps <- if (is.null(row_pane)) {
    rep(0, max(n_row - 1L, 0L))
  } else {
    # A panel with fewer rows than the grid carries on in its last pane.
    row_pane <- rep_len_last(row_pane, n_row)
    as.numeric(spacing$pane %||% 0) * (diff(row_pane) != 0L)
  }

  cols <- calendar_tiles(n_col, col_gaps)
  rows <- calendar_tiles(n_row, row_gaps)
  list(
    col = list(x = cols$pos, width = cols$extent),
    # Rows are indexed from the top; npc `y` measures from the bottom.
    row = list(y = 1 - (rows$pos + rows$extent), height = rows$extent)
  )
}

#' Equally sized tiles separated by gaps, filling `[0, 1]`
#' @param n Number of tiles.
#' @param gaps Gap following each of the first `n - 1` tiles, as a fraction of
#'   one tile's extent.
#' @returns A list with `pos` (the start of each tile) and `extent` (the shared
#'   tile size, one per tile).
#' @noRd
calendar_tiles <- function(n, gaps) {
  extent <- 1 / (n + sum(gaps))
  list(
    pos = cumsum(c(0, 1 + gaps))[seq_len(n)] * extent,
    extent = rep(extent, n)
  )
}

#' Extend a vector to a length by repeating its last value
#' @noRd
rep_len_last <- function(x, n) {
  if (length(x) >= n) {
    return(x[seq_len(n)])
  }
  c(x, rep(x[length(x)], n - length(x)))
}

#' Extend a vector to a length with zeros
#' @noRd
pad_zeros <- function(x, n) {
  c(x, rep(0L, n - length(x)))
}

#' The lengths of the runs a logical vector of run starts describes
#' @param starts A logical vector, `TRUE` wherever a new run begins.
#' @noRd
run_lengths <- function(starts) {
  diff(c(which(starts), length(starts) + 1L))
}

#' Split one dimension of the layout into gtable tracks
#'
#' A gap between calendar tiles becomes an empty track, since a gtable's
#' tracks tile it end to end.
#' @param dim One dimension of `calendar_layout()`'s result, i.e. a list of
#'   position and extent.
#' @param reverse `TRUE` when the gtable is built against the direction npc
#'   measures in, as a column of gtable rows is (downwards, against `y`).
#' @returns A list with `sizes` (npc track sizes, summing to 1) and `tile` (a
#'   logical marking which tracks hold a tile rather than a gap), in the
#'   gtable's own order.
#' @noRd
calendar_dim_tracks <- function(dim, reverse) {
  pos <- dim[[1]]
  extent <- dim[[2]]
  if (reverse) {
    pos <- 1 - (pos + extent)
  }
  ord <- order(pos)
  pos <- pos[ord]
  extent <- extent[ord]

  n <- length(pos)
  ends <- pos + extent
  # Interleave the space before each tile with the tile itself.
  sizes <- c(c(rbind(pos - c(0, ends[-n]), extent)), 1 - ends[n])
  tile <- c(rep(c(FALSE, TRUE), n), FALSE)
  keep <- tile | sizes > NPC_TOL
  list(sizes = sizes[keep], tile = tile[keep])
}

#' Place a repeated grob into tile tracks, leaving the gaps empty
#' @param grob The grob to repeat.
#' @param tile The `tile` flags from `calendar_dim_tracks()`.
#' @noRd
calendar_track_grobs <- function(grob, tile) {
  grobs <- rep(list(ggplot2::zeroGrob()), length(tile))
  grobs[tile] <- list(grob)
  grobs
}

#' Repeat an axis along one dimension of the grid
#'
#' A gtable of one track per tile, with an empty track wherever the layout
#' leaves a gap.
#' @param axis_grobs The axis grobs from the wrapped coord's own renderer.
#' @param ctx Render context, from `calendar_render_context()`; only `layout`
#'   is used.
#' @param axis The dimension to repeat along and its direction, i.e.
#'   `ctx$transpose$axis_h` or `$axis_v`.
#' @param vertical Is this the vertical (left/right) axis?
#' @noRd
calendar_axis_gtable <- function(axis_grobs, ctx, axis, vertical) {
  dim <- ctx$layout[[axis$dim]]
  if (length(dim[[1]]) < 2L) {
    return(axis_grobs)
  }
  track <- calendar_dim_tracks(dim, reverse = axis$reverse)
  sizes <- unit(track$sizes, "npc")
  lapply(axis_grobs, function(grob) {
    grobs <- calendar_track_grobs(grob, track$tile)
    if (vertical) {
      gtable_col("y_axis", grobs, width = grobWidth(grob), heights = sizes)
    } else {
      gtable_row("x_axis", grobs, height = grobHeight(grob), widths = sizes)
    }
  })
}

# calendar pieces -------------------------------------------------------

#' The pieces the calendar grid is made of
#'
#' Cuts the axis at every `col`/`pane`/`block`/`row` boundary at once. Each
#' piece is a maximal span falling in a single column, pane, block and row,
#' so exactly one row of one column, and is folded onto the `row` it came
#' from rather than its own start.
#'
#' @param col_cuts,row_cuts Numeric `col` cuts and `row` grid, the latter
#'   spanning the whole axis rather than a single column.
#' @param pane_cuts,block_cuts Numeric `pane`/`block` cuts, or `NULL` for a
#'   calendar with no panes or blocks to cut at.
#' @param close The numeric time the calendar closes at, from
#'   `calendar_close()`. Cuts past it describe time the calendar has no room
#'   for, and are dropped.
#' @returns A list describing `length(cuts) - 1` pieces:
#'   * `cuts`: the numeric boundaries, one more than there are pieces.
#'   * `origins`: the start of the `row` each piece is a part of, to fold by.
#'   * `col`: which column each piece is drawn in.
#'   * `pane`,`pane_row`: which pane of that column, and which row within that
#'     pane. Together with `panel_params$grid$pane_rows` these give the row of
#'     the grid (`calendar_piece_rows()`); a calendar with no panes has one
#'     pane holding every row.
#'   * `pane_start`,`block_start`: does the piece start a new pane or block?
#'     `block_start` is `NULL` where there are no blocks.
#' @noRd
calendar_pieces <- function(col_cuts, row_cuts, pane_cuts, block_cuts, close) {
  start <- col_cuts[1L]
  # Cuts from different granules reaching the same boundary can differ in
  # the last bit or two, so they're compared with a tolerance (`time_tol()`)
  # rather than matched exactly.
  tol <- time_tol(
    c(start, close),
    cut_spacings(col_cuts, row_cuts, pane_cuts, block_cuts)
  )

  # The last column cut closes the columns rather than opening one, so it
  # never cuts a row; `close` says where the calendar's last row ends.
  inner <- c(col_cuts[-length(col_cuts)], row_cuts, pane_cuts, block_cuts)
  inner <- sort(inner[inner > start + tol & inner < close - tol])
  if (length(inner) > 0L) {
    inner <- inner[c(TRUE, diff(inner) > tol)]
  }
  cuts <- c(start, inner, close)

  n <- length(cuts) - 1L
  starts <- cuts[-length(cuts)]
  col <- loop_index(starts, col_cuts)
  pane <- if (is.null(pane_cuts)) rep(1L, n) else loop_index(starts, pane_cuts)

  # Pieces run in time order, so each ordinal is a count along the runs of
  # the level above it.
  new_col <- c(TRUE, col[-1L] != col[-n])
  pane_start <- new_col | c(TRUE, pane[-1L] != pane[-n])
  pane_lengths <- run_lengths(pane_start)

  list(
    cuts = cuts,
    origins = row_cuts[loop_index(starts, row_cuts)],
    col = col,
    pane = rep(sequence(run_lengths(new_col[pane_start])), pane_lengths),
    pane_row = sequence(pane_lengths),
    pane_start = pane_start,
    block_start = if (!is.null(block_cuts)) {
      block <- loop_index(starts, block_cuts)
      new_col | c(TRUE, block[-1L] != block[-n])
    }
  )
}

#' How many rows each pane of a panel's grid needs
#'
#' A pane is as tall as the most rows any one column puts in it.
#' `vec_group_loc()` groups by value since `pieces$pane` restarts at 1 in
#' every column (see `calendar_pieces()`).
#' @param pieces The panel's pieces, from `calendar_pieces()`.
#' @returns An integer row count, one per pane.
#' @noRd
calendar_pane_rows <- function(pieces) {
  groups <- vctrs::vec_group_loc(pieces$pane)
  rows <- integer(max(pieces$pane))
  rows[groups$key] <- vapply(
    groups$loc,
    function(loc) max(pieces$pane_row[loc]),
    integer(1)
  )
  rows
}

#' Which row of the grid is each piece drawn in?
#'
#' Panes align across the grid: every column's second pane starts at the
#' same row.
#' @param pieces The panel's pieces, from `calendar_pieces()`.
#' @param pane_rows `panel_params$grid$pane_rows`, the row count of each
#'   pane across every column and panel.
#' @noRd
calendar_piece_rows <- function(pieces, pane_rows) {
  cumsum(c(0L, pane_rows))[pieces$pane] + pieces$pane_row
}

#' Invalidation key for anything derived from the shared grid
#'
#' Lets `CoordCalendar$grid_layout()`/`$piece_rows()` recompute their cache
#' when `grid$pane_rows` has grown since.
#' @param grid The build's grid environment, `panel_params$grid`.
#' @noRd
pane_rows_stamp <- function(grid) {
  as.double(c(length(grid$pane_rows), sum(grid$pane_rows)))
}

# granule instances -----------------------------------------------------

#' Where every instance of one granule falls, and what time it is at
#'
#' Labels and fills can't be a single grob repeated into every tile like the
#' rules, since each instance names a different time and sits at a
#' different position. Positions are derived the same way as for data: from
#' the piece the instance falls in and the plain coord's own rescaling.
#'
#' @param granule One of `calendar_granules`.
#' @param params The panel params, holding the numeric cuts, the pieces the
#'   calendar is made of, and `granule_source` (see
#'   `calendar_granule_source()`).
#' @returns A data frame of `time` (native), `col`, `piece` (which the row is
#'   resolved from at draw time, see `calendar_piece_rows()`), `last_piece`
#'   (the last piece the instance covers, for a granule that holds whole rows;
#'   `piece` itself for one that is a row) and `start`/`end` (the span of the
#'   instance within its row's window, in npc); or `NULL` for a granule with
#'   nothing to place.
#' @noRd
calendar_granule_table <- function(granule, params) {
  source <- params$granule_source
  pieces <- params$pieces

  switch(
    granule,
    # Cut over the whole calendar rather than a single row, so a granule
    # that doesn't divide a row evenly still lands correctly. The largest
    # table (one row per cell of the calendar), so built only on demand.
    cell = {
      cell_cuts <- calendar_axis_cuts(
        source$cell_span,
        source$cell,
        name = "cells"
      )
      if (is.null(cell_cuts)) {
        return(NULL)
      }
      cell_cuts <- as.numeric(source$trans$transform(cell_cuts))
      n_cell <- length(cell_cuts) - 1L
      calendar_place_instances(
        starts = cell_cuts[seq_len(n_cell)],
        ends = cell_cuts[-1L],
        pieces = pieces,
        trans = source$trans,
        rescale = source$rescale
      )
    },
    # Named by the time it starts at, a `row` boundary unless a coarser
    # granule cut it short.
    row = {
      cuts <- pieces$cuts
      n_piece <- length(cuts) - 1L
      calendar_place_instances(
        starts = cuts[seq_len(n_piece)],
        ends = cuts[-1L],
        pieces = pieces,
        trans = source$trans,
        rescale = source$rescale
      )
    },
    block = calendar_group_instances(
      pieces,
      if (!is.null(params$block_cuts)) pieces$block_start,
      source$trans
    ),
    pane = calendar_group_instances(
      pieces,
      if (!is.null(params$pane_cuts)) pieces$pane_start,
      source$trans
    ),
    col = calendar_col_instances(
      source$col_times,
      pieces,
      length(params$col_cuts) - 1L
    )
  )
}

#' What a granule's instance table is built from
#'
#' Stashed on the panel params since a granule's *background* isn't known
#' until the theme is in hand at draw time (`render_bg()`, see
#' `calendar_instances()`).
#' @param cut_params The panel params being built.
#' @param time_scale `self$time_scale` from the coord, naming the scale
#'   within `cut_params` that measures time.
#' @param cell_span The span the `cell` granule is cut over, in the scale's
#'   own time type, measured from the start of the row grid so cells stay
#'   aligned to the rows they subdivide.
#' @param col_times The column cuts in the scale's own time type, before
#'   `setup_panel_params()` reduces them to numbers.
#' @param cell The resolved `cells` granule, from `cuts$granules`.
#' @param trans The time scale's transformation, from `get_transformation()`.
#' @noRd
calendar_granule_source <- function(
  cut_params,
  time_scale,
  cell_span,
  col_times,
  cell,
  trans
) {
  list(
    cell_span = cell_span,
    col_times = col_times,
    cell = cell,
    trans = trans,
    # Maps folded time onto the `[0, 1]` a row's window is drawn in.
    rescale = cut_params[[time_scale]]$rescale
  )
}

#' Instance tables for a set of granules
#' @param params The panel params being built.
#' @param granule_names Which granules to build a table for, as names from
#'   `calendar_granules`.
#' @returns A named list of instance tables, `NULL` for a granule with nothing
#'   to place.
#' @noRd
calendar_granule_tables <- function(params, granule_names) {
  tables <- lapply(granule_names, calendar_granule_table, params = params)
  names(tables) <- granule_names
  tables
}

#' One granule's instance table, building it if it was not built eagerly
#'
#' `panel_cuts()` builds a table for every granule the coord labels, since
#' those are known before the theme is. A granule fill is not: it is set on
#' the theme, seen only by the renderers, and the `cell` table is expensive
#' enough (see `calendar_granule_table()`) that building all five up front
#' would cost every calendar that never sets one. So a table asked for and
#' not found is built here and cached on the panel, keeping `render_bg()`
#' from rebuilding it on a redraw.
#' @param panel_params Panel params, set up by `CoordCalendar$panel_cuts()`.
#' @param granule One of `calendar_granules`.
#' @noRd
calendar_instances <- function(panel_params, granule) {
  table <- panel_params$granule_instances[[granule]]
  if (!is.null(table)) {
    return(table)
  }
  # Wrapped in a list so "built, nothing to place" isn't cached as "not
  # built yet".
  cache <- panel_params$granule_cache
  held <- cache[[granule]]
  if (is.null(held)) {
    held <- list(calendar_granule_table(granule, panel_params))
    cache[[granule]] <- held
  }
  held[[1L]]
}

#' Resolve a granule's instances onto the grid, dropping any with no tile
#'
#' Adds each instance's row (`CoordCalendar$piece_rows()`) and drops any
#' whose row or column falls outside this panel's share of the grid.
#' @param instances A granule's instance table, or `NULL`.
#' @param ctx Render context, from `calendar_render_context()`; only `layout`
#'   and `piece_rows` are used.
#' @returns The table with a `row` column added, or `NULL` if there was
#'   nothing to place.
#' @noRd
calendar_drawn_instances <- function(instances, ctx) {
  if (is.null(instances)) {
    return(NULL)
  }
  instances$row <- ctx$piece_rows[instances$piece]
  instances <- vctrs::vec_slice(
    instances,
    instances$row <= length(ctx$layout$row$y) &
      instances$col <= length(ctx$layout$col$x)
  )
  if (nrow(instances) == 0L) {
    return(NULL)
  }
  instances
}

#' Instances of the `col` granule
#'
#' Named by where the column starts, placed at the first row of the grid
#' and spanning only that row (unlike `block`/`pane`, see
#' `calendar_group_instances()`).
#' @param col_times The column cuts in the scale's own time type.
#' @param pieces The panel's pieces, from `calendar_pieces()`.
#' @param n_col The number of columns the cuts describe.
#' @noRd
calendar_col_instances <- function(col_times, pieces, n_col) {
  piece <- match(seq_len(n_col), pieces$col)
  col <- which(!is.na(piece))
  calendar_row_spanning_instances(
    time = vctrs::vec_slice(col_times, col),
    piece = piece[col],
    pieces = pieces
  )
}

#' Instances of a granule that groups rows rather than cutting one
#'
#' A `block`/`pane` instance is placed at the row its group starts with and
#' covers every row through the last of the group (see
#' `calendar_covered_instances()`, one rect per row for a fill). Named by
#' the time in the middle of the group, clear of the boundaries it was cut
#' at.
#' @param pieces The panel's pieces, from `calendar_pieces()`.
#' @param starts Which pieces start a group, or `NULL` for a calendar with no
#'   instances of this granule to place.
#' @inheritParams calendar_place_instances
#' @noRd
calendar_group_instances <- function(pieces, starts, trans) {
  if (is.null(starts)) {
    return(NULL)
  }
  cuts <- pieces$cuts
  first <- which(starts)
  # A group runs until the next starts; the last runs to the calendar's end.
  last <- c(first[-1L] - 1L, length(starts))

  calendar_row_spanning_instances(
    time = trans$inverse((cuts[first] + cuts[last + 1L]) / 2),
    piece = first,
    pieces = pieces,
    last_piece = last
  )
}

#' Instances spanning the whole of the row they are placed at
#'
#' `col`, `block` and `pane` all hold whole rows, so unlike
#' `calendar_place_instances()` none has a partial extent to compute.
#' Spanning the whole row (not just the group's own first cell) keeps a
#' group's label at the same edge wherever in the row it starts.
#' @param time The native time each instance is named by.
#' @param piece The piece each instance is placed at.
#' @param pieces The panel's pieces, from `calendar_pieces()`.
#' @param last_piece The last piece each instance covers, for a granule that
#'   holds several rows; the piece it is placed at (so, one row) by default.
#' @noRd
calendar_row_spanning_instances <- function(
  time,
  piece,
  pieces,
  last_piece = piece
) {
  vctrs::data_frame(
    time = time,
    col = pieces$col[piece],
    piece = piece,
    last_piece = last_piece,
    start = rep(0, length(piece)),
    end = rep(1, length(piece))
  )
}

#' Place instances of one granule into the calendar grid
#'
#' Placed by the piece each instance starts in, giving its column, row and
#' fold offset. Kept where it overlaps the calendar rather than where it
#' starts inside it, so a first column opening part way through a granule
#' still gets that granule placed.
#' @param starts,ends The numeric start and end of each instance.
#' @param pieces The panel's pieces, from `calendar_pieces()`.
#' @inheritParams calendar_granule_source
#' @param rescale The time scale's `rescale()`, mapping folded time onto the
#'   `[0, 1]` the row's window is drawn in.
#' @noRd
calendar_place_instances <- function(starts, ends, pieces, trans, rescale) {
  cuts <- pieces$cuts
  close <- cuts[length(cuts)]
  tol <- time_tol(c(cuts[1L], close), c(cut_spacings(cuts), ends - starts))
  inside <- ends > cuts[1L] + tol & starts < close - tol
  starts <- starts[inside]
  ends <- ends[inside]

  piece <- loop_index(starts, cuts)
  # Clipped to the row it starts in: labelled where it begins, the rest
  # belongs to the row (or pane, or column) it carries on into.
  ends <- pmin(ends, cuts[piece + 1L])
  pos <- starts - pieces$origins[piece] + pieces$origins[1L]

  vctrs::data_frame(
    time = trans$inverse(starts),
    col = pieces$col[piece],
    piece = piece,
    # Clipped above, so this never reaches past its own row.
    last_piece = piece,
    start = rescale(pos),
    end = pmin(rescale(pos + (ends - starts)), 1)
  )
}

#' Expand instances onto every row they cover
#'
#' A `block`/`pane` instance is *placed* at the row its group starts, but
#' covers every row through the end of the group; a fill needs one rect per
#' row, so each piece of the group becomes its own instance here, carrying
#' the group's own time and extent. Instances already one row long (`piece
#' == last_piece`) pass through untouched.
#' @param instances A granule's instance table, or `NULL`.
#' @returns The table with one row per piece covered, or `NULL` if there was
#'   nothing to place.
#' @noRd
calendar_covered_instances <- function(instances) {
  if (is.null(instances)) {
    return(NULL)
  }
  covers <- instances$last_piece - instances$piece + 1L
  if (all(covers == 1L)) {
    return(instances)
  }
  out <- vctrs::vec_rep_each(instances, covers)
  out$piece <- out$piece + sequence(covers) - 1L
  out$last_piece <- out$piece
  out
}

# theme elements --------------------------------------------------------

#' The calendar's granules, from finest to coarsest
#'
#' Named the same way throughout: as arguments of `coord_calendar()`, as
#' theme elements, and as entries of a granule instance table.
#' @noRd
calendar_granules <- c("cell", "row", "block", "pane", "col")

#' The theme element naming one part of one granule
#' @param granule One of `calendar_granules`.
#' @param part `"line"`, `"background"` or `"text"`.
#' @noRd
calendar_element_name <- function(granule, part) {
  paste("ggtime.calendar", granule, part, sep = ".")
}

#' Register the calendar's theme elements
#'
#' Called from `.onLoad()`. Every granule gets a rule (`line`), a fill
#' (`background`) and a label (`text`), each inheriting from the ordinary
#' panel element it specialises (`panel.grid`, `panel.background`, `text`),
#' which keeps ggplot2's ink/paper theming working.
#'
#' `line` defaults to a hairline within a row, the plain `panel.grid` rule
#' between rows, and a heavy rule at each block; `pane`/`col` are blank,
#' since they separate with a gap instead. `size`/`just` place each
#' granule's label in a different corner so several can be labelled at once.
#' `background` is blank for all five; a fill is only drawn where a theme
#' asks for one (see `calendar_granule_backgrounds()`).
#' @noRd
register_calendar_theme_elements <- function() {
  # Built here, not as a package-level constant, since these are ggplot2
  # objects that would otherwise freeze in at byte-compile time.
  rule <- function(linewidth) {
    ggplot2::element_line(
      linewidth = ggplot2::rel(linewidth),
      inherit.blank = TRUE
    )
  }
  label <- function(size, just) {
    ggplot2::element_text(
      size = ggplot2::rel(size),
      hjust = just[1],
      vjust = just[2],
      # Keeps a label off the rule it is justified against.
      margin = ggplot2::margin(2, 2, 2, 2),
      inherit.blank = TRUE
    )
  }
  blank <- ggplot2::element_blank()

  granules <- list(
    cell = list(line = rule(0.5), size = 1, just = c(1, 1)),
    row = list(line = NULL, size = 0.8, just = c(0, 0)),
    block = list(line = rule(2), size = 0.8, just = c(1, 0)),
    pane = list(line = blank, size = 0.8, just = c(0, 1)),
    col = list(line = blank, size = 0.8, just = c(0.5, 0))
  )

  parts <- list(
    line = list(class = "element_line", inherit = "panel.grid"),
    background = list(class = "element_rect", inherit = "panel.background"),
    text = list(class = "element_text", inherit = "text")
  )

  element_tree <- list()
  defaults <- list()
  for (granule in calendar_granules) {
    spec <- granules[[granule]]
    for (part in names(parts)) {
      element_tree[[calendar_element_name(granule, part)]] <- ggplot2::el_def(
        parts[[part]]$class,
        parts[[part]]$inherit
      )
    }
    # `NULL` registers nothing, leaving the granule to inherit `panel.grid`.
    if (!is.null(spec$line)) {
      defaults[[calendar_element_name(granule, "line")]] <- spec$line
    }
    defaults[[calendar_element_name(granule, "background")]] <- blank
    defaults[[calendar_element_name(granule, "text")]] <-
      label(spec$size, spec$just)
  }

  inject(ggplot2::register_theme_elements(
    !!!defaults,
    element_tree = element_tree
  ))
}

#' The resolved theme element for one part of one granule
#'
#' @param theme The plot's theme.
#' @param granule One of `calendar_granules`.
#' @param part `"line"`, `"background"` or `"text"`.
#' @returns The calculated element, or `NULL` if it is blank (i.e. nothing to
#'   draw).
#' @noRd
calendar_element <- function(theme, granule, part) {
  el <- ggplot2::calc_element(calendar_element_name(granule, part), theme)
  if (is.null(el) || inherits(el, "element_blank")) {
    return(NULL)
  }
  el
}

# granule backgrounds ---------------------------------------------------

#' Fill every instance of every granule a theme asks for
#'
#' Fills the same instances as the granule's labels
#' (`calendar_granule_table()`). `block`/`pane` are expanded to one instance
#' per row first (`calendar_covered_instances()`), since a fill covers the
#' whole group. Drawn coarsest granule first, so the more specific fill
#' wins where two overlap.
#' @param panel_params Panel params, set up by `CoordCalendar$panel_cuts()`.
#' @param ctx Render context, from `calendar_render_context()`.
#' @returns A list of grobs, one per granule with a fill to draw.
#' @noRd
calendar_granule_backgrounds <- function(panel_params, ctx) {
  grobs <- list()
  for (granule in rev(calendar_granules)) {
    el <- calendar_element(ctx$theme, granule, "background")
    if (is.null(el)) {
      next
    }
    instances <- calendar_drawn_instances(
      calendar_covered_instances(calendar_instances(panel_params, granule)),
      ctx
    )
    if (is.null(instances)) {
      next
    }
    grobs <- c(
      grobs,
      list(calendar_background_grob(el, instances, ctx, granule))
    )
  }
  grobs
}

#' Draw one granule's fills
#'
#' One vectorised `rectGrob()` for the whole granule. Positioned in the
#' panel's own coordinates, since each instance sits in a different tile
#' (see `calendar_transposition()`).
#' @param el The `element_rect` to draw them with.
#' @param instances The granule's instances, from `calendar_drawn_instances()`.
#' @param ctx Render context, from `calendar_render_context()`.
#' @param granule The granule being filled, used to name the grob as the rules
#'   and labels are named.
#' @noRd
calendar_background_grob <- function(el, instances, ctx, granule) {
  layout <- ctx$layout
  col <- instances$col
  row <- instances$row
  # Spans part of the row's window in the time direction, and the whole of
  # its tile in the other, as its label is justified (`calendar_label_grob()`).
  box <- ctx$transpose$span(
    along = layout$col$x[col],
    across = layout$row$y[row],
    along_extent = layout$col$width[col],
    across_extent = layout$row$height[row],
    along_at = instances$start,
    along_span = instances$end - instances$start
  )
  grob <- ggplot2::element_grob(
    el,
    x = unit(box$x, "npc"),
    y = unit(box$y, "npc"),
    width = unit(box$width, "npc"),
    height = unit(box$height, "npc"),
    just = c(0, 0)
  )
  grob$name <- calendar_element_name(granule, "background")
  grob
}

# gridlines -------------------------------------------------------------

#' Cutpoints spanning a time range at a granule, or a fallback
#'
#' `col`'s `fallback` is `time_range` itself (a single piece spanning the
#' whole range); `pane`/`block`/`cell` are optional decoration, so theirs is
#' `NULL` ("not drawn"). A granule that fails to *cut* (e.g. a `cell` too
#' fine for the axis's chronon) warns, naming `name` and the underlying
#' failure, and also returns `fallback`.
#' @param time_range A length-2 vector of the scale's own time type.
#' @param granule A duration already reduced to a granule, or `NULL`.
#' @param fallback What to return for a `NULL` granule, or one that fails to
#'   cut against `time_range`.
#' @param name The coord argument `granule` came from (`"cols"`, `"panes"`,
#'   `"blocks"`, `"cells"`), used only to name it in the warning.
#' @noRd
calendar_axis_cuts <- function(
  time_range,
  granule,
  fallback = NULL,
  name = "granule"
) {
  if (is.null(granule)) {
    return(fallback)
  }
  tryCatch(
    loop_cuts_by_duration(time_range, granule),
    error = function(e) {
      cli::cli_warn(
        c(
          "{.arg {name}} could not be cut against this time axis, so it has \\
           been dropped.",
          "x" = conditionMessage(e)
        ),
        call = NULL
      )
      fallback
    }
  )
}

#' The row grid every column is measured on
#'
#' One `row`-aligned grid spanning the whole axis, so every column agrees on
#' where a row starts. Rows themselves are cut short from it in
#' `calendar_pieces()` wherever a coarser boundary falls inside one.
#' @param col_cuts Column cuts in the scale's own time type, from
#'   `calendar_axis_cuts()`.
#' @param row The resolved `rows` granule, or `NULL` for a single row per
#'   column.
#' @returns The row grid in the scale's own time type, spanning every column.
#' @noRd
calendar_row_cuts <- function(col_cuts, row) {
  if (is.null(row)) {
    # No row cutting: each column is a single row, folded onto its own
    # start.
    return(col_cuts)
  }
  loop_cuts_by_duration(c(col_cuts[1], col_cuts[length(col_cuts)]), row)
}

#' The span of time any of the calendar's granules reaches
#'
#' The columns, closing with the row grid wherever that reaches past the
#' last of them. What `pane`/`block`/`cell` are cut over; see
#' `calendar_close()` for where the calendar actually closes.
#' @param col_cuts,row_cuts Column cuts and the row grid, in the scale's own
#'   time type.
#' @returns A length-2 vector of the scale's own time type.
#' @noRd
calendar_span <- function(col_cuts, row_cuts) {
  # `[`/`c()` preserve the time class, unlike `base::unique()`.
  ends <- c(col_cuts[length(col_cuts)], row_cuts[length(row_cuts)])
  c(col_cuts[1], ends[which.max(as.numeric(ends))])
}

#' Where the calendar closes
#'
#' A calendar's last row is a whole row, not one clipped to the data, but
#' carried only as far as the next boundary of any granule, since a row
#' running past it would straddle it (see `calendar_pieces()`).
#' @param col_cuts,row_cuts,pane_cuts,block_cuts Numeric cuts of each granule,
#'   `NULL` for a granule the calendar does not have.
#' @param has_col Is `col` set? Without it the last column cut is the end of
#'   the data rather than a boundary of the calendar's own.
#' @returns The numeric time the calendar's last row ends at.
#' @noRd
calendar_close <- function(col_cuts, row_cuts, pane_cuts, block_cuts, has_col) {
  end <- col_cuts[length(col_cuts)]
  cuts <- c(row_cuts, pane_cuts, block_cuts, if (has_col) col_cuts)
  tol <- time_tol(
    end,
    cut_spacings(col_cuts, row_cuts, pane_cuts, block_cuts)
  )
  cuts <- cuts[cuts >= end - tol]
  # `min()` on an empty vector returns `Inf` with a warning, which would
  # close the calendar at infinity; falling back to `end` guards against
  # that.
  if (!length(cuts)) {
    return(end)
  }
  min(cuts)
}

#' Check that `panes` sits between `rows` and `cols` in coarseness
#'
#' Checked on the actual cuts, since only they know how long each granule is
#' for a given range.
#' @param pane_cuts,row_cuts,col_cuts Numeric cuts, from `panel_params`.
#' @param has_col Is `cols` set? An unset `cols` spans the whole time range,
#'   which no `panes` can be coarser than.
#' @param is_default Was `panes` left at its default?
#' @returns `TRUE` if the panes can be drawn, `FALSE` if a defaulted `panes`
#'   does not fit; errors for a user-set `panes` that does not fit.
#' @noRd
check_pane_granule <- function(
  pane_cuts,
  row_cuts,
  col_cuts,
  has_col,
  is_default = FALSE,
  call = caller_env()
) {
  pane <- diff(pane_cuts)
  if (min(pane) <= min(diff(row_cuts))) {
    if (is_default) {
      return(FALSE)
    }
    cli::cli_abort(
      c(
        "{.arg panes} must be coarser than {.arg rows}.",
        i = "A pane groups whole rows, so it cannot be shorter than one."
      ),
      call = call
    )
  }
  if (has_col && max(pane) > max(diff(col_cuts))) {
    if (is_default) {
      return(FALSE)
    }
    cli::cli_abort(
      c(
        "{.arg panes} must not be coarser than {.arg cols}.",
        i = "Rows restart at the top of every column, so a pane spanning more
             than a column would never separate any of them."
      ),
      call = call
    )
  }
  TRUE
}

#' Cell gridline positions, as fractions of one row's own window
#' @param row_window The window one row is drawn in, in the scale's own time
#'   type: the widest a row gets, which every row is drawn within.
#' @param cell A duration already reduced to a granule, or `NULL`.
#' @param trans The time scale's transformation, from `get_transformation()`.
#' @returns A numeric vector of positions strictly inside `(0, 1)`, or `NULL`
#'   if `cell` is `NULL`.
#' @noRd
calendar_cell_breaks <- function(row_window, cell, trans) {
  cuts <- calendar_axis_cuts(row_window, cell, name = "cells")
  if (is.null(cuts)) {
    return(NULL)
  }
  cuts_num <- as.numeric(trans$transform(cuts))
  window <- as.numeric(trans$transform(row_window))
  breaks <- (cuts_num - window[1]) / (window[2] - window[1])
  # Only boundaries *within* a row are cell boundaries; the window's own
  # edges belong to the coarser granule that cut it.
  breaks[breaks > NPC_TOL & breaks < 1 - NPC_TOL]
}

#' Pare the panel's own grid back to what a calendar can read
#'
#' A calendar tiles the panel's decoration into every cell, so its own grid
#' would otherwise be drawn dozens of times over. Both axes are dropped
#' outright: the time axis is ruled by `cell` boundaries instead
#' (`calendar_cell_grid()`), and the value axis by the rules between rows
#' (`calendar_add_rules()`).
#'
#' Each name is blanked in its own right, not just at `panel.grid`, so a
#' more specific theme element is overridden too; `panel.grid` itself is
#' left alone, since the calendar's own rules inherit from it.
#' @param theme The plot's theme.
#' @noRd
calendar_panel_grid_theme <- function(theme) {
  blank <- c(
    "panel.grid.minor",
    "panel.grid.minor.x",
    "panel.grid.minor.y",
    "panel.grid.major",
    "panel.grid.major.x",
    "panel.grid.major.y"
  )
  for (element in blank) {
    theme[[element]] <- ggplot2::element_blank()
  }
  theme
}

#' Thin gridlines at `cell` boundaries within one row's window
#'
#' Described once in a single tile's own `[0, 1]` space, for the caller to tile
#' into the row x column grid (see `tile_grob_in_grid()`), so it repeats into
#' every cell for free.
#' @param breaks Cell break fractions, from `calendar_cell_breaks()`.
#' @param ctx Render context, from `calendar_render_context()`; only `theme`
#'   and `transpose` are used.
#' @returns The rules for one tile, or `NULL` if there are none to draw.
#' @noRd
calendar_cell_grid <- function(breaks, ctx) {
  if (is.null(breaks) || length(breaks) == 0L) {
    return(NULL)
  }
  el <- calendar_element(ctx$theme, "cell", "line")
  if (is.null(el)) {
    return(NULL)
  }

  calendar_rule_grob(
    el,
    from = rep(0, length(breaks)),
    to = rep(1, length(breaks)),
    at = breaks,
    horizontal = FALSE,
    # The cell rules are drawn once in a single tile's own `[0, 1]` space and
    # tiled from there, so they transpose with the tile's interior rather than
    # with the grid of tiles.
    transpose = ctx$transpose$local,
    granule = "cell"
  )
}

#' Draw a set of parallel rules
#'
#' @param el The `element_line` to draw them with.
#' @param from,to,at The start, end and offset of each rule, all in npc and all
#'   the same length. A `horizontal` rule runs from `x = from` to `x = to` at
#'   `y = at`, and a vertical one the other way about.
#' @param horizontal Does the rule run along the panel's x axis, in the grid's
#'   own (unflipped) orientation?
#' @param transpose The mapping onto the physical axes: `ctx$transpose$point`
#'   for rules positioned in the panel's own grid geometry, or
#'   `ctx$transpose$local` for rules drawn inside a single tile (see
#'   `calendar_transposition()`).
#' @param granule The granule the rules belong to, used to name the grob as
#'   ggplot2 names the panel's own gridlines.
#' @noRd
calendar_rule_grob <- function(
  el,
  from,
  to,
  at,
  horizontal,
  transpose,
  granule
) {
  n <- length(at)
  along <- as.vector(rbind(from, to))
  across <- rep(at, each = 2L)
  args <- if (horizontal) {
    transpose(along, across)
  } else {
    transpose(across, along)
  }
  inject(ggplot2::element_grob(
    el,
    !!!args,
    id.lengths = rep(2L, n),
    name = calendar_element_name(granule, "line")
  ))
}

#' Add the rules separating rows and columns
#'
#' Drawn on top of the tiled background, since these rules describe the
#' grid as a whole. Each row boundary gets exactly one rule from the
#' coarsest granule it belongs to: none at a `pane` gap, the heavy block
#' rule at a new `block`, the plain row rule otherwise. Drawn per column so
#' they stop at column gaps.
#' @param grid The tiled background grob, from `tile_grob_in_grid()`.
#' @param panel_params Panel params, with the cuts and pieces set by
#'   `setup_panel_params()`.
#' @param ctx Render context, from `calendar_render_context()`.
#' @noRd
calendar_add_rules <- function(grid, panel_params, ctx) {
  grobs <- c(
    calendar_row_rules(panel_params, ctx),
    calendar_col_rules(ctx)
  )
  if (length(grobs) == 0L) {
    return(grid)
  }
  inject(grobTree(grid, !!!grobs))
}

#' The rules between rows, one granule at a time
#' @inheritParams calendar_add_rules
#' @returns A list of grobs, one per granule that has anything to draw.
#' @noRd
calendar_row_rules <- function(panel_params, ctx) {
  layout <- ctx$layout
  piece_rows <- ctx$piece_rows
  y <- layout$row$y
  height <- layout$row$height
  n_row <- length(y)
  n_col <- length(layout$col$x)
  if (n_row < 2L) {
    return(list())
  }
  boundary <- seq_len(n_row - 1L)

  # Which granule owns each boundary of each column, as an integer code
  # into `calendar_granules` for a faster dispatch below. A boundary with a
  # gap already open belongs to `pane`, in every column at once since panes
  # align across the grid.
  row_code <- match("row", calendar_granules)
  block_code <- match("block", calendar_granules)
  pane_code <- match("pane", calendar_granules)
  granule <- matrix(row_code, nrow = n_row - 1L, ncol = n_col)
  block_start <- panel_params$pieces$block_start
  if (!is.null(block_start) && any(block_start)) {
    rows <- piece_rows[block_start]
    cols <- panel_params$pieces$col[block_start]
    # A block at the first row has no boundary above to be ruled at.
    keep <- rows > 1L & rows <= n_row & cols <= n_col
    granule[cbind(rows[keep] - 1L, cols[keep])] <- block_code
  }
  gap <- y[boundary] - (y[boundary + 1L] + height[boundary + 1L])
  granule[gap > NPC_TOL, ] <- pane_code

  at <- ifelse(
    gap > NPC_TOL,
    (y[boundary] + y[boundary + 1L] + height[boundary + 1L]) / 2,
    y[boundary]
  )

  grobs <- list()
  for (code in unique(as.vector(granule))) {
    g <- calendar_granules[code]
    el <- calendar_element(ctx$theme, g, "line")
    if (is.null(el)) {
      next
    }
    at_g <- which(granule == code, arr.ind = TRUE)
    grobs <- c(
      grobs,
      list(calendar_rule_grob(
        el,
        from = layout$col$x[at_g[, 2L]],
        to = (layout$col$x + layout$col$width)[at_g[, 2L]],
        at = at[at_g[, 1L]],
        horizontal = TRUE,
        transpose = ctx$transpose$point,
        granule = g
      ))
    )
  }
  grobs
}

#' The rules between columns
#'
#' Blank by default, since columns are separated by a gap rather than a
#' rule, but drawn in the middle of that gap when a theme asks for one.
#' @inheritParams calendar_add_rules
#' @returns A list holding the grob, or an empty list.
#' @noRd
calendar_col_rules <- function(ctx) {
  layout <- ctx$layout
  x <- layout$col$x
  width <- layout$col$width
  n_col <- length(x)
  if (n_col < 2L) {
    return(list())
  }
  el <- calendar_element(ctx$theme, "col", "line")
  if (is.null(el)) {
    return(list())
  }
  boundary <- seq_len(n_col - 1L)
  at <- (x[boundary] + width[boundary] + x[boundary + 1L]) / 2
  list(calendar_rule_grob(
    el,
    from = rep(0, length(at)),
    to = rep(1, length(at)),
    at = at,
    horizontal = FALSE,
    transpose = ctx$transpose$point,
    granule = "col"
  ))
}

# labels ----------------------------------------------------------------

#' Label every granule instance the coord asks for
#' @param granule_instances The instance tables, from
#'   `calendar_granule_tables()`.
#' @param formats `self$label_formats` from the coord.
#' @inheritParams calendar_add_rules
#' @returns A list of grobs, one per granule with labels to draw.
#' @noRd
calendar_label_grobs <- function(granule_instances, formats, ctx) {
  grobs <- list()
  for (granule in calendar_granules) {
    label_format <- formats[[granule]]
    instances <- granule_instances[[granule]]
    if (is.null(label_format) || is.null(instances)) {
      next
    }
    el <- calendar_element(ctx$theme, granule, "text")
    if (is.null(el)) {
      next
    }
    instances <- calendar_drawn_instances(instances, ctx)
    if (is.null(instances)) {
      next
    }
    grobs <- c(
      grobs,
      list(calendar_label_grob(el, instances, label_format, ctx, granule))
    )
  }
  grobs
}

#' Draw one granule's labels
#' @param el The `element_text` to draw them with.
#' @param instances The granule's instance table.
#' @param label_format The granule's entry in `self$label_formats`.
#' @inheritParams calendar_add_rules
#' @param granule The granule being labelled.
#' @noRd
calendar_label_grob <- function(el, instances, label_format, ctx, granule) {
  layout <- ctx$layout
  labels <- calendar_format_labels(instances$time, label_format, granule)
  hjust <- el@hjust %||% 0.5
  vjust <- el@vjust %||% 0.5
  margin <- el@margin %||% ggplot2::margin()

  col <- instances$col
  row <- instances$row
  # Justified within the instance's span along the time direction and
  # within the row across it (`calendar_row_spanning_instances()` gives
  # `block`/`pane` the whole row). Fractions are the same whether flipped or
  # not; only the tile they sit in moves (see `calendar_transposition()`).
  pos <- ctx$transpose$at(
    along = layout$col$x[col],
    across = layout$row$y[row],
    along_extent = layout$col$width[col],
    across_extent = layout$row$height[row],
    along_at = instances$start + hjust * (instances$end - instances$start),
    across_at = vjust
  )
  just <- ctx$transpose$just(hjust, vjust)
  margin <- ctx$transpose$margin(margin)
  # Pads the label away from the edge it's justified to.
  x <- unit(pos$x, "npc") +
    (1 - just$hjust) * margin[4] -
    just$hjust * margin[2]
  y <- unit(pos$y, "npc") +
    (1 - just$vjust) * margin[3] -
    just$vjust * margin[1]

  grob <- ggplot2::element_grob(
    el,
    label = labels,
    x = x,
    y = y,
    hjust = just$hjust,
    vjust = just$vjust
  )
  grob$name <- calendar_element_name(granule, "text")
  grob
}

#' Format the times of one granule's instances
#' @param time The instances' native times.
#' @param labels The granule's entry in `self$label_formats`: a mixtime format
#'   string, or a function of the times.
#' @param granule The granule being labelled, for the error message.
#' @noRd
calendar_format_labels <- function(
  time,
  labels,
  granule,
  call = caller_env()
) {
  arg <- paste0("label_", granule, "s")
  out <- try_fetch(
    if (is.function(labels)) {
      labels(time)
    } else {
      format(as_labelled_time(time), format = labels)
    },
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Can't label the calendar's {.field {granule}} granule.",
          i = "{.arg {arg}} must format the times of a
               {.cls {class(time)[1]}} axis."
        ),
        parent = cnd,
        call = call
      )
    }
  )
  if (length(out) != vctrs::vec_size(time)) {
    cli::cli_abort(
      "{.arg {arg}} must give one label per {.field {granule}}.",
      call = call
    )
  }
  as.character(out)
}

#' Wrap a plain date or time so that a mixtime format string applies to it
#' @noRd
as_labelled_time <- function(x) {
  if (inherits(x, "Date")) {
    return(mixtime::date(x))
  }
  if (inherits(x, "POSIXt")) {
    return(mixtime::datetime(x))
  }
  x
}
