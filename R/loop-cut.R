# Cutting and folding geometry for looped time axes
#
# The engine behind [coord_loop()] and [coord_calendar()]: data is folded
# into the first loop window, and geometry crossing a loop boundary is cut
# into one piece per loop, so the panel is drawn once regardless of loop
# count.
#
# Operates on plain data frames in scale-transformed data space (what
# `Coord$transform()` receives), serving both cartesian and radial coords,
# and kept free of ggproto so it's directly unit testable.

# Multiplier for rekeying an `id` column, spacing pieces apart so
# `GeomRibbon$draw_group()`'s lower-edge offset (`max(ids)`) can't collide
# upper piece k with lower piece k + max(ids). A constant rather than
# derived from the data, since the upper and lower edges are cut separately
# and the upper edge drops `NA` positions.
#
# Two invariants, checked in `rekey_loops()`:
#
#   * The stride must exceed `max(ids)`.
#   * `piece * stride` must stay within integer range (`polygonGrob()`
#     coerces `id` to integer, turning overflow into a silent `NA`).
loop_id_stride <- 1e5

#' Which loop does each time value fall in?
#' @param t Time values, in transformed data space.
#' @param cuts Increasing numeric loop boundaries. Loop `k` is
#'   `[cuts[k], cuts[k + 1])`, so `length(cuts) - 1` loops are described.
#' @returns Integer loop index, clamped to `[1, length(cuts) - 1]` so that
#'   `-Inf`/`Inf` and out-of-range values land in the first/last loop. `NA`
#'   values propagate.
#' @noRd
loop_index <- function(t, cuts) {
  n <- max(length(cuts) - 1L, 1L)
  idx <- findInterval(as.numeric(t), as.numeric(cuts))
  pmin(pmax(idx, 1L), n)
}

#' Fold time values into the first loop window
#' @inheritParams loop_index
#' @param loop Loop index of each value, from `loop_index()`.
#' @param origins The time each loop is folded onto, one per loop. Defaults
#'   to the cuts themselves. [coord_calendar()] uses a coarser origin, the
#'   start of the calendar row a column begins in, so a position within a
#'   row keeps its meaning across columns.
#' @returns Numeric time values translated into the first loop's window.
#' @noRd
fold_time <- function(t, loop, cuts, origins = cuts) {
  origins <- as.numeric(origins)
  as.numeric(t) - origins[loop] + origins[1L]
}

#' Fold pointwise data into the first loop window
#'
#' Used for data whose rows are independent marks (points, text, axis keys).
#' No rows are added or removed; every positional aesthetic on the time axis is
#' folded independently.
#' @param data A data frame as passed to `Coord$transform()`.
#' @param time The time aesthetic, `"x"` or `"y"`.
#' @inheritParams loop_index
#' @returns `data` with time columns folded and a `.loop` column added.
#' @noRd
cut_pointwise <- function(data, time, cuts, origins = cuts) {
  loop <- NULL
  for (col in intersect(loop_position_aes(time), names(data))) {
    idx <- loop_index(data[[col]], cuts)
    data[[col]] <- fold_time(data[[col]], idx, cuts, origins)
    loop <- loop %||% idx
  }
  data$.loop <- loop %||% rep.int(1L, nrow(data))
  data$.loop[is.na(data$.loop)] <- 1L
  data
}

#' Cut connected data at loop boundaries and fold it
#'
#' Where consecutive vertices of a path fall in different loops, a pair of
#' vertices is interpolated at each boundary crossed, closing the piece in
#' the old loop and opening one in the new. Pieces are rekeyed by
#' `(original key, loop)`, so upper and lower piece k of a ribbon share an
#' id and `polygonGrob()` reassembles each loop into one closed polygon
#' (`GeomRibbon$draw_group()`); rects, tiles, bars, columns and areas fall
#' out of the same rule.
#'
#' Holds for any ring monotone in the time direction; non-monotone concave
#' rings are a known limitation.
#' @inheritParams cut_pointwise
#' @returns `data` with boundary vertices inserted, time folded, `id`/`group`
#'   rekeyed per piece, and a `.loop` column added.
#' @noRd
cut_connected <- function(data, time, cuts, origins = cuts) {
  n <- nrow(data)
  other <- if (identical(time, "x")) "y" else "x"

  if (n == 0L) {
    data$.loop <- integer()
    return(data)
  }

  t <- as.numeric(data[[time]])
  v <- as.numeric(data[[other]])
  lp <- loop_index(t, cuts)

  key <- loop_key_column(data)
  path <- if (is.null(key)) rep.int(1L, n) else data[[key]]

  # Number of loop boundaries crossed by the segment leaving each vertex.
  nb <- integer(n)
  if (n > 1L) {
    same <- path[-n] == path[-1L]
    same[is.na(same)] <- FALSE
    delta <- lp[-1L] - lp[-n]
    delta[is.na(delta)] <- 0L
    nb[-n] <- ifelse(same, abs(delta), 0L)
  }
  lp[is.na(lp)] <- 1L

  cross <- which(nb > 0L)
  if (length(cross) == 0L) {
    # Nothing spans a boundary: fold in place.
    data[[time]] <- fold_time(t, lp, cuts, origins)
    data$.loop <- lp
    return(rekey_loops(data, lp, length(cuts) - 1L))
  }

  extra <- 2L * nb
  src <- rep.int(seq_len(n), 1L + extra)
  # Output row index of each original vertex.
  at <- cumsum(c(1L, (1L + extra)[-n]))

  # Cut indices of every boundary crossed, in vertex then crossing order.
  bidx <- unlist(
    .mapply(
      function(a, b) if (b > a) seq.int(a + 1L, b) else seq.int(a, b + 1L),
      list(lp[cross], lp[cross + 1L]),
      NULL
    ),
    use.names = FALSE
  )
  # The segment each boundary belongs to, and its direction along the axis.
  seg <- rep.int(cross, nb[cross])
  forward <- lp[seg + 1L] > lp[seg]

  # Interpolate the non-time coordinate at each boundary.
  bt <- as.numeric(cuts)[bidx]
  span <- t[seg + 1L] - t[seg]
  frac <- ifelse(span == 0, 0, (bt - t[seg]) / span)
  bv <- v[seg] + frac * (v[seg + 1L] - v[seg])

  # A crossing at cut k separates loops k - 1 and k: forward closes k - 1
  # and opens k, backward the reverse.
  close_loop <- ifelse(forward, bidx - 1L, bidx)
  open_loop <- ifelse(forward, bidx, bidx - 1L)

  # Output row indices of the inserted vertices.
  ins <- unlist(
    .mapply(function(p, e) p + seq_len(e), list(at[cross], extra[cross]), NULL),
    use.names = FALSE
  )

  out_t <- t[src]
  out_v <- v[src]
  out_lp <- lp[src]
  out_t[ins] <- rep(bt, each = 2L)
  out_v[ins] <- rep(bv, each = 2L)
  out_lp[ins] <- vctrs::vec_interleave(close_loop, open_loop)

  data <- vctrs::vec_slice(data, src)
  data[[time]] <- fold_time(out_t, out_lp, cuts, origins)
  data[[other]] <- out_v
  data$.loop <- out_lp
  rekey_loops(data, out_lp, length(cuts) - 1L)
}

#' Get cutpoints spanning a time range at a fixed duration
#'
#' The duration-stepping half of `loop_cuts()`, as a pure function of a
#' time range so it can also cut a range not taken from a scale's panel
#' params (e.g. `coord_calendar()`'s `col` boundaries from its `row` cuts'
#' span).
#' @param time_range A length-2 vector of the scale's own time type.
#' @param granule A duration, already reduced to a granule by
#'   `duration_as_granule()`.
#' @returns A vector of time cutpoints of `time_range`'s type, as described
#'   in `loop_cuts()`.
#' @noRd
loop_cuts_by_duration <- function(time_range, granule) {
  step <- granule_seq_by(time_range, granule)
  from <- mixtime::time_floor(time_range[1], granule)
  # `time_ceiling()` has already rounded past the end of the data, so this
  # sequence closes the last loop without extending beyond it.
  to <- mixtime::time_ceiling(time_range[2], granule)
  cuts <- seq(from, to, by = step)
  if (length(cuts) < 2L) {
    # All of the data sits at a single instant on a loop boundary.
    cuts <- seq(from, by = step, length.out = 2L)
  }
  # Snapping only corrects daylight saving drift, so skipped where the axis
  # can't drift (`cuts_can_drift()`).
  if (cuts_can_drift(cuts, to)) {
    cuts <- snap_cuts_to_granule(cuts, granule, to, step)
  }
  # `vec_unique()` preserves mixtime's S7 class, unlike `base::unique()`,
  # which would drop it and break downstream `time_floor()`/`time_ceiling()`.
  vctrs::vec_unique(cuts)
}

#' Can snapping these cutpoints move any of them?
#'
#' `snap_cuts_to_granule()`/`fill_skipped_cuts()` cost about a third of a
#' cutting call, and only ever correct drift from the axis's UTC offset
#' changing under a fixed-duration step (a `Date` axis, or a fixed-offset
#' zone, never drifts). Since a cut's clock time can only shift by the
#' difference in offset from the cut before it, cuts sharing one offset have
#' not drifted and need no correction.
#'
#' Even spacing is *not* a usable test: a zoned axis stepped by a constant
#' 86400 seconds is evenly spaced in seconds even though every cut past a
#' daylight saving change sits an hour off its boundary.
#' @param cuts The stepped cutpoints.
#' @param to The end the cuts were asked to span. Not skippable when the
#'   cuts fall short of it, since closing the range is snapping's job too.
#' @returns `TRUE` when the cuts must go through the snapping path, `FALSE`
#'   only when it's provably a no-op. Anything not established cheaply and
#'   reliably answers `TRUE`.
#' @noRd
cuts_can_drift <- function(cuts, to) {
  at <- as.numeric(cuts)
  n <- length(at)
  if (n < 2L) {
    return(TRUE)
  }
  tol <- cut_tol(diff(at))
  # Closing a short range is snapping's job too, unrelated to drift.
  if (at[n] < as.numeric(to) - tol) {
    return(TRUE)
  }

  zone <- try_fetch(mixtime::tz_name(cuts), error = function(cnd) NULL)
  if (is.null(zone)) {
    return(TRUE)
  }
  # `Date`, and any mixtime whose chronon carries no zone: no offset to move.
  if (all(is.na(zone))) {
    return(FALSE)
  }
  if (all(zone %in% fixed_offset_zones)) {
    return(FALSE)
  }

  offsets <- try_fetch(cut_utc_offsets(cuts), error = function(cnd) NULL)
  if (is.null(offsets) || anyNA(offsets) || length(offsets) != n) {
    return(TRUE)
  }
  length(unique(offsets)) > 1L
}

# Zones with the same offset at every instant, so no vector of cuts in one
# can straddle a change. Named rather than detected, since detecting it
# means checking every instant of the zone's existence.
fixed_offset_zones <- c(
  "UTC",
  "GMT",
  "UCT",
  "Universal",
  "Zulu",
  "GMT0",
  "GMT+0",
  "GMT-0",
  "Greenwich"
)

#' Each cutpoint's offset from UTC, in seconds
#'
#' `as.POSIXlt()` reads offsets from the platform's zone database in
#' microseconds; `mixtime::tz_offset()` answers the same question but takes
#' hundreds of milliseconds, more than the snapping it would guard.
#' @param cuts The stepped cutpoints.
#' @returns Integer seconds east of UTC, one per cut. `NA` where the platform
#'   does not report the offset, which `cuts_can_drift()` treats as unknown.
#' @noRd
cut_utc_offsets <- function(cuts) {
  if (!inherits(cuts, "POSIXt")) {
    cuts <- as.POSIXct(cuts)
  }
  as.POSIXlt(cuts)$gmtoff
}

#' A comparison tolerance for time values, relative to their own spacing
#'
#' Scaled to the gaps being compared, not the epoch: a relative epsilon on
#' the epoch is ~1.8 seconds on a modern axis, a granule rather than a
#' tolerance.
#' @param spacing The gaps between the cuts, as numerics.
#' @returns A tolerance small enough to admit only floating point noise: a
#'   millionth of the smallest gap, floored at a few ULPs of it.
#' @noRd
cut_tol <- function(spacing) {
  spacing <- abs(spacing[is.finite(spacing) & spacing != 0])
  if (length(spacing) == 0L) {
    return(0)
  }
  max(min(spacing) * 1e-6, .Machine$double.eps * max(spacing) * 8)
}

#' Snap stepped cutpoints back onto the granule's own boundaries
#'
#' Stepping by a duration drifts off a calendar's own boundaries wherever
#' its units aren't that duration long, e.g. an hour at each daylight
#' saving change, so each cut is floored back to the boundary it stands
#' for. Only snapped where that keeps the cuts in order, since a granule
#' that doesn't floor exactly against this axis would otherwise turn a
#' drifting cut into an unusable one.
#'
#' Snapping alone doesn't give one cut per instance: a drifted step can
#' land two boundaries on one instance or skip one entirely, so the result
#' is deduplicated (`loop_cuts_by_duration()`) and skipped boundaries are
#' put back (`fill_skipped_cuts()`).
#' @param cuts The stepped cutpoints.
#' @param granule The granule they were cut at.
#' @param to The end the cuts were asked to span, which snapping a drifted cut
#'   back can leave the last of them short of.
#' @param step The `seq()` step, from `granule_seq_by()`.
#' @noRd
snap_cuts_to_granule <- function(cuts, granule, to, step) {
  snapped <- try_fetch(
    mixtime::time_floor(cuts, granule),
    error = function(cnd) NULL
  )
  if (is.null(snapped) || is.unsorted(as.numeric(snapped))) {
    return(cuts)
  }
  snapped <- fill_skipped_cuts(snapped, granule)

  # The last cut can end up short after snapping; step on, snapping as we
  # go, until it's closed. Checked to advance each time, so a granule
  # flooring onto itself stops rather than looping.
  last <- function(x) vctrs::vec_slice(x, vctrs::vec_size(x))
  while (as.numeric(last(snapped)) < as.numeric(to)) {
    on <- mixtime::time_floor(
      vctrs::vec_slice(seq(last(snapped), by = step, length.out = 2L), 2L),
      granule
    )
    if (as.numeric(on) <= as.numeric(last(snapped))) {
      break
    }
    snapped <- vctrs::vec_c(snapped, on)
  }
  snapped
}

#' Put back the granule boundaries a drifted step stepped over
#'
#' Snapping (`snap_cuts_to_granule()`) puts every cut on a boundary, but not
#' every boundary on a cut: drift carried across a daylight saving change
#' can put two steps' worth of time into one, skipping the boundary between
#' them entirely, which would otherwise draw a missing calendar cell next
#' to one twice as wide.
#'
#' The boundary that should follow each cut is found by stepping one
#' nominal granule on and snapping back: landing on the next cut means
#' nothing was skipped, landing back on the same cut means the step stayed
#' within one instance, and anywhere in between is the boundary to
#' restore. The nominal step is the median gap between the cuts, so it
#' holds for granules of uneven length too.
#' @param cuts The snapped cutpoints.
#' @param granule The granule they were cut at.
#' @returns `cuts` with any skipped boundaries inserted, in time order.
#' @noRd
fill_skipped_cuts <- function(cuts, granule) {
  cuts <- vctrs::vec_unique(cuts)
  # One change hides at most one boundary, so a pass or two suffices; the
  # cap just guards against a granule that never stops finding "missing"
  # ones.
  for (pass in seq_len(10L)) {
    n <- vctrs::vec_size(cuts)
    if (n < 3L) {
      break
    }
    at <- as.numeric(cuts)
    step <- stats::median(diff(at))
    if (!is.finite(step) || step <= 0) {
      break
    }
    # Numeric step, not `seq()`'s own: steps the whole vector at once and
    # works the same for `Date`/`POSIXct` axes.
    on <- try_fetch(
      mixtime::time_floor(cuts + step, granule),
      error = function(cnd) NULL
    )
    if (is.null(on)) {
      break
    }
    next_at <- as.numeric(on)[-n]
    skipped <- which(next_at > at[-n] & next_at < at[-1L])
    if (length(skipped) == 0L) {
      break
    }
    cuts <- vctrs::vec_c(cuts, vctrs::vec_slice(on, skipped))
    cuts <- vctrs::vec_slice(cuts, order(as.numeric(cuts)))
  }
  cuts
}

#' Get cutpoints along the time axis
#'
#' @param panel_params Panel params, e.g. as returned by
#'   `Coord$setup_panel_params()`.
#' @param scale Name of the time scale within `panel_params` (`"x"`, `"y"`,
#'   `"theta"`, ...).
#' @param loops A vector of time points at which to loop, or a waiver.
#' @param time_loops A duration to loop by (e.g. `mixtime::years(1L)`), or a
#'   waiver. Takes precedence over `loops`. Already reduced to a granule by
#'   `duration_as_granule()`.
#' @returns A vector of time cutpoints of the scale's own type. Loop `k`
#'   spans `[cuts[k], cuts[k + 1])`, so there is always one more cut than
#'   loop.
#'
#'   The final cut only closes the last loop; a trailing cut beyond the
#'   data would add an empty loop (drawn as an empty row by
#'   `coord_calendar()`). A value landing exactly on it is clamped into the
#'   last loop by `loop_index()`.
#' @noRd
loop_cuts <- function(
  panel_params,
  scale,
  loops = waiver(),
  time_loops = waiver()
) {
  trans <- panel_params[[scale]]$get_transformation()
  time_range <- trans$inverse(panel_params[[scale]]$limits)

  if (!is_waiver(time_loops) && !is.null(time_loops)) {
    loop_cuts_by_duration(time_range, time_loops)
  } else if (!is_waiver(loops) && !is.null(loops)) {
    cuts <- sort(unique(loops))
    n <- length(cuts)
    # Closes the final loop with the widest loop's width, or the data's
    # overhang past the last loop point if that's larger.
    end <- if (n > 1L) cuts[n] + max(diff(cuts)) else cuts[n]
    # Compared rather than `max()`ed to keep the scale's own time type.
    if (end < time_range[2]) {
      end <- time_range[2]
    }
    c(cuts, end)
  } else {
    # No looping: a single window spanning the whole range.
    c(time_range[1], time_range[2])
  }
}

#' A `seq()` step usable for the axis's own time type
#'
#' `seq.mixtime::mt_time` steps by a granule directly, but plain `Date`/
#' `POSIXct` axes dispatch to base R's `seq.Date()`/`seq.POSIXct()`, which
#' only step by a duration string like `"2 quarters"`, built here from the
#' granule's own count and unit name.
#' @param x A value of the axis's time type, used only to dispatch on class.
#' @param granule The step, as reduced by `duration_as_granule()`.
#' @returns `granule` unchanged for a mixtime axis, or a string `seq()` step
#'   for a `Date`/`POSIXct` axis.
#' @noRd
granule_seq_by <- function(x, granule) {
  if (is_mixtime(x) || S7::S7_inherits(x, mixtime::mt_time)) {
    return(granule)
  }
  paste(granule@n, mixtime::time_unit_plural(granule))
}

# helpers -----------------------------------------------------------------

#' Positional aesthetics that live on the time axis
#' @param time The time aesthetic, `"x"` or `"y"`.
#' @returns Character vector of column names, `time` first.
#' @noRd
loop_position_aes <- function(time) {
  aes <- if (identical(time, "x")) ggplot_global$x_aes else ggplot_global$y_aes
  union(time, aes)
}

#' Which column identifies separate paths in connected data?
#'
#' `GeomPath` and friends key on `group`; `GeomRibbon` munches its edges as bare
#' `x`/`y`/`id` frames with no `group` at all.
#' @noRd
loop_key_column <- function(data) {
  for (col in c("group", "id")) {
    if (!is.null(data[[col]])) {
      return(col)
    }
  }
  NULL
}

#' Rekey pieces by `(original key, loop)`
#'
#' Derived arithmetically from the original key and loop index, rather than
#' a sequential run counter, so independently-cut edges of the same ring
#' agree: `GeomRibbon` walks its lower edge in reverse, so anything
#' order-dependent would key the two edges differently.
#' @noRd
rekey_loops <- function(data, loop, n_loops) {
  for (col in intersect(c("group", "id"), names(data))) {
    orig <- data[[col]]
    if (!is.numeric(orig)) {
      orig <- vctrs::vec_group_id(orig)
    }
    piece <- as.numeric(orig) * n_loops + loop
    if (identical(col, "id")) {
      check_loop_id_range(orig, piece)
      piece <- piece * loop_id_stride
    }
    data[[col]] <- piece
  }
  data
}

#' Check that rekeyed `id` values stay usable
#'
#' Both failures below would otherwise be silent: the first draws spurious lines
#' joining pieces that should be separate, the second drops geometry entirely.
#' @param orig The original `id` values being rekeyed.
#' @param piece The piece index of each row, before applying the stride.
#' @noRd
check_loop_id_range <- function(orig, piece) {
  max_id <- suppressWarnings(max(as.numeric(orig), na.rm = TRUE))
  max_piece <- suppressWarnings(max(piece, na.rm = TRUE))
  if (!is.finite(max_id) || !is.finite(max_piece)) {
    return(invisible())
  }

  if (max_id >= loop_id_stride) {
    cli::cli_abort(c(
      "Too many groups in a single layer to loop the time axis.",
      x = "Found {max_id} groups, but at most {loop_id_stride - 1} are supported.",
      i = "Split the layer, or reduce the number of groups it draws."
    ))
  }

  if (max_piece * loop_id_stride > .Machine$integer.max) {
    max_pieces <- floor(.Machine$integer.max / loop_id_stride)
    cli::cli_abort(c(
      "Too many pieces to loop the time axis.",
      x = "Cutting this layer produces {max_piece} pieces, but at most \\
           {max_pieces} are supported.",
      i = "Reduce the number of loops, or the number of groups in the layer."
    ))
  }

  invisible()
}
