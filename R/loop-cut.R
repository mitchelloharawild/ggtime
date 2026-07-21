# Cutting and folding geometry for looped time axes
#
# These functions implement the engine behind [coord_loop()] and
# [coord_calendar()]. Rather than drawing the panel once per loop and clipping
# each copy (which costs O(n_loops x panel content) and needs clipping paths),
# the *data* is folded into the first loop window and any geometry crossing a
# loop boundary is cut into one piece per loop. The panel is then drawn once.
#
# Everything here operates on plain data frames in the scale-transformed data
# space (i.e. the space `Coord$transform()` receives, before rescaling to npc),
# so the same code serves both cartesian and radial coords. Keeping it free of
# ggproto also keeps it directly unit testable.

# The multiplier applied when rekeying an `id` column. `GeomRibbon$draw_group()`
# offsets the lower edge's ids by `max(ids)` before drawing the outline, which
# would make lower piece k collide with upper piece k + max(ids) once cutting
# has turned one id into many. Spacing our ids out keeps the two sets disjoint.
#
# It has to be a constant rather than derived from the data: the upper and lower
# edges are cut in separate calls and must agree on the keying, but the upper
# edge drops `NA` positions and so cannot be relied on to see the same ids.
#
# Two invariants follow, both checked in `rekey_loops()`:
#
#   * The stride must exceed `max(ids)`, or the offset lands on another piece.
#   * `piece * stride` must stay within integer range, because `polygonGrob()`
#     coerces `id` to integer and turns anything larger into `NA` (with only a
#     warning, so the failure would otherwise be silent and near-invisible).
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
#' @param loop Loop index of each value, from [loop_index()].
#' @returns Numeric time values translated into `[cuts[1], cuts[2])`.
#' @noRd
fold_time <- function(t, loop, cuts) {
  cuts <- as.numeric(cuts)
  as.numeric(t) - cuts[loop] + cuts[1L]
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
cut_pointwise <- function(data, time, cuts) {
  loop <- NULL
  for (col in intersect(loop_position_aes(time), names(data))) {
    idx <- loop_index(data[[col]], cuts)
    data[[col]] <- fold_time(data[[col]], idx, cuts)
    loop <- loop %||% idx
  }
  data$.loop <- loop %||% rep.int(1L, nrow(data))
  data$.loop[is.na(data$.loop)] <- 1L
  data
}

#' Cut connected data at loop boundaries and fold it
#'
#' Used for data whose rows are vertices of a path or ring. Wherever consecutive
#' vertices of the same path fall in different loops, a pair of vertices is
#' interpolated at each boundary crossed: one closing the piece in the old loop,
#' one opening the piece in the new loop. Pieces are then rekeyed by
#' `(original key, loop)`.
#'
#' The `(key, loop)` keying is what makes rings work without a polygon clipping
#' algorithm. `GeomRibbon$draw_group()` munches the upper and lower edges as two
#' separate open paths and reassembles them with `polygonGrob(id = )`; keying by
#' `(key, loop)` means upper piece k and lower piece k get the same id, so each
#' loop reassembles into exactly one closed polygon. Rects, tiles, bars, columns
#' and areas are structurally ribbons and fall out of the same rule.
#'
#' This holds for any ring that is monotone in the time direction, which covers
#' everything that plausibly appears on a looped time axis. Non-monotone concave
#' rings are a known limitation.
#' @inheritParams cut_pointwise
#' @returns `data` with boundary vertices inserted, time folded, `id`/`group`
#'   rekeyed per piece, and a `.loop` column added.
#' @noRd
cut_connected <- function(data, time, cuts) {
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
    data[[time]] <- fold_time(t, lp, cuts)
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

  # A crossing at cut k separates loops k - 1 and k. Travelling forward we close
  # the piece in k - 1 and open one in k; travelling backward, the reverse.
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
  data[[time]] <- fold_time(out_t, out_lp, cuts)
  data[[other]] <- out_v
  data$.loop <- out_lp
  rekey_loops(data, out_lp, length(cuts) - 1L)
}

#' Get cutpoints along the time axis
#'
#' @param panel_params Panel params, e.g. as returned by
#'   `Coord$setup_panel_params()`.
#' @param scale Name of the time scale within `panel_params` (`"x"`, `"y"`,
#'   `"theta"`, ...).
#' @param loops A vector of time points at which to loop, or a waiver.
#' @param time_loops A duration to loop by (e.g. `"1 year"`), or a waiver.
#'   Takes precedence over `loops`.
#' @returns A vector of time cutpoints of the scale's own type. Loop `k` spans
#'   `[cuts[k], cuts[k + 1])`, so there is always one more cut than loop.
#'
#'   The final cut closes the last loop and is never a loop start in its own
#'   right: a trailing cut beyond the data would add a loop containing nothing,
#'   which `coord_calendar()` would then lay out as an empty row. A time value
#'   landing exactly on the final cut is clamped into the last loop by
#'   [loop_index()], drawing it at the right edge of the window rather than
#'   opening an otherwise empty loop for it.
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
    from <- mixtime::time_floor(time_range[1], time_loops)
    # `time_ceiling()` has already rounded past the end of the data, so this
    # sequence closes the last loop without extending beyond it.
    to <- mixtime::time_ceiling(time_range[2], time_loops)
    cuts <- seq(from, to, by = time_loops)
    if (length(cuts) < 2L) {
      # All of the data sits at a single instant on a loop boundary.
      cuts <- seq(from, by = time_loops, length.out = 2L)
    }
    unique(cuts)
  } else if (!is_waiver(loops) && !is.null(loops)) {
    cuts <- sort(unique(loops))
    n <- length(cuts)
    # Close the final loop with the width of the widest loop before it, falling
    # back to the data's overhang past the last loop point when that is larger
    # (or when there is only one loop point to go on).
    end <- if (n > 1L) cuts[n] + max(diff(cuts)) else cuts[n]
    # Compared rather than `max()`ed to avoid coercing the scale's time type.
    if (end < time_range[2]) {
      end <- time_range[2]
    }
    c(cuts, end)
  } else {
    # No looping: a single window spanning the whole range.
    c(time_range[1], time_range[2])
  }
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
#' New key values only need to be distinct per piece: `GeomPath` takes
#' `match(group, unique(group))` and `GeomRibbon` uses `id` directly. Deriving
#' them arithmetically from the original key and the loop index (rather than a
#' sequential run counter) is what makes independently-cut edges of the same
#' ring agree — `GeomRibbon` walks its lower edge in reverse, so anything
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
