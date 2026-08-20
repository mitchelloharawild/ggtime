# geom_time_line() and time-offset transitions: design notes
#
# This is a distilled version of the exploration that led here. Earlier,
# more exploratory versions of this file (probing `Geom$draw_layer()`,
# various rejected scale-column designs) are not reproduced in full below;
# where they mattered to the final design, the conclusion is kept and the
# dead-end code is summarised in prose instead.

library(ggplot2)
library(dplyr)
library(mixtime)
pkgload::load_all(".", quiet = TRUE) # or library(ggtime)

# =============================================================================
# The problem
# =============================================================================
#
# `geom_time_line()` draws a dashed segment wherever the "offset" applied to
# a time series changes -- most visibly at DST transitions, but the
# `[x/y]timeoffset` aesthetics were always meant to be general (see "Changing
# time offsets" in R/geom-time-line.R): a sensor clock resynced/recalibrated
# at a known instant is the same kind of event as a DST change, just not
# timezone-related.
#
# Today (`GeomTimeLine$draw_panel()`, R/geom-time-line.R:207-225) a jump is
# *detected* by comparing `xtimeoffset` between adjacent observed rows, and
# *positioned* at whichever x-values happened to be sampled either side of
# it. Both parts of that are wrong in ways that matter:
#
# 1. Imprecise placement. A DST transition happens at a specific instant
#    (`mixtime::tz_transitions()` gives it exactly), which will almost never
#    coincide with a sampled row. Coarse/irregular sampling visibly misplaces
#    the jump and gets the interpolated value at the jump wrong.
#
# 2. Some transitions are invisible to row-diffing entirely. If two
#    recalibrations land inside the same sampling gap (+5 at t=10, -5 at
#    t=15, with the nearest samples at t=0 and t=20), the endpoints agree --
#    there is nothing to diff, so the heuristic reports no transition at all,
#    even though two real, documented events occurred:

transitions_demo <- data.frame(time = c(10, 15), offset_before = c(0, 5), offset_after = c(5, 0))
data_demo <- data.frame(x = c(0, 20, 40, 60, 80, 100), y = c(0, 20, 40, 60, 80, 100))
breaks_demo <- c(-Inf, transitions_demo$time, Inf)
offsets_demo <- c(transitions_demo$offset_before[1], transitions_demo$offset_after)
data_demo$xtimeoffset <- offsets_demo[findInterval(data_demo$x, breaks_demo)]
data_demo$xtimeoffset[1] != data_demo$xtimeoffset[2]
#> [1] FALSE   <- heuristic sees no transition, despite two real ones in (0, 20)
#
# More generally, "the row where the offset changes" isn't even a coherent
# concept when the event's instant is known from something independent of
# the sampled series (a maintenance log, an NTP resync record) -- there may
# be no row anywhere near it.
#
# 3. Multi-zone data loses per-point timezone before any Geom runs.
#    `ScaleContinuousMixtime$transform_df()` (R/scale-time.R:286-336) computes
#    ONE shared chronon for the whole column via `chronon_common()`, and
#    `transform()` then re-expresses every point in it, discarding the
#    per-point tz that a combined multi-city `mixtime` column actually
#    carries beforehand:

df_mixed <- bind_rows(
  london = tibble(
    time = datetime(as.POSIXct("2026-07-30 06:00:00", tz = "Europe/London") + 0:3 * 3600)
  ),
  melbourne = tibble(
    time = datetime(as.POSIXct("2026-07-30 06:00:00", tz = "Australia/Melbourne") + 0:3 * 3600)
  ),
  .id = "city"
)
mixtime::tz_name(df_mixed$time) # correct per point, before any scale involved
#> [1] "Europe/London" ... "Australia/Melbourne" ...
b_lost <- ggplot_build(
  ggplot(df_mixed, aes(time, 1, colour = city)) +
    geom_point() +
    scale_x_mixtime(time_breaks = hours(3L), time_labels = "{cyc(hour, day)}:00")
)
sx_lost <- b_lost$layout$panel_scales_x[[1]]
mixtime::tz_name(sx_lost$get_transformation()$inverse(b_lost$data[[1]]$x))
#> [1] NA NA NA NA NA NA NA NA   <- gone, for every point, regardless of city
#
# The fix has to happen inside `transform_df()`, the last point at which the
# original, per-piece `mixtime` column (still heterogeneous) is available --
# `transform()`'s output and anything derived from it afterwards has already
# lost this information irrecoverably.

# =============================================================================
# Two designs that were considered and rejected
# =============================================================================
#
# * Stash `xtimezone` (name) and/or `xtime` (the whole original pre-collapse
#   value) as new columns, computed inside `transform_df()` before the
#   collapse, and read directly by `GeomTimeLine$draw_panel()`. Both were
#   validated working (chronon_common()'s tz loss is fixed by capturing the
#   per-point value before `self$transform()` runs), but both are
#   mixtime-specific -- there is no `xtimezone`/`xtime` for a plain POSIXct
#   sensor log with a hand-known recalibration event. Building the fix around
#   a mixtime-only column quietly narrows "time offsets" back down to
#   "timezone offsets", which undoes the generality `[x/y]timeoffset` was
#   already designed for.
#
# * Detect transitions by diffing `xtimeoffset` between adjacent rows (i.e.
#   keep today's heuristic, just fix where the offset comes from). Rejected
#   for the reason in problem #2 above: some real transitions produce no
#   diff between the nearest sampled rows at all, so detection-by-diffing is
#   unsound in general, not just imprecise.
#
# * Recover the tz from the scale on demand inside `Geom$draw_layer()`
#   (ggplot2 exposes the real per-panel Scale objects via
#   `layout$panel_scales_x`, confirmed working in ggplot2 4.0.3). This works
#   for a single named zone, but can't reconstruct *per-point* zones for
#   combined multi-city data -- that identity is already gone by the time any
#   Geom code runs (see problem #3). It also only reaches Geoms, when what's
#   actually needed is a second *table* of known transitions, which a Geom's
#   `draw_panel()`/`draw_layer()` has no clean way to accept and join against
#   groups -- that pushed the design towards a Stat instead (see below).

# =============================================================================
# Design
# =============================================================================
#
# ## 1. `[x/y]timeoffset` stays the single, general per-point offset aesthetic
#
# It already means "how much to correct this point by", with no assumption
# about *why* (timezone, drift correction, resync). That's exactly right and
# doesn't change. What's fixed is where it's populated from for mixtime data:
# inside `ScaleContinuousMixtime$transform_df()`, per point, before the
# `chronon_common()` collapse -- not via `position_time_civil()` (being
# deleted, and never reached `ScaleContinuousMixtime` anyway since
# `PositionTimeCivil$compute_panel()` only checks `ScaleContinuousDatetime`,
# R/position-time.R:86). An explicit `aes(xtimeoffset = )` mapping always
# passes through untouched -- this only fills the gap when nothing was
# supplied.
#
# `xtimeoffset` is also still needed for one more reason, independent of
# transitions: inverting a *local* x back to absolute time is a 1:many
# relationship during a fall-back transition ("02:30 Australia/Melbourne"
# happens twice on 2023-04-02, once at each offset) -- the offset is what
# disambiguates which occurrence a given row is. Knowing only the timezone
# name does not: both occurrences agree on the zone.
#
# ## 2. A `transitions` table, not aesthetics, carries WHEN offsets change
#
# Since a transition can't reliably be detected from the observed data (see
# problem #2), it has to be supplied as data in its own right: a table of
# known transitions, shaped exactly like what `mixtime::tz_transitions()`
# already returns:
#
#   data.frame(time, offset_before, offset_after)
#
# Implicit (timezone, the default) and explicit (recalibration, or anything
# else) transitions are then just two different *sources* filling the same
# shape -- nothing downstream needs to know which source it came from:
#
#   * Implicit: the scale calls `tz_transitions()` per distinct timezone
#     present in the data, over that data's absolute time range.
#   * Explicit: the user supplies a table in the same shape directly, e.g.
#     `stat_time_line(transitions = data.frame(time = ..., offset_before =
#     ..., offset_after = ...))`.
#   * The two can simply be combined with `rbind()` (e.g. DST transitions
#     plus a manually logged resync in the same series) -- same schema, so
#     there's no special-casing needed to support both at once.
#
# ## 3. An identifier aesthetic: `offset` alone doesn't say WHICH transitions
#    table rows apply
#
# This is the direct answer to "do we need something that identifies the
# timezone or calibration series": yes, and it needs to be a genuine
# per-point aesthetic (not just a Stat parameter), for two reasons:
#
#   * More than one identity can appear in the same panel/group. Combined
#     Melbourne+London data needs to know, per point, which city's
#     `tz_transitions()` calendar applies -- two zones can even share an
#     offset at a given moment while having completely different transition
#     calendars, so the offset alone can't stand in for identity. The same
#     is true for recalibration: two sensors in the same plot can have
#     independent recalibration histories.
#   * It can legitimately vary *within* a single line/group (e.g. a
#     travelling sensor/device crossing timezones), so it can't just be
#     folded into the existing `group`/`colour` aesthetics, which serve a
#     different purpose (which points are connected by a line).
#
# Call it `[x/y]timeid` rather than `[x/y]timezone`, to keep it as general as
# `xtimeoffset` already is: for the implicit/default case the scale populates
# it automatically as the timezone name (the same per-point value already
# needed to fix `xtimeoffset`, so this is a nearly-free addition to the same
# `transform_df()` patch); for the explicit case, the user maps whatever key
# their own `transitions` table uses (a sensor ID, say) via `aes()`.
#
# So: **offset + identifier**, exactly as suggested -- `xtimeoffset` gives
# the correction currently in force (and disambiguates local->absolute
# inversion), `xtimeid` selects which rows of `transitions` are relevant, and
# together they let a consumer reconstruct the full step function and find
# every boundary within a group's range, not just the ones a sample happened
# to straddle.
#
# Concretely, per point: `abs_x <- x - xtimeoffset`, filter `transitions` to
# `id == xtimeid` (or take the whole table if `xtimeid` is absent/constant),
# then `findInterval(abs_x, transitions$time)` places the point in the step
# function and identifies which entries in that group's absolute-time range
# were never touched by a sample -- exactly the ones that need an inserted
# row.

# ---------------------------------------------------------------------------
# Proof: the scale can emit both `xtimeoffset` and `xtimeid` from the same
# pre-collapse value, fixing the multi-city loss in problem #3
# ---------------------------------------------------------------------------

ScaleContinuousMixtimeTransitions <- ggproto(
  "ScaleContinuousMixtimeTransitions",
  ggtime:::ScaleContinuousMixtime,
  transform_df = function(self, df) {
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0 || is_waiver(df)) {
      return()
    }
    aesthetics <- intersect(self$aesthetics, names(df))
    if (length(aesthetics) == 0) {
      return()
    }
    if (is_waiver(self$time_chronon)) {
      self$time_chronon <- mixtime::chronon_common(do.call(c, df[aesthetics]))
    }

    # Both derived from the SAME pre-collapse column, before self$transform()
    # below erases per-point tz. Explicit aes() mappings for either are left
    # untouched -- these only fill the gap when nothing was supplied.
    offset_cols <- stats::setNames(
      lapply(aesthetics, function(a) {
        col <- paste0(a, "timeoffset")
        if (!is.null(df[[col]])) df[[col]] else mixtime::tz_offset(df[[a]])
      }),
      paste0(aesthetics, "timeoffset")
    )
    id_cols <- stats::setNames(
      lapply(aesthetics, function(a) {
        col <- paste0(a, "timeid")
        if (!is.null(df[[col]])) df[[col]] else mixtime::tz_name(df[[a]])
      }),
      paste0(aesthetics, "timeid")
    )

    res <- .mapply(self$transform, list(df[aesthetics], aesthetics), MoreArgs = NULL)
    names(res) <- aesthetics

    c(res, offset_cols, id_cols)
  }
)
scale_x_mixtime_transitions <- function(
  ...,
  time_chronon = waiver(),
  time_breaks = waiver(),
  time_labels = waiver()
) {
  ggplot2::continuous_scale(
    ggplot2:::ggplot_global$x_aes,
    palette = identity,
    breaks = if (!is_waiver(time_breaks)) ggtime:::breaks_time_seq(time_breaks) else scales::breaks_pretty(),
    labels = if (!is_waiver(time_labels)) function(self, x) format(x, format = time_labels) else waiver(),
    transform = ggtime:::transform_mixtime("identity"),
    guide = waiver(),
    position = "bottom",
    super = ggproto(
      NULL,
      ScaleContinuousMixtimeTransitions,
      time_chronon = time_chronon,
      align_discrete = aes_nudge()
    )
  )
}

b_kept <- ggplot_build(
  ggplot(df_mixed, aes(time, 1, colour = city)) +
    geom_point() +
    scale_x_mixtime_transitions(time_breaks = hours(3L), time_labels = "{cyc(hour, day)}:00")
)
b_kept$data[[1]][, c("x", "xtimeoffset", "xtimeid")]
#>            x xtimeoffset            xtimeid
#> 1  ...             0      Europe/London
#> ...                                     Australia/Melbourne
# -- correct per point, unlike sx_lost above.

# ---------------------------------------------------------------------------
# Can `xtimeoffset`/`xtimeid` just be `default_aes`, computed from `x`?
# ---------------------------------------------------------------------------
#
# No -- and specifically for the Geom, not just "not easily": every hook a
# Geom has is fixed by ggplot2 to run *after* scale transform, full stop.
# There is no declaration a Geom can make to move `use_defaults()` (or any
# other Geom hook) earlier. This is laid out precisely in ggplot2's own
# `Layer` data-flow diagram (`?Layer`, R/layer.R:195-326 in the checkout
# here) -- the full, fixed order for one layer:
#
#   Layer$compute_aesthetics()      # evaluate aes(), infer group
#   ScalesList$transform_df()       # <- Scale$transform_df() runs HERE
#   Layout$map_position()           # initial x/y scale mapping
#   Layer$compute_statistic()       # Stat$setup_data()/compute_layer()
#   Layer$map_statistic()           # after_stat()/stage() resolved
#   Layer$compute_geom_1()          # -> Geom$setup_data()
#   Layer$compute_position()        # Position$compute_layer()
#   Layout$map_position()           # final x/y mapping
#   ScalesList$map_df()             # non-position aesthetics mapped
#   Layer$compute_geom_2()          # -> Geom$use_defaults()  (default_aes filled in HERE)
#
# `Geom$use_defaults()` -- the thing that actually fills in `default_aes` --
# is literally the second-to-last step of the whole build, running after
# *both* rounds of scale mapping. `Geom$setup_data()` is earlier but is still
# well after `transform_df()` and after the Stat has already run. Neither
# Geom hook, nor even `Stat$setup_data()`/`compute_layer()` (also after
# `transform_df()`, see the diagram), ever sees `x` before the scale has
# transformed it. `ScalesList$transform_df()` is the only step in the entire
# diagram that runs before a scale's own transform -- because it *is* the
# transform. That's not a workaround or a missing `after_stat()` trick, it's
# the one point in a fixed, non-reorderable pipeline where the original
# per-piece time value is still available (same conclusion as "Where the
# multi-zone case actually breaks" above). A Geom or Stat declaring "run my
# default before scaling" isn't a real option ggplot2 exposes to extension
# packages at all -- only the Scale itself sits at that point.
#
# So what's implemented above already *is* the default -- it's just a scale
# default, not a `default_aes` one, with the same "explicit mapping always
# wins" semantics (`if (!is.null(df[[col]])) df[[col]] else <derived>`), just
# enforced in the one place capable of computing it.
#
# On "the actual defaults may need more input checking for if [x/y] are time
# types": within `ScaleContinuousMixtime` specifically, no runtime check is
# needed -- every aesthetic reaching that scale's `transform_df()` is, by
# construction, a `mixtime` object (that's the whole domain of the scale), so
# `tz_offset()`/`tz_name()` are always valid there. The check becomes real
# the moment this is generalised beyond `scale_x_mixtime()`: the very first
# examples in `geom_time_line()`'s own docs (`df_tz_back`, `df_tz_forward`)
# use plain POSIXct picked up by ggplot2's own `scale_x_datetime()`, not
# `scale_x_mixtime()` at all -- and `mixtime::tz_offset()`/`tz_name()` work
# on POSIXct too (used throughout this file's own demos). It'd be
# inconsistent for the mixtime path to get automatic `xtimeoffset`/`xtimeid`
# while the far more common plain-POSIXct path doesn't. A shared helper with
# an explicit type guard covers both:
#
#   derive_time_cols <- function(df, aesthetics) {
#     is_time <- vapply(df[aesthetics], function(x) {
#       inherits(x, "mixtime") || inherits(x, "POSIXct")
#     }, logical(1))
#     aesthetics <- aesthetics[is_time]  # silently skip anything else
#     if (length(aesthetics) == 0) return(list())
#     c(
#       stats::setNames(
#         lapply(aesthetics, function(a) {
#           col <- paste0(a, "timeoffset")
#           if (!is.null(df[[col]])) df[[col]] else mixtime::tz_offset(df[[a]])
#         }),
#         paste0(aesthetics, "timeoffset")
#       ),
#       stats::setNames(
#         lapply(aesthetics, function(a) {
#           col <- paste0(a, "timeid")
#           if (!is.null(df[[col]])) df[[col]] else mixtime::tz_name(df[[a]])
#         }),
#         paste0(aesthetics, "timeid")
#       )
#     )
#   }
#
# `ScaleContinuousMixtime$transform_df()` can call this unconditionally (the
# guard is a no-op there, always true). Extending the same call to
# `ScaleContinuousDatetime$transform_df()` is where the guard actually earns
# its keep, and is also the bigger open decision: `ScaleContinuousDatetime`
# is a ggplot2-owned class, so getting this behaviour onto plain POSIXct data
# means either patching it (fragile across ggplot2 versions) or having
# ggtime register its own datetime scale to take over for time-aesthetic
# geoms by default (bigger surface, but consistent with how `scale_x_mixtime`
# already works). Not resolved here -- worth its own decision once the
# mixtime-only version above is settled.

# ---------------------------------------------------------------------------
# Proof: offset + id + transitions table finds every boundary, including the
# ones a sample never straddled
# ---------------------------------------------------------------------------

resolve_transitions <- function(data, transitions) {
  # data: one group's rows, with $abs_x (absolute time) and $id already
  # resolved (abs_x <- x - xtimeoffset; id <- xtimeid).
  # transitions: data.frame(id, time, offset_before, offset_after).
  tr <- transitions[transitions$id %in% unique(data$id), ]
  tr <- tr[order(tr$time), ]

  breaks <- c(-Inf, tr$time, Inf)
  offsets <- c(if (nrow(tr)) tr$offset_before[1] else 0, tr$offset_after)
  data$xtimeoffset <- offsets[findInterval(data$abs_x, breaks)]
  data$is_transition <- FALSE

  in_range <- tr$time > min(data$abs_x) & tr$time < max(data$abs_x)
  tr <- tr[in_range, , drop = FALSE]
  if (nrow(tr) == 0) {
    return(data)
  }

  new_rows <- do.call(rbind, lapply(seq_len(nrow(tr)), function(i) {
    y0 <- stats::approx(data$abs_x, data$y, xout = tr$time[i])$y
    data.frame(
      abs_x = tr$time[i],
      y = y0,
      id = tr$id[i],
      xtimeoffset = c(tr$offset_before[i], tr$offset_after[i]),
      is_transition = TRUE,
      .side = c(0, 2) # arrival (old offset) sorts before departure (new offset)
    )
  }))
  data$.side <- 1
  out <- rbind(data, new_rows)
  out <- out[order(out$abs_x, out$.side), ]
  out$.side <- NULL
  out
}

# The multi-gap recalibration case from problem #2, now resolved correctly:
recal_data <- data.frame(abs_x = c(0, 20, 40, 60, 80, 100), y = c(0, 20, 40, 60, 80, 100), id = "sensor1")
recal_transitions <- data.frame(id = "sensor1", time = c(10, 15), offset_before = c(0, 5), offset_after = c(5, 0))
resolve_transitions(recal_data, recal_transitions)
#>   abs_x   y      id xtimeoffset is_transition
#> 1     0   0 sensor1           0         FALSE
#> 2    10  10 sensor1           0          TRUE   <- exact instant, not inferred
#> 3    10  10 sensor1           5          TRUE
#> 4    15  15 sensor1           5          TRUE   <- both events found, not just endpoints
#> 5    15  15 sensor1           0          TRUE
#> 6    20  20 sensor1           0         FALSE
#> ...

# The real multi-timezone case, using actual tz_transitions() per id, over a
# DST fold-back window (Melbourne's clocks wind back at 2023-04-02 03:00
# local -- absolute time is just epoch seconds, the same units abs_x is in):
t_local <- seq(
  as.POSIXct("2023-04-02 01:00:00", tz = "Australia/Melbourne"),
  as.POSIXct("2023-04-02 05:00:00", tz = "Australia/Melbourne"),
  by = "1 hour"
)
tz_data <- data.frame(abs_x = as.numeric(t_local), y = seq_along(t_local), id = "Australia/Melbourne")
tz_transitions_tbl <- mixtime::tz_transitions(min(t_local), max(t_local))
tz_transitions_tbl$id <- "Australia/Melbourne"
tz_transitions_tbl$time <- as.numeric(tz_transitions_tbl$time)
resolve_transitions(tz_data, tz_transitions_tbl)
# -> inserts the exact 2023-04-02 03:00:00->02:00:00 local transition point,
#    regardless of the 1-hour sampling grid.

# ---------------------------------------------------------------------------
# Are there other Geom-specific hooks that see data mapped to x/y?
# ---------------------------------------------------------------------------
#
# Only two Geom methods run early enough to be candidates at all --
# `setup_params(data, params)` and `setup_data(data, params)`, both at
# `compute_geom_1` (see the diagram above). Both are still strictly after
# `transform_df()`, `compute_statistic()` and `map_statistic()`, so the `x`/
# `y` they see are already numeric and any pre-transform time-typed value is
# already gone -- same conclusion as above, they don't help derive
# `xtimeoffset`/`xtimeid` itself. Everything later (`draw_key`, `draw_panel`,
# `draw_group`, `draw_layer`) is strictly downstream of `use_defaults()`
# (`compute_geom_2`), so even less useful for computing new columns.
#
# `setup_data()` is a different *kind* of hook than `use_defaults()` though --
# it can add/remove rows and columns, not just fill blanks -- which makes it
# a real (if weaker) alternative home for `resolve_transitions()`'s row
# insertion, instead of a Stat. Weaker for two concrete reasons:
#
#   * It's called once per whole layer, not pre-split per group the way
#     `Stat$compute_group()` is -- per-id/per-group segment logic would have
#     to be done by hand inside it.
#   * `Geom$setup_data(data, params)` does not receive `scales` at all.
#     `Stat$compute_group(data, scales, ...)` does. The implicit ("auto")
#     transitions source needs to query the x/y scale for
#     `get_time_transitions()`, so a Stat has a direct route to the scale
#     object that `Geom$setup_data()` simply doesn't -- it would need extra
#     plumbing (e.g. reaching into `params` for something stashed there
#     earlier) to get the same information.
#
# So this doesn't open a new place to derive the defaults (the Scale is still
# the only option for that), but it does reinforce, for a different reason
# than before, why Stat is the better fit for the transitions-resolution step
# specifically -- confirming the `StatTimeLine` sketch below over a
# `Geom$setup_data()`-based alternative.

# ---------------------------------------------------------------------------
# Better answer: inject the defaults into the MAPPING, from geom_time_line()
# itself -- IMPLEMENTED in R/geom-time-line.R
# ---------------------------------------------------------------------------
#
# All of the above assumed the defaulting logic has to live in a hook that
# runs on *data* somewhere in the pipeline. It doesn't have to -- it can run
# on the *mapping*, before any of that pipeline starts, if `geom_time_line()`
# builds an `xtimeoffset`/`xtimeid` mapping that reuses whatever expression
# the user already gave for `x`/`y`, and hands ggplot2 the augmented mapping
# instead of the original.
#
# The catch is *where* to do the reusing. `geom_time_line()`'s own `mapping`
# argument only has `x` in it if the user mapped `x` locally in that call --
# when `x` comes from `ggplot(df, aes(x = time))` and the geom call is just
# `geom_time_line()`, the geom constructor never sees an `x` expression to
# reuse at all, because plot-level inheritance is merged in later. The first
# version of this (kept only in prior git history now) subclassed
# `ggplot2:::Layer` and overrode `setup_layer()`, since that's the step that
# merges layer + plot mapping. It worked, but `Layer` is unexported and
# `layer(layer_class = )`'s own roxygen says "intended for ggplot2 internal
# use only" -- more fragile than necessary for something with a fully public
# alternative.
#
# The public alternative: `ggplot2::ggplot_add()`. It's the documented
# extension point ggplot2 dispatches on whenever `plot + object` runs, and it
# receives the *plot* -- `plot$mapping` is right there, no need to subclass
# anything internal. `geom_time_line()` marks its own return value with an
# extra (prepended, so nothing else about the object changes) S3 class, and
# `ggplot_add.<that class>()` peeks at the merged mapping (own + inherited,
# only if `inherit.aes`) to see what `x`/`y` will resolve to, adds
# `xtimeoffset`/`xtimeid` (if not already present, locally or inherited) to
# the layer's *own* mapping only, then falls through to the normal handling
# via `NextMethod()` -- the usual `inherit.aes` merge in `Layer$setup_layer()`
# still runs afterwards, untouched, so nothing else about how the layer gets
# added changes:
#
#   geom_time_line <- function(mapping = NULL, ...) {
#     l <- layer(geom = GeomTimeLine, mapping = mapping, ...)
#     class(l) <- c("ggtime_time_line_layer", class(l))
#     l
#   }
#
#   ggplot_add.ggtime_time_line_layer <- function(object, plot, object_name) {
#     mapping <- object$mapping %||% aes()
#     inherited <- if (isTRUE(object$inherit.aes)) plot$mapping else aes()
#     peek <- c(mapping, inherited[setdiff(names(inherited), names(mapping))])
#     mapping <- inject_time_aes(mapping, peek, "x")
#     mapping <- inject_time_aes(mapping, peek, "y")
#     object$mapping <- mapping
#     NextMethod()
#   }
#
# `inject_time_aes(mapping, peek, aesthetic)` reads the `x`/`y` expression
# from `peek` (so it sees inherited mappings too) but only ever writes into
# `mapping` (the layer's own), and only if `peek` doesn't already have the
# offset/id aesthetic from *either* source -- an explicit mapping, local or
# inherited, is always left alone:
#
#   inject_time_aes <- function(mapping, peek, aesthetic) {
#     expr <- peek[[aesthetic]]
#     if (is.null(expr) || !rlang::is_quosure(expr)) return(mapping)
#     offset_aes <- paste0(aesthetic, "timeoffset")
#     id_aes <- paste0(aesthetic, "timeid")
#     if (is.null(peek[[offset_aes]])) {
#       mapping[[offset_aes]] <- rlang::new_quosure(
#         rlang::expr(derive_timeoffset(!!rlang::quo_get_expr(expr))),
#         env = rlang::quo_get_env(expr)
#       )
#     }
#     if (is.null(peek[[id_aes]])) {
#       mapping[[id_aes]] <- rlang::new_quosure(
#         rlang::expr(derive_timeid(!!rlang::quo_get_expr(expr))),
#         env = rlang::quo_get_env(expr)
#       )
#     }
#     mapping
#   }
#
# `derive_timeoffset()`/`derive_timeid()` are ordinary functions with the
# type guard *inside* them, since the injector can't know in advance whether
# `x`/`y` will actually be time-typed -- it always tries, and the guard
# becomes an ordinary runtime `if`, evaluated against real data, not a
# special case the injector itself has to handle:
#
#   is_time_typed <- function(x) is_mixtime(x) || inherits(x, "POSIXct")
#   derive_timeoffset <- function(x) if (is_time_typed(x)) mixtime::tz_offset(x) else 0
#   derive_timeid <- function(x) if (is_time_typed(x)) mixtime::tz_name(x) else NA_character_
#
# (Caught two real bugs building this, both from only checking
# `xtimeoffset`/`xtimeid` in `ggplot_build()$data` and never actually
# rendering a plot with a plain non-time `y`:
#
#   * `mixtime` objects have class `"mixtime::mixtime"` via S7, not plain
#     `"mixtime"` -- `inherits(x, "mixtime")` silently returns `FALSE` for
#     every real `mixtime` value. Use `is_mixtime()` (imported from mixtime).
#   * `derive_timeoffset()`'s non-time-typed fallback has to be `0`, not
#     `NA`. `PositionTimeCivil$compute_panel()` (R/position-time.R, unchanged
#     by this) unconditionally does `data$y <- data$y + data$ytimeoffset`
#     whenever a `ytimeoffset` column is present at all -- previously that
#     column only ever existed when `y` really was a datetime scale. Now that
#     the mapping injection always adds it, a plain numeric `y` (the common
#     case: most time series plot value against time, not time against time)
#     got `NA` added to every point, silently dropping the whole layer.
#     `0` is a true no-op add, and matches the "no offset" sentinel
#     `PositionTimeAbsolute` already uses (`xtimeoffset <- ytimeoffset <- 0`).
# )
#
# Verified against real `geom_time_line()` (not just a `geom_time_line2()`
# stand-in): (1) `x` mapped locally, (2) `x` inherited from
# `ggplot(df, aes(x = time))`, (3) an explicit `aes(xtimeoffset = 999)`
# (local or plot-level) left untouched while `xtimeid` still auto-derives,
# (4) plain POSIXct through the default `scale_x_datetime()` -- all four
# produce correct per-point `xtimeoffset`/`xtimeid`, and all three of
# `geom_time_line()`'s own documented examples render without warnings (the
# `NA`-offset bug above showed up as `ggplot_build()` silently dropping every
# row of the plot, "Removed 12 rows containing missing values").
#
# Unlike the `ScaleContinuousMixtime`-based version further up, this
# evaluates against the raw data *before any scale is involved*, so the same
# code works for plain `scale_x_datetime()`/POSIXct as well as
# `scale_x_mixtime()` -- no scale subclass, no need to patch or replace
# `ScaleContinuousDatetime`. It doesn't touch the separate
# Stat-vs-`setup_data()` question below for *where the transitions table
# gets resolved into rows* -- that's a different problem (when to insert
# dashed-jump rows) from this one (defaulting `xtimeoffset`/`xtimeid`).

# =============================================================================
# Architecture: a Stat, not Geom$draw_layer()
# =============================================================================
#
# A Geom's `draw_panel()`/`draw_layer()` only ever sees the one already-built
# `data` for its layer -- there's no clean way for it to accept and join a
# second table like `transitions`. A Stat's `compute_group()` already takes
# `scales` *and* arbitrary fixed params, so `transitions` fits naturally as
# an ordinary stat parameter (default `"auto"`, or a user-supplied table):
#
#   StatTimeLine <- ggproto("StatTimeLine", Stat,
#     compute_group = function(data, scales, transitions = "auto") {
#       tr <- if (identical(transitions, "auto")) {
#         scales$x$get_time_transitions(range(data$abs_x))  # per id, via tz_transitions()
#       } else {
#         transitions  # explicit, matched to data$id via resolve_transitions()
#       }
#       resolve_transitions(data, tr)
#     }
#   )
#
# `scale$get_time_transitions()` (the implicit side) needs the scale to
# retain, during `transform_df()`, which id/tz applied over which absolute
# range -- an accumulation of `(id, range)` segments across calls, since
# `transform_df()` already sees the pre-collapse column each time it runs.
#
# With `resolve_transitions()` flagging inserted rows explicitly
# (`is_transition`), `GeomTimeLine$draw_panel()` no longer infers jumps from
# `xtimeoffset` differences at all -- it draws a dashed segment at every row
# the Stat marked. That's both simpler than today's heuristic and correct for
# the cases the heuristic silently got wrong (multiple transitions per
# sampling gap, net-zero-diff transitions, imprecise placement).

# =============================================================================
# Open questions / next steps
# =============================================================================
#
# * Matching an explicit `transitions` table to groups/ids when there are
#   multiple series: requires a `group`/`id` column on `transitions`,
#   applied to all ids when absent.
# * `scale$get_time_transitions()` needs real segment-tracking added to
#   `ScaleContinuousMixtimeTransitions$transform_df()` (sketched above, not
#   yet implemented) -- accumulating `(id, range)` across possibly multiple
#   `transform_df()` calls (multiple layers sharing one scale).
# * Assumes seconds-granularity coordinates. Coarser chronons (minute/hour)
#   need `mixtime::chronon_convert()` to translate `tz_transitions()`'s
#   second-based `time`/offsets into the scale's actual units.
# * Transitions outside the panel's x-range should be dropped before
#   insertion (no need to insert rows the coord will clip anyway).
# * Caching: `tz_transitions()`/`get_time_transitions()` shouldn't be
#   recomputed on every redraw (resize, etc.) if the group's absolute time
#   range hasn't changed.
# * This only covers the x aesthetic; `GeomTimeLine` also supports
#   `ytimeoffset` for a time-valued y and would need the same treatment
#   (`ytimeid`, `layout$panel_scales_y`) mirrored throughout.
# * No production code has changed yet for any of this -- `R/scale-time.R`
#   and `R/geom-time-line.R` still work as documented; this file is only the
#   design record.
