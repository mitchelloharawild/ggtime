# Gallery: geom_time_line() dashed-transition behaviour
#
# Small, focused panels isolating one aspect of how geom_time_line() draws
# dashed jumps at time offset transitions each: the direction of the offset
# change (forward/backward), whether observations are sampled close to the
# transition or far from it, and what happens once the transition is no
# longer a local-time artefact (absolute time). Every panel plots a trend of
# 1 unit per hour, so the dashed jump is easy to tell apart from whatever
# else varies between panels.

library(ggplot2)
library(mixtime)
library(patchwork)
pkgload::load_all(".", quiet = TRUE)

set.seed(1)

mel <- "Australia/Melbourne"

# Melbourne's 2024 transitions (from mixtime::tz_transitions()):
#   2024-04-07 03:00 -> 02:00 local (AEDT -> AEST, offset +11 -> +10): backward
#   2024-10-06 02:00 -> 03:00 local (AEST -> AEDT, offset +10 -> +11): forward

# Hourly points are built by adding real seconds to a starting instant, so
# they land on genuine instants regardless of what the local clock reads --
# exactly what's needed to sample across a transition.
hourly <- function(start, n) {
  time <- as.POSIXct(start, tz = mel) + (0:(n - 1)) * 3600
  data.frame(time = time, value = seq_along(time))
}

# Just the first and last instant of `hourly(start, n)`, with nothing sampled
# in between -- the same span as the dense panels, but without the
# intermediate hourly points that would otherwise sit close to the
# transition.
endpoints <- function(start, n) {
  time <- as.POSIXct(start, tz = mel) + c(0, n - 1) * 3600
  data.frame(time = time, value = c(1, 2))
}

# Sub-hourly points (every 15 minutes) built the same way as `hourly()`, so
# some observations land within the hour either side of a transition instead
# of only right at its boundary -- and, for a backward transition, within the
# repeated local hour itself, since real instants pass through it twice.
# `value` keeps the same 1-unit-per-hour trend as `hourly()`, with a random
# walk added on top so the series reads like noisy real observations rather
# than a perfectly straight line.
overlapping <- function(start, hours, noise = 0.6) {
  step_minutes <- 15
  n <- hours * 60 / step_minutes + 1
  time <- as.POSIXct(start, tz = mel) + (0:(n - 1)) * step_minutes * 60
  trend <- (0:(n - 1)) * step_minutes / 60
  data.frame(time = time, value = trend + cumsum(rnorm(n, sd = noise)))
}

hourly_scale <- function() {
  scale_x_mixtime(
    time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA),
    time_breaks = mixtime::hours(2L),
    time_labels = "{cyc(hour, day)}:00"
  )
}

absolute_scale <- function() {
  scale_x_mixtime(
    time_chronon = mixtime::cal_gregorian$hour(1L, tz = "UTC"),
    time_breaks = mixtime::hours(2L),
    time_labels = "{cyc(hour, day)}:00"
  )
}

panel <- function(df, scale, title, subtitle = NULL) {
  ggplot(df, aes(time, value)) +
    geom_time_line(linewidth = 0.8) +
    geom_point(size = 1.3) +
    scale +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(size = 10, face = "bold"),
      plot.subtitle = element_text(size = 8)
    )
}

# 1. No transition: a quiet day, nothing to jump over.
no_transition <- panel(
  hourly("2024-06-10 20:00:00", 10),
  hourly_scale(),
  "No transition",
  "Solid line -- no offset change nearby"
)

# 2. Forward, dense sampling: sub-hourly points straddle the transition
#    closely, some within the hour either side of it.
forward_dense_df <- overlapping("2024-10-05 20:00:00", 9)
forward_dense <- panel(
  forward_dense_df,
  hourly_scale(),
  "Forward, observations overlapping",
  "AEST -> AEDT, 02:00 skips to 03:00"
)

# 3. Forward, sparse sampling: same span as (2), but with nothing sampled
#    near the transition -- the jump is still inserted at the exact instant.
forward_sparse <- panel(
  endpoints("2024-10-05 20:00:00", 10),
  hourly_scale(),
  "Forward, observations not overlapping",
  "Same span as (2), no samples nearby"
)

# 4. Backward, dense sampling: sub-hourly points, some of which fall in the
#    repeated local hour (02:00-03:00 occurs twice).
backward_dense <- panel(
  overlapping("2024-04-06 21:00:00", 9),
  hourly_scale(),
  "Backward, observations overlapping",
  "AEDT -> AEST, 03:00 repeats as 02:00"
)

# 5. Backward, sparse sampling.
backward_sparse <- panel(
  endpoints("2024-04-06 21:00:00", 10),
  hourly_scale(),
  "Backward, observations not overlapping",
  "Same span as (4), no samples nearby"
)

# 6. Same forward-dense data, but in absolute time: nothing to jump over once
#    positions are placed by instant rather than by local wall-clock reading.
absolute <- panel(
  forward_dense_df,
  absolute_scale(),
  "Absolute time",
  "Same data as (2), no local offset applied"
)

gallery <- (no_transition | forward_dense | forward_sparse) /
  (absolute | backward_dense | backward_sparse) +
  plot_annotation(
    title = "geom_time_line(): dashed transition behaviour",
    subtitle = "Dashes mark where the local time offset changes"
  )

print(gallery)
ggsave("dev/gallery-transitions.png", gallery, width = 13, height = 6, dpi = 150)
