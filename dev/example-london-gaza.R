# Example: geom_time_line() with London/Gaza DST transitions
#
# Demonstrates the dashed jumps geom_time_line() draws at DST transitions,
# using two zones whose spring-forward transitions land just a day apart:
# Gaza jumps first, 02:00 to 03:00 local on 28 Mar, then London jumps 01:00
# to 02:00 local on 29 Mar. The two series are combined into a single column
# with `dplyr::bind_rows()`, plotting mixed-timezone data in one layer -- see
# dev/mixed-tz-geom.md for the bug this used to trip over.

library(ggplot2)
library(dplyr)
library(mixtime)
library(patchwork)
pkgload::load_all(".", quiet = TRUE)

set.seed(1)

# Dummy hourly data spanning both zones' DST transitions. `datetime()` wraps
# each city's POSIXct in its own timezone as a `mixtime`, so `bind_rows()`
# combines them into one column that still carries a per-point timezone,
# rather than collapsing to a single shared tz the way plain POSIXct does.
# 72 hours (27-30 Mar) is enough to cover both zoom windows below plus the
# transition days themselves.
df <- bind_rows(
  Gaza = tibble(
    time = datetime(
      as.POSIXct("2026-03-27", tz = "Asia/Gaza") + (0:71) * 3600
    ),
    value = cumsum(rnorm(72))
  ),
  London = tibble(
    time = datetime(
      as.POSIXct("2026-03-27", tz = "Europe/London") + (0:71) * 3600
    ),
    value = cumsum(rnorm(72))
  ),
  .id = "city"
)

# Overview data: only the two calendar days that actually have a transition
# (28 and 29 Mar), each read in its own city's local calendar -- Gaza's and
# London's local midnights fall at different UTC instants, so each city is
# filtered separately rather than with one shared cutoff.
overview_df <- bind_rows(
  df |>
    filter(
      city == "Gaza",
      between(
        as.POSIXct(time),
        as.POSIXct("2026-03-28 00:00:00", tz = "Asia/Gaza"),
        as.POSIXct("2026-03-30 00:00:00", tz = "Asia/Gaza")
      )
    ),
  df |>
    filter(
      city == "London",
      between(
        as.POSIXct(time),
        as.POSIXct("2026-03-28 00:00:00", tz = "Europe/London"),
        as.POSIXct("2026-03-30 00:00:00", tz = "Europe/London")
      )
    )
)

# Local time (`tz = NA`) plots each point by its own zone's wall-clock
# reading, which is what shows the DST jump as a dashed segment. The default
# scale would instead pick UTC as the common chronon (since Gaza and London
# disagree on timezone), aligning both series by absolute instant and hiding
# both jumps.
overview <- ggplot(overview_df, aes(time, value, colour = city)) +
  geom_time_line() +
  scale_x_mixtime(
    time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA),
    time_breaks = mixtime::hours(6L),
    time_labels = "{cyc(day, month)} {cyc(hour, day)}:00"
  ) +
  labs(
    title = "geom_time_line(): DST transitions in local time",
    subtitle = "Gaza springs forward (28 Mar); London springs forward (29 Mar)",
    x = NULL,
    y = "value",
    colour = NULL
  )

# Zoomed windows around each transition -- the dashed segment is easy to miss
# against a day of hourly noise, but unmistakable a few hours out either
# side of the change.
zoom_scale <- function() {
  scale_x_mixtime(
    time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA),
    time_breaks = mixtime::hours(1L),
    time_labels = "{cyc(hour, day)}:00"
  )
}

gaza_zoom <- df |>
  filter(
    city == "Gaza",
    between(
      as.POSIXct(time),
      as.POSIXct("2026-03-27 22:00:00", tz = "Asia/Gaza"),
      as.POSIXct("2026-03-28 06:00:00", tz = "Asia/Gaza")
    )
  )
london_zoom <- df |>
  filter(
    city == "London",
    between(
      as.POSIXct(time),
      as.POSIXct("2026-03-28 22:00:00", tz = "Europe/London"),
      as.POSIXct("2026-03-29 06:00:00", tz = "Europe/London")
    )
  )

gaza_plot <- ggplot(gaza_zoom, aes(time, value)) +
  geom_time_line(colour = "#F8766D") +
  zoom_scale() +
  labs(title = "Gaza: EEST starts 28 Mar, 02:00 to 03:00", x = NULL)

london_plot <- ggplot(london_zoom, aes(time, value)) +
  geom_time_line(colour = "#00BFC4") +
  zoom_scale() +
  # scale_x_mixtime(time_breaks = mixtime::hours(1L), time_labels = "{cyc(hour, day)}:00") +
  labs(title = "London: BST starts 29 Mar, 01:00 to 02:00", x = NULL)
london_plot

p <- overview / (gaza_plot | london_plot)

print(p)
ggsave("dev/example-london-gaza.png", p, width = 9, height = 7, dpi = 150)
