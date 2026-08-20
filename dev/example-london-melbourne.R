# Example: geom_time_line() with London/Melbourne DST transitions
#
# Demonstrates the dashed jumps geom_time_line() draws at DST transitions,
# using two cities whose transitions fall a week apart: London's clocks
# spring forward (last Sunday of March) while Melbourne's fall back (first
# Sunday of April). The two series are combined into a single column with
# `dplyr::bind_rows()`, plotting mixed-timezone data in one layer -- see
# dev/mixed-tz-geom.md for the bug this used to trip over.

library(ggplot2)
library(dplyr)
library(mixtime)
library(patchwork)
pkgload::load_all(".", quiet = TRUE)

set.seed(1)

# Dummy hourly data spanning both cities' DST transitions. `datetime()` wraps
# each city's POSIXct in its own timezone as a `mixtime`, so `bind_rows()`
# combines them into one column that still carries a per-point timezone,
# rather than collapsing to a single shared tz the way plain POSIXct does.
df <- bind_rows(
  London = tibble(
    time = datetime(
      as.POSIXct("2026-04-04", tz = "Europe/London") + (0:47) * 3600
    ),
    value = cumsum(rnorm(48))
  ),
  Melbourne = tibble(
    time = datetime(
      as.POSIXct("2026-04-04", tz = "Australia/Melbourne") + (0:47) * 3600
    ),
    value = cumsum(rnorm(48))
  ),
  .id = "city"
)

# Local time (`tz = NA`) plots each point by its own city's wall-clock
# reading, which is what shows the DST jump as a dashed segment. The default
# scale would instead pick UTC as the common chronon (since London and
# Melbourne disagree on timezone), aligning both series by absolute instant
# and hiding both jumps.
overview <- ggplot(df, aes(time, value, colour = city)) +
  geom_time_line() +
  scale_x_mixtime(
    time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA),
    # time_breaks = mixtime::days(2L),
    # time_labels = "{cyc(day, month)} {cyc(hour, day)}:00"
  ) +
  labs(
    title = "geom_time_line(): DST transitions in local time",
    subtitle = "London springs forward (29 Mar); Melbourne falls back (5 Apr)",
    x = NULL,
    y = "value",
    colour = NULL
  )

overview
ggsave(
  "dev/example-london-melbourne-overview.png",
  overview,
  width = 30,
  height = 4,
  dpi = 150
)

# Zoomed windows around each transition -- the dashed segment is easy to miss
# against 20 days of hourly noise, but unmistakable a few hours out either
# side of the change.
zoom_scale <- function() {
  scale_x_mixtime(
    time_chronon = mixtime::cal_gregorian$hour(1L, tz = NA),
    time_breaks = mixtime::hours(1L),
    time_labels = "{cyc(hour, day)}:00"
  )
}

london_zoom <- df |>
  filter(
    city == "London",
    between(
      as.POSIXct(time),
      as.POSIXct("2026-03-28 22:00:00", tz = "Europe/London"),
      as.POSIXct("2026-03-29 06:00:00", tz = "Europe/London")
    )
  )
melbourne_zoom <- df |>
  filter(
    city == "Melbourne",
    between(
      as.POSIXct(time),
      as.POSIXct("2026-04-05 00:00:00", tz = "Australia/Melbourne"),
      as.POSIXct("2026-04-05 08:00:00", tz = "Australia/Melbourne")
    )
  )

london_plot <- ggplot(london_zoom, aes(time, value)) +
  geom_time_line(colour = "#F8766D") +
  zoom_scale() +
  labs(title = "London: BST starts 29 Mar, 01:00 to 02:00", x = NULL)

melbourne_plot <- ggplot(melbourne_zoom, aes(time, value)) +
  geom_time_line(colour = "#00BFC4") +
  zoom_scale() +
  labs(title = "Melbourne: AEDT ends 5 Apr, 03:00 to 02:00", x = NULL)

p <- overview / (london_plot | melbourne_plot)

print(p)
ggsave("dev/example-london-melbourne.png", p, width = 9, height = 7, dpi = 150)
