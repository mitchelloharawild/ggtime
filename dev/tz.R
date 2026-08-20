# scale_x_mixtime() with data from multiple timezones
#
# mixtime's <mixtime> vectors carry a timezone per element, so a single
# vector (or a stack of rbind()-ed data frames) can hold instants recorded
# in different zones. scale_x_mixtime() maps everything onto one common
# chronon before drawing, so the two series end up positioned by absolute
# instant (UTC), not by local wall-clock time.

library(ggplot2)
library(dplyr)
library(mixtime)
pkgload::load_all(".", quiet = TRUE) # or library(ggtime)

# Same local start time (6am), but recorded in each city's own timezone.
london <- tibble(
  city = "London",
  time = datetime(seq(
    as.POSIXct("2026-07-30 06:00:00", tz = "Europe/London"),
    by = "1 hour",
    length.out = 13
  )),
  temp = 15 + 5 * sin(seq(0, pi, length.out = 13))
)

melbourne <- tibble(
  city = "Melbourne",
  time = datetime(seq(
    as.POSIXct("2026-07-30 06:00:00", tz = "Australia/Melbourne"),
    by = "1 hour",
    length.out = 13
  )),
  temp = 8 + 4 * sin(seq(0, pi, length.out = 13))
)

print(london$time)
print(melbourne$time)

df <- bind_rows(london, melbourne)

# The default behaviour is to use UTC as the timezone for mixed-zone data
# (the Melbourne series is ~10 hours ahead of London in absolute UTC time)
ggplot(df, aes(time, temp, colour = city)) +
  geom_line() +
  scale_x_mixtime(
    time_breaks = hours(3L),
    time_labels = "{cyc(hour, day)}:00"
  )

# Alternatively, a naive-zone can be specified to show local wall-clock time
# (the London and Melbourne series are both 6am to 6pm local time))
ggplot(df, aes(time, temp, colour = city)) +
  geom_line() +
  scale_x_mixtime(
    time_breaks = hours(3L),
    time_labels = "{cyc(hour, day)}:00",
    time_chronon = cal_gregorian$second(1L, tz = NA)
  )
