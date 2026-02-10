#' Position adjustments for timezones
#'
#' Timezone-aware position adjustments preserve the vertical position of a
#' geometry while adjusting its horizontal position display the time in
#' either civil or absolute time. It requires the time variables to be mapped to
#' either the x or y aesthetic to be represented in either a [`base::POSIXt`] or
#' [`mixtime::mixtime()`] format.
#'
#' These position adjustments handle time data with different timezone behaviors:
#' * `position_time_civil()` applies timezone offsets to position time to
#'    align times that would be experienced in each respective timezone.
#' * `position_time_absolute()` does not apply any timezone offsets, keeping
#'    time positioned in their exact relative timing across timezones.
#'
#' @section Practical usage:
#'
#' Using timezone information to position time differently reveals different
#' structures in the data. This is most evident when plotting multiple time
#' series across different timezones.
#'
#' Civil time (`position_time_civil()`) positions time as experienced by the
#' observer in their timezone (also known as local time). It will align 9AM in
#' Australia/Melbourne with 9AM in America/New_York, even though they occur at
#' different absolute times. This is useful for comparing behavioural patterns
#' that vary throughout times of the day, for example the amount of traffic
#' during morning rush hour as people commute to work.
#'
#' Absolute time (`position_time_absolute()`) positions time in a single
#' reference timezone (usually UTC), reflecting the exact time when events
#' happen. In absolute time, 9AM in Australia/Melbourne (AEST, UTC+10) will be
#' aligned with 7PM in America/New_York (EST, UTC-5) of the previous day. This
#' accurately reflects the equivalent timing of events across different
#' timezones, which is useful for comparing events that happen simultaneously
#' around the world, such as global financial market openings or international
#' conference calls.
#'
#' @examples
#'
#' df_tz_mixed <- data.frame(
#'   time = mixtime::mixtime(
#'     as.POSIXct("2023-10-01", tz = "Australia/Melbourne") + 0:23 * 3600,
#'     as.POSIXct("2023-10-01", tz = "America/New_York") + 0:23 * 3600
#'   ),
#'   value = c(cumsum(rnorm(12, 2)), cumsum(rnorm(12, -2)))
#' )
#' # Civil time positioning aligns times in the same local timezone
#' #ggplot(df_tz_mixed, aes(time, value)) +
#' #  geom_time_line(position = position_time_civil())
#' # Absolute time positioning aligns times in a common timezone (e.g. UTC)
#' #ggplot(df_tz_mixed, aes(time, value)) +
#' #  geom_time_line(position = position_time_absolute())
#'
#' # Positioning can also be used in other geoms
#' #ggplot(df_tz_mixed, aes(time, value)) +
#' #  geom_point(position = position_time_civil())
#'
#' @name position_time
#' @aliases position_time_civil position_time_absolute
#' @export position_time_civil position_time_absolute
NULL

#' @rdname position_time
#' @export
position_time_civil <- function() {
  PositionTimeCivil
}

#' @rdname position_time
#' @format NULL
#' @usage NULL
#' @export
PositionTimeCivil <- ggproto(
  "PositionTimeCivil",
  Position,
  required_aes = c("x|y"),

  default_aes = aes(xoffset = 0, yoffset = 0),

  # TODO: Check and error if multiple times are given in the scale, since
  # individual timezones from each calendar need to be used here.
  compute_panel = function(self, data, params, scales) {
    if (inherits(scales$x, "ScaleContinuousDatetime")) {
      data$xtimeoffset <- mixtime::tz_offset(scales$x$get_transformation()$inverse(
        data$x
      ))
      scales$x$timezone <- "UTC"
    }
    if (inherits(scales$y, "ScaleContinuousDatetime")) {
      data$ytimeoffset <- mixtime::tz_offset(scales$y$get_transformation()$inverse(
        data$y
      ))
      scales$y$timezone <- "UTC"
    }

    # Apply offset to x/y positions
    if (!is.null(data$xtimeoffset)) {
      data$x <- data$x + data$xtimeoffset
    }
    if (!is.null(data$ytimeoffset)) {
      data$y <- data$y + data$ytimeoffset
    }

    # TODO: Update mixtime scales to enforce labelling in UTC time
    # (since tz is handled by position offsets)

    data
  }
)

#' @rdname position_time
#' @export
position_time_absolute <- function() {
  PositionTimeAbsolute
}

#' @rdname position_time
#' @format NULL
#' @usage NULL
#' @export
PositionTimeAbsolute <- ggproto(
  "PositionTimeAbsolute",
  Position,
  required_aes = c("x|y"),

  compute_panel = function(self, data, params, scales) {
    data$xtimeoffset <- data$ytimeoffset <- 0
    data
  }
)
