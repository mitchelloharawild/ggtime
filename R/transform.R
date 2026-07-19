# Inversion requires recollection of offset and regularity
# Warping between specific time points numeric 0-n for n warp points, decimals indicate time between warp points
transform_mixtime <- function(ptype = NULL) {
  force(ptype)

  # To original granularity
  to_mixtime <- function(x) {
    # Restore mt_time structure
    attributes(x) <- attributes(ptype)
    mixtime:::new_time(
      x,
      chronon = attr(ptype, "chronon"),
      cycle = attr(ptype, "cycle")
    )
    x <- vctrs::vec_restore(x, ptype)

    if (!is.null(cycle <- attr(ptype, "cycle"))) {
      # Offset timezone for labelling purposes
      x <- x - tz_offset(x, tz_name(cycle))
    }
    x
  }

  from_mixtime <- function(x) {
    if (!S7::S7_inherits(x, mixtime::class_mixtime)) {
      return(x)
    }
    if (is.null(ptype)) {
      # TODO - better method for obtaining the ptype?
      ptype <<- x@x[[1L]][0L]
    }

    if (!is.null(cycle <- attr(ptype, "cycle"))) {
      # Set cyclical time points to be relative to the start of the cycle, so
      # they are plotted cyclically irrespective of linear time point.
      tz <- tz_name(ptype)
      x <- x - mixtime::time_floor(x, cycle)
    }

    return(as.numeric(x))
  }

  scales::new_transform(
    "mixtime",
    transform = "from_mixtime",
    inverse = "to_mixtime",
    breaks = scales::breaks_pretty()
  )
}
