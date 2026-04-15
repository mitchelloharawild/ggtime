# Inversion requires recollection of offset and regularity
# Warping between specific time points numeric 0-n for n warp points, decimals indicate time between warp points
transform_mixtime <- function(ptype = NULL) {
  # TODO: replace common_attr with ptype provided by scale
  force(ptype)

  # To original granularity
  to_mixtime <- function(x) {
    # Restore mt_time structure
    vec_restore(x, ptype)
  }
  # To common granularity (possibly with alignment?)
  # If local time is set, then an offset argument should be passed into the geom?

  # For aligning, find the range of times with a duration-based floor/ceiling.
  from_mixtime <- function(x) {
    if (!S7::S7_inherits(x, mixtime::class_mixtime)) {
      return(x)
    }
    if (is.null(ptype)) {
      ptype <<- x@x[[1L]][0L]
    }
    return(as.numeric(x))
    # if (length(attr(x, "v")) != 1L) {
    #   cli::cli_abort(
    #     "{.fun transform_mixtime} currently works with single granularity vectors only."
    #   )
    # }
    # common_attr <<- attributes(attr(x, "v")[[1L]])
    # return(as.numeric(x))
  }
  scales::new_transform(
    "mixtime",
    transform = "from_mixtime",
    inverse = "to_mixtime",
    breaks = scales::breaks_pretty() #,
    #domain = to_mixtime(c(-Inf, Inf))

    # TODO - define a format() method for mixtime
    # Which is what ScaleContinuous$get_labels() uses if labels = waiver()
    #format =
  )
}
