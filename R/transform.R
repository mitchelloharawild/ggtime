# Inversion requires recollection of offset and regularity
# Warping between specific time points numeric 0-n for n warp points, decimals indicate time between warp points
transform_mixtime <- function(ptype = NULL) {
  force(ptype)

  # To original granularity
  to_mixtime <- function(x) {
    # Restore mt_time structure
    vec_restore(x, ptype)
  }

  from_mixtime <- function(x) {
    if (!S7::S7_inherits(x, mixtime::class_mixtime)) {
      return(x)
    }
    if (is.null(ptype)) {
      # TODO - better method for obtaining the ptype?
      ptype <<- x@x[[1L]][0L]
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
