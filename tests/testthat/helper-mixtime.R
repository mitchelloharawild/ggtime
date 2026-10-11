# TODO: Remove once ggtime requires a mixtime release newer than 0.3.0. These
# tests use functionality from the development version of mixtime.
skip_if_cran_mixtime <- function() {
  skip_if_not(
    utils::packageVersion("mixtime") > "0.3.0",
    "requires the development version of mixtime"
  )
}
