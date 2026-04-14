# #' @keywords internal
# #' @importFrom scales ContinuousRange
# MixtimeRange <- R6::R6Class(
#   "MixtimeRange",
#   inherit = scales::ContinuousRange,
#   list(
#     train = function(x, call = caller_env()) {
#       self$range <- train_mixtime(x, self$range, call = call)
#     },
#     reset = function() self$range <- NULL
#   )
# )

# #' @keywords internal
# train_mixtime <- function(new, existing = NULL, call = caller_env()) {
#   if (is.null(new)) {
#     return(existing)
#   }

#   new <- try_fetch(
#     suppressWarnings(range(new, na.rm = TRUE, finite = TRUE)),
#     error = function(cnd) new
#   )

#   # Might need casting to numeric because some `new` vectors can misbehave when
#   # combined with a NULL `existing` (#369)
#   suppressWarnings(range(
#     vctrs::vec_c(existing, new),
#     na.rm = TRUE,
#     finite = TRUE
#   ))
# }
