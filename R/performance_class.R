#' @include bin_class.R
NULL

setClassUnion("NumericOrFactor", members = c("numeric", "factor"))

#' generic method for bin_
#' @name bin_
#' @param .self the Performance object calling bin_
#' @param b a Bin object
setGeneric("bin_", def = function(.self, b, ...) callGeneric("bin_"))

#' Performnace reference class generator#'
#' @name Performance-class
#' @field y numeric or factor response variable
#' @field w numeric weight variable
#' @export Performance
#' @exportClass Performance
Performance <- setRefClass("Performance", fields = c(
  y = "NumericOrFactor",
  w = "numeric"))

Performance$methods(initialize = function(y=numeric(0), ..., w=rep(1, length(y))) {

  if (any(is.na(y))) {
    stop("y cannot have missing values", call. = FALSE)
  }

  if (length(y) != length(w)) {
    stop("y and w must be the same length", call. = FALSE)
  }

  callSuper(y=y, w=w, ...)
})

Performance$methods(bin = function(...) {
  stop("Must implement")
})

Performance$methods(summarize = function(...) {
  stop("Must implement")
})

Performance$methods(update = function(...) {
  stop("Must implement")
})

Performance$methods(plot = function(...) {
  stop("Must implement")
})

Performance$methods(summary = function(...) {
  stop("Must implement")
})

Performance$methods(sort_value = function(...) {
  stop("Must implement")
})
