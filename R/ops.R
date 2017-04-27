#' Expand Bins
#' @name Bin_plus
#' @aliases +,Bin,numeric-method
#' @param e1 A Continuous or Discrete bin
#' @param e2 The levels to expand
#' @export 
setMethod("+", signature = c("Bin", "numeric"), function(e1, e2) {
  e1$expand(e2)
})

#' Collapse Bins
#' @name Bin_minus
#' @aliases -,Bin,numeric-method
#' @param e1 A Continuous or Discrete bin
#' @param e2 the levels to collapse
#' @export
setMethod("-", signature = c("Bin", "numeric"), function(e1, e2) {
  e1$collapse(e2)
})

#' Neutralize Bins
#' @name Bin_not_equal
#' @aliases !=,Bin,numeric-method
#' @param e1 A Continuous or Discrete bin
#' @param e2 the levels to neutralize
#' @export
setMethod("!=", signature = c("Bin", "numeric"), function(e1, e2) {
  e1$neutralize(e2)
})
