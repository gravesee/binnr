#' Expand Bins
#' @name Bin_plus
#' @param e1 A Continuous or Discrete bin
#' @param e2 The levels to expand
setMethod("+", signature = c("Bin", "numeric"), function(e1, e2) {
  e1$expand(e2)
})

#' Collapse Bins
#' @name Bin_minus
#' @param e1 A Continuous or Discrete bin
#' @param e2 the levels to collapse
setMethod("-", signature = c("Bin", "numeric"), function(e1, e2) {
  e1$collapse(e2)
})

#' Neutralize Bins
#' @name Bin_not_equal
#' @param e1 A Continuous or Discrete bin
#' @param e2 the levels to neutralize
setMethod("!=", signature = c("Bin", "numeric"), function(e1, e2) {
  e1$neutralize(e2)
})
