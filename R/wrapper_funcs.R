#' @title bin
#'
#' @description The starting point for any binnr scorecard. The bin function
#' prepares variables for scorecard modeling by returning a
#' \code{\link{Scorecard-class}} object that is easily manipulated.
#'
#' @param data \code{\link{data.frame}} of independent predictors to discretize.
#' Can only bin factors and numeric columns. All other data types are removed
#' from the scorecard object.
#' @param y Response variable. Currently, only binary performance is supported.
#' @param w Weight variable. If omitted, a weight variable of all 1s is created.
#' @param min.iv The minimum information value to split a continuous variable.
#' @param min.cnt The minumum number of observations that must be in a bin after
#' any split.
#' @param min.res The minimum number of response observations that must be in a
#' bin after any split.
#' @param mono Monotonicity constraint
#' \itemize{
#'  \item{0 }{ No monotoncity enforced - the default.}
#'  \item{1 }{ Increasing monotoncically with the \code{y}}
#'  \item{-1 } {Decreasing monotoncically with the \code{y}}
#'  \item{2 }{ Either increasing or decreasing montonically with the \code{y}}
#' }
#' @param max.bin The maximum number of bins into which \code{x} is discretized.
#' @param exceptions A numeric vector of values to be excluded from the
#' discretization process.
#'
#' @details \code{bin} Is the workhorse of binnr Depending on the input
#' variable class type, it will behave differently. For numeric and integer
#' variables it will discretize using information value. The returned object is
#' of class \code{Continuous}. Factors are summarized and passed through as-is.
#' The returned object for factors if of class \code{Discrete}.
#'
#' The operations that can be performed on a bin depend on its class. Continuous
#' bins are subject to the constratins passed into the bin functions.
#' Furthermore, only adjacent levels may be collapsed. Discrete bins are not
#' constrained by the \code{bin} function parameters and non-adjacent levels
#' may be collapsed.
#'
#' Missing values are always held out of binning. They may be combined only if
#' the variable was binned as discrete.
#'
#' @return A \code{Scorecard} object with \code{data} variables discretized.
#' @export
bin <- function(data, y, w=rep(1, length(y)), min.iv=0.001, min.cnt=25,
                min.res=5, mono=0, max.bin=10, exceptions=numeric(0)) {

  ## only binary performance is supported currently
  perf <- Binary_Performance$new(y=y, w=w)

  sc <- Scorecard$new(data=data, performance=perf)

  sc$bin(min.iv=min.iv, min.cnt=min.cnt, min.res=min.res, mono=mono,
         max.bin=max.bin, exceptions=exceptions)

  return(sc)
}
