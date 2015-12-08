#' Trim
#' 
#' Trim variables from binnr.models based on contribution
#' 
#' @param x A \code{binnr.model} or \code{segmented} object
#' @param contribution the contribution threshold. Variables below this
#' threshold will be flagged as dropped for subsequent \code{fit}
#' operations.
#' 
#' @return an object of the same type that was provided in \code{x}. Variables
#' below the passed contribution threshold are now flagged as dropped.
#' 
#' @export
trim <- function(x, contribution=0) {
  UseMethod("trim", x)
}

#' @export
trim.binnr.model <- function(mod, contribution) {
  d <- names(mod$contribution[mod$contribution < contribution])
  set.meta.attr(mod, "skip", d, TRUE)
}

#' @export
trim.segmented <- function(mods, contribution) {
  structure( lapply(mods, trim, contribution), class="segmented")
}