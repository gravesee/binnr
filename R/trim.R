# functions for trimming variables from binnr.models
#' @export
trim <- function(x, contribution=0) {
  UseMethod("trim", x)
}

#' @export
trim.binnr.model <- function(mod, contribution) {
  d <- names(mod$contribution[mod$contribution < contribution])
  drop(mod) <- d
  mod
}

#' @export
trim.segmented <- function(mods, contribution) {
  structure(
    lapply(mods, trim, contribution),
  class="segmented")
}