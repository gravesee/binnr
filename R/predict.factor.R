#' @export
predict.bin.factor <- function(object, x, type="woe", coef=1) {
  if (type == "bins") {
    out <- predict.bin.factor.bins(object, x)
  } else if (type == "dist") {
    out <- predict.bin.factor.dist(object, x, coef)
  } else if (type == "rcs") {
    out <- predict.bin.factor.rcs(object, x)
  } else { # "woe"
    out <- predict.bin.factor.woe(object, x)
  }
  names(out) <- NULL
  out
}

predict.bin.factor.woe <- function(object, x, ...) {
  res <- numeric(length(x))
  ids <- match(x[!is.na(x)], object$core$breaks)
  res[!is.na(x)] <- object$core$values$var[ids]
  res[is.na(res)] <- 0
  res
}

predict.bin.factor.bins <- function(object, x, ...) {
  lvls <- unique(c(object$core$breaks, 'Missing'))
  res <- character(length(x))
  res[is.na(x)] <- 'Missing'
  res[!is.na(x)] <- unlist(object$core$breaks[x])
  factor(res, lvls)
}

predict.bin.factor.dist <- function(object, x, coef) {
  res <- coef * predict.bin.factor.woe(object, x)
  min(res) - res
}

predict.bin.factor.rcs <- function(object, x, ...) {
  if (is.null(object$rcs)) return(rep("", length(x)))
  rcs <- unlist(object$rcs)
  bins <- predict.bin.factor.bins(object, x)
  rcs[as.integer(bins)]
}
