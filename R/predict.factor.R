#' @export
predict.bin.factor <- function(bin, x=NULL, type="woe", coef=NULL) {
  if (is.null(x)) x <- bin$data$x
  if (is.null(coef)) coef <- 1
  if (type == "bins") {
    out <- predict.bin.factor.bins(bin, x)
  } else if (type == "dist") {
    out <- predict.bin.factor.dist(bin, x, coef)
  } else if (type == "rcs") {
    out <- predict.bin.factor.rcs(bin, x)
  } else { # "woe"
    out <- predict.bin.factor.woe(bin, x)
  }
  names(out) <- NULL
  out
}

# todo: Clean this cluster up...
predict.bin.factor.woe <- function(object, x, ...) {
  res <- numeric(length(x))
  ids <- match(x[!is.na(x)], names(object$core$breaks))
  ids <- unlist(object$core$breaks)[ids]
  ids <- match(ids, names(object$core$values$var))
  res[!is.na(x)] <- object$core$values$var[ids]
  res[is.na(x)] <- 0
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
