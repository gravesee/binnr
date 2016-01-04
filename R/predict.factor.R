predict.bin <- function(bin, x=NULL, type=c('woe', 'dist', 'bins', 'rcs'), coef=NULL, mode=c('max','neutral')) {
  type <- match.arg(type)
  mode <- match.arg(mode)
  if (is.null(x)) x <- bin$data$x
  if (is.null(coef)) coef <- 1
  NextMethod(bin, object=NULL, x=x, type=type, coef=coef, mode=mode)
}

#' @export
predict.bin.factor <- function(bin, x=NULL, type="woe", coef=NULL, mode="max") {
  if (type == "bins") {
    out <- predict.bin.factor.bins(bin, x)
  } else if (type == "dist") {
    out <- predict.bin.factor.dist(bin, x, coef, mode)
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

predict.bin.factor.dist <- function(object, x, coef, mode="max") {
  res <- coef * predict.bin.factor.woe(object, x)
  if(mode == "max") {
    min(res) - res
  } else if (mode == "neutral"){
    ifelse(min(res) == res, Inf, (0 - res))
  }
}

predict.bin.factor.rcs <- function(object, x, ...) {
  if (is.null(object$rcs)) return(rep("", length(x)))
  rcs <- unlist(object$rcs)
  bins <- predict.bin.factor.bins(object, x)
  rcs[as.integer(bins)]
}
