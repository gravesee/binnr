#' @export
predict.bin.integer <- function(bin, x=NULL, type="woe", coef=NULL, mode="max") {
  if (type == "bins") {
    out <- predict.bin.numeric.bins(bin, x)
  } else if (type == "dist") {
    out <- predict.bin.numeric.dist(bin, x, coef, mode)
  } else if (type == "rcs") {
    out <- predict.bin.numeric.rcs(bin, x)
  } else { # "woe"
    out <- predict.bin.numeric.woe(bin, x)
  }
  names(out) <- NULL
  out
}

#' @export
predict.bin.numeric <- function(bin, x=NULL, type="woe", coef=NULL, mode="max") {
  if (type == "bins") {
    out <- predict.bin.numeric.bins(bin, x)
  } else if (type == "dist") {
    out <- predict.bin.numeric.dist(bin, x, coef, mode)
  } else if (type == "rcs") {
    out <- predict.bin.numeric.rcs(bin, x)
  } else { # "woe"
    out <- predict.bin.numeric.woe(bin, x)
  }
  names(out) <- NULL
  out
}

predict.bin.numeric.woe <- function(object, x, type=NULL, ...) {
  breaks <- object$core$breaks
  values <- object$core$values
  res <- values$var[cut(x, breaks, labels = FALSE)]
  
  exceptions <- names(values$exc)
  if (length(values$exc) != 0) { # exception values
    for (i in seq_along(exceptions)) {
      res[x == exceptions[i]] <- object$core$values$exc[i]
    }
  }
  
  res[is.na(res)] <- 0
  res
}

predict.bin.numeric.bins <- function(object, x, ...) {
  lvls <- do.call(c, lapply(object$core$counts, rownames))
  
  res <- lvls[cut(x, object$core$breaks, labels = FALSE)]
  res <- factor(res, levels=c(lvls, 'Missing'))
  
  exceptions <- names(object$core$values$exc)
  if (length(exceptions) != 0) { # exception values
    for (i in seq_along(exceptions)) {
      res[x == exceptions[i]] <- exceptions[i]
    }
  }
  res[is.na(res)] <- 'Missing'
  res
}

predict.bin.numeric.dist <- function(object, x, coef, mode="max") {
  res <- coef * predict.bin.numeric.woe(object, x)
  if(mode == "max") {
    min(res) - res
  } else if (mode == "neutral"){
    ifelse(min(res) == res, Inf, (0 - res))
  }
}

predict.bin.numeric.rcs <- function(object, x, ...) {
  if (is.null(object$rcs)) return(rep("", length(x)))
  rcs <- unlist(object$rcs)
  bins <- predict.bin.numeric.bins(object, x)
  rcs[as.numeric(bins)]
}
