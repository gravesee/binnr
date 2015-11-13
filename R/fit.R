
#' @export
fit <- function(x, ...) {
  UseMethod("fit")
}

#' @export
fit.binnr.model <- function(mod, data, y, nfolds=3, lower.limits=0, upper.limits=3, family="binomial", alpha=1, drop=F) {
  argg <- as.list(environment())
  do.call(fit.bin.list, c(list(bins=mod$bins), argg[-1]))
}

#' @export
fit.bin.list <- function(bins, data, y, nfolds=3, lower.limits=0, upper.limits=3, family="binomial", alpha=1, drop=F) {
  x <- predict(bins, data)
  fit <- cv.glmnet(x, y, nfolds=nfolds, lower.limits=lower.limits,
                   upper.limits=upper.limits, family=family, alpha=alpha)
  
  betas <- coef(fit, s="lambda.min")[,1]
  betas <- betas[betas != 0]
  
  # drop vars that aren't in the model and reorder to have the kept ones first
  k <- which(names(bins) %in% names(betas))
  bins <- notinmodel(bins) # reset which bins are in the model
  inmodel(bins) <- k
  
  if (drop) {
    drop(bins) <- seq_along(bins)[-k]
  }
  
  bins.reorder <- bin.list(c(bins[k], bins[-k]))
  mod <- binnr.model(bins.reorder, betas)
  mod$contribution <- sort(predict(mod, data, y, type="contribution"), decreasing = T)
  mod
}

#' @export
fit.segmented <- function(obj, data, y, seg, nfolds=3, lower.limits=0, upper.limits=3, family="binomial", alpha=1, drop=F) {
  argg <- as.list(environment())[-(1:5)]
  xs <- split(data, seg, drop=T)
  ys <- split(y, seg, drop=T)
  structure(mapply(fit, obj, xs, ys, MoreArgs = argg, SIMPLIFY = F), class = c("segmented"))
}
