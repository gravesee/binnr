
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
fit.bin.list <- function(bins, data=NULL, y=NULL, nfolds=3, lower.limits=0, upper.limits=3, family="binomial", alpha=1, drop=F) {
  if (is.null(y)) y <- bins[[1]]$data$y # grab the first y from a bin
  x <- predict(bins, data)
  
  fit <- cv.glmnet(x, y, nfolds=nfolds, lower.limits=lower.limits,
                   upper.limits=upper.limits, family=family, alpha=alpha)
  
  betas <- coef(fit, s="lambda.min")[,1]
  betas <- betas[betas != 0]
  
  # drop vars that aren't in the model and reorder to have the kept ones first
  
  k <- which(names(bins) %in% names(betas))
  bins <- notinmodel(bins) # reset which bins are in the model
  inmodel(bins) <- k
  
  if (drop) drop(bins) <- seq_along(bins)[-k]
  
  mod <- binnr.model(bins, betas)
  mod$contribution <- sort(predict(mod, data, y, type="contribution"), decreasing = T)
  mod
}

#' @export
fit.segmented <- function(obj, data=NULL, y=NULL, seg, nfolds=3, lower.limits=0, upper.limits=3, family="binomial", alpha=1, drop=F) {
  argg <- as.list(environment())[-(1:5)]
  
  # use data stored with objects if none is passed in
  if (is.null(data) & is.null(y)) {
    structure(lapply(obj, function(x) {do.call(fit, c(list(x), argg))}),
              class = c("segmented"))
  } else {
    # else use the passed in data
    xs <- split(data, seg, drop=T)
    ys <- split(y, seg, drop=T)
    structure(mapply(fit, obj, xs, ys, MoreArgs = argg, SIMPLIFY = F), class = c("segmented"))
  }
}
