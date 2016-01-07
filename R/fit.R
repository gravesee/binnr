#' fit binnr objects
#' 
#' \code{fit} is the main engine for fitting binnr objects to data.
#' 
#' @param x
#' 
#' @details \code{fit} does a lot of heavy lifting for the user. It will apply
#' the weight of evidence transformations stored in the bins of the passed object.
#' If provided a dataset, it will apply these transformations on the dataset. If
#' not given a dataset, \code{fit} will use the data that was provided to create
#' the bins.
#' 
#' The transformed data is passed to \code{cv.glmnet} to fit a LASSO regression
#' model.
#' 
#' @param x a \code{bin.list}, \code{binnr.model}, or \code{segmented} object
#' @param data a \code{data.frame} with a names attribute or NULL
#' @param y a numeric vector the same length as the number of rows in \code{data}
#' or NULL
#' @param nfolds Number of cross-validation folds for \code{cv.glmnet}
#' @param lower.limits minimum value of coefficients in the glmnet model
#' @param upper.limits maximum value of coefficients in the glmnet model
#' @param family Link function family to pass on to \code{cv.glmnet}
#' @param alpha parameter passed to \code{cv.glmnet}. 0 == LASSO, 1 == Ridge
#' @param drop boolean indicator for whether to flag variables not in the model
#' as "dropped" which will prevent them from being fit in subsequent iterations.
#' 
#' @return a \code{binnr.model} or \code{segmented} object depending on
#' what was passed.

#' @export
fit <- function(x, data=NULL, y=NULL, nfolds=3, lower.limits=0, upper.limits=3,
                family="binomial", alpha=1, drop=F, fixed=F) {
  UseMethod("fit")
}

#' @export
fit.binnr.model <- function(mod, data=NULL, y=NULL, nfolds=3, lower.limits=0,
                            upper.limits=3, family="binomial", alpha=1, drop=F,
                            fixed=F) {
  argg <- as.list(environment())
  
  old <- names(which(get.meta.attr(mod$bins, "inmodel")))
  out <- do.call(fit.bin.list, c(list(bins=mod$bins), argg[-1]))
  out$bins <- set.meta.attr(out$bins, "new", names(out$bins), FALSE)
  
  new <- names(which(get.meta.attr(out$bins, "inmodel")))
  out$bins <- set.meta.attr(out$bins, "new", setdiff(new, old), TRUE)
  
  out
}

#' @export
fit.bin.list <- function(bins, data=NULL, y=NULL, nfolds=3, lower.limits=0,
                         upper.limits=3, family="binomial", alpha=1, drop=F,
                         fixed=F) {
  
  if (is.null(y)) y <- bins[[1]]$data$y # grab the first y from a bin
  x <- predict(bins, data)
  
  # create penalty factor
  pf <- rep(1, length(bins))
  if (fixed) {
    im <- get.meta.attr(bins, "inmodel")
    sk <- get.meta.attr(bins, "skip")
    pf[im & !sk] <- 0
  }
  
  fit <- glmnet::cv.glmnet(x, y, nfolds=nfolds, lower.limits=lower.limits, 
                           upper.limits=upper.limits, family=family,
                           alpha=alpha, penalty.factor=pf)
  
  betas <- glmnet::coef.cv.glmnet(fit, s="lambda.min")[,1]
  betas <- betas[betas != 0]
  
  # drop vars that aren't in the model and reorder to have the kept ones first
  k <- which(names(bins) %in% names(betas))
  
  # reset which bins are in the model
  bins <- set.meta.attr(bins, 'inmodel', names(bins), FALSE)
  bins <- set.meta.attr(bins, 'inmodel', names(bins)[k], TRUE)
  
  if (drop) {
    bins <- set.meta.attr(bins, "skip", names(bins)[-k], TRUE) 
  }
  
  bins.reorder <- bin.list(c(bins[k], bins[-k])) # put the mod vars first
  mod <- binnr.model(bins.reorder, betas)
  mod$contribution <- sort(predict(mod, data, y, type="contribution"),
                           decreasing = T)
  
  in.model <- names(which(get.meta.attr(mod$bins, 'inmodel')))
  mod$bins <- set.meta.attr(mod$bins, 'new', in.model, TRUE)
  mod
}

#' @export
fit.segmented <- function(obj, data=NULL, y=NULL, seg, nfolds=3, lower.limits=0,
                          upper.limits=3, family="binomial", alpha=1, drop=F,
                          fixed=T) {
  argg <- as.list(environment())[-(1:5)]
  
  # use data stored with objects if none is passed in
  if (is.null(data) & is.null(y)) {
    structure(
      lapply(obj, function(x) {do.call(fit, c(list(x), argg))}),
    class = c("segmented"))
  } else {
    # else use the passed in data
    xs <- split(data, seg, drop=T)
    ys <- split(y, seg, drop=T)
    structure(
      mapply(fit, obj, xs, ys, MoreArgs = argg, SIMPLIFY = F),
    class = c("segmented"))
  }
}
