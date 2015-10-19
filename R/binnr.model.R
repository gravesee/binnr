# functions and class def for binnr model

# coefficients must be named and include (Intercept)
#' @export
binnr.model <- function(bins, coefficients) {
  stopifnot(is.bin.list(bins))
  stopifnot(!is.null(coefficients))
  if (is.null(names(coefficients))) {
    stop("Coefficients must be a named vector")
  }
  if (is.na(coefficients['(Intercept)'])) {
    stop("Coefficients vector have '(Intercept)' element")
  }

  # filter out 0 coefs
  coefficients <- coefficients[coefficients != 0]
  
  # put intercept in first position
  i <- which(names(coefficients) == '(Intercept)')
  coefficients <- c(coefficients[i], coefficients[-i])
  
  # check names of all pieces
  if(!all(names(coefficients[-1]) %in% names(bins))) {
    stop("Not all coefficient names found in bins")
  }
  
  structure(list(
    bins=bins,
    coef=coefficients
  ), class="binnr.model")
}

calc.score <- function(newdata, coefs) {
  newdata %*% coefs[-1] + coefs[1]
}

calc.lr2 <- function(f, y) {
  f <- plogis(f)
  sum((y == 1)*log(f) + (y == 0)*log(1 - f))
}

calc.contributions <- function(newdata, coefs, y) {
  base <- calc.lr2(0, y)
  lr2 <- sapply(1:length(coefs), function(i) {
    if (i > 1) {
      coefs[i] <- 0
    }
    1 - (calc.lr2(calc.score(newdata, coefs), y)/base)
  })
  names(lr2) <- c("Base", names(coefs)[-1])
  lr2[1] - lr2[-1]
}

#' @export
predict.binnr.model <- function(object, newdata, y=NULL, type='score') {
  v <- names(object$coef[-1])
  missing <- v[!(v %in% names(object$bins))]
  if (length(missing) > 0) {
    stop(sprintf("Vars not found in data: %s", paste0(missing, collapse = ',')))
  }
  
  types <- c("score","contribution","woe","dist","bins","rcs")
  stopifnot(type %in% types)
  if (type == 'score') {
    binned <- predict(object$bins[v], newdata)
    calc.score(binned, object$coef)
  } else if (type == 'contribution') {
    if (is.null(y)) stop("Must provide y if calculating score contributions")
    stopifnot(nrow(newdata) == length(y))
    binned <- predict(object$bins[v], newdata)
    calc.contributions(binned, object$coef, y)
  } else{
    predict(object$bins, newdata, type, object$coef)
  }
}