# functions and class def for binnr model

#' @export
is.binnr.model <- function(x) {
  inherits(x, "binnr.model")
}

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
  nms <- names(coefficients)[-1]
  
  # put intercept in first position
  i <- which(names(coefficients) == '(Intercept)')
  coefficients <- c(coefficients[i], coefficients[-i])
  
  # check names of all pieces
  if(!all(nms %in% names(bins))) {
    stop("Not all coefficient names found in bins")
  }
  
  # warn of large coefficients
  if (any(abs(coefficients[-1]) > 3)) {
    for (i in which(abs(coefficients[-1]) > 3)) {
      warning(sprintf("Coefficient > 3: %s %0.3f", nms[i],
                      coefficients[-1][i]), call. = F)
    }
  }
  
  structure(list(
    bins=bins,
    coef=coefficients
  ), class="binnr.model")
}

#' @export
predict.binnr.model <- function(obj, data, y=NULL, type='score') {
  v <- names(obj$coef[-1])
  missing <- v[!(v %in% names(obj$bins))]

  if (length(missing) > 0) {
    stop(sprintf("Vars not found in data: %s", paste0(missing, collapse = ',')))
  }
  
  types <- c("score","contribution","woe","dist","bins","rcs")
  stopifnot(type %in% types)
  if (type == 'score') {
    binned <- predict(obj$bins[v], data)
    calc.score(binned, obj$coef)
  } else if (type == 'contribution') {
    if (is.null(y)) stop("Must provide y if calculating score contributions")
    stopifnot(nrow(data) == length(y))
    binned <- predict(obj$bins[v], data)
    calc.contributions(binned, obj$coef, y)
  } else{
    predict(obj$bins[v], data, type, obj$coef)
  }
}

#' @export
print.binnr.model <- function(mod) {
  print(mod$bins)
  out <- merge(mod$coef[-1], mod$contribution, by=0, all=T)
  rownames(out) <- out$Row.names
  out$Row.names <- NULL
  out <- with(out, out[order(-out[,2]),])
  
  cnt <- out[,2]/max(out[,2]) * 10
  out[,3] <- sapply(cnt, function(i) paste(rep("*", i), collapse=""))
  out[,3] <- format(out[,3], justify = "left")
  colnames(out) <- c("Coefficient", "Contribution", "Importance")
  cat("\n")
  print(out) 
}