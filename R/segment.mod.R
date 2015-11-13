#' @export
is.segmented <- function(x) {
  inherits(x, "segmented")
}

#' @export
predict.segmented <- function(obj, data, seg=NULL, type='score') {
  phat <- lapply(obj, predict, data, type)
  if (is.null(seg)) return(phat)
  do.call(cbind, phat)[cbind(1:nrow(data), match(seg, names(phat)))]
}
