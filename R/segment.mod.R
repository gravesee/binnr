#' @export
is.segmented <- function(x) {
  inherits(x, "segmented")
}

#' @export
predict.segmented <- function(obj, data, seg=NULL) {
  phat <- lapply(obj, predict, data, type='score')
  if (is.null(seg)) return(phat)
  do.call(cbind, phat)[cbind(1:length(seg), match(seg, names(phat)))]
}

# TODO: print.segmented