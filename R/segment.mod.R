#' @export
is.segmented <- function(x) {
  inherits(x, "segmented")
}


# TODO: print.segmented
predict.segmented <- function(object, data=NULL, y=NULL, seg=NULL, type="score", mode="max") {
  pred <- lapply(object, predict, data=data, type=type, mode=mode, y=y)
  
  # if no segment is passed return all seg predictions on all 
  if (is.null(seg) | type != "score") return(pred)
  
  idx <- cbind(seq_along(seg), match(seg, names(pred)))
  do.call(cbind, pred)[idx]
}
