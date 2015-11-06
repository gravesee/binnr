#' @export
compare <- function(...) {
  # check that all objects are binnr.models
  if (!all(sapply(list(...), is.binnr.model))) {
    stop("All objects must be binnr.models", call. = F)
  }
  
  conts <- lapply(list(...), '[[', 'contribution')
  
  out <- Reduce(function(a, b) {
    tmp <- merge(a, b, by=0, all=T)
    rownames(tmp) <- tmp$Row.names
    tmp$Row.names <- NULL
    tmp
  }, conts)
  
  names(out) <- sapply(substitute(list(...))[-1], deparse)
  out <- out[order(-out[,ncol(out)]),]
  round(out, digits = 5)
}