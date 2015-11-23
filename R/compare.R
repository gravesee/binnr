#' Compare binnr models
#' 
#' @param ... Any number of \code{binnr.model} objects
#' 
#' @return A \code{data.frame} of model contributions merged by variable name
#' and sorted by the last, passed model in descending order
#' 
#' @details \code{compare} can be used to assess the effects of dropping variables
#' from a \code{binnr.model} and refitting. Dropped variables will show up as 'NA'
#' in the re-fit model. Newly added variables will show up as 'NA' in the previous
#' model
#' 
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
  
  nms <- if (is.null(names(conts))) paste0("Mod ", 1:length(conts)) else names(conts)
  names(out) <- nms
  out <- out[order(-out[,ncol(out)]),]
  round(out, digits = 5)
}
