#' @export
`-.bin.factor` <- function(e1, e2) {
  x <- e1$data$x
  breaks <- e1$core$breaks
  f <- breaks %in% rownames(e1$core$counts$var)[e2]
  breaks[f] <- paste(names(breaks)[f], collapse=',')
  levels(x) <- unlist(breaks)
  
  b <- bin.factory(x, e1$data$y, e1$name, breaks, e1$opts)
  update.bin(b, e1)
}

#' @export
`+.bin.factor` <- function(e1, e2) {
  x <- e1$data$x
  breaks <- e1$core$breaks
  f <- breaks %in% breaks[e2]
  breaks[f] <- levels(e1$data$x)[f]
  levels(x) <- unlist(breaks)
  b <- bin.factory(x, e1$data$y, e1$name, breaks, e1$opts)
  update.bin(b, e1)
}

update.bin <- function(new.bin, old.bin){
  new.bin$data$x <- old.bin$data$x
  new.bin$history <- old.bin
  new.bin$meta <- old.bin$meta
  new.bin$meta$type <- "MANUAL"
  new.bin$meta$modified <- date()
  new.bin
}