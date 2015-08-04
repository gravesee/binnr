bin <- function(x, y, min.iv=.025, min.cnt = NULL, max.bin=10, mono=0, sv=NULL) {
  stopifnot(length(x) == length(y))
  stopifnot(mono %in% c(-1,0,1))
  stopifnot(max.bin > 0)
  if (is.null(min.cnt)) min.cnt <- sqrt(length(x))
  stopifnot(min.cnt > 0)
  .Call('bin', as.double(x), as.double(y), as.double(min.iv), as.integer(min.cnt), as.integer(max.bin), as.integer(mono), as.double(sv))
}