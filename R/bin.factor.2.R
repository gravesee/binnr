
bin.factory <- function(x, y, name, breaks, bc) {
  counts <- get.counts(x, y, bc, breaks)  
  values <- get.values(x, y, counts)
  make.bin(x, y, name, breaks, counts, values, bc)
}

#' @export
get.counts <- function(x, y, bc, breaks) {
  UseMethod('get.counts')
}

#' @export
get.values <- function(x, y, values) {
  UseMethod('get.values')
}

#' @export
get.counts.factor <- function(x, y, bc) {
  list(
    var = cnts(x[!is.na(x)], y[!is.na(x)]),
    exc = matrix(nrow=0,ncol=2),
    nas = cnts(factor(x[is.na(x)], levels=NA), y[is.na(x)], NA))
}

#' @export
get.values.factor <- function(x, y, counts) {
  list(
    var = woe(counts$var, y[!is.na(x)]),
    exc = matrix(nrow=0,ncol=2),
    nas = woe(counts$nas, y[is.na(x)]))
}

make.bin <- function(x, y, name, breaks, counts, values, bc) {
  structure(list(
    name = name,
    data = list(x=x, y=y),
    opts = bc,
    core = list(
      breaks=breaks,
      counts=counts,
      values=values),
    meta=list(
      skip=FALSE,
      type="AUTO",
      inmodel=FALSE,
      new=FALSE,
      modified=date()),
    notes=NULL),
    class=c(paste0("bin.", class(x)), "bin"))
}

#' @export
bin.control <- function(min.iv=0.01, min.cnt=25, min.res=0, max.bin=10, mono=0, exceptions=numeric(0)) {
  structure(
    list(
      min.iv=min.iv,
      min.cnt=min.cnt,
      min.res=min.res,
      max.bin=max.bin,
      mono=mono,
      exceptions=exceptions),
    class="bin.control")
}

#' @export
bin.factor <- function(x, y, name, bc=NULL, ...) {
  if(is.null(bc)) bc <- bin.control()
  breaks <- as.list(levels(x))
  names(breaks) <- levels(x)
  bin.factory(x, y, name, breaks, bc)
}