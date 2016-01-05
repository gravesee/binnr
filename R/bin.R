#' Bin variables for credit modeling
#' 
#' @description bin returns a \code{bin} object containing information necessary
#' for modeler interaction. With a \code{bin} object, a modeler can collapse,
#' expand, neutralize, cap, and predict variable transformations
#' 
#' @param x a \code{factor} or \code{numeric} variable. Can also be a
#' \code{data.frame} in which case a \code{bin.list} object is returned.
#' @param y a binary response variable
#' @param name the name used for plotting and printing. Automatically supplied
#' if called on a \code{data.frame}
#' @param min.iv minimum information value required to split a continuous
#' predictor
#' @param min.cnt minimum number of observations to split a continuous
#' predictor. Defaults to the square root of the number of observations.
#' @param min.res minimum number of responses required to split
#' @param max.bin maximum number of levels in resulting discretized, continuous
#' predictor.
#' @param mono monotonicity constraint for binning algorith. \code{binnr} uses 
#' the sane assumption that the 1-labelled target is of most interest. Therefore
#' monotonicity of 1 implies the target rate increases with the predictor. The
#' opposite for -1. A value of 0 enforces no monotonicity constraint and a 2
#' enforces *any* monotonic constraint but doesn't assume a particular
#' direction. If \code{bin} is used on a \code{data.frame}, mono can be a named
#' vector. The reserved name ALL is used for global values.
#' @param exceptions a \code{list} of values to be excluded from the binning
#' algorithm. These levels count towards weight of evidence calculations but are
#' not collapsed. Exceptions values for \code{factors} are ignored. A reserved
#' name, ALL, can be used to supply global exception values.
#' 
#' @return a \code{bin} or \code{bin.list} object
#' 
#' @export
bin <- function(x, y, name=NULL, bc=NULL, seg, mono, exceptions, ...) {
  UseMethod("bin", x)
}

#' @export
is.bin <- function(x) {
  inherits(x, "bin")
}

# create the bin 
bin.factory <- function(x, y, name, breaks, bc) {
  counts <- get.counts(x, y, bc, breaks)  
  values <- get.values(x, y, counts)
  make.bin(x, y, name, breaks, counts, values, bc)
}

# get the counts for each level of the bin
get.counts <- function(x, y, bc, breaks) {
  UseMethod('get.counts')
}

# get the WoE values for each kind of bin level
get.values <- function(x, y, values) {
  UseMethod('get.values')
}

get.counts.factor <- function(x, y, bc, breaks) {
  list(
    var = cnts(x[!is.na(x)], y[!is.na(x)]),
    exc = matrix(nrow=0,ncol=2),
    nas = cnts(factor(x[is.na(x)], levels=NA), y[is.na(x)], NA))
}

get.values.factor <- function(x, y, counts) {
  list(
    var = woe(counts$var, y[!is.na(x)]),
    exc = matrix(nrow=0,ncol=2),
    nas = woe(counts$nas, y[is.na(x)]))
}

make.bin <- function(x, y, name, breaks, counts, values, bc) {
  class <- if (is.numeric(x)) "numeric" else class(x)
  
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
    class=c("bin", paste0("bin.", class)))
}

#' @export bin.control
bin.control <- function(min.iv=0.01, min.cnt=25, min.res=0, max.bin=10, mono=0,
                        exceptions=numeric(0)) {
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

get.counts.numeric <- function(x, y, bc, breaks) {
  exc <- (x %in% bc$exceptions)
  f   <- !(is.na(x) | exc)
  xb  <- cut(x[f], breaks, dig.lab = 10)
  list(
    var = cnts(xb, y[f]),
    exc = cnts(x[exc], y[exc]),
    nas = cnts(x[is.na(x)], y[is.na(x)], NA))
}

get.values.numeric <- function(x, y, counts) {
  list(
    var=woe(counts$var, y[!is.na(x)]),
    exc=woe(counts$exc, y[!is.na(x)]),
    nas=woe(counts$nas, y[is.na(x)]))
}

#' @export
bin.numeric <- function(x, y, name, bc=NULL, ...) {
  if(is.null(bc)) bc <- bin.control()
  breaks <- .Call(
    'bin', as.double(x[!is.na(x)]), as.double(y[!is.na(x)]),
    as.double(bc$min.iv), as.integer(bc$min.cnt), as.integer(bc$min.res),
    as.integer(bc$max.bin), as.integer(bc$mono), as.double(bc$exceptions))
  
  bin.factory(x, y, name, breaks, bc)
}

# modify the bin meta data after certain operations
update.bin <- function(new.bin, old.bin){
  new.bin$data$x <- old.bin$data$x
  new.bin$history <- old.bin
  new.bin$meta <- old.bin$meta
  new.bin$meta$type <- "MANUAL"
  new.bin$meta$modified <- date()
  new.bin
}

#' @export
bin.logical <- function(x, y, name, bc=NULL, ...) {
  warning(sprintf("Not binned: %s -- All missing", name), call. = F)
  NULL
}

#' @export
bin.character <- function(x, y, name, bc=NULL, ...) {
  warning(sprintf("Not binned: %s -- Character, hint: cast to factor", name), call. = F)
  NULL
}

#' @export
bin.NULL <- function(x, y, name, bc=NULL, ...) {
  NULL
}