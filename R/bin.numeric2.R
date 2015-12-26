#' @export
get.counts.numeric <- function(x, y, bc, breaks) {
  exc <- (x %in% bc$exceptions)
  f   <- !(is.na(x) | exc)
  xb  <- cut(x[f], breaks, dig.lab = 10)
  list(
    var = cnts(xb, y[f]),
    exc = cnts(x[exc], y[exc]),
    nas = cnts(x[is.na(x)], y[is.na(x)], NA))
}

#' @export
get.values.numeric <- function(x, y, counts) {
  list(
    var=woe(counts$var, y[!is.na(x)]),
    exc=woe(counts$exc, y[!is.na(x)]),
    nas=woe(counts$nas, y[is.na(x)]))
}

#' @export
bin.numeric <- function(x, y, name, bc=NULL, ...) {
  if(is.null(bc)) bc <- bin.control()
  breaks <- .Call('bin', as.double(x[!is.na(x)]), as.double(y[!is.na(x)]),
                  as.double(bc$min.iv), as.integer(bc$min.cnt), as.integer(bc$min.res),
                  as.integer(bc$max.bin), as.integer(bc$mono), as.double(bc$exceptions))
  bin.factory(x, y, name, breaks, bc)
}

#' @export
`-.bin.numeric` <- function(e1, e2) {
  # need to handle when 1 is included in the range
  # error check that the range is continuous
  stopifnot(all(diff(e2)==1))
  if (length(e2) <= 1) return(e1)
  
  # handle out of range values...
  e2 <- unique(pmax(pmin(tail(e2, -1), length(e1$core$breaks) - 1), 2))
  
  new_breaks = e1$core$breaks[-(e2)]
  b <- bin.factory(e1$data$x, e1$data$y, new_breaks, e1$name, e1$opts)
  
  update.bin(b, e1)
}

#' @export
`+.bin.numeric` <- function(e1, e2) {
  # insert new break points into selected bin range
  b <- e1$core$breaks
  x <- e1$data$x
  a <- max(1, e2) # can't be smaller than 1
  z <- min(e2 + 1, length(b)) # or larger than max els
  f <- x > b[a] & x <= b[z] & !is.na(x) & !(x %in% e1$opts$exceptions)
  vals <- x[f]
  q <- unique(quantile(vals, seq(0, 1, 0.2)))
  
  new_breaks <- sort(c(b[-z], q))
  b <- bin.factory(e1$data$x, e1$data$y, new_breaks, e1$name, e1$opts)
  update.bin(b, e1)
}

#' @export
`<=.bin.numeric` <- function(e1, e2) {
  b <- do.call(bin,c(list(x=pmin(e1$data$x, e2), y=e1$data$y, name=e1$name),
                     e1$opts))
  b$data$x <- e1$data$x
  
  update.bin(b, e1)
}