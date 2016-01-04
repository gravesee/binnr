#' @export
`-.bin.integer` <- function(e1, e2) {
  `-.bin.numeric`(e1, e2)
}

#' @export
`+.bin.integer` <- function(e1, e2) {
  `+.bin.numeric`(e1, e2)
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
  b <- bin.factory(e1$data$x, e1$data$y, e1$name, new_breaks, e1$opts)
  
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
  
  b <- bin.factory(e1$data$x, e1$data$y, e1$name, new_breaks, e1$opts)
  update.bin(b, e1)
}

#' @export
`<=.bin.numeric` <- function(e1, e2) {
  b <- do.call(bin,c(list(x=pmin(e1$data$x, e2), y=e1$data$y, name=e1$name),
                     e1$opts))
  b$data$x <- e1$data$x
  
  update.bin(b, e1)
}

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


#' @export
`!=.bin` <- function(e1, e2) {
  # TODO: add bounds checking for NAs
  y <- e1$data$y
  zero <- unlist.matrix(e1, 1, e2)
  ones <- unlist.matrix(e1, 2, e2)
  counts <- mapply(cbind, zero, ones, SIMPLIFY = F)
  
  tots <- matrix(apply(rbind(counts$var, counts$exc), 2, sum), ncol=2)
  
  values <- e1$core$values
  values$var <- log((counts$var[,2]/tots[,2])/(counts$var[,1]/tots[,1]))
  values$exc <- log((counts$exc[,2]/tots[,2])/(counts$exc[,1]/tots[,1]))
  values <- lapply(values, function(x) {x[is.nan(x) | is.infinite((x))] <- 0; x})
  b <- e1
  b$core$counts <- counts
  b$core$values <- values
  
  update.bin(b, e1)
}

#' @export
reset <- function(b) {
  bin(x=b$data$x, y=b$data$y, name=b$name, bc=b$opts)
}

#' @export
mono <- function(b, v) {
  v <- if(v %in% c(-1,0,1,2)) v else 0
  opts <- b$opts
  opts$mono <- v
  out <- bin(x=b$data$x, y=b$data$y, name=b$name, bc=opts)
  out$opts <- b$opts
  update.bin(out, b)
}

#' @export
exception <- function(b, v) {
  opts <- b$opts
  opts$exceptions <- v
  out <- bin(x=b$data$x, y=b$data$y, name=b$name, bc=opts)
  out$opts <- opts
  update.bin(out, b)
}

#' @export
set.equal <- function(b, v1, v2) {
  out <- b
  skeleton <- b$core$values
  flesh <- unlist(skeleton)
  
  # check that requested levels are valid, else return bin untouched
  l <- length(flesh)
  if (v1 > l | v2 > l | v1 < 1 | v2 < 1) return(b)
  
  flesh[v1] <- flesh[v2]
  out$core$values <- relist(flesh, skeleton)
  update.bin(out, b)
}

#' @export
undo <- function(x) {
  if (is.null(x$history)) return(x)
  return(x$history)
}