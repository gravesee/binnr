bin.factory.numeric <- function(x, y, breaks, name, options) {
  exc <- (x %in% options$exceptions)
  f   <- !(is.na(x) | exc)
  xb  <- cut(x[f], breaks, dig.lab = 10)
  
  counts <- list(
    var=cnts(xb, y[f]),
    exc=cnts(x[exc], y[exc]),
    nas=cnts(x[is.na(x)], y[is.na(x)], NA)
  )
  
  values <- list(
    var=woe(counts$var, y[!is.na(x)]),
    exc=woe(counts$exc, y[!is.na(x)]),
    nas=woe(counts$nas, y[is.na(x)])
  )
  
  structure(list(
    name = name,
    data = list(
      x=x,
      y=y),
    opts = options,
    core = list(
      breaks=breaks,
      counts=counts,
      values=values),
    meta=list(
      skip=FALSE,
      type="AUTO",
      inmodel=FALSE,
      modified=date()
    ),
    notes=NULL),
    class=c("bin.numeric", "bin"))
  
}

#' @export
bin.numeric <- function(x, y=NULL, name=NULL, min.iv=.01, min.cnt = NULL, min.res = 0, max.bin=10, mono=0, exceptions=numeric(0)) {
  if(is.null(min.cnt)) min.cnt <- sqrt(length(x))
  
  options <- list(
    min.iv    = min.iv,
    min.cnt   = min.cnt,
    min.res   = min.res,
    max.bin   = max.bin,
    mono      = mono,
    exceptions= exceptions)
  
  breaks <-
    .Call('bin', as.double(x[!is.na(x)]), as.double(y[!is.na(x)]),
          as.double(min.iv), as.integer(min.cnt), as.integer(min.res),
          as.integer(max.bin), as.integer(mono), as.double(exceptions))
  
  bin.factory.numeric(x, y, breaks, name, options)
}

#' @export
`-.bin.numeric` <- function(e1, e2) {
  # need to handle when 1 is included in the range
  # error check that the range is continuous
  stopifnot(all(diff(e2)==1))
  
  e2 <- tail(e2, -1)
  d <- which(e2 > length(e1$core$breaks) - 1)
  if(length(d) > 0) e2 <- e2[-d]
  new_breaks = e1$core$breaks[-(e2)]
  b <- bin.factory(e1$data$x, e1$data$y, new_breaks, e1$name, e1$opts)
  b$history <- e1
  
  b$meta$type <- "MANUAL"
  b$meta$modified <- date()
  b
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
  b$history <- e1
  
  b$meta$type <- "MANUAL"
  b$meta$modified <- date()
  
  b
}

#' @export
`<=.bin.numeric` <- function(e1, e2) {
  b <- do.call(bin,c(list(x=pmin(e1$data$x, e2), y=e1$data$y, name=e1$name),
                     e1$opts))
  b$history <- e1
  b$data$x <- e1$data$x
  b$meta$type <- "MANUAL"
  b$meta$modified <- date()
  
  b
}