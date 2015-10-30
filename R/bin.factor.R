bin.factory.factor <- function(x, y, breaks, name, options) {
  counts <- list(
    var=cnts(x[!is.na(x)], y[!is.na(x)]),
    exc=matrix(nrow=0,ncol=2),
    nas=cnts(factor(x[is.na(x)], levels=NA), y[is.na(x)], NA)
  )
  
  values <- list(
    var=woe(counts$var, y[!is.na(x)]),
    exc=matrix(nrow=0,ncol=2),
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
      visited=FALSE,
      modified=date()
    ),
    notes=NULL),
    class=c("bin.factor", "bin"))
  
}

#' @export
bin.factor <- function(x, y=NULL, name=NULL, min.iv=.01, min.cnt = NULL, min.res = 0, max.bin=10, mono=0, exceptions=numeric(0)) {
  if(is.null(min.cnt)) min.cnt <- sqrt(length(x))
  
  options <- list(
    min.iv    = min.iv,
    min.cnt   = min.cnt,
    min.res   = min.res,
    max.bin   = max.bin,
    mono      = mono,
    exceptions= exceptions)
  
  map <- as.list(levels(x))
  names(map) <- levels(x)
  
  bin.factory.factor(x, y, map, name, options)
  
}

#' @export
`-.bin.factor` <- function(e1, e2) {
  x <- e1$data$x
  map <- e1$core$breaks
  f <- map %in% rownames(e1$core$counts$var)[e2]
  map[f] <- paste(names(map)[f], collapse=',')
  levels(x) <- unlist(map)
  b <- bin.factory(x, e1$data$y, map, e1$name, e1$opts)
  b$core$breaks <- map
  b$data$x <- e1$data$x
  b$history <- e1
  
  b$meta$type <- "MANUAL"
  b$meta$modified <- date()
  
  b
}

#' @export
`+.bin.factor` <- function(e1, e2) {
  x <- e1$data$x
  map <- e1$core$breaks
  f <- map %in% map[e2]
  map[f] <- levels(e1$data$x)[f]
  levels(x) <- unlist(map)
  b <- bin.factory(x, e1$data$y, map, e1$name, e1$opts)
  b$core$breaks <- map
  b$data$x <- e1$data$x
  b$history <- e1
  
  b$meta$type <- "MANUAL"
  b$meta$modified <- date()
  
  b
}