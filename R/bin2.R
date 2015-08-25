bin <- function(x, y, name, min.iv, min.cnt, max.bin, mono, exceptions){
  UseMethod("bin", x)
}

bin.factory <- function(x, ...) {
  UseMethod("bin.factory")
}

woe <- function(cnts, y) {
  if (length(cnts) == 0) return(NA)
  ytot <- table(factor(y, levels=c(0,1)))
  pct0 <- cnts[,1]/ytot[1]
  pct1 <- cnts[,2]/ytot[2]
  woe <- log(pct1/pct0)
  woe[is.infinite(woe) | is.na(woe)] <- 0
  woe
}

cnts <- function(x, y) {
  tbl <- table(x, factor(y, levels=c(0,1)), useNA='ifany')
  if (sum(tbl) == 0) return(matrix(nrow = 0, ncol=2))
  out <- matrix(tbl, ncol=2)
  rownames(out) <- rownames(tbl)
  out
}

bin.factory.numeric <- function(x, y, breaks, name, options) {
  exc <- (x %in% options$exceptions)
  f   <- !(is.na(x) | exc)
  xb  <- cut(x[f], breaks)
  
  counts <- list(
    var=cnts(xb, y[f]),
    exc=cnts(x[exc], y[exc]),
    nas=cnts(x[is.na(x)], y[is.na(x)])
  )
  
  values <- list(
    var=woe(counts$var, y[!is.na(x)]),
    exc=woe(counts$exc, y[exc]),
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
      values=values)),
    class=c("bin.numeric", "bin"))
  
}

bin.factory.factor <- function(x, y, breaks, name, options) {
  counts <- list(
    var=cnts(x[!is.na(x)], y[!is.na(x)]),
    exc=numeric(0),
    nas=cnts(factor(x[is.na(x)], levels=NA), y[is.na(x)])
  )
  
  values <- list(
    var=woe(counts$var, y[!is.na(x)]),
    exc=numeric(0),
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
      values=values)),
    class=c("bin.factor", "bin"))
  
}

bin.numeric <- function(x, y=NULL, name=NULL, min.iv=.01, min.cnt = NULL, max.bin=10, mono=0, exceptions=numeric(0)) {
  if(is.null(min.cnt)) min.cnt <- sqrt(length(x))
  
  options <- list(
    min.iv    = min.iv,
    min.cnt   = min.cnt,
    max.bin   = max.bin,
    mono      = mono,
    exceptions= exceptions)
  
  breaks <-
    .Call('bin', as.double(x[!is.na(x)]), as.double(y[!is.na(x)]), as.double(min.iv),
          as.integer(min.cnt), as.integer(max.bin), as.integer(mono),
          as.double(exceptions))
  
  
  bin.factory.numeric(x, y, breaks, name, options)
  
}

bin.factor <- function(x, y=NULL, name=NULL, min.iv=.01, min.cnt = NULL, max.bin=10, mono=0, exceptions=numeric(0)) {
  if(is.null(min.cnt)) min.cnt <- sqrt(length(x))
  
  options <- list(
    min.iv    = min.iv,
    min.cnt   = min.cnt,
    max.bin   = max.bin,
    mono      = mono,
    exceptions= exceptions)
  
  map <- as.list(levels(x))
  names(map) <- levels(x)
  
  bin.factory.factor(x, y, map, name, options)

}
  
as.data.frame.bin <- function(x, row.names = NULL, optional = FALSE, ...) {
  cnts <- do.call(rbind, x$core$counts)
#  rnames <- rownames(cnts)
#   if (any(is.na(rnames))) { # exclude NAs from total calculations
#     f <- !is.na(rnames)
#     tots <- apply(cnts[-which(is.na(rnames)),], 2, sum)
#   } else {
#     tots <- apply(cnts, 2, sum)
#   }
  f <- !is.na(rownames(cnts))
  tots <- apply(cnts[f,], 2, sum)
  
  pcts <- t(apply(cnts, 1, '/',  tots))
  cnts <- cbind(cnts, apply(cnts, 1, sum))
  woe <- na.omit(unlist(x$core$values))
  ivs <- woe * (pcts[,2] - pcts[,1])
  prob <- cnts[,2] / cnts[,3]
  pcts[!f,] <- 0
  out <- cbind(cnts, pcts, prob, woe, ivs)
  colnames(out) <- c("#0","#1","N","%0","%1","P(1)","WoE","IV")
  
  # rbind total and output
  rbind(out, Total=apply(out, 2, sum))
}

print.bin <- function(x, ...) {
  print(as.data.frame.bin(x))
}

`-.bin.numeric` <- function(e1, e2) {
  # need to handle when 1 is included in the range
  # error check that the range is continuous
  stopifnot(all(diff(e2)==1))
  e2 <- pmax(2, e2)
  new_breaks = e1$core$breaks[-(e2)]
  bin.factory(e1$data$x, e1$data$y, breaks = new_breaks, name = e1$name, options = e1$opts)
}

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
  bin.factory(e1$data$x, e1$data$y, breaks = new_breaks, name = e1$name, options = e1$opts)
  
}

`<=.bin.numeric` <- function(e1, e2) {
  new_breaks <- c(unique(pmin(e1$core$breaks, e2)), Inf)
  bin.factory(e1$data$x, e1$data$y, breaks = new_breaks, name = e1$name, options = e1$opts) 
}


