bin2 <- function(x, y, name, min.iv, min.cnt, max.bin, mono, exceptions){
  UseMethod("bin", x)
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
  cnts <- table(x, factor(y, levels=c(0,1)), useNA='ifany')
  cbind(cnts, margin.table(cnts, 1))
}

bin.factory <- function(x, y, breaks, exceptions) {
  exc <- (x %in% exceptions)
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
  
  list(
    breaks=breaks,
    counts=counts,
    values=values)
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
  
  
  structure(list(
    name = name,
    opts = options,
    core = bin.factory(x, y, breaks, exceptions)),
    class=c("bin.numeric", "bin"))
  
}

as.data.frame.bin.numeric <- function(x, row.names = NULL, optional = FALSE, ...) {
  cnts <- do.call(rbind, x$core$counts)
  rnames <- rownames(cnts)
  if (any(is.na(rnames))) {
    tots <- apply(cnts[-which(is.na(rnames)),], 2, sum)
  } else {
    tots <- apply(cnts, 2, sum)
  }
  
  pcts <- t(apply(cnts, 1, '/',  tots))
  woe  <- log(pcts[,2]/pcts[,1])
  ivs  <- woe * (pcts[,2] - pcts[,1])
  out <- cbind(cnts, pcts, woe, ivs)
  colnames(out) <- c("#0","#1","N","%0","%1","%W","WoE","IV")
  out
}
