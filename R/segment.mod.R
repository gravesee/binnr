
seg.bins <- function(x, y, seg, ...) {
  xs <- split(x, seg)
  ys <- split(y, seg)
  
  mapply(bin, xs, ys, MoreArgs = list(...), SIMPLIFY = F)
}

seg.fit <- function(bins, x, y, seg, ...) {
  xs <- split(x, seg)
  ys <- split(y, seg)
  
  mapply(fit, bins, xs, ys, MoreArgs = list(...), SIMPLIFY = F)
}

# data(titanic)
# bins <- bin(titanic[,-1], titanic$Survived, seg=titanic$Pclass, mono=c(ALL=2), min.res=10)
# mods <- fit(bins, titanic[,-1], titanic$Survived, seg=titanic$Pclass)
# 
# phat <- lapply(mods, predict, titanic)
# 
# x <- sample(letters[1:4], 100, replace = T)
# 
# y <- list(
#   "a"=rnorm(100),
#   "b"=rnorm(100),
#   "c"=rnorm(100))
# 
# i <- match(x, names(y))
# m <- sapply(i, function(i) {out <- rep(0,3); out[i] <- 1; out})
# 
# final <- apply(t(m) * do.call(cbind, y), 1, sum)
# 
# final2 <- sapply(seq_along(x), function(i) if(is.null(y[[x[i]]][i])) NA else y[[x[i]]][i])
# 
# 
# 
