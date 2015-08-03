#library(microbenchmark)
data(titanic, package='mjollnir')

#dyn.load('/binnr/src/bin.so')

# dummy data
x <- rep(titanic$Fare, 100)
y <- rep(titanic$Survived, 100)
x <- rnorm(1e6)
y <- sample(0:1 , 1e6, replace=T)

#x <- as.numeric(as.character(titanic$Pclass))
y <- titanic$Survived

f <- is.na(titanic$Age)
x <- titanic$SibSp
x <- titanic$Fare

x <- titanic$Age[!f]
y <- titanic$Survived[!f]


#SEXP bin(SEXP x, SEXP y, SEXP miniv, SEXP mincnt, SEXP maxbin)
test_func <- function(x, y, min.iv=.025, min.cnt = 50, max.bin=20, sv=NULL, mono=0) {
  stopifnot(length(x) == length(y))
  f <- !(x %in% sv)
  if (all(!f)) return(numeric(length(x)))
  breaks <- .Call('bin', as.double(x[f]), as.double(y[f]), as.double(min.iv), as.integer(min.cnt), as.integer(max.bin), as.integer(mono))
  
  out <- cut(x, breaks)
  levels(out) <- c(levels(out), sv)
  out[!f] <- x[!f]
  tots <- table(y)
  out <- ave(y, out, FUN=function(x) {
    tmp = table(x)
    log((tmp[2]/tots[2])/(tmp[1]/tots[1]))
  })
  
  out[is.na(out)] <- log(mean(y)/(1-mean(y)))
  out
}

test_func2 <- function(x, y, min.iv=.025, min.cnt = 50, max.bin=20, sv=NULL, mono=0) {
  stopifnot(length(x) == length(y))
  f <- !(x %in% sv)
  .Call('bin', as.double(x[f]), as.double(y[f]), as.double(min.iv), as.integer(min.cnt), as.integer(max.bin), as.integer(mono))
}

#test_func(x, y)


