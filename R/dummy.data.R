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
test_func <- function(x, y, min.iv=.025, min.cnt = 50, max.bin=20, sv=NULL) {
  stopifnot(length(x) == length(y))
  f <- !(x %in% sv)
  breaks <- .Call('bin', as.double(x[f]), as.double(y[f]), as.double(min.iv), as.integer(min.cnt), as.integer(max.bin))
  
  out <- cut(x, breaks)
  levels(out) <- c(levels(out), sv)
  out[!f] <- x[!f]
  
  ave(y, out, FUN=function(x) {
    apply(prop.table(table(x, y), 2), 1, function(x) log(x[1]/x[2]))    
  })
}

test_func2 <- function(x, y, min.iv=.025, min.cnt = 50, max.bin=20, sv=NULL) {
  stopifnot(length(x) == length(y))
  # f <- !(x %in% sv)
  .Call('bin', as.double(x[f]), as.double(y[f]), as.double(min.iv), as.integer(min.cnt), as.integer(max.bin))
}

#test_func(x, y)


