#library(microbenchmark)
data(titanic, package='mjollnir')

#dyn.load('/binnr/src/bin.so')

# dummy data
x <- sample(1:10, 1e5, replace=T)
y <- sample(0:1 , 1e5, replace=T)

#x <- as.numeric(as.character(titanic$Pclass))
y <- titanic$Survived

#f <- is.na(titanic$Age)

x <- titanic$Fare

#x <- titanic$Age[!f]
#y <- titanic$Survived[!f]


test_func <- function(x, y){
  stopifnot(length(x) == length(y))
  .Call('bin', as.double(x), as.double(y))
}

#test_func(x, y)