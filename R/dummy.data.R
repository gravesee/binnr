library(microbenchmark)
data(titanic, package='mjollnir')

# dummy data
x <- sample(1:10, 1e6, replace=T)
y <- sample(0:1, 1e6, replace = T)

x <- as.numeric(as.character(titanic$Pclass))
y <- titanic$Survived

f <- is.na(titanic$Age)

x <- titanic$Fare

x <- rep(titanic$Age[!f], 1000)
y <- rep(titanic$Survived[!f], 1000)


test_func <- function(x, y){
  stopifnot(length(x) == length(y))
  
  .Call('bin', as.double(x), as.double(y))
}