library(microbenchmark)

# dummy data
x <- runif(100000)
y <- sample(0:1, 100000, replace = T)

test_func <- function(x, y){
  stopifnot(length(x) == length(y))
  
  .Call('bin', as.double(x), as.double(y))
}