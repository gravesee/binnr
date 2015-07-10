# dummy data
x <- sample(1:10, 100, replace = T)
y <- sample(0:1, 100, replace = T)

test_func <- function(x, y){
  stopifnot(length(x) == length(y))
  
  .Call('bin', as.double(x), as.double(y))
}