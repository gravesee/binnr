# dummy data
x <- runif(1000)
y <- log(x/(1-x)) + rnorm(1000)/10
plot(x, y)
