library(binnr)
### test a method that uses ridge regression instead... no constraints

data(titanic)

mod <- bin(titanic, titanic$Survived)

mod$set_step(names(titanic)[2:4], 1L)
mod$step[1] <- NA
mod$fit2("test1")
mod$fit2("test2")

p <- mod$predict2()

mod$adjust()




