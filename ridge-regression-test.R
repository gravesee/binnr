library(binnr)
### test a method that uses ridge regression instead... no constraints

data(titanic)

mod <- bin(titanic, titanic$Survived)

mod$set_step(names(titanic)[2:4], 1L)
mod$step[c(1,7)] <- NA
mod$fit()


mod$select("model1")
mod$secondary_performance(y = titanic$Fare)
mod$compare("model1", "model2", "model4")




p2 <- mod$predict()
mod$select("model1")
p1 <- mod$predict()



i1 <- order(-p1)
i2 <- order(-p2)


plot(cumsum(titanic$Fare[i1]), type='l', col='blue')
par(new=TRUE)
plot(cumsum(titanic$Fare[i2]), type='l', col='red')


mod$adjust()


library(ellen)

ks.table(-p1, titanic$Survived)
ks.table(-p2, titanic$Survived)


