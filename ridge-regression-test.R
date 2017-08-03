library(binnr)
### test a method that uses ridge regression instead... no constraints

data(titanic)

mod <- bin(titanic, titanic$Survived)

mod$set_step(names(titanic)[2:5], 1L)
mod$step[c(1)] <- NA
mod$fit()
mod$secondary_performance(y = titanic$Fare, epsilon = 0.10)

p2 <- mod$predict()
mod$select("model1")
p1 <- mod$predict()

i1 <- order(-p1)
i2 <- order(-p2)

plot(cumsum(titanic$Fare[i1]), type='l', col='blue')
par(new=TRUE)
plot(cumsum(titanic$Fare[i2]), type='l', col='red')



mod$select("model4")
mod$adjust()


library(ellen)

ks.table(-p1, titanic$Survived)
ks.table(-p2, titanic$Survived)


### test this on a larger dataset:
d <- read.csv("F:/R Dev/rulefit-testing/rv5a_xeno.csv", check.names = FALSE, na.strings = c("","."))

v <- c("_pb_","_x_","billgrp")
k <- c(242:258, 78:86, 1:3, 189, unlist(sapply(v, grep, names(d))))
mod <- bin(d[,-k], d$`_depvar`, mono = 2)

mod$sort()
s <- mod$summary()

mod$step[] <- NA
s2 <- c("rv_C22_stl_recency", "iv_C13_prv_addr_lres", "iv_A46_prv_addr_avm_auto_val", "iv_L79_adls_per_apt_addr_c6")
mod$set_step(s2, 2L)
mod$set_step("iv_I60_inq_phones_per_adl", 1L)

mod$fit()






### vars

X <- as.matrix(mod$variables$iv_I60_inq_phones_per_adl$predict_sparse()) + 0

fit <- glm(d$`_depvar`~X, family="binomial")


X <- mod$variables$iv_I60_inq_phones_per_adl$predict_sparse()
fit <- glmnet::glmnet(X, d$`_depvar`, lambda=0)









load("Z:/User Folders/Steve B/to Eric/meta/GBM_fraud_comfort_3_50_0.1.rData")
x <- gbm:::reconstructGBMdata(mod.1)

for (i in 1:3) x <- rbind(x, x)

b <- bin(x[-1], x[,1], min.cnt = 100, min.res=25, exceptions=-1, mono=2)
b$sort()
b$step[] <- NA
nm <- names(b$step)
b$set_step(nm[1:2], 1L)
b$set_step(nm[3:40], 2L)

b$fit()

p <- b$predict()

b$fit(nlambda=2)

val <- x[1:10,]
val[1,] <- NA

woe <- b$predict(newdata = x, type="woe")
p <- rowSums(woe) + b$models$model9@coefs[1]


tapply(x[,1], plogis(woe[,3] + coef(b)[1]), mean)


library(ellen)
ks.table(-p, x[,1])



p[is.na(x$iv_Estimated_Income)]








b <- bin(x[,-1], x[,1], min.cnt = 100, min.res=25, exceptions=-1, mono=2)
b$sort()
b$variables$iv_Estimated_Income$collapse(1:4)


sas_code <- b$gen_code_sas("meta_binnr")


writeLines(sas_code, con = "meta_binnr.sas", sep = "\n")

