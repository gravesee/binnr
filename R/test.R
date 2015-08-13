#library(binnr)

titanic <- read.csv('~/Downloads/train.csv', header=T)
#data(titanic, package='mjollnir')
x <- titanic$Fare
y <- titanic$Survived

keep <- c('Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked')

# createe an as.data 
#load("F:/SANT1507_5516/R-Santander/rv40_vs_rv50.analysis.rData")
#bins <- bin.data(rv50[,keep50], rv50$depvar, mono=mono50, min.iv=0.03, exceptions=list(ALL=-1))




# library(glmnet)
# fit <- cv.glmnet(binned, rv50$depvar, alpha=1, nfolds=5, keep=TRUE, family='binomial')
# 
# phat <- fit$fit.preval[,which.min(fit$cvm)]
# phat <- log(phat/(1-phat))
# bin
# library(mjollnir)
# ks.table()
# 
# 
# bins40 <- bin.data(rv40[,keep40], rv40$depvar, mono=mono40, min.iv=0)
# binned40 <- predict(bins, rv50)
# library(glmnet)
# fit <- cv.glmnet(binned, rv50$depvar, alpha=1, nfolds=5, keep=TRUE, family='binomial')




# view the bins and modify them if necessary

#binned <- predict(bins, titanic)

 #bin(x, y)