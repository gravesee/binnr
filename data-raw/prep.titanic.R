# file to prep titanic dataset for inclusion in binnr package
titanic <- read.csv('data-raw/train.csv', header=T)
k <- c(2,3,5,6,7,8,10,12)
titanic <- titanic[,k]
titanic$Pclass <- factor(titanic$Pclass)
devtools::use_data(titanic)