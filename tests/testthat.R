library(testthat)
library(binnr)

data(titanic)
bins <- bin(titanic[,-1], titanic$Survived)

test_check("binnr")

expect_equal(2 + 2, 4)
