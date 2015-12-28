library(testthat)
library(binnr)

data(titanic)
bins <- bin(titanic[,-1], titanic$Survived)

# test_check("binnr")

test_that("bin.factor operations", {
  expect_equal((bins$Pclass)$core, (bins$Pclass - 1:2 + 1)$core)
  expect_equal((bins$Pclass)$core, (bins$Pclass - c(1,3) + 1)$core)
})

test_that("bin.numeric operations", {
  expect_equal(tail(as.data.frame(bins$Age), 1)[,1:8],
               tail(as.data.frame(bins$Age - 1:5 + 1), 1)[,1:8],
               label="check collapse totals")
})

test_that("Global operations", {
  expect_equal(bins$Pclass, reset(bins$Pclass - 1:2), label="reset bin.factor")
  expect_equal(bins$Age, reset(bins$Age - 1:5), label="reset bin.numeric")
})

test_that("Monotonicity", {
  expect_equal(all(diff(mono(bins$Age, -1)$core$value$var) < 0), TRUE)
  expect_equal(all(diff(mono(bins$Fare, 1)$core$value$var) > 0), TRUE)
})
