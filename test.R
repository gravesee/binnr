data(titanic)
x <- titanic




fn <- function(par, data, y, ...) {
  f <- data %*% par
  dev <- -2 * sum(y*f - log(1 + exp(f)))
  dev
}

ineq <- function(par, data, y, constraints, ...) {
  (constraints %*% par)[,1]
}

mod <- bin(x, x$Survived, mono=2)

mod$variables$Age$neutralize(1:2)

mod$variables$Fare$set_constraints(1:8, "inc")
# mod$variables$Age$set_constraints(1:5, "dec")
mod$variables$SibSp$set_constraints(1:2, "inc")
mod$variables$Parch$set_constraints(1:2, "inc")
mod$variables$Embarked$set_constraints(1:3, "dec")

covars <- lapply(mod$variables[mod$get_dropped(TRUE)], function(x) x$predict_sparse())
X <- cbind(TRUE, do.call(cbind, covars))

## constraint matrix
hin <- lapply(mod$variables[mod$get_dropped(TRUE)], function(x) x$make_constraint_matrix())
constraints <- cbind(FALSE, do.call(bdiag, hin))

#p0 <- rnorm(ncol(X), mean = 0, sd = 0.1)
p0 <- rep(0, ncol(X))

S <- nloptr::cobyla(p0, fn, hin=ineq, nl.info = TRUE, data=X, y=titanic$Survived, constraints=constraints, control = list(maxeval=20000, ftol_abs=0.1))
#S <- nloptr::cobyla(p0, fn, hin = ineq, nl.info = TRUE, data=fare, y=titanic$Survived, control = list(maxeval=20000, ftol_abs=0.1))

pars <- S$par
pars[colSums(X) == 0] <- 0

p <- (X %*% pars)[,1]

library(ellen)

ks.table(-p, titanic$Survived)

## extract the covars
l <- sapply(covars, ncol)
ors <- setNames(split(S$par[-1], rep(seq_along(l), l)), names(l))

## map these back on the bins
for (i in names(ors)) {
  mod$variables[[i]]$set_overrides(ors[[i]])
}

### step two?

fn <- function(par, data, y, ...) {
  f <- data %*% par
  dev <- -2 * sum(y*f - log(1 + exp(f)))
  dev
}

res <- titanic$Survived - plogis(p)

steptwo <- mod$get_dropped()
covars2 <- lapply(mod$variables[steptwo], function(x) x$predict_sparse())


fn2 <- function(par, data, y, ...) {
  f <- data %*% par
  sum((y - f)**2)
}

fit_mod <- function(X) {
  p0 <- rep(0, ncol(X))
  nloptr::cobyla(p0, fn2, nl.info = TRUE, data=X, y=res, control = list(maxeval=20000, ftol_abs=0.1))
}

## fit the variables NOT in the model to the residuals?
margin_ <- lapply(covars2[-1], fit_mod)

## map these back on the bins
for (i in names(margin_)) {
  mod$variables[[i]]$set_overrides(margin_[[i]]$par)
}
