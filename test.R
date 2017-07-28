
data(titanic)

fn <- function(par, data, y, ...) {
  f <- data %*% par
  -2 * mean(y * f - log(1 + exp(f)))
}

ineq <- function(par, data, y, ...) {
  (constraints %*% par)[,1]
}

mod$variables$Fare$set_constraints(1:8, "inc")
mod$variables$Age$set_constraints(1:5, "dec")
mod$variables$SibSp$set_constraints(1:2, "inc")
mod$variables$Parch$set_constraints(1:2, "inc")
mod$variables$Embarked$set_constraints(1:3, "dec")

#mod$adjust()
x <- titanic
mod <- bin(x, x$Survived, mono=2)

binfo <- lapply(mod$variables[mod$get_dropped(TRUE)], function(x) x$predict_sparse())
covars <- lapply(binfo, '[[', 'm')
constraints <- cbind(FALSE, do.call(bdiag, lapply(binfo, '[[', 'constraints')))

ncols <- sapply(covars, ncol)
neutral <- sapply(binfo, '[[', 'neutral')
mask <- c(FALSE, unlist(mapply(function(l, i) {
  res <- rep(FALSE, l)
  res[i] <- TRUE
  res
}, neutral, ncols)))


X <- cbind(TRUE, do.call(cbind, covars))
p0 <- rep(0, ncol(X))

ub <- rep(10, length(p0))
lb <- rep(-10, length(p0))

ub[mask] <- 1e-8
lb[mask] <- -1e-8

S <- nloptr::cobyla(p0, fn, hin=ineq, nl.info = TRUE, data=X, y=x$Survived, mask=mask, constraints=constraints,
  control = list(maxeval=5000, ftol_abs=0.01))

p0 <- rep(0, ncol(X))
S <- nloptr::auglag(p0, fn, hin=ineq, nl.info = TRUE,
  control = list(maxeval=1000, ftol_abs=0.01),
  localsolver="LBFGS", upper=ub, lower=lb, data=X, y=x$Survived, constraints=constraints)

pars <- S$par

pars[mask] <- 0
p <- (X %*% pars)[,1]

## recenter the parameters somehow?
library(ellen)
ks.table(-p, x$Survived)
ks.table(-phat, x$Survived)



## divide the intercept amoung all of the non-zero

## extract the covars
l <- sapply(covars, ncol)
#intercept <- pars[1]
ors <- setNames(split(pars[-1], rep(seq_along(l), l)), names(l))
mps <- sapply(ors, function(z) (max(head(z, -1)) + min(head(z, -1)) )/2 )

centered <- mapply(function(a, b) {
  n <- length(a)
  a[-n] <- a[-n] - b
  a[n] <- b
  a
}, ors, mps)

# centered <- mapply(`-`, ors, mps)

final <- c(0, unlist(centered))
final[1] <- sum(mps)
final[mask] <- 0
coef <- setNames(split(final[-1], rep(seq_along(l), l)), names(l))


## center around the mid=-range?
## map these back on the bins
for (i in names(ors)) {
  mod$variables[[i]]$set_overrides(ors[[i]])
}


## score matches
woe <- mod$predict(type="woe")
phat <- rowSums(mod$predict(type="woe")) + final[1]

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




p1 <- pars


p1

p2 <- pars


ors
z <- ors$Pclass


t(c(-1, -1, -1))













