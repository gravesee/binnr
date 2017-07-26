
data(titanic)

fn <- function(par, data, y, mask, ...) {
  par[mask] <- 0
  f <- data %*% par
  (-2 * sum(y*f - log(1 + exp(f))))/length(f)
  #sum((y - plogis(f[,1]))^2)
}

ineq <- function(par, data, y, mask, constraints, ...) {
  par[mask] <- 0
  (constraints %*% par)[,1]
}

# for (i in 1:7) x <- rbind(x, x)


# mod$variables$Age$neutralize(1:2)

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

#p0 <- rnorm(ncol(X), mean = 0, sd = 0.1)
p0 <- rep(0, ncol(X))

## constraints
lower <- rep(-20, length(mask))
upper <- rep(20, length(mask))
lower[mask] <- 0
upper[mask] <- 0

S <- nloptr::cobyla(p0, fn, hin=ineq, nl.info = TRUE, data=X, y=x$Survived, mask=mask, constraints=constraints,
  control = list(maxeval=5000, ftol_abs=0.01))
#S <- nloptr::cobyla(p0, fn, hin = ineq, nl.info = TRUE, data=fare, y=titanic$Survived, control = list(maxeval=20000, ftol_abs=0.1))

pars <- S$par
pars[mask] <- 0
p <- (X %*% pars)[,1]

## recenter the parameters somehow?




library(ellen)

ks.table(-p, x$Survived)
ks.table(-p2, x$Survived)



## divide the intercept amoung all of the non-zero

## extract the covars
l <- sapply(covars, ncol)
#intercept <- pars[1]
ors <- setNames(split(pars, rep(seq_along(l), l)), names(l))

ors <- lapply(ors, function(z) z + tail(z, 1))

## center them about the mean
rngs <- sapply(ors, function(z) ((max(z) + min(z)) / 2))


## set zeros to rngs?
# ors <- mapply(function(a, b) {a[a == 0] <- b; a}, ors, rngs)

centered <- mapply(`-`, ors, rngs)


## need to account for missing values somehow ...
#par2 <- c(S$par[1], unlist(centered))
par2 <- unlist(centered)
par2[mask] <- 0

p2 <- (X %*% par2)[,1]

par3 <- par2
par3[1] <- par3[1] + sum(par3[mask])
par3[mask] <- 0
p3 <- (X %*% par3)[,1]


f <- !is.na(x$Age)
## are these equivalent?
## center around the mid=-range?
## map these back on the bins
for (i in names(ors)) {
  mod$variables[[i]]$set_overrides(centered[[i]])
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




m <- matrix(rnorm(100), 10, 10)


m[2:6,2] <- NA










betas <- rnorm(10)






