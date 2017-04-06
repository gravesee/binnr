# What is binnr?

`binnr` is an R package that helps modelers build scorecards. Scorecard models
need to be more than predictive. Regulatory oversight often necessitates they be
transparent as well. Model transparency is difficult to enforce in more
predictive, non-linear methods such as neural networks, random forest, or
gradient boosted decision trees. Often direct modeler intervention is required
to ensure proper treatment is given for values of independent variables.

`binnr` attempts to solve these problems by providing interactive variable
manipulation facilities giving the modeler total control over how variables
are treated in scorecard models.

## Installing binnr

```
install.packages(repo="Z:\Resources\_R\binnr\binnr_0.1.0.zip", repos=NULL,
  method="binary")
```
Alternatively, if you have `devtools` and `Rtools` installed you can use the 
following method to get the latest version hosted on Gitlab.

```
devtools::install_git("https://gitlab.ins.risk.regn.net/minneapolis-r-packages/binnr.git")
```

## Overview of binnr

Modeling with `binnr` follows four phases of development.
 1. Bin
 2. Fit
 3. Review
 4. Adjust
 
The model is usually binned just once, while steps 2 - 4 are repeated as many 
times as necessary.

### Bin

```
library(binnr)
data(titanic)

mod <- bin(data=titanic, y=titanic$Survived)

## optional arguments controlling binning

mod <- bin(
  data = titanic,
  y = titanic$Survived,
  w = weight,
  min.iv = 0.01,
  min.cnt = 100,
  min.res = 25,
  max.bin = 10,
  mono = 2,
  exceptions = c(-1))

## check for multicollinearity

cc <- mod$cluster()
to_drop <- mod$prune_clusters(cc, 0.90, 1)
```

### Fit

```
mod$fit("model 1", "Initial fit with all variables")
```

### Review

```
mod$sort()
mod$summary()
mod$compare("model 1", "model 2")
mod$select("model 1")

mod$get_dropped()
mod$get_inmodel()

```

### Adjust

```
## show and plot are called automatically from the adjust method
mod$variables$Pclass$show()
mod$variables$Pclass$plot()

mod$drop(c("Pclass", "Fare"))
mod$undrop(all=TRUE)


## no longer need to assign to an object
mod$adjust()

## old way of using binnr
# mod2 <- adjust(mod1)
```
