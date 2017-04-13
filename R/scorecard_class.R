#' @include classing_class.R classing_adjust_method.R
NULL

#' @title Scorecard reference class generator
#' @name Scorecard_Class
#' @description Scorecard class that wraps a data.frame and prepares it for
#' scorecard modeling.
#' @field seed saved random seed that is based on  \code{\link{Sys.time}}.
#' @field models list of fitted models for this Scorecard
#' @field selected_model name of last  selected that was loaded into the
#' transforms
#' @field inmodel character vector of variable names that are in the selected
#' model
#' @export Scorecard
#' @exportClass Scorecard
Scorecard <- setRefClass("Scorecard",
 fields = c(
   seed = "numeric",
   models = "list",
   selected_model = "character",
   inmodel = "character"),
 contains = "Classing")

Scorecard$methods(initialize = function(..., seed=as.numeric(strftime(Sys.time(), format="%H%M%S"))) {
  seed <<- seed
  callSuper(...)
})


Scorecard$methods(has_model = function(model) {
  "Assert whether the Scorecard contains the requested model"
  if (!model %in% names(models)) {
    stop("Requested model not found: ", model, call. = FALSE)
  }
})


#' Select a fitted model and load the Transforms
#'
#' @name Scorecard_select
#' @description Select searches the model fitting history for the requested
#' model. It then loads the associated transforms for the model variables into
#' the current Scorevard object. Additionally it loads the dropped and inmodel
#' vectors at the time of the fit.
NULL
Scorecard$methods(select = function(model) {

  has_model(model) ## check that model exists
  mod <- models[[model]]
  selected_model <<- model

  dropped <<- mod@dropped
  inmodel <<- mod@inmodel

  for (v in names(mod@transforms)) {
    variables[[v]]$tf <<- mod@transforms[[v]]
  }

})


Scorecard$methods(add_model = function(mod) {
  "Register a new Model object with the scorecard"
  models[[mod@name]] <<- mod
  select(mod@name)
})


#' Subsequent call from the bin function passed to the Scorecard
#'
#' @name Scorecard_bin
#' @description This bin function should not be directly called by the user.
#' The Scorecard bin function is subsequently called from the
#' \code{\link{bin}} wrapper function.
NULL
Scorecard$methods(bin = function(...) {
  callSuper(...)
  scratch <- new("Model", name="scratch", description="", fit=NULL, ks=0,
                 dropped=dropped, transforms=get_transforms())
  add_model(scratch)
})


#' Fit a model to the current set of variable transforms
#'
#' @name Scorecard_fit
#' @description fits a regularized regressoion model using the glmnet
#' package.
#' @details the fit function first calls predict and substitutes the
#' weight-of-evidence for all predictor variables. It then passes this matrix
#' on to \code{\link[glmnet]{cv.glmnet}}. The coefficients of a binner model fit
#' are restricted to [0,3]. This ensures there are no sign flips in the
#' model coefficients and that the relationships observed on margin are
#' retained in the final model.
#' \code{\link{bin}} wrapper function.
#' @param name brief model name as character
#' @param description character description describing the model
#' @param overwrite should model be overwriiten if it already exists?
#' @param newdata data.frame of independent variables. Default is to
#' use the binned data.
#' @param y target to fit the data to. Default is the y variable used
#' for discretization.
#' @param w optional weight variable
#' @param nfolds number of k-folds with which to select the optimal
#' lambda value
#' @param upper.limits maximum value of fitted coefficients
#' @param lower.limits minimimum value of fitted coefficients
#' @param alpha type of regularization. Default is alpha == 1 for LASSO
#' regression. Alpha of 0 is Ridge regression while anythin in between
#' is the elastic net mixture.
#' @param family response variable distribution. Default is "binomial".
#' @param ... additional arguments passed on to cv.glmnet
NULL
Scorecard$methods(fit = function(name, description="", overwrite=FALSE,
  newdata=.self$get_variables(), y=performance$y, w=performance$w,
  nfolds=5, upper.limits=3, lower.limits=0, alpha=1,
  family="binomial", ...) {

  ## check for consistent dimensions
  if (length(newdata[[1]]) != length(y)) {
    stop("newdata and y must be the same length", call. = FALSE)
  }

  if (length(y) != length(w)) {
    stop("y and w must be the same length", call. = FALSE)
  }

  if (!overwrite) {
    if (name %in% names(models)) {
        stop("Model name already exists and overwrite=FALSE",
             call. = FALSE)
      }
  }

  v <- setdiff(vnames, dropped)
  x <- predict(newdata=newdata[v], type="woe")

  set.seed(seed)
  this_fit <- cv.glmnet(x = x, y = y, weights = w, nfolds = nfolds,
    family=family, alpha=alpha, upper.limits=upper.limits,
    lower.limits=lower.limits, keep=TRUE, ...)

  ## get the coeficients
  coefs <- glmnet::coef.cv.glmnet(this_fit, s="lambda.min")[,1]
  coefs <- coefs[which(coefs != 0)]

  ## set the inmodel vector
  inmodel <<- names(coefs)[-1]

  ## performance metrics
  contr <- contributions_(x[,names(coefs)[-1]], coefs, y, w)
  ks <- ks_(this_fit$fit.preval[,which.min(this_fit$cvm)], y, w) # kfold

  ## store the last transforms
  m <- new("Model", name=name, description=description, dropped=dropped,
           transforms=get_transforms(), coefs=coefs, inmodel=inmodel,
           contribution=contr, ks=ks, fit=this_fit)

  add_model(m)

})


#' Print the Scorecard representation to the console
#'
#' @name Scorecard_show
NULL
Scorecard$methods(show = function(...) {
  ## show the models / coefs?
  cat(sprintf("%d models", length(models)), sep="\n")
  i <- rep("", length(models))
  i[names(models) == selected_model] <- "*"
  cat(sprintf(" |-- %-2s %-20s | %04.1f ks | %s", i,
              sapply(models, slot, "name"),
              sapply(models, slot, "ks") * 100,
              sapply(models, slot, "description")), sep="\n")
})


#' Return scorecard predictions or WoE substitution
#'
#' @name Scorecard_predict
#' @param newdata data.frame on which to calculate predictions
#' @param keep whether to keep dropped values
#' @param type "score" to return the model score. "woe" to return the WoE
#' substitution for the input dataset
#' @return Either a single column matrix of score predictions or a matrix
#' matching the input dimension of the dataset containing weight-of-evidence
#' substitutions.
NULL
Scorecard$methods(predict = function(newdata=NULL, keep=FALSE, type="score", ...) {

  woe <- callSuper(newdata=newdata, keep=keep)

  if (type == "woe") return(woe)

  mod <- models[[selected_model]]
  v <- names(mod@coefs[-1])

  woe[,v] %*% mod@coefs[v] + mod@coefs[1]
})


#' Summarize the currently selected model
#'
#' @name Scorecard_summary
#' @param keep whether to summarize droppped variables as well
#' @return a matrix summarizing the independent variables using the performance
#' implementation summary function. Also displays the coefficients and model
#' contributions of the predictors.
NULL
Scorecard$methods(summary = function(keep=FALSE, inmodel.only=FALSE) {

  mod <- models[[selected_model]]

  cat(mod@name, "\nOut-of-Fold KS: ", mod@ks, "\n")

  res <- callSuper(keep=keep)
  vars <- row.names(res)


  out <- cbind(res, `In Model` = 0, `Coefs` = mod@coefs[vars],
    `Contribution` = mod@contribution[vars])

  out[inmodel,"In Model"] <- 1

  if (inmodel.only) {
    out[match(inmodel, row.names(out), 0), ]
  } else {
    out
  }
})


#' Scorecard adjust method entry point
#'
#' @name Scorecard_adjust
#' @details calling adjust enters an interactive variable edit mode. Press "h"
#' for a list of commands.
NULL
Scorecard$methods(adjust = function(...) {
  callSuper(...)
})


#' Sort variables of a scorecard
#'
#' @name Scorecard_sort
#' @details variables that are in the currently selected model are sorted to the
#' front while dropped variables are sorted to the end. Variables within each
#' group are sort by descending information value.
NULL
Scorecard$methods(sort = function() {

  v <- setNames(sapply(variables, function(x) x$sort_value()), names(variables))

  im <- setNames(rep(0, length(v)), names(v))
  dr <- setNames(rep(0, length(v)), names(v))

  im[inmodel] <- 1
  dr[dropped] <- 1

  i <- order(im, -dr, v, decreasing = TRUE, na.last = TRUE)

  variables <<- variables[i]

})


#' Scorecard adjust method entry point
#'
#' @name Scorecard_adjust
#' @details calling adjust enters an interactive variable edit mode. Press "h"
#' for a list of commands.
NULL
Scorecard$methods(pseudo_pvalues = function(times=20, bag.fraction = 0.50,
  replace=FALSE,  nfolds=5, upper.limits=3, lower.limits=0, alpha=1, ...) {

  x <- predict(newdata=get_variables(), type="woe")

  coefs <- list()
  for (i in seq.int(times)) {
    progress_(i, times, "Fitting   ")

    s <- sample.int(nrow(x), nrow(x)*bag.fraction, replace = replace)

    fit <- glmnet::cv.glmnet(x = x[s,], y = performance$y[s],
      weights = performance$w[s], nfolds = 10, alpha = alpha,
      upper.limits=upper.limits, lower.limits=lower.limits, keep=TRUE)

    coefs[[i]] <- coef(fit, s="lambda.min")
  }

  res <- as.matrix(do.call(cbind, coefs))

  ## what is the probability of the coefficient being zero?
  pvals <- sapply(apply(res, 1, ecdf), function(x) x(0))

  structure(
    list(
      pvalues = pvals,
      coefs = res),
    class = "psuedo_pvalues")
})


#' Compare multiple scorecards side-by-side
#'
#' @name Scorecard_compare
#' @details calling adjust enters an interactive variable edit mode. Press "h"
#' for a list of commands.
NULL
Scorecard$methods(compare = function(...) {
  mods <- unlist(list(...))

  ## check that requested models are in the scorecard
  stopifnot(all(mods %in% names(models)))

  on.exit(select(selected_model))

  ## select each and get the summary
  summaries <- lapply(mods, function(x) {

    select(x)
    res <- summary(inmodel.only = TRUE)

    ## only keep vars in model
    res[inmodel, c("IV","Dropped","In Model","Coefs","Contribution")]

  })

  ## merge them all
  contribution <- lapply(summaries, function(x) x[,"Contribution"])
  coefficients <- lapply(summaries, function(x) x[,"Coefs"])

  # merge helper for use with Reduce
  merge_ <- function(a, b) {
    tmp <- merge(a,b, by=0, all=T)
    row.names(tmp) <- tmp$Row.names
    subset(tmp, select = -Row.names)
  }

  res <- merge(
    Reduce(merge_, contribution),
    Reduce(merge_, coefficients), by=0, all=T)

  cols <- c("Contribution", "Coefficients")
  colnames(res) <- c("Variable", paste(rep(cols, each=length(mods)), mods))

  res[order(-res[2], na.last = TRUE),]

})


#' Generate SAS code for Scorecard object
#'
#' @name Scorecard_gen_code_sas
#' @description generate SAS code represenation of the Scorecard object. The SAS
#' code that is genereated calculates the score, adverse action code distances,
#' and provides a set of macro assignments for assigning adverse action codes
#' to particular bin levels.
#' @param pfx character prefix to prepend to variable names
#' by the Scorecard model object. Defaults to 1.
#' @param method method used for calculating the reference level for adverse
#' action codes. Three possible choices:
#' \itemize{
#'  \item{"min" }{Calculate difference from minimum of perf values - default}
#'  \item{"max" }{Calculate difference from maximum of perf values}
#'  \item{"neutral" }{Calculate difference from zero}#'
#'  }
#' @return a character vector of SAS code
NULL
Scorecard$methods(gen_code_sas = function(pfx="", method="min", out=NULL, ...) {

  v <- inmodel
  mod <- models[[selected_model]]

  coefs <- mod@coefs[-1][v]

  if (!is.null(getOption("mkivtools_REGISTERED"))) {
    out <- do.call(c, mkivtools::pkg.env$mkiv_map[tolower(v)])
  }

  ## Print the reason code mappings
  out <- c(out, "/** Adverse Action Code Mappings **/")
  out <- c(out, lapply(seq_along(v), function(i) {
    sprintf("%%let %s_AA_%02d = \"\"; /** %s **/", pfx, i, v[i])
  }))

  ### Print the variables
  out <- c(out, lapply(seq_along(v), function(i) {
    variables[[v[i]]]$gen_code_sas(method=method, pfx=pfx, coef=coefs[i])
  }))

  out <- c(out,
    sprintf("\n/*** Final Score Calculation ***/"),
    sprintf("%s_lgt = %s", pfx, mod@coefs[1]),
    sprintf("  + %s_V%02d_w", pfx, seq_along(v)),
    ";")

  unname(unlist(out))

})


#' Return names of varibales flagged as dropped
#'
#' @name Scorecard_get_dropped
#' @return a character vector of dropped variable names
NULL
Scorecard$methods(get_dropped = function(invert=FALSE) {
  if (invert) setdiff(vnames, dropped) else dropped
})


#' Return names of varibales flagged as inmodel
#'
#' @name Scorecard_get_inmodel
#' @return a character vector of inmodel variable names
NULL
Scorecard$methods(get_inmodel = function(invert=FALSE) {
  if (invert) setdiff(vnames, inmodel) else inmodel
})
