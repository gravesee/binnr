#' @include classing_class.R classing_adjust_method.R
NULL

#' @title Scorecard reference class generator
#' @name Scorecard-class
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
   inmodel = "character",
   steptwo = "numeric"),
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




#' Summarize the currently selected model
#'
#' @name Scorecard_summary
#' @param keep whether to summarize droppped variables as well
#' @return a matrix summarizing the independent variables using the performance
#' implementation summary function. Also displays the coefficients and model
#' contributions of the predictors.
NULL
Scorecard$methods(summary = function() {

  mod <- models[[selected_model]]

  cat(mod@name, "\nOut-of-Fold KS: ", mod@ks, "\n")

  res <- callSuper()

  ## get the contributions
  contr <- round(sapply(variables, function(x) sum(x$tf@contribution)), 5)

  out <- cbind(res, Step=step, Contr=contr)

  out

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

  ## use IV if not yet fit otherwise use contribution
  if (selected_model == "scratch") {
    v <- sapply(variables, function(x) x$sort_value())
  } else {
    v <- sapply(variables, function(x) sum(x$tf@contribution))
  }

  i <- order(step, -v, na.last = TRUE)

  variables <<- variables[i]
  step <<- step[names(variables)]

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
    res <- summary()

    ## only keep vars in model
    res[, c("IV", "Step", "Contr")]

  })

  ## merge them all
  # contribution <- lapply(summaries, function(x) x[,"Contr"])

  # merge helper for use with Reduce
  merge_ <- function(a, b) {
    tmp <- merge(a, b, by=0, all=T, sort=FALSE)
    row.names(tmp) <- tmp$Row.names
    subset(tmp, select = -Row.names)
  }


  res <- Reduce(merge_, summaries)

  cols <- paste(c("IV", "Step", "Contr"), rep(mods, each=3), sep = ".")
  colnames(res) <- cols

  res
  # res[order(-res[2], na.last = TRUE),]

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
#'  \item{"neutral" }{Calculate difference from zero}
#'  }
#' @return a character vector of SAS code
NULL
Scorecard$methods(gen_code_sas = function(pfx="", method="min", ...) {

  out <- character(0)
  v <- names(which(step == 1))
  mod <- models[[selected_model]]

  # coefs <- mod@coefs[-1][v]

  if (getOption("mkivtools_REGISTERED", default = FALSE)) {
    out <- do.call(c, lapply(v, mkivtools::get_mkiv_code))
    # out <- do.call(c, mkivtools::pkg.env$mkiv_map[tolower(v)])
  }

  ## Print the reason code mappings
  out <- c(out, "/** Adverse Action Code Mappings **/")
  out <- c(out, lapply(seq_along(v), function(i) {
    sprintf("%%let %s_AA_%02d = \"\"; /** %s **/", pfx, i, v[i])
  }))

  ### Print the variables
  out <- c(out, lapply(seq_along(v), function(i) {
    variables[[v[i]]]$gen_code_sas(method=method, pfx=pfx, i=i)
  }))

  ### TODO ###

  out <- c(out,
    sprintf("\n/*** Final Score Calculation ***/"),
    sprintf("%s_lgt = %s + sum(of: %s_V:);", pfx, mod@coefs["(Intercept)"], pfx))

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
Scorecard$methods(fit = function(name=NULL, description="", overwrite=FALSE,
  newdata=.self$get_variables(), y=performance$y, w=performance$w,
  nfolds=5, upper.limits=3, lower.limits=0, alpha=0,
  family="binomial", ...) {


  if (is.null(name)) {
    name <- sprintf("model%d", length(models))
  }

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

  if (!any(step %in% 1)) stop("no step 1 variables in classing", call. = F)
  ## get step one fields

    ## change this to make step 3?
  v <- names(step)[step %in% 1]

  # browser()
  x <- lapply(variables[v], function(x) x$predict_sparse())

  l <- sapply(x, ncol)
  x <- do.call(cbind, x)

  set.seed(seed)
  this_fit <- cv.glmnet(x = x, y = y, weights = w, nfolds = nfolds,
    family=family, alpha=alpha, keep=TRUE, ...)

  betas <- coef(this_fit, s="lambda.min")

  ## split the coefficients up
  wgts <- setNames(split(betas[-1],  rep(seq_along(l), l)), names(l))
  coefs <- c(`(Intercept)`=betas[1], unlist(wgts))

  #### PERFORMANCE METRICS ####
  ## calculate per-level contributions & total contributions
  lvls <- contributions_(x, coefs, y, w)
  lvls <- setNames(split(lvls, rep(seq_along(l), l)), v)
  contr <- sapply(lvls, sum)

  ## overwrite the bin weights
  for (v in names(wgts)) {
    variables[[v]]$set_overrides(wgts[[v]])
    variables[[v]]$tf@contribution <<- lvls[[v]]
  }

  ks <- ks_(this_fit$fit.preval[,which.min(this_fit$cvm)], y, w) # kfold

  ### add weights for step two predictors
  if (any(step %in% 2)) {

    phat <- glmnet::predict.cv.glmnet(this_fit, x)
    s2 <- names(step)[step %in% 2]

    # set.seed(seed)

    s2_wgts <- list()
    s2_lvls <- list()
    for (v in s2) {
      x <- variables[[v]]$predict_sparse()

      s2_fit <- cv.glmnet(x = x, y = y, weights = w, nfolds = nfolds,
        family=family, alpha=alpha, keep=FALSE, offset=phat, ...)

      s2_coefs <- coef(s2_fit, s="lambda.min")[,1]
      s2_wgts[[v]] <- s2_coefs[-1]

      variables[[v]]$set_overrides(s2_wgts[[v]])
      variables[[v]]$tf@contribution <<- contributions_(x, s2_coefs, y, w)
    }
  }

  ## store the last transforms
  m <- new("Model", name=name, description=description, dropped=dropped,
    transforms=get_transforms(), coefs=coefs, ks=ks, fit=this_fit)

  add_model(m)
  sort()

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
Scorecard$methods(predict = function(newdata=NULL, keep=FALSE, type=c("score", "woe", "labels", "indicators"), ...) {

  type <- match.arg(type)

  mod <- models[[selected_model]]
  woe <- do.call(cbind, callSuper(newdata, keep=keep))
  row.names(woe) <- NULL

  if (type == "score") {

    return(rowSums(woe) + mod@coefs["(Intercept)"])

  } else if (type == "woe") {

    return(woe)

  } else if (type == "labels") {


    stop("Not implemented yet")


  } else if (type == "indicators") {

    callSuper(newdata=newdata)


  } else {

    stop("invalid prediction type requested")
  }

})



Scorecard$methods(secondary_performance = function(name=NULL, description="",
  overwrite=FALSE, newdata=NULL, y, w=.self$performance$w, epsilon=0.10,
  family="gaussian", alpha=0, nfolds=5, ...) {

  # browser()


  if (is.null(name)) {
    name <- sprintf("model%d", length(models))
  }

  ## check for consistent dimensions
  if (length(y) != length(w)) {
    stop("y and w must be the same length", call. = FALSE)
  }

  if (!overwrite) {
    if (name %in% names(models)) {
      stop("Model name already exists and overwrite=FALSE",
        call. = FALSE)
    }
  }

  ## fit another glmnet model with lower and upper limits on the coefficients
  mod <- models[[selected_model]]
  v <- names(step)[step %in% 1]

  # browser()
  x <- lapply(variables[v], function(x) x$predict_sparse())

  if (nrow(x[[1]]) != length(y)) {
    stop("newdata and y must be the same length", call. = FALSE)
  }

  l <- sapply(x, ncol)
  x <- do.call(cbind, x)

  f <- !is.na(y)

  set.seed(seed)
  this_fit <- cv.glmnet(x = x[f,], y = y[f], weights = w[f], nfolds = nfolds, family=family,
    alpha=alpha, keep=TRUE, upper=epsilon, lower=-epsilon, ...)

  ## add modified betas to the original betas
  betas <- coef(this_fit, s="lambda.min")
  betas <- unlist(setNames(split(betas[-1],  rep(seq_along(l), l)), names(l)))

  coefs <- c(mod@coefs[1], betas + mod@coefs[names(betas)]) ## index using the original coef order
  wgts <- setNames(split(coefs[-1],  rep(seq_along(l), l)), names(l))

  ## order them the same way

  #### PERFORMANCE METRICS ####
  ## calculate per-level contributions & total contributions
  lvls <- contributions_(x, coefs, .self$performance$y, .self$performance$y)
  lvls <- setNames(split(lvls, rep(seq_along(l), l)), v)
  contr <- sapply(lvls, sum)

  ## overwrite the bin weights
  for (v in names(wgts)) {
    variables[[v]]$set_overrides(wgts[[v]])
    variables[[v]]$tf@contribution <<- lvls[[v]]
  }

  m <- new("Model", name=name, description=description, dropped=character(),
    transforms=get_transforms(), coefs=coefs, ks=-1, fit=this_fit)

  add_model(m)
  sort()

})



