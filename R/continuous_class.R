#' @include bin_class.R
NULL

#' @title Continuous reference class generator
#'
#' @name Continuous_Class
#' @description Continuous object generator class used to wrap binned
#' numeric variables.
#' @export Continuous
#' @exportClass Continuous
Continuous <- setRefClass("Continuous", contains = "Bin")


Continuous$methods(initialize = function(...) {
  callSuper(...)
})


#' Collapse adjacent levels of a Continuous bin object
#'
#' @name Continuous_collapse
#' @param i numeric vector of bin levels to collapse. Must be adjacent.
#' @return modifies the transform object in place.
NULL
Continuous$methods(collapse = function(i) {
    d <- unique(pmax(pmin(tail(i, -1), length(tf@tf) - 1), 2))
    tf@tf <<- tf@tf[-d]
    callSuper()
  }
)


#' Expand a level of a Continuous bin into multiple new levels
#'
#' @name Continuous_expand
#' @param i numeric vector of length 1 indiicating bin level to expand.
#' @details The requested level is divided into quintiles if possible. Duplicate
#' levels are removed.
#' @return modifies the transform object in place.
NULL
Continuous$methods(expand = function(i) {

    stopifnot(length(i) == 1)

    f <- !(is.na(x) | x %in% as.numeric(names(tf@exceptions)))

    a <- min(max(1, i), length(tf@tf))  # can't be smaller than 1
    z <- max(min(i + 1, length(tf@tf)), a) # or larger than max els

    vals <- c(x[x > tf@tf[a] & x <= tf@tf[z] & f])

    q <- c(quantile(vals, seq(0.2, 0.8, 0.2))) # quintiles

    tf@tf <<- sort(unique(c(tf@tf, q)))
    callSuper()
  }
)


#' Helper function to format Continuous bin labels
#'
#' @name Continuous_fmt_numeric_cuts
#' @return character vector of bin labels
NULL
Continuous$methods(fmt_numeric_cuts = function() {
  fmt <- sprintf("(%%%1$ds - %%%1$ds]", max(nchar(tf@tf))) ## get width of largest value
  sprintf(fmt, head(tf@tf, -1), tail(tf@tf, -1))
})

#' Factorize for Continuous bins
#'
#' @name Continuous_factorize
#' @param newdata Numeric vector on which to apply the transformation. Defaults
#' to the \code{x} field of the Continuous object
#' @details \code{factorize} returns a list with two fields:
#' \itemize{
#'  \item{factor }{ Factor with the bin labels applied to \code{x}}
#'  \item{types }{ list of logical vectors for missing, exception, and normal}
#' }
#' @return \code{list} with two fields. See details.
NULL
Continuous$methods(factorize = function(newdata=.self$x) {
  f <- callSuper(newdata=newdata)

  lbls <- fmt_numeric_cuts()
  out <- factor(newdata, exclude=NULL,
    levels = c(lbls, names(tf@exceptions), NA))

  levels(out)[is.na(levels(out))] <- "Missing"
  out[f$normal] <- cut(newdata[f$normal], tf@tf, include.lowest = T,
    labels = lbls)

  list(factor=out, types=f)
})

#' Weight-of-Evidence subistitution for Continuous bins
#'
#' @name Continuous_predict
#' @param newdata numeric vector to apply performance substition. Defaults to
#' data used to create the Continuous object.
#' @return numeric variable with bin performance values substituted for
#' the inputs.
NULL
Continuous$methods(predict = function(newdata=.self$x) {
  stopifnot(is.numeric(newdata))
  callSuper(newdata=newdata)
})


#' Generate SAS code for Continuous object
#'
#' @name Continuous_gen_code_sas
#' @description generate SAS code representing the transformation from input
#' numeric values to the substituted performance values. Also generates code
#' calculating difference from min/max/neutral and adverse action code
#' assignments.
#' @param pfx character prefix to prepend to variable names
#' @param coef numeric coefficient to multiply performance values by. Passed in
#' by the Scorecard model object. Defaults to 1.
#' @param method method used for calculating the reference level for adverse
#' action codes. Three possible choices:
#' \itemize{
#'  \item{"min" }{Calculate difference from minimum of perf values - default}
#'  \item{"max" }{Calculate difference from maximum of perf values}
#'  \item{"neutral" }{Calculate difference from zero}#'
#'  }
#' @param i numeric value enumerating the variables. Passed in from other code.
#' @return a character vector of SAS code
NULL
Continuous$methods(gen_code_sas = function(pfx="", coef=1, method="min", i=1, ...) {

  val <- tail(head(tf@tf, -1), -1)
  p <- tf@subst * coef

  ref <- switch(method,"min" = min(p), "max" = max(p), "neutral" = 0, min(p))

  E <- names(tf@exceptions)
  m <- if (length(tf@nas) == 0) 0 else tf@nas * coef
  e <- if (length(tf@exceptions) == 0) 0 else tf@exceptions * coef

  ## WoE Substitution
  c(sprintf("\n/*** %s ***/", name),
    sprintf("if missing(%s)\n  then %s_V%02d_w = %s;", name, pfx, i, m),
    sprintf("else if %s = %s\n  then %s_V%02d_w = %s;", name, E, pfx, i, e),
    sprintf("else if %s <= %s\n  then %s_V%02d_w = %s;", name, val, pfx, i,
      head(p, -1)),
    sprintf("else %s_V%02d_w = %s;" , pfx, i, tail(p, 1)),

    ## Reason Codes
    sprintf("\nif missing(%s)\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";",
      name, pfx, i, pfx, i),
    sprintf("else if %s = %s\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";",
      name, E, pfx, i, pfx, i),
    sprintf("else if %s <= %s\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";",
      name, val, pfx, i, pfx, i),
    sprintf("else %s_AA_code_%02d = \"&%s_AA_%02d\";", pfx, i, pfx, i),

    ## Distance Calculations
    sprintf("\n%s_AA_dist_%02d = %s - %s_V%02d_w;", pfx, i, ref, pfx, i))
})


#' Set monotoncity for Continuous bins
#'
#' @name Continuous_mono
#' @param m the monotonic relationship to enforce.
#' @description \code{mono} calls the \code{bin} function with the requested
#' monotoncity. The variable is discretized while enforcing the monotonicity.
#' The possible values are:
#' \itemize{
#'  \item{0 }{ No monotoncity enforced - the default.}
#'  \item{1 }{ Increasing monotoncically with the \code{y}}
#'  \item{-1 } {Decreasing monotoncically with the \code{y}}
#'  \item{2 }{ Either increasing or decreasing montonically with the \code{y}}
#' }
NULL
Continuous$methods(mono = function(m) {
  args$mono <<- m
  do.call(perf$bin, c(list(b=.self), args))
  update()
})


#' Set the exception values for Continuous bins
#'
#' @name Continuous_exceptions
#' @param e numeric vector of exception values to withhold from binning
NULL
Continuous$methods(exceptions = function(e) {
  stopifnot(is(e, "numeric"))
  args$exceptions <<- e
  tf@exceptions <<- setNames(rep(0, length(e)), e)
  update()
})


#' Explicity set bin boundaries for Continuous objects
#'
#' @name Continuous_set_cutpoints
#' @param cuts space-separated list of numeric cutpoints
#' @description \code{set_equal} sets the performance summary value of \code{i1}
#' equal to that of \code{i2}. This can be used to force two bins to have the
#' same substituted value.
NULL
Continuous$methods(set_cutpoints = function(cuts) {
  cuts <- sort(unique(c(-Inf, cuts, Inf)))
  tf@tf <<- cuts
  update()
})
