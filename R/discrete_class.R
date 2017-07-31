#' @include bin_class.R
NULL

# #' @exportClass Discrete

#' Discrete class
#'
#' Discrete object generator class used to wrap binned factor variables.
#'
#' @export Discrete
Discrete <- setRefClass("Discrete", contains = "Bin")

Discrete$methods(initialize = function(x_, ...) {
  ## get rid of pesky blank levels
  levels(x_)[levels(x_) == ""] <- NA
  x_ <- droplevels(x_)
  callSuper(x=x_, ...)
})


#' Collapse levels of a Discrete bin object
#'
#' @name Discrete_collapse
#' @param i numeric vector of bin levels to collapse. Do not have to be
#' adjacent.
#' @return modifies the transform object in place.
NULL
Discrete$methods(collapse = function(i) {
  f <- which(tf@tf %in% unique(tf@tf)[i]) ## which values were selected for collapse?
  tf@tf[f] <<- paste(names(tf@tf)[f], collapse=',') # collapse them with commas
  callSuper()
})


#' Expand a level of a Discrete bin into multiple new levels
#'
#' @name Discrete_expand
#' @param i numeric vector of length 1 indiicating bin level to expand.
#' @details All of the collapsed levels will be expanded.
#' @return modifies the transform object in place.
NULL
Discrete$methods(expand = function(i) {
  f <- tf@tf %in% unique(tf@tf)[i]
  tf@tf[f] <<- levels(x)[f]
  callSuper()
})


#' Factorize for Discrete bins
#'
#' @name Discrete_factorize
#' @param newdata Factor vector on which to apply the transformation. Defaults
#' to the \code{x} field of the Discrete object
#' @details \code{factorize} returns a list with two fields:
#' \itemize{
#'  \item{factor }{ Factor with the bin labels applied to \code{x}}
#'  \item{types }{ list of logical vectors for missing, exception, and normal}
#' }
#' @return \code{list} with two fields. See details.
NULL
Discrete$methods(factorize = function(newdata=.self$x) {

  out <- newdata
  levels(out) <- unlist(tf@tf)[levels(out)]
  out <- addNA(out)
  levels(out)[is.na(levels(out))] <- "Missing"

  out
})


#' Weight-of-Evidence substitution for Discrete bins
#'
#' @name Discrete_predict
#' @param newdata Factor vector to apply performance substition. Defaults to
#' data used to create the Discrete object.
#' @return numeric variable with bin performance values substituted for
#' the inputs.
NULL
Discrete$methods(predict = function(newdata=.self$x) {
  if (is.character(newdata)) newdata <- factor(newdata)
  stopifnot(is.factor(newdata))
  callSuper(newdata=newdata)
})


#' Generate SAS code for Discrete object
#'
#' @name Discrete_gen_code_sas
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
Discrete$methods(gen_code_sas = function(pfx="", method="min", i=1, ...) {

  val <- gsub(",", "','", names(tf@subst))
  # p <- tf@subst * coef
  p <- tf@subst

  ref <- switch(method,"min" = min(p), "max" = max(p), "neutral" = 0)
  # m <- if (length(tf@nas) == 0) 0 else tf@nas * coef
  m <- if (length(tf@nas) == 0) 0 else tf@nas

  ## WoE Substitution
  c(sprintf("\n/*** %s ***/", name),
    sprintf("if missing(%s)\n  then %s_V%02d_w = %s;", name, pfx, i, m),
    sprintf("else if %s in ('%s')\n  then %s_V%02d_w = %s;",
      name, val, pfx, i, p),
    sprintf("else %s_V%02d_w = 0;", pfx, i),

    ## AA Code
    sprintf("\nif missing(%s)\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";",
      name, pfx, i, pfx, i),
    sprintf("else if %s in ('%s')\n  then %s_AA_code_%02d = \"&%s_AA_%02d\";",
      name, val, pfx, i, pfx, i),
    sprintf("else %s_AA_code_%02d = \"&%s_AA_%02d\";", pfx, i, pfx, i),

    ## AA Dist
    sprintf("\n%s_AA_dist_%02d = %s - %s_V%02d_w;", pfx, i, ref, pfx, i))
})


Discrete$methods(mono = function(m) {
  cat("Monotoncity does not apply to Discrete variables")
  return(invisible(NULL))
})


Discrete$methods(exceptions = function(e) {
  cat("Exceptions are not used with Discrete variables")
  return(invisible(NULL))
})

Discrete$methods(set_cutpoints = function(cuts) {
  cat("Cut Points cannot be set for Discrete variables")
  return(invisible(NULL))
})
