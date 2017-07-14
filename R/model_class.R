#' @title Model class definition
#'
#' @name Model-class
#' @description The Model object contains all of the information describing a
#' fitted binnr Scorecard. This includes a list of the Transforms used for WoE
#' substitution, which variables entered the model, and some performance
#' metrics.
#' @slot name short model identifier
#' @slot description short description of the model
#' @slot settings saved list of settings used to bin and fit the model
#' @slot transforms list of Bin Transforms used to fit the model
#' @slot dropped character vector denoting which variables were dropped at time
#' of fit
#' @slot inmodel character vector denoting which variables entered the model
#' @slot steptwo named numeric vector of how varaibles would enter the model
#' if the lambda value was lowered (a "looser" fitting model).
#' @slot coefs intercept and coefficients of the fitted model as a numeric
#' vector
#' @slot contribution some measure of variable contribution to the model's
#' predictive power
#' @slot ks statistic measuring the separation of the two performance classes
#' @slot fit the result of the cv.glmnet function call
#' @exportClass Model
setClass("Model", slots = c(
  name = "character",
  description = "character",
  settings = "list",
  transforms = "list",
  dropped = "character",
  inmodel = "character",
  steptwo = "numeric",
  coefs = "numeric",
  contribution = "numeric",
  ks = "numeric",
  fit = "ANY"))
