#' @include performance_class.R
NULL

## #' @export Classing
## #' @exportClass Classing


#' @title Class Classing
#' @name Classing-class
#' @description Classing class that wraps a data.frame and prepares it for
#' scorecard modeling.
#' @field variables list of binned variables
#' @field vnames character vector of binned variable names
#' @field performance Performance object with \code{y} and \code{w} values and
#' implementation of the Performance object interface.
#' @field dropped character vector of variable names that are flagged as
#' dropped
Classing <- setRefClass("Classing",
  fields = c(
    variables = "list",
    vnames = "character",
    performance = "Performance",
    dropped = "character",
    step = "integer"))


#' generic method for create_bin
#' @name create_bin
#' @param x the varibale used for the Bins \code{x} value
#' @param ... additional arguments passed on to the Bin constructors
setGeneric("create_bin", function(x, ...) callGeneric("create_bin"))


#' @describeIn create_bin wrap variable in Continuous object
#' @return a Continuous object
setMethod("create_bin", "numeric", function(x, ...) {
  binnr::Continuous$new(x = x, ...)
})


#' @describeIn create_bin wrap variable in Discrete object
#' @return a Discrete object
setMethod("create_bin", "factor", function(x, ...) {

  if (length(levels(x)) > 20) {
    nm <- list(...)$name
    warning(sprintf("Variable, %s, has more than 20 levels -- Skipping", nm),
      call. = FALSE)
    return(NULL)
  }
  binnr::Discrete$new(x = x, ...)
})


#' @describeIn create_bin create_bin fallback warning user
setMethod("create_bin", "character", function(x, ...) {
    create_bin(factor(x), ...)
})


#' @describeIn create_bin create_bin fallback warning user
setMethod("create_bin", "ANY", function(x, ...) {
  warning(sprintf("Class: %s cannot be binned. Removed from classing.",
    class(x)), call. = FALSE)
  NULL
})


Classing$methods(initialize = function(data=NULL,
  performance=Performance$new(), ...) {

  ## drop variables with all NA
  all_na <- names(which(sapply(data, function(x) all(is.na(x)))))

  if (length(all_na) > 0) {
    warning(sprintf("dropping variables with all NA values: %s",
      paste0(all_na, collapse = ", ")), call. = FALSE)
  }

  .self$performance <<- performance
  vnames <<- setdiff(names(data), all_na)

  variables <<- lapply(vnames, function(nm) {
    create_bin(x = data[[nm]], perf = performance, name = nm, ...)
  })

  names(variables) <<- vnames

  ## drop variables that aren't numeric or factors
  f <- !sapply(variables, is.null)

  variables <<- variables[f]
  vnames <<- vnames[f]

  ## start all models as step 3
  step <<- setNames(seq_along(vnames), vnames)
  step[] <<- 2L ## start all variables as step 2

})


#' Subsequent call from the bin function passed to Classing object
#'
#' @name Classing_bin
#' @description This bin function should not be directly called by the user.
#' The Classing bin function is subsequently called from the
#' \code{\link{bin}} wrapper function.
NULL
Classing$methods(bin = function(...) {
  on.exit(cat(sep = "\n"))

  for (i in seq_along(variables)) {
    progress_(i, length(variables), "Binning   ", variables[[i]]$name)
    variables[[i]]$bin(...)
  }

  ## drop vars with zero information value
  zeros <- sapply(variables, function(b) b$sort_value()) == 0

  dropped <<- names(variables)[zeros]

})

Classing$methods(get_variables = function(keep=FALSE) {
  if (!keep) {
    lapply(variables[step == 1L & !is.na(step)], function(x) x$x)
  } else {
    lapply(variables, function(x) x$x[s])
  }
})


Classing$methods(get_transforms = function(keep=FALSE) {
  if (!keep) {
    lapply(variables[step == 1L & !is.na(step)], function(x) x$tf)
  } else {
    lapply(variables, function(x) x$tf)
  }
})


#' Flag supplied variables as dropped
#'
#' @name Classing_drop
#' @param vars character vector of variables to drop
#' @param all logical indicating whether all variables should be dropped
NULL
Classing$methods(drop = function(vars=character(0), all=FALSE, ...) {
  if (all) {
    dropped <<- vnames
  } else {
    stopifnot(all(vars %in% vnames))
    dropped <<- unique(c(dropped, vars))
  }
})



#' Summarize the Classing object
#'
#' @name Classing_summary
#' @return a matrix summarizing the independent variables using the Performance
#' object summary function
NULL
Classing$methods(summary = function() {

  s <- lapply(variables, function(v) v$summary())
  res <- do.call(rbind, s)

  res
})


#' Add variables from one classing to another
#'
#' @name Classing_combine
#' @param other an object of class \code{\link{Classing-class}} that will be
#' incorporated into the calling method's Classing object
#' @details \code{combine} adds a Classing object's variables to the caller
#' object. The 'other' Classing object must not have any of the same variables.
#' It must also use the same performance as the calling method's Classing
#' object.
#' @return an updated Classing object
NULL
Classing$methods(combine = function(other) {
  stopifnot(is(other, "Classing"))

  ## check for name collisions
  if (any(other$vnames %in% vnames)) {

    dups <- other$vnames[other$vnames %in% vnames]

    stop("duplicate variables from 'other' found in Classing: ",
      paste0(dups, collapse = ", "), call. = FALSE)
  }

  ## check that the same performance is used
  if (!identical(all.equal(performance, oth$performance), TRUE)) {
    stop("'other' performance does not match current performance",
      call. = FALSE)
  }

  ## if all checks are passed, can add the other variables to this

  variables <<- c(variables, other$variables)
  vnames <<- c(vnames, other$vnames)
  dropped <<- c(dropped, other$dropped)

})



#' Flag supplied variables as dropped
#'
#' @name Classing_set_step
#' @param vars character vector of variables to drop
#' @param all logical indicating whether all variables should be dropped
NULL
Classing$methods(set_step = function(vars=character(0), lvl=1L) {

  ## only using two levels
  stopifnot(lvl[1] %in% as.integer(1:2))
  step[vars] <<- lvl

})



Classing$methods(predict = function(newdata=NULL, keep=FALSE) {

  #browser()

  on.exit(cat(sep = "\n"))

  ## grab names from the step variable
  vnm <- if (keep) names(step) else names(which(step == 1))

  if (is.null(newdata)) {
    newdata <- .self$get_variables(keep = keep)
  }

  ## check that data has var names
  stopifnot(!is.null(names(newdata)))
  dnm <- names(newdata)

  ## check that all variables are found in newdata
  if (!all(vnm %in% dnm)) {
    msg <- paste0(vnm[!vnm %in% dnm], collapse = ", ")
    stop(sprintf("Vars not found in data: %s", msg), call. = F)
  }

  ## put the newdata in the same order as the variables
  func <- function(i, b, v) {
    progress_(i, length(vnm), "Predicting", b$name)
    b$predict(newdata=v)
  }

  mapply(func, setNames(seq_along(vnm), vnm), variables[vnm], newdata[vnm],
    SIMPLIFY = FALSE)

})