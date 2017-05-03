#' @include performance_class.R transform_class.R
NULL

setClassUnion("NumericOrFactor", members = c("numeric", "factor"))

#' Bin reference class generator
#'
#' @name Bin-class
#' @description Bin object generator class used to wrap binned variables.
#' @field x numeric or factor vector to be discretized or summarized
#' @field name name of the variable
#' @field perf \code{\link{Performance-class}} object used to discretize and
#' summarize \code{x}. Currently only binary performance supported.
#' @field tf \code{\link{Transform-class}} object containing information for
#' discretizing and summarizing \code{x}.
#' @field history \code{list} of \code{Transform} objects. One for every
#' operation on the \code{Bin} object.
#' @field args saved arguments that were used to call the bin method.
#' @export Bin
#' @exportClass Bin
Bin <- setRefClass("Bin",
  fields = c(
    x = "NumericOrFactor",
    name = "character",
    perf = "Performance",
    tf = "Transform", ## current transform
    history = "list", ## current + all previous transforms
    args = "list"
    ),
  contains = "VIRTUAL")

Bin$methods(initialize = function(name="Unknown", x, perf, ...) {
  ## perform bin checks here
  stopifnot(length(x) > 0)
  callSuper(name=name, x=x, perf=perf, ...)
  stopifnot(length(x) == length(perf$y))
})

#' Update the Bin object after performing requested operations
#'
#' @name Bin_update
#' @description The update method is called any time the user makes changes to
#' the Bin object. The transform object is changed and the history list is
#' updated.
NULL
Bin$methods(update = function() {

  result <- perf$update(b = .self)

  ## do it in the result!
  result <- lapply(result, function(v) {
    v[!is.finite(v)] <- 0

    ## find witch levels are in the overrides
    i <- intersect(names(tf@overrides), row.names(v))
    v[i, "Pred"] <- tf@overrides[i]
    v
  })

  tf <<- update_transform(tf, result)

  ## append to the history and the cache
  history <<- c(history, list(tf))
  invisible()
})

#' Discretize and summarize variables
#'
#' @name Bin_bin
#' @description The bin function is the entry point for discretizing and
#' summarizing variables by some arbitray performance metric. Each Performance
#' object must implement a bin method for numeric and factor variables.
#' Currently only binary performance is supported.
#'
#' @param ... arguments passed on to the Performance object's \code{bin}
#' implementation. This varies depending on the performance type.
#' @return \code{bin} creates a Transform object and adds it to the Bin object.
NULL
Bin$methods(bin = function(...) {
  perf$bin(b=.self, ...)
  args <<- modifyList(args, list(...))
  update()
})

#' Collapse levels of a Bin object
#'
#' @name Bin_collapse
#' @param i numeric vector of bin levels to collapse.
#' @seealso Continuous_collapse Discrete_collapse
#' @return modifies the transform object in place.
NULL
Bin$methods(collapse = function() {
  update()
})


#' Expand a level of a Bin into multiple new levels
#'
#' @name Bin_expand
#' @param i numeric vector of length 1 indiicating bin level to expand.
#' @details All of the collapsed levels will be expanded.
#' @seealso Continuous_expand Discrete_expand
#' @return modifies the transform object in place.
NULL
Bin$methods(expand = function(...) {
  "Updated Bin after inherited expand"
  update()
})


#' Factrorize superclass method
#'
#' @name Bin_factorize
#' @param newdata vector on which to apply the transformation. Defaults to the
#' \code{x} field of the Bin object
#' @details \code{factorize} returns a list with three fields:
#' \itemize{
#'  \item{normal }{ logical vector indicating non-missing, non-exceptoin values}
#'  \item{exception }{ logical vector indicating exception values}
#'  \item{missing }{ logical vector indicating missing values}
#' }
#' @return \code{list} with three fields. See details.
NULL
Bin$methods(factorize = function(newdata=.self$x) {
  "Return list of filters for exceptions, missing, and normal values"
  val_nas <- is.na(newdata)
  val_exc <- newdata %in% as.numeric(names(tf@exceptions))
  val_nrm <- !(val_nas | val_exc)
  list(normal = val_nrm, exception = val_exc, missing = val_nas)
})


#' Return matrix summarizing Bin object
#'
#' @name Bin_as.matrix
#' @details \code{as.matrix} summarizes the \code{\link{Performance-class}}
#' object within each level of the Bin object. As such, the summarization
#' process must be described by implementing a summarize method for the
#' Performance object.
#'
#' @return \code{matrix} of summarized bin data
NULL
Bin$methods(as.matrix = function() {
  if (length(tf@repr) == 0) {
    stop("`bin` function not called yet.", call. = FALSE)
  }

  round(do.call(rbind, tf@repr), 3)
})


#' Print representation of Bin object
#'
#' @name Bin_show
#' @param ... optional arguments passed on to the \code{\link{print}} function.
NULL
Bin$methods(show = function(...) {

  m <- as.matrix()

  ## add row labels
  lbls <- sprintf("[%02d]  ", seq.int(nrow(m)))
  lbls[length(lbls)] <- ""

  row.names(m) <- paste0(lbls, row.names(m))

  cat(name, sep="\n")
  print(m, ...)

})


#' Undo the last operation
#'
#' @name Bin_undo
NULL
Bin$methods(undo = function() {
  if (length(history) > 1) {
    tf <<- history[[length(history) - 1]]
    history <<- head(history, -1)
  } else {
    tf <<- history[[1]]
    history <<- list()
  }
  show()
})


#' Reset the Bin to the original settings
#'
#' @name Bin_reset
#' @description \code{reset} re-bins the object using the \code{args} that were
#' saved during the first call to \code{bin}.
NULL
Bin$methods(reset = function() {
  do.call(.self$perf$bin, c(list(b=.self), args))
  tf@overrides <<- numeric(0)
  update()
})


#' Set one level equal to another
#'
#' @name Bin_set_equal
#' @param i1 index of target bin
#' @param i2 index of source bin
#' @description \code{set_equal} sets the performance summary value of \code{i1}
#' equal to that of \code{i2}. This can be used to force two bins to have the
#' same substituted value.
NULL
Bin$methods(set_equal = function(i1, i2) {
  tf <<- set_equal_(tf, i1, i2)
  update()
})


#' Set performance value to zero
#'
#' @name Bin_neutralize
#' @param i numeric vector of bin indices to neutralize.
#' @description \code{neutralize} sets the performance substitution value of
#' the requested indices to zero. This has the effect of neither adding nor
#' subtracting points in the final score.
NULL
Bin$methods(neutralize = function(i) {
  tf <<- neutralize_(tf, i)
  update()
})


#' Plot the Bin object
#'
#' @name Bin_plot
NULL
Bin$methods(plot = function() {
  perf$plot(b = .self)
})


#' Substitute weight-of-evidence for the input \code{x} values
#'
#' @name Bin_predict
#' @param newdata vector of the same type as \code{x} for which to substitute
#' the bin weight-of-evidence values.
NULL
Bin$methods(predict = function(newdata=.self$x) {
  idx <- factorize(newdata=newdata)

  out <- c(tf@subst, tf@nas, tf@exceptions)[idx]

  ors <- intersect(names(out), names(tf@overrides))
  out[ors] <- tf@overrides[ors]

  out
})


#' Access and return the sort value for the performance object
#'
#' @name Bin_sort_value
NULL
Bin$methods(sort_value = function() {
  perf$sort_value(b=.self)
})


#' Return summary information for Bin based on associated performance object
#'
#' @name Bin_summary
NULL
Bin$methods(summary = function(tf=.self$tf) {
  perf$summary(tf = tf)
})


#' Return object for exporting to Excel
#'
#' @name Bin_get_excel_table
NULL
Bin$methods(get_excel_table = function() {
  perf$get_excel_table(b=.self)
})
