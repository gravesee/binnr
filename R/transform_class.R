
#' Transfrom reference class generator
#'
#' @name Transform-class
#' @description Transform object class definition. The Transform object controls
#' how an independent variable is binned and manages missing values, exceptions,
#' overrides, and weight-of-evidence substitution.
#' @slot tf the mapping of input values to bin levels
#' @slot subst the WoE substitution for the mapped values
#' @slot exceptions numeric vector of exception values
#' @slot nas WoE value for missing value sof \code{x}
#' @slot overrides named vector of bin levels to overriden substituion values
#' @slot repr cached, tabular representation of the bin object for quick display
#' @slot neutralized which levels are neutrazlied
#' @exportClass Transform
setClass("Transform", slots = c(
  tf = "ANY",
  subst = "numeric",
  exceptions = "numeric",
  nas = "numeric",
  overrides = "numeric",
  repr = "list",
  neutralized = "character"),
  prototype = list(nas = c(Missing=0), neutralized="Missing")
)


setMethod("initialize", "Transform", function(.Object, ...) {
  .Object <- callNextMethod()
  .Object@overrides <- .Object@nas
  validObject(.Object)
  .Object
})


#' Neutralize selected levels of Transform setting substitution to zero
#'
#' @name neutralize
#' @param tf Transform object
#' @param i numeric vector of levels to neutralize
#' @return new Transform object with updated overrides
neutralize_ <- function(tf, i) {
  x <- names(c(tf@subst, tf@exceptions, tf@nas))
  if (!all(i %in% seq_along(x))) return(tf)

  ## ones that are already neutralized are UN-neutralized
  neutral <- names(which(tf@overrides == 0))
  nix <- intersect(neutral, x[i])

  to_drop <- match(nix, names(tf@overrides), 0)
  if (length(to_drop) > 0) {
    tf@overrides <- tf@overrides[-to_drop]
  }

  overrides <- setdiff(x[i], nix)
  tf@overrides[overrides] <- 0

  ## Experimental
  tf@neutralized <- unique(c(tf@neutralized, x[i]))

  tf
}


#' Set substitution of one level equal to substitution of another
#'
#' @name set_equal
#' @param tf Transform object
#' @param v1 level to override
#' @param v2 value with which to override v1's substitution
#' @return new Transform object with updated overrides
set_equal_ <- function(tf, v1, v2) {

  if (! (length(v1) == 1L & length(v2) == 1L) ) return(tf)

  x <- c(tf@subst, tf@exceptions, tf@nas)
  if (!(all(c(v1, v2)) %in% seq_along(x))) return(tf)

  overrides <- setNames(x[v2], names(x)[v1])
  tf@overrides[names(overrides)] <- overrides

  tf

}


#' Update Transform object with new information after Bin operations
#'
#' @name update_transform
#' @param tf Transform object
#' @param result list of summarized performane values for the updated Bin object
#' @return new Transform object with updated subst, nas, exceptions, and repr
update_transform <- function(tf, result) {

  tf@subst <- setNames(result$normal[,"Pred"], row.names(result$normal))
  tf@nas <- c(Missing=result$missing[,"Pred"])

  exception_names <- row.names(result$exception)
  tf@exceptions[exception_names] <- result$exception[,"Pred"]

  tf@repr <- result

  tf
}



#' Set substitution of one level equal to substitution of another
#'
#' @name set_equal
#' @param tf Transform object
#' @param v1 level to override
#' @param v2 value with which to override v1's substitution
#' @return new Transform object with updated overrides
set_overrides_ <- function(tf, v1) {
  x <- c(tf@subst, tf@exceptions, tf@nas)
  tf@overrides <- setNames(v1, names(x)) ## neutralized aren't even covariates
  tf
}

