setOldClass("constraint_map", "constraint_map")

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
#' @slot constraints list of pairs of constraints
#' @exportClass Transform
setClass("Transform", slots = c(
  tf = "ANY",
  subst = "numeric",
  exceptions = "numeric",
  nas = "numeric",
  overrides = "numeric",
  repr = "list",
  neutralized = "character",
  constraints = "constraint_map"),
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


constraint_map <- function(i, dir=c("inc", "dec", "cls")) {
  dir <- match.arg(dir)
  d <- switch(dir, "inc" = -1, "dec" = 1, "cls" = 0)

  res <- structure(
    list(
      ids = mapply(c, head(i, -1), i[-1], SIMPLIFY = F),
      dir = rep(d, length(i) - 1)),
    class = "constraint_map")

  if (length(res$ids) != length(res$dir)) stop("Error constructing constraing map")

  return(res)
}

`+.constraint_map` <- function(e1, e2) {

  ## check if nothing has been assigned yet
  if (identical(new("constraint_map"), e1)) {
    return(e2)
  }

  ## update the constraint map
  current <- e1

  i <- lapply(e2$ids, function(z) which(sapply(current$ids, function(y) all(z == y))))
  lens <- lengths(i)
  i[lens == 0] <- 0
  i <- unlist(i)

  ## if dir is 0 then remove them
  if (unique(e2$dir) == 0) {
    current <- current[-i]
  } else {
    current$dir[i] <- unique(e2$dir)
  }

  ## add the new additions
  current$ids <- c(current$ids, e2$ids[i == 0])
  current$dir <- c(current$dir, e2$dir[i == 0])
  current
}

### Turn a constraint map into a matrix?

make_constraint_matrix_ <- function(tf, ...) {
  x <- tf@constraints

  ncols <- length(c(tf@subst, tf@exceptions, tf@nas))

  if (identical(new("constraint_map"), x)) {
    return(matrix(0, ncols, ncols))
  }

  ## always return the number of columns as there are bins
  m <- matrix(0, nrow = length(x$ids), ncol = ncols)

  ## create index vector used to assign -1 & 1
  idx <- cbind(seq.int(nrow(m)), as.vector(do.call(rbind, x$ids)))
  m[idx] <- t(sapply(x$dir, `*`, c(1,-1)))

  return(m)
}


set_constraints_ <- function(tf, i, dir=c("inc", "dec", "cls")) {

  m <- constraint_map(i, dir)

  tf@constraints <- tf@constraints + m

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
update_transform <- function(tf, result, keep_constraints=FALSE) {

  tf@subst <- setNames(result$normal[,"Pred"], row.names(result$normal))
  tf@nas <- c(Missing=result$missing[,"Pred"])

  exception_names <- row.names(result$exception)
  tf@exceptions[exception_names] <- result$exception[,"Pred"]

  tf@repr <- result

  ## changes must(?) invalidate a constraint map because number of cols changes
  if (!keep_constraints) {
    tf@constraints <- new("constraint_map")
  }

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


toString.constraint_map <- function(x, ...) {
  if (identical(new("constraint_map"), x)) {
    return (list(i=0, str=""))
  } else {
    sym <- ifelse(x$dir == 1, ">", "<")
    str <- paste0(sym, sapply(x$ids, '[', 2))
    list(i=sapply(x$ids, '[', 1), str=str)
  }
}
