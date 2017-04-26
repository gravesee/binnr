#' @include performance_class.R
NULL

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
#' @export Classing
#' @exportClass Classing
Classing <- setRefClass("Classing",
  fields = c(
    variables = "list",
    vnames = "character",
    performance = "Performance",
    dropped = "character"))


#' generic method for create_bin
#' @name create_bin
#' @param x the varibale used for the Bins \code{x} value
#' @param ... additional arguments passed on to the Bin constructors
setGeneric("create_bin", function(x, ...) callGeneric("create_bin"))


#' @describeIn create_bin wrap variable in Continuous object
#' @return a Continuous object
setMethod("create_bin", "numeric", function(x, ...) {
  Continuous$new(x = x, ...)
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
  Discrete$new(x = x, ...)
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

Classing$methods(get_variables = function(keep=FALSE, bag.fraction=1) {
  n <- length(variables[[1]]$x)
  if (identical(bag.fraction, 1)) {
    s <- TRUE
  } else {
    s <- sample(seq.int(n), min(n, n * bag.fraction), replace=FALSE)
  }

  if (!keep) {
    lapply(variables[setdiff(vnames, dropped)], function(x) x$x[s])
  } else {
    lapply(variables, function(x) x$x[s])
  }
})


Classing$methods(get_transforms = function(keep=FALSE) {
  if (!keep) {
    lapply(variables[setdiff(vnames, dropped)], function(x) x$tf)
  } else {
    lapply(variables, function(x) x$tf)
  }
})


Classing$methods(predict = function(newdata=NULL, keep=keep) {
  on.exit(cat(sep = "\n"))

  if (is.null(newdata)) newdata <- get_variables(keep=keep)
  vnm <- if (keep) vnames else setdiff(vnames, dropped)

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

  woe <- mapply(func, seq_along(vnm), variables[vnm], newdata[vnm])

  colnames(woe) <- vnm
  woe

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


#' Flag supplied variables as undropped
#'
#' @name Classing_undrop
#' @param vars character vector of variables to undrop
#' @param all logical indicating whether all variables should be undropped
NULL
Classing$methods(undrop = function(vars=character(0), all=FALSE, ...) {
  if (all) {
    dropped <<- character(0)
  } else {
    stopifnot(all(vars %in% vnames))
    dropped <<- setdiff(dropped, vars)
  }
})



#' Cluster variables by correlation
#'
#' @name Classing_cluster
#' @param keep logical indicating whether to include dropped variables in the
#' correlation cluster analysis.
#' @param bag.fraction sample the development data without replacement to
#' cluster more quickly. The bag.fraction is multiplied by the number of
#' observations.
#' @details the cluster function first performs weight-of-evidence substitution.
#' Cluster returns a classing_cluster object which is a list containing
#' two fields: correlation & cluster. The first is a correlation matrix for all
#' of the variables in the classing. The second is an hclust object which is
#' result of hierarchical clustering of the correlation matrix.
#' @return a classing_cluster object
NULL
Classing$methods(cluster = function(keep=FALSE, bag.fraction=1, ...) {

  woe <- predict(newdata=get_variables(keep = keep, bag.fraction=bag.fraction),
    type="woe", ...)

  dups <- apply(woe, 2, function(x) all(duplicated(x)[-1L]))

  corr <- cor(woe[,which(!dups)])

  structure(
    list(
      correlations = corr,
      cluster = hclust(as.dist(1 - abs(corr)))),
    class="classing_cluster")

})


#' Return a data.frame summarizing the variable clusters
#'
#' @name Classing_get_clusters
#' @param cc classing_cluster object produced by \code{\link{Classing_cluster}}
#' method
#' @param corr minimum correlation coefficient threshold with which to group
#' variables
#' @return a data.frame of variable clusters and information value
NULL
Classing$methods(get_clusters =  function(cc, corr=0.80) {
  stopifnot(is(cc, "classing_cluster"))

  ## get information values
  p <- sapply(variables[colnames(cc$correlations)], function(x) x$sort_value())

  ## cutree
  grps <- cutree(cc$cluster, h=1-corr)

  # split correlations into groups and return everyone after the first
  out <- split(data.frame(variable=names(grps), sort_value=p,
    stringsAsFactors = F), grps)

  out <- lapply(out, function(x) x[order(-x$sort_value),])
  out <- Map(`[<-.data.frame`, out, "Cluster", value=seq_along(out))

  out <- do.call(rbind, out)
  row.names(out) <- NULL
  out
})


#' Prune clusters keeping only the most informative variables
#'
#' @name Classing_prune_clusters
#' @param cc classing_cluster object produced by \code{\link{Classing_cluster}}
#' method
#' @param corr minimum correlation coefficient threshold with which to group
#' variables
#' @param n number of variables to keep from each cluster exceeding the
#' correlation threshold. The n variables with the highest information value
#' are retained. The remaining variables are returned as a character vector.
#' @return a character vector of variables to drop
NULL
Classing$methods(prune_clusters =  function(cc, corr=0.80, n=1) {
  stopifnot(is(cc, "classing_cluster"))

  clusters <- get_clusters(cc=cc, corr=corr)
  splt <- split(clusters, clusters$Cluster)

  ## order each group by descending perf value and drop all but the first n
  to_drop <- lapply(splt, function(x) {
    x$variable[order(-x$sort_value)][-seq.int(n)]
  })

  unlist(to_drop)
})


#' Summarize the Classing object
#'
#' @name Classing_summary
#' @return a matrix summarizing the independent variables using the Performance
#' object summary function
NULL
Classing$methods(summary = function(keep=FALSE) {

  if (keep) {
    k <- TRUE
  } else {
    k <- !names(variables) %in% dropped
  }

  s <- lapply(variables[k], function(v) v$summary())

  res <- cbind(do.call(rbind, s), Dropped=0)
  res[match(rownames(res), dropped, 0), "Dropped"] <- 1

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
