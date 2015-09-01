bin.list <- function(bins){
  stopifnot(all(sapply(bins, is.bin)))
  structure(bins, class='bin.list')
}

#' @export
`[.bin.list` <- function(x, i) {
  binnr:::bin.list(unclass(x)[i])
}

#' @export
is.bin.list <- function(x) {
  inherits(x, "bin.list")
}

#' @export
bin.data.frame <- function(df, y, mono=c(ALL=0), exceptions=list(ALL=NULL), ...) {
  stopifnot(is.list(exceptions))

  vars <- colnames(df)
  .mono <- rep(mono["ALL"], ncol(df))
  names(.mono) <- vars
  .mono[names(mono)] <- mono
  .exceptions <- rep(list(exceptions[['ALL']]), length.out=ncol(df))
  names(.exceptions) <- vars
  .exceptions[names(exceptions)] <- exceptions

  dashes <- c('\\','|','/','-')

  drop.vars <- list()
  res <- list()
  for (i in seq_along(vars)) {
    nm <- vars[i]
    cat(sprintf("\rProgress: %s %6.2f%%", dashes[(i %% 4) + 1], (100*i/length(vars))))
    flush.console()
    b <- bin(df[,nm], y, name = nm, mono=.mono[nm], exceptions=.exceptions[[nm]], ...)
    if (!is.null(b)) res[[nm]] <- b
  }
  cat("\n")

  return(bin.list(res))
}

#' @export
predict.bin.list <- function(object, newdata) {
  if (is.null(names(object))) stop("bin.list object must have names attribute")
  if (is.null(colnames(newdata))) stop("newdata requires column names")

  nms <- names(object)
  if (!is.null(nms)) nms <- nms[!(nms == "")]
  vars <- intersect(colnames(newdata), nms)

  if (length(vars) == 0) stop("no vars in common between newdata and bin.list")

  res <- list()
  for (i in seq_along(vars)) {
    nm <- vars[i]
    cat(sprintf("\rProgress: %%%3d", as.integer(100*i/length(vars))))
    flush.console()
    if (!object[[nm]]$skip) {
      res[[vars[i]]] <- predict(object[[nm]], newdata[,nm])
    }
  }
  cat("\n")
  res <- do.call(cbind, res)
  rownames(res) <- rownames(newdata)
  return(res)
}

#' @export
print.bin.list <- function(x, n=NULL) {
  if (is.null(n)) {
    n <- 1:length(x)
  } else {
    n <- 1:min(length(x), n)
  }

  # TODOD: make iv a part of the bin object?
  ivs <- sapply(x, function(x) as.data.frame(x)['Total', 'IV'])

  for (b in x[order(-ivs)]) {
    print(b)
  }
}

#' @export
`!=.bin.list` <- function(e1, e2) {
  stopifnot(is.bin.list(e1))
  out <- e1
  for (i in seq_along(e1)) {
    b <- as.data.frame(e1[[i]])
    nms <- rownames(as.data.frame(b))
    nms <- nms[-which(nms %in% c("Missing", "Total"))]
    nms <- sapply(strsplit(nms, '^\\s*\\d+\\.'), '[[', 2)
    nms <- gsub('\\s*', '', nms)

    d <- nms %in% e2
    if (any(d)) {
      out[[i]] <- e1[[i]] != which(d)
    }
  }
  eval(parse(text = paste(substitute(e1), "<<- out")))
}