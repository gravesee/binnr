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
  if (any(is.na(y))) {
    stop("y response cannot have missing values")
  }

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
    if (all(is.na(df[,nm]))) {df[,nm] <- as.logical(df[,nm])}
    b <- bin(df[,nm], y, name = nm, mono=.mono[nm], exceptions=.exceptions[[nm]], ...)
    if (!is.null(b)) res[[nm]] <- b
  }
  cat("\n")

  return(bin.list(res))
  #return(res)
}

#' @export
predict.bin.list <- function(object, newdata, type="woe", coefs=NULL) {
  if (is.null(names(object))) stop("bin.list object must have names attribute")
  if (is.null(colnames(newdata))) stop("newdata requires column names")
  
  # check variable name overlap
  nms <- names(object)
  if (!is.null(nms)) nms <- nms[!(nms == "")]
  vars <- intersect(colnames(newdata), nms)

  # coefs are all ones if not passsed in
  if (is.null(coefs)) {
    coefs <- rep(1, length(vars))
    names(coefs) <- vars
  }
  
  if (length(vars) == 0) stop("no vars in common between newdata and bin.list")
  
  res <- list()
  for (i in seq_along(vars)) {
    nm <- vars[i]
    cat(sprintf("\rProgress: %%%3d", as.integer(100*i/length(vars))))
    flush.console()
    if (!object[[nm]]$skip) {
      res[[vars[i]]] <- predict(object[[nm]], newdata[,nm], type, coefs[nm])
    }
  }
  cat("\n")
  if (type == "bins") {
    res <- as.data.frame(res)
  } else {
    res <- do.call(cbind, res)
  }
  rownames(res) <- rownames(newdata)
  return(res)
}

#' @export
print.bin.list <- function(x) {
  lvls <- c("bin", "bin.factor", "bin.numeric")
  cnts <- table(factor(sapply(x, class), levels=lvls))
  names(cnts) <- c("# Bins", "# Discrete", "# Continuous")
  cat("binnr bin.list object\n")
  cat(sprintf("  |-- %3d Bins\n", cnts[1]))
  cat(sprintf("  |-- %3d Discrete\n", cnts[2]))
  cat(sprintf("  |-- %3d Continuous\n", cnts[3]))
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

#' @export
summary.bin.list <- function(object, ...) {
  # print table showing raw, coarse levels, NAs, # exception, mono, iv
  out <- list()
  for (i in seq_along(object)) {
    b <- object[[i]]
    df <- as.data.frame(b)
    out[[i]] <- data.frame(
      Name  = b$name,
      IV    = max(df$IV),
      NBins = nrow(b$core$counts$var),
      totN  = sum(sapply(b$core$counts, sum)),
      NVar  = sum(b$core$counts$var),
      NExc  = sum(b$core$counts$exc),
      NMiss = sum(b$core$counts$nas),
      Mono = b$opts$mono,
      stringsAsFactors = F
    )
    colnames(out[[i]]) <- c(
      "Name", "IV", "# Bins", "Tot N", "# Valid",
      "# Exception", "# Missing", "Monotonicty")
  }
  out <- as.data.frame(do.call(rbind, out))
  rownames(out) <- NULL
  out[order(-out$IV),]
}
