#' Bins to CSV
#' 
#' Export bins to csv file that can be opened in Excel
#' 
#' @param bins The \code{bin.object} to export
#' @param file The fully qualified path to write the bins
#' 
#' @export
bins.to.csv <- function(bins, file=NULL) {
  if (file.exists(file)) file.remove(file)
  for (b in bins) {
    df <- as.data.frame(b)
    df <- cbind(rownames(df), df)
    colnames(df)[1] <- b$name
    ff <- file(file, open="at") # open file in append mode
    write.table(df, file = ff, row.names = F, sep = ',')
    cat("\n", file = ff, append = TRUE)
    close(ff)
  }
}

#' In Model
#' 
#' Function that returns T/F vector for whether a bin is in a model
#' 
#' @param bins The \code{bin.object} to check for model membership
#' @return logical vector the same length as bins. TRUE if the bins has been 
#' visited with the \code{adjust} function, FALSE otherwise.
#' @export
inmodel <- function(bins) {
  stopifnot(is.bin.list(bins))
  sapply(bins, function(x) x$meta$inmodel)
}

#' @export
notinmodel <- function(bins) {
  for (v in seq_along(bins)) {
    bins[[v]]$meta$inmodel <- FALSE
  }
  bins
}

#' @export
`inmodel<-` <- function(bins, value) {
  stopifnot(is.bin.list(bins))
  for (v in value) {
    bins[[v]]$meta$inmodel <- TRUE
  }
  bins
}

#' @export
dropped <- function(x) {
  UseMethod("dropped")
}

#' @export
`drop<-` <- function(x, value) {
  UseMethod("drop<-")
}

#' @export
`keep<-` <- function(x, value) {
  UseMethod("keep<-")
}

#' @export
dropped.bin.list <- function(bins) {
  stopifnot(is.bin.list(bins))
  sapply(bins, function(x) x$meta$skip)
}

#' @export
dropped.binnr.model <- function(mod) {
  #stopifnot(is.bin.list(bins))
  sapply(mod$bins, function(x) x$meta$skip)
}

#' @export
`drop<-.bin.list` <- function(bins, value) {
  for (v in value) {
    bins[[v]]$meta$skip <- TRUE
  }
  bins
}

#' @export
`drop<-.binnr.model` <- function(mod, value) {
  drop(mod$bins) <- value
  mod
}

#' @export
`keep<-.bin.list` <- function(bins, value) {
  for (v in value) {
    bins[[v]]$meta$skip <- FALSE
  }
  bins
}

#' @export
`keep<-.binnr.model` <- function(mod, value) {
  keep(mod$bins) <- value
  mod
}

calc.score <- function(newdata, coefs) {
  newdata %*% coefs[-1] + coefs[1]
}

calc.lr2 <- function(f, y) {
  f <- plogis(f)
  sum((y == 1)*log(f) + (y == 0)*log(1 - f))
}

calc.contributions <- function(newdata, coefs, y) {
  base <- calc.lr2(0, y)
  lr2 <- sapply(1:length(coefs), function(i) {
    if (i > 1) {
      coefs[i] <- 0
    }
    1 - (calc.lr2(calc.score(newdata, coefs), y)/base)
  })
  names(lr2) <- c("Base", names(coefs)[-1])
  lr2[1] - lr2[-1]
}

woe <- function(cnts, y) {
  if (length(cnts) == 0) return(matrix(nrow=0, ncol=2))
  ytot <- table(factor(y, levels=c(0,1)))
  pct0 <- cnts[,1]/ytot[1]
  pct1 <- cnts[,2]/ytot[2]
  woe <- log(pct1/pct0)
  woe[is.infinite(woe) | is.na(woe)] <- 0
  woe
}

cnts <- function(x, y, nms=NULL) {
  tbl <- table(x, factor(y, levels=c(0,1)), useNA='ifany')
  if (sum(tbl) == 0) {
    if (!is.null(nms)) {
      return(matrix(0, nrow=1, ncol=2, dimnames = list(nms)))
    } else {
      return(matrix(nrow=0, ncol=2))
    }
  }
  out <- matrix(tbl, ncol=2)
  rownames(out) <- rownames(tbl)
  out
}

unlist.matrix <- function(b, i, e2) {
  stopifnot(is.bin(b))
  skeleton <- lapply(b$core$counts, function(x) x[,i])
  flesh <- unlist(skeleton)
  flesh[e2] <- 0
  relist(flesh, skeleton)
}