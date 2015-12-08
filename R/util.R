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

#' @export
get.meta.attr <- function(bins, attr) {
  stopifnot(is.bin.list(bins))
  sapply(bins, function(x) x$meta[[attr]])
}

#' @export
set.meta.attr <- function(bins, attr, vars, value) {
  if (!is.character(vars)) vars <- names(bins)[vars]
  UseMethod("set.meta.attr")
}

#' @export
set.meta.attr.bin.list <- function(bins, attr, vars, value) {
  stopifnot(is.bin.list(bins))
  for (v in vars) {
    bins[[v]]$meta[[attr]] <- value
  }
  bins
}

#' @export
set.meta.attr.binnr.model <- function(mod, attr, vars, value) {
  stopifnot(is.binnr.model(mod))
  for (v in vars) {
    mod$bins[[v]]$meta[[attr]] <- value
  }
  mod
}

#' @export 
set.meta.attr.segmented <- function(segs, attr, vars, value) {
  stopifnot(is.segmented(segs))
  for (i in seq_along(segs)) {
    segs[[i]] <- set.meta.attr(segs[[i]], attr, vars, value)
  }
  segs
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