#' @export
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
predict.bin.list <- function(bins, data=NULL, type="woe", coefs=NULL) {
  if (!is.null(data)) {
    vars <- intersect(colnames(data), names(bins))
    if (length(vars) == 0) stop("no vars in common between newdata and bin.list")
  } else {
    vars <- names(bins)
  }
  
  res <- list()
  for (i in seq_along(vars)) {
    nm <- vars[i]
    cat(sprintf("\rProgress: %%%3d", as.integer(100*i/length(vars))))
    flush.console()
    if (!bins[[nm]]$meta$skip) {
      res[[vars[i]]] <- predict(bins[[nm]], data[,nm], type, coefs[nm])
    }
  }
  cat("\n")
  if (type == "bins") {
    res <- as.data.frame(res)
  } else {
    res <- do.call(cbind, res)
  }
  rownames(res) <- rownames(data)
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
    f <- !(is.na(b$data$x) | (b$data$x %in% b$opts$exceptions))
    n.uniq <- length(unique(b$data$x[f]))
    
    df <- as.data.frame(b)
    out[[i]] <- data.frame(
      "Name"  = b$name,
      "IV"    = max(df$IV),
      "# Bins"  = nrow(b$core$counts$var),
      "# Unique"= n.uniq,
      "Tot N"   = sum(sapply(b$core$counts, sum)),
      "# Valid" = sum(b$core$counts$var),
      "# Exception"  = sum(b$core$counts$exc),
      "# Missing"    = sum(b$core$counts$nas),
      "Monotonicity" = b$opts$mono,
      "In Model"  = b$meta$inmodel,
      "Modified" = b$meta$modified,
      stringsAsFactors = F,
      check.names = F
    )
  }
  out <- as.data.frame(do.call(rbind, out))
  rownames(out) <- NULL
  out[order(-out$IV),]
}