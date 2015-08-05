predict.bin <- function(object, x) {
  # two types of maps -- numeric and character/factor 
  if (class(x) %in% c('numeric', 'integer')) {
    res <- object$values[cut(x, object$breaks, labels = FALSE)]
  } else {
    # check if breaks are actual characters
    res <- object$values[match(as.character(x), object$breaks)]
  }
  
  if (length(object$sv) != 0) { # special values
    for (i in seq_along(object$sb)) {
      res[x == object$sb[i]] <- object$sv[i]
    }
  }
  
  if (!is.null(object$na)) res[is.na(res)] <- object$na # nas
  
  names(res) <- NULL
  return(res)
}

predict.bin.list <- function(object, newdata) {
  if (is.null(names(object))) stop("bin.list object must have names attribute")
  if(is.null(colnames(newdata))) stop("newdata requires column names")
  
  nms <- names(object)
  if (!is.null(nms)) nms <- nms[!(nms == "")]
  vars <- intersect(colnames(newdata), nms)
  
  if (length(vars) == 0) stop("no vars in common between newdata and bin.list")
  
  res <- list()
  for (i in seq_along(vars)) {
    nm <- vars[i]
    cat(sprintf("\rProgress: %%%3d", as.integer(100*i/length(vars))))
    flush.console()
    res[[i]] <- predict(object[[nm]], newdata[,nm])
  }
  cat("\n")
  res <- do.call(cbind, res)
  colnames(res) <- vars
  return(res)
}

bin <- function(x, y, min.iv=.025, min.cnt = NULL, max.bin=10, mono=0, sv=NULL){
  stopifnot(length(x) == length(y))
  stopifnot(mono %in% c(-1,0,1))
  stopifnot(max.bin > 0)
  if (is.null(min.cnt)) min.cnt <- sqrt(length(x))
  stopifnot(min.cnt > 0)
  
  # different approach for factors and numerics
  if (is.numeric(x)) {
    out <- .Call('bin', as.double(x), as.double(y), as.double(min.iv),
                 as.integer(min.cnt), as.integer(max.bin), as.integer(mono),
                 as.double(sv))
  } else {
    tbl <- table(x, y)
    pt <- prop.table(tbl, margin = 2)
    woe <- log(pt[,2]/pt[,1])
    woe[(is.infinite(woe))] <- 0
    
    iv <- sum((pt[,2] - pt[,1]) * woe)
    print(iv)
    
    out <- structure(list(breaks=names(woe), values = woe, na = 0, sb=numeric(0),
                     sv=numeric(0)), class="bin")
  }
  out
}

bin.data <- function(df, y, ...) {
  vars <- colnames(df)
  
  res <- list()
  for (i in seq_along(vars)) {
    nm <- vars[i]    
    cat(sprintf("\rProgress: %%%3d", as.integer(100*i/length(vars))))
    flush.console()
    res[[nm]] <- bin(df[,nm], y, ...)
  }
  cat("\n")
  return(structure(res, class='bin.list'))
}
