

`[<-.bin` <- function(x, idx=NULL, value) {
  skeleton <- list(v=x$values, n=x$na, e=x$except_woe)
  flesh <- unlist(skeleton)
  
  if (is.null(idx)) idx <- seq_along(flesh)
  
  if (is.null(x$rcs)) {
    rcs <- character(length(flesh))
  } else {
    rcs <- unlist(x$rcs)
  }
  
  rcs[idx] <- value
  x$rcs <- relist(rcs, skeleton)
  x
}
