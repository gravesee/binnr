#' @export
`rcs<-.bin` <- function(x, idx=NULL, value) {
  skeleton <- x$core$values
  flesh <- unlist(skeleton)

  if (is.null(idx)) idx <- seq_along(flesh)

  rcs <- if(is.null(x$rcs)) rep("", (length(flesh))) else unlist(x$rcs)
  rcs[idx] <- value
  x$rcs <- relist(rcs, skeleton)
  x
}

#' @export
rcs <- function(x) {
  UseMethod("rcs", x)
}

#' @export
rcs.bin <- function(x) {
  unlist(x$rcs)
}

#' @export
`rcs<-` <- function(x, value) {
  UseMethod("rcs<-", x)
}

#' @export
`rcs<-.bin.list` <- function(x, value) {
  stopifnot(is.list(value))

  nms <- names(value)
  if (!is.null(nms)) nms <- nms[!(nms == "")]
  vars <- intersect(names(x), nms)

  if (length(vars) == 0) stop("no vars in common between bins and rc list",
                              call. = F)

  for (v in vars) {
    rcs(x[[v]]) <- value[v]
  }
  x
}