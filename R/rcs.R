# add reason codes to a bin object

data(titanic)
bins <- bin(titanic[,-1], titanic[,1])

`[<-.bin` <- function(b, i, values) {
  skeleton <- b$core$values
  flesh <- unlist(skeleton)
  rcs <- if (is.null(b$rcs$aa)) character(length(flesh)) else unlist(b$rcs$aa)
  if (i == 0) i <- seq_along(rcs)
  rcs[i] <- values
  b$rcs$aa  <- relist(rcs, skeleton)
  b$rcs$pts <- relist(flesh - min(flesh), skeleton)
  b
}