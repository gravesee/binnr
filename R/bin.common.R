#' Bin variables for credit modeling
#' 
#' @description bin returns a \code{bin} object containing information necessary
#' for modeler interaction. With a \code{bin} object, a modeler can collapse,
#' expand, neutralize, cap, and predict variable transformations
#' 
#' @param x a \code{factor} or \code{numeric} variable. Can also be a
#' \code{data.frame} in which case a \code{bin.list} object is returned.
#' @param y a binary response variable
#' @param name the name used for plotting and printing. Automatically supplied
#' if called on a \code{data.frame}
#' @param min.iv minimum information value required to split a continuous
#' predictor
#' @param min.cnt minimum number of observations to split a continuous
#' predictor. Defaults to the square root of the number of observations.
#' @param min.res minimum number of responses required to split
#' @param max.bin maximum number of levels in resulting discretized, continuous
#' predictor.
#' @param mono monotonicity constraint for binning algorith. \code{binnr} uses 
#' the sane assumption that the 1-labelled target is of most interest. Therefore
#' monotonicity of 1 implies the target rate increases with the predictor. The
#' opposite for -1. A value of 0 enforces no monotonicity constraint and a 2
#' enforces *any* monotonic constraint but doesn't assume a particular
#' direction. If \code{bin} is used on a \code{data.frame}, mono can be a named
#' vector. The reserved name ALL is used for global values.
#' @param exceptions a \code{list} of values to be excluded from the binning
#' algorithm. These levels count towards weight of evidence calculations but are
#' not collapsed. Exceptions values for \code{factors} are ignored. A reserved
#' name, ALL, can be used to supply global exception values.
#' 
#' @return a \code{bin} or \code{bin.list} object
#' 
#' @export
bin <- function(x, y, seg=NULL, name, min.iv, min.cnt, min.res, max.bin, mono, exceptions){
  UseMethod("bin", x)
}

bin.factory <- function(x, ...) {
  UseMethod("bin.factory")
}

#' @export
is.bin <- function(x) {
  inherits(x, "bin")
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

#' @export
as.data.frame.bin <- function(x, row.names = NULL, optional = FALSE, ...) {
  # create filters excluding NAs
  cnts <- do.call(rbind, x$core$counts)
  f <- !is.na(rownames(cnts)) # NAs aren't used for certain calculations
  tots <- apply(matrix(cnts[f,], ncol=2), 2, sum)
  
  # create pieces for output data.frame
  pcts <- t(apply(cnts, 1, '/',  tots))
  cnts <- cbind(cnts, apply(cnts, 1, sum))
  woe <- na.omit(unlist(x$core$values))
  ivs <- woe * (pcts[,2] - pcts[,1])
  prob <- cnts[,2] / cnts[,3]
  pcts[!f,] <- 0 # keep counts, but set NA pcts to zero
  out <- cbind(cnts, pcts, prob, woe, ivs)
  
  # row and column labels
  colnames(out) <- c("#0","#1","N","%0","%1","P(1)","WoE","IV")
  rownames(out)[f] <- sprintf("%2d. %s", seq(1, nrow(out[f,,drop=F])),
                              rownames(out[f,,drop=F]))
  rownames(out)[is.na(rownames(out))] <- "Missing"
  
  # total row
  total <- apply(out, 2, sum)
  total["P(1)"] <- total["#1"] / total["N"]
  total["WoE"] <- 0
  
  # output
  out <- as.data.frame(rbind(out, Total=total))
  
  # check for RCS
  if (!is.null(x$rcs)) out$RC <- c(unlist(x$rcs), '')
  out
}

#' @export
print.bin <- function(x, ...) {
  out <- as.data.frame(x)
  iv <- out['Total', 'IV']
  fmts <- c(rep("%d",3), rep("%1.3f", 4), "%0.5f", "%s", "%1.3f", "%s")
  
  for (i in seq_along(out)) { out[,i] <- sprintf(fmts[i], out[,i]) }
  
  status <- ifelse(x$meta$skip, " *** DROPPED ***", "")
  cat(sprintf("\nIV: %0.5f | Variable: %s%s\n", iv, x$name, status))
  print(out)
  cat(sprintf("\nModified: %s | In Model: %d | Bin: %s",
      x$meta$modified, x$meta$inmodel, x$meta$type))
  if (!is.null(x$notes)) {
    cat("\nNotes:\n", x$notes)
  }
}

#' @export
`!=.bin` <- function(e1, e2) {
  # TODO: add bounds checking for NAs
  y <- e1$data$y
  zero <- unlist.matrix(e1, 1, e2)
  ones <- unlist.matrix(e1, 2, e2)
  counts <- mapply(cbind, zero, ones, SIMPLIFY = F)
  
  tots <- matrix(apply(rbind(counts$var, counts$exc), 2, sum), ncol=2)
  
  values <- e1$core$values
  values$var <- log((counts$var[,2]/tots[,2])/(counts$var[,1]/tots[,1]))
  values$exc <- log((counts$exc[,2]/tots[,2])/(counts$exc[,1]/tots[,1]))
  values <- lapply(values, function(x) {x[is.nan(x) | is.infinite((x))] <- 0; x})
  b <- e1
  b$core$counts <- counts
  b$core$values <- values
  b$history <- e1
  
  b$meta$type <- "MANUAL"
  b$meta$modified <- date()
  
  b
}

unlist.matrix <- function(b, i, e2) {
  skeleton <- lapply(b$core$counts, function(x) x[,i])
  flesh <- unlist(skeleton)
  flesh[e2] <- 0
  relist(flesh, skeleton)
}

#' @export
reset <- function(b) {
  do.call(bin, c(list(x=b$data$x, y=b$data$y, name=b$name), b$opts))
}

#' @export
mono <- function(b, v) {
  v <- if(v %in% c(-1,0,1,2)) v else 0
  opts <- b$opts
  opts$mono <- v
  out <- do.call(bin, c(list(x=b$data$x, y=b$data$y, name=b$name), opts))
  out$opts <- b$opts
  out$meta <- b$meta
  out$history <- b
  out$meta$type <- "MANUAL"
  out$meta$modified <- date()
  out
}

#' @export
undo <- function(x) {
  if (is.null(x$history)) return(x)
  return(x$history)
}

#' @export
bin.logical <- function(x, y=NULL, name=NULL, min.iv=.01, min.cnt = NULL,
                        min.res=0, max.bin=10, mono=0, exceptions=numeric(0)) {
  warning(sprintf("Not binned: %s -- All missing", name), call. = F)
  NULL
}

#' @export
bin.character <- function(x, y=NULL, name=NULL, min.iv=.01, min.cnt = NULL,
                          min.res=0, max.bin=10, mono=0, exceptions=numeric(0)) {
  warning(sprintf("Not binned: %s -- Character, hint: cast to factor", name),
          call. = F)
  NULL
}
 
