
#' @export
bin.data.frame <- function(x, y, bc=bin.control(), seg=NULL, mono=c(ALL=0), exceptions=list(ALL=numeric(0))) {
  # if segment var is passed, recursively call bin on each split data.frame
  if(!is.null(seg)) {
    ys <- split(y, seg, drop=T)
    xs <- split(x, seg, drop=T)
    out <- mapply(bin, xs, ys, MoreArgs = list(bc=bc, mono=mono, exceptions=exceptions), SIMPLIFY = F)
    return(structure(out, class=c("segmented")))
  }
  
  # stop checks
  stopifnot(is.list(exceptions))
  if (any(is.na(y))) stop("y response cannot have missing values")
  
  v <- colnames(x)
  bcdf <- get.bin.control.df(v, mono, exceptions)
  
  dashes <- c('\\','|','/','-')
  
  res <- list()
  for (i in seq_along(v)) {
    cat(sprintf("\rProgress: %s %6.2f%%", dashes[(i %% 4) + 1], (100*i/length(v))))
    flush.console()
    bc$mono <- bcdf$mono[v[i]]
    bc$exceptions <- bcdf$exceptionns[v[i]]
    res[[v[i]]] <- bin(x[,v[i]], y, name = v[i], bc)
  }
  cat("\n")
  
  return(bin.list(res))
}

get.bin.control.df <- function(v, mono, exceptions) {
  out.mono <- rep(mono["ALL"], length(v))
  names(out.mono) <- v
  out.mono[names(mono)] <- mono
  
  out.except <- rep(list(exceptions[['ALL']]), length.out=length(v))
  names(out.except) <- v
  out.except[names(exceptions)] <- exceptions
  
  list(
    mono=out.mono,
    exceptions=out.except)
}


#' @export
as.data.frame.bin <- function(b, row.names = NULL, optional = FALSE, ...) {
  # create filters excluding NAs
  cnts <- do.call(rbind, b$core$counts)
  cnts <- cbind(cnts, rowSums(cnts))
  f <- !is.na(rownames(cnts)) # NAs aren't used for certain calculations
  tots <- apply(matrix(cnts[f,], ncol=3), 2, sum)
  
  # create pieces for output data.frame
  pcts <- sweep(cnts, 2, tots, FUN = '/')
  #cnts <- cbind(cnts, apply(cnts, 1, sum))
  woe <- na.omit(unlist(b$core$values))
  ivs <- woe * (pcts[,2] - pcts[,1])
  prob <- cnts[,2] / cnts[,3]
  pcts[!f,] <- 0 # keep counts, but set NA pcts to zero
  out <- cbind(cnts, pcts, prob, woe, ivs)
  
  # row and column labels
  colnames(out) <- c("#0","#1","N","%N","%0","%1","P(1)","WoE","IV")
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
  if (!is.null(b$rcs)) out$RC <- c(unlist(b$rcs), '')
  out
}

#' @export
print.bin <- function(x, ...) {
  out <- as.data.frame(x)
  iv <- out['Total', 'IV']
  fmts <- c(rep("%d",3), rep("%1.3f", 5), "%0.5f", "%s", "%1.3f", "%s")
  
  for (i in seq_along(out)) { out[,i] <- sprintf(fmts[i], out[,i]) }
  
  status <- ifelse(x$meta$skip, " *** DROPPED ***", "")
  cat(sprintf("\nIV: %0.5f | Variable: %s%s\n", iv, x$name, status))
  print(out)
  cat(sprintf("\nModified: %s | In Model: %d | New: %s | Bin: %s",
              x$meta$modified, x$meta$inmodel, x$meta$new, x$meta$type))
  if (!is.null(x$notes)) {
    cat("\nNotes:\n", x$notes)
  }
}