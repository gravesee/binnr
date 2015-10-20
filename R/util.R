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

#' Visited
#' 
#' Function that returns T/F vector for whether a bin has been visited with
#' \code{adjust}
#' 
#' @param bins The \code{bin.object} to check for visitation
#' @return logical vector the same length as bins. TRUE if the bins has been 
#' visited with the \code{adjust} function, FALSE otherwise.
#' @export
visited <- function(bins) {
  stopifnot(is.bin.list(bins))
  sapply(bins, function(x) x$meta$visited)
}