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