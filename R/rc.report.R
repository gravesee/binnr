table.merge <- function(a, b) {
  m <- merge(a, b, by='Var1', all=T)
  m[is.na(m)] <- 0
  o <- m$Freq.x + m$Freq.y
  names(o) <- m$Var1
  as.table(o)
}

rc.report <- function(mod, data) {
    d <- predict(mod, data, type="dist")
    r  <- predict(mod, data, type="rcs")
    
    # aggregate them
    out <- t(sapply(1:nrow(d), function(i) {
      agg <- tapply(d[i,], r[i,], sum, na.rm=T)
      ord <- order(agg)
      out <- names(agg)[ord]
      out[agg[ord] == 0] <- ""
      out
    }))  
    # return
    as.data.frame.matrix(out, stringsAsFactors = F)
}

#' Reason Code Table
#' 
#' Create a reason code distribution table from binnr objects
#' 
#' @param x a \code{segmented} or \code{binnr.model} object
#' @param data a \code{data.frame} to predict rcs
#' @param nret number of reason code positions returned
#' @param seg segment variable if called on \code{segmented} object
#' 
#' @return a data.frame or list of data.frames with detailed
#' reason code reporting statistics
#' 
#' @details If called on a data.frame, rc.table returns a single
#' data.frame with detailed statistcs about the number and percentage
#' of returned and triggered reason codes. If called on a \code{segmented}
#' object, rc.table returns a list of data.frames - one for each level 
#' found of the segment variable found in the data.
#' 
#' @export
rc.table <- function(x, data, nret, seg) {
  UseMethod("rc.table")
}

#' @export
rc.table.binnr.model <- function(mod, data, nret = 4) {
  rcs <- rc.report(mod, data)
  tbls <- lapply(rcs, table)
  
  nret <- min(nret, length(tbls))
  
  ret <- Reduce(table.merge, tbls[1:nret])
  hit <- Reduce(table.merge, tbls)
  out <- merge(ret, hit, by='Var1', all=T)
  colnames(out) <- c("RC", "N Returned", "N Triggered")
  out$`% Returned` <- out[[2]]/sum(nrow(rcs))
  out$`% Triggered` <- out[[3]]/sum(nrow(rcs))
  out[is.na(out)] <- 0
  out <- subset(out, subset = RC != "")
  out[order(-out[,2]),]
}

#' @export
rc.table.segmented <- function(mod, data, nret = 4, seg=NULL) {
  stopifnot(!is.null(seg))
  stopifnot(is.segmented(mod)) # if seg passed make sure segmented mod
  seg <- factor(seg, levels = names(mod))
  if (any(is.na(seg))) {
    stop("Segment variable levels are not found in names(mod)", call. = F)
  }
  mapply(rc.table, mod, split(data, seg, drop=T), SIMPLIFY = F)
}



