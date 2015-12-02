table.merge <- function(a, b) {
  m <- merge(a, b, by='Var1', all=T)
  m[is.na(m)] <- 0
  o <- m$Freq.x + m$Freq.y
  names(o) <- m$Var1
  as.table(o)
}

# function that assigns and sorts RCs for every record
get.all.rcs <- function(mod, data) {
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
    rcs <- as.data.frame.matrix(out, stringsAsFactors = F)
    rcs
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

# return a table of the rc rates for each position
get.colpctN <- function(tbls, nret) {
  # first N return percentages
  pcts <- lapply(tbls[1:nret], prop.table)
  colpctN <- Reduce(function(a, b) {
    out <- merge(a,b, by='Var1', all=T)
    out[is.na(out)] <- 0
    rownames(out) <- out$Var1
    out
  }, pcts)
  colpctN$Var1 <- NULL
  colnames(colpctN) <- paste0("RC", 1:nret)
  colpctN[order(-colpctN[,1]),]
}

rc.report <- function(rcs, nret) {
  tbls <- lapply(rcs, table)
  nret <- min(nret, length(tbls))
  
  # RC rates for each of the returned RCs position
  colpctN <- get.colpctN(tbls, nret)
  
  # return rates for reason codes
  ret <- Reduce(table.merge, tbls[1:nret])
  
  # trigger rates for reason codes
  hit <- Reduce(table.merge, tbls)
  out <- merge(ret, hit, by='Var1', all=T)
  rownames(out) <- out$Var1
  out$Var1 <- NULL
  colnames(out) <- c("N Returned", "N Triggered")

  out$`% Returned` <- out[[1]]/sum(nrow(rcs))
  out$`% Triggered` <- out[[2]]/sum(nrow(rcs))
  out[is.na(out)] <- 0
  
  # merge the col pcts
  out <- merge(colpctN, out, by=0, all=T)
  colnames(out)[1] <- "RC"
  out <- subset(out, subset = RC != "")
  out[order(-out$RC1),]
}

#' @export
rc.table.binnr.model <- function(mod, data, nret = 4) {
  out <- get.all.rcs(mod, data)
  rc.report(out, nret)
}

#' @export
rc.table.segmented <- function (mod, data, nret = 4, seg = NULL) {
  stopifnot(!is.null(seg))
  stopifnot(is.segmented(mod))
  
  rcs <- mapply(get.all.rcs, mod, split(data, seg, drop = T), SIMPLIFY = F)
  
  Ncols <- max(sapply(rcs, ncol))
  padded <- lapply(rcs, function(x) {
    if (ncol(x) < Ncols) {
      x[,(ncol(x) + 1):Ncols] <- ""
    }
    x
  })
  rc.report(do.call(rbind, padded), nret)
}



