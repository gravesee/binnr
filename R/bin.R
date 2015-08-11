library(ggplot2)
library(scales)
library(gridExtra)

woe <- function(x, y, y0, y1) {
  #if (is.null(tot)) tot <- table(y)
  if (length(x) == 0)  return(NULL)
  pt  <- table(x, y) # need to make sure dims are the same
  if (dim(pt)[2] == 2) {
    pct0 <- pt[,1]/y0
    pct1 <- pt[,2]/y1
    woe <- log(pct1/pct0)
    woe[(is.infinite(woe))] <- 0
  } else {
    woe <- NULL
  }
  woe
}

is.bin <- function(x) {
  inherits(x, "bin")
}

### TODO: pass in own breaks as well if necessary also caps... 
bin <- function(x, y=NULL, min.iv=.01, min.cnt = NULL, max.bin=10, mono=0, exceptions=NULL){
  if (is.bin(x)) {
    b <- bin(x$x, x$y, min.iv, min.cnt, max.bin, mono, exceptions)
    b$history[[length(history) + 1]] <- x
    return(b)
  }
  
  if (is.na(mono)) mono <- 0
  stopifnot(length(x) == length(y))
  stopifnot(mono %in% c(-1,0,1))
  stopifnot(max.bin > 0)
  if (is.null(min.cnt)) min.cnt <- sqrt(length(x))
  stopifnot(min.cnt > 0)
  
  # filter NAs and special values
  f0 <- x %in% exceptions
  f1 <- is.na(x)
  f <- f1 | f0
  y0 <- sum(y[!f1] == 0)
  y1 <- sum(y[!f1] == 1)
  
  # different approach for factors and numerics
  if (is.numeric(x)) {
    type <- "numeric"
    brks <-
      .Call('bin', as.double(x[!f]), as.double(y[!f]), as.double(min.iv),
                   as.integer(min.cnt), as.integer(max.bin), as.integer(mono),
                   as.double(exceptions))
    
    xb <- cut(x[!f], brks, labels = brks[-1]) # x-binned witout na or exceptions
    counts <- table(xb, y[!f]) # x-counts
    woe <- woe(xb, y[!f], y0, y1)
    except_counts <- table(x[f0], y[f0])
    if (dim(except_counts)[2] != 2) {
      except_ones <- NULL
      except_zero <- NULL
    } else {
      except_ones <-  except_counts[,2]
      except_zero <-  except_counts[,1]
    }
    
    except_woe <- if(any(f0)) woe(x[f0], y[f0], y0, y1) else 0
    
  } else {
    type <- "factor"
    woe <- woe(x, y, y0, y1)
    brks <- names(woe)
    exceptions <- NULL
    except_woe <- NULL
    counts <- table(x, y)
    except_ones <- NULL
    except_zero <- NULL
  }
  
  num_ones <- counts[,2]
  num_zero <- counts[,1]
  
  structure(list(
    x = x,
    y = y,
    type = type,
    breaks = brks,
    values = woe,
    num_ones = num_ones,
    num_zero = num_zero,
    na = 0,
    na_ones = sum(y[is.na(x)] == 1),
    na_zero = sum(y[is.na(x)] == 0),
    exceptions = exceptions,
    except_woe = except_woe,
    except_ones = except_ones,
    except_zero = except_zero,
    history=list(),
    min.iv=min.iv,
    min.cnt = min.cnt,
    max.bin=max.bin,
    mono=mono,
    exceptions=exceptions
  ), class = "bin")
}

predict.bin <- function(object, x) {
  # two types of maps -- numeric and character/factor 
  if (is.numeric(x)) {
    res <- object$values[cut(x, object$breaks, labels = FALSE)]
  } else {
    # check if breaks are actual characters
    res <- object$values[match(as.character(x), object$breaks)]
  }
  
  if (length(object$exceptions) != 0) { # exception values
    for (i in seq_along(object$exceptions)) {
      res[x == object$exceptions[i]] <- object$except_woe[i]
    }
  }
  
  res[is.na(res)] <- object$na # nas
  
  names(res) <- NULL
  return(res)
}

bin.list <- function(bins){
  names <- names(bins)
  if(is.null(names)) stop("list of bins must be named")
  stopifnot(length(bins) == length(names))
  classes <- sapply(bins, class)
  stopifnot(all(classes == "bin"))
  out <- structure(bins, class='bin.list')
}

bin.data <- function(df, y, mono=NULL, exceptions=NULL, ...) {
  mono.fill <- if(!is.na(mono["ALL"])) mono["ALL"] else 0
  .mono <- rep(mono.fill, ncol(df))
  
  except.fill <- if(!is.null(exceptions[["ALL"]])) exceptions[["ALL"]] else NA
  .exceptions <- as.list(rep( except.fill, ncol(df)))
  
  vars <- colnames(df)
  names(.mono) <- vars
  names(.exceptions) <- vars
  
  .mono[names(mono)] <- mono
  .exceptions[names(exceptions)] <- exceptions
  
  res <- list()
  for (i in seq_along(vars)) {
    nm <- vars[i]    
    cat(sprintf("\rProgress: %%%6.2f", (100*i/length(vars))))
    flush.console()
    res[[nm]] <- bin(df[,nm], y, mono=.mono[nm], exceptions=.exceptions[[nm]], ...)
  }
  cat("\n")
  
  return(bin.list(res))
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


`-.bin` <- function(e1, e2) {
  out <- e1
  
  if(e2[1] == 1 & length(e2) == 1) e2 <- 1:2
  
  a <- e2[1]
  z <- max(a + 1, e2[length(e2)])
  
  new_breaks <- e1$breaks[-((a+1):z)]
  new_ones   <- e1$num_ones[-(a:(z-1))]
  new_zero   <- e1$num_zero[-(a:(z-1))]
  
  new_ones[e2[1]] <- sum(e1$num_ones[a:z])
  new_zero[e2[1]] <- sum(e1$num_zero[a:z])
  
  pct1 <- new_ones/sum(new_ones)
  pct0 <- new_zero/sum(new_zero)
  
  out$values   <- log(pct1/pct0)
  out$num_ones <- new_ones
  out$num_zero <- new_zero
  out$breaks   <- new_breaks
  out$history[[length(out$history) + 1]] <- e1
  out
}

`<=.bin` <- function(e1, e2, ...) {
  if (e1$type == "factor") {
    print("cannot place cap on a factor")
    return(e1)
  }
  
  #f <- !(e1$x %in% e1$exceptions) & !is.na(e1$x)
  x <- pmin(e1$x, e2)
  
  bin(x, e1$y, e1$min.iv, min.cnt = e1$min.ctn, max.bin = e1$max.bin,
      mono = e1$mono, exceptions = e1$exceptions)
}

`+.bin` <- function(e1, e2) {
  if (e1$type == "factor") {
    print("factors currently not supported for grouping")
    return(e1)
  }
  
  out <- e1
  n <- length(e1$num_ones)
  f <-  (e1$x > e1$breaks[e2] & e1$x <= e1$breaks[e2 + 1]) &
        !(e1$x %in% e1$exceptions) &
        !is.na(e1$x)
  
  nvals <- length(unique(e1$x[f]))
  
  # break into quintiles if possible else split in twain
  if ((nvals) == 1){
    return(e1)
  } else if (nvals <= 5) {
    b <- bin(factor(e1$x[f]), e1$y[f])
    eps <- head(as.numeric(b$breaks), -1)
  } else {
    q <- unique(quantile(e1$x[f], seq(0, 1, 0.2)))
    b <- bin(cut(e1$x[f], c(-Inf, q)), e1$y[f])
    # grab endpoints from expanded range
    eps <- sapply(strsplit(b$breaks, ','), '[[', 2)
    eps <- head(as.numeric(gsub('\\(|\\]', '', eps)), -1)
  }
  
  if (e2 == 1) { # if first
    new_ones <- c(b$num_ones, e1$num_ones[-1])
    new_zero <- c(b$num_zero, e1$num_zero[-1])
  } else if (e2 == n) { # if last
    new_ones <- c(e1$num_ones[1:(e2-1)], b$num_ones)
    new_zero <- c(e1$num_zero[1:(e2-1)], b$num_zero)
  } else { # if in-between
    new_ones <- c(e1$num_ones[1:(e2-1)], b$num_ones, e1$num_ones[(e2+1):length(e1$num_ones)])
    new_zero <- c(e1$num_zero[1:(e2-1)], b$num_zero, e1$num_zero[(e2+1):length(e1$num_zero)])  
  }
  
  pct1 <- new_ones/sum(new_ones, na.rm=T)
  pct0 <- new_zero/sum(new_zero, na.rm=T)
  
  out$values   <- log(pct1/pct0)
  out$num_ones <- new_ones
  out$num_zero <- new_zero
  out$breaks   <- c(e1$breaks[1:(e2)], eps, e1$breaks[(e2 + 1):length(e1$breaks)])
  out$history[[length(out$history) + 1]] <- e1
  out
}

# neutralize levels
`!=.bin` <- function(e1, e2) {
  out <- e1
  # simply zero out the counts?
  out$num_ones[e2] <- 0
  out$num_zero[e2] <- 0
  
  values <- as.data.frame(out)[,'WoE']
  values[is.nan(values)] <- 0
  out$values  <- values
  out$history[[length(out$history) + 1]] <- e1
  out
}

undo <- function(x) {
  if (length(x$history) == 0) {
    return(x)
  } else {
    out <- x$history[[length(x$history)]]
    out$history <- x$history[-length(x$history)]
  }
  return(out)
}

as.data.frame.bin <- function(x, row.names = NULL, optional = FALSE, ...) {
  zero_ct <- c(x$num_zero, x$except_zero, x$na_zero)
  ones_ct <- c(x$num_ones, x$except_ones, x$na_ones)
  tot_ct  <- zero_ct + ones_ct
  zero_pct <- c(head(zero_ct, -1) / sum(head(zero_ct, -1), na.rm=T), NA)
  ones_pct <- c(head(ones_ct, -1) / sum(head(ones_ct, -1), na.rm=T), NA)
  tot_pct  <- c(head(tot_ct , -1) / sum(head(tot_ct , -1), na.rm=T), NA)
  prob <- c(ones_ct / tot_ct)
  woe  <- log(ones_pct / zero_pct)
  iv   <- woe * (ones_pct - zero_pct)
  
  out <- data.frame(zero_ct, ones_ct, zero_pct, ones_pct, tot_pct,
                    prob, woe, iv)
  
  colnames(out) <- c('#0', '#1', 'W%0', 'W%1', 'W%' ,'P(1)', 'WoE', 'IV')
  keep <- which(!apply(out, 1, function(x) all(is.na(x))))
  out <- out[keep,]
  
  if(x$type == "numeric") {
    rnames <- paste(seq(1, length(x$breaks) - 1), ' (', paste(head(x$breaks, -1), x$breaks[-1], sep = " - "), ']', sep='')
    rnames <- c(rnames, x$exceptions, "Missing")
    rownames(out) <- rnames[keep] # filter out NA exceptions
  } else {
    rnames <- paste(seq(1, length(x$breaks)), c(x$breaks))
    rownames(out) <- c(rnames, x$exceptions, "Missing")
  }
  
  out['Missing', c('WoE', 'IV')] <- 0
  
  tot.row <- apply(out, 2, sum, na.rm=T)
  tot.row["WoE"] <- 0
  tot.row["P(1)"] <- tot.row["#1"] / sum(tot.row["#1"], tot.row["#0"])
  rbind(out, Total=tot.row)
}

print.bin <- function(x) {
  out <- as.data.frame(x)
  fmts <- c("%d", "%d", rep("%1.3f", 5), "%0.5f")
  
  for (i in seq_along(out)) {
    out[,i] <- sprintf(fmts[i], out[,i])
  }
  
  print(out)
}

bin.theme <- theme(
  axis.line=element_blank(),
  axis.text.y=element_blank(),axis.ticks=element_blank(),
  axis.title.y=element_blank(),legend.position="none")


plot.bin <- function(x, y, ...) {
  tmp <- as.data.frame(x)
  n <- 1:(nrow(tmp) - 2)
  plt <- data.frame(
    "Range"=factor(row.names(tmp[n,]), levels=rev(unique(row.names(tmp[n,])))),
    "Count"=apply(tmp[n,1:2], 1, sum),
    "Pct"=tmp[n,5],
    "Prob"=tmp[n,6],
    "WoE"=tmp[n,7],
    row.names=NULL
  )
  
  plt$WoE[is.nan(plt$WoE)] <- 0
  print(plt)
  
  # number of data points
  h <- nrow(plt) *.1
  
  with(plt[order(plt$Range),], barplot(WoE, names.arg = Range, horiz=T, las=2, ylim=c(0,10), xlim=c(min(WoE)-1,max(WoE)+1)))
  
}

print.bin.list <- function(x) {
  vars <- names(x)
  ivs <- sapply(x, function(x) as.data.frame(x)['Total', 'IV'])
  
  for (v in vars[order(-ivs)]) {
    cat(sprintf("\nIV: %0.3f | Variable: %s\n", ivs[v], v))
    print(x[[v]])
  }
}




