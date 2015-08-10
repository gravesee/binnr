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

### TODO: pass in own breaks as well if necessary also caps... 
bin <- function(x, y, min.iv=.01, min.cnt = NULL, max.bin=10, mono=0, exceptions=NA){
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
    except_zero = except_zero
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
  .mono <- numeric(ncol(df))
  .exceptions <- as.list(rep(NA, ncol(df)))
  vars <- colnames(df)
  names(.mono) <- vars
  names(.exceptions) <- vars
  
  .mono[names(mono)] <- mono
  .exceptions[names(exceptions)] <- exceptions
  
  res <- list()
  for (i in seq_along(vars)) {
    nm <- vars[i]    
    cat(sprintf("\rProgress: %%%3d", as.integer(100*i/length(vars))))
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
  
  new_breaks <- e1$breaks[-(e2 + 1)]
  
  new_ones <- e1$num_ones[-e2]
  new_zero <- e1$num_zero[-e2]
  
  new_ones[e2] <- e1$num_ones[e2] + e1$num_ones[e2 + 1]
  new_zero[e2] <- e1$num_zero[e2] + e1$num_zero[e2 + 1]
  
  pct1 <- new_ones/sum(new_ones)
  pct0 <- new_zero/sum(new_zero)
  
  out$values <- log(pct1/pct0)
  out$num_ones <- new_ones
  out$num_zero <- new_zero
  out$breaks <- new_breaks
  out
}

xeno.table <- function(x, y, object) {
  if (class(x) %in% c('numeric', 'integer')) {
    res <- cut(x, object$breaks)
  } else {
    # check if breaks are actual characters
    res <- levels(x)[match(as.character(x), object$breaks)]
  }
  if (length(object$exceptions) != 0) { # special values
    levels(res) <- c(levels(res), object$exceptions)
    for (i in seq_along(object$exceptions)) {
    res[x == object$exceptions[i]] <- object$exceptions[i]
    }
  }
  if (!is.null(object$na)) res <- addNA(res)
  # add the stats
  tbl <- table(res, y)
  pct <- prop.table(tbl, margin = 2)
  tot <- prop.table(table(res))
  prb <- apply(tbl, 1, function(x) x[2]/sum(x))
  woe <- log(pct[,2]/pct[,1])
  iv  <- woe * (pct[,2] - pct[,1])
  out <- cbind(tbl, pct, tot, prb, woe, iv)
  colnames(out) <- c('#0', '#1', 'W%0', 'W%1', 'W%' ,'P(1)', 'WoE', 'IV')
  rownames(out)[is.na(rownames(out))] <- "Missing"
  return(structure(out, class='xeno.table'))
}

#plot.xeno.table


titanic <- read.csv('~/Downloads/train.csv', header=T)

x <- titanic$Fare
y <- titanic$Survived

# bin(x, y)

#bins <- bin.data(titanic[,-1], titanic[,1], min.iv=)



### TESTING ###
# s <- sample(nrow(titanic), nrow(titanic)/2)
# bins <- bin.data(titanic[s,], titanic$Survived[s], min.iv=.025, mono=c(Age=-1, Fare=1))
# binned <- predict(bins, titanic)
# 
# library(glmnet)
# fit <- cv.glmnet(binned, titanic$Survived, family='binomial', alpha=1, nfolds=10, keep = T)
# phat <- fit$fit.preval[,which.min(fit$cvm)]
# 
# library(mjollnir)
# ks.table(-phat, titanic$Survived)


# bins <- bin.data(rv50[,keep50], y = rv50$depvar, mono = mono50, exceptions = -1, min.iv=.01)
# bins$confirmationsubjectfound <- bin(rv50$confirmationsubjectfound, rv50$depvar, min.iv=0)
# binned <- predict(bins, rv50)
# library(glmnet)
# 
# s <- sample(nrow(binned), nrow(binned)/10)
# fit <- cv.glmnet(binned[s,], rv50$depvar[s], alpha=1, lower.limit=0, family="binomial", nfolds=3)
# phat <- predict(fit, binned[-s,], s="lambda.min")
# 
# library(mjollnir)
# ks.table(-phat, rv50$depvar[-s])
# 
# tables <- lapply(names(bins), function(v) xeno.table(rv50[,v], rv50$depvar, bins[[v]]))
# names(tables) <- names(bins)
# 
# 













