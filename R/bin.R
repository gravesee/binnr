#' @useDynLib binnr
#' @import ggplot2
#' @import grid

library(ggplot2)
library(scales)
library(gridExtra)

woe <- function(x, y, y0, y1) {
  if (length(x) == 0)  return(NULL)
  pt <- table(x, factor(y, levels=c(0,1)))
  pct0 <- pt[,1]/y0
  pct1 <- pt[,2]/y1
  woe <- log(pct1/pct0)
  woe[(is.infinite(woe))] <- 0
  woe
}

is.bin <- function(x) {
  inherits(x, "bin")
}

# TODO: split bin into bin.factor and bin.numeric
#' @export
bin <- function(x, y=NULL, name=NULL, min.iv=.01, min.cnt = NULL, max.bin=10, mono=0, exceptions=NULL){
  
  if (is.bin(x)) {
    b <- bin(x$x, x$y, x$name, min.iv, min.cnt, max.bin, mono, exceptions)
    b$history <- x
    return(b)
  }
  
  # Error checking 
  stopifnot(length(x) == length(y))
  if (is.na(mono)) mono <- 0
  stopifnot(mono %in% c(-1,0,1,2))
  stopifnot(max.bin > 0)
  if (is.null(min.cnt)) min.cnt <- sqrt(length(x))
  stopifnot(min.cnt > 0)
  
  # filter NAs and special values
  f0 <- x %in% exceptions
  f1 <- is.na(x)
  f <- f1 | f0
  y0 <- sum(y[!f1] == 0)
  y1 <- sum(y[!f1] == 1)
  
  if (all(is.na(x[!f0]))) {
    warning("\nnon-exception levels are all NA: ", name)
    return(NULL)
  }
  
  # different approach for factors and numerics
  
  if (is.numeric(x)) {
    type <- "numeric"
    brks <-
      .Call('bin', as.double(x[!f1]), as.double(y[!f1]), as.double(min.iv),
                   as.integer(min.cnt), as.integer(max.bin), as.integer(mono),
                   as.double(exceptions))
    
    xb <- cut(x[!f], brks, labels = brks[-1]) # x-binned witout na or exceptions
    counts <- table(xb, factor(y[!f], levels=c(0, 1)))
    woe <- woe(xb, y[!f], y0, y1)
    
    except_counts <- table(x[f0], factor(y[f0], levels=c(0,1)))
    except_zero <- except_counts[,1]
    except_ones <- except_counts[,2]
    except_woe <- if(any(f0) & all(c(y0, y1)) > 0) woe(x[f0], y[f0], y0, y1) else 0
    
    map <- NULL
    
  } else {
    type <- "factor"
    
    map <- as.list(levels(x))
    names(map) <- levels(x)
      
    woe <- woe(x, y, y0, y1)
    brks <- levels(x)
    exceptions <- NULL
    except_woe <- NULL
    counts <- table(x, factor(y, levels=c(0,1)))
    except_ones <- NULL
    except_zero <- NULL
  }
  
  num_ones <- counts[,2]
  num_zero <- counts[,1]
  
  structure(list(
    x = x,
    y = y,
    name = name,
    type = type,
    map = map,
    breaks = brks,
    values = woe,
    num_ones = num_ones,
    num_zero = num_zero,
    na = 0,
    na_ones = sum(y[is.na(x)] == 1),
    na_zero = sum(y[is.na(x)] == 0),
    exceptions = sort(unique(x[f0])),
    except_woe = except_woe,
    except_ones = except_ones,
    except_zero = except_zero,
    history=list(),
    skip=FALSE
  ), class = "bin")
}

#' @export
predict.bin <- function(object, x) {
  # two types of maps -- numeric and character/factor 
  if (is.numeric(x)) {
    res <- object$values[cut(x, object$breaks, labels = FALSE)]
    
    if (length(object$exceptions) != 0) { # exception values
      for (i in seq_along(object$exceptions)) {
        res[x == object$exceptions[i]] <- object$except_woe[i]
      }
    }
  } else {
    res <- numeric(length(x))
    res[is.na(x)] <- NA
    res[!is.na(x)] <- object$values[unlist(object$map[x])]
  }
  
  res[is.na(res)] <- object$na
  
  names(res) <- NULL
  return(res)
}

bin.list <- function(bins){
  stopifnot(all(sapply(bins, is.bin)))
  structure(bins, class='bin.list')
}

#' @export
bin.data <- function(df, y, mono=c(ALL=0), exceptions=list(ALL=NULL), ...) {
  stopifnot(is.list(exceptions))
  
  vars <- colnames(df)
  .mono <- rep(mono["ALL"], ncol(df))
  names(.mono) <- vars
  .mono[names(mono)] <- mono
  .exceptions <- rep(list(exceptions[['ALL']]), length.out=ncol(df))
  names(.exceptions) <- vars
  .exceptions[names(exceptions)] <- exceptions
  
  dashes <- c('\\','|','/','-')
  
  drop.vars <- list()
  res <- list()
  for (i in seq_along(vars)) {
    nm <- vars[i]
    cat(sprintf("\rProgress: %s %6.2f%%", dashes[(i %% 4) + 1], (100*i/length(vars))))
    flush.console()
    b <- bin(df[,nm], y, name = nm, mono=.mono[nm], exceptions=.exceptions[[nm]], ...)
    if (!is.null(b)) res[[nm]] <- b
  }
  cat("\n")
  
  return(bin.list(res))
}

#' @export
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
    if (!object[[nm]]$skip) {
      res[[vars[i]]] <- predict(object[[nm]], newdata[,nm])
    }
  }
  cat("\n")
  res <- do.call(cbind, res)
  return(res)
}

collapse.bin.numeric <- function(e1, e2) {
  if (max(e2) > length(e1$values)) {
    warning("Cannot collapse exception bins")
    return(e1)
  }
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
  out
}

#' @export
`-.bin` <- function(e1, e2) {
  if (e1$type == "numeric") {
    out <- collapse.bin.numeric(e1, e2)
  } else if (e1$type == "factor") {
    out <- collapse.bin.factor(e1, e2)
  }
  out$history <- e1
  out
}

#' @export
`<=.bin` <- function(e1, e2, ...) {
  if (e1$type == "factor") {
    print("cannot place cap on a factor")
    return(e1)
  }
  
  #f <- !(e1$x %in% e1$exceptions) & !is.na(e1$x)
  x <- pmin(e1$x, e2)
  
  bin(x, e1$y, e1$name, e1$min.iv, min.cnt = e1$min.ctn, max.bin = e1$max.bin,
      mono = e1$mono, exceptions = e1$exceptions)
}

expand.bin.numeric <- function(e1, e2) {
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
    b <- bin(factor(e1$x[f]), e1$y[f], e1$name)
    eps <- head(as.numeric(b$breaks), -1)
  } else {
    q <- unique(quantile(e1$x[f], seq(0, 1, 0.2)))
    b <- bin(cut(e1$x[f], c(-Inf, q[-1])), e1$y[f], e1$name)
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
  out$history <- e1
  out
}

### need to figure out how to combine *combined* bins!

### some work to do yet
collapse.bin.factor <- function(e1, e2) {
  x <- e1$x
  map <- e1$map
  f <- e1$map %in% e1$breaks[e2]
  map[f] <- paste(e1$breaks[e2], collapse=',')
  levels(x) <- unlist(map)
  b <- bin(x, e1$y, e1$name)
  b$map <- map
  b$x <- e1$x
  b
}

expand.bin.factor <- function(e1, e2) {
  x <- e1$x
  f <- e1$map == e1$breaks[e2]
  map <- e1$map
  map[f] <- levels(e1$x)[f]
  levels(x) <- unlist(map)
  b <- bin(x, e1$y, e1$name)
  b$map <- map
  b$x <- e1$x
  b
}

#' @export
`+.bin` <- function(e1, e2) {
  if (e1$type == "factor") {
    expand.bin.factor(e1, e2)
  } else {
    expand.bin.numeric(e1, e2)
  }
}

# neutralize levels
#' @export
`!=.bin` <- function(e1, e2) {
  out <- e1
  if (e1$type == 'numeric') {
    n <- length(out$values)
    N <- n + length(out$except_zero)
    # simply zero out the counts?
    num_zero <- c(out$num_zero, out$except_zero)
    num_ones <- c(out$num_ones, out$except_ones)
    
    num_ones[e2] <- 0
    num_zero[e2] <- 0
    
    out$num_zero <- num_zero[1:n]
    out$num_ones <- num_ones[1:n]
    
    if (N > n) {
      out$except_zero <- num_zero[(n+1):N]
      out$except_ones <- num_ones[(n+1):N]
    }
    
    values <- as.data.frame(out)[1:N,'WoE']
    values[is.nan(values)] <- 0
    out$values  <- values[1:n]
    out$except_woe <- values[(n+1):N]
    out$history <- e1
  } else {
    if (max(e2) > length(out$breaks)) return(out)
    out$num_zero[e2] <- 0
    out$num_ones[e2] <- 0
    out$history <- e1
  }
  out
}

#' @export
undo <- function(x) {
  if (is.null(x$history)) return(x)
  return(x$history)
}

#' @export
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
    rnames <- paste('(', paste(head(x$breaks, -1), x$breaks[-1], sep = " - "), ']', sep='')
    rnames <- c(rnames, x$exceptions)[head(keep, -1)]
    rnames <- c(paste(sprintf("%2d:", seq(1,length(rnames))), rnames), "Missing")
  } else {
    rnames <- paste(sprintf("%2d:", seq(1, length(x$breaks))), c(x$breaks))
    rnames <- c(rnames, "Missing")
  }
  
  out[nrow(out), c('WoE', 'IV')] <- 0
  out[is.infinite(out[,'WoE']),'WoE'] <- 0
  out[is.infinite(out[,'IV']),'IV'] <- 0
  
  tot.row <- apply(out, 2, function(x) sum(x[!is.infinite(x)], na.rm=T))
  tot.row["WoE"] <- 0
  tot.row["P(1)"] <- tot.row["#1"] / sum(tot.row["#1"], tot.row["#0"])
  out <- rbind(out, Total=tot.row)
  rownames(out) <- c(rnames, "Total")
  out
}

#' @export
print.bin <- function(x, ...) {
  #var <- strsplit(deparse(match.call()$x), "\\$|\\s+")[[1]][2]
  
  out <- as.data.frame(x)
  iv <- out['Total', 'IV']
  fmts <- c("%d", "%d", rep("%1.3f", 5), "%0.5f")
  for (i in seq_along(out)) {
    out[,i] <- sprintf(fmts[i], out[,i])
  }
  status <- ifelse(x$skip, " *** DROPPED ***", "")
  cat(sprintf("\nIV: %0.5f | Variable: %s%s\n", iv, x$name, status))
  print(out)
}


#' @export
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
  
  plt$WoE[is.nan(plt$WoE) | is.infinite(plt$WoE)] <- 0
  
  plt.theme <- theme(
    axis.line=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.y=element_blank(),legend.position="none")
  
  g1 <- ggplot(plt, aes(x=Range, y=Count)) +
    geom_bar(stat="identity", position="identity") +
    coord_flip()
  
  g2 <- ggplot(plt, aes(x=Range, y=WoE, fill=WoE)) +
    geom_bar(stat="identity", position="identity") +
    scale_fill_gradient(low="blue", high="red") + coord_flip() + plt.theme
  
  g3 <- ggplot(plt, aes(x=Range, y=Prob)) +
    geom_bar(stat="identity", position="identity") +
    geom_hline(yintercept=tmp[nrow(tmp),6], col="red", size=1) +
    coord_flip() + plt.theme
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(2, 3, heights = unit(c(1, 10), "null"))))
  grid.text(x$name, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:3))
  print(g1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(g2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  print(g3, vp = viewport(layout.pos.row = 2, layout.pos.col = 3))  
}

#' @export
print.bin.list <- function(x, n=NULL, plot=F) {
  if (is.null(n)) {
    n <- 1:length(x)
  } else {
    n <- 1:min(length(x), n)
  }
  
  # TODOD: make iv a part of the bin object?
  ivs <- sapply(x, function(x) as.data.frame(x)['Total', 'IV'])
  
  for (b in x[order(-ivs)]) {
    print(b)
  }
}

#' @export
adjust <- function(x, vars=NULL, min.iv=0) {
  eval(parse(text = paste(substitute(x), "<<- adjust.bins(x, vars, min.iv)")))
}

#' @export
adjust.bins <- function(x, vars=NULL, min.iv=0) {
  
  if (is.null(vars)) vars <- names(x)
  ivs <- sapply(x, function(x) as.data.frame(x)['Total', 'IV'])
  f <- (names(x) %in% vars) & (ivs >= min.iv)
  idx <- seq_along(names(x))[f]
  
  out <- x
  i <- 1
  while(i <= length(idx)) {
    print(out[[idx[i]]])
    plot(out[[idx[i]]])
    cat ("\nEnter command (Q to quit):")
    command <- readLines(n = 1)
    if (command == "Q") {
      break
    }  else if (command %in% c("h", "help")) {
      cat(
"binnr interactive commands:
 (Q)uit
 (n)ext
 (p)revious
 (g)oto
 (u)ndo
 (b)in
 (d)drop
binnr bin operations
 != <#> : Neutralize level
 + <#>  : Expand level
 - <#>  : Collapse level(s)
 <= <#> : Cap at # and rebin\n")
      cat("Press [Enter] to continue")
      scan(n=1)
    } else if (command == "g") {
      cat("Goto variable:")
      v <- readLines(n = 1)
      # find the position of the variable
      while (v != "Q") {
        pos <- which(vars[f] == v)[1]
        if (is.na(pos)) {
          # find similar matches
          sim <- agrep(v, vars[f], ignore.case = T, max.distance = 0.1)
          if (length(sim) > 0){
            cat(sprintf("%s not found, similar matches:", v))
            cat(sprintf("\n %2d: %s", seq_along(sim), vars[f][sim]))
            cat("\nGoto variable:")
            inp <- readLines(n = 1)
            n <- suppressWarnings(as.integer(inp))
            if (!is.na(n) & n <= length(sim)) { # check if number entered
              v <- vars[f][sim[n]]
            }
          } else {
            cat("No similar variables found")
            cat("\nHit [Enter] to continue")
            readLines(n=1)
            invisible()
            break
          }
        } else { # found exact match
          i <- pos
          break
        }
      }
    } else if (command == "d") {
      out[[idx[i]]]$skip <- !out[[idx[i]]]$skip
      #i <- i + 1
    } else if (command == "n") {
      i <- i + 1
    } else if (command == "p") {
      if (i > 1) {
        i <- i - 1 
      } else {
        cat("\nAt beginning of list")
      }
    } else if (command == "u") {
      out[[idx[i]]] <- undo(out[[idx[i]]])
    } else if (command == "b") {
      cat("\nEnter bin commands separated by commas:")
      args <- readLines(n = 1)
      tryCatch({
        out[[idx[i]]] <- eval(parse(text=sprintf("bin(out[[idx[i]]], %s)", paste(args, sep=','))))
      }, error = function(err) {
        cat("\nInvalid command entered")
      })
    } else {
      tryCatch({
        out[[idx[i]]] <- eval(parse(text=paste("out[[idx[i]]]", command)))
      }, error = function(err) {
        cat("\nInvalid command entered")
      })
    }
  }
  out
}