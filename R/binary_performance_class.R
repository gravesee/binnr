#' @include performance_class.R continuous_class.R discrete_class.R
NULL

#' Bin reference class generator
#'
#' @name Binary_Performance_Class
#' @description Binary_Performance object generator class that implements
#' the Performance class methods
#' @field ones sum of \code{y == 0}
#' @field zeros sum of \code{y == 1}
#' @export Binary_Performance
#' @exportClass Binary_Performance
Binary_Performance <- setRefClass("Binary_Performance", fields = c(
  ones = "numeric",
  zeros = "numeric"),
  contains = "Performance")


Binary_Performance$methods(initialize =  function(y=.self$y, ...) {
  callSuper(y=y, ...)
  ones <<- sum((y == 1) * w)
  zeros <<- sum((y == 0) * w)
})


#' @describeIn bin_ Bin a Continuous object using Binary_Performance
#' @return modifies the Bin object in place
setMethod("bin_",
  signature = c(.self="Binary_Performance", b="Continuous"),
  function(.self, b, min.iv=0.01, min.cnt=10, min.res=0,
    max.bin=10, mono=0, exceptions=numeric(0)) {

    f <- !is.na(b$x)
    b$tf@tf <- .Call("bin", as.double(b$x[f]), as.double(.self$y[f]),
      as.double(.self$w[f]), as.double(min.iv), as.integer(min.cnt),
      as.integer(min.res), as.integer(max.bin), as.integer(mono),
      as.double(exceptions))

    b$tf@exceptions <- setNames(rep(0, length(exceptions)), exceptions)

  })


#' @describeIn bin_ Bin a Discrete object using Binary_Performance
#' @return modifies the Bin object in place
setMethod("bin_",
  signature = c(.self="Binary_Performance", b="Discrete"),
  function(.self, b, exceptions=numeric(0), ...) {

    b$tf@tf <- as.list(levels(b$x))
    names(b$tf@tf) <- levels(b$x)
    b$tf@exceptions <- setNames(rep(0, length(exceptions)), exceptions)
  })


#' Subsequent call from the bin function passed to Binary_Performance object
#'
#' @name Binary_Performance_bin
#' @description This bin function should not be directly called by the user.
#' The Classing bin function is subsequently called from the
#' \link{\code{bin}} wrapper function.
NULL
Binary_Performance$methods(bin = bin_)


#' Summarize method implementation for Binary_Performance
#'
#' @name Binary_Performance_summarize
#' @param x discretized independent variable as a factor
#' @param y response variable
#' @param w weight variable
#' @description this function summarizes the relationship between the
#' independent and response variables. The inherited Performance class demands
#' that it be implemented.
#' @return a matrix of summary information for every level in the factor x
NULL
Binary_Performance$methods(summarize = function(x, y, w) {
  N1 <- tapply((y == 1) * w, x, sum)
  N0 <- tapply((y == 0) * w, x, sum)

  N <- tapply(w, x, sum)
  P1 <- N1 / ones
  P0 <- N0 / zeros
  WoE <- log(P1 / P0)
  IV <- (P1 - P0) * WoE

  res <- cbind(N = N, `#1` = N1, `#0` = N0, `%N` = N / sum(ones, zeros),
    `%1` = N1 / ones, `%0` = N0 / zeros, `P(1)` = N1 / N,
    WoE = WoE, IV = IV, Pred = WoE)

  res[is.na(res) | is.infinite(res)] <- 0
  res

})


#' Update method implementation for Binary_Performance
#'
#' @name Binary_Performance_update
#' @param b Bin object to update
#' @description update is called after every applicable bin operation that
#' modifies the Transform object. The result of calling update is used for
#' displaying the Bin matrix as well as plotting the Bin object.
#' @return a list of matrices with summarized information.
NULL
Binary_Performance$methods(update = function(b, ...) {

  f <- b$factorize()

  m <- summarize(f, y, w)

  lvls <- levels(f)
  ids <- list(
    normal = NULL,
    exceptions = match(names(b$tf@exceptions), lvls, 0),
    missing = match("Missing", lvls, 0))

  ids$normal <- seq.int(nrow(m))[-c(ids$missing, ids$exceptions)]

  out <- lapply(ids, function(x) m[x,,drop=FALSE])

  out$Total <- matrix(colSums(m, na.rm=TRUE), nrow=1,
    dimnames = list("Total", colnames(m)))

  out$Total[,c("P(1)", "WoE", "Pred")] <- 0

  out

})

# internal helper function for plotting
make_bars_ <- function(v, width=0.70, ...) {
  left <- pmin(v, 0)
  right <- pmax(v, 0)
  center <- seq_along(left)
  top <- center - 0.5 * width
  bottom <- center + 0.5 * width
  rect(left, bottom, right, top, ...)
  center
}

#' Plot method implementation for Binary_Performance
#'
#' @name Binary_Performance_plot
#' @param b Bin object to update
#' @description plot displays a horizontal bar chart showing summarized
#' performance information. The large bars represent the observed WoE within
#' each bin. The smaller bars within represent the WoE that will be substituted
#' during prediction. Record count percentages are shown on the right side of
#' the plot while a sequential index used for referencing bin levels is printed
#' along the left.
NULL
Binary_Performance$methods(plot = function(b, ...) {

    on.exit(par(oma=rep(0, 4))) # restore them on exit


    tmp <- head(b$as.matrix(), -1)
    lbls <- rev(row.names(tmp))
    woe <- rev(tmp[,"WoE"])
    val <- rev(tmp[,"Pred"])
    pctN <- sprintf("%0.1f%%", rev(tmp[, "%N"] * 100))

    ## find the max and min
    xlim <- range(c(woe, val)) + c(-0.5, 0.5)

    ## set margin based on nchars
    width <- max(nchar(lbls))
    par(oma=c(0, width/6, 0, 0))

    graphics::plot(NA, xlim=xlim, ylim=c(0.5, length(woe) + 0.5),
      xlab = "Weight of Evidence", ylab=NA, yaxt="n", main = b$name)

    abline(v = 0, lty=3)
    center <- make_bars_(woe, col=rgb(0, 0, 0, alpha = 0.30))
    center <- make_bars_(val, width=0.2, col="red")

    text(x = min(xlim), y = center, labels = sprintf(" [%02d]",
      rev(seq_along(lbls))), cex=0.80)

    text(x = max(xlim) - 0.1, y = center, labels = pctN, cex=0.80)

    axis(side = 2, labels = lbls, at = center, las = 2, lwd.ticks = 0,
      cex.axis = 0.80)

  })


#' Summary method implementation for Binary_Performance
#'
#' @name Binary_Performance_summary
#' @param tf Transform object to summarize
#' @description summary returns the appropriate pieces from the Transform object
#' to summarize the relationship in a conciese one-line entry.
#' @return a name vector of summary information
NULL
Binary_Performance$methods(summary = function(tf, ...) {
  ## return the information value of the bin
  tot <- tf@repr$Total[,c("IV", "N", "#1", "#0", "P(1)")]
  nas <- unname(colSums(tf@repr$missing[,"N", drop=F], na.rm=T))
  exc <- unname(colSums(tf@repr$exception[,"N", drop=F], na.rm=T))

  out <- c(tot, "N missing"=nas, "N Exceptions"=exc)
  out[is.na(out)] <- 0
  out

})


#' Sort_value method implementation for Binary_Performance
#'
#' @name Binary_Performance_sort_value
#' @param b Bin object to update
#' @description sort_value returns the information value for the requested Bin
#' @return the value to use for sorting bins using Binary_Performance
NULL
Binary_Performance$methods(sort_value = function(b, ...) {
  b$tf@repr$Total[,"IV"]
})


#' Return an object that tells openxlsx how to print it to Excel
#'
#' @name Binary_Performance_get_excel_table
#' @param b Bin object to update
#' @return Returns a \code{openxlsx-table} object that \code{binnrtools} can
#' use to format a bin object for Excel
NULL
Binary_Performance$methods(get_excel_table = function(b, ...) {

  structure(
    list(
      data = b$as.matrix(),
      name = b$name,
      format_info = list(
        percent_cols = 4:7,
        comma_cols = 1:3,
        rounded_cols = 8:10,
        bar_cols = 10
      )
    ),
    class="openxlsx-table"
  )

})
