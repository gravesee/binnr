## print progress update bar to console
progress_ <- function(i, max, text = "Progress", extra="") {
  progress <- paste(rep("=", (10*i/max)), collapse="")
  cat(sprintf("\r%s : %-10s| %-50s", text, progress, extra))
}


## calcualte LR2 for logistic regression
lr2_ <- function(f, y, w) {
  f <- plogis(as.vector(f))
  sum((y == 1)*w*log(f) + (y == 0)*w*log(1 - f))
}

## calculate score contributions for Scorecard
contributions_ <- function(data, coefs, y, w, offset=0) {
  base <- lr2_(0, y, w)

  lr2 <- sapply(seq_along(coefs), function(i) {
    if (i > 1) {
      coefs[i] <- 0
    }
    1 - (lr2_(data %*% coefs[-1] + coefs[1] + offset, y, w) / base)
  })
  names(lr2) <- c("Base", names(coefs)[-1])
  lr2[1] - lr2[-1]
  #lr2[-1]
}

ks_ <- function(score, y, w) {
  cml_bds <- cumsum(((y == 1)*w)[order(-score)]) / sum((y == 1) * w)
  cml_gds <- cumsum(((y == 0)*w)[order(-score)]) / sum((y == 0) * w)
  max(abs(cml_bds - cml_gds))
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("For binnr cheat sheet run: vignette(\"binnr-cheat-sheet\")")
  packageStartupMessage("For binnr quick start run: vignette(\"binnr-quick-start-guide\")")
}
