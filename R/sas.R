#' Create character vector of SAS code for printing
#' 
#' @param x a \code{bin} or \code{bin.list} object
#' @return a character vector of SAS statements. Use \code{cat} to export

#' @export
sas <- function(x) {
  UseMethod("sas", x)
}

# pieces common to both factor and numeric translation
base.sas <- function(x) {
  v <- x$name
  text <- list(sprintf("  * Variable: %s ;\n", v))
  # missing text
  txt1 <- sprintf("    if missing(%s)", v)
  txt2 <- sprintf("\n        then %s_w = %02.8f;\n", v, 0)
  text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
  
  # exceptions text
  e <- x$core$values$exc
  for (i in seq_along(e)) {
    txt1 <- sprintf("    else if %s = %s", v, names(e)[i])
    txt2 <- sprintf("\n        then %s_w = %02.8f;\n", v, e[i])
    text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
  }
  text
}

#' @export
sas.bin.numeric <- function(x) {
  text <- base.sas(x)
  
  b <- x$core$breaks[-1]
  for (i in  seq_along(b)) {
    if (i == length(b)) break
    txt1 <- sprintf("    else if %s <= %s", x$name, b[i])
    txt2 <-
      sprintf("\n        then %s_w = %02.8f;\n", x$name, x$core$values$var[i])
    text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
    
  }
  text[[length(text)+1]] <-
    sprintf("    else %s_w = %02.8f;\n", x$name, x$core$values$var[i])
  do.call(c, text)
}

#' @export
sas.bin.factor <- function(x) {
  text <- base.sas(x)
  
  b <- x$core$breaks
  for (i in  seq_along(b)) {
    items <- gsub(',', "','", b[i])
    txt1 <- sprintf("    else if %s in ('%s')", x$name, items)
    txt2 <-
      sprintf("\n        then %s_w = %02.8f;\n", x$name, x$core$values$var[i])
    text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
  }
  text[[length(text)+1]] <- sprintf("    else %s_w = %02.8f;\n", x$name, 0)
  do.call(c, text)
}

sas.bin.list <- function(x) {
  out <- list()
  for (i in seq_along(x)) {
    out[[i]] <- c('\n', sas(x[[i]]))
  }
  do.call(c, out)
}