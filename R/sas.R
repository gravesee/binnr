# functions for exporting SAS code

#' @export
sas.bin <- function(x) {
  v <- x$name
  text <- list(sprintf("    * Variable: %s ;\n", v))
  # missing text
  txt1 <- sprintf("    if missing(%s)", v)
  txt2 <- sprintf("\n        then %s_w = %02.8f;\n", v, x$na)
  text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
  
  # exceptions text
  e <- x$exceptions
  for (i in seq_along(e)) {
    txt1 <- sprintf("    else if %s = %s", v, e[i])
    txt2 <- sprintf("\n        then %s_w = %02.8f;\n", v, x$except_woe[i])
    text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
  }
  
  # all normal values
  if (x$type == "numeric") {
    b <- x$breaks[-1]
    for (i in  seq_along(b)) {
      if (i == length(b)) break
      txt1 <- sprintf("    else if %s <= %s", v, b[i])
      txt2 <- sprintf("\n        then %s_w = %02.8f;\n", v, x$values[i])
      text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
      
    }
    text[[length(text)+1]] <- sprintf("    else %s_w = %02.8f;\n", v, x$values[i])
  } else {
    b <- x$breaks
    for (i in  seq_along(b)) {
      items <- gsub(',', "','", b[i])
      txt1 <- sprintf("    else if %s in ('%s')", v, items)
      txt2 <- sprintf("\n        then %s_w = %02.8f;\n", v, x$values[i])
      text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
    }
    text[[length(text)+1]] <- sprintf("    else %s_w = %02.8f;\n", v, 0)
  }
  return(c(unlist(text), sprintf("\n")))
}