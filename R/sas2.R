
# pieces common to both factor and numeric translation
base.sas <- function(x) {
  
  
  
  
}

#' @export
sas.bin.numeric <- function(b, p, sfx) {
  v <- b$name
  vs <- paste(b$name, sfx, sep='_')
  
  text <- list(sprintf("  * Variable: %s ;\n", v))
  
  # missing text
  txt1 <- sprintf("    if missing(%s)", v)
  txt2 <- sprintf("\n        then %s = %s;\n", vs, p$nas)
  text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
  
  # exceptions text
  for (i in seq_along(p$exc)) {
    txt1 <- sprintf("    else if %s = %s", v, names(p$exc)[i])
    txt2 <- sprintf("\n        then %s = %s;\n", vs, p$exc[i])
    text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
  }
  
  for (i in  head(seq_along(p$var), -1)) {
    txt1 <- sprintf("    else if %s <= %s", v, names(p$var)[i])
    txt2 <-
      sprintf("\n        then %s = %s;\n", vs, p$var[i])
    text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
  }
  text[[length(text)+1]] <-
    sprintf("    else %s = %s;\n", vs, p$var[i+1])
  do.call(c, text)
}

# interface to the SAS function:
pred.list.woe <- function(b) {
  
  # var woe
  var <- b$core$values$var
  names(var) <- b$core$breaks[-1]
  
  list(
    nas=0,
    exc=b$core$values$exc,
    var=var)
}

pred.list.dist <- function(b, coef=1) {
  
  # var woe
  var <- coef * b$core$values$var
  var <- min(var) - var
  names(var) <- b$core$breaks[-1]
  
  list(
    nas=0,
    exc=b$core$values$exc,
    var=var)
}

pred.list.rcs <- function(b, coef=1) {
  if (is.null(b$rcs)) {
    rcs(b) <- "''"
  }
  
  # var woe
  var <- dQuote(b$rcs$var)
  names(var) <- b$core$breaks[-1]
  
  list(
    nas=dQuote(b$rcs$nas),
    exc=dQuote(b$rcs$exc),
    var=var)
}