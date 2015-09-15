
sas <- function(x, type, sfx, coef) {
  UseMethod("sas", x)
}

# pieces common to both factor and numeric translation
#' @export
sas.bin.numeric <- function(b,  type='woe', sfx='w', coef=1) {
  op <- options("useFancyQuotes")
  options(useFancyQuotes = FALSE)
  p <- pred.list(b, type, coef)
  
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
  
  eyes <- head(seq_along(p$var), -1)
  if (length(eyes) > 0) {
    for (i in  eyes) {
      txt1 <- sprintf("    else if %s <= %s", v, names(p$var)[i])
      txt2 <-
        sprintf("\n        then %s = %s;\n", vs, p$var[i])
      text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
    }
  } else {
    i <- 0
  }
  
  text[[length(text)+1]] <- sprintf("    else %s = %s;\n", vs, p$var[i+1])
  do.call(c, text)
}


# pieces common to both factor and numeric translation
#' @export
sas.bin.factor <- function(b, type="woe", sfx="w",  coef=1) {
  p <- pred.list(b, type, coef)
  
  v <- b$name
  vs <- paste(b$name, sfx, sep='_')
  
  text <- list(sprintf("  * Variable: %s ;\n", v))
  
  # missing text
  txt1 <- sprintf("    if missing(%s)", v)
  txt2 <- sprintf("\n        then %s = %s;\n", vs, p$nas)
  text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
  
  # exceptions text
  for (i in seq_along(p$exc)) {
    txt1 <- sprintf("    else if %s in ('%s')", v, paste(names(p$exc)[i], sep=','))
    text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
  }
  
  for (i in seq_along(p$var)) {
    val <- gsub(",","','", names(p$var)[i])
    txt1 <- sprintf("    else if %s in ('%s')", v, val)
    txt2 <- sprintf("\n        then %s = %s;\n", vs, p$var[i])
    text[[length(text)+1]] <- sprintf("%-50s%s" , txt1, txt2)
  }
  text[[length(text)+1]] <- sprintf("    else %s = %s;\n", vs, 0)
  do.call(c, text)
}

# interface to the SAS function:
pred.list <- function(b, type, coef) {
  UseMethod("pred.list", b)
}

pred.list.bin.numeric <- function(b, type="woe", coef=1) {
  if (type == "woe") {
    nas <- 0
    exc <- b$core$values$exc
    var <- b$core$values$var
    names(var) <- b$core$breaks[-1]
  } else if (type == "dist") {
    exc <- b$core$values$exc
    var <- coef * b$core$values$var
    var <- min(var) - var
    names(var) <- b$core$breaks[-1]
    nas <- min(var) - 0
  } else if (type == "rcs") {
    if (is.null(b$rcs)) rcs(b) <- ""
    var <- sQuote(b$rcs$var)
    names(var) <- b$core$breaks[-1]
    exc <- sQuote(b$rcs$exc)
    nas <- if(length(b$rcs$nas) == 0) "" else b$rcs$nas
    nas <- sQuote(nas)
  }
  list(nas=nas,exc=exc,var=var)
}

pred.list.bin.factor <- function(b, type="woe", coef=1) {
  if (type == "woe") {
    nas <- 0
    exc <- b$core$values$exc
    var <- b$core$values$var
  } else if (type == "dist") {
    exc <- b$core$values$exc
    var <- coef * b$core$values$var
    nas <- min(var) - 0
    var <- min(var) - var
  } else if (type == "rcs") {
    if (is.null(b$rcs)) rcs(b) <- ""
    var <- sQuote(b$rcs$var)
    names(var) <- names(b$core$values$var)
    exc <- sQuote(b$rcs$exc)
    nas <- if(length(b$rcs$nas) == 0) "" else b$rcs$nas
    nas <- sQuote(nas)
  }
  list(nas=nas,exc=exc,var=var)
}


#' @export
sas.bin.list <- function(x, type="woe", sfx="w", coef=NULL) {
  if (type == "dist" & is.null(coef)) {
    warning("Distance calculations requested without model coefficients, using 1",
            call. = F)
    nms <- names(x)
  } else if (type == "dist" & !is.null(coef)) {
    cnm <- names(coef)
    bnm <- names(x)
    nms <- intersect(cnm, bnm)
  } else {
    nms <- names(x)
  }
  
  if (is.null(coef)) {
    coef <- rep(1, length(nms))
    names(coef) <- nms
  }
  out <- list()
  for (i in seq_along(nms)) {
    out[[i]] <- c('\n', sas(x[[i]], type, sfx, coef=coef[nms[i]]))
  }
  do.call(c, out)
}


export.binnr.to.sas <- function(x, coef=NULL) {
  stopifnot(is.bin.list(x))
  if (is.null(coef)) {
    coef <- c(1, rep(1, length(x)))
    names(coef) <- c('(Intercept)', names(x))
  }
  
  coef <- coef[coef != 0]
  
  
  cat("\n ******************************;\n")
  cat(" ***    WoE Substitution    ***;\n")
  cat(" ******************************;\n")
  
  cat(sas(x, type = "woe", sfx = "w"))
  
  cat("\n ******************************;\n")
  cat(" *** Distance Calculations  ***;\n")
  cat(" ******************************;\n")
  
  cat(sas(x, type = "dist", sfx = "d", coef))
  
  cat("\n ******************************;\n")
  cat(" *** Reason Code Assignment ***;\n")
  cat(" ******************************;\n")
  
  cat(sas(x, type = "rcs", sfx = "a"))
  
  cat("\n ******************************;\n")
  cat(" ***  Final Model Equation  ***;\n")
  cat(" ******************************;\n")
  
  ni <- which(!(names(coef) == '(Intercept)'))
  cat(sprintf("\n  final_score = %s", coef['(Intercept)']))
  cat(sprintf("\n    + %32s%s * %s", names(coef[ni]), '_w', coef[ni]))
  cat("\n  ;")
  options(op)
}