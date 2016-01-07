#' @export
sas_ <- function(x, coef, pfx, d, mode) {
  UseMethod("sas_", x)
}

#' @export
sas <- function(x, pfx="", mode='max') {
  if(is.null(coef(x))) coef <- rep(1, length(x))
  d <- seq_along(x)
  mode <- match.arg(mode, c('max','neutral'))
  sas_(x, coef, pfx, d, mode)
}

#' @export
sas_.binnr.model <- function(mod, coef, pfx, d, mode) {
  out  <- list()
  v <- names(mod$coef[-1])
  
  # print the variable transforms
  out[[length(out)+1]] <- sas_(mod$bins[v], mod$coef, pfx, seq_along(v), mode)
  
  # print the final model equation
  out[[length(out)+1]] <- .sas.mod.equation(mod$coef, pfx)
  
  unlist(out)
}

#' @export
sas_.bin.list <- function(bins, coef, pfx, d, mode='max') {
  out <- list()
  for(i in seq_along(bins)) {
    b <- bins[[i]]
    out[[i]] <- c(sprintf("\n\n*** Variable: %s ***;\n", b$name),
                  sas_bin(b, coef[b$name], pfx, d[i], mode))
  }
  unlist(out)
}

#' @export
sas_bin_ <- function(x, coef=1, pfx='', d, mode, conds, name) {
  UseMethod("sas_bin_", x)
}

#' @export
sas_bin <- function(b, coef=1, pfx='', d=1, mode='max') {
  mode <- match.arg(mode, c('max','neutral'))
  name  <- b$name
  conds <- conditions(b, pfx, coef)
  name  <- b$name
  sas_bin_(b, coef, pfx, d, mode, conds, name)
}

#' @export
sas_bin_.bin.numeric <- function(b, coef, pfx, d, mode, conds, name) {
  if (is.null(rcs(b))) rcs(b) <- "BL"
  
  rcs <- c(b$rcs$nas, b$rcs$exc, b$rcs$var)
  
  vals <- unlist(rev(b$core$values))
  output.sas(name, conds, vals, coef, rcs, pfx, d, mode)
}

#' @export
sas_bin_.bin.factor <- function(b, coef, pfx, d, mode, conds, name) {
  if (is.null(rcs(b))) rcs(b) <- "BL"
  rcs   <- rcs(b)
  vals <- c(unlist(rev(b$core$values)), 0) # adding a zero to handle unseen lvls
  rcs <- c(unlist(rev(b$rcs)), b$rcs$nas) # add RC for unknowns
  output.sas(name, conds, vals, coef, rcs, pfx, d, mode)
}

conditions <- function(b, pfx='', coef=1) {
  UseMethod("conditions", b)
}

conditions.bin.numeric <- function(b, pfx, coef) {
  exclusions <- names(b$core$values$exc)
  values     <- tail(head(b$core$breaks, -1), -1)
  c(sprintf("if missing(%s)\n  then"  , b$name),
    sprintf("else if %s = %s\n  then" , b$name, exclusions),
    sprintf("else if %s <= %s\n  then", b$name, values),
    sprintf("else"))
}

conditions.bin.factor <- function(b, pfx, coef) {
  #name <- paste0(pfx, name, '_w')
  values <- gsub(",","','", names(b$core$values$var))
  c(sprintf("if missing(%s)\n  then", b$name),
    sprintf("else if %s in ('%s')\n  then", b$name, values),
    sprintf("else"))
}

output.sas <- function(name, conds, vals, coef, rcs, pfx, d, mode) {
  ref <- if (mode == "max") min(vals) else 0
  name <- paste(pfx, name, 'w', sep='_')
  c(sprintf("%s %s = %0.5f;\n", conds, name, vals),
    sprintf("\n  %s_AA_dist_%d = (%0.5f - %s) * %0.5f;\n\n", pfx, d, ref, name, coef),
    sprintf("attrib %s_AA_code_%d length = $5;\n", pfx, d),
    sprintf("if %s = %0.5f\n  then %s_AA_code_%d = ''\nelse ", name, min(vals), pfx, d),
    sprintf("%s %s_AA_code_%d = '%s';\n", conds, pfx, d, rcs)
    
    )
}

.sas.mod.equation <- function(coef, pfx) {
  c(sprintf("\n\n%s_final_score = %s", pfx, coef[1]),
    sprintf("\n    + %s_%s_w * %s", pfx, names(coef[-1]), coef[-1]),
    "\n  ;")
}