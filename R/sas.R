#' @export
sas <- function(x, pfx='', coef, d) {
  UseMethod("sas", x)
}

#' @export
conditions <- function(x, pfx='', coef=NULL) {
  UseMethod("conditions", x)
}

conditions.bin.numeric <- function(b, coef, pfx='') {
  name       <- b$name
  exclusions <- names(b$core$values$exc)
  breaks     <- tail(head(b$core$breaks, -1), -1)
  
  c(sprintf("if missing(%s)\n  then"  , name),
    sprintf("else if %s = %s\n  then" , name, exclusions),
    sprintf("else if %s <= %s\n  then", name, breaks),
    sprintf("else"))
}

sas.bin.numeric <- function(b, coef=1, pfx='', d=1) {
  conds <- conditions(b)
  vals  <- unlist(rev(b$core$values))
  
  if (is.null(rcs(b))) rcs(b) <- "BL"
  rcs <- rcs(b)

  output.sas(conds, vals, coef, rcs, pfx, b, d)
}

conditions.bin.factor <- function(b, coef, pfx='') {
  name   <- b$name
  values <- gsub(",","','", names(b$core$values$var))
  
  c(sprintf("if missing(%s)\n  then", name),
    sprintf("else if %s in ('%s')\n  then", name, values),
    sprintf("else"))
}

sas.bin.factor <- function(b, coef=1, pfx='', d=1) {
  if (is.null(coef)) coef <- 1
  
  conds <- conditions(b)
  vals  <- c(unlist(rev(b$core$values)), 0) # adding a zero to handle unseen lvls
  
  if (is.null(rcs(b))) rcs(b) <- "BL"
  
  rcs   <- c(unlist(rev(b$rcs)), b$rcs$nas)
  
  output.sas(conds, vals, coef, rcs, pfx, b, d)
}

output.sas <- function(conds, vals, coef, rcs, pfx, b, d) {
  name <- sprintf("%s_%s_w", pfx, b$name)
  
  # print the woe sub
  out <- sprintf("%s %s = %0.5f;\n", conds, name, vals)
  
  # AA dist calculation
  out <- c(out, sprintf("\n*** points logic ***\n"))
  out <- c(out, sprintf("  %s_AA_dist_%d = (%0.5f - %s) * %0.5f;\n",
                        pfx, d, min(vals), name, coef))
  
  # RC Assignment
  out <- c(out, sprintf("\n*** RC logic ***\n"))
  out <- c(out, sprintf("%s %s_AA_code_%d = '%s';\n", conds, pfx, d, rcs))

  out
}

#' @export
sas.bin.list <- function(bins, coef=NULL, pfx="", d=1) {
  out <- list()
  for(i in seq_along(bins)) {
    b <- bins[[i]]
    c <- coef[b$name]
    out[[i]] <- c(sprintf("\n\n*** Variable: %s ***;\n", b$name),
                sas(b, c, pfx, d[i]))
  }
  unlist(out)
}


### Put these pieces into helper functions
.sas.sort.logic <- function(pfx) {
sprintf("\n\n*** Reason code sorting logic ***;
drop i, j, tmp_pts, tmp_cd;
do i = 1 to (dim(CDS)-1);
  do j = i to dim(CDS);
    if %1$s_PTS[j] < %1$s_PTS[i] then do;
      tmp_pts = %1$s_PTS[i]; tmp_cd = %1$s_CDS[i];
      %1$s_PTS[i] = %1$s_PTS[j]; %1$s_CDS[i] = %1$s_CDS[j];
      %1$s_PTS[j] = tmp_pts; %1$s_CDS[j] = tmp_cd;
    end;
  end;
end;", pfx)
}

.sas.mod.equation <- function(coef, pfx) {
  c(sprintf("\n\n%s_final_score = %s", pfx, coef[1]),
    sprintf("\n    + %s_%s_w * %s", pfx, names(coef[-1]), coef[-1]),
    "\n  ;")
}

.sas.rc.arrays <- function(rcs, pfx) {
  n  <- length(rcs)
  nc <- max(sapply(rcs, nchar))
  
  pts <- paste(strwrap(
    paste0(pfx, "_RC_", rcs, collapse = " "), width=80), collapse="\n")
  
  cds <- paste(strwrap(paste(rcs, collapse="' '"), width=80), collapse="\n")
  
  c(sprintf("\narray %s_PTS {%d} \n%s\n(0);\n", pfx, n, pts),
    sprintf("\narray %s_CDS {%d} $%d _TEMPORARY_ ('%s');\n", pfx, n, nc, cds))
}

#' @export
sas.binnr.model <- function(mod, pfx="") {
  out  <- list()
  v <- names(mod$coef[-1])
  rcs  <- rcs(mod$bins[v])
  
  # print the variable transforms
  out[[length(out)+1]] <- sas(mod$bins[v], mod$coef, pfx, seq_along(v))
  
  # print the final model equation
  out[[length(out)+1]] <- .sas.mod.equation(mod$coef, pfx)
  
  # sort the reason code arrays
  if (!is.null(rcs)) out[[length(out)+1]] <- .sas.sort.logic(pfx)
  
  unlist(out)
  
}


