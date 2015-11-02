#' @export
sas <- function(x, pfx, coef=NULL) {
  UseMethod("sas", x)
}

sas.bin.numeric <- function(b, pfx='') {
  conds <- c(
    sprintf("if missing(%s) then", b$name),
    sprintf("else if %s = %s then", b$name, names(b$core$values$exc)),
    sprintf("else if %s <= %s then", b$name, tail(head(b$core$breaks, -1), -1)),
    sprintf("else"))
  
  vals  <- unlist(rev(b$core$values))
  dists <- min(vals) - vals
  rcs   <- unlist(rev(b$rcs))
  
  output.sas(conds, vals, dists, rcs, pfx, b)
}

sas.bin.factor <- function(b, pfx='') {
  
  v <- gsub(",","','", names(b$core$values$var))
  conds <- c(
    sprintf("if missing(%s) then", b$name),
    sprintf("else if %s in ('%s') then", b$name, v),
    sprintf("else"))
  
  vals  <- c(unlist(rev(b$core$values)), 0) # adding a zero to handle unseen lvls
  dists <- min(vals) - vals # TODO: put code in here for choosing max/min/etc...
  rcs   <- c(unlist(rev(b$rcs)), b$rcs$nas)
  
  output.sas(conds, vals, dists, rcs, pfx, b)
}

output.sas <- function(conds, vals, dists, rcs, pfx, b) {
  # print them all together
  if (!is.null(rcs)) {
    sprintf("%s do;\n  %s_%s_w = %s;\n  %s_RC_%s + %s;\nend;",
            conds, pfx, b$name, vals, pfx, rcs, dists)
  } else {
    sprintf("%s do;\n  %s_%s_w = %s;\nend;",
            conds, pfx, b$name, vals)
  }
}


#' @export
sas.bin.list <- function(bins, pfx="") {
  out <- list()
  for(i in seq_along(bins)) {
    b <- bins[[i]]
    out[[i]] <- c(sprintf("\n\n*** Variable: %s ***;\n", b$name), sas(b, pfx))
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
  vars <- names(mod$coef[-1])
  rcs  <- rcs(mod$bins[vars])
  
  # set up the reason code arrays
  if (!is.null(rcs)) out[[length(out)+1]] <- .sas.rc.arrays(rcs, pfx)
  
  # print the variable transforms
  out[[length(out)+1]] <- sas(mod$bins[vars], pfx)
  
  # print the final model equation
  out[[length(out)+1]] <- .sas.mod.equation(mod$coef, pfx)
  
  # sort the reason code arrays
  if (!is.null(rcs)) out[[length(out)+1]] <- .sas.sort.logic(pfx)
  
  unlist(out)
  
}


