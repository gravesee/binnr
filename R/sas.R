#' @export
sas <- function(x, pfx, coef=NULL) {
  UseMethod("sas", x)
}

sas.sort.logic <- sprintf("
do i = 1 to (dim(CDS)-1);
    do j = i to dim(CDS);
      if PTS[j] < PTS[i] then do;
      tmp_pts = PTS[i]; tmp_cd = CDS[i];
      PTS[i] = PTS[j]; CDS[i] = CDS[j];
      PTS[j] = tmp_pts; CDS[j] = tmp_cd;
    end;
  end;
end;")

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
  dists <- min(vals) - vals
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
  for(b in bins) {
    cat(sprintf("\n\n*# %s;\n", b$name))
    cat(sas(b, pfx))
  }
}

#' @export
sas.binnr.model <- function(mod, pfx="") {
  # print the reason code sorting logic
  rcs <- rcs(mod$bins[names(mod$coef[-1])])
  
  if (!is.null(rcs)) {
    n <- length(rcs)
    nc <- max(sapply(rcs, nchar))
    rcs2 <- paste(strwrap(paste0(pfx, "_RC_", rcs, collapse = " "), width=80),
                  collapse="\n")
    cat(sprintf("\narray PTS {%d} \n%s\n(%d*0);\n", n, rcs2, n))
    
    # set up the reason code arrays
    rcs1 <- paste(strwrap(paste(rcs, collapse="' '"), width=80), collapse="\n")
    cat(sprintf("\narray CDS {%d} $%d _TEMPORARY_ ('%s');\n", n, nc, rcs1))
  }
  
  sas(mod$bins[names(mod$coef[-1])], pfx)
  
  # print the final model equation
  cat(sprintf("\n\n%s_final_score = %s", pfx, mod$coef[1]))
  cat(sprintf("\n    + %s_%s_w * %s", pfx, names(mod$coef[-1]), mod$coef[-1]))
  cat("\n  ;")
  
  if (!is.null(rcs)) {
    # set up the points array
    cat(sas.sort.logic)
  }
  # print the bins
  
}


