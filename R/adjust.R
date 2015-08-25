#' @export
adjust <- function(x, vars=NULL, min.iv=0) {
  eval(parse(text = paste(substitute(x), "<<- adjust.bins(x, vars, min.iv)")))
}

#' @export
adjust.bins <- function(x, vars=NULL, min.iv=0) {
  
  if (is.null(vars)) vars <- names(x)
  ivs <- sapply(x, function(x) as.data.frame(x)['Total', 'IV'])
  f <- (names(x) %in% vars) & (ivs >= min.iv)
  idx <- seq_along(names(x))[f]
  
  out <- x
  i <- 1
  while(i <= length(idx)) {
    cat("\014")
    print(out[[idx[i]]])
    plot(out[[idx[i]]])
    cat ("\nEnter command (Q to quit):")
    command <- readLines(n = 1)
    if (command == "Q") {
      break
    }  else if (command %in% c("h", "help")) {
      cat(
"binnr interactive commands:
 (Q)uit
 (n)ext
 (p)revious
 (g)oto
 (u)ndo
 (r)eset
 (d)rop
binnr bin operations
 != <#> : Neutralize level
 + <#>  : Expand level
 - <#>  : Collapse level(s)
 <= <#> : Cap at # and rebin\n")
      cat("Press [Enter] to continue")
      scan(n=1)
    } else if (command == "g") {
      cat("Goto variable:")
      v <- readLines(n = 1)
      # find the position of the variable
      while (v != "Q") {
        pos <- which(vars[f] == v)[1]
        if (is.na(pos)) {
          # find similar matches
          sim <- agrep(v, vars[f], ignore.case = T, max.distance = 0.1)
          if (length(sim) > 0){
            cat(sprintf("%s not found, similar matches:", v))
            cat(sprintf("\n %2d: %s", seq_along(sim), vars[f][sim]))
            cat("\nGoto variable:")
            inp <- readLines(n = 1)
            n <- suppressWarnings(as.integer(inp))
            if (!is.na(n) & n <= length(sim)) { # check if number entered
              v <- vars[f][sim[n]]
            } else {
              v <- inp
            }
          } else {
            cat("No similar variables found")
            cat("\nHit [Enter] to continue")
            readLines(n=1)
            invisible()
            break
          }
        } else { # found exact match
          i <- pos
          break
        }
      }
    } else if (command == "d") {
      out[[idx[i]]]$skip <- !out[[idx[i]]]$skip
      #i <- i + 1
    } else if (command == "n") {
      i <- i + 1
    } else if (command == "p") {
      if (i > 1) {
        i <- i - 1 
      } else {
        cat("\nAt beginning of list")
      }
    } else if (command == "u") {
      out[[idx[i]]] <- undo(out[[idx[i]]])
    } else if (command == "r") {
      out[[idx[i]]] <- eval(parse(text=sprintf("reset(out[[idx[i]]])")))
    } else {
      tryCatch({
        out[[idx[i]]] <- eval(parse(text=paste("out[[idx[i]]]", command)))
      }, error = function(err) {
        cat("\nInvalid command entered")
      })
    }
  }
  out
}