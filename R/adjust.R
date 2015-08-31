#' @export
adjust <- function(x) {
  eval(parse(text = paste(substitute(x), "<<- adjust.bins(x)")))
}

adjust.bins <- function(x) {
  out <- x
  i <- 1
  while(i <= length(x)) {
    #cat("\014") # clear the console
    print(out[[i]])
    plot(out[[i]])
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
 (m)ono
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
        pos <- which(names(x) == v)[1]
        if (is.na(pos)) {
          # find similar matches
          sim <- agrep(v, names(x), ignore.case = T, max.distance = 0.1)
          if (length(sim) > 0){
            cat(sprintf("%s not found, similar matches:", v))
            cat(sprintf("\n %2d: %s", seq_along(sim), names(x)[sim]))
            cat("\nGoto variable:")
            inp <- readLines(n = 1)
            n <- suppressWarnings(as.integer(inp))
            if (!is.na(n) & n <= length(sim)) { # check if number entered
              v <- names(x)[sim[n]]
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
      out[[i]]$skip <- !out[[i]]$skip
    } else if (command == "m") {
      cat("Enter Monotonicity:")
      v <- readLines(n = 1)
      out[[i]] <- mono(out[[i]], v)
    } else if (command == "n") {
      i <- i + 1
    } else if (command == "p") {
      if (i > 1) {
        i <- i - 1 
      } else {
        cat("\nAt beginning of list")
      }
    } else if (command == "u") {
      out[[i]] <- undo(out[[i]])
    } else if (command == "r") {
      out[[i]] <- eval(parse(text=sprintf("reset(out[[i]])")))
    } else {
      tryCatch({
        out[[i]] <- eval(parse(text=paste("out[[i]]", command)))
      }, error = function(err) {
        cat("\nInvalid command entered")
      })
    }
  }
  return(out)
}