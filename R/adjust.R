#' @export
adjust <- function(x, ...) {
  UseMethod("adjust")
}

#' @export
adjust.bin.list <- function(x) {
  out <- x
  i <- 1
  while(i <= length(x)) {
    cat("\014") # clear the console
    print(out[[i]])
    plot(out[[i]])
    # out[[i]]$meta$visited <- TRUE
    cat ("\nEnter command (Q to quit):")
    command <- readLines(n = 1)
    if (command == "Q") {
      break
    }  else if (command %in% c("h", "help")) {
      cat(
"binnr interactive commands:
 (Q)uit
 (n)ext, (N)ext in model
 (p)revious, (P)revious in model
 (g)oto
 (m)ono
 (u)ndo
 (r)eset
 (d)rop
 (a)ssign reason code
 (c)omment
binnr bin operations
 != <#> : Neutralize level
 +  <#> : Expand level
 -  <#> : Collapse level(s)
 <= <#> : Cap at # and rebin\n")
      cat("Press any key to continue")
      readLines(n=1)
      invisible()
    } else if (command == "c") {
      cat("Enter a comment")
      inp <- readLines(n=1)
      invisible()
      if (inp == "DELETE") {
        out[[i]]$notes <- NULL
      } else if (length(grep("\\S+", inp)) > 0) {
        out[[i]]$notes <- paste(out[[i]]$notes, inp, sep="\n -")
      }
    } else if (command == "a") {
      cat("Enter position(optional) and reason code")
      inp <- readLines(n=1)
      invisible()
      type1 <- grep("\\d+\\s+\\S+", inp) # check for matching input
      type2 <- grep("^\\s*\\S+\\s*$", inp) # check for matching input
      if (length(type1) > 0) {
        inp <- strsplit(inp, "\\s+")
        pos <- as.integer(inp[[1]][1])
        aac <- (inp[[1]][2])
        rcs(out[[i]])[pos] <- aac
      } else if (length(type2) > 0) {
        aac <- gsub("\\s", "", inp)
        rcs(out[[i]]) <- aac
      }
    } else if (command == "g") {
      cat("Goto variable:")
      v <- readLines(n = 1)
      # find the position of the variable
      while (!(v %in% c("","Q"))) {
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
      out[[i]]$meta$skip <- !out[[i]]$meta$skip
    } else if (command == "m") {
      cat("Enter Monotonicity:")
      v <- readLines(n = 1)
      out[[i]] <- mono(out[[i]], v)
    } else if (command == "n") {
      i <- i + 1
    } else if (command == "N") {
      nv <- inmodel(out)
      nvi <- which(nv) # index of the in-model
      if (any(nv) & any(nvi > i)) i <- nvi[nvi > i][1]
    } else if (command == "p") {
      if (i > 1) {
        i <- i - 1 
      } else {
        cat("\nAt beginning of list")
      }
    } else if (command == "P") {
      nv <- inmodel(out)
      nvi <- rev(which(nv)) # index of the last in model
      if (any(nv) & any(nvi < i)) i <- nvi[nvi < i][1]
    } else if (command == "u") {
      out[[i]] <- undo(out[[i]])
    } else if (command == "r") {
      out[[i]] <- reset(out[[i]])
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

#' @export
adjust.binnr.model <- function(mod) {
  start <- strptime(date(), "%a %b  %d %T %Y")
  mod$bins <- adjust(mod$bins)
  
  # updated bins that are in the coefficients?
  changed <- sapply(mod$bins, function(x) {
    strptime(x$meta$modified, "%a %b  %d %T %Y") > start
  })
  
  # if bins are adjusted only return a bin.list
  if (any(names(which(changed)) %in% names(mod$coef[-1]))) {
    warning("Coefficient variable modified, must re-fit the model", call. = F)
  }
  mod
}

#' @export
adjust.segmented <- function(obj) {
  # choose which segment?
  cat("\014") # clear the console
  cat ("\nChoose Segment (Q to quit):\n")
  cat(paste(seq_along(obj), names(obj)), sep="\n")
  inp <- readLines(n = 1)
  while (inp != "Q") {
    if (inp == "Q") {
      break
    } else {
      i <- as.integer(inp)
      tryCatch(
        obj[[i]] <- adjust(obj[[i]])
      , error = function(err){
        cat("\nInvalid entry, choose again:")
      })
      cat("\014") # clear the console
      cat ("\nChoose Segment (Q to quit):\n")
      cat(paste(seq_along(obj), names(obj)), sep="\n")
      inp <- readLines(n = 1)
    }
  }
  obj
}


