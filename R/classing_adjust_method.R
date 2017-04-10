#' @include classing_class.R
NULL

#' adjust method for Classing objects
#'
#' @name Classing_adjust
#' @param start character vector of length one denoting which variable to start
#' on. Must be an exact match including case.
#' @description The adjust method starts an interactive loop where users may
#' enter commands representing various bin operations. The loop starts with
#' the first variable in the Scorecard object. The interactive session continues
#' until the user submits the quit command or the last variable is navigated
#' past.
#' @details The list of commands is rather extensive and summarized here:
#' \itemize{
#'  \item{(Q)}{uit the interactive session}
#'  \item{(n)}{ext variable in the list}
#'  \item{(p)}{revious variable in the list}
#'  \item{(g)}{oto variable entered by user when prompted}
#'  \item{(m)}{ono - change the monotoncity of the variable}
#'  \item{(e)}{xceptions - change the exception values of the variable}
#'  \item{(s)}{et equal - set one WoE equal to another}
#'  \item{(u)}{ndo the last entered operation}
#'  \item{(r)}{eset the Bin object to its original state}
#'  \item{(d)}{rop or undrop the current variable}
#'  \item{(!= )}{Set requested bin levels to zero}
#'  \item{(+ )}{Expand requested bin level. Only one level allowed.}
#'  \item{(- )}{Collapse requested range of bins. Must be adjacent for
#'  Continuous bins. Can be separate for Discrete Bins}
#' }
#' @return all of the requested operations modify the bin in place
NULL
Classing$methods(adjust = function(start=NULL, ...) {
  if (!is.null(start)) {
    i <- match(start, names(variables))
    if (is.na(i)) {
      stop(sprintf("variable, %s, not found"), start)
    }
  } else {
    i <- 1
  }


  while(i <= length(variables)) {
    nm <- variables[[i]]$name

    cat("\014")
    variables[[i]]$show()

    cat(sprintf("\n [In Model: %5s | Dropped: %5s]",
      nm %in% inmodel, nm %in% dropped), sep = "\n")

    variables[[i]]$plot()
    cat ("\nEnter command (Q to quit; h for help):")
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
        (e)xceptions
        (s)et equal
        (u)ndo
        (r)eset
        (d)rop / undrop
        != <#> : Neutralize level
        +  <#> : Expand level
        -  <#> : Collapse level(s)\n")
      cat("Press any key to continue")
      readLines(n=1)
      invisible()

    } else if (command == "g") {
      cat("Goto variable:")
      v <- readLines(n = 1)
      # find the position of the variable
      while (!(v %in% c("","Q"))) {
        pos <- which(names(variables) == v)[1]
        if (is.na(pos)) {
          # find similar matches
          sim <- agrep(v, names(variables), ignore.case = T, max.distance = 0.1)
          if (length(sim) > 0){
            cat(sprintf("%s not found, similar matches:", v))
            cat(sprintf("\n %2d: %s", seq_along(sim), names(variables)[sim]))
            cat("\nGoto variable:")
            input <- readLines(n = 1)
            n <- suppressWarnings(as.integer(input))
            if (!is.na(n) & n <= length(sim)) { # check if number entered
              v <- names(variables)[sim[n]]
            } else {
              v <- input
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

      ## get current status
      if (nm %in% dropped) {
        undrop(nm)
      } else {
        drop(nm)
      }

    } else if (command == "m") {

      cat("Enter Monotonicity:")
      v <- readLines(n = 1)
      variables[[i]]$mono(v)

    } else if (command == "e") {

      cat("Enter Exceptions:")
      v <- readLines(n = 1)
      e <- eval(parse(text=v))

      if (is.numeric(e) | is.null(e)) {
        variables[[i]]$exceptions(e)
      }

    } else if (command == "s") {

      cat("Enter Level to Change:")
      v1 <- as.integer(readLines(n = 1))
      cat("Change WoE to which level?:")
      v2 <- as.integer(readLines(n = 1))
      variables[[i]]$set_equal(v1, v2)

    } else if (command == "n") {
      i <- i + 1
    } else if (command == "p") {
      if (i > 1) {
        i <- i - 1
      } else {
        cat("\nAt beginning of list")
      }

    } else if (command == "u") {

      variables[[i]]$undo()

    } else if (command == "r") {

      variables[[i]]$reset()

    } else if (command == "c") {

      cat("Enter cut-points separated by spaces:")
      n <- as.numeric(strsplit(readline(), "\\s+")[[1]])
      variables[[i]]$set_cutpoints(n)

    } else {
      tryCatch({
        eval(parse(text=paste("variables[[i]]", command)))
      }, error = function(err) {
        cat("\nInvalid command entered")
      })
    }
  }
})
