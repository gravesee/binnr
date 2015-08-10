# library(binnr)

titanic <- read.csv('~/Downloads/train.csv', header=T)

x <- titanic$Fare
y <- titanic$Survived

keep <- c('Pclass', 'Sex', 'Age', 'SibSp', 'Parch', 'Fare', 'Embarked')

# createe an as.data 
as.data.frame.bin <- function(x, row.names = NULL, optional = FALSE, ...) {
  zero_ct <- c(x$num_zero, x$except_zero, x$na_zero)
  ones_ct <- c(x$num_ones, x$except_ones, x$na_ones)
  tot_ct  <- zero_ct + ones_ct
  zero_pct <- c(head(zero_ct, -1) / sum(head(zero_ct, -1), na.rm=T), NA)
  ones_pct <- c(head(ones_ct, -1) / sum(head(ones_ct, -1), na.rm=T), NA)
  tot_pct  <- c(head(tot_ct , -1) / sum(head(tot_ct , -1), na.rm=T), NA)
  prob <- c(ones_ct / tot_ct)
  woe  <- log(ones_pct / zero_pct)
  iv   <- woe * (ones_pct - zero_pct)
  
  out <- data.frame(
    "#0"=zero_ct,
    "#1"=ones_ct,
    "W%0"=zero_pct,
    "W%1"=ones_pct,
    "W%"=tot_pct,
    "P(1)"=prob,
    "WoE"=woe,
    "IV"=iv)
  
  colnames(out) <- c('#0', '#1', 'W%0', 'W%1', 'W%' ,'P(1)', 'WoE', 'IV')
  
  if(x$type == "numeric") {
    rnames <- paste(head(x$breaks, -1), x$breaks[-1], sep = " - ")
    rnames <- c(rnames, x$exceptions, "Missing")
    rownames(out) <- rnames[!is.na(rnames)] # filter out NA exceptions
  } else {
    rownames(out) <- c(x$breaks, x$exceptions, "Missing")
  }
  tot.row <- apply(out, 2, sum, na.rm=T)
  tot.row[c("P(1)","WoE")] <- NA
  rbind(out, Total=tot.row)
}

print.bin <- function(x) {
  print(as.data.frame(x))
}

sas <- function(x, ...) {
  UseMethod("sas", x)
}

sas.bin <- function(x, ...) {
  out <- list()
  # missing values
  out[[length(out)+1]] <- sprintf("if missing(");
  
  # exceptions
  print(substitute(x))
  print(...i)
  # ranges
  
}

bins <- bin.data(titanic[, keep], titanic$Survived, mono=c(Age=-1, Fare=1), exceptions = list(Pclass=c(1,3)))

# view the bins and modify them if necessary

#binned <- predict(bins, titanic)

 #bin(x, y)