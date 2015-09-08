library(binnr)
data(titanic)
bins <- bin(titanic[,-1], titanic[,1])



#' @export
newplot <- function(x, y, ...) {
# coerce bin into data.frame
full <- as.data.frame(x)
f <- !(rownames(full) %in% "Missing") & rownames(full) != "Total"
plt <- full[f,]

# get max number of chars

maxN <- 1 + max(sapply(rownames(plt), nchar)) / 3

# add columns for plotting
rng <- rownames(full[f,])
plt$WoE[is.nan(plt$WoE) | is.infinite(plt$WoE)] <- 0

plt

WoE <- plt$WoE
names(WoE) <- rng
#names(WoE) <- factor(rng, levels=rev(unique(rng)))

par(mar=c(5,maxN,2,2))
a <- min(plt$WoE) - 0.5
z <- max(plt$WoE) + 0.5
barplot(rev(WoE), horiz=T, las=1, xlim = c(a, z), cex.names = 0.80, xlab = "Weight-of-Evidence")
title(x$name)
#axis(2, at=seq_along(rng), labels=rng)


}
  
  