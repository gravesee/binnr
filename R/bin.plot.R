cold.day.palette <- rev(c(
  "#131F2C",
  "#263F5D",
  "#2B688E",
  "#6EA9C3",
  "#B8D2DD"))

cold.day.breaks <- c(-Inf, 100, 500, 1000, 5000, Inf)

#' @export
plot.bin <- function(x, y, ...) {
  # coerce bin into data.frame
  full <- as.data.frame(x)
  f <- !(rownames(full) %in% "Missing") & rownames(full) != "Total"
  plt <- full[f,]
  
  # get max number of chars for left margin spacing
  maxN <- 0.5 + max(sapply(rownames(plt), nchar)) / 3
  
  # add columns for plotting
  rng <- strsplit(rownames(full[f,]), '\\. ')
  rng <- sapply(rng, function(x) { if (length(x) > 1) x[[2]] else "''"})
  WoE <- plt$WoE
  names(WoE) <- rng
  colors <- cold.day.palette[cut(rev(plt$N), cold.day.breaks, labels = F)]
  
  # lots of margin magic
  # http://dr-k-lo.blogspot.com/2014/03/the-simplest-way-to-plot-legend-outside.html
  par(oma = c(2, 1, 1, 1))
  lims <- c(min(WoE) - 0.5, max(WoE) + 0.5)
  par(mar=c(5, maxN, 1, 1))
  bp <- barplot(rev(WoE), horiz=T, las=1, xlim = lims, cex.names = 0.8,
                xlab = "Weight-of-Evidence", col = colors, cex.axis = 0.8)
  
  # add guide
  text(x=min(WoE), y=bp, length(WoE):1, font=2, cex=0.8, pos=2, col="#131F2C")
  
  # overlay legend across entire plot and add to bottom
  title(x$name)
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0.2, 0, 0, 0), new = T)
  plot(0,0, type="n", bty="n", xaxt="n", yaxt="n")
  legend("bottom", c("<100", "<500", "<1000", "<5000", "5000+"), 
         fill = cold.day.palette, horiz=T, inset = c(0,0), xpd=T, cex = 0.75,
         pt.cex = 0.8)
}