library(ggplot2)
library(grid)

#' @export
plot.bin <- function(x, y, ...) {
  # coerce bin into data.frame
  full <- as.data.frame(x)
  f <- !(rownames(full) %in% "Missing") & rownames(full) != "Total"
  plt <- full[f,]
  
  # add columns for plotting
  rng <- rownames(full[f,])
  plt$Range <- factor(rng, levels=rev(unique(rng)))
  plt$WoE[is.nan(plt$WoE) | is.infinite(plt$WoE)] <- 0
  colnames(plt)[6] <- "Prob"
  
  # create "empty" themes for certain elements
  plt.theme <- theme(
    axis.line=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.y=element_blank(),legend.position="none")
  
  g1 <- ggplot(plt, aes(x=Range, y=N)) +
    geom_bar(stat="identity", position="identity") + 
    coord_flip()
  
  g2 <- ggplot(plt, aes_string(x="Range", y="WoE", fill="WoE")) +
    geom_bar(stat="identity", position="identity") +
    scale_fill_gradient(low="blue", high="red") + coord_flip() +
    theme(legend.position="none")
  
  g3 <- ggplot(plt, aes_string(x="Range", y="Prob")) +
    geom_bar(stat="identity", position="identity") +
    geom_hline(yintercept=full["Total","P(1)"], col="red", size=1) +
    coord_flip()
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(4, 1, heights = unit(c(1, 8, 8, 8), "null"))))
  grid.text(x$name, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(g1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(g2, vp = viewport(layout.pos.row = 3, layout.pos.col = 1))
  print(g3, vp = viewport(layout.pos.row = 4, layout.pos.col = 1))
}


