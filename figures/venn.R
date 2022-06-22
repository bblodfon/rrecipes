# Venn diagram
a = c(1:10)
b = c(5:12)
c = c(1,7,12,14,15,16,17,18,19,20)

# supress logger output
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")

# set colors
set1_col = RColorBrewer::brewer.pal(n = 3, name = 'Set1')

VennDiagram::venn.diagram(x = list(a, b, c), disable.logging = TRUE,
  category.names = c("1-10" , "5-12" , "1,7,12,14-20"),
  main = 'Common Elements',
  filename = 'figures/venn.png', output = TRUE, imagetype = "png",
  lty = 'blank', fill = set1_col, cex = 2, margin = 0.1, cat.cex = 1.6)

