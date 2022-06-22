# Create two network topologies for PhD thesis image
library(igraph)

set.seed(40)
g1 = erdos.renyi.game(n = 200, p.or.m = 0.033, type = "gnp", directed = FALSE)
g2 = sample_pa(n = 100, directed = FALSE, power = 1.3, zero.appeal = 1)

#png(filename = 'graph.png', width = 7, height = 5, units = 'in', res = 300)
pdf(file = 'graph.pdf')
par(mfrow = c(1, 2), mar = c(1, 1, 1, 1))
plot(g1, vertex.label = NA, vertex.size = 5, vertex.color = 'steelblue')
plot(g2, vertex.label = NA, vertex.size = 5, vertex.color = 'steelblue')
dev.off()
