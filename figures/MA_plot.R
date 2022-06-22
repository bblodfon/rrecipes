library(affy)
library(affydata)
library(preprocessCore)

# https://en.wikipedia.org/wiki/MA_plot
# M => intensity ratio ('M') - fold-change (log2 diff)
# A => average intensity ('A') - avg log2 expression
# Microarrays typically show a bias here, with higher A resulting in higher |M|, i.e. the brighter the spot the more likely an observed difference between sample and control).

data(Dilution)

# 20B, 10A probes signals
y = exprs(Dilution)[, c(2,3)]

dim(y)
y[1:10,]

# cut it a bit
y = y[1:50000, ]

ma.plot(rowMeans(log2(y)), log2(y[, 1]) - log2(y[, 2]), cex = 1) # adds loess, shows M statistics (median,IQR)
title("Dilutions Dataset (array 20B v 10A)")

# do a quantile normalization
x = preprocessCore::normalize.quantiles(y)

ma.plot(rowMeans(log2(x)), log2(x[, 1]) - log2(x[, 2]), cex = 1)
title("Post Norm: Dilutions Dataset (array 20B v 10A)") # Median difference almost 0 now!!! yay!
