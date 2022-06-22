# ROC and PR with awesome quality
library(PRROC)

# Data and parameters
my_col = '#377EB8'
res_roc = readRDS(file = 'res_roc.rds') # from usefun::get_roc_stats()
res_pr  = readRDS(file = 'res_pr.rds')  # from PRROC::pr.curve()

svg('roc_pr.svg', width = 10, height = 5)
par(mfrow=c(1,2))

# ROC
par(mar = c(5, 5, 4, 4))
plot(x = res_roc$roc_stats$FPR, y = res_roc$roc_stats$TPR, cex.main = 3, cex.axis = 1.5, cex.lab = 1.5,
  type = 'l', lwd = 8, col = my_col, main = 'ROC curve',
  xlab = 'False Positive Rate (FPR)', ylab = 'True Positive Rate (TPR)')
text(0.55, 0.5, "AUC", cex = 6, col = 'grey42')
# legend(x = 0.58, y = 0.157, col = my_col, pch = 19,
#   legend = paste('AUC =', round(res_roc$AUC, digits = 3)), cex = 2) # works only when plotted alone
grid(lwd = 0.5)
abline(a = 0, b = 1, col = 'lightgrey', lty = 'dotdash', lwd = 1.2)

# PR
plot(res_pr, main = 'PR curve', auc.main = FALSE, color = my_col, rand.plot = TRUE,
  cex.main = 3, cex.axis = 1.5, cex.lab = 1.5, lwd = 8)
text(0.45, 0.25, "AUC", cex = 6, col = 'grey42')
# legend(x = 0.58, y = 0.157, col = my_col, pch = 19,
#   legend = paste('AUC =', round(res_pr$auc.davis.goadrich, digits = 3)), cex = 1.5) # works only when plotted alone
grid(lwd = 0.5)

dev.off()
