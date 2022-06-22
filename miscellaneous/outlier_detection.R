# Quantile method
set.seed(42)
x = rnorm(100)
## depends how strict you want to be, below is the strictest interval,
## (0.025, 0.975) is also fine
low_bound = quantile(x, probs = 0.01)
upper_bound = quantile(x, probs = 0.99)

outlier_ids = which(x < low_bound | x > upper_bound)
# the outlier values
x[outlier_ids]
# the rest of the data
x[-outlier_ids]

# Hampel filter (based on median)
## Interval from median(X) +- 3 x mad(x)
low_b = median(x) - 3 * mad(x, constant = 1)
upper_b = median(x) + 3 * mad(x, constant = 1)

outlier_ids2 = which(x < low_b | x > upper_b)
# the outlier values (more than before)
x[outlier_ids2]

# IRQ/boxplot method
q1 = quantile(x, 0.25)
q3 = quantile(x, 0.75)
irq = q3 - q1
low_b2 = q1 - 1.5 * irq
upper_b2 = q3 + 1.5 * irq
outlier_ids3 = which(x < low_b2 | x > upper_b2)
# the outlier values (more than before)
x[outlier_ids3]
# same as:
boxplot.stats(x)$out # 2 outliers

# Z-score method (no outliers detected)
z = abs(x - mean(x))/sd(x)
sum(z>3)

set.seed(42)
x = rnorm(100000)
z = abs(x - mean(x))/sd(x)
sum(z>3) # we got some!
