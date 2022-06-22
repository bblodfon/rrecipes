# Compare Boolean distributions

n1 = 100
n2 = 1000

set.seed(42)

x1 = sample(x = c(0,1), size = n1, replace = TRUE)
x2 = sample(x = c(0,1), size = n2, replace = TRUE)

x1 = ifelse(rbeta(n = n1,5,2) > 0.5, 1, 0) # mostly 0's
mean(x1)

x2 = ifelse(rbeta(n = n2,5,2) > 0.5, 1, 0) # mostly 1's
mean(x2)

# probability that the next model number is active (1)?
ppois(1, mean(x1)) - ppois(0, mean(x1))
dpois(1, mean(x1)) # same

ppois(1, mean(x2)) - ppois(0, mean(x2))
dpois(1, mean(x2)) # same

# https://userweb.ucs.louisiana.edu/~kxk4695/statcalc/pois2pval.for

# Exact Poisson test for the ratio between the two lambdas, e.g. l1 = sum(x1)/n1
poisson.test(x = c(n1,n2), T = c(sum(x1), sum(x2)))
# returns rate ratio as: l2/l1 = mean(x2)/mean(x1)

#' https://pypi.org/project/poisson-etest/ => python implementation of the paper:
#' https://www.sciencedirect.com/science/article/abs/pii/S0378375802004081
#' A more powerful test for comparing two Poisson means

