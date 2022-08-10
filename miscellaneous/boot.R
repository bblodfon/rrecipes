library(ISLR2)
library(boot)
library(dplyr)
library(tibble)

# Estimate accuracy of a statistic of interest ----

Portfolio %>% as_tibble()

alpha_fun = function(data, index) {
  X = data$X[index]
  Y = data$Y[index]
  # a value that minimizes risk (page 209 in ISLR2)
  (var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y))
}


alpha_fun(Portfolio, 1:100) # use all obs
alpha_fun(Portfolio, sample(1:nrow(Portfolio), 23)) # random sample of 23 obs
alpha_fun(Portfolio, sample(x = 1:100, size = 100, replace = TRUE)) # bootstrap 100 obs

port_boot = boot(data = Portfolio, statistic = alpha_fun, R = 1000)
boot.ci(port_boot, type = c("norm", "basic", "perc"))

# Estimating regression coefficients
Auto %>% as_tibble()

coef_fun = function(data, index) {
  coef(lm(mpg ~ horsepower, data = Auto, subset = index))
}

coef_fun(Auto, sample(1:nrow(Auto), 100))
coef_fun(Auto, sample(1:nrow(Auto), 100))

# bootstraps
set.seed(1)
coef_fun(Auto, sample(nrow(Auto), nrow(Auto), replace = T))
coef_fun(Auto, sample(nrow(Auto), nrow(Auto), replace = T))

boot(Auto, coef_fun, 1000)

summary(lm(mpg ~ horsepower, Auto))$coef # compare, bootstrap are better!
