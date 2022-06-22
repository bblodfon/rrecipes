#########################################
# Benchmark ways to create 0-1 matrices #
#########################################
library(rbenchmark)
library(dplyr)

# 4 ways to generate a matrix of 0-1 with uniform probability:
mat1 = matrix(data = round(runif(100, min = 0, max = 1)), nrow = 10, ncol = 10)
mat2 = matrix(data = round(rbeta(n = 100, shape1 = 5, shape2 = 5)), nrow = 10, ncol = 10)
mat3 = matrix(data = rbinom(n = 100, size = 1, prob = 0.5), ncol = 10, nrow = 10)
mat4 = matrix(data = sample(x = c(0,1), size = 100, replace = TRUE), nrow = 10, ncol = 10)

## Extra: skewed 0-1 matrices with beta distribution
matrix(data = round(rbeta(n = 100, shape1 = 1, shape2 = 5)), nrow = 10, ncol = 10) %>%
  density(adjust = 0.3) %>% plot()
matrix(data = round(rbeta(n = 100, shape1 = 5, shape2 = 1)), nrow = 10, ncol = 10) %>%
  density(adjust = 0.3) %>% plot()

rbenchmark::benchmark(
  "round-runif" = { matrix(data = round(runif(10000, min = 0, max = 1)), nrow = 100, ncol = 100) },
  "round-rbeta" = { matrix(data = round(rbeta(n = 10000, shape1 = 5, shape2 = 5)), nrow = 100, ncol = 100) },
  "rbinom"      = { matrix(data = rbinom(n = 10000, size = 1, prob = 0.5), ncol = 100, nrow = 100) },
  "sample"      = { matrix(data = sample(x = c(0,1), size = 10000, replace = TRUE), nrow = 100, ncol = 100) },
  replications = 1000,
  columns = c("test", "replications", "elapsed", "relative", "user.self", "sys.self")
)

# sample() is the fastest!!!
