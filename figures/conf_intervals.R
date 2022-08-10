library(dplyr)
library(tibble)
library(ggplot2)

set.seed(42)

t = c(0.3, 0.5, 0.1)
low_t  = t - runif(3, min = 0, max = 0.1)
high_t = t + runif(3, min = 0, max = 0.1)
res = tibble(name = c('A', 'B', 'C'), t = t, low_ci = low_t, high_ci = high_t)
res

res %>% ggplot(aes(x = name, y = t, color = name)) +
  geom_pointrange(aes(ymin = low_ci, ymax = high_ci)) +
  geom_errorbar(aes(ymin = low_ci, ymax = high_ci), width = 0.1) +
  theme_bw(base_size = 14)
