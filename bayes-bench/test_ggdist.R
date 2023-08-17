library(tidyverse)
library(ggdist)
library(distributional)
library(tidybayes)

# Distribution data ----
dist_df = tibble(
  dist = c(dist_normal(1,0.25), dist_beta(3,3), dist_gamma(5,5)),
  dist_name = format(dist)
)

dist_df %>%
  ggplot(aes(y = dist_name, xdist = dist)) +
  stat_dotsinterval() +
  ggtitle(
    "stat_dotsinterval()",
    "aes(y = dist_name, xdist = dist)"
  )

# Sample data ----
set.seed(42)
df = data.frame(
  group = c("a", "b", "c", "c", "c"),
  value = rnorm(2500, mean = c(5, 7, 9, 9, 9), sd = c(1, 1.5, 1, 1, 1))
)
head(df)

# point + intervals
df %>%
  ggplot(aes(y = group, x = value)) +
  stat_pointinterval(point_interval = 'median_qi',
    .width = c(0.5, 0.9, 0.99)) +
  theme_bw()

# slab (just the distribution)
probs = c(0.9, 0.5)
probs = 1
df %>%
  ggplot(aes(y = group, x = value)) +
  stat_slab(
    aes(fill = after_stat(level)),
    .width = probs
  ) +
  scale_fill_brewer(na.translate = FALSE) + # color match alright here
  theme_bw()

my_cols = c("#A6CEE3", "#1F78B4")
df %>%
  ggplot(aes(y = group, x = value)) +
  stat_slab(
    aes(fill = after_stat(level)),
    .width = probs
  ) +
  scale_fill_manual(
    na.translate = FALSE,
    values = my_cols,
    labels = scales::label_percent()(probs)
  ) +
  theme_bw()

# eyes (not nice)
df %>%
  ggplot(aes(y = group, x = value)) +
  stat_eye()

# half eyes (nicer - best?)
df %>%
  ggplot(aes(y = group, x = value)) +
  stat_halfeye()

# half eyes filled with color (aesthetic)
df %>%
  ggplot(aes(y = group, x = value)) +
  stat_halfeye(
    aes(fill = after_stat(level)),
    .width = c(0.8, 0.95, 0.99)
  ) +
  #' `na.translate` = FALSE drops the unnecessary NA from the legend, which covers
  # slab values outside the intervals. An alternative would be to use
  # na.value = ... to set the color for values outside the intervals.
  scale_fill_brewer(na.translate = FALSE) +
  labs(
    title = "stat_halfeye()",
    subtitle = "aes(fill = after_stat(level))",
    fill = "interval"
  ) +
  theme_bw()

# for an image in a figure
df %>%
  ggplot(aes(y = group, x = value)) +
  stat_halfeye(
    aes(fill = after_stat(level)),
    .width = c(0.8, 0.99),
    show.legend = FALSE
  ) +
  #' `na.translate` = FALSE drops the unnecessary NA from the legend, which covers
  # slab values outside the intervals. An alternative would be to use
  # na.value = ... to set the color for values outside the intervals.
  scale_fill_brewer(na.translate = FALSE) +
  theme_bw(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = 'black'),
    axis.line.y = element_line(color = 'black'),
    axis.line.x.top = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.x.top = element_line(color = 'gray50'),
    axis.ticks.y = element_blank(),
  )
ggsave(filename = 'bayes-bench/distr.svg', dpi = 300)

# investigate not coloring groups when negative values only are given
df %>%
  mutate(value = value - 20) %>%
  ggplot(aes(y = group, x = value, fill = group)) +
  stat_slab(alpha = 0.8, height = 2, show.legend = FALSE) +
  scale_fill_ramp_discrete(from = 'grey80') +
  scale_fill_viridis_d()


