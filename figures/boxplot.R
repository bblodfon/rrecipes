library(tidyverse)

# generate data
vec1 = rnorm(20, mean = 1, sd = 0.2)
vec2 = rnorm(20, mean = 2, sd = 0.2)
vec3 = rnorm(20, mean = 2, sd = 0.5)
res = tibble(vec1 = vec1, vec2 = vec2, vec3 = vec3)

# tidy data
tbl = res %>%
  tidyr::pivot_longer(cols = everything(), names_to = 'name', values_to = 'vals') %>%
  mutate(name = case_when(
    name == 'vec1' ~ 'AA',
    name == 'vec2' ~ 'BB',
    name == 'vec3' ~ 'CC'
  ))
tbl # 2 columns

# calculate p-values
comp = list(c('AA', 'BB'), c('BB', 'CC'))
stat_test = tbl %>%
  rstatix::wilcox_test(formula = vals ~ name, comparisons = comp) %>% # name = group (character)
  rstatix::add_significance('p') %>%
  rstatix::add_y_position()

# boxplot
tbl %>%
  ggplot(aes(x = name, y = vals, color = name)) +
  geom_boxplot(show.legend = FALSE) +
  geom_hline(yintercept = 1.5, linetype = 'dashed', color = 'red') +
  ggpubr::stat_pvalue_manual(stat_test, label = "p = {p} ({p.signif})") +
  labs(x = 'Methods', y = 'Score') +
  theme_bw(base_size = 14) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
