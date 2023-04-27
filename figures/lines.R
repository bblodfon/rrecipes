library(tidyverse)

tbl = tibble(index = 1:50, a = runif(50), b = runif(50, min = 2, max = 3),
  c = runif(50, min = 5, max = 6))
tbl

set1_colors = RColorBrewer::brewer.pal(n = 3, name = 'Set1')
tbl %>% ggplot(aes(x = index)) +
  geom_line(aes(y = a, color = 'A'), linetype = 'dashed') +
  geom_line(aes(y = b, color = 'B'), linetype = 'dashed') +
  geom_line(aes(y = c, color = 'C')) +
  scale_color_manual(name = '',
    values = c(
      'A' = set1_colors[1],
      'B' = set1_colors[2],
      'C' = set1_colors[3])) +
  geom_hline(yintercept = 1.8, linetype = 'dotted', color = 'red') +
  labs(x = 'Index', y = 'Value') +
  theme_bw(base_size = 14) + theme(legend.position = 'top')

# another way, more complicated
tbl %>% tidyr::pivot_longer(cols = c(everything(), -index),
  values_to = c('value')) %>%
  mutate(name = case_when(
    name == 'a' ~ 'A',
    name == 'b' ~ 'B',
    name == 'c' ~ 'C',
  )) %>%
  ggplot(aes(x = index, y = value, color = name)) +
  #scale_color_brewer(palette = 'Set1') +
  geom_line() +
  geom_hline(yintercept = 1.8, linetype = 'dotted', color = 'red') +
  labs(x = 'Index', y = 'Value') +
  theme_bw(base_size = 14) + theme(legend.position = 'top')


