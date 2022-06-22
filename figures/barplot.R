library(dplyr)
library(ggplot2)

# Frequency (Table Count) Barplot
set.seed(42)
vec = sample(x = LETTERS[c(1,4,8,13,21)], size = 1000, replace =  TRUE)
tbl = vec %>% table() %>% as.data.frame() %>% `colnames<-` (c("Letter", "Freq"))

tbl %>%
  mutate(Letter = forcats::fct_reorder(Letter, Freq, .desc = TRUE)) %>%
  ggplot(aes(x = Letter, y = Freq, fill = Letter)) +
  geom_bar(stat = "identity") + # show.legend = FALSE
  scale_fill_brewer(palette = 'Set1') +
  # geom_text(aes(label = Freq), vjust = -0.3, size = 10) + # to have the numbers above the bars
  # scale_y_continuous(expand = c(0,0)) + # to remove padding between bars and x-axis
  ggpubr::theme_classic2() +
  labs(y = 'Letter Frequency') +
  theme(axis.text.x = element_blank()) # remove x-axis names
ggsave(filename = 'barplot.png', width = 7, height = 5, units = "in", dpi = 300)


