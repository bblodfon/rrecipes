library(dplyr)
library(tibble)
library(stringr)
library(usefun)
library(uwot)
library(ggplot2)

# make train data
n_bits = 10
numbers = 0:(2^n_bits-1)
res = lapply(numbers, function(n) {
  bin_n = usefun::dec_to_bin(decimal_num = n, bits = n_bits)
  unlist(stringr::str_split(string = bin_n, pattern = "")) %>% as.integer()
})

mat = t(sapply(res, unlist))
colnames(mat) = paste0("V", 1:ncol(mat))

# use umap
set.seed(0)
mat_umap = uwot::umap(X = mat, metric = "manhattan", n_threads = 4)

colnames(mat_umap) = c('X','Y')
data = mat_umap %>%
  as_tibble() %>%
  mutate(labels = numbers)

  mutate(labels = factor(c(rep(0, length(numbers)/4), rep(1, length(numbers)/4),
    rep(2, length(numbers)/4), rep(3, length(numbers)/4))))

ggplot(data) +
  geom_point(aes(x = X, y = Y, color = labels), size = 1) +
  theme_classic() +
  scale_color_distiller(palette = "Spectral", guide = guide_colourbar(title = "Labels"))
  #guides(colour = guide_legend(title = "Labels", )
