library(snedata) # remotes::install_github("jlmelville/snedata")
library(uwot)
library(ggplot2)
library(dplyr)
library(tibble)

# Get mnist dataset
mnist = snedata::download_mnist()

# Run umap
mnist_umap = uwot::umap(X = mnist, n_neighbors = 15, min_dist = 0.001, verbose = TRUE)

saveRDS(object = mnist_umap, file = '~/tmp/r-tests/mnist_umap.rds')

# Plot
mnist_umap %>%
  `colnames<-` (c("X", "Y")) %>%
  tibble::as_tibble() %>%
  tibble::add_column(label = mnist$Label) %>%
  ggplot(aes(x = X, y = Y, colour = label)) +
  geom_point(shape = '.') +
  scale_color_brewer(palette = "Paired") +
  guides(colour = guide_legend(title = "Digit", label.theme = element_text(size = 12),
    override.aes = list(shape = 19, size = 10))) +
  labs(title = "MNIST UMAP") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "mnist.png", dpi = "print", width = 7, height = 5)

# Supervised dimension reduction
mnist_sumap = uwot::umap(X = mnist, y = mnist$Label, n_neighbors = 15, min_dist = 0.001, verbose = TRUE)

saveRDS(object = mnist_sumap, file = '~/tmp/r-tests/mnist_sumap.rds')

# Plot
mnist_sumap %>%
  `colnames<-` (c("X", "Y")) %>%
  tibble::as_tibble() %>%
  tibble::add_column(label = mnist$Label) %>%
  ggplot(aes(x = X, y = Y, colour = label)) +
  geom_point(shape = '.') +
  scale_color_brewer(palette = "Paired") +
  guides(colour = guide_legend(title = "Digit", label.theme = element_text(size = 12),
    override.aes = list(shape = 19, size = 10))) +
  labs(title = "MNIST Supervised UMAP") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = "mnist_sup.png", dpi = "print", width = 7, height = 5)
