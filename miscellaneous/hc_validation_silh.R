##########################################################
# Silhouette Hierarchical Clustering Validation Analysis #
##########################################################
library(factoextra)
library(FactoMineR)
library(uwot)
library(dplyr)
library(tidyr)
library(tibble)

# make skewed (0,1) matrix
mat = matrix(data = round(rbeta(n = 10000, shape1 = 1, shape2 = 5)), nrow = 100, ncol = 100)

row_dist = dist(x = mat, method = 'euclidean')
k_max = 20

avg_sil_width_com = c()
avg_sil_width_ward = c()
avg_sil_width_com[1] = 0 # for k=1 cluster, it's defined as 0
avg_sil_width_ward[1] = 0
hc_cut_list_com = list()
hc_cut_list_ward = list()
for (k in 2:k_max) {
  # Complete Linkage
  hc_cut_com = factoextra::hcut(x = row_dist, hc_method = "complete", k = k)
  hc_cut_list_com[[k]] = hc_cut_com
  avg_sil_width_com[k] = hc_cut_com$silinfo$avg.width

  # Ward's D2
  hc_cut_ward = factoextra::hcut(x = row_dist, hc_method = "ward.D2", k = k)
  hc_cut_list_ward[[k]] = hc_cut_ward
  avg_sil_width_ward[k] = hc_cut_ward$silinfo$avg.width
}

tbl = tibble::tibble(clusters = as.factor(1:k_max),
  avg_sil_width_com = avg_sil_width_com,
  avg_sil_width_ward = avg_sil_width_ward)

# Compare Ward's Method vs Complete Linkage across various clusters k
tbl %>%
  rename(Ward = avg_sil_width_ward, Complete = avg_sil_width_com) %>%
  tidyr::pivot_longer(cols = c(Complete, Ward), names_to = "method", values_to = "avg_sil_width") %>%
  ggpubr::ggline(x = "clusters", y = "avg_sil_width", group = "method",
    color = "method", ylab = "Average silhouette width", palette = "Set1",
    xlab = "Number of clusters k", main = "Optimal number of clusters") +
    geom_vline(xintercept = which.max(avg_sil_width_ward), linetype = 2, color = "green")

# Silhouette Plot and UMAP 2D Plot for k = 2 clusters (Ward's Method)
k = 2

hc_cut = hc_cut_list_ward[[k]]
factoextra::fviz_silhouette(hc_cut, print.summary = FALSE)

## Execute PCA with `factoMineR`
set.seed(42)
pca_res = FactoMineR::PCA(mat, graph = FALSE)

factoextra::fviz_pca_ind(pca_res, geom.ind = "point",
  col.ind = hc_cut$cluster %>% as.factor(), addEllipses = TRUE,
  legend.title = "cluster", pointshape = 20,
  title = "Row PCA (First 2 dimensions)")
#ggsave(filename = 'pca.png', width = 7, height = 5, units = 'in', dpi = 300)

# Execute UMAP and keep neighbors
set.seed(42)
umap_res = uwot::umap(X = mat, n_neighbors = 15,
  metric = "cosine", ret_nn = TRUE, verbose = TRUE)

umap_res$embedding %>%
  `colnames<-` (c("X", "Y")) %>%
  tibble::as_tibble() %>%
  tibble::add_column(cluster = hc_cut$cluster %>% as.factor()) %>%
  ggplot(aes(x = X, y = Y, color = cluster)) +
  geom_point(size = 0.5) + # shape = '.'
  labs(title = paste0("Cluster Row Map (15 Neighbours)")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(title = "Cluster", label.theme = element_text(size = 12),
        override.aes = list(shape = 19, size = 12)))
