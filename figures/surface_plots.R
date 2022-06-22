#'###########################
# Plotting contour surfaces #
#'###########################
library(interp)
library(akima)
library(ggplot2)
library(metR)

set.seed(42)
x = runif(1000)
y = runif(1000, min = 1, max = 4)
z = runif(1000, min = 0, max = 1)

# data frame with 'irregular' data (non-grid)
df = data.frame(x,y,z)

#'########
# interp #
#'########
grid = interp::interp(x = df$x, y = df$y, z = df$z)
griddf = subset(data.frame(x = rep(grid$x, nrow(grid$z)),
  y = rep(grid$y, each = ncol(grid$z)),
  z = as.numeric(grid$z)),
  !is.na(z))

# binned version
griddf %>% ggplot(aes(x, y, z = z)) +
  geom_raster(aes(fill = z), interpolate = FALSE) +
  #scale_fill_continuous(type = "viridis", name = "Measure") +
  scale_fill_distiller(palette = "Spectral", direction = 1, name = "Measure") +
  labs(x = 'X', y = 'Y') +
  theme_classic()

# trick to use the Brewer palettes
scale_fill_brewer_discretised = metR::as.discretised_scale(scale_fill_distiller)

# filled version
griddf %>% ggplot(aes(x, y, z = z)) +
  metR::geom_contour_fill(aes(fill = stat(level))) +
  scale_fill_brewer_discretised(name = 'Measure', palette = 'RdYlBu', direction = 1) +
  #metR::scale_fill_discretised(name = "Measure", low = "red") +
  labs(x = 'X', y = 'Y') +
  # geom_point(size = 0.1) + # doesn't look nice, points are in a grid
  theme_classic()

#'#######
# akima # DON'T USE THIS, DOESN'T LOOK AS GOOD AS USING interp()
#'#######
grid = akima::interp(x = df$x, y = df$y, z = df$z, nx = 500, ny = 500)
grid_df = as.data.frame(akima::interp2xyz(grid))

# binned version
grid_df %>% ggplot(aes(x = x, y = y, fill = z)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_distiller(palette = "Spectral", direction = 1, name = "Measure") +
  labs(x = 'X', y = 'Y') +
  theme_classic()

# filled version
grid_df %>% ggplot(aes(x, y, z = z)) +
  metR::geom_contour_fill(aes(fill = stat(level))) +
  scale_fill_brewer_discretised(name = 'Measure', palette = 'RdYlBu', direction = 1) +
  #metR::scale_fill_discretised(name = "Measure", low = "red") +
  labs(x = 'X', y = 'Y') +
  # geom_point(size = 0.1) + # doesn't look nice, points are in a grid
  theme_classic()
