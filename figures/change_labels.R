library(ggplot2)

p = iris %>%
  ggplot(aes(x = Species, y = Sepal.Length)) +
  geom_boxplot()
p

# recode group labels on x-axis
p + scale_x_discrete(labels = c('setosa' = 'Se', 'versicolor' = 'Ve',
  'virginica' = 'Vi'))
