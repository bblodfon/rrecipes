#'################################
# Plot Variable Importance Plots #
#'################################
library(rpart)
library(vip)
library(mlr3verse)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tibble)

# rpart example ----
task = tsk('pima')
learner = lrn('classif.rpart')
learner$train(task)

var_imp = learner$importance() # goodness of split
fit = learner$model
fit$variable.importance # same as `var_imp`

vimp = tibble::enframe(var_imp, name = 'Variable', value = 'Importance')
vimp # tidy format

# `vip` simple point plot (not so much configurable but supports other)
vimp2 = vip::vi(learner$model)
vip::vip(vimp2, geom = 'point', include_type = TRUE, horizontal = TRUE,
  aesthetics = list(size = 3)) +
  xlab('Features') +
  ggpubr::theme_classic2(base_size = 14)

# Cleveland's dot plot (nice)
p = vimp %>% ggpubr::ggdotchart(x = 'Variable', y = 'Importance',
  add = 'segment', dot.size = 5, rotate = TRUE, color = 'Variable',
  xlab = 'Features', sorting = 'ascending')
ggpubr::ggpar(p, legend = 'none')
#ggsave(filename = 'tree_vimp.png', width = 4, height = 4)

# DIY (barplot)
vimp %>%
  mutate(Variable = forcats::fct_reorder(Variable, Importance, .desc = TRUE)) %>%
  ggplot(aes(x = Variable, y = Importance, fill = Variable)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  ggpubr::theme_classic2(base_size = 14) +
  labs(y = 'Importance', x = 'Features') +
  coord_flip()
#ggsave(filename = 'tree_vimp.png', width = 5, height = 5)
