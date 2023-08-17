library(survmob)
library(tidyverse)
library(rstanarm)
library(ggdist)

set.seed(42)

# Benchmark results ----
res = readRDS(file = 'bayes-bench/result_no_models.rds')

# Reshape results
nrsmps = res$boot_res[[1]]$test_nrsmps
df = res %>%
  mutate(scores = map(boot_res, 'scores')) %>%
  select(c(task_id, lrn_id, scores)) %>%
  tibble::add_column(id = list(tibble(rsmp_id = 1:nrsmps))) %>%
  tidyr::unnest(cols = c(id, scores)) %>%
  dplyr::relocate(rsmp_id, .after = lrn_id) %>%
  tidyr::pivot_longer(cols = matches('^rcll|^harrell_c|^uno_c|^ibrier|^dcal'),
    names_to = 'measure', values_to = 'value')
df

# Subset (less rsmps due to bootstrap)
df_sub = df %>% filter(rsmp_id %in% sample(x = 1:nrsmps, size = 100))

# inspect performance stats before running MCMC
msr_id = 'uno_c'
df_sub %>%
  filter(measure == msr_id) %>%
  group_by(lrn_id) %>%
  summarise(average = mean(value), std = sd(value)) %>%
  arrange(desc(average))

# Run Stan model ----
# (SOS) Different model per measure!!!
msr_id = 'uno_c'
stan_res =
  df_sub %>%
  filter(measure == msr_id) %>%
  rstanarm::stan_glmer(
    formula = value ~ -1 + lrn_id + (1 | task_id/rsmp_id),
    #formula = value ~ -1 + lrn_id + (1 | task_id),
    chains = 4,
    cores = 4,
    iter = 2000,
    seed = 42
  )

print(stan_res, digits = 3)
dim(as.matrix(stan_res)) # first is number of posterior draws

saveRDS(stan_res, file = 'bayes-bench/stan_res.rds')
stan_res = readRDS(file = 'bayes-bench/stan_res.rds')

# Visualizations ----
# https://clauswilke.com/dataviz/visualizing-uncertainty.html (nice)

## Model performance ----
### Intervals and point estimates ----
?bayesplot::mcmc_intervals
plot(stan_res, regex_pars = 'lrn', prob = 0.5, prob_outer = 0.9) +
  ggplot2::ggtitle(paste0('Posterior medians with\n 50% and 90% credible intervals')) +
  labs(x = 'Harrell\'s C-index')

### Histograms & density plots ----
?rstanarm::plot.stanreg
# histograms are not nice since they are not stacked!!!
plot(stan_res, plotfun = 'hist', regex_pars = '^lrn') +
  ggplot2::ggtitle(paste0('Posterior Distributions for ', msr_id))

# Areas are the BEST I think
plot(stan_res, plotfun = 'areas', regex_pars = '^lrn', prob = 0.5,
  prob_outer = 0.9) +
  ggplot2::ggtitle(paste0('Posterior Distributions (50% and 90% prob. mass)')) +
  labs(x = 'Harrell\'s C-index')

# Also cool :)
plot(stan_res, plotfun = 'areas_ridges', regex_pars = '^lrn', prob = 0.5,
  prob_outer = 0.9) +
  ggplot2::ggtitle(paste0('Posterior Distributions (50% and 90% prob. mass)')) +
  labs(x = 'Harrell\'s C-index')

## Posterior samples ----
#' need a data.frame with `lrn_id` and the learner ids
nd = distinct(df_sub, lrn_id)
nd
seed = 42
#' can specify `draws` in `posterior_epred`
post_samples =
  rstanarm::posterior_epred(stan_res, newdata = nd, re.form = NA, seed = seed) %>%
  as_tibble() %>%
  rlang::set_names(nd$lrn_id) %>%
  pivot_longer(everything(), names_to = 'model', values_to = 'posterior') %>%
  mutate(
    model = factor(model),
    model = fct_reorder(model, .x = posterior, .desc = TRUE)
  )

### Histogram faceted ----
# doesn't look nice with too many learners
post_samples %>%
  ggplot(aes(x = posterior)) +
  geom_histogram(bins = 40, col = 'blue', fill = 'blue', alpha = .4) +
  facet_wrap(~ model, ncol = 1) +
  geom_vline(xintercept = 0, col = 'red', linetype = 'dashed') +
  labs(x = 'Harrell\'s C-index', y = 'Posterior distribution') +
  xlim(c(0.4, 0.7))

### Credible intervals plot ----
#' like `mcmc_intervals` (GREAT)
#' set `probs` DECREASING (SOS) to match colors later in plots
probs = c(0.8, 0.95)
probs = sort(probs, decreasing = TRUE)

if (length(probs) == 3) {
  my_cols = c('#A6CEE3', '#1F78B4', '#08306B')
} else if (length(probs) == 2) {
  my_cols = c('#A6CEE3', '#1F78B4')
} else if (length(probs) == 1) {
  my_cols = c('#1F78B4')
} else {
  stop('Too many or too few credible intervals')
}

post_samples %>%
  ggplot(aes(y = model, x = posterior)) +
  stat_pointinterval(
    aes(interval_color = after_stat(level)),
    point_interval = 'median_qi',
    point_size = 3,
    point_color = '#D55E00',
    .width = probs
  ) +
  scale_color_manual(
    values = my_cols,
    labels = scales::label_percent()(probs),
    aesthetics = 'interval_color',
    guide = guide_legend(
      override.aes = list(point_color = NA),
      title = 'Credible Intervals'
    )
  ) +
  labs(x = 'Uno\'s C-index', y = '') +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = 'black'),
    axis.line.x.top = element_blank(),
    axis.ticks.x = element_line(color = 'black'),
    axis.ticks.x.top = element_line(color = 'gray50'),
    axis.ticks.y = element_blank(),
    legend.position = 'top'
  )

#' Slab => posterior distributions only
post_samples %>%
  ggplot(aes(y = model, x = posterior)) +
  stat_slab(
    aes(fill = after_stat(level)),
    .width = probs
  ) +
  # adding the following makes it a half_eye plot
  # stat_pointinterval(
  #   .width = probs,
  #   point_interval = 'median_qi',
  #   point_size = 3,
  #   point_color = '#D55E00'
  # ) +
  #scale_fill_brewer() +
  scale_fill_manual(
    values = my_cols,
    labels = scales::label_percent()(probs),
    na.translate = FALSE,
    guide = guide_legend(
      #override.aes = list(point_color = NA, interval_color = NA),
      title = 'Probability mass'
    )
  ) +
  labs(x = 'Uno\'s C-index', y = '') +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = 'black'),
    axis.line.x.top = element_blank(),
    axis.ticks.x = element_line(color = 'black'),
    axis.ticks.x.top = element_line(color = 'gray50'),
    axis.ticks.y = element_blank(),
    legend.position = 'top'
  )

#' Half eye plots (interval + distr)
post_samples %>%
  ggplot(aes(y = model, x = posterior)) +
  stat_halfeye(
    aes(fill = after_stat(level)),
    .width = probs,
    point_size = 3,
    point_interval = 'median_qi',
    point_color = '#D55E00'
  ) +
  # works => brewer colors (blues)
  # scale_fill_brewer(
  #   na.translate = FALSE,
  #   guide = guide_legend(
  #     override.aes = list(point_color = NA)
  #   )
  # ) +
  scale_fill_manual(
    values = my_cols,
    labels = scales::label_percent()(probs),
    na.translate = FALSE,
    guide = guide_legend(
      override.aes = list(point_color = NA),
      title = 'Probability mass'
    )
  ) +
  geom_vline(xintercept = 0.5, linetype = 'dashed', color = 'red') +
  labs(x = 'Uno\'s C-index', y = '') +
  theme_bw(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.border = element_blank(),
    axis.line.x = element_line(color = 'black'),
    axis.line.x.top = element_blank(),
    axis.ticks.x = element_line(color = 'black'),
    axis.ticks.x.top = element_line(color = 'gray50'),
    axis.ticks.y = element_blank(),
    legend.position = 'top'
  )

# SOME BLUES =>
# "#F7FBFF" "#DEEBF7" "#C6DBEF" "#9ECAE1" "#6BAED6" "#4292C6" "#2171B5" "#08519C" "#08306B"

# Model contrasts ----
## Generate Multi-plot ----
#' Using `posterior_samples`

## Tidymodels way ----
#' Largely inefficient as I draw from the posterior of a model multiple times
#' and was following the output of the `tidymodels` model contrasts function
nd = distinct(df_sub, lrn_id)
nd # 7 learners
seed = 42

# Get the unique learner ids somehow
models = nd$lrn_id
#models = as.character(unique(post_samples$model))
model_combos = utils::combn(models, 2)

#' `posterior_epred` provides the posterior distribution of the expected value
#' of the outcome variable, while `posterior_predict` provides the posterior
#' distribution of the outcome variable itself.

# SOS => contrasts are 'model1 - model2'
contrast_list = list()
for (i in 1:ncol(model_combos)) {
  model1 = model_combos[1,i]
  model2 = model_combos[2,i]
  message(model1, ' vs ', model2)

  # no need to redraw from the posterior of every model multiple times!
  nd1 = data.frame(lrn_id = model1) # has to be `lrn_id`
  ps1 = rstanarm::posterior_epred(stan_res, newdata = nd1, re.form = NA, seed = seed)

  nd2 = data.frame(lrn_id = model2) # has to be `lrn_id`
  ps2 = rstanarm::posterior_epred(stan_res, newdata = nd2, re.form = NA, seed = seed)

  contrast_list[[i]] = tibble(
    post_diff = list(ps1[,1] - ps2[,1]),
    model_1 = nd1$lrn_id,
    model_2 = nd2$lrn_id,
    contrast = paste0(nd1$lrn_id, ' vs ', nd2$lrn_id)
  )
}
contrast_tbl = dplyr::bind_rows(contrast_list)
contrast_tbl

## Histogram post diff ----
p = contrast_tbl %>%
  #filter(model_1 == 'coxph', model_2 == 'coxnet') %>%
  filter(contrast == 'coxph vs coxnet') %>%
  tidyr::unnest(cols = 'post_diff') %>%
  ggplot(aes(x = post_diff)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_histogram(bins = 50, color = 'white', fill = 'red', alpha = 0.4) +
  labs(x = 'Mean difference in C-index (CoxPH - CoxNet)',
       y = 'Posterior Probability') +
  theme_bw(base_size = 14)
p

#' Following is not needed is using `ggdist` anymore
#' Add 90% Credible/Uncertainty intervals for the model difference
#' wider intervals => greater uncertainty => higher `prob`
post_inter = contrast_tbl %>%
  filter(contrast == 'coxph vs coxnet') %>%
  unnest(post_diff) %>%
  pull(post_diff) %>%
  matrix(ncol = 1) %>% # need it as a matrix with 1 col
  rstanarm::posterior_interval(prob = 0.9, seed = seed) %>%
  as.numeric()
post_inter

p = p +
  ggplot2::geom_vline(xintercept = post_inter, lty = 1, alpha = .7, col = 'blue')
p

# add ROPE
size = 0.01
if (size != 0) {
  p = p +
    ggplot2::geom_vline(xintercept = c(-size, size), lty = 2, alpha = .5)
}
p

## Summary of posterior differences ----

## Calculate all possible posterior model contrasts
size = 0.01 # practical effect difference size for a chosen measure (e.g. C-index)
prob_mass = 0.9 # desired probability mass for the posterior intervals
seed = 42
ll = list()
for (contrast in unique(contrast_tbl$contrast)) {
  post_diff = contrast_tbl %>%
    filter(contrast == !!contrast) %>%
    unnest(post_diff) %>%
    pull(post_diff)

  post_int = post_diff %>%
    matrix(ncol = 1) %>%
    rstanarm::posterior_interval(prob = prob_mass, seed = seed) %>%
    as.numeric()

  ll[[contrast]] = tibble::tibble(
    contrast = !!contrast,
    posterior_diff = list(post_diff),
    # probability that the positive difference is real
    prob = mean(post_diff > 0, na.rm = T),
    mean = mean(post_diff, na.rm = T),
    # credible interval for the posterior difference
    lower = post_int[1],
    upper = post_int[2],
    # effect size for ROPE (Region of Practical Equivalence)
    size = size,
    # if this is large (e.g. > 0.95) => model1 >> model2
    pract_neg   = mean(post_diff < -size, na.rm = T),
    # if this is large (e.g. > 0.95) => model1 ~ model2
    pract_equiv = mean(post_diff >= -size & post_diff <= size, na.rm = T),
    # if this is large (e.g. > 0.95) => model1 >> model2
    pract_pos   = mean(post_diff > size, na.rm = T)
  )
}
contrast_summary = dplyr::bind_rows(ll)
contrast_summary
