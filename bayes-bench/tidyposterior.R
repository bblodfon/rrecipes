library(survmob)
library(tidyverse)
library(tidymodels)
library(tidyposterior)

# Comparing model performance with linear mixed-effect Bayesian models
# SOS => no multiple datasets, just multiple resamplings

# Data & Models ----
data(two_class_dat, package = 'modeldata')

set.seed(100)
folds = vfold_cv(two_class_dat)

logistic_reg_glm_spec =
  logistic_reg() %>%
  set_engine('glm')

mars_earth_spec =
  mars(prod_degree = 1) %>%
  set_engine('earth') %>%
  set_mode('classification')

rs_ctrl = control_resamples(save_workflow = TRUE)

# Run models ----
logistic_reg_glm_res =
  logistic_reg_glm_spec %>%
  fit_resamples(Class ~ ., resamples = folds, control = rs_ctrl)

mars_earth_res =
  mars_earth_spec %>%
  fit_resamples(Class ~ ., resamples = folds, control = rs_ctrl)

# Collect model results ----
logistic_roc =
  collect_metrics(logistic_reg_glm_res, summarize = FALSE) %>%
  dplyr::filter(.metric == 'roc_auc') %>%
  dplyr::select(id, logistic = .estimate)

mars_roc =
  collect_metrics(mars_earth_res, summarize = FALSE) %>%
  dplyr::filter(.metric == 'roc_auc') %>%
  dplyr::select(id, mars = .estimate)

### quite generic data.frame (SOS) ###
resamples_df = full_join(logistic_roc, mars_roc, by = 'id')
resamples_df

# Another way to view this (but can not be used in later functions):
resamples_df %>%
  pivot_longer(cols = c(logistic, mars), names_to = 'model', values_to = 'AUC')

# Run Stan model ----
set.seed(101)
stan_res2 = perf_mod(
  resamples_df,
  #' `stan_glmer` options
  #' formula = AUC ~ model + (1 | id), # doesn't work
  #' formula = statistic ~ model + (1 | id), # standard random intercept (that's what they transform to inside)
  #' formula = statistic ~ -1 + model + (1 | id), # different from above
  prior_intercept = rstanarm::student_t(df = 1),
  hetero_var = FALSE,
  chains = 4, # The number of independent Markov Chain Monte Carlo analyses to compute.
  iter = 4000, # The total number of Montre Carlo iterations used (including the burn-in samples).
  seed = 42,
  cores = 4, # as the number of chains
  refresh = 1 # logging, set to 0 to have no logging
)
#summary(stan_res2)
print(stan_res2$stan, digits = 3)

# Posterior samples ----
#' `tidy` gets the posterior samples (8000 per model)
tidy(stan_res2) #%>% summary()

## Functions to retrieve posterior ----

#' `x` is a result from `perf_mod`
tidy.perf_mod = function(x, seed = sample.int(10000, 1), ...) {
  post_dat = get_post(x, seed = seed)
  post_dat =
    post_dat %>%
    tidyr::pivot_longer(c(dplyr::everything()),
      names_to = 'model',
      values_to = 'posterior') %>%
    dplyr::mutate(posterior = x$transform$inv(posterior))
  post_dat = as_tibble(post_dat)
  class(post_dat) = c('posterior', class(post_dat))
  post_dat
}

#' `x` is a result from `perf_mod`
get_post = function(x, seed = sample.int(10000, 1)) {
  new_dat = data.frame(model = unique(x$names))
  new_dat =
    as.data.frame(lapply(x$ids, function(x) rep(x[1], nrow(new_dat)))) %>%
    bind_cols(new_dat)
  post_data =
    rstanarm::posterior_epred(
      x$stan,
      newdata = new_dat,
      seed = seed,
      re.form = NA
    )
  post_data = as.data.frame(post_data)
  names(post_data) = x$names
  post_data
}

# Visualize model performance ----

## posterior distributions of the regression parameters ==
## posterior distributions of the performance results
stan_res2 %>%
  tidy() %>%
  ggplot(aes(x = posterior)) +
  geom_histogram(bins = 40, col = 'blue', fill = 'blue', alpha = .4) +
  facet_wrap(~ model, ncol = 1) +
  xlab('Area Under the ROC Curve') # logistic is a little better than MARs
autoplot(stan_res2) # density plots of the posterior

# Model difference(s) ----
## Take posterior samples of performance differences
roc_diff = contrast_models(
  stan_res2,
  #list_1 = 'logistic', # it's: 'list1 - list2'
  #list_2 = 'mars',
  seed = 42
)
roc_diff

# visualize differences with a histogram
roc_diff %>%
  as_tibble() %>%
  ggplot(aes(x = difference)) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_histogram(bins = 50, color = 'white', fill = 'red', alpha = 0.4) +
  labs(y = 'Posterior Probability',
       x = 'Mean difference in ROC-AUC (logistic - mars)')
# autoplot(roc_diff) # same

# statistical significance
# probability of model difference (best: 1, no diff: 0.5 and less)
?summary.posterior_diff
a = summary(roc_diff, prob = 0.9) %>%
  select(-starts_with('pract'))
a

#' posterior = difference
#' probability = mean(posterior > 0)
#' mean = mean(posterior)

# credible interval for the difference of 2 models
a$lower
a$upper # 0 included ?
a$mean

#' practical significance: proportion of posterior that is within [-size, size]
#' look at `pract_equiv`, if for example above `0.95` they are practically equivalent
summary(roc_diff, size = 0.002) %>%
  select(contrast, starts_with('pract'))
autoplot(roc_diff, size = 0.002) # just visual
