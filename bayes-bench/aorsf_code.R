# from the paper: "Accelerated and interpretable oblique random survival forests"
library(tidyverse)
library(rstanarm)

set.seed(42)

# Get bm results ----
res = readRDS(file = 'bayes-bench/result_no_models.rds')
nrsmps = res$boot_res[[1]]$test_nrsmps

# reshape data ----
#' running `stan_glm` with formula that compares to a reference model
df2 = res %>%
  mutate(scores = map(boot_res, 'scores')) %>%
  select(c(task_id, lrn_id, scores)) %>%
  tibble::add_column(id = list(tibble(rsmp_id = 1:nrsmps))) %>%
  tidyr::unnest(cols = c(id, scores)) %>%
  dplyr::relocate(rsmp_id, .after = lrn_id) %>%
  # select the appropriate measures
  select(task_id, lrn_id, rsmp_id, harrell_c, rcll) %>%
  pivot_wider(
    names_from = lrn_id,
    values_from = c(harrell_c, rcll),
    names_sep = '..'
  ) %>%
  mutate(
    across(
      .cols = starts_with('rcll'),
      .fns = ~ .x - rcll..coxph # minus the baseline
    ),
    across(
      .cols = starts_with('harrell_c'),
      .fns = ~ .x - harrell_c..coxph
    )
  ) %>%
  select(-harrell_c..coxph, -rcll..coxph) %>%
  pivot_longer(cols = matches('^rcll|^harrell_c')) %>%
  separate(name, into = c('metric', 'model'), sep = '\\.\\.') #%>%
#mutate(across(where(is.character), as.factor)) # not needed

# subsample for speed
set.seed(42)
df2_sub = df2 %>% filter(rsmp_id %in% sample(x = 1:nrsmps, size = 100))
df2_sub

# run stan model ----
mdl = list(
  harrell_c = stan_glmer(
    data = df2_sub,
    formula = value ~ -1 + model + (1 | task_id/rsmp_id),
    subset = metric == 'harrell_c',
    chains = 4,
    cores = 4,
    iter = 5000,
    seed = 42
  ),
  rcll = stan_glmer(
    data = df2_sub,
    formula = value ~ -1 + model + (1 | task_id/rsmp_id),
    subset = metric == 'rcll',
    chains = 4,
    cores = 4,
    iter = 5000,
    seed = 42
  )
)
#saveRDS(object = mdl, file = 'bayes-bench/mdl.rds')
mdl = readRDS(file = 'bayes-bench/mdl.rds')

# draw from posteriors ----
newdata_harrell_c = distinct(df2_sub, model)
newdata_rcll = newdata_harrell_c
newdata_rcll

data_infer = map2_dfr(
  .x = mdl,
  .y = list(newdata_harrell_c, newdata_rcll),
  .id = 'metric',
  .f = ~ posterior_epred(.x, newdata = .y,  re.form = ~0) |>
    as_tibble() |>
    set_names(.y$model) |>
    pivot_longer(everything(), names_to = 'model') |>
    mutate(model = factor(model),
           model = fct_reorder(model, .x = value, .desc = TRUE))
)
data_infer

draws = as.matrix(mdl$harrell_c)
print(dim(draws)) # 10K draws

post_samples =
  posterior_epred(mdl$harrell_c, newdata = newdata_harrell_c, re.form = NA) %>% # ,draws = 800
  as_tibble() %>%
  set_names(newdata_harrell_c$model) %>%
  pivot_longer(everything(), names_to = 'model', values_to = 'posterior') %>%
  mutate(model = factor(model),
    model = fct_reorder(model, .x = posterior, .desc = TRUE))
post_samples

post_samples %>%
  ggplot(aes(x = posterior)) +
  geom_histogram(bins = 40, col = 'blue', fill = 'blue', alpha = .4) +
  facet_wrap(~ model, ncol = 1) +
  geom_vline(xintercept = 0, col = 'red', linetype = 'dashed') +
  labs(x = 'Harrell\'s C-index', y = 'Posterior distribution',
    title = 'Posterior difference with baseline model (CoxPH)')
