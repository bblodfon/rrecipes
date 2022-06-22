#' http://www.sthda.com/english/wiki/cox-proportional-hazards-model
library(survival) # for analysis
library(survminer) # for summarizing and visualization
library(dplyr)
library(tibble)

#'##############
# Some info ----
#'##############

# Cox proportional-hazards model (Cox, 1972) is a REGRESSION MODEL

# KM and log-rank are examples of univariate analyses

# Cox's model is multivariate
# Works for both categorical and quantitative variables
# Can assess the effect of multiple predictors (covariates) on the survival outcome

# Hazard function: prob/risk of dying at t
# h(t) = ho(t) * exp(bt), b = vector of covariates coefficients

# HAZARD RATIO: HR = exp(b(i))
# if HR > 1, hazard increases => BAD prognostic factor
# if HR < 1, hazard decreases => GOOD prognostic factor

# Proportional hazards assumptions: h(t)/h'(t) = something with no time
# the hazard curves for the groups should be proportional and cannot cross

#'##############################
# Univariate Cox regression ----
#'##############################

data('lung')
head(lung)
# sex: Male=1 Female=2

res_cox = survival::coxph(Surv(time, status) ~ sex, data = lung, model = TRUE)
res_cox

# Wald Statistic value z + p-value: is sex statistically different than 0 (i.e. important)? YES
# sex_coef = -0.5310 => negative means 2nd group does better than 1st group (females(2) > males(1))
# exp(sex_coef) = 0.58 < 1, hazard decreases when you are in the 2nd group (larger number), being female is a good prognostic! (HR == EFFECT SIZE OF COVARIATES)
# Likelihood ratio => global statistical significance of the model

#'#################################
# Multiple univariate analysis ----
#'#################################

covariates = c("age", "sex",  "ph.karno", "ph.ecog", "wt.loss")
univ_formulas = sapply(covariates, function(x) {
  as.formula(paste('Surv(time, status)~', x))
})

univ_models = lapply(univ_formulas, function(x) {
  survival::coxph(x, data = lung)
})

univ_results = lapply(univ_models, function(x) {
    x = summary(x)
    p.value = signif(x$waldtest["pvalue"], digits = 2)
    wald.test = signif(x$waldtest["test"], digits = 2)
    beta = signif(x$coefficients[1], digits = 2); # coefficient beta
    HR = signif(x$coefficients[2], digits = 2); # exp(beta)
    HR.confint.lower = signif(x$conf.int[,"lower .95"], 2)
    HR.confint.upper = signif(x$conf.int[,"upper .95"], 2)
    #HR = paste0(HR, " (", HR.confint.lower, "-", HR.confint.upper, ")")
    res = tibble::as_tibble_row(list(beta = beta, `HR (95% CI for HR)` = HR, wald.test = wald.test, p.value = p.value))
    return(res)
})

res = dplyr::bind_rows(univ_results) %>%
  mutate(var_names = names(univ_results)) %>%
  relocate(var_names, .before = 1)

#'##########################
# Multivariate analysis ----
#'##########################

fit_cox2 = survival::coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung, model = TRUE)
summary(fit_cox2)
fit_cox2

# Age does not play a role!!!
# Being female (sex=2) reduces the hazard by a factor of 0.58, or 42%. We conclude that, being female is associated with good prognostic
# Higher ph.ecog is worst (bad prognostic)

res_cox3 = survival::coxph(Surv(time, status) ~ sex + ph.ecog + ph.karno, data = lung)

# Plot the baseline survival function (USELESS!)
# `newdata` => default is the covariate mean values - doesn't make sense!
fit = survival::survfit(res_cox3, data = lung)
survminer::ggsurvplot(fit, palette = "#2E9FDF")

# Impact of sex on survival
# make `newdata` with SAME VARIABLES as in Cox's model formula
# continuous variables to average value across all data
# categorical variables to the lowest level across all data
avg_age = lung %>% as_tibble() %>% summarise(avg_age = mean(age)) %>% pull()
min_ph_ecog = min(lung$ph.ecog, na.rm = T)

# REMEMBER: EACH ROW REPRESENTS A PATIENT SAMPLE WITH THESE VARIABLES
newdata = tibble::tibble(sex = c(1,2),
  age = c(avg_age, avg_age),
  ph.ecog = c(min_ph_ecog, min_ph_ecog))

newdata = lung %>%
  as_tibble() %>%
  group_by(sex) %>% # group_var: sex
  summarize(age = mean(age)) %>% # avg age per sex
  tibble::add_column(ph.ecog = c(1,1)) # same value, lower is better for this one (HR=1.58)

# fit for plot
fit = survival::survfit(fit_cox2, newdata = newdata, data = lung)

# plot
survminer::ggsurvplot(fit, conf.int = TRUE,
  legend.labs = c("Sex=1", "Sex=2"),
  ggtheme = theme_minimal())
