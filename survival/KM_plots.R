# http://www.sthda.com/english/wiki/survival-analysis-basics
library(survival) # for analysis
library(survminer) # for summarizing and visualization
library(dplyr)
library(tibble)

###### SOS ----
# KM and log-rank test are univariate analyses => e.g. focus on 1 categorical predictor (e.g. sex)
# They don't work for many variables/predictors or continuous ones!
#
# S(t) = S(t-1)(1-d(t)/n(t)), d(t) = #events at t, n(t) = #alive people at t
#'###################

#data("lung")
head(lung)

# COLUMNS
# inst: Institution code
# time: Survival time in days
# status: censoring status 1=censored, 2=dead
# age: Age in years
# sex: Male=1 Female=2
# ph.ecog: ECOG performance score (0=good, 5=dead)
# ph.karno: Karnofsky performance score (bad=0, good=100) rated by physician
# pat.karno: Karnofsky performance score as rated by patient
# meal.cal: Calories consumed at meals
# wt.loss: Weight loss in last six months

#'#########################################################
# Surv(time/days, survival_status{binary}) ~ group_var ----
#'#########################################################
fit2 = survival::survfit(formula = Surv(time, status) ~ sex, data = lung)

# `survfit` creates survival curve data from a KM `Surv(time, status` formula or
# a fitted Cox model

# LCL = Lower Confidence Limit, UCL = Upper
print(fit)
summary(fit)
summary(fit)$table

# DYI
# d = tibble::tibble(time = fit$time,
#   n.risk = fit$n.risk,
#   n.event = fit$n.event,
#   n.censor = fit$n.censor,
#   surv = fit$surv,
#   upper = fit$upper,
#   lower = fit$lower)
# head(d)

# Get summary stats nicely
sum_res = survminer::surv_summary(fit) %>% as_tibble()
head(sum_res)
attr(sum_res, 'table')

# Plot survival curves
set1_cols = RColorBrewer::brewer.pal(n = 2, name = 'Set1')
survminer::ggsurvplot(fit,
  #fun = 'cumhaz', # event = 1-y(surv), cumhaz = 1 - log(y(surv)) : number of events that would be expected for each individual by time t
  pval = TRUE, #pval = "The hot p-value is: 0.031",
  pval.method	= TRUE,
  conf.int = TRUE,
  #conf.int.style = "ribbon", # default
  #conf.int.alpha = 0.3, # default
  break.time.by = 100,
  #ncensor.plot = TRUE,
  risk.table = 'percentage', # Add risk table
  risk.table.col = "strata", # Change risk table color by groups
  linetype = "strata", # Change line type by groups
  surv.median.line = "hv", # Specify median survival (v: vertical, h:horizontal)
  ggtheme = theme_bw(), # Change ggplot2 theme
  palette = set1_cols,
  xlim = c(0,600)) # shorten the considered time frame to NOT have huge confidence intervals dues to censoring and thus more certainty in the observed curves

#'######################################
# Compare 2 or more survival curves ----
#'######################################
#'
#' use the non-parametetric log-rank test (~chi-square test statistic)
surv_diff = survival::survdiff(Surv(time, status) ~ sex, data = lung)
surv_diff

#'###########################################
# Fit complex survival curves (Faceting) ----
#'###########################################
head(colon)
fit3 = survival::survfit(Surv(time, status) ~ sex + rx + adhere, data = colon)

print(fit3)
# Note: all variables are categorical
# Treatment groups
colon %>% count(rx)
# Sex
colon %>% count(sex)
# adherence to nearby organs (1 is bad)
colon %>% count(adhere)

ggsurv = survminer::ggsurvplot(fit3, #data = colon,
  #fun = 'pct', # survival
  #fun = "cumhaz", # transforms survival probability to f(y) = -log(y)
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col="strata",
  ggtheme = theme_bw())

# y = rx (treatment groups), x = adhere, so in each subplot you will see the differences between the two sexes for a specific (rx, adhere) combination
curv_facet = ggsurv$plot + facet_grid(rx ~ adhere)
curv_facet
