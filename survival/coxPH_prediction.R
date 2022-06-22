library(survival)
library(survminer)
library(dplyr)
library(tibble)

#########################
# Using predict (FAIL?) #
#########################

lung = lung %>% as_tibble()
lung

# 2nd is a better model
fit_cox1 = survival::coxph(Surv(time, status) ~ sex, data = lung, model = TRUE)
fit_cox2 = survival::coxph(Surv(time, status) ~ age + sex + ph.ecog,
  data = lung, model = TRUE)
fit_cox2

# You need all the following for predicting survival (as in the formula above)
# 4 TEST PATIENTS (from the same data)
newdata = lung[c(1,3,13,28),] %>% select(time, status, age, sex, ph.ecog)

pred_lp = predict(fit_cox2, newdata, type = "lp", se.fit = TRUE)
# pred_risk$fit == exp(pred_lp$fit)
pred_risk1 = predict(fit_cox1, newdata, type = "risk", se.fit = TRUE)
pred_risk2 = predict(fit_cox2, newdata, type = "risk", se.fit = TRUE)
# the coefficients per subject fitted
pred_terms = predict(fit_cox2, newdata, type = "terms", se.fit = TRUE)

pred_expected = predict(fit_cox2, newdata, type = "expected", se.fit = TRUE)
# pred_survival$fit == exp(-pred_expected$fit)
pred_survival1 = predict(fit_cox1, newdata, type = "survival", se.fit = TRUE)
pred_survival2 = predict(fit_cox2, newdata, type = "survival", se.fit = TRUE)
# what exactly is that survival prob given above?

data = dplyr::bind_cols(newdata, pred_risk1 = pred_risk1$fit,
  pred_risk2 = pred_risk2$fit, pred_survival1 = pred_survival1$fit,
  pred_survival2 = pred_survival2$fit)

# the 4th subject should be worse than all others, how is survival prob only 0.572?
data

# plot survival curves for the 4 new data points
res1 = survival::survfit(fit_cox1, newdata = newdata)
res2 = survival::survfit(fit_cox2, newdata = newdata)

# only separates sex
survminer::ggsurvplot(res1, conf.int = F, data = lung, ggtheme = theme_minimal())
# better separation (better Cox model, 4th is doing worst)
survminer::ggsurvplot(res2, conf.int = F, data = lung, ggtheme = theme_minimal())

# => survfit calculates correctly the survival probabilities per test subject
dd1 = survminer::surv_summary(res1) %>% as_tibble()
dd2 = survminer::surv_summary(res2) %>% as_tibble()

# Median and Average survival probabilities per patient
# SOS: based on the time frame from the data
dd2 %>% group_by(strata) %>%
  summarise(avg_surv = mean(surv), median_surv = median(surv))

###############################
# Use survfit() AND summary() #
###############################
# https://www.drizopoulos.com/courses/emc/survival%20analysis%20in%20r%20companion
# time and status are not needed now
newdata = lung[c(1,3,13,28),] %>% select(age, sex, ph.ecog)

?survival::survfit.coxph # NOTE: stype = 2, Breslow estimator for survival function, e.g. exp(-BaselineCumHazard)
res1 = survival::survfit(fit_cox1, newdata = newdata)
res2 = survival::survfit(fit_cox2, newdata = newdata)

?print.survfit

?summary.survfit
obj1 = summary(res1, times = c(182, 365, 2*365, 3*365, 4*365, 5*365), extend = TRUE)
obj2 = summary(res2, times = c(182, 365, 2*365, 3*365, 4*365, 5*365), extend = TRUE)

# Get event time (time to death?) prediction (`rmean` or `median`)
obj1$table
obj2$table

# Get predicted survival probabilities for the specific follow-up times per subject
dplyr::bind_cols(time = obj1$time, obj1$surv)
dplyr::bind_cols(time = obj2$time, obj2$surv)
