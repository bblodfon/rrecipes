##############################
# TEST COX MODEL ASSUMPTIONS #
##############################
# http://www.sthda.com/english/wiki/cox-model-assumptions

library(survival)
library(survminer)
library(tibble)

lung = lung %>% as_tibble()
lung

res.cox = survival::coxph(Surv(time, status) ~ age + sex + wt.loss, data =  lung)

# Testing proportional Hazards assumption
# check association between scaled Schoenfeld residuals with time

test.ph = survival::cox.zph(res.cox)
test.ph

# p-values NOT significant (covariates + global test)
# So => ASSUME PROPORTIONAL HAZARDS

# Schoenfeld residuals b(t) against the transformed time for each covariate
# b(t) don't vary a lot with time, they are horizontal
survminer::ggcoxzph(test.ph)

# Dealing with violations (non-proportional hazards, residuals change with time)
# `strata()` in `coxph` => you loss it though
# `var*time` in `coxph` (get warning, pointless covariate)

# Time Dependent Covariates in Cox model
# Read => https://cran.rstudio.com/web/packages/survival/vignettes/timedep.pdf


####################################
# Testing influential observations #
####################################

# none of the observations is terribly influential individually (around 0 everything, nothing stands out too much)
survminer::ggcoxdiagnostics(res.cox, type = "dfbeta",
  ox.scale = 'observation.id', ggtheme = theme_bw())

# Outlier detection
# residuals should be roughly symmetrically distributed about zero with a standard deviation of 1
survminer::ggcoxdiagnostics(res.cox, type = "deviance",
  ox.scale = 'observation.id', ggtheme = theme_bw())

##################################################
# Testing non linearity of continuous covariates #
# CHOOSE FUNCTIONAL FORM OF A CONTINUOUS VARIABLE#
##################################################

# Fitted lines with lowess function should be linear to satisfy cox proportional hazards model assumptions.
res.cox = survival::coxph(Surv(time, status) ~ age + log(age) + sqrt(age), data = lung)
# seems that various forms of age are non-linear
survminer::ggcoxfunctional(res.cox, data = lung)
