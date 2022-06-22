#' Took it from c060, doesn't work as expected: at different times points `basesurv`
#' outputs same values???

###############################################################
# baseline survival/ hazard Breslow estimator
# function essentially based on gbm::basehaz.gbm
###############################################################
basesurv = function(response, lp, times.eval = NULL, centered = FALSE) {
  if (is.null(times.eval)) times.eval <- sort(unique(response[,1]))

  t.unique <- sort(unique(response[,1][response[,2] == 1]))
  alpha    <- length(t.unique)

  for (i in 1:length(t.unique)) {
    alpha[i] <- sum(response[,1][response[,2] == 1] == t.unique[i])/sum(exp(lp[response[,1] >=  t.unique[i]]))
  }

  obj   <- approx(t.unique, cumsum(alpha), yleft=0, xout = times.eval, rule=2)

  if (centered) obj$y <- obj$y * exp(mean(lp))
  obj$z <- exp(-obj$y)

  names(obj) <- c("times","cumBaseHaz","BaseSurv")
  return(obj)
}

#' fit => a fitted model of class `glmnet`
#' ytest => a two-column matrix with columns named 'time' and 'status'. The latter is a binary variable, with '1' indicating death, and '0' indicating right censored. The function Surv() in package survival produces such a matrix
#' xtest => n*p matrix of covariates (test set)
#' times => vector of evaluation time points
#' lambda => penalty value
predictProb.coxnet = function(fit, ytest, xtest, times, lambda) {
  lp = as.numeric(predict(fit, newx = xtest, s = lambda, type = "link")) # linear predictors
  basesurv_res = basesurv(response = ytest, lp, sort(unique(times)))
  p = exp(exp(lp) %*% -t(basesurv_res$cumBaseHaz))

  if (NROW(p) != NROW(xtest) || NCOL(p) != length(times)) {
    stop("Prediction failed")
  }
  p # rows = samples, columns = times

  # return tibble
  p = t(p)
  colnames(p) = 1:ncol(p)
  dplyr::bind_cols(time = times, p)
}
