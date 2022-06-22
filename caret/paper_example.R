library(caret)
library(dplyr)
library(tibble)

#'#####################
# Data preparation ----
#'#####################

# `descr` and `mutagen` data
load(file = 'caret/descr.RData')
load(file = 'caret/mutagen.RData')

descr = descr %>% as_tibble()

# check #samples the same
stopifnot(nrow(descr) == length(mutagen))

# 75% training data (25% for testing) => get row position INDEXES
set.seed(1)
inTrain = caret::createDataPartition(mutagen, p = 0.75, list = FALSE, times = 1)

trainDescr = descr[inTrain,]
testDescr  = descr[-inTrain,]

# Class outputs
trainClass = mutagen[inTrain]
testClass  = mutagen[-inTrain]

# some sanity checks
stopifnot(nrow(trainDescr) + nrow(testDescr) == nrow(descr))
stopifnot(length(trainClass) + length(testClass) == length(mutagen))
stopifnot(ncol(trainDescr) == ncol(descr))
stopifnot(ncol(testDescr) == ncol(descr))

# DATASET IS STRATIFIED
prop.table(table(mutagen))
prop.table(table(trainClass))
prop.table(table(testClass))

## Remove predictors with one distinct value
isZV = apply(trainDescr, 2, function(u) length(unique(u)) == 1)
# 1 predictor has 1 unique value (zero variance)
sum(isZV)
which(isZV)
trainDescr = trainDescr[, !isZV]
testDescr  =  testDescr[, !isZV]

## `nearZeroVar` => more elaborate function for removing near zero-variance predictors (see paper)
nzv_data = caret::nearZeroVar(x = descr, saveMetrics = TRUE)
nzv_indexes = caret::nearZeroVar(x = descr, saveMetrics = FALSE) # just get the indexes
stopifnot(sum(nzv_data$nzv) == length(nzv_indexes))

## Deal with MULTI-COLLINEARITY
## Remove columns to reduce pair-wise correlations between high correlated predictors
cor_mat = cor(trainDescr)
high_corr_indexes = findCorrelation(cor_mat, 0.9)

# 923 predictors are highly correlated with others
length(high_corr_indexes)

trainDescr = trainDescr[, -high_corr_indexes]
testDescr  =  testDescr[, -high_corr_indexes]

# 655 features remain!
ncol(trainDescr)

## PREPROCESS DATA (SCALING, CENTERING, REMOVE ZV/NZV predictors, REMOVE HIGH-CORRELATED predictors)
xTrans = caret::preProcess(trainDescr, verbose = T)
xTrans
# use `predict` to apply the same pre-processing to other samples
trainDescr = predict(xTrans, trainDescr)
testDescr  = predict(xTrans,  testDescr)

#'#########################################
## ALL DATA PREPARATION STEPS TOGETHER ----
#'#########################################
if (FALSE) {
  dim(trainDescr) # 1579 features

  xTrans = caret::preProcess(trainDescr, method = c("center", "scale", "zv", "corr"), verbose = T)

  trainDescr = predict(xTrans, trainDescr)
  testDescr  = predict(xTrans, testDescr)

  # 655 features remain!
  ncol(trainDescr)
  ncol(testDescr)
  stopifnot(all(colnames(testDescr) == colnames(trainDescr))) # :)
}

#'###############################
# Building and tuning models ----
#'###############################

# Control parameters for train (TRAIN CONFIG)
bootControl = caret::trainControl(
  method = "boot", # re-sampling with replacement (picks n samples, ~63% are in the initial sample)
  number = 2, # how many re-samples to do! (default is 25 bootstraps)
  search = "grid",
  classProbs = TRUE, savePredictions = TRUE, # for ROC via `evalm()`
  verboseIter = TRUE) # to see some output when running `train`

# train
set.seed(42)
svmFit = caret::train(
  x = trainDescr,
  y = trainClass,
  method = "svmRadial", # algorithm to use (SVM)
  tuneLength = 5, # one parameter to tune across 5 values
  trControl = bootControl,
  scaled = FALSE) # passed to `ksvm` function to avoid pre-processing duplication

svmFit
svmFit$finalModel

# Get some curves (ROC, PR)
library(MLeval)
plots = MLeval::evalm(svmFit)

# find parameters to tune and make the grid yourself
svm_model_info = getModelInfo(model = 'svmRadial')
svm_model_info$svmRadial$parameters['parameter']
svm_grid = expand.grid(sigma = 0.001, C = c(0.1, 1, 10, 100))

svmFit2 = caret::train(
  x = trainDescr,
  y = trainClass,
  method = "svmRadial", # algorithm to use
  tuneGrid = svm_grid, # your own parameter grid
  trControl = bootControl, # train config
  scaled = FALSE)

svmFit2
svmFit2$finalModel

# Stochastic Gradient Boosting (SGB)
gbm_model_info = getModelInfo(model = 'gbm')
gbm_model_info$gbm$parameters['parameter']

# make a Stochastic Gradient Boosting parameter grid
gbmGrid = expand.grid(interaction.depth = (1:3) * 2, n.trees = c(25,100,250), shrinkage = 0.1, n.minobsinnode = 5)

set.seed(42)
gbmFit = train(x = trainDescr, y = trainClass, method = "gbm", trControl = bootControl,
  verbose = FALSE, bag.fraction = 0.5, tuneGrid = gbmGrid)

#'#######################
# Plot train objects ----
#'#######################
#' plot.train()
plot(gbmFit, metric = 'Accuracy') # gbmGrid parameters are there
plot(gbmFit, metric = 'Kappa')
plot(gbmFit, metric = 'Kappa', plotType = "level") # nice!!!
caret::resampleHist(gbmFit, type = 'density') # density plots of the accuracy values (bootstrapped estimates, the more the better)

#'##############################
# Prediction of new samples ----
#'##############################
#' extractPrediction() vs extractProb()

# Use predict() for INDIVIDUAL MODELS
class(svmFit) # train
# wrapper predict function for each separate package, best fit models parameters used
?caret::predict.train()

# Get vector of class predictions for an INDIVIDUAL MODEL
res = predict(svmFit, newdata = testDescr)
# type = 'raw' default (type = 'prob' doesn't work with all models)

sum(res == testClass)/length(res) # 84.8% accuracy

# MANY models (list of vector classes prediction results)
models = list(svm1 = svmFit, svm2 = svmFit2, gbm = gbmFit)

testPred = predict(models, newdata = testDescr)
pred_acc = lapply(testPred, function(model_pred) {
  sum(model_pred == testClass)/length(model_pred)
})

# MANY models (data frame format) - extractPrediction() or extractProb()
predValues = extractPrediction(models, testX = testDescr, testY = testClass) %>%
  tibble::as_tibble() %>%
  filter(dataType == 'Test')

predValues %>% count(object) # object => model
nrow(testDescr)

# Test and Training data
plotObsVsPred(extractPrediction(models[1], testX = testDescr, testY = testClass))

# doesn't work for all models (use only with gbm)
probValues = extractProb(models[3], testX = testDescr, testY = testClass) %>%
  as_tibble() %>%
  filter(dataType == 'Test')
plotClassProbs(object = extractProb(models[3], testX = testDescr, testY = testClass))
plotClassProbs(object = probValues, plotType = 'densityPlot')

# same as getting with predict since its one model
res_prob = predict(gbmFit, newdata = tfestDescr, type = 'prob') %>% as_tibble()
stopifnot(probValues$mutagen == res_prob$mutagen)
stopifnot(probValues$nonmutagen == res_prob$nonmutagen)

# plot ROC
df = probValues %>% select(mutagen, obs) %>% mutate(label = case_when(obs == 'mutagen' ~ 1, TRUE ~ 0))

res = usefun::get_roc_stats(df, pred_col = 'mutagen', label_col = 'label', direction = '>')

# Plot ROC with a legend showing the AUC value
plot(x = res$roc_stats$FPR, y = res$roc_stats$TPR,
  type = 'l', lwd = 2, col = '#377EB8', main = 'ROC curve',
  xlab = 'False Positive Rate (FPR)', ylab = 'True Positive Rate (TPR)')
legend('bottomright', legend = round(res$AUC, digits = 3),
  title = 'AUC', col = '#377EB8', pch = 19)
grid()
abline(a = 0, b = 1, col = '#FF726F', lty = 2)

# plot PR
pr_res = PRROC::pr.curve(scores.class0 = df %>% pull(mutagen), weights.class0 = df %>% pull(label), curve = TRUE)

plot(pr_res, main = 'PR curve', auc.main = TRUE, color = 'blue', rand.plot = TRUE,
  cex.main = 2, cex.axis = 1.5, cex.lab = 1.5, lwd = 8)

#' ##############################
# Characterizing performance ----
#' ##############################

svm1_pred = predValues %>% filter(object == 'svm1')
gbm_pred  = predValues %>% filter(object == 'gbm')
perf_res_svm = caret::confusionMatrix(data = svm1_pred$pred, reference = svm1_pred$obs)
perf_res_gbm = caret::confusionMatrix(data = svm1_pred$pred, reference = svm1_pred$obs)

perf_res_svm$overall
perf_res_svm$byClass

pred_res = predValues %>%
  group_split(object)

model_names = sapply(pred_res, function(res) {
  res %>% distinct(object) %>% pull() %>% as.character()
})

names(pred_res) = model_names

perf_res = lapply(pred_res, function(pred_res) {
  caret::confusionMatrix(data = pred_res$pred, reference = pred_res$obs,
    mode = 'everything') # includes: Sen,Spec,Precision,Recall,F1...
})

names(perf_res) = model_names
perf_res$svm1

# some basic functions to get performance metrics
defaultSummary(data = pred_res$gbm) # Kappa, Accuracy
postResample(pred = pred_res$gbm$pred, obs = pred_res$gbm$obs) # same as above

#' ########################
#  Variable importance ----
#' ########################

# MODEL-SPECIFIC importance
library(gbm) # gbm HAS AN INTERNAL METHOD FOR CALCULATING VARIABLE IMPORTANCE
gbmImp = caret::varImp(gbmFit, scale = TRUE) # SCALE: 0 - 100 (x - min)/(max - min)
gbmImp # "varImp.train" class
plot(gbmImp, top = 20)
ggplot2::ggplot(gbmImp, top = 20)

# MODEL-AGNOSTIC
roc_imp = caret::filterVarImp(x = testDescr, y = testClass)
head(roc_imp, n = 20) # variable importance PER CLASS

