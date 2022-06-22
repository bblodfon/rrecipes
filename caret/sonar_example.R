library(caret)
library(mlbench)

data(Sonar)
Sonar = Sonar %>% as_tibble()

# Data split ----
set.seed(107)
inTrain = createDataPartition(
  y = Sonar$Class, ## the outcome data are needed
  p = 0.75, ## The percentage of data in the training set
  list = FALSE
)

length(inTrain)/nrow(Sonar) # 75%

training = Sonar[ inTrain,]
testing  = Sonar[-inTrain,]

nrow(training)
nrow(testing)

# Train PLSDA model ----
plsFit = train(
  Class ~ .,
  data = training,
  method = "pls",
  ## Center and scale the predictors for the training set and all future samples.
  preProc = c("center", "scale")
)

## TrainControl ----
# By default, the function will tune over 3 values of each tuning parameter (we have 1 here)
plsFit$modelInfo$parameters
plsFit

plsFit = train(
  Class ~ .,
  data = training,
  method = "pls",
  preProc = c("center", "scale"),
  ## added:
  tuneLength = 15 # 1 to 15 now
)
plsFit

# By default, 25 bootstraps is used
plsFit$control$method
plsFit$control$number

# change resampling: Cross-Validated (5 fold, repeated 3 times)
ctrl = trainControl(method = "repeatedcv", number = 5, repeats = 3,
  classProbs = TRUE, summaryFunction = twoClassSummary) # for ROC stats

plsFit = train(
  Class ~ .,
  data = training,
  method = "pls",
  preProc = c("center", "scale"),
  tuneLength = 15,
  ## added:
  trControl = ctrl,
  metric = "ROC"
)

# visualize resampled performance values and the number of PLS components
ggplot(plsFit)

## Predict ----
plsClasses = predict(plsFit, newdata = testing)
plsProbs   = predict(plsFit, newdata = testing, type = "prob")

# data => predicted classes
# reference => true classes
perf = caret::confusionMatrix(data = plsClasses, reference = testing$Class, mode = 'everything')
perf

# Train RDA model ----
# Regularized Discriminant Analysis

rdaGrid = data.frame(gamma = (0:4)/4, lambda = 3/4)
set.seed(123)
rdaFit = train(
  Class ~ .,
  data = training,
  method = "rda",
  tuneGrid = rdaGrid,
  trControl = ctrl,
  metric = "ROC"
)

rdaFit

rdaClasses = predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)

# PLS vs RDA ----
resamps = caret::resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)

# Visualize resampling results across models
?caret:::xyplot.resamples
xyplot(resamps, what = "BlandAltman")
xyplot(resamps, what = "scatter") # more informative => rda is better

# Methods for making inferences about differences between models
diffs = diff(resamps)
summary(diffs) # see the ROC difference and p-value => rda is better
