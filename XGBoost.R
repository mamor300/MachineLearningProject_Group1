
#XGBoost model
library(dplyr)
library(ggplot2)
library(xgboost)
library(caret)
library(Matrix)
library(data.table)
library(readxl)
CFPB <- read_excel("C:/Users/whaler/Downloads/CFPB.xlsx")
y <- as.numeric(CFPB$Relief)
y_factor <- as.factor(CFPB$Relief)
#this is removing tags from the xgboost model parameters
#I had to do this colnames thing since sparse.model.matrix was giving a name mismatch error
colnames(CFPB) <- make.names(colnames(CFPB))
x <- sparse.model.matrix(Relief ~ ., CFPB[,c(1:5, 13:44)])
train_control <- trainControl(method = "cv", number = 5)
XGBoostdata <- xgb.DMatrix(data = x, label = y)
XGBparams <- list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta = 0.05,
  max_depth = 6,
  min_child_weight = 10,
  max_delta_step = 0,
  gamma = 0,
  colsample_bytree = 1,
  subsample = 0.8,
  verbosity = 1
)
set.seed(123)
#find optimal number of rounds
system.time ({
  XGBoostCV <- xgb.cv(
    data = XGBoostdata,
    params = XGBparams,
    nrounds = 200,
    nfold = 5,
    metrics = "error"
  )
})
XGBnrounds <- which.min(XGBoostCV$evaluation_log$test_error_mean)
XGBnrounds
system.time({
  XGBmodel <- xgb.train(
    params = XGBparams,
    data = XGBoostdata,
    nrounds = XGBnrounds
  )
})
XGBpredictions <- predict(XGBmodel, XGBoostdata)
XGBresiduals <- y - XGBpredictions
tune_grid <- expand.grid(
  nrounds = seq(from = 200, to =500, by =50),
  eta = c(0.05, 0.1, 0.3),
  max_depth = c(2, 4, 6),
  gamma = c(0, 0.1, 0.5),
  colsample_bytree = c(0.5, 0.8, 1),
  min_child_weight = c(1, 10, 100),
  subsample = c(0.8, 1)
  #alpha = (0:2),
  #lambda = (0:5)
)
tune_control <- caret::trainControl(
  method = "cv",
  number = 3,
  verboseIter = TRUE, #training log
  allowParallel = TRUE #FALSE for reproducible results
)
system.time({xgb_tune <- caret::train(
  x = x, y = y_factor,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)
})
xgb_tune$bestTune
max(xgb_tune$results$Accuracy)
qqnorm(XGBresiduals)
library(DiagrammeR)
xgb.plot.tree(model = xgb_tune$finalModel, trees = 1)
### plot
#get the first three trees
xgb.plot.tree(model = xgb_tune$finalModel, trees = 0:2)
xgb.plot.multi.trees(xgb_tune$finalModel)
importance_matrix <- xgb.importance(model = xgb_tune$finalModel)
xgb.plot.importance(importance_matrix, xlab = "Feature Importance")
#accuracy
XGBpredictions_class <- ifelse(XGBpredictions > 0.5, 1, 0)
accuracy <- mean(XGBpredictions_class == y)
accuracy
#accuracy <- 1 - min(XGBoostCV2$evaluation_log$test_error_mean)
#accuracy
#confusion matrix
table(XGBpredictions_class, y)
caret::confusionMatrix(as.factor(XGBpredictions_class), as.factor(y))
