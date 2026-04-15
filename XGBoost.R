#13

#XGBoost model
library(dplyr)
library(ggplot2)
library(xgboost)
library(caret)
library(Matrix)
library(data.table)
CFPB <- read_excel("C:/Users/wh00ler/Desktop/Machine Learning/project/CFPB.xlsx")
y <- as.numeric(CFPB$Relief)
#this is removing tags from the xgboost model parameters
#I had to do this colnames thing since sparse.model.matrix was giving a name mismatch error
colnames(CFPB) <- make.names(colnames(CFPB))
x <- sparse.model.matrix(Relief ~ ., CFPB[,c(1:5, 13:44)])
train_control <- trainControl(method = "cv", number = 5)
XGBoostdata <- xgb.DMatrix(data = x, label = y)
XGBparams <- list(
  booster = "gbtree",
  objective = "reg:squarederror",
  eta = 0.05,
  max_depth = 6,
  min_child_weight = 10,
  max_delta_step = 0,
  gamma = 0,
  colsample_bytree = 1,
  subsample = 0.8,
  verbosity = 1
)
#find optimal number of rounds
system.time ({
  XGBoostCV <- xgb.cv(
    data = XGBoostdata,
    params = XGBparams,
    nrounds = 200,
    nfold = 5,
    metrics = "rmse"
  )
})
XGBnrounds <- which.min(XGBoostCV$evaluation_log$test_rmse_mean)
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
  x = x, y = y,
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  verbose = TRUE
)
})
