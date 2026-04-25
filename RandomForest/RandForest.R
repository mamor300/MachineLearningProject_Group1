setwd("/Users/mattamor/MachineLearningProject_Group1/RandomForest")

install.packages("caret")
install.packages("randomForest")
install.packages("rpart.plot")
install.packages("RANN")
install.packages("pacman")

pacman::p_load(
  tidyverse,
  caret,
  rpart,
  rpart.plot,
  RANN,
  randomForest,
  randomForestExplainer
)

CFPB0 <- readRDS("CFPB.rds")

set.seed(124)
CFPB <- CFPB0[sample(1:nrow(CFPB0), 20000),]
CFPB.test <- anti_join(CFPB0,CFPB)
CFPB.test <- CFPB.test[sample(1:nrow(CFPB.test), 20000),]
  
# Single Random Forest - Commented to avoid rerunning
ctrl <- trainControl(method = "repeatedcv")
tunegrid <- expand.grid(.mtry = (10:17))
CFPB.rf <- train(Relief ~ .,
               data = CFPB,
               method = 'rf',
               metric = 'Accuracy',
               trControl = ctrl,
               tuneGrid = tunegrid,
               importance = TRUE,
               ntree = 500)
# CFPB.rf <- randomForest(Relief~.,
#                         data=CFPB,
#                         ntree = 100,
#                         importance = TRUE)
saveRDS(CFPB.rf,"CFPB_rf.rds")
CFPB.rf <- readRDS("CFPB_rf.rds")
CFPB.rf$finalModel
plot(CFPB.rf)
CFPB.imp.rf <- varImp(CFPB.rf)
CFPB.imp.rf <- CFPB.imp.rf$importance|>rownames_to_column()
varImpPlot(CFPB.rf$finalModel)


# Importance frame
CFPB_importance_frame <- measure_importance(CFPB.rf$finalModel)
CFPB_importance_other <- data.frame(importance(CFPB.rf$finalModel)) %>%
  rownames_to_column(var = "variable")
CFPB_importance_frame <- left_join(CFPB_importance_frame, CFPB_importance_other, by = "variable")
#write_csv(CFPB_importance_frame,"CFPB_importance_frame.csv")

### Plot multiway importance
CFPB_importance_frame %>%
  select(variable, mean_min_depth, times_a_root) %>%
  arrange(times_a_root, mean_min_depth) %>%
  mutate(variable = factor(variable, levels = variable)) %>%  # lock in the order
  pivot_longer(-variable, names_to = "measure", values_to = "value") %>%
  ggplot(aes(x = variable, y = value, fill = measure)) +
  geom_col() +
  facet_wrap(~ measure, scales = "free_x") +
  coord_flip() +
  labs(x = "Variable", y = "Value", title = "Multiway Importance Plot") +
  theme_bw() +
  theme(legend.position = "none")

# Testing model on test data
CFPB.test <- readRDS("CFPB_test.rds")
CFPB.pred <- predict(CFPB.rf, newdata = CFPB.test)
confusionMatrix(CFPB.pred,reference = CFPB.test$Relief, mode = "everything")

CFPB.old <- CFPB.test|>filter(is_older_american==1)

logit <- readRDS('logit_full.rds')
summary(logit,)
plot(logit, which = 1)
plot(logit,which = 2)
plot(logit,which = 3)
plot(logit,which = 5)