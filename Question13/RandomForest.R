# RF Model

# Random Forest Model
CFPB.complete <- CFPB[which(complete.cases(CFPB)),] %>%
  mutate(ZIP = as.character(ZIP))

ctrl <- trainControl(method = "oob")
tunegrid <- expand.grid(.mtry = (4:8))
CFPB.rf <- train(Relief ~ .,
                 data = CFPB.complete,
                 method = 'rf',
                 metric = 'Accuracy',
                 trControl = ctrl,
                 tuneGrid = tunegrid,
                 importance = TRUE)
CFPB.rf$finalModel
plot(SF.rf)