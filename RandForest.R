setwd("/Users/mattamor/MachineLearningProject_Group1")

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

CFPB0 <- readRDS("CFPB.rds")|>
  mutate(Company = as.character(Company),
         freq = n(),
         .by = Company)|>
  mutate(Company = case_when(
    freq     >= 10 ~ Company,
    .default = paste0("Other_", freq)),
    Company  = as.factor(Company))|>
  select(-c(freq,
            ZIP,
            FIPS,
            Company))|>
  rename(Share_pplofcolor = `Share of people of color`,
         CI_score = `CI Index Score`,
         NotCreditIncluded = `Not Credit Included`,
         CreditConstrained = `Credit Constrained`)

set.seed(124)
CFPB <- CFPB0[sample(1:nrow(CFPB0), 10000),]
CFPB.test <- anti_join(CFPB0,CFPB)
CFPB.test <- CFPB.test[sample(1:nrow(CFPB.test), 2000),]
  
# Single Random Forest - Commented to avoid rerunning
ctrl <- trainControl(method = "cv")
tunegrid <- expand.grid(.mtry = (5:12))
CFPB.rf <- train(Relief ~ .,
               data = CFPB,
               method = 'rf',
               metric = 'Accuracy',
               trControl = ctrl,
               tuneGrid = tunegrid,
               importance = TRUE,
               ntree = 500)
CFPB.rf <- randomForest(Relief~.,
                        data=CFPB,
                        ntree = 100, 
                        importance = TRUE)
CFPB.rf$finalModel
plot(CFPB.rf$finalModel)
CFPB.imp.rf <- varImp(CFPB.rf)
CFPB.imp.rf <- CFPB.imp.rf$importance|>rownames_to_column()
varImpPlot(CFPB.rf$finalModel)
saveRDS(CFPB.rf,"CFPB_rf_noZipFipsCompany.rds")
CFPB.rf <- readRDS("CFPB_rf.rds")

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

CFPB.pred <- predict(CFPB.rf,newdata = CFPB.test)
confusionMatrix(CFPB.pred,reference = CFPB.test$Relief)
