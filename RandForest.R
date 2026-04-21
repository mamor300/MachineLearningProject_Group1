setwd("/Users/mattamor/MachineLearningProject_Group1")

install.packages("caret")
install.packages("randomForest")
install.packages("rpart.plot")
install.packages("RANN")
install.packages("pacman")

pacman::p_load(
  caret,
  rpart,
  rpart.plot,
  RANN
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
            ZIP))|>
  rename(Share_pplofcolor = `Share of people of color`,
         CI_score = `CI Index Score`,
         NotCreditIncluded = `Not Credit Included`,
         CreditConstrained = `Credit Constrained`)

set.seed(124)
CFPB <- CFPB0[sample(1:nrow(CFPB0), 10000),]
  
# Single Random Forest - Commented to avoid rerunning
ctrl <- trainControl(method = "cv")
tunegrid <- expand.grid(.mtry = (5:14))
CFPB.rf <- train(Relief ~ .,
               data = CFPB,
               method = 'rf',
               metric = 'Accuracy',
               trControl = ctrl,
               tuneGrid = tunegrid,
               importance = TRUE,
               ntree = 250)
CFPB.rf <- randomForest(Relief~.,
                        data=CFPB,
                        ntree = 100, 
                        importance = TRUE)
CFPB.rf$finalModel
plot(CFPB.rf)
saveRDS(CFPB.rf,"CFPB_rf.rds")
CFPB.rf <- readRDS("CFPB_rf.rds")