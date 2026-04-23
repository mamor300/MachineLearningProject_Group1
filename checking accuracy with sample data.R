library(tidyverse)
library(rpart)
library(caret)

# 1. Load Training Data and Build the Deep Tree
CFPB_train <- readRDS("C:/Users/alanj/OneDrive/Desktop/GWU/sem 2/ML/ml project code, sources etc/models/CFPB.rds")
CFPB_train$Relief <- as.factor(CFPB_train$Relief)

tree_deep <- rpart(Relief ~ ., data = CFPB_train, method = "class",
                   control = rpart.control(cp = 0.0005, maxdepth = 6))

# 2. Load the Test Data
prof_test <- readRDS("C:/Users/alanj/OneDrive/Desktop/GWU/sem 2/ML/ml project code, sources etc/models/CFPB_test.rds")
prof_test$Relief <- as.factor(prof_test$Relief)

# 3. Run Predictions on the Test Data
prof_preds <- predict(tree_deep, newdata = prof_test, type = "class")

# 4. Evaluate Performance
prof_cm <- confusionMatrix(prof_preds, prof_test$Relief, positive = "1")

print("--- PROFESSOR TEST FILE PERFORMANCE ---")
print(prof_cm$table)
print(paste("Accuracy on Professor's Data:", round(prof_cm$overall['Accuracy'], 4)))

# 1. Identify the rows where the demographic category is "proportion greater than 65"
# Note: Replace 'Age_Group' with the actual column name in your dataset
subgroup_test <- prof_test %>% 
  filter(Age_Group == "proportion greater than 65") 

# 2. Run predictions specifically on this subgroup
subgroup_preds <- predict(tree_deep, newdata = subgroup_test, type = "class")

# 3. Generate the Confusion Matrix for this subgroup
subgroup_cm <- confusionMatrix(subgroup_preds, subgroup_test$Relief, positive = "1")

# 4. Display Results
print("--- SUBGROUP PERFORMANCE: Proportion > 65 ---")
print(subgroup_cm$table)
print(paste("Subgroup Accuracy:", round(subgroup_cm$overall['Accuracy'], 4)))
print(paste("Subgroup Sensitivity (Recall):", round(subgroup_cm$byClass['Sensitivity'], 4)))

#Ethical consideration - 65+

# 1. Filter the Professor's test data for the Older American subset
older_test <- prof_test %>% filter(is_older_american == 1)

# 2. Predict using the deep tree
older_preds <- predict(tree_deep, newdata = older_test, type = "class")

# 3. Generate the Confusion Matrix
older_cm <- confusionMatrix(older_preds, older_test$Relief, positive = "1")

# 4. Display the table and key accuracy metric
print("--- CONFUSION MATRIX: OLDER AMERICANS SUBSET ---")
print(older_cm$table)
print(older_cm$overall['Accuracy'])