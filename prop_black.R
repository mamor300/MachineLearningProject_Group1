df2 <- CFPB %>%
  mutate(
    majority_black = as.factor(if_else(prop_black > 0.144, 1, 0)),
    Relief = as.factor(Relief)  # ensure outcome is also a factor
  ) %>% 
  filter(majority_black == 1)
# 1. Convert your new data (df2) into a CatBoost Pool
feature_df <- df2 %>% 
  select(-Relief) %>%
  mutate(Issue_combined = as.factor(Issue_combined))  # must be factor, not character

pool_df2 <- catboost.load_pool(data = feature_df)

predicted_prob  <- catboost.predict(CBmodel, pool = pool_df2, prediction_type = "Probability")
predicted_class <- catboost.predict(CBmodel, pool = pool_df2, prediction_type = "Class")
ydf2 <- as.factor(df2$Relief)
# Convert predicted_class to a factor with the same levels as your outcome
predicted_class_factor <- as.factor(predicted_class)

# Make sure levels match
levels(predicted_class_factor) <- levels(ydf2)

# Now run confusionMatrix with the CLASS predictions, not probabilities
caret::confusionMatrix(predicted_class_factor, ydf2)

df2 <- CFPB %>%
  mutate(Relief = as.factor(Relief)) %>%
  filter(is_older_american == 0)

# Convert your new data into a CatBoost Pool
feature_df <- df2 %>% 
  select(-Relief) %>%
  mutate(Issue_combined = as.factor(Issue_combined))

pool_df2 <- catboost.load_pool(data = feature_df)
predicted_prob  <- catboost.predict(CBmodel, pool = pool_df2, prediction_type = "Probability")
predicted_class <- catboost.predict(CBmodel, pool = pool_df2, prediction_type = "Class")

ydf2 <- as.factor(df2$Relief)
predicted_class_factor <- as.factor(predicted_class)
levels(predicted_class_factor) <- levels(ydf2)
caret::confusionMatrix(predicted_class_factor, ydf2)
