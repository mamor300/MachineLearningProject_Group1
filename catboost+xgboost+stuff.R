
#XGBoost model
library(dplyr)
library(ggplot2)
library(xgboost)
library(caret)
library(Matrix)
library(data.table)
library(readxl)
CFPB <- read_excel("C:/Users/whaler/Desktop/Machine learning/group project/CFPB.xlsx")
#i'm just testing here to see if I get a better result with the same set in XGBoost as I did with nnet
#CFPBxgboost<- CFPBxgboost[,-c(2:4,6:18, 20:21, 23, 25:28, 30, 33, 40:41, 43:44, 48)]
char_cols <- sapply(CFPB, is.character)

if (any(char_cols)) {
  cat("Converting character columns to factors:", 
      paste(names(char_cols[char_cols]), collapse = ", "), "\n")
  CFPB[char_cols] <- lapply(CFPB[char_cols], as.factor)
} else {
  cat("No character columns found.\n")
}
y <- as.numeric(CFPB$Relief) - 1 #ONLY DO THIS IF IN THE ENVIRONMENT IT SHOWS THAT Y IS 1,2 # OUT -1 IF IT'S 0,1
y_factor <- as.factor(CFPB$Relief)
#this is removing tags from the xgboost model parameters
#I had to do this colnames thing since sparse.model.matrix was giving a name mismatch error
colnames(CFPB) <- make.names(colnames(CFPB))
x <- sparse.model.matrix(Relief ~ ., CFPB[,c(1:37)])
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
#linear regression
CFPB$Relief <- as.numeric(CFPB$Relief)
linear <- lm(Relief ~., data = CFPB)
summary(linear)
qqnorm(linear$residuals)
plot(CFPB, which=1)
#CATBOOST
#install.packages('remotes')
remotes::install_url('https://github.com/catboost/catboost/releases/download/v1.2.10/catboost-R-windows-x86_64-1.2.10.tgz', INSTALL_opts = c("--no-multiarch", "--no-test-load"))
library(catboost)
packageVersion("catboost")  # should show 1.2.10

# ── Key difference: CatBoost handles categoricals natively ──────────────────
# No need for sparse.model.matrix — pass raw data frame directly
# Identify categorical feature indices (0-based for CatBoost)
feature_cols <- CFPB[, 1:37]
feature_cols_no_target <- feature_cols[, colnames(feature_cols) != "Relief"]

cat_feature_indices <- which(sapply(feature_cols_no_target, is.factor)) - 1  # 0-based

#y <- as.numeric(CFPB$Relief) - 1  # same as before: must be 0/1 (don't run if already 01)
y_factor <- as.factor(CFPB$Relief)

# Build CatBoost Pool (equivalent to xgb.DMatrix)
CBdata <- catboost.load_pool(
  data  = feature_cols_no_target,
  label = y,
  #cat_features = cat_feature_indices  # CatBoost encodes these internally (dont need if already factors)
)

# ── Parameters (mapped from your XGBparams) ─────────────────────────────────
# eta          → learning_rate
# max_depth    → depth
# min_child_weight → min_data_in_leaf
# subsample    → subsample (called bagging_fraction in some versions)
# gamma        → no direct equivalent; l2_leaf_reg (lambda) is closest
# colsample_bytree → rsm (Random Subspace Method)

CBparams <- list(
  loss_function    = "Logloss",        # binary:logistic equivalent
  eval_metric      = "Accuracy",
  learning_rate    = 0.05,             # eta
  depth            = 6,                # max_depth
  min_data_in_leaf = 10,               # min_child_weight
  l2_leaf_reg      = 3,                # regularization (gamma analog)
  rsm              = 1,                # colsample_bytree
  subsample        = 0.8,
  iterations       = 200              # nrounds
  #verbose          = 50                # print every 50 rounds (verbosity analog)[doesn't work with catboost]
)
# ── Cross-validation to find optimal iterations ──────────────────────────────
set.seed(123)
system.time({
  CBcv <- catboost.cv(
    pool       = CBdata,
    params     = CBparams,
    fold_count = 5,              # nfold
    type       = "Classical"
  )
})
# Find best iteration (lowest test error = highest test accuracy)
XGBnrounds <- which.max(CBcv$test.Accuracy.mean)
cat("Optimal iterations:", XGBnrounds, "\n")

# ── Train final model ────────────────────────────────────────────────────────
CBparams$iterations <- XGBnrounds

system.time({
  CBmodel <- catboost.train(
    learn_pool = CBdata,
    params     = CBparams
  )
})

# ── Predictions ──────────────────────────────────────────────────────────────
CBpredictions <- catboost.predict(CBmodel, CBdata, prediction_type = "Probability")
CBpredictions_class <- ifelse(CBpredictions > 0.5, 1, 0)
CBresiduals <- y - CBpredictions

# ── Accuracy & Confusion Matrix ──────────────────────────────────────────────
accuracy <- mean(CBpredictions_class == y)
cat("Accuracy:", accuracy, "\n")

table(CBpredictions_class, y)
caret::confusionMatrix(as.factor(CBpredictions_class), as.factor(y))

# ── QQ plot of residuals ─────────────────────────────────────────────────────
qqnorm(CBresiduals)

# ── Feature Importance ───────────────────────────────────────────────────────
importance <- catboost.get_feature_importance(CBmodel, CBdata)
importance_df <- data.frame(
  Feature    = colnames(feature_cols_no_target),
  Importance = importance
) |> arrange(desc(Importance))

ggplot(importance_df[1:20, ], aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "CatBoost Feature Importance", x = "Feature", y = "Importance")

# ── Hyperparameter Tuning via caret ─────────────────────────────────────────
# Note: use the catboost caret wrapper
tune_grid <- expand.grid(
  depth            = c(2, 4, 6),
  learning_rate    = c(0.05, 0.1, 0.3),
  iterations       = c(200, 350, 500),
  l2_leaf_reg      = c(1, 3, 5),
  rsm              = c(0.8, 1),
  border_count     = 128
)

tune_control <- caret::trainControl(
  method       = "cv",
  number       = 3,
  verboseIter  = FALSE,
  allowParallel = FALSE
)

system.time({
  cb_tune <- caret::train(
    x          = feature_cols_no_target,
    y          = y_factor,
    method     = catboost.caret,   # built-in caret interface
    trControl  = tune_control,
    tuneGrid   = tune_grid#,
    #verbose    = FALSE
  )
})

cb_tune$bestTune
max(cb_tune$results$Accuracy)
CATBpredictions_class <- ifelse(CBpredictions > 0.5, 1, 0)
accuracy <- mean(CBpredictions_class == y)
accuracy
#accuracy <- 1 - min(XGBoostCV2$evaluation_log$test_error_mean)
#accuracy
#confusion matrix
table(CBpredictions_class, y)
caret::confusionMatrix(as.factor(CBpredictions_class), as.factor(y))
#tests
CFPB_test <- readRDS("C:/Users/whaler/Downloads/CFPB_test.rds")
# 1. Process CFPB_test the same way as your training data
colnames(CFPB_test) <- make.names(colnames(CFPB_test))

# Convert character columns to factors
char_cols_test <- sapply(CFPB_test, is.character)
CFPB_test[char_cols_test] <- lapply(CFPB_test[char_cols_test], as.factor)

# 2. Align factor levels to match training data
for (col in names(CFPB)) {
  if (is.factor(CFPB[[col]]) && col %in% names(CFPB_test)) {
    CFPB_test[[col]] <- factor(CFPB_test[[col]], levels = levels(CFPB[[col]]))
  }
}

# 3. Create sparse matrix using the SAME formula
x_test <- sparse.model.matrix(Relief ~ ., CFPB_test[, c(1:37)])

# 4. Verify column names match
stopifnot(all(colnames(x_test) == colnames(x)))

# 5. Now predict
CFPB_predXGB <- predict(xgb_tune, newdata = x_test)
confusionMatrix(CFPB_predXGB,reference = CFPB_test$Relief)

CFPB_predCAT <- predict(cb_tune, newdata = CFPB_test)
confusionMatrix(CFPB_predCAT,reference = CFPB_test$Relief)

CFPB_old <- CFPB_test|>filter(is_older_american==1)
CFPB_pred <- predict(cb_tune,newdata = CFPB_old)
confusionMatrix(CFPB_pred,reference = CFPB_old$Relief)
# 1. Process CFPB_old the same way as training data
colnames(CFPB_old) <- make.names(colnames(CFPB_old))

# 2. Convert character columns to factors
char_cols_old <- sapply(CFPB_old, is.character)
CFPB_old[char_cols_old] <- lapply(CFPB_old[char_cols_old], as.factor)

# 3. Align factor levels to match training data
for (col in names(CFPB)) {
  if (is.factor(CFPB[[col]]) && col %in% names(CFPB_old)) {
    CFPB_old[[col]] <- factor(CFPB_old[[col]], levels = levels(CFPB[[col]]))
  }
}

# 4. Create sparse matrix using the SAME formula
x_old <- sparse.model.matrix(Relief ~ ., CFPB_old[, c(1:37)])

# 5. Verify column names match
stopifnot(all(colnames(x_old) == colnames(x)))
CFPB_predoldxgb <- predict(xgb_tune,newdata = x_old)
confusionMatrix(CFPB_predoldxgb,reference = CFPB_old$Relief)
#scores
library(ModelMetrics)
f1cb <- f1Score(actual = y, predicted = CBpredictions, cutoff = 0.5)
f1cb
#xgb_probs <- predict(xgb_tune, newdata = x, type = "prob")[, 2]  # prob of class 1
f1xgb <- f1Score(actual = y, predicted = XGBpredictions, cutoff = 0.5)
f1xgb
# CatBoost - older Americans
cb_probs_old <- catboost.predict(cb_tune$finalModel, 
                                 catboost.load_pool(data = CFPB_old[, colnames(feature_cols_no_target)],
                                                    label = NULL),
                                 prediction_type = "Probability")
f1cb_old <- f1Score(actual = as.numeric(CFPB_old$Relief) - 1, predicted = cb_probs_old, cutoff = 0.5)

# XGBoost - older Americans
xgb_probs_old <- predict(xgb_tune$finalModel, newdata = x_old)  # already returns probabilities
f1xgb_old <- f1Score(actual = as.numeric(CFPB_old$Relief) - 1, predicted = xgb_probs_old, cutoff = 0.5)
#logit
logit_full <- readRDS("C:/Users/whaler/Downloads/logit_full.rds")

### Visualizing: Creating odds ratio's for the logit model ###

# Extract coefficients
my_coefs <- coef(logit_full)

# Calculate Odds Ratios
my_odds <- exp(my_coefs)

# Combine into a clean table for your top variables
results_table <- data.frame(
  LogOdds = my_coefs,
  OddsRatio = my_odds
)

# Look specifically at companies and submission methods
print(round(results_table[grep("Company_grp|Submitted.via|is_timely"
                               , rownames(results_table)), ], 3))  

#### Visualizing with all variables ####   
library(ggplot2)

# 1. Extract the summary matrix
s <- summary(logit_full)
results <- as.data.frame(s$coefficients)
results$Variable <- rownames(results)

# 2. Calculate Odds Ratios and 95% Confidence Intervals
results$OR    <- exp(results$Estimate)
results$Lower <- exp(results$Estimate - 1.96 * results$`Std. Error`)
results$Upper <- exp(results$Estimate + 1.96 * results$`Std. Error`)

# 3. Filter for the most interesting variables (Companies and Methods)
# We exclude Intercept and State variables for clarity
# Remove rows with non-positive or infinite OR values before plotting
plot_data <- results[!grepl("Intercept|State", results$Variable), ]
plot_data <- plot_data[is.finite(plot_data$OR) & plot_data$OR > 0, ]
plot_data <- plot_data[is.finite(plot_data$Lower) & is.finite(plot_data$Upper), ]

ggplot(plot_data, aes(x = reorder(Variable, OR), y = OR)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), color = "steelblue") +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "Predictors of Consumer Relief (Logit Model)",
    subtitle = "Odds Ratios > 1 indicate higher likelihood of relief",
    x = "Variable",
    y = "Odds Ratio (Log Scale)"
  ) +
  theme_minimal()

library(ggplot2)
library(dplyr)

# --- Configuration ---
N <- 15  # Number of top AND bottom variables to show (adjust as desired)

# --- Clean variable names for display ---
plot_data <- results %>%
  filter(!grepl("Intercept|State", Variable)) %>%
  filter(is.finite(OR) & OR > 0 & is.finite(Lower) & is.finite(Upper)) %>%
  mutate(
    # Shorten long names
    Label = Variable %>%
      gsub("Issue_combined", "", .) %>%
      gsub("Pub.response", "Co. Response: ", .) %>%
      gsub("Submitted.via", "Submitted via: ", .) %>%
      gsub("Company_grp", "Company: ", .) %>%
      # Truncate anything still too long
      substr(1, 55)
  )

# --- Select top N and bottom N by OR ---
top_n    <- plot_data %>% arrange(desc(OR)) %>% slice_head(n = N)
bottom_n <- plot_data %>% arrange(OR)       %>% slice_head(n = N)
focused  <- bind_rows(top_n, bottom_n) %>% distinct()

# --- Color by direction ---
focused <- focused %>%
  mutate(Direction = ifelse(OR > 1, "Higher Relief", "Lower Relief"))

# --- Plot ---
ggplot(focused, aes(x = reorder(Label, OR), y = OR, color = Direction)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), size = 0.5, linewidth = 0.7) +
  coord_flip() +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("Higher Relief" = "#2196F3", "Lower Relief" = "#F44336")) +
  labs(
    title    = "Top Predictors of Consumer Relief",
    subtitle = paste0("Top and bottom ", N, " variables by Odds Ratio — Logit Model"),
    x        = NULL,
    y        = "Odds Ratio (Log Scale)",
    color    = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "gray40", size = 10),
    legend.position  = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y      = element_text(size = 9)
  )
