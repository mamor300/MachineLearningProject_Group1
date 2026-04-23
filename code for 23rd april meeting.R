# --- PREPARATION: DATA SPLITTING ---
library(glmnet)
library(rpart)
library(rpart.plot)
library(caret)
library(Matrix)

# 1. Load the data
CFPB <- readRDS("C:/Users/alanj/OneDrive/Desktop/GWU/sem 2/ML/ml project code, sources etc/models/CFPB.rds")

# 2. Ensure Relief is a factor for classification
CFPB$Relief <- as.factor(CFPB$Relief)

# 3. Create Train/Test Split (80/20)
set.seed(12345)
train_idx <- createDataPartition(CFPB$Relief, p = 0.8, list = FALSE)


train_data <- CFPB[train_idx, ]
test_data  <- CFPB[-train_idx, ]

# LOGISTIC LASSO (PART C)

# 1. Prepare Matrix (Expanding categorical factors like 'Issue' into dummies)
X_dense <- model.matrix(Relief ~ . - 1, data = train_data)

# Align target variable with the expanded matrix rows

Y_train <- train_data$Relief[as.numeric(rownames(X_dense))]

# Final Clean: Remove NAs that might have survived and convert to Sparse Matrix

# This is crucial for performance with high-dimensional data (many Issues/States)

keep_idx <- !is.na(Y_train)

X_final  <- Matrix(X_dense[keep_idx, ], sparse = TRUE)

Y_final  <- Y_train[keep_idx]

# 2. Cross-Validated Lasso

set.seed(12345)

cv_lasso <- cv.glmnet(
  
  x = X_final, 
  
  y = Y_final, 
  
  family = "binomial", 
  
  alpha = 1,           # Alpha = 1 is LASSO (L1 penalty)
  
  nfolds = 10,
  
  type.measure = "auc" # Optimizing for Area Under Curve
  
)

# 3. Extract Significant Drivers

# s = "lambda.min" gives the lambda that minimizes prediction error

best_coefs <- coef(cv_lasso, s = "lambda.min")

coef_matrix <- as.matrix(best_coefs)

significant_drivers <- data.frame(
  
  Variable = rownames(coef_matrix),
  
  Coefficient = coef_matrix[,1]
  
) %>%
  
  filter(Coefficient != 0) %>%
  
  arrange(desc(abs(Coefficient)))

# Display Top 20 Predictors

print("Top 20 Drivers of Relief (LASSO):")

head(significant_drivers, 20)

# Try extracting coefficients with a slightly higher threshold to see more variables
flexible_coefs <- coef(cv_lasso, s = cv_lasso$lambda.1se)
flex_matrix <- as.matrix(flexible_coefs)

significant_drivers_extended <- data.frame(
  Variable = rownames(flex_matrix),
  Coefficient = flex_matrix[,1]
) %>%
  filter(Coefficient != 0) %>%
  arrange(desc(abs(Coefficient)))

print("Extended List of Drivers:")
head(significant_drivers_extended, 20)
# --- CART TREES (PART F: TRIPLE-DEPTH SEQUENCE) ---

# 1. Plotting Wrapper Function

# Standardizes text for long variable names (like Census proportions)

wrap_and_cap <- function(x, labs, digits, varlen, faclen) {
  
  sapply(labs, function(l) {
    
    if (nchar(l) > 40) l <- paste0(substr(l, 1, 40), "...")
    
    paste(strwrap(l, width = 12), collapse = "\n")
    
  })
  
}

# 2. Resolution Models

# We use 'class' for a classification tree to predict 0/1 (Relief/No Relief)

tree_exec  <- rpart(Relief ~ ., data = train_data_final, method = "class",
                    
                    control = rpart.control(cp = 0.01, maxdepth = 2))

tree_inter <- rpart(Relief ~ ., data = train_data_final, method = "class",
                    
                    control = rpart.control(cp = 0.005, maxdepth = 4))

tree_deep  <- rpart(Relief ~ ., data = train_data_final, method = "class",
                    
                    control = rpart.control(cp = 0.0005, maxdepth = 6))

# 3. Visualization

# extra = 101 shows the % of observations in each node

plot_audit <- function(model, title) {
  
  prp(model, extra = 101, box.palette = "RdYlGn", 
      
      split.fun = wrap_and_cap, faclen = 0, varlen = 0, 
      
      nn = TRUE, main = title)
  
}

plot_audit(tree_exec, "I. Executive Snapshot")
plot_audit(tree_inter, "II. Intermediate Summary")
plot_audit(tree_deep, "III. Deep Forensic Audit")

# This prints the text version of the deep tree to your console
summary(tree_deep)

# Alternatively, for a cleaner 'node-by-node' list:
print(tree_deep)

# 1. Generate predictions using the deep tree on the test set
tree_preds <- predict(tree_deep, newdata = test_data, type = "class")

# 2. Create the Confusion Matrix
# 'positive = "1"' tells R that "Relief Received" is our target of interest
tree_cm <- confusionMatrix(tree_preds, test_data$Relief, positive = "1")

print("--- DEEP TREE PERFORMANCE ---")
print(tree_cm$table)
print(tree_cm$overall['Accuracy'])

# Identify FP and FN
cat("False Positives (Predicted Relief, but didn't get it):", tree_cm$table[2,1], "\n")
cat("False Negatives (Predicted No Relief, but they DID get it):", tree_cm$table[1,2], "\n")

# 1. Prepare the test matrix (must match the training matrix structure)
X_test <- model.matrix(Relief ~ . - 1, data = test_data)

# 2. Predict probabilities and convert to 0/1 factors
# s = "lambda.min" uses the optimal model found during cross-validation
lasso_probs <- predict(cv_lasso, newx = X_test, s = "lambda.min", type = "response")
lasso_preds <- factor(ifelse(lasso_probs > 0.5, "1", "0"), levels = c("0", "1"))

# 3. Create the Confusion Matrix
lasso_cm <- confusionMatrix(lasso_preds, test_data$Relief, positive = "1")

print("--- LASSO PERFORMANCE ---")
print(lasso_cm$table)
print(lasso_cm$overall['Accuracy'])

# Identify FP and FN for LASSO
cat("LASSO False Positives (Predicted Relief, but denied):", lasso_cm$table[2,1], "\n")
cat("LASSO False Negatives (Predicted No Relief, but received):", lasso_cm$table[1,2], "\n")

# Count how many unique companies are in your finalized dataset
total_companies <- length(unique(CFPB$Company))

# Count the companies in your 'Power List' (Node 3)
power_list <- c("Alliance One, Inc.", "AMERICAN EXPRESS COMPANY", "American First Finance, Inc.", 
                "Bread Financial Holdings, Inc.", "Capio Partners, LLC", "CITIBANK, N.A.", 
                "DISCOVER BANK", "ENCORE CAPITAL GROUP INC.", "EQUIFAX, INC.", 
                "GOLDMAN SACHS BANK USA", "Harris & Harris, Ltd.", "KEYCORP", 
                "Kriya Capital, LLC", "LEXISNEXIS", "MRS BPO, LLC", "NAVY FEDERAL CREDIT UNION", 
                "ONLINE Information Services, Inc.", "Paypal Holdings, Inc", 
                "Portfolio Recovery Associates, LLC", "Reliant Capital Solutions, LLC", 
                "Revco Management, LLC", "Sequium Asset Solutions, LLC", 
                "Southwest Credit Systems, L.P.", "Spring Oaks Capital, LLC", 
                "TD BANK US HOLDING COMPANY", "The CBE Group, Inc.", "The CMI Group, Inc.", 
                "TRANSUNION INTERMEDIATE HOLDINGS, INC.", "U.S. BANCORP", 
                "W&A Intermediate Co., LLC", "WELLS FARGO & COMPANY")

power_count <- length(power_list)
percentage_of_market <- (power_count / total_companies) * 100

print(paste("The high-relief 'Power List' represents", round(percentage_of_market, 2), "% of all companies."))

# 1. Filter the original test_data for older Americans
original_older_test <- test_data %>% filter(is_older_american == 1)

# 2. Predict using your existing deep tree model
original_older_preds <- predict(tree_deep, newdata = original_older_test, type = "class")

# 3. Generate the Confusion Matrix
original_older_cm <- confusionMatrix(original_older_preds, original_older_test$Relief, positive = "1")

# 4. Display Results
print("--- ORIGINAL DATASET: OLDER AMERICANS CONFUSION MATRIX ---")
print(original_older_cm$table)
print(paste("Accuracy:", round(original_older_cm$overall['Accuracy'], 4)))

