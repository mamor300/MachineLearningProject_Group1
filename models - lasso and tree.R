# --- REPLACEMENT PART 13: C and D ---

# 1. Align and Clean Data (The fix for the TRUE/FALSE error)
# We create the matrix first; it automatically handles NAs in predictors
X_dense <- model.matrix(Relief ~ . - 1, data = train_data)

# We subset Y to match the rows that survived in X
Y_train <- train_data$Relief[as.numeric(rownames(X_dense))]

# CRITICAL FIX: Remove any rows where Y is NA (glmnet requirement)
keep_idx <- !is.na(Y_train)
X_final  <- Matrix(X_dense[keep_idx, ], sparse = TRUE)
Y_final  <- Y_train[keep_idx]

# 2. LOGISTIC LASSO (Part C)
set.seed(12345)
cv_lasso <- cv.glmnet(
  x = X_final, 
  y = Y_final, 
  family = "binomial", 
  alpha = 1,
  nfolds = 10,
  type.measure = "auc"
)

# Output for Part C
plot(cv_lasso)
best_coefs <- coef(cv_lasso, s = "lambda.min")
print(best_coefs)

# 1. Convert the sparse matrix to a standard data frame
coef_matrix <- as.matrix(best_coefs)
active_variables <- data.frame(
  Variable = rownames(coef_matrix),
  Coefficient = coef_matrix[,1]
)

# 2. Filter out the zeros (the variables Lasso discarded)
significant_drivers <- active_variables[active_variables$Coefficient != 0, ]

# 3. Sort by impact (absolute value)
significant_drivers <- significant_drivers[order(-abs(significant_drivers$Coefficient)), ]

# 4. View the top drivers
head(significant_drivers, 20)

# --- PART 13: F (The Final Triple-Depth Sequence) ---

library(rpart)
library(rpart.plot)

# 1. UPDATED HYBRID WRAPPING FUNCTION
# Caps the root at 40 chars, wraps every 12, line-to-line verticality.
wrap_and_cap <- function(x, labs, digits, varlen, faclen) {
  labs <- sapply(labs, function(l) {
    if (nchar(l) > 40) {
      l <- paste0(substr(l, 1, 40), "...")
    }
    paste(strwrap(l, width = 12), collapse = "\n")
  })
  return(labs)
}

# 2. RUN MODELS AT THREE RESOLUTIONS
set.seed(12345)

# Resolution A: Executive (Depth 2)
tree_exec <- rpart(Relief ~ ., data = train_data_final, method = "class",
                   control = rpart.control(cp = 0.01, maxdepth = 2))

# Resolution B: Intermediate (Depth 4) - NEW
tree_inter <- rpart(Relief ~ ., data = train_data_final, method = "class",
                    control = rpart.control(cp = 0.005, maxdepth = 4))

# Resolution C: Deep Forensic (Depth 6)
tree_deep <- rpart(Relief ~ ., data = train_data_final, method = "class",
                   control = rpart.control(cp = 0.0005, maxdepth = 6))

# 3. PLOTTING FUNCTION
plot_audit <- function(model, title) {
  prp(model, extra = 101, box.palette = "RdYlGn", 
      split.fun = wrap_and_cap, faclen = 0, varlen = 0, 
      nn = TRUE, main = title)
}

# Generate the Set
plot_audit(tree_exec, "I. Executive Snapshot (High-Level Filter)")
plot_audit(tree_inter, "II. Intermediate Summary (Structural Drivers)")
plot_audit(tree_deep, "III. Deep Forensic Audit (Granular Evidence)")