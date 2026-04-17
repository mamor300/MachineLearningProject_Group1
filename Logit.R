setwd('/Users/mattamor/MachineLearningProject_Group1')

pacman::p_load(
  readr,
  readxl,
  writexl,
  tidyverse,
  VIM,
  caret,
  missMDA,
  glmnet,
  biglm
)

CFPB0 <- read_xlsx("CFPB.xlsx") 
sapply(CFPB0,class)
CFPB <- CFPB0|>
  mutate(Relief = as.numeric(Relief),
         Year   = as.factor(Year),
         across(where(is.character), as.factor))|>
  select(-c(
    "Received",
    "Sent",
    "sample",
    "ZIP",
    "FIPS",
    "Company"))

CFPB.matrix <- model.matrix(Relief~., data = CFPB)
CFPB.lm.test <- glmnet(CFPB.matrix, CFPB$Relief, family=binomial(link=logit))
cvfit <- cv.glmnet(CFPB.matrix, CFPB$Relief, family = "binomial")
coef(cvfit, s = "lambda.min")   # best fit
coef(cvfit, s = "lambda.1se")  # more regularized (simpler model)
coefs <- coef(cvfit, s = "lambda.min")
coefs[coefs != 0]
as.matrix(coef(cvfit, s = "lambda.min"))
plot(cvfit$glmnet.fit, xvar = "lambda")
plot(cvfit)
pred <- predict(cvfit, newx = CFPB.matrix, s = "lambda.min", type = "response")
resid_raw <- CFPB$Relief - pred
resid_dev <- sign(CFPB$Relief - pred) * sqrt(
  -2 * (CFPB$Relief * log(pred) + (1 - CFPB$Relief) * log(1 - pred))
)
plot(pred, resid_raw,
     xlab = "Predicted probability",
     ylab = "Residuals")
abline(h = 0, col = "red")

CFPB.lm <- glm(Relief~., data=CFPB, family=binomial(link=logit))

plot(CFPB.lm, which = 1)
plot(CFPB.lm,which = 2)
plot(CFPB.lm,which = 3)
plot(CFPB.lm,which = 5)
summary(influence.measures(CFPB.lm))
summary(CFPB.lm)
coef <- as.data.frame(CFPB.lm$coefficients)

(exp(coef(CFPB.lm))-1)*100
exp(coefRexp(coef(CFPB.lm))

CFPB.lm.test$a0
