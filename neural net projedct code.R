library(readxl)
CFPB <- read_excel("C:/Users/wh00ler/Desktop/Machine Learning/project/CFPB(1).xlsx")
############################################# NN Code
library(ggplot2)
library(reshape2)
library(arrow)
library(Matrix)
library(nnet)
library(caret)
library(tidyr)
library(dplyr)
library(NeuralNetTools)
set.seed(27514)
char_cols <- sapply(CFPB, is.character)

if (any(char_cols)) {
  cat("Converting character columns to factors:", 
      paste(names(char_cols[char_cols]), collapse = ", "), "\n")
  CFPB[char_cols] <- lapply(CFPB[char_cols], as.factor)
} else {
  cat("No character columns found.\n")
}
CFPBnnet <- CFPB
CFPBnnet$Relief <- as.factor(CFPBnnet$Relief)
#narrows it down to 18 variables
#removed month and quarter since neural net needs less variables and I'm keeping year
#company was found to be very unimportant
CFPBnnet<- CFPBnnet[,-c(4:6, 8:9, 14, 16, 18:21, 27, 34, 36)]
#I had previously changed characters up above but if you reimport and change the is.factor to is.character you can run this without needing to run the code above
CFPBnnet[sapply(CFPBnnet, is.factor)] <- lapply(CFPBnnet[sapply(CFPBnnet, is.factor)], as.numeric)
CFPBnnet <- CFPBnnet %>%
  rename(
    Credit_Constrained       = `Credit Constrained`
  )
CFPBnnet$Relief <- as.factor(CFPBnnet$Relief)
mm<- model.matrix(~. -1 -Relief, data=CFPBnnet)
library(scales)
#mm<- mm[,c(2:11, 15:21)]
## rescale all the variables
mm2 <- as.data.frame(apply(mm, 2, rescale))
mm2$Relief <- CFPBnnet$Relief
myControl <- trainControl(## 3-fold CV
  method = "cv",
  number = 3)
nnGrid <- expand.grid(size = seq(3, 21, 3),
                      decay = c(0, 0.2, 0.4, 0.8))
set.seed(27543)
nnetFit <- train(Relief ~ .,
                 data = mm2,
                 method = "nnet",
                 maxit = 1000,
                 tuneGrid = nnGrid,
                 trControl = myControl)
plotnet(nnetFit)
olden(nnetFit) + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
lekprofile(nnetFit)+ theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))


####################### Reference code
# No need to run, just do you have the code.
library(neuralnet)
# nueral network with 2 hidden nodes
#modifying to mimic the one below
smp_size1 <- floor(0.75 * nrow(mm2))
## set the seed to make your partition reproducible
set.seed(123)
train_ind1 <- sample(seq_len(nrow(mm2)), size = smp_size1)


###### set up dataset for lightgbm
train1 <- mm2[train_ind1, ]
test1  <- mm2[-train_ind1, ]
train1$Relief <- as.factor(as.character(train1$Relief))
test1$Relief  <- as.factor(as.character(test1$Relief))
nn_train1 <- neuralnet(Relief~., data=train1, hidden=c(4), linear.output=F, rep=3, algorithm = "backprop", learningrate = 0.03, err.fct = "ce", stepmax = 1e5)
# on test data
nn_test1 <- neuralnet(Relief~., data=test1, hidden = c(5),
                      linear.output = F)
# Do not run! nueral network with 2 layers, 3 hidden nodes and then 2
# nn_train2 <- neuralnet(Uber~., data=train1, hidden=c(3,2), linear.output=F, rep=3)
plot(nn_train1, rep = "best")
set.seed(42132) # set the random seed for reproducibility
# Compute fitted values from the training data
predictions_train <- predict(nn_train1, newdata = train1)
# Test the neural networks out of sample performance
predictions_test <- predict(nn_train1, newdata = test1)
p.test<-round(predictions_test,0 )
cm<- table(p.test, test1[,1])
cm
pred_classes <- predict(nnetFit, newdata = mm2)
confusionMatrix(pred_classes, mm2$Relief)
