#Loading the necessary packages
library(randomForest)
library(caret)
library(e1071)
library(ranger)
library(MLmetrics)

#Reading the data and setting the seed
data <- read.csv("churn.csv", stringsAsFactors = T)
set.seed(582)

#Removing NAs from the dataset
sapply(data, function(x) sum(is.na(x)))
data <- na.omit(data)

#Normalizing quantitative variables
data[, 19:20] <- scale(data[, 19:20])

#Creating features for variable "tenure" and making it factor
for(i in 1:nrow(data)){
  if(data[i, "tenure"] >= 0 & data[i, "tenure"] <= 12){
    data$tenure[i] <- "0-1 years"
  }else if(data[i, "tenure"] > 12 & data[i, "tenure"] <= 24){
    data$tenure[i] <- "1-2 years"
  }else if(data[i, "tenure"] > 24 & data[i, "tenure"] <= 36){
    data$tenure[i] <- "2-3 years"
  }else if(data[i, "tenure"] > 36 & data[i, "tenure"] <= 48){
    data$tenure[i] <- "3-4 years"
  }else if(data[i, "tenure"] > 48 & data[i, "tenure"] <= 60){
    data$tenure[i] <- "4-5 years" 
  }else{
    data$tenure[i] <- "More than 5 years"
  }
}
data$tenure <- factor(data$tenure)

#Converting SeniorCitizen as a factor
data$SeniorCitizen <- as.factor(ifelse(data$SeniorCitizen == 1, "Yes", "No"))

#Converting "No", "No internet service" and "Yes" into categorical variables with 2 levels
levels(data$MultipleLines) <- c("No","No","Yes")
levels(data$OnlineSecurity) <- c("No","No","Yes")
levels(data$OnlineBackup) <- c("No","No","Yes")
levels(data$DeviceProtection) <- c("No","No","Yes")
levels(data$TechSupport) <- c("No","No","Yes")
levels(data$StreamingTV) <- c("No","No","Yes")
levels(data$StreamingMovies) <- c("No","No","Yes")

#Removing the ID column
data <- data[,2:21]

#Split test-train
train_indices <- sample(1:7032, 7032*0.7)
train <- data[train_indices,]
test <- data[-train_indices,]

#Baseline accuracy if we say "No churn" to every customer.
sum(data$Churn == "No") / nrow(data)

#Random Forest -> Grid Search mtry tuning
folds <- createMultiFolds(train$Churn, k = 10, times = 1)
control <- trainControl(method = "cv", number = 10, verboseIter = TRUE, summaryFunction = twoClassSummary,
                        classProbs = TRUE, savePredictions = TRUE, index = folds, allowParallel = TRUE)
tune_grid <- expand.grid(mtry = c(2, 5, 7, 9, 11, 13), splitrule = "gini", min.node.size = 5)
rf_gridsearch <- train(Churn ~., data = train, method = "ranger", metric = "Accuracy",
                       tuneGrid = tune_grid, trControl = control, num.trees = 500)
rf_gridsearch
plot(rf_gridsearch)
predictions <- predict(rf_gridsearch, newdata = test)
Accuracy(predictions, test$Churn)
ConfusionMatrix(predictions, test$Churn)

#Gradient Boosting -> depth, learning rate, and number of trees tuning
folds <- createMultiFolds(train$Churn, k = 10, times = 1)
control <- trainControl(method = "cv", number = 10, verboseIter = TRUE, summaryFunction = twoClassSummary,
                        classProbs = TRUE, savePredictions = TRUE, index = folds, allowParallel = TRUE)
tune_grid <- expand.grid(interaction.depth=c(1, 3, 5), n.trees = c(500, 1000), shrinkage=c(0.01, 0.001), n.minobsinnode = 10)
gbm_gridsearch <- train(Churn ~., data = train, method = "gbm", metric = "Accuracy", tuneGrid = tune_grid, trControl = control)
gbm_gridsearch
plot(gbm_gridsearch)
predictions <- predict(gbm_gridsearch, newdata = test)
Accuracy(predictions, test$Churn)
ConfusionMatrix(predictions, test$Churn)

#Weighted rf
model_weights <- ifelse(train$Churn == "No",
                        sum(train$Churn == "No")/nrow(train),
                        sum(train$Churn == "Yes")/nrow(train))
folds <- createMultiFolds(train$Churn, k = 10, times = 1)
control <- trainControl(method = "cv", number = 10, verboseIter = TRUE, summaryFunction = twoClassSummary,
                        classProbs = TRUE, savePredictions = TRUE, index = folds, allowParallel = TRUE)
tune_grid <- expand.grid(mtry = c(2, 5, 7, 9, 11, 13), splitrule = "gini", min.node.size = 5)
weighted_rf_gridsearch <- train(Churn ~., data = train, method = "ranger", weights = model_weights,
                       metric = "Accuracy", tuneGrid = tune_grid, trControl = control, num.trees = 500)
weighted_rf_gridsearch
plot(weighted_rf_gridsearch)
predictions <- predict(weighted_rf_gridsearch, newdata = test)
Accuracy(predictions, test$Churn)
ConfusionMatrix(predictions, test$Churn)

#Weighted gbm
folds <- createMultiFolds(train$Churn, k = 10, times = 1)
control <- trainControl(method = "cv", number = 10, verboseIter = TRUE, summaryFunction = twoClassSummary,
                        classProbs = TRUE, savePredictions = TRUE, index = folds, allowParallel = TRUE)
tune_grid <- expand.grid(interaction.depth=c(1, 3, 5), n.trees = c(500, 1000), shrinkage=c(0.01, 0.001), n.minobsinnode = 10)
weighted_gbm_gridsearch <- train(Churn ~., data = train, method = "gbm", weights = model_weights,
                        metric = "Accuracy", tuneGrid = tune_grid, trControl = control)
weighted_gbm_gridsearch
plot(weighted_gbm_gridsearch)
predictions <- predict(weighted_gbm_gridsearch, newdata = test)
Accuracy(predictions, test$Churn)
ConfusionMatrix(predictions, test$Churn)
