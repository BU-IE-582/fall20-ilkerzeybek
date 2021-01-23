#Loading necessary libraries
library(caret)
library(ranger)
library(MLmetrics)

#Setting the seed and reading the data
set.seed(582)
data <- read.csv("hcvdat0.csv", stringsAsFactors = T)

#Removing the ID column and renaming the output factor levels to use in caret package
data <- data[,2:14]
levels(data$Category) <- c("BloodDonor", "SuspectBloodDonor", "Hepatitis", "Fibrosis", "Cirrhosis")

#Splitting the data into train and test sets
train_indices <- sample(1:615, 615 * 0.60)
train <- data[train_indices,]
test <- data[-train_indices,]

#Removing NA observations
data <- na.omit(data)

#Baseline accuracy if we say everyone is blood donor since blood donor category dominates the output classes.
sum(data$Category == "BloodDonor") / nrow(data)

#RF tuning
folds <- createMultiFolds(train$Category, k = 10, times = 3)
control <- trainControl(method = "cv", number = 10, verboseIter = TRUE,
                        classProbs = TRUE, savePredictions = TRUE, index = folds, allowParallel = TRUE)
tune_grid <- expand.grid(mtry = c(2,5,7,10), splitrule = "gini", min.node.size = 5)
rf_gridsearch <- train(Category~., train, method = "ranger", tuneGrid = tune_grid, trControl = control)
rf_gridsearch
plot(rf_gridsearch)
predictions <- predict(rf_gridsearch, newdata = test[,2:13])
Accuracy(predictions, test$Category)
ConfusionMatrix(predictions, test$Category)

#GBM tuning
folds <- createMultiFolds(train$Category, k = 10, times = 3)
control <- trainControl(method = "cv", number = 10, verboseIter = TRUE,
                        classProbs = TRUE, savePredictions = TRUE, index = folds, allowParallel = TRUE)
tune_grid <- expand.grid(interaction.depth = c(1, 3, 5), n.trees = (1:50) * 50, shrinkage = c(0.01, 0.001), n.minobsinnode = 10)
gbm_gridsearch <- train(Category~., data = train, method = "gbm", tuneGrid= tune_grid, trControl = control)
gbm_gridsearch
plot(gbm_gridsearch)
predictions <- predict(gbm_gridsearch, newdata = test[,2:13])
Accuracy(predictions, test$Category)
ConfusionMatrix(predictions, test$Category)
