#Loading necessary packages
library(caret)
library(ranger)
library(MLmetrics)
#Setting the seed and reading the data
set.seed(582)
data <- read.csv("spambase.csv")

#Changing output class to factor and renaming levels of it.
head(data, 5)
data$class <- as.factor(data$class)
levels(data$class) <- c("No", "Yes")

#Counts of not spam and spam mails
table(data$class)

#Split data into train and test
train_indices <- sample(1:4601, 4601*0.8)
train <- data[train_indices,]
test <- data[-train_indices,]

#Random Forest -> Grid Search mtry tuning
folds <- createMultiFolds(train$class, k = 10, times = 1)
control <- trainControl(method = "cv", number = 10, verboseIter = TRUE, summaryFunction = twoClassSummary,
                        classProbs = TRUE, savePredictions = TRUE, index = folds, allowParallel = TRUE)
tune_grid <- expand.grid(mtry = c(2, 5, 7, 9, 11, 13), splitrule = "gini", min.node.size = 5)
rf_gridsearch <- train(class ~., data = train, method = "ranger", metric = "Accuracy",
                       tuneGrid = tune_grid, trControl = control, num.trees = 500)
rf_gridsearch
plot(rf_gridsearch)
predictions <- predict(rf_gridsearch, newdata = test)
Accuracy(predictions, test$class)
ConfusionMatrix(predictions, test$class)

#Gradient Boosting -> depth, learning rate, and number of trees tuning
folds <- createMultiFolds(train$class, k = 10, times = 1)
control <- trainControl(method = "cv", number = 10, verboseIter = TRUE, summaryFunction = twoClassSummary,
                        classProbs = TRUE, savePredictions = TRUE, index = folds, allowParallel = TRUE)
tune_grid <- expand.grid(interaction.depth=c(1, 3, 5), n.trees = c(500, 1000), shrinkage=c(0.01, 0.001), n.minobsinnode = 10)
gbm_gridsearch <- train(class ~., data = train, method = "gbm", metric = "Accuracy", tuneGrid = tune_grid, trControl = control)
gbm_gridsearch
plot(gbm_gridsearch)
predictions <- predict(gbm_gridsearch, newdata = test)
Accuracy(predictions, test$class)
ConfusionMatrix(predictions, test$class)
