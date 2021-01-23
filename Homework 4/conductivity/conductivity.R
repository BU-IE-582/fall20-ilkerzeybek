#Loading necessary packages
library(GGally)
library(corrplot)
library(caret)
library(glmnet)

#Reading the data and setting the seed
set.seed(582)
cond <- read.csv("conductivity.csv")

#No missing data
sum(is.na(cond))

#Filtering the irrelevant features
nearZeroVar(cond, freqCut = 90/10, names = T)
cond <- subset(cond, select = -c(range_FusionHeat, range_ThermalConductivity))
tmp <- cor(cond[, 1:42])
tmp[upper.tri(tmp)] <- 0
diag(tmp) <- 0
data.new <- cond[,!apply(tmp, 2, function(x) any(abs(x) > 0.80))]
data.new$CriticalTemp <- cond$critical_temp

#Split train test
train_indices <- sample(1:21263, 21263*0.7)
train <- data.new[train_indices,]
test <- data.new[-train_indices,]

#Tune Lambda parameter and fit lasso regression
lambdas <- 10^seq(2, -3, by = -.1)
cv <- cv.glmnet(as.matrix(train[, -20]), as.matrix(train[, 20]), lambda = lambdas,
                nfolds = 10)
cv
min_lambda <- cv$lambda.min
lasso_model <- glmnet(as.matrix(train[, -20]),
                      as.matrix(train[, 20]), alpha = 1,
                      lambda = min_lambda)
#Lasso regression evaluation
eval_results <- function(true, predicted, df) {
  SSE <- sum((predicted - true)^2)
  SST <- sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE <- sqrt(SSE/nrow(df))
  data.frame(
    RMSE <- RMSE,
    Rsquare <- R_square
  )
}
lasso_predictions <- predict(lasso_model, newx = as.matrix(test[, -20]))
eval_results(test[, 20], lasso_predictions, test)
