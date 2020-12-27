#Load the libraries
library(lubridate)
library(dplyr)
library(tidyr)
library(MLmetrics)
library(glmnet)
library(ggplot2)
library(plotly)
#Set the seed for reproducibility
set.seed(582)
#Read the data
data <- read.csv("data.csv")
#Change the column names
colnames(data) <- c("Date", "Hour", "Consumption")
#Format the dates and hours
data$Date <- dmy(data$Date)
data$Hour <- hm(data$Hour)
data$Hour <- data$Hour$hour
#Format the consumption
data$Consumption <- gsub(".", "", data$Consumption, fixed = T)
data$Consumption <- gsub(",", ".", data$Consumption, fixed = T)
data$Consumption <- as.numeric(data$Consumption)
#Check if all entries are correct
sum(duplicated(data))
#The sum is 1, so there is a True entry, which indicates there is a duplicated row.
which(duplicated(data))
data[2069,]
#The 2069th row is duplicated, lets investigate this.
duplicate <- data[data$Date == "2016-03-27",]
duplicate
#The error is due to the the daylight savings program Turkey used to apply in those years.
#To correct the error, we will change the hour from 4 to 3 and replace the Consumption column with zero.
data$Hour[2068] <- 3
data$Consumption[2068] <- 0
data[data$Date == "2016-03-27",]

#TASK 1
#Add 48 hour and 168 hour lags as columns
data <- mutate(data, lag48 = lag(Consumption, 48))
data <- mutate(data, lag168 = lag(Consumption, 168))
#Split train and test
train <- data[data$Date < "2020-11-01",]
test <- data[data$Date >= "2020-11-01",]
#MAPE calculations
MAPE(test$lag48, test$Consumption)
MAPE(test$lag168, test$Consumption)
#Result: Lag168 works better with half of lag48 error.

#TASK 2
#Build linear regression model with the lags as features.
train <- train[complete.cases(train),]
fit <- lm(Consumption ~ lag48 + lag168, data = train)
summary(fit)
predictions <- predict(fit, newdata = test)
MAPE(predictions, test$Consumption)
#Result: Lag168 performs better than linear regression model with lags as features.

#TASK 3
#Build Linear regression model for each hour
mape_hour <- data.frame(Hour = 0:23, MAPE = rep(NA, 24))
for(i in 0:23){
  fit_hour <- lm(Consumption ~ lag48 + lag168, data = train[train$Hour == i,])
  predictions_hour <- predict(fit_hour, newdata = test[test$Hour == i,])
  mape_hour$MAPE[i+1] <- MAPE(predictions_hour, test[test$Hour == i,]$Consumption)
}
mape_hour[mape_hour$MAPE < MAPE(test$lag168, test$Consumption),]
#Result: We can clearly see that the morning hours and evening hours are grouped together,
#Their MAPE's are lower than the naive approach, so hourly seasonality is more important
#Than weekly seasonality.

#TASK 4
#Penalized regression
wide_48 <- data[, 1:3] %>% pivot_wider(names_from = Hour, values_from = Consumption, names_prefix = "lag_day2_")
wide_168 <- data[, 1:3] %>% pivot_wider(names_from = Hour, values_from = Consumption, names_prefix = "lag_day7_")
wide_48 <- wide_48[, -1] %>% mutate_all(funs(lag), n = 2)
wide_48$Date <- wide_168$Date
wide_48 <- wide_48[, c(25, 1:24)]
wide_168 <- wide_168[, -1] %>% mutate_all(funs(lag), n = 7)
wide_168$Date <- wide_48$Date
wide_168 <- wide_168[, c(25, 1:24)]
wide_all <- data[, 1:3] %>% inner_join(wide_48, by = "Date") %>% inner_join(wide_168, by = "Date")
wide_all <- wide_all[complete.cases(wide_all),]
train_lasso <- wide_all[wide_all$Date < "2020-11-01", ]
test_lasso <- wide_all[wide_all$Date >= "2020-11-01", ]
cv <- cv.glmnet(as.matrix(train_lasso[, -c(1,2,3)]), as.matrix(train_lasso[, 3]))
cv
plot(cv)
min_lambda <- cv$lambda.min
lasso_mape <- data.frame(Hour = 0:23, MAPE = rep(NA, 24))
for(i in 0:23){
  lasso_model <- glmnet(as.matrix(train_lasso[train_lasso$Hour == i, 4:51]),
                        as.matrix(train_lasso[train_lasso$Hour == i, 3]), alpha = 1,
                        lambda = min_lambda)
  lasso_predictions <- predict(lasso_model, newx = as.matrix(test_lasso[test_lasso$Hour == i, 4:51]))
  lasso_mape$MAPE[i+1] <- MAPE(lasso_predictions, as.matrix(test_lasso[test_lasso$Hour == i, 3]))
}
#Best MAPE metrics are achieved with lasso regression, therefore this approach is best until now.

#TASK 6
hourly <- c()
for(i in 0:23){
  hourly[i+1] <- paste("Hour ", i, " Linear Regression", sep = "")
}
lasso_hourly <- c()
for(i in 0:23){
  lasso_hourly[i+1] <- paste("Hour ", i, " Lasso Regression", sep = "")
}
mape <- data.frame(MAPE = c(MAPE(test$lag48, test$Consumption), MAPE(test$lag168, test$Consumption),
                            MAPE(predictions, test$Consumption), mape_hour$MAPE, lasso_mape$MAPE),
                   Model = c("2 days Lag Naive", "7 days Lag Naive", "2 days + 7 days Linear Regression",
                             hourly, lasso_hourly ))
g <- ggplot(mape, aes(y = MAPE)) +
  geom_boxplot()
g <- ggplotly(g)
g
mape_ordered <- mape[order(mape$MAPE),]
head(mape_ordered, 5)
#Result: Top 5 models are listed. Lasso regression approach works better compared to other approaches.
# Hour 3 Lasso Regression has the best MAPE metric.