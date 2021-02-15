require(caret)
library(ggplot2)
library(dplyr)
library(yardstick)
library(broom)
library(data.table)
library(stringr)
library(tidyr)
library(purrr)
require(glmnet)
library(WRTDStidal)
library(varhandle)
library(cvAUC)
library(pROC)
library(parallel)
library(doParallel)
library(ISLR)
library(klaR)
library(e1071)
library(ada)

train <- fread("C:/Users/Abdulsamed/Desktop/IE582/IE582_Fall20_ProjectTrain.csv")
test <- fread("C:/Users/Abdulsamed/Desktop/IE582/IE582_Fall20_ProjectTest.csv")

set.seed(42)
myFolds <- createMultiFolds(train$y,k=10,times = 3)


myControl<-trainControl(method="cv",number=10,verboseIter=TRUE,summaryFunction=twoClassSummary,classProbs=TRUE,savePredictions=TRUE,index=myFolds,allowParallel=TRUE)

myGridGlmnet <- expand.grid(alpha= 0:1, lambda = c(seq(0.0001,0.1,length=10),seq(0.15,5,length=10)))
myGridForest <- expand.grid(mtry = c(2,5,7,10,15,30,60),splitrule = c("gini","extratrees"), min.node.size=c(1,3,5,10))
myGridGbm <- expand.grid(interaction.depth=c(1,3,5), n.trees = (1:50)*50,shrinkage=c(0.01, 0.001), n.minobsinnode=10)

glm_model0 <- train(y~.,train, method="glm",trControl = myControl,preProcess= c("center","scale")) 
glm_model1 <- train(y~.,train, method="glm",trControl = myControl,preProcess= c("nzv","center","scale")) 
glm_model2 <- train(y~.,train, method="glm",trControl = myControl,preProcess= c("nzv","center","scale","pca")) 
glm_model3 <- train(y~.,train, method="glm",trControl = myControl,preProcess= c("nzv","center","scale","spatialSign"))


penalized_model0 <- train(y~.,train, method="glmnet",tuneGrid=myGridGlmnet ,trControl = myControl,preProcess= c("center","scale"))
penalized_model1 <- train(y~.,train, method="glmnet",tuneGrid=myGridGlmnet ,trControl = myControl,preProcess= c("nzv","center","scale"))
penalized_model2 <- train(y~.,train, method="glmnet",tuneGrid=myGridGlmnet ,trControl = myControl,preProcess= c("nzv","center","scale","pca"))
penalized_model3 <- train(y~.,train, method="glmnet",tuneGrid=myGridGlmnet ,trControl = myControl,preProcess= c("nzv","center","scale","spatialSign"))

plot(penalized_model0)

forest_model0 <- train(y~.,train, method="ranger",tuneGrid=myGridForest,trControl = myControl)
plot(forest_model0)

knn_model0 <- train(y~.,data=train, method = "knn", trControl = myControl,preProcess = c("nzv","center","scale"), tuneLength = 20)
plot(knn_model0)

nb_model0 = train(y~.,data=train, method = "nb", trControl = myControl,preProcess = c("nzv","center","scale"))
plot(nb_model0)


ada_model0 =  train(y~.,data=train, method = "ada", trControl = myControl,preProcess = c("zv"))

gbm_model0 = train(y~.,data=train, method = "gbm",tuneGrid= myGridGbm ,trControl = myControl,preProcess = c("zv"))

#Imbalanced Class Weighting
model_weights <- ifelse(train$y == "a",
                        (1/table(train$y)[1]) * 0.5,
                        (1/table(train$y)[2]) * 0.5)


glm_model4 <- train(y~.,train, method="glm",trControl = myControl,weights =model_weights,preProcess= c("center","scale")) 
glm_model5 <- train(y~.,train, method="glm",trControl = myControl,weights =model_weights,preProcess= c("nzv","center","scale")) 
glm_model6 <- train(y~.,train, method="glm",trControl = myControl,weights =model_weights,preProcess= c("nzv","center","scale","pca")) 
glm_model7 <- train(y~.,train, method="glm",trControl = myControl,weights =model_weights,preProcess= c("nzv","center","scale","spatialSign"))


penalized_model4<-train(y~.,train,method="glmnet",tuneGrid=myGridGlmnet,weights=model_weights,trControl=myControl,preProcess=c("center","scale"))
penalized_model5<-train(y~.,train,method="glmnet",tuneGrid=myGridGlmnet,weights=model_weights,trControl=myControl,preProcess=c("nzv","center","scale"))
penalized_model6<-train(y~.,train,method="glmnet",tuneGrid=myGridGlmnet,weights=model_weights,trControl=myControl,preProcess=c("nzv","center","scale","pca"))
penalized_model7<-train(y~.,train,method="glmnet",tuneGrid=myGridGlmnet,weights=model_weights,trControl=myControl,preProcess=c("nzv","center","scale","spatialSign"))

forest_model1 <- train(y~.,train, method="ranger",tuneGrid=myGridForest,weights=model_weights,trControl = myControl)
ada_model1 <-   train(y~.,data=train, method = "ada", weights=model_weights, trControl = myControl,preProcess = c("zv"))

gbm_model1 = train(y~.,data=train, method = "gbm",weights=model_weights,tuneGrid= myGridGbm ,trControl = myControl,preProcess = c("zv"))

gbm_model2 = train(y~.,data=train, method = "gbm",weights=model_weights,tuneGrid= myGridGbm ,trControl = myControl,preProcess = c("zv","pca"))

gbm_model3 = train(y~.,data=train, method = "gbm",weights=model_weights,tuneGrid= myGridGbm ,trControl = myControl,preProcess = c("nzv"))

gbm_model4 = train(y~.,data=train, method = "gbm",weights=model_weights,tuneGrid= myGridGbm ,trControl = myControl)


##MODELLER YUKARDA, BUNDAN SONRASI ANALÝZ

model_list <- list(#glm0=glm_model0,glm1=glm_model1,glm2=glm_model2,glm3=glm_model3,
                   glm4=glm_model4,glm5=glm_model5,glm6=glm_model6,glm7=glm_model7,
                   glmnet0=penalized_model0,glmnet1=penalized_model1,glmnet2=penalized_model2,glmnet3=penalized_model3,glmnet4=penalized_model4,
                   glmnet5=penalized_model5,glmnet6=penalized_model6,glmnet7=penalized_model7,
                   forest0=forest_model0,forest1=forest_model1,
                   #knn0 = knn_model0,
                   nb0= nb_model0,
                   ada0 = ada_model0,ada1 = ada_model1,
                   gbm0 = gbm_model0, gbm1 = gbm_model1, gbm2 = gbm_model2,gbm3=gbm_model3,gbm4=gbm_model4)



resamp <- resamples(model_list)
summary(resamp)

bwplot(resamp,metric ="ROC")
bwplot(resamp,metric ="Sens")
bwplot(resamp,metric ="Spec")

dotplot(resamp,metric ="ROC")
dotplot(resamp,metric ="Sens")
dotplot(resamp,metric ="Spec")


##Comparison forest1 vs glmnet4
max(forest_model1[["results"]]$ROC) - max(penalized_model4[["results"]]$ROC)
(forest_model1[["results"]]$Sens[19]+forest_model1[["results"]]$Spec[19]) - (penalized_model4[["results"]]$Sens[22]+penalized_model4[["results"]]$Spec[22])

#Comparison gbm1 vs glmnet4
max(gbm_model1[["results"]]$ROC) - max(penalized_model4[["results"]]$ROC)
(gbm_model1[["results"]]$Sens[54]+gbm_model1[["results"]]$Spec[54]) - (penalized_model4[["results"]]$Sens[22]+penalized_model4[["results"]]$Spec[22])

#Comparison gbm1 vs forest1
max(gbm_model1[["results"]]$ROC) - max(forest_model1[["results"]]$ROC)
(gbm_model1[["results"]]$Sens[54]+gbm_model1[["results"]]$Spec[54]) - (forest_model1[["results"]]$Sens[19]+forest_model1[["results"]]$Spec[19])

#Comparison gbm1 vs gbm0
max(gbm_model1[["results"]]$ROC) - max(gbm_model0[["results"]]$ROC)
(gbm_model1[["results"]]$Sens[54]+gbm_model1[["results"]]$Spec[54]) - (gbm_model0[["results"]]$Sens[72]+gbm_model0[["results"]]$Spec[72])

#Comparison gbm1 vs gbm4
max(gbm_model1[["results"]]$ROC) - max(gbm_model4[["results"]]$ROC)
(gbm_model1[["results"]]$Sens[54]+gbm_model1[["results"]]$Spec[54]) - (gbm_model4[["results"]]$Sens[76]+gbm_model4[["results"]]$Spec[76])



#1-((gbm_model4[["results"]]$Sens[76]+gbm_model4[["results"]]$Spec[76])/2)


predictions <- predict(penalized_model4,test,type ="prob")
predictions <- predictions %>% mutate(prediction = case_when(a > 0.5~"a", TRUE ~"b"))
table(predictions$prediction)
table(train$y)


predictions2 <- predict(forest_model1,test,type ="prob")
predictions2 <- predictions2 %>% mutate(prediction = case_when(a > 0.5~"a", TRUE ~"b"))
table(predictions2$prediction)
table(train$y)


predictions3 <- predict(gbm_model1,test,type ="prob")
predictions3 <- predictions3 %>% mutate(prediction = case_when(a > 0.5~"a", TRUE ~"b"))
table(predictions3$prediction)
table(train$y)


predictions4 <- predict(gbm_model4,test,type ="prob")
predictions4 <- predictions4 %>% mutate(prediction = case_when(a > 0.5~"a", TRUE ~"b"))
table(predictions4$prediction)
table(train$y)




results = data.table()
for(i in model_list)
{
  k <- which.max(i[["results"]]$ROC)
  temp<- max(i[["results"]]$ROC) - (1- 0.5*i[["results"]]$Sens[k]-0.5*i[["results"]]$Spec[k])
  results = rbind(results,temp)
}
results$model <- names(model_list)
results %>% arrange(desc(x))

