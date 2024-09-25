#Setting working directory:
setwd("~/Documents/Conestoga/PA/R/Multivariate")

#load libraries
if(!require(caret)){install.packages("caret")}
library(caret)
if(!require(tidyverse)){install.packages("tidyverse")}
library(tidyverse)
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
if(!require(rsample)){install.packages("rsample")}
library(rsample)
if(!require(rpart.plot)){install.packages("rpart.plot")}
library(rpart.plot)
if(!require(ROSE)){install.packages("ROSE")}
library(ROSE)
if(!require(randomForest)){install.packages("randomForest")}
library(randomForest)
if(!require(class)){install.packages("class")}
library(class)





#Load a dataset
heart_data <- read.csv('heart_disease_2020.csv')
head(heart_data)

set.seed(123)

#Fixing datatypes

heart_data$HeartDisease<-as.factor(heart_data$HeartDisease)
heart_data$Smoking<-as.factor(heart_data$Smoking)
heart_data$AlcoholDrinking<-as.factor(heart_data$AlcoholDrinking)
heart_data$Stroke<-as.factor(heart_data$Stroke)
heart_data$DiffWalking<-as.factor(heart_data$DiffWalking)
heart_data$Sex<-as.factor(heart_data$Sex)
heart_data$AgeCategory<-as.factor(heart_data$AgeCategory)
heart_data$Race<-as.factor(heart_data$Race)
heart_data$Diabetic<-factor(heart_data$Diabetic,labels=c("No","BL","Yes","YesPreg")) # Renaming levels
heart_data$PhysicalActivity<-as.factor(heart_data$PhysicalActivity)
heart_data$GenHealth<-factor(heart_data$GenHealth,labels=c(5,2,3,1,4)) # Recoding to Likert scale (1-5)
heart_data$Asthma<-as.factor(heart_data$Asthma)
heart_data$KidneyDisease<-as.factor(heart_data$KidneyDisease)
heart_data$SkinCancer <-as.factor(heart_data$SkinCancer)



#summary of data
summary(heart_data)



#spliting data into test and train
set.seed(321)
s<-sort(sample(nrow(heart_data),nrow(heart_data)*.8))
train<-heart_data[s,]
test<-heart_data[-s,]

#checking is data balance or not
table(train$HeartDisease)

#balancing data
train <-ovun.sample(HeartDisease ~ ., data = train, method = "under", N = (21869+21869), seed = 3)$data



#Single Decision Tree

HeartDisease_model <- train(HeartDisease ~ .,
                       data = train,
                       method = "rpart"
)
rpart.plot(HeartDisease_model$finalModel)

#creating confusion matrix for test data using tree model
confusionMatrix(predict(HeartDisease_model,test),test$HeartDisease)

# Random Forest model
#finding number of tree which minimize the error
rf<-randomForest(HeartDisease~.,ntree=500,data=train)
plot(rf$err.rate[,1],type="l",main="Random Forest Error Rate",xlab="Number of Trees")

# Random Forest model with hyper parameter
rf_model <- train(HeartDisease ~ .,
                  data = train,
                  method = "ranger",
                  trControl = trainControl(method="cv", number = 5,
                                           verboseIter = TRUE, classProbs = TRUE),
                  num.trees = 300,
                  importance = "impurity"
)
rf_model
confusionMatrix(predict(rf_model,test),test$HeartDisease)





