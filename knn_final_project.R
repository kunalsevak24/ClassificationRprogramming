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
heart_data$GenHealth<-factor(heart_data$GenHealth,labels=c(5,2,3,1,4)) # Recoding to Like scale (1-5)
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



#knn

knn_HeartDisease_model <- train(
  HeartDisease ~ . ,
  data = train,
  method = "knn",
  preProcess = c("center","scale"),
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = data.frame(k = c(8,9,10))
)
knn_HeartDisease_model
confusionMatrix(predict(knn_HeartDisease_model,test),test$HeartDisease)


#GLM - Single variable

logistic_model <- glm(HeartDisease ~ Smoking, 
                      data = train, 
                      family = "binomial")
coef(logistic_model)
exp(coef(logistic_model))

confusionMatrix(factor(predict(logistic_model,test[3],type="response")>0.5),
                factor(ifelse(test$HeartDisease == "Yes",TRUE,FALSE)))


# GML - Multi-variable model
bigger_Heart_model <- glm(HeartDisease ~ . - AgeCategory - GenHealth - SleepTime,
                          data = train, 
                          family = "binomial")
coef(bigger_Heart_model)
exp(coef(bigger_Heart_model))
pred <- predict(bigger_Heart_model,test,type="response")
confusionMatrix(factor(pred > 0.5),factor(ifelse(test$HeartDisease == "Yes",TRUE,FALSE)))

big_prediction <- data.frame(BMI=C(24),
                             Smoking= c("Yes"),
                             AlcoholDrinking = c("Yes"),
                             Stroke= c("Yes"),
                             PhysicalHealth = c(25),
                             MentalHealth = c(5),
                             DiffWalking = c("No"),
                             Sex = c("Male"),
                             Race = c("White"),
                             Diabetic = c("Yes"),
                             PhysicalActivity = c("Yes"),
                             Asthma = c("Yes"),
                             KidneyDisease = c("Yes"),
                             SkinCancer = c("Yes")
                             )
#predict(bigger_Heart_model, big_prediction, type="response")

logistic_diabetes_model <- train(
  HeartDisease ~ . - AgeCategory - GenHealth - SleepTime,
  data = diabetesData,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
logistic_diabetes_model

