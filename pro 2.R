library(ISLR2)
library(tidyverse)
library(caret)
library(mclust)
library(rsample)



logistic_model <- glm(HeartDisease ~ Smoking, 
                    data = heart_Disease_Data, 
                    family = "binomial")
coef(logistic_model)
exp(coef(logistic_model))
Smoking_prediction <- data.frame(Smoking = c("Yes"))
predict(logistic_model, Smoking_prediction, type="response")


bigger_Heart_model <- glm(HeartDisease ~ . - AgeCategory - GenHealth - SleepTime,
                          data = diabetesData, 
                          family = "binomial")
coef(bigger_Heart_model)
exp(coef(bigger_Heart_model))


big_prediction <- data.frame(BMI=C(24),
                             Smoking= c("Yes"),
                             AlcoholDrinking = c("Yes"),
                             Stroke= c("Yes"),
                             PhysicalHealth = c(25),
                             MentalHealth = c(5),
                             DiffWalking = c("No"),
                             Sex = c("Male),
                             Race = c("White"),
                             Diabetic = c("Yes"),
                             PhysicalActivity = c("Yes"),
                             Asthma = c("Yes"),
                             KidneyDisease = c("Yes"),
                             SkinCancer = c("Yes")
                             )
predict(bigger_Heart_model, big_prediction, type="response")

logistic_diabetes_model <- train(
  HeartDisease ~ . - AgeCategory - GenHealth - SleepTime,
  data = diabetesData,
  method = "glm",
  family = "binomial",
  trControl = trainControl(method = "cv", number = 10)
)
logistic_diabetes_model



