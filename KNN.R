library(tidyverse)
library(ISLR2)
library(caret)
library(mclust)
library(rsample)


diabetes <- read.csv("D:/Conestoga/Multivariate Stats/Projects/Group Project/diabetes.csv")

view(diabetes)

diabetes$Outcome <- factor(diabetes$Outcome, labels = c("Diabetic", "NonDiabetic"))

k_grid <- expand.grid(k = seq(1, 15))
knn_diabetes_model <- train(
  Outcome ~ . ,
  data = diabetes,
  method = "knn",
  preProcess = c("center","scale"),
  tuneGrid = k_grid,
  trControl = trainControl(method = "cv", number = 10)
)
knn_diabetes_model


