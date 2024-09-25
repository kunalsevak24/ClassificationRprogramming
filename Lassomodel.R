library(caret)
library(mclust)
library(rsample)
library(glmnet)

tune_grid = expand.grid(alpha = 1, lambda = seq(0.0001, 1, length = 10))

lassomodel <- train(
  HeartDisease ~ .,
  data = train,
  method = "glmnet",
  family = "binomial",
  tuneGrid = tune_grid,
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = null
)
lassomodel
# confusion Matrix
confusionMatrix(predict(lassomodel,test),test$HeartDisease)



tune_grid = expand.grid(alpha = 0, lambda = seq(0.0001, 1, length = 10))

ridgemodel <- train(
  HeartDisease ~ .,
  data = train,
  method = "glmnet",
  family = "binomial",
  tuneGrid = tune_grid,
  preProcess = c("center", "scale"),
  trControl = trainControl(method = "cv", number = 10)
)

ridgemodel

confusionMatrix(predict(ridgemodel,test),test$HeartDisease)
