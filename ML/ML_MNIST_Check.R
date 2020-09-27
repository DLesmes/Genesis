models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
library(tidyverse)
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

length(models)
length(mnist_27$test$y)

i <- seq(1, 10)
pred_models <- sapply(i, function(i){
  predict(fits[i], mnist_27$test)
})
y_hat_Mx = matrix(unlist(pred_models), ncol=length(pred_models))

Accuracies <- sapply(i,function(i){
  confusionMatrix(factor(y_hat_Mx[,i]),mnist_27$test$y)$overall["Accuracy"]
})

mean_Accuracy <- mean(Accuracies)

p_hat_Mx <- ifelse(y_hat_Mx == 7, 1 ,0)
Majority <- rowSums(p_hat_Mx)/10
Majority_y_hat <- ifelse(Majority > 0.5, 7 ,2)
Ensamble_Accuracy <- confusionMatrix(factor(Majority_y_hat),mnist_27$test$y)$overall["Accuracy"]
Ensamble_Accuracy
names(Accuracies) <- models

index <- which(Accuracies > Ensamble_Accuracy)
length(index)
Accuracies[index]

Min_train_Accuracies <- sapply(i,function(i){
  min(fits[[i]]$results$Accuracy)
})
names(Min_train_Accuracies) <- models
Min_train_Accuracies
mean(Min_train_Accuracies)

index <- which(Min_train_Accuracies > 0.8)
length(index)
Accuracies[index]

Majority <- rowSums(p_hat_Mx[,index])/6
Majority_y_hat <- ifelse(Majority > 0.5, 7 ,2)
Better_Ensamble_Accuracy <- confusionMatrix(factor(Majority_y_hat),mnist_27$test$y)$overall["Accuracy"]
Better_Ensamble_Accuracy
