install.packages(titanic)
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  factor(Survived, levels = c('0','1'))  
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

##Primera Parte_________________________________________________________________________----
nrow(titanic_clean)
set.seed(42, sample.kind="Rounding")
index_train <- createDataPartition(titanic_clean$Survived, times = 1, p=0.2, list = FALSE)
test_set <- titanic_clean[index_train, ]
train_set <- titanic_clean[-index_train, ]

nrow(test_set)
nrow(train_set)

Survived_table <- table(train_set$Survived)
Survived_table
Survived_table/sum(Survived_table)
275/712

#Dummiest model__________________________________________________________________
set.seed(3, sample.kind="Rounding")
Dummiest_survived <- sample(c(0,1),nrow(test_set),replace = TRUE)
Dummiest_survived <- factor(Dummiest_survived, levels = c('0','1'))
y_hat <- factor(test_set$Survived, levels = c('0','1'))
confusionMatrix(y_hat,Dummiest_survived)$overall["Accuracy"]
mean(y_hat==Dummiest_survived)

##Predicting survival by sex_______________________________________________________
SexSur_Table <- table(train_set$Survived,train_set$Sex)
SexSur_Table
SexSur_Table[2]/colSums(SexSur_Table)[1]
SexSur_Table[4]/colSums(SexSur_Table)[2]

train_set %>% group_by(Sex) %>% 
  summarise('Survived' = mean(Survived==1))

# there is somenthing differente
Sex_survived <- ifelse(test_set$Sex == "female", 1 ,0)
Sex_survived <- factor(Sex_survived, levels = c('0','1'))
confusionMatrix(data = Sex_survived,reference = y_hat)$overall["Accuracy"]

SexSur_Table <- table(test_set$Survived,test_set$Sex)
SexSur_Table
SexSur_Table[2]/colSums(SexSur_Table)[1]
SexSur_Table[4]/colSums(SexSur_Table)[2]

##Predicting survival by passenger class___________________________________________________
PclassSur_Table <- table(train_set$Survived,train_set$Pclass)
PclassSur_Table
PclassSur_Table[2]/colSums(PclassSur_Table)[1]
PclassSur_Table[4]/colSums(PclassSur_Table)[2]
PclassSur_Table[6]/colSums(PclassSur_Table)[3]

train_set %>% group_by(Pclass) %>% 
  summarise('Survived' = mean(Survived==1))

Pclass_survived <- ifelse(test_set$Pclass == 1, 1,0)
Pclass_survived <- factor(Pclass_survived, levels = c('0','1'))
confusionMatrix(y_hat,Pclass_survived)$overall["Accuracy"]


###Predicting survival by sex and PClass
Sex_Pclass <- train_set %>% mutate("Sex_Pclass" = paste(Sex,Pclass,sep = ""))
Sex_PclassSur_Table <- table(Sex_Pclass$Survived,Sex_Pclass$Sex_Pclass)
Sex_PclassSur_Table
train_set %>% group_by(Sex, Pclass) %>%
summarise("Survived" = mean(Survived == 1))

names(Sex_Pclass)
names(test_Sex_Pclass)

confusionMatrix(reference = y_hat,data = Sex_Pclass_survived)
confusionMatrix(y_hat,Pclass_survived)
confusionMatrix(y_hat,Sex_survived)

train_set %>% group_by(Sex, Pclass) %>%
  summarise("Survival" = mean(Survived == 1))

test_Sex_Pclass <- test_set %>% mutate("Sex_Pclass" = paste(Sex,Pclass,sep = ""))
test_Sex_Pclass %>% group_by(Sex, Pclass) %>%
  summarise("Survival" = mean(Survived == 1))

test_Sex_Pclass %>% group_by(Sex_Pclass) %>%
  summarise("Survival" = mean(Survived == 1))

Sex_Pclass_survived <- ifelse(test_Sex_Pclass$Sex_Pclass == "female1" |
test_Sex_Pclass$Sex_Pclass == "female2",
1,0)
Sex_Pclass_survived <- factor(Sex_Pclass_survived, levels = c('0','1'))
confusionMatrix(y_hat,Sex_Pclass_survived)$overall["Accuracy"]
confusionMatrix(y_hat,Sex_Pclass_survived)

mean(y_hat==Sex_survived)
mean(y_hat==Pclass_survived)
mean(y_hat==Sex_Pclass_survived)

sensitivity(reference = y_hat, data = Sex_Pclass_survived)
sensitivity(reference = y_hat,data = Sex_survived)
sensitivity(reference = y_hat,data = Pclass_survived)

specificity(data = Sex_Pclass_survived,reference = y_hat)
specificity(data = Sex_survived,reference = y_hat)
specificity(data = Pclass_survived,reference = y_hat)

F_meas(Sex_Pclass_survived,y_hat)
F_meas(Sex_survived,y_hat)
F_meas(Pclass_survived,y_hat)

confusionMatrix(Sex_Pclass_survived,y_hat)
confusionMatrix(Sex_survived,y_hat)
confusionMatrix(Pclass_survived,y_hat)

confusionMatrix(data = Sex_Pclass_survived,reference = y_hat)
confusionMatrix(data = Sex_survived,reference = y_hat)
confusionMatrix(data = Pclass_survived,reference = y_hat)

confusionMatrix(data = Sex_Pclass_survived,reference = y_hat)$byClass["Balanced Accuracy"]
confusionMatrix(data = Sex_survived,reference = y_hat)$byClass["Balanced Accuracy"]
confusionMatrix(data = Pclass_survived,reference = y_hat)$byClass["Balanced Accuracy"]

confusionMatrix(Sex_Pclass_survived,y_hat)$byClass["Balanced Accuracy"]
confusionMatrix(Sex_survived,y_hat)$byClass["Balanced Accuracy"]
confusionMatrix(Pclass_survived,y_hat)$byClass["Balanced Accuracy"]

##Segunda parte________________________________________________________________________-----
##Survival by fare - LDA and QDA________________________
set.seed(1, sample.kind = "Rounding")
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set, na.action = na.fail, contrasts = NULL)
y_hat <- predict(train_qda, test_set)
y_hat <- factor(y_hat, levels = c('0','1'))
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]

set.seed(1, sample.kind = "Rounding")
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set, na.action = na.fail, contrasts = NULL)
y_hat <- predict(train_lda, test_set)
y_hat <- factor(y_hat, levels = c('0','1'))
confusionMatrix(data = y_hat, reference = test_set$Survived)$overall["Accuracy"]

titanic_clean %>% ggplot(aes(Fare))+
  geom_histogram(binwidth = 10, fill='green', alpha=0.5)+
  ggtitle('Tarifa')

##Logistic regression models_____________________________
set.seed(1, sample.kind = "Rounding")
fit_glm <- glm(Survived ~ Age, data=train_set, family = "binomial")
p_hat_glm <- predict(fit_glm, test_set,  type = "response")
plot(test_set$Survived,p_hat_glm)
  abline(h=0.385, col="blue")
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 1, 0), levels = c('0','1'))
confusionMatrix(data = y_hat_glm, reference = test_set$Survived)$overall["Accuracy"]

B <- seq(-1,0,0.005)
Age_Accracy <- sapply(B, function(B){
  y_hat_glm <- factor(ifelse(p_hat_glm > B, 1, 0), levels = c('0','1'))
  confusionMatrix(data = y_hat_glm, reference = test_set$Survived)$overall["Accuracy"]
  })
plot(B,Age_Accracy)


names(train_set)
set.seed(1, sample.kind = "Rounding")
fit_glm <- glm(Survived ~ Sex+Pclass+Age+Fare, data=train_set, family = "binomial")
p_hat_glm <- predict(fit_glm, test_set)
plot(test_set$Survived,p_hat_glm)
y_hat_glm <- factor(ifelse(p_hat_glm > 0, 1, 0))
confusionMatrix(data = y_hat_glm, reference = test_set$Survived)$overall["Accuracy"]

set.seed(1, sample.kind = "Rounding")
fit_glm <- glm(Survived ~ ., data=train_set, family = "binomial")
p_hat_glm <- predict(fit_glm, test_set)
plot(test_set$Survived,p_hat_glm)
y_hat_glm <- factor(ifelse(p_hat_glm > 0, 1, 0))
confusionMatrix(data = y_hat_glm, reference = test_set$Survived)$overall["Accuracy"]

###kNN model__________________________________________
set.seed(6, sample.kind = "Rounding")
fit_knn <- train(Survived ~ .,
                 method = "knn",
                 data = as.data.frame(train_set),
                 tuneGrid = data.frame(k = seq(3, 51, 2)))
ggplot(fit_knn, highlight = TRUE)
fit_knn$bestTune

fit_knn <- knn3(Survived ~ ., data = train_set, k=fit_knn$bestTune)
y_hat_knn <- predict(fit_knn, test_set, type = "class")
y_hat_knn <- factor(y_hat_knn, levels = c('0','1'))
confusionMatrix(data = y_hat_knn, reference = test_set$Survived)$overall["Accuracy"]

###Cross-validation__________________________________
set.seed(8, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(Survived ~ ., method = "knn", 
                      data = train_set,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

fit_knn <- knn3(Survived ~ ., data = train_set, k=train_knn_cv$bestTune)
y_hat_knn <- predict(fit_knn, test_set, type = "class")
y_hat_knn <- factor(y_hat_knn, levels = c('0','1'))
confusionMatrix(data = y_hat_knn, reference = test_set$Survived)$overall["Accuracy"]

##Classification tree model____________________________
set.seed(10, sample.kind = "Rounding")
train_rpart <- train(Survived ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp =seq(0, 0.05, 0.002)),
                     data = train_set)
plot(train_rpart)
train_rpart$bestTune
# compute accuracy
confusionMatrix(predict(train_rpart, test_set), test_set$Survived)$overall["Accuracy"]
tree_terms <- as.character(unique(train_rpart$finalModel$frame$var[!(train_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

fit_rpart <- rpart(Survived ~ ., data = train_set,cp = 0.02)
y_hat_rpart <- predict(fit_rpart, test_set, type = "class")
y_hat_rpart <- factor(y_hat_rpart, levels = c('0','1'))
confusionMatrix(y_hat_rpart, test_set$Survived)$overall["Accuracy"]


# visualize the splits
plot(fit_rpart, margin = 0.1)
text(fit_rpart, cex = 0.75)


example <- test_set[1,2:9]
example <- rbind(example,c("male","",28,"","","",""))
example <- rbind(example,c("female",2,"","","","",""))
example <- rbind(example,c("female",3,"",8,"","",""))
example <- rbind(example,c("male","",5,"","","",""))
example <- rbind(example,c("female",3,"",25,"","",""))
example <- rbind(example,c("female",1,17,"","","",""))
example <- rbind(example,c("male",1,17,"","","",""))

example$Pclass <- as.integer(example$Pclass)
example$Age <- as.numeric(example$Age)
example$Fare <- as.numeric(example$Fare)
example$SibSp <- as.integer(example$SibSp)
example$Parch <- as.integer(example$Parch)
example$FamilySize <- as.numeric(example$FamilySize)
str(example)
example
survived_example <- predict(fit_rpart,newdata = example, type = "class")
survived_example

our_test_set <- data.frame(
  Sex = c("male", "female", "female", "male", "female", "female", "male"),
  Age = as.numeric(c(28,"","",5,"",17,17)),
  Pclass = as.integer(c("",2,3,"",3,1,1)),
  Fare =  as.numeric(c("","",8.00,"",25.00,"","")),
  SibSp = as.integer(rep(1,7)),
  Parch = as.integer(rep(1,7)),
  FamilySize = rep(1,7),
  Embarked = factor( sample(levels(test_set$Embarked),size = 7, replace = T) ) )

survived_example <- predict(fit_rpart,our_test_set, type = "class")
print(survived_example)

#Random forest model________________________________________________________
library(randomForest)
set.seed(14, sample.kind = "Rounding")
train_rf <- train(Survived ~ .,
                     method = "rf",
                     tuneGrid = data.frame(mtry = seq(1:7)),
                     data = train_set,
                     ntree = 100)
plot(train_rf, highlight = TRUE)
train_rf$bestTune
confusionMatrix(predict(train_rf, test_set), test_set$Survived)$overall["Accuracy"]
varImp(train_rf)

fit_rf <- randomForest(Survived ~ ., data = train_set, mtry = 2)
y_hat_rf <- predict(fit_rf, test_set, type = "class")
y_hat_rf <- factor(y_hat_rf, levels = c('0','1'))
confusionMatrix(y_hat_rf, test_set$Survived)$overall["Accuracy"]

#survived_example <- predict(fit_rf,our_test_set, type = "prob")
#print(survived_example)
