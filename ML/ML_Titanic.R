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
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)


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

##Segunda parte__________________________________________________________________________________-----------------
##Survival by fare - LDA and QDA________________________

