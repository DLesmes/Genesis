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
Survived_table[2]/sum(Survived_table)

#Dummiest model__________________________________________________________________
set.seed(3, sample.kind="Rounding")
Dummiest_survived <- sample(c(0,1),nrow(test_set),replace = TRUE)
Dummiest_survived <- factor(Dummiest_survived, levels = c('0','1'))
y_hat <- factor(test_set$Survived, levels = c('0','1'))
confusionMatrix(y_hat,Dummiest_survived)$overall["Accuracy"]
mean(y_hat==Dummiest_survived)

##Predicting survival by sex________________________________________________________________
SexSur_Table <- table(train_set$Survived,train_set$Sex)
SexSur_Table
SexSur_Table[2]/colSums(SexSur_Table)[1]
SexSur_Table[4]/colSums(SexSur_Table)[2]

