#Part1

library(dslabs)
library(dplyr)
library(lubridate)
library(caret)
library(tidyverse)
install.packages("e1071")
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat %>% filter(type == "online") %>% 
  count(sex) %>%
  mutate("Prop" = n/sum(n))

dat %>% filter(type == "inclass") %>% 
  count(sex) %>%
  mutate("Prop" = n/sum(n))

y_hat <- ifelse(dat$type == "online", "Male", "Female")%>% 
  factor()
y <- factor(dat$sex)
mean(y_hat == y)

confusionMatrix(data = y_hat, reference = y)
table(predicted = y_hat, actual = y)
table(y_hat,y)

#Part2
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
#train as test
train <- iris[test_index,]
test <- iris[-test_index,]

str(iris)
unique(iris$Species)
iris %>% group_by(Species) %>% 
  summarize(mean(Sepal.Length),mean(Sepal.Width),mean(Petal.Length),mean(Petal.Width))

#prediction para Sepal.lenght
cutoff_SL <- seq(round(min(iris$Sepal.Length),0),round(max(iris$Sepal.Length),0),0.1)
accuracy <- map_dbl(cutoff_SL, function(x){
  y_hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff_SL, accuracy) %>% 
  ggplot(aes(cutoff_SL, accuracy)) + 
  geom_point() + 
  geom_line()+
  ggtitle("Sepal.length")
max(accuracy)

best_cutoff <- cutoff_SL[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test$Sepal.Length > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat)
accuracy_SL <- mean(y_hat == factor(test$Species))

#prediction para Sepal.Width
cutoff_SW <- seq(round(min(iris$Sepal.Width),0),round(max(iris$Sepal.Width),0),0.1)
accuracy <- map_dbl(cutoff_SW, function(x){
  y_hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff_SW, accuracy) %>% 
  ggplot(aes(cutoff_SW, accuracy)) + 
  geom_point() + 
  geom_line()+
  ggtitle("Sepal.Width")
max(accuracy)

best_cutoff <- cutoff_SW[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test$Sepal.Width > best_cutoff, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat)
accuracy_SW <- mean(y_hat == factor(test$Species))

#prediction para Petal.Width
cutoff_PW <- seq(round(min(iris$Petal.Width),0),round(max(iris$Petal.Width),0),0.1)
accuracy <- map_dbl(cutoff_PW, function(x){
  y_hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff_PW, accuracy) %>% 
  ggplot(aes(cutoff_PW, accuracy)) + 
  geom_point() + 
  geom_line()+
  ggtitle("Petal.Width")
max(accuracy)

best_cutoff_PW <- cutoff_PW[which.max(accuracy)]
best_cutoff_PW

y_hat <- ifelse(test$Petal.Width > best_cutoff_PW, "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat)
accuracy_PW <- mean(y_hat == factor(test$Species))

#prediction para Petal.Length
cutoff_PL <- seq(round(min(iris$Petal.Length),0),round(max(iris$Petal.Length),0),0.1)
accuracy <- map_dbl(cutoff_PL, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
data.frame(cutoff_PL, accuracy) %>% 
  ggplot(aes(cutoff_PL, accuracy)) + 
  geom_point() + 
  geom_line()+
  ggtitle("Petal.Length")
max(accuracy)

best_cutoff_PL <- cutoff_PL[which.max(accuracy)]
best_cutoff_PL

y_hat <- ifelse(test$Petal.Length > best_cutoff_PL, "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
y_hat <- factor(y_hat)
accuracy_PL <- mean(y_hat == factor(test$Species))


plot(iris,pch=21,bg=iris$Species)

#Prediction para Pedal.Width Pedal.Length
y_hat <- if(train$Petal.Length > best_cutoff_PL | train$Petal.With > best_cutoff_PW)
  {"virginica"} else
  {"versicolor"} %>%
  factor(levels = levels(test$Species))
#y_hat <- factor(levels = levels(y_hat))
y_hat <- factor(y_hat)
accuracy_PLW <- mean(y_hat == factor(test$Species))


#Conditional probabilities Part1
#________________________________________________________________________________

#p(D|+)
(0.85*0.02)/((0.85*0.02)+(0.1*0.98))


set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#p(+)
((0.85*0.02)+(0.1*0.98))
#P(D|-)
0.02*0.15
#P(D|+)
(0.85*0.02)/((0.85*0.02)+(0.1*0.98))
#P(D|+)/P(D)
((0.85*0.02)/((0.85*0.02)+(0.1*0.98)))/0.02

#Conditional probabilities Part2
#________________________________________________________________________________

library(dslabs)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)



ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

plot(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)

#_____________________________________________________________
#Linear model comprehension
library(dslabs)
library(lubridate)
library(caret)
library(tidyverse)
install.packages("e1071")
#set.seed(1)
set.seed(1, sample.kind="Rounding")# if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

errors <- replicate(100, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x, data = train)
  y_hat <- predict(fit, test)

  RMSE(y_hat,test$y)
})
c(mean(errors), sd(errors))


#larger data set
#set.seed(1)
set.seed(1, sample.kind="Rounding")# if using R 3.6 or later
n <- c(100, 500, 1000, 5000, 10000)
#n <- c(100, 500)
sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  B <- 100
  errors <- replicate(B, {
    test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
    test <- dat[test_index,]
    train <- dat[-test_index,]
    fit <- lm(y ~ x, data = train)
    y_hat <- predict(fit, test)
    a <- RMSE(y_hat,test$y)
  })
  resolve <- c(mean(errors), sd(errors))
})

#with Corr higher
#set.seed(1)
set.seed(1, sample.kind="Rounding")# if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding")# if using R 3.6 or later
errors <- replicate(100, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x, data = train)
  y_hat <- predict(fit, test)
  
  RMSE(y_hat,test$y)
})
c(mean(errors), sd(errors))

#Q6_ more variables
#set.seed(1)
set.seed(1, sample.kind="Rounding")# if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

errors_x1 <- replicate(1, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x_1, data = train)
  y_hat <- predict(fit, test)
  
  RMSE(y_hat,test$y)
})
errors_x2 <- replicate(1, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x_2, data = train)
  y_hat <- predict(fit, test)
  
  RMSE(y_hat,test$y)
})
set.seed(1, sample.kind="Rounding")# if using R 3.6 or later
errors_x1x2 <- replicate(1, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x_1+x_2, data = train)
  y_hat <- predict(fit, test)
  
  RMSE(y_hat,test$y)
})


#Q8_ more variables higher correlation
#set.seed(1)
set.seed(1, sample.kind="Rounding")# if using R 3.6 or later
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind="Rounding")# if using R 3.6 or later
errors_x1 <- replicate(1, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x_1, data = train)
  y_hat <- predict(fit, test)
  
  RMSE(y_hat,test$y)
})
set.seed(1, sample.kind="Rounding")# if using R 3.6 or later
errors_x2 <- replicate(1, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x_2, data = train)
  y_hat <- predict(fit, test)
  
  RMSE(y_hat,test$y)
})
set.seed(1, sample.kind="Rounding")# if using R 3.6 or later
errors_x1x2 <- replicate(1, {
  test_index <- createDataPartition(dat$y,times=1,p=0.5,list=FALSE)
  test <- dat[test_index,]
  train <- dat[-test_index,]
  fit <- lm(y ~ x_1+x_2, data = train)
  y_hat <- predict(fit, test)
  
  RMSE(y_hat,test$y)
})

#______________________________________________
#Logistic Regression check
#set.seed(2) #if you are using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
dat$train %>% ggplot(aes(x, color = y)) + geom_density()

#Q1
#seq(0, 10, len=25)
set.seed(1, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 3, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

# fit against train data
glm_fit <- glm(y ~ x, data = dat$train, family = "binomial")
p_hat_logit <- predict(glm_fit, newdata = dat$test, type = "response")
y_hat_logit <- ifelse(p_hat_logit > 0.5, 1, 0) %>% factor
confusionMatrix(y_hat_logit, dat$test$y)$overall[["Accuracy"]]
  # Get the accuracy using test data

#smoothing__________________________________________________________________
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

dat %>% ggplot(aes(date, deaths)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.05, method = "loess", method.args = list(degree=1))


##Digits_27____Example
library(broom)
p_hat_glm_1cov <- mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .)
y_hat_glm <- predict(p_hat_glm_1cov,newdata = mnist_27$test, type = "response")
qplot(x_2, y_hat_glm, data = mnist_27$test)
# Muy clave colocar el operador as.numeric(y) para esta vaiable categorica
mnist_27$train %>% ggplot(aes(x_2, as.numeric(y))) +
  geom_point() + 
  geom_smooth(color="red", span = 0.15, method = "loess", method.args = list(degree=1))


##Matrices_________________________________________________________________
x <- matrix(rnorm(100*10), 100, 10)
dim(x)
n(x)
length(x)
length(t(x))
length(t(x))
cols(x)
nrow(x)
nrow(t(x))
ncol(x)

##P(Grey_Pixels/All_Pixels)
Grey_x <- mnist$train$images
dim(Grey_x)
Grey_x[Grey_x <= 50] <- 0
Grey_x[Grey_x >= 205] <- 0
Grey_x[Grey_x > 50 & Grey_x < 205] <- 1
GreyPixels <- rowSums(Grey_x)
sum(GreyPixels)
sum(GreyPixels)/(784*60000)
