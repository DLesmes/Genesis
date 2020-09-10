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

##distance
library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)
abs(d[1]-d[2])
abs(d[39]-d[40])
abs(d[73]-d[74])

tissue_gene_expression$y[1]
tissue_gene_expression$y[2]
tissue_gene_expression$y[39]
tissue_gene_expression$y[40]
tissue_gene_expression$y[73]
tissue_gene_expression$y[74]

abs(d[73]-d[39])

#Knn_Algorithm________________________________________
##Heights
library(dslabs)
library(purrr)
data("heights")

#set.seed(2) #if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

#pick the k in knn
ks <- seq(1, 101, 3)
Scores <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  F_meas(data = y_hat, reference = factor(test_set$sex))
})

#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(Scores)]
max(Scores)

result <- data.frame(ks,Scores)
result %>% ggplot(aes(ks,Scores)) +
  geom_line()

##tissu_gene_expresion
data("tissue_gene_expression")
tiss <- as.data.frame(tissue_gene_expression)
set.seed(1, sample.kind = "Rounding") #if you are using R 3.6 or later

test_index <- createDataPartition(tiss$y, times = 1, p = 0.5, list = FALSE)
train_set <- tiss %>% slice(-test_index)
test_set <- tiss %>% slice(test_index)

#pick the k in knn
ks <- seq(1, 11, 2)
Scores <- sapply(ks, function(k){
  fit <- knn3(y ~ ., data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  confusionMatrix(y_hat, test_set$y)$overall[["Accuracy"]]
})
Scores

result <- data.frame(ks,Scores)
result %>% ggplot(aes(ks,Scores)) +
  geom_line()


#k folds Cross validation________________________________________________

library(tidyverse)
library(caret)
install.packages(e1071)

# set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]
fit <- train(x_subset, y, method = "glm")
fit$results

install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)

pvals <- tt$p.value

str(pvals)

ind <- ifelse(abs(pvals) < 0.01, 1, 0)
sum(ind)
str(x_subset)

ind <- which(abs(pvals) < 0.01)
x_subset <- x[,ind]
colnames(x_subset) <- colnames(x[,ind])
fit <- train(x_subset, y, method = "glm")
fit$results

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


library(dslabs)
data(tissue_gene_expression)
str(tissue_gene_expression)
#tiss <- as.data.frame(tissue_gene_expression)
tiss <- tissue_gene_expression
dat <- tiss$x
fit <- train(dat, tiss$y, method = "knn", tuneGrid = data.frame(k = seq(1, 7, 2)))
ggplot(fit)


#Bootstrap______________________________________________________________________________
library(dslabs)
library(caret)
data(mnist_27)
# set.seed(1995) # if R 3.5 or earlier
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later
indexes <- createResample(mnist_27$train$y, 10)

Mindexes <- as.data.frame(indexes)

Mindexes3 <- Mindexes[ ,1] == 3
sum(Mindexes3)
Mindexes4 <- Mindexes[ ,1] == 4
sum(Mindexes4)
Mindexes7 <- Mindexes[ ,1] == 7
sum(Mindexes7)

length(Mindexes[ ,1])

set.seed(1, sample.kind="Rounding") # if R 3.6 or later
B <- 10000
exp_value <- replicate(B,{
  y <- rnorm(100,0,1)
  quantile(y,0.75)
})
mean(exp_value)
sd(exp_value)

set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1)
indexes <- createResample(y, 10)
Mindexes <- as.matrix.data.frame(Mindexes)

quatiles[0] <- quantile(y[Mindexes[ ,1]],0.75)
quatiles[1] <- quantile(y[Mindexes[ ,2]],0.75)
quatiles[2] <- quantile(y[Mindexes[ ,3]],0.75)
quatiles[3] <- quantile(y[Mindexes[ ,4]],0.75)
quatiles[4] <- quantile(y[Mindexes[ ,5]],0.75)
quatiles[5] <- quantile(y[Mindexes[ ,6]],0.75)
quatiles[6] <- quantile(y[Mindexes[ ,7]],0.75)
quatiles[7] <- quantile(y[Mindexes[ ,8]],0.75)
quatiles[8] <- quantile(y[Mindexes[ ,9]],0.75)
quatiles[9] <- quantile(y[Mindexes[ ,10]],0.75)
mean(quatiles)
sd(quatiles)

for (i in seq(1:10)){quatiles[i]<-quantile(y[Mindexes[ ,i]],0.75)}
mean(quatiles)
sd(quatiles)


quantiles <- replicate(10,{
tmp <- sample(y,100,replace = TRUE)
quantile(tmp,0.75)
})
mean(quantiles)
sd(quantiles)

quantiles <- sapply(indexes, function(x){
  quantile(y[x],0.75)
  })
mean(quantiles)
sd(quantiles)

set.seed(1, sample.kind = "Rounding") # if R 3.6 or later
y <- rnorm(100, 0, 1) 
indexes <- createResample(y, 10)
quantiles <- sapply(indexes, function(x){quantile(y[x],0.75)})
mean(quantiles)
sd(quantiles)



q0 <- qplot(y, bin=30, color='red')
q1 <- qplot(y[Mindexes[ ,1]], bin=30)
q2 <- qplot(y[Mindexes[ ,2]], bin=30)
q3 <- qplot(y[Mindexes[ ,3]], bin=30)
q4 <- qplot(y[Mindexes[ ,4]], bin=30)
q5 <- qplot(y[Mindexes[ ,5]], bin=30)
q6 <- qplot(y[Mindexes[ ,6]], bin=30)
q7 <- qplot(y[Mindexes[ ,7]], bin=30)
q8 <- qplot(y[Mindexes[ ,8]], bin=30)
q9 <- qplot(y[Mindexes[ ,9]], bin=30)
q10 <- qplot(y[Mindexes[ ,10]], bin=30)
grid.arrange(q0,q1,q2,q3,q4,q5,q6,ncol = 3)
grid.arrange()
q0

for (i in seq(1:10)){qplot(y[Mindexes[ ,i]], bin=50, color='red')}

##QDA&LDA_______________________________________________________________________
library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
dat <- data.frame(y,x)

data_lda <- train(y ~ ., method = "lda", data = dat)
confusionMatrix(predict(data_lda, dat), y)$overall["Accuracy"]

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]
dat <- data.frame(y,x)

data_qda <- train(y ~ ., method = "qda", data = dat)


data_lda_scaled <- train(y ~ ., method = "lda", data = dat, preProcess = "center")
confusionMatrix(predict(data_lda, dat), y)$overall["Accuracy"]

#the same but all types
library(dslabs)      
library(caret)
data("tissue_gene_expression")

# set.seed(1993) # if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
dat <- data.frame(y,x)

data_lda_scaled_all <- train(y ~ ., method = "lda", data = dat, preProcess = "center")


#####Classifcation tree________________________________________________________________

library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~ ., data = dat)
# visualize the splits 
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)


####Ramdon forest______________________________________________________________________

library(randomForest)
fit <- randomForest(y ~ x, data = dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
  
plot(fit)

library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

?randomForest
