#set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3) # report 3 significant digits

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mean_mother <- mean(female_heights$mother)
sd_mother <- sd(female_heights$mother)
mean_daughter <- mean(female_heights$daughter)
sd_daughter <- sd(female_heights$daughter)
CCF <- cor(female_heights$daughter,female_heights$mother)

estatistics <- female_heights %>% summarise(mean(mother),sd(mother),mean(daughter),sd(daughter))

change <- CCF*(sd_daughter/sd_mother)
cut <- mean_daughter - (change*mean_mother)

Porcentage_daughter = CCF^2

Forcast_daugther = mean_daughter + change*(60 - mean_mother)

galton_heights <- GaltonFamilies %>% 
  filter(gender == "male") %>% 
  group_by(family) %>% 
  sample_n(1) %>% 
  ungroup() %>% 
  select(father, childHeight) %>%
  rename(son = childHeight)

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father) 
  return(sum(resid^2)) }

# plot RSS as a function of beta1 when beta0=25 
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1, rss = sapply(beta1, rss, beta0 = 36)) 
results %>% ggplot(aes(beta1, rss)) +
  geom_line(aes(beta1, rss))

# fit regression line to predict son's height from father's height 
fit <- lm(son ~ father, data = galton_heights) 
fit # summary statistics 
summary(fit)

# Monte Carlo simulation 
B <- 1000 
N <- 50
lse <- replicate(B,{
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef }) 

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# Plot the distribution of beta_0 and beta_1 
library(gridExtra) 
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2) 

# summary statistics 
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>% 
  .$coef 

lse %>%
  summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

lse %>% summarize(cor(beta_0, beta_1))
B <- 1000 
N <- 50 
MQ <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
  mutate(father = father - mean(father)) %>%
  lm(son ~ father, data = .) %>% .$coef
})
cor(MQ[1,], MQ[2,])

# plot predictions and confidence intervals 
galton_heights %>% ggplot(aes(son, father)) + 
  geom_point() + 
  geom_smooth(method = "lm") 

# predict Y directly 
fit <- galton_heights %>% 
  lm(son ~ father, data = .) 
  

Y_hat <- predict(fit, se.fit = TRUE) 
names(Y_hat) 

# plot best fit line 
galton_heights %>% 
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+ 
  geom_line()

# fit regression line to predict dauther's height from mother's height 
fitfemale <- lm(mother ~ daughter, data = female_heights) 
fitfemale # summary statistics 
summary(fitfemale) %>% 
  .$coef

Female_resume <- data.frame(summary(fitfemale) %>% .$coef)

Forcast_mother = Female_resume[1,1] + Female_resume[2,1]*female_heights[1,2]
print(Forcast_mother)

#parent-child relation
library(broom)
galton <- GaltonFamilies %>% 
  group_by(family, gender) %>% 
  sample_n(1) %>% 
  ungroup() %>% 
  gather(parent , parentHeight, father:mother) %>% 
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>% 
  unite(pair, c("parent", "child"))

parents_child_n <- galton %>% 
  group_by(pair) %>%
  summarise(a = sum(parentHeight)/mean(parentHeight))

parents_child_cor <- galton %>% 
  group_by(pair) %>%
  summarise(a = cor(parentHeight,childHeight))
parents_child_cor

get_lse <- function(data){
  fit <- lm(childHeight ~ parentHeight, data = data) 
  data.frame(term = names(fit$coefficients), 
             slope = fit$coefficients, 
             se = summary(fit)$coefficient[,2],
             low = tidy(fit, conf.int = TRUE)[,5],
             high = tidy(fit, conf.int = TRUE)[,6],
             p_value = summary(fit)$coefficient[,4])
} 

parents_child_stata <- galton %>% 
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>% 
  filter(term == "parentHeight")
parents_child_stata
