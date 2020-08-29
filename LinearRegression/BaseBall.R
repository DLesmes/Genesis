library(Lahman)
ds_theme_set()
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, r_per_game = R/G) %>% 
  ggplot(aes(HR_per_game,r_per_game))+
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB/G, r_per_game = R/G) %>% 
  ggplot(aes(SB_per_game, r_per_game))+
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, r_per_game = R/G) %>% 
  ggplot(aes(BB_per_game, r_per_game))+
  geom_point(alpha = 0.5)

?Teams

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB/G, r_per_game = R/G) %>%
  ggplot(aes(AB_per_game, r_per_game))+
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Triples_per_game = X3B/G, Doubles_per_game = X2B/G) %>%
  ggplot(aes(Triples_per_game, Doubles_per_game))+
  geom_point(alpha = 0.5)+
  geom_smooth(method=lm)
  

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  ggplot(aes(W_per_game, E_per_game))+
  geom_point(alpha = 0.5)
  

Teams_plus <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W/G, E_per_game = E/G)
  
Corr_WE <- cor(Teams_plus$W_per_game,Teams_plus$E_per_game) %>% 
  round(4)

Teams_plus %>% ggplot(aes(W_per_game, E_per_game))+
  geom_point(alpha = 0.5)+
  geom_smooth(method=lm)+
  annotate(x=0.5, y=1.2, geom = "text", label = c("Correlación\n\n",Corr_WE))

Teams_AB_R <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Bat_per_game = AB/G, R_per_game = R/G)
  
Corr_AB_R <- cor(Teams_AB_R$Bat_per_game,Teams_AB_R$R_per_game)
print(Corr_AB_R)

Teams_X2_X3 <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(Triples_per_game = X3B/G, Doubles_per_game = X2B/G)
  
Corr_X2_X3 <- cor(Teams_X2_X3$Triples_per_game,Teams_X2_X3$Doubles_per_game)
print(Corr_X2_X3)

Teams_BB_R <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G)

Corr_BB_R <- cor(Teams_BB_R$BB_per_game,Teams_BB_R$R_per_game)
print(Corr_AB_R)

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game))

# lsm for BB and HR
Teamfull <- Teams %>% 
  mutate(BB_per_game = BB/G,
         R_per_game = R/G, 
         HR_per_game = HR/G)

# summary statistics 
sample_n(Teamfull, N, replace = TRUE) %>% 
  lm(R_per_game ~ BB_per_game + HR_per_game, data = .) %>% 
  summary %>% 
  .$coef 

# relation between 02' and 01' seasons
library(Lahman)
bat_02 <- Batting %>%
  filter(yearID == 2002) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% 
  select(playerID, singles, bb) %>% 
  group_by(playerID) %>% 
  summarise(mean_singles = mean(singles), mean_bb = mean(bb))

bat_01 <- Batting %>%
  filter(yearID %in% 1999:2001) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% 
  select(playerID, singles, bb) %>% 
  group_by(playerID) %>% 
  summarise(mean_singles = mean(singles), mean_bb = mean(bb))
  
bat_Singles_01 <- Batting %>%
  filter(yearID %in% 1999:2001) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% 
  select(playerID, singles, bb) %>% 
  group_by(playerID) %>% 
  summarise(mean_singles = mean(singles), mean_bb = mean(bb)) %>% 
  filter(mean_singles > 0.2) %>% 
  nrow()  

bat_bb_01 <- Batting %>%
  filter(yearID %in% 1999:2001) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% 
  select(playerID, singles, bb) %>% 
  group_by(playerID) %>% 
  summarise(mean_singles = mean(singles), mean_bb = mean(bb)) %>% 
  filter(mean_bb > 0.2) %>% 
  nrow()

bat_01_02 <- merge(bat_01, bat_02, by = "playerID")
bat_01_02
str(bat_01_02)

Cor_singles_01_02 <- cor(bat_01_02$mean_singles.x, bat_01_02$mean_singles.y)
Cor_bb_01_02 <- cor(bat_01_02$mean_bb.x, bat_01_02$mean_bb.y)

bat_02_all <- Batting %>%
  filter(yearID == 2002) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% 
  select(playerID, singles, bb)

bat_01_all <- Batting %>%
  filter(yearID %in% 1999:2001) %>% 
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% 
  select(playerID, singles, bb)

mean_bb_vs_bb <- ggplot(aes(bat_01$mean_singles, bat_01_all$singles)) +
  geom_point(alpha = 0.5)

merge_seasons <-left_join(bat_02_all, bat_01, by = "playerID")

fit_singles <- lm(singles ~ mean_singles, data = merge_seasons) %>% 
  summary()
fit_singles
fit_bb <- lm(bb ~ mean_bb, data = merge_seasons) %>% 
  summary()
fit_bb

# do funtion!!!!
# use do to fit a regression line to each HR stratum 
dat %>% group_by(HR) %>% 
  do(fit = lm(R ~ BB, data = .)) 

# using do without a column name gives an error 
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .)) 

# define a function to extract slope from lm 
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data) 
  data.frame(slope = fit$coefficients[2],
             se = summary(fit)$coefficient[2,2])
  } 
# return the desired data frame 
dat %>% group_by(HR) %>%
  do(get_slope(.))

# not the desired output: a column containing data frames 
dat %>%
  group_by(HR) %>% 
  do(slope = get_slope(.)) 

# data frames with multiple rows will be concatenated appropriately 
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data) 
  data.frame(term = names(fit$coefficients), 
             slope = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
  } 

dat %>% 
  group_by(HR) %>%
  do(get_lse(.))

###Broom library
# use tidy to return lm estimates and related information as a data frame 
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit) 

# add confidence intervals with tidy 
tidy(fit, conf.int = TRUE) 

# pipeline with lm, do, tidy 
dat %>% 
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>% 
  filter(term == "BB") %>% 
  select(HR, estimate, conf.low, conf.high) 

# make ggplots 
dat %>% 
  group_by(HR) %>% 
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>% 
  filter(term == "BB") %>% 
  select(HR, estimate, conf.low, conf.high) %>% 
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) + 
  geom_errorbar() + 
  geom_point() 

# inspect with glance 
glance(fit)

#___________________________________________________________________________________________________
# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

#Betas de 1971
fit <- Teams %>% 
  filter(yearID == 1971) %>% 
  mutate(BB = BB / G,
         #singles = (H - X2B - X3B - HR) / G, 
         #doubles = X2B / G, 
         #triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  #lm(R ~ BB + singles + doubles + triples + HR, data = .)
  lm(R ~ BB + HR, data = .)
coefs_BB_HR_71 <- tidy(fit, conf.int = TRUE)
coefs_BB_HR_71

#Effect of BB and HR from 1961 to 2018
Temas_stats <- Teams %>% 
  filter(yearID %in% 1961:2018)

Temas_stats %>%
  group_by(yearID) %>% 
  do(tidy(lm(R ~ BB + HR, data = .))) %>% 
  filter(term == "BB") %>% 
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = lm)

BB_stats <- Temas_stats %>%
  group_by(yearID) %>% 
  do(tidy(lm(R ~ BB + HR, data = .))) %>% 
  filter(term == "BB") %>% 
  lm(estimate ~ yearID, data = .) %>% 
  tidy()

?admissions
