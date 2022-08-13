library(HistData)
library(dslabs)
library(tidyverse)
library(ggplot2)
options(digits = 3)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# The following code was used in the video to plot RSS with b0 = 25.

rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}
beta1 = seq(0, 1, len=nrow(galton_heights))
results = data.frame(beta1 = beta1, rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

# In a model for sons’ heights vs fathers’ heights, what is the least 
# squares estimate (LSE) for if we assume is b0 = 36?

beta1 = seq(0, 1, len=nrow(galton_heights))
results = data.frame(beta1 = beta1, rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

# The least squares estimates for the parameters b0, b1, bn
# minimize the the residual sum of squares(RSS).

# Load the Lahman library and filter the Teams data frame 
# to the years 1961-2001. Run a linear model in R predicting 
# the number of runs per game based on both the number of 
# bases on balls per game and the number of home runs per game.
library(Lahman)
data("Teams")

teams = Teams %>%
  filter(yearID %in% 1961:2011) %>%
  mutate(R_per_game = R /G, 
         BB_per_game = BB / G,
         HR_per_game = HR / G) %>%
  select(R_per_game, BB_per_game, HR_per_game)

model = teams %>%
  lm(R_per_game ~ BB_per_game + HR_per_game, data = .)

# What is the coefficient for bases on balls?

summary(model)

# We run a Monte Carlo simulation where we repeatedly take
# samples of N = 100 from the Galton heights data and compute 
# the regression slope coefficients for each sample: 

B = 1000
N = 100
events = replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

LSEs = data.frame(beta_0 = events[1,], beta_1 = events[2,]) 
plot(LSEs)

# What does the central limit theorem tell us about the
# variables beta_0 and beta_1?

# - They are approximately normally distributed.
# - The expected value of each is the true value of and
# (assuming the Galton heights data is a complete population).

# Which R code(s) below would properly plot the predictions
# and confidence intervals for our linear model of sons’ heights?

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

# OR

model = lm(son ~ father, data = galton_heights)
predictions = predict(model, interval = c("confidence"), level = 0.95)
data = as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

# Define female_heights, a set of mother and daughter
# heights sampled from GaltonFamilies, as follows:

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights = GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

female_heights %>% head()

# Fit a linear regression model predicting the 
# mothers' heights using daughters' heights.

model = lm(mother ~ daughter, data = female_heights)

# What is the slope of the model?

model$coefficients

# Predict mothers' heights using the model from 
# Question 7 and the predict() function.

predictions = predict(model)

# What is the predicted height of the first mother
# in the dataset?

predictions[1]

# What is the actual height of the first 
# mother in the dataset?

female_heights$mother[1]

# We have shown how BB and singles have similar predictive
# power for scoring runs. Another way to compare the 
# usefulness of these baseball metrics is by assessing 
# how stable they are across the years. Because we have
# to pick players based on their previous performances,
# we will prefer metrics that are more stable. In these
# exercises, we will compare the stability of singles and BBs.

# Before we get started, we want to generate two tables: 
# one for 2002 and another for the average of 1999-2001 
# seasons. We want to define per plate appearance 
# statistics, keeping only players with more than 
# 100 plate appearances. Here is how we create the 2002 table:

bat_02 = Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

# Now compute a similar table but with rates computed 
# over 1999-2001. Keep only rows from 1999-2001 where players
# have 100 or more plate appearances, calculate each 
# player's single rate and BB rate per stint (where 
# each row is one stint - a player can have multiple
# stints within a season), then calculate the average
# single rate (mean_singles) and average BB rate 
# (mean_bb) per player over the three year period.

bat_99_01 = Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb)) %>%
  ungroup()

# How many players had a single rate mean_singles of 
# greater than 0.2 per plate appearance over 1999-2001?

sum(bat_99_01$mean_singles > 0.2)

# How many players had a BB rate mean_bb of greater 
# than 0.2 per plate appearance over 1999-2001?

sum(bat_99_01$mean_bb > 0.2)

# Use inner_join() to combine the bat_02 table with 
# the table of 1999-2001 rate averages you created 
# in the previous question.

all_years = bat_99_01 %>% inner_join(bat_02)

# What is the correlation between 2002 singles 

# rates and 1999-2001 average singles rates?
cor(all_years$singles, all_years$mean_singles)

# What is the correlation between 2002 BB rates and 
# 1999-2001 average BB rates?
cor(all_years$bb, all_years$mean_bb)

# Make scatterplots of mean_singles versus singles
# and mean_bb versus bb.
# Are either of these distributions bivariate normal?

all_years %>%
  ggplot(aes(singles, mean_singles)) +
  geom_point(alpha = 0.5)

all_years %>%
  ggplot(aes(bb, mean_bb)) +
  geom_point(alpha = 0.5)

# Fit a linear model to predict 2002 singles given 
# 1999-2001 mean_singles.

fit = lm(singles ~ mean_singles, data = all_years)

# What is the coefficient of mean_singles, 
# the slope of the fit?
summary(fit)

# Fit a linear model to predict 2002 bases on balls given 
# 1999-2001 mean_bb.

fit = lm(bb ~ mean_bb, data = all_years)

# What is the coefficient of mean_bb, 
# the slope of the fit?
summary(fit)
