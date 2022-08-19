# Assessment: Linear Models

# This assessment has 6 multi-part questions that will 
# all use the setup below. Game attendance in baseball
# varies partly as a function of how well a team is playing.
# Load the Lahman library. The Teams data frame contains 
# an attendance column. This is the total attendance 
# for the season. To calculate average attendance,
# divide by the number of games played, as follows:
library(tidyverse)
library(ggplot2)
library(broom)
library(Lahman)
Teams_small = Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, 
         R_per_game = R / G, 
         HR_per_game = HR / G)

# Use runs (R) per game to predict average attendance.
# For every 1 run scored per game, average attendance
# increases by how much?
fit = lm(avg_attendance ~ R_per_game, data = Teams_small)
fit$coefficients[2]

# Use home runs (HR) per game to predict average attendance.
# For every 1 home run hit per game, average attendance 
# increases by how much?
fit = lm(avg_attendance ~ HR_per_game, data = Teams_small)
fit$coefficients[2]

# Use number of wins to predict average attendance; 
# do not normalize for number of games. For every game 
# won in a season, how much does average attendance increase?
fit = lm(avg_attendance ~ W, data = Teams_small)
fit$coefficients[2]

# Suppose a team won zero games in a season.
# Predict the average attendance.
fit$coefficients[1]

# Use year to predict average attendance.
# How much does average attendance increase each year?
fit = lm(avg_attendance ~ yearID, data = Teams_small)
fit$coefficients[2]

# Game wins, runs per game and home runs per game are 
# positively correlated with attendance. We saw in the 
# course material that runs per game and home runs per
# game are correlated with each other. Are wins and runs
# per game or wins and home runs per game correlated? Use 
# the Teams_small data once again.
# What is the correlation coefficient for runs per game 
# and wins?
cor(Teams_small$R_per_game, Teams_small$W)
cor(Teams_small$HR_per_game, Teams_small$W)

# Stratify Teams_small by wins: divide number of wins
# by 10 and then round to the nearest integer. Filter 
# to keep only strata 5 through 10. (The other strata 
# have fewer than 20 data points, too few for our analyses).

Teams_small_stratified = Teams_small %>% 
  mutate(W_round = round(W / 10)) %>% 
  filter(W_round >=5 & W_round <= 10)

# Use the stratified dataset to answer this three-part question.
# How many observations are in the 8 win strata?

sum(Teams_small_stratified$W_round == 8)

# Calculate the slope of the regression line predicting 
# average attendance given runs per game for each of the
# win strata. Which win stratum has the largest 
# regression line slope?

Teams_small_stratified %>%
  group_by(W_round) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))
  
# Calculate the slope of the regression line predicting 
# average attendance given HR per game for each of the 
# win strata.
# Which win stratum has the largest regression line slope?

Teams_small_stratified %>%
  group_by(W_round) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))

# Fit a multivariate regression determining the effects 
# of runs per game, home runs per game, wins, and year
# on average attendance. Use the original Teams_small wins 
# column, not the win strata from question 3.

fit = Teams_small %>% 
  summarize(tidy(lm(avg_attendance ~ R_per_game + HR_per_game + yearID + W, data = .), conf.int = TRUE))

# What is the estimate of the effect of runs per
# game on average attendance?

fit %>% 
  filter(term == "R_per_game") %>%
  pull(estimate)

# What is the estimate of the effect of home 
# runs per game on average attendance?

fit %>% 
  filter(term == "HR_per_game") %>%
  pull(estimate)

# What is the estimate of the effect of number 
# of wins in a season on average attendance?

fit %>% 
  filter(term == "W") %>%
  pull(estimate)

# Use the multivariate regression model from Question 4. 
# Suppose a team averaged 5 runs per game, 1.2 home runs 
# per game, and won 80 games in a season. Use the
# predict() function to generate predictions for this team.
fit = Teams_small %>%
  lm(avg_attendance ~ R_per_game + HR_per_game + yearID + W, data = .)

# What would this team's average attendance be in 2002?

predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 2002))

# # What would this team's average attendance be in 1960?

predict(fit, data.frame(R_per_game = 5, HR_per_game = 1.2, W = 80, yearID = 1960))

# Use your model from Question 4 to predict average
# attendance for teams in 2002 in the original Teams 
# data frame.
# What is the correlation between the predicted 
# attendance and actual attendance?
dat = Teams %>%
  filter(yearID == 2002) %>%
  mutate(R_per_game = R / G,
         HR_per_game = HR / G,
         avg_attendance = attendance/G)
cor(dat$avg_attendance, predict(fit, newdata = dat))
