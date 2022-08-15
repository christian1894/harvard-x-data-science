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
