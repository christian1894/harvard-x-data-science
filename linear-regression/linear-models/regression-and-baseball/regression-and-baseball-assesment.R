# Assessment: Regression and Baseball, part 1
library(dslabs)
library(tidyverse)
library(broom)
library(Lahman)
data("Teams")

# We want to estimate runs per game scored by individual 
# players, not just by teams. What summary metric do we 
# calculate to help estimate this?

# Look at the code from the video "Building a Metter 
# Offensive Metric for Baseball" for a hint: 

pa_per_game <- Batting %>% 
  filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  .$pa_per_game %>% 
  mean

# pa_per_game is the number of plate appearances per 
# team per game averaged across all teams. We initially
#calculated the pa_per_game grouped by teams but then 
# took the means across all teams to get one summary metric. 

# Imagine you have two teams. Team A is comprised of 
# batters who, on average, get two bases on balls, 
# four singles, one double, no triples, and one home run. 
# Team B is comprised of batters who, on average, 
# get one base on balls, six singles, two doubles, 
# one triple, and no home runs.
# (For convenience, the coefficients for the model are as
# follows: BB 0.371, singles 0.519, doubles 0.771, 
# triples 1.24, and home runs 1.44.) Which team scores
# more runs, as predicted by our model?
get_R_hat = function(BB, singles, doubles, triples, HR) {
  return(-2.769 + (0.371 * BB) + (0.519 * singles) + 
           (0.771 * doubles) + (1.24 * triples) + (1.443 * HR))
  }

team_A_R_hat = get_R_hat(2, 4, 1, 0, 1)
team_B_R_hat = get_R_hat(1, 6, 2, 1, 0)
team_A_R_hat < team_B_R_hat

# Use the Teams data frame from the Lahman package. Fit 
# a multivariate linear regression model to obtain 
# the effects of BB and HR on Runs (R) in 1971. 
# Use the tidy() function in the broom package to
# obtain the results in a data frame.  

dat = Teams %>% filter(yearID == 1971) %>%
  mutate(HR = HR/G, 
         BB = BB/G,
         R = R/G)
fit = lm(R ~ BB + HR, data = dat)
tidy(fit, conf.int = TRUE)

# The p-value for HR is less than 0.05, but the p-value
# of BB is greater than 0.05 (0.06), so the evidence 
# is not strong enough to suggest that BB has a 
# significant effect on runs at a p-value cutoff of 0.05.

# Repeat the above exercise to find the effects of BB and 
# HR on runs (R) for every year from 1961 to 2018 using 
# do() and the broom package.

# Make a scatterplot of the estimate for the effect of
# BB on runs over time and add a trend line with confidence
# intervals.

# Fill in the blank to complete the statement:

res = Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  summarize(tidy(lm(R ~ BB + HR, data = across()))) %>%
  ungroup() 
res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

# Fit a linear model on the results from Question 10 
# to determine the effect of year on the impact of BB.
# That is, determine how the estimated coefficients of
# BB from the models in Question 10 can be predicted by 
# the year (recall that we grouped the data by year 
# before fitting the models, so we have different 
# estimated coefficients for each year).
# For each additional year, by what value does the 
# impact of BB on runs change?

res = res %>%
  filter(term == "BB")
tidy(lm(estimate ~ yearID, data = res), conf.int = TRUE)
