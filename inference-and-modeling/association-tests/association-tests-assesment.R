# Comparing Proportions of Hits
# In a previous exercise, we determined whether or not each poll
# predicted the correct winner for their state in the 2016 U.S. 
# presidential election. Each poll was also assigned a grade by 
# the poll aggregator. Now we're going to determine if polls 
# rated A- made better predictions than polls rated C-.
# In this exercise, filter the errors data for just polls 
# with grades A- and C-. Calculate the proportion of times 
# each grade of poll predicted the correct winner.
library(dplyr)
library(dslabs)
library(ggplot2)
library(tidyverse)
data("polls_us_election_2016")
polls = polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
cis = polls %>%
  mutate(X_hat = (spread + 1) / 2,
         se = 2 * sqrt(X_hat * (1 - X_hat) / samplesize),
         lower = spread - qnorm(0.975) * se,
         upper = spread + qnorm(0.975) * se,
  ) %>%
  select(state, startdate, enddate, pollster,
         grade, spread, lower, upper)
cis = cis %>%
  mutate(state = as.character(state)) %>% 
  left_join(results, by = "state")
errors = cis %>%
  mutate(error = spread - actual_spread,
         hit = sign(spread) == sign(actual_spread))
grades = c("C-", "A-")
totals = errors %>%
  filter(grade %in% grades) %>%
  group_by(grade, hit) %>%
  summarize(num = n()) %>%
  spread(grade, num)
A_rate = totals[which(totals$hit),]$`A-` / sum(totals$`A-`)
C_rate = totals[which(totals$hit),]$`C-` / sum(totals$`C-`)

# Chi-squared Test
# We found that the A- polls predicted the correct winner about 
# 80% of the time in their states and C- polls predicted the 
# correct winner about 86% of the time.
# Use a chi-squared test to determine if these proportions
# are different.
chisq_test = totals %>% select(-hit) %>% chisq.test()
chisq_test$p.value

# It doesn't look like the grade A- polls performed significantly
# differently than the grade C- polls in their states.
# Calculate the odds ratio to determine the magnitude of 
# the difference in performance between these two grades of polls.
A_not_hit_rate = totals[which(totals$hit == FALSE),]$`A-` / sum(totals$`A-`)
odds_A = A_rate / A_not_hit_rate
C_not_hit_rate = totals[which(totals$hit == FALSE),]$`C-` / sum(totals$`C-`)
odds_C = C_rate / C_not_hit_rate
odds_A / odds_C

# We did not find meaningful differences between the poll 
# results from grade A- and grade C- polls in this subset of the
# data, which only contains polls for about a week before the 
# election. Imagine we expanded our analysis to include all 
# election polls and we repeat our analysis. In this hypothetical 
# scenario, we get that the p-value for the difference in 
# prediction success if 0.0015 and the odds ratio describing 
# the effect size of the performance of grade A- over grade B- 
# polls is 1.07.
# Based on what we learned in the last section, we can classify
# this difference as scientifically insignificant since the p-value
# is very small(could be explained by the sample size increase) but
# the odds ratio is very close to 1.


