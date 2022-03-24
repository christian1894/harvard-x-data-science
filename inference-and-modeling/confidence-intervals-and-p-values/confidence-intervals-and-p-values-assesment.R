library(dslabs)
library(tidyverse)
data("polls_us_election_2016")
# Confidence interval for p
# Assume there are only two candidates and construct a 
# 95% confidence interval for the election night proportion p.
polls = polls_us_election_2016 %>% 
  filter(state =="U.S." & enddate >= "2016-10-31")
first_poll = polls[1,]
N = first_poll$samplesize
X_hat = first_poll$rawpoll_clinton / 100
SE_hat = sqrt(X_hat*(1-X_hat)/N) 
ci = c(X_hat - (qnorm(0.975) * SE_hat), X_hat + (qnorm(0.975) * SE_hat))

# Pollster results for p
# Create a new object called pollster_results that contains 
# the pollster's name, the end date of the poll, the 
# proportion of voters who declared a vote for Clinton, 
# the standard error of this estimate, and the lower and 
# upper bounds of the confidence interval for the estimate.
pollster_results = polls %>% 
  mutate(X_hat = rawpoll_clinton / 100,
         se_hat = sqrt(X_hat*(1-X_hat)/samplesize),
         lower = X_hat - (qnorm(0.975) * se_hat),
         upper = X_hat + (qnorm(0.975) * se_hat)
         ) %>%
  select(pollster, enddate, X_hat, se_hat, lower, upper)

# Comparing to actual results - p
# The final tally for the popular vote was Clinton 48.2% 
# and Trump 46.1%. Add a column called hit to pollster_results
# that states if the confidence interval included the true 
# proportion or not. What proportion of confidence intervals
# included p?
p = 0.482
avg_hit = pollster_results %>%
  mutate(hit = lower <= p & upper >= p) %>%
  summarize(mean(hit))

# Confidence interval for d
# A much smaller proportion of the polls than expected 
# produce confidence intervals containing p. Notice that most 
# polls that fail to include are underestimating. The rationale 
# for this is that undecided voters historically divide 
# evenly between the two main candidates on election day.
# In this case, it is more informative to estimate the spread 
# or the difference between the proportion of two candidates d,
# or 0.482 - 0.461 = 0.021 for this election.
# Assume that there are only two parties and that d = 2p - 1. 
# Construct a 95% confidence interval for difference in 
# proportions on election night.
polls = polls %>%
  mutate(d_hat = rawpoll_clinton / 100 - rawpoll_trump / 100)
first_poll = polls[1,]
N = first_poll$samplesize
d_hat = first_poll$d_hat
X_hat = (d_hat + 1) / 2
SE_hat = 2 * sqrt(X_hat*(1-X_hat)/N) 
ci = c(d_hat - (qnorm(0.975) * SE_hat), d_hat + (qnorm(0.975) * SE_hat))

# Pollster results for d
# Create a new object called pollster_results that contains 
# the pollster's name, the end date of the poll, the 
# difference in the proportion of voters who declared a 
# vote either, and the lower and upper bounds of the 
# confidence interval for the estimate.
pollster_results = polls %>% 
  mutate(X_hat = (d_hat + 1) / 2,
         se_hat = 2 * sqrt(X_hat*(1-X_hat)/samplesize),
         lower = d_hat - (qnorm(0.975) * se_hat),
         upper = d_hat + (qnorm(0.975) * se_hat)) %>%
  select(pollster, enddate, d_hat, lower, upper)

# Comparing to actual results - d
# What proportion of confidence intervals for the 
# difference between the proportion of voters included d, 
# the actual difference in election day?
d = 0.021
avg_hit  = pollster_results %>% 
  mutate(hit = lower <= d & upper >= d) %>%
  summarize(mean(hit))

# Comparing to actual results by pollster
# Although the proportion of confidence intervals that 
# include the actual difference between the proportion of 
# voters increases substantially, it is still lower that 0.95. 
# In the next chapter, we learn the reason for this.
# To motivate our next exercises, calculate the difference 
# between each poll's estimate d and the actual d = 0.021.
# Stratify this difference, or error, by pollster in a plot.
errors = polls %>% mutate(error = abs(d - d_hat)) %>%
  select(pollster, error)
errors %>% ggplot(aes(x = pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Comparing to actual results by pollster - multiple polls
# Remake the plot you made for the previous exercise, but 
# only for pollsters that took five or more polls.
# You can use dplyr tools group_by and n to group data
# by a variable of interest and then count the number of
# observations in the groups. The function filter filters 
# data piped into it by your specified condition.
errors = errors %>% group_by(pollster) %>%
  filter(n() >= 5)
errors %>% ggplot(aes(x = pollster, y = error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
