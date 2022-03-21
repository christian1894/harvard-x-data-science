# In June 2016, the United Kingdom (UK) held a referendum to
# determine whether the country would "Remain" in the European 
# Union (EU) or "Leave" the EU. This referendum is commonly 
# known as Brexit. Although the media and others interpreted 
# poll results as forecasting "Remain" p > 0.5, the actual 
# proportion that voted "Remain" was only 48.1% and the UK 
# thus voted to leave the EU. Pollsters in the UK were 
# criticized for overestimating support for "Remain". 
# suggested libraries and options
library(tidyverse)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)
p = 0.481    # official proportion voting "Remain"
d = 2 * p - 1    # official spread

# The final proportion of voters choosing "Remain" was p = 0.481. 
# Consider a poll with a sample of 1500 voters.
# What is the expected total number of voters in the sample 
# choosing "Remain"?
N = 1500
mu = p * N

# What is the standard error of the total number of voters
# in the sample choosing "Remain"?
se_x = sqrt(N * p * (1 - p))

# What is the expected value of X_hat, the proportion of 
# "Remain" voters?
p

# What is the standard error of X_hat, the proportion of 
# "Remain" voters?
se = sqrt(p * (1 - p) / N)

# What is the expected value of d, the spread between 
# the proportion of "Remain" voters and "Leave" voters?
d

# What is the standard error of d, the spread between the 
# proportion of "Remain" voters and "Leave" voters?
2 * se

# Actual Brexit poll estimates 
# Calculate x_hat for each poll, the estimate of the proportion 
# of voters choosing "Remain" on the referendum day (p = 0.481), 
# given the observed spread and the relationship d - 2X - 1. 
# Use mutate to add a variable x_hat to the brexit_polls 
# object by filling in the skeleton code below: 
brexit_polls = brexit_polls %>%
  mutate(x_hat = (spread + 1) / 2)

# What is the average of the observed spreads (spread)?
brexit_polls %>% 
  pull(spread) %>%
  mean()

# What is the standard deviation of the observed spreads?
brexit_polls %>% 
  pull(spread) %>%
  sd()

# What is the average of x_hat, the estimates of the parameter p?
brexit_polls %>% 
  pull(x_hat) %>%
  mean()

# What is the standard deviation of x_hat?
brexit_polls %>% 
  pull(x_hat) %>%
  sd()

# Confidence interval of a Brexit poll 
# Consider the first poll in brexit_polls, a YouGov poll 
# run on the same day as the Brexit referendum:
# Use qnorm to compute the 95% confidence interval for X_hat.
# What is the lower bound of the 95% confidence interval?
brexit_polls = brexit_polls %>% 
  mutate(se_x_hat = sqrt(x_hat * (1 - x_hat) / samplesize))
yougov_poll = brexit_polls[1,]
x_hat = yougov_poll$x_hat
se_x_hat = yougov_poll$se_hat_x
ci = c(x_hat - 1.96 * se_x_hat, x_hat + 1.96 * se_x_hat)

# The interval predicts a winner but does not cover 
# the true value of p.
lower = ci[1]
upper = ci[2]
!between(0.5, lower, upper) # predicts winner
between(p, lower, upper) # does not contain p

# Create the data frame june_polls containing only Brexit 
# polls ending in June 2016 (enddate of "2016-06-01" and later). 
# We will calculate confidence intervals for all polls and 
# determine how many cover the true value of d.
june_polls = brexit_polls %>%
  filter(enddate >= "2016-06-01")

# First, use mutate to calculate a plug-in estimate se_x_hat 
# for the standard error of the estimate se_hat for each poll 
# given its sample size and value of (x_hat). Second, use 
# mutate to calculate an estimate for the standard error of 
# the spread for each poll given the value of se_x_hat. Then, 
# use mutate to calculate upper and lower bounds for 95% 
# confidence intervals of the spread. Last, add a column 
# hit that indicates whether the confidence interval for 
# each poll covers the correct spread d = -0.038.
june_polls = june_polls %>%
  mutate(se_spread = 2 * se_x_hat,
         lower =  spread - 1.96 * se_spread,
         upper = spread + 1.96 * se_spread,
         hit_0 = lower < 0 & 0 < upper,
         above_0 = lower > 0 & 0 < upper,
         hit = lower < d & d < upper
  )
# What proportion of polls have a confidence interval that 
# covers the value 0?
mean(june_polls$hit_0)

# What proportion of polls predict "Remain" (confidence interval 
# entirely above 0)?
mean(june_polls$above_0)

# What proportion of polls have a confidence 
# interval covering the true value of d?
mean(june_polls$hit)

# Hit rate by pollster 
# Group and summarize the june_polls object by pollster 
# to find the proportion of hits for each pollster and 
# the number of polls per pollster. Use arrange
# to sort by hit rate.
pollster_hits = june_polls %>%
  group_by(pollster) %>%
  summarize(number_of_polls = n(),
          hit_rate = sum(hit) / number_of_polls) %>%
  arrange(hit_rate)
pollster_hits
# The pollsters hit rate is consisten with the general bias
# that affects all pollsters

# Boxplot of Brexit polls by poll type 
# Make a boxplot of the spread in june_polls by poll type.
june_polls %>%
  ggplot(aes(poll_type, spread)) +
  geom_boxplot()
# there is a clear bias introduced by poll types

# Combined spread across poll type 
# Calculate the confidence intervals of the spread combined 
# across all polls in june_polls, grouping by poll type. 
# Recall that to determine the standard error of the spread,
# you will need to double the standard error of the estimate.

# Use this code (which determines the total sample size per
# poll type, gives each spread estimate a weight based on the 
# poll's sample size, and adds an estimate of p from the
# combined spread) to begin your analysis:
combined_by_type = june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)

combined_by_type = combined_by_type %>%
  mutate(se_spread = 2 *  sqrt((p_hat * (1 - p_hat)) / N),
         lower = spread - 1.96 * se_spread,
         upper = spread + 1.96 * se_spread)
# What is the lower bound of the 95% confidence interval for
# online voters?
combined_by_type %>% filter(poll_type == "Online") %>% pull(lower)

# What is the upper bound of the 95% confidence interval for
# online voters?
combined_by_type %>% filter(poll_type == "Online") %>% pull(upper)

# By this analysis, we can see that none of the combined polls 
# provided a confidence interval that could predict a winner or
# even include the real value of d

# Chi-squared p-value 
# Define brexit_hit, with the following code, which computes 
# the confidence intervals for all Brexit polls in 2016 and then 
# calculates whether the confidence interval covers the actual 
# value of the spread d = -0.038
brexit_hit = brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
# What is the p-value of the chi-squared test comparing 
# the hit rate of online and telephone polls?
two_by_two = table(brexit_hit$poll_type, brexit_hit$hit)
chi_square_test = chisq.test(two_by_two)
online_hit_rate = brexit_hit %>% 
  filter(poll_type == "Online") %>%
  pull(hit) %>%
  mean()
telephone_hit_rate = brexit_hit %>% 
  filter(poll_type == "Telephone") %>% 
  pull(hit) %>% 
  mean()

# Determine which poll type has a higher probability of producing 
# a confidence interval that covers the correct value of the 
# spread. Also determine whether this difference is statistically 
# significant at a p-value cutoff of 0.05.
online_hit_rate > telephone_hit_rate
chi_square_test$p.value < 0.05

# Online polls are more likely to cover the correct value of the 
# spread and this difference is statistically significant

# Odds ratio of online and telephone poll hit rate

# Use the two-by-two table constructed in the previous 
# exercise to calculate the odds ratio between the hit 
# rate of online and telephone polls to determine the magnitude 
# of the difference in performance between the poll types.
total_online = sum(two_by_two[1,])
total_online_correct = two_by_two[1,2]
total_online_incorrect = two_by_two[1,1]
total_telephone = sum(two_by_two[2,])
total_telephone_correct = two_by_two[2,2]
total_telephone_incorrect = two_by_two[2,1]

# Calculate the odds that an online and telephone polls generate
# a confidence interval that covers the actual value of the spread.
online_correct_rate = total_online_correct / total_online
online_incorrect_rate = total_online_incorrect / total_online
odds_online = online_correct_rate / online_incorrect_rate
telephone_correct_rate = total_telephone_correct / total_telephone
telephone_incorrect_rate = total_telephone_incorrect / total_telephone
odds_telephone = telephone_correct_rate / telephone_incorrect_rate

# Calculate the odds ratio to determine how many times larger 
# the odds are for online polls to hit versus telephone polls.
odds_online / odds_telephone

# Plotting spread over time 
# Use brexit_polls to make a plot of the spread (spread) over 
# time (enddate) colored by poll type (poll_type). Use geom_smooth
# with method = "loess" to plot smooth curves with a span of 0.4. 
# Include the individual data points colored by poll type. Add a 
# horizontal line indicating the final value of d = -0.038.

brexit_polls %>%
  ggplot(aes(enddate, spread, color = poll_type)) +
  geom_smooth(method = "loess", span = 0.4) +
  geom_point() +
  geom_hline(aes(yintercept = -0.038))

# Use the following code to create the object brexit_long, 
# which has a column vote containing the three possible votes
# on a Brexit poll ("remain", "leave", "undecided") and a column
# proportion containing the raw proportion choosing that vote 
# option on the given poll:
brexit_long = brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))

# Make a graph of proportion over time colored by vote. 
# Add a smooth trendline with geom_smooth and method = "loess"
# with a span of 0.3.
brexit_long %>%
  ggplot(aes(enddate, proportion, color = vote)) +
  geom_smooth(method = "loess", span = 0.3)

# From this plot we can deduce that the undecided vote proportion
# declines over time and its around 10%-5% throughout June, 
# Over most of the selected range, leave and remain votes overlap
# and are below 50%; also, during the first half of June the leave
# vote was above remain but this difference was within the
# confidence intervals we previously found.
