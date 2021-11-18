# Heights Revisited
# Let's revisit the heights dataset. For now, consider x to
# be the heights of all males in the data set. Mathematically 
# speaking, x is our population. Using the urn analogy, 
# we have an urn with the values of x in it.
# What are the population average and standard deviation of 
# our population?
library(dslabs)
library(tidyverse)
data("heights")
x = heights %>%
  filter(sex == "Male") %>%
  .$height

# Sample the population of heights
# Call the population average computed above mu and the 
# standard deviation sigma. Now take a sample of size 50, 
# with replacement, and construct an estimate for mu and sigma.
set.seed(1)
N = 50
mu = mean(x)
sigma = sd(x)
X_hat = sample(x, N, replace = TRUE)
mean(X_hat)
sd(X_hat)

# Confidence Interval Calculation
# We will use X_hat as our estimate of the heights in the
# population from our sample size N. We know from previous 
# exercises that the standard estimate of 
# our error X_hat - mu is sigma / sqrt(N).
# Construct a 95% confidence interval for mu.
X_bar = mean(X_hat)
se = sd(X_hat) / sqrt(N)
ci = c(mu - (se * qnorm(0.975)), mu + (se * qnorm(0.975)))

# Monte Carlo Simulation for Heights
# Now run a Monte Carlo simulation in which you compute
# 10,000 confidence intervals as you have just done. What 
# proportion of these intervals include mu?
B = 10000
results = replicate(B, {
  X_hat = sample(x, N, replace = TRUE)
  X_bar = mean(X_hat)
  se = sd(X_hat) / sqrt(N)
  lower = X_bar - (se * qnorm(0.975)) 
  upper = X_bar + (se * qnorm(0.975))
  between(mu, lower, upper)
})
mean(results)

# Visualizing Polling Bias
# In this section, we used visualization to motivate the 
# presence of pollster bias in election polls. Here we will
# examine that bias more rigorously. Lets consider two
# pollsters that conducted daily polls and look at
# national polls for the month before the election.
# Is there a poll bias? Make a plot of the 
# spreads for each poll.
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

polls %>% ggplot(aes(pollster, spread)) +
  geom_boxplot() + geom_point(alpha = 0.2)

# Compute the Estimates
# The answer to the previous question depends on sigma_1
# and sigma_2, which we don't know. We learned that 
# we can estimate these values using the sample
# standard deviation.
# Compute the estimates of sigma_1 and sigma_2.
sigma = polls %>%
  group_by(pollster) %>% 
  summarize(s = sd(spread))

# Calculate the 95% Confidence Interval of the Spreads
# We have constructed a random variable that has expected 
# value b2 - b1, the pollster bias difference. If our 
# model holds, then this random variable has an approximately 
# normal distribution. The standard error of this 
# random variable depends on sigma_1 and sigma_2
# , but we can use the sample standard deviations we 
# computed earlier. We have everything we need to answer 
# our initial question b2 - b1: is different from 0?
# Construct a 95% confidence interval for the difference
# b2 and b1. Does this interval contain zero?
res = polls %>%
  group_by(pollster) %>%
  summarize(sd = sd(spread), avg = mean(spread), n = n()) %>%
  data.frame()
poll2 = res[which(res$avg == max(res$avg)),]
poll1 = res[which(res$avg == min(res$avg)),]
estimate = poll2$avg - poll1$avg
se_hat = sqrt((poll2$sd ^ 2 / poll2$n) + (poll1$sd ^ 2 / poll1$n))
ci = c(estimate - (se_hat * qnorm(0.975)), estimate + se_hat * qnorm(0.975))

# Calculate the P-value
# The confidence interval tells us there is relatively strong 
# pollster effect resulting in a difference of about 5%. 
# Random variability does not seem to explain it.
# Compute a p-value to relay the fact that chance does not
# explain the observed pollster effect.
2 * (1 - pnorm(estimate / se_hat))

# Comparing Within-Poll and Between-Poll Variability
# We compute statistic called the t-statistic by dividing our
# estimate of b2 - b1 by its estimated standard error:
# (Y_hat1 - Y_hat2) / sqrt(s2^2/N2 + s1^2/N1)
# Later we learn will learn of another approximation for 
# the distribution of this statistic for values of
# N2 and N1 that aren't large enough for the CLT.
# Note that our data has more than two pollsters. 
# We can also test for pollster effect using all 
# pollsters, not just two. The idea is to compare the 
# variability across polls to variability within polls.
# We can construct statistics to test for effects and 
# approximate their distribution. The area of statistics 
# that does this is called Analysis of Variance or ANOVA. 
# We do not cover it here, but ANOVA provides a very useful 
# set of tools to answer questions such as: is there 
# a pollster effect?
# Compute the average and standard deviation for each
# pollster and examine the variability across the 
# averages and how it compares to the variability 
# within the pollsters, summarized by the standard deviation.
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()
var = polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread))
