# Bayes theorem
# hypothetical cystic fibrosis test as an example
test_accuracy = 0.99
cystic_fibrosis_rate = 0.00025

# Suppose we select a random person and they test positive.
# What is the probability that they have the disease?
# Notation: D=1|0 (having or not having the disease)
# +|- (positive or negative test)
# Bayes theorem equation:
# Pr(A|B) = Pr(B|A)*Pr(A) / Pr(B)
# Applied to this example becomes:
# Pr(D=1|+) = Pr(+|D=1) * Pr(D=1) / Pr(+)
# => Pr(+|D=1) * Pr(D=1) / Pr(+|D=1) * Pr(D=1) + Pr(-|D=0)*Pr(D=0)
p_having_disease = (0.99 * 0.00025) / ((0.99 * 0.00025) + (0.01* (1 - 0.00025)))

# Bayes theorem simulation
prev = 0.00025
N = 100000
health_outcomes = sample(
  c("Healthy", "Disease"), 
  size = N,
  replace = TRUE,
  prob = c(1 - prev, prev))
healthy_people = sum(health_outcomes == "Healthy")
infected_people = sum(health_outcomes == "Disease")
test_accuracy = 0.99
tests = vector("character", N)
tests[health_outcomes == "Healthy"] = sample(
  c("-", "+"),
  size = healthy_people,
  replace = TRUE, 
  prob = c(test_accuracy, 1 -test_accuracy)
)
tests[health_outcomes == "Disease"] = sample(
  c("+", "-"),
  size = infected_people,
  replace = TRUE, 
  prob = c(test_accuracy, 1 -test_accuracy)
)
table(health_outcomes, tests)

# Bayes theorem in practice

# José Iglesias is a professional baseball player. 
# In April 2013, when he was starting his career, he was 
# performing rather well:
# | Month | At Bats | H | AVG |
# |-------|---------|---|-----|
# | April | 20      | 9 | .450|
# The batting average (`AVG`) statistic is one way of 
# measuring success. Roughly speaking, it tells us the 
# success rate when batting. An `AVG` of .450 means 
# José has been successful 45% of the times he has batted 
# (`At Bats`) which is rather high, historically speaking.
# Keep in mind that no one has finished a season with an 
# `AVG` of .400 or more since Ted Williams did it in 1941!
# To illustrate the way hierarchical models are powerful, 
# we will try to predict José's batting average at the end
# of the season. Note that in a typical season, players 
# have about 500 at bats.
# With the techniques we have learned up to now, referred 
# to as _frequentist techniques_, the best we can do is 
# a confidence interval. We can think of outcomes from
# hitting as a binomial with a success rate of p. So
# if the success rate is indeed .450, the standard error 
# of just 20 at bats is:
sqrt(.450 * (1-.450) / 20)

# This means that our confidence interval is .450 - .222 to
# .450 + .222 or .228 to .672.
# This prediction has two problems. First, it is very large, 
# so not very useful. Second, it is centered at .450, which
# implies that our best guess is that this new player 
# will break Ted Williams' record. 
# If you follow baseball, this last statement will seem wrong
# and this is because you are implicitly using a hierarchical 
# model that factors in information from years of following
# baseball. Here we show how we can quantify this intuition.
# First, let's explore the distribution of batting averages 
# for all players with more than 500 at bats during the 
# previous three seasons:

library(tidyverse)
library(Lahman)
filter(Batting, yearID %in% 2010:2012) %>% 
  mutate(AVG = H/AB) %>% 
  filter(AB > 500) %>% 
  ggplot(aes(AVG)) +
  geom_histogram(color="black", binwidth = .01) +
  facet_wrap( ~ yearID)

# mean = 0.275, sd = 0.027, 0.450 would be an anomaly
# a hiearchical model will reveal if this is due to luck or talent

# Hierarchical Model
# p ~ N(mu, tau^2)
# Y | p ~ N(p, sigma^2)
# mu being the global average, tau global sd
# p being probability of success, Y the observed
# average(taking luck into account), N a normal distribution
# with mean p and sd sigma^2
global_mu = 0.275
global_tau = 0.027
jose_p = 0.450
jose_sigma = 0.111
# p ~ N(0.275, 0.027^2)
# Y | p ~ N(p, 0.111^2)

# Posterior probability function:
# E(p | Y = y) = B * mu + (1 - B) * y
#              = mu + (1 - B) * (y - mu)
# with B = sigma ^ 2 / (sigma^2 * tau^2)
# y being average of observed data
B = jose_sigma ^ 2 / (jose_sigma ^ 2 + global_tau ^ 2)
E = global_mu + (1 - B) * (jose_p - global_mu)

# calculating SE:
# SE(p | y) ^ 2 = 1 / ((1 / sigma ^ 2) + (1 / tau ^ 2))
SE = 1 / ((1 / jose_sigma ^ 2) + (1 / global_tau ^ 2))
ci = c(E - (qnorm(0.975) * global_tau), E + (qnorm(0.975) * global_tau))

# this predicts a 0.975 CI of .285 with se = 0.052 for
# Jose Iglesias, without taking into account the 0.450 April 
# Batting average, his performance at 330 At bats was 0.293
# this suggests that his batting average was being overvalued
# at the time
