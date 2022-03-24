# Statistics in the Courtroom
# In 1999 in England Sally Clark was found guilty of the 
# murder of two of her sons. Both infants were found dead
# in the morning, one in 1996 and another in 1998, and she 
# claimed the cause of death was sudden infant death 
# syndrome (SIDS). No evidence of physical harm was
# found on the two infants so the main piece of evidence 
# against her was the testimony of Professor Sir Roy Meadow,
# who testified that the chances of two infants dying of
# SIDS was 1 in 73 million. He arrived at this figure by 
# finding that the rate of SIDS was 1 in 8,500 and then 
# calculating that the chance of two SIDS cases was 
# 8,500  * 8,500 = 73 million.

# Sir Meadow's reasoning is assuming independence,
# however there could be genetic causes involved since 
# the 2 children come from the same mother.

# Recalculating the SIDS Statistics
# Let's assume that there is in fact a genetic component
# to SIDS and the the probability of
# Pr(Second case | First case) = 1/100,
# is much higher than 1 in 8,500.
# What is the probability of both of Sally Clark's sons dying of SIDS?

p_first_case = 1/8500
p_second_case = 1/100
p_both_cases = p_first_case * p_second_case

# Bayes' Rule in the Courtroom
# Many press reports stated that the expert claimed 
# the probability of Sally Clark being innocent as 1 
# in 73 million. Perhaps the jury and judge also 
# interpreted the testimony this way. This probability 
# can be written like this: 
# Pr(mother is a murderer | two children found dead with no
# evidence of harm)
# which is:
# Pr(two children found dead with no
# evidence of harm | mother is a murderer) * 
# Pr(mother is a murderer) / Pr(two 
# children found dead with no evidence of harm)

# Calculate the Probability
# Assume that the probability of a murderer finding
# a way to kill her two children without leaving 
# evidence of physical harm is:
# Pr(two children found dead with no evidence of harm | mother
# is a murderer) = 0.50
# Assume that the murder rate among mothers is 1 in 1,000,000. 
# According to Bayes' rule, what is the probability of:
# Pr(mother is a murderer | two children found dead with no
# evidence of harm)
p_mother_is_a_murderer = 1 / 1000000
p_two_children_killed_with_no_evidence_of_harm = 0.50

p = 
  (p_two_children_killed_with_no_evidence_of_harm *
  p_mother_is_a_murderer) / p_both_cases

# Back to Election Polls
# Florida is one of the most closely watched states in the 
# U.S. election because it has many electoral votes and 
# the election is generally close. Create a table with 
# the poll spread results from Florida taken during the 
# last days before the election using the sample code.

# The CLT tells us that the average of these spreads 
# is approximately normal. Calculate a spread average
# and provide an estimate of the standard error.
# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of 
# predictions for each candidate in Florida during the 
# last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns 
# containing the average spread (`avg`) and the standard
# error (`se`). Print the results to the console.
results = polls %>% summarize(avg = mean(spread), se = sd(spread)/ sqrt(n()))

# Estimate the Posterior Distribution
# The CLT tells us that our estimate of the spread d_hat
# has a normal distribution with expected value d and
# standard deviation sigma, which we calculated in a 
# previous exercise.
# Use the formulas for the posterior distribution to 
# calculate the expected value of the posterior 
# distribution if we set mu = 0 and tau = 0.01.
mu = 0
tau = 0.01
sigma = results$se
Y = results$avg
B = sigma^2 / (sigma^2 + tau^2)
E = (B * mu) + (1 - B) * Y
SE = sqrt(1 / ((1 / sigma^2) + (1 / tau^2)))

# Constructing a Credible Interval
ci = c(E - (SE * qnorm(0.975)), E + (SE * qnorm(0.975)))

# Odds of Winning Florida
# According to this analysis, what was the probability
# that Trump wins Florida?
pnorm(0, E, SE)

# Change the Priors
# We had set the prior variance tau to 0.01, reflecting 
# that these races are often close.
# Change the prior variance to include values 
# ranging from 0.005 to 0.05 and observe how the 
# probability of Trump winning Florida changes by
# making a plot.
taus <- seq(0.005, 0.05, len = 100)
p_calc = function(tau) {
  mu = 0
  sigma = results$se
  Y = results$avg
  B = sigma^2 / (sigma^2 + tau^2)
  E = (B * mu) + (1 - B) * Y
  SE = sqrt(1 / ((1 / sigma^2) + (1 / tau^2)))
  pnorm(0, E, SE)
}
results = sapply(taus, p_calc)
plot(taus, results)
