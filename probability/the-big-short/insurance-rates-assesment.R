options(digits = 3)
library(tidyverse)
library(dslabs)
data(death_prob)

# An insurance company offers a one-year term life insurance 
# policy that pays $150,000 in the event of death within 
# one year. The premium (annual cost) for this policy for 
# a 50 year old female is $1,150. Suppose that in the 
# event of a claim, the company forfeits the premium 
# and loses a total of $150,000, and if there is no 
# claim the company gains the premium amount of $1,150. 
# The company plans to sell 1,000 policies to this demographic.

# Use death_prob to determine the death probability 
# of a 50 year old female, p.
p_death = death_prob %>%
  filter(sex == "Female" & age == 50) %>% 
  pull(prob) %>%
  mean()

# The loss in the event of the policy holder's death is 
# -$150,000 and the gain if the policy holder remains 
# alive is the premium $1,150.
# What is the expected value of the company's net 
# profit on one policy for a 50 year old female?

loss_per_collection = -150000
gain = 1150
number_of_policies = 1000
mu = loss_per_collection * p_death + (gain * (1 - p_death))

# Calculate the standard error of the profit on one 
# policy for a 50 year old female.

sigma = abs(gain - loss_per_collection) * sqrt(1 * p_death * (1 - p_death))

# What is the expected value of the company's profit over all 
# 1,000 policies for 50 year old females?
mu = number_of_policies * ((gain * (1 - p_death)) +
  (loss_per_collection * p_death))

# What is the standard error of the sum of the expected 
# value over all 1,000 policies for 50 year old females?
sigma = sqrt(number_of_policies) *  
  (abs(loss_per_collection - gain) * sqrt(p_death * (1 - p_death)))

# Use the Central Limit Theorem to calculate the probability
# that the insurance company loses money on this set of
# 1,000 policies.
pnorm(0, mu, sigma)

# Use death_prob to determine the probability of death 
# within one year for a 50 year old male.
male_p_death = death_prob %>% 
  filter(sex == "Male" & age == 50) %>%
  pull(prob)

# Suppose the company wants its expected profits from 
# 1,000 50 year old males with $150,000 life insurance
# policies to be $700,000. Use the formula for expected 
# value of the sum of draws with the following values
# and solve for the premium b:
p = male_p_death
n = 1000
S = 700000
a = -150000
b = ((S / n) - (a * p)) / (1 - p)
b

# Using the new 50 year old male premium rate, calculate the
# standard error of the sum of 1,000 premiums.
sigma = sqrt(n) * (abs(b - a) * (sqrt(p * (1 - p))))
sigma

# What is the probability of losing money on a series of 
# 1,000 policies to 50 year old males?
mu = n * ((a * p) + (b * (1 - p)))
pnorm(0, mu, sigma)

# Life insurance rates are calculated using mortality 
# statistics from the recent past. They are priced such
# that companies are almost assured to profit as long as 
# the probability of death remains similar. If an event 
# occurs that changes the probability of death in a given
# age group, the company risks significant losses.

# In this 6-part question, we'll look at a scenario in which a
# lethal pandemic disease increases the probability of death 
# within 1 year for a 50 year old to .015. Unable to predict 
# the outbreak, the company has sold 1,000 $150,000 life
# insurance policies for $1,150.

# What is the expected value of the company's profits over 1,000 policies?
p_death = 0.015
number_of_policies = 1000
loss_per_collection = -150000
profit_per_policy = 1150
mu = number_of_policies * 
  ((loss_per_collection * p_death) + 
     (profit_per_policy * (1 - p_death)))
mu

# What is the standard error of the expected value of the 
# company's profits over 1,000 policies?
sigma = sqrt(number_of_policies) * 
  (abs(profit_per_policy - loss_per_collection) *
     (sqrt(p_death * (1 - p_death))))
sigma

# What is the probability of the company losing money?
pnorm(0, mu, sigma)

# What is the probability of losing more than $1 million?
pnorm(-10 ^ 6, mu, sigma)

# Investigate death probabilities p <- seq(.01, .03, .001).
# What is the lowest death probability for which the chance 
# of losing money exceeds 90%?
death_probabilities = seq(.01, .03, .001)
get_chances_of_losing_money = function(death_probability) {
  number_of_policies = 1000
  loss_per_collection = -150000
  profit_per_policy = 1150
  mu = number_of_policies * 
    ((loss_per_collection * death_probability) + 
       (profit_per_policy * (1 - death_probability)))
  sigma = sqrt(number_of_policies) * 
    (abs(profit_per_policy - loss_per_collection) *
       (sqrt(death_probability * (1 - death_probability))))
  pnorm(0, mu, sigma)
}

chances_of_losing_money = 
  sapply(death_probabilities, get_chances_of_losing_money)
min(death_probabilities[chances_of_losing_money > 0.9])

# Investigate death probabilities p <- seq(.01, .03, .0025).
# What is the lowest death probability for which the chance 
# of losing over $1 million exceeds 90%?
death_probabilities = seq(.01, .03, .0025)
get_chances_of_losing_money = function(death_probability) {
  number_of_policies = 1000
  loss_per_collection = -150000
  profit_per_policy = 1150
  mu = number_of_policies * 
    ((loss_per_collection * death_probability) + 
       (profit_per_policy * (1 - death_probability)))
  sigma = sqrt(number_of_policies) * 
    (abs(profit_per_policy - loss_per_collection) *
       (sqrt(death_probability * (1 - death_probability))))
  pnorm(-10 ^ 6, mu, sigma)
}

chances_of_losing_money = 
  sapply(death_probabilities, get_chances_of_losing_money)
min(death_probabilities[chances_of_losing_money > 0.9])

# Define a sampling model for simulating the total profit 
# over 1,000 loans with probability of claim p_loss = .015, 
# loss of -$150,000 on a claim, and profit of $1,150 when 
# there is no claim. Set the seed to 25, then run the model once.
set.seed(25)
number_of_policies = 1000
p_death = 0.015
profit_per_policy = 1150
loss_per_collection = -150000
profits = sample(
  c(loss_per_collection, profit_per_policy), 
  replace = TRUE,
  size = number_of_policies, 
  prob = c(p_death, 1 - p_death)
)
sum(profits) / 10 ^ 6

# Set the seed to 27, then run a Monte Carlo simulation 
# of your sampling model with 10,000 replicates to simulate 
# the range of profits/losses over 1,000 loans.
# What is the observed probability of losing $1 million or more?
set.seed(27)
B = 10000
events = replicate(B, {
  profits = sample(
    c(loss_per_collection, profit_per_policy), 
    replace = TRUE,
    size = number_of_policies, 
    prob = c(p_death, 1 - p_death)
  )
  sum(profits)
})
mean(events < -10 ^ 6)

# Suppose that there is a massive demand for life 
# insurance due to the pandemic, and the company wants 
# to find a premium cost for which the probability of 
# losing money is under 5%, assuming the death rate stays 
# stable at 0.015.

# Calculate the premium required for a 5% chance of 
# losing money given 1000 loans, probability of death 0.015, and 
# loss per claim -150000. Save this premium as x for use in 
# further questions.
chances_of_losing_money = 0.05
p_death = 0.015
loss_per_collection = -150000
number_of_policies = 1000
l = loss_per_collection
p = p_death
n = number_of_policies
z = qnorm(chances_of_losing_money)
x = -l*(n*p-z*sqrt(n*p*(1-p)))/(n*(1-p)+z*sqrt(n*p*(1-p)))

# what is the expected profit per policy at this rate?
mu = (loss_per_collection * p_death) + (x * (1 - p_death))

# What is the expected profit over 1,000 policies?
mu = number_of_policies * 
  ((loss_per_collection * p_death) + (x * (1 - p_death)))

# Run a Monte Carlo simulation with B=10000to determine 
# the probability of losing money on 1,000 policies given 
# the new premium x, loss on a claim of $150,000,
# and probability of claim . Set the seed to 28 before 
# running your simulation.
set.seed(28)
B = 10000
events = replicate(B, {
  profits = sample(
    c(loss_per_collection, x),
    size = number_of_policies, 
    replace = TRUE, 
    prob = c(p_death, 1 - p_death)
  )
  sum(profits)
})

# What is the probability of losing money?
mean(events < 0)

# The company cannot predict whether the pandemic death rate 
# will stay stable. Set the seed to 29, then write a Monte
# Carlo simulation that for each of B = 10000 iterations:
# randomly changes by adding a value between -0.01 and 0.01 
# with sample(seq(-0.01, 0.01, length = 100), 1)
# uses the new random to generate a sample of n = 1000 policies 
# with premium x and loss per claim l = -150000
# returns the profit over policies (sum of random variable)
set.seed(29)
B = 10000
events = replicate(B, {
  p_death_variability = sample(seq(-0.01, 0.01, length = 100), 1)
  p_death = p_death + p_death_variability
  profits = sample(
    c(loss_per_collection, x),
    size = number_of_policies, 
    replace = TRUE, 
    prob = c(p_death, 1 - p_death)
  )
  sum(profits)
  }
)

# What is the expected value over 1,000 policies?
mean(events)

# What is the probability of losing money?
mean(events < 0)

# What is the probability of losing more than $1 million?
mean(events < -10 ^ 6)
