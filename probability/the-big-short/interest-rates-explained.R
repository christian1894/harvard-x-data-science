# Interest rates for loans are set using the probability of 
# loan defaults to calculate a rate that minimizes the 
# probability of losing money.
# We can define the outcome of loans as a random variable.
# We can also define the sum of outcomes of many loans as a 
# random variable.
# The Central Limit Theorem can be applied to fit a normal 
# distribution to the sum of profits over many loans. We 
# can use properties of the normal distribution to calculate 
# the interest rate needed to ensure a certain probability 
# of losing money for a given probability of default.

number_of_loans = 1000
loss_per_foreclosure = -200000
probability_default = 0.02

# sampling model
defaults = sample(
  c(1, 0), 
  size = number_of_loans,
  prob = c(probability_default, 1 - probability_default),
  replace = TRUE)
losses = sum(defaults) * loss_per_foreclosure

# Monte carlo simulation
B = 10000
losses = replicate(B, {
  defaults = sample(
    c(1, 0), 
    size = number_of_loans,
    prob = c(probability_default, 1 - probability_default),
    replace = TRUE)
  sum(defaults) * loss_per_foreclosure
})
mean(losses)

# Plot expected losses

library(tidyverse)
data.frame(losses_in_millions = losses / 10^6) %>% 
  ggplot(aes(losses_in_millions)) + 
  geom_histogram(binwidth = 0.6, col = "black")

# Expected value and standard error of the sum of 1,000 loans
mu = number_of_loans * 
  (loss_per_foreclosure * probability_default + 
     (0 * (1 - probability_default)))
sigma = sqrt(number_of_loans) * 
  abs(loss_per_foreclosure) * 
  (sqrt(probability_default * (1 - probability_default)))

# Calculating interest rates for expected value of 0
b = loss_per_foreclosure * probability_default /
  (1 - probability_default) 
interest_rate = b / 180000

# Calculating interest rate for 1% probability of losing money
l = loss_per_foreclosure
p = probability_default
n = number_of_loans
z = qnorm(0.01)
x = -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
interest_rate = x / 180000
loss_per_foreclosure * p + (x * (1 - p))
n * (loss_per_foreclosure * p + x * (1 - p))

# Monte Carlo simulation for 1% probability of losing money
B = 10000
profit =  replicate(B, {
  draws = sample(c(x, loss_per_foreclosure), 
                 size = number_of_loans,
                 replace = TRUE,
                 c(1 - probability_default, probability_default)
                 )
  sum(draws)
})
mean(profit)
mean(profit < 0)

# Expected value with higher default rate and interest rate
probability_default = 0.04
loss_per_foreclosure = -200000
interest_rate = 0.05
profit = interest_rate * 180000
loss_per_foreclosure * probability_default +
  (profit * (1 - probability_default)) 

# Calculating number of loans for desired 
# probability of losing money
z =- qnorm(0.01)
l = loss_per_foreclosure
n = ceiling((z^2*(profit-l)^2*probability_default*(1-probability_default))/(l*probability_default + profit*(1-probability_default))^2)
n*(l*probability_default + profit * (1-probability_default))    # expected profit over n loans

# Monte Carlo simulation with known default probability
B = 10000
profits = replicate(B, {
  draws = sample(c(profit, loss_per_foreclosure),
                 replace = TRUE, 
                 size = n, 
                 prob = c(1 - probability_default, probability_default))
  sum(draws)
})
mean(profits)

# Monte Carlo simulation with unknown default probability
probability_default = 0.04
profits = replicate(B, {
  new_probability =  probability_default + 
    sample(seq(-0.01, 0.01, len = 100), 1)
  draws = sample(c(profit, loss_per_foreclosure),
                 size = n,
                 replace = TRUE, 
                 prob = c(1 - new_probability, new_probability)
                 )
  sum(draws)
})
mean(profits)
mean(profits < 0)
mean(profits < -10000000)
