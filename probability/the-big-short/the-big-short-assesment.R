# Bank earnings
# Say you manage a bank that gives out 10,000 loans. 
# The default rate is 0.03 and you lose $200,000 in 
# each foreclosure.
# Create a random variable S that contains the earnings 
# of your bank. Calculate the total amount of money
# lost in this scenario.
number_of_loans = 10000
default_rate = 0.03
loss_per_foreclosure = -200000
defaults = sample(
  c(0, 1),
  replace = TRUE,
  size = number_of_loans,
  prob = c(1 - default_rate, default_rate)
)
S = sum(defaults) * loss_per_foreclosure

# Run a Monte Carlo simulation with 10,000 outcomes for S, 
# the sum of losses over 10,000 loans. Make a histogram 
# of the results.
B = 10000
events = replicate(B, {
  loans = sample(
    c(0, 1),
    replace = TRUE, 
    size = number_of_loans,
    prob = c(1 - default_rate, default_rate)
  )
  sum(loans) * loss_per_foreclosure
})
hist(events)

# What is the expected value of S, the sum of losses over
# 10,000 loans? For now, assume a bank makes no money if 
# the loan is paid.
n = 10000
loss_per_foreclosure = -200000
default_rate = 0.03

mu = n * (loss_per_foreclosure * default_rate) +
  (0 * (1 - default_rate))

# What is the standard error of S?
sigma = sqrt(n) * 
  abs(loss_per_foreclosure) * 
  (sqrt(default_rate * (1 - default_rate)))

# So far, we've been assuming that we make no money when 
# people pay their loans and we lose a lot of money 
# when people default on their loans. Assume we give 
# out loans for $180,000. How much money do we need to 
# make when people pay their loans so that our net loss is $0?
# In other words, what interest rate do we need to charge 
# in order to not lose money?
x = -(loss_per_foreclosure * default_rate) /
  (1 - default_rate)
180000 / x

# With the interest rate calculated in the last example, 
# we still lose money 50% of the time. What should the 
# interest rate be so that the chance of losing money 
# is 1 in 20? 
l <- loss_per_foreclosure
z = qnorm(0.05)
n = number_of_loans
p = default_rate
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x / 180000
