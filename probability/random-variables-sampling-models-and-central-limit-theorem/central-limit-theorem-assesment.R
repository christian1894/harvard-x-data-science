# American Roulette probability of winning money
# What is the probability that you end up winning money if you bet on 
# green 100 times?

# Assign a variable `p_green` as the probability of the ball
# landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of 
# the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if 
# you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Using the expected value 'avg' and standard error 'se', 
# the probability that you win money betting on green 100 times.
1 - pnorm(0, mean = avg, sd = se)

# Create a Monte Carlo simulation that generates 10,000 
# outcomes of S, the sum of 100 bets.

# Compute the average and standard deviation of the resulting 
# list and compare them to the expected value (-5.263158) and 
# standard error (40.19344) for S that you calculated previously.
B = 10000
S = replicate(B, {
  draws = sample(c(17, -1), size = 100, replace = TRUE, prob = c(p_green, p_not_green))
  sum(draws)
})

mean(S)
sd(S)

# Calculate the proportion of outcomes in the vector `S` that exceed $0
mean(S > 0)

# Now create a random variable that contains your average
# winnings per bet after betting on green 10,000 times
n = 10000
X = sample(c(17, -1), size = n, replace = TRUE, prob = c(p_green, p_not_green))
Y = mean(X)

# What is the expected value of Y, the average outcome per 
# bet after betting on green 10,000 times?
EY = (17  * p_green) + (-1  * (1 - p_green))

# What is the standard error of Y, the average result of 10,000 spins?
SEY = abs((17 - -1))*sqrt(p_green*p_not_green) / sqrt(n)

# What is the probability that your winnings are positive after betting
# on green 10,000 times?
1 - pnorm(0, mean = EY, sd = SEY)

# Create a Monte Carlo simulation that generates 10,000 outcomes of
# S, the average outcome from 10,000 bets on green.
# Compute the average and standard deviation of the resulting list 
# to confirm the results from previous exercises using the Central Limit Theorem.
B = 10000
S = replicate(B, {
  n = 10000
  X = sample(c(17, -1), size = n, replace = TRUE, prob = c(p_green, p_not_green))
  mean(X)
})
mean(S)
sd(S)

# Compute the proportion of outcomes in the vector 'S'
# where you won more than $0
mean(S > 0)
