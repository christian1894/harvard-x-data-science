# An American roulette wheel has 18 red, 18 black, 
# and 2 green pockets. Each red and black pocket is 
# associated with a number from 1 to 36. The two remaining 
# green slots feature "0" and "00". Players place bets on 
# which pocket they think a ball will land in after the 
# wheel is spun. Players can bet on a specific number 
# (0, 00, 1-36) or color (red, black, or green).

# What are the chances that the ball lands in a green pocket?
green <- 2
black <- 18
red <- 18
probability_green = green / (black + green + red)
probability_not_green = 1 - probability_green

# In American roulette, the payout for winning on green is $17.
# This means that if you bet $1 and it lands on green,
# you get $17 as a prize.

# Create a model to predict your winnings from betting on 
# green one time.

X <- sample(c(17,-1), 1, prob = c(probability_green, probability_not_green))

# Calculate the expected outcome if you win $17 if the
# ball lands on green and you lose $1 if the ball doesn't 
# land on green
EX = (17 * probability_green) + (-1 * (1 - probability_green)) 

# Compute the standard error of the random variable
SE = abs(-1 - 17) * sqrt(probability_green * (1 - probability_green))

# Now create a random variable S that sums your winnings
# after betting on green 1,000 times.
n = 1000
S = sample(c(17, -1),
           n,
           prob = c(probability_green, probability_not_green),
           replace = TRUE)
sum(S)

# In the previous exercise, you generated a vector of random outcomes,
# after betting on green 1,000 times.
# What is the expected value of S?
EX * 1000

# What is the standard error of S?
sqrt(1000) * SE
