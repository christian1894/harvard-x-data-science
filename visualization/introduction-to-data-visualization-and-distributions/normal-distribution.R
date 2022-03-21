library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]
# proportion of data between 2 values
mean(x > 69 & x <= 72)
# estimate proportion using normal approximation (notice those
# are similar)
avg <- mean(x)
stdev <- sd(x)
pnorm(72, avg, stdev) - pnorm(69, avg, stdev)

# normal approximation is not accurate at the 
# tails of the distribution
exact <- mean(x > 79 & x <= 81)
approx = pnorm(81, avg, stdev) - pnorm(79, avg, stdev)
exact / approx
# use pnorm to calculate the proportion over 7 feet (7*12 inches)
approx_7_feet = pnorm(84, 69, 3)
p = 1 - approx_7_feet
round(p * 10^9)
# proportion of world's 18-40 year old
# 7 footers in the NBA
N = round(p * 10^9)
10 / N
# Note: normal approximation tends to underestimate the 
# extreme values. It's possible that there are more 
#seven footers than we predicted.

# 6 Feet 8 inches(Lebron James' height)
# 1 foot = 12 inches
lebron_height = 6*12 + 8
p <- 1 - pnorm(lebron_height, 69, 3)
N <- round(p * 10^9)
150/N




