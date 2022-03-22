# Distribution of female heights
# Assume the distribution of female heights is approximated by a normal 
# distribution with a mean of 64 inches and a standard deviation of 3 inches.
# If we pick a female at random, what is the probability that 
# she is 5 feet orshorter?
female_avg = 64 
female_sd = 3
pnorm(5 * 12, mean = female_avg, sd = female_sd)

# If we pick a female at random, what is the probability that
# she is 6 feet or taller?
1 - pnorm(6 * 12, mean = female_avg, sd = female_sd)

# If we pick a female at random, what is the probability that she 
# is between 61 and 67 inches?
pnorm(67, mean = female_avg, sd = female_sd) - 
  pnorm(61, mean = female_avg, sd = female_sd)

# Repeat the previous exercise, but convert everything to centimeters. 
# That is, multiply every height, including the standard deviation, by 2.54. 
# What is the answer now?
pnorm(67 * 2.54, mean = female_avg * 2.54, sd = female_sd * 2.54) - 
  pnorm(61 * 2.54, mean = female_avg * 2.54, sd = female_sd * 2.54)

# Compute the probability that the height of a randomly chosen female is within 
# 1 SD from the average height.
pnorm(female_avg + female_sd, mean = female_avg, sd = female_sd) - 
  pnorm(female_avg - female_sd, mean = female_avg, sd = female_sd)

# Imagine the distribution of male adults is approximately normal with an 
# average of 69 inches and a standard deviation of 3 inches. How tall is a 
# male in the 99th percentile?
male_avg = 69
male_sd = 3
qnorm(0.99, mean = male_avg, sd = male_sd)

# The distribution of IQ scores is approximately normally distributed. 
# The average is 100 and the standard deviation is 15. Suppose you want to know
# the distribution of the person with the highest IQ in your school district, 
# where 10,000 people are born each year.

# Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation. 
# Make a histogram of the highest IQ scores.

iq_avg = 100
iq_sd = 15
B = 1000
number_of_scores = 10000
events = replicate(B, {
  scores = rnorm(number_of_scores, mean = iq_avg, sd = iq_sd)
  max(scores)
})
hist(events)
