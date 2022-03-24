# Sample average
# Write a function called `take_sample` that takes `p` and 
# `N` as arguments and returns the average value of a randomly
# sampled population.
take_sample = function(p, N) {
  samples = sample(c(0, 1),
                   size = N,
                   replace = TRUE,
                   prob = c(1 - p, p))
  mean(samples)
}

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `p` as the proportion of Democrats in the population being polled
p = 0.45

# Define `N` as the number of people polled
N = 100

# Call the `take_sample` function to determine the sample average of `N` randomly selected people from a population containing a proportion of Democrats equal to `p`. Print this value to the console.
take_sample(p, N)

# Distribution of errors - 1
# Assume the proportion of Democrats in the population 
# equals 0.45 and that your sample size is 100 polled voters. 
# The take_sample function you defined previously generates
# our estimate X hat.
# Replicate the random sampling 10,000 times and calculate 
# p - X hat for each random sample. Save these differences as 
# a vector called errors. Find the average of errors 
# and plot a histogram of the distribution.
B = 10000
errors = replicate(B, {
  samples = take_sample(p, N)
  X_hat = mean(samples)
  p - X_hat
})
mean(errors)

# Average size of error
# The error p - X_hat is a random variable. In practice, 
# the error is not observed because we do not know the 
# actual proportion of Democratic voters, p. However, we 
# can describe the size of the error by 
# constructing a simulation. 
# What is the average size of the error if we define 
# the size by taking the absolute value p - X_bar?
errors = replicate(B, p - take_sample(p, N))
errors_abs = abs(errors)
mean(errors_abs)

# Standard deviation of the spread:
# The standard error is related to the typical size 
# of the error we make when predicting. We say size because, 
# as we just saw, the errors are centered around 0. 
# In that sense, the typical error is 0. For mathematical 
# reasons related to the central limit theorem, 
# we actually use the standard deviation of errors 
# rather than the average of the absolute values.
# As we have discussed, the standard error is the square 
# root of the average squared distance (X_hat - p) ^ 2. 
# The standard deviation is defined as the square root of
# the distance squared.
# Calculate the standard deviation of the spread.
sqrt(mean(errors ^ 2))

# Estimating the standard error
# The theory we just learned tells us what this standard 
# deviation is going to be because it is the standard error of X_hat.
# Estimate the standard error given an expected value 
# of 0.45 and a sample size of 100.
p = 0.45
N = 100
SE = sqrt(p * (1 - p) / N)

# Standard error of the estimate
# In practice, we don't know p, so we construct an estimate
# of the theoretical prediction based by plugging in X_hat for p.
# Calculate the standard error of the estimate
X = sample(
  c(0, 1), 
  replace = TRUE,
  size = N,
  prob = c(1 - p, p)
  
)
X_bar = mean(X)
SE = sqrt(X_bar * (1 - X_bar) / N)
SE

# Plotting the standard error
# The standard error estimates obtained from the Monte 
# Carlo simulation, the theoretical prediction, and the 
# estimate of the theoretical prediction are all very close,
# which tells us that the theory is working. This gives 
# us a practical approach to knowing the typical error we 
# will make if we predict p with X_hat.
# The theoretical result gives us an idea of how large a 
# sample size is required to obtain the precision we need. 
# Earlier we learned that the largest standard errors 
# occur for p =0.5.
# Create a plot of the largest standard error for N
# ranging from 100 to 5,000. Based on this plot, how 
# large does the sample size have to be to have a 
# standard error of about 1%?
N = seq(100, 5000, len = 100)
p = 0.5
se = sqrt(p*(1-p)/N)
library(ggplot2)
library(tidyverse)
data.frame(se = se, N = N) %>% ggplot(aes(se, N)) +
  geom_line()

# Plotting the errors
# Make a qq-plot of the errors you generated previously 
# to see if they follow a normal distribution.
p = 0.45
N = 100
B = 10000
errors = replicate(B, p - take_sample(p, N))
qqnorm(errors)
qqline(errors)

# Estimating the probability of a specific value of X-bar
# If p = 0.45 and N = 100, use the central limit theorem 
# to estimate the probability that X_hat > 0.5
p = 0.45
N = 100
X_hat = p
SE = sqrt((p * (1 - p)) / N)
1 - pnorm(0.5, X_hat, SE)

#  Estimating the probability of a specific error size
# Assume you are in a practical situation and you don't know p.
# Take a sample of size N = 100 and obtain a sample 
# average of X_hat = 0.51
# What is the CLT approximation for the probability 
# that your error size is equal or larger than 0.01?
N = 100
X_hat = 0.51
SE_hat = sqrt((X_hat * (1 - X_hat)) / N)
1 - (pnorm(0.01, 0, SE_hat) - pnorm(-0.01, 0, SE_hat)) 
