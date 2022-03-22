library(tidyverse)
library(ggplot2)

# For the three year period 2016-2018, ACT standardized 
# test scores were approximately normally distributed with 
# a mean of 20.9 and standard deviation of 5.7. 
# (Real ACT scores are integers between 1 and 36, but we
# will ignore this detail and use continuous values instead.)

# First we'll simulate an ACT test score dataset and answer 
# some questions about it.
# Set the seed to 16, then use rnorm() to generate a normal
# distribution of 10000 tests with a mean of 20.9 and 
# standard deviation of 5.7. Save these values as act_scores. 
# You'll be using this dataset throughout these four multi-part questions.
set.seed(16)
act_scores_mean = 20.9
act_scores_sd = 5.7
act_scores = rnorm(10000, mean = act_scores_mean, sd = act_scores_sd)

# What is the mean of act_scores
mean(act_scores)

# What is the standard deviation of act_scores
sd(act_scores)

# A perfect score is 36 or greater (the maximum reported score is 36).
# In act_scores, how many perfect scores are there 
# out of 10,000 simulated tests?
sum(act_scores >= 36)

# In act_scores, what is the probability of an ACT score greater than 30?
mean(act_scores > 30)

# In act_scores, what is the probability of an ACT score less than or 
# equal to 10?
mean(act_scores <= 10)

# Set x equal to the sequence of integers 1 to 36. Use dnorm to determine
# the value of the probability density function over x given 
# a mean of 20.9 and standard deviation of 5.7; save the
# result as f_x. Plot x against f_x.

x = seq(36)
f_x = dnorm(x, mean = act_scores_mean, sd = act_scores_sd)
plot(x, f_x, type = "l")

# Convert to z-scores
act_z_scores = (act_scores - mean(act_scores))/sd(act_scores)

# What is the probability of a Z-score greater than 2 
# (2 standard deviations above the mean)?
mean(act_z_scores > 2)

# What ACT score value corresponds to 2 standard deviations
# above the mean (Z = 2)?
2*sd(act_scores) + mean(act_scores)

# A Z-score of 2 corresponds roughly to the 97.5th percentile.
# Use qnorm() to determine the 97.5th percentile of normally
# distributed data with the mean and standard deviation 
# observed in act_scores.
# What is the 97.5th percentile of act_scores?
qnorm(0.975, mean = act_scores_mean, sd = act_scores_sd)

# Write a function that takes a value and produces the 
# probability of an ACT score less than or equal to that 
# value (the CDF). Apply this function to the range 1 to 36.
values = seq(1, 36)
get_cdf = function(value) {
  mean(act_scores <= value)
}
probabilities = sapply(values, get_cdf)
min(which(probabilities >= 0.95))

# Use qnorm() to determine the expected 95th percentile, 
# the value for which the probability of receiving that score or lower is 0.95, 
# given a mean score of 20.9 and standard deviation of 5.7.
# What is the expected 95th percentile of ACT scores?
qnorm(0.95, mean = act_scores_mean, sd = act_scores_sd)

# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), 
# the 1st through 99th percentiles of the act_scores data. Save these as 
# sample_quantiles.
quantiles = seq(0.01, 0.99, 0.01)
sample_quantiles = quantile(act_scores, probs = quantiles)
names(sample_quantiles[max(which(sample_quantiles < 26))])

# Make a corresponding set of theoretical quantiles 
# using qnorm() over the interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 
# and standard deviation 5.7. Save these as theoretical_quantiles. 
# Make a QQ-plot graphing sample_quantiles on the y-axis versus
# theoretical_quantiles on the x-axis.
theoretical_quantiles = 
  qnorm(quantiles, mean = act_scores_mean, sd = act_scores_sd)
qplot(theoretical_quantiles, sample_quantiles) + geom_abline()
