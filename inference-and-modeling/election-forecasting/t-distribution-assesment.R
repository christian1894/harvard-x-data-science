#  Using the t-Distribution
# We know that, with a normal distribution, only 5% of values are
# more than 2 standard deviations away from the mean.
# Calculate the probability of seeing t-distributed random 
# variables being more than 2 in absolute value when the degrees
# of freedom are 3.
1 - pt(2, 3) + pt(-2, 3)

# Plotting the t-distribution
# Now use sapply to compute the same probability for degrees of
# freedom from 3 to 50.
# Make a plot and notice when this probability converges to the 
# normal distribution's 5%.
df = seq(3, 50)
pt_func = function(degree_of_freedom) {
 1 - pt(2, degree_of_freedom) + pt(-2, degree_of_freedom)
}
probs = sapply(df, pt_func)
plot(df, probs)

# Sampling From the Normal Distribution
# In a previous section, we repeatedly took random samples of 50 
# heights from a distribution of heights. We noticed that about
# 95% of the samples had confidence intervals spanning the 
# true population mean.
# Re-do this Monte Carlo simulation, but now instead of N = 50
# N = 15, use . Notice what happens to the proportion of hits.
library(dslabs)
library(dplyr)
data(heights)
male_heights = heights %>% filter(sex == "Male") %>%
  .$height
mu = mean(male_heights)
N = 15
B = 10000
set.seed(1)
res = replicate(B, {
  data = sample(male_heights, N, replace = TRUE)
  interval = mean(data) + c(-1,1)*qnorm(0.975)*sd(data)/sqrt(N)
  between(mu, interval[1], interval[2])
})
mean(res)

# Sampling from the t-Distribution
# N = 15 is not that big. We know that heights are normally
# distributed, so the t-distribution should apply. Repeat the 
# previous Monte Carlo simulation using the t-distribution 
# instead of using the normal distribution to construct the 
# confidence intervals.
# What are the proportion of 95% confidence intervals that 
# span the actual mean height now?
res = replicate(B, {
  data = sample(male_heights, N, replace = TRUE)
  interval = mean(data) + c(-1,1)*qt(0.975, N -1)*sd(data)/sqrt(N)
  between(mu, interval[1], interval[2])
})
mean(res)
# For small sample sizes(< 30) the t-distribution seems more 
# accurate since it takes into account variability in while
# estimating the standard deviation
