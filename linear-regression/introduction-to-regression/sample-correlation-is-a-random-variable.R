# Sample correlation is a random variable
# Before we continue connecting correlation to regression, 
# let’s remind ourselves about random variability.
# In most data science applications, we observe data that 
# includes random variation. For example, in many cases,
# we do not observe data for the entire population of interest
# but rather for a random sample. As with the average and 
# standard deviation, the sample correlation is the most 
# commonly used estimate of the population correlation. 
# This implies that the correlation we compute and use as
# a summary is a random variable.
# By way of illustration, let’s assume that the 179 pairs 
# of fathers and sons is our entire population. A less fortunate
# geneticist can only afford measurements from a random sample of 
# 25 pairs. The sample correlation can be computed with:
library(dslabs)
library(tidyverse)
library(HistData)
data("GaltonFamilies")
galton_heights = GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  ungroup() %>%
  summarize(father, son = childHeight)
R = sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(correlation = cor(father, son)) %>% pull(correlation)

# R is a random variable. We can run a Monte Carlo simulation to 
# see its distribution:
n = 179
B = 1000
events = replicate(B, {
  R = sample_n(galton_heights, n, replace = TRUE) %>%
    summarize(correlation = cor(father, son)) %>% pull(correlation)
  R
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
mean(events)
sd(events)

# So, when interpreting correlations, remember that correlations 
# derived from samples are estimates containing uncertainty.

# Also, note that because the sample correlation is an average of 
# independent draws, the central limit actually applies. Therefore,
# for large enough N, the distribution of R is approximately normal 
# with expected value ρ. The standard deviation, which is somewhat
# complex to derive, is √1−r^2/N−2.

# In our example, N=25 does not seem to be large enough to make
# the approximation a good one:
as.data.frame(events) %>%
  ggplot(aes(sample = events)) +
  stat_qq() +
  geom_abline(intercept = mean(events),slope = sd(events))

# If you increase N, you will see the distribution converging to normal.
