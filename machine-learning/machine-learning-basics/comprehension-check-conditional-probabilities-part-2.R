# We are now going to write code to compute conditional probabilities
# for being male in the heights dataset. Round the heights to the closest 
# inch. Plot the estimated conditional probability 
# P(x) = Pr(Male|height = x) for each x. 

library(dslabs)
library(tidyverse)
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

# In the plot we just made in Q6 we see high variability for low values
# of height. This is because we have few data points. This time use 
# the quantile 0.1 - 0.9 and the cut() function to assure each group has 
# the same number of points. Note that for any numeric vector x, you
# can create groups based on quantiles like this:
# cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE).
# Part of the code is provided here:

ps = seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

# You can generate data from a bivariate normal distrubution using the
# MASS package using the following code: 

Sigma = 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat = MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

# Using an approach similar to that used in the previous exercise, 
# let's estimate the conditional expectations and make a plot. Part of 
# the code has again been provided for you: 
ps = seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
