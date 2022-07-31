# Assessment: Introduction to Linear Models

# We run a linear model for sons’ heights vs. fathers’ heights
# using the Galton height data, and get the following results: 

library(tidyverse)
library(dslabs)
library(HistData)
data("GaltonFamilies")

galton_heights = GaltonFamilies %>%
  filter(gender == "male") %>%
  mutate(son = childHeight) %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, son)
lm(son ~ father, data = galton_heights)

# For every inch we increase the father’s height, 
# the predicted son’s height grows by 0.5 inches.

# We want the intercept term for our model to be more 
# interpretable, so we run the same model as before but 
# now we subtract the mean of fathers’ heights from each
# individual father’s height to create a new variable
# centered at zero. 

galton_heights = galton_heights %>%
  mutate(father_centered = father - mean(father))

# We run a linear model using this centered fathers’ height variable. 

lm(son ~ father_centered, data = galton_heights)

# Because the fathers’ heights (the independent variable) 
# have been centered on their mean, the intercept represents 
# the height of the son of a father of average height. 
# In this case, that means that the height of a son of
# a father of average height is 70.45 inches.

# If we had not centered fathers’ heights to its mean, then 
# the intercept would represent the height of a son when a 
# father’s height is zero.

# Suppose we fit a multivariate regression model for 
# expected runs based on BB and HR:

# E [R | BB = x1, HR = x2] = B0 + B1x1 + B2x2

# Suppose we fix BB = x1. Then we observe a linear 
# relationship between runs and HR with intercept of:
# B0 + B1x1

# Which of the following are assumptions for the errors in Ei
# a linear regression model?

# - The Ei are independent from each other
# - Have expected value 0
# - Ei's variance is constant
