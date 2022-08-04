# Predicted values are random variables

library(HistData)
library(dslabs)
library(tidyverse)
library(ggplot2)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# Once we fit our model, we can obtain prediction of Y
# by plugging in the estimates into the regression model.
# For example, if the father’s height is x, then our 
# prediction Y_hat for the son’s height will be:
# Y_hat = β0_hat+β1_hat * x
# When we plot Y_hat versus x, we see the regression line.

# Keep in mind that the prediction Y_hat is also a 
# random variable and mathematical theory tells us what
# the standard errors are. If we assume the errors are 
# normal, or have a large enough sample size, we can use 
# theory to construct confidence intervals as well. In fact,
# the ggplot2 layer geom_smooth(method = "lm") that we 
# previously used plots Y_hat and surrounds it by 
# confidence intervals:

galton_heights %>% 
  ggplot(aes(son, father)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = 'lm')

# The R function predict takes an lm object as input and 
# returns the prediction. If requested, the standard errors 
# and other information from which we can construct confidence 
# intervals is provided:

fit = galton_heights %>%
  lm(son ~ father, data = .)
y_hat = predict(fit, se.fit = TRUE)
names(y_hat)
