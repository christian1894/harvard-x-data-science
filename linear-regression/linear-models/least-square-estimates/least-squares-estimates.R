# Least squares estimates

# We have described how if data is bivariate normal then the 
# conditional expectations follow the regression line. The 
# fact that the conditional expectation is a line is not an
# extra assumption but rather a derived result. However, in 
# practice it is common to explicitly write down a model 
# that describes the relationship between two or more
# variables using a linear model.

# We note that “linear” here does not refer to lines 
# exclusively, but rather to the fact that the conditional 
# expectation is a linear combination of known quantities.
# In mathematics, when we multiply each variable by a 
# constant and then add them together, we say we formed 
# a linear combination of the variables. For example, 3x−4y+5z
# is a linear combination of x, y, and z. We can also 
# add a constant so 2+3x−4y+5z is also linear combination 
# of x, y, and z.

# So β0+β1x1+β2x2, is a linear combination of x1 and x2.
# The simplest linear model is a constant β0; the second 
# simplest is a line β0+β1x. If we were to specify a 
# linear model for Galton’s data, we would denote the N 
# observed father heights with x1,…,xn, then we model the N
# son heights we are trying to predict with:
# Yi=β0+β1xi+εi,i=1,…,N. Here xi is the father’s height,
# which is fixed (not random) due to the conditioning, 
# and Yi is the random son’s height that we want to
# predict. We further assume that εi are independent 
# from each other, have expected value 0 and the 
# standard deviation, call it σ, does not depend on i.

# In the above model, we know the xi, but to have a useful 
# model for prediction, we need β0 and β1. We estimate 
# these from the data. Once we do this, we can predict 
# son’s heights for any father’s height x. We show how 
# to do this in the next section.

# Note that if we further assume that the ε is normally 
# distributed, then this model is exactly the same one 
# we derived earlier by assuming bivariate normal data.
# A somewhat nuanced difference is that in the first 
# approach we assumed the data was bivariate normal and 
# that the linear model was derived, not assumed. 
# In practice, linear models are just assumed without
# necessarily assuming normality: the distribution of the εs is 
# not specified. Nevertheless, if your data is bivariate
# normal, the above linear model holds. If your data
# is not bivariate normal, then you will need to have 
# other ways of justifying the model.

# Interpreting linear models

# One reason linear models are popular is that they are 
# interpretable. In the case of Galton’s data, we can 
# interpret the data like this: due to inherited genes, 
# the son’s height prediction grows by β1 for each inch 
# we increase the father’s height x. Because not all 
# sons with fathers of height x are of equal height, 
# we need the term ε, which explains the remaining variability. 
# This remaining variability includes the mother’s genetic 
# effect, environmental factors, and other biological randomness.

# Given how we wrote the model above, the intercept β0 is not 
# very interpretable as it is the predicted height of a son 
# with a father with no height. Due to regression to the 
# mean, the prediction will usually be a bit larger 
# than 0. To make the slope parameter more interpretable,
# we can rewrite the model slightly as:
  
# Yi=β0+β1(xi−¯x)+εi,i=1,…,N with ¯x= the average of the x.
# In this case β0 represents the height when xi=¯x, which is
# the height of the son of an average father.

# For linear models to be useful, we have to estimate the unknown βs. 
# The standard approach in science is to find the values that 
# minimize the distance of the fitted model to the data. 
# The following is called the least squares (LS) equation 
# and we will see it often in this chapter. This quantity is called 
# the residual sum of squares (RSS). Once we find the values 
# that minimize the RSS, we will call the values the least 
# squares estimates (LSE) and denote them with β0 and β1. Let’s
# demonstrate this with the previously defined dataset:

library(HistData)
library(dslabs)
library(tidyverse)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# Let’s write a function that computes the RSS for
# any pair of values β0 and β1.

rss = function(beta0, beta1, data) {
  residual = galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(residual^2))
}

# So for any pair of values, we get an RSS. Here is a plot 
# of the RSS as a function of β1 when we keep the β0 fixed at 25.

beta1 = seq(0, 1, len = (nrow(galton_heights)))
rss_estimates = sapply(beta1, rss, beta0 = 36)
results = data.frame(beta1 = beta1, rss = rss_estimates)

results %>%
  ggplot(aes(beta1, rss)) + 
  geom_line()

# We can see a clear minimum for β1 at around 0.65. However,
# this minimum for β1 is for when β0=25, a value we 
# arbitrarily picked. We don’t know if (25, 0.65) is the 
# pair that minimizes the equation across all possible pairs.

# Trial and error is not going to work in this case. 
# We could search for a minimum within a fine grid of β0 
# and β1 values, but this is unnecessarily time-consuming 
# since we can use calculus: take the partial derivatives,
# set them to 0 and solve for β1 and β2. Of course, if
# we have many parameters, these equations can get rather
# complex. But there are functions in R that do these 
# calculations for us. We will learn these next. To 
# learn the mathematics behind this, you can consult a
# book on linear models.
