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
