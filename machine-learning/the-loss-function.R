# The loss function

# Up to now we have described evaluation metrics that apply exclusively
# to categorical data. Specifically, for binary outcomes, we have
# described how sensitivity, specificity, accuracy, and F1 can be used
# as quantification. However, these metrics are not useful for
# continuous outcomes. In this section, we describe how 
# the general approach to defining “best” in machine learning is 
# to define a loss function, which can be applied to both 
# categorical and continuous data.

# The most commonly used loss function is the squared loss function. If ^y
# is our predictor and y is the observed outcome, the squared 
# loss function is simply: (^y−y)**2

# Because we often have a test set with many observations, say N,
# we use the mean squared error (MSE):
# MSE=(1 / N) * RSS=(1 / N) * N∑i=1(^yi−yi)**2

# In practice, we often report the root mean squared error (RMSE),
# which is √MSE, because it is in the same units as the outcomes. 
# But doing the math is often easier with the MSE and it is
# therefore more commonly used in textbooks, since these usually
# describe theoretical properties of algorithms.

# If the outcomes are binary, both RMSE and MSE are equivalent to 
# one minus accuracy, since (^y−y)**2 is 0 if the prediction was 
# correct and 1 otherwise. In general, our goal is to build an 
# algorithm that minimizes the loss so it is as close to 0 as possible.

# Because our data is usually a random sample, we can think of the MSE
# as a random variable and the observed MSE can be thought of
# as an estimate of the expected MSE, which in mathematical
# notation we write like this:

# E{(1 / N) N∑i=1(^Yi−Yi)**2}

# This is a theoretical concept because in practice we only have 
# one dataset to work with. But in theory, we think of having a very 
# large number of random samples (call it B), apply our algorithm to 
# each, obtain an MSE for each random sample, and think of the
# expected MSE as:

# (1 / B) B∑b=1(1/N)N∑i=1(^ybi−ybi)**2

# with ybi denoting the ith observation in the bth random sample and ^ybi 
# the resulting prediction obtained from applying the exact same algorithm
# to the bth random sample. Again, in practice we only observe one 
# random sample, so the expected MSE is only theoretical. However, in 
# Chapter 29 we describe an approach to estimating the MSE that tries 
# to mimic this theoretical quantity.

# Note that there are loss functions other than the squared loss. For
# example, the Mean Absolute Error uses absolute values, |^Yi−Yi|
# instead of squaring the errors (^Yi−Yi)2. However, in this 
# book we focus on minimizing square loss since it is the most widely used.
