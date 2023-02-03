# Conditional expectations

# For binary data, you can think of the probability Pr(Y=1∣X=x) as the 
# proportion of 1s in the stratum of the population for which X=x. Many of 
# the algorithms we will learn can be applied to both categorical and 
# continuous data due to the connection between conditional probabilities 
# and conditional expectations. Because the expectation is the average 
# of values y1,…,yn in the population, in the case in which the ys are 0 
# or 1, the expectation is equivalent to the probability of randomly
# picking a one since the average is simply the proportion of ones:
# E(Y∣X=x)=Pr(Y=1∣X=x). As a result, we often only use the expectation 
# to denote both the conditional probability and conditional expectation.
# Just like with categorical outcomes, in most applications the 
# same observed predictors do not guarantee the same continuous 
# outcomes. Instead, we assume that the outcome follows the same
# conditional distribution. We will now explain why we use the 
# conditional expectation to define our predictors.

# Conditional expectation minimizes squared loss function
# Why do we care about the conditional expectation in machine 
# learning? This is because the expected value has an attractive 
# mathematical property: it minimizes the MSE. Specifically, of 
# all possible predictions ^Y, ^Y=E(Y∣X=x) minimizes E{(^Y−Y)2∣X=x}
# Due to this property, a succinct description of the main task of 
# machine learning is that we use data to estimate: f(x)≡E(Y∣X=x)
# for any set of features x=(x1,…,xp). Of course this is easier
# said than done, since this function can take any shape and p can be
# very large. Consider a case in which we only have one predictor x. 
# The expectation E{Y∣X=x} can be any function of x: a line, a parabola,
# a sine wave, a step function, anything. It gets even more complicated
# when we consider instances with large p, in which case f(x) is a 
# function of a multidimensional vector x. For example, in our digit 
# reader example p=784! The main way in which competing machine learning
# algorithms differ is in their approach to estimating this expectation.
