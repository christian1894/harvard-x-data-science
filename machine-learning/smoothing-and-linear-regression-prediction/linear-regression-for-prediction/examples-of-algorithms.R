# Linear Regression

# Linear regression can be considered a machine learning algorithm. 
# In Section 27.8 we demonstrated how linear regression can be too 
# rigid to be useful. This is generally true, but for some challenges 
# it works rather well. It also serves as a baseline approach: if you 
# can’t beat it with a more complex approach, you probably want to stick
# to linear regression. To quickly make the connection between regression 
# and machine learning, we will reformulate Galton’s study with heights, a 
# continuous outcome.

library(HistData)

set.seed(1983)
galton_heights = GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# Suppose you are tasked with building a machine learning algorithm 
# that predicts the son’s height Y using the father’s height X. Let’s 
# generate testing and training sets:

y = galton_heights$son
test_index = createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set = galton_heights %>% slice(-test_index)
test_set = galton_heights %>% slice(test_index)

# In this case, if we were just ignoring the father’s height and
# guessing the son’s height, we would guess the average height of sons.

m = mean(train_set$son)

# Our root mean squared error is:

sqrt(mean((m - test_set$son)^2))


# Can we do better? In the regression chapter, we learned that if the
# pair (X,Y) follow a bivariate normal distribution, the conditional 
# expectation (what we want to estimate) is equivalent to the regression line:
# f(x)=E(Y∣X=x)=β0+β1x
# In Section 18.3 we introduced least squares as a method for estimating 
# the slope β0 and intercept β1:

fit = lm(son ~ father, data = train_set)
fit$coefficients

# This gives us an estimate of the conditional expectation:

# ^f(x)=35+0.5x

# We can see that this does indeed provide an improvement over our 
# guessing approach.

y_hat = fit$coef[1] + fit$coef[2]*test_set$father
sqrt(mean((y_hat - test_set$son)^2))

# The predict function
# The predict function is very useful for machine learning applications. 
# This function takes a fitted object from functions such as lm or glm 
# (we learn about glm soon) and a data frame with the new predictors for
# which to predict. So in our current example, we would use predict like this:

y_hat = predict(fit, test_set)

# Using predict, we can get the same results as we did previously:
sqrt(mean((y_hat - test_set$son) ^2))

# predict does not always return objects of the same types; it 
# depends on what type of object is sent to it. To learn about the 
# specifics, you need to look at the help file specific for the type
# of fit object that is being used. The predict is actually a special 
# type of function in R (called a generic function) that calls other 
# functions depending on what kind of object it receives. So if predict
# receives an object coming out of the lm function, it will call predict.lm.
# If it receives an object coming out of glm, it calls predict.glm.
# These two functions are similar but different. You can learn more
# about the differences by reading the help files:

?predict.lm
?predict.glm
