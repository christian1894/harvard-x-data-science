# Measurement error models
library(dslabs)
library(tidyverse)
library(broom)
library(ggplot2)

# Up to now, all our linear regression examples have been 
# applied to two or more random variables. We assume the
# pairs are bivariate normal and use this to motivate a 
# linear model. This approach covers most real-life 
# examples of linear regression. The other major 
# application comes from measurement errors models. 
# In these applications, it is common to have a 
# non-random covariate, such as time, and randomness 
# is introduced from measurement error rather than 
# sampling or natural variability.

# To understand these models, imagine you are Galileo 
# in the 16th century trying to describe the velocity
# of a falling object. An assistant climbs the Tower 
# of Pisa and drops a ball, while several other 
# assistants record the position at different times. 
# Let’s simulate some data using the equations we know
# today and adding some measurement error. The dslabs 
# function rfalling_object generates these simulations:

falling_object = rfalling_object()

# The assistants hand the data to Galileo and this is
# what he sees:

falling_object %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  xlab("Time in seconds") +
  ylab("Distance in meters")

# Galileo does not know the exact equation, but by looking 
# at the plot above, he deduces that the position should 
# follow a parabola, which we can write like this:
# f(x)=β0+β1x+β2x2

# The data does not fall exactly on a parabola. Galileo 
# knows this is due to measurement error. His helpers 
# make mistakes when measuring the distance. To account 
# for this, he models the data with:
# Yi=β0+β1xi+β2x2i+εi,i=1,…,n with Yi representing distance
# in meters, xi representing time in seconds, and ε 
# accounting for measurement error. The measurement 
# error is assumed to be random, independent from
# each other, and having the same distribution for 
# each i. We also assume that there is no bias, 
# which means the expected value E[ε]=0.

# Note that this is a linear model because it is a 
#linear combination of known quantities (x and x2 are
# known) and unknown parameters (the βs are unknown 
# parameters to Galileo). Unlike our previous examples, 
# here x is a fixed quantity; we are not conditioning.

# To pose a new physical theory and start making 
# predictions about other falling objects, Galileo
# needs actual numbers, rather than unknown parameters. 
# Using LSE seems like a reasonable approach. How do 
# we find the LSE?
  
# LSE calculations do not require the errors to be 
# approximately normal. The lm function will find the βs
# that will minimize the residual sum of squares:

fit = falling_object %>%
  mutate(time_sq = time ^ 2) %>%
  lm(observed_distance ~ time + time_sq, data = .) 
tidy(fit)  

# Let’s check if the estimated parabola fits the data.
# The broom function augment lets us do this easily:

augment(fit) %>%
  ggplot(aes(time, observed_distance)) +
  geom_point() +
  geom_line(aes(time, .fitted), col = "blue")

# Thanks to my high school physics teacher, I know that
# the equation for the trajectory of a falling object is:
# d=h0+v0t−0.5×9.8t2 with h0 and v0 the starting height 
# and velocity, respectively. The data we simulated 
# above followed this equation and added measurement 
# error to simulate n observations for dropping the 
# ball (v0=0) from the tower of Pisa (h0=55.86).

# These are consistent with the parameter estimates:
tidy(fit, conf.int = TRUE)

# The Tower of Pisa height is within the confidence 
# interval for β0, the initial velocity 0 is in the 
# confidence interval for β1 (note the p-value is
# larger than 0.05), and the acceleration constant
# is in a confidence interval for −2×β2.
