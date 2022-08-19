# Outliers
library(dslabs)
library(tidyverse)
library(ggplot2)

# Suppose we take measurements from two independent 
# outcomes, X and Y, and we standardize the measurements. 
# However, imagine we make a mistake and forget to 
# standardize entry 23. We can simulate such data using:
set.seed(1985)
x = rnorm(100,100,1)
y = rnorm(100,84,1)
x[-23] = scale(x[-23]) 
y[-23] = scale(y[-23])

# The data look like this:

as.data.frame(x, y) %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = 0.5)

# Not surprisingly, the correlation is very high:

cor(x, y)

# But this is driven by the one outlier. If we remove
# this outlier, the correlation is greatly reduced to 
# almost 0, which is what it should be:

cor(x[-23], y[-23])

# In Section 12 we described alternatives to the average 
# and standard deviation that are robust to outliers. 
# There is also an alternative to the sample correlation 
# for estimating the population correlation that is 
# robust to outliers. It is called Spearman correlation. 
# The idea is simple: compute the correlation on the ranks
# of the values. Here is a plot of the ranks plotted against 
# each other:

as.data.frame(x, y) %>%
  ggplot(aes(rank(x), rank(y))) +
  geom_point(alpha = 0.5)

# The outlier is no longer associated with a very large
# value and the correlation comes way down:

cor(rank(x), rank(y))

# Spearman correlation can also be calculated like this:

cor(x, y, method = "spearman")
  
# There are also methods for robust fitting of linear
# models which you can learn about in, for instance, 
# this book: Robust Statistics: Edition 2 by Peter J.
# Huber & Elvezio M. Ronchetti.
