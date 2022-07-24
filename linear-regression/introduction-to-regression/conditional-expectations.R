# Conditional expectations
library(HistData)
library(tidyverse)
library(dslabs)
data("GaltonFamilies")
galton_heights = GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  mutate(son = childHeight) %>%
  select(father, son)

# Suppose we are asked to guess the height of a randomly selected
# son and we don’t know his father’s height. Because the distribution
# of sons’ heights is approximately normal, we know the average 
# height, 69.2, is the value with the highest proportion and 
# would be the prediction with the highest chance of minimizing 
# the error. But what if we are told that the father is taller 
# than average, say 72 inches tall, do we still guess 69.2 for the son?

# It turns out that if we were able to collect data from a very
# large number of fathers that are 72 inches, the distribution 
# of their sons’ heights would be normally distributed. This 
# implies that the average of the distribution computed on this
# subset would be our best prediction.

# In general, we call this approach conditioning. The general 
# idea is that we stratify a population into groups and compute 
# summaries in each group. Conditioning is therefore related 
# to the concept of stratification described in Section 9.13. 
# To provide a mathematical description of conditioning, consider 
# we have a population of pairs of values (x1,y1),…,(xn,yn), for 
# example all father and son heights in England. In the previous 
# chapter we learned that if you take a random pair (X,Y), the 
# expected value and best predictor of Y is E(Y)=μy, the 
# population average 1/n ∑ni=1yi. However, we are no longer 
# interested in the general population, instead we are 
# interested in only the subset of a population with a 
# specific xi value, 72 inches in our example. This subset
# of the population, is also a population and thus the same
# principles and properties we have learned apply. The yi
# in the subpopulation have a distribution, referred to as 
# the conditional distribution, and this distribution has an 
# expected value referred to as the conditional expectation. 
# In our example, the conditional expectation is the average 
# height of all sons in England with fathers that are 72 inches.
# The statistical notation for the conditional expectation is E(Y∣X=x)
# with x representing the fixed value that defines that subset, 
# for example 72 inches. Similarly, we denote the standard 
# deviation of the strata with SD(Y∣X=x)=√Var(Y∣X=x)
# Because the conditional expectation E(Y∣X=x) is the best 
# predictor for the random variable Y for an individual 
# in the strata defined by X=x, many data science challenges 
# reduce to estimating this quantity. The conditional standard
# deviation quantifies the precision of the prediction.

# In the example we have been considering, we are interested in
# computing the average son height conditioned on the father 
# being 72 inches tall. We want to estimate E(Y|X=72) using the 
# sample collected by Galton. We previously learned that the 
# sample average is the preferred approach to estimating the 
# population average. However, a challenge when using this 
# approach to estimating conditional expectations is that for
# continuous data we don’t have many data points matching exactly
# one value in our sample. For example, we have only 8 father
# with a height of 72 inches:

sum(galton_heights$father == 72)

# A practical way to improve these estimates of the conditional
# expectations, is to define strata of with similar values of x.
# In our example, we can round father heights to the nearest 
# inch and assume that they are all 72 inches. If we do this,
# we end up with the following prediction for the son of a father
# that is 72 inches tall:

conditional_average = galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(son = mean(son)) %>%
  pull(son)

# Note that a 72-inch father is taller than average – specifically,
# (72.0 - 69.1)/2.5 = 1.1 standard deviations taller than the 
# average father. Our prediction 70.5 is also taller than average,
# but only 0.49 standard deviations larger than the average son. 
# The sons of 72-inch fathers have regressed some to the average 
# height. We notice that the reduction in how many SDs taller is 
# about 0.5, which happens to be the correlation. As we will see 
# in a later section, this is not a coincidence.

# If we want to make a prediction of any height, not just 72, 
# we could apply the same approach to each strata. Stratification
# followed by boxplots lets us see the distribution of each group:

galton_heights %>%
  mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point(alpha = 0.5)

# Not surprisingly, the centers of the groups are increasing 
# with height. Furthermore, these centers appear to follow a 
# linear relationship. Below we plot the averages of each group.
# If we take into account that these averages are random variables
# with standard errors, the data is consistent with these points 
# following a straight line:



