# We previously calculated that the correlation coefficient between 
# fathers’ and sons’ heights is 0.5.
# Given this, what percent of the variation in sons’ heights is 
# explained by fathers’ heights?
r = 0.5
r ^ 2

# Suppose the correlation between father and son’s height is 0.5, 
# the standard deviation of fathers’ heights is 2 inches, and the 
# standard deviation of sons’ heights is 3 inches.
# Given a one inch increase in a father’s height, what is the 
# predicted change in the son’s height?
# m=ρ(σy/σx)
m = r * (3/2)
m

# In the second part of this assessment, you'll analyze a set of
# mother and daughter heights, also from GaltonFamilies.
# Define female_heights, a set of mother and daughter heights 
# sampled from GaltonFamilies, as follows:

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
library(dslabs)
library(tidyverse)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Calculate the mean and standard deviation of mothers' heights, 
# the mean and standard deviation of daughters' heights, and 
# the correlaton coefficient between mother and daughter heights.
mothers_mean = mean(female_heights$mother)
mothers_sd = sd(female_heights$mother)
daughters_mean = mean(female_heights$daughter)
daughters_sd = sd(female_heights$daughter)
r = cor(female_heights$mother, female_heights$daughter)

# Calculate the slope and intercept of the regression line 
# predicting daughters' heights given mothers' heights. Given 
# an increase in mother's height by 1 inch, how many inches is t
# the daughter's height expected to change?
# Slope of regression line predicting daughters' height from 
# mothers' heights:
m = r *(sd_daughter / sd_mother) 

# Intercept of regression line predicting daughters' height 
# from mothers' heights

b = daughters_mean - (m * mothers_mean)

# Change in daughter's height in inches given a 1 inch increase 
# in the mother's height:

m

# What percent of the variability in daughter heights is 
# explained by the mother's height?

r ^ 2 * 100

# A mother has a height of 60 inches.
# Using the regression formula, what is the conditional 
# expected value of her daughter's height given the mother's height?

Y = daughters_mean + r * ((60 - mothers_mean) / mothers_sd) * daughters_sd
m * 60 + b
