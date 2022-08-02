# The lm function

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

# In R, we can obtain the least squares estimates using the
# lm function. To fit the model:
# Yi=β0+β1xi+εi with Yi the son’s height and xi the father’s
# height, we can use this code to obtain the least squares estimates.

fit = lm(son ~ father, data = galton_heights)
fit$coef

# The most common way we use lm is by using the character ~ 
# to let lm know which is the variable we are predicting
# (left of ~) and which we are using to predict (right of ~). 
# The intercept is added automatically to the model that will be fit.

# The object fit includes more information about the fit. We 
# can use the function summary to extract more of this
# information (not shown):

summary(fit)

# To understand some of the information included in this 
# summary we need to remember that the LSE are random variables. 
# Mathematical statistics gives us some ideas of the distribution
# of these random variables

