# Bivariate normal distribution

# Correlation and the regression slope are a widely used summary 
# statistic, but they are often misused or misinterpreted. 
# Anscombe’s examples provide over-simplified cases of dataset
# in which summarizing with correlation would be a mistake. 
# But there are many more real-life examples.

# The main way we motivate the use of correlation involves what 
# is called the bivariate normal distribution.

# When a pair of random variables is approximated by the bivariate 
# normal distribution, scatterplots look like ovals. As we saw 
# in Section 18.2, they can be thin (high correlation) or 
# circle-shaped (no correlation).

# A more technical way to define the bivariate normal distribution 
# is the following: if X is a normally distributed random variable,
# Y is also a normally distributed random variable, and the 
# conditional distribution of Y for any X=x is approximately normal,
# then the pair is approximately bivariate normal. When three 
# or more variables have the property that each pair is 
# bivariate normal, we say the variables follow a multivariate
# normal distribution or that they are jointly normal.

# If we think the height data is well approximated by the 
# bivariate normal distribution, then we should see the normal
# approximation hold for each strata. Here we stratify the 
# son heights by the standardized father heights and see 
# that the assumption appears to hold:

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

galton_heights %>%
  mutate(z_father = round(scale(father))) %>%
  select(z_father, son) %>%
  filter(z_father %in% -2:2) %>%
  ggplot(aes(sample = son)) +
  geom_qq() +
  facet_wrap(~z_father)
