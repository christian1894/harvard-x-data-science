# Correlation is not always a useful summary
# Correlation is not always a good summary of the 
# relationship between two variables. The following
# four artificial datasets, referred to as Anscombe’s
# quartet, famously illustrate this point. All 
# these pairs have a correlation of 0.82:

library(dslabs)
library(tidyverse)
anscombe %>%
  mutate(row = seq_len(n())) %>%
  gather(name, value, -row) %>%
  separate(name, c("axis", "group"), sep=1) %>%
  spread(axis, value) %>% select(-row) %>%
  ggplot(aes(x, y)) +
  facet_wrap(~group) +
  geom_point(alpha = 0.5) +
  geom_smooth(method ="lm", fill = NA, fullrange = TRUE)

# Correlation is only meaningful in a particular context. 
# To help us understand when it is that correlation is 
# meaningful as a summary statistic, we will return to 
# the example of predicting a son’s height using his father’s 
# height. This will help motivate and define linear regression.
# We start by demonstrating how correlation can be useful for 
# prediction.
