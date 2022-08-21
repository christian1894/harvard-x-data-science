# Warning: there are two regression lines

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

# We computed a regression line to predict the son’s height 
# from father’s height. We used these calculations:

mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

# which gives us the function E(Y∣X=x)= 37.9 + 0.45 x.
# What if we want to predict the father’s height based on the son’s? 
# It is important to know that this is not determined by computing
# the inverse function: x={E(Y∣X=x)−37.3 }/0.5.
# We need to compute E(X∣Y=y). Since the data is approximately
# bivariate normal, the theory described above tells us that 
# this conditional expectation will follow a line with slope and
# intercept:
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2 * mu_y

# So we get E(X∣Y=y)= 38.3 + 0.44y. Again we see regression to the
# average: the prediction for the father is closer to the father 
# average than the son heights y is to the son average.

# Here is a plot showing the two regression lines, with blue for the
# predicting son heights with father heights and red for predicting 
# father heights with son heights:

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = m_1, intercept = b_1, col = 'blue') +
  geom_abline(slope = 1 / m_2, intercept = -b_2 / m_2, col = 'red')
