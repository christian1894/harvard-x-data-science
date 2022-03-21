library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
data(murders)

# Murders Scatter plot

murders %>% ggplot(aes(population, total, label = abb, color = region)) + 
  geom_label() +
  scale_x_log10() + 
  scale_y_log10() +
  ggtitle("Gun murder data")

# Heights histogram 

heights %>% ggplot(aes(x = height)) + 
  geom_histogram(binwidth = 1)

# Heights Density Plot

heights %>% ggplot(aes(x = height, fill = sex)) + 
  geom_density(alpha = 0.2)
