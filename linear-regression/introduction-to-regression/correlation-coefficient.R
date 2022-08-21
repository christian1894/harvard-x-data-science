# The correlation coefficient
# The correlation coefficient is defined for a list of 
# pairs (x1,y1),…,(xn,yn) as the average of the product 
# of the standardized values. The Greek letter ρ (rho) 
# is commonly used in statistics books to denote the correlation. 
# The Greek letter for r, ρ, because it is the first letter of 
# regression. Soon we learn about the connection between 
# correlation and regression. We can represent the correlation 
# coefficient formula with R code using:
n = 100
x = sample(0:25, n, replace = TRUE)   
y = sample(0:25, n, replace = TRUE)   
rho = mean(scale(x)*scale(y))

# The correlation between father and son’s heights is about 0.5:
library(dslabs)
library(tidyverse)
library(HistData)
data("GaltonFamilies")
galton_heights = GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  ungroup() %>%
  summarize(father, son = childHeight)
galton_heights %>%
  summarize(rho = cor(father, son)) %>%
  pull(rho)

# To see what data looks like for different values of ρ, 
# here are six examples of pairs with correlations ranging 
# from -0.9 to 0.99

library(MASS)
n = 250
correlations = c(-0.99, -0.9, -0.5, 0, 0.5, 0.9, 0.99)
data_matrix = function(value) {
  return(matrix(c(1,value,value,1),2,2))
}
generate_simulated_data = function(correlation) {
  return(mvrnorm(n, c(0, 0), data_matrix(correlation)))
}
  
simulated_data = lapply(correlations, generate_simulated_data)
simulated_data = Reduce(rbind, simulated_data)
simulated_data = cbind(rep(correlations, each=n), simulated_data)
colnames(simulated_data) = c("r","x","y")
as.data.frame(simulated_data) %>% 
  ggplot(aes(x, y)) +
  facet_wrap(vars(r)) +
  geom_point(alpha = 0.5) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2)







