# Computing the probability of X hat being within .01 of p
X_hat = 0.48
N = 25
SE = sqrt(X_hat * ((1 - X_hat) / N))
pnorm(0.01/SE) - pnorm(-0.01/SE)

# Monte carlo simulation
p = 0.45
N = 1000
B = 1000
x = sample(
  c(0, 1), 
  replace = TRUE, 
  size = N, 
  prob = c(1 - p, p))
X_hat = replicate(B, {
  x = sample(
    c(0, 1), 
    replace = TRUE, 
    size = N, 
    prob = c(1 - p, p))
  mean(x)
})

library(tidyverse)
library(gridExtra)
histogram = data.frame(X_hat = X_hat) %>%
  ggplot(aes(X_hat)) +
  geom_histogram(binwidth = 0.005, color = "black")
qq_plot = data.frame(X_hat = X_hat) %>%
  ggplot(aes(sample = X_hat)) +
  stat_qq(dparams = list(mean = mean(X_hat), sd = sd(X_hat))) +
  geom_abline() +
  ylab("X hat") +
  xlab("Theoretical Normal")
grid.arrange(histogram, qq_plot, nrow = 1)

# Plotting margin of error in an extremely large poll 
# over a range of values of p
N = 100000
p = seq(0.35, 0.65, length = 100)
get_se = function(x) {
  2 * sqrt(x * (1 - x) / N)
}
SE = sapply(p, get_se)
data.frame(p = p, SE = SE) %>%
  ggplot(aes(p, SE)) + 
  geom_line()
