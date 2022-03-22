# Probability of casino losing money in roulette
# sampling model 1: define urn, then sample
urn = rep(c("Red", "Black", "Green"), c(18, 18, 2))
n = 1000
X = sample(ifelse(urn == "Red", 1, -1), n, replace = TRUE)
X[1:10]

# sampling model 2: define urn inside sample function by noting probabilities
x = sample(c(-1, 1), n, c(9/19, 10/19), replace = TRUE)
S = sum(x)
S

# Monte carlo simulation
B <- 10000
S = replicate(B, {
  X = sample(c(-1, 1), n, c(9/19, 10/19), replace = TRUE)
  sum(X)
}) 
mean(S < 0) # probability of the casino losing money after 1000 games

library(tidyverse)
# sequence of 100 values across range of S
s <- seq(min(S), max(S), length = 100)   
# generate normal density for S
normal_density = 
  data.frame(s = s, f = dnorm(s, mean = mean(S), sd = sd(S)))
data.frame(S = S) %>%
  ggplot(aes(S, stat(density))) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") + 
  geom_line(data = normal_density, mapping = aes(s, f), color = "blue")



