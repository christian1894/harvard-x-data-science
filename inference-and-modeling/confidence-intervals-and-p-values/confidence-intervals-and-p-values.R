# ggplot geom_smooth example
library(tidyverse)
data("nhtemp")
data.frame(year = as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp)) %>%
  ggplot(aes(year, temperature)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Average Yearly Temperatures in New Haven")

# Monte carlo simulation of confidence intervals
p = 0.45
N = 1000
X = sample(
  c(0, 1), 
  size = N, 
  replace = TRUE,
  prob = c(1 - p, p)
)
X_hat = mean(X)
SE_hat = sqrt(X_hat*(1-X_hat)/N)

# generate interval 2 SEs apart from the average
c(X_hat - 2 *SE_hat, X_hat + 2 * SE_hat)

# solving for z with qnorm
# demonstrating that qnorm gives z value for given p
z = qnorm(0.995)
pnorm(qnorm(0.995))

# checking 1 - qnorm symmetry
pnorm(qnorm(1 - 0.995))

# demonstrating that this z value gives correct probability
# for given interval
pnorm(z) - pnorm(-z)

# Monte carlo simulation
# Note that to compute the exact 95% confidence interval,
# we would use qnorm(.975)*SE_hat instead of 2*SE_hat.
B = 10000
N = 1000
inside = replicate(B, {
  X = sample(
    c(0, 1),
    replace = TRUE, 
    size = N, 
    prob = c(1 - p, p)
  )
  X_hat = mean(X)
  SE_hat = sqrt(X_hat*(1-X_hat)/N) 
  between(p, 
          X_hat - (qnorm(0.975) * SE_hat), 
          X_hat + (qnorm(0.975) * SE_hat))
})
mean(inside)

#  Confidence interval for the spread with sample size of 25
N = 25
X_hat = 0.48
SE_hat = sqrt((X_hat*(1-X_hat))/N) 
(2*X_hat - 1) + c(-2, 2)*2*SE_hat

# Computing a p-value for observed spread of 0.02
N = 100
z = sqrt(N) * 0.02/0.5    # spread of 0.02
1 - (pnorm(z) - pnorm(-z))
