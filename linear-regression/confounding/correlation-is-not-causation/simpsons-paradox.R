# Simpson’s paradox

library(dslabs)
library(tidyverse)
library(ggplot2)

# The case we have just covered is an example of Simpson’s 
# paradox. It is called a paradox because we see the sign 
# of the correlation flip when comparing the entire 
# publication and specific strata. As an illustrative 
# example, suppose you have three random variables 
# X, Y, and Z and that we observe realizations of these.
# Here is a plot of simulated observations for X and Y 
# along with the sample correlation:

N = 100
Sigma = matrix(c(1,0.75,0.75, 1), 2, 2)*1.5
means = list(c(x = 11, y = 3), 
              c(x = 9, y = 5), 
              c(x = 7, y = 7), 
              c(x = 5, y = 9), 
              c(x = 3, y = 11))
dat = lapply(means, function(mu){
  res = MASS::mvrnorm(N, mu, Sigma)
  colnames(res) = c("x", "y")
  res
})
dat = do.call(rbind, dat) %>% 
  as_tibble() %>%
  mutate(z = as.character(rep(seq_along(means), each = N)))
dat %>% ggplot(aes(x, y)) + geom_point(alpha = .5) +
  ggtitle(paste("Correlation = ", round(cor(dat$x, dat$y), 2)))

# You can see that X and Y are negatively correlated. 
# However, once we stratify by Z (shown in different colors 
# below) another pattern emerges:

means = do.call(rbind, means) %>% 
  as_tibble() %>%
  mutate(z = as.character(seq_along(means)))

corrs = dat %>% group_by(z) %>% summarize(cor = cor(x, y)) %>% pull(cor)
dat %>% ggplot(aes(x, y, color = z)) + 
  geom_point(show.legend = FALSE, alpha = 0.5) +
  ggtitle(paste("Correlations =",  paste(signif(corrs,2), collapse=" "))) +
  annotate("text", x = means$x, y = means$y, label = paste("z =", means$z), cex = 5)  


# It is really Z that is negatively correlated with X. 
# If we stratify by Z, the X and Y are actually positively 
# correlated as seen in the plot above.
