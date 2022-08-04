# LSEs are random variables

library(HistData)
library(dslabs)
library(tidyverse)
library(ggplot2)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# The LSE is derived from the data y1,…,yN, which are a 
# realization of random variables Y1,…,YN. This implies 
# that our estimates are random variables. To see this,
# we can run a Monte Carlo simulation in which we assume
# the son and father height data defines a population, 
# take a random sample of size N=50, and compute the 
# regression slope coefficient for each one:

B = 1000
N = 50
LSE = replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son ~ father, data = .) %>%
    .$coef
})
beta0_dataframe = data.frame(value = LSE[1,], coefficient = "beta0")
beta1_dataframe = data.frame(value = LSE[2,], coefficient = "beta1")
LSE_dataframe = rbind(beta0_dataframe, beta1_dataframe)

# We can see the variability of the estimates by 
# plotting their distributions:

LSE_dataframe %>% 
  ggplot() +
  geom_histogram(aes(value), col = 'black') +
  facet_grid(cols = vars(coefficient), scales="free")

# The reason these look normal is because the central 
# limit theorem applies here as well: for large enough N, 
# the least squares estimates will be approximately normal 
# with expected value β0 and β1, respectively. The standard 
# errors are a bit complicated to compute, but mathematical 
# theory does allow us to compute them and they are included 
# in the summary provided by the lm function. Here it is 
# for one of our simulated data sets:

sample_n(galton_heights, size = 50, replace = TRUE) %>%
  lm(son ~ father, data = .) %>%
  summary %>% .$coef

# You can see that the standard errors estimates reported by 
# the summary are close to the standard errors from the simulation:

standard_errors = LSE_dataframe %>% 
  group_by(coefficient) %>%
  mutate(se = sd(value)) %>%
  distinct(se) 

# The summary function also reports t-statistics (t value) 
# and p-values (Pr(>|t|)). The t-statistic is not actually
# based on the central limit theorem but rather on the assumption 
# that the εs follow a normal distribution. Under this
# assumption, mathematical theory tells us that the LSE 
# divided by their standard error, β0_hat/SE_hat(β0_hat)
# and β1_hat/SE_hat(β1_hat), follow a t-distribution with
# N−p degrees of freedom, with p the number of parameters 
# in our model. In the case of height p=2, the two p-values 
# are testing the null hypothesis that β0=0 and β1=0,respectively.

# Remember that, as we described in Section 17.10 for large 
# enough N, the CLT works and the t-distribution becomes almost
# the same as the normal distribution. Also, notice that 
# we can construct confidence intervals, but we will soon 
# learn about broom, an add-on package that makes this easy.

# Although we do not show examples in this book, hypothesis
# testing with regression models is commonly used in epidemiology 
# and economics to make statements such as “the effect of A
# on B was statistically significant after adjusting for
# X, Y, and Z”. However, several assumptions have to hold for 
# these statements to be true.
