# Linear regression in the tidyverse
library(dslabs)
library(tidyverse)
library(Lahman)
data("Teams")

# To see how we use the lm function in a more complex 
# analysis, let’s go back to the baseball example. 
# In a previous example, we estimated regression lines 
# to predict runs for BB in different HR strata. We 
# first constructed a data frame similar to this:

dat = Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2) 

# Since we didn’t know the lm function, to compute the
# regression line in each strata, we used the formula 
# directly like this:

get_slope = function(x, y) cor(x, y) * sd(y) / sd(x)
dat %>%  
  group_by(HR) %>%
  summarize(slope = get_slope(BB, R))

# We argued that the slopes are similar and that the
# differences were perhaps due to random variation. 
# To provide a more rigorous defense of the slopes being 
# the same, which led to our multivariable model, we could 
# compute confidence intervals for each slope. We have not
# learned the formula for this, but the lm function 
# provides enough information to construct them.

# First, note that if we try to use the lm function
# to get the estimated slope like this:

dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>% .$coef

# we don’t get the result we want. The lm function
# ignores the group_by. This is expected because lm
# is not part of the tidyverse and does not know how 
# to handle the outcome of a grouped tibble.

# The tidyverse functions know how to interpret grouped 
# Furthermore, to facilitate stringing commands through 
# the pipe %>%, tidyverse functions consistently return 
# data frames, since this assures that the output of a
# function is accepted as the input of another. But 
# most R functions do not recognize grouped tibbles nor
# do they return data frames. The lm function is an 
# example. However, we can write a function that uses
# lm to compute and return the wanted summaries in a 
# data frame and then use the summarize function:

get_slope <- function(x, y){
  fit <- lm(y ~ x)
  tibble(slope = fit$coefficients[2], 
         se = summary(fit)$coefficient[2,2])
}
dat %>%  
  group_by(HR) %>%
  summarize(get_slope(BB, R))

# Here is an example in which we return both 
# estimated parameters:

get_lse <- function(x, y){
  fit <- lm(y ~ x)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}

dat %>%  
  group_by(HR) %>%
  summarize(get_lse(BB, R))

# If you think this is all a bit too complicated, you are not alone. 
# To simplify things, we introduce the broom package which 
# was designed to facilitate the use of model fitting 
# functions, such as lm, with the tidyverse.
