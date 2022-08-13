#  The broom package

library(dslabs)
library(tidyverse)
library(Lahman)
data("Teams")

dat = Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2) 

# Our original task was to provide an estimate and 
# confidence interval for the slope estimates of 
# each strata. The broom package will make this quite easy.

# The broom package has three main functions, all 
# of which extract information from the object 
# returned by lm and return it in a tidyverse 
# friendly data frame. These functions are tidy, 
# glance, and augment. The tidy function returns 
# estimates and related information as a data frame:

library(broom)
fit = lm(R ~ BB, data = dat)
tidy(fit)

# We can add other important summaries, such as 
# confidence intervals:

tidy(fit, conf.int = TRUE)

# Because the outcome is a data frame, we can
# immediately use it with summarize to string together 
# the commands that produce the table we are after. 
# Because a data frame is returned, we can filter and 
# select the rows and columns we want, which facilitates 
# working with ggplot2:

dat %>%  
  group_by(HR) %>%
  summarize(tidy(lm(R ~ BB), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

# Now we return to discussing our original task of determining
# if slopes changed. The plot we just made, using 
# summarize and tidy, shows that the confidence 
# intervals overlap, which provides a nice visual 
# confirmation that our assumption that the slope does
# not change is safe.

# The other functions provided by broom, glance, and augment,
# relate to model-specific and observation-specific
# outcomes, respectively. Here, we can see the model fit 
# summaries glance returns:

glance(fit)

# You can learn more about these summaries in any 
# regression text book.
# We will see an example of augment in the next section.
