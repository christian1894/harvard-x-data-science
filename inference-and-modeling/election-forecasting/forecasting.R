# Forecasting
# We will analyze how informative are polls taken weeks before
# the election, let's stick to one pollter:
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
single_pollster_data = polls_us_election_2016 %>%
  filter(pollster == "Ipsos", 
         state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# this distribution does not look normal
single_pollster_data %>% 
  ggplot(aes(spread)) + 
  geom_histogram(binwidth = 0.01, color = "black")

# empirical standard error is a bit higher that the theoretical value
se <- single_pollster_data %>% 
  summarize(empirical = sd(spread), 
            theoretical = 2 * sqrt(mean(spread) * (1 - mean(spread)) /
                                     min(samplesize)))

# let's plot the spread against end_date and 
# see if p changes accross time
single_pollster_data %>% ggplot(aes(enddate, spread)) +
  geom_point() +
  geom_smooth(method = 'loess', span = 0.1)

# the plot shows that p is not fixed accross time,
# some of the peaks and valleys may come from candidates's
# conventions and rallies. Let's see if this applies to 
# all pollsters
all_pollsters = 
  polls_us_election_2016 %>% 
  filter(state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
all_pollsters %>% ggplot(aes(enddate, spread)) + 
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), alpha = 0.6, show.legend = FALSE) 

# All pollsters seem to be affected by the time, This implies
# that we should add a new term to our model to account 
# for the time effect. we could also add a function that predicts
# trends in a candidate's popularity, our model would be:
# Y_i_j_t = d + b + h_j + b_t + f(t) + εi,jt 
# where d stands for the spread expected value, b for general bias, 
# h pollster to pollster variability(indexed by pollster) 
# b_t for time effect(indexed by time/date), f() for a trend prediction 
# function and E for the spread's standard error(indexed by pollster,
# poll and time/date)

# We usually see the trend function not for the difference between
# each candidate's spread but separate, like this:
trend_data = polls_us_election_2016 %>%
  filter(state == "U.S." & enddate>="2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(clinton = rawpoll_clinton, trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("trump", "clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup()
trend_data %>% ggplot(aes(enddate, percentage, color = candidate)) +
  geom_smooth(method = 'loess', span =  0.15) +
  geom_point(alpha = 0.4, show.legend = FALSE) +
  scale_y_continuous(limits = c(30, 50))

# With the above model we can build solid prediction by estimating
# the spread using current and historical data, to estimate trends
# though we may need machine learning
