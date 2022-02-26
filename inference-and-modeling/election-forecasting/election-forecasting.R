# Election forecasting
library(tidyverse)
library(dslabs)
polls = polls_us_election_2016 %>%
  filter(state == "U.S." &
           enddate >= "2016-10-31" &
           (grade %in% c("B+", "A-", "A", "A+") |
              is.na(grade))) %>%
  mutate(spread = rawpoll_clinton / 100 - rawpoll_trump / 100)
one_poll_per_pollster = polls %>%
  group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()
results = one_poll_per_pollster %>% 
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - (qnorm(0.975) * se), end = avg + (qnorm(0.975) * se))
mu = 0
tau = 0.035
sigma = results$se
Y = results$avg
B = sigma^2 / (sigma^2 + tau^2)
posterior_mean = B * mu + (1 - B) * Y
posterior_se = sqrt(1 / ((1 / sigma^2) + (1 / tau^2)))
c(posterior_mean - (posterior_se * qnorm(0.975)), 
posterior_mean + (posterior_se * qnorm(0.975)))
1 - pnorm(0, posterior_mean, posterior_se)

# this gives Clinton a 99% of winning which is a bit overconfident
# probably because is not taking into account the general bias 
# that affects all presidential elections

# Let's start by assuming there is no general bias affecting 
# each poll and we will simulate data for 6 pollsters
# assuming an average of 0, SE = 2 * sqrt(p * (1 - p) / N)
# a sample size of 2000 and a expected value of 2.1
# Model: Xj = d + Ej (Ej being a random variable
# that explains poll to poll variability)

number_of_polls = 6
number_of_pollsters = 5
N = 2000
d  = 0.021
p = (d + 1) / 2
generate_polls = function (i) {
  d + rnorm(number_of_polls, 0, 2 * (sqrt(p * (1 - p) / N)))
}

X = sapply(1:number_of_pollsters, generate_polls)
simulated_data = 
  tibble(spread = as.vector(X), 
         pollster = rep(as.character(1:number_of_pollsters),
                        each=number_of_polls),
         type = "Simulated Data")
observed_data = polls %>% 
  group_by(pollster) %>%
  filter(n() >= 6) %>%
  ungroup() %>%
  select(pollster, spread) %>%
  mutate(pollster = as.character(pollster), type = "Observed Data") 
poll_data = rbind(simulated_data, observed_data)
poll_data %>% ggplot(aes(spread, pollster)) +
  geom_point() +
  geom_vline(xintercept = 0, col = "red") + 
  facet_wrap( ~ type, scales = "free_y", nrow = 2)

# this model does not seem to represent patterns found in
# the observed data because it does not account for 
# pollster to pollster variability(house effect), we will add a new term
# hi to represent this variability, the equation then will be:
# Xij = d + hi + Eij, we will assume sigma_h is 0.025
number_of_polls = 6
number_of_pollsters = 5
N = 2000
d  = 0.021
p = (d + 1) / 2
house_effects = rnorm(number_of_pollsters, 0, 0.025)
generate_polls = function (i) {
  d + house_effects[i] + rnorm(number_of_polls, 0, 2 * (sqrt((p * (1 - p)) / N)))
}
X = sapply(1:number_of_pollsters, generate_polls)
simulated_data = 
  tibble(spread = as.vector(X), 
         pollster = rep(as.character(1:number_of_pollsters),
                        each=number_of_polls),
         type = "Simulated Data")
observed_data = polls %>% 
  group_by(pollster) %>%
  filter(n() >= 6) %>%
  ungroup() %>%
  select(pollster, spread) %>%
  mutate(pollster = as.character(pollster), type = "Observed Data") 
poll_data = rbind(simulated_data, observed_data)
poll_data %>% ggplot(aes(spread, pollster)) +
  geom_point() +
  geom_vline(xintercept = 0, col = "red") + 
  facet_wrap( ~ type, scales = "free_y", nrow = 2)

# now the simulated data looks more like the observed data,
# we think that for every pollster in favor of our party, 
# there is another one in favor of the other party.
# Something that our model is missing is the general 
# bias that affects all elections historically, this value
# is know to be around 2-3%, adding this term to our model 
# looks like this:
# Xij= d + b + hi + Eij
# b stands for the general bias, this could be thought of as
# the difference between the election results and the average from 
# all pollsters.
# by using this new model in our Bayesian calculations we get
# a lot closer to Fivethirtyeight's prediction
mu = 0
tau = 0.035
general_bias = 0.025
sigma = sqrt((results$se) ^ 2 + (general_bias ^ 2))
Y = results$avg
B = sigma^2 / (sigma^2 + tau^2)
posterior_mean = B * mu + (1 - B) * Y
posterior_se = sqrt(1 / ((1 / sigma^2) + (1 / tau^2)))
c(posterior_mean - (posterior_se * qnorm(0.975)), 
  posterior_mean + (posterior_se * qnorm(0.975)))
1 - pnorm(0, posterior_mean, posterior_se)
