# Predicting the electoral college
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
results = polls_us_election_2016 %>%
  filter(state != "U.S." &
           !str_detect(state, "CD") &
           enddate >="2016-10-31" & 
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state)) %>%
  arrange(abs(avg))
results = left_join(results, results_us_election_2016, by = "state")
# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)
# assign median sd to states without sd value
results = results %>%
  mutate(sd = ifelse(is.na(sd), median(sd, na.rm = TRUE), sd))

# we will start by establishing a prior with mu = 0 and 
# tau = 0.02 (results from each state do not chang that much every year)
# let's also make the incorrect assumption that state results are independent.
mu = 0
tau = 0.02
results = results %>% 
  mutate(sigma = sd / sqrt(n),
         B = (sigma ^ 2) / ((sigma ^ 2) + (tau ^ 2)),
         posterior_mean  = B * mu + ((1 - B) * avg),
         posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))
results %>% ggplot(aes(avg, posterior_mean, size = n)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)
# results based on posterior mean move estimates towards 0
# this is less noticeable in states with many polls since
# the more polls conducted, the more we trust those results
# let's do 10,000 monte carlo simulations
# and construct an estimate
B = 10000
mu = 0
tau = 0.02
clinton_ev_no_bias = replicate(B, {
  results %>% 
    mutate(sigma = sd / sqrt(n),
           B = (sigma ^ 2) / ((sigma ^ 2) + (tau ^ 2)),
           posterior_mean = B * mu + ((1 - B) * avg),
           posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
           result = rnorm(length(posterior_mean), 
                          posterior_mean,
                          posterior_se),
           clinton = ifelse(result > 0, electoral_votes, 0)) %>%
    summarize(clinton = sum(clinton)) %>%
    pull(clinton) + 7
})
mean(clinton_ev_no_bias > 269) 

# This prediction gives Clinton a 99% chance of winning, this is 
# quite off since the analysis is ignoring the general bias and assumes
# that state results are independent from one another.
# Let's improve the estimate by adding a general bias term sigma_bias = 0.03
# (most election results have a general bias of 1-2%), we will add this
# bias to sigma: sigma = sqrt(sd ^ 2 / n + bias ^ 2)
B = 10000
mu = 0
tau = 0.02
sigma_bias = 0.03
clinton_ev_bias = replicate(B, {
  results %>% 
    mutate(sigma = sqrt((sd ^ 2/ n) + sigma_bias ^ 2),
           B = (sigma ^ 2) / ((sigma ^ 2) + (tau ^ 2)),
           posterior_mean = B * mu + ((1 - B) * avg),
           posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
           result = rnorm(length(posterior_mean), 
                          posterior_mean,
                          posterior_se),
           clinton = ifelse(result > 0, electoral_votes, 0)) %>%
    summarize(clinton = sum(clinton)) %>%
    pull(clinton) + 7
})
mean(clinton_ev_bias > 269) 
# This model gives Clinton a more reasonable chance of winning and
# its closer to the FiveThirtyEight estimate of 71%(this estimate
# could be improved by using a t-distribution), a bar plot
# shows how this general bias variability affects results:
simulation_results = data.frame(bias = clinton_ev_bias, 
                                no_bias = clinton_ev_no_bias) %>%
  gather(approach, result)
simulation_results %>% ggplot(aes(result)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269, col = "red") +
  facet_grid(rows = vars(approach), scales = "free")
