library(tidyverse)
library(dslabs)
# Poll aggregators
# simulating polls
d = 0.039
Ns = c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p = (d + 1) / 2

# calculate confidence intervals of the spread
confidence_intervals = sapply(Ns, function(N) {
  X = sample(c(0, 1),
             replace = TRUE,
             size = N, 
             prob = c(1 - p, p))
  X_hat = mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  c(X_hat, X_hat - (2 * SE_hat), X_hat + (2 * SE_hat))
})

# generate a data frame storing results
polls = data.frame(poll = 1:ncol(confidence_intervals),
                   t(confidence_intervals), 
                   sample_size = Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")

# plot
polls %>% ggplot(aes(poll, estimate, ymin = low, ymax = high)) +
  geom_hline(yintercept = 0.5) + 
  geom_point(color = "#00AFBB") +
  geom_errorbar(color = "#00AFBB") +
  coord_flip() +
  geom_hline(yintercept = p, lty = 2)

# Calculating the spread of combined polls
d_hat = polls %>%
  summarize(avg = sum(estimate * sample_size) / sum(sample_size)) %>%
  .$avg
p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
round(d_hat*100,1)
round(moe*100, 1)
SE_hat <- sqrt(d_hat*(1-d_hat)/sum(polls$sample_size))
c(d_hat, d_hat - (2 * SE_hat), d_hat + (2 * SE_hat))

# Plot including aggregated poll
new_row = tibble(13, d_hat, d_hat-moe, d_hat+moe, sum(polls$sample_size))
names(new_row) = names(polls)
polls_with_avg = bind_rows(polls, new_row)
polls_with_avg$poll = as.character(polls_with_avg$poll)
polls_with_avg$poll[13] = "Avg"
polls_with_avg$col = c(rep(2, 12), 1)
polls_with_avg %>% 
  ggplot(aes(poll, estimate, ymin = low, ymax = high)) +
  geom_point(show.legend = FALSE, color = "#00AFBB") +
  geom_errorbar(show.legend = FALSE, color = "#00AFBB") +
  coord_flip() +
  geom_hline(yintercept = 0.5) + 
  geom_hline(yintercept = p, lty = 2)

# Pollster bias
data(polls_us_election_2016)

# keep only national polls from week before election with a grade considered reliable
polls = polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls = polls %>%
  mutate(spread = abs((rawpoll_clinton - rawpoll_trump) / 100))

# compute estimated spread for combined polls
d_hat = polls %>%
  summarize(d_hat = sum(spread * samplesize) / sum(samplesize)) %>%
  .$d_hat
p_hat = (d_hat + 1) / 2
moe = qnorm(.975) * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
polls %>%
  ggplot(aes(spread)) +
  geom_histogram(color="black", binwidth = .01)

# investigating pollster bias
# plot pollsters with at least 6 polls
polls %>% group_by(pollster) %>% 
  filter(n() >= 6) %>%
  ggplot(aes(pollster, spread)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# standard error within each pollster
polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>% 
  summarize(se = 2 * sqrt(p_hat * (1-p_hat) / median(samplesize)))

# Data driven model
last_poll_per_pollster = polls %>% 
  group_by(pollster) %>%
  filter(enddate == max(enddate)) %>% 
  ungroup()

last_poll_per_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01)

sd(last_poll_per_pollster$spread)

results = last_poll_per_pollster %>%
  summarize(
    avg = mean(spread), 
    se = sd(spread) / sqrt(length(spread))) %>%
  mutate(lower = avg - se * qnorm(0.975), upper = avg + se * qnorm(0.975)) 
round(results*100, 1)
