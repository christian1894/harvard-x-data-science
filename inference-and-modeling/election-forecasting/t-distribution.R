# T-distribution
# When estimating using the CLT, we need to keep in mind that
# we are estimating sigma, and this is introducing variability
# to our model, for big sample sizes this variability is negligible
# but for sample sizes under 30 our confidence interval might be
# overconfident. If our data follows a normal distribution, there
# is mathematical theory that can tell us how much we need to adjust
# our confidence interval to account for the sigma's estimation.
# confidence intervals for d are based on this equation:
# Z = X_hat - d / sigma * sqrt(N)
# the CLT tells us that Z is normally distributed with expected value 0, 
# and standard deviation 1, since we do not know sigma, 
# we sustitute it with s:
# t = X_hat - d / s * sqrt(N)
# This is know as a t-statistic, by changing sigma to s, 
# we add some variability, theory tells us that t follows a 
# student t-distribution with N - 1 degrees of freedom, these degrees
# are parameters that control variability via fatter tails:

data_points = seq(-5, 5, len = 100)
data = data.frame(x = data_points, 
           normal = dnorm(data_points, 0, 1), 
           t_03 = dt(data_points,3), 
           t_05 = dt(data_points,5), 
           t_15=dt(data_points,15)) %>%
  gather(distribution, f, -x)
data %>% ggplot(aes(x, f, color = distribution)) +
  geom_line() +
  ylab("f(x)")

# Let's see if the pollster effect follows a normal distribution based
# on sample data:
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls = polls_us_election_2016 %>% 
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate == max(enddate)) %>%
  ungroup()
one_poll_per_pollster %>% ggplot(aes(sample = spread)) +
  stat_qq()

# This is very close to a normal distribution, so we can assume that
# y follows a t-distribution with N - 1 degrees of freedom.
# So, a better confidence interval for d would be:
degrees_of_freedom = nrow(one_poll_per_pollster) - 1
z = qt(0.975, degrees_of_freedom)
one_poll_per_pollster_with_t_disttribution = one_poll_per_pollster %>%
  summarize(avg = mean(spread),
            moe = z * sd(spread) / sqrt(length(spread)),
            start = avg - moe,
            end = avg + moe)
# the t-distribution quantile is a bit larger than the normal 
# distribution quantile(fatter tails explain this).
qt(0.975, 14)
qnorm(0.975)

# The t-distribution can also be used to model errors in bigger deviations
# more precisely than the normal distribution. FiveThirtyEight uses this
# to generate errors that better model the deviations we see in election data.
# For example, in Wisconsin the average of six polls was 7% in favor of 
# Clinton with a sd of 1%, however Trump won by 0.7%. Even after taking
# into account the general bias, the 7.7% residual is more in line with 
# t-distributed data than with the normal distribution.
wisconsin_results = polls_us_election_2016 %>%
  filter(state == "Wisconsin" & 
           enddate >="2016-10-31" & 
           (grade %in% c("A+","A","A-","B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  mutate(state = as.character(state)) %>%
  left_join(results_us_election_2016, by = "state") %>%
  mutate(actual = clinton / 100 - trump / 100) %>%
  summarize(actual = first(actual), 
            avg = mean(spread),
            sd = sd(spread),
            n = n()) %>%
  select(actual, avg, sd, n)

