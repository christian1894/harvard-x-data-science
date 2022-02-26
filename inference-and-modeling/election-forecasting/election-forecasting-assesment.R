# Election forecasting assesment
# Load the libraries and data
library(dplyr)
library(dslabs)
library(ggplot2)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls = polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Confidence Intervals of Polling Data
# For each poll in the polling data set, use the CLT to create a 
# 95% confidence interval for the spread. Create a new table called 
# cis that contains columns for the lower and upper limits of the 
# confidence intervals.
cis = polls %>%
  mutate(X_hat = (spread + 1) / 2,
         se = 2 * sqrt(X_hat * (1 - X_hat) / samplesize),
         lower = spread - qnorm(0.975) * se,
         upper = spread + qnorm(0.975) * se,
         ) %>%
  select(state, startdate, enddate, pollster,
         grade, spread, lower, upper)

# Compare to Actual Results
# You can add the final result to the cis table you just created 
# using the left_join function as shown in the sample code.
# Now determine how often the 95% confidence interval includes 
# the actual result.
results = results_us_election_2016 %>% 
  mutate(actual_spread = clinton/100 - trump/100) %>% 
  select(state, actual_spread)
ci_data = cis %>% mutate(state = as.character(state)) %>% 
  left_join(results, by = "state")
p_hits = ci_data %>%
  mutate(hit = lower < actual_spread & actual_spread <= upper) %>%
  summarize(mean(hit))

# Stratify by Pollster and Grade
# Now find the proportion of hits for each pollster. 
# Show only pollsters with at least 5 polls and order 
# them from best to worst. Show the number of polls conducted
# by each pollster and the FiveThirtyEight grade of each pollster.
p_hits = ci_data %>%
  mutate(hit = lower < actual_spread & actual_spread <= upper) %>%
  group_by(pollster) %>% filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit),
            n = n(),
            grade = first(grade)) %>%
  arrange(desc(proportion_hits))

# Stratify by State
# Repeat the previous exercise, but instead of pollster, 
# stratify by state. Here we can't show grades.
p_hits = ci_data %>%
  mutate(hit = lower <= actual_spread & actual_spread <= upper) %>%
  group_by(state) %>% filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit),
            n = n()) %>%
  arrange(desc(proportion_hits))

# Plotting Prediction Results
# Make a barplot based on the result from the previous exercise.
p_hits %>% mutate(state = reorder(state, proportion_hits)) %>% 
  ggplot(aes(state, proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Predicting the Winner
# Even if a forecaster's confidence interval is incorrect, 
# the overall predictions will do better if they correctly 
# called the right winner.
# Add two columns to the cis table by computing, for each poll, 
# the difference between the predicted spread and the actual 
# spread, and define a column hit that is true if the signs
# are the same.
cis = cis %>%
  mutate(state = as.character(state)) %>% 
  left_join(results, by = "state")
errors = cis %>%
  mutate(error = spread - actual_spread,
         hit = sign(spread) == sign(actual_spread))
tail(errors)

# Plotting Prediction Results
# Create an object called p_hits that contains the proportion of 
# instances when the sign of the actual spread matches the 
# predicted spread for states with 5 or more polls.
# Make a barplot based on the result from the previous exercise 
# that shows the proportion of times the sign of the spread matched
# the actual result for the data in p_hits.
p_hits = errors %>%
  group_by(state) %>%
  summarize(proportion_hits = mean(hit),
            n = n())

p_hits %>%
  mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Plotting the Errors
# In the previous graph, we see that most states' polls predicted 
# the correct winner 100% of the time. Only a few states polls' 
# were incorrect more than 25% of the time. Wisconsin got every
# single poll wrong. In Pennsylvania and Michigan, more than 90% 
# of the polls had the signs wrong.
# Make a histogram of the errors. What is the median of these errors?
hist(errors$error)
median(errors$error)

# Plot Bias by State
# We see that, at the state level, the median error was slightly 
# in favor of Clinton. The distribution is not centered at 0, 
# but at 0.037. This value represents the general bias we 
# described in an earlier section.
# Create a boxplot to examine if the bias was general to all 
# states or if it affected some states differently. Filter 
# the data to include only pollsters with grades B+ or higher.
errors %>%
  filter(grade %in% c("B+", "A+", "A", "A-")) %>%
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(state, error)) +
  geom_boxplot() +
  geom_point() +
  coord_flip()
# From the plot we can tell that some state overestimated or 
# underestimated Clinton's performance; this proves that there 
# is regional bias aside from general bias, adding this would improve
# our model's estimate. This can be referred to as random effects/mixed models
