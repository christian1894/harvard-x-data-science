library(dplyr)
library(ggplot2)
library(dslabs)

# Customizing plots - watch and learn
dat <- us_contagious_diseases %>%
  filter(year == 1967
         & disease=="Measles"
         & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting)
# must be ordered by rate to aid visualization
state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)
state = reorder(state, rate)

# Customizing plots - redefining order
dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate(state = reorder(state, rate))

dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()

# Showing the data and customizing plots
data("murders")
# this plot does not include state data, we are not aware
# of variability within a region, showing the distribution
# or data will be a better summary
murders %>% mutate(rate = total/population*100000) %>%
  group_by(region) %>%
  summarize(avg = mean(rate)) %>%
  mutate(region = factor(region)) %>%
  ggplot(aes(region, avg)) +
  geom_bar(stat="identity") +
  ylab("Murder Rate Average")

# Making a box plot(showing distribution + showing data points)
murders %>% mutate(rate = total/population*100000) %>% 
  mutate(region = reorder(region, rate)) %>%
  ggplot(aes(region, rate)) + geom_boxplot() +
  geom_point(position = "jitter", alpha = 0.7) 


