# Bases on Balls or Stolen Bases?
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

# One of the challenges in this analysis is that it is not 
# obvious how to determine if a player produces runs because
# so much depends on his teammates. We do keep track of the 
# number of runs scored by a player. However, remember that
# if a player X bats right before someone who hits many HRs, 
# batter X will score many runs. But these runs don’t 
# necessarily happen if we hire player X but not his HR
# hitting teammate. However, we can examine team-level 
# statistics. How do teams with many SB compare to teams 
# with few? How about BB? We have data! Let’s examine some.

# Let’s start with an obvious one: HRs. Do teams that 
# hit more home runs score more runs? We examine data 
# from 1961 to 2001. The visualization of choice when exploring 
# the relationship between two variables, such as HRs and wins, 
# is a scatterplot:

# The visualization of choice when exploring the relationship 
# between two variables like home runs and runs is a scatterplot.

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# The plot shows a strong association: teams with more HRs
# tend to score more runs. Now let’s examine the relationship 
# between stolen bases and runs:

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Here the relationship is not as clear. Finally, let’s 
# examine the relationship between BB and runs:

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Here again we see a clear association. But does this 
# mean that increasing a team’s BBs causes an increase 
# in runs? One of the most important lessons you learn 
# in this book is that association is not causation.
# In fact, it looks like BBs and HRs are also associated:

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(HR_per_game = HR/G, BB_per_game = BB/G) %>%
  ggplot(aes(HR_per_game, BB_per_game)) + 
  geom_point(alpha = 0.5)

# We know that HRs cause runs because, as the name 
# “home run” implies, when a player hits a HR they are 
# guaranteed at least one run. Could it be that HRs also
# cause BB and this makes it appear as if BB cause runs?
# When this happens we say there is confounding, an 
# important concept we will learn more about throughout this chapter.

# Linear regression will help us parse all this out and
# quantify the associations. This will then help us 
# determine what players to recruit. Specifically, we 
# will try to predict things like how many more runs 
# will a team score if we increase the number of BBs, 
# but keep the HRs fixed? Regression will help us answer
# questions like this one.
