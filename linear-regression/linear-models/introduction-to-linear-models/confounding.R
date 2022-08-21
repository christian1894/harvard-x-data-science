# Confounding

# Previously, we noted a strong relationship between Runs and BB.
# If we find the regression line for predicting runs from bases 
# on balls, we a get slope of:

library(tidyverse)
library(Lahman)
get_slope = function(x, y) { 
  cor(x, y) * (sd(y) / sd(x))
}

bb_slope = Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  summarize(get_slope(BB_per_game, R_per_game))
bb_slope

# So does this mean that if we go and hire low salary players 
# with many BB, and who therefore increase the number of walks
# per game by 2, our team will score 1.5 more runs per game?
# We are again reminded that association is not causation. The 
# data does provide strong evidence that a team with two more
# BB per game than the average team, scores 1.5 runs per game.
# But this does not mean that BB are the cause.

# Note that if we compute the regression line 
# slope for singles we get:

singles_slope = Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles_per_game = (H - HR - X2B - X3B)/ G, R_per_game = R / G) %>%
  summarize(get_slope(Singles_per_game, R_per_game))
singles_slope

# which is a lower value than what we obtain for BB.

# Also, notice that a single gets you to first base just 
# like a BB. Those that know about baseball will tell you
# that with a single, runners on base have a better chance 
# of scoring than with a BB. So how can BB be more 
# predictive of runs? The reason this happen is 
# because of confounding. Here we show the correlation 
# between HR, BB, and singles:

Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))

# It turns out that pitchers, afraid of HRs, will sometimes 
# avoid throwing strikes to HR hitters. As a result, HR 
# hitters tend to have more BBs and a team with many HRs 
# will also have more BBs. Although it may appear that
# BBs cause runs, it is actually the HRs that cause 
# most of these runs. We say that BBs are confounded 
# with HRs. Nonetheless, could it be that BBs still 
# help? To find out, we somehow have to adjust 
# for the HR effect. Regression can help with this as well.
