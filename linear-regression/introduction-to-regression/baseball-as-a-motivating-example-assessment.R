# Baseball as a motivating example
# The application of statistics and data science to baseball is known
# as "sabermetrics” it was coined by Bill James, 
# and is derived from the acronym SABR: the society 
# for American baseball research.
# One of the goals of this system is to build a batting average
# to measure how good a player is, this average inspects on a player's
# plate appearances, home-runs, stolen bases, singles, doubles,
# triples,outs and bases on balls to produce a performance average.
# Some important details about Sabermetrics are that we have to take
# into account team statistics as well as player statistics since some
# of a player success could be justified by the success of its team and
# also a player's number of bases on balls will not improve his batting 
# average as we will see shortly.

# You want to know whether teams with more at-bats
# per game have more runs per game. Make a plot.
library(Lahman)
library(dslabs)
library(tidyverse)
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(at_bats_per_game = AB / G,
         runs_per_game = R / G) %>%
  ggplot(aes(at_bats_per_game, runs_per_game)) + geom_point()

# This plot confirms that as the number of at 
# bats increases so does runs per game.

# Use the filtered Teams data frame from Question 6. 
# Make a scatterplot of win rate (number of wins per game) 
# versus number of fielding errors (E) per game.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(win_rate = W / G, 
         errors_per_game = E / G) %>%
  ggplot(aes(win_rate, errors_per_game)) + geom_point()

# This plost confirms that as errors increase win rate decreases.

# Use the filtered Teams data frame from Question 6. 
# Make a scatterplot of triples (X3B) per game versus 
# doubles (X2B) per game.
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(doubles_per_game = X2B / G, 
         triples_per_game = X3B / G) %>%
  ggplot(aes(doubles_per_game, triples_per_game)) + geom_point()

# This plot confirms that there is no
# clear relationship between doubles and triples.
