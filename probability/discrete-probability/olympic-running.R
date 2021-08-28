library(gtools)
library(tidyverse)
set.seed(1)
# In the 200m dash finals in the Olympics, 8 runners 
# compete for 3 medals (order matters). In the 2012 Olympics, 
# 3 of the 8 runners were from Jamaica and the other 5 were 
# from different countries. The three medals were all won by
# Jamaica (Usain Bolt, Yohan Blake, and Warren Weir).

# How many different ways can the 3 medals be 
# distributed across 8 runners?
medals = nrow(permutations(8, 3))

# How many different ways can the three medals be distributed 
# among the 3 runners from Jamaica?
jamaican_medals = nrow(permutations(3, 3))

# What is the probability that all 3 medals are won by Jamaica?

jamaican_medals / medals

# Run a Monte Carlo simulation on this vector representing the
# countries of the 8 runners in this race:

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", 
             "Ecuador", "Netherlands", "France", "South Africa")
B = 10000
medals_events = replicate(B, {
  winners = sample(runners, 3)
  all(winners == "Jamaica")
})
mean(medals_events)