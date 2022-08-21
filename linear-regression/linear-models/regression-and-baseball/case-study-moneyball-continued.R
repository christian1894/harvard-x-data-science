# Case study: Moneyball (continued)
library(dslabs)
library(tidyverse)
library(broom)
library(ggplot2)
library(Lahman)
data("Teams")

# In trying to answer how well BBs predict runs, 
# data exploration led us to a model:

# E[R∣BB=x1,HR=x2]= β0 + β1x1 + β2x2

# Here, the data is approximately normal and conditional 
# distributions were also normal. Thus, we are justified
# in using a linear model:
  
# Yi=β0+β1xi,1+β2xi,2+εi

# with Yi runs per game for team i, xi,1 walks per 
# game, and xi,2 home runs per game. To use lm here, 
# we need to let the function know we have two predictor
# variables. So we use the + symbol as follows:

fit = Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)

# We can use tidy to see a nice summary:

tidy(fit, conf.int = TRUE) 

# When we fit the model with only one variable, the 
# estimated slopes were 0.735 and 1.845 for BB and HR,
# respectively. Note that when fitting the multivariable
# model both go down, with the BB effect decreasing much more.

# Now we want to construct a metric to pick players,
# we need to consider singles, doubles, and triples as
# well. Can we build a model that predicts runs based 
# on all these outcomes?
  
# We now are going to take somewhat of a “leap of faith” and 
# assume that these five variables are jointly normal. 
# This means that if we pick any one of them, and hold 
# the other four fixed, the relationship with the outcome
# is linear and the slope does not depend on the four 
# values held constant. If this is true, then a linear
# model for our data is:

# Yi=β0+β1xi,1+β2xi,2+β3xi,3+β4xi,4+β5xi,5+εi
# with xi,1,xi,2,xi,3,xi,4,xi,5 representing BB, singles, 
# doubles, triples, and HR respectively.
# Using lm, we can quickly find the LSE 
# for the parameters using:

fit = Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)

# We can see the coefficients using tidy:

coefs = tidy(fit, conf.int = TRUE)

# To see how well our metric actually predicts runs, 
# we can predict the number of runs for each team in
# 2002 using the function predict, then make a plot:

Teams %>%
  filter(yearID == 2002) %>%
  mutate(BB = BB / G, 
         singles = (H - HR - X3B - X2B) / G, 
         doubles = X2B / G,
         triples = X3B / G,
         HR = HR / G, 
         R = R / G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R, R_hat, label = teamID)) +
  geom_point(alpha = 0.8) +
  geom_text(nudge_x = 0.1, cex = 2) +
  geom_abline()

# Our model does quite a good job as demonstrated by 
# the fact that points from the observed versus 
# predicted plot fall close to the identity line.

# So instead of using batting average, or just number
# of HR, as a measure of picking players, we can use 
# our fitted model to form a metric that relates more
# directly to run production. Specifically, to define 
# a metric for player A, we imagine a team made up of
# players just like player A and use our fitted 
# regression model to predict how many runs this 
# team would produce. The formula would look like 
# this: -2.769 + 0.371 × BB + 0.519 × singles + 
# 0.771 × doubles + 1.24 × triples + 1.443 × HR.

# To define a player-specific metric, we have a bit 
# more work to do. A challenge here is that we 
# derived the metric for teams, based on team-level
# summary statistics. For example, the HR value 
# that is entered into the equation is HR per game 
# for the entire team. If we compute the HR per game 
# for a player, it will be much lower since the total
# is accumulated by 9 batters. Furthermore, if a 
# player only plays part of the game and gets fewer 
# opportunities than average, it is still considered 
# a game played. For players, a rate that takes into 
# account opportunities is the per-plate-appearance rate.

# To make the per-game team rate comparable to the
# per-plate-appearance player rate, we compute the 
# average number of team plate appearances per game:

PAs_per_game = Batting %>%
  filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB + BB) / max(G)) %>%
  pull(pa_per_game) %>%
  mean()

# We compute the per-plate-appearance rates for players 
# available in 2002 on data from 1997-2001. To avoid 
# small sample artifacts, we filter players with less
# than 200 plate appearances per year. Here is the 
# entire calculation in one line:

players = Batting %>% filter(yearID %in% 1997:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/PAs_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 1000) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# The player-specific predicted runs computed here can
# be interpreted as the number of runs we predict a team 
# will score if all batters are exactly like that player. 
# The distribution shows that there is wide variability 
# across players:
players %>% 
  ggplot(aes(R_hat)) +
  geom_histogram(binwidth = 0.5, col = "black")

# Adding salary and position information

# To actually build the team, we will need to know 
# their salaries as well as their defensive position.
# For this, we join the players data frame we just 
# created with the player information data frame 
# included in some of the other Lahman data tables.
# We will learn more about the join function we 
# learned in Section 23.1.

# Start by adding the 2002 salary of each player:

players = Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by = "playerID")

# Next, we add their defensive position. This is a 
# somewhat complicated task because players play more 
# than one position each year. The Lahman package table 
# Appearances tells how many games each player played in 
# each position, so we can pick the position that was most 
# played using which.max on each row. We use apply to do 
# this. However, because some players are traded, they
# appear more than once on the table, so we first sum their
# appearances across teams. Here, we pick the one position
# the player most played using the top_n function. To make
# sure we only pick one position, in the case of ties, 
# we pick the first row of the resulting data frame. 

# We also remove the OF position which stands for outfielder,
# a generalization of three positions: left field (LF), 
# center field (CF), and right field (RF). We also
# remove pitchers since they don’t bat in the league
# in which the A’s play.

position_names = 
  paste0("G_", c("p","c","1b","2b","3b","ss","lf","cf","rf", "dh"))

tmp = Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()

pos = tmp %>%
  select(position_names) %>%
  apply(., 1, which.max) 

players = tibble(playerID = tmp$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# Finally, we add their first and last name:

players = Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# If you are a baseball fan, you will recognize 
# the top 10 players:

players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% top_n(10) 

# Picking nine players

# On average, players with a higher metric have higher salaries:
  
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()
