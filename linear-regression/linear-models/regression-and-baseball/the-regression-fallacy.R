# The regression fallacy
library(dslabs)
library(tidyverse)
library(broom)
library(ggplot2)
library(Lahman)
data("Teams")

# Wikipedia defines the sophomore slump as:

# A sophomore slump or sophomore jinx or sophomore 
# jitters refers to an instance in which a second, 
# or sophomore, effort fails to live up to the 
# standards of the first effort. It is commonly used 
# to refer to the apathy of students (second year of
# high school, college or university), the performance 
# of athletes (second season of play), singers/bands 
# (second album), television shows (second seasons) 
# and movies (sequels/prequels).

# In Major League Baseball, the rookie of the year
# (ROY) award is given to the first-year player who
# is judged to have performed the best. The sophmore 
# slump phrase is used to describe the observation 
# that ROY award winners don’t do as well during 
# their second year. For example, this Fox Sports 
# article73 asks “Will MLB’s tremendous rookie 
# class of 2015 suffer a sophomore slump?”.

# Does the data confirm the existence of a sophomore 
# slump? Let’s take a look. Examining the data for 
# batting average, we see that this observation holds
# true for the top performing ROYs:

player_info = Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

# The code to create a table with only the ROY award 
# winners and add their batting statistics:

ROY = AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>%
  left_join(player_info, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

# The code to keep only the rookie and sophomore seasons
# and remove players who did not play sophomore seasons:

ROY = ROY %>%
  filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

# The code to use the spread function to have one column 
# for the rookie and sophomore years batting averages:

ROY = ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))

# In fact, the proportion of players that have a lower
# batting average their sophomore year is 0.686.
mean(ROY$sophomore - ROY$rookie <= 0)

# So is it “jitters” or “jinx”? To answer this question,
# let’s turn our attention to all players that played 
# the 2013 and 2014 seasons and batted more than 130 
# times (minimum to win Rookie of the Year).

# The same pattern arises when we look at the top 
# performers: batting averages go down for most of 
# the top performers.

two_years = Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID, yearID) %>%
  filter(sum(AB) >= 130) %>%
  summarize(AVG = sum(H)/sum(AB)) %>%
  ungroup %>%
  spread(yearID, AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(player_info, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`, `2014`)
two_years

# But these are not rookies! Also, look at what happens
# to the worst performers of 2013:

arrange(two_years, `2013`)

# Their batting averages mostly go up! Is this some 
# sort of reverse sophomore slump? It is not. There 
# is no such thing as the sophomore slump. This is all 
# explained with a simple statistical fact: the 
# correlation for performance in two separate years 
# is high, but not perfect:

qplot(`2013`, `2014`, data = two_years)

summarize(two_years, cor(`2013`,`2014`))

# The correlation is 0.46 and the data look very much 
# like a bivariate normal distribution, which means 
# we predict a 2014 batting average Y for any 
# given player that had a 2013 batting average X with:
# (Y−.255)/.032=0.46((X−.261)/.023)
# Because the correlation is not perfect, regression 
# tells us that, on average, expect high performers from 
# 2013 to do a bit worse in 2014. It’s not a jinx; it’s 
# just due to chance. The ROY are selected from the 
# top values of X so it is expected that Y will 
# regress to the mean.
