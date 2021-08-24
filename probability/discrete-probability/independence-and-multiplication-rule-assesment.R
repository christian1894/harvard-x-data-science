# Sampling with replacement
# Say you’ve drawn 5 balls from the a box that has 3 cyan balls,
# 5 magenta balls, and 7 yellow balls, with replacement,
# and all have been yellow.
# What is the probability that the next one is yellow?

cyan <- 3
magenta <- 5
yellow <- 7
total = cyan + magenta + yellow
yellow / total

# Rolling a die
# If you roll a 6-sided die once, 
# what is the probability of not seeing a 6? 

p_not_6 = 5 / 6

# If you roll a 6-sided die six times,
# what is the probability of not seeing a 6 on any of those rolls?
p_not_6 ^ 6

# Probability the Celtics win a game(Celtics vs Cavs)
# 7 game series
cavs_win_rate = 0.6
probability_cavs_winning_first_four_games = cavs_win_rate ^ 4
1 - probability_cavs_winning_first_four_games

# Monte Carlo simulation for Celtics winning a game 

simulated_games = function() {
  sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
}
B = 10000
celtic_wins_events = replicate(B, {
  any(simulated_games() == "win")
})

probability_celtics_win_at_least_one_game = mean(celtic_wins_events)
