library(gtools)

# Two teams, say the Cavs and the Warriors, 
# are playing a seven game championship series. 
# The teams are equally good, so they each have a 50-50 
# The first to win four games wins the series. 
# chance of winning each game.
# If the Cavs lose the first game, 
# what is the probability that they win the series?

# Assign a variable 'n' as the number of remaining games.
n = 6

# Assign a variable `outcomes` as a vector of possible game outcomes, 
# where 0 indicates a loss and 1 indicates a win for the Cavs.
outcomes = c(0, 1)

# Assign a variable `l` to a list of all possible outcomes in all 
# remaining games. Use the `rep` function on `list(outcomes)` 
# to create list of length `n`.
l = rep(list(outcomes), length.out = n)


# Create a data frame named 'possibilities' that contains
# all combinations of possible outcomes for the remaining games.
possibilities = expand.grid(l)

# Create a vector named 'results' that indicates whether
# each row in the data frame 'possibilities' contains 
# enough wins for the Cavs to win the series.
results = rowSums(possibilities) >= 4

# Calculate the proportion of 'results' in which the Cavs win the series. 
# Print the outcome to the console.
mean(results)

# Monte carlo simulation to check result
B = 10000
events = replicate(B, {
  games_results = sum(sample(outcomes, 6, replace = TRUE, prob = c(0.5, 0.5)))
  games_results >= 4
})
mean(events)

# Two teams, and , are playing a seven series game series. 
# Team is better than team and has a chance of 
# winning each game.
probabilities = seq(0.5, 0.95, 0.025)
generate_winning_events = function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}
winning_events_probabilities = sapply(probabilities, generate_winning_events)
plot(probabilities, winning_events_probabilities)

# Repeat the previous exercise, but now keep the probability 
# that team wins fixed at p <- 0.75 and compute the
# probability for different series lengths. For example,
# wins in best of 1 game, 3 games, 5 games, and so on 
# through a series that lasts 25 games.
numbers_of_games_series = seq(1, 25, 2)

generate_winning_events <- function(N, p=0.75){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), N, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=(N+1)/2
  })
  mean(result)
}

winning_events_probabilities = sapply(numbers_of_games_series, generate_winning_events)
plot(numbers_of_games_series, winning_events_probabilities)
