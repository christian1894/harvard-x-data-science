# How many Monte Carlo simulations are enough?
# We will simulate many Monte Carlo experiments and plot
# event's probabilities and see where they become stable
B = 10^seq(1, 5, len = 100)
compute_probability = function(B, n = 50) {
  events = replicate(B, {
    birthdays = sample(365, n, replace = TRUE)
    any(duplicated(birthdays))
  })
  mean(events)
}
probabilities = sapply(B, compute_probability)
plot(log10(B), probabilities, type = "l")
