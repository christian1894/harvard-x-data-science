# checking for duplicated birthday in 50 people group
n = 50
birthdays = function() {
  sample(1:365, n, replace = TRUE)
}
any(duplicated(birthdays()))
# Monte carlo simulation
B = 10000
events = replicate(B, {
  any(duplicated(birthdays()))
})
mean(events)

# approach using sapply
compute_probability = function(n, B = 10000) {
  events = replicate(B, {
    birthdays = sample(1:365, n, replace = TRUE)
    any(duplicated(birthdays))
  })
  mean(events)
}
# plot results of groups from 1 to 60 people
groups = seq(60)
events_probabilities = sapply(groups, compute_probability)
plot(groups, events_probabilities)

# exact probability(theoretical)
exact_probability = function(n) {
  probability_unique_birthday = seq(365, 365 - n + 1)/365
  1 - prod(probability_unique_birthday)
}
exact_probabilities = sapply(groups, exact_probability)
# plot events and theoretical probabilities
plot(groups, events_probabilities)
lines(groups, exact_probabilities, col = "red")

