set.seed(21, sample.kind = "Rounding")
# The SAT is a standardized college admissions test used in the 
# United States. The following two multi-part questions will 
# ask you some questions about SAT testing.
# An old version of the SAT college entrance exam had a 
# -0.25 point penalty for every incorrect answer and awarded
# 1 point for a correct answer. The quantitative test consisted 
# of 44 multiple-choice questions each with 5 answer choices. 
# Suppose a student chooses answers by guessing for all 
# questions on the test.

# What is the probability of guessing correctly for one question?
choices_per_question = 5
number_of_questions = 44
probability_guess_one_question = 1 / choices_per_question

# What is the expected value of points for guessing on one question?
mu = probability_guess_one_question +
  (-0.25 * (1 - probability_guess_one_question))
mu

# What is the expected score of guessing on all 44 questions?
44 * mu

# What is the standard error of guessing on all 44 questions?
sigma = sqrt(44) * abs(-0.25 - 1) * 
  sqrt(probability_guess_one_question * (1 -  probability_guess_one_question))

# Use the Central Limit Theorem to determine the probability 
# that a guessing student scores 8 points or higher on the test.
1 - pnorm(8, mean = mu, sd = sigma)

# Run a Monte Carlo simulation of 10,000 students guessing on the test.
B = 10000
S = replicate(B, {
  x = sample(
    size =  44, 
    x = c(1, -0.25),
    replace = TRUE, 
    prob = c(probability_guess_one_question, 1 - probability_guess_one_question))
  sum(x)
})
# What is the probability that a guessing student scores 8 points or higher?
mean(S > 8)

# The SAT was recently changed to reduce the number of multiple choice 
# options from 5 to 4 and also to eliminate the penalty for
# guessing.

# In this two-part question, you'll explore how that affected 
# the expected values for the test.
choices_per_question = 4
number_of_questions = 44
probability_guess_one_question = 1 / choices_per_question

# What is the expected value of the score when guessing on this new test?
mu = 44 * (probability_guess_one_question +
  (0 * (1 - probability_guess_one_question)))
mu

# Consider a range of correct answer probabilities 
# p <- seq(0.25, 0.95, 0.05) representing a range of 
# student skills. 
# What is the lowest p such that the probability of 
# scoring over 35 exceeds 80%?
probability_values = seq(0.25, 0.95, 0.05)
get_score_probability = function(probability_guessing) {
  desired_score =  35
  mu = number_of_questions * 1 * probability_guessing +  0 * (1 - probability_guessing)
  sigma <- sqrt(number_of_questions) * abs(0 - 1) * sqrt(probability_guessing * (1 - probability_guessing))
  1 - pnorm(desired_score, mean = mu, sd = sigma)
  }
results = get_score_probability(probability_values)
min(probability_values[which(results > 0.8)])
