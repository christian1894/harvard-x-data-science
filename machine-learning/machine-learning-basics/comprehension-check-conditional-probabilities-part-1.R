

# In a previous module, we covered Bayes' theorem and the Bayesian paradigm. 
# Conditional probabilities are a fundamental part of this 
# previous covered rule.

# P(A|B) = P(B|A) * (P(A) / P(B))

# We first review a simple example to go over conditional probabilities.

# Assume a patient comes into the doctor’s office to test whether 
# they have a particular disease.
# The test is positive 85% of the time when tested on a patient with 
# the disease (high sensitivity): 
# The test is negative 90% of the time when tested on a healthy 
# patient (high specificity):
# The disease is prevalent in about 2% of the community:

# Using Bayes' theorem, calculate the probability that you have
# the disease if the test is positive.
p_positive_test_sick_patient = 0.85
p_negative_test_healthy_patient = 0.9
disease_prevalence = 0.02

p_having_the_disease = p_positive_test *
  (disease_prevalence /
     (p_positive_test_sick_patient * disease_prevalence + 
        (1 - p_negative_test_healthy_patient) * (1 - disease_prevalence)))
  
# We have a hypothetical population of 1 million individuals with the 
# following conditional probabilities as described below:

# The test is positive 85% of the time when tested on a patient with the 
# disease (high sensitivity): 
  
# The test is negative 90% of the time when tested on a healthy patient
# (high specificity):
# The disease is prevalent in about 2% of the community:
  
set.seed(1) 
disease = sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test = rep(NA, 1e6)
test[disease==0] = 
  sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] = 
  sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

# What is the probability that a test is positive?
mean(test)

# What is the probability that an individual has the disease
# if the test is negative?
mean(disease[test==0])

# What is the probability that you have the disease if the test is positive?
mean(disease[test==1])

# Compare the prevalence of disease in people who test positive to 
# the overall prevalence of disease.

# If a patient's test is positive, by how many times does that 
# increase their risk of having the disease?
mean(disease[test==1]==1)/mean(disease==1)
