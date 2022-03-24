# Write a line of code that calculates the standard error se 
# of a sample average when you poll 25 people in the 
# population. Generate a sequence of 100 proportions 
# of Democrats p that vary from 0 (no Democrats) to 1 
# (all Democrats).
# Plot se versus p for the 100 different proportions.
N = 25
proportions = seq(0, 1, len = 100)

get_estimate_se = function(p, N) {
  se = sqrt((p * (1 - p)) / N)
  se
}
se = get_estimate_se(proportions, N)
plot(proportions, se)

# Using the same code as in the previous exercise, 
# create a for-loop that generates three plots of p
# versus se when the sample sizes 
# equal N = 25, N = 100, and N = 1000.
sample_sizes = c(25, 100, 1000)
for (size in sample_sizes) {
  se = get_estimate_se(proportions, size)
  plot(proportions, se, ylim = c(0, 0.1))
}

# Say the actual proportion of Democratic voters is 0.45. 
# In this case, the Republican party is winning by 
# a relatively large margin of -0.1, or a 10% margin 
# of victory. What is the standard error of the spread
# in this case?
p = 0.45 
2 * sqrt((p * (1 -p)) / N)
  
