# Balanced accuracy and F1 score

# Although we usually recommend studying both specificity and sensitivity, 
# very often it is useful to have a one-number summary, for example 
# for optimization purposes. One metric that is preferred over overall 
# accuracy is the average of specificity and sensitivity, referred
# to as balanced accuracy. Because specificity and sensitivity are rates,
# it is more appropriate to compute the harmonic average. In fact, 
# the F1-score, a widely used one-number summary, is the harmonic 
# average of precision and recall:

# 1 / 0.5 (1 / recall + 1 / precision)

# Because it is easier to write, you often see this harmonic
# average rewritten as:
  
# 2 × (precision⋅recall / precision+recall)

# when defining F1.

# Remember that, depending on the context, some types of errors are 
# more costly than others. For example, in the case of plane safety,
# it is much more important to maximize sensitivity over specificity: 
# failing to predict a plane will malfunction before it crashes is a 
# much more costly error than grounding a plane when, in fact, the
# plane is in perfect condition. In a capital murder criminal case, 
# the opposite is true since a false positive can lead to executing an 
# innocent person. The F1-score can be adapted to weigh specificity 
# and sensitivity differently. To do this, we define β to 
# represent how much more important sensitivity is compared to 
# specificity and consider a weighted harmonic average:

# 1 / (((β^2/1+β^2) * (1/recall))+((1/1+β^2) * (1/precision)))

# The F_meas function in the caret package computes this summary with
# beta defaulting to 1.

# #Let’s rebuild our prediction algorithm, but this time maximizing
# the F-score instead of overall accuracy:


library(tidyverse)
library(caret)
library(dslabs)
data(heights)

x = heights$height
y = heights$sex

set.seed(2007)
test_index = createDataPartition(y, times = 1, p = 0.5, list = FALSE)

test_set = heights[test_index,]
train_set = heights[-test_index,]

cutoff = seq(61, 70)
F_1_score = map_dbl(cutoff, function(cutoff) {
  y_hat = ifelse(train_set$height > cutoff, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
  
})

# As before, we can plot these F1 measures versus the cutoffs:

as.data.frame(cbind(cutoff, F_1_score)) %>%
  ggplot(aes(cutoff, F_1_score)) +
  geom_line() +
  geom_point()

# We see that it is maximized at F1 value of:
max(F_1_score)

# This maximum is achieved when we use the following cutoff:
best_cutoff = cut_off[which.max(F_1_score)]
best_cutoff

# A cutoff of 66 makes more sense than 64. Furthermore, it
# balances the specificity and sensitivity of our confusion matrix:

y_hat = ifelse(test_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
specificity(data = y_hat, reference = test_set$sex)

# We now see that we do much better than guessing, that both
# sensitivity and specificity are relatively high, and that we have
# built our first machine learning algorithm. It takes height as a 
# predictor and predicts female if you are 65 inches or shorter.
